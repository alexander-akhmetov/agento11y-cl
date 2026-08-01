(in-package :sigil-cl)

;;; Built-in output evaluators.
;;;
;;; An evaluator grades one (input, output, expected) triple in this process
;;; and returns an EVALUATION-RESULT. TRIAL-RECORD-EVALUATION turns that result
;;; into a score on a trial. Ports go/agento11y/experiments/evaluators.go.
;;;
;;; The LLM judge calls no provider itself: it takes an invoke closure, so the
;;; SDK's dependency list stays as it is and any client can be adapted in a few
;;; lines. Same choice the Go SDK made.

(defgeneric evaluate-output (evaluator input)
  (:documentation "Grade INPUT with EVALUATOR and return an EVALUATION-RESULT.
INPUT is a plist of :input, :output, and :expected."))

(defstruct evaluation-result
  "One evaluator's verdict on one output.
GRADER is NIL, or a plist of :input, :output, :model-provider, :model-name,
:agent-name, :agent-version, :operation-name, and :usage describing the call
that produced the verdict."
  evaluator-id
  evaluator-version
  evaluator-kind
  value
  passed
  explanation
  score-key
  metadata
  grader)

;;; --- Prompt rendering ---

(defparameter +default-llm-judge-prompt+
  "Grade the candidate output against the input and expected result.

Input:
{input}

Expected:
{expected}

Candidate output:
{output}

Return only JSON with this shape:
{\"score\": <number from 0 to 1>, \"passed\": <boolean>, \"explanation\": \"<brief reason>\"}
"
  "Default judge prompt. Matches DefaultLLMJudgePrompt in
go/agento11y/experiments/evaluators.go character for character.")

(defparameter +judge-placeholder-scanner+
  (cl-ppcre:create-scanner "\\{(input|output|expected)\\}")
  "The placeholders %RENDER-JUDGE-PROMPT substitutes.")

(defun %render-judge-prompt (template input output expected)
  "Substitute {input}, {output}, and {expected} in TEMPLATE in a single pass.
A second pass would substitute placeholders that appeared inside a substituted
value, which is what Go's strings.NewReplacer avoids. :SIMPLE-CALLS writes each
replacement straight out, so a value is never rescanned and a backslash in it
is not read as a register reference."
  (cl-ppcre:regex-replace-all
   +judge-placeholder-scanner+ template
   (lambda (match name)
     (declare (ignore match))
     (%text (cond ((string= name "input") input)
                  ((string= name "output") output)
                  (t expected))))
   :simple-calls t))

;;; --- Judge response parsing ---

(defun %top-level-json-objects (raw)
  "Collect complete top-level JSON objects from RAW, in order.
JZON:PARSE rejects trailing content, so each candidate is read through a
streaming parser over a stream positioned at the opening brace; FILE-POSITION
afterwards gives the end offset. A nested object is consumed as part of its
parent and never collected on its own, so a top-level score always beats a
nested one."
  (let ((objects nil)
        (cursor 0)
        (len (length raw)))
    (loop while (< cursor len)
          do (let ((start (position #\{ raw :start cursor)))
               (unless start (return))
               (let ((stream (make-string-input-stream raw start)))
                 (handler-case
                     (let ((value (jzon:with-parser (parser stream)
                                    (jzon:parse-next-element parser))))
                       (when (hash-table-p value)
                         (push value objects))
                       (setf cursor (max (1+ start) (+ start (file-position stream)))))
                   (error ()
                     (setf cursor (1+ start)))))))
    (nreverse objects)))

(defparameter +max-judge-score-exponent+ 1000
  "Largest decimal shift %PARSE-REAL will apply. A double float tops out near
10^308, so nothing beyond this bound is a score. The bound is checked before
the shift because (EXPT 10 N) builds the bignum first: 1e10000000 does not
finish in a minute, long before any float could overflow.")

(defparameter +max-judge-score-digits+ 1000
  "Most digits %PARSE-REAL reads from one run, for the mantissa and for the
exponent alike. Each digit folds into a bignum that grows with the run, so the
cost is quadratic in its length: 300000 digits take three seconds, and the
judge writes this text. The count is checked before each fold rather than
after the run, so a rejected number is never built. A double carries 17
significant digits and 10^308, so nothing this rejects was a score.")

(defun %parse-real (value)
  "Parse VALUE as a real number, or return NIL.
Hand-rolled rather than READ-based: a judge response is untrusted text, and
the reader would evaluate read macros in it."
  (when (realp value)
    (return-from %parse-real value))
  (unless (stringp value)
    (return-from %parse-real nil))
  (let* ((s (%trim value))
         (len (length s))
         (i 0)
         (sign 1)
         (digits 0)
         (mantissa 0)
         (scale 0)
         (exponent 0))
    (when (zerop len)
      (return-from %parse-real nil))
    (when (member (char s i) '(#\+ #\-))
      (when (char= (char s i) #\-) (setf sign -1))
      (incf i))
    (loop while (and (< i len) (digit-char-p (char s i)))
          do (when (>= digits +max-judge-score-digits+)
               (return-from %parse-real nil))
             (setf mantissa (+ (* mantissa 10) (digit-char-p (char s i))))
             (incf digits)
             (incf i))
    (when (and (< i len) (char= (char s i) #\.))
      (incf i)
      (loop while (and (< i len) (digit-char-p (char s i)))
            do (when (>= digits +max-judge-score-digits+)
                 (return-from %parse-real nil))
               (setf mantissa (+ (* mantissa 10) (digit-char-p (char s i))))
               (incf digits)
               (incf scale)
               (incf i)))
    (when (zerop digits)
      (return-from %parse-real nil))
    (when (and (< i len) (member (char s i) '(#\e #\E)))
      (let ((j (1+ i))
            (esign 1)
            (edigits 0)
            (evalue 0))
        (when (and (< j len) (member (char s j) '(#\+ #\-)))
          (when (char= (char s j) #\-) (setf esign -1))
          (incf j))
        (loop while (and (< j len) (digit-char-p (char s j)))
              do (when (>= edigits +max-judge-score-digits+)
                   (return-from %parse-real nil))
                 (setf evalue (+ (* evalue 10) (digit-char-p (char s j))))
                 (incf edigits)
                 (incf j))
        (when (plusp edigits)
          (setf exponent (* esign evalue)
                i j))))
    (unless (= i len)
      (return-from %parse-real nil))
    (let ((power (- exponent scale)))
      (when (> (abs power) +max-judge-score-exponent+)
        (return-from %parse-real nil))
      ;; A value inside the bound can still overflow the double, and this
      ;; function promises NIL rather than a condition no caller expects.
      (handler-case
          (coerce (* sign mantissa (expt 10 power)) 'double-float)
        (arithmetic-error () nil)))))

(defun %judge-passed-value (value fallback)
  "Read a judge's pass verdict from VALUE, or return FALLBACK.
JSON null parses to a symbol that is neither T nor NIL, so an explicit null
falls back rather than reading as false."
  (cond
    ((eq value t) t)
    ((null value) nil)
    ((stringp value)
     (let ((normalized (string-downcase (%trim value))))
       (cond
         ((member normalized '("true" "yes" "1" "pass" "passed") :test #'string=) t)
         ((member normalized '("false" "no" "0" "fail" "failed") :test #'string=) nil)
         (t fallback))))
    (t fallback)))

(defun %judge-string-value (object key)
  "Read KEY off OBJECT as text. Anything but a string or a number is dropped
rather than printed, since a judge that answered with a structure there has
not written an explanation."
  (multiple-value-bind (value found-p) (gethash key object)
    (cond
      ((not found-p) nil)
      ((stringp value) value)
      ((realp value) (%text value))
      (t nil))))

(defun %parse-judge-response (raw threshold)
  "Read (values score passed explanation) out of a judge's reply.
Walks the top-level JSON objects last to first and takes the first that has a
`score` key, so a judge that reasons in JSON before answering is graded on its
answer. A `score` the reply carries but this cannot parse signals: falling
through to an earlier object would grade the judge's scratch work instead.
The score is clamped to [0,1] and `passed` defaults to score >= THRESHOLD
unless the reply overrides it."
  (let ((objects (%top-level-json-objects (%text raw))))
    (dolist (object (reverse objects))
      (multiple-value-bind (raw-score found-p) (gethash "score" object)
        (when found-p
          (let ((score (%parse-real raw-score)))
            (unless score
              (error 'sigil-validation-error
                     :message "LLM judge response requires a numeric score"))
            (let* ((clamped (min 1.0d0 (max 0.0d0 (coerce score 'double-float))))
                   (passed (>= clamped threshold)))
              (multiple-value-bind (raw-passed passed-p) (gethash "passed" object)
                (if passed-p
                    (setf passed (%judge-passed-value raw-passed passed))
                    (multiple-value-bind (raw-pass pass-p) (gethash "pass" object)
                      (when pass-p
                        (setf passed (%judge-passed-value raw-pass passed))))))
              (let ((explanation (or (%judge-string-value object "explanation")
                                     (%judge-string-value object "reason")
                                     "")))
                (return-from %parse-judge-response
                  (values clamped passed (%trim explanation)))))))))
    ;; Two distinct failures: nothing that parses at all, versus objects that
    ;; parse but carry no score. They point at different prompt problems.
    (if (null objects)
        (error 'sigil-validation-error
               :message "LLM judge response did not contain a JSON object")
        (error 'sigil-validation-error
               :message "LLM judge response requires a numeric score"))))

;;; --- LLM judge ---

(defparameter +llm-judge-kind+ "llm_judge")
(defparameter +deterministic-judge-kind+ "deterministic")

(defclass llm-judge ()
  ((evaluator-id :initarg :evaluator-id :reader llm-judge-evaluator-id)
   ;; (lambda (prompt) (values text usage))
   (invoke :initarg :invoke :reader llm-judge-invoke)
   (model-name :initarg :model-name :reader llm-judge-model-name)
   (model-provider :initarg :model-provider :initform nil :reader llm-judge-model-provider)
   (prompt-template :initarg :prompt-template :reader llm-judge-prompt-template)
   (version :initarg :version :reader llm-judge-version)
   (score-key :initarg :score-key :reader llm-judge-score-key)
   (threshold :initarg :threshold :reader llm-judge-threshold)
   ;; (lambda (text) (values score passed explanation))
   (parser :initarg :parser :initform nil :reader llm-judge-parser)
   (agent-name :initarg :agent-name :reader llm-judge-agent-name)
   (agent-version :initarg :agent-version :reader llm-judge-agent-version)
   (operation-name :initarg :operation-name :reader llm-judge-operation-name)))

(defun make-llm-judge (&key evaluator-id invoke model-name model-provider
                            prompt-template version score-key (threshold 0.5)
                            parser agent-name agent-version operation-name)
  "Build an LLM-as-judge evaluator.

INVOKE is called as (funcall invoke prompt) and returns the judge's reply text
plus, as a second value, an optional TOKEN-USAGE. PARSER, when supplied, is
called as (funcall parser text) and returns (values score passed explanation);
the default reads the JSON shape in +DEFAULT-LLM-JUDGE-PROMPT+."
  (when (%blank-string-p evaluator-id)
    (error 'sigil-validation-error
           :message "evaluator validation failed: evaluator_id is required"))
  (unless (functionp invoke)
    (error 'sigil-validation-error
           :message "evaluator validation failed: invoke callback is required"))
  (when (%blank-string-p model-name)
    (error 'sigil-validation-error
           :message "evaluator validation failed: model name is required"))
  (unless (and (realp threshold) (<= 0 threshold 1))
    (error 'sigil-validation-error
           :message "evaluator validation failed: pass threshold must be between 0 and 1"))
  (let ((resolved-version (if (%blank-string-p version) "1" (%trimmed-text version))))
    (make-instance 'llm-judge
                   :evaluator-id (%trimmed-text evaluator-id)
                   :invoke invoke
                   :model-name (%trimmed-text model-name)
                   :model-provider (unless (%blank-string-p model-provider)
                                     (%trimmed-text model-provider))
                   :prompt-template (if (%blank-string-p prompt-template)
                                        +default-llm-judge-prompt+
                                        prompt-template)
                   :version resolved-version
                   :score-key (if (%blank-string-p score-key) "final" (%trimmed-text score-key))
                   :threshold threshold
                   :parser parser
                   :agent-name (if (%blank-string-p agent-name)
                                   "agento11y-llm-judge"
                                   (%trimmed-text agent-name))
                   :agent-version (if (%blank-string-p agent-version)
                                      resolved-version
                                      (%trimmed-text agent-version))
                   :operation-name (if (%blank-string-p operation-name)
                                       "llm-judge"
                                       (%trimmed-text operation-name)))))

(defmethod evaluate-output ((judge llm-judge) input)
  (let ((prompt (%render-judge-prompt (llm-judge-prompt-template judge)
                                      (getf input :input)
                                      (getf input :output)
                                      (getf input :expected))))
    (multiple-value-bind (text usage) (funcall (llm-judge-invoke judge) prompt)
      (multiple-value-bind (score passed explanation)
          (if (llm-judge-parser judge)
              (funcall (llm-judge-parser judge) text)
              (%parse-judge-response text (llm-judge-threshold judge)))
        (let ((metadata (jobj)))
          (setf (gethash "judge_model" metadata) (llm-judge-model-name judge))
          (when (llm-judge-model-provider judge)
            (setf (gethash "judge_provider" metadata) (llm-judge-model-provider judge)))
          (make-evaluation-result
           :evaluator-id (llm-judge-evaluator-id judge)
           :evaluator-version (llm-judge-version judge)
           :evaluator-kind +llm-judge-kind+
           :value score
           :passed passed
           :explanation explanation
           :score-key (llm-judge-score-key judge)
           :metadata metadata
           :grader (list :input prompt
                         :output (%text text)
                         :model-provider (llm-judge-model-provider judge)
                         :model-name (llm-judge-model-name judge)
                         :agent-name (llm-judge-agent-name judge)
                         :agent-version (llm-judge-agent-version judge)
                         :operation-name (llm-judge-operation-name judge)
                         :usage usage)))))))

;;; --- Regex judge ---

(defclass regex-judge ()
  ((evaluator-id :initarg :evaluator-id :reader regex-judge-evaluator-id)
   (pattern :initarg :pattern :reader regex-judge-pattern)
   (scanner :initarg :scanner :reader regex-judge-scanner)
   (version :initarg :version :reader regex-judge-version)
   (score-key :initarg :score-key :reader regex-judge-score-key)
   (full-match :initarg :full-match :reader regex-judge-full-match)
   (negate :initarg :negate :reader regex-judge-negate)
   (explanation :initarg :explanation :reader regex-judge-explanation)))

(defun make-regex-judge (&key evaluator-id pattern version score-key
                              full-match negate explanation
                              case-insensitive multiline dot-all)
  "Build a deterministic evaluator matching the output against PATTERN.

The flags become an inline (?ims) prefix, as in Go. FULL-MATCH requires the
leftmost match to span the whole output, which is what CL-PPCRE:SCAN reports
naturally; Python's re.fullmatch backtracks inside the pattern to find a match
that spans the whole string, so it accepts patterns this check rejects."
  (when (%blank-string-p evaluator-id)
    (error 'sigil-validation-error
           :message "evaluator validation failed: evaluator_id is required"))
  (when (%blank-string-p pattern)
    (error 'sigil-validation-error
           :message "evaluator validation failed: pattern is required"))
  (let* ((prefix (concatenate 'string
                              (if case-insensitive "i" "")
                              (if multiline "m" "")
                              (if dot-all "s" "")))
         (source (if (plusp (length prefix))
                     (format nil "(?~a)~a" prefix pattern)
                     pattern))
         (scanner (handler-case
                      (cl-ppcre:create-scanner source)
                    (error (e)
                      (error 'sigil-validation-error
                             :message (format nil "evaluator validation failed: compile regex judge pattern: ~a"
                                              (princ-to-string e)))))))
    (make-instance 'regex-judge
                   :evaluator-id (%trimmed-text evaluator-id)
                   :pattern pattern
                   :scanner scanner
                   :version (if (%blank-string-p version) "1" (%trimmed-text version))
                   :score-key (if (%blank-string-p score-key)
                                  "regex_match"
                                  (%trimmed-text score-key))
                   :full-match (and full-match t)
                   :negate (and negate t)
                   :explanation (unless (%blank-string-p explanation)
                                  (%text explanation)))))

(defmethod evaluate-output ((judge regex-judge) input)
  (let* ((text (%text (getf input :output)))
         (matched (multiple-value-bind (start end)
                      (cl-ppcre:scan (regex-judge-scanner judge) text)
                    (if (regex-judge-full-match judge)
                        (and start (= start 0) (= end (length text)))
                        (and start t))))
         (passed (if (regex-judge-negate judge) (not matched) matched))
         (explanation (or (regex-judge-explanation judge)
                          (format nil "~a /~a/"
                                  (cond
                                    ((not matched) "output did not match")
                                    ((regex-judge-negate judge) "output matched excluded")
                                    (t "output matched"))
                                  (regex-judge-pattern judge)))))
    (make-evaluation-result
     :evaluator-id (regex-judge-evaluator-id judge)
     :evaluator-version (regex-judge-version judge)
     :evaluator-kind +deterministic-judge-kind+
     :value passed
     :passed passed
     :explanation explanation
     :score-key (regex-judge-score-key judge)
     :metadata (jobj "pattern" (regex-judge-pattern judge))
     :grader nil)))
