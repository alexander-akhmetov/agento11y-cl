(in-package :agento11y-cl/t)

;;; Conformance suites against the vendored cross-SDK fixtures in t/fixtures/.
;;;
;;; These are the same files the Go, Python, and JavaScript suites check
;;; themselves against, so a shape this SDK gets wrong fails here instead of
;;; reaching a live backend. t/fixtures/README.md records the upstream commit
;;; and the refresh command.
;;;
;;; This file loads before t/tests.lisp and shares nothing with it: every
;;; helper here is prefixed CF- and built only on the SDK and t/suite.lisp.

;;; --- Fixture loading ---

(defun cf-fixture-path (relative)
  (asdf:system-relative-pathname :agento11y-cl/t
                                 (concatenate 'string "t/fixtures/" relative)))

(defun cf-fixture-text (relative)
  (uiop:read-file-string (cf-fixture-path relative)))

(defun cf-fixture (relative)
  "Parse a vendored fixture into jzon values."
  (jzon:parse (cf-fixture-text relative)))

;;; --- Structural JSON differ ---
;;;
;;; conformance/hooks/README.md and conformance/experiments/README.md both say
;;; to compare parsed structures rather than bytes: key order and the
;;; whitespace inside embedded JSON are not part of the contract. A missing key
;;; and a key holding JSON null are different, so key presence is read with the
;;; second value of GETHASH.

(defun cf-json-diff (actual expected &optional (path "$"))
  "Differences between two parsed JSON values, as a list of strings.
An empty list means ACTUAL conforms to EXPECTED."
  (flet ((differs (fmt &rest args)
           (list (format nil "~a: ~?" path fmt args))))
    (cond
      ((hash-table-p expected)
       (if (not (hash-table-p actual))
           (differs "expected an object, got ~s" actual)
           (let ((diffs nil))
             (maphash (lambda (key value)
                        (multiple-value-bind (found found-p) (gethash key actual)
                          (setf diffs
                                (append diffs
                                        (if found-p
                                            (cf-json-diff found value
                                                          (format nil "~a.~a" path key))
                                            (list (format nil "~a.~a: missing" path key)))))))
                      expected)
             (maphash (lambda (key value)
                        (declare (ignore value))
                        (unless (nth-value 1 (gethash key expected))
                          (setf diffs (append diffs
                                              (list (format nil "~a.~a: unexpected"
                                                            path key))))))
                      actual)
             diffs)))
      ((stringp expected)
       (unless (and (stringp actual) (string= actual expected))
         (differs "expected ~s, got ~s" expected actual)))
      ((vectorp expected)
       (cond
         ((or (not (vectorp actual)) (stringp actual))
          (differs "expected an array, got ~s" actual))
         ((/= (length actual) (length expected))
          (differs "expected ~d element(s), got ~d" (length expected) (length actual)))
         (t (loop for i below (length expected)
                  append (cf-json-diff (aref actual i) (aref expected i)
                                       (format nil "~a[~d]" path i))))))
      ((realp expected)
       (unless (and (realp actual) (= actual expected))
         (differs "expected ~s, got ~s" expected actual)))
      (t
       (unless (eq actual expected)
         (differs "expected ~s, got ~s" expected actual))))))

(defun cf-label (label diffs)
  "LABEL, with up to three differences appended so a failure names them.
The make test filter keeps only the check line, so a diff printed separately
would never be seen."
  (if diffs
      (format nil "~a -- ~{~a~^; ~}~:[~; ...~]"
              label (subseq diffs 0 (min 3 (length diffs))) (> (length diffs) 3))
      label))

(defun cf-signals-p (thunk)
  "Whether THUNK signals an AGENTO11Y-ERROR."
  (handler-case (progn (funcall thunk) nil)
    (agento11y-error () t)))

;;; ================================================================
;;; Hooks conformance
;;; ================================================================

(defun cf-hooks-preflight-context ()
  (make-hook-context :model-provider "anthropic"
                     :model-name "claude-sonnet-4"
                     :agent-name "conformance-agent"
                     :agent-version "1.2.3"
                     :tags '(("env" . "test") ("team" . "agent-observability"))
                     :conversation-id "conv-hooks-conformance"
                     :trace-id "0123456789abcdef0123456789abcdef"
                     :span-id "0123456789abcdef"))

(defparameter +cf-bash-schema+
  "{\"type\":\"object\",\"properties\":{\"command\":{\"type\":\"string\",\"description\":\"Shell command to run.\"}},\"required\":[\"command\"]}")

(defparameter +cf-read-file-schema+
  "{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}},\"required\":[\"path\"]}")

(defun cf-hooks-preflight-input ()
  (make-hook-input
   :system-prompt "You are a careful assistant."
   :conversation-preview "user: Delete the cache directory under /tmp."
   :messages
   (list (make-message :role :user
                       :parts (list (make-text-part "Delete the cache directory under /tmp.")))
         (make-message
          :role :assistant
          :parts (list (make-thinking-part
                        "The request is destructive, so inspect the directory first.")
                       (make-tool-call-part :id "call-read" :name "read_file"
                                            :input-json "{\"path\":\"/tmp/cache/manifest.json\"}")
                       (make-tool-call-part :id "call-bash" :name "Bash"
                                            :input-json "{\"command\":\"rm -rf /tmp/cache\"}")))
         (make-message
          :role :tool :name "read_file"
          :parts (list (make-tool-result-part :tool-call-id "call-read" :name "read_file"
                                              :content "3 entries"
                                              :content-json "{\"entries\":3}")))
         (make-message
          :role :tool :name "Bash"
          :parts (list (make-tool-result-part :tool-call-id "call-bash" :name "Bash"
                                              :is-error t
                                              :content "rm: cannot remove '/tmp/cache': Permission denied"
                                              :content-json "{\"exit_code\":1}"))))
   :tools (list (list :name "Bash" :description "Run a shell command." :type "function"
                      :input-schema-json +cf-bash-schema+)
                (list :name "read_file" :description "Read a file from disk." :type "function"
                      :input-schema-json +cf-read-file-schema+))))

(defun cf-hooks-postflight-context ()
  (make-hook-context :model-provider "anthropic"
                     :model-name "claude-sonnet-4"
                     :agent-name "conformance-guard"
                     :agent-version "1.2.3"))

(defun cf-hooks-postflight-input ()
  (make-hook-input
   :output (list (make-message
                  :role :assistant
                  :parts (list (make-tool-call-part
                                :id "call-bash" :name "Bash"
                                :input-json "{\"command\":\"rm -rf /tmp/cache\"}"))))))

(defun cf-serialize-hook-request (phase context input)
  "The request body as it goes on the wire, re-read through jzon so the
comparison sees the encoded form and not the Lisp values behind it."
  (jzon:parse (jzon:stringify (agento11y-cl::%serialize-request phase context input))))

(defun cf-parse-part (part)
  "Parse one wire PART through the message parser. NIL means it was dropped."
  (let ((message (agento11y-cl::%parse-wire-message
                  (jobj "role" "assistant" "parts" (vector part)))))
    (first (message-parts message))))

(defun cf-part-shape (part)
  "A comparable description of a parsed part."
  (typecase part
    (null :dropped)
    (text-part (list :text (text-part-text part)))
    (thinking-part (list :thinking (thinking-part-text part)))
    (tool-call-part (list :tool-call
                          (tool-call-part-id part)
                          (tool-call-part-name part)
                          (tool-call-part-input-json part)))
    (tool-result-part (list :tool-result
                            (tool-result-part-tool-call-id part)
                            (tool-result-part-name part)
                            (tool-result-part-content part)
                            (agento11y-cl::tool-result-part-content-json part)
                            (and (tool-result-part-is-error part) t)))
    (t (list :unrecognized (type-of part)))))

(defun cf-tool-call-part (payload &rest extra)
  (apply #'jobj "kind" "tool_call" "tool_call" payload extra))

(defparameter +cf-part-dispatch-cases+
  ;; (label wire-part expected-shape). The rules come from
  ;; conformance/hooks/README.md: the kind decides which field a parser reads,
  ;; no parser reads a second one, and a part with nothing to recover is
  ;; dropped. Only a part with no kind at all reads whichever field is set.
  (list
   (list "kind text keeps the text"
         (jobj "kind" "text" "text" "hello")
         '(:text "hello"))
   (list "kind text with an empty text is dropped"
         (jobj "kind" "text" "text" "")
         :dropped)
   (list "kind text commits: empty text alongside thinking is dropped"
         (jobj "kind" "text" "text" "" "thinking" "reasoning")
         :dropped)
   (list "kind thinking keeps the thinking"
         (jobj "kind" "thinking" "thinking" "reasoning")
         '(:thinking "reasoning"))
   (list "kind thinking with an empty thinking is dropped"
         (jobj "kind" "thinking" "thinking" "")
         :dropped)
   (list "kind thinking commits: empty thinking alongside text is dropped"
         (jobj "kind" "thinking" "thinking" "" "text" "hello")
         :dropped)
   (list "kind tool_call keeps the call"
         (cf-tool-call-part (jobj "id" "call-bash" "name" "Bash"
                                  "input_json" "eyJjb21tYW5kIjoicm0gLXJmIC90bXAvY2FjaGUifQ=="))
         '(:tool-call "call-bash" "Bash" "{\"command\":\"rm -rf /tmp/cache\"}"))
   (list "kind tool_call without a payload object is dropped"
         (jobj "kind" "tool_call")
         :dropped)
   (list "kind tool_call commits: no payload object and a text is dropped"
         (jobj "kind" "tool_call" "text" "hello")
         :dropped)
   (list "kind tool_call without a name is dropped"
         (cf-tool-call-part (jobj "id" "call-bash" "input_json" "e30="))
         :dropped)
   (list "kind tool_result keeps the result"
         (jobj "kind" "tool_result"
               "tool_result" (jobj "tool_call_id" "call-bash" "name" "Bash"
                                   "content" "denied"
                                   "content_json" "eyJleGl0X2NvZGUiOjF9"
                                   "is_error" t))
         '(:tool-result "call-bash" "Bash" "denied" "{\"exit_code\":1}" t))
   (list "kind tool_result without a payload object is dropped"
         (jobj "kind" "tool_result")
         :dropped)
   (list "kind tool_result commits: no payload object and a text is dropped"
         (jobj "kind" "tool_result" "text" "hello")
         :dropped)
   ;; Protobuf JSON writes this shape when kind is the zero enum and the
   ;; submessage is set to its default. The fixtures do not cover it, and the
   ;; four SDKs disagree: Go and Python build a blank part, JS drops it. This
   ;; SDK follows Go and Python, which is what the README's drop list says.
   (list "kind tool_result with an empty payload object still parses"
         (jobj "kind" "tool_result" "tool_result" (jobj))
         '(:tool-result "" nil "" nil nil))
   (list "a JSON null is_error is false, not true"
         (jobj "kind" "tool_result"
               "tool_result" (jobj "tool_call_id" "c1" "content" "ok"
                                   "is_error" 'cl:null))
         '(:tool-result "c1" nil "ok" nil nil))
   (list "an unknown kind carrying text becomes a text part"
         (jobj "kind" "citation" "text" "see page 3")
         '(:text "see page 3"))
   (list "an unknown kind without text is dropped"
         (jobj "kind" "citation" "thinking" "reasoning")
         :dropped)
   (list "no kind falls back to tool_call"
         (jobj "tool_call" (jobj "id" "c1" "name" "Bash" "input_json" "e30="))
         '(:tool-call "c1" "Bash" "{}"))
   (list "no kind falls back to tool_result"
         (jobj "tool_result" (jobj "tool_call_id" "c1" "content" "ok"))
         '(:tool-result "c1" nil "ok" nil nil))
   (list "no kind falls back to thinking"
         (jobj "thinking" "reasoning")
         '(:thinking "reasoning"))
   (list "no kind falls back to text"
         (jobj "text" "hello")
         '(:text "hello"))
   (list "no kind prefers tool_call over text"
         (jobj "tool_call" (jobj "id" "c1" "name" "Bash") "text" "hello")
         '(:tool-call "c1" "Bash" ""))
   (list "no kind prefers thinking over text"
         (jobj "thinking" "reasoning" "text" "hello")
         '(:thinking "reasoning"))
   ;; The payload object that is present resolves the kind, and the parser then
   ;; commits to it: a nameless call is dropped rather than read as the text
   ;; beside it. Go, Python and JS resolve the fallback kind the same way.
   (list "no kind commits: a nameless tool_call beside a text is dropped"
         (jobj "tool_call" (jobj "id" "c1") "text" "hello")
         :dropped)
   (list "no kind commits: an empty tool_result beside a text hides it"
         (jobj "tool_result" (jobj) "text" "hello")
         '(:tool-result "" nil "" nil nil))
   (list "no kind and no payload field is dropped"
         (jobj "role" "assistant")
         :dropped)
   ;; The four payload rules. A response payload is base64 of whatever bytes
   ;; the proto field held, and every SDK has to end up with a valid JSON
   ;; document.
   (list "base64 that decodes to JSON becomes that document"
         (cf-tool-call-part (jobj "id" "c1" "name" "t" "input_json" "eyJhIjoxfQ=="))
         '(:tool-call "c1" "t" "{\"a\":1}"))
   (list "base64 that decodes to plain text becomes a JSON string"
         (cf-tool-call-part (jobj "id" "c1" "name" "t" "input_json" "aGVsbG8gd29ybGQ="))
         '(:tool-call "c1" "t" "\"hello world\""))
   (list "a non-base64 JSON document is kept as is"
         (cf-tool-call-part (jobj "id" "c1" "name" "t" "input_json" "{\"a\": 1}"))
         '(:tool-call "c1" "t" "{\"a\": 1}"))
   (list "a string that is neither becomes a JSON string"
         (cf-tool-call-part (jobj "id" "c1" "name" "t" "input_json" "hi there!"))
         '(:tool-call "c1" "t" "\"hi there!\""))
   (list "a payload whose length is not a multiple of 4 is not base64"
         (cf-tool-call-part (jobj "id" "c1" "name" "t" "input_json" "eyJh IjoxfQ=="))
         '(:tool-call "c1" "t" "\"eyJh IjoxfQ==\""))
   (list "whitespace-padded base64 is not base64"
         ;; 16 characters, so this one clears the length test and cl-base64 is
         ;; what rejects it. Without :whitespace :error it would decode to
         ;; {"a":1} and the padding would vanish from the transform.
         (cf-tool-call-part (jobj "id" "c1" "name" "t" "input_json" "  eyJhIjoxfQ==  "))
         '(:tool-call "c1" "t" "\"  eyJhIjoxfQ==  \""))
   (list "a multi-byte payload survives the decode"
         ;; base64 of {"t":"h\u00e9llo"} in UTF-8. base64-string-to-string maps
         ;; each byte through code-char and would produce two mojibake chars.
         (cf-tool-call-part (jobj "id" "c1" "name" "t" "input_json" "eyJ0IjoiaMOpbGxvIn0="))
         (list :tool-call "c1" "t" (format nil "{\"t\":\"h~cllo\"}" (code-char 233))))
   (list "base64 of bytes that are not UTF-8 still yields a JSON string"
         ;; base64 of #xFF #xFE. Go keeps the bytes in a string; the fallback
         ;; here maps each one through code-char rather than dropping the part.
         (cf-tool-call-part (jobj "id" "c1" "name" "t" "input_json" "//4="))
         (list :tool-call "c1" "t"
               (jzon:stringify (map 'string #'code-char #(255 254)))))))

(defun cf-transformed-tool-names (input)
  (mapcar (lambda (tool) (getf tool :name)) (hook-input-tools input)))

(defun run-hooks-conformance-tests ()
  (with-test-suite ("Conformance: hooks")
    ;; --- Request bodies ---
    (let* ((expected (cf-fixture "hooks/request-preflight.json"))
           (actual (cf-serialize-hook-request :preflight
                                              (cf-hooks-preflight-context)
                                              (cf-hooks-preflight-input)))
           (diffs (cf-json-diff actual expected)))
      (check (cf-label "preflight request body matches the fixture" diffs)
             (null diffs)))

    (let* ((expected (cf-fixture "hooks/request-postflight-guard.json"))
           (actual (cf-serialize-hook-request :postflight
                                              (cf-hooks-postflight-context)
                                              (cf-hooks-postflight-input)))
           (diffs (cf-json-diff actual expected)))
      (check (cf-label "postflight guard request body matches the fixture" diffs)
             (null diffs)))

    ;; --- Canned responses ---
    (let* ((responses (cf-fixture "hooks/responses.json"))
           (allow (agento11y-cl::%parse-response (jget responses "allow")))
           (deny (agento11y-cl::%parse-response (jget responses "deny")))
           (transformed (agento11y-cl::%parse-response
                         (jget responses "allow_with_transformed_input"))))
      (check "allow response parses to :allow" (eq (response-action allow) :allow))
      (let ((eval (first (response-evaluations allow))))
        (check "allow response carries its evaluation"
               (and eval
                    (equal (evaluation-rule-id eval) "pii-detect")
                    (equal (evaluation-evaluator-id eval) "evaluator-pii")
                    (equal (evaluation-evaluator-kind eval) "regex")
                    (eq (evaluation-passed eval) t)
                    (= (evaluation-latency-ms eval) 12)
                    (equal (evaluation-explanation eval) "no PII matches"))))

      (check "deny response parses to :deny" (eq (response-action deny) :deny))
      (check "deny response carries rule and reason"
             (and (equal (response-rule-id deny) "block-destructive-bash")
                  (equal (response-reason deny)
                         "Bash(*rm*) is not allowed in this environment")))
      (check "deny evaluation carries reason, not explanation"
             (let ((eval (first (response-evaluations deny))))
               (and eval
                    (null (evaluation-passed eval))
                    (equal (evaluation-reason eval) "blocked tool Bash"))))

      (let ((input (response-transformed-input transformed)))
        (check "transformed_input parses" (not (null input)))
        (when input
          (check "transformed system prompt"
                 (equal (hook-input-system-prompt input) "You are a careful assistant."))
          (check "transformed conversation preview"
                 (equal (hook-input-conversation-preview input)
                        "user: Delete the cache directory under [REDACTED]."))
          (check "transformed input holds three messages"
                 (= (length (hook-input-messages input)) 3))
          (let* ((assistant (second (hook-input-messages input)))
                 (parts (and assistant (message-parts assistant))))
            (check "transformed assistant keeps thinking and the tool call"
                   (and (= (length parts) 2)
                        (typep (first parts) 'thinking-part)
                        (typep (second parts) 'tool-call-part)))
            (check "transformed tool call arguments decode"
                   (let ((call (find-if (lambda (p) (typep p 'tool-call-part)) parts)))
                     (and call
                          (equal (tool-call-part-name call) "Bash")
                          (equal (tool-call-part-input-json call)
                                 "{\"command\":\"rm -rf /tmp/cache\"}")))))
          (let* ((tool-msg (third (hook-input-messages input)))
                 (result (first (and tool-msg (message-parts tool-msg)))))
            (check "transformed tool result content decodes"
                   (and (typep result 'tool-result-part)
                        (equal (agento11y-cl::tool-result-part-content-json result)
                               "{\"exit_code\":1}")
                        (eq (and (tool-result-part-is-error result) t) t))))
          (check "transformed tools parse"
                 (equal (cf-transformed-tool-names input) '("Bash")))
          (check "transformed tools round-trip to the fixture encoding"
                 (let ((diffs (cf-json-diff
                               (jzon:parse (jzon:stringify
                                            (coerce (mapcar #'agento11y-cl::%serialize-tool
                                                            (hook-input-tools input))
                                                    'vector)))
                               (jget* responses "allow_with_transformed_input"
                                      "transformed_input" "tools"))))
                   (null diffs))))))

    ;; --- Part dispatch ---
    (dolist (case +cf-part-dispatch-cases+)
      (destructuring-bind (label part expected) case
        (let ((actual (cf-part-shape (cf-parse-part part))))
          (check (if (equal actual expected)
                     label
                     (format nil "~a -- got ~s" label actual))
                 (equal actual expected)))))

    ;; --- Role collapse ---
    (check "the system role collapses to user"
           (eq (message-role (agento11y-cl::%parse-wire-message
                              (jobj "role" "system"
                                    "parts" (vector (jobj "kind" "text" "text" "hi")))))
               :user))))

;;; ================================================================
;;; Experiments conformance
;;; ================================================================

(defparameter +cf-experiment-host+ "https://experiments.example")

(defun cf-experiment-requests ()
  "requests.json with ${SDK_ID} replaced by this SDK's source id."
  (jzon:parse (agento11y-cl::%replace-all (cf-fixture-text "experiments/requests.json")
                                          "${SDK_ID}"
                                          agento11y-cl::+experiment-run-source-id+)))

(defun cf-experiment-client (calls &key (body "{}") (status 200))
  "A client whose HTTP layer answers BODY and records every call onto the cdr
of CALLS as (method url content), newest first."
  (make-client
   (make-config
    :generation-endpoint (concatenate 'string +cf-experiment-host+
                                      "/api/v1/generations:export")
    :eval-path-prefix "/api/v1"
    :scores-export-path "/api/v1/scores:export"
    :experimental-features t
    :max-retries 1
    :http-fn (lambda (url &key method headers content &allow-other-keys)
               (declare (ignore headers))
               (push (list method url content) (cdr calls))
               (values body status)))
   :env-fn (constantly nil)))

(defun cf-last-call (calls)
  (first (cdr calls)))

(defun cf-call-method (call)
  (string-upcase (symbol-name (first call))))

(defun cf-call-path (call)
  (let ((url (second call)))
    (if (eql 0 (search +cf-experiment-host+ url))
        (subseq url (length +cf-experiment-host+))
        url)))

(defun cf-call-body (call)
  (let ((content (third call)))
    (if content (jzon:parse content) 'cl:null)))

(defun cf-check-request (check fixtures name thunk &key (body "{}"))
  "Drive THUNK against a capturing client and check the one call it makes
against the NAME entry of requests.json."
  (let* ((calls (list :calls))
         (client (cf-experiment-client calls :body body))
         (expected (jget fixtures name)))
    (funcall thunk client)
    (let ((call (cf-last-call calls)))
      (if (null call)
          (funcall check (format nil "~a: issues one request" name) nil)
          (let ((diffs (append
                        (unless (string= (cf-call-method call) (jget expected "method"))
                          (list (format nil "method ~a, want ~a"
                                        (cf-call-method call) (jget expected "method"))))
                        (unless (string= (cf-call-path call) (jget expected "path"))
                          (list (format nil "path ~a, want ~a"
                                        (cf-call-path call) (jget expected "path"))))
                        (cf-json-diff (cf-call-body call) (jget expected "body") "$.body"))))
            (funcall check (cf-label name diffs) (null diffs)))))))

(defun cf-experiment-inputs ()
  "inputs.json: the run, case, evaluator, conversation and score identity every
SDK's experiment suite feeds in. Reading them from the fixture is what makes an
upstream change to an input show up as one readable failure."
  (cf-fixture "experiments/inputs.json"))

(defun cf-trial-id (inputs)
  "The trial id the pinned inputs mint. requests.json carries it in every trial
path, so a drift in the mint fails those cases rather than this one."
  (trial-mint-id (jget inputs "experiment_id")
                 (jget inputs "test_case_id")
                 (jget inputs "attempt")))

(defun cf-score-item (inputs)
  "The one score requests.json pins, built off the fixture inputs. Its id is
the same stable-id the other SDKs derive; the verdict fields are not pinned
upstream either."
  (let ((run-id (jget inputs "experiment_id"))
        (trial-id (cf-trial-id inputs)))
    (list :score-id (stable-id "score" run-id trial-id
                               (jget inputs "score_key")
                               (jget inputs "score_evaluator_id"))
          :evaluator-id (jget inputs "score_evaluator_id")
          :evaluator-version (jget inputs "score_evaluator_version")
          :score-key (jget inputs "score_key")
          :value t
          :conversation-id (jget inputs "conversation_id")
          :experiment-id run-id
          :trial-id trial-id
          :test-case-id (jget inputs "test_case_id")
          :passed t
          :explanation "matched the expected answer"
          :created-at (jget inputs "score_created_at")
          :source (jobj "kind" "experiment" "id" run-id))))

(defun cf-stable-id-part (value)
  "One ids.json part as STABLE-ID takes it. JSON null keeps its separator slot,
which is what NIL does in %JOIN-STABLE-ID-PARTS."
  (if (eq value 'cl:null) nil value))

(defun run-experiments-conformance-tests ()
  (with-test-suite ("Conformance: experiments")
    ;; --- stable_id vectors ---
    (let ((vectors (jget (cf-fixture "experiments/ids.json") "vectors")))
      (loop for vector across vectors
            for expected = (jget vector "id")
            for actual = (apply #'stable-id (jget vector "prefix")
                                (map 'list #'cf-stable-id-part (jget vector "parts")))
            do (check (if (equal actual expected)
                          (format nil "stable_id ~a" expected)
                          (format nil "stable_id ~a -- got ~a" expected actual))
                      (equal actual expected))))

    ;; --- Request shapes ---
    (let* ((inputs (cf-experiment-inputs))
           (run-id (jget inputs "experiment_id"))
           (trial-id (cf-trial-id inputs))
           (evaluator-id (jget inputs "evaluator_id"))
           (conversation-id (jget inputs "conversation_id"))
           (fixtures (cf-experiment-requests))
           (responses (cf-fixture "experiments/responses.json")))
      (cf-check-request #'check fixtures "run_upsert"
                        (lambda (client)
                          (upsert-experiment-run client
                                                 :experiment-id run-id
                                                 :name (jget inputs "experiment_name")
                                                 :tags (coerce (jget inputs "tags") 'list)
                                                 :suite-id (jget inputs "suite_id")
                                                 :suite-version (jget inputs "suite_version")
                                                 :planned-trial-count
                                                 (jget inputs "planned_trial_count"))))
      (cf-check-request #'check fixtures "trial_create"
                        (lambda (client)
                          (create-trial client run-id
                                        :trial-id trial-id
                                        :test-case-id (jget inputs "test_case_id")
                                        :attempt (jget inputs "attempt")
                                        :status "running")))
      (cf-check-request #'check fixtures "trial_patch_conversation"
                        (lambda (client)
                          (finalize-trial client run-id trial-id
                                          :conversation-id conversation-id)))
      (cf-check-request #'check fixtures "trial_patch_terminal"
                        (lambda (client)
                          (finalize-trial client run-id trial-id
                                          :status "completed"
                                          :conversation-id conversation-id)))
      (let ((queued (jzon:stringify (jget responses "evaluation_queued"))))
        (cf-check-request #'check fixtures "trial_evaluate"
                          (lambda (client)
                            (trigger-trial-evaluation client run-id trial-id
                                                      :evaluator-id evaluator-id
                                                      :evaluator-version
                                                      (jget inputs "evaluator_version")))
                          :body queued)
        (cf-check-request #'check fixtures "trial_evaluate_latest_version"
                          (lambda (client)
                            (trigger-trial-evaluation client run-id trial-id
                                                      :evaluator-id evaluator-id))
                          :body queued)
        (cf-check-request #'check fixtures "trial_evaluate_reserved_trial_id"
                          (lambda (client)
                            (trigger-trial-evaluation client run-id
                                                      "trial:one/blue"
                                                      :evaluator-id evaluator-id))
                          :body queued)
        (cf-check-request #'check fixtures "trial_evaluation_status"
                          (lambda (client)
                            (get-trial-evaluation client run-id trial-id
                                                  (jget inputs "evaluation_id")))
                          :body queued))
      (cf-check-request #'check fixtures "scores_export"
                        (lambda (client) (export-scores client (list (cf-score-item inputs))))
                        :body (jzon:stringify (jget responses "scores_export_response")))
      (cf-check-request #'check fixtures "run_finalize"
                        (lambda (client)
                          (finalize-experiment-run client run-id :status "completed")))
      (cf-check-request #'check fixtures "run_finalize_with_score_count"
                        (lambda (client)
                          (finalize-experiment-run client run-id
                                                   :status "completed"
                                                   :score-count 1))))

    ;; --- Response parsing ---
    (let* ((inputs (cf-experiment-inputs))
           (run-id (jget inputs "experiment_id"))
           (trial-id (cf-trial-id inputs))
           (responses (cf-fixture "experiments/responses.json")))
      (flet ((evaluation-call (name)
               (let* ((calls (list :calls))
                      (client (cf-experiment-client
                               calls :body (jzon:stringify (jget responses name)))))
                 (lambda ()
                   (get-trial-evaluation client run-id trial-id
                                         (jget inputs "evaluation_id"))))))
        (check "a queued evaluation parses and is not terminal"
               (let ((evaluation (funcall (evaluation-call "evaluation_queued"))))
                 (and (equal (jget evaluation "status") "queued")
                      (not (trial-evaluation-terminal-p evaluation)))))
        (check "a claimed evaluation parses and is not terminal"
               (let ((evaluation (funcall (evaluation-call "evaluation_claimed"))))
                 (and (equal (jget evaluation "status") "claimed")
                      (not (trial-evaluation-terminal-p evaluation)))))
        (check "a successful evaluation is terminal"
               (trial-evaluation-terminal-p (funcall (evaluation-call "evaluation_success"))))
        (check "a failed evaluation is terminal"
               (trial-evaluation-terminal-p (funcall (evaluation-call "evaluation_failed"))))
        (check "an unsupported status fails the call"
               (cf-signals-p (evaluation-call "evaluation_unsupported_status")))
        (check "a missing evaluation_id fails the call"
               (cf-signals-p (evaluation-call "evaluation_missing_id"))))

      (let* ((calls (list :calls))
             (client (cf-experiment-client
                      calls :body (jzon:stringify (jget responses "run_upsert_response"))))
             (parsed (upsert-experiment-run client :experiment-id run-id
                                                   :name (jget inputs "experiment_name"))))
        (check "the upsert response keys the run under run"
               (equal (jget* parsed "run" "status") "running")))

      (let* ((calls (list :calls))
             (client (cf-experiment-client
                      calls :body (jzon:stringify (jget responses "run_finalize_response"))))
             (parsed (finalize-experiment-run client run-id)))
        (check "the finalize response keys the run under run"
               (equal (jget* parsed "run" "status") "completed")))

      (flet ((report (name)
               (let* ((calls (list :calls))
                      (client (cf-experiment-client
                               calls :body (jzon:stringify (jget responses name)))))
                 (get-experiment-report client run-id))))
        (let ((experiment-envelope (report "report_experiment_envelope"))
              (run-envelope (report "report_run_envelope")))
          (flet ((same-key-p (key)
                   ;; A key absent from both envelopes would diff clean, so the
                   ;; check would pass on a report that carries neither.
                   (and (nth-value 1 (gethash key experiment-envelope))
                        (nth-value 1 (gethash key run-envelope))
                        (null (cf-json-diff (jget experiment-envelope key)
                                            (jget run-envelope key))))))
            (check "a report keyed under experiment reads back under run"
                   (equal (jget* experiment-envelope "run" "experiment_id") run-id))
            (check "a report keyed under run reads back under run"
                   (equal (jget* run-envelope "run" "experiment_id") run-id))
            (check "both report envelopes carry the same run" (same-key-p "run"))
            (check "both report envelopes carry the same summary" (same-key-p "summary"))
            (check "both report envelopes carry the same rows" (same-key-p "rows")))))

      (let* ((calls (list :calls))
             (client (cf-experiment-client
                      calls :body (jzon:stringify (jget responses "scores_export_response")))))
        (check "the score export response reports one accepted score"
               (= (export-scores client (list (cf-score-item inputs))) 1))))))

;;; ================================================================
;;; Redaction conformance
;;; ================================================================

;;; The redaction engine and the content-capture rules are behind the fixtures,
;;; and a follow-up ticket owns catching them up. RUN-TESTS returns the exit
;;; code the Makefile and CI read, so a suite that fails by design would leave
;;; every later change reading "29 failed" and no way to see a 30th. Each known
;;; gap is listed by check id instead: a listed check that fails is reported as
;;; a known gap and passes the build, anything else fails it, and a listed check
;;; that starts passing fails too, so the list cannot outlive the gap.

(defparameter +cf-known-redaction-failures+
  '(;; The call error is not scanned at all.
    "generations/input-redaction-disabled/callError"
    "generations/input-redaction-enabled/callError"
    "generations/email-redaction-disabled/callError"
    ;; tool_result.content_json in the payload is derived from content when
    ;; content parses as JSON, so the part's own content-json never reaches it.
    "generations/input-redaction-disabled/input.tool.toolResultContentJson"
    "generations/input-redaction-disabled/output.tool.toolResultContentJson"
    "generations/input-redaction-enabled/input.tool.toolResultContentJson"
    "generations/input-redaction-enabled/output.tool.toolResultContentJson"
    "generations/email-redaction-disabled/input.tool.toolResultContentJson"
    "generations/email-redaction-disabled/output.tool.toolResultContentJson"
    ;; With input redaction off, the fixture still expects light on assistant
    ;; input messages and full on tool input messages; the SDK redacts neither.
    "generations/input-redaction-disabled/input.assistant.text"
    "generations/input-redaction-disabled/input.assistant.thinking"
    "generations/input-redaction-disabled/input.assistant.toolCallInputJson"
    "generations/input-redaction-disabled/input.tool.text"
    "generations/input-redaction-disabled/input.tool.toolResultContent")
  "Ids of the redaction conformance checks the SDK is known to fail.")

(defun cf-redaction-check (check id label passed)
  "Report one redaction check, reading ID against the known-failure list."
  (cond
    ((not (member id +cf-known-redaction-failures+ :test #'string=))
     (funcall check label passed))
    (passed
     (funcall check
              (format nil "~a -- now passes, drop ~s from +cf-known-redaction-failures+"
                      label id)
              nil))
    (t (funcall check (format nil "known gap: ~a" label) t))))

(defun cf-run-redaction-strings-cases (check)
  (let ((cases (jget (cf-fixture "redaction/strings.json") "cases")))
    (loop for case across cases
          for id = (jget case "id")
          for full-p = (string= (jget case "mode") "full")
          for redactor = (agento11y-cl::make-secret-redactor
                          :include-emails (and (jget case "emails") t))
          for actual = (if full-p
                           (agento11y-cl::redact-full redactor (jget case "input"))
                           (agento11y-cl::redact-light redactor (jget case "input")))
          for expected = (jget case "expected")
          do (cf-redaction-check
              check (format nil "strings/~a" id)
              (if (equal actual expected)
                  id
                  (format nil "~a -- got ~s, want ~s" id actual expected))
              (equal actual expected)))))

(defun cf-base64-text (value)
  "Decode a base64 payload out of a generation payload, or NIL."
  (when (and (stringp value) (plusp (length value)))
    (handler-case
        (babel:octets-to-string (cl-base64:base64-string-to-usb8-array value)
                                :encoding :utf-8)
      (error () nil))))

(defun cf-redaction-generation-payload (case probe-input)
  "Build the generation payload for one generations.json case.
Every content slot carries the same probe string, so the exported payload shows
which mode the SDK redacted each slot under."
  (let* ((config (make-config
                  :generation-endpoint "https://redaction.example/api/v1/generations:export"
                  :content-capture-mode :full
                  :redact-secrets t
                  :redact-input-messages (and (jget case "redactInputMessages") t)
                  :redact-email-addresses (and (jget case "redactEmailAddresses") t)))
         (client (make-client config :env-fn (constantly nil)))
         (messages (list (make-message :role :user
                                       :parts (list (make-text-part probe-input)))
                         (make-message :role :assistant
                                       :parts (list (make-text-part probe-input)
                                                    (make-thinking-part probe-input)
                                                    (make-tool-call-part
                                                     :id "call-1" :name "Bash"
                                                     :input-json probe-input)))
                         (make-message :role :tool
                                       :parts (list (make-text-part probe-input)
                                                    (make-tool-result-part
                                                     :tool-call-id "call-1"
                                                     :content probe-input
                                                     :content-json probe-input)))))
         (rec (start-generation client :mode :sync
                                       :model-provider "anthropic" :model-name "claude"
                                       :conversation-title probe-input
                                       :system-prompt probe-input
                                       :input-messages messages)))
    (set-result rec :output-messages messages)
    (set-call-error rec probe-input)
    (agento11y-cl::build-generation-payload rec config)))

(defparameter +cf-redaction-slot-readers+
  ;; One reader per generations.json slot name. A reader returns the exported
  ;; text for that slot, or NIL when the payload has no such field.
  (list
   (cons "systemPrompt" (lambda (gen) (jget gen "system_prompt")))
   (cons "conversationTitle"
         (lambda (gen) (jget* gen "metadata" "agento11y.conversation.title")))
   (cons "callError" (lambda (gen) (jget gen "call_error")))
   (cons "input.user.text" (lambda (gen) (jget* gen "input" 0 "parts" 0 "text")))
   (cons "input.assistant.text" (lambda (gen) (jget* gen "input" 1 "parts" 0 "text")))
   (cons "input.assistant.thinking" (lambda (gen) (jget* gen "input" 1 "parts" 1 "thinking")))
   (cons "input.assistant.toolCallInputJson"
         (lambda (gen) (cf-base64-text (jget* gen "input" 1 "parts" 2 "tool_call" "input_json"))))
   (cons "input.tool.text" (lambda (gen) (jget* gen "input" 2 "parts" 0 "text")))
   (cons "input.tool.toolResultContent"
         (lambda (gen) (jget* gen "input" 2 "parts" 1 "tool_result" "content")))
   (cons "input.tool.toolResultContentJson"
         (lambda (gen) (cf-base64-text (jget* gen "input" 2 "parts" 1 "tool_result" "content_json"))))
   (cons "output.assistant.text" (lambda (gen) (jget* gen "output" 1 "parts" 0 "text")))
   (cons "output.assistant.thinking" (lambda (gen) (jget* gen "output" 1 "parts" 1 "thinking")))
   (cons "output.assistant.toolCallInputJson"
         (lambda (gen) (cf-base64-text (jget* gen "output" 1 "parts" 2 "tool_call" "input_json"))))
   (cons "output.tool.text" (lambda (gen) (jget* gen "output" 2 "parts" 0 "text")))
   (cons "output.tool.toolResultContent"
         (lambda (gen) (jget* gen "output" 2 "parts" 1 "tool_result" "content")))
   (cons "output.tool.toolResultContentJson"
         (lambda (gen) (cf-base64-text (jget* gen "output" 2 "parts" 1 "tool_result" "content_json"))))))

(defun cf-run-redaction-generations-cases (check)
  (let* ((fixture (cf-fixture "redaction/generations.json"))
         (probe (jget fixture "probe"))
         (probe-input (jget probe "input")))
    (loop for case across (jget fixture "cases")
          for case-id = (jget case "id")
          for slots = (jget case "slots")
          for payload = (cf-redaction-generation-payload case probe-input)
          do (let ((known nil))
               (maphash (lambda (slot mode)
                          (push slot known)
                          (let* ((reader (cdr (assoc slot +cf-redaction-slot-readers+
                                                     :test #'string=)))
                                 (expected (jget probe mode))
                                 (actual (and reader (funcall reader payload)))
                                 (label (format nil "~a ~a (~a)" case-id slot mode)))
                            (cf-redaction-check
                             check (format nil "generations/~a/~a" case-id slot)
                             (if (equal actual expected)
                                 label
                                 (format nil "~a -- got ~s" label actual))
                             (equal actual expected))))
                        slots)
               ;; Upstream asks every harness to assert that the slots it builds
               ;; are exactly the slots listed, so neither side can grow a slot
               ;; the other does not know about.
               (funcall check
                        (format nil "~a reads exactly the listed slots" case-id)
                        (null (set-exclusive-or
                               (mapcar #'car +cf-redaction-slot-readers+) known
                               :test #'string=)))))))

(defun cf-lisp-pattern-ids (table)
  "The pattern ids in one Lisp pattern table. A table is either a single entry
whose first element is the id, or a list of such entries."
  (if (stringp (car table))
      (list (car table))
      (mapcar #'car table)))

(defun cf-upstream-tier-count (upstream tier)
  (count tier upstream :key (lambda (pattern) (jget pattern "tier")) :test #'equal))

(defun cf-run-redaction-patterns-drift (check)
  "Check every pattern id in patterns.json against the Lisp tables: same id,
same tier, and the same number of patterns per tier, so a pattern dropped
upstream does not keep matching here under an id no other SDK writes.

Regex text is deliberately not compared: cl-ppcre supports lookbehind and Go
RE2 does not, so the two spellings differ by design."
  (let* ((upstream (jget (cf-fixture "redaction/patterns.json") "patterns"))
         (tier1 (cf-lisp-pattern-ids
                 (agento11y-cl::tier1-patterns agento11y-cl::+tier1+)))
         (tier2 (cf-lisp-pattern-ids agento11y-cl::+tier2-patterns+))
         (email (cf-lisp-pattern-ids agento11y-cl::+email-pattern+)))
    (loop for pattern across upstream
          for id = (jget pattern "id")
          for tier = (jget pattern "tier")
          do (cf-redaction-check
              check (format nil "patterns/~a" id)
              (format nil "pattern ~a (tier ~a) has a Lisp counterpart" id tier)
              (and (member id (cond ((equal tier "email") email)
                                    ((equal tier 1) tier1)
                                    ((equal tier 2) tier2))
                           :test #'string=)
                   t)))
    (loop for (key tier ids) in (list (list "tier1" 1 tier1)
                                      (list "tier2" 2 tier2)
                                      (list "email" "email" email))
          for want = (cf-upstream-tier-count upstream tier)
          do (cf-redaction-check
              check (format nil "patterns/~a-count" key)
              (format nil "~d ~a pattern(s) upstream (have ~d)" want key (length ids))
              (= (length ids) want)))))

(defun run-redaction-conformance-tests ()
  (with-test-suite ("Conformance: redaction")
    (cf-run-redaction-strings-cases #'check)
    (cf-run-redaction-generations-cases #'check)
    (cf-run-redaction-patterns-drift #'check)))

;;; ================================================================
;;; GenAI-semconv wire conformance
;;;
;;; The fixtures in t/fixtures/otlpwire/ pin the generation span wire format.
;;; Each generation.json is fed through the otel export path and the resulting
;;; span is compared against its span.json counterpart: name, start and end
;;; exactly, attributes as an unordered set.
;;;
;;; Unlike every other fixture this repo vendors, these come from a Go-private
;;; testdata/ directory rather than agento11y's shared conformance/ contract.
;;; t/fixtures/README.md says what that costs.
;;; ================================================================

(defparameter +cf-genai-fixtures+ '("openai_sync" "anthropic_stream" "gemini_sync"))

(defun cf-genai-render-value (value)
  "Render an OTLP attribute value as \"type:value\", so a type mismatch shows up
as a value mismatch instead of passing silently. It reads both the camelCase
keys this SDK writes and the snake_case keys protojson writes into a fixture."
  (flet ((slot (camel snake)
           (multiple-value-bind (found found-p) (gethash camel value)
             (if found-p (values found t) (gethash snake value)))))
    (multiple-value-bind (text found) (slot "stringValue" "string_value")
      (when found (return-from cf-genai-render-value (format nil "string:~a" text))))
    (multiple-value-bind (flag found) (slot "boolValue" "bool_value")
      (when found
        (return-from cf-genai-render-value
          (format nil "bool:~a" (if (and flag (not (eq flag :false))) "true" "false")))))
    (multiple-value-bind (number found) (slot "intValue" "int_value")
      (when found (return-from cf-genai-render-value (format nil "int64:~a" number))))
    (multiple-value-bind (number found) (slot "doubleValue" "double_value")
      (when found (return-from cf-genai-render-value (format nil "double:~a" number))))
    (multiple-value-bind (array found) (slot "arrayValue" "array_value")
      (when found
        (return-from cf-genai-render-value
          (format nil "stringslice:~{~s~^,~}"
                  (map 'list (lambda (item)
                               (multiple-value-bind (text found-p)
                                   (gethash "stringValue" item)
                                 (if found-p text (gethash "string_value" item))))
                       (gethash "values" array))))))
    (format nil "unknown:~a" (jzon:stringify value))))

(defun cf-genai-attribute-map (attributes)
  "An OTLP attribute vector as a key -> rendered-value hash table."
  (let ((out (make-hash-table :test 'equal)))
    (map nil (lambda (attr)
               (setf (gethash (jget attr "key") out)
                     (cf-genai-render-value (jget attr "value"))))
         attributes)
    out))

(defun cf-genai-metadata-superset-diffs (actual expected)
  "Differences that keep the emitted metadata document from covering the
fixture's. The SDK stamps its own keys on every export path, so the fixture's
entries are a subset rather than the whole document."
  (flet ((decode (rendered)
           (let ((payload (subseq rendered (length "string:"))))
             (handler-case (jzon:parse payload) (error () nil)))))
    (let ((got (decode actual))
          (want (decode expected))
          (diffs nil))
      (cond
        ((null want) (list "fixture metadata is not a JSON object"))
        ((null got) (list "emitted metadata is not a JSON object"))
        (t (maphash (lambda (key value)
                      (multiple-value-bind (found found-p) (gethash key got)
                        (cond
                          ((not found-p) (push (format nil "metadata is missing ~s" key) diffs))
                          ((not (equal (jzon:stringify found) (jzon:stringify value)))
                           (push (format nil "metadata[~s] = ~a, want ~a" key
                                         (jzon:stringify found) (jzon:stringify value))
                                 diffs)))))
                    want)
           (nreverse diffs))))))

(defun cf-genai-expected-attributes (fixture)
  "The fixture's attributes, with the carve-outs the otel path needs applied."
  (let ((expected (cf-genai-attribute-map (jget fixture "attributes"))))
    ;; 1. otel mode reports the conventions' operation, so the fixture's
    ;;    proprietary generateText/streamText spelling cannot match. It is the
    ;;    backend decoder's name, not what this mode emits.
    (when (nth-value 1 (gethash "gen_ai.operation.name" expected))
      (setf (gethash "gen_ai.operation.name" expected) "string:chat"))
    ;; 2. agento11y.record is what makes the backend store a generation. This
    ;;    SDK adds it; the fixtures do not carry it.
    (setf (gethash "agento11y.record" expected) "string:true")
    ;; 3. agento11y.agent.effective_version has no source in this SDK, which
    ;;    has no effective-version concept. Leaving it out beats inventing one.
    (remhash "agento11y.agent.effective_version" expected)
    expected))

(defun cf-genai-span-diffs (name)
  "Differences between the span this SDK builds for a fixture generation and
the fixture span beside it."
  (let* ((generation (cf-fixture (format nil "otlpwire/~a.generation.json" name)))
         (fixture (cf-fixture (format nil "otlpwire/~a.span.json" name)))
         (span (agento11y-cl::build-genai-span (agento11y-cl::genai-invocation-from-generation generation :full)))
         (expected (cf-genai-expected-attributes fixture))
         (actual (cf-genai-attribute-map (jget span "attributes")))
         (want-name (format nil "chat ~a" (jget* generation "model" "name")))
         (diffs nil))
    (unless (equal (jget span "name") want-name)
      (push (format nil "name = ~s, want ~s" (jget span "name") want-name) diffs))
    (unless (equal (jget span "startTimeUnixNano") (jget fixture "start_time_unix_nano"))
      (push (format nil "start = ~s, want ~s" (jget span "startTimeUnixNano")
                    (jget fixture "start_time_unix_nano"))
            diffs))
    (unless (equal (jget span "endTimeUnixNano") (jget fixture "end_time_unix_nano"))
      (push (format nil "end = ~s, want ~s" (jget span "endTimeUnixNano")
                    (jget fixture "end_time_unix_nano"))
            diffs))
    (maphash (lambda (key want)
               (multiple-value-bind (got found-p) (gethash key actual)
                 (cond
                   ((not found-p) (push (format nil "missing attribute ~a" key) diffs))
                   ;; 4. The SDK stamps its own metadata keys on every export
                   ;;    path, so this one is a superset check.
                   ((string= key "agento11y.generation.metadata")
                    (setf diffs (append (reverse (cf-genai-metadata-superset-diffs got want))
                                        diffs)))
                   ((not (string= got want))
                    (push (format nil "attribute ~a = ~a, want ~a" key got want) diffs)))))
             expected)
    ;; The metadata attribute is allowed even when the fixture has none: the
    ;; SDK's own keys are enough to produce one.
    (maphash (lambda (key got)
               (declare (ignore got))
               (unless (or (nth-value 1 (gethash key expected))
                           (string= key "agento11y.generation.metadata"))
                 (push (format nil "unexpected attribute ~a" key) diffs)))
             actual)
    (nreverse diffs)))

(defun run-genai-conformance-tests ()
  (with-test-suite ("GenAI wire conformance (t/fixtures/otlpwire)")
    (dolist (name +cf-genai-fixtures+)
      (let ((diffs (cf-genai-span-diffs name)))
        (check (cf-label (format nil "~a span matches the fixture" name) diffs)
               (null diffs))))
    ;; The conventions leave the status unset on success; the native span path
    ;; writes code 1. This is the one shape the fixtures cannot pin, because a
    ;; protojson span with no status renders no status key either way.
    (let ((span (agento11y-cl::build-genai-span
                 (agento11y-cl::genai-invocation-from-generation
                  (cf-fixture "otlpwire/openai_sync.generation.json") :full))))
      (check "a successful generation span carries no status"
             (not (nth-value 1 (gethash "status" span)))))))
