(in-package :sigil-cl)

;;; Typed trials.
;;;
;;; A trial is one attempt at one test case inside an experiment run. Typed
;;; trials populate the structured experiment report; scores anchor to them, so
;;; a run does not need a generation for every score.
;;;
;;;   POST  /api/v1/experiment-runs/{id}/trials
;;;   PATCH /api/v1/experiment-runs/{id}/trials/{trial_id}
;;;
;;; This file holds the trial value and its transport. The score buffer is
;;; filled and drained by experiment.lisp, which loads later and owns score
;;; serialization; a trial reaches back into it through the FLUSH-FN closure
;;; installed when the trial is opened.

;;; --- Value ---

(defclass experiment-trial ()
  ((client :initarg :client :reader trial-client)
   ;; The owning EXPERIMENT-RUN, or NIL for a standalone trial.
   (run :initarg :run :initform nil :reader trial-run)
   (experiment-id :initarg :experiment-id :reader trial-experiment-id)
   (trial-id :initarg :trial-id :reader trial-id)
   (test-case-id :initarg :test-case-id :reader trial-test-case-id)
   (attempt :initarg :attempt :initform 1 :reader trial-attempt)
   ;; Local lifecycle state. A caller's own vocabulary ("passed", "errored",
   ;; anything else) collapses to the two values the wire accepts.
   (status :initform "running" :accessor trial-status)
   (error-text :initform nil :accessor trial-error-text)
   ;; Correlation, all bound locally without issuing a request.
   (conversation-id :initform nil :accessor trial-conversation-id)
   (generation-id :initform nil :accessor trial-generation-id)
   (trace-id :initform nil :accessor trial-trace-id)
   (span-id :initform nil :accessor trial-span-id)
   ;; Scores recorded on this trial but not yet exported.
   (buffer :initform nil :accessor trial-buffer)
   (created-p :initform nil :accessor trial-created-p)
   (closed-p :initform nil :accessor trial-closed-p)
   ;; Called as (funcall flush-fn trial) before the closing PATCH.
   (flush-fn :initarg :flush-fn :initform nil :reader trial-flush-fn)
   ;; Called as (funcall closed-fn trial) after a successful close.
   (closed-fn :initarg :closed-fn :initform nil :reader trial-closed-fn)
   ;; Called as (funcall cloud-evaluated-fn) after a cloud evaluation is
   ;; queued, so the run stops asserting a score count the backend will grow.
   (cloud-evaluated-fn :initarg :cloud-evaluated-fn :initform nil
                       :reader trial-cloud-evaluated-fn)
   (lock :initform (bt2:make-lock :name "sigil-experiment-trial")
         :reader trial-lock)))

(defun trial-mint-id (experiment-id test-case-id attempt)
  "The deterministic id of one test-case attempt.
Matches stable_id(\"trial\", ...) in agento11y experiments/experiment.py:207,
so a rerun from any SDK addresses the same trial."
  (stable-id "trial" experiment-id test-case-id attempt))

;;; --- Transport ---

(defun create-trial (client experiment-id &key trial-id test-case-id (attempt 1)
                                            (status "running") conversation-id
                                            trace-id span-id test-case metadata)
  "POST a typed trial under EXPERIMENT-ID. Idempotent on TRIAL-ID."
  (when (%blank-string-p trial-id)
    (error 'sigil-validation-error :message "trial validation failed: trial_id is required"))
  (when (%blank-string-p test-case-id)
    (error 'sigil-validation-error :message "trial validation failed: test_case_id is required"))
  (let* ((config (client-config client))
         (payload (jobj "trial_id" (%trimmed-text trial-id)
                        "test_case_id" (%trimmed-text test-case-id)
                        "attempt" attempt
                        "status" (%trimmed-text status))))
    (unless (%blank-string-p conversation-id)
      (setf (gethash "conversation_id" payload) (%trimmed-text conversation-id)))
    (unless (%blank-string-p trace-id)
      (setf (gethash "trace_id" payload) (%trimmed-text trace-id)))
    (unless (%blank-string-p span-id)
      (setf (gethash "span_id" payload) (%trimmed-text span-id)))
    (when test-case
      (setf (gethash "test_case" payload) test-case))
    (when metadata
      (let ((json (%jsonify metadata)))
        (when (and (hash-table-p json) (plusp (hash-table-count json)))
          (setf (gethash "metadata" payload) json))))
    (request-eval-json config :post
                       (%experiment-run-url config experiment-id "/trials")
                       payload "trial create")))

(defun finalize-trial (client experiment-id trial-id
                       &key status ((:error error-message)) cost input-tokens
                            output-tokens duration-ms conversation-id trace-id span-id)
  "PATCH a typed trial. The body is sparse: only supplied fields are sent."
  (when (%blank-string-p trial-id)
    (error 'sigil-validation-error :message "trial validation failed: trial_id is required"))
  (let* ((config (client-config client))
         (payload (jobj)))
    (unless (%blank-string-p status)
      (setf (gethash "status" payload) (%trimmed-text status)))
    (unless (%blank-string-p error-message)
      (setf (gethash "error" payload) (%text error-message)))
    (when cost (setf (gethash "cost" payload) cost))
    (when input-tokens (setf (gethash "input_tokens" payload) input-tokens))
    (when output-tokens (setf (gethash "output_tokens" payload) output-tokens))
    (when duration-ms (setf (gethash "duration_ms" payload) duration-ms))
    (unless (%blank-string-p conversation-id)
      (setf (gethash "conversation_id" payload) (%trimmed-text conversation-id)))
    (unless (%blank-string-p trace-id)
      (setf (gethash "trace_id" payload) (%trimmed-text trace-id)))
    (unless (%blank-string-p span-id)
      (setf (gethash "span_id" payload) (%trimmed-text span-id)))
    (request-eval-json config :patch
                       (%experiment-run-url
                        config experiment-id
                        (format nil "/trials/~a" (%url-encode trial-id)))
                       payload "trial update")))

;;; --- Cloud trial evaluation (experimental) ---
;;;
;;; A stored tenant evaluator grades the conversation the backend already has,
;;; instead of a score this process computes. The evaluator writes that score
;;; itself, so a run that queued one stops asserting its local score count.

(defun %validate-trial-evaluation (response)
  "Reject an evaluation response the SDK cannot act on. An unknown status would
read as non-terminal and poll to the caller's deadline; a blank evaluation_id
would turn the next status read into a validation error blaming the caller."
  (let ((status (%trimmed-text (jget response "status"))))
    (unless (member status +trial-evaluation-statuses+ :test #'string=)
      (error 'sigil-export-error
             :message (format nil "unsupported evaluation status ~s" status)))
    (when (%blank-string-p (jget response "evaluation_id"))
      (error 'sigil-export-error
             :message "evaluation response carries no evaluation_id")))
  response)

(defun trial-evaluation-terminal-p (evaluation)
  "Whether EVALUATION reached a status the worker will not move off again."
  (and (member (%trimmed-text (jget evaluation "status"))
               +trial-evaluation-terminal-statuses+ :test #'string=)
       t))

(defun trigger-trial-evaluation (client experiment-id trial-id
                                 &key evaluator-id evaluator-version)
  "Queue a stored tenant evaluator against the trial's bound conversation.
The evaluation is keyed by trial, conversation, evaluator, and resolved
version, so triggering the same combination returns the existing row instead
of grading twice. Experimental: signals SIGIL-EXPERIMENTAL-DISABLED-ERROR
without sending a request unless the gate is on."
  (let ((config (client-config client)))
    (%require-experimental config "cloud trial evaluation")
    (when (%blank-string-p trial-id)
      (error 'sigil-validation-error
             :message "trial validation failed: trial_id is required"))
    (when (%blank-string-p evaluator-id)
      (error 'sigil-validation-error
             :message "trial validation failed: evaluator_id is required"))
    (let ((payload (jobj "evaluator_id" (%trimmed-text evaluator-id))))
      (unless (%blank-string-p evaluator-version)
        (setf (gethash "evaluator_version" payload) (%trimmed-text evaluator-version)))
      (%validate-trial-evaluation
       (request-eval-json config :post
                          (%experiment-run-url
                           config experiment-id
                           (format nil "/trials/~a:evaluate" (%url-encode trial-id)))
                          payload "trial evaluation trigger")))))

(defun get-trial-evaluation (client experiment-id trial-id evaluation-id)
  "Read durable status for a triggered trial evaluation. Experimental."
  (let ((config (client-config client)))
    (%require-experimental config "cloud trial evaluation")
    (when (%blank-string-p trial-id)
      (error 'sigil-validation-error
             :message "trial validation failed: trial_id is required"))
    (when (%blank-string-p evaluation-id)
      (error 'sigil-validation-error
             :message "trial validation failed: evaluation_id is required"))
    (%validate-trial-evaluation
     (request-eval-json config :get
                        (%experiment-run-url
                         config experiment-id
                         (format nil "/trials/~a/evaluations/~a"
                                 (%url-encode trial-id)
                                 (%url-encode evaluation-id)))
                        nil "trial evaluation status"))))

(defun %evaluation-deadline (timeout-sec)
  (+ (get-internal-real-time)
     (round (* timeout-sec internal-time-units-per-second))))

(defun %evaluation-remaining-sec (deadline)
  (/ (- deadline (get-internal-real-time)) internal-time-units-per-second))

(defun trial-evaluate (trial evaluator-id
                       &key evaluator-version
                            (timeout-sec +default-evaluation-timeout-sec+)
                            (poll-interval-sec +default-evaluation-poll-interval-sec+))
  "Grade this trial's bound conversation with a stored tenant evaluator.

Persists the local conversation binding, flushes pending generations, queues
the evaluation, then blocks until it reaches a terminal status. Returns the
evaluation on success; signals SIGIL-TRIAL-EVALUATION-FAILED-ERROR when the
worker fails and SIGIL-TRIAL-EVALUATION-TIMEOUT-ERROR when TIMEOUT-SEC runs
out. Both conditions carry the evaluation id, and the evaluation keeps running
server-side either way.

Queuing an evaluation makes the owning run omit `score_count` when it
finalizes. A trial opened outside a run cannot mark anything, so that caller
must pass :score-count NIL to EXPERIMENT-RUN-FINALIZE itself.

TIMEOUT-SEC and POLL-INTERVAL-SEC are taken literally, unlike Go, where zero
means the default. A zero timeout still queues the evaluation, but the wait
signals SIGIL-TRIAL-EVALUATION-TIMEOUT-ERROR unless the trigger already
reported a terminal status; a caller that only wants to queue the evaluation
uses TRIGGER-TRIAL-EVALUATION. A zero interval reads status back to back. Use
both in tests, not to make a real wait faster.

Experimental: signals SIGIL-EXPERIMENTAL-DISABLED-ERROR without sending a
request unless the gate is on."
  (let* ((client (trial-client trial))
         (config (client-config client))
         (experiment-id (trial-experiment-id trial))
         (id (trial-id trial)))
    ;; Checked before anything is persisted or flushed, so a blocked call
    ;; leaves nothing behind.
    (%require-experimental config "cloud trial evaluation")
    (when (%blank-string-p evaluator-id)
      (error 'sigil-validation-error
             :message "trial validation failed: evaluator_id is required"))
    (when (minusp timeout-sec)
      (error 'sigil-validation-error
             :message "trial validation failed: evaluation timeout must not be negative"))
    (when (minusp poll-interval-sec)
      (error 'sigil-validation-error
             :message "trial validation failed: evaluation poll interval must not be negative"))
    (when (%blank-string-p (trial-conversation-id trial))
      (error 'sigil-validation-error
             :message "trial validation failed: bind a conversation before evaluating a trial"))
    ;; TRIAL-BIND-CONVERSATION is local until now, and the backend refuses an
    ;; evaluation for a trial with no stored conversation.
    (finalize-trial client experiment-id id
                    :conversation-id (trial-conversation-id trial))
    ;; The evaluator reads the stored conversation, so its generation has to
    ;; exist before the wait starts, not when the trial closes.
    (client-flush client)
    (let ((deadline (%evaluation-deadline timeout-sec))
          (evaluation (trigger-trial-evaluation client experiment-id id
                                                :evaluator-id evaluator-id
                                                :evaluator-version evaluator-version))
          (interval poll-interval-sec)
          (max-interval (max poll-interval-sec +max-evaluation-poll-interval-sec+)))
      ;; The row exists from here on and its score counts toward the run's
      ;; stored total whether or not this wait sees it finish. Marking only on
      ;; success would leave a timed-out wait asserting a stale count.
      (let ((mark-fn (trial-cloud-evaluated-fn trial)))
        (when mark-fn (funcall mark-fn)))
      (loop
        (let ((status (%trimmed-text (jget evaluation "status"))))
          (cond
            ((string= status "success")
             (return evaluation))
            ((string= status "failed")
             (let ((evaluation-id (%trimmed-text (jget evaluation "evaluation_id")))
                   (detail (%trimmed-text (jget evaluation "error"))))
               (error 'sigil-trial-evaluation-failed-error
                      :evaluation-id evaluation-id
                      :detail (when (plusp (length detail)) detail)
                      :message (format nil "trial evaluation failed~@[: ~a~]"
                                       (when (plusp (length detail)) detail)))))))
        (let ((remaining (%evaluation-remaining-sec deadline)))
          ;; The deadline is checked here and nowhere else. Every sleep is
          ;; followed by a status read, including the one clamped to the
          ;; remaining budget, so an evaluation that finishes inside the last
          ;; window is not reported as a timeout.
          (when (<= remaining 0)
            (error 'sigil-trial-evaluation-timeout-error
                   :evaluation-id (%trimmed-text (jget evaluation "evaluation_id"))
                   :message (format nil "trial evaluation did not finish within ~a second(s)"
                                    timeout-sec)))
          (sleep (min interval remaining))
          (setf interval (min (* interval 2) max-interval))
          (setf evaluation
                (get-trial-evaluation client experiment-id id
                                      (%trimmed-text (jget evaluation "evaluation_id")))))))))

;;; --- Trial artifacts ---

(defun %artifact-kind-from-mime (mime)
  "Map a MIME type onto a Sigil artifact kind.
Follows _kind_from_mime in agento11y experiments/experiment.py:129-145, whose
`text/x-markdown` handling is the superset of Go's."
  (let ((m (string-downcase (%trimmed-text mime))))
    (cond
      ((eql 0 (search "image/" m)) "image")
      ((string= m "application/json") "json")
      ((member m '("text/markdown" "text/x-markdown") :test #'string=) "markdown")
      ((string= m "application/pdf") "pdf")
      ((string= m "text/csv") "csv")
      ((eql 0 (search "text/" m)) "text")
      (t "binary"))))

(defparameter +artifact-mime-by-extension+
  '(("json" . "application/json")
    ("md" . "text/markdown")
    ("markdown" . "text/markdown")
    ("csv" . "text/csv")
    ("txt" . "text/plain")
    ("log" . "text/plain")
    ("html" . "text/html")
    ("htm" . "text/html")
    ("yaml" . "text/yaml")
    ("yml" . "text/yaml")
    ("pdf" . "application/pdf")
    ("png" . "image/png")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif" . "image/gif")
    ("webp" . "image/webp")
    ("svg" . "image/svg+xml"))
  "MIME type per file extension for artifacts read from a path.
Hardcoded rather than looked up with trivial-mimes, which dexador already
pulls in: that library prefers the host's /etc/mime.types, so the same file
would upload with a different Content-Type from a Linux CI box than from a
laptop. Anything not listed uploads as application/octet-stream.")

(defun %artifact-mime-from-path (path)
  (let ((type (pathname-type (pathname path))))
    (or (and (stringp type)
             (cdr (assoc (string-downcase type) +artifact-mime-by-extension+
                         :test #'string=)))
        "application/octet-stream")))

(defun upload-trial-artifact (client experiment-id trial-id
                              &key name kind mime content)
  "POST raw CONTENT as an artifact of TRIAL-ID. Returns the artifact record.

The body is the bytes themselves; all metadata rides the query string, in the
order name, kind, mime (Python's spelling; Go sorts the same three keys).
CONTENT is an octet vector or a string, and a string is encoded as UTF-8.
Content-Type comes from the trimmed MIME, or application/octet-stream when it
is blank."
  (let ((config (client-config client)))
    (dolist (check (list (cons trial-id "trial_id")
                         (cons name "name")
                         (cons kind "kind")))
      (when (%blank-string-p (car check))
        (error 'sigil-validation-error
               :message (format nil "artifact validation failed: ~a is required"
                                (cdr check)))))
    (let ((bytes (if (stringp content) (string-to-utf8-octets content) content)))
      (when (or (null bytes) (zerop (length bytes)))
        (error 'sigil-validation-error
               :message "artifact validation failed: content is required"))
      (request-eval-bytes-json
       config :post
       (%experiment-run-url
        config experiment-id
        (format nil "/trials/~a/artifacts:upload?~a"
                (%url-encode trial-id)
                ;; %QUERY-ENCODE skips only NIL values, so a blank MIME still
                ;; emits `mime=` and the backend sees the key it expects.
                (%query-encode (list (cons "name" (%trimmed-text name))
                                     (cons "kind" (%trimmed-text kind))
                                     (cons "mime" (%trimmed-text mime))))))
       bytes (%trimmed-text mime) "trial artifact upload"))))

(defun trial-artifact (trial &key name kind mime content text path)
  "Attach an artifact to TRIAL: raw bytes, text, or a file.

Supply exactly one of CONTENT, TEXT, or PATH. KIND is inferred from the MIME
type when unset, and the MIME type is inferred from the file extension for
PATH and defaults to text/plain for TEXT.

Unlike the Go and Python SDKs, this SDK does not redact text-like artifacts:
it has no secret sanitizer yet. Content uploads exactly as supplied, so a
caller porting code that relied on the default redaction has to strip secrets
itself."
  (let ((sources (count-if-not #'null (list content text path))))
    (unless (= sources 1)
      (error 'sigil-validation-error
             :message "artifact validation failed: supply exactly one of content, text, or path")))
  (multiple-value-bind (bytes resolved-mime)
      (cond
        (path (let ((resolved (if (%blank-string-p mime)
                                  (%artifact-mime-from-path path)
                                  (%trimmed-text mime))))
                (values (alex:read-file-into-byte-vector path) resolved)))
        (text (values (string-to-utf8-octets (%text text))
                      (if (%blank-string-p mime) "text/plain" (%trimmed-text mime))))
        (t (values (if (stringp content) (string-to-utf8-octets content) content)
                   (%trimmed-text mime))))
    (upload-trial-artifact (trial-client trial)
                           (trial-experiment-id trial)
                           (trial-id trial)
                           :name name
                           :kind (if (%blank-string-p kind)
                                     (%artifact-kind-from-mime resolved-mime)
                                     kind)
                           :mime resolved-mime
                           :content bytes)))

;;; --- Lifecycle ---

(defun trial-wire-status (trial)
  "Collapse the local status onto the two values the backend accepts.
Only an errored trial is \"failed\": a run that could not execute the case.
A locally failed assertion executed fine, so it closes \"completed\" and the
verdict rides on the final score's `passed`. Matches _wire_status in
agento11y experiments/experiment.py:312."
  (if (equal (trial-status trial) "errored")
      "failed"
      "completed"))

(defun trial-open (client experiment-id test-case-id
                   &key (attempt 1) run trial-id test-case suite-id suite-version
                        metadata flush-fn closed-fn cloud-evaluated-fn)
  "Build a trial and create it on the backend. Returns the EXPERIMENT-TRIAL.
TRIAL-ID defaults to the deterministic mint for (EXPERIMENT-ID, TEST-CASE-ID,
ATTEMPT); a caller that already minted it passes it so both agree."
  (let* ((normalized-case-id (%trimmed-text test-case-id))
         (trial (make-instance 'experiment-trial
                               :client client
                               :run run
                               :experiment-id (%trimmed-text experiment-id)
                               :trial-id (or trial-id
                                             (trial-mint-id experiment-id
                                                            normalized-case-id
                                                            attempt))
                               :test-case-id normalized-case-id
                               :attempt attempt
                               :flush-fn flush-fn
                               :closed-fn closed-fn
                               :cloud-evaluated-fn cloud-evaluated-fn)))
    (create-trial client experiment-id
                  :trial-id (trial-id trial)
                  :test-case-id normalized-case-id
                  :attempt attempt
                  :status "running"
                  :test-case (test-case-snapshot test-case
                                                 :suite-id suite-id
                                                 :suite-version suite-version)
                  :metadata metadata)
    (setf (trial-created-p trial) t)
    trial))

(defun trial-bind-generation (trial generation-id)
  "Record the generation this trial produced. Local only; issues no request."
  (let ((id (%trimmed-text generation-id)))
    (when (plusp (length id))
      (setf (trial-generation-id trial) id)))
  trial)

(defun trial-bind-conversation (trial conversation-id &key trace-id span-id)
  "Record the conversation this trial ran in. Local only; issues no request."
  (let ((id (%trimmed-text conversation-id)))
    (when (plusp (length id))
      (setf (trial-conversation-id trial) id)))
  (let ((tid (%trimmed-text trace-id)))
    (when (plusp (length tid))
      (setf (trial-trace-id trial) tid)))
  (let ((sid (%trimmed-text span-id)))
    (when (plusp (length sid))
      (setf (trial-span-id trial) sid)))
  trial)

(defun trial-close (trial &key ((:error error-message)) status)
  "Flush this trial's scores, then PATCH it terminal. Later calls are ignored.

The close is claimed under TRIAL-LOCK before any work starts, so a worker
thread and run teardown racing to close the same trial cannot both flush and
both PATCH, which would double-count the run's accepted scores. A close that
signals releases the claim again, so the caller or run teardown can retry it."
  (bt2:with-lock-held ((trial-lock trial))
    (when (trial-closed-p trial)
      (return-from trial-close trial))
    (setf (trial-closed-p trial) t))
  (let ((closed-p nil))
    (unwind-protect
         (progn
           (unless (%blank-string-p status)
             (setf (trial-status trial) (%trimmed-text status)))
           (unless (%blank-string-p error-message)
             (setf (trial-status trial) "errored"
                   (trial-error-text trial) (%text error-message)))
           ;; Scores go out before the PATCH so the run's terminal state never
           ;; claims scores the backend has not stored yet.
           (let ((flush-fn (trial-flush-fn trial)))
             (when flush-fn
               (funcall flush-fn trial)))
           (when (trial-created-p trial)
             (finalize-trial (trial-client trial)
                             (trial-experiment-id trial)
                             (trial-id trial)
                             :status (trial-wire-status trial)
                             :error (trial-error-text trial)
                             :conversation-id (trial-conversation-id trial)
                             :trace-id (trial-trace-id trial)
                             :span-id (trial-span-id trial)))
           (setf closed-p t))
      (unless closed-p
        (bt2:with-lock-held ((trial-lock trial))
          (setf (trial-closed-p trial) nil)))))
  (let ((closed-fn (trial-closed-fn trial)))
    (when closed-fn
      (funcall closed-fn trial)))
  trial)

(defmacro with-trial ((trial run test-case &rest options) &body body)
  "Open a trial on RUN for TEST-CASE, bind it, and close it on exit.
TEST-CASE is a TEST-CASE value or a test-case id. On normal exit the trial
closes completed; on an error it closes failed carrying the error text, and
the error propagates."
  `(%call-with-trial ,run ,test-case
                     (lambda (,trial)
                       ,@body)
                     ,@options))
