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
                        metadata flush-fn closed-fn)
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
                               :closed-fn closed-fn)))
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
