(in-package :agento11y-cl)

;;; Diagnostic spans use the trace queue. REST remains the storage protocol.

(defstruct (%experiment-otel-state (:constructor %make-experiment-otel-state))
  context start name attributes
  (events nil)
  verdict
  (score-ids (make-hash-table :test 'equal))
  (ended-p nil)
  (lock (bt2:make-lock :name "agento11y-experiment-otel")))

(defun %experiment-otel-safely (thunk)
  "Telemetry must not change application results or replace an escaping error."
  (handler-case (funcall thunk) (serious-condition () nil)))

(defun %experiment-otel-context (state)
  (or (%experiment-otel-safely
       (lambda () (when state (%experiment-otel-state-context state))))
      *trace-context*))

(defun %experiment-otel-active-p (run)
  (and run (experiment-run-use-experimental-otel run)
       (config-traces-enabled (client-config (experiment-run-client run)))))

(defun %experiment-otel-text (config mode text)
  "Withhold private text before secret scanning."
  (when (and text (capture-keeps-span-content-p mode))
    (let ((text (%text text)))
      (if (config-redact-secrets config)
          (apply-secret-redaction
           (make-secret-redactor :include-emails (config-redact-email-addresses config))
           :full text)
          text))))

(defun %experiment-otel-strings (pairs)
  (loop for (key . value) in pairs
        unless (%blank-string-p value)
          collect (otel-string-attr key (%text value))))

(defun %experiment-otel-identity (run &optional trial test-case)
  (let ((suite (experiment-run-suite run))
        (candidate (experiment-run-candidate run)))
    (append
     (list (otel-string-attr "agento11y.eval.schema.version" "experiments-otel-2026-06")
           ;; The receiver must not materialize diagnostic GenAI spans as generations.
           (otel-bool-attr "agento11y.record" nil))
     (%experiment-otel-strings
      (list (cons "test.suite.run.id" (experiment-run-run-id run))
            (cons "test.suite.name"
                  (if (and suite (not (%blank-string-p (test-suite-name suite))))
                      (test-suite-name suite) (experiment-run-name run)))
            (cons "test.suite.id" (when suite (test-suite-suite-id suite)))
            (cons "test.suite.version" (when suite (test-suite-version suite)))
            (cons "gen_ai.agent.name" (or (%metadata-field candidate "agent_name")
                                                (experiment-run-agent-name run)))
            (cons "gen_ai.agent.version" (or (%metadata-field candidate "agent_version")
                                                   (experiment-run-agent-version run)))
            (cons "gen_ai.provider.name" (%metadata-field candidate "model_provider"))
            (cons "gen_ai.request.model" (%metadata-field candidate "model_name"))))
     (when trial
       (append
        (%experiment-otel-strings
         (list (cons "gen_ai.operation.name" "invoke_agent")
               (cons "test.suite.run.status" "in_progress")
               (cons "test.case.id" (trial-test-case-id trial))
               (cons "test.case.name"
                     (if (and test-case (not (%blank-string-p (test-case-name test-case))))
                         (test-case-name test-case) (trial-test-case-id trial)))
               (cons "test.case.run.id" (trial-id trial))))
        (list (otel-int-attr "test.case.run.attempt" (trial-attempt trial))))))))

(defun %new-experiment-otel-state (run name attributes &optional parent)
  (let* ((config (client-config (experiment-run-client run)))
         (parent (or parent *trace-context*))
         (mode (resolve-content-capture-mode
                config :parent-mode (getf parent :content-capture-mode)))
         (trace-id (getf parent :trace-id))
         (span-id (getf parent :span-id))
         (context (child-trace-context
                   (if (trace-hex-id-p trace-id 32) (string-downcase trace-id)
                       (generate-trace-id))
                   (generate-span-id)
                   :parent-span-id (when (and (trace-hex-id-p trace-id 32)
                                              (trace-hex-id-p span-id 16))
                                     (string-downcase span-id))
                   :trace-flags (getf parent :trace-flags 1)
                   :trace-state (getf parent :trace-state)
                   :content-capture-mode mode)))
    (%make-experiment-otel-state :context context :start (current-unix-nano)
                                :name name :attributes attributes)))

(defun %finish-experiment-otel (client state attributes error-text &key links)
  (when state
    (bt2:with-lock-held ((%experiment-otel-state-lock state))
      (unless (%experiment-otel-state-ended-p state)
        ;; Claim before serialization: a failed diagnostic export is not retried by close.
        (setf (%experiment-otel-state-ended-p state) t)
        (when (config-traces-enabled (client-config client))
          (let* ((context (%experiment-otel-state-context state))
                 (safe-error (%experiment-otel-text
                              (client-config client) (getf context :content-capture-mode)
                              error-text)))
            (queue-enqueue
             (client-trace-queue client)
             (build-span
              :trace-id (getf context :trace-id) :span-id (getf context :span-id)
              :parent-span-id (getf context :parent-span-id)
              :trace-flags (getf context :trace-flags 1)
              :trace-state (getf context :trace-state)
              :name (%experiment-otel-state-name state) :kind 1
              :scope-name "agento11y.experiments"
              :start-time-unix-nano (%experiment-otel-state-start state)
              :end-time-unix-nano (current-unix-nano)
              :attributes (coerce (append (%experiment-otel-state-attributes state)
                                           attributes) 'vector)
              :events (coerce (reverse (%experiment-otel-state-events state)) 'vector)
              :links links
              :status-code (if error-text 2 :unset)
              :status-message (or safe-error (when error-text "experiment failed") "")))))))
    (bt2:with-lock-held ((client-lock client))
      (bt2:condition-notify (client-wake-cv client)))))

(defun %call-with-suite-otel (run thunk)
  (let* ((state (%experiment-otel-safely
                 (lambda ()
                   (when (%experiment-otel-active-p run)
                     (%new-experiment-otel-state run "test_suite_run"
                                                 (%experiment-otel-identity run))))))
         (completed-p nil)
         (error-text nil))
    (setf (experiment-run-otel-state run) state)
    (unwind-protect
         (let ((*trace-context* (%experiment-otel-context state)))
           (handler-bind ((serious-condition (lambda (e)
                                  (setf error-text
                                        (or (%experiment-otel-safely
                                             (lambda () (condition-status-message e)))
                                            "experiment failed")))))
             (multiple-value-prog1 (funcall thunk) (setf completed-p t))))
      (%experiment-otel-safely
       (lambda ()
         (let* ((status (experiment-run-status run))
                (failed-p (or (not completed-p) (equal status "failed"))))
           (%finish-experiment-otel
            (experiment-run-client run) state
            (list (otel-string-attr "test.suite.run.status"
                                    (cond (failed-p "failure")
                                          ((equal status "running") "in_progress")
                                          (t "success"))))
            (when failed-p (or error-text "experiment failed")))))))))

(defun %start-trial-otel (trial test-case)
  (let ((run (trial-run trial)))
    (when (%experiment-otel-active-p run)
      (let* ((suite-state (experiment-run-otel-state run))
             (parent (if (and suite-state (not (getf *trace-context* :span-id)))
                         (let* ((suite-context (%experiment-otel-state-context suite-state))
                                (merged (copy-list suite-context)))
                           (loop for (key value) on *trace-context* by #'cddr
                                 unless (member key '(:trace-id :span-id :parent-span-id))
                                   do (when (or (not (eq key :content-capture-mode))
                                                (and value (not (eq value :default))))
                                        (setf (getf merged key) value)))
                           merged)
                         *trace-context*))
             (state (%new-experiment-otel-state
                     run (format nil "eval.trial ~a" (trial-test-case-id trial))
                     (%experiment-otel-identity run trial test-case) parent))
             (context (%experiment-otel-state-context state)))
        (setf (trial-otel-state trial) state
              (trial-trace-id trial) (getf context :trace-id)
              (trial-span-id trial) (getf context :span-id))))))

(defun %trial-otel-score-event (trial item)
  (let* ((state (trial-otel-state trial))
         (run (trial-run trial))
         (dataset (experiment-run-dataset run))
         (metadata (jget item "metadata"))
         (value (jget item "value"))
         (attrs (%experiment-otel-strings
                 (list
                  (cons "gen_ai.evaluation.name" (jget item "score_key"))
                  (cons "gen_ai.evaluation.evaluator.id" (jget item "evaluator_id"))
                  (cons "gen_ai.evaluation.evaluator.version" (jget item "evaluator_version"))
                  (cons "gen_ai.evaluation.evaluator.type" (jget item "evaluator_kind"))
                  (cons "gen_ai.response.id" (jget item "generation_id"))
                  (cons "gen_ai.evaluation.reference_set.id"
                        (or (%metadata-field metadata "reference_set_id")
                            (%metadata-field dataset "id")))
                  (cons "gen_ai.evaluation.reference_set.version"
                        (or (%metadata-field metadata "reference_set_version")
                            (%metadata-field dataset "version")))
                  (cons "gen_ai.evaluation.explanation"
                        (%experiment-otel-text
                         (client-config (trial-client trial))
                         (getf (%experiment-otel-state-context state) :content-capture-mode)
                         (jget item "explanation")))))))
    (multiple-value-bind (number number-p) (gethash "number" value)
      (multiple-value-bind (boolean boolean-p) (gethash "bool" value)
        (when (or number-p boolean-p)
          (push (otel-double-attr "gen_ai.evaluation.score.value"
                                  (if number-p number (if boolean 1d0 0d0))) attrs))))
    (multiple-value-bind (passed passed-p) (gethash "passed" item)
      (let ((label (if passed-p (if passed "pass" "fail") (jget value "string"))))
        (unless (%blank-string-p label)
          (push (otel-string-attr "gen_ai.evaluation.score.label" label) attrs))))
    (jobj "name" "gen_ai.evaluation.result" "timeUnixNano" (current-unix-nano)
          "attributes" (coerce (nreverse attrs) 'vector))))

(defun %trial-otel-add-scores (trial items)
  ;; The caller holds TRIAL-LOCK, so close cannot race this batch of events.
  (let ((state (trial-otel-state trial)))
    (when state
      (bt2:with-lock-held ((%experiment-otel-state-lock state))
        (unless (%experiment-otel-state-ended-p state)
          (dolist (item items)
            (let ((id (jget item "score_id")))
              (unless (gethash id (%experiment-otel-state-score-ids state))
                (let ((event (%trial-otel-score-event trial item)))
                  (setf (gethash id (%experiment-otel-state-score-ids state)) t)
                  (when (equal (jget item "score_key") "final")
                    (multiple-value-bind (passed passed-p) (gethash "passed" item)
                      (multiple-value-bind (boolean boolean-p) (gethash "bool" (jget item "value"))
                        (when (or passed-p boolean-p)
                          (setf (%experiment-otel-state-verdict state)
                                (if (if passed-p passed boolean) "pass" "fail"))))))
                  (push event (%experiment-otel-state-events state)))))))))))

(defun %end-trial-otel (trial &optional close-error)
  (let ((attrs (%experiment-otel-strings
                (list (cons "gen_ai.conversation.id" (trial-conversation-id trial))
                      (cons "test.case.result.status"
                            (cond ((equal (trial-status trial) "passed") "pass")
                                  ((equal (trial-status trial) "failed") "fail")
                                  ((equal (trial-status trial) "errored") nil)
                                  ((trial-otel-state trial)
                                   (%experiment-otel-state-verdict (trial-otel-state trial)))))))))
    (when (trial-input-tokens trial)
      (push (otel-int-attr "gen_ai.usage.input_tokens" (trial-input-tokens trial)) attrs))
    (when (trial-output-tokens trial)
      (push (otel-int-attr "gen_ai.usage.output_tokens" (trial-output-tokens trial)) attrs))
    (%finish-experiment-otel
     (trial-client trial) (trial-otel-state trial) attrs
     (or (trial-error-text trial) close-error)
     :links (list (list :trace-id (trial-conversation-trace-id trial)
                        :span-id (trial-conversation-span-id trial))))))
