(in-package :agento11y-cl)

(defun do-http-post (config url headers body)
  "POST body to url. Uses config's http-fn if set, otherwise dexador."
  (let ((http-fn (config-http-fn config)))
    (if http-fn
        (funcall http-fn url :headers headers :content body)
        (dexador:post url
                      :headers headers
                      :content body
                      :force-string t
                      :connect-timeout (config-export-timeout-sec config)
                      :read-timeout (config-export-timeout-sec config)))))

(defun post-with-retry (config url body auth-headers label count)
  "POST BODY to URL with exponential backoff retry.
Retries on 5xx/connection errors, fails immediately on 4xx.
Returns (values success-p response-body) on 2xx, NIL on failure."
  (declare (ignore count))
  (let ((headers (append (list (cons "Content-Type" "application/json"))
                         auth-headers)))
    (loop for attempt from 0 below (config-max-retries config)
          do (handler-case
                 (multiple-value-bind (resp-body status)
                     (do-http-post config url headers body)
                   (cond
                     ((<= 200 status 299)
                      (return (values t resp-body)))
                     ((<= 400 status 499)
                      (agento11y-log config :warn "exporter"
                                (format nil "~a rejected with ~d" label status))
                      (return nil))
                     (t
                      (agento11y-log config :warn "exporter"
                                (format nil "~a failed (~d), retrying ~d/~d"
                                        label status (1+ attempt) (config-max-retries config)))
                      (sleep (backoff-seconds attempt
                                             (config-initial-backoff-sec config)
                                             (config-max-backoff-sec config))))))
               (error (e)
                 (agento11y-log config :warn "exporter"
                           (format nil "~a error: ~a, retrying ~d/~d"
                                   label (princ-to-string e)
                                   (1+ attempt) (config-max-retries config)))
                 (sleep (backoff-seconds attempt
                                        (config-initial-backoff-sec config)
                                        (config-max-backoff-sec config)))))
          finally (progn
                    (agento11y-log config :warn "exporter"
                              (format nil "~a failed after ~d retries"
                                      label (config-max-retries config)))
                    (return nil)))))

(defun log-rejected-generations (config resp-body)
  "Parse export response and log individually rejected generations."
  (handler-case
      (when (and resp-body (stringp resp-body) (plusp (length resp-body)))
        (let ((parsed (jzon:parse resp-body)))
          (when (hash-table-p parsed)
            (let ((errors (jget parsed "errors")))
              (when (and errors (vectorp errors))
                (loop for err across errors
                      when (hash-table-p err)
                      do (agento11y-log config :warn "exporter"
                                   (format nil "generation rejected: ~a"
                                           (or (jget err "message")
                                               (jget err "error")
                                               "unknown")))))))))
    (error () nil)))

(defun export-generations (config generations auth-headers)
  "POST a batch of generations to the generation endpoint."
  (when (null generations) (return-from export-generations t))
  (let* ((url (config-generation-endpoint config))
         (payload (jzon:stringify (jobj "generations" (coerce generations 'vector)))))
    (multiple-value-bind (ok resp-body)
        (post-with-retry config url payload auth-headers "generations" (length generations))
      (when ok
        (log-rejected-generations config resp-body))
      ok)))

(defun export-traces (config spans auth-headers)
  "POST spans to the OTLP traces endpoint."
  (when (null spans) (return-from export-traces t))
  (let* ((url (config-traces-endpoint config))
         (payload (jzon:stringify (build-otlp-payload spans
                                                      (config-service-name config)
                                                      (config-service-version config)))))
    (post-with-retry config url payload auth-headers "traces" (length spans))))

(defun export-workflow-steps (config workflow-steps auth-headers)
  "POST a batch of workflow steps to the workflow steps endpoint."
  (when (null workflow-steps) (return-from export-workflow-steps t))
  (let* ((url (config-workflow-steps-endpoint config))
         (payload (jzon:stringify (jobj "workflow_steps"
                                        (coerce workflow-steps 'vector)))))
    (post-with-retry config url payload auth-headers
                     "workflow-steps" (length workflow-steps))))
