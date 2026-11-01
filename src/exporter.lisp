(in-package :agento11y-cl)

(defvar *export-deadline* nil)

(defun do-http-post (config url headers body)
  "POST BODY with a cooperative SBCL deadline for native Dexador I/O.
Custom HTTP functions own their deadlines. Other Lisp/backends reject native requests.
SBCL deadlines cover blocking I/O and lock waits, not foreign DNS or CPU work."
  (let* ((http-fn (config-http-fn config))
         (remaining (deadline-remaining *export-deadline*))
         (timeout (if remaining
                      (min remaining (config-export-timeout-sec config))
                      (config-export-timeout-sec config))))
    (if http-fn
        (funcall http-fn url :headers headers :content body)
        #+(and sbcl (not windows) (not win32))
        ;; A pooled socket retains its old read timeout. One deadline also bounds
        ;; repeated reads, connection establishment, and connection-pool locks.
        ;; WITH-DEADLINE keeps an earlier caller deadline unless OVERRIDE is true.
        (if (eq dexador:*dexador-backend* :usocket)
            (sb-sys:with-deadline (:seconds timeout)
              (sleep 0)
              (multiple-value-prog1
                  ;; The POST wrapper does not accept MAX-REDIRECTS.
                  (dexador:request url :method :post :headers headers :content body
                                       :force-string t :max-redirects 0
                                       :connect-timeout timeout :read-timeout timeout)
                (sleep 0)))
            (error 'agento11y-export-error
                   :message "Native HTTP deadlines require Dexador's USOCKET backend"
                   :diagnostics (list :kind :unsupported-deadline)))
        #-(and sbcl (not windows) (not win32))
        (error 'agento11y-export-error
               :message "Native HTTP deadlines require SBCL's USOCKET backend; configure :http-fn on this Lisp/backend."
               :diagnostics (list :kind :unsupported-deadline)))))

(defun response-header (headers name)
  (let ((value
          (cond ((hash-table-p headers)
                 (loop for key being the hash-keys of headers using (hash-value value)
                       when (string-equal key name) return value))
                ((listp headers)
                 (cdr (assoc name headers :test #'string-equal))))))
    (if (listp value) (first value) value)))

(defun retry-after-seconds (headers)
  "Read Retry-After delay-seconds or an IMF-fixdate. Ignore invalid values."
  (let ((value (response-header headers "Retry-After")))
    (when (stringp value)
      (let ((text (string-trim '(#\Space #\Tab) value)))
        (or (when (and (plusp (length text)) (every #'digit-char-p text))
              (parse-integer text))
            (ignore-errors
              (let* ((parts (cl-ppcre:split "[ ,:]+" text))
                     (month (position (third parts)
                                      '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                                      :test #'string-equal)))
                (when (and (= (length parts) 8) month
                           (string-equal (eighth parts) "GMT"))
                  (max 0 (- (encode-universal-time
                             (parse-integer (seventh parts))
                             (parse-integer (sixth parts))
                             (parse-integer (fifth parts))
                             (parse-integer (second parts)) (1+ month)
                             (parse-integer (fourth parts)) 0)
                            (get-universal-time)))))))))))

(defun retryable-export-status-p (status label)
  (if (member label '("traces" "metrics") :test #'string=)
      (member status '(429 502 503 504))
      (or (= status 429) (<= 500 status 599))))

(defun post-with-retry (config url body auth-headers label count)
  "Return (values success-p response-body diagnostics).
MAX-RETRIES retains its historical meaning: total attempts, with at least one.
Diagnostics contain no endpoint, credentials, response text, or condition text."
  (let ((headers (append (list (cons "Content-Type" "application/json")) auth-headers))
        (attempts (max 1 (config-max-retries config))))
    (labels ((failure (kind attempt &optional status)
               (let ((diagnostics (list :kind kind :signal label :count count
                                        :attempts attempt :status-code status)))
                 (agento11y-log config :warn "exporter"
                                (format nil "~a export failed (~a, status ~a, attempts ~d)"
                                        label kind (or status "none") attempt))
                 (values nil nil diagnostics))))
      (loop for attempt from 1 to attempts
            do (when (and *export-deadline*
                          (zerop (deadline-remaining *export-deadline*)))
                 (return (failure :timeout (1- attempt))))
               (multiple-value-bind (resp-body status response-headers transport-error)
                   (handler-case
                       (do-http-post config url headers body)
                     (dexador.error:http-request-failed (e)
                       (values nil (dexador.error:response-status e)
                               (ignore-errors (dexador.error:response-headers e))))
                     ;; SBCL deadline timeouts are SERIOUS-CONDITIONs, not ERRORs.
                     ;; Do not retry: the native request or caller budget is spent.
                     #+sbcl
                     (sb-ext:timeout () (return (failure :timeout attempt)))
                     #+(and sbcl (not windows) (not win32))
                     (usocket:timeout-error ()
                       (if (config-http-fn config)
                           (values nil nil nil :timeout)
                           (return (failure :timeout attempt))))
                     (agento11y-export-error (e)
                       (if (eq :unsupported-deadline
                               (getf (agento11y-export-error-diagnostics e) :kind))
                           (return (failure :unsupported-deadline attempt))
                           (values nil nil nil t)))
                     (error () (values nil nil nil t)))
                 (when (and (integerp status) (<= 200 status 299))
                   (let ((diagnostic
                           (when (member label '("traces" "metrics") :test #'string=)
                             (otlp-response-diagnostics resp-body label count))))
                     (when diagnostic
                       (agento11y-log config :warn "exporter"
                                      (format nil "~a response reports rejected or invalid data" label)))
                     (return (values (not diagnostic) resp-body
                                     (when diagnostic
                                       (append diagnostic (list :status-code status :attempts attempt)))))))
                 (let ((kind (cond ((eq transport-error :timeout) :timeout)
                                   (transport-error :transport)
                                   (t :http))))
                   (unless (or transport-error
                               (and (integerp status)
                                    (retryable-export-status-p status label)))
                     (return (failure kind attempt status)))
                   (when (= attempt attempts)
                     (return (failure kind attempt status)))
                   (let* ((delay (max (or (retry-after-seconds response-headers) 0)
                                      (backoff-seconds (1- attempt)
                                                       (config-initial-backoff-sec config)
                                                       (config-max-backoff-sec config))))
                          (remaining (deadline-remaining *export-deadline*)))
                     (when (and remaining (>= delay remaining))
                       (return (failure :timeout attempt status)))
                     (handler-case (sleep delay)
                       #+sbcl
                       (sb-ext:timeout ()
                         (return (failure :timeout attempt status)))))))))))

(defun log-rejected-generations (config resp-body)
  "Return the number of generation rejections without logging response text."
  (let ((count
          (ignore-errors
            (when (and (stringp resp-body) (plusp (length resp-body)))
              (let* ((parsed (jzon:parse resp-body))
                     (errors (and (hash-table-p parsed) (jget parsed "errors"))))
                (when (and (vectorp errors) (not (stringp errors)))
                  (length errors)))))))
    (when (and count (plusp count))
      (agento11y-log config :warn "exporter"
                     (format nil "generations rejected: ~d" count)))
    (or count 0)))

(defun export-generations (config generations auth-headers)
  "POST a batch of generations and report individual rejections."
  (when (null generations) (return-from export-generations t))
  (let* ((url (config-generation-endpoint config))
         (payload (jzon:stringify (jobj "generations" (coerce generations 'vector)))))
    (multiple-value-bind (ok resp-body diagnostics)
        (post-with-retry config url payload auth-headers "generations" (length generations))
      (let ((rejected (if ok (log-rejected-generations config resp-body) 0)))
        (if (plusp rejected)
            (values nil resp-body (list :kind :partial-success :signal "generations"
                                       :count (length generations) :rejected-count rejected))
            (values ok resp-body diagnostics))))))

(defun otlp-response-diagnostics (resp-body label count)
  "Inspect partialSuccess without retrying an already accepted batch."
  (handler-case
      (when (and (stringp resp-body) (plusp (length resp-body)))
        (let ((parsed (jzon:parse resp-body)))
          (unless (hash-table-p parsed) (error "OTLP response must be an object"))
          (multiple-value-bind (partial present-p) (gethash "partialSuccess" parsed)
            (unless present-p
              (multiple-value-setq (partial present-p) (gethash "partial_success" parsed)))
            (when (and present-p
                       (not (and (symbolp partial) (string= (symbol-name partial) "NULL"))))
              (unless (hash-table-p partial) (error "OTLP partialSuccess must be an object"))
              (let* ((keys (if (equal label "metrics")
                               '("rejectedDataPoints" "rejected_data_points")
                               '("rejectedSpans" "rejected_spans")))
                     (value (loop for key in keys
                                  when (nth-value 1 (gethash key partial))
                                    return (gethash key partial)
                                  finally (return 0)))
                     (rejected (cond
                                 ((and (symbolp value) (string= (symbol-name value) "NULL")) 0)
                                 ((integerp value) value)
                                 ((stringp value) (parse-integer value))
                                 (t (error "Invalid OTLP rejected count")))))
                (unless (and (>= rejected 0)
                             (or (equal label "metrics") (<= rejected count)))
                  (error "Invalid OTLP rejected count"))
                (when (plusp rejected)
                  (list :kind :partial-success :signal label :count count
                        :rejected-count rejected
                        :server-message-present
                        (let ((message (or (jget partial "errorMessage")
                                           (jget partial "error_message"))))
                          (and (stringp message) (plusp (length message)))))))))))
    (error () (list :kind :invalid-response :signal label :count count))))

(defun export-traces (config spans auth-headers)
  "POST spans to OTLP and report partial rejection without whole-batch retry."
  (when (null spans) (return-from export-traces t))
  (let* ((url (config-traces-endpoint config))
         (payload (jzon:stringify
                   (build-otlp-payload spans
                                       (config-service-name config)
                                       (config-service-version config)
                                       :resource-attributes (config-otel-resource-attributes config)))))
    (post-with-retry config url payload auth-headers "traces" (length spans))))

(defun export-workflow-steps (config workflow-steps auth-headers)
  "POST a batch of workflow steps to the workflow steps endpoint."
  (when (null workflow-steps) (return-from export-workflow-steps t))
  (let* ((url (config-workflow-steps-endpoint config))
         (payload (jzon:stringify (jobj "workflow_steps"
                                        (coerce workflow-steps 'vector)))))
    (post-with-retry config url payload auth-headers
                     "workflow-steps" (length workflow-steps))))
