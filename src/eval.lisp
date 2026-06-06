(in-package :sigil-cl)

(defparameter +eval-experiments-suffix+ "/eval/experiments")
(defparameter +max-eval-response-bytes+ (* 8 1024 1024))

(defun %blank-string-p (value)
  (or (null value)
      (and (stringp value)
           (zerop (length (%trim value))))))

(defun %text (value)
  (cond
    ((null value) "")
    ((stringp value) value)
    ((keywordp value) (string-downcase (symbol-name value)))
    (t (princ-to-string value))))

(defun %trimmed-text (value)
  (%trim (%text value)))

(defun %strip-trailing-slash (value)
  (string-right-trim "/" value))

(defun %ensure-leading-slash (value)
  (let ((text (%trimmed-text value)))
    (cond
      ((zerop (length text)) "/")
      ((char= (char text 0) #\/) text)
      (t (concatenate 'string "/" text)))))

(defun %base-url-from-endpoint (endpoint)
  (let* ((trimmed (%trimmed-text endpoint))
         (scheme-pos (search "://" trimmed)))
    (unless scheme-pos
      (error 'sigil-config-error
             :message (format nil "eval endpoint must include a URL scheme: ~a" trimmed)))
    (let* ((scheme (subseq trimmed 0 scheme-pos))
           (rest (subseq trimmed (+ scheme-pos 3)))
           (slash-pos (position #\/ rest))
           (host (if slash-pos (subseq rest 0 slash-pos) rest)))
      (when (or (zerop (length scheme)) (zerop (length host)))
        (error 'sigil-config-error
               :message (format nil "eval endpoint host is required: ~a" trimmed)))
      (format nil "~a://~a" scheme host))))

(defun eval-base-url (config)
  "Return the scheme and host used for Sigil eval API requests."
  (cond
    ((config-eval-endpoint config)
     (%base-url-from-endpoint (config-eval-endpoint config)))
    ((config-generation-endpoint config)
     (%base-url-from-endpoint (config-generation-endpoint config)))
    (t
     (error 'sigil-config-error
            :message "eval endpoint is required when generation endpoint is unset"))))

(defun %url-encode (value)
  (with-output-to-string (out)
    (loop for ch across (%trimmed-text value)
          for code = (char-code ch)
          do (if (or (and (>= code (char-code #\a)) (<= code (char-code #\z)))
                     (and (>= code (char-code #\A)) (<= code (char-code #\Z)))
                     (and (>= code (char-code #\0)) (<= code (char-code #\9)))
                     (member ch '(#\- #\_ #\. #\~) :test #'char=))
                 (write-char ch out)
                 (format out "%~2,'0X" code)))))

(defun %query-encode (pairs)
  (let ((parts nil))
    (dolist (pair pairs)
      (when (cdr pair)
        (push (format nil "~a=~a"
                      (%url-encode (car pair))
                      (%url-encode (cdr pair)))
              parts)))
    (format nil "~{~a~^&~}" (nreverse parts))))

(defun %experiments-url (config)
  (format nil "~a~a~a"
          (%strip-trailing-slash (eval-base-url config))
          (%ensure-leading-slash (or (config-eval-path-prefix config) "/api/v1"))
          +eval-experiments-suffix+))

(defun %experiment-api-url (config run-id)
  (when (%blank-string-p run-id)
    (error 'sigil-validation-error :message "experiment run_id is required"))
  (format nil "~a/~a" (%experiments-url config) (%url-encode run-id)))

(defun %json-key (key)
  (cond
    ((stringp key) key)
    ((keywordp key)
     (substitute #\_ #\- (string-downcase (symbol-name key))))
    ((symbolp key)
     (substitute #\_ #\- (string-downcase (symbol-name key))))
    (t (princ-to-string key))))

(defun %jsonify (value)
  (cond
    ((null value) nil)
    ((hash-table-p value)
     (let ((out (jobj)))
       (maphash (lambda (k v)
                  (setf (gethash (%json-key k) out) (%jsonify v)))
                value)
       out))
    ((and (listp value) (every #'consp value))
     (let ((out (jobj)))
       (dolist (pair value)
         (setf (gethash (%json-key (car pair)) out) (%jsonify (cdr pair))))
       out))
    ((listp value)
     (coerce (mapcar #'%jsonify value) 'vector))
    ((stringp value) value)
    ((vectorp value)
     (map 'vector #'%jsonify value))
    (t value)))

(defun %field (object string-key keyword-key)
  (cond
    ((hash-table-p object)
     (multiple-value-bind (value found-p) (gethash string-key object)
       (if found-p
           (values value t)
           (gethash keyword-key object))))
    ((and (listp object) (every #'consp object))
     (let ((by-string (assoc string-key object :test #'equal))
           (by-keyword (assoc keyword-key object :test #'eq)))
       (cond
         (by-string (values (cdr by-string) t))
         (by-keyword (values (cdr by-keyword) t))
         (t (values nil nil)))))
    ((listp object)
     (let ((sentinel (list :missing)))
       (let ((value (getf object keyword-key sentinel)))
         (if (eq value sentinel)
             (let ((string-value (getf object string-key sentinel)))
               (if (eq string-value sentinel)
                   (values nil nil)
                   (values string-value t)))
             (values value t)))))
    (t (values nil nil))))

(defun %required-field (object string-key keyword-key label)
  (multiple-value-bind (value found-p) (%field object string-key keyword-key)
    (let ((text (%trimmed-text value)))
      (unless (and found-p (plusp (length text)))
        (error 'sigil-validation-error
               :message (format nil "score validation failed: missing ~a" label)))
      text)))

(defun %score-value-object-p (value)
  (and (hash-table-p value)
       (or (nth-value 1 (gethash "number" value))
           (nth-value 1 (gethash "bool" value))
           (nth-value 1 (gethash "string" value)))))

(defun %serialize-score-value (value &optional (supplied-p t))
  (cond
    ((not supplied-p)
     (error 'sigil-validation-error
            :message "score validation failed: value is required"))
    ((%score-value-object-p value) value)
    ((and (numberp value) (not (typep value 'boolean)))
     (jobj "number" value))
    ((or (eq value t) (eq value nil))
     (jobj "bool" (if value t nil)))
    ((stringp value)
     (jobj "string" value))
    (t
     (error 'sigil-validation-error
            :message "score validation failed: value must be number, boolean, or string"))))

(defun %maybe-set-field (target key object string-key keyword-key)
  (multiple-value-bind (value found-p) (%field object string-key keyword-key)
    (when found-p
      (setf (gethash key target) (%jsonify value)))))

(defun %serialize-source (value)
  (cond
    ((null value) nil)
    ((hash-table-p value) (%jsonify value))
    ((listp value) (%jsonify value))
    ((stringp value) (jobj "kind" "experiment" "id" value))
    (t value)))

(defun %serialize-score-item (item)
  (let* ((score-id (%required-field item "score_id" :score-id "score_id"))
         (generation-id (%required-field item "generation_id" :generation-id "generation_id"))
         (evaluator-id (%required-field item "evaluator_id" :evaluator-id "evaluator_id"))
         (evaluator-version (%required-field item "evaluator_version" :evaluator-version
                                             "evaluator_version"))
         (score-key (%required-field item "score_key" :score-key "score_key")))
    (multiple-value-bind (value value-found-p) (%field item "value" :value)
      (let ((out (jobj "score_id" score-id
                       "generation_id" generation-id
                       "evaluator_id" evaluator-id
                       "evaluator_version" evaluator-version
                       "score_key" score-key
                       "value" (%serialize-score-value value value-found-p))))
        (%maybe-set-field out "conversation_id" item "conversation_id" :conversation-id)
        (%maybe-set-field out "trace_id" item "trace_id" :trace-id)
        (%maybe-set-field out "span_id" item "span_id" :span-id)
        (%maybe-set-field out "rule_id" item "rule_id" :rule-id)
        (%maybe-set-field out "run_id" item "run_id" :run-id)
        (multiple-value-bind (passed passed-p) (%field item "passed" :passed)
          (when passed-p
            (setf (gethash "passed" out) (if passed t nil))))
        (%maybe-set-field out "explanation" item "explanation" :explanation)
        (%maybe-set-field out "metadata" item "metadata" :metadata)
        (multiple-value-bind (created-at created-at-p) (%field item "created_at" :created-at)
          (when created-at-p
            (setf (gethash "created_at" out) (%text created-at))))
        (multiple-value-bind (source source-p) (%field item "source" :source)
          (when source-p
            (let ((serialized (%serialize-source source)))
              (when serialized
                (setf (gethash "source" out) serialized)))))
        out))))

(defun %parse-response-body (body status label)
  (let ((text (cond
                ((null body) "")
                ((stringp body) (%trim body))
                (t (%trim (princ-to-string body))))))
    (when (> (length text) +max-eval-response-bytes+)
      (error 'sigil-export-error
             :status-code status
             :message (format nil "~a response too large" label)))
    (if (zerop (length text))
        (jobj)
        (handler-case
            (jzon:parse text)
          (error (e)
            (error 'sigil-export-error
                   :status-code status
                   :message (format nil "~a returned invalid JSON: ~a"
                                    label (princ-to-string e))))))))

(defun %status-detail (body status)
  (let ((text (if (stringp body) (%trim body) (princ-to-string body))))
    (if (plusp (length text)) text (format nil "status ~d" status))))

(defun %signal-status-error (status body label)
  (let ((message (format nil "~a: ~a" label (%status-detail body status))))
    (cond
      ((member status '(400 422))
       (error 'sigil-validation-error :message message))
      ((= status 404)
       (error 'sigil-not-found-error :message message))
      ((= status 409)
       (error 'sigil-conflict-error :message message))
      (t
       (error 'sigil-export-error :status-code status :message message)))))

(defun %do-http-request (config method url headers body)
  (let ((http-fn (config-http-fn config)))
    (if http-fn
        (funcall http-fn url :method method :headers headers :content body)
        (dexador:request url
                         :method method
                         :headers headers
                         :content body
                         :force-string t
                         :connect-timeout (config-export-timeout-sec config)
                         :read-timeout (config-export-timeout-sec config)))))

(defun request-eval-json (config method url payload label)
  "Send one eval JSON request and return the parsed response body."
  (let* ((body (when payload (jzon:stringify payload)))
         (headers (append (list (cons "Content-Type" "application/json"))
                          (build-auth-headers config)))
         (attempts (max 1 (config-max-retries config))))
    (loop for attempt from 0 below attempts
          do (handler-case
                 (multiple-value-bind (resp-body status)
                     (%do-http-request config method url headers body)
                   (cond
                     ((<= 200 status 299)
                      (return (%parse-response-body resp-body status label)))
                     ((or (= status 429) (<= 500 status 599))
                      (if (< attempt (1- attempts))
                          (sleep (backoff-seconds attempt
                                                  (config-initial-backoff-sec config)
                                                  (config-max-backoff-sec config)))
                          (%signal-status-error status resp-body label)))
                     (t
                      (%signal-status-error status resp-body label))))
               (dexador.error:http-request-failed (e)
                 (let ((status (dexador.error:response-status e))
                       (resp-body (dexador.error:response-body e)))
                   (if (and (or (= status 429) (<= 500 status 599))
                            (< attempt (1- attempts)))
                       (sleep (backoff-seconds attempt
                                               (config-initial-backoff-sec config)
                                               (config-max-backoff-sec config)))
                       (%signal-status-error status resp-body label))))
               (sigil-error (e)
                 (error e))
               (error (e)
                 (if (< attempt (1- attempts))
                     (sleep (backoff-seconds attempt
                                             (config-initial-backoff-sec config)
                                             (config-max-backoff-sec config)))
                     (error 'sigil-export-error
                            :message (format nil "~a request failed: ~a"
                                             label (princ-to-string e)))))))))

(defun create-experiment (client &key run-id name (source "external") description
                                   tags collection-id evaluators metadata)
  "Create an experiment run and return the parsed response object."
  (when (%blank-string-p name)
    (error 'sigil-validation-error :message "experiment name is required"))
  (let* ((config (client-config client))
         (payload (jobj "name" (%trimmed-text name)
                        "source" (%trimmed-text source))))
    (unless (%blank-string-p run-id)
      (setf (gethash "run_id" payload) (%trimmed-text run-id)))
    (unless (%blank-string-p description)
      (setf (gethash "description" payload) (%text description)))
    (when tags
      (setf (gethash "tags" payload) (coerce tags 'vector)))
    (unless (%blank-string-p collection-id)
      (setf (gethash "collection_id" payload) (%trimmed-text collection-id)))
    (when evaluators
      (setf (gethash "evaluators" payload) (%jsonify evaluators)))
    (when metadata
      (setf (gethash "metadata" payload) (%jsonify metadata)))
    (request-eval-json config :post (%experiments-url config) payload "experiment create")))

(defun get-experiment (client run-id)
  "Fetch an experiment run by id."
  (let ((config (client-config client)))
    (request-eval-json config :get (%experiment-api-url config run-id)
                       nil "experiment get")))

(defun update-experiment (client run-id &key name description tags status metadata
                                            ((:error error-message)) score-count)
  "Patch an experiment run and return the parsed response object."
  (let* ((config (client-config client))
         (payload (jobj)))
    (when name
      (setf (gethash "name" payload) (%text name)))
    (when description
      (setf (gethash "description" payload) (%text description)))
    (when tags
      (setf (gethash "tags" payload) (coerce tags 'vector)))
    (when status
      (setf (gethash "status" payload) (%trimmed-text status)))
    (when metadata
      (setf (gethash "metadata" payload) (%jsonify metadata)))
    (when error-message
      (setf (gethash "error" payload) (%text error-message)))
    (when score-count
      (setf (gethash "score_count" payload) score-count))
    (request-eval-json config :patch (%experiment-api-url config run-id)
                       payload "experiment update")))

(defun complete-experiment (client run-id status &key score-count ((:error error-message)) metadata)
  "Mark an experiment run terminal with STATUS."
  (update-experiment client run-id
                     :status status
                     :score-count score-count
                     :error error-message
                     :metadata metadata))

(defun cancel-experiment (client run-id)
  "Cancel a running experiment run."
  (let* ((config (client-config client))
         (url (format nil "~a:cancel" (%experiment-api-url config run-id))))
    (request-eval-json config :post url (jobj) "experiment cancel")))

(defun get-experiment-report (client run-id)
  "Fetch the aggregated report for an experiment run."
  (let* ((config (client-config client))
         (url (format nil "~a/report" (%experiment-api-url config run-id))))
    (request-eval-json config :get url nil "experiment report")))

(defun list-experiment-scores (client run-id &key (limit 50) cursor)
  "List stored scores for an experiment run."
  (let* ((config (client-config client))
         (base (format nil "~a/scores" (%experiment-api-url config run-id)))
         (query (%query-encode (list (cons "limit" (princ-to-string (max 1 limit)))
                                     (cons "cursor" cursor))))
         (url (if (plusp (length query))
                  (format nil "~a?~a" base query)
                  base)))
    (request-eval-json config :get url nil "experiment scores list")))

(defun export-scores (client score-items)
  "Export SCORE-ITEMS to the Sigil scores ingest API. Returns accepted count."
  (when (null score-items)
    (return-from export-scores 0))
  (let* ((config (client-config client))
         (serialized (mapcar #'%serialize-score-item score-items))
         (payload (jobj "scores" (coerce serialized 'vector)))
         (url (format nil "~a~a"
                      (%strip-trailing-slash (eval-base-url config))
                      (%ensure-leading-slash (config-scores-export-path config))))
         (response (request-eval-json config :post url payload "score export"))
         (results (jget response "results"))
         (accepted 0)
         (rejected nil))
    (when (vectorp results)
      (loop for result across results
            do (if (jget result "accepted")
                   (incf accepted)
                   (push (format nil "~a~@[ (~a)~]"
                                 (or (jget result "score_id") "<unknown>")
                                 (jget result "error"))
                         rejected))))
    (when rejected
      (error 'sigil-export-error
             :status-code 202
             :message (format nil "score export rejected: ~{~a~^, ~}"
                              (nreverse rejected))))
    accepted))

(defun %replace-all (text old new)
  (with-output-to-string (out)
    (let ((start 0)
          (old-len (length old)))
      (loop for pos = (search old text :start2 start)
            while pos
            do (write-string text out :start start :end pos)
               (write-string new out)
               (setf start (+ pos old-len))
            finally (write-string text out :start start)))))

(defun experiment-url (client run-id)
  "Return a best-effort UI URL for an experiment run."
  (let* ((config (client-config client))
         (base (%strip-trailing-slash (eval-base-url config)))
         (normalized (%trimmed-text run-id))
         (template (config-experiment-url-template config)))
    (if (and template (plusp (length (%trimmed-text template))))
        (%replace-all (%replace-all template "{run_id}" normalized) "{base}" base)
        (format nil "~a/a/grafana-sigil-app/evaluation/experiments/~a"
                base (%url-encode normalized)))))
