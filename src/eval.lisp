(in-package :agento11y-cl)

;;; Experiment wire protocol.
;;;
;;; Writes and reads live on two different planes.
;;;
;;;   Writes  POST  /api/v1/experiment-runs:upsert
;;;           POST  /api/v1/experiment-runs/{id}:finalize
;;;           POST  /api/v1/experiment-runs/{id}/trials              (trial.lisp)
;;;           PATCH /api/v1/experiment-runs/{id}/trials/{trial_id}   (trial.lisp)
;;;           POST  /api/v1/scores:export
;;;
;;;   Reads   GET   /api/v1/eval/experiments/{id}
;;;           GET   /api/v1/eval/experiments/{id}/report
;;;           GET   /api/v1/eval/experiments/{id}/scores
;;;
;;; /eval/experiments answers GET only. This SDK must not issue POST or PATCH
;;; to it; +EVAL-EXPERIMENTS-SUFFIX+ is for reads.
;;;
;;; Provenance of the contract below. Every route, payload key, and status
;;; value is taken from the first-party SDKs at agento11y:
;;;
;;;   python/agento11y/_experiments_transport.py  routes, upsert and score
;;;                                               serialization, finalize
;;;                                               status vocabulary
;;;   python/agento11y/experiments/client.py      trial request bodies
;;;   python/agento11y/experiments/experiment.py  trial id, snapshot rules
;;;   python/agento11y/errors.py                  409 classification
;;;   go/agento11y/experiments.go                 route constants (agree)
;;;
;;; NOT empirically confirmed against a live tenant. Two values are worth
;;; re-checking before trusting them in production, and both are overridable:
;;;
;;;   1. Ingest-actor header name. Python sends X-Agento11y-Ingest-Actor and
;;;      asserts X-Sigil-Ingest-Actor is absent; Go still sends the Sigil
;;;      spelling. Both SDKs were touched within a day of each other, so this
;;;      is a live fork, not one lagging the other. This SDK follows Python
;;;      (see +INGEST-ACTOR-HEADER+). A wrong choice surfaces as HTTP 401
;;;      naming actor ownership on trial create, which AGENTO11Y-ACTOR-MISMATCH-
;;;      ERROR reports by name.
;;;   2. Whether the /eval/experiments write routes still answer. If they do,
;;;      the plane was superseded rather than removed and callers pinned to it
;;;      would need a compatibility path. Nothing in this repo depends on it.

;;; Read routes only.
(defparameter +eval-experiments-suffix+ "/eval/experiments")
;;; Absolute paths: unlike the read routes these are not composed with
;;; CONFIG-EVAL-PATH-PREFIX, matching _experiments_transport.py:65-66.
(defparameter +experiment-runs-upsert-path+ "/api/v1/experiment-runs:upsert")
(defparameter +experiment-runs-prefix+ "/api/v1/experiment-runs")
;;; Identifies this SDK as the writer of a run and its trials.
(defparameter +experiment-run-source-id+ "lisp")
(defparameter +ingest-actor-header+ "X-Agento11y-Ingest-Actor")
(defparameter +max-eval-response-bytes+ (* 8 1024 1024))

;;; Cloud trial evaluation (experimental).
;;;
;;;   POST /api/v1/experiment-runs/{id}/trials/{trial_id}:evaluate
;;;   GET  /api/v1/experiment-runs/{id}/trials/{trial_id}/evaluations/{eval_id}
;;;   POST /api/v1/experiment-runs/{id}/trials/{trial_id}/artifacts:upload
(defparameter +trial-evaluation-statuses+ '("queued" "claimed" "success" "failed"))
(defparameter +trial-evaluation-terminal-statuses+ '("success" "failed"))
(defparameter +default-evaluation-timeout-sec+ 300)
(defparameter +default-evaluation-poll-interval-sec+ 0.5)
;; Ceiling for the poll backoff, so a long wait costs tens of status reads
;; rather than hundreds. A caller asking for a slower cadence keeps it.
(defparameter +max-evaluation-poll-interval-sec+ 5)

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
      (error 'agento11y-config-error
             :message (format nil "eval endpoint must include a URL scheme: ~a" trimmed)))
    (let* ((scheme (subseq trimmed 0 scheme-pos))
           (rest (subseq trimmed (+ scheme-pos 3)))
           (slash-pos (position #\/ rest))
           (host (if slash-pos (subseq rest 0 slash-pos) rest)))
      (when (or (zerop (length scheme)) (zerop (length host)))
        (error 'agento11y-config-error
               :message (format nil "eval endpoint host is required: ~a" trimmed)))
      (format nil "~a://~a" scheme host))))

(defun eval-base-url (config)
  "Return the scheme and host used for eval API requests."
  (cond
    ((config-eval-endpoint config)
     (%base-url-from-endpoint (config-eval-endpoint config)))
    ((config-generation-endpoint config)
     (%base-url-from-endpoint (config-generation-endpoint config)))
    (t
     (error 'agento11y-config-error
            :message "eval endpoint is required when generation endpoint is unset"))))

(defun %url-encode (value)
  (with-output-to-string (out)
    (loop for code across (string-to-utf8-octets (%trimmed-text value))
          do (if (or (<= (char-code #\a) code (char-code #\z))
                     (<= (char-code #\A) code (char-code #\Z))
                     (<= (char-code #\0) code (char-code #\9))
                     (member code '#.(mapcar #'char-code '(#\- #\_ #\. #\~))))
                 (write-char (code-char code) out)
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
  "Base URL of the read plane. Not valid for POST or PATCH."
  (format nil "~a~a~a"
          (%strip-trailing-slash (eval-base-url config))
          (%ensure-leading-slash (config-eval-path-prefix config))
          +eval-experiments-suffix+))

(defun %experiment-api-url (config run-id)
  "URL of one experiment on the read plane."
  (when (%blank-string-p run-id)
    (error 'agento11y-validation-error :message "experiment run_id is required"))
  (format nil "~a/~a" (%experiments-url config) (%url-encode run-id)))

(defun %experiment-runs-upsert-url (config)
  (format nil "~a~a"
          (%strip-trailing-slash (eval-base-url config))
          +experiment-runs-upsert-path+))

(defun %experiment-run-url (config experiment-id &optional (suffix ""))
  "URL of one experiment run on the write plane, plus an optional SUFFIX
such as \":finalize\" or \"/trials\"."
  (when (%blank-string-p experiment-id)
    (error 'agento11y-validation-error :message "experiment experiment_id is required"))
  (format nil "~a~a/~a~a"
          (%strip-trailing-slash (eval-base-url config))
          +experiment-runs-prefix+
          (%url-encode experiment-id)
          suffix))

(defun %experiment-run-source ()
  "The run's writer identity. An object, not a bare string."
  (jobj "kind" "sdk" "id" +experiment-run-source-id+))

(defun %json-key (key)
  (cond
    ((stringp key) key)
    ((keywordp key)
     (substitute #\_ #\- (string-downcase (symbol-name key))))
    ((symbolp key)
     (substitute #\_ #\- (string-downcase (symbol-name key))))
    (t (princ-to-string key))))

(defun %plist-p (value)
  "Whether VALUE is a property list: an even-length list starting with a
keyword. A plain list of strings and an alist of conses both fail this test,
so the three list shapes callers pass stay distinguishable."
  (and (consp value)
       (keywordp (first value))
       (evenp (length value))))

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
    ((%plist-p value)
     (let ((out (jobj)))
       (loop for (k v) on value by #'cddr
             do (setf (gethash (%json-key k) out) (%jsonify v)))
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

(defun %metadata-field (object key)
  "Read KEY from OBJECT, accepting the string key or its keyword spelling."
  (multiple-value-bind (value found-p)
      (%field object key (intern (substitute #\- #\_ (string-upcase key)) :keyword))
    (if found-p value nil)))

(defun %required-field (object string-key keyword-key label)
  (multiple-value-bind (value found-p) (%field object string-key keyword-key)
    (let ((text (%trimmed-text value)))
      (unless (and found-p (plusp (length text)))
        (error 'agento11y-validation-error
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
     (error 'agento11y-validation-error
            :message "score validation failed: value is required"))
    ((%score-value-object-p value) value)
    ((and (numberp value) (not (typep value 'boolean)))
     (jobj "number" value))
    ((or (eq value t) (eq value nil))
     (jobj "bool" (if value t nil)))
    ((stringp value)
     (jobj "string" value))
    (t
     (error 'agento11y-validation-error
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

(defun %optional-text-field (object string-key keyword-key)
  "Return the trimmed value of a field, or NIL when absent or blank."
  (multiple-value-bind (value found-p) (%field object string-key keyword-key)
    (when found-p
      (let ((text (%trimmed-text value)))
        (when (plusp (length text)) text)))))

(defun %score-experiment-id (item)
  "The run a score belongs to. `experiment_id` is the wire key; `run_id` is a
client-side alias kept because callers and the run object still speak it."
  (or (%optional-text-field item "experiment_id" :experiment-id)
      (%optional-text-field item "run_id" :run-id)))

(defun %serialize-score-item (item)
  (let* ((score-id (%required-field item "score_id" :score-id "score_id"))
         (evaluator-id (%required-field item "evaluator_id" :evaluator-id "evaluator_id"))
         (evaluator-version (%required-field item "evaluator_version" :evaluator-version
                                             "evaluator_version"))
         (score-key (%required-field item "score_key" :score-key "score_key"))
         (generation-id (%optional-text-field item "generation_id" :generation-id))
         (trial-id (%optional-text-field item "trial_id" :trial-id))
         (experiment-id (%score-experiment-id item)))
    ;; The backend anchors a score to a generation or to a typed trial. Mirror
    ;; that rule here so the error names the missing anchor instead of arriving
    ;; as an opaque 400.
    (unless (or generation-id trial-id)
      (error 'agento11y-validation-error
             :message "score validation failed: generation_id or trial_id is required"))
    (multiple-value-bind (value value-found-p) (%field item "value" :value)
      (let ((out (jobj "score_id" score-id
                       "evaluator_id" evaluator-id
                       "evaluator_version" evaluator-version
                       "score_key" score-key
                       "value" (%serialize-score-value value value-found-p))))
        (when generation-id
          (setf (gethash "generation_id" out) generation-id))
        (when trial-id
          (setf (gethash "trial_id" out) trial-id))
        (when experiment-id
          (setf (gethash "experiment_id" out) experiment-id))
        (%maybe-set-field out "conversation_id" item "conversation_id" :conversation-id)
        (%maybe-set-field out "test_case_id" item "test_case_id" :test-case-id)
        (%maybe-set-field out "trace_id" item "trace_id" :trace-id)
        (%maybe-set-field out "span_id" item "span_id" :span-id)
        (%maybe-set-field out "grader_conversation_id" item
                          "grader_conversation_id" :grader-conversation-id)
        (%maybe-set-field out "grader_generation_id" item
                          "grader_generation_id" :grader-generation-id)
        (%maybe-set-field out "grader_trace_id" item "grader_trace_id" :grader-trace-id)
        (%maybe-set-field out "rule_id" item "rule_id" :rule-id)
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
      (error 'agento11y-export-error
             :status-code status
             :message (format nil "~a response too large" label)))
    (if (zerop (length text))
        (jobj)
        (handler-case
            (jzon:parse text)
          (error (e)
            (error 'agento11y-export-error
                   :status-code status
                   :message (format nil "~a returned invalid JSON: ~a"
                                    label (princ-to-string e))))))))

(defun %status-detail (body status)
  (let ((text (if (stringp body) (%trim body) (princ-to-string body))))
    (if (plusp (length text)) text (format nil "status ~d" status))))

(defun %contains-p (needle text)
  (and (search needle text) t))

(defun classify-conflict (message)
  "Classify a backend HTTP 409 body so callers do not parse strings.
Ports classify_conflict from agento11y python/agento11y/errors.py:71-97."
  (let ((v (string-downcase (or message ""))))
    (cond
      ((or (%contains-p "score_count" v)
           (%contains-p "score count" v)
           (and (%contains-p "expected " v) (%contains-p " scores, found " v)))
       :score-count-mismatch)
      ((%contains-p "pending evaluation" v) :pending-evaluations)
      ((or (%contains-p "running trial" v)
           (and (%contains-p "cannot complete experiment with " v)
                (%contains-p " trial" v)))
       :running-trials)
      ((or (%contains-p "terminal" v)
           (%contains-p "already completed" v)
           (%contains-p "already finalized" v)
           (%contains-p "already published" v))
       :terminal)
      ((or (%contains-p "immutable" v)
           (%contains-p "cannot change" v)
           (%contains-p "conflicts with the existing experiment" v)
           (%contains-p "not a draft" v))
       :immutable-field)
      ((or (%contains-p "open draft" v)
           (%contains-p "draft already exists" v))
       :open-draft)
      (t :unknown))))

(defun conflict-recoverable-p (kind)
  "Whether a conflict of KIND can be resolved by the caller and retried."
  (and (member kind '(:score-count-mismatch :running-trials
                      :pending-evaluations :open-draft))
       t))

(defun %actor-mismatch-p (detail)
  "Whether an HTTP 401 body reports that another actor owns the run."
  (let ((v (string-downcase (or detail ""))))
    (or (%contains-p "actor" v)
        (%contains-p "owned by" v))))

(defun %signal-status-error (status body label)
  (let* ((detail (%status-detail body status))
         (message (format nil "~a: ~a" label detail)))
    (cond
      ((member status '(400 422))
       (error 'agento11y-validation-error :message message))
      ;; Not retried: a run claimed by another ingest actor stays claimed, so
      ;; every retry returns the same 401 and only spends the budget.
      ((and (= status 401) (%actor-mismatch-p detail))
       (error 'agento11y-actor-mismatch-error
              :message (format nil "~a: experiment is owned by another ingest actor: ~a"
                               label detail)))
      ((= status 404)
       (error 'agento11y-not-found-error :message message))
      ((= status 409)
       (error 'agento11y-conflict-error :kind (classify-conflict detail) :message message))
      (t
       (error 'agento11y-export-error :status-code status :message message)))))

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

(defun %ingest-actor-headers (config)
  "The ingest-actor header, or NIL when the actor is blank.
Appended to both eval auth and score-export auth, so it rides every eval
request (reads included) under one spelling and the run, its trials, and its
scores are claimed by the same identity."
  (let ((actor (%trimmed-text (config-ingest-actor config))))
    (when (plusp (length actor))
      (list (cons +ingest-actor-header+ actor)))))

(defun build-eval-auth-headers (config)
  "Auth headers for eval control-plane requests (experiment lifecycle,
reports, conversations). When :eval-auth-token is set it replaces the
generation auth headers with a single bearer Authorization header (matching
AGENTO11Y_EVAL_AUTH_TOKEN in the reference SDKs); otherwise the generation
export auth headers are reused. Score export deliberately does not use
this: it is a tenant ingest write that goes out with generation auth."
  (let ((token (%trimmed-text (config-eval-auth-token config))))
    (append
     (if (plusp (length token))
         (list (cons "Authorization"
                     (if (and (>= (length token) 7)
                              (string-equal (subseq token 0 7) "bearer "))
                         token
                         (concatenate 'string "Bearer " token))))
         (build-auth-headers config))
     (%ingest-actor-headers config))))

(defun build-score-export-headers (config)
  "Auth headers for score export: generation auth plus the ingest actor."
  (append (build-auth-headers config) (%ingest-actor-headers config)))

(defun %request-eval (config method url body content-type label
                      &key headers headers-supplied-p)
  "Send one eval request with an already-encoded BODY and return the parsed
JSON response. Holds the retry loop, the 429/5xx policy, and the status
mapping for every eval call, whatever the body encoding.

HEADERS-SUPPLIED-P replaces the default eval auth headers with HEADERS, and
an empty HEADERS still counts: score export supplies generation auth, and an
unauthenticated deployment builds no headers at all. Falling back on an empty
list would send the eval token to the generation host. The wrappers pass their
own supplied-p through, which is why this takes it as a plain argument."
  (let* ((headers (append (list (cons "Content-Type" content-type))
                          (if headers-supplied-p
                              headers
                              (build-eval-auth-headers config))))
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
               (agento11y-error (e)
                 (error e))
               (error (e)
                 (if (< attempt (1- attempts))
                     (sleep (backoff-seconds attempt
                                             (config-initial-backoff-sec config)
                                             (config-max-backoff-sec config)))
                     (error 'agento11y-export-error
                            :message (format nil "~a request failed: ~a"
                                             label (princ-to-string e)))))))))

(defun request-eval-json (config method url payload label
                          &key (headers nil headers-supplied-p))
  "Send one eval JSON request and return the parsed response body.
See %REQUEST-EVAL for what supplying HEADERS means."
  (%request-eval config method url
                 (when payload (jzon:stringify payload))
                 "application/json" label
                 :headers headers :headers-supplied-p headers-supplied-p))

(defun request-eval-bytes-json (config method url body content-type label
                                &key (headers nil headers-supplied-p))
  "Send raw BODY bytes and return the parsed JSON response.
Artifact upload posts the content as the request body, so it cannot go through
REQUEST-EVAL-JSON, which stringifies its payload. A blank CONTENT-TYPE falls
back to application/octet-stream."
  (%request-eval config method url body
                 (if (%blank-string-p content-type)
                     "application/octet-stream"
                     (%trimmed-text content-type))
                 label
                 :headers headers :headers-supplied-p headers-supplied-p))

(defun upsert-experiment-run (client &key experiment-id run-id name description tags
                                      suite-id suite-version candidate planned-trial-count
                                      metadata)
  "Create or idempotently claim an experiment run; return the parsed response.
RUN-ID is accepted as an alias for EXPERIMENT-ID because the run object and
this SDK's callers speak `run_id` while the wire key is `experiment_id`.

`collection_id` and `evaluators` are deliberately absent: the route rejects
unknown fields. Callers that group by collection carry it in tags and
metadata instead."
  (when (%blank-string-p name)
    (error 'agento11y-validation-error :message "experiment name is required"))
  (when (and planned-trial-count (minusp planned-trial-count))
    (error 'agento11y-validation-error
           :message "experiment validation failed: planned_trial_count must be non-negative"))
  (let* ((config (client-config client))
         (id (or (unless (%blank-string-p experiment-id) (%trimmed-text experiment-id))
                 (unless (%blank-string-p run-id) (%trimmed-text run-id))))
         (payload (jobj "name" (%trimmed-text name)
                        "source" (%experiment-run-source))))
    (when id
      (setf (gethash "experiment_id" payload) id))
    (unless (%blank-string-p description)
      (setf (gethash "description" payload) (%text description)))
    (when tags
      (setf (gethash "tags" payload) (coerce tags 'vector)))
    (unless (%blank-string-p suite-id)
      (setf (gethash "suite_id" payload) (%trimmed-text suite-id)))
    (unless (%blank-string-p suite-version)
      (setf (gethash "suite_version" payload) (%trimmed-text suite-version)))
    (when candidate
      (setf (gethash "candidate" payload) (%jsonify candidate)))
    (when planned-trial-count
      (setf (gethash "planned_trial_count" payload) planned-trial-count))
    (when metadata
      (setf (gethash "metadata" payload) (%jsonify metadata)))
    (request-eval-json config :post (%experiment-runs-upsert-url config)
                       payload "experiment upsert")))

(defun %normalize-finalize-status (status)
  "Map STATUS onto the wire vocabulary, or signal before any request is sent.
The backend's terminal success value is \"completed\"; \"succeeded\" is
accepted as a caller-facing alias."
  (let ((normalized (string-downcase (%trimmed-text status))))
    (cond
      ((member normalized '("succeeded" "completed") :test #'string=) "completed")
      ((string= normalized "failed") "failed")
      (t
       (error 'agento11y-validation-error
              :message (format nil "experiment validation failed: status must be completed or failed, got ~s"
                               normalized))))))

(defun finalize-experiment-run (client experiment-id
                                &key (status "completed") score-count ((:error error-message)))
  "Finalize an experiment run as completed or failed."
  (let* ((normalized (%normalize-finalize-status status))
         (config (client-config client))
         (payload (jobj "status" normalized
                        "source" (%experiment-run-source))))
    (when score-count
      (setf (gethash "score_count" payload) score-count))
    (unless (%blank-string-p error-message)
      (setf (gethash "error" payload) (%text error-message)))
    (request-eval-json config :post
                       (%experiment-run-url config experiment-id ":finalize")
                       payload "experiment finalize")))

(defun get-experiment (client run-id)
  "Fetch an experiment run by id."
  (let ((config (client-config client)))
    (request-eval-json config :get (%experiment-api-url config run-id)
                       nil "experiment get")))

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

(defun %scores-base-url (config)
  "Scheme and host for score export. Scores are a tenant ingest write that
goes to the generation-export host with generation auth, independent of the
eval control-plane target (matching the reference SDKs); the eval base is
only a fallback when no generation endpoint is configured."
  (if (config-generation-endpoint config)
      (%base-url-from-endpoint (config-generation-endpoint config))
      (eval-base-url config)))

(defun export-scores (client score-items)
  "Export SCORE-ITEMS to the scores ingest API. Returns accepted count."
  (when (null score-items)
    (return-from export-scores 0))
  (let* ((config (client-config client))
         (serialized (mapcar #'%serialize-score-item score-items))
         (payload (jobj "scores" (coerce serialized 'vector)))
         (url (format nil "~a~a"
                      (%strip-trailing-slash (%scores-base-url config))
                      (%ensure-leading-slash (config-scores-export-path config))))
         (response (request-eval-json config :post url payload "score export"
                                      :headers (build-score-export-headers config)))
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
      (error 'agento11y-export-error
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
        (format nil "~a/a/grafana-agento11y-app/evaluation/experiments/~a"
                base (%url-encode normalized)))))
