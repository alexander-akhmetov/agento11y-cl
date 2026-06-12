(in-package :sigil-cl)

(defparameter +experiment-run-id-tag+ "experiment.run_id")
(defparameter +experiment-run-id-metadata-key+ "experiment_run_id")
(defparameter +experiment-score-source-kind+ "experiment")

(defun %join-stable-id-parts (parts)
  (with-output-to-string (out)
    (loop for part in parts
          for first-p = t then nil
          do (unless first-p
               (write-char (code-char #x1f) out))
             (when part
               (write-string (princ-to-string part) out)))))

(defun stable-id (prefix &rest parts)
  "Return a deterministic id from PARTS for idempotent retries.
Matches StableID in the Go and Python SDKs (first 16 hex chars of SHA-1
over the parts joined with #\Us), so reruns from another SDK dedupe to the
same score and conversation ids. Cross-SDK parity holds for string parts;
non-string parts are printed with PRINC-TO-STRING, whose output (e.g. for
booleans and floats) differs from Go fmt.Sprint and Python str."
  (let ((joined (%join-stable-id-parts parts)))
    (format nil "~a-~a" prefix
            (subseq (sha1-hex (string-to-utf8-octets joined)) 0 16))))

(defun make-score (&key evaluator-id evaluator-version score-key
                        (value nil value-supplied-p)
                        ((:passed passed) nil passed-supplied-p)
                        explanation generation-id metadata)
  "Build a score output for EXPERIMENT-RUN-ADD-SCORES."
  (let ((score (list :evaluator-id evaluator-id
                     :evaluator-version evaluator-version
                     :score-key score-key)))
    (when value-supplied-p
      (setf score (append score (list :value value))))
    (when generation-id
      (setf score (append score (list :generation-id generation-id))))
    (when passed-supplied-p
      (setf score (append score (list :passed passed))))
    (when explanation
      (setf score (append score (list :explanation explanation))))
    (when metadata
      (setf score (append score (list :metadata metadata))))
    score))

(defclass experiment-run ()
  ((client :initarg :client :reader experiment-run-client)
   (run-id :initarg :run-id :reader experiment-run-run-id)
   (name :initarg :name :reader experiment-run-name)
   (upload :initarg :upload :reader experiment-run-upload :initform :continuous)
   (buffer :initform nil :accessor experiment-run-buffer)
   (dataset :initarg :dataset :accessor experiment-run-dataset :initform nil)
   (candidate :initarg :candidate :accessor experiment-run-candidate :initform nil)
   (lock :initform (bt2:make-lock :name "sigil-experiment-run")
         :reader experiment-run-lock)
   (agent-name :initarg :agent-name :accessor experiment-run-agent-name :initform nil)
   (agent-version :initarg :agent-version :accessor experiment-run-agent-version :initform nil)
   (extra-tags :initarg :extra-tags :accessor experiment-run-extra-tags :initform nil)
   (extra-metadata :initarg :extra-metadata :accessor experiment-run-extra-metadata :initform nil)
   (active-conversation-id :initarg :active-conversation-id
                           :accessor experiment-run-active-conversation-id
                           :initform nil)
   (tracked-generation-ids :initform nil :accessor experiment-run-tracked-generation-ids)
   (accepted-count :initform 0 :accessor experiment-run-accepted-count)
   (finalized-p :initform nil :accessor experiment-run-finalized-p)))

(defun %normalize-upload-mode (upload)
  (let ((mode (or upload :continuous)))
    (unless (member mode '(:continuous :bulk :manual))
      (error 'sigil-validation-error
             :message (format nil "unknown upload mode ~s; expected :continuous, :bulk, or :manual"
                              mode)))
    mode))

(defun %make-experiment-run (&key client run-id name dataset candidate
                                  (upload :continuous)
                                  agent-name agent-version extra-tags extra-metadata)
  (make-instance 'experiment-run
                 :client client
                 :run-id (%trimmed-text run-id)
                 :name (%text name)
                 :upload upload
                 :dataset dataset
                 :candidate candidate
                 :agent-name agent-name
                 :agent-version agent-version
                 :extra-tags extra-tags
                 :extra-metadata extra-metadata))

(defun %string-alist (value)
  (cond
    ((null value) nil)
    ((hash-table-p value)
     (let ((out nil))
       (maphash (lambda (k v)
                  (when v
                    (push (cons (%json-key k) (%text v)) out)))
                value)
       (nreverse out)))
    ((and (listp value) (every #'consp value))
     (loop for (k . v) in value
           when v
           collect (cons (%json-key k) (%text v))))
    ((listp value)
     (loop for (k v) on value by #'cddr
           when v
           collect (cons (%json-key k) (%text v))))
    (t nil)))

(defun %merge-string-alists (&rest lists)
  "Merge string-keyed alists. Later lists override earlier lists."
  (let ((out nil))
    (dolist (alist lists)
      (dolist (pair (%string-alist alist))
        (let ((existing (assoc (car pair) out :test #'equal)))
          (if existing
              (setf (cdr existing) (cdr pair))
              (setf out (append out (list (cons (car pair) (cdr pair)))))))))
    out))

(defun %merge-metadata (&rest objects)
  "Merge metadata-like objects into one JSON object. Later objects override earlier objects."
  (let ((out (jobj)))
    (dolist (object objects)
      (when object
        (let ((json (%jsonify object)))
          (when (hash-table-p json)
            (maphash (lambda (k v)
                       (setf (gethash k out) v))
                     json)))))
    out))

(defun %metadata-empty-p (metadata)
  (and (hash-table-p metadata) (zerop (hash-table-count metadata))))

(defun %metadata-field (object key)
  (multiple-value-bind (value found-p)
      (%field object key (intern (substitute #\- #\_ (string-upcase key)) :keyword))
    (if found-p value nil)))

(defun %run-metadata (metadata dataset candidate)
  (let ((out (%merge-metadata metadata)))
    (when dataset
      (let ((id (%metadata-field dataset "id"))
            (version (%metadata-field dataset "version"))
            (uri (%metadata-field dataset "uri")))
        (when (and id (not (gethash "dataset_id" out)))
          (setf (gethash "dataset_id" out) id))
        (when (and version (not (gethash "dataset_version" out)))
          (setf (gethash "dataset_version" out) version))
        (when (and uri (not (gethash "dataset_uri" out)))
          (setf (gethash "dataset_uri" out) uri))))
    (when (and candidate (not (gethash "candidate" out)))
      (setf (gethash "candidate" out) (%jsonify candidate)))
    (unless (gethash "created_at" out)
      (setf (gethash "created_at" out) (iso8601-now)))
    out))

(defun %score-output-field (score string-key keyword-key)
  (%field score string-key keyword-key))

(defun %score-output-required (score string-key keyword-key label)
  (%required-field score string-key keyword-key label))

(defun %item-id (item)
  (cond
    ((null item) "")
    ((stringp item) item)
    (t (let ((value (%metadata-field item "id")))
         (if value (%text value) "")))))

(defun %item-metadata (item)
  (when item
    (multiple-value-bind (metadata found-p) (%field item "metadata" :metadata)
      (if found-p metadata nil))))

(defun %score-metadata (run score item trial-id)
  (let* ((dataset (experiment-run-dataset run))
         (candidate (experiment-run-candidate run))
         (score-meta (nth-value 0 (%score-output-field score "metadata" :metadata)))
         (out (jobj)))
    (when dataset
      (let ((id (%metadata-field dataset "id"))
            (version (%metadata-field dataset "version")))
        (when id (setf (gethash "dataset_id" out) id))
        (when version (setf (gethash "dataset_version" out) version))))
    (when candidate
      (setf (gethash "candidate" out) (%jsonify candidate)))
    (when item
      (let ((id (%item-id item))
            (item-meta (%item-metadata item)))
        (when (plusp (length id))
          (setf (gethash "item_id" out) id))
        (when item-meta
          (let ((json (%jsonify item-meta)))
            (when (hash-table-p json)
              (maphash (lambda (k v)
                         (unless (equal k "id")
                           (setf (gethash k out) v)))
                       json))))))
    (when trial-id
      (setf (gethash "trial_id" out) (%text trial-id)))
    (when score-meta
      (let ((json (%jsonify score-meta)))
        (when (hash-table-p json)
          (maphash (lambda (k v)
                     (setf (gethash k out) v))
                   json))))
    out))

(defun %experiment-run-produced-generation-ids-unlocked (run)
  (let ((seen (make-hash-table :test 'equal))
        (ids nil))
    (dolist (raw-id (reverse (experiment-run-tracked-generation-ids run)))
      (let ((id (%trimmed-text raw-id)))
        (when (and (plusp (length id))
                   (not (gethash id seen)))
          (setf (gethash id seen) t)
          (push id ids))))
    (nreverse ids)))

(defun experiment-run-track-generation-id (run generation-id)
  "Record GENERATION-ID for later score attribution."
  (let ((id (%trimmed-text generation-id)))
    (when (plusp (length id))
      (bt2:with-lock-held ((experiment-run-lock run))
        (push id (experiment-run-tracked-generation-ids run)))))
  run)

(defun experiment-run-reset-capture (run &key conversation-id)
  "Clear captured generation ids and set the active conversation id."
  (bt2:with-lock-held ((experiment-run-lock run))
    (setf (experiment-run-tracked-generation-ids run) nil
          (experiment-run-active-conversation-id run) (%trimmed-text conversation-id))
    (experiment-run-active-conversation-id run)))

(defun experiment-run-produced-generation-ids (run)
  "Return generation ids captured since the last reset."
  (bt2:with-lock-held ((experiment-run-lock run))
    (%experiment-run-produced-generation-ids-unlocked run)))

(defun %experiment-run-register-recorder (run recorder)
  (let ((id (gen-rec-generation-id recorder)))
    (when id
      (experiment-run-track-generation-id run id)))
  recorder)

(defun %new-conversation-id (run)
  (stable-id "conv" (experiment-run-run-id run) (generate-id)))

(defun %experiment-run-prepare-generation-options (run client &key conversation-id
                                                           agent-name agent-version
                                                           tags metadata)
  (declare (ignore client))
  (bt2:with-lock-held ((experiment-run-lock run))
    (let* ((conv-id (%trimmed-text (or conversation-id
                                       (experiment-run-active-conversation-id run))))
           (final-conv-id (if (plusp (length conv-id))
                              conv-id
                              (%new-conversation-id run)))
           (merged-tags (%merge-string-alists
                         (experiment-run-extra-tags run)
                         tags
                         (list (cons +experiment-run-id-tag+
                                     (experiment-run-run-id run)))))
           (merged-metadata (%merge-metadata
                             (experiment-run-extra-metadata run)
                             metadata
                             (list (cons +experiment-run-id-metadata-key+
                                         (experiment-run-run-id run)))))
           (final-agent-name (or agent-name (experiment-run-agent-name run)))
           (final-agent-version (or agent-version (experiment-run-agent-version run))))
      (setf (experiment-run-active-conversation-id run) final-conv-id)
      (list :conversation-id final-conv-id
            :agent-name final-agent-name
            :agent-version final-agent-version
            :tags merged-tags
            :metadata merged-metadata))))

(defun %score-generation-id (score generation-ids)
  (multiple-value-bind (generation-id found-p)
      (%score-output-field score "generation_id" :generation-id)
    (let ((id (%trimmed-text generation-id)))
      (cond
        ((and found-p (plusp (length id))) id)
        ((= (length generation-ids) 1) (first generation-ids))
        ((> (length generation-ids) 1)
         (error 'sigil-validation-error
                :message "score generation_id is required when an experiment item produced multiple generations"))
        (t
         (error 'sigil-validation-error
                :message "score generation_id is required"))))))

(defun %build-score-item (run score item generation-ids conversation-id trial-id)
  (let* ((generation-id (%score-generation-id score generation-ids))
         (evaluator-id (%score-output-required score "evaluator_id" :evaluator-id "evaluator_id"))
         (evaluator-version (%score-output-required score "evaluator_version" :evaluator-version
                                                    "evaluator_version"))
         (score-key (%score-output-required score "score_key" :score-key "score_key"))
         (item-id (%item-id item))
         (score-id (stable-id "score"
                              (experiment-run-run-id run)
                              item-id
                              generation-id
                              evaluator-id
                              evaluator-version
                              score-key
                              (or trial-id ""))))
    (multiple-value-bind (value value-found-p) (%score-output-field score "value" :value)
      (let ((out (jobj "score_id" score-id
                       "generation_id" generation-id
                       "evaluator_id" evaluator-id
                       "evaluator_version" evaluator-version
                       "score_key" score-key
                       "value" (%serialize-score-value value value-found-p)
                       "run_id" (experiment-run-run-id run)
                       "source" (jobj "kind" +experiment-score-source-kind+
                                      "id" (experiment-run-run-id run)))))
        (when (plusp (length (%trimmed-text conversation-id)))
          (setf (gethash "conversation_id" out) (%trimmed-text conversation-id)))
        (multiple-value-bind (passed passed-p) (%score-output-field score "passed" :passed)
          (when passed-p
            (setf (gethash "passed" out) (if passed t nil))))
        (multiple-value-bind (explanation explanation-p)
            (%score-output-field score "explanation" :explanation)
          (when (and explanation-p (not (%blank-string-p explanation)))
            (setf (gethash "explanation" out) (%text explanation))))
        (let ((metadata (%score-metadata run score item trial-id)))
          (unless (%metadata-empty-p metadata)
            (setf (gethash "metadata" out) metadata)))
        out))))

(defun experiment-run-add-scores (run scores &key item generation-ids conversation-id trial-id)
  "Attach SCORES to this experiment run. In :continuous upload mode (the
default) they are exported immediately and the accepted count is returned.
In :bulk and :manual modes they are buffered and the buffered count is
returned; EXPERIMENT-RUN-PUBLISH exports them."
  (when (null scores)
    (return-from experiment-run-add-scores 0))
  (multiple-value-bind (ids conv-id)
      (bt2:with-lock-held ((experiment-run-lock run))
        (values (or generation-ids (%experiment-run-produced-generation-ids-unlocked run))
                (%trimmed-text (or conversation-id
                                   (experiment-run-active-conversation-id run)))))
    (let ((items (mapcar (lambda (score)
                           (%build-score-item run score item ids conv-id trial-id))
                         scores)))
      (if (eq (experiment-run-upload run) :continuous)
          (progn
            (client-flush (experiment-run-client run))
            (let ((accepted (export-scores (experiment-run-client run) items)))
              (bt2:with-lock-held ((experiment-run-lock run))
                (incf (experiment-run-accepted-count run) accepted))
              accepted))
          (bt2:with-lock-held ((experiment-run-lock run))
            (setf (experiment-run-buffer run)
                  (append (experiment-run-buffer run) items))
            (length items))))))

(defun experiment-run-publish (run)
  "Export scores buffered by the :bulk and :manual upload modes. Returns the
newly accepted count. The buffer is kept on export failure so a retry
publishes the same items (their score ids are stable)."
  (let ((items (bt2:with-lock-held ((experiment-run-lock run))
                 (copy-list (experiment-run-buffer run)))))
    (if (null items)
        0
        (progn
          (client-flush (experiment-run-client run))
          (let ((accepted (export-scores (experiment-run-client run) items)))
            (bt2:with-lock-held ((experiment-run-lock run))
              (incf (experiment-run-accepted-count run) accepted)
              (setf (experiment-run-buffer run)
                    (nthcdr (length items) (experiment-run-buffer run))))
            accepted)))))

(defun experiment-run-buffered-score-count (run)
  "Number of scores waiting for EXPERIMENT-RUN-PUBLISH."
  (bt2:with-lock-held ((experiment-run-lock run))
    (length (experiment-run-buffer run))))

(defun experiment-run-finalize (run &key (status "succeeded") ((:error error-message)) metadata)
  "Finalize RUN once. Later calls are ignored."
  (bt2:with-lock-held ((experiment-run-lock run))
    (unless (experiment-run-finalized-p run)
      (complete-experiment (experiment-run-client run)
                           (experiment-run-run-id run)
                           status
                           :score-count (experiment-run-accepted-count run)
                           :error error-message
                           :metadata metadata)
      (setf (experiment-run-finalized-p run) t)))
  run)

(defun experiment-run-url (run)
  "Return the UI URL for RUN."
  (experiment-url (experiment-run-client run) (experiment-run-run-id run)))

(defun experiment-run-report (run)
  "Fetch the aggregated report for RUN."
  (get-experiment-report (experiment-run-client run) (experiment-run-run-id run)))

(defun %open-experiment-run (client &key run-id name description tags metadata dataset candidate
                                      collection-id agent-name agent-version extra-tags
                                      extra-metadata (source "external") (on-conflict :reopen)
                                      (upload :continuous))
  (when (%blank-string-p run-id)
    (error 'sigil-validation-error :message "experiment run_id is required"))
  (when (%blank-string-p name)
    (error 'sigil-validation-error :message "experiment name is required"))
  (setf upload (%normalize-upload-mode upload))
  (let* ((create-metadata (%run-metadata metadata dataset candidate))
         (run-tags tags))
    (when (and collection-id (not (%blank-string-p collection-id)))
      (unless (member (format nil "collectionId:~a" collection-id) run-tags :test #'equal)
        (setf run-tags (append run-tags (list (format nil "collectionId:~a" collection-id)))))
      (unless (gethash "collection_id" create-metadata)
        (setf (gethash "collection_id" create-metadata) collection-id)))
    (handler-case
        (create-experiment client
                           :run-id run-id
                           :name name
                           :source source
                           :description description
                           :tags run-tags
                           :collection-id collection-id
                           :metadata create-metadata)
      (sigil-conflict-error (e)
        (if (eq on-conflict :reopen)
            (update-experiment client run-id :status "running")
            (error e))))
    (%make-experiment-run :client client
                          :run-id run-id
                          :name name
                          :upload upload
                          :dataset dataset
                          :candidate candidate
                          :agent-name agent-name
                          :agent-version agent-version
                          :extra-tags extra-tags
                          :extra-metadata extra-metadata)))

(defun %safe-finalize (thunk)
  (handler-case
      (funcall thunk)
    (error () nil)))

(defun %experiment-log (run message)
  (sigil-log (client-config (experiment-run-client run)) :info "experiment" message))

(defun %call-with-experiment (client body-fn &key run-id name description tags metadata
                                      dataset candidate collection-id agent-name agent-version
                                      extra-tags extra-metadata (source "external")
                                      (on-conflict :reopen) (upload :continuous)
                                      (print-url t))
  (let ((run (%open-experiment-run client
                                   :run-id run-id
                                   :name name
                                   :description description
                                   :tags tags
                                   :metadata metadata
                                   :dataset dataset
                                   :candidate candidate
                                   :collection-id collection-id
                                   :agent-name agent-name
                                   :agent-version agent-version
                                   :extra-tags extra-tags
                                   :extra-metadata extra-metadata
                                   :source source
                                   :on-conflict on-conflict
                                   :upload upload))
        (exit-kind :canceled)
        (error-text nil))
    (unwind-protect
         (handler-case
             (let ((*experiment-run* run))
               (multiple-value-prog1
                   (funcall body-fn run)
                 (setf exit-kind :succeeded)))
           (error (e)
             (setf exit-kind :failed
                   error-text (princ-to-string e))
             (error e)))
      (case exit-kind
        (:succeeded
         (if (eq (experiment-run-upload run) :manual)
             (when print-url
               (ignore-errors
                 (%experiment-log
                  run
                  (format nil "experiment ~a left open (manual mode): ~d score(s) buffered; call experiment-run-publish then experiment-run-finalize"
                          (experiment-run-run-id run)
                          (experiment-run-buffered-score-count run)))))
             (progn
               (handler-case
                   (experiment-run-publish run)
                 (error (e)
                   (%safe-finalize
                    (lambda ()
                      (experiment-run-finalize run :status "failed"
                                               :error (princ-to-string e))))
                   (error e)))
               (experiment-run-finalize run :status "succeeded")
               (when print-url
                 (ignore-errors
                   (%experiment-log
                    run
                    (format nil "experiment ~a finished (~d scores): ~a"
                            (experiment-run-run-id run)
                            (experiment-run-accepted-count run)
                            (experiment-run-url run))))))))
        (:failed
         (%safe-finalize
          (lambda ()
            (experiment-run-finalize run :status "failed" :error error-text))))
        (t
         (%safe-finalize
          (lambda ()
            (cancel-experiment client (experiment-run-run-id run))))
         (bt2:with-lock-held ((experiment-run-lock run))
           (setf (experiment-run-finalized-p run) t)))))))

(defmacro with-experiment ((run client &rest options) &body body)
  "Create or reopen an experiment run, bind it, and finalize it on exit.
On normal exit buffered scores are published and the run is finalized
succeeded (:upload :manual instead leaves the run open for the caller to
publish and finalize); on error it is finalized failed; on a non-local exit
it is canceled."
  `(%call-with-experiment ,client
                          (lambda (,run)
                            ,@body)
                          ,@options))

;;; --- Dataset runner ---

(defun make-dataset-item (&key id input expected metadata)
  "Build a dataset item for RUN-EXPERIMENT."
  (let ((item (jobj "id" (%text id))))
    (when input (setf (gethash "input" item) (%jsonify input)))
    (when expected (setf (gethash "expected" item) (%jsonify expected)))
    (when metadata (setf (gethash "metadata" item) (%jsonify metadata)))
    item))

(defun make-target-result (&key output generation-ids conversation-id metadata)
  "Build a target result for RUN-EXPERIMENT targets."
  (let ((result (jobj)))
    (when output (setf (gethash "output" result) (%jsonify output)))
    (when generation-ids
      (setf (gethash "generation_ids" result)
            (coerce (mapcar #'%text generation-ids) 'vector)))
    (when conversation-id
      (setf (gethash "conversation_id" result) (%text conversation-id)))
    (when metadata (setf (gethash "metadata" result) (%jsonify metadata)))
    result))

(defun %id-list (value)
  (let ((raw (cond
               ((null value) nil)
               ((stringp value) (list value))
               ((listp value) value)
               ((vectorp value) (coerce value 'list))
               (t nil))))
    (remove-if #'%blank-string-p (mapcar #'%trimmed-text raw))))

(defun %scorer-outputs (produced)
  "Normalize one scorer's return value to a list of score outputs.
A bare MAKE-SCORE plist is wrapped in a list."
  (cond
    ((null produced) nil)
    ((and (listp produced) (keywordp (first produced))) (list produced))
    ((and (vectorp produced) (not (stringp produced))) (coerce produced 'list))
    ((listp produced) produced)
    (t (list produced))))

(defun run-experiment (client items target scorers
                       &key run-id name description tags metadata dataset candidate
                            collection-id agent-name agent-version extra-tags extra-metadata
                            (source "external") (on-conflict :reopen)
                            (upload :continuous) (print-url t) (fetch-report t))
  "Run TARGET over dataset ITEMS inside an experiment and export scores.

For each item the run's capture is reset with a stable per-item
conversation id, TARGET is called as (funcall target item run), and each
scorer in SCORERS is called as (funcall scorer item result) where RESULT is
TARGET's return value (or an empty object when it returns NIL; see
MAKE-TARGET-RESULT). Scorers return lists of score outputs (see
MAKE-SCORE). Scores fall back to the generation ids captured by the run
when the target result does not name them.

Returns a plist with :run-id, :accepted-scores, :url, and :report (NIL
unless FETCH-REPORT is set and the report fetch succeeds)."
  (let ((completed-run nil))
    (with-experiment (run client
                      :run-id run-id :name name :description description
                      :tags tags :metadata metadata :dataset dataset
                      :candidate candidate :collection-id collection-id
                      :agent-name agent-name :agent-version agent-version
                      :extra-tags extra-tags :extra-metadata extra-metadata
                      :source source :on-conflict on-conflict
                      :upload upload :print-url print-url)
      (setf completed-run run)
      (dolist (item items)
        (experiment-run-reset-capture
         run
         :conversation-id (stable-id "conv" (experiment-run-run-id run) (%item-id item)))
        (let* ((result (or (funcall target item run) (jobj)))
               (generation-ids
                 (or (%id-list (nth-value 0 (%field result "generation_ids" :generation-ids)))
                     (experiment-run-produced-generation-ids run)))
               (conv-id
                 (let ((cid (%trimmed-text
                             (nth-value 0 (%field result "conversation_id" :conversation-id)))))
                   (if (plusp (length cid))
                       cid
                       (experiment-run-active-conversation-id run))))
               (outputs (loop for scorer in scorers
                              append (%scorer-outputs (funcall scorer item result)))))
          (experiment-run-add-scores run outputs
                                     :item item
                                     :generation-ids generation-ids
                                     :conversation-id conv-id))))
    (list :run-id (experiment-run-run-id completed-run)
          :accepted-scores (experiment-run-accepted-count completed-run)
          :url (experiment-run-url completed-run)
          :report (when fetch-report
                    (%safe-finalize (lambda () (experiment-run-report completed-run)))))))
