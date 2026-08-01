(in-package :sigil-cl)

(defparameter +experiment-run-id-tag+ "experiment.run_id")
(defparameter +experiment-run-id-metadata-key+ "experiment_run_id")
(defparameter +experiment-score-source-kind+ "experiment")

(defun make-score (&key evaluator-id evaluator-version score-key
                        (value nil value-supplied-p)
                        ((:passed passed) nil passed-supplied-p)
                        explanation generation-id metadata
                        grader-conversation-id grader-generation-id grader-trace-id)
  "Build a score output for EXPERIMENT-RUN-ADD-SCORES.
The GRADER-* ids record where the judgement came from, as opposed to the
generation or trial being judged."
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
    (when grader-conversation-id
      (setf score (append score (list :grader-conversation-id grader-conversation-id))))
    (when grader-generation-id
      (setf score (append score (list :grader-generation-id grader-generation-id))))
    (when grader-trace-id
      (setf score (append score (list :grader-trace-id grader-trace-id))))
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
   (finalized-p :initform nil :accessor experiment-run-finalized-p)
   ;; Local test suite, when the caller supplied one. Its id and version go on
   ;; the run payload; its cases feed trial snapshots.
   (suite :initarg :suite :accessor experiment-run-suite :initform nil)
   ;; Every trial id ever minted on this run, so a repeated
   ;; (test-case-id, attempt) pair is caught before it collides on the backend.
   (claimed-trial-ids :initform (make-hash-table :test 'equal)
                      :reader experiment-run-claimed-trial-ids)
   ;; trial-id -> trial, for trials still open at finalization time.
   (open-trials :initform (make-hash-table :test 'equal)
                :reader experiment-run-open-trials)
   ;; Trial ids claimed whose create has not returned yet. A trial reaches
   ;; OPEN-TRIALS only once the backend created it, so two opens racing inside
   ;; that round trip would both read an empty OPEN-TRIALS and neither would
   ;; report the overlap. Held from the claim to the end of the create.
   (opening-trial-ids :initform (make-hash-table :test 'equal)
                      :reader experiment-run-opening-trial-ids)
   ;; (anchor score-key evaluator-id) -> times seen, for deterministic score
   ;; ids. The anchor is the trial id for a trial-scoped score, so one table
   ;; still counts each trial separately.
   (score-occurrences :initform (make-hash-table :test 'equal)
                      :reader experiment-run-score-occurrences)))

(defun %experiment-log (run level message)
  (sigil-log (client-config (experiment-run-client run)) level "experiment" message))

(defun %call-swallowing-errors (run label thunk)
  "Run THUNK and return its value, or log the error and return NIL.
Used on teardown paths, where there is nowhere left to signal. The error is
logged rather than dropped: these are the paths that can leave a run
non-terminal, and a silent one leaves no trace of why."
  (handler-case
      (funcall thunk)
    (error (e)
      (ignore-errors
        (%experiment-log run :warn
                         (format nil "~a failed: ~a" label (princ-to-string e))))
      nil)))

(defun %normalize-upload-mode (upload)
  (let ((mode (or upload :continuous)))
    (unless (member mode '(:continuous :bulk :manual))
      (error 'sigil-validation-error
             :message (format nil "unknown upload mode ~s; expected :continuous, :bulk, or :manual"
                              mode)))
    mode))

(defun %make-experiment-run (&key client run-id name dataset candidate suite
                                  (upload :continuous)
                                  agent-name agent-version extra-tags extra-metadata)
  (make-instance 'experiment-run
                 :client client
                 :run-id (%trimmed-text run-id)
                 :name (%text name)
                 :upload upload
                 :dataset dataset
                 :candidate candidate
                 :suite suite
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

;; `trial_id` is a top-level field on the wire, so metadata carries only
;; dataset, candidate, and item context.
(defun %score-metadata (run score item)
  (let* ((dataset (experiment-run-dataset run))
         (candidate (experiment-run-candidate run))
         (score-meta (nth-value 0 (%field score "metadata" :metadata)))
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
  "Record GENERATION-ID for later score attribution.
A generation recorded after the run finalized is still tracked, because it did
happen and dropping it would lose the id a later score needs, but it is
reported: the score count and the trial statuses on the backend were written
without it."
  (let ((id (%trimmed-text generation-id))
        (late-p nil))
    (when (plusp (length id))
      (bt2:with-lock-held ((experiment-run-lock run))
        (setf late-p (experiment-run-finalized-p run))
        (push id (experiment-run-tracked-generation-ids run)))
      ;; Logged outside the lock: %experiment-log reaches the caller's log-fn,
      ;; and no lock is ever held across a user callback.
      (when late-p
        (%experiment-log
         run :warn
         (format nil "generation ~a was recorded on run ~a after the run finalized, ~
so it is not counted in the score count or the trial statuses already reported. A ~
context captured with capture-telemetry-context outlives the with-experiment scope it ~
was taken in; join the threads that use it before that scope exits."
                 id (experiment-run-run-id run))))))
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

(defun %score-generation-id (score generation-ids trial-id)
  "The generation a score attributes to, or \"\" when it has none.
A trial anchors the score on its own, so an anchored score needs no
generation and is not asked to pick between several. Without a trial a
generation is required, and an item that produced several must name which
one."
  (multiple-value-bind (generation-id found-p)
      (%field score "generation_id" :generation-id)
    (let ((id (%trimmed-text generation-id))
          (anchored-p (plusp (length (%trimmed-text trial-id)))))
      (cond
        ((and found-p (plusp (length id))) id)
        ((= (length generation-ids) 1) (first generation-ids))
        (anchored-p "")
        ((> (length generation-ids) 1)
         (error 'sigil-validation-error
                :message "score generation_id is required when an experiment item produced multiple generations"))
        (t
         (error 'sigil-validation-error
                :message "score generation_id or trial_id is required"))))))

(defun %score-anchor (trial-id item-id generation-id)
  "The identity a score's deterministic id hangs off.
A trial when there is one, otherwise the dataset item, otherwise the
generation. Keeps ids stable across reruns in every anchoring mode."
  (let ((trial (%trimmed-text trial-id))
        (item (%trimmed-text item-id)))
    (cond
      ((plusp (length trial)) trial)
      ((plusp (length item)) item)
      (t (%trimmed-text generation-id)))))

(defun %mint-score-id (run key)
  "Derive a deterministic score id for KEY, counting repeats of the same
(anchor, score-key, evaluator-id) triple so a rescore gets a distinct durable
id. Call under EXPERIMENT-RUN-LOCK.
Matches _next_score_id in agento11y experiments/experiment.py:850-857."
  (destructuring-bind (anchor score-key evaluator-id) key
    (let ((occurrence (gethash key (experiment-run-score-occurrences run) 0)))
      (setf (gethash key (experiment-run-score-occurrences run)) (1+ occurrence))
      (if (zerop occurrence)
          (stable-id "score" (experiment-run-run-id run) anchor score-key evaluator-id)
          (stable-id "score" (experiment-run-run-id run) anchor score-key evaluator-id
                     (1+ occurrence))))))

(defun %commit-score-ids (run items keys)
  "Stamp deterministic score ids onto ITEMS.
The occurrence counter advances here rather than while the batch is being
built, so a batch that fails validation halfway leaves the corrected retry
minting the same ids the backend would have deduped against."
  (bt2:with-lock-held ((experiment-run-lock run))
    (loop for item in items
          for key in keys
          do (setf (gethash "score_id" item) (%mint-score-id run key))))
  items)

(defun %score-object (score)
  "Copy a caller's SCORE (plist, alist, or hash-table) into a JSON object with
wire keys. Values are left alone; %SERIALIZE-SCORE-ITEM checks them at export
time. Anything else returns an empty object, which then fails the required
field checks with a message naming what is missing."
  (let ((out (jobj)))
    (flet ((put (k v) (setf (gethash (%json-key k) out) v)))
      (cond
        ((hash-table-p score) (maphash #'put score))
        ((null score) nil)
        ((and (listp score) (every #'consp score))
         (dolist (pair score) (put (car pair) (cdr pair))))
        ((%plist-p score)
         (loop for (k v) on score by #'cddr do (put k v)))))
    out))

(defun %set-text-field (object key value)
  "Set KEY on OBJECT to the trimmed VALUE when it has content."
  (let ((text (%trimmed-text value)))
    (when (plusp (length text))
      (setf (gethash key object) text)))
  object)

(defun %build-score-item (run score &key item generation-ids conversation-id trial)
  "Turn a caller's SCORE into a wire score object and return
(values item occurrence-key). The `score_id` is left off until
%COMMIT-SCORE-IDS stamps the whole validated batch.

Fields this SDK does not compute -- `passed`, the grader ids, anything else
the caller set -- are copied through for %SERIALIZE-SCORE-ITEM to handle, so
the two functions do not each carry their own list of score keys."
  (let* ((out (%score-object score))
         (trial-id (when trial (trial-id trial)))
         (generation-id (%score-generation-id score generation-ids trial-id))
         (evaluator-id (%required-field score "evaluator_id" :evaluator-id "evaluator_id"))
         (evaluator-version (%required-field score "evaluator_version" :evaluator-version
                                             "evaluator_version"))
         (score-key (%required-field score "score_key" :score-key "score_key"))
         (anchor (%score-anchor trial-id (%item-id item) generation-id)))
    (multiple-value-bind (value value-found-p) (%field score "value" :value)
      (setf (gethash "value" out) (%serialize-score-value value value-found-p)))
    (setf (gethash "evaluator_id" out) evaluator-id
          (gethash "evaluator_version" out) evaluator-version
          (gethash "score_key" out) score-key
          ;; `experiment_id` is the wire key for the run. `run_id` is a
          ;; client-side alias the backend has no field for.
          (gethash "experiment_id" out) (experiment-run-run-id run)
          (gethash "source" out) (jobj "kind" +experiment-score-source-kind+
                                       "id" (experiment-run-run-id run)))
    (remhash "run_id" out)
    (let ((gid (%trimmed-text generation-id)))
      (if (plusp (length gid))
          (setf (gethash "generation_id" out) gid)
          (remhash "generation_id" out)))
    (when trial
      (setf (gethash "trial_id" out) trial-id
            (gethash "test_case_id" out) (trial-test-case-id trial))
      (%set-text-field out "trace_id" (trial-trace-id trial))
      (%set-text-field out "span_id" (trial-span-id trial)))
    (%set-text-field out "conversation_id"
                     (or conversation-id
                         (when trial (trial-conversation-id trial))))
    (multiple-value-bind (explanation explanation-p) (%field score "explanation" :explanation)
      (if (and explanation-p (not (%blank-string-p explanation)))
          (setf (gethash "explanation" out) (%text explanation))
          (remhash "explanation" out)))
    (let ((metadata (%score-metadata run score item)))
      (if (%metadata-empty-p metadata)
          (remhash "metadata" out)
          (setf (gethash "metadata" out) metadata)))
    (values out (list anchor score-key evaluator-id))))

(defun %export-score-items (run items)
  "Flush pending generations, export ITEMS, and record the accepted count."
  (if (null items)
      0
      (progn
        (client-flush (experiment-run-client run))
        (let ((accepted (export-scores (experiment-run-client run) items)))
          (bt2:with-lock-held ((experiment-run-lock run))
            (incf (experiment-run-accepted-count run) accepted))
          accepted))))

(defun %check-trial-open (trial)
  "Signal when TRIAL has already closed, since its buffer will never drain."
  (when (trial-closed-p trial)
    (error 'sigil-validation-error
           :message (format nil "trial ~a is closed; its scores were already flushed"
                            (trial-id trial)))))

(defun experiment-run-add-scores (run scores &key item generation-ids conversation-id trial)
  "Attach SCORES to this experiment run.

With TRIAL the scores anchor to that trial and are held on it until the
trial closes, whatever the upload mode; the buffered count is returned. A
closed trial signals instead: its buffer has already drained, so anything
added later would be counted and never sent.

Without TRIAL, :continuous exports immediately and returns the accepted
count, while :bulk and :manual buffer on the run and return the buffered
count for EXPERIMENT-RUN-PUBLISH to export."
  (when (null scores)
    (return-from experiment-run-add-scores 0))
  (when trial
    (%check-trial-open trial))
  (multiple-value-bind (ids conv-id)
      (bt2:with-lock-held ((experiment-run-lock run))
        (values (or generation-ids (%experiment-run-produced-generation-ids-unlocked run))
                (%trimmed-text (or conversation-id
                                   (experiment-run-active-conversation-id run)))))
    (let ((items nil)
          (keys nil))
      (dolist (score scores)
        (multiple-value-bind (built key)
            (%build-score-item run score
                               :item item
                               :generation-ids ids
                               :conversation-id conv-id
                               :trial trial)
          (push built items)
          (push key keys)))
      (setf items (nreverse items)
            keys (nreverse keys))
      (%commit-score-ids run items keys)
      (cond
        (trial
         ;; Adding a score never closes a trial; the flush happens at close.
         (bt2:with-lock-held ((trial-lock trial))
           (%check-trial-open trial)
           (setf (trial-buffer trial) (append (trial-buffer trial) items))
           (length (trial-buffer trial))))
        ((eq (experiment-run-upload run) :continuous)
         (%export-score-items run items))
        (t
         (bt2:with-lock-held ((experiment-run-lock run))
           (setf (experiment-run-buffer run)
                 (append (experiment-run-buffer run) items))
           (length items)))))))

(defun trial-add-scores (trial scores &key item generation-ids conversation-id)
  "Attach SCORES to TRIAL. They are exported when the trial closes."
  (let ((run (trial-run trial)))
    (unless run
      (error 'sigil-validation-error
             :message "trial is not attached to an experiment run"))
    (experiment-run-add-scores run scores
                               :item item
                               :generation-ids generation-ids
                               :conversation-id conversation-id
                               :trial trial)))

(defun %trial-flush-scores (trial)
  "Drain TRIAL's score buffer. In :continuous mode the scores are exported
now; in :bulk and :manual they move to the run buffer for a later publish.
The buffer is kept on export failure so a retry sends the same items."
  (let* ((run (trial-run trial))
         (items (bt2:with-lock-held ((trial-lock trial))
                  (copy-list (trial-buffer trial)))))
    (when (or (null run) (null items))
      (return-from %trial-flush-scores 0))
    (if (eq (experiment-run-upload run) :continuous)
        (let ((accepted (%export-score-items run items)))
          (bt2:with-lock-held ((trial-lock trial))
            (setf (trial-buffer trial)
                  (nthcdr (length items) (trial-buffer trial))))
          accepted)
        (progn
          (bt2:with-lock-held ((experiment-run-lock run))
            (setf (experiment-run-buffer run)
                  (append (experiment-run-buffer run) items)))
          (bt2:with-lock-held ((trial-lock trial))
            (setf (trial-buffer trial)
                  (nthcdr (length items) (trial-buffer trial))))
          (length items)))))

(defun %resolve-test-case (run test-case)
  "Return (values test-case-id test-case). TEST-CASE may be a TEST-CASE, a
test-case id resolved against the run's suite, or a dataset item."
  (cond
    ((test-case-p test-case)
     (values (test-case-test-case-id test-case) test-case))
    ((stringp test-case)
     (values (%trimmed-text test-case)
             (test-suite-case (experiment-run-suite run) test-case)))
    (t
     ;; A dataset item: snapshot it as a case so the trial carries its I/O.
     ;; The id is trimmed here and nowhere else, so the duplicate guard and
     ;; the minted trial id cannot disagree about which case this is.
     (let ((id (%trimmed-text (%item-id test-case))))
       (values id
               (or (test-suite-case (experiment-run-suite run) id)
                   (make-test-case :id id
                                   :input (%metadata-field test-case "input")
                                   :expected (%metadata-field test-case "expected")
                                   :metadata (%item-metadata test-case))))))))

(defun experiment-run-open-trial (run test-case &key (attempt 1) metadata)
  "Open a typed trial on RUN for TEST-CASE and create it on the backend.
Signals SIGIL-VALIDATION-ERROR before any request when the
(test-case-id, attempt) pair was already used on this run: both would mint
the same trial id and collide."
  (multiple-value-bind (test-case-id resolved-case) (%resolve-test-case run test-case)
    (when (zerop (length test-case-id))
      (error 'sigil-validation-error :message "trial requires a test case id"))
    (let* ((run-id (experiment-run-run-id run))
           (trial-id (trial-mint-id run-id test-case-id attempt))
           (suite (experiment-run-suite run))
           (already-open nil)
           (trial nil))
      (bt2:with-lock-held ((experiment-run-lock run))
        (when (gethash trial-id (experiment-run-claimed-trial-ids run))
          (error 'sigil-validation-error
                 :message (format nil "trial for test case ~s attempt ~a already exists on run ~s; increment attempt for a retry"
                                  test-case-id attempt run-id)))
        (setf (gethash trial-id (experiment-run-claimed-trial-ids run)) t)
        ;; Snapshot the ids of every trial that claimed the run and has not
        ;; closed yet, for the warning below. Trials still being created count
        ;; too, because the wipe on the next line is what they lose. Every id
        ;; is named: an overlap is already an error state, so the count is
        ;; small, and naming them all tells the reader which trials just lost
        ;; their captured generations.
        (flet ((collect-ids (table)
                 (maphash (lambda (id value)
                            (declare (ignore value))
                            (push id already-open))
                          table)))
          (collect-ids (experiment-run-open-trials run))
          (collect-ids (experiment-run-opening-trial-ids run)))
        (setf already-open (sort already-open #'string<))
        (setf (gethash trial-id (experiment-run-opening-trial-ids run)) t)
        ;; A trial is a fresh unit of work. Generations captured before it
        ;; opened belong to the previous one, so its scores must not fall back
        ;; on them. The active conversation id is left alone: callers bind it
        ;; per trial and reading it back is how a score finds its conversation.
        (setf (experiment-run-tracked-generation-ids run) nil))
      ;; The claim and the opening marker are released again when the create
      ;; fails, because nothing exists on the backend to collide with and the
      ;; retry needs the same deterministic id. The warning is inside the
      ;; unwind-protect for the same reason: a log-fn that signals would
      ;; otherwise leave the id claimed with no trial behind it.
      (unwind-protect
           (progn
             ;; Logged outside the lock: %experiment-log reaches the caller's
             ;; log-fn, and no lock is ever held across a user callback.
             (when already-open
               (%experiment-log
                run :warn
                (format nil "trial ~a opened on run ~a while ~a still open. ~
Generation capture is held per run, not per trial. This open cleared the ~
generation ids captured for ~:[that trial~;those trials~], so scores on ~
~:*~:[it~;them~] can attribute to the wrong generations. Run trials sequentially."
                        trial-id run-id
                        (if (= 1 (length already-open))
                            (format nil "trial ~a is" (first already-open))
                            (format nil "trials ~{~a~^, ~} are" already-open))
                        (rest already-open))))
             (setf trial (trial-open (experiment-run-client run) run-id test-case-id
                                     :attempt attempt
                                     :run run
                                     :trial-id trial-id
                                     :test-case resolved-case
                                     :suite-id (when suite (test-suite-suite-id suite))
                                     :suite-version (when suite (test-suite-version suite))
                                     :metadata metadata
                                     :flush-fn #'%trial-flush-scores
                                     :closed-fn (lambda (closed)
                                                  (bt2:with-lock-held ((experiment-run-lock run))
                                                    (remhash (trial-id closed)
                                                             (experiment-run-open-trials run)))))))
        (bt2:with-lock-held ((experiment-run-lock run))
          (remhash trial-id (experiment-run-opening-trial-ids run))
          (if trial
              (setf (gethash trial-id (experiment-run-open-trials run)) trial)
              (remhash trial-id (experiment-run-claimed-trial-ids run)))))
      trial)))

(defun %call-with-trial (run test-case body-fn &rest options)
  (let ((trial (apply #'experiment-run-open-trial run test-case options))
        (error-text nil)
        (completed-p nil))
    (unwind-protect
         (handler-case
             (multiple-value-prog1
                 (funcall body-fn trial)
               (setf completed-p t))
           (error (e)
             (setf error-text (princ-to-string e))
             (error e)))
      ;; A throw or a return-from reaches the cleanup with no error text, and
      ;; would otherwise close the trial completed. Report it as errored: the
      ;; body never finished, so nothing verified the case.
      (let ((close-error (or error-text
                             (unless completed-p
                               "trial abandoned by a non-local exit"))))
        (if error-text
            ;; The body is already unwinding with its own error. A close that
            ;; signals here would replace it with a less useful one.
            (%call-swallowing-errors run "closing the trial"
                                     (lambda () (trial-close trial :error close-error)))
            (trial-close trial :error close-error))))))

(defun experiment-run-open-trials-list (run)
  "Trials opened on RUN that have not closed yet, in no particular order."
  (bt2:with-lock-held ((experiment-run-lock run))
    (let ((trials nil))
      (maphash (lambda (id trial) (declare (ignore id)) (push trial trials))
               (experiment-run-open-trials run))
      trials)))

(defun experiment-run-publish (run)
  "Export scores buffered by the :bulk and :manual upload modes. Returns the
newly accepted count. The buffer is kept on export failure so a retry
publishes the same items (their score ids are stable)."
  (let ((items (bt2:with-lock-held ((experiment-run-lock run))
                 (copy-list (experiment-run-buffer run)))))
    (prog1 (%export-score-items run items)
      (bt2:with-lock-held ((experiment-run-lock run))
        (setf (experiment-run-buffer run)
              (nthcdr (length items) (experiment-run-buffer run)))))))

(defun experiment-run-buffered-score-count (run)
  "Number of scores waiting for EXPERIMENT-RUN-PUBLISH."
  (bt2:with-lock-held ((experiment-run-lock run))
    (length (experiment-run-buffer run))))

(defun experiment-run-close-open-trials (run)
  "Close every trial still open on RUN. Returns a list of the errors raised;
one trial failing to close does not stop the rest from being attempted."
  (let ((errors nil))
    (dolist (trial (experiment-run-open-trials-list run))
      (handler-case
          (trial-close trial)
        (error (e)
          (push e errors)
          ;; Drop it from the open set regardless: a trial whose close failed
          ;; will not close on a second attempt either, and leaving it here
          ;; would block finalization.
          (bt2:with-lock-held ((experiment-run-lock run))
            (remhash (trial-id trial) (experiment-run-open-trials run))))))
    (nreverse errors)))

(defun experiment-run-finalize (run &key (status "completed") ((:error error-message))
                                      (score-count :accepted))
  "Finalize RUN once. Later calls are ignored.

STATUS accepts \"completed\" (or its alias \"succeeded\") and \"failed\";
anything else signals before a request is sent. SCORE-COUNT defaults to the
count this run saw accepted; pass NIL to omit it, which is what a run with a
trial that would not close must do because its local count is incomplete."
  (bt2:with-lock-held ((experiment-run-lock run))
    (unless (experiment-run-finalized-p run)
      (finalize-experiment-run (experiment-run-client run)
                               (experiment-run-run-id run)
                               :status status
                               :score-count (if (eq score-count :accepted)
                                                (experiment-run-accepted-count run)
                                                score-count)
                               :error error-message)
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
                                      extra-metadata suite planned-trial-count
                                      (on-conflict :reopen) (upload :continuous))
  (when (%blank-string-p run-id)
    (error 'sigil-validation-error :message "experiment run_id is required"))
  (when (%blank-string-p name)
    (error 'sigil-validation-error :message "experiment name is required"))
  (setf upload (%normalize-upload-mode upload))
  (let* ((create-metadata (%run-metadata metadata dataset candidate))
         (run-tags tags))
    ;; The upsert route rejects collection_id, so a collection is carried as a
    ;; tag and a metadata key instead of its own field.
    (when (and collection-id (not (%blank-string-p collection-id)))
      (unless (member (format nil "collectionId:~a" collection-id) run-tags :test #'equal)
        (setf run-tags (append run-tags (list (format nil "collectionId:~a" collection-id)))))
      (unless (gethash "collection_id" create-metadata)
        (setf (gethash "collection_id" create-metadata) collection-id)))
    (handler-case
        (upsert-experiment-run client
                               :experiment-id run-id
                               :name name
                               :description description
                               :tags run-tags
                               :suite-id (when suite (test-suite-suite-id suite))
                               :suite-version (when suite (test-suite-version suite))
                               :candidate candidate
                               :planned-trial-count planned-trial-count
                               :metadata create-metadata)
      (sigil-conflict-error (e)
        ;; Upsert claims an existing run, so a conflict means the backend
        ;; would not reclaim this one. :reopen keeps going with the run as it
        ;; is, but only for a conflict the caller can still work through: a
        ;; terminal or immutable run would take trials and a finalize it has
        ;; already refused.
        (let ((kind (sigil-conflict-error-kind e)))
          (when (or (not (eq on-conflict :reopen))
                    (not (conflict-recoverable-p kind)))
            (error e))
          (sigil-log (client-config client) :warn "experiment"
                     (format nil "run ~a: reopening after a recoverable upsert conflict (~a)"
                             run-id kind)))))
    (%make-experiment-run :client client
                          :run-id run-id
                          :name name
                          :upload upload
                          :dataset dataset
                          :candidate candidate
                          :suite suite
                          :agent-name agent-name
                          :agent-version agent-version
                          :extra-tags extra-tags
                          :extra-metadata extra-metadata)))

(defun %close-error-summary (errors)
  (format nil "trial close failed: ~{~a~^; ~}"
          (mapcar #'princ-to-string errors)))

(defun %call-with-experiment (client body-fn &key run-id name description tags metadata
                                      dataset candidate collection-id agent-name agent-version
                                      extra-tags extra-metadata suite planned-trial-count
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
                                   :suite suite
                                   :planned-trial-count planned-trial-count
                                   :on-conflict on-conflict
                                   :upload upload))
        (exit-kind :aborted)
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
      ;; Trials always close before the run finalizes: the backend refuses to
      ;; finalize a run that still has running trials.
      (let* ((close-errors (%call-swallowing-errors
                            run "closing the run's open trials"
                            (lambda () (experiment-run-close-open-trials run))))
             ;; A trial that would not close means some of its scores never
             ;; reached the backend, so the local count cannot be asserted.
             (score-count (if close-errors nil :accepted)))
        (when close-errors
          (setf error-text
                (format nil "~@[~a; ~]~a" error-text (%close-error-summary close-errors))))
        (case exit-kind
          (:succeeded
           (cond
             ;; A close failure is tested first: it means scores were lost, so
             ;; the run is failed even in manual mode, where reporting a
             ;; buffered count would hide the trials that went with them.
             (close-errors
              (%call-swallowing-errors
               run "finalizing the run as failed"
               (lambda ()
                 (experiment-run-finalize run :status "failed"
                                          :error error-text
                                          :score-count score-count))))
             ((eq (experiment-run-upload run) :manual)
              (when print-url
                (ignore-errors
                  (%experiment-log
                   run :info
                   (format nil "experiment ~a left open (manual mode): ~d score(s) buffered; call experiment-run-publish then experiment-run-finalize"
                           (experiment-run-run-id run)
                           (experiment-run-buffered-score-count run))))))
             (t
              (handler-case
                  (experiment-run-publish run)
                (error (e)
                  (%call-swallowing-errors
                   run "finalizing the run as failed"
                   (lambda ()
                     (experiment-run-finalize run :status "failed"
                                              :error (princ-to-string e))))
                  (error e)))
              (experiment-run-finalize run :status "completed")
              (when print-url
                (ignore-errors
                  (%experiment-log
                   run :info
                   (format nil "experiment ~a finished (~d scores): ~a"
                           (experiment-run-run-id run)
                           (experiment-run-accepted-count run)
                           (experiment-run-url run))))))))
          ;; Errors and non-local exits share this branch. There is no cancel
          ;; route, so an abandoned run finalizes failed rather than staying
          ;; running forever.
          (t
           (%call-swallowing-errors
            run "finalizing the run as failed"
            (lambda ()
              (experiment-run-finalize
               run
               :status "failed"
               :score-count score-count
               :error (or error-text
                          (when (eq exit-kind :aborted)
                            "experiment abandoned by a non-local exit")))))))))))

(defmacro with-experiment ((run client &rest options) &body body)
  "Create or claim an experiment run, bind it, and finalize it on exit.
Open trials are always closed first. On normal exit buffered scores are
published and the run is finalized completed (:upload :manual instead leaves
the run open for the caller to publish and finalize); on an error, and on a
non-local exit, it is finalized failed."
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
                            suite (attempt 1) (on-conflict :reopen)
                            (upload :continuous) (print-url t) (fetch-report t))
  "Run TARGET over dataset ITEMS inside an experiment and export scores.

Each item gets one typed trial, so its scores anchor to the trial and roll
up into the experiment report. Within a trial the run's capture is reset
with a stable per-item conversation id, TARGET is called as
(funcall target item run), and each scorer in SCORERS is called as
(funcall scorer item result) where RESULT is TARGET's return value (or an
empty object when it returns NIL; see MAKE-TARGET-RESULT). Scorers return
lists of score outputs (see MAKE-SCORE). Scores fall back to the generation
ids captured by the run when the target result does not name them.

Items must have distinct ids: two trials for the same (item, ATTEMPT) pair
would mint the same trial id, which is rejected before any request.

Returns a plist with :run-id, :accepted-scores, :url, and :report (NIL
unless FETCH-REPORT is set and the report fetch succeeds)."
  (let ((completed-run nil))
    (with-experiment (run client
                      :run-id run-id :name name :description description
                      :tags tags :metadata metadata :dataset dataset
                      :candidate candidate :collection-id collection-id
                      :agent-name agent-name :agent-version agent-version
                      :extra-tags extra-tags :extra-metadata extra-metadata
                      :suite suite :planned-trial-count (length items)
                      :on-conflict on-conflict
                      :upload upload :print-url print-url)
      (setf completed-run run)
      (dolist (item items)
        (with-trial (trial run item :attempt attempt)
          (let ((conv-id (experiment-run-reset-capture
                          run
                          :conversation-id (stable-id "conv"
                                                      (experiment-run-run-id run)
                                                      (%item-id item)))))
            (trial-bind-conversation trial conv-id))
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
            (trial-bind-conversation trial conv-id)
            (when (= (length generation-ids) 1)
              (trial-bind-generation trial (first generation-ids)))
            (trial-add-scores trial outputs
                              :item item
                              :generation-ids generation-ids
                              :conversation-id conv-id)))))
    (list :run-id (experiment-run-run-id completed-run)
          :accepted-scores (experiment-run-accepted-count completed-run)
          :url (experiment-run-url completed-run)
          :report (when fetch-report
                    (%call-swallowing-errors completed-run "fetching the experiment report"
                                             (lambda ()
                                               (experiment-run-report completed-run)))))))
