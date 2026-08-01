(in-package :sigil-cl)

;;; Local test suites.
;;;
;;; Deliberately minimal. There is no YAML loader (sigil-cl has no YAML
;;; dependency, and every dependency here becomes a vendored dependency
;;; downstream) and no stored-suite control plane (nine routes, a second host,
;;; and a Grafana service-account token, none of which a local evaluation
;;; needs). Suites are built in Lisp from plists, alists, or jzon-parsed JSON.
;;;
;;; Evaluator provenance travels as the :evaluator-id and :evaluator-version
;;; strings on MAKE-SCORE; there is no evaluator value here because no route
;;; accepts one.

;;; --- Test cases and suites ---

(defstruct (test-case (:constructor %make-test-case))
  (test-case-id "" :type string)
  (name "")
  (description "")
  (tags nil)
  (category "")
  (input nil)
  (expected nil)
  (metadata nil))

(defstruct (test-suite (:constructor %make-test-suite))
  (suite-id "" :type string)
  (version "" :type string)
  (name "")
  (description "")
  (cases nil)
  ;; test-case-id -> case, so a run with one trial per case does not rescan
  ;; the whole suite for every trial.
  (cases-by-id (make-hash-table :test 'equal)))

(defun %string-list (value)
  "Normalize VALUE to a list of strings. Accepts a string, list, or vector."
  (cond
    ((null value) nil)
    ((stringp value) (list value))
    ((vectorp value) (map 'list #'%text value))
    ((listp value) (mapcar #'%text value))
    (t (list (%text value)))))

(defun make-test-case (&key id test-case-id name description tags category
                            input expected metadata)
  "Build a TEST-CASE. ID and TEST-CASE-ID are interchangeable."
  (let ((case-id (%trimmed-text (or (unless (%blank-string-p test-case-id) test-case-id)
                                    id))))
    (when (zerop (length case-id))
      (error 'sigil-validation-error :message "test case requires an id"))
    (%make-test-case :test-case-id case-id
                     :name (%text name)
                     :description (%text description)
                     :tags (%string-list tags)
                     :category (%text category)
                     :input input
                     :expected expected
                     :metadata metadata)))

(defun %coerce-test-case (value)
  "Accept a TEST-CASE, or build one from a plist, alist, or parsed JSON object."
  (if (test-case-p value)
      value
      (make-test-case :id (%metadata-field value "id")
                      :test-case-id (%metadata-field value "test_case_id")
                      :name (%metadata-field value "name")
                      :description (%metadata-field value "description")
                      :tags (%metadata-field value "tags")
                      :category (%metadata-field value "category")
                      :input (%metadata-field value "input")
                      :expected (%metadata-field value "expected")
                      :metadata (%metadata-field value "metadata"))))

(defun make-test-suite (&key suite-id version name description cases)
  "Build a TEST-SUITE. CASES accepts TEST-CASE values or anything
%COERCE-TEST-CASE understands (plists, alists, jzon-parsed objects)."
  (let ((id (%trimmed-text suite-id)))
    (when (zerop (length id))
      (error 'sigil-validation-error :message "suite_id is required"))
    (let* ((coerced (mapcar #'%coerce-test-case
                            (if (vectorp cases) (coerce cases 'list) cases)))
           (index (make-hash-table :test 'equal)))
      ;; The first case wins, so the index agrees with a linear scan.
      (dolist (c coerced)
        (let ((case-id (test-case-test-case-id c)))
          (unless (gethash case-id index)
            (setf (gethash case-id index) c))))
      (%make-test-suite :suite-id id
                        :version (%trimmed-text version)
                        :name (%text name)
                        :description (%text description)
                        :cases coerced
                        :cases-by-id index))))

(defun test-suite-case (suite test-case-id)
  "Return the case in SUITE with TEST-CASE-ID, or NIL."
  (when suite
    (gethash (%trimmed-text test-case-id) (test-suite-cases-by-id suite))))

;;; --- Trial snapshots ---

(defun %object-value (value)
  "Coerce VALUE to a JSON object. The trial snapshot's `input` and `expected`
are always objects on the wire, so a scalar is wrapped as {\"value\": x}.
Mirrors object_value in agento11y experiments/experiment.py:372-402."
  (cond
    ((null value) (jobj))
    ((hash-table-p value) (%jsonify value))
    ;; An alist or a plist is a mapping; a plain list is not.
    ((and (listp value) (every #'consp value)) (%jsonify value))
    ((%plist-p value) (%jsonify value))
    (t (jobj "value" (%jsonify value)))))

(defun test-case-snapshot (case &key suite-id suite-version)
  "Serialize CASE as the immutable `test_case` object stored with a trial."
  (when case
    (let ((out (jobj "test_case_id" (test-case-test-case-id case)
                     "name" (test-case-name case)
                     "description" (test-case-description case)
                     "tags" (coerce (test-case-tags case) 'vector)
                     "category" (test-case-category case)
                     "input" (%object-value (test-case-input case))
                     "expected" (%object-value (test-case-expected case)))))
      (unless (%blank-string-p suite-id)
        (setf (gethash "suite_id" out) (%trimmed-text suite-id)))
      (unless (%blank-string-p suite-version)
        (setf (gethash "suite_version" out) (%trimmed-text suite-version)))
      (let ((metadata (test-case-metadata case)))
        (when metadata
          (let ((json (%jsonify metadata)))
            (when (and (hash-table-p json) (plusp (hash-table-count json)))
              (setf (gethash "metadata" out) json)))))
      out)))
