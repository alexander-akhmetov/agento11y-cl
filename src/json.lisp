(in-package :agento11y-cl)

(defun jobj (&rest pairs)
  "Build a JSON object (hash-table) from alternating key value pairs."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun jarr (&rest items)
  "Build a JSON array (simple-vector) from items."
  (coerce items 'simple-vector))

(defun jget (obj key)
  "Get value from parsed JSON object by string key."
  (gethash key obj))

(defun jget* (obj &rest keys)
  "Nested lookup in parsed JSON."
  (reduce (lambda (o k)
            (when o
              (typecase k
                (integer (when (vectorp o) (aref o k)))
                (t (gethash k o)))))
          keys :initial-value obj))
