(in-package :agento11y-cl)

;;; --- Bounded queue ---

(defclass bounded-queue ()
  ((items    :initform nil   :accessor queue-items)
   (count    :initform 0     :accessor queue-count :type fixnum)
   (max-size :initarg :max-size :accessor queue-max-size :initform 500)
   (lock     :initform (bt2:make-lock :name "agento11y-queue") :accessor queue-lock)
   (name     :initarg :name :accessor queue-name :initform "queue")))

(defun make-bounded-queue (&key (max-size 500) (name "queue"))
  (make-instance 'bounded-queue :max-size max-size :name name))

(defun queue-enqueue (queue item)
  "Add ITEM to the queue. Drops oldest on overflow. Returns T."
  (bt2:with-lock-held ((queue-lock queue))
    (push item (queue-items queue))
    (incf (queue-count queue))
    (when (> (queue-count queue) (queue-max-size queue))
      (let ((excess (- (queue-count queue) (queue-max-size queue))))
        (setf (queue-items queue)
              (subseq (queue-items queue) 0 (queue-max-size queue)))
        (decf (queue-count queue) excess)))
    t))

(defun queue-drain-batch (queue batch-size)
  "Remove up to BATCH-SIZE items from the queue (FIFO). Returns list or NIL."
  (bt2:with-lock-held ((queue-lock queue))
    (when (queue-items queue)
      (let* ((all (nreverse (queue-items queue)))
             (n (queue-count queue))
             (batch-end (min batch-size n))
             (batch (subseq all 0 batch-end))
             (rest-count (- n batch-end))
             (rest (when (plusp rest-count)
                     (nreverse (subseq all batch-end)))))
        (setf (queue-items queue) rest)
        (setf (queue-count queue) rest-count)
        batch))))

(defun queue-drain-all (queue)
  "Remove all items from the queue. Returns list or NIL."
  (bt2:with-lock-held ((queue-lock queue))
    (when (queue-items queue)
      (let ((all (nreverse (queue-items queue))))
        (setf (queue-items queue) nil)
        (setf (queue-count queue) 0)
        all))))

(defun queue-length (queue)
  (bt2:with-lock-held ((queue-lock queue))
    (queue-count queue)))

(defun queue-empty-p (queue)
  (bt2:with-lock-held ((queue-lock queue))
    (null (queue-items queue))))
