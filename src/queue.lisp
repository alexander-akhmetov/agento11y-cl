(in-package :agento11y-cl)

;;; --- Bounded queue ---

(defclass bounded-queue ()
  ((items    :initform nil   :accessor queue-items)
   (count    :initform 0     :accessor queue-count :type fixnum)
   (max-size :initarg :max-size :accessor queue-max-size :initform 500)
   (lock     :initform (bt2:make-lock :name "agento11y-queue") :accessor queue-lock)
   (capacity-cv :initform (bt2:make-condition-variable :name "agento11y-capacity")
                :reader queue-capacity-cv)
   (dropped :initform 0 :accessor queue-dropped)
   (enqueue-timeouts :initform 0 :accessor queue-enqueue-timeouts)
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
        (decf (queue-count queue) excess)
        (incf (queue-dropped queue) excess)))
    t))

(defun monotonic-deadline (timeout-sec)
  (when timeout-sec
    (check-type timeout-sec (real 0))
    (+ (get-internal-real-time)
       (* timeout-sec internal-time-units-per-second))))

(defun deadline-remaining (deadline)
  (when deadline
    (max 0 (/ (- deadline (get-internal-real-time))
              (float internal-time-units-per-second 1d0)))))

(defun queue-enqueue-wait (queue item timeout-sec)
  "Wait for capacity without evicting an item. Return NIL on timeout."
  (let ((deadline (monotonic-deadline timeout-sec)))
    (bt2:with-lock-held ((queue-lock queue))
      (loop while (>= (queue-count queue) (queue-max-size queue))
            for remaining = (deadline-remaining deadline)
            do (when (and remaining (zerop remaining))
                 (incf (queue-enqueue-timeouts queue))
                 (return-from queue-enqueue-wait nil))
               (bt2:condition-wait (queue-capacity-cv queue) (queue-lock queue)
                                   :timeout remaining))
      (push item (queue-items queue))
      (incf (queue-count queue))
      t)))

(defun queue-loss-diagnostics (queue)
  (bt2:with-lock-held ((queue-lock queue))
    (when (plusp (+ (queue-dropped queue) (queue-enqueue-timeouts queue)))
      (list :queue (queue-name queue) :dropped (queue-dropped queue)
            :enqueue-timeouts (queue-enqueue-timeouts queue)))))

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
        (when (plusp batch-end)
          (bt2:condition-broadcast (queue-capacity-cv queue)))
        batch))))

(defun queue-drain-all (queue)
  "Remove all items from the queue. Returns list or NIL."
  (bt2:with-lock-held ((queue-lock queue))
    (when (queue-items queue)
      (let ((all (nreverse (queue-items queue))))
        (setf (queue-items queue) nil)
        (setf (queue-count queue) 0)
        (bt2:condition-broadcast (queue-capacity-cv queue))
        all))))

(defun queue-length (queue)
  (bt2:with-lock-held ((queue-lock queue))
    (queue-count queue)))

(defun queue-empty-p (queue)
  (bt2:with-lock-held ((queue-lock queue))
    (null (queue-items queue))))
