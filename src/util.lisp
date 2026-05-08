(in-package :sigil-cl)

;;; --- Epoch constant ---

(defvar +unix-epoch-universal+
  (encode-universal-time 0 0 0 1 1 1970 0)
  "Universal time at Unix epoch (1970-01-01T00:00:00Z).")

;;; --- Timestamps ---

(defun iso8601-now ()
  "Return current UTC time as ISO 8601 string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour min sec)))

(defun iso8601-to-unix-nano (iso-string)
  "Convert ISO 8601 timestamp to Unix nanoseconds as a string.
Parses fractional seconds if present. Returns NIL for invalid input."
  (when (and (stringp iso-string) (>= (length iso-string) 20))
    (handler-case
        (let ((year   (parse-integer iso-string :start 0 :end 4))
              (month  (parse-integer iso-string :start 5 :end 7))
              (day    (parse-integer iso-string :start 8 :end 10))
              (hour   (parse-integer iso-string :start 11 :end 13))
              (minute (parse-integer iso-string :start 14 :end 16))
              (second (parse-integer iso-string :start 17 :end 19))
              (frac-nano 0))
          (when (and (> (length iso-string) 19)
                     (char= (char iso-string 19) #\.))
            (let* ((frac-start 20)
                   (frac-end (or (position-if (lambda (c) (not (digit-char-p c)))
                                              iso-string :start frac-start)
                                 (length iso-string)))
                   (frac-str (subseq iso-string frac-start frac-end))
                   (padded (subseq (concatenate 'string frac-str "000000000") 0 9)))
              (setf frac-nano (parse-integer padded))))
          (let* ((ut (encode-universal-time second minute hour day month year 0))
                 (unix-sec (- ut +unix-epoch-universal+))
                 (unix-nano (+ (* unix-sec 1000000000) frac-nano)))
            (format nil "~d" unix-nano)))
      (error () nil))))

(defun current-unix-nano ()
  "Return current wall-clock time as a Unix nanosecond string."
  #+sbcl
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (format nil "~d" (+ (* sec 1000000000) (* usec 1000))))
  #-sbcl
  (let* ((ut (get-universal-time))
         (unix-sec (- ut +unix-epoch-universal+)))
    (format nil "~d" (* unix-sec 1000000000))))

(defun unix-nano-plus-seconds (start-nano-str duration-seconds)
  "Add DURATION-SECONDS to START-NANO-STR, return nanosecond string."
  (if (and start-nano-str duration-seconds (plusp duration-seconds))
      (let* ((start (parse-integer start-nano-str))
             (delta (round (* duration-seconds 1.0d9))))
        (format nil "~d" (+ start delta)))
      (or start-nano-str "0")))

;;; --- ID generation (thread-safe) ---

(defvar *id-counter* 0)
(defvar *id-lock* (bt2:make-lock :name "sigil-id"))
(defvar *id-random-state* (make-random-state t))

(defun generate-id ()
  "Generate a unique generation ID like gen_<hex>."
  (let ((ts (get-universal-time))
        (seq (bt2:with-lock-held (*id-lock*)
               (incf *id-counter*))))
    (format nil "gen_~8,'0x~4,'0x" ts (mod seq #xFFFF))))

(defun generate-workflow-step-id ()
  "Generate a unique workflow step ID like wfs_<hex>."
  (let ((ts (get-universal-time))
        (seq (bt2:with-lock-held (*id-lock*)
               (incf *id-counter*))))
    (format nil "wfs_~8,'0x~4,'0x" ts (mod seq #xFFFF))))

(defun generate-trace-id ()
  "Generate a 32-hex-char trace ID (128-bit random)."
  (bt2:with-lock-held (*id-lock*)
    (format nil "~32,'0x" (random (expt 2 128) *id-random-state*))))

(defun generate-span-id ()
  "Generate a 16-hex-char span ID (64-bit random)."
  (bt2:with-lock-held (*id-lock*)
    (format nil "~16,'0x" (random (expt 2 64) *id-random-state*))))

;;; --- Backoff ---

(defun backoff-seconds (attempt initial-sec max-sec)
  "Exponential backoff: initial * 2^attempt, capped at max."
  (min max-sec (* initial-sec (expt 2 attempt))))

;;; --- Internal logging ---

(defun sigil-log (config level component message &rest kvs)
  "Log via config's log-fn callback if set."
  (let ((log-fn (when config (config-log-fn config))))
    (when log-fn
      (apply log-fn level component message kvs))))
