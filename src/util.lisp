(in-package :agento11y-cl)

;;; --- Epoch constant ---

(defvar +unix-epoch-universal+
  (encode-universal-time 0 0 0 1 1 1970 0)
  "Universal time at Unix epoch (1970-01-01T00:00:00Z).")

;;; --- Timestamps ---

(defun iso8601-now ()
  "Return current UTC time as an ISO 8601 string with millisecond precision.

The fraction is load-bearing rather than cosmetic. A recorder's started-at and
completed-at are what the backend derives a generation's latency from, so at
whole-second resolution every call shorter than a second reports zero, and a
caller that records post-hoc (opening the recorder once the call has returned)
reports zero for every call at any length. The other agento11y SDKs all send
sub-second timestamps.

Implementations without SB-EXT:GET-TIME-OF-DAY keep whole-second accuracy but
still emit the .000 fraction, so the string has one length everywhere."
  #+sbcl
  (multiple-value-bind (unix-sec usec) (sb-ext:get-time-of-day)
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time (+ unix-sec +unix-epoch-universal+) 0)
      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.~3,'0dZ"
              year month day hour min sec (floor usec 1000))))
  #-sbcl
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.000Z"
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
(defvar *id-lock* (bt2:make-lock :name "agento11y-id"))
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
  "Generate a 32-hex-char lowercase trace ID (128-bit random).

Lowercase because the W3C traceparent header a peer receives is lowercase by
specification, and the exported span has to carry the identifier that header
named or the two never join in the backend. ~X alone formats uppercase."
  (bt2:with-lock-held (*id-lock*)
    (format nil "~(~32,'0x~)" (random (expt 2 128) *id-random-state*))))

(defun generate-span-id ()
  "Generate a 16-hex-char lowercase span ID (64-bit random).
Lowercase for the reason GENERATE-TRACE-ID gives."
  (bt2:with-lock-held (*id-lock*)
    (format nil "~(~16,'0x~)" (random (expt 2 64) *id-random-state*))))

(defun condition-status-message (condition)
  "CONDITION rendered for a span's status description, never signalling.

PRINC-TO-STRING runs the condition's report method, which is arbitrary code
belonging to whichever library raised it: dexador's HTTP-REQUEST-FAILED reader
signals when the condition carries no response, and a span built around such a
call would take the whole request down while recording telemetry about it. The
type name is the fallback, which is what this used to report unconditionally."
  (or (ignore-errors (princ-to-string condition))
      (ignore-errors (princ-to-string (type-of condition)))
      "error"))

(defun trace-hex-id-p (value width)
  "True when VALUE is a WIDTH-character trace or span identifier.
Both cases are accepted on the way in: what a caller hands over comes from
another tracer, and the specification only constrains what goes out. This SDK's
own identifiers are lowercase."
  (and (stringp value)
       (= (length value) width)
       (every (lambda (ch)
                (or (char<= #\0 ch #\9)
                    (char<= #\a ch #\f)
                    (char<= #\A ch #\F)))
              value)))

;;; --- UTF-8 + SHA-1 ---
;;;
;;; Hand-rolled because stable experiment ids must match the SHA-1-based
;;; StableID in the Go/Python SDKs byte-for-byte, and the project keeps its
;;; dependency list small. Not used for anything security-sensitive.

(defun string-to-utf8-octets (string)
  "Encode STRING as UTF-8 octets. Assumes CHAR-CODE returns the Unicode
codepoint (true on SBCL, CCL, ECL)."
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8)
                                         :adjustable t :fill-pointer 0)))
    (loop for ch across string
          for code = (char-code ch)
          do (cond
               ((< code #x80)
                (vector-push-extend code out))
               ((< code #x800)
                (vector-push-extend (logior #xC0 (ash code -6)) out)
                (vector-push-extend (logior #x80 (logand code #x3F)) out))
               ((< code #x10000)
                (vector-push-extend (logior #xE0 (ash code -12)) out)
                (vector-push-extend (logior #x80 (logand (ash code -6) #x3F)) out)
                (vector-push-extend (logior #x80 (logand code #x3F)) out))
               (t
                (vector-push-extend (logior #xF0 (ash code -18)) out)
                (vector-push-extend (logior #x80 (logand (ash code -12) #x3F)) out)
                (vector-push-extend (logior #x80 (logand (ash code -6) #x3F)) out)
                (vector-push-extend (logior #x80 (logand code #x3F)) out))))
    out))

(defun sha1-hex (octets)
  "Return the SHA-1 digest of OCTETS as a lowercase hex string."
  (let* ((msg-len (length octets))
         (bit-len (* 8 msg-len))
         ;; message + 0x80 byte + zero padding + 8-byte big-endian bit length,
         ;; rounded up to a multiple of 64 bytes.
         (total (* 64 (ceiling (+ msg-len 9) 64)))
         (buf (make-array total :element-type '(unsigned-byte 8) :initial-element 0))
         (w (make-array 80 :element-type '(unsigned-byte 32)))
         (h0 #x67452301) (h1 #xEFCDAB89) (h2 #x98BADCFE)
         (h3 #x10325476) (h4 #xC3D2E1F0))
    (replace buf octets)
    (setf (aref buf msg-len) #x80)
    (loop for i from 0 below 8
          do (setf (aref buf (- total 1 i))
                   (logand (ash bit-len (* -8 i)) #xFF)))
    (flet ((rotl (x n)
             (logand #xFFFFFFFF (logior (ash x n) (ash x (- n 32))))))
      (loop for chunk from 0 below total by 64
            do (loop for i from 0 below 16
                     do (setf (aref w i)
                              (logior (ash (aref buf (+ chunk (* 4 i))) 24)
                                      (ash (aref buf (+ chunk (* 4 i) 1)) 16)
                                      (ash (aref buf (+ chunk (* 4 i) 2)) 8)
                                      (aref buf (+ chunk (* 4 i) 3)))))
               (loop for i from 16 below 80
                     do (setf (aref w i)
                              (rotl (logxor (aref w (- i 3)) (aref w (- i 8))
                                            (aref w (- i 14)) (aref w (- i 16)))
                                    1)))
               (let ((a h0) (b h1) (c h2) (d h3) (e h4))
                 (loop for i from 0 below 80
                       do (multiple-value-bind (f k)
                              (cond
                                ((< i 20) (values (logior (logand b c) (logandc1 b d))
                                                  #x5A827999))
                                ((< i 40) (values (logxor b c d) #x6ED9EBA1))
                                ((< i 60) (values (logior (logand b c) (logand b d)
                                                          (logand c d))
                                                  #x8F1BBCDC))
                                (t (values (logxor b c d) #xCA62C1D6)))
                            (let ((tmp (logand #xFFFFFFFF
                                               (+ (rotl a 5) f e k (aref w i)))))
                              (setf e d
                                    d c
                                    c (rotl b 30)
                                    b a
                                    a tmp))))
                 (setf h0 (logand #xFFFFFFFF (+ h0 a))
                       h1 (logand #xFFFFFFFF (+ h1 b))
                       h2 (logand #xFFFFFFFF (+ h2 c))
                       h3 (logand #xFFFFFFFF (+ h3 d))
                       h4 (logand #xFFFFFFFF (+ h4 e))))))
    (format nil "~(~8,'0x~8,'0x~8,'0x~8,'0x~8,'0x~)" h0 h1 h2 h3 h4)))

;;; --- Deterministic ids ---

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
over the parts joined with #\\Us), so reruns from another SDK dedupe to the
same score and conversation ids. Cross-SDK parity holds for string parts;
non-string parts are printed with PRINC-TO-STRING, whose output (e.g. for
booleans and floats) differs from Go fmt.Sprint and Python str."
  (let ((joined (%join-stable-id-parts parts)))
    (format nil "~a-~a" prefix
            (subseq (sha1-hex (string-to-utf8-octets joined)) 0 16))))

;;; --- Backoff ---

(defun backoff-seconds (attempt initial-sec max-sec)
  "Exponential backoff: initial * 2^attempt, capped at max."
  (min max-sec (* initial-sec (expt 2 attempt))))

;;; --- Internal logging ---

(defun agento11y-log (config level component message &rest kvs)
  "Log via config's log-fn callback if set."
  (let ((log-fn (when config (config-log-fn config))))
    (when log-fn
      (apply log-fn level component message kvs))))
