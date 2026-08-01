(in-package :sigil-cl)

(define-condition sigil-error (error)
  ((message :initarg :message :reader sigil-error-message :initform ""))
  (:report (lambda (c stream)
             (format stream "Sigil error: ~a" (sigil-error-message c)))))

(define-condition sigil-config-error (sigil-error)
  ()
  (:report (lambda (c stream)
             (format stream "Sigil config error: ~a" (sigil-error-message c)))))

(define-condition sigil-export-error (sigil-error)
  ((status-code :initarg :status-code :reader sigil-export-error-status-code
                :initform nil))
  (:report (lambda (c stream)
             (format stream "Sigil export error (~a): ~a"
                     (or (sigil-export-error-status-code c) "?")
                     (sigil-error-message c)))))

(define-condition sigil-validation-error (sigil-error)
  ()
  (:report (lambda (c stream)
             (format stream "Sigil validation error: ~a" (sigil-error-message c)))))

(define-condition sigil-not-found-error (sigil-error)
  ()
  (:report (lambda (c stream)
             (format stream "Sigil not found error: ~a" (sigil-error-message c)))))

(define-condition sigil-conflict-error (sigil-error)
  ;; KIND classifies the backend's 409 text (see CLASSIFY-CONFLICT). It lets
  ;; callers branch on the conflict without parsing the message themselves.
  ((kind :initarg :kind :reader sigil-conflict-error-kind :initform :unknown))
  (:report (lambda (c stream)
             (format stream "Sigil conflict error (~a): ~a"
                     (sigil-conflict-error-kind c)
                     (sigil-error-message c)))))

(define-condition sigil-actor-mismatch-error (sigil-error)
  ()
  (:report (lambda (c stream)
             (format stream "Sigil actor mismatch error: ~a" (sigil-error-message c)))))
