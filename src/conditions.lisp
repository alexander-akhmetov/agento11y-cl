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

(define-condition sigil-hook-denied-error (sigil-error)
  ((rule-id     :initarg :rule-id     :reader sigil-hook-denied-error-rule-id     :initform "")
   (reason      :initarg :reason      :reader sigil-hook-denied-error-reason      :initform "")
   (evaluations :initarg :evaluations :reader sigil-hook-denied-error-evaluations :initform nil))
  (:report (lambda (c stream)
             (format stream "Sigil hook denied (rule ~a): ~a"
                     (sigil-hook-denied-error-rule-id c)
                     (sigil-hook-denied-error-reason c)))))

(define-condition sigil-hook-transport-error (sigil-error)
  ()
  (:report (lambda (c stream)
             (format stream "Sigil hook transport error: ~a"
                     (sigil-error-message c)))))

(define-condition sigil-experimental-disabled-error (sigil-error)
  ()
  (:report (lambda (c stream)
             (format stream "Sigil experimental feature disabled: ~a"
                     (sigil-error-message c)))))

;; Both evaluation conditions carry the evaluation id: the row survives on the
;; backend, and triggering the same combination again returns that same id, so a
;; caller can resume the wait instead of queueing a second evaluation.
(define-condition sigil-trial-evaluation-failed-error (sigil-error)
  ((evaluation-id :initarg :evaluation-id :reader sigil-trial-evaluation-error-id
                  :initform nil)
   (detail :initarg :detail :reader sigil-trial-evaluation-error-detail
           :initform nil))
  (:report (lambda (c stream)
             (format stream "Sigil trial evaluation failed (~a): ~a"
                     (or (sigil-trial-evaluation-error-id c) "?")
                     (sigil-error-message c)))))

(define-condition sigil-trial-evaluation-timeout-error (sigil-error)
  ((evaluation-id :initarg :evaluation-id :reader sigil-trial-evaluation-error-id
                  :initform nil))
  (:report (lambda (c stream)
             (format stream "Sigil trial evaluation timed out (~a): ~a"
                     (or (sigil-trial-evaluation-error-id c) "?")
                     (sigil-error-message c)))))
