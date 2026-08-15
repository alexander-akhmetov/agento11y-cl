(in-package :agento11y-cl)

(define-condition agento11y-error (error)
  ((message :initarg :message :reader agento11y-error-message :initform ""))
  (:report (lambda (c stream)
             (format stream "Agento11y error: ~a" (agento11y-error-message c)))))

(define-condition agento11y-config-error (agento11y-error)
  ()
  (:report (lambda (c stream)
             (format stream "Agento11y config error: ~a" (agento11y-error-message c)))))

(define-condition agento11y-export-error (agento11y-error)
  ((status-code :initarg :status-code :reader agento11y-export-error-status-code
                :initform nil))
  (:report (lambda (c stream)
             (format stream "Agento11y export error (~a): ~a"
                     (or (agento11y-export-error-status-code c) "?")
                     (agento11y-error-message c)))))

(define-condition agento11y-validation-error (agento11y-error)
  ()
  (:report (lambda (c stream)
             (format stream "Agento11y validation error: ~a" (agento11y-error-message c)))))

(define-condition agento11y-not-found-error (agento11y-error)
  ()
  (:report (lambda (c stream)
             (format stream "Agento11y not found error: ~a" (agento11y-error-message c)))))

(define-condition agento11y-conflict-error (agento11y-error)
  ;; KIND classifies the backend's 409 text (see CLASSIFY-CONFLICT). It lets
  ;; callers branch on the conflict without parsing the message themselves.
  ((kind :initarg :kind :reader agento11y-conflict-error-kind :initform :unknown))
  (:report (lambda (c stream)
             (format stream "Agento11y conflict error (~a): ~a"
                     (agento11y-conflict-error-kind c)
                     (agento11y-error-message c)))))

(define-condition agento11y-actor-mismatch-error (agento11y-error)
  ()
  (:report (lambda (c stream)
             (format stream "Agento11y actor mismatch error: ~a" (agento11y-error-message c)))))

(define-condition agento11y-hook-denied-error (agento11y-error)
  ((rule-id     :initarg :rule-id     :reader agento11y-hook-denied-error-rule-id     :initform "")
   (reason      :initarg :reason      :reader agento11y-hook-denied-error-reason      :initform "")
   (evaluations :initarg :evaluations :reader agento11y-hook-denied-error-evaluations :initform nil)
   ;; EVALUATE-HOOK signals instead of returning the denied response, so a
   ;; transformed_input sent with a deny reaches the caller only through this
   ;; slot.
   (transformed-input :initarg :transformed-input
                      :reader agento11y-hook-denied-error-transformed-input
                      :initform nil))
  (:report (lambda (c stream)
             (format stream "Agento11y hook denied (rule ~a): ~a"
                     (agento11y-hook-denied-error-rule-id c)
                     (agento11y-hook-denied-error-reason c)))))

(define-condition agento11y-hook-transport-error (agento11y-error)
  ()
  (:report (lambda (c stream)
             (format stream "Agento11y hook transport error: ~a"
                     (agento11y-error-message c)))))

(define-condition agento11y-experimental-disabled-error (agento11y-error)
  ()
  (:report (lambda (c stream)
             (format stream "Agento11y experimental feature disabled: ~a"
                     (agento11y-error-message c)))))

;; Both evaluation conditions carry the evaluation id: the row survives on the
;; backend, and triggering the same combination again returns that same id, so a
;; caller can resume the wait instead of queueing a second evaluation.
(define-condition agento11y-trial-evaluation-failed-error (agento11y-error)
  ((evaluation-id :initarg :evaluation-id :reader agento11y-trial-evaluation-error-id
                  :initform nil)
   (detail :initarg :detail :reader agento11y-trial-evaluation-error-detail
           :initform nil))
  (:report (lambda (c stream)
             (format stream "Agento11y trial evaluation failed (~a): ~a"
                     (or (agento11y-trial-evaluation-error-id c) "?")
                     (agento11y-error-message c)))))

(define-condition agento11y-trial-evaluation-timeout-error (agento11y-error)
  ((evaluation-id :initarg :evaluation-id :reader agento11y-trial-evaluation-error-id
                  :initform nil))
  (:report (lambda (c stream)
             (format stream "Agento11y trial evaluation timed out (~a): ~a"
                     (or (agento11y-trial-evaluation-error-id c) "?")
                     (agento11y-error-message c)))))
