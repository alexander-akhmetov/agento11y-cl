(in-package :agento11y-cl)

;;; The agento11y extension of the vendor-neutral layer in otel-genai.lisp, a
;;; port of go/agento11y/otelhook. A GenAI-semconv span carries the
;;; agento11y.* attributes for the generation fields the conventions do not
;;; define; everything the conventions do define (model, usage, messages,
;;; finish reasons) stays on the invocation itself.
;;;
;;; Content reaches the hook already sanitized: BUILD-GENERATION-PAYLOAD ran
;;; the capture-mode stripping and the secret redaction before the adapter
;;; built the invocation, so the hook adds attributes rather than re-running
;;; redaction. It still reads the resolved capture mode itself, because the
;;; span builder cannot inspect what the hook returns and two of the
;;; attributes below carry caller text.

(defparameter +genai-vendor-attr-record+ "agento11y.record")
(defparameter +genai-vendor-attr-generation-id+ "agento11y.generation.id")
(defparameter +genai-vendor-attr-parent-generation-ids+
  "agento11y.generation.parent_generation_ids")
(defparameter +genai-vendor-attr-tags+ "agento11y.generation.tags")
(defparameter +genai-vendor-attr-metadata+ "agento11y.generation.metadata")
(defparameter +genai-vendor-attr-conversation-title+ "agento11y.conversation.title")
(defparameter +genai-vendor-attr-tool-choice+ "agento11y.gen_ai.request.tool_choice")
(defparameter +genai-vendor-attr-thinking-enabled+
  "agento11y.gen_ai.request.thinking.enabled")
(defparameter +genai-vendor-attr-total-tokens+ "agento11y.gen_ai.usage.total_tokens")
(defparameter +genai-vendor-attr-user-id+ "user.id")
(defparameter +genai-vendor-attr-error-category+ "error.category")

(defclass genai-vendor-generation ()
  ((id :initarg :id :accessor genai-vendor-id :initform nil)
   (user-id :initarg :user-id :accessor genai-vendor-user-id :initform nil)
   ;; Caller text, so it goes out only under content capture.
   (conversation-title :initarg :conversation-title
                       :accessor genai-vendor-conversation-title :initform nil)
   ;; Both are (key . value) alists rather than hash tables, so the JSON
   ;; documents below come out in a fixed key order.
   (tags :initarg :tags :accessor genai-vendor-tags :initform nil)
   (metadata :initarg :metadata :accessor genai-vendor-metadata :initform nil)
   (parent-generation-ids :initarg :parent-generation-ids
                          :accessor genai-vendor-parent-generation-ids :initform nil)
   (tool-choice :initarg :tool-choice :accessor genai-vendor-tool-choice :initform nil)
   ;; T, NIL, or :UNSET, matching GEN-REC-THINKING-ENABLED.
   (thinking-enabled :initarg :thinking-enabled
                     :accessor genai-vendor-thinking-enabled :initform :unset)
   (total-tokens :initarg :total-tokens :accessor genai-vendor-total-tokens :initform 0)
   (error-category :initarg :error-category
                   :accessor genai-vendor-error-category :initform nil)))

(defun make-genai-vendor-generation (&rest args)
  (apply #'make-instance 'genai-vendor-generation args))

;;; Two attributes the Go hook emits have no source here and are deliberately
;;; absent rather than invented: agento11y.agent.effective_version, because
;;; this SDK has no effective-version concept, and agento11y.raw_artifacts,
;;; because it emits raw_artifacts as an always-empty vector.
;;; gen_ai.token.semantics is the same case: this SDK has no token input
;;; semantics to report.

(defun %genai-vendor-nested-json (value)
  "VALUE as a pre-encoded JSON document, or NIL when it is not one the JSON
library can render. Caller metadata is merged into the payload map unread, so a
value can be a nested object or array; PRINC-TO-STRING would put a heap address
on the wire for one, which is neither the value nor stable across runs."
  (let ((renderable (typecase value
                      (hash-table value)
                      (string nil)
                      (vector value)
                      (cons (handler-case (coerce value 'vector) (error () nil)))
                      (t nil))))
    (when renderable
      (handler-case (genai-raw-json (jzon:stringify renderable))
        (error () nil)))))

(defun %genai-vendor-json-document (alist)
  "ALIST as a compact JSON object, in the order given. A nested object or array
is written as itself; anything else the JSON library cannot render is rendered
as a string, which is what the generation payload's own tags map holds."
  (genai-json
   (cons :object
         (mapcar (lambda (pair)
                   (cons (car pair)
                         (let ((value (cdr pair)))
                           (typecase value
                             (string value)
                             (integer value)
                             (double-float value)
                             (single-float value)
                             (null :false)
                             (t (cond
                                  ((eq value t) :true)
                                  ((and (symbolp value) (string= (symbol-name value) "NULL"))
                                   :null)
                                  (t (or (%genai-vendor-nested-json value)
                                         (princ-to-string value)))))))))
                 alist))))

(defun genai-vendor-metadata-document (metadata)
  "METADATA without the keys the SDK mirrors content into.
The strip runs in every capture mode, not only a redacting one: a reducing
forwarder keeps the document and deletes by key, so content nested in it would
otherwise survive."
  (remove-if (lambda (pair)
               (member (car pair) +content-metadata-keys+ :test #'string=))
             metadata))

(defun genai-vendor-conversation-title-value (vendor)
  "The conversation title, falling back to the metadata mirrors where a
generation filled from the payload shape carries it."
  (or (genai-vendor-conversation-title vendor)
      (loop for key in '("agento11y.conversation.title" "sigil.conversation.title")
            for hit = (cdr (assoc key (genai-vendor-metadata vendor) :test #'string=))
            when (and (stringp hit) (plusp (length hit))) return hit)))

(defun genai-vendor-attributes (vendor capture)
  "OTLP attributes for the agento11y fields the conventions do not define.
CAPTURE gates the content-bearing ones."
  (when (null vendor)
    (return-from genai-vendor-attributes nil))
  (let ((attrs nil)
        (with-content (genai-capture-span-content-p capture))
        (id (genai-vendor-id vendor)))
    (when (plusp (length (or id "")))
      ;; agento11y.record is what makes the backend store a generation, and it
      ;; is meaningless without the id, so the two go on together.
      (push (otel-string-attr +genai-vendor-attr-record+ "true") attrs)
      (push (otel-string-attr +genai-vendor-attr-generation-id+ id) attrs))
    (let ((user-id (genai-vendor-user-id vendor)))
      (when (plusp (length (or user-id "")))
        (push (otel-string-attr +genai-vendor-attr-user-id+ user-id) attrs)))
    (let ((total (genai-vendor-total-tokens vendor)))
      (when (and (integerp total) (/= 0 total))
        (push (otel-int-attr +genai-vendor-attr-total-tokens+ total) attrs)))
    (let ((choice (genai-vendor-tool-choice vendor)))
      (when (plusp (length (or choice "")))
        (push (otel-string-attr +genai-vendor-attr-tool-choice+ choice) attrs)))
    (let ((thinking (genai-vendor-thinking-enabled vendor)))
      (unless (eq thinking :unset)
        (push (otel-bool-attr +genai-vendor-attr-thinking-enabled+ thinking) attrs)))
    (let ((parents (genai-vendor-parent-generation-ids vendor)))
      (when parents
        (push (otel-string-array-attr +genai-vendor-attr-parent-generation-ids+
                                      (coerce parents 'list))
              attrs)))
    (let ((tags (genai-vendor-tags vendor)))
      (when tags
        ;; The document keeps the tags map as the payload holds it. The
        ;; agento11y.tag.* dimensions trim it: the capture-mode marker is an
        ;; SDK-owned key that the native span path deliberately keeps off
        ;; every span, and republishing it as a dimension would add a series
        ;; that says nothing about the caller.
        (push (otel-string-attr +genai-vendor-attr-tags+
                                (%genai-vendor-json-document tags))
              attrs)
        (dolist (kv (prefixed-tag-pairs
                     (remove +content-capture-mode-key+ tags
                             :key #'car :test #'string=)))
          (push (otel-string-attr (car kv) (cdr kv)) attrs))))
    (when with-content
      (let ((title (genai-vendor-conversation-title-value vendor)))
        (when (plusp (length (or title "")))
          (push (otel-string-attr +genai-vendor-attr-conversation-title+ title) attrs))))
    (let ((metadata (genai-vendor-metadata-document (genai-vendor-metadata vendor))))
      (when metadata
        (push (otel-string-attr +genai-vendor-attr-metadata+
                                (%genai-vendor-json-document metadata))
              attrs)))
    (let ((category (genai-vendor-error-category vendor)))
      (when (plusp (length (or category "")))
        (push (otel-string-attr +genai-vendor-attr-error-category+ category) attrs)))
    (nreverse attrs)))

(defun genai-vendor-config-attributes (config &key error-category)
  "The client-level agento11y attributes a non-generation span carries in otel
mode: the user id, the agento11y.tag.* dimensions, and the error category.

agento11y.sdk.name is deliberately absent, matching the generation span, whose
shape the golden fixtures pin."
  (let ((attrs nil))
    (let ((uid (config-user-id config)))
      (when uid
        (let ((text (princ-to-string uid)))
          (when (plusp (length text))
            (push (otel-string-attr +genai-vendor-attr-user-id+ text) attrs)))))
    (dolist (kv (prefixed-tag-pairs (config-tags config)))
      (push (otel-string-attr (car kv) (cdr kv)) attrs))
    (when (plusp (length (or error-category "")))
      (push (otel-string-attr +genai-vendor-attr-error-category+ error-category) attrs))
    (nreverse attrs)))
