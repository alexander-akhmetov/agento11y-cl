(in-package :agento11y-cl)

;;; OpenTelemetry GenAI semantic conventions: the vendor-neutral core.
;;;
;;; This file is a port of go/otelgenai (invocation.go, content.go, genai.go,
;;; metrics.go). It knows the conventions and nothing else: no SDK
;;; configuration, no vendor attribute names, no recorder types. A vendor that
;;; needs its own attributes builds them elsewhere and hands them in through
;;; the invocation's EXTRA-ATTRIBUTES slot. The suite pins that boundary, the
;;; way go/otelgenai/boundary_test.go pins the import set.
;;;
;;; The divergence from Go worth stating: Go drives the OTel API and needs the
;;; application's TracerProvider, a span handle and a Start/End lifecycle. This
;;; SDK owns its exporter, so the layer here is a pure function from an
;;; invocation to an OTLP span object, which then rides the existing trace
;;; queue.

(defparameter +genai-scope-name+ "otel-genai-cl"
  "Instrumentation scope name for the telemetry this file builds.
EXPORT-TRACES declares it on the OTLP envelope in otel generation mode, where
every span comes from here. The SDK's own name would be wrong there: what a
receiver needs from the scope is which instrumentation wrote the attributes, and
that is this layer.")

(defparameter +genai-version+ "0.1.0"
  "Instrumentation version reported alongside +GENAI-SCOPE-NAME+.")

(defparameter +genai-schema-url+ "https://opentelemetry.io/schemas/1.41.0"
  "Semantic-convention schema the emitted telemetry follows, declared on the
OTLP envelope in otel generation mode.
v1.41.0 is the last core semconv release that carried the GenAI conventions.
They now live in open-telemetry/semantic-conventions-genai, which has no
release and whose README leaves its schema URL open, so there is no newer URL
to point at. A few attributes below were added after v1.41.0 and are therefore
conventions the declared schema does not describe.")

(defparameter +genai-error-type-other+ "_OTHER"
  "The conventions' fallback error.type classification.")

;;; --- Ordered JSON writing ---
;;;
;;; The conventions pin the key order of the message schema, and the golden
;;; fixtures compare the encoded attribute as a string, so the encoders below
;;; write their objects field by field instead of handing a hash table to the
;;; JSON library, whose iteration order is not part of its contract.
;;;
;;; The output is compact, with no space after a separator. Unlike Go's
;;; encoding/json it leaves <, > and & unescaped; both spellings decode to the
;;; same string, and the unescaped one is what a reader sees.

(defstruct (genai-raw-json (:constructor genai-raw-json (text)))
  "A pre-encoded JSON document spliced into the output as-is."
  text)

(defun genai-write-json-string (string stream)
  (write-char #\" stream)
  (loop for ch across string
        do (cond
             ((char= ch #\") (write-string "\\\"" stream))
             ((char= ch #\\) (write-string "\\\\" stream))
             ((char= ch #\Newline) (write-string "\\n" stream))
             ((char= ch #\Return) (write-string "\\r" stream))
             ((char= ch #\Tab) (write-string "\\t" stream))
             ((char= ch #\Backspace) (write-string "\\b" stream))
             ((char= ch #\Page) (write-string "\\f" stream))
             ((< (char-code ch) #x20)
              (format stream "\\u~4,'0x" (char-code ch)))
             (t (write-char ch stream))))
  (write-char #\" stream))

(defun genai-write-json (value stream)
  "Write VALUE as compact JSON.
VALUE is a string, an integer, a real, :TRUE, :FALSE, :NULL, a GENAI-RAW-JSON,
an ordered object (:OBJECT (key . value) ...), or an array (:ARRAY value ...)."
  (typecase value
    (string (genai-write-json-string value stream))
    (integer (format stream "~d" value))
    (genai-raw-json (write-string (genai-raw-json-text value) stream))
    (real (let ((*read-default-float-format* 'double-float))
            (format stream "~a" (float value 1d0))))
    (t
     (cond
       ((eq value :true) (write-string "true" stream))
       ((eq value :false) (write-string "false" stream))
       ((eq value :null) (write-string "null" stream))
       ((and (consp value) (eq (car value) :object))
        (write-char #\{ stream)
        (loop for (key . item) in (cdr value)
              for first = t then nil
              do (unless first (write-char #\, stream))
                 (genai-write-json-string key stream)
                 (write-char #\: stream)
                 (genai-write-json item stream))
        (write-char #\} stream))
       ((and (consp value) (eq (car value) :array))
        (write-char #\[ stream)
        (loop for item in (cdr value)
              for first = t then nil
              do (unless first (write-char #\, stream))
                 (genai-write-json item stream))
        (write-char #\] stream))
       (t (error "cannot encode ~s as JSON" value))))))

(defun genai-json (value)
  "VALUE as a compact JSON string. See GENAI-WRITE-JSON for the value forms."
  (with-output-to-string (out) (genai-write-json value out)))

(defun genai-valid-json-p (text)
  "True when TEXT parses as JSON."
  (and (stringp text)
       (plusp (length text))
       (handler-case (progn (jzon:parse text) t)
         (error () nil))))

;;; --- Capture vocabulary ---
;;;
;;; Deliberately not the SDK's own +CONTENT-CAPTURE-MODES+: this one is the
;;; conventions' vocabulary, and a vendor translates between the two.

(defparameter +genai-capture-modes+
  '(:no-content :span-only :event-only :span-and-event)
  "The conventions' content capture modes. The exporter reads a mode from the
SDK's own configuration, never from
OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT: a traces-side variable must
not widen a content policy the SDK set. That is why only the first two are
produced here, and why nothing parses the conventions' spelling of them.")

(defun genai-capture-span-content-p (mode)
  "True when MODE puts message content on the span."
  (and (member mode '(:span-only :span-and-event)) t))

;;; --- Wire model ---
;;;
;;; These classes are the conventions' message schema, kept apart from the SDK
;;; model in types.lisp: that one serializes the proprietary payload, and
;;; conflating the two would couple the two wire formats.
;;;
;;; NIL means unset. Where the schema requires a key, the encoder emits an
;;; unset value as an empty string rather than omitting the key. Modality is
;;; the exception: its enum has no empty member, so an unset modality is
;;; omitted and reported.

(defclass genai-part ()
  ((part-type :initarg :part-type :accessor genai-part-type :initform nil)
   (content   :initarg :content   :accessor genai-part-content :initform nil)
   (id        :initarg :id        :accessor genai-part-id :initform nil)
   (name      :initarg :name      :accessor genai-part-name :initform nil)
   ;; Raw JSON documents, as text. ARGUMENTS carries tool_call and
   ;; server_tool_call payloads, RESPONSE the two response shapes.
   (arguments :initarg :arguments :accessor genai-part-arguments :initform nil)
   (response  :initarg :response  :accessor genai-part-response :initform nil)
   (mime-type :initarg :mime-type :accessor genai-part-mime-type :initform nil)
   (modality  :initarg :modality  :accessor genai-part-modality :initform nil)
   (uri       :initarg :uri       :accessor genai-part-uri :initform nil)
   (file-id   :initarg :file-id   :accessor genai-part-file-id :initform nil)
   ;; Alist of (key . value) vendor keys merged into the encoded part, sorted
   ;; by key. Namespace them: a key the schema already uses is dropped.
   (extensions :initarg :extensions :accessor genai-part-extensions :initform nil)))

(defun make-genai-part (&key part-type content id name arguments response
                             mime-type modality uri file-id extensions)
  (make-instance 'genai-part :part-type part-type :content content :id id
                             :name name :arguments arguments :response response
                             :mime-type mime-type :modality modality :uri uri
                             :file-id file-id :extensions extensions))

(defun make-genai-text-part (content)
  (make-genai-part :part-type "text" :content content))

(defclass genai-message ()
  ((role  :initarg :role  :accessor genai-message-role :initform "")
   (name  :initarg :name  :accessor genai-message-name :initform nil)
   (parts :initarg :parts :accessor genai-message-parts :initform nil)
   ;; Required on an output message, where the encoder emits an unset value as
   ;; empty. On an input message an unset value omits the key.
   (finish-reason :initarg :finish-reason :accessor genai-message-finish-reason
                  :initform nil)))

(defun make-genai-message (&key (role "") name parts finish-reason)
  (make-instance 'genai-message :role role :name name :parts parts
                                :finish-reason finish-reason))

(defclass genai-tool-definition ()
  ((tool-type   :initarg :tool-type   :accessor genai-tool-definition-type :initform nil)
   (name        :initarg :name        :accessor genai-tool-definition-name :initform "")
   (description :initarg :description :accessor genai-tool-definition-description :initform nil)
   ;; The tool's JSON schema, raw, under the same constraint as a part's
   ;; ARGUMENTS.
   (parameters  :initarg :parameters  :accessor genai-tool-definition-parameters :initform nil)
   (extensions  :initarg :extensions  :accessor genai-tool-definition-extensions :initform nil)))

(defun make-genai-tool-definition (&key tool-type (name "") description
                                        parameters extensions)
  (make-instance 'genai-tool-definition :tool-type tool-type :name name
                                        :description description
                                        :parameters parameters
                                        :extensions extensions))

(defparameter +genai-tool-type-function+ "function"
  "The schema's default tool shape, and the only one the conventions name. A
tool definition with no type of its own gets it, because the schema requires
the key.")

(defclass genai-usage ()
  ((input-tokens  :initarg :input-tokens  :accessor genai-usage-input-tokens  :initform 0)
   (output-tokens :initarg :output-tokens :accessor genai-usage-output-tokens :initform 0)
   (cache-read-input-tokens :initarg :cache-read-input-tokens
                            :accessor genai-usage-cache-read-input-tokens :initform 0)
   (cache-creation-input-tokens :initarg :cache-creation-input-tokens
                                :accessor genai-usage-cache-creation-input-tokens :initform 0)
   (reasoning-output-tokens :initarg :reasoning-output-tokens
                            :accessor genai-usage-reasoning-output-tokens :initform 0)
   ;; Marks an all-zero usage a provider really did return. Any non-zero count
   ;; already counts as reported without it.
   (reported :initarg :reported :accessor genai-usage-reported :initform nil)))

(defun make-genai-usage (&key (input 0) (output 0) (cache-read 0)
                              (cache-creation 0) (reasoning 0) reported)
  (make-instance 'genai-usage :input-tokens input :output-tokens output
                              :cache-read-input-tokens cache-read
                              :cache-creation-input-tokens cache-creation
                              :reasoning-output-tokens reasoning
                              :reported reported))

(defun genai-usage-reported-p (usage)
  "True when USAGE carries data a provider returned. A non-zero count counts on
its own; the REPORTED flag is needed only for the all-zero usage a provider did
return. A call that never reached a provider must leave both unset, so its
zeros never reach the token histogram."
  (and usage
       (or (genai-usage-reported usage)
           (/= 0 (genai-usage-input-tokens usage))
           (/= 0 (genai-usage-output-tokens usage))
           (/= 0 (genai-usage-cache-read-input-tokens usage))
           (/= 0 (genai-usage-cache-creation-input-tokens usage))
           (/= 0 (genai-usage-reasoning-output-tokens usage)))))

(defclass genai-invocation ()
  (;; Identity and routing
   (operation :initarg :operation :accessor genai-invocation-operation :initform nil)
   (kind      :initarg :kind      :accessor genai-invocation-kind :initform nil)
   (trace-id  :initarg :trace-id  :accessor genai-invocation-trace-id :initform nil)
   (span-id   :initarg :span-id   :accessor genai-invocation-span-id :initform nil)
   (parent-span-id :initarg :parent-span-id
                   :accessor genai-invocation-parent-span-id :initform nil)
   ;; Request
   (provider       :initarg :provider       :accessor genai-invocation-provider :initform nil)
   (request-model  :initarg :request-model  :accessor genai-invocation-request-model :initform nil)
   (conversation-id :initarg :conversation-id
                    :accessor genai-invocation-conversation-id :initform nil)
   (agent-name     :initarg :agent-name     :accessor genai-invocation-agent-name :initform nil)
   (agent-version  :initarg :agent-version  :accessor genai-invocation-agent-version :initform nil)
   (agent-id       :initarg :agent-id       :accessor genai-invocation-agent-id :initform nil)
   (agent-description :initarg :agent-description
                      :accessor genai-invocation-agent-description :initform nil)
   (data-source-id :initarg :data-source-id :accessor genai-invocation-data-source-id :initform nil)
   (workflow-name  :initarg :workflow-name  :accessor genai-invocation-workflow-name :initform nil)
   (tool-name      :initarg :tool-name      :accessor genai-invocation-tool-name :initform nil)
   (tool-call-id   :initarg :tool-call-id   :accessor genai-invocation-tool-call-id :initform nil)
   (tool-type      :initarg :tool-type      :accessor genai-invocation-tool-type :initform nil)
   (tool-description :initarg :tool-description
                     :accessor genai-invocation-tool-description :initform nil)
   (server-address :initarg :server-address :accessor genai-invocation-server-address :initform nil)
   (server-port    :initarg :server-port    :accessor genai-invocation-server-port :initform nil)
   (stream         :initarg :stream         :accessor genai-invocation-stream :initform nil)
   (stream-cursor  :initarg :stream-cursor  :accessor genai-invocation-stream-cursor :initform nil)
   ;; Sampling parameters
   (max-tokens  :initarg :max-tokens  :accessor genai-invocation-max-tokens :initform nil)
   (temperature :initarg :temperature :accessor genai-invocation-temperature :initform nil)
   (top-p       :initarg :top-p       :accessor genai-invocation-top-p :initform nil)
   (top-k       :initarg :top-k       :accessor genai-invocation-top-k :initform nil)
   (frequency-penalty :initarg :frequency-penalty
                      :accessor genai-invocation-frequency-penalty :initform nil)
   (presence-penalty :initarg :presence-penalty
                     :accessor genai-invocation-presence-penalty :initform nil)
   (stop-sequences :initarg :stop-sequences :accessor genai-invocation-stop-sequences :initform nil)
   (seed         :initarg :seed         :accessor genai-invocation-seed :initform nil)
   (choice-count :initarg :choice-count :accessor genai-invocation-choice-count :initform nil)
   (output-type  :initarg :output-type  :accessor genai-invocation-output-type :initform nil)
   (encoding-formats :initarg :encoding-formats
                     :accessor genai-invocation-encoding-formats :initform nil)
   (dimension-count :initarg :dimension-count
                    :accessor genai-invocation-dimension-count :initform nil)
   ;; Content
   (system-instructions :initarg :system-instructions
                        :accessor genai-invocation-system-instructions :initform nil)
   (input-messages  :initarg :input-messages  :accessor genai-invocation-input-messages :initform nil)
   (output-messages :initarg :output-messages :accessor genai-invocation-output-messages :initform nil)
   (tool-definitions :initarg :tool-definitions
                     :accessor genai-invocation-tool-definitions :initform nil)
   (tool-call-arguments :initarg :tool-call-arguments
                        :accessor genai-invocation-tool-call-arguments :initform nil)
   (tool-call-result :initarg :tool-call-result
                     :accessor genai-invocation-tool-call-result :initform nil)
   (retrieval-query-text :initarg :retrieval-query-text
                         :accessor genai-invocation-retrieval-query-text :initform nil)
   (retrieval-documents :initarg :retrieval-documents
                        :accessor genai-invocation-retrieval-documents :initform nil)
   ;; Response
   (response-model  :initarg :response-model  :accessor genai-invocation-response-model :initform nil)
   (response-id     :initarg :response-id     :accessor genai-invocation-response-id :initform nil)
   (response-status :initarg :response-status :accessor genai-invocation-response-status :initform nil)
   (finish-reasons  :initarg :finish-reasons  :accessor genai-invocation-finish-reasons :initform nil)
   (usage           :initarg :usage           :accessor genai-invocation-usage :initform nil)
   ;; Timing, as OTLP nanosecond strings.
   (started-at-nano :initarg :started-at-nano
                    :accessor genai-invocation-started-at-nano :initform nil)
   (completed-at-nano :initarg :completed-at-nano
                      :accessor genai-invocation-completed-at-nano :initform nil)
   (first-chunk-at-nano :initarg :first-chunk-at-nano
                        :accessor genai-invocation-first-chunk-at-nano :initform nil)
   ;; Outcome
   (error-type    :initarg :error-type    :accessor genai-invocation-error-type :initform nil)
   (error-message :initarg :error-message :accessor genai-invocation-error-message :initform nil)
   ;; Content capture mode for this invocation.
   (capture :initarg :capture :accessor genai-invocation-capture :initform :no-content)
   ;; Extra OTLP attribute objects, emitted after the convention attributes so
   ;; a caller can override one it disagrees with. This is where a vendor
   ;; layer's own attributes arrive.
   (extra-attributes :initarg :extra-attributes
                     :accessor genai-invocation-extra-attributes :initform nil)
))

(defun make-genai-invocation (&rest args)
  (apply #'make-instance 'genai-invocation args))

(defun genai-invocation-operation-name (inv)
  "The invocation's operation, defaulting to chat."
  (let ((op (genai-invocation-operation inv)))
    (if (and (stringp op) (plusp (length op))) op "chat")))

(defun genai-invocation-error-type-value (inv)
  "error.type for a failed invocation, or NIL.
The conventions require the attribute whenever the operation ended in an error,
so a failure the caller described only in prose still classifies as _OTHER."
  (let ((declared (genai-invocation-error-type inv))
        (message (genai-invocation-error-message inv)))
    (cond
      ((and (stringp declared) (plusp (length declared))) declared)
      ((and (stringp message) (plusp (length message))) +genai-error-type-other+)
      (t nil))))

;;; --- Encoders ---
;;;
;;; A field the encoder cannot represent is left out and reported: the returned
;;; payload is always usable, and the returned problem list names every dropped
;;; field. Encoding problems never fail the span.

(defun %genai-or-empty (value)
  "VALUE, or the empty string when the caller left it unset. This is how a
schema-required key is emitted rather than omitted."
  (or value ""))

(defun %genai-raw-json-field (text field)
  "A raw JSON document for a schema field that carries one.
Returns (values RAW PROBLEMS). Invalid JSON is dropped rather than emitted,
because embedding it would make the enclosing document unparseable and lose the
whole attribute."
  (cond
    ((or (null text) (and (stringp text) (zerop (length text)))) (values nil nil))
    ((not (genai-valid-json-p text))
     (values nil (list (format nil "drop ~a: not valid JSON" field))))
    (t (values (genai-raw-json text) nil))))

(defun %genai-splice-extensions (fields extensions)
  "Append EXTENSIONS to the ordered alist FIELDS, sorted by key.
Returns (values FIELDS PROBLEMS). A key the schema object already holds would
produce a duplicate that decoders resolve differently, so it is dropped and
reported. The clash test reads the keys the object actually holds rather than
every key the schema can hold, because an extension is free to reuse a key the
encoder omitted."
  (if (null extensions)
      (values fields nil)
      (let ((taken (mapcar #'car fields))
            (problems nil)
            (kept nil))
        (dolist (pair (sort (copy-alist extensions) #'string< :key #'car))
          (if (member (car pair) taken :test #'string=)
              (push (format nil "drop extension key ~s: the message schema already uses it"
                            (car pair))
                    problems)
              (push pair kept)))
        (values (append fields (nreverse kept)) (nreverse problems)))))

(defun encode-genai-part (part)
  "Encode one part object. Returns (values RAW-JSON PROBLEMS); a NIL payload
means the part was dropped.

It encodes only the fields the part's type owns, so a part carrying leftovers
from another shape cannot smuggle them onto the wire. A type the schema does
not name is encoded as the generic part: the type and the extensions, nothing
else. A part with no type is dropped, because the schema requires one."
  (let ((type (genai-part-type part))
        (problems nil)
        (fields nil)
        (needs-modality nil))
    (when (or (null type) (zerop (length type)))
      (return-from encode-genai-part
        (values nil (list "drop message part with no type"))))
    (flet ((field (key value) (push (cons key value) fields))
           (raw-or-null (text name)
             (multiple-value-bind (raw errs) (%genai-raw-json-field text name)
               (setf problems (append problems errs))
               (or raw (genai-raw-json "null")))))
      (cond
        ((or (string= type "text") (string= type "reasoning"))
         (field "type" type)
         (field "content" (%genai-or-empty (genai-part-content part))))
        ((string= type "tool_call")
         (field "type" type)
         (when (plusp (length (or (genai-part-id part) "")))
           (field "id" (genai-part-id part)))
         (field "name" (%genai-or-empty (genai-part-name part)))
         (field "arguments" (raw-or-null (genai-part-arguments part) "arguments")))
        ((string= type "server_tool_call")
         (field "type" type)
         (when (plusp (length (or (genai-part-id part) "")))
           (field "id" (genai-part-id part)))
         (field "name" (%genai-or-empty (genai-part-name part)))
         (field "server_tool_call"
                (raw-or-null (genai-part-arguments part) "server_tool_call")))
        ((string= type "tool_call_response")
         (field "type" type)
         (when (plusp (length (or (genai-part-id part) "")))
           (field "id" (genai-part-id part)))
         (field "response" (raw-or-null (genai-part-response part) "response")))
        ((string= type "server_tool_call_response")
         (field "type" type)
         (when (plusp (length (or (genai-part-id part) "")))
           (field "id" (genai-part-id part)))
         (field "server_tool_call_response"
                (raw-or-null (genai-part-response part) "server_tool_call_response")))
        ((string= type "compaction")
         (field "type" type)
         ;; Unlike text, an unset compaction content is omitted rather than
         ;; emitted empty: the schema does not require it.
         (when (genai-part-content part)
           (field "content" (genai-part-content part)))
         (when (plusp (length (or (genai-part-id part) "")))
           (field "id" (genai-part-id part))))
        ((string= type "blob")
         (field "type" type)
         (field "content" (%genai-or-empty (genai-part-content part)))
         (when (plusp (length (or (genai-part-mime-type part) "")))
           (field "mime_type" (genai-part-mime-type part)))
         (setf needs-modality t))
        ((string= type "file")
         (field "type" type)
         (when (plusp (length (or (genai-part-mime-type part) "")))
           (field "mime_type" (genai-part-mime-type part)))
         (setf needs-modality t))
        ((string= type "uri")
         (field "type" type)
         (when (plusp (length (or (genai-part-mime-type part) "")))
           (field "mime_type" (genai-part-mime-type part)))
         (setf needs-modality t))
        (t (return-from encode-genai-part (encode-genai-generic-part part))))
      (setf fields (nreverse fields))
      (when needs-modality
        (let ((modality (genai-part-modality part)))
          (if (and modality (plusp (length modality)))
              (setf fields (append fields (list (cons "modality" modality))))
              (push (format nil "~a part has no modality; omitting the schema-required key"
                            type)
                    problems))))
      ;; uri and file_id follow modality in the schema's key order.
      (when (string= type "uri")
        (setf fields (append fields
                             (list (cons "uri" (%genai-or-empty (genai-part-uri part)))))))
      (when (string= type "file")
        (setf fields (append fields
                             (list (cons "file_id"
                                         (%genai-or-empty (genai-part-file-id part)))))))
      (multiple-value-bind (spliced errs)
          (%genai-splice-extensions fields (genai-part-extensions part))
        (values (genai-raw-json (genai-json (cons :object spliced)))
                (append problems errs))))))

(defun encode-genai-generic-part (part)
  "Encode a part whose type the schema does not name: the type and the
extensions, nothing else. Any other field it carries is dropped and reported."
  (let ((type (genai-part-type part))
        (problems nil))
    (when (or (null type) (zerop (length type)))
      (return-from encode-genai-generic-part
        (values nil (list "drop message part with no type"))))
    (when (or (genai-part-content part) (genai-part-id part) (genai-part-name part)
              (genai-part-arguments part) (genai-part-response part)
              (genai-part-mime-type part) (genai-part-modality part)
              (genai-part-uri part) (genai-part-file-id part))
      (push (format nil "message part of type ~s keeps only its type and its extensions: the schema's generic part has no other field"
                    type)
            problems))
    (multiple-value-bind (fields errs)
        (%genai-splice-extensions (list (cons "type" type))
                                  (genai-part-extensions part))
      (values (genai-raw-json (genai-json (cons :object fields)))
              (append (nreverse problems) errs)))))

(defun encode-genai-messages (messages &key output)
  "Render MESSAGES as the JSON string carried by gen_ai.input.messages or
gen_ai.output.messages. Returns (values JSON-STRING PROBLEMS).
OUTPUT selects the output-message schema, which requires a finish reason on
every entry; an input message carries one only when the caller set it."
  (let ((problems nil)
        (encoded nil))
    (dolist (msg messages)
      (let ((parts nil))
        (dolist (part (genai-message-parts msg))
          (multiple-value-bind (raw errs) (encode-genai-part part)
            (setf problems (append problems errs))
            (when raw (push raw parts))))
        (let ((fields (list (cons "role" (genai-message-role msg)))))
          (when (plusp (length (or (genai-message-name msg) "")))
            (setf fields (append fields (list (cons "name" (genai-message-name msg))))))
          (setf fields (append fields (list (cons "parts" (cons :array (nreverse parts))))))
          (let ((finish (genai-message-finish-reason msg)))
            (when (or finish output)
              (setf fields (append fields
                                   (list (cons "finish_reason" (%genai-or-empty finish)))))))
          (push (genai-raw-json (genai-json (cons :object fields))) encoded))))
    (values (genai-json (cons :array (nreverse encoded))) problems)))

(defun encode-genai-system-instructions (parts)
  "Render PARTS as the JSON string carried by gen_ai.system_instructions, which
is a bare parts array rather than a messages array."
  (let ((problems nil)
        (encoded nil))
    (dolist (part parts)
      (multiple-value-bind (raw errs)
          (if (equal (genai-part-type part) "text")
              (encode-genai-part part)
              (encode-genai-generic-part part))
        (setf problems (append problems errs))
        (when raw (push raw encoded))))
    (values (genai-json (cons :array (nreverse encoded))) problems)))

(defun encode-genai-tool-definitions (tools)
  "Render TOOLS as the JSON string carried by gen_ai.tool.definitions."
  (let ((problems nil)
        (encoded nil))
    (dolist (tool tools)
      (multiple-value-bind (parameters errs)
          (%genai-raw-json-field (genai-tool-definition-parameters tool) "parameters")
        (setf problems (append problems errs))
        (let* ((declared (genai-tool-definition-type tool))
               (tool-type (if (plusp (length (or declared ""))) declared
                              +genai-tool-type-function+))
               (fields (list (cons "type" tool-type)
                             (cons "name" (genai-tool-definition-name tool)))))
          (when (plusp (length (or (genai-tool-definition-description tool) "")))
            (setf fields (append fields
                                 (list (cons "description"
                                             (genai-tool-definition-description tool))))))
          (when parameters
            (setf fields (append fields (list (cons "parameters" parameters)))))
          (multiple-value-bind (spliced splice-errs)
              (%genai-splice-extensions fields
                                        (genai-tool-definition-extensions tool))
            (setf problems (append problems splice-errs))
            (push (genai-raw-json (genai-json (cons :object spliced))) encoded)))))
    (values (genai-json (cons :array (nreverse encoded))) problems)))

(defun genai-system-instructions-from-text (text)
  "System-instruction parts for a plain text prompt, which is the shape most
providers take. Blank text produces none."
  (when (and (stringp text)
             (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (list (make-genai-text-part text))))

;;; --- Timing ---
;;;
;;; The conventions' duration and token instruments are recorded from the
;;; recorder in metrics.lisp, so the only derived timing this layer needs is
;;; the first-chunk delay that goes on the span.

(defun genai-invocation-time-to-first-chunk (inv)
  "Delay before the first streamed chunk, in seconds. NIL for a non-streaming
call and for a call with no timed chunk."
  (let ((start (genai-invocation-started-at-nano inv))
        (first-chunk (genai-invocation-first-chunk-at-nano inv)))
    (when (and (genai-invocation-stream inv) start first-chunk)
      (let ((seconds (/ (- (parse-integer first-chunk) (parse-integer start)) 1d9)))
        (if (< seconds 0) 0d0 seconds)))))

;;; --- Attribute builders ---

(defun %genai-push-string (attrs key value)
  (if (and (stringp value) (plusp (length value)))
      (cons (otel-string-attr key value) attrs)
      attrs))

(defun genai-request-attributes (inv)
  "Attributes known before the provider replies."
  (let ((attrs (list (otel-string-attr "gen_ai.operation.name"
                                       (genai-invocation-operation-name inv)))))
    (setf attrs (%genai-push-string attrs "gen_ai.provider.name"
                                    (genai-invocation-provider inv)))
    (setf attrs (%genai-push-string attrs "gen_ai.request.model"
                                    (genai-invocation-request-model inv)))
    (when (genai-invocation-stream inv)
      (push (otel-bool-attr "gen_ai.request.stream" t) attrs))
    (setf attrs (%genai-push-string attrs "gen_ai.conversation.id"
                                    (genai-invocation-conversation-id inv)))
    (setf attrs (%genai-push-string attrs "gen_ai.agent.name"
                                    (genai-invocation-agent-name inv)))
    (setf attrs (%genai-push-string attrs "gen_ai.agent.version"
                                    (genai-invocation-agent-version inv)))
    (setf attrs (%genai-push-string attrs "gen_ai.agent.id"
                                    (genai-invocation-agent-id inv)))
    (setf attrs (%genai-push-string attrs "gen_ai.agent.description"
                                    (genai-invocation-agent-description inv)))
    (setf attrs (%genai-push-string attrs "gen_ai.data_source.id"
                                    (genai-invocation-data-source-id inv)))
    (setf attrs (%genai-push-string attrs "gen_ai.workflow.name"
                                    (genai-invocation-workflow-name inv)))
    (setf attrs (%genai-push-string attrs "gen_ai.request.stream_cursor"
                                    (genai-invocation-stream-cursor inv)))
    (when (integerp (genai-invocation-max-tokens inv))
      (push (otel-int-attr "gen_ai.request.max_tokens"
                           (genai-invocation-max-tokens inv)) attrs))
    (when (realp (genai-invocation-temperature inv))
      (push (otel-double-attr "gen_ai.request.temperature"
                              (genai-invocation-temperature inv)) attrs))
    (when (realp (genai-invocation-top-p inv))
      (push (otel-double-attr "gen_ai.request.top_p" (genai-invocation-top-p inv)) attrs))
    (when (realp (genai-invocation-top-k inv))
      (push (otel-double-attr "gen_ai.request.top_k" (genai-invocation-top-k inv)) attrs))
    (when (realp (genai-invocation-frequency-penalty inv))
      (push (otel-double-attr "gen_ai.request.frequency_penalty"
                              (genai-invocation-frequency-penalty inv)) attrs))
    (when (realp (genai-invocation-presence-penalty inv))
      (push (otel-double-attr "gen_ai.request.presence_penalty"
                              (genai-invocation-presence-penalty inv)) attrs))
    (when (genai-invocation-stop-sequences inv)
      (push (otel-string-array-attr "gen_ai.request.stop_sequences"
                                    (genai-invocation-stop-sequences inv)) attrs))
    (when (integerp (genai-invocation-seed inv))
      (push (otel-int-attr "gen_ai.request.seed" (genai-invocation-seed inv)) attrs))
    (when (integerp (genai-invocation-choice-count inv))
      (push (otel-int-attr "gen_ai.request.choice.count"
                           (genai-invocation-choice-count inv)) attrs))
    (setf attrs (%genai-push-string attrs "gen_ai.output.type"
                                    (genai-invocation-output-type inv)))
    (when (genai-invocation-encoding-formats inv)
      (push (otel-string-array-attr "gen_ai.request.encoding_formats"
                                    (genai-invocation-encoding-formats inv)) attrs))
    (when (integerp (genai-invocation-dimension-count inv))
      (push (otel-int-attr "gen_ai.embeddings.dimension.count"
                           (genai-invocation-dimension-count inv)) attrs))
    (setf attrs (%genai-push-string attrs "gen_ai.tool.name"
                                    (genai-invocation-tool-name inv)))
    (setf attrs (%genai-push-string attrs "gen_ai.tool.call.id"
                                    (genai-invocation-tool-call-id inv)))
    (when (string= (genai-invocation-operation-name inv) "execute_tool")
      (setf attrs (%genai-push-string attrs "gen_ai.tool.type"
                                      (genai-invocation-tool-type inv))))
    (setf attrs (%genai-push-string attrs "gen_ai.tool.description"
                                    (genai-invocation-tool-description inv)))
    (setf attrs (%genai-push-string attrs "server.address"
                                    (genai-invocation-server-address inv)))
    (when (integerp (genai-invocation-server-port inv))
      (push (otel-int-attr "server.port" (genai-invocation-server-port inv)) attrs))
    (nreverse attrs)))

(defun genai-response-attributes (inv)
  "Non-content attributes known once the invocation finishes."
  (let ((attrs nil))
    (setf attrs (%genai-push-string attrs "gen_ai.response.id"
                                    (genai-invocation-response-id inv)))
    (setf attrs (%genai-push-string attrs "gen_ai.response.model"
                                    (genai-invocation-response-model inv)))
    (setf attrs (%genai-push-string attrs "gen_ai.response.status"
                                    (genai-invocation-response-status inv)))
    (when (genai-invocation-finish-reasons inv)
      (push (otel-string-array-attr "gen_ai.response.finish_reasons"
                                    (genai-invocation-finish-reasons inv)) attrs))
    (let ((ttfc (genai-invocation-time-to-first-chunk inv)))
      (when ttfc
        (push (otel-double-attr "gen_ai.response.time_to_first_chunk" ttfc) attrs)))
    (let ((usage (genai-invocation-usage inv)))
      (when (genai-usage-reported-p usage)
        ;; Both counts go out even at zero, so a zero-valued usage a provider
        ;; did return still decodes as present.
        (push (otel-int-attr "gen_ai.usage.input_tokens"
                             (genai-usage-input-tokens usage)) attrs)
        (push (otel-int-attr "gen_ai.usage.output_tokens"
                             (genai-usage-output-tokens usage)) attrs)
        (when (/= 0 (genai-usage-cache-read-input-tokens usage))
          (push (otel-int-attr "gen_ai.usage.cache_read.input_tokens"
                               (genai-usage-cache-read-input-tokens usage)) attrs))
        (when (/= 0 (genai-usage-cache-creation-input-tokens usage))
          (push (otel-int-attr "gen_ai.usage.cache_creation.input_tokens"
                               (genai-usage-cache-creation-input-tokens usage)) attrs))
        (when (/= 0 (genai-usage-reasoning-output-tokens usage))
          (push (otel-int-attr "gen_ai.usage.reasoning.output_tokens"
                               (genai-usage-reasoning-output-tokens usage)) attrs))))
    (let ((error-type (genai-invocation-error-type-value inv)))
      (when error-type
        (push (otel-string-attr "error.type" error-type) attrs)))
    (nreverse attrs)))

(defun genai-content-attributes (inv capture &key on-problem)
  "Encoded message content. Returns nothing when CAPTURE keeps content off the
span. ON-PROBLEM, when given, is called with each encoder problem: an encoder
that cannot represent one field leaves that field out and keeps the rest of the
attribute rather than failing the span."
  (unless (genai-capture-span-content-p capture)
    (return-from genai-content-attributes nil))
  (let ((attrs nil))
    (flet ((report (problems)
             (when on-problem (mapc on-problem problems)))
           (add (key payload)
             (when (and payload (plusp (length payload)))
               (push (otel-string-attr key payload) attrs))))
      (when (genai-invocation-system-instructions inv)
        (multiple-value-bind (payload problems)
            (encode-genai-system-instructions (genai-invocation-system-instructions inv))
          (report problems)
          (add "gen_ai.system_instructions" payload)))
      (when (genai-invocation-input-messages inv)
        (multiple-value-bind (payload problems)
            (encode-genai-messages (genai-invocation-input-messages inv))
          (report problems)
          (add "gen_ai.input.messages" payload)))
      (when (genai-invocation-output-messages inv)
        (multiple-value-bind (payload problems)
            (encode-genai-messages (genai-invocation-output-messages inv) :output t)
          (report problems)
          (add "gen_ai.output.messages" payload)))
      ;; Tool definitions are opt-in in the registry because schemas and
      ;; descriptions can carry sensitive application data. They stay behind
      ;; the same content gate as messages.
      (when (genai-invocation-tool-definitions inv)
        (multiple-value-bind (payload problems)
            (encode-genai-tool-definitions (genai-invocation-tool-definitions inv))
          (report problems)
          (add "gen_ai.tool.definitions" payload)))
      (dolist (entry (list (list "gen_ai.tool.call.arguments"
                                 (genai-invocation-tool-call-arguments inv)
                                 "tool call arguments")
                           (list "gen_ai.tool.call.result"
                                 (genai-invocation-tool-call-result inv)
                                 "tool call result")
                           (list "gen_ai.retrieval.documents"
                                 (genai-invocation-retrieval-documents inv)
                                 "retrieval documents")))
        (destructuring-bind (key text field) entry
          (when text
            (multiple-value-bind (raw problems) (%genai-raw-json-field text field)
              (report problems)
              (when raw (add key (genai-raw-json-text raw)))))))
      (let ((query (genai-invocation-retrieval-query-text inv)))
        (when (and (stringp query) (plusp (length query)))
          (push (otel-string-attr "gen_ai.retrieval.query.text" query) attrs))))
    (nreverse attrs)))

;;; --- Span shape ---

(defun genai-span-name (inv)
  "The conventions' span name, \"<operation> <subject>\".
The operation decides the subject: execute_tool takes the tool name;
invoke_agent, create_agent and plan the agent name; retrieval the data source;
invoke_workflow the workflow name; fetch_response has no subject; every other
operation takes the request model. An empty operation-specific subject falls
back to the request model, and an empty subject to the operation alone."
  (let* ((op (genai-invocation-operation-name inv))
         (subject (cond
                    ((string= op "execute_tool") (genai-invocation-tool-name inv))
                    ((member op '("invoke_agent" "create_agent" "plan") :test #'string=)
                     (genai-invocation-agent-name inv))
                    ((string= op "retrieval") (genai-invocation-data-source-id inv))
                    ((string= op "invoke_workflow") (genai-invocation-workflow-name inv))
                    ((string= op "fetch_response") (return-from genai-span-name op))
                    (t (genai-invocation-request-model inv)))))
    (when (zerop (length (or subject "")))
      (setf subject (genai-invocation-request-model inv)))
    (if (zerop (length (or subject "")))
        op
        (concatenate 'string op " " subject))))

(defun genai-span-kind (inv)
  "OTLP span kind: 1=INTERNAL, 3=CLIENT. An explicit KIND wins; otherwise the
in-process operations are INTERNAL and the rest are CLIENT."
  (or (genai-invocation-kind inv)
      (if (member (genai-invocation-operation-name inv)
                  '("execute_tool" "invoke_workflow" "plan")
                  :test #'string=)
          1
          3)))

(defun build-genai-span (inv &key on-problem)
  "Build the OTLP span for a finished invocation.
A successful operation leaves the status unset, which is what an
instrumentation library reports; Ok is the application's to set."
  (let* ((capture (genai-invocation-capture inv))
         (attrs (append (genai-request-attributes inv)
                        (genai-response-attributes inv)
                        (genai-content-attributes inv capture :on-problem on-problem)
                        (genai-invocation-extra-attributes inv)))
         (error-type (genai-invocation-error-type-value inv)))
    (build-span :trace-id (genai-invocation-trace-id inv)
                :span-id (genai-invocation-span-id inv)
                :parent-span-id (genai-invocation-parent-span-id inv)
                :name (genai-span-name inv)
                :kind (genai-span-kind inv)
                :start-time-unix-nano (genai-invocation-started-at-nano inv)
                :end-time-unix-nano (genai-invocation-completed-at-nano inv)
                :attributes (coerce attrs 'vector)
                :status-code (if error-type 2 :unset)
                :status-message (or (genai-invocation-error-message inv) ""))))
