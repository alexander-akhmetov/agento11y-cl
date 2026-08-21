(in-package :agento11y-cl)

;;; Adapter: a generation payload becomes a GenAI-semconv invocation, which
;;; otel-genai.lisp turns into a span. A port of go/agento11y/otel_export.go.
;;;
;;; The seam is the payload rather than the recorder, matching Go's
;;; applyGenerationToInvocation and the golden generation/span fixture pairs.
;;; It also means content arrives already sanitized: BUILD-GENERATION-PAYLOAD
;;; applied the capture-mode stripping and the secret redaction, and under
;;; metadata_only the content is already gone.

(defparameter +feature-otel-generation-export+ "otel generation export"
  "Names the experimental feature that exports generations as GenAI-semconv
spans instead of POSTing the proprietary generation payload.
%REQUIRE-EXPERIMENTAL writes the name into a sentence, so the value is prose
rather than an identifier.")

;;; Extension keys carrying the parts of the agento11y data model that the
;;; conventions' message schema has no field for. They are the keys the
;;; backend's wire-format decoder reads.
(defparameter +otel-part-ext-provider-type+ "agento11y.provider_type")
(defparameter +otel-part-ext-tool-name+ "agento11y.tool_name")
(defparameter +otel-part-ext-is-error+ "agento11y.is_error")
(defparameter +otel-part-ext-arguments-b64+ "agento11y.arguments_b64")
(defparameter +otel-part-ext-response-b64+ "agento11y.response_b64")
(defparameter +otel-part-ext-media-name+ "agento11y.media_name")
(defparameter +otel-tool-ext-deferred+ "agento11y.deferred")
(defparameter +otel-tool-ext-input-schema-b64+ "agento11y.input_schema_b64")

(defparameter +otel-provider-stored-to-wire+
  '(("gemini" . "gcp.gemini")
    ("mistral" . "mistral_ai")
    ("moonshotai" . "moonshot_ai")
    ("vertex" . "gcp.vertex_ai")
    ("bedrock" . "aws.bedrock")
    ("azure-openai" . "azure.ai.openai")
    ("azure-ai-inference" . "azure.ai.inference")
    ("watsonx" . "ibm.watsonx.ai")
    ("x-ai" . "x_ai"))
  "The SDK's stored provider values mapped to the OTel GenAI registry
spellings. The backend applies the inverse, so the round trip leaves a stored
record unchanged.")

(defparameter +otel-synthesized-provider-types+
  '(("thinking" . "thinking") ("tool_call" . "tool_use") ("tool_result" . "tool_result"))
  "The provider_type value SERIALIZE-PART writes for each payload part kind.
It is derived from the kind and carries nothing the part type does not already
say, so the adapter emits the provider_type extension only for a value that
differs from this table. A media part has no entry: there the value is the
caller's own.")

;;; --- Protocol and gating ---

(defun otel-generation-protocol-p (config)
  "True when CONFIG asks for the GenAI-semconv generation export protocol."
  (eq (config-generation-protocol config) :otel))

(defun otel-generation-export-enabled-p (config)
  "True when otel generation export is configured and the experimental gate is
open. A shut gate exports nothing rather than falling back to :HTTP: the caller
asked for spans, and quietly POSTing the proprietary payload instead would ship
content to an endpoint they did not choose. Nothing else exports either: the
tool execution, embedding and workflow-step spans fall silent with the
generation, so a shut gate cannot leave a child span pointing at a parent that
was never sent."
  (and (otel-generation-protocol-p config)
       (config-experimental-features config)
       t))

(defun otel-generation-export-ready-p (config)
  "True when otel generation export is enabled and has somewhere to go.
In otel mode the traces endpoint is the generation's destination, so without
one there is nothing to export to. Queueing the span anyway would POST to a
NIL URL and sleep through every retry backoff inside the flush thread."
  (and (otel-generation-export-enabled-p config)
       (config-traces-endpoint config)
       t))

(defun generation-payload-export-p (config)
  "True when generations leave this client as proprietary payloads.
In otel mode nothing reaches the generation queue, so the queue is skipped
rather than drained into a POST the caller did not ask for."
  (and (config-generation-enabled config)
       (not (otel-generation-protocol-p config))))

(defun spans-export-active-p (config)
  "True when this client exports spans. In otel mode the traces endpoint is
where a generation goes, so span export runs even with traces-enabled unset."
  (or (config-traces-enabled config)
      (otel-generation-export-ready-p config)))

(defun otel-capture-mode (mode)
  "Translate the SDK's content-capture mode onto the conventions' vocabulary.
The SDK's own mode decides it alone: in otel mode the span is the export, so
reading OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT here would let a
traces-side setting overrule the SDK's content policy. That variable is
deliberately not read anywhere in this SDK. A mode outside the vocabulary maps
to :NO-CONTENT, so an unsupported value fails closed.

:FULL-WITH-METADATA-SPANS never reaches here in otel mode: the mode exists to
keep content off the shared traces destination, which is the only destination
this protocol has, so the recorder resolves it to :METADATA-ONLY first.
:NO-TOOL-CONTENT does reach here as :SPAN-ONLY and has its tool call arguments
and results stripped afterwards by OTEL-STRIP-TOOL-CONTENT, which is what
keeps it agreeing with CAPTURE-KEEPS-TOOL-SPAN-CONTENT-P."
  (case mode
    ((:full :no-tool-content :full-with-metadata-spans) :span-only)
    (t :no-content)))

(defun otel-operation-name (operation-name)
  "Map the SDK's operation name onto the conventions' operation. The two mode
defaults become the spec's chat operation; any other name is the caller's own
and passes through unchanged."
  (if (member (or operation-name "") '("" "generateText" "streamText") :test #'string=)
      "chat"
      operation-name))

(defun otel-provider-name (provider)
  "The registry spelling of a stored provider value."
  (or (cdr (assoc (or provider "") +otel-provider-stored-to-wire+ :test #'string=))
      provider))

;;; --- Raw JSON handling ---

(defun %json-compact (text)
  "TEXT with the whitespace between tokens removed, preserving key order.
Returns NIL when TEXT is not valid JSON. This is the equivalent of Go's
json.Compact, which is what the embeddable test below compares against."
  (unless (genai-valid-json-p text)
    (return-from %json-compact nil))
  (with-output-to-string (out)
    (let ((in-string nil)
          (escaped nil))
      (loop for ch across text
            do (cond
                 (escaped (write-char ch out) (setf escaped nil))
                 ((and in-string (char= ch #\\)) (write-char ch out) (setf escaped t))
                 ((char= ch #\") (write-char ch out) (setf in-string (not in-string)))
                 ((and (not in-string)
                       (member ch '(#\Space #\Tab #\Newline #\Return))))
                 (t (write-char ch out)))))))

(defun otel-embeddable-json (text)
  "TEXT when it can go on the wire raw and decode back to the same value, else
NIL. The bytes must already be compact, because the enclosing encoder compacts
what it embeds.

A JSON null is rejected too: the decoder reads a null as an absent payload."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (when (and (plusp (length trimmed)) (not (string= trimmed "null")))
      (let ((compact (%json-compact text)))
        (when (and compact (string= compact text)) text)))))

(defun otel-json-string (value)
  "VALUE as a JSON string document."
  (genai-json (or value "")))

(defun otel-json-string-p (text)
  "True when TEXT is a JSON string document."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (and (plusp (length trimmed)) (char= (char trimmed 0) #\"))))

(defun %otel-decode-b64 (value)
  "Decode a payload field that carries base64 text, or NIL."
  (when (and (stringp value) (plusp (length value)))
    (handler-case (cl-base64:base64-string-to-string value)
      (error () nil))))

(defun otel-json-document (b64)
  "Read a base64-encoded JSON document from a payload field.
Returns (values RAW B64), where RAW is the document to embed and B64 the
base64 escape hatch for one that would not survive a round trip. Go's rationale
is worth keeping: demanding the HTML-escaped form would push most coding-agent
tool calls onto the base64 key and leave the semconv field empty for every
generic OTel consumer."
  (let ((text (%otel-decode-b64 b64)))
    (cond
      ((null text) (values nil nil))
      ((otel-embeddable-json text) (values text nil))
      (t (values nil b64)))))

;;; --- Payload readers ---

(defun %otel-int (value)
  "An integer from a payload field, which protojson may render as a string."
  (typecase value
    (integer value)
    (double-float (round value))
    (single-float (round value))
    (string (handler-case (parse-integer value :junk-allowed t) (error () nil)))
    (t nil)))

(defun %otel-int-or-zero (value)
  (or (%otel-int value) 0))

(defun %otel-string (value)
  "A non-empty string from a payload field, or NIL."
  (when (and (stringp value) (plusp (length value))) value))

(defun %otel-alist (table &key sort)
  "A JSON object read from a payload as a (key . value) alist."
  (when (hash-table-p table)
    (let ((pairs nil))
      (maphash (lambda (k v) (push (cons k v) pairs)) table)
      (if sort
          (sort pairs #'string< :key #'car)
          (nreverse pairs)))))

(defun %otel-list (value)
  "A payload array as a list."
  (cond ((null value) nil)
        ((and (vectorp value) (not (stringp value))) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun otel-message-role (role)
  "The conventions' role for a payload MESSAGE_ROLE_* value."
  (let ((text (or role "")))
    (string-downcase
     (if (and (>= (length text) 13) (string= "MESSAGE_ROLE_" text :end2 13))
         (subseq text 13)
         text))))

;;; --- Part mapping ---

(defun %otel-part-provider-type (part kind)
  "The caller's provider_type for PART, or NIL when the payload holds only the
value SERIALIZE-PART synthesizes for KIND. See +OTEL-SYNTHESIZED-PROVIDER-TYPES+."
  (let ((value (%otel-string (jget* part "metadata" "provider_type")))
        (synthesized (cdr (assoc kind +otel-synthesized-provider-types+ :test #'string=))))
    (unless (and value synthesized (string= value synthesized))
      value)))

(defun %otel-set-extension (extensions key value)
  (append extensions (list (cons key value))))

(defun %otel-split-base64-data-url (url)
  "Return (values MIME PAYLOAD) for a data:<mime>;base64,<payload> URL, or NIL."
  (let ((prefix "data:")
        (marker ";base64,"))
    (when (and (stringp url) (>= (length url) (length prefix))
               (string= prefix url :end2 (length prefix)))
      (let* ((rest (subseq url (length prefix)))
             (at (search marker rest)))
        (when at
          (values (subseq rest 0 at) (subseq rest (+ at (length marker)))))))))

(defun otel-media-part (media modality name)
  "Render a media payload as one of the conventions' media part shapes: a
base64 data URL becomes an inline blob, any other URL a uri reference, and an
empty URL a file reference keyed by name.

A declared mime type that disagrees with the data URL's own counts as a
mislabeled payload and becomes a uri instead. Two cases keep the blob shape:
the comparison ignores case, and an undeclared mime type takes the data URL's."
  (let* ((url (or (jget media "url") ""))
         (declared (or (jget media "mime_type") ""))
         (extensions (when (plusp (length (or name ""))) (list (cons +otel-part-ext-media-name+ name)))))
    (multiple-value-bind (data-mime payload) (%otel-split-base64-data-url url)
      (cond
        ((and payload (or (zerop (length declared)) (string-equal data-mime declared)))
         (make-genai-part :part-type "blob" :content payload
                          :mime-type (if (plusp (length declared)) declared data-mime)
                          :modality modality :extensions extensions))
        ((plusp (length url))
         (make-genai-part :part-type "uri" :uri url :mime-type declared
                          :modality modality :extensions extensions))
        (t (make-genai-part :part-type "file" :file-id (or name "")
                            :mime-type declared :modality modality))))))

(defun otel-tool-response (content content-json-b64)
  "Render a tool result onto the schema's response key.
Returns (values RAW B64). A structured result goes on the wire raw; anything
else goes as a JSON string, with the exact bytes on the base64 extension key,
which is the shape the backend decoder inverts. Text content wins: when the
result carries text, the JSON is never inspected."
  (cond
    ((null (%otel-string content-json-b64)) (values (otel-json-string content) nil))
    ((zerop (length (or content "")))
     (let ((text (%otel-decode-b64 content-json-b64)))
       (if (and text (otel-embeddable-json text) (not (otel-json-string-p text)))
           (values text nil)
           (values (otel-json-string content) content-json-b64))))
    (t (values (otel-json-string content) content-json-b64))))

(defun otel-part (part)
  "Map one payload part onto a message part, or NIL when the part carries no
payload the schema has a shape for."
  (cond
    ((nth-value 1 (gethash "tool_call" part))
     (let* ((call (jget part "tool_call"))
            (extensions nil))
      (multiple-value-bind (raw b64) (otel-json-document (jget call "input_json"))
        (let ((provider-type (%otel-part-provider-type part "tool_call")))
          (when provider-type
            (setf extensions (%otel-set-extension extensions +otel-part-ext-provider-type+
                                                  provider-type))))
        (when b64
          (setf extensions (%otel-set-extension extensions +otel-part-ext-arguments-b64+ b64)))
        (make-genai-part :part-type "tool_call"
                         :id (or (jget call "id") "")
                         :name (or (jget call "name") "")
                         :arguments raw
                         :extensions extensions))))
    ((nth-value 1 (gethash "tool_result" part))
     (let* ((result (jget part "tool_result"))
            (extensions nil)
            (provider-type (%otel-part-provider-type part "tool_result")))
       (when provider-type
         (setf extensions (%otel-set-extension extensions +otel-part-ext-provider-type+
                                               provider-type)))
       (let ((name (%otel-string (jget result "name"))))
         (when name
           (setf extensions (%otel-set-extension extensions +otel-part-ext-tool-name+ name))))
       (when (eq (jget result "is_error") t)
         (setf extensions (%otel-set-extension extensions +otel-part-ext-is-error+ :true)))
       (multiple-value-bind (raw b64)
           (otel-tool-response (or (jget result "content") "") (jget result "content_json"))
         (when b64
           (setf extensions (%otel-set-extension extensions +otel-part-ext-response-b64+ b64)))
         (make-genai-part :part-type "tool_call_response"
                          :id (or (jget result "tool_call_id") "")
                          :response raw
                          :extensions extensions))))
    ((nth-value 1 (gethash "media" part))
     (let* ((media (jget part "media"))
            (provider-type (%otel-part-provider-type part "media"))
            (mapped (otel-media-part media
                                     (%otel-string (jget media "kind"))
                                     (%otel-string (jget media "name")))))
       (when provider-type
         (setf (genai-part-extensions mapped)
               (append (list (cons +otel-part-ext-provider-type+ provider-type))
                       (genai-part-extensions mapped))))
       mapped))
    ((nth-value 1 (gethash "thinking" part))
     (let ((provider-type (%otel-part-provider-type part "thinking")))
       (make-genai-part :part-type "reasoning"
                        :content (or (jget part "thinking") "")
                        :extensions (when provider-type
                                      (list (cons +otel-part-ext-provider-type+ provider-type))))))
    ((nth-value 1 (gethash "text" part))
     (let ((provider-type (%otel-part-provider-type part "text")))
       (make-genai-part :part-type "text"
                        :content (or (jget part "text") "")
                        :extensions (when provider-type
                                      (list (cons +otel-part-ext-provider-type+ provider-type))))))
    (t nil)))

(defun otel-messages (messages finish-reason)
  "Map payload messages onto the conventions' message schema.
FINISH-REASON is non-NIL for output messages, where the schema requires the key
even when the value is empty."
  (loop for msg in (%otel-list messages)
        collect (make-genai-message
                 :role (otel-message-role (jget msg "role"))
                 :name (%otel-string (jget msg "name"))
                 :finish-reason finish-reason
                 :parts (remove nil (mapcar #'otel-part (%otel-list (jget msg "parts")))))))

(defun otel-strip-tool-content (messages)
  "Clear the tool call arguments and tool results MESSAGES carry.
A tool_call keeps its id and name and a tool_call_response its id: the call
structure is what makes a trace readable, and only the documents are content.
The base64 escape hatches go with them, since they hold the same bytes.

This is what :NO-TOOL-CONTENT means on a span. On the native path the
generation span carries no messages at all and only the tool execution span
has to gate; in otel mode the messages are on the span, so the gate moves here."
  (dolist (message messages messages)
    (dolist (part (genai-message-parts message))
      (let ((type (genai-part-type part)))
        (when (member type '("tool_call" "server_tool_call") :test #'equal)
          (setf (genai-part-arguments part) nil)
          (setf (genai-part-extensions part)
                (remove +otel-part-ext-arguments-b64+ (genai-part-extensions part)
                        :key #'car :test #'string=)))
        (when (member type '("tool_call_response" "server_tool_call_response")
                      :test #'equal)
          (setf (genai-part-response part) nil)
          (setf (genai-part-extensions part)
                (remove +otel-part-ext-response-b64+ (genai-part-extensions part)
                        :key #'car :test #'string=)))))))

(defun otel-tool-definitions (tools)
  "Map payload tool definitions onto the conventions' tool schema."
  (loop for tool in (%otel-list tools)
        collect (multiple-value-bind (raw b64)
                    (otel-json-document (jget tool "input_schema_json"))
                  (let ((extensions nil))
                    (when (eq (jget tool "deferred") t)
                      (setf extensions (%otel-set-extension extensions
                                                            +otel-tool-ext-deferred+ :true)))
                    (when b64
                      (setf extensions (%otel-set-extension
                                        extensions +otel-tool-ext-input-schema-b64+ b64)))
                    (make-genai-tool-definition
                     :tool-type (%otel-string (jget tool "type"))
                     :name (or (jget tool "name") "")
                     :description (%otel-string (jget tool "description"))
                     :parameters raw
                     :extensions extensions)))))

;;; --- Invocation assembly ---

(defun otel-vendor-generation (payload error-category)
  "The agento11y fields of PAYLOAD that the conventions do not define."
  (let ((metadata (%otel-alist (jget payload "metadata") :sort t))
        (usage (jget payload "usage")))
    (make-genai-vendor-generation
     :id (or (jget payload "id") "")
     :user-id (%otel-string (cdr (assoc "agento11y.user.id" metadata :test #'string=)))
     :tags (%otel-alist (jget payload "tags") :sort t)
     :metadata metadata
     :parent-generation-ids (%otel-list (jget payload "parent_generation_ids"))
     :tool-choice (%otel-string (jget payload "tool_choice"))
     :thinking-enabled (multiple-value-bind (value found)
                           (gethash "thinking_enabled" payload)
                         (if found (and value t) :unset))
     :total-tokens (%otel-int-or-zero (and usage (jget usage "total_tokens")))
     :error-category error-category)))

(defun %otel-gate-tool-content (messages keep)
  (if keep messages (otel-strip-tool-content messages)))

(defun genai-invocation-from-generation (payload capture
                                         &key parent-span-id
                                              error-type error-message error-category)
  "Build the GenAI-semconv invocation for a generation payload.
CAPTURE is the SDK capture mode the generation resolved to. ERROR-TYPE,
ERROR-MESSAGE and ERROR-CATEGORY describe a failed call; the message is already
capture-gated by the caller, the way the native span's status message is."
  (let* ((genai-capture (otel-capture-mode capture))
         (keep-tool-content (capture-keeps-tool-span-content-p capture))
         (model (jget payload "model"))
         (usage (jget payload "usage"))
         (stop (%otel-string (jget payload "stop_reason")))
         (stream (equal (jget payload "mode") "GENERATION_MODE_STREAM"))
         (inv (make-genai-invocation
               :operation (otel-operation-name (jget payload "operation_name"))
               :capture genai-capture
               :trace-id (or (jget payload "trace_id") "")
               :span-id (or (jget payload "span_id") "")
               :parent-span-id parent-span-id
               :provider (otel-provider-name (and model (jget model "provider")))
               :request-model (and model (jget model "name"))
               :response-model (%otel-string (jget payload "response_model"))
               :response-id (%otel-string (jget payload "response_id"))
               :conversation-id (%otel-string (jget payload "conversation_id"))
               :agent-name (%otel-string (jget payload "agent_name"))
               :agent-version (%otel-string (jget payload "agent_version"))
               :stream stream
               :system-instructions (genai-system-instructions-from-text
                                     (jget payload "system_prompt"))
               :input-messages (%otel-gate-tool-content
                                (otel-messages (jget payload "input") nil)
                                keep-tool-content)
               ;; Every output message carries the generation's stop reason:
               ;; the payload has one per generation, not one per message.
               :output-messages (%otel-gate-tool-content
                                 (otel-messages (jget payload "output") (or stop ""))
                                 keep-tool-content)
               :tool-definitions (otel-tool-definitions (jget payload "tools"))
               :finish-reasons (when stop (list stop))
               ;; REPORTED stays unset: the SDK cannot tell an all-zero usage a
               ;; provider returned from a usage it never received, and any
               ;; non-zero count already counts as reported. Setting it here
               ;; would export zero tokens for a call that never reached a
               ;; provider.
               :usage (make-genai-usage
                       :input (%otel-int-or-zero (and usage (jget usage "input_tokens")))
                       :output (%otel-int-or-zero (and usage (jget usage "output_tokens")))
                       :cache-read (%otel-int-or-zero
                                    (and usage (jget usage "cache_read_input_tokens")))
                       :cache-creation (%otel-int-or-zero
                                        (and usage (jget usage "cache_write_input_tokens")))
                       :reasoning (%otel-int-or-zero
                                   (and usage (jget usage "reasoning_tokens"))))
               :max-tokens (%otel-int (jget payload "max_tokens"))
               :temperature (jget payload "temperature")
               :top-p (jget payload "top_p")
               :started-at-nano (iso8601-to-unix-nano (jget payload "started_at"))
               :completed-at-nano (iso8601-to-unix-nano (jget payload "completed_at"))
               :error-type error-type
               :error-message error-message)))
    (setf (genai-invocation-extra-attributes inv)
          (genai-vendor-attributes (otel-vendor-generation payload error-category)
                                   genai-capture))
    inv))

(defun otel-raw-or-json-string (text)
  "TEXT as a raw JSON document when it already is one, else as a JSON string.
The registry carries gen_ai.tool.call.arguments and .result as JSON values, and
this SDK holds them as plain strings that may or may not be JSON."
  (or (otel-embeddable-json text) (otel-json-string text)))
