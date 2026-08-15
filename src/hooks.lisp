(in-package :agento11y-cl)

;;; Synchronous hook evaluation.
;;;
;;; Mirrors the canonical agento11y SDKs Python hooks.py: a synchronous POST to
;;; /api/v1/hooks:evaluate before (preflight) or after (postflight) an
;;; upstream LLM call. Returns a hook-evaluate-response on allow; signals
;;; agento11y-hook-denied-error on deny. Honours hooks-config-fail-open for
;;; transport failures.

(alex:define-constant +hooks-evaluate-path+ "/api/v1/hooks:evaluate"
  :test #'string=)
(alex:define-constant +hook-timeout-header+ "X-Agento11y-Hook-Timeout-Ms"
  :test #'string=)
(defconstant +default-hook-timeout-sec+ 15.0)
(defconstant +max-hook-response-bytes+ (ash 4 20)) ; 4 MiB

;;; --- Hooks config (defstruct: small, value-like, no inheritance needed) ---

(defstruct hooks-config
  (enabled nil)
  (phases (list :preflight))
  (timeout-sec +default-hook-timeout-sec+)
  (fail-open t))

;;; --- Hook data types (CLOS classes for symmetry with the rest of the SDK) ---

(defclass hook-context ()
  ((model-provider  :initarg :model-provider  :accessor hook-context-model-provider  :initform "")
   (model-name      :initarg :model-name      :accessor hook-context-model-name      :initform "")
   (agent-name      :initarg :agent-name      :accessor hook-context-agent-name      :initform "")
   (agent-version   :initarg :agent-version   :accessor hook-context-agent-version   :initform "")
   (tags            :initarg :tags            :accessor hook-context-tags            :initform nil)
   ;; Correlation fields. trace-id and span-id fall back to *trace-context*
   ;; when left empty, so a hook called inside with-generation lands on the
   ;; same trace as the generation it guards.
   (conversation-id :initarg :conversation-id :accessor hook-context-conversation-id :initform "")
   (trace-id        :initarg :trace-id        :accessor hook-context-trace-id        :initform "")
   (span-id         :initarg :span-id         :accessor hook-context-span-id         :initform "")))

(defun make-hook-context (&key model-provider model-name agent-name agent-version tags
                               conversation-id trace-id span-id)
  (make-instance 'hook-context
    :model-provider (or model-provider "")
    :model-name (or model-name "")
    :agent-name (or agent-name "")
    :agent-version (or agent-version "")
    :tags tags
    :conversation-id (or conversation-id "")
    :trace-id (or trace-id "")
    :span-id (or span-id "")))

(defclass hook-input ()
  ((messages             :initarg :messages             :accessor hook-input-messages             :initform nil)
   (tools                :initarg :tools                :accessor hook-input-tools                :initform nil)
   (system-prompt        :initarg :system-prompt        :accessor hook-input-system-prompt        :initform "")
   (output               :initarg :output               :accessor hook-input-output               :initform nil)
   (conversation-preview :initarg :conversation-preview :accessor hook-input-conversation-preview :initform "")))

(defun make-hook-input (&key messages tools system-prompt output conversation-preview)
  (make-instance 'hook-input
    :messages messages
    :tools tools
    :system-prompt (or system-prompt "")
    :output output
    :conversation-preview (or conversation-preview "")))

(defclass hook-evaluation ()
  ((rule-id        :initarg :rule-id        :accessor evaluation-rule-id        :initform "")
   (evaluator-id   :initarg :evaluator-id   :accessor evaluation-evaluator-id   :initform "")
   (evaluator-kind :initarg :evaluator-kind :accessor evaluation-evaluator-kind :initform "")
   (passed         :initarg :passed         :accessor evaluation-passed         :initform nil)
   (latency-ms     :initarg :latency-ms     :accessor evaluation-latency-ms     :initform 0)
   (explanation    :initarg :explanation    :accessor evaluation-explanation    :initform "")
   (reason         :initarg :reason         :accessor evaluation-reason         :initform "")))

(defclass hook-evaluate-response ()
  ((action            :initarg :action            :accessor response-action            :initform :allow)
   (rule-id           :initarg :rule-id           :accessor response-rule-id           :initform "")
   (reason            :initarg :reason            :accessor response-reason            :initform "")
   (transformed-input :initarg :transformed-input :accessor response-transformed-input :initform nil)
   (evaluations       :initarg :evaluations       :accessor response-evaluations       :initform nil)))

;;; --- URL derivation ---

(defun %strip-trailing-slash (s)
  (if (and (stringp s) (plusp (length s)))
      (string-right-trim "/" s)
      s))

(defun %host-root (url)
  "Extract `scheme://host[:port]` from a full URL. Returns nil for nil/empty."
  (when (and (stringp url) (plusp (length url)))
    (let* ((scheme-end (search "://" url))
           (scheme (when scheme-end (subseq url 0 scheme-end)))
           (rest-start (when scheme-end (+ scheme-end 3))))
      (when (and scheme rest-start)
        (let* ((path-start (position #\/ url :start rest-start))
               (host (subseq url rest-start (or path-start (length url)))))
          (when (plusp (length host))
            (format nil "~a://~a" scheme host)))))))

(defun %has-http-scheme-p (s)
  (and (stringp s)
       (or (alex:starts-with-subseq "http://" s)
           (alex:starts-with-subseq "https://" s))))

(defun %has-grpc-scheme-p (s)
  (and (stringp s) (alex:starts-with-subseq "grpc://" s)))

(defun %https-host-root (endpoint)
  "Host root for a schemeless or grpc:// ENDPOINT: drop the scheme and any
path, then force https. The /api/v1 REST endpoints are HTTP, so a gRPC-shaped
endpoint contributes only its host."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) endpoint))
         (without-scheme (if (%has-grpc-scheme-p trimmed)
                             (subseq trimmed (length "grpc://"))
                             trimmed))
         (host (subseq without-scheme 0 (or (position #\/ without-scheme)
                                            (length without-scheme)))))
    (when (plusp (length host))
      (concatenate 'string "https://" host))))

(defun %resolve-api-base-url (config)
  "Return the host root the /api/v1 REST endpoints hang off: scheme and host,
no path and no trailing slash. NIL when neither api-endpoint nor
generation-endpoint is set. Schemeless and grpc:// values resolve to
https://host, matching the Python SDK.

Used by hook evaluation and by conversation rating. Both take api-endpoint
first and fall back to the generation endpoint's host: the generation endpoint
is an export path, so appending a REST path to it produces a URL no route
answers."
  (let ((api (config-api-endpoint config)))
    (cond
      ((and (stringp api) (plusp (length api)))
       (if (%has-http-scheme-p api)
           (or (%host-root api) (%strip-trailing-slash api))
           (%https-host-root api)))
      (t
       (let ((gen (config-generation-endpoint config)))
         (when (and (stringp gen) (plusp (length gen)))
           (cond
             ((%has-http-scheme-p gen) (%host-root gen))
             ((%has-grpc-scheme-p gen) (%https-host-root gen)))))))))

;;; --- Wire serialization ---

(defun %role-wire (role)
  ;; The hooks REST API only knows user/assistant/tool — system content is
  ;; conveyed via the input.system_prompt field, never as a message role.
  ;; Anything else (including :system) collapses to "user" to match Python.
  (let ((value (cond ((keywordp role) (string-downcase (symbol-name role)))
                     ((stringp role) role)
                     (t ""))))
    (cond ((string= value "user") "user")
          ((string= value "assistant") "assistant")
          ((string= value "tool") "tool")
          (t "user"))))

(defun %b64 (s)
  (cl-base64:string-to-base64-string (or s "")))

(defun %embedded-json (raw)
  "Decode JSON text for embedding in the hook payload. Text that does not
parse is sent as a JSON string, which keeps the body valid and leaves the
content visible to rules instead of dropping it."
  (handler-case (jzon:parse raw)
    (error () raw)))

(defun %serialize-message-part (part)
  "Serialize one message part for the hooks API.

Every part carries its KIND. The server dispatches on that field and only
recovers a missing one for text, so a kind-less thinking, tool call, or
tool result part reaches rule evaluation as an empty part: a tool-filter
rule then sees no tool calls and allows the request.

Tool arguments and tool result payloads go out as embedded JSON, not
base64. The hooks API reads them as raw JSON, so a base64 blob is what
argument-level rules would end up matching against. Generation export is
the other way around, because that is protobuf JSON.

A blank optional field is omitted rather than sent empty."
  (cond
    ((typep part 'text-part)
     (let ((text (text-part-text part)))
       (when (and text (plusp (length text)))
         (jobj "kind" "text" "text" text))))
    ((typep part 'thinking-part)
     (let ((text (thinking-part-text part)))
       (when (and text (plusp (length text)))
         (jobj "kind" "thinking" "thinking" text))))
    ((typep part 'tool-call-part)
     (let ((payload (jobj "name" (or (tool-call-part-name part) ""))))
       (let ((id (tool-call-part-id part)))
         (when (plusp (length (or id "")))
           (setf (gethash "id" payload) id)))
       (let ((input-json (tool-call-part-input-json part)))
         (when (and input-json (plusp (length input-json)))
           (setf (gethash "input_json" payload) (%embedded-json input-json))))
       (jobj "kind" "tool_call" "tool_call" payload)))
    ((typep part 'tool-result-part)
     (let ((payload (jobj)))
       (let ((tool-call-id (tool-result-part-tool-call-id part)))
         (when (plusp (length (or tool-call-id "")))
           (setf (gethash "tool_call_id" payload) tool-call-id)))
       (let ((name (tool-result-part-name part)))
         (when (plusp (length (or name "")))
           (setf (gethash "name" payload) name)))
       (when (tool-result-part-is-error part)
         (setf (gethash "is_error" payload) t))
       (let ((content (tool-result-part-content part)))
         (when (plusp (length (or content "")))
           (setf (gethash "content" payload) content)))
       (let ((cj (tool-result-part-content-json part)))
         (when (and cj (plusp (length cj)))
           (setf (gethash "content_json" payload) (%embedded-json cj))))
       (jobj "kind" "tool_result" "tool_result" payload)))
    (t nil)))

(defun %serialize-message (message)
  (let* ((parts (remove nil (mapcar #'%serialize-message-part
                                    (or (message-parts message) nil))))
         (out (jobj "role" (%role-wire (message-role message))
                    "parts" (coerce parts 'vector))))
    (when (message-name message)
      (setf (gethash "name" out) (message-name message)))
    out))

(defun %serialize-tool (tool)
  "Serialize a tool definition. Accepts hash-tables (already shaped) or
property lists with :name/:description/:type/:input-schema-json/:deferred.

INPUT_SCHEMA_JSON stays base64 even though tool call arguments do not: the
server decodes the tools list straight into its protobuf type, which
rejects embedded JSON for a bytes field."
  (cond
    ((hash-table-p tool) tool)
    ((listp tool)
     (let ((out (jobj "name" (or (getf tool :name) ""))))
       (let ((desc (getf tool :description)))
         (when desc (setf (gethash "description" out) desc)))
       (let ((typ (getf tool :type)))
         (when typ (setf (gethash "type" out) typ)))
       (let ((schema (getf tool :input-schema-json)))
         (when (and schema (plusp (length schema)))
           (setf (gethash "input_schema_json" out) (%b64 schema))))
       (when (getf tool :deferred)
         (setf (gethash "deferred" out) t))
       out))
    (t tool)))

(defun %non-empty-or (preferred fallback)
  "First non-empty string of PREFERRED and FALLBACK, or NIL."
  (find-if (lambda (s) (and (stringp s) (plusp (length s))))
           (list preferred fallback)))

(defun %serialize-context (ctx)
  (let ((out (make-hash-table :test 'equal)))
    (let ((provider (hook-context-model-provider ctx))
          (name (hook-context-model-name ctx)))
      (when (or (and provider (plusp (length provider)))
                (and name (plusp (length name))))
        (setf (gethash "model" out)
              (jobj "provider" (or provider "")
                    "name" (or name "")))))
    (let ((agent (hook-context-agent-name ctx)))
      (when (and agent (plusp (length agent)))
        (setf (gethash "agent_name" out) agent)))
    (let ((agent-ver (hook-context-agent-version ctx)))
      (when (and agent-ver (plusp (length agent-ver)))
        (setf (gethash "agent_version" out) agent-ver)))
    (let ((tags (hook-context-tags ctx)))
      (when tags
        (let ((tag-obj (make-hash-table :test 'equal)))
          (dolist (kv tags)
            (setf (gethash (car kv) tag-obj) (cdr kv)))
          (setf (gethash "tags" out) tag-obj))))
    (let ((conversation-id (hook-context-conversation-id ctx)))
      (when (and conversation-id (plusp (length conversation-id)))
        (setf (gethash "conversation_id" out) conversation-id)))
    (let ((trace-id (%non-empty-or (hook-context-trace-id ctx)
                                   (getf *trace-context* :trace-id)))
          (span-id (%non-empty-or (hook-context-span-id ctx)
                                  (getf *trace-context* :span-id))))
      (when trace-id
        (setf (gethash "trace_id" out) trace-id))
      (when span-id
        (setf (gethash "span_id" out) span-id)))
    out))

(defun %serialize-input (input)
  (let ((out (make-hash-table :test 'equal)))
    (when (hook-input-messages input)
      (setf (gethash "messages" out)
            (coerce (mapcar #'%serialize-message
                            (hook-input-messages input))
                    'vector)))
    (when (hook-input-tools input)
      (setf (gethash "tools" out)
            (coerce (mapcar #'%serialize-tool (hook-input-tools input))
                    'vector)))
    (let ((sp (hook-input-system-prompt input)))
      (when (and sp (plusp (length sp)))
        (setf (gethash "system_prompt" out) sp)))
    (when (hook-input-output input)
      (setf (gethash "output" out)
            (coerce (mapcar #'%serialize-message
                            (hook-input-output input))
                    'vector)))
    (let ((cp (hook-input-conversation-preview input)))
      (when (and cp (plusp (length cp)))
        (setf (gethash "conversation_preview" out) cp)))
    out))

(defun %phase-key (phase)
  "Normalize PHASE to :preflight or :postflight. Keywords and the matching
strings both resolve; anything else falls back to :preflight."
  (let ((value (cond ((keywordp phase) (string-downcase (symbol-name phase)))
                     ((stringp phase) (string-downcase phase))
                     (t ""))))
    (if (string= value "postflight") :postflight :preflight)))

(defun %phase-wire (phase)
  (ecase (%phase-key phase)
    (:preflight "preflight")
    (:postflight "postflight")))

(defun %serialize-request (phase context input)
  (jobj "phase" (%phase-wire phase)
        "context" (%serialize-context context)
        "input" (%serialize-input input)))

;;; --- Wire parsing ---

(defun %string-field (value)
  (if (stringp value) value ""))

(defun %bool-field (value)
  "A wire boolean. Only literal true counts: JSON null parses to a symbol that
is true in Lisp, and Go and Python both read a null flag as false."
  (eq value t))

(defun %int-field (value)
  (cond
    ((typep value 'boolean) 0)
    ((integerp value) value)
    ((realp value) (truncate value))
    ((stringp value) (or (ignore-errors (parse-integer value :junk-allowed t)) 0))
    (t 0)))

(defun %strict-base64-text (value)
  "Decode VALUE as strict base64 and return the text, or NIL when it is not
base64. Whitespace is rejected, matching Python's b64decode(validate=True): a
JSON document that only looks like base64 once its spaces are dropped must not
be treated as one. Go's StdEncoding ignores CR and LF, so a line-wrapped
payload decodes there and not here.

Decoding is base64-string-to-usb8-array plus babel, not
base64-string-to-string: the latter maps each byte through code-char and turns
a multi-byte UTF-8 payload into mojibake. Bytes that are not UTF-8 fall back to
the same per-byte mapping, mirroring Go's string(decoded)."
  (when (and (stringp value)
             (plusp (length value))
             (zerop (mod (length value) 4)))
    (let ((octets (handler-case
                      (cl-base64:base64-string-to-usb8-array value :whitespace :error)
                    (error () nil))))
      (when octets
        (handler-case (babel:octets-to-string octets :encoding :utf-8)
          (error () (map 'string #'code-char octets)))))))

(defun %json-document-p (text)
  (and (stringp text)
       (handler-case (progn (jzon:parse text) t)
         (error () nil))))

(defun %decode-wire-payload (value)
  "Resolve one response payload into the text of a valid JSON document, or NIL
when there is nothing to resolve.

An object or an array arrives already parsed, and is written back as its own
JSON text. A missing field, an empty string and any other non-string resolve to
NIL, so the four rules below all read a non-empty string.

A payload is base64 of whatever bytes the proto field held, and nothing
guarantees those bytes are JSON. The ladder is the one all three SDKs apply
(conformance/hooks/README.md):

  1. strict base64 that decodes to a JSON document becomes that document
  2. strict base64 that decodes to anything else becomes a JSON string holding
     the decoded text
  3. a string that is not base64 but is itself JSON is kept as is
  4. anything else becomes a JSON string holding the original text"
  (cond
    ((hash-table-p value) (jzon:stringify value))
    ((and (vectorp value) (not (stringp value))) (jzon:stringify value))
    ((not (stringp value)) nil)
    ((zerop (length value)) nil)
    (t
     (let ((decoded (%strict-base64-text value)))
       (cond
         ((and decoded (%json-document-p decoded)) decoded)
         (decoded (jzon:stringify decoded))
         ((%json-document-p value) value)
         (t (jzon:stringify value)))))))

(defun %parse-wire-tool-call (payload)
  "A tool-call part from its payload object, or NIL when there is nothing to
recover. A call with no name names no tool, so no rule can have written it."
  (when (hash-table-p payload)
    (let ((name (%string-field (jget payload "name"))))
      (when (plusp (length name))
        (make-tool-call-part :id (%string-field (jget payload "id"))
                             :name name
                             :input-json (%decode-wire-payload
                                          (jget payload "input_json")))))))

(defun %parse-wire-tool-result (payload)
  "A tool-result part from its payload object. Unlike a tool call, a result
names no required field, so an empty payload object still produces a part: Go
and Python both build one, and conformance/hooks/README.md drops a tool_result
only when the payload object itself is absent. JS drops the empty one, which is
the single shape the four SDKs disagree on."
  (when (hash-table-p payload)
    (let ((name (%string-field (jget payload "name"))))
      (make-tool-result-part
       :tool-call-id (%string-field (jget payload "tool_call_id"))
       :name (when (plusp (length name)) name)
       :content (%string-field (jget payload "content"))
       :content-json (%decode-wire-payload (jget payload "content_json"))
       :is-error (%bool-field (jget payload "is_error"))))))

(defun %parse-wire-text (raw)
  (let ((text (%string-field raw)))
    (when (plusp (length text)) (make-text-part text))))

(defun %parse-wire-thinking (raw)
  (let ((text (%string-field raw)))
    (when (plusp (length text)) (make-thinking-part text))))

(defun %parse-wire-part (part)
  "Parse one server-returned part, or NIL when it carries nothing to recover.

The `kind` field decides which field is read. Nothing else is read after it, so
a tool_call without its payload object is dropped even when the part carries
text. Recovering the leftover field would report a part the rule never wrote,
and give each SDK a different transformed_input for one body.

An unknown kind becomes a text part when it carries text, because text is the
only way the server can have described it.

A part with no `kind` at all is the one shape that reads whichever payload
field is set, in the order tool_call, tool_result, thinking, text. The server
always sets `kind`, so that shape can only come from a hand-written or
protobuf-JSON body."
  (when (hash-table-p part)
    (let ((kind (string-downcase (%string-field (jget part "kind")))))
      (cond
        ((string= kind "text") (%parse-wire-text (jget part "text")))
        ((string= kind "thinking") (%parse-wire-thinking (jget part "thinking")))
        ((string= kind "tool_call") (%parse-wire-tool-call (jget part "tool_call")))
        ((string= kind "tool_result") (%parse-wire-tool-result (jget part "tool_result")))
        ((plusp (length kind)) (%parse-wire-text (jget part "text")))
        (t
         ;; The payload field that is set resolves the kind, and the parser
         ;; commits to it exactly as a declared kind does: a payload object
         ;; that recovers nothing drops the part instead of falling through to
         ;; the text beside it. Go, Python and JS resolve it the same way.
         (let ((call (jget part "tool_call"))
               (result (jget part "tool_result")))
           (cond
             ((hash-table-p call) (%parse-wire-tool-call call))
             ((hash-table-p result) (%parse-wire-tool-result result))
             (t (or (%parse-wire-thinking (jget part "thinking"))
                    (%parse-wire-text (jget part "text")))))))))))

(defun %parse-wire-role (raw)
  "Map a wire role onto the SDK's vocabulary. The hooks API knows only
user/assistant/tool, and system content travels in system_prompt, so a system
role collapses to user rather than becoming a role no serializer emits."
  (if (integerp raw)
      (case raw (2 :assistant) (3 :tool) (t :user))
      (let ((value (string-downcase (%string-field raw))))
        (cond
          ((string= value "assistant") :assistant)
          ((string= value "tool") :tool)
          (t :user)))))

(defun %parse-wire-list (raw parser)
  "Parse a wire array through PARSER, dropping every element it rejects."
  (let ((out nil))
    (when (and (vectorp raw) (not (stringp raw)))
      (loop for item across raw
            for parsed = (funcall parser item)
            when parsed do (push parsed out)))
    (nreverse out)))

(defun %parse-wire-message (item)
  "Parse one server-returned message. A message carries its body in `parts` in
both directions; there is no `content` field on a wire message."
  (when (hash-table-p item)
    (let ((name (jget item "name")))
      (make-message :role (%parse-wire-role (jget item "role"))
                    :parts (%parse-wire-list (jget item "parts") #'%parse-wire-part)
                    :name (when (and (stringp name) (plusp (length name)))
                            name)))))

(defun %parse-wire-tool (item)
  "Parse one server-returned tool definition into the plist %SERIALIZE-TOOL
takes, so a transform can be sent back unchanged. INPUT_SCHEMA_JSON is base64
in both directions, and is decoded here because %SERIALIZE-TOOL re-encodes it."
  (when (hash-table-p item)
    (let ((name (%string-field (jget item "name"))))
      (when (plusp (length name))
        (let ((description (jget item "description"))
              (type (jget item "type"))
              (schema (%decode-wire-payload (jget item "input_schema_json"))))
          (append (list :name name)
                  (when (stringp description) (list :description description))
                  (when (stringp type) (list :type type))
                  (when schema (list :input-schema-json schema))
                  (when (%bool-field (jget item "deferred")) (list :deferred t))))))))

(defun %parse-transformed-input (data)
  "Parse a transformed_input object, or NIL when it carries no transform.
A body that rewrites only the output or only the tools is still a transform."
  (when (hash-table-p data)
    (let ((messages (%parse-wire-list (jget data "messages") #'%parse-wire-message))
          (output (%parse-wire-list (jget data "output") #'%parse-wire-message))
          (tools (%parse-wire-list (jget data "tools") #'%parse-wire-tool))
          (system-prompt "")
          (conversation-preview ""))
      (let ((sp (jget data "system_prompt")))
        (when (and (stringp sp) (plusp (length sp)))
          (setf system-prompt sp)))
      (let ((cp (jget data "conversation_preview")))
        (when (and (stringp cp) (plusp (length cp)))
          (setf conversation-preview cp)))
      (when (or messages output tools
                (plusp (length system-prompt))
                (plusp (length conversation-preview)))
        (make-hook-input :messages messages
                         :output output
                         :tools tools
                         :system-prompt system-prompt
                         :conversation-preview conversation-preview)))))

(defun %parse-evaluations (raw)
  (let ((out nil))
    (when (vectorp raw)
      (loop for entry across raw
            when (hash-table-p entry)
            do (push (make-instance 'hook-evaluation
                       :rule-id (%string-field (jget entry "rule_id"))
                       :evaluator-id (%string-field (jget entry "evaluator_id"))
                       :evaluator-kind (%string-field (jget entry "evaluator_kind"))
                       :passed (and (jget entry "passed") t)
                       :latency-ms (%int-field (jget entry "latency_ms"))
                       :explanation (%string-field (jget entry "explanation"))
                       :reason (%string-field (jget entry "reason")))
                     out)))
    (nreverse out)))

(defun %parse-response (parsed)
  (unless (hash-table-p parsed)
    (return-from %parse-response (%allow-response)))
  (let* ((action-raw (jget parsed "action"))
         (action (if (and (stringp action-raw)
                          (string= action-raw "deny"))
                     :deny
                     :allow)))
    (make-instance 'hook-evaluate-response
      :action action
      :rule-id (%string-field (jget parsed "rule_id"))
      :reason (%string-field (jget parsed "reason"))
      :transformed-input (%parse-transformed-input (jget parsed "transformed_input"))
      :evaluations (%parse-evaluations (jget parsed "evaluations")))))

(defun %allow-response ()
  (make-instance 'hook-evaluate-response :action :allow))

(defun %fail-open-or-raise (config hooks detail)
  (if (hooks-config-fail-open hooks)
      (progn
        ;; A dead evaluator allows every request. Without this line the outage
        ;; looks the same as a clean allow.
        (agento11y-log config :warn "hooks"
                   (format nil "hook evaluation failed, allowing request (fail-open): ~a"
                           detail))
        (%allow-response))
      (error 'agento11y-hook-transport-error
             :message (format nil "hook evaluation failed: ~a" detail))))

(defun %positive-timeout (value)
  "VALUE when it is a positive real, else NIL. Zero and negative timeouts fall
back to the default instead of producing a 1 ms budget."
  (when (and (realp value) (plusp value)) value))

;;; --- HTTP ---

(defun %hook-http-post (config url headers body timeout-sec)
  "POST and return (values body status). Real dexador returns octets; the
test-only http-fn injection may return a string. Caller normalizes."
  (let ((http-fn (config-http-fn config)))
    (if http-fn
        (funcall http-fn url :headers headers :content body)
        (dexador:post url
                      :headers headers
                      :content body
                      :force-binary t
                      :connect-timeout timeout-sec
                      :read-timeout timeout-sec))))

(defun %body-octet-length (body)
  "Byte length of BODY, whether it is octets, a string, or nil."
  (cond
    ((null body) 0)
    ((and (vectorp body)
          (subtypep (array-element-type body) '(unsigned-byte 8)))
     (length body))
    ((stringp body)
     (handler-case (babel:string-size-in-octets body :encoding :utf-8)
       (error () (length body))))
    (t 0)))

(defun %decode-response-body (body)
  "Coerce BODY to a UTF-8 string. Octets are decoded with replacement on
invalid sequences to mirror Python's errors='replace' behaviour."
  (cond
    ((null body) "")
    ((stringp body) body)
    ((and (vectorp body)
          (subtypep (array-element-type body) '(unsigned-byte 8)))
     (handler-case (babel:octets-to-string body :encoding :utf-8)
       (error () (map 'string #'code-char body))))
    (t "")))

;;; --- Public entry point ---

(defun evaluate-hook (client &key (phase :preflight) context input timeout-sec)
  "Synchronously evaluate a hook for CLIENT.

PHASE is :preflight or :postflight. CONTEXT and INPUT are hook-context and
hook-input instances respectively (constructed via make-hook-context /
make-hook-input). TIMEOUT-SEC overrides the configured per-call timeout.

Returns a hook-evaluate-response when the server allows. Signals
agento11y-hook-denied-error when the server denies. On transport failure,
honours (hooks-config-fail-open hooks): when t (default) returns a
synthetic allow response, when nil signals agento11y-hook-transport-error."
  (let* ((config (client-config client))
         (hooks (or (config-hooks-config config) (make-hooks-config))))
    (unless (hooks-config-enabled hooks)
      (return-from evaluate-hook (%allow-response)))
    (let ((phases (or (hooks-config-phases hooks) (list :preflight))))
      ;; Normalize both sides: a caller passing "preflight" must not silently
      ;; skip the hook because the configured phases hold keywords.
      (unless (member (%phase-key phase) (mapcar #'%phase-key phases))
        (return-from evaluate-hook (%allow-response))))
    (let ((base-url (%resolve-api-base-url config)))
      (unless base-url
        (return-from evaluate-hook
          (%fail-open-or-raise config hooks "api endpoint is required")))
      (let* ((endpoint (concatenate 'string
                                    (%strip-trailing-slash base-url)
                                    +hooks-evaluate-path+))
             (effective-timeout (or (%positive-timeout timeout-sec)
                                    (%positive-timeout (hooks-config-timeout-sec hooks))
                                    +default-hook-timeout-sec+))
             (timeout-ms (max 1 (round (* effective-timeout 1000))))
             (ctx (or context (make-hook-context)))
             (in (or input (make-hook-input)))
             (payload (jzon:stringify (%serialize-request phase ctx in)))
             (auth (build-auth-headers config))
             (headers (append (list (cons "Content-Type" "application/json")
                                    (cons +hook-timeout-header+
                                          (princ-to-string timeout-ms)))
                              auth)))
        (multiple-value-bind (resp-body status)
            (handler-case
                (%hook-http-post config endpoint headers payload effective-timeout)
              (error (e)
                (return-from evaluate-hook
                  (%fail-open-or-raise config hooks (princ-to-string e)))))
          (cond
            ((or (null status)
                 (not (integerp status))
                 (< status 200)
                 (>= status 300))
             (return-from evaluate-hook
               (%fail-open-or-raise config hooks
                                    (format nil "status ~a"
                                            (or status "?")))))
            ((null resp-body)
             (return-from evaluate-hook
               (%fail-open-or-raise config hooks "empty hook response payload")))
            ((> (%body-octet-length resp-body) +max-hook-response-bytes+)
             (return-from evaluate-hook
               (%fail-open-or-raise config hooks "hook response too large")))
            (t
             (let* ((decoded (%decode-response-body resp-body))
                    (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                          decoded)))
               (when (zerop (length trimmed))
                 (return-from evaluate-hook
                   (%fail-open-or-raise config hooks "empty hook response payload")))
               (let ((parsed (handler-case (jzon:parse trimmed)
                               (error (e)
                                 (return-from evaluate-hook
                                   (%fail-open-or-raise
                                    config hooks
                                    (format nil "invalid JSON response: ~a"
                                            (princ-to-string e))))))))
                 (let ((response (%parse-response parsed)))
                   (when (eq (response-action response) :deny)
                     (error 'agento11y-hook-denied-error
                            :message (or (response-reason response) "")
                            :rule-id (response-rule-id response)
                            :reason (response-reason response)
                            :evaluations (response-evaluations response)
                            :transformed-input (response-transformed-input response)))
                   response))))))))))
