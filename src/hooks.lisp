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
path, then force https. The hooks API is HTTP, so a gRPC-shaped endpoint
contributes only its host."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) endpoint))
         (without-scheme (if (%has-grpc-scheme-p trimmed)
                             (subseq trimmed (length "grpc://"))
                             trimmed))
         (host (subseq without-scheme 0 (or (position #\/ without-scheme)
                                            (length without-scheme)))))
    (when (plusp (length host))
      (concatenate 'string "https://" host))))

(defun %resolve-hooks-base-url (config)
  "Return the host root used to build the hooks endpoint, or NIL when neither
api-endpoint nor generation-endpoint is set. Schemeless and grpc:// values
resolve to https://host, matching the canonical Python SDK behaviour."
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
the other way around, because that is protobuf JSON."
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
     (let ((payload (jobj "id" (or (tool-call-part-id part) "")
                          "name" (or (tool-call-part-name part) ""))))
       (let ((input-json (tool-call-part-input-json part)))
         (when (and input-json (plusp (length input-json)))
           (setf (gethash "input_json" payload) (%embedded-json input-json))))
       (jobj "kind" "tool_call" "tool_call" payload)))
    ((typep part 'tool-result-part)
     (let ((payload (jobj "is_error" (and (tool-result-part-is-error part) t)
                          "content" (or (tool-result-part-content part) ""))))
       (when (tool-result-part-tool-call-id part)
         (setf (gethash "tool_call_id" payload)
               (tool-result-part-tool-call-id part)))
       (when (tool-result-part-name part)
         (setf (gethash "name" payload) (tool-result-part-name part)))
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

(defun %int-field (value)
  (cond
    ((typep value 'boolean) 0)
    ((integerp value) value)
    ((realp value) (truncate value))
    ((stringp value) (or (ignore-errors (parse-integer value :junk-allowed t)) 0))
    (t 0)))

(defun %parse-wire-message (item)
  "Parse one server-returned message. Only text and thinking parts are
reconstructed, matching the canonical SDK: transformed_input carries
rewritten prompt content, and the server does not send tool parts back."
  (when (hash-table-p item)
    (let* ((role-val (jget item "role"))
           (role (cond
                   ((integerp role-val)
                    (case role-val
                      (2 :assistant)
                      (3 :tool)
                      (t :user)))
                   (t (let ((s (string-downcase (or (and (stringp role-val) role-val)
                                                    "user"))))
                        (cond
                          ((string= s "assistant") :assistant)
                          ((string= s "tool") :tool)
                          ((string= s "system") :system)
                          (t :user))))))
           (parts-raw (jget item "parts"))
           (parts nil))
      (when (vectorp parts-raw)
        (loop for pr across parts-raw
              when (hash-table-p pr)
              do (let ((txt (jget pr "text"))
                       (think (jget pr "thinking")))
                   (cond
                     ((and (stringp txt) (plusp (length txt)))
                      (push (make-text-part txt) parts))
                     ((and (stringp think) (plusp (length think)))
                      (push (make-thinking-part think) parts))))))
      (let ((name (jget item "name")))
        (make-message :role role
                      :parts (nreverse parts)
                      :name (when (and (stringp name) (plusp (length name)))
                              name))))))

(defun %parse-transformed-input (data)
  (when (hash-table-p data)
    (let ((messages nil)
          (system-prompt "")
          (conversation-preview ""))
      (let ((sp (jget data "system_prompt")))
        (when (and (stringp sp) (plusp (length sp)))
          (setf system-prompt sp)))
      (let ((cp (jget data "conversation_preview")))
        (when (and (stringp cp) (plusp (length cp)))
          (setf conversation-preview cp)))
      (let ((raw-msgs (jget data "messages")))
        (when (vectorp raw-msgs)
          (loop for item across raw-msgs
                for parsed = (%parse-wire-message item)
                when parsed do (push parsed messages))))
      (when (or messages
                (plusp (length system-prompt))
                (plusp (length conversation-preview)))
        (make-hook-input :messages (nreverse messages)
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
    (let ((base-url (%resolve-hooks-base-url config)))
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
                            :evaluations (response-evaluations response)))
                   response))))))))))
