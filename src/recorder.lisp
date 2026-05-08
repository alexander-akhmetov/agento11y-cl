(in-package :sigil-cl)

(defconstant +tool-attr-max-length+ 2000)

;;; --- Trace context ---

(defvar *trace-context* nil
  "Current trace context plist (:trace-id ... :span-id ...).
Set by generation recorder, read by tool/embedding recorders.
Each with-generation call binds this per-thread via LET.")

;;; --- Base recorder ---

(defclass recorder ()
  ((client      :initarg :client      :accessor recorder-client)
   (started-at  :initarg :started-at  :accessor recorder-started-at  :initform nil)
   (completed-at :initarg :completed-at :accessor recorder-completed-at :initform nil)
   (call-error  :initarg :call-error  :accessor recorder-call-error  :initform nil)
   (ended-p     :initarg :ended-p     :accessor recorder-ended-p     :initform nil)))

(defgeneric set-result (recorder &key &allow-other-keys))
(defgeneric set-call-error (recorder error-string))
(defgeneric recorder-end (recorder))

(defmethod set-call-error ((rec recorder) error-string)
  (setf (recorder-call-error rec) error-string)
  rec)

;;; --- recorder-end :around — shared lifecycle for all recorder types ---

(defmethod recorder-end :around ((rec recorder))
  (when (recorder-ended-p rec) (return-from recorder-end nil))
  (setf (recorder-ended-p rec) t)
  (unless (recorder-completed-at rec)
    (setf (recorder-completed-at rec) (iso8601-now)))
  (let ((client (recorder-client rec)))
    (handler-case
        (progn
          (call-next-method)
          ;; Wake the background worker
          (bt2:with-lock-held ((client-lock client))
            (bt2:condition-notify (client-wake-cv client)))
          ;; Metrics callback
          (let ((metrics-fn (config-metrics-fn (client-config client))))
            (when metrics-fn
              (handler-case
                  (funcall metrics-fn (recorder-type-key rec) rec)
                (error (e)
                  (sigil-log (client-config client) :warn "recorder"
                            (format nil "metrics callback failed: ~a" (princ-to-string e))))))))
      (error (e)
        (handler-case
            (sigil-log (client-config client) :warn "recorder"
                      (format nil "~a end failed: ~a" (type-of rec) (princ-to-string e)))
          (error () nil))))))

(defgeneric recorder-type-key (recorder)
  (:method ((rec recorder)) :unknown))

;;; --- Role mapping ---

(defun role-string (role)
  (case role
    (:user      "MESSAGE_ROLE_USER")
    (:assistant "MESSAGE_ROLE_ASSISTANT")
    (:tool      "MESSAGE_ROLE_TOOL")
    (t          "MESSAGE_ROLE_UNSPECIFIED")))

;;; --- Capture mode predicates ---

(defun capture-keeps-content-p (mode)
  "True when MODE keeps full generation content: message text, system prompt,
call_error, and span status messages.
:full and :no-tool-content keep content; :metadata-with-system-prompt and
:metadata-only redact it."
  (or (eq mode :full) (eq mode :no-tool-content)))

(defun capture-keeps-tool-span-content-p (mode)
  "True when MODE keeps tool execution span arguments and results.
Only :full keeps tool span content; :no-tool-content, :metadata-with-system-prompt,
and :metadata-only redact it."
  (eq mode :full))

;;; --- Message serialization ---

(defun serialize-part (part capture-mode)
  "Serialize a message part to JSON hash-table. Respects capture mode."
  (let ((redact (eq capture-mode :metadata-only)))
    (typecase part
      (text-part
       (jobj "text" (if redact "" (text-part-text part))))
      (thinking-part
       (unless redact
         (jobj "thinking" (thinking-part-text part)
               "metadata" (jobj "provider_type" "thinking"))))
      (tool-call-part
       (jobj "tool_call"
             (jobj "id" (tool-call-part-id part)
                   "name" (tool-call-part-name part)
                   "input_json" (if redact ""
                                    (cl-base64:string-to-base64-string
                                     (or (tool-call-part-input-json part) ""))))
             "metadata" (jobj "provider_type" "tool_use")))
      (tool-result-part
       (let ((tr (jobj "tool_call_id" (tool-result-part-tool-call-id part)
                       "content" (if redact "" (tool-result-part-content part))
                       "is_error" (if (tool-result-part-is-error part) t nil))))
         (when (tool-result-part-name part)
           (setf (gethash "name" tr) (tool-result-part-name part)))
         (when (and (not redact) (tool-result-part-content part))
           (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (tool-result-part-content part))))
             (when (and (plusp (length trimmed))
                        (or (char= (char trimmed 0) #\{)
                            (char= (char trimmed 0) #\[)))
               (handler-case
                   (let ((parsed (jzon:parse trimmed)))
                     (when (or (hash-table-p parsed) (vectorp parsed))
                       (setf (gethash "content_json" tr)
                             (cl-base64:string-to-base64-string trimmed))))
                 (error () nil)))))
         (jobj "tool_result" tr
               "metadata" (jobj "provider_type" "tool_result"))))
      (t nil))))

(defun serialize-message (msg capture-mode)
  "Serialize a message object to JSON hash-table."
  (let ((parts (remove nil (mapcar (lambda (p) (serialize-part p capture-mode))
                                   (message-parts msg)))))
    (jobj "role" (role-string (message-role msg))
          "parts" (coerce parts 'vector))))

;;; --- Usage serialization ---

(defun serialize-usage (usage)
  "Serialize token-usage to JSON hash-table.
Uses the stored total-tokens value (which may differ from input+output
when reasoning or cache tokens are counted separately)."
  (if usage
      (jobj "input_tokens" (token-usage-input-tokens usage)
            "output_tokens" (token-usage-output-tokens usage)
            "total_tokens" (token-usage-total-tokens usage)
            "reasoning_tokens" (token-usage-reasoning-tokens usage)
            "cache_read_input_tokens" (token-usage-cache-read-tokens usage)
            "cache_creation_input_tokens" (token-usage-cache-creation-tokens usage))
      (jobj "input_tokens" 0 "output_tokens" 0 "total_tokens" 0)))

;;; --- Tool schema serialization ---

(defun serialize-tools (tools)
  "Serialize tool definition plists to JSON array."
  (if tools
      (coerce
       (loop for tool in tools
             collect (jobj "name" (or (getf tool :name) "")
                           "description" (or (getf tool :description) "")
                           "type" "function"
                           "input_schema_json"
                           (handler-case
                               (cl-base64:string-to-base64-string
                                (jzon:stringify (getf tool :parameters)))
                             (error () (cl-base64:string-to-base64-string "{}")))
                           "deferred" (if (getf tool :deferred) t nil)))
       'vector)
      (vector)))

;;; --- Truncation ---

(defun truncate-for-span (str)
  (if (and (stringp str) (> (length str) +tool-attr-max-length+))
      (subseq str 0 +tool-attr-max-length+)
      str))

;;; ================================================================
;;; Generation recorder
;;; ================================================================

(defclass generation-recorder (recorder)
  ((mode              :initarg :mode              :accessor gen-rec-mode              :initform :sync)
   (conversation-id   :initarg :conversation-id   :accessor gen-rec-conversation-id   :initform nil)
   (conversation-title :initarg :conversation-title :accessor gen-rec-conversation-title :initform nil)
   (user-id           :initarg :user-id           :accessor gen-rec-user-id           :initform nil)
   (agent-name        :initarg :agent-name        :accessor gen-rec-agent-name        :initform nil)
   (agent-version     :initarg :agent-version     :accessor gen-rec-agent-version     :initform nil)
   (model-provider    :initarg :model-provider    :accessor gen-rec-model-provider    :initform nil)
   (model-name        :initarg :model-name        :accessor gen-rec-model-name        :initform nil)
   ;; Set via set-result
   (input-messages    :initarg :input-messages    :accessor gen-rec-input-messages    :initform nil)
   (output-messages   :initarg :output-messages   :accessor gen-rec-output-messages   :initform nil)
   (system-prompt     :initarg :system-prompt     :accessor gen-rec-system-prompt     :initform nil)
   (tools             :initarg :tools             :accessor gen-rec-tools             :initform nil)
   (usage             :initarg :usage             :accessor gen-rec-usage             :initform nil)
   (stop-reason       :initarg :stop-reason       :accessor gen-rec-stop-reason       :initform nil)
   (response-id       :initarg :response-id       :accessor gen-rec-response-id       :initform nil)
   (response-model    :initarg :response-model    :accessor gen-rec-response-model    :initform nil)
   (tags              :initarg :tags              :accessor gen-rec-tags              :initform nil)
   (metadata          :initarg :metadata          :accessor gen-rec-metadata          :initform nil)
   ;; Request controls
   (temperature       :initarg :temperature       :accessor gen-rec-temperature       :initform nil)
   (top-p             :initarg :top-p             :accessor gen-rec-top-p             :initform nil)
   (max-tokens        :initarg :max-tokens        :accessor gen-rec-max-tokens        :initform nil)
   (tool-choice       :initarg :tool-choice       :accessor gen-rec-tool-choice       :initform nil)
   (thinking-enabled  :initarg :thinking-enabled  :accessor gen-rec-thinking-enabled  :initform :unset)
   (parent-generation-ids :initarg :parent-generation-ids :accessor gen-rec-parent-generation-ids :initform nil)
   ;; Timing
   (duration-seconds  :initarg :duration-seconds  :accessor gen-rec-duration-seconds  :initform nil)
   (ttft-seconds      :initarg :ttft-seconds      :accessor gen-rec-ttft-seconds      :initform nil)
   ;; IDs (generated at start)
   (generation-id     :initarg :generation-id     :accessor gen-rec-generation-id     :initform nil)
   (trace-id          :initarg :trace-id          :accessor gen-rec-trace-id          :initform nil)
   (span-id           :initarg :span-id           :accessor gen-rec-span-id           :initform nil)
   (parent-span-id    :initarg :parent-span-id    :accessor gen-rec-parent-span-id    :initform nil)))

(defmethod recorder-type-key ((rec generation-recorder)) :generation)

(defmethod set-result ((rec generation-recorder) &key input-messages output-messages
                                                       system-prompt tools usage
                                                       stop-reason response-id response-model
                                                       tags metadata
                                                       temperature top-p max-tokens
                                                       tool-choice thinking-enabled
                                                       parent-generation-ids
                                                       duration-seconds ttft-seconds)
  (when input-messages  (setf (gen-rec-input-messages rec) input-messages))
  (when output-messages (setf (gen-rec-output-messages rec) output-messages))
  (when system-prompt   (setf (gen-rec-system-prompt rec) system-prompt))
  (when tools           (setf (gen-rec-tools rec) tools))
  (when usage           (setf (gen-rec-usage rec) usage))
  (when stop-reason     (setf (gen-rec-stop-reason rec) stop-reason))
  (when response-id     (setf (gen-rec-response-id rec) response-id))
  (when response-model  (setf (gen-rec-response-model rec) response-model))
  (when tags            (setf (gen-rec-tags rec) tags))
  (when metadata        (setf (gen-rec-metadata rec) metadata))
  (when temperature     (setf (gen-rec-temperature rec) temperature))
  (when top-p           (setf (gen-rec-top-p rec) top-p))
  (when max-tokens      (setf (gen-rec-max-tokens rec) max-tokens))
  (when tool-choice     (setf (gen-rec-tool-choice rec) tool-choice))
  (unless (eq thinking-enabled nil)
    (when (member thinking-enabled '(t nil :unset))
      (setf (gen-rec-thinking-enabled rec) thinking-enabled)))
  (when parent-generation-ids
    (setf (gen-rec-parent-generation-ids rec) parent-generation-ids))
  (when duration-seconds (setf (gen-rec-duration-seconds rec) duration-seconds))
  (when ttft-seconds     (setf (gen-rec-ttft-seconds rec) ttft-seconds))
  rec)

(defun build-generation-payload (rec config)
  "Build a generation JSON hash-table from the recorder state."
  (let* ((capture (config-content-capture-mode config))
         (capture-content (capture-keeps-content-p capture))
         (capture-sys (or capture-content (eq capture :metadata-with-system-prompt)))
         (mode-str (if (eq (gen-rec-mode rec) :stream)
                       "GENERATION_MODE_STREAM" "GENERATION_MODE_SYNC"))
         (op-name (if (eq (gen-rec-mode rec) :stream) "streamText" "generateText"))
         (stop (or (gen-rec-stop-reason rec)
                   (if (recorder-call-error rec) "error" "end_turn")))
         ;; Build metadata: SDK fields + caller metadata merged
         (meta (jobj "sigil.sdk.name" +sdk-name+))
         (_ (progn
              (let ((uid (or (gen-rec-user-id rec) (config-user-id config))))
                (when uid
                  (setf (gethash "sigil.user.id" meta) (princ-to-string uid))))
              ;; Merge caller-supplied metadata
              (let ((user-meta (gen-rec-metadata rec)))
                (when (and user-meta (hash-table-p user-meta))
                  (maphash (lambda (k v) (setf (gethash k meta) v)) user-meta))
                (when (and user-meta (listp user-meta))
                  (loop for (k . v) in user-meta
                        when (and (stringp k) v)
                        do (setf (gethash k meta) v))))
              ;; Store conversation title in metadata (not as top-level proto field)
              (when (gen-rec-conversation-title rec)
                (setf (gethash "sigil.conversation.title" meta)
                      (gen-rec-conversation-title rec)))))
         (gen (jobj "id" (gen-rec-generation-id rec)
                    "mode" mode-str
                    "operation_name" op-name
                    "model" (jobj "provider" (or (gen-rec-model-provider rec) "")
                                  "name" (or (gen-rec-model-name rec) ""))
                    "usage" (serialize-usage (gen-rec-usage rec))
                    "stop_reason" stop
                    "started_at" (recorder-started-at rec)
                    "completed_at" (or (recorder-completed-at rec) (iso8601-now))
                    "metadata" meta
                    "raw_artifacts" (vector))))
    (declare (ignore _))
    ;; Optional fields
    (when (gen-rec-conversation-id rec)
      (setf (gethash "conversation_id" gen) (gen-rec-conversation-id rec)))
    (when (gen-rec-agent-name rec)
      (setf (gethash "agent_name" gen) (gen-rec-agent-name rec)))
    (when (gen-rec-agent-version rec)
      (setf (gethash "agent_version" gen) (gen-rec-agent-version rec)))
    (when (recorder-call-error rec)
      (setf (gethash "call_error" gen)
            (if capture-content (recorder-call-error rec) "<redacted>")))
    (when (gen-rec-response-id rec)
      (setf (gethash "response_id" gen) (gen-rec-response-id rec)))
    (when (gen-rec-response-model rec)
      (setf (gethash "response_model" gen) (gen-rec-response-model rec)))
    ;; Request controls
    (when (gen-rec-temperature rec)
      (setf (gethash "temperature" gen) (gen-rec-temperature rec)))
    (when (gen-rec-top-p rec)
      (setf (gethash "top_p" gen) (gen-rec-top-p rec)))
    (when (gen-rec-max-tokens rec)
      (setf (gethash "max_tokens" gen) (gen-rec-max-tokens rec)))
    (when (gen-rec-tool-choice rec)
      (setf (gethash "tool_choice" gen) (gen-rec-tool-choice rec)))
    (unless (eq (gen-rec-thinking-enabled rec) :unset)
      (setf (gethash "thinking_enabled" gen) (if (gen-rec-thinking-enabled rec) t nil)))
    (when (gen-rec-parent-generation-ids rec)
      (setf (gethash "parent_generation_ids" gen)
            (coerce (gen-rec-parent-generation-ids rec) 'vector)))
    ;; Tags (config tags + recorder tags merged)
    (let ((all-tags (append (config-tags config) (gen-rec-tags rec))))
      (when all-tags
        (let ((tags-obj (jobj)))
          (dolist (pair all-tags)
            (when (and (consp pair) (stringp (car pair)) (stringp (cdr pair)))
              (setf (gethash (car pair) tags-obj) (cdr pair))))
          (when (plusp (hash-table-count tags-obj))
            (setf (gethash "tags" gen) tags-obj)))))
    ;; System prompt
    (when (and capture-sys (gen-rec-system-prompt rec))
      (setf (gethash "system_prompt" gen) (gen-rec-system-prompt rec)))
    ;; Messages — in metadata-only mode, preserve structure with redacted content
    (when (gen-rec-input-messages rec)
      (setf (gethash "input" gen)
            (coerce (mapcar (lambda (m) (serialize-message m capture))
                            (gen-rec-input-messages rec))
                    'vector)))
    (when (gen-rec-output-messages rec)
      (setf (gethash "output" gen)
            (coerce (mapcar (lambda (m) (serialize-message m capture))
                            (gen-rec-output-messages rec))
                    'vector)))
    ;; Tools (names/descriptions are metadata, not gated)
    (when (gen-rec-tools rec)
      (setf (gethash "tools" gen) (serialize-tools (gen-rec-tools rec))))
    gen))

(defun build-generation-span (rec config gen-payload)
  "Build an OTel span for a generation. Returns span hash-table."
  (let* ((trace-id (gen-rec-trace-id rec))
         (span-id (gen-rec-span-id rec))
         (op-name (if (eq (gen-rec-mode rec) :stream) "streamText" "generateText"))
         (provider (or (gen-rec-model-provider rec) ""))
         (model (or (gen-rec-model-name rec) ""))
         (attrs (common-span-attrs config
                  :provider provider
                  :model model
                  :agent-name (gen-rec-agent-name rec)
                  :agent-version (gen-rec-agent-version rec)
                  :conversation-id (gen-rec-conversation-id rec))))
    (push (otel-string-attr "gen_ai.operation.name" op-name) attrs)
    (when gen-payload
      (let ((gen-id (jget gen-payload "id")))
        (when (and gen-id (plusp (length gen-id)))
          (push (otel-string-attr "sigil.generation.id" gen-id) attrs)))
      (setf (gethash "trace_id" gen-payload) trace-id)
      (setf (gethash "span_id" gen-payload) span-id))
    ;; Response metadata
    (when (gen-rec-response-id rec)
      (push (otel-string-attr "gen_ai.response.id" (gen-rec-response-id rec)) attrs))
    (when (gen-rec-response-model rec)
      (push (otel-string-attr "gen_ai.response.model" (gen-rec-response-model rec)) attrs))
    ;; Stop reason
    (let ((stop (or (gen-rec-stop-reason rec)
                    (if (recorder-call-error rec) "error" "end_turn"))))
      (push (otel-string-array-attr "gen_ai.response.finish_reasons" (list stop)) attrs))
    ;; Usage attributes
    (let ((usage (gen-rec-usage rec)))
      (when usage
        (let ((input (token-usage-input-tokens usage)))
          (when (plusp input)
            (push (otel-int-attr "gen_ai.usage.input_tokens" input) attrs)))
        (let ((output (token-usage-output-tokens usage)))
          (when (plusp output)
            (push (otel-int-attr "gen_ai.usage.output_tokens" output) attrs)))
        (let ((reasoning (token-usage-reasoning-tokens usage)))
          (when (plusp reasoning)
            (push (otel-int-attr "gen_ai.usage.reasoning_tokens" reasoning) attrs)))
        (let ((cr (token-usage-cache-read-tokens usage)))
          (when (plusp cr)
            (push (otel-int-attr "gen_ai.usage.cache_read_input_tokens" cr) attrs)))
        (let ((cc (token-usage-cache-creation-tokens usage)))
          (when (plusp cc)
            (push (otel-int-attr "gen_ai.usage.cache_creation_input_tokens" cc) attrs)))))
    ;; Request controls
    (when (gen-rec-temperature rec)
      (push (otel-string-attr "gen_ai.request.temperature"
                               (princ-to-string (gen-rec-temperature rec))) attrs))
    (when (gen-rec-max-tokens rec)
      (push (otel-int-attr "gen_ai.request.max_tokens" (gen-rec-max-tokens rec)) attrs))
    (when (gen-rec-top-p rec)
      (push (otel-string-attr "gen_ai.request.top_p"
                               (princ-to-string (gen-rec-top-p rec))) attrs))
    (when (gen-rec-tool-choice rec)
      (push (otel-string-attr "sigil.gen_ai.request.tool_choice"
                               (gen-rec-tool-choice rec)) attrs))
    (unless (eq (gen-rec-thinking-enabled rec) :unset)
      (push (otel-bool-attr "sigil.gen_ai.request.thinking.enabled"
                             (gen-rec-thinking-enabled rec)) attrs))
    ;; Error attributes
    (when (recorder-call-error rec)
      (push (otel-string-attr "error.type" "provider_call_error") attrs)
      (let ((category (classify-error (recorder-call-error rec))))
        (when category
          (push (otel-string-attr "error.category" category) attrs))))
    ;; Build span
    (let* ((capture (config-content-capture-mode config))
           (capture-content (capture-keeps-content-p capture))
           (start-nano (iso8601-to-unix-nano (recorder-started-at rec)))
           (end-nano (if (and start-nano (gen-rec-duration-seconds rec))
                         (unix-nano-plus-seconds start-nano (gen-rec-duration-seconds rec))
                         (or (iso8601-to-unix-nano (recorder-completed-at rec)) "0"))))
      (build-span :trace-id trace-id
                  :span-id span-id
                  :parent-span-id (gen-rec-parent-span-id rec)
                  :name (format nil "~a ~a" op-name (or model "unknown"))
                  :kind 3
                  :start-time-unix-nano start-nano
                  :end-time-unix-nano end-nano
                  :attributes (coerce (nreverse attrs) 'vector)
                  :status-code (if (recorder-call-error rec) 2 1)
                  :status-message (if (recorder-call-error rec)
                                      (if capture-content (recorder-call-error rec) "<redacted>")
                                      "")))))

(defmethod recorder-end ((rec generation-recorder))
  (let ((config (client-config (recorder-client rec))))
    (let ((gen-payload nil)
          (span nil))
      (when (config-generation-enabled config)
        (setf gen-payload (build-generation-payload rec config)))
      (when (config-traces-enabled config)
        (setf span (build-generation-span rec config gen-payload)))
      (when gen-payload
        (queue-enqueue (client-generation-queue (recorder-client rec)) gen-payload))
      (when span
        (queue-enqueue (client-trace-queue (recorder-client rec)) span)))))

;;; ================================================================
;;; Tool execution recorder
;;; ================================================================

(defclass tool-execution-recorder (recorder)
  ((tool-name        :initarg :tool-name        :accessor tool-rec-tool-name        :initform nil)
   (tool-call-id     :initarg :tool-call-id     :accessor tool-rec-tool-call-id     :initform nil)
   (tool-type        :initarg :tool-type        :accessor tool-rec-tool-type        :initform nil)
   (tool-description :initarg :tool-description :accessor tool-rec-tool-description :initform nil)
   (conversation-id  :initarg :conversation-id  :accessor tool-rec-conversation-id  :initform nil)
   (agent-name       :initarg :agent-name       :accessor tool-rec-agent-name       :initform nil)
   (agent-version    :initarg :agent-version    :accessor tool-rec-agent-version    :initform nil)
   (model-provider   :initarg :model-provider   :accessor tool-rec-model-provider   :initform nil)
   (model-name       :initarg :model-name       :accessor tool-rec-model-name       :initform nil)
   ;; Set via set-result
   (arguments        :initarg :arguments        :accessor tool-rec-arguments        :initform nil)
   (result           :initarg :result           :accessor tool-rec-result           :initform nil)
   (error-message    :initarg :error-message    :accessor tool-rec-error-message    :initform nil)
   (duration-seconds :initarg :duration-seconds :accessor tool-rec-duration-seconds :initform nil)))

(defmethod recorder-type-key ((rec tool-execution-recorder)) :tool-execution)

(defmethod set-result ((rec tool-execution-recorder) &key arguments result
                                                           error-message duration-seconds)
  (when arguments      (setf (tool-rec-arguments rec) arguments))
  (when result         (setf (tool-rec-result rec) result))
  (when error-message  (setf (tool-rec-error-message rec) error-message))
  (when duration-seconds (setf (tool-rec-duration-seconds rec) duration-seconds))
  rec)

(defmethod recorder-end ((rec tool-execution-recorder))
  (let ((config (client-config (recorder-client rec))))
    (when (config-traces-enabled config)
      (let* ((capture (config-content-capture-mode config))
             (capture-tool (capture-keeps-tool-span-content-p capture))
             (parent *trace-context*)
             (trace-id (or (getf parent :trace-id) (generate-trace-id)))
             (parent-span-id (getf parent :span-id))
             (span-id (generate-span-id))
             (provider (or (tool-rec-model-provider rec) ""))
             (model (or (tool-rec-model-name rec) ""))
             (attrs (common-span-attrs config
                      :provider provider :model model
                      :agent-name (tool-rec-agent-name rec)
                      :agent-version (tool-rec-agent-version rec)
                      :conversation-id (tool-rec-conversation-id rec))))
        (push (otel-string-attr "gen_ai.operation.name" "execute_tool") attrs)
        (push (otel-string-attr "gen_ai.tool.name"
                                 (or (tool-rec-tool-name rec) "")) attrs)
        (let ((tcid (tool-rec-tool-call-id rec)))
          (when (and tcid (plusp (length tcid)))
            (push (otel-string-attr "gen_ai.tool.call.id" tcid) attrs)))
        (let ((tt (tool-rec-tool-type rec)))
          (when (and tt (stringp tt) (plusp (length tt)))
            (push (otel-string-attr "gen_ai.tool.type" tt) attrs)))
        (let ((td (tool-rec-tool-description rec)))
          (when (and td (stringp td) (plusp (length td)))
            (push (otel-string-attr "gen_ai.tool.description" td) attrs)))
        (let ((args (tool-rec-arguments rec)))
          (when (and args (stringp args) (plusp (length args)))
            (push (otel-string-attr "gen_ai.tool.call.arguments"
                                     (if capture-tool (truncate-for-span args) "<redacted>"))
                  attrs)
            (push (otel-int-attr "gen_ai.tool.call.arguments.length" (length args)) attrs)))
        (let ((res (tool-rec-result rec)))
          (when (and res (stringp res) (plusp (length res)))
            (push (otel-string-attr "gen_ai.tool.call.result"
                                     (if capture-tool (truncate-for-span res) "<redacted>"))
                  attrs)
            (push (otel-int-attr "gen_ai.tool.call.result.length" (length res)) attrs)))
        (let ((err (or (tool-rec-error-message rec) (recorder-call-error rec))))
          (when err
            (push (otel-string-attr "error.type" "tool_execution_error") attrs)))
        (let ((start-nano (iso8601-to-unix-nano (recorder-started-at rec))))
          (queue-enqueue
           (client-trace-queue (recorder-client rec))
           (build-span :trace-id trace-id
                       :span-id span-id
                       :parent-span-id parent-span-id
                       :name (format nil "execute_tool ~a"
                                     (or (tool-rec-tool-name rec) "unknown"))
                       :kind 1
                       :start-time-unix-nano start-nano
                       :end-time-unix-nano
                       (if (and start-nano (tool-rec-duration-seconds rec))
                           (unix-nano-plus-seconds start-nano (tool-rec-duration-seconds rec))
                           (or (iso8601-to-unix-nano (recorder-completed-at rec)) "0"))
                       :attributes (coerce (nreverse attrs) 'vector)
                       :status-code (if (or (tool-rec-error-message rec)
                                            (recorder-call-error rec)) 2 1)
                       :status-message (let ((err (or (tool-rec-error-message rec)
                                                      (recorder-call-error rec))))
                                         (if err
                                             (if capture-tool err "<redacted>")
                                             "")))))))))

;;; ================================================================
;;; Embedding recorder
;;; ================================================================

(defclass embedding-recorder (recorder)
  ((model-provider   :initarg :model-provider   :accessor emb-rec-model-provider   :initform nil)
   (model-name       :initarg :model-name       :accessor emb-rec-model-name       :initform nil)
   (agent-name       :initarg :agent-name       :accessor emb-rec-agent-name       :initform nil)
   (agent-version    :initarg :agent-version    :accessor emb-rec-agent-version    :initform nil)
   (source           :initarg :source           :accessor emb-rec-source           :initform nil)
   ;; Set via set-result
   (input-count      :initarg :input-count      :accessor emb-rec-input-count      :initform nil)
   (input-tokens     :initarg :input-tokens     :accessor emb-rec-input-tokens     :initform nil)
   (dimensions       :initarg :dimensions       :accessor emb-rec-dimensions       :initform nil)
   (duration-seconds :initarg :duration-seconds :accessor emb-rec-duration-seconds :initform nil)))

(defmethod recorder-type-key ((rec embedding-recorder)) :embedding)

(defmethod set-result ((rec embedding-recorder) &key input-count input-tokens
                                                      dimensions duration-seconds)
  (when input-count      (setf (emb-rec-input-count rec) input-count))
  (when input-tokens     (setf (emb-rec-input-tokens rec) input-tokens))
  (when dimensions       (setf (emb-rec-dimensions rec) dimensions))
  (when duration-seconds (setf (emb-rec-duration-seconds rec) duration-seconds))
  rec)

(defmethod recorder-end ((rec embedding-recorder))
  (let ((config (client-config (recorder-client rec))))
    (when (config-traces-enabled config)
      (let* ((capture (config-content-capture-mode config))
             (capture-content (capture-keeps-content-p capture))
             (parent *trace-context*)
             (trace-id (or (getf parent :trace-id) (generate-trace-id)))
             (parent-span-id (getf parent :span-id))
             (span-id (generate-span-id))
             (provider (or (emb-rec-model-provider rec) ""))
             (model (or (emb-rec-model-name rec) ""))
             (attrs (common-span-attrs config :provider provider :model model
                                               :agent-name (emb-rec-agent-name rec)
                                               :agent-version (emb-rec-agent-version rec))))
        (push (otel-string-attr "gen_ai.operation.name" "embeddings") attrs)
        (when (plusp (length model))
          (push (otel-string-attr "gen_ai.request.model" model) attrs))
        (when (and (emb-rec-input-count rec) (plusp (emb-rec-input-count rec)))
          (push (otel-int-attr "gen_ai.embeddings.input_count"
                                (emb-rec-input-count rec)) attrs))
        (when (and (emb-rec-input-tokens rec) (plusp (emb-rec-input-tokens rec)))
          (push (otel-int-attr "gen_ai.usage.input_tokens"
                                (emb-rec-input-tokens rec)) attrs))
        (let ((src (emb-rec-source rec)))
          (when (and src (stringp src) (plusp (length src)))
            (push (otel-string-attr "sigil.embeddings.source" src) attrs)))
        (when (recorder-call-error rec)
          (push (otel-string-attr "error.type" "provider_call_error") attrs))
        (let ((start-nano (iso8601-to-unix-nano (recorder-started-at rec))))
          (queue-enqueue
           (client-trace-queue (recorder-client rec))
           (build-span :trace-id trace-id
                       :span-id span-id
                       :parent-span-id parent-span-id
                       :name (format nil "embeddings ~a" model)
                       :kind 3
                       :start-time-unix-nano start-nano
                       :end-time-unix-nano
                       (if (and start-nano (emb-rec-duration-seconds rec))
                           (unix-nano-plus-seconds start-nano (emb-rec-duration-seconds rec))
                           (or (iso8601-to-unix-nano (recorder-completed-at rec)) "0"))
                       :attributes (coerce (nreverse attrs) 'vector)
                       :status-code (if (recorder-call-error rec) 2 1)
                       :status-message (if (recorder-call-error rec)
                                           (if capture-content (recorder-call-error rec)
                                               "<redacted>")
                                           ""))))))))

;;; ================================================================
;;; Workflow step recorder
;;; ================================================================

(defclass workflow-step-recorder (recorder)
  ((step-id              :initarg :step-id              :accessor wfs-rec-step-id              :initform nil)
   (conversation-id      :initarg :conversation-id      :accessor wfs-rec-conversation-id      :initform nil)
   (step-name            :initarg :step-name            :accessor wfs-rec-step-name            :initform nil)
   (framework            :initarg :framework            :accessor wfs-rec-framework            :initform nil)
   (agent-name           :initarg :agent-name           :accessor wfs-rec-agent-name           :initform nil)
   (agent-version        :initarg :agent-version        :accessor wfs-rec-agent-version        :initform nil)
   (trace-id             :initarg :trace-id             :accessor wfs-rec-trace-id             :initform nil)
   (span-id              :initarg :span-id              :accessor wfs-rec-span-id              :initform nil)
   (input-state          :initarg :input-state          :accessor wfs-rec-input-state          :initform nil)
   (output-state         :initarg :output-state         :accessor wfs-rec-output-state         :initform nil)
   (error-message        :initarg :error-message        :accessor wfs-rec-error-message        :initform nil)
   (tags                 :initarg :tags                 :accessor wfs-rec-tags                 :initform nil)
   (metadata             :initarg :metadata             :accessor wfs-rec-metadata             :initform nil)
   (linked-generation-ids :initarg :linked-generation-ids :accessor wfs-rec-linked-generation-ids :initform nil)
   (parent-step-ids      :initarg :parent-step-ids      :accessor wfs-rec-parent-step-ids      :initform nil)
   (duration-seconds     :initarg :duration-seconds     :accessor wfs-rec-duration-seconds     :initform nil)))

(defmethod recorder-type-key ((rec workflow-step-recorder)) :workflow-step)

(defmethod set-result ((rec workflow-step-recorder) &key input-state output-state
                                                          error-message
                                                          linked-generation-ids
                                                          parent-step-ids
                                                          tags metadata
                                                          duration-seconds)
  (when input-state          (setf (wfs-rec-input-state rec) input-state))
  (when output-state         (setf (wfs-rec-output-state rec) output-state))
  (when error-message        (setf (wfs-rec-error-message rec) error-message))
  (when linked-generation-ids
    (setf (wfs-rec-linked-generation-ids rec) linked-generation-ids))
  (when parent-step-ids      (setf (wfs-rec-parent-step-ids rec) parent-step-ids))
  (when tags                 (setf (wfs-rec-tags rec) tags))
  (when metadata             (setf (wfs-rec-metadata rec) metadata))
  (when duration-seconds     (setf (wfs-rec-duration-seconds rec) duration-seconds))
  rec)

(defun build-workflow-step-payload (rec config)
  "Build a workflow-step JSON hash-table from the recorder state."
  (let ((step (jobj "id" (wfs-rec-step-id rec)
                    "conversation_id" (or (wfs-rec-conversation-id rec) "")
                    "step_name" (or (wfs-rec-step-name rec) "")
                    "started_at" (recorder-started-at rec)
                    "completed_at" (or (recorder-completed-at rec) (iso8601-now))
                    "trace_id" (or (wfs-rec-trace-id rec) "")
                    "span_id" (or (wfs-rec-span-id rec) ""))))
    (when (wfs-rec-framework rec)
      (setf (gethash "framework" step) (wfs-rec-framework rec)))
    (when (wfs-rec-agent-name rec)
      (setf (gethash "agent_name" step) (wfs-rec-agent-name rec)))
    (when (wfs-rec-agent-version rec)
      (setf (gethash "agent_version" step) (wfs-rec-agent-version rec)))
    (when (wfs-rec-input-state rec)
      (setf (gethash "input_state" step) (wfs-rec-input-state rec)))
    (when (wfs-rec-output-state rec)
      (setf (gethash "output_state" step) (wfs-rec-output-state rec)))
    (when (wfs-rec-metadata rec)
      (setf (gethash "metadata" step) (wfs-rec-metadata rec)))
    (let ((err (or (wfs-rec-error-message rec) (recorder-call-error rec))))
      (when err
        (setf (gethash "error" step) err)))
    (when (wfs-rec-linked-generation-ids rec)
      (setf (gethash "linked_generation_ids" step)
            (coerce (wfs-rec-linked-generation-ids rec) 'vector)))
    (when (wfs-rec-parent-step-ids rec)
      (setf (gethash "parent_step_ids" step)
            (coerce (wfs-rec-parent-step-ids rec) 'vector)))
    ;; Tags: merge config + recorder, output as map<string,string>
    (let ((all-tags (append (config-tags config) (wfs-rec-tags rec))))
      (when all-tags
        (let ((tags-obj (jobj)))
          (dolist (pair all-tags)
            (when (and (consp pair) (stringp (car pair)) (stringp (cdr pair)))
              (setf (gethash (car pair) tags-obj) (cdr pair))))
          (when (plusp (hash-table-count tags-obj))
            (setf (gethash "tags" step) tags-obj)))))
    step))

(defun build-workflow-step-span (rec config)
  "Build an OTel span for a workflow step. Returns span hash-table."
  (let* ((trace-id (wfs-rec-trace-id rec))
         (span-id (wfs-rec-span-id rec))
         (step-name (or (wfs-rec-step-name rec) "unknown"))
         (err (or (wfs-rec-error-message rec) (recorder-call-error rec)))
         (capture (config-content-capture-mode config))
         (capture-content (capture-keeps-content-p capture))
         (attrs (common-span-attrs config
                  :provider ""
                  :model ""
                  :agent-name (wfs-rec-agent-name rec)
                  :agent-version (wfs-rec-agent-version rec)
                  :conversation-id (wfs-rec-conversation-id rec))))
    (push (otel-string-attr "gen_ai.operation.name" "workflow_step") attrs)
    (push (otel-string-attr "sigil.workflow.step.id" (or (wfs-rec-step-id rec) "")) attrs)
    (push (otel-string-attr "sigil.workflow.step.name" step-name) attrs)
    (let ((fw (wfs-rec-framework rec)))
      (when (and fw (stringp fw) (plusp (length fw)))
        (push (otel-string-attr "sigil.workflow.framework" fw) attrs)))
    (let ((parents (wfs-rec-parent-step-ids rec)))
      (when parents
        (push (otel-string-array-attr "sigil.workflow.parent_step_ids" parents) attrs)))
    (let ((linked (wfs-rec-linked-generation-ids rec)))
      (when linked
        (push (otel-string-array-attr "sigil.workflow.linked_generation_ids" linked) attrs)))
    (when err
      (push (otel-string-attr "error.type" "workflow_step_error") attrs))
    (let* ((start-nano (iso8601-to-unix-nano (recorder-started-at rec)))
           (end-nano (if (and start-nano (wfs-rec-duration-seconds rec))
                         (unix-nano-plus-seconds start-nano (wfs-rec-duration-seconds rec))
                         (or (iso8601-to-unix-nano (recorder-completed-at rec)) "0"))))
      (build-span :trace-id trace-id
                  :span-id span-id
                  :name (format nil "workflow_step ~a" step-name)
                  :kind 1
                  :start-time-unix-nano start-nano
                  :end-time-unix-nano end-nano
                  :attributes (coerce (nreverse attrs) 'vector)
                  :status-code (if err 2 1)
                  :status-message (if err
                                      (if capture-content err "<redacted>")
                                      "")))))

(defmethod recorder-end ((rec workflow-step-recorder))
  (let ((config (client-config (recorder-client rec))))
    (let ((payload nil)
          (span nil))
      (when (config-workflow-steps-enabled config)
        (setf payload (build-workflow-step-payload rec config)))
      (when (config-traces-enabled config)
        (setf span (build-workflow-step-span rec config)))
      (when payload
        (queue-enqueue (client-workflow-queue (recorder-client rec)) payload))
      (when span
        (queue-enqueue (client-trace-queue (recorder-client rec)) span)))))
