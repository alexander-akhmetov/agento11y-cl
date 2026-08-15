(in-package :agento11y-cl)

(defclass agento11y-config ()
  (;; Generation export
   (generation-endpoint :initarg :generation-endpoint :reader config-generation-endpoint :initform nil)
   (generation-enabled  :initarg :generation-enabled  :reader config-generation-enabled  :initform nil)
   ;; Eval control plane and score export.
   ;;
   ;; eval-path-prefix, scores-export-path, and ingest-actor hold NIL until a
   ;; caller sets them; the readers below apply the non-NIL defaults. NIL is
   ;; what env resolution treats as "unset", so an explicit caller value that
   ;; happens to equal the default still wins over the environment.
   (eval-endpoint :initarg :eval-endpoint :reader config-eval-endpoint :initform nil)
   (eval-path-prefix :initarg :eval-path-prefix :initform nil)
   (eval-auth-token :initarg :eval-auth-token :reader config-eval-auth-token :initform nil)
   (scores-export-path :initarg :scores-export-path :initform nil)
   ;; Identity claiming the runs and trials this SDK writes.
   (ingest-actor :initarg :ingest-actor :initform nil)
   (experiment-url-template :initarg :experiment-url-template
                            :reader config-experiment-url-template
                            :initform nil)
   ;; REST API root (host root used to derive /api/v1/...). Read by hook
   ;; evaluation and by conversation rating.
   ;; When nil, the host is derived from generation-endpoint.
   (api-endpoint        :initarg :api-endpoint        :reader config-api-endpoint        :initform nil)
   ;; Synchronous hook evaluation config (hooks-config struct or nil).
   (hooks-config        :initarg :hooks-config        :reader config-hooks-config        :initform nil)
   ;; Trace export
   (traces-endpoint     :initarg :traces-endpoint     :reader config-traces-endpoint     :initform nil)
   (traces-enabled      :initarg :traces-enabled      :reader config-traces-enabled      :initform nil)
   (traces-forward-auth :initarg :traces-forward-auth :reader config-traces-forward-auth :initform t)
   ;; Workflow step export
   (workflow-steps-endpoint :initarg :workflow-steps-endpoint :reader config-workflow-steps-endpoint :initform nil)
   (workflow-steps-enabled  :initarg :workflow-steps-enabled  :reader config-workflow-steps-enabled  :initform nil)
   ;; Metrics export (OTLP histograms)
   (metrics-endpoint     :initarg :metrics-endpoint     :reader config-metrics-endpoint     :initform nil)
   (metrics-enabled      :initarg :metrics-enabled      :reader config-metrics-enabled      :initform nil)
   (metrics-forward-auth :initarg :metrics-forward-auth :reader config-metrics-forward-auth :initform t)
   ;; Auth
   (auth-mode     :initarg :auth-mode     :reader config-auth-mode     :initform :none)
   (auth-user     :initarg :auth-user     :reader config-auth-user     :initform nil)
   (auth-password :initarg :auth-password :reader config-auth-password :initform nil)
   (tenant-id     :initarg :tenant-id     :reader config-tenant-id     :initform nil)
   ;; Batching
   (batch-size         :initarg :batch-size         :reader config-batch-size         :initform 20)
   (flush-interval-sec :initarg :flush-interval-sec :reader config-flush-interval-sec :initform 5)
   (queue-max          :initarg :queue-max          :reader config-queue-max          :initform 500)
   (trace-queue-max    :initarg :trace-queue-max    :reader config-trace-queue-max    :initform nil)
   ;; HTTP
   (export-timeout-sec  :initarg :export-timeout-sec  :reader config-export-timeout-sec  :initform 10)
   (max-retries         :initarg :max-retries         :reader config-max-retries         :initform 5)
   (initial-backoff-sec :initarg :initial-backoff-sec :reader config-initial-backoff-sec :initform 0.1)
   (max-backoff-sec     :initarg :max-backoff-sec     :reader config-max-backoff-sec     :initform 5.0)
   ;; Content capture
   (content-capture-mode :initarg :content-capture-mode :reader config-content-capture-mode
                         :initform :no-tool-content)
   ;; Called when a generation, tool execution, or embedding starts and no
   ;; closer setting has decided its capture mode. Takes the metadata supplied
   ;; at start (NIL where the recording type carries none) and returns a mode,
   ;; or NIL to defer to the client-level mode. Metadata a later set-result call
   ;; supplies is not seen: the mode is resolved once, at start, which is what
   ;; makes it inheritable. See %resolve-capture-mode-from-resolver.
   (content-capture-resolver :initarg :content-capture-resolver
                             :reader config-content-capture-resolver :initform nil)
   ;; Embedding input text capture. Opt-in, and additionally gated by
   ;; content-capture-mode: only :full and :no-tool-content keep the texts.
   ;; The two limits hold NIL until a caller sets them; the readers below apply
   ;; the defaults. Programmatic only, matching the Go, Python, and JavaScript
   ;; SDKs, which read no environment variable for these.
   (embedding-capture-input :initarg :embedding-capture-input
                            :reader config-embedding-capture-input :initform nil)
   (embedding-max-input-items :initarg :embedding-max-input-items :initform nil)
   (embedding-max-text-length :initarg :embedding-max-text-length :initform nil)
   ;; Secret redaction (scans surviving content for known secret formats)
   (redact-secrets         :initarg :redact-secrets         :reader config-redact-secrets         :initform nil)
   (redact-input-messages  :initarg :redact-input-messages  :reader config-redact-input-messages  :initform nil)
   (redact-email-addresses :initarg :redact-email-addresses :reader config-redact-email-addresses :initform t)
   ;; Service identity (OTel resource attributes)
   (service-name    :initarg :service-name    :reader config-service-name    :initform "unknown")
   (service-version :initarg :service-version :reader config-service-version :initform nil)
   ;; Agent identity (gen_ai.agent.* span attributes; falls back to service-* when unset)
   (agent-name    :initarg :agent-name    :reader config-agent-name    :initform nil)
   (agent-version :initarg :agent-version :reader config-agent-version :initform nil)
   ;; User identity
   (user-id :initarg :user-id :reader config-user-id :initform nil)
   (tags    :initarg :tags    :reader config-tags    :initform nil)
   ;; Extra HTTP headers merged into auth headers (case-insensitive de-dup; user wins)
   (extra-headers :initarg :extra-headers :reader config-extra-headers :initform nil)
   ;; Debug flag (surfaced via AGENTO11Y_DEBUG env var)
   (debug :initarg :debug :reader config-debug :initform nil)
   ;; Opt-in switch for features that are not stable yet. A slot rather than a
   ;; process-env read per call, so a caller and a test can both flip it without
   ;; touching the environment. Env resolution fills it from
   ;; AGENTO11Y_ENABLE_EXPERIMENTAL_FEATURES.
   (experimental-features :initarg :experimental-features
                          :reader config-experimental-features :initform nil)
   ;; Callbacks
   (log-fn     :initarg :log-fn     :reader config-log-fn     :initform nil)
   (metrics-fn :initarg :metrics-fn :reader config-metrics-fn :initform nil)
   ;; Testing
   (http-fn :initarg :http-fn :reader config-http-fn :initform nil)))

;;; --- Content capture mode vocabulary ---
;;;
;;; The mode decides which content-bearing fields leave the process. env.lisp,
;;; recorder.lisp, and rating.lisp all read it, so the vocabulary lives here
;;; beside the slot it describes rather than in any one of them.

(defparameter +content-capture-modes+
  '(:full :no-tool-content :full-with-metadata-spans :metadata-only)
  "The supported :content-capture-mode values.")

(defparameter +content-capture-mode-key+ "agento11y.sdk.content_capture_mode"
  "Key the resolved capture mode is stamped under, on both the generation tags
map and the generation metadata map. The sigil backend reads it from tags; the
reference SDKs write it to metadata.")

(defparameter +content-metadata-keys+
  '("agento11y.conversation.title" "sigil.conversation.title" "call_error")
  "Generation metadata keys whose values carry caller content.
The SDK mirrors the conversation title into the first key, so a redacting mode
has to remove it from caller-supplied metadata too: a caller can write any of
these keys, and the strip does not ask who wrote them. The second is the
pre-rename spelling, which an older exporter can still send. The third mirrors
call_error, which this SDK does not write but the reference SDK does.")

(defun valid-content-capture-mode-p (mode)
  "True when MODE is one of +content-capture-modes+."
  (and (member mode +content-capture-modes+) t))

(defun capture-keeps-payload-content-p (mode)
  "True when MODE keeps caller content in exported payloads: message text,
thinking text, tool call inputs, tool result content, media URLs, the
conversation title, a tool's description and input_schema_json, the system
prompt, workflow step input and output state, the rating comment, and
call_error text. A tool's name, type, and deferred flag are structure, not
content, and export in every mode.
:full, :no-tool-content, and :full-with-metadata-spans keep payload content.
Every other value redacts it, so an unsupported mode fails closed.
Span-borne content has its own gate, capture-keeps-span-content-p."
  (or (eq mode :full) (eq mode :no-tool-content) (eq mode :full-with-metadata-spans)))

(defun capture-redacts-payload-content-p (mode)
  "True when MODE withholds caller content from payloads. The inverse of
capture-keeps-payload-content-p, so an unsupported mode redacts."
  (not (capture-keeps-payload-content-p mode)))

(defun capture-keeps-span-content-p (mode)
  "True when MODE keeps caller content on OTel spans: embedding input texts, a
tool's description, and every span status message. Tool call arguments and
results are stricter still and have capture-keeps-tool-span-content-p.
:full and :no-tool-content keep span content. :full-with-metadata-spans keeps
payload content but strips the spans, which is the point of the mode: the
generation ingest destination is private while the traces destination is
shared. Every other value redacts, so an unsupported mode fails closed."
  (or (eq mode :full) (eq mode :no-tool-content)))

(defun capture-keeps-tool-span-content-p (mode)
  "True when MODE keeps tool execution span arguments and results.
Only :full keeps tool span content; :no-tool-content, :full-with-metadata-spans,
and :metadata-only redact it."
  (eq mode :full))

(defun content-capture-mode-string (mode)
  "Wire value for the agento11y.sdk.content_capture_mode generation tag.
:full maps to \"full\", :no-tool-content to \"no_tool_content\",
:full-with-metadata-spans to \"full_with_metadata_spans\", and everything else
to \"metadata_only\". An unsupported mode reports metadata_only, matching how
serialization treats it. metadata_only is the only stripped marker the backend
acts on."
  (cond
    ((eq mode :full) "full")
    ((eq mode :no-tool-content) "no_tool_content")
    ((eq mode :full-with-metadata-spans) "full_with_metadata_spans")
    (t "metadata_only")))

;;; --- Content capture mode resolution ---
;;;
;;; Precedence, from the shared docs/concepts/content-capture-modes.md:
;;;   generation      per-call > resolver > client
;;;   tool execution  per-call > parent generation's resolved mode > resolver > client
;;;   embedding       resolver > client
;;; A mode is resolved once, when the recorder starts, and held on the recorder,
;;; so a config change mid-call cannot move a recording between modes. A layer
;;; defers by holding NIL or :default; only the client-level mode is final.

(defun %capture-mode-choice (config mode source)
  "Return MODE when a layer chose one, or NIL when it deferred.
NIL and :default both defer. A mode outside the vocabulary is a caller mistake
that would strip every field without saying why, so it warns, names SOURCE, and
resolves to :metadata-only."
  (cond
    ((null mode) nil)
    ((eq mode :default) nil)
    ((valid-content-capture-mode-p mode) mode)
    (t (agento11y-log config :warn "config"
                      (format nil "ignoring ~a content capture mode ~s (unsupported value), using :metadata-only"
                              source mode))
       :metadata-only)))

(defun %resolve-capture-mode-from-resolver (config metadata)
  "Ask the configured resolver for a capture mode, or return NIL.
NIL means no resolver, or a resolver that deferred. A resolver that signals
resolves to :metadata-only: a resolver that cannot decide must not widen what
leaves the process."
  (let ((resolver (config-content-capture-resolver config)))
    (when resolver
      (handler-case
          (%capture-mode-choice config (funcall resolver metadata) "resolver")
        (error (e)
          (agento11y-log config :warn "config"
                         (format nil "content capture resolver failed: ~a, using :metadata-only"
                                 (princ-to-string e)))
          :metadata-only)))))

(defun resolve-content-capture-mode (config &key override parent-mode metadata)
  "Effective capture mode for one recording.
OVERRIDE is the per-call mode, PARENT-MODE the mode a parent generation already
resolved to, METADATA what the resolver is given. A layer that decides the mode
stops the chain, so a per-call mode leaves the resolver uncalled."
  (or (%capture-mode-choice config override "per-call")
      parent-mode
      (%resolve-capture-mode-from-resolver config metadata)
      (config-content-capture-mode config)))

(defparameter +default-eval-path-prefix+ "/api/v1")
(defparameter +default-scores-export-path+ "/api/v1/scores:export")
(defparameter +default-ingest-actor+ "ingest:sdk/lisp")
(defparameter +default-embedding-max-input-items+ 20)
(defparameter +default-embedding-max-text-length+ 1024)

(defmacro %define-defaulted-reader (name slot default)
  "Define reader NAME returning SLOT, or DEFAULT when SLOT is NIL, plus a
NAME-SUPPLIED-P predicate reporting whether a caller set it. NIL means unset,
so env resolution can still fill the slot and an explicit NIL cannot produce a
NIL path or actor."
  `(progn
     (defun ,name (config)
       (or (slot-value config ',slot) ,default))
     (defun ,(intern (concatenate 'string (symbol-name name) "-SUPPLIED-P")) (config)
       (and (slot-value config ',slot) t))))

(%define-defaulted-reader config-eval-path-prefix eval-path-prefix
                          +default-eval-path-prefix+)
(%define-defaulted-reader config-scores-export-path scores-export-path
                          +default-scores-export-path+)
(%define-defaulted-reader config-ingest-actor ingest-actor
                          +default-ingest-actor+)
(%define-defaulted-reader config-embedding-max-input-items embedding-max-input-items
                          +default-embedding-max-input-items+)
(%define-defaulted-reader config-embedding-max-text-length embedding-max-text-length
                          +default-embedding-max-text-length+)

;; Suffix, not a full name: RESOLVE-CONFIG-FROM-ENV reads it under both the
;; preferred AGENTO11Y_ prefix and the legacy SIGIL_ one.
(defparameter +experimental-features-env-suffix+ "ENABLE_EXPERIMENTAL_FEATURES")
(defparameter +experimental-features-env-var+
  (concatenate 'string "AGENTO11Y_" +experimental-features-env-suffix+)
  "Name quoted back to callers when an experimental call is blocked.")

(defun %require-experimental (config feature)
  "Signal unless the experimental gate is set. FEATURE names the blocked call.
An experimental feature can change or be removed in any release."
  (unless (config-experimental-features config)
    (error 'agento11y-experimental-disabled-error
           :message (format nil "~a is experimental; set ~a=true to use it"
                            feature +experimental-features-env-var+))))

(defun effective-trace-queue-max (config)
  "Return trace queue max, falling back to queue-max."
  (or (config-trace-queue-max config) (config-queue-max config)))

(defun make-config (&rest args)
  (apply #'make-instance 'agento11y-config args))
