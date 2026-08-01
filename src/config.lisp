(in-package :sigil-cl)

(defclass sigil-config ()
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
   ;; Hook API root (host root used to derive /api/v1/...).
   ;; When nil, the hook endpoint is derived from generation-endpoint.
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
                         :initform :metadata-only)
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
   ;; Debug flag (surfaced via SIGIL_DEBUG env var)
   (debug :initarg :debug :reader config-debug :initform nil)
   ;; Opt-in switch for features that are not stable yet. A slot rather than a
   ;; process-env read per call, so a caller and a test can both flip it without
   ;; touching the environment. Env resolution fills it from
   ;; SIGIL_ENABLE_EXPERIMENTAL_FEATURES.
   (experimental-features :initarg :experimental-features
                          :reader config-experimental-features :initform nil)
   ;; Callbacks
   (log-fn     :initarg :log-fn     :reader config-log-fn     :initform nil)
   (metrics-fn :initarg :metrics-fn :reader config-metrics-fn :initform nil)
   ;; Testing
   (http-fn :initarg :http-fn :reader config-http-fn :initform nil)))

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

(defparameter +experimental-features-env-var+ "SIGIL_ENABLE_EXPERIMENTAL_FEATURES")

(defun %require-experimental (config feature)
  "Signal unless the experimental gate is set. FEATURE names the blocked call.
An experimental feature can change or be removed in any release."
  (unless (config-experimental-features config)
    (error 'sigil-experimental-disabled-error
           :message (format nil "~a is experimental; set ~a=true to use it"
                            feature +experimental-features-env-var+))))

(defun effective-trace-queue-max (config)
  "Return trace queue max, falling back to queue-max."
  (or (config-trace-queue-max config) (config-queue-max config)))

(defun make-config (&rest args)
  (apply #'make-instance 'sigil-config args))
