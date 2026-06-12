(in-package :sigil-cl)

(defclass sigil-config ()
  (;; Generation export
   (generation-endpoint :initarg :generation-endpoint :reader config-generation-endpoint :initform nil)
   (generation-enabled  :initarg :generation-enabled  :reader config-generation-enabled  :initform nil)
   ;; Eval control plane and score export
   (eval-endpoint :initarg :eval-endpoint :reader config-eval-endpoint :initform nil)
   (eval-path-prefix :initarg :eval-path-prefix :reader config-eval-path-prefix :initform "/api/v1")
   (scores-export-path :initarg :scores-export-path :reader config-scores-export-path
                       :initform "/api/v1/scores:export")
   (experiment-url-template :initarg :experiment-url-template
                            :reader config-experiment-url-template
                            :initform nil)
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
   ;; Callbacks
   (log-fn     :initarg :log-fn     :reader config-log-fn     :initform nil)
   (metrics-fn :initarg :metrics-fn :reader config-metrics-fn :initform nil)
   ;; Testing
   (http-fn :initarg :http-fn :reader config-http-fn :initform nil)))

(defun effective-trace-queue-max (config)
  "Return trace queue max, falling back to queue-max."
  (or (config-trace-queue-max config) (config-queue-max config)))

(defun make-config (&rest args)
  (apply #'make-instance 'sigil-config args))
