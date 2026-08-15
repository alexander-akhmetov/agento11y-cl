(in-package :agento11y-cl)

;;; --- Client ---

(defclass agento11y-client ()
  ((config           :initarg :config           :accessor client-config)
   (generation-queue :initarg :generation-queue :accessor client-generation-queue)
   (trace-queue      :initarg :trace-queue      :accessor client-trace-queue)
   (workflow-queue   :initarg :workflow-queue   :accessor client-workflow-queue)
   (metric-registry  :initarg :metric-registry  :accessor client-metric-registry)
   (worker-thread    :initform nil              :accessor client-worker-thread)
   (running-p        :initform nil              :accessor client-running-p)
   (lock             :initform (bt2:make-lock :name "agento11y-client")
                     :accessor client-lock)
   (wake-cv          :initform (bt2:make-condition-variable :name "agento11y-wake")
                     :accessor client-wake-cv)))

(defun make-client (config &key (env-fn #'uiop:getenv))
  "Create an Agent Observability client from CONFIG.
Layers AGENTO11Y_* env vars on top of the supplied config (caller > env > defaults)
so deployments can configure the SDK without code changes. ENV-FN is the
function used to read environment variables; defaults to uiop:getenv. Tests
can pass (constantly nil) to ignore the host environment."
  (let ((resolved (resolve-config-from-env config :env-fn env-fn)))
    (make-instance 'agento11y-client
      :config resolved
      :generation-queue (make-bounded-queue :max-size (config-queue-max resolved)
                                            :name "generation")
      :trace-queue (make-bounded-queue :max-size (effective-trace-queue-max resolved)
                                        :name "trace")
      :workflow-queue (make-bounded-queue :max-size (config-queue-max resolved)
                                           :name "workflow-step")
      :metric-registry (make-metric-registry))))

(defun noop-client ()
  "Create a client that discards everything (for testing/disabled mode)."
  (make-client (make-config)))

;;; --- Background flush loop ---

(defun run-flush-loop (client)
  "Background loop: drain and export batches from generation, trace, and workflow
queues. Metrics are cumulative, so they are pushed on a time gate (every
flush-interval-sec) rather than on every wake, to avoid re-pushing identical
series on each recorder end."
  (let ((config (client-config client))
        (last-metrics-export (get-internal-real-time)))
    (loop while (bt2:with-lock-held ((client-lock client))
                  (client-running-p client))
          do (handler-case
                 (let ((gen-batch (queue-drain-batch (client-generation-queue client)
                                                     (config-batch-size config)))
                       (trace-batch (queue-drain-all (client-trace-queue client)))
                       (wfs-batch (queue-drain-batch (client-workflow-queue client)
                                                     (config-batch-size config))))
                   (when (and gen-batch (config-generation-enabled config))
                     (export-generations config gen-batch (build-auth-headers config)))
                   (when (and trace-batch (config-traces-enabled config))
                     (export-traces config trace-batch (build-traces-auth-headers config)))
                   (when (and wfs-batch (config-workflow-steps-enabled config))
                     (export-workflow-steps config wfs-batch (build-auth-headers config)))
                   (when (and (config-metrics-enabled config)
                              (>= (/ (- (get-internal-real-time) last-metrics-export)
                                     internal-time-units-per-second)
                                  (config-flush-interval-sec config)))
                     (export-metrics config (client-metric-registry client)
                                     (build-metrics-auth-headers config))
                     (setf last-metrics-export (get-internal-real-time)))
                   (unless (or gen-batch trace-batch wfs-batch)
                     (bt2:with-lock-held ((client-lock client))
                       (bt2:condition-wait (client-wake-cv client) (client-lock client)
                                           :timeout (config-flush-interval-sec config)))))
               (error (e)
                 (agento11y-log config :warn "flush-loop"
                           (format nil "error: ~a" (princ-to-string e)))
                 (sleep 1))))))

;;; --- Lifecycle ---

(defun client-start (client)
  "Start the background export thread."
  (bt2:with-lock-held ((client-lock client))
    (when (client-running-p client)
      (return-from client-start client))
    (let ((config (client-config client)))
      (when (or (config-generation-enabled config)
                (config-traces-enabled config)
                (config-workflow-steps-enabled config)
                (config-metrics-enabled config))
        (setf (client-running-p client) t)
        (setf (client-worker-thread client)
              (bt2:make-thread (lambda () (run-flush-loop client))
                               :name "agento11y-flush"))
        (agento11y-log config :info "client" "started"))))
  client)

(defun client-shutdown (client &key (timeout-sec 5))
  "Flush remaining items and stop the background thread."
  (let ((was-running (bt2:with-lock-held ((client-lock client))
                       (prog1 (client-running-p client)
                         (setf (client-running-p client) nil)))))
    (when was-running
      (let ((config (client-config client)))
        (agento11y-log config :info "client" "shutting down")
        ;; Synchronous flush
        (client-flush client)
        ;; Wake the loop so it sees running-p=nil and exits
        (bt2:with-lock-held ((client-lock client))
          (bt2:condition-notify (client-wake-cv client)))
        ;; Wait for thread to finish
        (let ((thread (client-worker-thread client)))
          (when (and thread (bt2:thread-alive-p thread))
            (let ((deadline (+ (get-internal-real-time)
                               (* timeout-sec internal-time-units-per-second))))
              (loop while (and (bt2:thread-alive-p thread)
                               (< (get-internal-real-time) deadline))
                    do (sleep 0.1))
              (when (bt2:thread-alive-p thread)
                (handler-case (bt2:destroy-thread thread)
                  (error (e)
                    (agento11y-log config :warn "client"
                              (format nil "failed to stop worker: ~a" (princ-to-string e)))))))))
        (setf (client-worker-thread client) nil)
        (agento11y-log config :info "client" "stopped"))))
  t)

(defun client-flush (client)
  "Synchronously flush all pending items from both queues."
  (let ((config (client-config client)))
    (when (config-generation-enabled config)
      (loop for batch = (queue-drain-batch (client-generation-queue client)
                                           (config-batch-size config))
            while batch
            do (handler-case
                   (export-generations config batch (build-auth-headers config))
                 (error (e)
                   (agento11y-log config :warn "flush"
                             (format nil "generation batch export failed: ~a"
                                     (princ-to-string e)))))))
    (when (config-traces-enabled config)
      (let ((spans (queue-drain-all (client-trace-queue client))))
        (when spans
          (handler-case
              (export-traces config spans (build-traces-auth-headers config))
            (error (e)
              (agento11y-log config :warn "flush"
                        (format nil "trace export failed: ~a"
                                (princ-to-string e))))))))
    (when (config-workflow-steps-enabled config)
      (loop for batch = (queue-drain-batch (client-workflow-queue client)
                                            (config-batch-size config))
            while batch
            do (handler-case
                   (export-workflow-steps config batch (build-auth-headers config))
                 (error (e)
                   (agento11y-log config :warn "flush"
                             (format nil "workflow-step batch export failed: ~a"
                                     (princ-to-string e)))))))
    (when (config-metrics-enabled config)
      (handler-case
          (export-metrics config (client-metric-registry client)
                          (build-metrics-auth-headers config))
        (error (e)
          (agento11y-log config :warn "flush"
                    (format nil "metrics export failed: ~a"
                            (princ-to-string e)))))))
  nil)

;;; --- Recorder factories ---

(defun %resolve-agent-name (config caller-value)
  "Pick the per-recorder agent name. Precedence: caller > config-agent-name > config-service-name."
  (or caller-value
      (config-agent-name config)
      (config-service-name config)))

(defun %resolve-agent-version (config caller-value)
  "Pick the per-recorder agent version. Precedence: caller > config-agent-version > config-service-version."
  (or caller-value
      (config-agent-version config)
      (config-service-version config)))

(defvar *experiment-run* nil
  "Current experiment run. Bound by WITH-EXPERIMENT and read by START-GENERATION.
Thread-confined, like *TRACE-CONTEXT*. A spawned thread starts at the global
value NIL, so a generation recorded there is neither tagged with the run nor
tracked for score attribution. To keep a generation on another thread inside
the run, capture both specials on the parent with CAPTURE-TELEMETRY-CONTEXT
and replay them on the child, or wrap the child's thunk in
TELEMETRY-CONTEXT-THUNK.")

(declaim (ftype function %experiment-run-prepare-generation-options
                %experiment-run-register-recorder))

(defun start-generation (client &key (mode :sync) conversation-id conversation-title
                                      user-id agent-name agent-version
                                      model-provider model-name
                                      system-prompt input-messages tools
                                      temperature top-p max-tokens tool-choice
                                      (thinking-enabled :unset)
                                      parent-generation-ids
                                      tags metadata generation-id started-at
                                      content-capture)
  "Create and start a generation recorder.
When called inside a `with-workflow-step` (or any other context that binds
`*trace-context*`), the generation inherits the workflow's trace-id and uses
its span-id as parent so spans nest under the workflow span.

STARTED-AT overrides the wall clock. Pass it when the recorder is opened after
the call it describes has already returned: without it both timestamps are read
at record time, the backend derives a zero latency, and the exported span is
shifted forward by its own duration.

CONTENT-CAPTURE sets the capture mode for this generation only, ahead of the
capture-mode resolver and the client-level mode. A tool execution opened inside
`with-generation` inherits the mode resolved here."
  (let* ((config (client-config client))
         (run *experiment-run*)
         (ctx *trace-context*)
         (inherited-trace-id (getf ctx :trace-id))
         (inherited-parent-span-id (getf ctx :span-id)))
    (when run
      (let ((prepared (%experiment-run-prepare-generation-options
                       run client
                       :conversation-id conversation-id
                       :agent-name agent-name
                       :agent-version agent-version
                       :tags tags
                       :metadata metadata)))
        (setf conversation-id (getf prepared :conversation-id conversation-id)
              agent-name (getf prepared :agent-name agent-name)
              agent-version (getf prepared :agent-version agent-version)
              tags (getf prepared :tags tags)
              metadata (getf prepared :metadata metadata))))
    (let ((rec (make-instance 'generation-recorder
      :client client
      :started-at (or started-at (iso8601-now))
      :content-capture-mode (resolve-content-capture-mode
                             config :override content-capture :metadata metadata)
      :generation-id (or generation-id (generate-id))
      :trace-id (or inherited-trace-id (generate-trace-id))
      :span-id (generate-span-id)
      :parent-span-id inherited-parent-span-id
      :mode mode
      :conversation-id conversation-id
      :conversation-title conversation-title
      :user-id user-id
      :agent-name (%resolve-agent-name config agent-name)
      :agent-version (%resolve-agent-version config agent-version)
      :model-provider model-provider
      :model-name model-name
      :system-prompt system-prompt
      :input-messages input-messages
      :tools tools
      :temperature temperature
      :top-p top-p
      :max-tokens max-tokens
      :tool-choice tool-choice
      :thinking-enabled thinking-enabled
      :parent-generation-ids parent-generation-ids
      :tags tags
      :metadata metadata)))
      (when run
        (%experiment-run-register-recorder run rec))
      rec)))

(defun start-tool-execution (client &key tool-name tool-call-id tool-type tool-description
                                          conversation-id agent-name agent-version
                                          model-provider model-name started-at
                                          content-capture)
  "Create and start a tool execution recorder.
STARTED-AT overrides the wall clock; see START-GENERATION.

CONTENT-CAPTURE sets the capture mode for this tool execution only. Without it
the recorder takes the mode the enclosing generation resolved to, then the
capture-mode resolver, then the client-level mode."
  (let ((config (client-config client)))
    (make-instance 'tool-execution-recorder
      :client client
      :started-at (or started-at (iso8601-now))
      :content-capture-mode (resolve-content-capture-mode
                             config
                             :override content-capture
                             :parent-mode (getf *trace-context* :content-capture-mode))
      :tool-name tool-name
      :tool-call-id tool-call-id
      :tool-type tool-type
      :tool-description tool-description
      :conversation-id conversation-id
      :agent-name (%resolve-agent-name config agent-name)
      :agent-version (%resolve-agent-version config agent-version)
      :model-provider model-provider
      :model-name model-name)))

(defun start-embedding (client &key model-provider model-name
                                     agent-name agent-version source
                                     dimensions encoding-format started-at)
  "Create and start an embedding recorder.
DIMENSIONS is the requested dimension count; a result dimension count set later
via set-result takes precedence over it on the span.
STARTED-AT overrides the wall clock; see START-GENERATION.

An embedding has no per-call capture mode, matching the reference SDKs: its
mode comes from the capture-mode resolver, then the client-level mode."
  (let ((config (client-config client)))
    (make-instance 'embedding-recorder
      :client client
      :started-at (or started-at (iso8601-now))
      :content-capture-mode (resolve-content-capture-mode config)
      :model-provider model-provider
      :model-name model-name
      :agent-name (%resolve-agent-name config agent-name)
      :agent-version (%resolve-agent-version config agent-version)
      :source source
      :request-dimensions dimensions
      :encoding-format encoding-format)))

(defun start-workflow-step (client &key conversation-id step-name framework
                                         agent-name agent-version
                                         input-state output-state
                                         tags metadata
                                         linked-generation-ids parent-step-ids
                                         started-at)
  "Create and start a workflow step recorder.
Auto-generates step-id, trace-id, span-id, and started-at so callers can read
them immediately to build parent-step-id chains.
STARTED-AT overrides the wall clock; see START-GENERATION."
  (let ((config (client-config client)))
    (make-instance 'workflow-step-recorder
      :client client
      :started-at (or started-at (iso8601-now))
      :step-id (generate-workflow-step-id)
      :trace-id (generate-trace-id)
      :span-id (generate-span-id)
      :conversation-id conversation-id
      :step-name step-name
      :framework framework
      :agent-name (%resolve-agent-name config agent-name)
      :agent-version (%resolve-agent-version config agent-version)
      :input-state input-state
      :output-state output-state
      :tags tags
      :metadata metadata
      :linked-generation-ids linked-generation-ids
      :parent-step-ids parent-step-ids)))
