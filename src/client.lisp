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
   (stopping-p       :initform nil              :accessor client-stopping-p)
   (lock             :initform (bt2:make-lock :name "agento11y-client")
                     :accessor client-lock)
   (export-lock      :initform (bt2:make-lock :name "agento11y-export")
                     :reader client-export-lock)
   (export-failures   :initform nil :accessor client-export-failures)
   (wake-cv          :initform (bt2:make-condition-variable :name "agento11y-wake")
                     :accessor client-wake-cv)))

(defun make-client (config &key (env-fn #'uiop:getenv))
  "Create an Agent Observability client from CONFIG.
Layers AGENTO11Y_* env vars on top of the supplied config (caller > env > defaults)
so deployments can configure the SDK without code changes. ENV-FN is the
function used to read environment variables; defaults to uiop:getenv. Tests
can pass (constantly nil) to ignore the host environment."
  (let ((resolved (resolve-config-from-env config :env-fn env-fn)))
    (unless (or (config-option-supplied-p resolved :service-instance-id)
                (assoc "service.instance.id" (config-resource-attributes resolved) :test #'equal))
      (setf (slot-value resolved 'service-instance-id) (generate-trace-id)))
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
  (make-client (make-config) :env-fn (constantly nil)))

;;; --- Background flush loop ---

(defun enqueue-generation-span (client span)
  "Wait for generation capacity, then signal AGENTO11Y-EXPORT-ERROR on timeout."
  (let ((config (client-config client)))
    (unless (and (otel-generation-export-ready-p config)
                 (or (config-generation-enabled config)
                     (not (config-option-supplied-p config :generation-enabled))))
      (return-from enqueue-generation-span nil))
    (bt2:with-lock-held ((client-lock client))
      (bt2:condition-notify (client-wake-cv client)))
    (unless (queue-enqueue-wait (client-generation-queue client) span
                                (config-generation-enqueue-timeout-sec config))
      (error 'agento11y-export-error :message "Generation queue capacity wait timed out"
             :diagnostics (list* :kind :queue-capacity-timeout
                                 (client-export-diagnostics client))))
    (bt2:with-lock-held ((client-lock client))
      (bt2:condition-notify (client-wake-cv client)))
    t))

(defun client-export-diagnostics (client)
  "Return lifetime-sticky failure summaries and queue-loss counters."
  (list :failures (bt2:with-lock-held ((client-lock client))
                    (copy-tree (client-export-failures client)))
        :queue-loss (remove nil (mapcar #'queue-loss-diagnostics
                                       (list (client-generation-queue client)
                                             (client-trace-queue client)
                                             (client-workflow-queue client))))))

(defun %record-export-failure (client diagnostic)
  (bt2:with-lock-held ((client-lock client))
    (let ((existing (find diagnostic (client-export-failures client)
                          :test #'equal :key (lambda (entry) (getf entry :diagnostic)))))
      (if existing
          (incf (getf existing :occurrences))
          (push (list :diagnostic (copy-tree diagnostic) :occurrences 1)
                (client-export-failures client))))))

(defun %client-export (client channel count thunk)
  (handler-case
      (multiple-value-bind (ok body diagnostic) (funcall thunk)
        (declare (ignore body))
        (unless ok
          (%record-export-failure
           client (append (list :channel channel)
                          (or diagnostic (list :kind :export-failed :count count))))))
    (error ()
      (%record-export-failure client (list :kind :export-error :channel channel :count count))
      (agento11y-log (client-config client) :warn "exporter" "Batch export raised an error"))))

(defun %check-export-deadline ()
  (when (and *export-deadline* (zerop (deadline-remaining *export-deadline*)))
    (error 'agento11y-export-error :message "Export deadline expired"
           :diagnostics (list :timeout t))))

(defun %export-queue-batch (client queue channel exporter headers)
  (%check-export-deadline)
  (let ((batch (queue-drain-batch queue (max 1 (config-batch-size (client-config client))))))
    (when batch
      (%client-export client channel (length batch)
                      (lambda () (funcall exporter (client-config client) batch
                                          (funcall headers (client-config client)))))
      t)))

(defun %export-client-batches (client &key generations-only)
  "Export one batch per active queue. Caller holds the export lock."
  (let* ((config (client-config client))
         (generation
           (cond ((otel-generation-export-ready-p config)
                  (%export-queue-batch client (client-generation-queue client) :generation
                                       #'export-traces #'build-traces-auth-headers))
                 ((generation-payload-export-p config)
                  (%export-queue-batch client (client-generation-queue client) :generation
                                       #'export-generations #'build-auth-headers))))
         (trace (when (and (not generations-only) (spans-export-active-p config))
                  (%export-queue-batch client (client-trace-queue client) :trace
                                       #'export-traces #'build-traces-auth-headers)))
         (workflow (when (and (not generations-only) (config-workflow-steps-enabled config))
                     (%export-queue-batch client (client-workflow-queue client) :workflow
                                          #'export-workflow-steps #'build-auth-headers))))
    (or generation trace workflow)))

(defun %export-client-metrics (client)
  (when (config-metrics-enabled (client-config client))
    (%check-export-deadline)
    (%client-export client :metrics 0
                    (lambda () (export-metrics (client-config client)
                                               (client-metric-registry client)
                                               (build-metrics-auth-headers (client-config client)))))))

(defun %client-queues-pending-p (client)
  (let ((config (client-config client)))
    (or (and (or (generation-payload-export-p config)
                 (otel-generation-export-ready-p config))
             (not (queue-empty-p (client-generation-queue client))))
        (and (spans-export-active-p config)
             (not (queue-empty-p (client-trace-queue client))))
        (and (config-workflow-steps-enabled config)
             (not (queue-empty-p (client-workflow-queue client)))))))

(defun run-flush-loop (client)
  "Serialize queue drains and HTTP exports with explicit flushes."
  (let ((config (client-config client))
        (last-metrics-export (get-internal-real-time)))
    (loop while (bt2:with-lock-held ((client-lock client)) (client-running-p client))
          do (handler-case
                 (let ((worked nil))
                   (bt2:with-lock-held ((client-export-lock client))
                     (when (bt2:with-lock-held ((client-lock client)) (client-running-p client))
                       (setf worked (%export-client-batches client))
                       (when (>= (/ (- (get-internal-real-time) last-metrics-export)
                                    internal-time-units-per-second)
                                 (config-flush-interval-sec config))
                         (%export-client-metrics client)
                         (setf last-metrics-export (get-internal-real-time)))))
                   (unless worked
                     (bt2:with-lock-held ((client-lock client))
                       (when (and (client-running-p client)
                                  (not (%client-queues-pending-p client)))
                         (bt2:condition-wait (client-wake-cv client) (client-lock client)
                                             :timeout (config-flush-interval-sec config))))))
               (error ()
                 (%record-export-failure client (list :kind :worker-error))
                 (agento11y-log config :warn "flush-loop" "Background export failed")
                 (sleep (min 0.1 (config-flush-interval-sec config))))))))

;;; --- Lifecycle ---

(defun %warn-otel-config (config)
  "Report, once at start, the two otel-mode settings that do not do what their
name says.

:full-with-metadata-spans exists to keep content off the shared traces
destination while the private generation ingest keeps it. In otel mode there is
no second destination: the span is the generation. The recorder resolves the
mode to :metadata-only rather than widening it, and a silent narrowing is hard
to notice on a wire that only says metadata_only.

A missing traces endpoint leaves the mode with nowhere to export to, so it
exports nothing. This says so at start; the recorder says it again per
generation."
  (when (otel-generation-protocol-p config)
    (when (eq (config-content-capture-mode config) :full-with-metadata-spans)
      (agento11y-log config :warn "config"
                     "generation-protocol :otel exports the generation as a span, so :full-with-metadata-spans has no private destination left and resolves to :metadata-only"))
    (when (and (config-experimental-features config)
               (null (config-traces-endpoint config)))
      (agento11y-log config :warn "config"
                     "generation-protocol :otel needs traces-endpoint (AGENTO11Y_TRACES_ENDPOINT); nothing is exported without it"))))

(defun client-start (client)
  "Start the background export thread."
  (bt2:with-lock-held ((client-lock client))
    (when (client-running-p client)
      (return-from client-start client))
    (when (or (client-stopping-p client)
              (and (client-worker-thread client)
                   (bt2:thread-alive-p (client-worker-thread client))))
      (error 'agento11y-export-error :message "Previous export worker has not stopped"))
    (let ((config (client-config client)))
      (%warn-otel-config config)
      (when (or (config-generation-enabled config)
                (spans-export-active-p config)
                (config-workflow-steps-enabled config)
                (config-metrics-enabled config))
        (setf (client-running-p client) t)
        (setf (client-worker-thread client)
              (bt2:make-thread (lambda () (run-flush-loop client))
                               :name "agento11y-flush"))
        (agento11y-log config :info "client" "started"))))
  client)

(defun %report-client-export-result (client &key timeout worker-running generations-only)
  (let ((diagnostics (client-export-diagnostics client)))
    (when generations-only
      (setf (getf diagnostics :failures)
            (remove-if-not
             (lambda (entry)
               (let ((failure (getf entry :diagnostic)))
                 (or (eq (getf failure :channel) :generation)
                     (and (eq (getf failure :kind) :serialization-error)
                          (eq (getf failure :recorder-type) :generation)))))
             (getf diagnostics :failures))
            (getf diagnostics :queue-loss)
            (remove-if-not (lambda (entry) (equal (getf entry :queue) "generation"))
                           (getf diagnostics :queue-loss))))
    (when timeout (setf (getf diagnostics :timeout) t))
    (when worker-running (setf (getf diagnostics :worker-running) t))
    (when (or timeout worker-running (getf diagnostics :failures)
              (getf diagnostics :queue-loss))
      (error 'agento11y-export-error
             :message (if timeout "Export deadline expired" "Telemetry export failed or queue loss occurred")
             :diagnostics diagnostics))))

(defun client-shutdown (client &key (timeout-sec 5))
  "Stop the worker and drain even an unstarted client. Never interrupt HTTP.
A timed-out worker remains attached until it exits. Custom HTTP functions must
implement their own request timeouts; they cannot be safely interrupted here."
  (let ((deadline (monotonic-deadline timeout-sec))
        (timed-out nil)
        (thread nil))
    (bt2:with-lock-held ((client-lock client))
      (setf (client-running-p client) nil
            (client-stopping-p client) t
            thread (client-worker-thread client))
      (bt2:condition-notify (client-wake-cv client)))
    (unwind-protect
         (progn
           (handler-case
               (client-flush client :timeout-sec (deadline-remaining deadline))
             (agento11y-export-error (e)
               (setf timed-out (getf (agento11y-export-error-diagnostics e) :timeout))))
           (loop while (and thread (bt2:thread-alive-p thread))
                 for remaining = (deadline-remaining deadline)
                 do (when (and remaining (zerop remaining))
                      (setf timed-out t)
                      (return))
                    (sleep (min 0.01 (or remaining 0.01))))
           (%report-client-export-result client :timeout timed-out
                                          :worker-running (and thread (bt2:thread-alive-p thread))))
      (bt2:with-lock-held ((client-lock client))
        (setf (client-stopping-p client) nil)
        (when (and (eq thread (client-worker-thread client))
                   (or (null thread) (not (bt2:thread-alive-p thread))))
          (setf (client-worker-thread client) nil)))))
  t)

(defun client-flush (client &key timeout-sec generations-only)
  "Wait for in-flight exports, drain active queues, and report sticky failures.
GENERATIONS-ONLY excludes optional traces/metrics and their past failures.
TIMEOUT-SEC covers lock waiting, requests, and retry delays. NIL means success."
  (let* ((deadline (monotonic-deadline timeout-sec))
         (*export-deadline* deadline)
         (timed-out nil))
    (unless (bt2:acquire-lock (client-export-lock client)
                              :timeout (deadline-remaining deadline))
      (%report-client-export-result client :timeout t))
    (unwind-protect
         (handler-case
             (progn
               (loop while (%export-client-batches client :generations-only generations-only))
               (unless generations-only (%export-client-metrics client))
               (%check-export-deadline))
           (agento11y-export-error () (setf timed-out t)))
      (bt2:release-lock (client-export-lock client)))
    (%report-client-export-result client :timeout timed-out :generations-only generations-only))
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
                                      content-capture trace-id span-id)
  "Create and start a generation recorder.
When called inside a `with-workflow-step` (or any other context that binds
`*trace-context*`), the generation inherits the workflow's trace-id and uses
its span-id as parent so spans nest under the workflow span.

STARTED-AT overrides the wall clock. Pass it when the recorder is opened after
the call it describes has already returned: without it both timestamps are read
at record time, the backend derives a zero latency, and the exported span is
shifted forward by its own duration.

CONTENT-CAPTURE sets the capture mode for this generation only, ahead of the
enclosing span's resolved mode, the resolver and the client-level mode.
Children opened inside `with-generation` inherit the mode resolved here.

TRACE-ID and SPAN-ID name the span this generation will export, for a caller
that has to know the identifiers BEFORE the recorder opens. A recorder opened
after the provider returned is the case that needs them: at request time the
generation span does not exist, so a caller propagating W3C context to the
provider can only name the enclosing span, and the provider's server span comes
back a sibling of this one rather than its child. Minting the pair up front and
passing it here is what nests it.

Both are optional and independent, each falling back to the inherited or
generated value. PARENT-SPAN-ID is never overridden: the pair names this span,
not its place in the tree, which stays whatever *TRACE-CONTEXT* says. A value
that is zero, nonhex, or the wrong width is ignored. Accepted IDs are normalized
to lowercase. Invalid trace overrides fall back to the ambient trace, then a
generated trace; invalid span overrides get a generated span. Ambient IDs are
also validated because malformed identifiers can invalidate an entire OTLP batch."
  (let* ((config (client-config client))
         (run *experiment-run*)
         (ctx *trace-context*)
         (inherited-trace-id (normalized-trace-id (getf ctx :trace-id) 32))
         (inherited-parent-span-id (normalized-trace-id (getf ctx :span-id) 16))
         (caller-trace-id (normalized-trace-id trace-id 32))
         (caller-span-id (normalized-trace-id span-id 16)))
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
                             config :override content-capture :metadata metadata
                             :parent-mode (getf ctx :content-capture-mode))
      :generation-id (or generation-id (generate-id))
      :trace-id (or caller-trace-id inherited-trace-id (generate-trace-id))
      :span-id (or caller-span-id (generate-span-id))
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
                                          content-capture execution-kind
                                          skill-name skill-description skill-source-uri
                                          skill-resource-name process-executable-name
                                          process-executable-path)
  "Create and start a tool execution recorder.
STARTED-AT overrides the wall clock; see START-GENERATION.

CONTENT-CAPTURE sets the capture mode for this tool execution only. Without it
the recorder takes the mode the enclosing generation resolved to, then the
capture-mode resolver, then the client-level mode. Trace identity and parent
identity are captured here, not from the context at completion."
  (let ((config (client-config client))
        (ctx *trace-context*))
    (make-instance 'tool-execution-recorder
      :client client
      :trace-id (or (normalized-trace-id (getf ctx :trace-id) 32) (generate-trace-id))
      :span-id (generate-span-id)
      :parent-span-id (normalized-trace-id (getf ctx :span-id) 16)
      :started-at (or started-at (iso8601-now))
      :content-capture-mode (resolve-content-capture-mode
                             config
                             :override content-capture
                             :parent-mode (getf *trace-context* :content-capture-mode))
      :execution-kind (when (tool-execution-kind-p execution-kind) execution-kind)
      :skill-name (normalize-tool-string skill-name)
      :skill-description (normalize-tool-string skill-description)
      :skill-source-uri (project-tool-source-uri skill-source-uri)
      :skill-resource-name (normalize-tool-string skill-resource-name)
      :process-executable-name (normalize-tool-string process-executable-name)
      :process-executable-path (normalize-tool-string process-executable-path)
      :tool-name (if (tool-execution-kind-p execution-kind)
                     (normalize-tool-string tool-name)
                     tool-name)
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
                                     dimensions encoding-format started-at
                                     trace-id span-id)
  "Create and start an embedding recorder.
DIMENSIONS is the requested dimension count; a result dimension count set later
via set-result takes precedence over it on the span.
STARTED-AT overrides the wall clock; see START-GENERATION.

Capture inherits the enclosing span's resolved mode, then the resolver and
client mode. TRACE-ID and SPAN-ID follow START-GENERATION's independent override
rules. IDs and the ambient parent are snapshotted here, not at RECORDER-END."
  (let ((config (client-config client)))
    (make-instance 'embedding-recorder
      :client client
      :started-at (or started-at (iso8601-now))
      :content-capture-mode (resolve-content-capture-mode
                             config :parent-mode (getf *trace-context* :content-capture-mode))
      :trace-id (or (normalized-trace-id trace-id 32)
                    (normalized-trace-id (getf *trace-context* :trace-id) 32)
                    (generate-trace-id))
      :span-id (or (normalized-trace-id span-id 16) (generate-span-id))
      :parent-span-id (normalized-trace-id (getf *trace-context* :span-id) 16)
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
them immediately to build parent-step-id chains. The step opens a new trace but
inherits the ambient capture mode. Without a parent mode, it uses the resolver
(with METADATA) then the client mode. Payload and span gates use this frozen mode.
STARTED-AT overrides the wall clock; see START-GENERATION."
  (let ((config (client-config client)))
    (make-instance 'workflow-step-recorder
      :client client
      :started-at (or started-at (iso8601-now))
      :content-capture-mode (resolve-content-capture-mode
                             config :metadata metadata
                             :parent-mode (getf *trace-context* :content-capture-mode))
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
