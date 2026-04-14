(in-package :sigil-cl)

;;; --- Client ---

(defclass sigil-client ()
  ((config           :initarg :config           :accessor client-config)
   (generation-queue :initarg :generation-queue :accessor client-generation-queue)
   (trace-queue      :initarg :trace-queue      :accessor client-trace-queue)
   (worker-thread    :initform nil              :accessor client-worker-thread)
   (running-p        :initform nil              :accessor client-running-p)
   (lock             :initform (bt2:make-lock :name "sigil-client")
                     :accessor client-lock)
   (wake-cv          :initform (bt2:make-condition-variable :name "sigil-wake")
                     :accessor client-wake-cv)))

(defun make-client (config)
  "Create a Sigil client from CONFIG."
  (make-instance 'sigil-client
    :config config
    :generation-queue (make-bounded-queue :max-size (config-queue-max config)
                                          :name "generation")
    :trace-queue (make-bounded-queue :max-size (effective-trace-queue-max config)
                                      :name "trace")))

(defun noop-client ()
  "Create a client that discards everything (for testing/disabled mode)."
  (make-client (make-config)))

;;; --- Background flush loop ---

(defun run-flush-loop (client)
  "Background loop: drain and export batches from both queues."
  (let ((config (client-config client)))
    (loop while (bt2:with-lock-held ((client-lock client))
                  (client-running-p client))
          do (handler-case
                 (let ((gen-batch (queue-drain-batch (client-generation-queue client)
                                                     (config-batch-size config)))
                       (trace-batch (queue-drain-all (client-trace-queue client))))
                   (when (and gen-batch (config-generation-enabled config))
                     (export-generations config gen-batch (build-auth-headers config)))
                   (when (and trace-batch (config-traces-enabled config))
                     (export-traces config trace-batch (build-traces-auth-headers config)))
                   (unless (or gen-batch trace-batch)
                     (bt2:with-lock-held ((client-lock client))
                       (bt2:condition-wait (client-wake-cv client) (client-lock client)
                                           :timeout (config-flush-interval-sec config)))))
               (error (e)
                 (sigil-log config :warn "flush-loop"
                           (format nil "error: ~a" (princ-to-string e)))
                 (sleep 1))))))

;;; --- Lifecycle ---

(defun client-start (client)
  "Start the background export thread."
  (bt2:with-lock-held ((client-lock client))
    (when (client-running-p client)
      (return-from client-start client))
    (let ((config (client-config client)))
      (when (or (config-generation-enabled config) (config-traces-enabled config))
        (setf (client-running-p client) t)
        (setf (client-worker-thread client)
              (bt2:make-thread (lambda () (run-flush-loop client))
                               :name "sigil-flush"))
        (sigil-log config :info "client" "started"))))
  client)

(defun client-shutdown (client &key (timeout-sec 5))
  "Flush remaining items and stop the background thread."
  (let ((was-running (bt2:with-lock-held ((client-lock client))
                       (prog1 (client-running-p client)
                         (setf (client-running-p client) nil)))))
    (when was-running
      (let ((config (client-config client)))
        (sigil-log config :info "client" "shutting down")
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
                    (sigil-log config :warn "client"
                              (format nil "failed to stop worker: ~a" (princ-to-string e)))))))))
        (setf (client-worker-thread client) nil)
        (sigil-log config :info "client" "stopped"))))
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
                   (sigil-log config :warn "flush"
                             (format nil "generation batch export failed: ~a"
                                     (princ-to-string e)))))))
    (when (config-traces-enabled config)
      (let ((spans (queue-drain-all (client-trace-queue client))))
        (when spans
          (handler-case
              (export-traces config spans (build-traces-auth-headers config))
            (error (e)
              (sigil-log config :warn "flush"
                        (format nil "trace export failed: ~a"
                                (princ-to-string e)))))))))
  nil)

;;; --- Recorder factories ---

(defun start-generation (client &key (mode :sync) conversation-id conversation-title
                                      user-id agent-name agent-version
                                      model-provider model-name
                                      system-prompt input-messages tools
                                      temperature top-p max-tokens tool-choice
                                      (thinking-enabled :unset)
                                      parent-generation-ids
                                      tags metadata)
  "Create and start a generation recorder."
  (make-instance 'generation-recorder
    :client client
    :started-at (iso8601-now)
    :generation-id (generate-id)
    :trace-id (generate-trace-id)
    :span-id (generate-span-id)
    :mode mode
    :conversation-id conversation-id
    :conversation-title conversation-title
    :user-id user-id
    :agent-name agent-name
    :agent-version agent-version
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
    :metadata metadata))

(defun start-tool-execution (client &key tool-name tool-call-id tool-type tool-description
                                          conversation-id agent-name agent-version
                                          model-provider model-name)
  "Create and start a tool execution recorder."
  (make-instance 'tool-execution-recorder
    :client client
    :started-at (iso8601-now)
    :tool-name tool-name
    :tool-call-id tool-call-id
    :tool-type tool-type
    :tool-description tool-description
    :conversation-id conversation-id
    :agent-name agent-name
    :agent-version agent-version
    :model-provider model-provider
    :model-name model-name))

(defun start-embedding (client &key model-provider model-name
                                     agent-name agent-version source)
  "Create and start an embedding recorder."
  (make-instance 'embedding-recorder
    :client client
    :started-at (iso8601-now)
    :model-provider model-provider
    :model-name model-name
    :agent-name agent-name
    :agent-version agent-version
    :source source))
