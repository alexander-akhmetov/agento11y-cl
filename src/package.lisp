(defpackage :sigil-cl
  (:use :cl)
  (:local-nicknames (:jzon :com.inuoe.jzon)
                    (:bt2 :bordeaux-threads-2)
                    (:alex :alexandria)
                    (:c2mop :closer-mop))
  (:export
   ;; Client lifecycle
   #:make-client
   #:client-start
   #:client-shutdown
   #:client-flush
   #:noop-client
   ;; Recorder creation
   #:start-generation
   #:start-tool-execution
   #:start-embedding
   #:start-workflow-step
   ;; Recorder operations
   #:set-result
   #:set-call-error
   #:recorder-end
   ;; Macros
   #:with-generation
   #:with-tool-execution
   #:with-embedding
   #:with-workflow-step
   #:with-span
   ;; Ratings
   #:submit-conversation-rating
   ;; Experiments and scores
   #:create-experiment
   #:get-experiment
   #:update-experiment
   #:complete-experiment
   #:cancel-experiment
   #:get-experiment-report
   #:list-experiment-scores
   #:export-scores
   #:experiment-url
   #:with-experiment
   #:experiment-run
   #:experiment-run-add-scores
   #:experiment-run-finalize
   #:experiment-run-url
   #:experiment-run-report
   #:experiment-run-produced-generation-ids
   #:experiment-run-track-generation-id
   #:experiment-run-reset-capture
   #:experiment-run-active-conversation-id
   #:experiment-run-accepted-count
   #:make-score
   #:stable-id
   ;; Config
   #:sigil-config
   #:make-config
   #:resolve-config-from-env
   #:config-agent-name
   #:config-agent-version
   #:config-extra-headers
   #:config-debug
   #:config-metrics-endpoint
   #:config-metrics-enabled
   #:config-metrics-forward-auth
   #:config-eval-endpoint
   #:config-eval-path-prefix
   #:config-scores-export-path
   #:config-experiment-url-template
   ;; Types + constructors
   #:message
   #:text-part
   #:thinking-part
   #:tool-call-part
   #:tool-result-part
   #:token-usage
   #:make-message
   #:make-text-part
   #:make-thinking-part
   #:make-tool-call-part
   #:make-tool-result-part
   #:make-token-usage
   ;; Accessors
   #:message-role
   #:message-parts
   #:text-part-text
   #:thinking-part-text
   #:tool-call-part-id
   #:tool-call-part-name
   #:tool-call-part-input-json
   #:tool-result-part-tool-call-id
   #:tool-result-part-name
   #:tool-result-part-content
   #:tool-result-part-is-error
   ;; Normalization
   #:normalize-input-messages
   #:normalize-message
   #:normalize-content-to-parts
   #:extract-system-prompt
   #:build-tool-name-map
   #:build-output-message
   ;; Trace context
   #:*trace-context*
   #:gen-rec-generation-id
   #:gen-rec-trace-id
   #:gen-rec-span-id
   #:wfs-rec-step-id
   #:wfs-rec-trace-id
   #:wfs-rec-span-id
   #:wfs-rec-conversation-id
   #:wfs-rec-step-name
   ;; OTel helpers
   #:otel-string-attr
   #:otel-int-attr
   #:otel-bool-attr
   #:otel-string-array-attr
   ;; JSON helpers
   #:jobj
   #:jarr
   #:jget
   #:jget*
   ;; Utility
   #:current-unix-nano
   #:iso8601-now
   #:iso8601-to-unix-nano
   ;; Conditions
   #:sigil-error
   #:sigil-error-message
   #:sigil-config-error
   #:sigil-export-error
   #:sigil-export-error-status-code
   #:sigil-validation-error
   #:sigil-not-found-error
   #:sigil-conflict-error))
