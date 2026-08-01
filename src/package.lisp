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
   #:upsert-experiment-run
   #:finalize-experiment-run
   #:get-experiment
   #:get-experiment-report
   #:list-experiment-scores
   #:export-scores
   #:experiment-url
   #:classify-conflict
   #:conflict-recoverable-p
   #:with-experiment
   #:run-experiment
   #:experiment-run
   #:experiment-run-add-scores
   #:experiment-run-publish
   #:experiment-run-buffered-score-count
   #:experiment-run-finalize
   #:experiment-run-url
   #:experiment-run-report
   #:experiment-run-produced-generation-ids
   #:experiment-run-track-generation-id
   #:experiment-run-reset-capture
   #:experiment-run-active-conversation-id
   #:experiment-run-accepted-count
   #:experiment-run-open-trial
   #:experiment-run-open-trials-list
   #:experiment-run-close-open-trials
   #:experiment-run-suite
   #:make-score
   #:make-dataset-item
   #:make-target-result
   #:stable-id
   ;; Trials
   #:experiment-trial
   #:with-trial
   #:create-trial
   #:finalize-trial
   #:trial-open
   #:trial-close
   #:trial-add-scores
   #:trial-bind-generation
   #:trial-bind-conversation
   #:trial-mint-id
   #:trial-id
   #:trial-run
   #:trial-experiment-id
   #:trial-test-case-id
   #:trial-attempt
   #:trial-status
   #:trial-error-text
   #:trial-conversation-id
   #:trial-generation-id
   #:trial-trace-id
   #:trial-span-id
   #:trial-created-p
   #:trial-closed-p
   ;; Local suites
   #:test-suite
   #:test-case
   #:make-test-suite
   #:make-test-case
   #:test-case-snapshot
   #:test-suite-case
   #:test-suite-suite-id
   #:test-suite-version
   #:test-suite-name
   #:test-suite-description
   #:test-suite-cases
   #:test-case-test-case-id
   #:test-case-name
   #:test-case-description
   #:test-case-tags
   #:test-case-category
   #:test-case-input
   #:test-case-expected
   #:test-case-metadata
   ;; Conversations and collections (read-only)
   #:list-collection-members
   #:get-conversation
   #:initial-user-prompt
   #:dataset-from-collection
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
   #:config-eval-auth-token
   #:config-scores-export-path
   #:config-ingest-actor
   #:config-experiment-url-template
   #:config-embedding-capture-input
   #:config-embedding-max-input-items
   #:config-embedding-max-text-length
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
   #:sigil-conflict-error
   #:sigil-conflict-error-kind
   #:sigil-actor-mismatch-error))
