(defpackage :agento11y-cl
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
   ;; Cloud trial evaluation (experimental)
   #:trigger-trial-evaluation
   #:get-trial-evaluation
   #:trial-evaluation-terminal-p
   #:trial-evaluate
   ;; Trial artifacts
   #:upload-trial-artifact
   #:trial-artifact
   ;; Built-in evaluators
   #:evaluate-output
   #:evaluation-result
   #:make-evaluation-result
   #:evaluation-result-p
   #:evaluation-result-evaluator-id
   #:evaluation-result-evaluator-version
   #:evaluation-result-evaluator-kind
   #:evaluation-result-value
   #:evaluation-result-passed
   #:evaluation-result-explanation
   #:evaluation-result-score-key
   #:evaluation-result-metadata
   #:evaluation-result-grader
   #:llm-judge
   #:make-llm-judge
   #:regex-judge
   #:make-regex-judge
   #:trial-record-evaluation
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
   ;; Hooks (synchronous evaluation)
   #:evaluate-hook
   #:hook-context
   #:hook-input
   #:hook-evaluation
   #:hook-evaluate-response
   #:make-hook-context
   #:make-hook-input
   #:make-hooks-config
   #:hooks-config
   #:hooks-config-enabled
   #:hooks-config-phases
   #:hooks-config-timeout-sec
   #:hooks-config-fail-open
   #:hook-context-model-provider
   #:hook-context-model-name
   #:hook-context-agent-name
   #:hook-context-agent-version
   #:hook-context-tags
   #:hook-context-conversation-id
   #:hook-context-trace-id
   #:hook-context-span-id
   #:hook-input-messages
   #:hook-input-tools
   #:hook-input-system-prompt
   #:hook-input-output
   #:hook-input-conversation-preview
   #:response-action
   #:response-rule-id
   #:response-reason
   #:response-transformed-input
   #:response-evaluations
   #:evaluation-rule-id
   #:evaluation-evaluator-id
   #:evaluation-evaluator-kind
   #:evaluation-passed
   #:evaluation-latency-ms
   #:evaluation-explanation
   #:evaluation-reason
   #:agento11y-hook-denied-error
   #:agento11y-hook-denied-error-rule-id
   #:agento11y-hook-denied-error-reason
   #:agento11y-hook-denied-error-evaluations
   #:agento11y-hook-denied-error-transformed-input
   #:agento11y-hook-transport-error
   ;; Config
   #:agento11y-config
   #:make-config
   #:resolve-config-from-env
   #:config-agent-name
   #:config-agent-version
   #:config-extra-headers
   #:config-api-endpoint
   #:config-hooks-config
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
   #:config-content-capture-mode
   #:config-content-capture-resolver
   #:config-embedding-capture-input
   #:config-embedding-max-input-items
   #:config-embedding-max-text-length
   #:config-experimental-features
   ;; Types + constructors
   #:message
   #:text-part
   #:thinking-part
   #:tool-call-part
   #:tool-result-part
   #:media-part
   #:token-usage
   #:make-message
   #:make-text-part
   #:make-thinking-part
   #:make-tool-call-part
   #:make-tool-result-part
   #:make-media-part
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
   #:media-part-kind
   #:media-part-url
   #:media-part-mime-type
   #:media-part-name
   #:media-part-provider-type
   ;; Normalization
   #:normalize-input-messages
   #:normalize-message
   #:normalize-content-to-parts
   #:extract-system-prompt
   #:build-tool-name-map
   #:build-output-message
   ;; Thread-propagated specials: both are thread-confined, so a spawned
   ;; thread sees NIL unless the caller carries them across with
   ;; CAPTURE-TELEMETRY-CONTEXT / TELEMETRY-CONTEXT-THUNK.
   #:*trace-context*
   #:*experiment-run*
   #:child-trace-context
   #:capture-telemetry-context
   #:with-telemetry-context
   #:telemetry-context-thunk
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
   #:otel-double-attr
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
   #:agento11y-error
   #:agento11y-error-message
   #:agento11y-config-error
   #:agento11y-export-error
   #:agento11y-export-error-status-code
   #:agento11y-validation-error
   #:agento11y-not-found-error
   #:agento11y-conflict-error
   #:agento11y-conflict-error-kind
   #:agento11y-actor-mismatch-error
   #:agento11y-experimental-disabled-error
   #:agento11y-trial-evaluation-failed-error
   #:agento11y-trial-evaluation-timeout-error
   #:agento11y-trial-evaluation-error-id
   #:agento11y-trial-evaluation-error-detail))
