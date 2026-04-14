(in-package :sigil-cl/t)

;;; --- Helper: make a test client with captured requests ---

(defun make-test-client (&key (generation-enabled t) (traces-enabled t)
                               (capture :metadata-only)
                               (generation-endpoint "http://test-sigil:4318/api/v1/generations:export")
                               (traces-endpoint "http://test-sigil:4318/v1/traces"))
  "Create a client with mock HTTP that captures requests."
  (let ((requests nil))
    (values
     (make-client
      (make-config
       :generation-endpoint generation-endpoint
       :generation-enabled generation-enabled
       :traces-endpoint traces-endpoint
       :traces-enabled traces-enabled
       :content-capture-mode capture
       :service-name "test-service"
       :service-version "1.0.0"
       :auth-mode :bearer
       :auth-password "test-token"
       :http-fn (lambda (url &key headers content)
                  (declare (ignore headers))
                  (push (list url content) requests)
                  (values "{}" 200))))
     (lambda () (reverse requests)))))

;;; ================================================================
;;; Test suites
;;; ================================================================

(defun run-util-tests ()
  (with-test-suite ("Util")
    ;; ID generation
    (let ((id1 (sigil-cl::generate-id))
          (id2 (sigil-cl::generate-id)))
      (check "generate-id starts with gen_"
             (and (stringp id1) (eql 0 (search "gen_" id1))))
      (check "generate-id unique" (not (equal id1 id2))))

    ;; Trace/span IDs
    (let ((tid (sigil-cl::generate-trace-id)))
      (check "trace-id is 32 hex chars" (and (stringp tid) (= (length tid) 32))))
    (let ((sid (sigil-cl::generate-span-id)))
      (check "span-id is 16 hex chars" (and (stringp sid) (= (length sid) 16))))

    ;; ISO 8601
    (let ((now (iso8601-now)))
      (check "iso8601-now format" (and (stringp now) (= (length now) 20)
                                       (char= (char now 19) #\Z))))

    ;; iso8601-to-unix-nano
    (let ((nano (iso8601-to-unix-nano "2024-01-01T00:00:00Z")))
      (check "iso8601-to-unix-nano basic" (and (stringp nano) (plusp (length nano))))
      (check "iso8601-to-unix-nano value" (equal nano "1704067200000000000")))

    (let ((nano-frac (iso8601-to-unix-nano "2024-01-01T00:00:00.500Z")))
      (check "iso8601-to-unix-nano fractional"
             (equal nano-frac "1704067200500000000")))

    (check "iso8601-to-unix-nano nil for bad input"
           (null (iso8601-to-unix-nano "not-a-date")))

    ;; current-unix-nano
    (let ((n (current-unix-nano)))
      (check "current-unix-nano returns string" (stringp n))
      (check "current-unix-nano is positive" (plusp (parse-integer n))))

    ;; unix-nano-plus-seconds
    (check "unix-nano-plus-seconds"
           (equal (sigil-cl::unix-nano-plus-seconds "1000000000000" 1.0d0)
                  "1001000000000"))
    (check "unix-nano-plus-seconds nil duration"
           (equal (sigil-cl::unix-nano-plus-seconds "1000000000000" nil)
                  "1000000000000"))

    ;; backoff
    (check "backoff attempt 0" (= (sigil-cl::backoff-seconds 0 0.1 5.0) 0.1))
    (check "backoff attempt 3" (= (sigil-cl::backoff-seconds 3 0.1 5.0) 0.8))
    (check "backoff capped" (= (sigil-cl::backoff-seconds 10 0.1 5.0) 5.0))))

(defun run-json-tests ()
  (with-test-suite ("JSON")
    (let ((obj (jobj "a" 1 "b" "two")))
      (check "jobj creates hash-table" (hash-table-p obj))
      (check "jget retrieves value" (= (jget obj "a") 1))
      (check "jget string value" (equal (jget obj "b") "two"))
      (check "jget missing key" (null (jget obj "c"))))

    (let ((arr (jarr 1 2 3)))
      (check "jarr creates vector" (vectorp arr))
      (check "jarr length" (= (length arr) 3)))

    (let ((nested (jobj "outer" (jobj "inner" 42))))
      (check "jget* nested" (= (jget* nested "outer" "inner") 42))
      (check "jget* missing" (null (jget* nested "outer" "missing"))))))

(defun run-auth-tests ()
  (with-test-suite ("Auth")
    ;; None
    (let ((headers (sigil-cl::build-auth-headers
                    (make-config :auth-mode :none))))
      (check "auth none: no headers" (null headers)))

    ;; Bearer
    (let ((headers (sigil-cl::build-auth-headers
                    (make-config :auth-mode :bearer :auth-password "tok123"))))
      (check "auth bearer: has Authorization"
             (equal (cdr (assoc "Authorization" headers :test #'equal))
                    "Bearer tok123")))

    ;; Basic with explicit user
    (let ((headers (sigil-cl::build-auth-headers
                    (make-config :auth-mode :basic
                                 :auth-user "user" :auth-password "pass"
                                 :tenant-id "t1"))))
      (check "auth basic: has Authorization"
             (search "Basic " (cdr (assoc "Authorization" headers :test #'equal))))
      (check "auth basic: has tenant"
             (equal (cdr (assoc "X-Scope-OrgID" headers :test #'equal)) "t1")))

    ;; Basic with tenant-id fallback (Grafana Cloud pattern)
    (let ((headers (sigil-cl::build-auth-headers
                    (make-config :auth-mode :basic
                                 :auth-password "glc_key123"
                                 :tenant-id "12345"))))
      (check "auth basic fallback: has Authorization"
             (search "Basic " (cdr (assoc "Authorization" headers :test #'equal))))
      (check "auth basic fallback: uses tenant-id as user"
             (let ((expected (cl-base64:string-to-base64-string "12345:glc_key123")))
               (search expected (cdr (assoc "Authorization" headers :test #'equal)))))
      (check "auth basic fallback: has tenant header"
             (equal (cdr (assoc "X-Scope-OrgID" headers :test #'equal)) "12345")))

    ;; Tenant
    (let ((headers (sigil-cl::build-auth-headers
                    (make-config :auth-mode :tenant :tenant-id "org42"))))
      (check "auth tenant: has X-Scope-OrgID"
             (equal (cdr (assoc "X-Scope-OrgID" headers :test #'equal)) "org42"))
      (check "auth tenant: no Authorization"
             (null (assoc "Authorization" headers :test #'equal))))

    ;; Traces auth forwarding
    (let ((cfg (make-config :generation-endpoint "http://a:4318/api/v1/generations:export"
                             :traces-endpoint "http://b:4318/v1/traces"
                             :auth-mode :bearer :auth-password "tok"
                             :traces-forward-auth t)))
      (check "traces auth: forward-auth=t -> headers"
             (not (null (sigil-cl::build-traces-auth-headers cfg)))))
    (let ((cfg (make-config :generation-endpoint "http://a:4318/api/v1/generations:export"
                             :traces-endpoint "http://b:4318/v1/traces"
                             :auth-mode :bearer :auth-password "tok"
                             :traces-forward-auth nil)))
      (check "traces auth: forward-auth=nil -> nil"
             (null (sigil-cl::build-traces-auth-headers cfg))))))

(defun run-queue-tests ()
  (with-test-suite ("Queue")
    ;; Basic enqueue/drain
    (let ((q (sigil-cl::make-bounded-queue :max-size 10)))
      (sigil-cl::queue-enqueue q :a)
      (sigil-cl::queue-enqueue q :b)
      (sigil-cl::queue-enqueue q :c)
      (check "queue length" (= (sigil-cl::queue-length q) 3))
      (let ((batch (sigil-cl::queue-drain-batch q 2)))
        (check "drain-batch returns 2" (= (length batch) 2))
        (check "drain-batch FIFO" (equal batch '(:a :b))))
      (check "remaining after drain" (= (sigil-cl::queue-length q) 1)))

    ;; Drain all
    (let ((q (sigil-cl::make-bounded-queue :max-size 10)))
      (sigil-cl::queue-enqueue q 1)
      (sigil-cl::queue-enqueue q 2)
      (let ((all (sigil-cl::queue-drain-all q)))
        (check "drain-all returns all" (= (length all) 2))
        (check "drain-all FIFO" (equal all '(1 2))))
      (check "empty after drain-all" (sigil-cl::queue-empty-p q)))

    ;; Overflow drops oldest
    (let ((q (sigil-cl::make-bounded-queue :max-size 3)))
      (dotimes (i 5) (sigil-cl::queue-enqueue q i))
      (check "overflow caps at max" (= (sigil-cl::queue-length q) 3))
      (let ((items (sigil-cl::queue-drain-all q)))
        (check "overflow keeps newest" (= (first items) 2))))

    ;; Empty queue
    (let ((q (sigil-cl::make-bounded-queue)))
      (check "drain-batch on empty" (null (sigil-cl::queue-drain-batch q 10)))
      (check "drain-all on empty" (null (sigil-cl::queue-drain-all q))))))

(defun run-otel-tests ()
  (with-test-suite ("OTel")
    ;; Attribute helpers
    (let ((attr (otel-string-attr "key" "val")))
      (check "string-attr key" (equal (jget attr "key") "key"))
      (check "string-attr value" (equal (jget* attr "value" "stringValue") "val")))

    (let ((attr (otel-int-attr "count" 42)))
      (check "int-attr value" (equal (jget* attr "value" "intValue") "42")))

    (let ((attr (otel-bool-attr "flag" t)))
      (check "bool-attr true" (eq (jget* attr "value" "boolValue") t)))
    (let ((attr (otel-bool-attr "flag" nil)))
      (check "bool-attr false" (eq (jget* attr "value" "boolValue") nil)))

    ;; Span building
    (let ((span (sigil-cl::build-span :trace-id "abc" :span-id "def" :name "test"
                                       :kind 1 :start-time-unix-nano "100"
                                       :end-time-unix-nano "200")))
      (check "span traceId" (equal (jget span "traceId") "abc"))
      (check "span name" (equal (jget span "name") "test"))
      (check "span kind" (= (jget span "kind") 1))
      (check "span no parentSpanId" (null (jget span "parentSpanId"))))

    (let ((span (sigil-cl::build-span :trace-id "a" :span-id "b"
                                       :parent-span-id "p" :name "child" :kind 1)))
      (check "span has parentSpanId" (equal (jget span "parentSpanId") "p")))

    ;; OTLP payload
    (let* ((span (sigil-cl::build-span :trace-id "t" :span-id "s" :name "x" :kind 1))
           (payload (sigil-cl::build-otlp-payload (list span) "my-svc" "1.0")))
      (check "payload has resourceSpans" (jget payload "resourceSpans"))
      (let* ((rs (aref (jget payload "resourceSpans") 0))
             (svc-name (jget* (aref (jget* rs "resource" "attributes") 0) "value" "stringValue")))
        (check "payload service.name" (equal svc-name "my-svc"))))

    ;; Error classification
    (check "classify 429" (equal (sigil-cl::classify-error "status=429") "rate_limit"))
    (check "classify 500" (equal (sigil-cl::classify-error "status=500") "server_error"))
    (check "classify timeout" (equal (sigil-cl::classify-error "Connection timed out") "timeout"))
    (check "classify nil" (null (sigil-cl::classify-error nil)))))

(defun run-recorder-tests ()
  (with-test-suite ("Recorder")
    ;; Generation recorder lifecycle
    (multiple-value-bind (client get-requests) (make-test-client)
      (let ((rec (start-generation client
                   :mode :sync
                   :conversation-id "conv-1"
                   :agent-name "test-agent"
                   :agent-version "1.0"
                   :model-provider "openai"
                   :model-name "gpt-4")))
        (set-result rec :usage (make-token-usage :input 100 :output 50)
                        :stop-reason "end_turn"
                        :response-id "resp-1"
                        :response-model "gpt-4-0125")
        (recorder-end rec)
        (check "recorder ended" (sigil-cl::recorder-ended-p rec))

        ;; Check generation queue
        (let ((gen-items (sigil-cl::queue-drain-all
                          (sigil-cl::client-generation-queue client))))
          (check "generation enqueued" (= (length gen-items) 1))
          (let ((gen (first gen-items)))
            (check "gen has id" (search "gen_" (jget gen "id")))
            (check "gen id matches recorder" (equal (jget gen "id") (gen-rec-generation-id rec)))
            (check "gen mode" (equal (jget gen "mode") "GENERATION_MODE_SYNC"))
            (check "gen model provider" (equal (jget* gen "model" "provider") "openai"))
            (check "gen conversation_id" (equal (jget gen "conversation_id") "conv-1"))
            (check "gen usage input" (= (jget* gen "usage" "input_tokens") 100))
            (check "gen response_id" (equal (jget gen "response_id") "resp-1"))
            ;; No conversation_title as top-level field
            (check "gen no top-level conversation_title"
                   (null (jget gen "conversation_title"))))))

        ;; Check trace queue
        (let ((trace-items (sigil-cl::queue-drain-all
                            (sigil-cl::client-trace-queue client))))
          (check "span enqueued" (= (length trace-items) 1))
          (let ((span (first trace-items)))
            (check "span has traceId" (plusp (length (jget span "traceId"))))
            (check "span name" (search "generateText" (jget span "name")))
            (check "span kind=CLIENT" (= (jget span "kind") 3)))))

    ;; Idempotent end
    (multiple-value-bind (client2 get-requests2) (make-test-client)
      (declare (ignore get-requests2))
      (let ((rec2 (start-generation client2 :mode :sync)))
        (recorder-end rec2)
        (recorder-end rec2)
        (check "idempotent end" t)))

    ;; Generation with call error
    (multiple-value-bind (client get-requests) (make-test-client)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "openai"
                                          :model-name "gpt-4")))
        (set-call-error rec "Connection refused")
        (recorder-end rec)
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client)))))
          (check "call error -> redacted (metadata-only)"
                 (equal (jget gen "call_error") "<redacted>"))
          (check "stop_reason error" (equal (jget gen "stop_reason") "error")))))

    ;; Content capture full
    (multiple-value-bind (client get-requests) (make-test-client :capture :full)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m"
                                          :system-prompt "Be helpful"
                                          :input-messages (list (make-message
                                                                 :role :user
                                                                 :parts (list (make-text-part "Hello")))))))
        (set-result rec :output-messages (list (make-message
                                                :role :assistant
                                                :parts (list (make-text-part "Hi there")))))
        (set-call-error rec "test error")
        (recorder-end rec)
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client)))))
          (check "full capture: system prompt included"
                 (equal (jget gen "system_prompt") "Be helpful"))
          (check "full capture: input messages included"
                 (plusp (length (jget gen "input"))))
          (check "full capture: output messages included"
                 (plusp (length (jget gen "output"))))
          (check "full capture: call error not redacted"
                 (equal (jget gen "call_error") "test error")))))

    ;; Metadata-only preserves message structure
    (multiple-value-bind (client get-requests) (make-test-client :capture :metadata-only)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m"
                                          :input-messages (list (make-message
                                                                 :role :user
                                                                 :parts (list (make-text-part "Secret")))))))
        (set-result rec :output-messages (list (make-message
                                                :role :assistant
                                                :parts (list (make-text-part "Also secret")))))
        (recorder-end rec)
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client)))))
          (check "metadata-only: input present" (jget gen "input"))
          (check "metadata-only: output present" (jget gen "output"))
          (let ((input-msg (aref (jget gen "input") 0)))
            (check "metadata-only: role preserved"
                   (equal (jget input-msg "role") "MESSAGE_ROLE_USER"))
            (check "metadata-only: text redacted"
                   (equal (jget (aref (jget input-msg "parts") 0) "text") ""))))))

    ;; Caller metadata merge
    (multiple-value-bind (client get-requests) (make-test-client)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m"
                                          :metadata '(("my.key" . "my-value")
                                                      ("framework" . "my-framework")))))
        (recorder-end rec)
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client)))))
          (check "caller metadata: my.key present"
                 (equal (jget* gen "metadata" "my.key") "my-value"))
          (check "caller metadata: framework present"
                 (equal (jget* gen "metadata" "framework") "my-framework"))
          (check "caller metadata: sdk.name still present"
                 (equal (jget* gen "metadata" "sigil.sdk.name") "sigil-cl")))))

    ;; Conversation title in metadata
    (multiple-value-bind (client get-requests) (make-test-client)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m"
                                          :conversation-title "My Chat")))
        (recorder-end rec)
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client)))))
          (check "conversation title in metadata"
                 (equal (jget* gen "metadata" "sigil.conversation.title") "My Chat"))
          (check "no top-level conversation_title"
                 (null (jget gen "conversation_title"))))))

    ;; Parent generation IDs
    (multiple-value-bind (client get-requests) (make-test-client)
      (declare (ignore get-requests))
      ;; Set at start
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m"
                                          :parent-generation-ids '("gen-aaa" "gen-bbb"))))
        (recorder-end rec)
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client)))))
          (check "parent_generation_ids present"
                 (= (length (jget gen "parent_generation_ids")) 2))
          (check "parent_generation_ids first"
                 (equal (aref (jget gen "parent_generation_ids") 0) "gen-aaa"))
          (check "parent_generation_ids second"
                 (equal (aref (jget gen "parent_generation_ids") 1) "gen-bbb")))))

    ;; Parent generation IDs via set-result
    (multiple-value-bind (client get-requests) (make-test-client)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m")))
        (set-result rec :parent-generation-ids '("gen-ccc"))
        (recorder-end rec)
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client)))))
          (check "parent_generation_ids via set-result"
                 (= (length (jget gen "parent_generation_ids")) 1))
          (check "parent_generation_ids set-result value"
                 (equal (aref (jget gen "parent_generation_ids") 0) "gen-ccc")))))

    ;; Omitted parent_generation_ids -> absent from payload
    (multiple-value-bind (client get-requests) (make-test-client)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m")))
        (recorder-end rec)
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client)))))
          (check "no parent_generation_ids when omitted"
                 (null (jget gen "parent_generation_ids"))))))

    ;; Total tokens preserved
    (multiple-value-bind (client get-requests) (make-test-client)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m")))
        (set-result rec :usage (make-token-usage :input 100 :output 50 :total 200))
        (recorder-end rec)
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client)))))
          (check "total_tokens preserves explicit value"
                 (= (jget* gen "usage" "total_tokens") 200)))))

    ;; Tool call input_json is base64-encoded
    (multiple-value-bind (client get-requests) (make-test-client :capture :full)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m"
                                          :input-messages (list (make-message
                                                                 :role :assistant
                                                                 :parts (list (make-tool-call-part
                                                                               :id "tc1"
                                                                               :name "search"
                                                                               :input-json "{\"q\":\"test\"}")))))))
        (recorder-end rec)
        (let* ((gen (first (sigil-cl::queue-drain-all
                            (sigil-cl::client-generation-queue client))))
               (input-msg (aref (jget gen "input") 0))
               (tc-part (aref (jget input-msg "parts") 0))
               (input-json (jget* tc-part "tool_call" "input_json")))
          (check "tool call input_json is base64"
                 (equal input-json
                        (cl-base64:string-to-base64-string "{\"q\":\"test\"}"))))))

    ;; Tool execution recorder
    (multiple-value-bind (client get-requests) (make-test-client :generation-enabled nil)
      (declare (ignore get-requests))
      (let ((*trace-context* (list :trace-id "parent-trace" :span-id "parent-span")))
        (let ((rec (start-tool-execution client
                     :tool-name "search"
                     :tool-call-id "tc-1"
                     :tool-type "function")))
          (set-result rec :arguments "{\"q\":\"test\"}"
                          :result "found it"
                          :duration-seconds 0.5d0)
          (recorder-end rec)
          (let ((span (first (sigil-cl::queue-drain-all
                              (sigil-cl::client-trace-queue client)))))
            (check "tool span enqueued" (not (null span)))
            (check "tool span parent" (equal (jget span "parentSpanId") "parent-span"))
            (check "tool span trace" (equal (jget span "traceId") "parent-trace"))
            (check "tool span name" (search "execute_tool search" (jget span "name")))
            (check "tool span kind=INTERNAL" (= (jget span "kind") 1))))))

    ;; Embedding recorder
    (multiple-value-bind (client get-requests) (make-test-client :generation-enabled nil)
      (declare (ignore get-requests))
      (let ((rec (start-embedding client
                   :model-provider "openai"
                   :model-name "text-embedding-3-small"
                   :source "test-source")))
        (set-result rec :input-count 5 :input-tokens 100 :duration-seconds 0.2d0)
        (recorder-end rec)
        (let ((span (first (sigil-cl::queue-drain-all
                            (sigil-cl::client-trace-queue client)))))
          (check "embedding span enqueued" (not (null span)))
          (check "embedding span name"
                 (search "embeddings" (jget span "name"))))))))

(defun run-client-tests ()
  (with-test-suite ("Client")
    ;; Noop client
    (let ((client (noop-client)))
      (check "noop client created" (not (null client)))
      (let ((rec (start-generation client :mode :sync)))
        (recorder-end rec)
        (check "noop: generation queue empty"
               (sigil-cl::queue-empty-p (sigil-cl::client-generation-queue client)))))

    ;; Flush drains queues
    (multiple-value-bind (client get-requests) (make-test-client)
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m")))
        (set-result rec :usage (make-token-usage :input 10 :output 5))
        (recorder-end rec))
      (client-flush client)
      (check "flush: generation queue empty"
             (sigil-cl::queue-empty-p (sigil-cl::client-generation-queue client)))
      (check "flush: trace queue empty"
             (sigil-cl::queue-empty-p (sigil-cl::client-trace-queue client)))
      (check "flush: HTTP requests made" (plusp (length (funcall get-requests)))))

    ;; Endpoint URL not doubled
    (multiple-value-bind (client get-requests) (make-test-client)
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m")))
        (recorder-end rec))
      (client-flush client)
      (let* ((reqs (funcall get-requests))
             (gen-req (find "generations:export" reqs :key #'first :test #'search)))
        (check "endpoint URL uses configured URL directly"
               (equal (first gen-req) "http://test-sigil:4318/api/v1/generations:export"))))

    ;; Start/shutdown lifecycle
    (multiple-value-bind (client get-requests) (make-test-client)
      (declare (ignore get-requests))
      (client-start client)
      (check "client started" (sigil-cl::client-running-p client))
      (check "worker thread alive"
             (bt2:thread-alive-p (sigil-cl::client-worker-thread client)))
      ;; Double start should be a no-op
      (let ((thread1 (sigil-cl::client-worker-thread client)))
        (client-start client)
        (check "double start: same thread"
               (eq thread1 (sigil-cl::client-worker-thread client))))
      (client-shutdown client :timeout-sec 2)
      (check "client stopped" (not (sigil-cl::client-running-p client)))
      (check "worker thread nil" (null (sigil-cl::client-worker-thread client))))))

(defun run-macro-tests ()
  (with-test-suite ("Macros")
    ;; with-generation auto-ends
    (multiple-value-bind (client get-requests) (make-test-client)
      (declare (ignore get-requests))
      (let ((rec nil))
        (with-generation (r client :mode :sync :model-provider "t" :model-name "m")
          (setf rec r)
          (set-result r :usage (make-token-usage :input 1 :output 1)))
        (check "with-generation: ended" (sigil-cl::recorder-ended-p rec))))

    ;; with-generation on error still ends
    (multiple-value-bind (client get-requests) (make-test-client)
      (declare (ignore get-requests))
      (let ((rec nil))
        (handler-case
            (with-generation (r client :mode :sync :model-provider "t" :model-name "m")
              (setf rec r)
              (error "boom"))
          (error () nil))
        (check "with-generation: ended on error" (sigil-cl::recorder-ended-p rec))))

    ;; with-generation binds *trace-context*
    (multiple-value-bind (client get-requests) (make-test-client)
      (declare (ignore get-requests))
      (check "trace-context nil before" (null *trace-context*))
      (with-generation (r client :mode :sync :model-provider "t" :model-name "m")
        (check "trace-context set inside" (not (null *trace-context*)))
        (check "trace-context has trace-id" (getf *trace-context* :trace-id))
        (check "trace-context has span-id" (getf *trace-context* :span-id)))
      (check "trace-context nil after" (null *trace-context*)))))

(defun run-normalize-tests ()
  (with-test-suite ("Normalize")
    ;; Anthropic text content
    (let ((parts (normalize-content-to-parts "Hello world")))
      (check "string content -> text part" (= (length parts) 1))
      (check "text part value" (equal (text-part-text (first parts)) "Hello world")))

    ;; Anthropic content blocks
    (let* ((content (vector (jobj "type" "text" "text" "Hi")
                            (jobj "type" "thinking" "thinking" "Let me think...")
                            (jobj "type" "tool_use" "id" "tc1" "name" "search"
                                  "input" (jobj "q" "test"))))
           (parts (normalize-content-to-parts content)))
      (check "anthropic blocks: 3 parts" (= (length parts) 3))
      (check "anthropic: text part" (typep (first parts) 'text-part))
      (check "anthropic: thinking part" (typep (second parts) 'thinking-part))
      (check "anthropic: tool-call part" (typep (third parts) 'tool-call-part))
      (check "anthropic: tool id" (equal (tool-call-part-id (third parts)) "tc1"))
      (check "anthropic: tool input-json is string"
             (stringp (tool-call-part-input-json (third parts)))))

    ;; OpenAI assistant message with tool_calls
    (let* ((msg (jobj "role" "assistant"
                      "content" "Let me search"
                      "tool_calls" (vector (jobj "id" "call_1"
                                                 "type" "function"
                                                 "function" (jobj "name" "search"
                                                                   "arguments" "{\"q\":\"test\"}")))))
           (normalized (normalize-message msg)))
      (check "openai assistant: message created" (not (null normalized)))
      (check "openai assistant: role" (eq (message-role normalized) :assistant))
      (check "openai assistant: has parts" (= (length (message-parts normalized)) 2))
      (let ((tc (second (message-parts normalized))))
        (check "openai tool_call part" (typep tc 'tool-call-part))
        (check "openai tool_call id" (equal (tool-call-part-id tc) "call_1"))
        (check "openai tool_call name" (equal (tool-call-part-name tc) "search"))))

    ;; OpenAI tool message
    (let* ((tool-map (let ((m (make-hash-table :test 'equal)))
                       (setf (gethash "call_1" m) "search")
                       m))
           (msg (jobj "role" "tool" "tool_call_id" "call_1" "content" "results here"))
           (normalized (normalize-message msg tool-map)))
      (check "openai tool msg: created" (not (null normalized)))
      (check "openai tool msg: role" (eq (message-role normalized) :tool))
      (let ((part (first (message-parts normalized))))
        (check "openai tool result: has name from map"
               (equal (tool-result-part-name part) "search"))
        (check "openai tool result: content"
               (equal (tool-result-part-content part) "results here"))))

    ;; System message extraction
    (let* ((messages (list (jobj "role" "system" "content" "You are helpful")
                           (jobj "role" "user" "content" "Hi")))
           (prompt (extract-system-prompt messages)))
      (check "extract-system-prompt" (equal prompt "You are helpful")))

    ;; normalize-input-messages filters system
    (let* ((messages (list (jobj "role" "system" "content" "Be helpful")
                           (jobj "role" "user" "content" "Hello")
                           (jobj "role" "assistant" "content" "Hi")))
           (normalized (normalize-input-messages messages)))
      (check "normalize-input: 2 messages (no system)" (= (length normalized) 2))
      (check "normalize-input: first is user" (eq (message-role (first normalized)) :user)))

    ;; build-output-message
    (let ((msg (build-output-message :text "Hello"
                                     :reasoning "Let me think"
                                     :tool-calls (list (list :id "tc1" :name "search"
                                                             :arguments "{\"q\":\"test\"}")))))
      (check "build-output: message created" (not (null msg)))
      (check "build-output: role" (eq (message-role msg) :assistant))
      (check "build-output: 3 parts" (= (length (message-parts msg)) 3))
      (check "build-output: thinking first" (typep (first (message-parts msg)) 'thinking-part))
      (check "build-output: tool-call second" (typep (second (message-parts msg)) 'tool-call-part))
      (check "build-output: text last" (typep (third (message-parts msg)) 'text-part)))))

;;; ================================================================
;;; Main test runner
;;; ================================================================

(defun run-tests ()
  "Run all sigil-cl tests. Returns (values ok-p total-pass total-fail)."
  (let ((total-pass 0)
        (total-fail 0))
    (dolist (test-fn (list #'run-util-tests
                           #'run-json-tests
                           #'run-auth-tests
                           #'run-queue-tests
                           #'run-otel-tests
                           #'run-recorder-tests
                           #'run-client-tests
                           #'run-macro-tests
                           #'run-normalize-tests))
      (multiple-value-bind (ok pass fail) (funcall test-fn)
        (declare (ignore ok))
        (incf total-pass pass)
        (incf total-fail fail)))
    (format t "~%============================~%")
    (format t "TOTAL: ~d passed, ~d failed~%" total-pass total-fail)
    (values (zerop total-fail) total-pass total-fail)))
