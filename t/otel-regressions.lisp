(in-package :agento11y-cl/t)

(defmacro integrity-privacy-regression-cases ()
  '(progn
    (dolist (protocol '(:http :otel))
      (let* ((client (make-test-client :generation-protocol protocol :experimental-features t))
             (rec (start-generation client :input-messages '(invalid-message))))
        (check "serialization errors are explicit"
               (handler-case (progn (recorder-end rec) nil)
                 (agento11y-export-error () t)))
        (check "serialization failures remain visible to flush"
               (handler-case (progn (client-flush client) nil)
                 (agento11y-export-error (condition)
                   (getf (agento11y-export-error-diagnostics condition) :failures))))
        (check "failed serialization remains retryable"
               (not (agento11y-cl::recorder-ended-p rec)))))
    (let* ((text " {\"city\":\"中ü😀\"} ")
           (encoded (agento11y-cl::utf8-base64-encode text)))
      (check "UTF-8 base64 round trip includes supplementary characters"
             (equal (agento11y-cl::utf8-base64-decode encoded) text))
      (check "base64 carries UTF-8 octets"
             (equalp (cl-base64:base64-string-to-usb8-array encoded)
                     (babel:string-to-octets text :encoding :utf-8)))
      (let* ((part (make-tool-call-part :id "tc" :name "weather" :input-json text))
             (wire (agento11y-cl::serialize-part part :full)))
        (check "tool arguments retain Unicode and whitespace"
               (equal (agento11y-cl::utf8-base64-decode (jget* wire "tool_call" "input_json")) text)))
      (let* ((part (make-tool-result-part :tool-call-id "tc" :content "summary"
                                         :content-json text))
             (wire (agento11y-cl::serialize-part part :full)))
        (check "independent tool result JSON retains Unicode and whitespace"
               (equal (agento11y-cl::utf8-base64-decode (jget* wire "tool_result" "content_json")) text))
        (check "tool result text remains independent"
               (equal (jget* wire "tool_result" "content") "summary"))
        (let ((hidden (agento11y-cl::serialize-part part :metadata-only)))
          (check "metadata-only hides independent tool result JSON"
                 (null (jget* hidden "tool_result" "content_json")))))
      (let ((wire (agento11y-cl::serialize-part
                   (make-tool-result-part :tool-call-id "tc" :content text) :full)))
        (check "JSON inferred from text retains original whitespace"
               (equal (agento11y-cl::utf8-base64-decode (jget* wire "tool_result" "content_json")) text)))
      (let* ((bytes (make-array 4 :element-type '(unsigned-byte 8)
                                 :initial-contents '(255 0 128 42)))
             (redactor (agento11y-cl::make-secret-redactor)))
        (dolist (kind '(:arguments :result))
          (let* ((part (if (eq kind :arguments)
                           (make-tool-call-part :id "tc" :input-json bytes)
                           (make-tool-result-part :tool-call-id "tc" :content "text"
                                                  :content-json bytes)))
                 (key (if (eq kind :arguments) "tool_call" "tool_result"))
                 (field (if (eq kind :arguments) "input_json" "content_json"))
                 (raw (agento11y-cl::serialize-part part :full))
                 (scrubbed (agento11y-cl::serialize-part
                            part :full :redactor redactor :secret-mode :full)))
            (check (format nil "~a preserves raw bytes without scanning" kind)
                   (equalp bytes (cl-base64:base64-string-to-usb8-array
                                  (jget* raw key field))))
            (check (format nil "~a invalid UTF-8 fails closed with scanning" kind)
                   (equal (agento11y-cl::utf8-base64-decode (jget* scrubbed key field)) "[REDACTED]")))))
      (let ((tools (agento11y-cl::serialize-tools
                    (list (list :name "weather" :parameters (jobj "city" "中ü😀")))
                    :keep-content t)))
        (check "tool schema base64 uses UTF-8"
               (equal (jget (jzon:parse (cf-base64-text
                                        (jget (aref tools 0) "input_schema_json"))) "city")
                      "中ü😀"))))
    (let* ((client (make-test-client :capture :full))
           (rec (start-generation client :thinking-enabled t))
           (msg (make-message :role :assistant :name "named-assistant"
                              :parts (list (make-text-part "hello")))))
      (set-result rec :thinking-enabled nil :output-messages (list msg))
      (let ((payload (agento11y-cl::build-generation-payload
                      rec (agento11y-cl::client-config client))))
        (check "explicit false replaces true thinking-enabled"
               (and (nth-value 1 (gethash "thinking_enabled" payload))
                    (null (jget payload "thinking_enabled"))))
        (check "message name survives recorder serialization"
               (equal (jget* payload "output" 0 "name") "named-assistant")))
      (set-result rec :stop-reason "done")
      (check "omitted thinking-enabled leaves false unchanged"
             (null (agento11y-cl::gen-rec-thinking-enabled rec))))
    (let* ((inclusive (make-token-usage :input 100 :output 20 :cache-read 60
                                       :cache-creation 10 :input-semantics :inclusive))
           (legacy (make-token-usage :input 100)))
      (check "manual token usage defaults to unspecified semantics"
             (eq (agento11y-cl::token-usage-input-semantics legacy) :unspecified))
      (check "inclusive semantics uses the current proto enum"
             (equal (jget (agento11y-cl::serialize-usage inclusive) "input_semantics")
                    "TOKEN_INPUT_SEMANTICS_INCLUSIVE"))
      (check "unspecified semantics uses the current proto enum"
             (equal (jget (agento11y-cl::serialize-usage legacy) "input_semantics")
                    "TOKEN_INPUT_SEMANTICS_UNSPECIFIED"))
      (check "inclusive semantics does not add cache buckets to totals"
             (= (agento11y-cl::token-usage-total-tokens inclusive) 120))
      (dolist (protocol '(:http :otel))
        (let* ((client (make-test-client :capture :full :generation-protocol protocol
                                        :experimental-features t :metrics-enabled t))
               (rec (start-generation client)))
          (set-result rec :usage inclusive)
          (recorder-end rec)
          (let ((span (genai-only-span client)))
            (check (format nil "~a span declares inclusive semantics" protocol)
                   (equal (genai-attr-string span "gen_ai.token.semantics") "inclusive")))
          (let ((series (%series-named (agento11y-cl::client-metric-registry client)
                                      "gen_ai.client.token.usage")))
            (check (format nil "~a token metrics declare inclusive semantics" protocol)
                   (and series
                        (every (lambda (item)
                                 (equal (%series-attr item "gen_ai.token.semantics") "inclusive"))
                               series)))))))
    (let* ((client (genai-otel-client :capture :full))
           (arguments "{\"city\":\"中ü😀\"}")
           (response " {\"detail\":\"中ü😀\"} ")
           (rec (start-generation client)))
      (set-result rec :output-messages
                  (list (make-message :role :assistant :name "named-assistant"
                                      :parts (list (make-tool-call-part
                                                    :id "tc" :name "weather" :input-json arguments)))
                        (make-message :role :tool :parts
                                      (list (make-tool-result-part :tool-call-id "tc"
                                                                   :content "summary"
                                                                   :content-json response)))))
      (recorder-end rec)
      (let* ((span (genai-only-span client))
             (messages (jzon:parse (genai-attr-string span "gen_ai.output.messages")))
             (call (jget* messages 0 "parts" 0))
             (result (jget* messages 1 "parts" 0)))
        (check "recorder-to-OTel tool arguments retain Unicode"
               (equal (jget (jget call "arguments") "city") "中ü😀"))
        (check "recorder-to-OTel message name survives"
               (equal (jget* messages 0 "name") "named-assistant"))
        (check "recorder-to-OTel independent result JSON retains exact UTF-8 bytes"
               (equal (agento11y-cl::utf8-base64-decode (jget result "agento11y.response_b64")) response))
        (check "recorder-to-OTel independent result text survives"
               (equal (jget result "response") "summary"))))
    (let* ((bytes (make-array 3 :element-type '(unsigned-byte 8)
                                :initial-contents '(255 0 128)))
           (encoded (agento11y-cl::utf8-base64-encode bytes)))
      (multiple-value-bind (raw fallback) (agento11y-cl::otel-json-document encoded)
        (check "OTel tool arguments retain opaque bytes in their base64 extension"
               (and (null raw) (equal fallback encoded)))))
    (dolist (factory '(agento11y-cl::generate-id agento11y-cl::generate-workflow-step-id))
      (let ((ids (loop repeat 10000 collect (funcall factory))))
        (check (format nil "~a has 128 random bits after its prefix" factory)
               (every (lambda (id)
                        (let ((suffix (subseq id (1+ (position #\_ id)))))
                          (and (= (length suffix) 32)
                               (every (lambda (ch) (digit-char-p ch 16)) suffix)))) ids))
        (check (format nil "~a has no collisions in 10000 IDs" factory)
               (= (length (remove-duplicates ids :test #'equal)) 10000))))
    (let* ((secret "glc_abcdefghijklmnopqrstuvwxyz123456")
           (prose (format nil "token: explanation ~a" secret))
           (structured (format nil "{\"password\":\"private-value\",\"secret-id\":\"~a\"}" secret)))
      (dolist (protocol '(:http :otel))
        (dolist (capture '(:full :no-tool-content :metadata-only :full-with-metadata-spans))
          (let* ((client (make-test-client :capture capture :redact-secrets t
                                          :generation-protocol protocol
                                          :experimental-features t
                                          :embedding-capture-input t
                                          :embedding-max-text-length 28))
                 (tool (start-tool-execution client :tool-name "shell"
                                             :tool-description prose
                                             :execution-kind :load-skill
                                             :skill-name "review" :skill-description prose))
                 (embedding (start-embedding client :model-name "embed"))
                 (generation (start-generation client)))
            (set-result tool :arguments structured :result structured :error-message prose)
            (set-result embedding :input-texts (list secret))
            (set-call-error embedding prose)
            (set-call-error generation prose)
            (dolist (rec (list tool embedding generation)) (recorder-end rec))
            (let ((spans (append
                          (agento11y-cl::queue-drain-all (agento11y-cl::client-trace-queue client))
                          (when (eq protocol :otel)
                            (agento11y-cl::queue-drain-all (agento11y-cl::client-generation-queue client))))))
              (check (format nil "~a/~a exported three recorder spans" protocol capture)
                     (= (length spans) 3))
              (check (format nil "~a/~a error, tool and embedding spans contain no secret" protocol capture)
                     (not (search secret (jzon:stringify (coerce spans 'vector)))))
              (let* ((tool-span (first spans))
                     (embedding-span (second spans))
                     (args (genai-attr-string tool-span "gen_ai.tool.call.arguments"))
                     (error-text (jget* tool-span "status" "message")))
                (when (eq capture :full)
                  (check (format nil "~a tool structured arguments use full tier" protocol)
                         (and args (not (search "private-value" args))))
                  (check (format nil "~a error prose uses light tier" protocol)
                         (and error-text (search "token: explanation" error-text)))
                  (check (format nil "~a description prose uses light tier" protocol)
                         (search "token: explanation"
                                 (genai-attr-string tool-span "gen_ai.tool.description")))
                  (let* ((key (if (eq protocol :http) "gen_ai.embeddings.input_texts"
                                  "agento11y.gen_ai.embeddings.input_texts"))
                         (text (jget* (genai-attr embedding-span key)
                                     "arrayValue" "values" 0 "stringValue")))
                    (check (format nil "~a embedding secret is scanned before truncation" protocol)
                           (and text (search "[REDACTED:" text))))))))))
      (let* ((client (make-test-client :capture :full :redact-secrets t))
             (rec (start-generation client :tools
                                    (list (list :name "shell" :description prose
                                                :parameters (jobj "password" "private-value"))))))
        (let* ((payload (agento11y-cl::build-generation-payload
                         rec (agento11y-cl::client-config client)))
               (tool (aref (jget payload "tools") 0)))
          (check "generation tool descriptions use light tier"
                 (and (search "token: explanation" (jget tool "description"))
                      (not (search secret (jget tool "description")))))
          (check "generation tool schemas use full tier"
                 (not (search "private-value" (agento11y-cl::utf8-base64-decode (jget tool "input_schema_json")))))))
      (dolist (protocol '(:http :otel))
        (let* ((client (make-test-client :capture :full :generation-protocol protocol
                                        :experimental-features t :redact-secrets nil))
               (tool (start-tool-execution client :tool-name "shell")))
          (set-result tool :arguments structured :result structured :error-message prose)
          (recorder-end tool)
          (let ((span (genai-only-span client)))
            (check (format nil "~a disabling secret scanning preserves tool arguments" protocol)
                   (search secret (genai-attr-string span "gen_ai.tool.call.arguments")))
            (check (format nil "~a disabling secret scanning preserves error text" protocol)
                   (search secret (jget* span "status" "message")))))))
    (dolist (protocol '(:http :otel))
      (let* ((client (make-test-client :capture :full :generation-protocol protocol
                                      :experimental-features t))
             (trace-id "11111111111111111111111111111111")
             (parent-id "2222222222222222")
             (*trace-context* (list :trace-id trace-id :span-id parent-id))
             (rec (start-tool-execution client :tool-name "delayed"))
             (span-id (agento11y-cl::tool-rec-span-id rec)))
        (let ((*trace-context* (list :trace-id "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                    :span-id "bbbbbbbbbbbbbbbb")))
          (recorder-end rec))
        (let ((span (genai-only-span client)))
          (check (format nil "~a delayed tool retains start trace ID" protocol)
                 (equal (jget span "traceId") trace-id))
          (check (format nil "~a delayed tool retains start parent ID" protocol)
                 (equal (jget span "parentSpanId") parent-id))
          (check (format nil "~a delayed tool retains start span ID" protocol)
                 (equal (jget span "spanId") span-id)))))))


(in-package :agento11y-cl/t)

(defun %transport-regression-client (http-fn &rest options)
  (make-client
   (apply #'make-config
          (append options
                  (list :generation-protocol :otel :experimental-features t
                        :traces-endpoint "http://fake.invalid/v1/traces"
                        :traces-enabled nil :max-retries 1
                        :initial-backoff-sec 0 :max-backoff-sec 0
                        :http-fn http-fn)))
   :env-fn (constantly nil)))

(defun %transport-regression-span ()
  (agento11y-cl::build-span :trace-id "11111111111111111111111111111111"
                          :span-id "2222222222222222" :name "transport-test" :kind 3))

(defun %transport-regression-error (thunk)
  (handler-case (progn (funcall thunk) nil)
    (agento11y-cl:agento11y-export-error (e) e)))

(defun %transport-regression-wait (predicate &optional (timeout 2))
  (let ((deadline (agento11y-cl::monotonic-deadline timeout)))
    (loop until (funcall predicate)
          do (when (zerop (agento11y-cl::deadline-remaining deadline))
               (return-from %transport-regression-wait nil))
             (sleep 0.001))
    t))

(defun %transport-regression-failure-kind (client)
  (getf (getf (first (getf (agento11y-cl::client-export-diagnostics client) :failures))
             :diagnostic) :kind))

(defmacro transport-lifecycle-regression-cases ()
  '(progn
    (flet ((ok-http (&rest args) (declare (ignore args)) (values "{}" 200)))
      (let* ((lock (bt2:make-lock)) (cv (bt2:make-condition-variable))
             (entered nil) (release nil) (finished nil) (flushing nil)
             (flush-done nil) (flush-error nil)
             (client (%transport-regression-client
                      (lambda (&rest args)
                        (declare (ignore args))
                        (bt2:with-lock-held (lock)
                          (setf entered t)
                          (bt2:condition-broadcast cv)
                          (loop until release
                                do (bt2:condition-wait cv lock :timeout 2))
                          (setf finished t))
                        (values "{}" 200))))
             (flusher nil))
        (unwind-protect
             (progn
               (agento11y-cl::enqueue-generation-span client (%transport-regression-span))
               (client-start client)
               (check "worker entered fake HTTP"
                      (%transport-regression-wait
                       (lambda () (bt2:with-lock-held (lock) entered))))
               (check "in-flight generation batch has left queue"
                      (zerop (agento11y-cl::queue-length
                              (agento11y-cl::client-generation-queue client))))
               (setf flusher
                     (bt2:make-thread
                      (lambda ()
                        (bt2:with-lock-held (lock) (setf flushing t))
                        (setf flush-error (%transport-regression-error
                                           (lambda () (client-flush client :timeout-sec 2))))
                        (bt2:with-lock-held (lock) (setf flush-done t)))))
               (%transport-regression-wait (lambda () (bt2:with-lock-held (lock) flushing)))
               (sleep 0.02)
               (check "flush does not pass blocked worker"
                      (bt2:with-lock-held (lock) (not flush-done)))
               (let ((e (%transport-regression-error
                         (lambda () (client-flush client :timeout-sec 0.01)))))
                 (check "in-flight flush deadline is explicit"
                        (and e (getf (agento11y-cl::agento11y-export-error-diagnostics e) :timeout))))
               (bt2:with-lock-held (lock) (setf release t) (bt2:condition-broadcast cv))
               (check "flush completes after HTTP completes"
                      (%transport-regression-wait
                       (lambda () (bt2:with-lock-held (lock) (and finished flush-done)))))
               (check "successful flush keeps NIL result" (null flush-error)))
          (bt2:with-lock-held (lock) (setf release t) (bt2:condition-broadcast cv))
          (when flusher (bt2:join-thread flusher))
          (client-shutdown client)))

      (let* ((lock (bt2:make-lock)) (cv (bt2:make-condition-variable))
             (entered nil) (release nil)
             (client (%transport-regression-client
                      (lambda (&rest args)
                        (declare (ignore args))
                        (bt2:with-lock-held (lock)
                          (setf entered t)
                          (loop until release do (bt2:condition-wait cv lock :timeout 2)))
                        (values "{}" 200)))))
        (unwind-protect
             (progn
               (agento11y-cl::enqueue-generation-span client (%transport-regression-span))
               (client-start client)
               (%transport-regression-wait (lambda () (bt2:with-lock-held (lock) entered)))
               (check "shutdown reports blocked HTTP timeout"
                      (%transport-regression-error (lambda () (client-shutdown client :timeout-sec 0.01))))
               (check "shutdown leaves in-flight worker alive"
                      (bt2:thread-alive-p (agento11y-cl::client-worker-thread client))))
          (bt2:with-lock-held (lock) (setf release t) (bt2:condition-broadcast cv))
          (check "second shutdown can await retained worker" (client-shutdown client :timeout-sec 2))))

      (let* ((calls 0)
             (client (%transport-regression-client
                      (lambda (&rest args) (declare (ignore args)) (incf calls) (values "{}" 200)))))
        (agento11y-cl::enqueue-generation-span client (%transport-regression-span))
        (check "shutdown drains an unstarted client" (client-shutdown client))
        (check "unstarted shutdown exported generation once" (= calls 1)))

      (dolist (body '("[]" "null" "false" "{\"partialSuccess\":false}"
                      "{\"partialSuccess\":[]}" "{\"partialSuccess\":{\"rejectedSpans\":false}}"
                      "{\"partialSuccess\":{\"rejectedSpans\":-1}}"
                      "{\"partialSuccess\":{\"rejectedSpans\":2}}"))
        (let* ((calls 0)
               (client (%transport-regression-client
                        (lambda (&rest args) (declare (ignore args))
                          (incf calls) (values body 200)) :max-retries 4)))
          (agento11y-cl::enqueue-generation-span client (%transport-regression-span))
          (check "invalid OTLP response shapes are explicit export failures"
                 (%transport-regression-error (lambda () (client-flush client))))
          (check "invalid successful responses are not retried" (= calls 1))
          (check "invalid response diagnostics distinguish rejection"
                 (eq (%transport-regression-failure-kind client) :invalid-response))))
      (dolist (body '("{}" "{\"partialSuccess\":{}}" "{\"partialSuccess\":null}"
                      "{\"partialSuccess\":{\"rejectedSpans\":null}}"))
        (check "empty and null OTLP partial-success fields remain successful"
               (null (agento11y-cl::otlp-response-diagnostics body "traces" 1))))
      (check "metric rejection counts measure datapoints rather than metric names"
             (eq (getf (agento11y-cl::otlp-response-diagnostics
                        "{\"partialSuccess\":{\"rejectedDataPoints\":2}}" "metrics" 1) :kind)
                 :partial-success))
      (dolist (body '("{\"partialSuccess\":{\"rejectedSpans\":\"1\",\"errorMessage\":\"secret-response\"}}"
                      "{\"partial_success\":{\"rejected_spans\":1,\"error_message\":\"secret-response\"}}"))
        (let* ((calls 0) (logs nil)
               (client (%transport-regression-client
                        (lambda (&rest args) (declare (ignore args)) (incf calls) (values body 200))
                        :max-retries 4
                        :log-fn (lambda (level component message &rest args)
                                  (declare (ignore level component args)) (push message logs)))))
          (agento11y-cl::enqueue-generation-span client (%transport-regression-span))
          (check "OTLP partial rejection signals flush failure"
                 (%transport-regression-error (lambda () (client-flush client))))
          (check "partially accepted batch is not retried" (= calls 1))
          (check "partial rejection is structured"
                 (eq (%transport-regression-failure-kind client) :partial-success))
          (check "partial rejection remains visible after queue drains"
                 (%transport-regression-error (lambda () (client-flush client))))
          (check "server response text is not logged"
                 (notany (lambda (text) (search "secret-response" text)) logs))))

      (let ((client (%transport-regression-client
                     (lambda (&rest args) (declare (ignore args)) (values "{}" 503)))))
        (agento11y-cl::enqueue-generation-span client (%transport-regression-span))
        (client-start client)
        (%transport-regression-wait
         (lambda () (getf (agento11y-cl::client-export-diagnostics client) :failures)))
        (check "flush reports a batch previously lost by worker"
               (%transport-regression-error (lambda () (client-flush client))))
        (check "shutdown reports sticky worker failure"
               (%transport-regression-error (lambda () (client-shutdown client)))))

      (let* ((client (%transport-regression-client #'ok-http :queue-max 1
                                                   :generation-enqueue-timeout-sec 0.5))
             (queue (agento11y-cl::client-generation-queue client))
             (result nil) (thread nil))
        (agento11y-cl::enqueue-generation-span client :first)
        (setf thread (bt2:make-thread
                      (lambda () (setf result (agento11y-cl::enqueue-generation-span client :second)))))
        (sleep 0.02)
        (check "full generation queue waits for capacity" (bt2:thread-alive-p thread))
        (check "waiting enqueue preserves existing generation"
               (equal (agento11y-cl::queue-drain-batch queue 1) '(:first)))
        (bt2:join-thread thread)
        (check "waiting enqueue succeeds after drain" result)
        (check "waiting generation follows drained generation"
               (equal (agento11y-cl::queue-drain-all queue) '(:second)))
        (check "capacity wait has no loss evidence"
               (null (getf (agento11y-cl::client-export-diagnostics client) :queue-loss))))

      (let* ((client (%transport-regression-client #'ok-http :queue-max 1
                                                   :generation-enqueue-timeout-sec 0.02))
             (queue (agento11y-cl::client-generation-queue client)))
        (agento11y-cl::enqueue-generation-span client :first)
        (check "generation capacity timeout signals export condition"
               (%transport-regression-error
                (lambda () (agento11y-cl::enqueue-generation-span client :second))))
        (check "capacity timeout never evicts queued generation"
               (equal (agento11y-cl::queue-drain-all queue) '(:first)))
        (check "capacity loss evidence survives draining"
               (getf (agento11y-cl::client-export-diagnostics client) :queue-loss))
        (check "later flush reports failed enqueue"
               (%transport-regression-error (lambda () (client-flush client)))))

      (let ((client (%transport-regression-client #'ok-http :queue-max 1
                                                 :generation-enqueue-timeout-sec 0.01)))
        (recorder-end (start-generation client :model-name "first"))
        (check "recorder-end must propagate generation capacity failure"
               (%transport-regression-error
                (lambda () (recorder-end (start-generation client :model-name "second")))))
        (agento11y-cl::queue-drain-all (agento11y-cl::client-generation-queue client)))

      (let ((client (%transport-regression-client #'ok-http :queue-max 1 :trace-queue-max 1)))
        (agento11y-cl::enqueue-generation-span client (%transport-regression-span))
        (dotimes (i 3)
          (agento11y-cl::queue-enqueue (agento11y-cl::client-trace-queue client) i))
        (check "ordinary trace overflow cannot evict generation"
               (= 1 (agento11y-cl::queue-length (agento11y-cl::client-generation-queue client))))
        (check "trace queue eviction is observable"
               (= 2 (getf (first (getf (agento11y-cl::client-export-diagnostics client) :queue-loss)) :dropped))))

      (let* ((client (%transport-regression-client #'ok-http :traces-enabled t))
             (trace "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") (parent "bbbbbbbbbbbbbbbb")
             (rec (let ((*trace-context* (list :trace-id trace :span-id parent)))
                    (start-tool-execution client :tool-name "cross-context"))))
        (let ((*trace-context* (list :trace-id "cccccccccccccccccccccccccccccccc"
                                    :span-id "dddddddddddddddd")))
          (recorder-end rec))
        (let ((span (first (agento11y-cl::queue-drain-all (agento11y-cl::client-trace-queue client)))))
          (check "tool keeps start-time trace across completion context"
                 (string= trace (agento11y-cl::jget span "traceId")))
          (check "tool keeps start-time parent across completion context"
                 (string= parent (agento11y-cl::jget span "parentSpanId")))
          (check "tool exports its start-time span ID"
                 (string= (agento11y-cl::tool-rec-span-id rec) (agento11y-cl::jget span "spanId")))))

      (let* ((client (%transport-regression-client #'ok-http :generation-enabled nil))
             (rec (start-generation client :model-name "disabled")))
        (recorder-end rec)
        (check "explicit false disables OTel generation enqueue"
               (and (zerop (agento11y-cl::queue-length (agento11y-cl::client-generation-queue client)))
                    (zerop (agento11y-cl::queue-length (agento11y-cl::client-trace-queue client))))))
      (let ((client (noop-client)))
        (recorder-end (start-generation client))
        (client-start client)
        (check "noop client never starts worker" (null (agento11y-cl::client-worker-thread client)))
        (check "noop client queues no generation"
               (zerop (agento11y-cl::queue-length (agento11y-cl::client-generation-queue client))))
        (check "noop flush succeeds without HTTP" (null (client-flush client))))

      ;; A custom socket timeout is a retryable transport failure, not proof
      ;; that the SDK's export budget has expired.
      (labels ((custom-config (http-fn &rest options)
                 (apply #'make-config
                        (append options
                                (list :http-fn http-fn :max-retries 3
                                      :initial-backoff-sec 0 :max-backoff-sec 0)))))
        (dolist (label '("generations" "traces" "metrics"))
          (dolist (budget '(nil 2))
            (let* ((agento11y-cl::*export-deadline* (agento11y-cl::monotonic-deadline budget))
                   (calls 0)
                   (config (custom-config
                            (lambda (url &key headers content)
                              (declare (ignore url headers content))
                              (incf calls)
                              (when (= calls 1) (error 'usocket:timeout-error))
                              (values "{}" 200)))))
              (multiple-value-bind (ok body diagnostic)
                  (agento11y-cl::post-with-retry config "http://fake.invalid" "{}" nil label 1)
                (check (format nil "~a custom socket timeout recovers with budget ~s" label budget)
                       (and ok (equal body "{}") (null diagnostic) (= calls 2)))))
            (dolist (max-retries '(0 1 3))
              (let* ((agento11y-cl::*export-deadline* (agento11y-cl::monotonic-deadline budget))
                     (calls 0)
                     (config (custom-config
                              (lambda (&rest args)
                                (declare (ignore args))
                                (incf calls) (error 'usocket:timeout-error))
                              :max-retries max-retries)))
                (multiple-value-bind (ok body diagnostic)
                    (agento11y-cl::post-with-retry config "http://fake.invalid" "{}" nil label 1)
                  (check (format nil "~a custom socket timeout exhausts max-retries ~d with budget ~s"
                                 label max-retries budget)
                         (and (not ok) (null body) (= calls (max 1 max-retries))
                              (eq :timeout (getf diagnostic :kind))
                              (eql calls (getf diagnostic :attempts))))))))
          (let* ((agento11y-cl::*export-deadline* (agento11y-cl::monotonic-deadline 0))
                 (calls 0)
                 (config (custom-config
                          (lambda (&rest args)
                            (declare (ignore args))
                            (incf calls) (error 'usocket:timeout-error)))))
            (multiple-value-bind (ok body diagnostic)
                (agento11y-cl::post-with-retry config "http://fake.invalid" "{}" nil label 1)
              (declare (ignore body))
              (check (format nil "~a expired budget never enters custom callback" label)
                     (and (not ok) (zerop calls) (eq :timeout (getf diagnostic :kind))
                          (eql 0 (getf diagnostic :attempts))))))
          (let* ((agento11y-cl::*export-deadline* (agento11y-cl::monotonic-deadline 2))
                 (calls 0)
                 (config (custom-config
                          (lambda (&rest args)
                            (declare (ignore args))
                            (incf calls)
                            (setf agento11y-cl::*export-deadline*
                                  (agento11y-cl::monotonic-deadline 0))
                            (error 'usocket:timeout-error)))))
            (multiple-value-bind (ok body diagnostic)
                (agento11y-cl::post-with-retry config "http://fake.invalid" "{}" nil label 1)
              (declare (ignore body))
              (check (format nil "~a budget expiring during custom timeout prevents retry" label)
                     (and (not ok) (= calls 1) (eq :timeout (getf diagnostic :kind))
                          (eql 1 (getf diagnostic :attempts))))))
          (let* ((agento11y-cl::*export-deadline* (agento11y-cl::monotonic-deadline 1))
                 (calls 0) (start (get-internal-real-time))
                 (config (custom-config
                          (lambda (&rest args)
                            (declare (ignore args))
                            (incf calls) (error 'usocket:timeout-error))
                          :initial-backoff-sec 2 :max-backoff-sec 2)))
            (multiple-value-bind (ok body diagnostic)
                (agento11y-cl::post-with-retry config "http://fake.invalid" "{}" nil label 1)
              (declare (ignore body))
              (check (format nil "~a custom timeout backoff cannot consume remaining budget" label)
                     (and (not ok) (= calls 1) (eq :timeout (getf diagnostic :kind))
                          (eql 1 (getf diagnostic :attempts))
                          (< (/ (- (get-internal-real-time) start)
                                internal-time-units-per-second) 0.5)))))
          #+sbcl
          (dolist (budget '(nil 2))
            (let* ((agento11y-cl::*export-deadline* (agento11y-cl::monotonic-deadline budget))
                   (calls 0)
                   (config (custom-config
                            (lambda (&rest args)
                              (declare (ignore args))
                              (incf calls) (error 'sb-ext:timeout)))))
              (multiple-value-bind (ok body diagnostic)
                  (agento11y-cl::post-with-retry config "http://fake.invalid" "{}" nil label 1)
                (declare (ignore body))
                (check (format nil "~a SBCL timeout stays terminal with budget ~s" label budget)
                       (and (not ok) (= calls 1) (eq :timeout (getf diagnostic :kind))
                            (eql 1 (getf diagnostic :attempts)))))))))

      (dolist (protocol '(:http :otel))
        (dolist (budget '(nil 2))
          (let* ((calls 0)
                 (client (%transport-regression-client
                          (lambda (&rest args)
                            (declare (ignore args))
                            (incf calls)
                            (when (= calls 1) (error 'usocket:timeout-error))
                            (values "{}" 200))
                          :generation-protocol protocol :generation-enabled t
                          :generation-endpoint "http://fake.invalid/generations"
                          :max-retries 3)))
            (recorder-end (start-generation client :model-name "recovered-timeout"))
            (check (format nil "~a recovery test queued a generation with budget ~s" protocol budget)
                   (= 1 (agento11y-cl::queue-length
                         (agento11y-cl::client-generation-queue client))))
            (check (format nil "~a generation flush recovers custom timeout with budget ~s" protocol budget)
                   (null (%transport-regression-error
                          (lambda () (client-flush client :timeout-sec budget)))))
            (check (format nil "~a recovered generation is exported on second attempt" protocol)
                   (= calls 2))
            (let ((diagnostics (agento11y-cl::client-export-diagnostics client)))
              (check (format nil "~a recovered generation leaves no pending work or sticky failure" protocol)
                     (and (not (agento11y-cl::%client-queues-pending-p client))
                          (null (getf diagnostics :failures))
                          (null (getf diagnostics :queue-loss)))))
            (check (format nil "~a later flush stays successful without re-export" protocol)
                   (and (null (%transport-regression-error (lambda () (client-flush client))))
                        (= calls 2))))))

      (dolist (signaled '(nil t))
        (dolist (status '(302 400 401 429 500 501 502 503 504))
          (let* ((calls 0)
                 (config (make-config
                          :max-retries 2 :initial-backoff-sec 0 :max-backoff-sec 0
                          :http-fn
                          (lambda (&rest args)
                            (declare (ignore args)) (incf calls)
                            (if (= calls 1)
                                (if signaled
                                    (error 'dexador.error:http-request-failed
                                           :status status :headers nil :body "secret" :uri nil :method :post)
                                    (values "secret" status))
                                (values "{}" 200))))))
            (agento11y-cl::post-with-retry config "http://fake.invalid" "{}" nil "traces" 1)
            (check (format nil "OTLP retry classification status ~d signaled ~s" status signaled)
                   (= calls (if (member status '(429 502 503 504)) 2 1))))))
      (let ((calls 0) (logs nil))
        (agento11y-cl::post-with-retry
         (make-config :max-retries 0 :initial-backoff-sec 1 :max-backoff-sec 1
                      :http-fn (lambda (&rest args) (declare (ignore args))
                                 (incf calls) (error "synthetic-secret-token"))
                      :log-fn (lambda (level component text &rest args)
                                (declare (ignore level component args)) (push text logs)))
         "http://user:secret@fake.invalid" "{}" '(("Authorization" . "secret")) "traces" 1)
        (check "zero max-retries still performs one attempt" (= calls 1))
        (check "transport error text and URL are not logged"
               (notany (lambda (text) (search "secret" text)) logs)))
      (let* ((start (get-internal-real-time))
             (config (make-config :max-retries 1 :initial-backoff-sec 1 :max-backoff-sec 1
                                  :http-fn (lambda (&rest args) (declare (ignore args)) (values "{}" 503)))))
        (agento11y-cl::post-with-retry config "http://fake.invalid" "{}" nil "traces" 1)
        (check "no backoff sleep after final attempt"
               (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 0.5)))
      (let* ((calls 0)
             (config (make-config :max-retries 3 :initial-backoff-sec 0 :max-backoff-sec 0
                                  :http-fn (lambda (&rest args) (declare (ignore args))
                                             (incf calls)
                                             (values "{\"partial_success\":{\"rejected_data_points\":\"2\"}}" 200)))))
        (multiple-value-bind (ok body diagnostic)
            (agento11y-cl::post-with-retry config "http://fake.invalid" "{}" nil "metrics" 3)
          (declare (ignore body))
          (check "metrics OTLP rejection is structured without retry"
                 (and (not ok) (= calls 1) (= 2 (getf diagnostic :rejected-count))))))
      (let ((client (%transport-regression-client
                     (lambda (&rest args) (declare (ignore args)) (values "{}" 503))
                     :traces-enabled t)))
        (agento11y-cl::enqueue-generation-span client (%transport-regression-span))
        (agento11y-cl::queue-enqueue (agento11y-cl::client-trace-queue client)
                                    (%transport-regression-span))
        (%transport-regression-error (lambda () (client-flush client)))
        (check "flush aggregates generation and trace export failures"
               (= 2 (length (getf (agento11y-cl::client-export-diagnostics client) :failures)))))
      (let ((calls 0) (start (get-internal-real-time)))
        (agento11y-cl::post-with-retry
         (make-config :max-retries 2 :initial-backoff-sec 0 :max-backoff-sec 0
                      :http-fn (lambda (&rest args) (declare (ignore args))
                                 (incf calls)
                                 (if (= calls 1)
                                     (values "{}" 429 '(("Retry-After" . "1")))
                                     (values "{}" 200))))
         "http://fake.invalid" "{}" nil "traces" 1)
        (check "Retry-After controls the next attempt"
               (and (= calls 2)
                    (>= (/ (- (get-internal-real-time) start) internal-time-units-per-second) 0.99))))
      (let* ((deadline (agento11y-cl::monotonic-deadline 0.02))
             (agento11y-cl::*export-deadline* deadline)
             (calls 0)
             (config (make-config :max-retries 3 :initial-backoff-sec 1 :max-backoff-sec 1
                                  :http-fn (lambda (&rest args) (declare (ignore args))
                                             (incf calls) (values "{}" 503)))))
        (multiple-value-bind (ok body diagnostic)
            (agento11y-cl::post-with-retry config "http://fake.invalid" "{}" nil "traces" 1)
          (declare (ignore body))
          (check "retry delay cannot exceed export deadline"
                 (and (not ok) (= calls 1) (eq (getf diagnostic :kind) :timeout)))))
      (let ((original (symbol-function 'dexador:request)) (redirect-limit nil))
        (unwind-protect
             (progn
               (setf (symbol-function 'dexador:request)
                     (lambda (url &rest options)
                       (declare (ignore url))
                       (setf redirect-limit (getf options :max-redirects))
                       (values "{}" 200)))
               (agento11y-cl::do-http-post (make-config) "http://fake.invalid" nil "{}")
               (check "HTTP exporter refuses credential-forwarding redirects" (eql redirect-limit 0)))
          (setf (symbol-function 'dexador:request) original)))
      (check "Retry-After delay seconds parse"
             (= 3 (agento11y-cl::retry-after-seconds '(("retry-after" . "3")))))
      (check "invalid Retry-After is ignored"
             (null (agento11y-cl::retry-after-seconds '(("Retry-After" . "invalid"))))))))


(in-package :agento11y-cl)

(defmacro config-parity-regression-cases ()
  '(labels ((check (label actual) (agento11y-cl/t::check label actual))
            (resolve (config &rest pairs)
              (resolve-config-from-env config :env-fn
                                       (lambda (name) (cdr (assoc name pairs :test #'equal)))))
            (header (headers name) (cdr (assoc name headers :test #'string-equal)))
            (fails (thunk) (handler-case (progn (funcall thunk) nil) (error () t))))
    (let* ((config (make-config :generation-protocol :http :experimental-features nil
                                :content-capture-mode :no-tool-content :auth-mode :none
                                :redact-secrets nil :redact-input-messages nil
                                :redact-email-addresses nil :debug nil :traces-endpoint nil
                                :generation-enabled nil :extra-headers nil :tags nil
                                :eval-path-prefix nil :use-experimental-otel nil))
           (resolved (resolve config '("AGENTO11Y_PROTOCOL" . "otel")
                              '("AGENTO11Y_ENABLE_EXPERIMENTAL_FEATURES" . "true")
                              '("AGENTO11Y_USE_EXPERIMENTAL_OTEL" . "true")
                              '("AGENTO11Y_CONTENT_CAPTURE_MODE" . "full")
                              '("AGENTO11Y_AUTH_MODE" . "bearer")
                              '("AGENTO11Y_REDACT_SECRETS" . "true")
                              '("AGENTO11Y_REDACT_INPUT_MESSAGES" . "true")
                              '("AGENTO11Y_REDACT_EMAIL_ADDRESSES" . "true")
                              '("AGENTO11Y_DEBUG" . "true")
                              '("AGENTO11Y_TRACES_ENDPOINT" . "https://unexpected.example")
                              '("AGENTO11Y_HEADERS" . "Authorization=secret")
                              '("AGENTO11Y_TAGS" . "tag=env")
                              '("AGENTO11Y_EVAL_PATH_PREFIX" . "/env"))))
      (check "explicit HTTP survives env" (eq :http (config-generation-protocol resolved)))
      (check "explicit experiment gate false survives" (not (config-experimental-features resolved)))
      (check "explicit experiment telemetry false survives" (not (config-use-experimental-otel resolved)))
      (check "explicit default capture survives" (eq :no-tool-content (config-content-capture-mode resolved)))
      (check "explicit auth none survives" (eq :none (config-auth-mode resolved)))
      (check "explicit false master redaction survives" (not (config-redact-secrets resolved)))
      (check "explicit false input redaction survives" (not (config-redact-input-messages resolved)))
      (check "explicit false email redaction survives" (not (config-redact-email-addresses resolved)))
      (check "explicit false debug survives" (not (config-debug resolved)))
      (check "explicit NIL endpoint survives" (null (config-traces-endpoint resolved)))
      (check "explicit NIL headers clear env" (null (config-extra-headers resolved)))
      (check "explicit NIL tags clear env" (null (config-tags resolved)))
      (check "explicit NIL path blocks env, keeps fallback" (equal "/api/v1" (config-eval-path-prefix resolved)))
      (check "defaulted reader supplied predicate includes NIL" (config-eval-path-prefix-supplied-p resolved))
      (dolist (key '(:generation-protocol :experimental-features :redact-secrets :tags :eval-path-prefix))
        (check (format nil "copy retains supplied ~a" key) (config-option-supplied-p resolved key)))
      (let ((twice (resolve resolved '("AGENTO11Y_PROTOCOL" . "otel"))))
        (check "second copy retains explicit HTTP" (eq :http (config-generation-protocol twice)))
        (check "second copy retains explicit NIL" (config-option-supplied-p twice :experimental-features))))
    (let* ((config (make-instance 'agento11y-config :experimental-features nil :generation-protocol :http))
           (resolved (resolve config '("AGENTO11Y_ENABLE_EXPERIMENTAL_FEATURES" . "true"))))
      (check "direct make-instance tracks NIL" (config-option-supplied-p config :experimental-features))
      (check "direct make-instance blocks env" (not (config-experimental-features resolved)))
      (check "omitted options are not supplied" (not (config-option-supplied-p resolved :metrics-enabled))))
    (let* ((config (resolve (make-config) '("AGENTO11Y_PROTOCOL" . "otel")
                            '("AGENTO11Y_ENABLE_EXPERIMENTAL_FEATURES" . "true")
                            '("AGENTO11Y_USE_EXPERIMENTAL_OTEL" . "true")))
           (next (resolve config '("AGENTO11Y_PROTOCOL" . "http")
                          '("AGENTO11Y_ENABLE_EXPERIMENTAL_FEATURES" . "false"))))
      (check "omitted protocol reads env" (eq :otel (config-generation-protocol config)))
      (check "env does not become supplied" (not (config-option-supplied-p config :generation-protocol)))
      (check "second env replaces earlier env" (eq :http (config-generation-protocol next)))
      (check "second env replaces true with false" (not (config-experimental-features next)))
      (check "experiment telemetry resolves independently" (config-use-experimental-otel next)))
    (let ((config (resolve (make-config) '("SIGIL_USE_EXPERIMENTAL_OTEL" . "true"))))
      (check "upstream experiment legacy alias supported" (config-use-experimental-otel config))
      (check "experiment telemetry does not open experiment suite gate" (not (config-experimental-features config)))
      (check "experiment telemetry does not change generation transport" (eq :http (config-generation-protocol config))))
    (check "preferred experimental alias wins before parsing"
           (not (config-use-experimental-otel
                 (resolve (make-config) '("AGENTO11Y_USE_EXPERIMENTAL_OTEL" . "false")
                          '("SIGIL_USE_EXPERIMENTAL_OTEL" . "true")))))
    (let ((config (resolve (make-config) '("OTEL_EXPORTER_OTLP_ENDPOINT" . "https://collector.example/otlp/"))))
      (check "base appends trace path" (equal "https://collector.example/otlp/v1/traces" (config-traces-endpoint config)))
      (check "base appends metric path" (equal "https://collector.example/otlp/v1/metrics" (config-metrics-endpoint config)))
      (check "base does not enable metrics" (not (config-metrics-enabled config)))
      (check "base does not enable ordinary traces" (not (config-traces-enabled config))))
    (let ((config (resolve (make-config)
                           '("OTEL_EXPORTER_OTLP_ENDPOINT" . "https://base.example")
                           '("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT" . "https://trace.example/custom?x=1")
                           '("OTEL_EXPORTER_OTLP_METRICS_ENDPOINT" . "https://metric.example/custom/"))))
      (check "signal trace URL is exact" (equal "https://trace.example/custom?x=1" (config-traces-endpoint config)))
      (check "signal metric URL is exact" (equal "https://metric.example/custom/" (config-metrics-endpoint config)))
      (check "metrics-specific env enables metrics" (config-metrics-enabled config)))
    (let ((config (resolve (make-config)
                           '("AGENTO11Y_OTEL_EXPORTER_OTLP_ENDPOINT" . "https://branded.example/base")
                           '("SIGIL_OTEL_EXPORTER_OTLP_ENDPOINT" . "https://legacy.example")
                           '("OTEL_EXPORTER_OTLP_ENDPOINT" . "https://base.example")
                           '("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT" . "https://signal.example")
                           '("OTEL_EXPORTER_OTLP_METRICS_ENDPOINT" . "https://metric.example"))))
      (check "branded base wins standard base and signal, as upstream"
             (equal "https://branded.example/base/v1/traces" (config-traces-endpoint config)))
      (check "shadowed metric-specific endpoint does not enable metrics" (not (config-metrics-enabled config))))
    (check "legacy branded base supported"
           (equal "https://legacy.example/v1/traces"
                  (config-traces-endpoint
                   (resolve (make-config) '("SIGIL_OTEL_EXPORTER_OTLP_ENDPOINT" . "https://legacy.example/")))))
    (check "branded exact trace URL wins base"
           (equal "https://trace.example/exact"
                  (config-traces-endpoint
                   (resolve (make-config) '("AGENTO11Y_TRACES_ENDPOINT" . "https://trace.example/exact")
                            '("AGENTO11Y_OTEL_EXPORTER_OTLP_ENDPOINT" . "https://base.example")))))
    (check "caller trace URL wins env"
           (equal "https://caller.example/exact"
                  (config-traces-endpoint
                   (resolve (make-config :traces-endpoint "https://caller.example/exact")
                            '("AGENTO11Y_TRACES_ENDPOINT" . "https://env.example")))))
    (check "query stays after appended path"
           (equal "https://collector.example/otlp/v1/traces?token=opaque"
                  (%otlp-signal-endpoint "https://collector.example/otlp/?token=opaque" "traces")))
    (check "explicit false metrics blocks specific env activation"
           (not (config-metrics-enabled
                 (resolve (make-config :metrics-enabled nil)
                          '("OTEL_EXPORTER_OTLP_METRICS_ENDPOINT" . "https://metric.example")))))
    (check "caller metric URL enables metrics"
           (config-metrics-enabled (resolve (make-config :metrics-endpoint "https://metric.example"))))
    (check "explicit NIL metric endpoint blocks env activation"
           (not (config-metrics-enabled
                 (resolve (make-config :metrics-endpoint nil)
                          '("OTEL_EXPORTER_OTLP_METRICS_ENDPOINT" . "https://metric.example")))))
    (check "metric enabled env activates generic base"
           (config-metrics-enabled
            (resolve (make-config) '("AGENTO11Y_METRICS_ENABLED" . "true")
                     '("OTEL_EXPORTER_OTLP_ENDPOINT" . "https://collector.example"))))
    (check "false metric enabled env blocks specific endpoint"
           (not (config-metrics-enabled
                 (resolve (make-config) '("AGENTO11Y_METRICS_ENABLED" . "false")
                          '("OTEL_EXPORTER_OTLP_METRICS_ENDPOINT" . "https://metric.example")))))
    (let ((config (resolve (make-config :auth-mode :bearer :auth-password "api-secret"
                                       :extra-headers '(("X-Api-Secret" . "private")))
                           '("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT" . "https://traces.example")
                           '("OTEL_EXPORTER_OTLP_METRICS_ENDPOINT" . "https://metrics.example")
                           '("OTEL_EXPORTER_OTLP_HEADERS" . "Authorization=Bearer generic,X-Common=yes")
                           '("OTEL_EXPORTER_OTLP_TRACES_HEADERS" . "authorization=Bearer trace")
                           '("OTEL_EXPORTER_OTLP_METRICS_HEADERS" . "Authorization=Bearer metric"))))
      (check "trace headers are independent" (equal "Bearer trace" (header (build-traces-auth-headers config) "Authorization")))
      (check "metric headers are independent" (equal "Bearer metric" (header (build-metrics-auth-headers config) "Authorization")))
      (check "signal headers replace generic whole" (not (header (build-traces-auth-headers config) "X-Common")))
      (check "API extras never enter ambient trace headers" (not (header (build-traces-auth-headers config) "X-Api-Secret"))))
    (let ((config (resolve (make-config :traces-headers nil)
                           '("OTEL_EXPORTER_OTLP_HEADERS" . "Authorization=Bearer generic"))))
      (check "explicit NIL trace headers block generic env" (null (build-traces-auth-headers config)))
      (check "generic headers still apply to metrics" (equal "Bearer generic" (header (build-metrics-auth-headers config) "Authorization"))))
    (let ((config (resolve (make-config :auth-mode :bearer :auth-password "api-secret"
                                       :generation-endpoint "https://api.example/generations"
                                       :traces-endpoint "https://other.example/v1/traces"
                                       :metrics-endpoint "https://other.example/v1/metrics"))))
      (check "cross-origin traces do not inherit generation credentials" (null (build-traces-auth-headers config)))
      (check "cross-origin metrics do not inherit generation credentials" (null (build-metrics-auth-headers config))))
    (let ((config (make-config :auth-mode :bearer :auth-password "api-secret"
                               :generation-endpoint "https://API.example:443/generations"
                               :traces-endpoint "https://api.example/v1/traces"
                               :extra-headers '(("X-Api-Secret" . "private")))))
      (check "same-origin auth inherited" (equal "Bearer api-secret" (header (build-traces-auth-headers config) "Authorization")))
      (check "same-origin API extras still withheld" (not (header (build-traces-auth-headers config) "X-Api-Secret"))))
    (let ((config (make-config :auth-mode :bearer :auth-password "api-secret"
                               :traces-forward-auth t :metrics-forward-auth t
                               :traces-endpoint "https://other.example"
                               :metrics-endpoint "https://other.example"
                               :extra-headers '(("X-Api-Secret" . "private")))))
      (check "explicit forwarding permits cross-origin auth" (equal "Bearer api-secret" (header (build-traces-auth-headers config) "Authorization")))
      (check "explicit forwarding permits API extras" (equal "private" (header (build-traces-auth-headers config) "X-Api-Secret")))
      (check "explicit metrics forwarding works" (equal "Bearer api-secret" (header (build-metrics-auth-headers config) "Authorization"))))
    (let ((config (make-config :traces-forward-auth nil :metrics-forward-auth nil
                               :auth-mode :bearer :auth-password "private"
                               :traces-headers '(("Authorization" . "trace"))
                               :metrics-headers '(("Authorization" . "metric")))))
      (check "false forwarding preserves own trace credentials" (equal "trace" (header (build-traces-auth-headers config) "Authorization")))
      (check "false forwarding preserves own metric credentials" (equal "metric" (header (build-metrics-auth-headers config) "Authorization"))))
    (check "explicit forwarding still lets signal auth win"
           (equal "signal"
                  (header (build-traces-auth-headers
                           (make-config :traces-forward-auth t :auth-mode :bearer :auth-password "api"
                                        :traces-headers '(("authorization" . "signal")))) "Authorization")))
    (let ((config (resolve (make-config) '("OTEL_SERVICE_NAME" . "service-env")
                           '("OTEL_RESOURCE_ATTRIBUTES" . "service.name=resource-env,service.version=1,service.instance.id=instance-env,deployment.environment.name=test"))))
      (check "OTEL_SERVICE_NAME overrides env resource service.name" (equal "service-env" (config-service-name config)))
      (check "resource env supplies version" (equal "1" (config-service-version config)))
      (check "resource env supplies instance id" (equal "instance-env" (config-service-instance-id config)))
      (check "arbitrary resource survives" (equal "test" (cdr (assoc "deployment.environment.name" (config-resource-attributes config) :test #'equal)))))
    (let ((config (resolve (make-config :service-name "caller" :service-instance-id "caller-instance"
                                       :resource-attributes '(("deployment.environment.name" . "caller")))
                           '("OTEL_SERVICE_NAME" . "env")
                           '("OTEL_RESOURCE_ATTRIBUTES" . "service.instance.id=env,deployment.environment.name=env,team=env"))))
      (check "explicit service name wins" (equal "caller" (config-service-name config)))
      (check "explicit instance id wins" (equal "caller-instance" (config-service-instance-id config)))
      (check "caller resource key wins" (equal "caller" (cdr (assoc "deployment.environment.name" (config-resource-attributes config) :test #'equal))))
      (check "additional env resources merge" (assoc "team" (config-resource-attributes config) :test #'equal)))
    (check "explicit resource service name wins env service name"
           (equal "caller-resource"
                  (config-service-name
                   (resolve (make-config :resource-attributes '(("service.name" . "caller-resource")))
                            '("OTEL_SERVICE_NAME" . "env")))))
    (let ((config (make-config :resource-attributes '(("service.name" . "direct")
                                                       ("service.version" . "v-direct")
                                                       ("service.instance.id" . "direct-instance")))))
      (check "direct resource service name is readable" (equal "direct" (config-service-name config)))
      (check "direct resource version is readable" (equal "v-direct" (config-service-version config)))
      (check "direct resource instance identity is readable" (equal "direct-instance" (config-service-instance-id config))))
    (check "explicit NIL resources clear environment"
           (null (config-resource-attributes
                  (resolve (make-config :resource-attributes nil)
                           '("OTEL_RESOURCE_ATTRIBUTES" . "team=env")))))
    (let* ((config (make-config)) (copy (resolve config)) (other (make-config)))
      (check "instance id defaults to random nonempty value" (= 32 (length (config-service-instance-id config))))
      (check "instance identity survives config copy" (equal (config-service-instance-id config) (config-service-instance-id copy)))
      (check "independent producers get different ids" (not (equal (config-service-instance-id config) (config-service-instance-id other))))
      (check "instance id default is not explicit" (not (config-option-supplied-p copy :service-instance-id))))
    (let* ((first (resolve (make-config :tags '(("caller" . "fixed")))
                           '("AGENTO11Y_TAGS" . "dynamic=old")))
           (second (resolve first '("AGENTO11Y_TAGS" . "dynamic=new"))))
      (check "map env values do not become caller values after copy" (equal "new" (cdr (assoc "dynamic" (config-tags second) :test #'equal))))
      (check "caller map values survive repeat resolution" (equal "fixed" (cdr (assoc "caller" (config-tags second) :test #'equal)))))
    (let* ((config (make-config))
           (original (config-service-instance-id config))
           (first (make-client config :env-fn (constantly nil)))
           (second (make-client config :env-fn (constantly nil))))
      (check "independent clients sharing a config receive unique metric producer IDs"
             (not (equal (config-service-instance-id (client-config first))
                         (config-service-instance-id (client-config second)))))
      (check "client construction does not mutate source config identity"
             (equal original (config-service-instance-id config))))
    (dolist (config (list (make-config :service-instance-id "explicit")
                         (make-config :resource-attributes '(("service.instance.id" . "explicit")))))
      (dolist (client (list (make-client config :env-fn (constantly nil))
                            (make-client config :env-fn (constantly nil))))
        (check "caller-selected metric producer identity survives client construction"
               (equal (config-service-instance-id (client-config client)) "explicit"))))
    (let ((client (make-client (make-config) :env-fn
                               (lambda (name)
                                 (when (equal name "OTEL_RESOURCE_ATTRIBUTES")
                                   "service.instance.id=environment")))))
      (check "environment-selected producer identity survives client construction"
             (equal (config-service-instance-id (client-config client)) "environment")))
    (check "enqueue timeout defaults to one second" (= 1.0 (config-generation-enqueue-timeout-sec (make-config))))
    (check "enqueue timeout survives copy" (= 0.25 (config-generation-enqueue-timeout-sec (resolve (make-config :generation-enqueue-timeout-sec 0.25)))))
    (check "HTTP JSON accepted" (eq :http/json (config-traces-protocol (resolve (make-config) '("OTEL_EXPORTER_OTLP_PROTOCOL" . "http/json")))))
    (check "gRPC env rejected" (fails (lambda () (resolve (make-config) '("OTEL_EXPORTER_OTLP_PROTOCOL" . "grpc")))))
    (check "protobuf signal env rejected" (fails (lambda () (resolve (make-config) '("OTEL_EXPORTER_OTLP_TRACES_PROTOCOL" . "http/protobuf")))))
    (check "explicit protocol can override unsupported ambient protocols"
           (eq :http/json (config-traces-protocol
                          (resolve (make-config :traces-protocol :http/json :metrics-protocol :http/json)
                                   '("OTEL_EXPORTER_OTLP_PROTOCOL" . "grpc")))))
    (check "unsupported explicit OTLP protocol rejected" (fails (lambda () (make-config :traces-protocol :grpc))))
))


(in-package :agento11y-cl)

(defun %exp-otel-test-client (&key enabled (traces t) (capture :full) redact
                                 (protocol :http) http-hook)
  (let ((calls (list :calls)))
    (values
     (make-client
      (make-instance 'agento11y-config
                     :eval-endpoint "https://eval.example.test"
                     :generation-endpoint "https://ingest.example.test/api/v1/generations:export"
                     :traces-endpoint "https://traces.example.test/v1/traces"
                     :generation-enabled t :traces-enabled traces
                     :generation-protocol protocol :experimental-features (eq protocol :otel)
                     :use-experimental-otel enabled :content-capture-mode capture
                     :redact-secrets redact :max-retries 1
                     :http-fn
                     (let ((base (agento11y-cl/t::routed-http calls "exp-otel")))
                       (lambda (url &rest args &key method &allow-other-keys)
                         (when http-hook (funcall http-hook url method))
                         (apply base url args))))
      :env-fn (constantly nil))
     calls)))

(defun %exp-otel-test-spans (client calls)
  (append (loop for call in (reverse (cdr calls))
                when (search "traces.example.test" (second call))
                  append (loop for group across (jget* (agento11y-cl/t::payload call)
                                                        "resourceSpans" 0 "scopeSpans")
                               append (coerce (jget group "spans") 'list)))
          (queue-drain-batch (client-trace-queue client) 1000)
          (when (otel-generation-protocol-p (client-config client))
            (queue-drain-batch (client-generation-queue client) 1000))))

(defun %exp-otel-test-attr (object key &optional type)
  (let ((attribute (find key (jget object "attributes")
                         :key (lambda (a) (jget a "key")) :test #'equal)))
    (when attribute
      (if type (jget* attribute "value" type) (jget attribute "value")))))

(defmacro reviewed-experiment-regression-cases ()
  '(progn
    (dolist (enabled '(nil t))
      (multiple-value-bind (client calls) (%exp-otel-test-client :enabled enabled)
        (let ((result
                (handler-case
                    (handler-bind
                        ((error (lambda (condition)
                                  (declare (ignore condition))
                                  (let ((restart (find-restart 'recover)))
                                    (when restart (invoke-restart restart))))))
                      (with-experiment (run client :run-id "exp-otel" :name "restart" :print-url nil)
                        (with-trial (trial run "restart")
                          (with-generation (rec client :model-name "model" :model-provider "provider")
                            (restart-case (error "recoverable") (recover () :recovered))))))
                  (error () :escaped))))
          (check "experiment and trial scopes preserve application restart recovery"
                 (eq result :recovered)))
        (check "recovered errors do not fail completed experiment diagnostics"
               (every (lambda (span) (not (eql (jget* span "status" "code") 2)))
                      (%exp-otel-test-spans client calls)))))
    (multiple-value-bind (client calls) (%exp-otel-test-client :enabled t :capture :full)
      (with-experiment (run client :run-id "exp-otel" :name "capture inheritance" :print-url nil)
        (let ((*trace-context* nil))
          (with-span ((noop-client) "capture only" :content-capture :metadata-only)
            (with-trial (trial run "capture")
              (check "suite identity fallback preserves the active capture restriction"
                     (eq (getf *trace-context* :content-capture-mode) :metadata-only))
              (trial-add-scores trial
                                (list (make-score :evaluator-id "judge" :evaluator-version "1"
                                                  :score-key "final" :value t
                                                  :explanation "PRIVATE EXPLANATION")))))))
      (let* ((spans (%exp-otel-test-spans client calls))
             (trial (find "eval.trial capture" spans :key (lambda (span) (jget span "name"))
                          :test #'equal)))
        (check "identity fallback does not export withheld score explanations"
               (and trial (not (search "PRIVATE EXPLANATION" (jzon:stringify trial)))))))
    (multiple-value-bind (client calls) (%exp-otel-test-client :enabled t)
      (let* ((run (%make-experiment-run :client client :run-id "exp-otel" :name "delayed"
                                        :upload :manual))
             (trial (let ((*trace-context* (list :trace-id "11111111111111111111111111111111"
                                                 :span-id "2222222222222222"
                                                 :trace-flags 0 :trace-state "vendor=origin")))
                      (experiment-run-open-trial run "delayed"))))
        (let ((*trace-context* (list :trace-flags 1 :trace-state "vendor=other")))
          (trial-close trial))
        (let ((span (first (%exp-otel-test-spans client calls))))
          (check "delayed diagnostic completion preserves captured propagation state"
                 (and (eql (jget span "flags") 0)
                      (equal (jget span "traceState") "vendor=origin"))))))
    (multiple-value-bind (client calls) (%exp-otel-test-client :enabled t)
      (let (saved-trial stable-trace stable-span)
        (with-experiment (run client :run-id "exp-otel" :name "conversation link" :print-url nil)
          (with-trial (trial run "correlation")
            (setf saved-trial trial stable-trace (trial-trace-id trial) stable-span (trial-span-id trial))
            (trial-bind-conversation trial "conversation"
                                     :trace-id "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                     :span-id "bbbbbbbbbbbbbbbb")
            (trial-add-scores trial
                              (list (make-score :evaluator-id "judge" :evaluator-version "1"
                                                :score-key "final" :value t)))))
        (check "conversation binding does not replace diagnostic identifiers"
               (and (equal stable-trace (trial-trace-id saved-trial))
                    (equal stable-span (trial-span-id saved-trial))))
        (check "conversation trace attribution is stored separately"
               (and (equal (trial-conversation-trace-id saved-trial) "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                    (equal (trial-conversation-span-id saved-trial) "bbbbbbbbbbbbbbbb")))
        (dolist (call (cdr calls))
          (when (or (search "/trials" (second call))
                    (agento11y-cl/t::score-call-p call))
            (let* ((body (agento11y-cl/t::payload call))
                   (wire (if (agento11y-cl/t::score-call-p call) (aref (jget body "scores") 0) body)))
              (check "trial creation, score and PATCH retain one diagnostic identity"
                     (and (equal (jget wire "trace_id") stable-trace)
                          (equal (jget wire "span_id") stable-span))))))
        (let* ((trial (find "eval.trial correlation" (%exp-otel-test-spans client calls)
                            :key (lambda (span) (jget span "name")) :test #'equal))
               (links (jget trial "links")))
          (check "diagnostic span links to the separately bound conversation"
                 (and (= (length links) 1)
                      (equal (jget* links 0 "traceId") "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                      (equal (jget* links 0 "spanId") "bbbbbbbbbbbbbbbb"))))))
    (dolist (protocol '(:http :otel))
      (multiple-value-bind (client calls)
          (%exp-otel-test-client
           :enabled t :protocol protocol
           :http-hook (lambda (url method)
                        (declare (ignore method))
                        (when (search "traces.example.test" url) (error "diagnostic unavailable"))))
        (with-experiment (run client :run-id "exp-otel" :name "optional traces" :print-url nil)
          (dolist (case '("first" "second"))
            (with-trial (trial run case)
              (trial-add-scores trial (list (make-score :evaluator-id "judge" :evaluator-version "1"
                                                        :score-key "final" :value t))))
            (when (equal case "first")
              (check "explicit flush still reports a diagnostic export failure"
                     (handler-case (progn (client-flush client) nil)
                       (agento11y-export-error () t))))))
        (check "failed optional diagnostics do not block either REST score write"
               (= (count-if #'agento11y-cl/t::score-call-p (cdr calls)) 2))
        (check "generation-only barriers retain optional failure evidence"
               (getf (client-export-diagnostics client) :failures))
        (check "explicit full flush continues to report optional failure"
               (handler-case (progn (client-flush client) nil) (agento11y-export-error () t)))))
    (multiple-value-bind (client calls)
        (%exp-otel-test-client
         :enabled t :protocol :otel
         :http-hook (lambda (url method)
                      (declare (ignore method))
                      (when (search "traces.example.test" url) (error "generation unavailable"))))
      (check "mandatory generation failure still blocks dependent score storage"
             (handler-case
                 (progn
                   (with-experiment (run client :run-id "exp-otel" :name "required generation" :print-url nil)
                     (with-trial (trial run "generation")
                       (with-generation (rec client :model-provider "provider" :model-name "model")
                         (set-result rec :usage (make-token-usage :input 1)))
                       (trial-add-scores trial (list (make-score :evaluator-id "judge" :evaluator-version "1"
                                                                 :score-key "final" :value t)))))
                   nil)
               (error () t)))
      (check "scores never claim a generation that failed its required export"
             (zerop (count-if #'agento11y-cl/t::score-call-p (cdr calls)))))))

(defmacro experiment-otel-regression-cases ()
  '(flet ((check (label value) (agento11y-cl/t::check label value)))
      (reviewed-experiment-regression-cases)
      (dolist (protocol '(:http :otel))
        (multiple-value-bind (client calls) (%exp-otel-test-client :protocol protocol)
          (let ((ambient (child-trace-context (generate-trace-id) (generate-span-id))))
            (let ((*trace-context* ambient))
              (with-experiment (run client :run-id "exp-otel" :name "off" :print-url nil)
                (check "default experiment telemetry is off"
                       (not (experiment-run-use-experimental-otel run)))
                (with-trial (trial run "case")
                  (check "disabled trial preserves ambient context" (eq ambient *trace-context*))
                  (trial-add-scores trial
                                    (list (make-score :evaluator-id "judge" :evaluator-version "1"
                                                      :score-key "final" :value t))))))
            (check "disabled feature emits no diagnostic spans"
                   (null (%exp-otel-test-spans client calls)))
            (let* ((create (find-if (lambda (c) (and (eq :post (first c))
                                                     (search "/trials" (second c)))) (cdr calls)))
                   (score-call (find-if #'agento11y-cl/t::score-call-p (cdr calls)))
                   (score (aref (jget (agento11y-cl/t::payload score-call) "scores") 0)))
              (check "disabled create has no invented trace correlation"
                     (not (nth-value 1 (gethash "trace_id" (agento11y-cl/t::payload create)))))
              (check "disabled HTTP score retains its bool value"
                     (eq t (jget* score "value" "bool")))
              (check "disabled HTTP score has no telemetry-only fields"
                     (not (nth-value 1 (gethash "evaluator_kind" score))))))))
      (multiple-value-bind (client calls) (%exp-otel-test-client :enabled t)
        (let ((root (child-trace-context (generate-trace-id) (generate-span-id)))
              suite-context trial-context saved-trial saved-run)
          (let ((*trace-context* root))
            (check "suite callback preserves multiple return values"
                   (equal '(one two)
                          (multiple-value-list
                           (with-experiment (run client :run-id "exp-otel" :name "on"
                                                 :upload :manual :print-url nil
                                                 :suite (make-test-suite :suite-id "suite" :version "v1"
                                                                         :name "Named suite")
                                                 :dataset '(:id "refs" :version "v2"))
                             (setf suite-context (copy-list *trace-context*) saved-run run)
                             (with-trial (trial run (make-test-case :id "case" :name "Named case"))
                               (setf trial-context (copy-list *trace-context*) saved-trial trial)
                               (trial-bind-conversation trial "conversation")
                               (with-generation (rec client :model-name "model" :model-provider "provider")
                                 (check "generation is an automatic child of the trial"
                                        (equal (gen-rec-parent-span-id rec) (trial-span-id trial))))
                               (trial-add-scores
                                trial
                                (list (make-score :evaluator-id "judge" :evaluator-version "3"
                                                  :evaluator-kind "llm_judge" :generation-id "gen-result"
                                                  :score-key "numeric" :value 0.5 :passed t
                                                  :explanation "ordinary explanation")
                                      (make-score :evaluator-id "judge" :evaluator-version "3"
                                                  :score-key "bool" :value nil :passed nil)
                                      (make-score :evaluator-id "judge" :evaluator-version "3"
                                                  :score-key "category" :value "good")))
                               (trial-close trial :usage (make-token-usage :input 9 :output 4)))
                             (values 'one 'two)))))
            (check "suite context is restored" (eq root *trace-context*)))
          (trial-close saved-trial)
          (experiment-run-publish saved-run)
          (let* ((score-call (find-if #'agento11y-cl/t::score-call-p (cdr calls)))
                 (score (aref (jget (agento11y-cl/t::payload score-call) "scores") 0)))
            (check "HTTP scores correlate to the same diagnostic trial"
                   (and (equal (jget score "trace_id") (trial-trace-id saved-trial))
                        (equal (jget score "span_id") (trial-span-id saved-trial)))))
          (let* ((spans (%exp-otel-test-spans client calls))
                 (suite (find "test_suite_run" spans :key (lambda (s) (jget s "name")) :test #'equal))
                 (trial (find "eval.trial case" spans :key (lambda (s) (jget s "name")) :test #'equal))
                 (events (jget trial "events"))
                 (create (find-if (lambda (c) (and (eq :post (first c))
                                                  (search "/trials" (second c)))) (cdr calls)))
                 (patch (find-if (lambda (c) (and (eq :patch (first c))
                                                 (search "/trials/" (second c)))) (cdr calls))))
            (check "one suite and one trial survive repeated close"
                   (= 2 (count-if (lambda (span)
                                    (%exp-otel-test-attr span "agento11y.eval.schema.version")) spans)))
            (check "suite is a child of ambient span"
                   (equal (jget suite "parentSpanId") (getf root :span-id)))
            (check "trial is a child of suite span"
                   (equal (jget trial "parentSpanId") (getf suite-context :span-id)))
            (check "trial context identifies exported span"
                   (equal (jget trial "spanId") (getf trial-context :span-id)))
            (check "schema is versioned"
                   (equal "experiments-otel-2026-06"
                          (%exp-otel-test-attr trial "agento11y.eval.schema.version" "stringValue")))
            (check "suite identity includes supplied suite name"
                   (equal "Named suite" (%exp-otel-test-attr suite "test.suite.name" "stringValue")))
            (check "case name and attempt are typed"
                   (and (equal "Named case" (%exp-otel-test-attr trial "test.case.name" "stringValue"))
                        (equal "1" (%exp-otel-test-attr trial "test.case.run.attempt" "intValue"))))
            (check "diagnostic span explicitly refuses generation materialization"
                   (and (%exp-otel-test-attr trial "agento11y.record")
                        (not (%exp-otel-test-attr trial "agento11y.record" "boolValue"))))
            (check "create correlates to diagnostic trial"
                   (equal (jget trial "spanId") (jget (agento11y-cl/t::payload create) "span_id")))
            (check "finalize preserves trace correlation"
                   (equal (jget trial "traceId") (jget (agento11y-cl/t::payload patch) "trace_id")))
            (check "duration is measured and usage forwarded without invented cost"
                   (let ((body (agento11y-cl/t::payload patch)))
                     (and (integerp (jget body "duration_ms")) (>= (jget body "duration_ms") 0)
                          (= 9 (jget body "input_tokens")) (= 4 (jget body "output_tokens"))
                          (not (nth-value 1 (gethash "cost" body))))))
            (check "one result event per buffered score" (= 3 (length events)))
            (check "numeric score uses double and pass label"
                   (and (= 0.5 (%exp-otel-test-attr (aref events 0) "gen_ai.evaluation.score.value" "doubleValue"))
                        (equal "pass" (%exp-otel-test-attr (aref events 0) "gen_ai.evaluation.score.label" "stringValue"))))
            (check "false boolean score uses double zero and fail label"
                   (and (= 0 (%exp-otel-test-attr (aref events 1) "gen_ai.evaluation.score.value" "doubleValue"))
                        (equal "fail" (%exp-otel-test-attr (aref events 1) "gen_ai.evaluation.score.label" "stringValue"))))
            (check "string score is a label, not numeric"
                   (and (equal "good" (%exp-otel-test-attr (aref events 2) "gen_ai.evaluation.score.label" "stringValue"))
                        (not (%exp-otel-test-attr (aref events 2) "gen_ai.evaluation.score.value"))))
            (check "event carries evaluator, reference set and generation provenance"
                   (and (equal "llm_judge" (%exp-otel-test-attr (aref events 0) "gen_ai.evaluation.evaluator.type" "stringValue"))
                        (equal "refs" (%exp-otel-test-attr (aref events 0) "gen_ai.evaluation.reference_set.id" "stringValue"))
                        (equal "gen-result" (%exp-otel-test-attr (aref events 0) "gen_ai.response.id" "stringValue")))))))
      (dolist (mode '(:full :metadata-only :full-with-metadata-spans))
        (multiple-value-bind (client calls) (%exp-otel-test-client :enabled t :capture mode :redact t)
          (with-experiment (run client :run-id "exp-otel" :name "privacy" :print-url nil)
            (with-trial (trial run "privacy")
              (trial-add-scores trial (list (make-score :evaluator-id "judge" :evaluator-version "1"
                                                        :evaluator-kind "custom"
                                                        :score-key "final" :value 1
                                                        :explanation "email alice@example.com")))))
          (let* ((trial (find "eval.trial privacy" (%exp-otel-test-spans client calls)
                              :key (lambda (s) (jget s "name")) :test #'equal))
                 (event (aref (jget trial "events") 0))
                 (explanation (%exp-otel-test-attr event "gen_ai.evaluation.explanation" "stringValue"))
                 (http (find-if #'agento11y-cl/t::score-call-p (cdr calls)))
                 (score (aref (jget (agento11y-cl/t::payload http) "scores") 0)))
            (check "explanation follows capture and secret redaction"
                   (if (eq mode :full)
                       (and explanation (not (search "alice@example.com" explanation)))
                       (null explanation)))
            (check "HTTP explanation is unchanged by telemetry policy"
                   (equal "email alice@example.com" (jget score "explanation")))
            (check "evaluator_kind never changes HTTP score body"
                   (not (nth-value 1 (gethash "evaluator_kind" score)))))))
      (dolist (case '(:explicit-false :traces-disabled))
        (multiple-value-bind (client calls) (%exp-otel-test-client :enabled t :traces (eq case :explicit-false))
          (with-experiment (run client :run-id "exp-otel" :name "off" :print-url nil
                                :use-experimental-otel (not (eq case :explicit-false)))
            (with-trial (trial run "case") (declare (ignore trial))))
          (check "per-run false or normal traces disabled prevents diagnostics"
                 (null (%exp-otel-test-spans client calls)))))
      (multiple-value-bind (client calls) (%exp-otel-test-client)
        (with-experiment (run client :run-id "exp-otel" :name "explicit true"
                              :use-experimental-otel t :print-url nil)
          (with-trial (trial run "true") (declare (ignore trial))))
        (check "per-run explicit true overrides disabled client default"
               (= 2 (length (%exp-otel-test-spans client calls)))))
      (multiple-value-bind (client calls) (%exp-otel-test-client :enabled t)
        (let ((original (make-condition 'simple-error :format-control "original error")))
          (check "trial and suite teardown preserve error identity"
                 (handler-case
                     (with-experiment (run client :run-id "exp-otel" :name "error" :print-url nil)
                       (with-trial (trial run "error") (declare (ignore trial)) (error original)))
                   (error (e) (eq e original))))
          (let* ((spans (%exp-otel-test-spans client calls))
                 (trial (find "eval.trial error" spans :key (lambda (s) (jget s "name")) :test #'equal)))
            (check "escaping error marks diagnostic trial failed" (= 2 (jget* trial "status" "code"))))))
      (multiple-value-bind (client calls) (%exp-otel-test-client :enabled t :protocol :otel)
        (with-experiment (run client :run-id "exp-otel" :name "otel generations"
                              :upload :manual :print-url nil)
          (with-trial (trial run "otel")
            (declare (ignore trial))
            (with-generation (rec client :model-name "model" :model-provider "provider")
              (set-result rec :usage (make-token-usage :input 2 :output 1)))))
        (check "OTel mode never posts a native generation payload"
               (notany (lambda (call) (search "generations:export" (second call))) (cdr calls)))
        (let ((spans (%exp-otel-test-spans client calls)))
          (check "OTel generation export remains alongside two diagnostic spans"
                 (and (= 3 (length spans))
                      (= 2 (count-if (lambda (span)
                                       (%exp-otel-test-attr span "agento11y.eval.schema.version"))
                                     spans))))))
      (let ((fail-p t))
        (multiple-value-bind (client calls)
            (%exp-otel-test-client
             :enabled t
             :http-hook (lambda (url method)
                          (when (and fail-p (eq method :patch) (search "/trials/" url))
                            (error "mock trial PATCH failed"))))
          (let* ((run (%make-experiment-run :client client :run-id "exp-otel" :name "retry"
                                            :upload :manual))
                 (trial (experiment-run-open-trial run "retry")))
            (trial-add-scores trial (list (make-score :evaluator-id "judge" :evaluator-version "1"
                                                      :score-key "final" :value t)))
            (check "failed PATCH is retryable" (handler-case (progn (trial-close trial) nil)
                                                 (error () (not (trial-closed-p trial)))))
            (setf fail-p nil)
            (trial-close trial)
            (trial-close trial)
            (let ((spans (%exp-otel-test-spans client calls)))
              (check "failed PATCH and retry produce one diagnostic span and event"
                     (and (= 1 (length spans)) (= 1 (length (jget (first spans) "events")))))
              (check "failed PATCH marks diagnostics as error"
                     (= 2 (jget* (first spans) "status" "code")))
              (check "true boolean scores use double one"
                     (= 1 (%exp-otel-test-attr (aref (jget (first spans) "events") 0)
                                               "gen_ai.evaluation.score.value" "doubleValue")))))))
      (multiple-value-bind (client calls) (%exp-otel-test-client :enabled t)
        (declare (ignore calls))
        (let ((original (make-condition 'simple-error :format-control "keep me")))
          (check "telemetry failure cannot replace callback error"
                 (handler-case
                     (with-experiment (run client :run-id "exp-otel" :name "broken telemetry" :print-url nil)
                       (with-trial (trial run "broken")
                         (setf (%experiment-otel-state-attributes (trial-otel-state trial)) :broken)
                         (error original)))
                   (error (e) (eq e original))))))))
