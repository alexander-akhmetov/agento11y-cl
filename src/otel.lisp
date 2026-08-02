(in-package :agento11y-cl)

(defvar +sdk-name+ "agento11y-cl")

;;; --- Attribute helpers ---

(defun otel-string-attr (key value)
  "Build an OTLP string attribute."
  (jobj "key" key "value" (jobj "stringValue" value)))

(defun otel-int-attr (key value)
  "Build an OTLP int attribute (value as string per OTLP JSON)."
  (jobj "key" key "value" (jobj "intValue" (format nil "~d" value))))

(defun otel-bool-attr (key value)
  "Build an OTLP bool attribute."
  (jobj "key" key "value" (jobj "boolValue" (if value t nil))))

(defun otel-string-array-attr (key values)
  "Build an OTLP string array attribute."
  (jobj "key" key
        "value" (jobj "arrayValue"
                      (jobj "values" (coerce (mapcar (lambda (v)
                                                       (jobj "stringValue" v))
                                                     values)
                                             'vector)))))

;;; --- Span building ---

(defun build-span (&key trace-id span-id parent-span-id name kind
                        start-time-unix-nano end-time-unix-nano
                        attributes status-code status-message)
  "Build an OTLP-compatible span JSON object.
KIND: 1=INTERNAL, 3=CLIENT. STATUS-CODE: 1=OK, 2=ERROR."
  (let ((span (jobj "traceId" trace-id
                     "spanId" span-id
                     "name" name
                     "kind" kind
                     "startTimeUnixNano" (or start-time-unix-nano "0")
                     "endTimeUnixNano" (or end-time-unix-nano "0")
                     "attributes" (or attributes (vector))
                     "status" (jobj "code" (or status-code 1)
                                    "message" (or status-message "")))))
    (when parent-span-id
      (setf (gethash "parentSpanId" span) parent-span-id))
    span))

(defun build-otlp-payload (spans service-name service-version)
  "Wrap spans in the OTLP resourceSpans envelope."
  (let ((resource-attrs (list (otel-string-attr "service.name" (or service-name "unknown")))))
    (when service-version
      (push (otel-string-attr "service.version" service-version) resource-attrs))
    (jobj "resourceSpans"
          (vector
           (jobj "resource" (jobj "attributes" (coerce (nreverse resource-attrs) 'vector))
                 "scopeSpans"
                 (vector
                  (jobj "scope" (jobj "name" +sdk-name+)
                        "spans" (coerce spans 'vector))))))))

;;; --- Metric histogram buckets ---
;;; Duration and token bucket boundaries match the current OTel GenAI semantic
;;; conventions (https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-metrics/)
;;; and the reference agento11y wire output. The older [0.001..10.0] duration set
;;; shown on some sources is superseded; do not revert to it.

(defparameter +duration-buckets+
  #(0.01d0 0.02d0 0.04d0 0.08d0 0.16d0 0.32d0 0.64d0 1.28d0 2.56d0
    5.12d0 10.24d0 20.48d0 40.96d0 81.92d0)
  "Explicit bounds for duration histograms (seconds), per OTel GenAI semconv.")

(defparameter +token-buckets+
  #(1 4 16 64 256 1024 4096 16384 65536 262144 1048576 4194304 16777216 67108864)
  "Explicit bounds for token-usage histograms, per OTel GenAI semconv.")

;; tool_calls_per_operation is not in the OTel spec; no spec bucket advice exists.
;; The reference relies on OTel's default [0..10000] set, which is wrongly scaled
;; for tool counts. This small set is a deliberate divergence chosen for correctness.
(defparameter +tool-call-buckets+ #(0 1 2 4 8 16 32 64)
  "Explicit bounds for the non-semconv tool_calls_per_operation histogram.")

(defparameter +temporality-cumulative+ 2
  "OTLP AggregationTemporality: 1=DELTA, 2=CUMULATIVE.")

;;; --- Metric OTLP shape ---

(defun build-histogram-datapoint (attrs bounds counts sum total start-nano now-nano)
  "Build one OTLP histogram dataPoint JSON object.
ATTRS is a vector of OTLP attribute objects. BOUNDS is the explicit-bounds
sequence. COUNTS is the per-bucket count list (length = bounds + 1). SUM is the
running total value, TOTAL the number of recorded values. COUNT/bucketCounts are
encoded as uint64 strings (OTLP JSON convention); sum stays a JSON number."
  (jobj "attributes" (coerce attrs 'vector)
        "startTimeUnixNano" start-nano
        "timeUnixNano" now-nano
        "count" (format nil "~d" total)
        "sum" (coerce sum 'double-float)
        "bucketCounts" (coerce (mapcar (lambda (c) (format nil "~d" c)) counts) 'vector)
        "explicitBounds" (coerce (map 'list (lambda (b) (coerce b 'double-float)) bounds)
                                 'vector)))

(defun build-otlp-metric (name unit datapoints)
  "Build one OTLP metric object wrapping histogram DATAPOINTS."
  (jobj "name" name
        "unit" unit
        "histogram" (jobj "aggregationTemporality" +temporality-cumulative+
                          "dataPoints" (coerce datapoints 'vector))))

(defun build-otlp-metrics-payload (metrics service-name service-version)
  "Wrap METRICS in the OTLP resourceMetrics envelope (scope name +sdk-name+)."
  (let ((resource-attrs (list (otel-string-attr "service.name" (or service-name "unknown")))))
    (when service-version
      (push (otel-string-attr "service.version" service-version) resource-attrs))
    (jobj "resourceMetrics"
          (vector
           (jobj "resource" (jobj "attributes" (coerce (nreverse resource-attrs) 'vector))
                 "scopeMetrics"
                 (vector
                  (jobj "scope" (jobj "name" +sdk-name+)
                        "metrics" (coerce metrics 'vector))))))))

;;; --- Error classification ---

(defun extract-http-status (error-string)
  "Extract HTTP status code from an error string. Returns integer or NIL."
  (when (stringp error-string)
    (let ((pos (search "status=" error-string)))
      (when pos
        (let ((start (+ pos 7)))
          (when (< (+ start 2) (length error-string))
            (handler-case (parse-integer (subseq error-string start
                                                  (min (+ start 3) (length error-string)))
                                          :junk-allowed t)
              (error () nil))))))))

(defun classify-error (error-string)
  "Classify an error string into an SDK error category.
The contract is a string. A caller passing a condition object or any other
value gets it printed rather than an error: callers include payload and span
builders, where signalling here would drop the whole record."
  (when (null error-string) (return-from classify-error nil))
  (unless (stringp error-string)
    (setf error-string (princ-to-string error-string)))
  (let ((status (extract-http-status error-string))
        (lower (string-downcase error-string)))
    (cond
      ((and status (= status 429)) "rate_limit")
      ((and status (or (= status 401) (= status 403))) "auth_error")
      ((and status (>= status 500)) "server_error")
      ((and status (= status 408)) "timeout")
      ((and status (>= status 400)) "client_error")
      ((or (search "timeout" lower) (search "timed out" lower)) "timeout")
      ((search "retry attempts exhausted" lower) "server_error")
      (t "sdk_error"))))

(defun redacted-error-text (error-string)
  "Error text to export when capture mode withholds the provider's message.
Returns the classified category so consumers keep the classification."
  (or (classify-error error-string) "sdk_error"))

;;; --- Common span attributes ---

(defun prefixed-tag-pairs (tags)
  "Turn a tags alist into (\"agento11y.tag.<key>\" . value) pairs, skipping
non-string entries. Mirrors the reference SDK's agento11y.tag.* promotion."
  (loop for pair in tags
        when (and (consp pair) (stringp (car pair)) (stringp (cdr pair)))
          collect (cons (concatenate 'string "agento11y.tag." (car pair)) (cdr pair))))

(defun common-span-attrs (config &key provider model agent-name agent-version
                                      conversation-id)
  "Build list of common OTLP attributes for Agent Observability spans."
  (let ((attrs nil))
    (push (otel-string-attr "agento11y.sdk.name" +sdk-name+) attrs)
    (when (and conversation-id (stringp conversation-id) (plusp (length conversation-id)))
      (push (otel-string-attr "gen_ai.conversation.id" conversation-id) attrs))
    (when (and agent-name (plusp (length agent-name)))
      (push (otel-string-attr "gen_ai.agent.name" agent-name) attrs))
    (when (and agent-version (plusp (length agent-version)))
      (push (otel-string-attr "gen_ai.agent.version" agent-version) attrs))
    (when (and provider (plusp (length provider)))
      (push (otel-string-attr "gen_ai.provider.name" provider) attrs))
    (when (and model (plusp (length model)))
      (push (otel-string-attr "gen_ai.request.model" model) attrs))
    (let ((uid (config-user-id config)))
      (when uid
        (let ((uid-str (princ-to-string uid)))
          (when (plusp (length uid-str))
            (push (otel-string-attr "user.id" uid-str) attrs)))))
    (dolist (kv (prefixed-tag-pairs (config-tags config)))
      (push (otel-string-attr (car kv) (cdr kv)) attrs))
    attrs))
