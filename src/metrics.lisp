(in-package :agento11y-cl)

;;; ================================================================
;;; In-memory histogram registry + OTLP metrics export
;;;
;;; The reference SDK gets metrics for free by delegating to the
;;; OpenTelemetry SDK Meter. Common Lisp has no comparable OTel SDK, so this
;;; module hand-rolls the same four GenAI histograms: cumulative in-memory
;;; aggregation plus an OTLP `resourceMetrics` push path. Metric names, units,
;;; buckets, and attributes match the reference wire output; the mechanism
;;; differs by necessity.
;;; ================================================================

(defstruct hist-state
  "One histogram series: its bounds, per-bucket counts, running sum/count, and
the identifying attribute alist. Cumulative — never reset after creation."
  name
  unit
  bounds
  counts
  (count 0)
  (sum 0d0)
  attrs
  start-nano)

(defstruct metric-registry
  "Lock-protected collection of histogram series keyed by (name unit . attrs).
START-NANO is fixed at creation; cumulative series report it as
startTimeUnixNano on every export."
  (table (make-hash-table :test 'equal))
  (lock (bt2:make-lock :name "agento11y-metrics"))
  (start-nano (current-unix-nano)))

(defun metric-identity-attrs (config provider model agent-name agent-version)
  "Build the provider/model/agent identity attribute alist shared by all four
metrics. Provider, model, and agent name are always present (empty string when
unset); agent version only when non-empty. Client-level config tags are
appended as agento11y.tag.* attributes. Mirrors the reference SDK."
  (flet ((trimmed (v) (if (stringp v) (string-trim '(#\Space #\Tab #\Newline #\Return) v) "")))
    (let ((attrs (list (cons "gen_ai.provider.name" (trimmed provider))
                       (cons "gen_ai.request.model" (trimmed model))
                       (cons "gen_ai.agent.name" (trimmed agent-name)))))
      (let ((ver (trimmed agent-version)))
        (when (plusp (length ver))
          (setf attrs (append attrs (list (cons "gen_ai.agent.version" ver))))))
      (setf attrs (append attrs (prefixed-tag-pairs (config-tags config))))
      attrs)))

(defparameter +genai-token-usage-unit+ "{token}"
  "Unit of gen_ai.client.token.usage as the registry spells it. The SDK's own
native metrics keep the plain \"token\" spelling they shipped with. A client is
on one path or the other, never both: a repeated metric name under two units is
a conflict for a collector, so every otel-mode recorder that reports tokens
uses this one.")

(defun record-histogram (registry metric-name unit bounds attrs-alist value)
  "Find or create the series for (METRIC-NAME, UNIT, ATTRS-ALIST) and add VALUE.
Bumps count and sum, and increments the bucket for the first bound >= VALUE
(the overflow bucket when VALUE exceeds every bound)."
  (bt2:with-lock-held ((metric-registry-lock registry))
    (let* ((sorted-attrs (sort (copy-alist attrs-alist) #'string< :key #'car))
           (key (list metric-name unit sorted-attrs))
           (st (or (gethash key (metric-registry-table registry))
                   (setf (gethash key (metric-registry-table registry))
                         (make-hist-state
                          :name metric-name
                          :unit unit
                          :bounds bounds
                          :counts (make-list (1+ (length bounds)) :initial-element 0)
                          :attrs sorted-attrs
                          :start-nano (metric-registry-start-nano registry)))))
           (v (coerce value 'double-float)))
      (incf (hist-state-count st))
      (incf (hist-state-sum st) v)
      (let ((i (position-if (lambda (b) (<= v (coerce b 'double-float))) bounds)))
        (incf (nth (or i (length bounds)) (hist-state-counts st))))
      st)))

;;; record-builtin-metrics dispatches per recorder type. The recorder classes
;;; do not exist when this file loads, so the methods live in recorder.lisp.
(defgeneric record-builtin-metrics (recorder registry config)
  (:documentation "Record this recorder's built-in histograms into REGISTRY."))

(defun %series-attrs->otlp (attrs-alist)
  "Convert a string-keyed attribute alist into a vector of OTLP attributes."
  (coerce (mapcar (lambda (kv) (otel-string-attr (car kv) (cdr kv))) attrs-alist)
          'vector))

(defun export-metrics (config registry auth-headers)
  "Snapshot every series, group by metric name+unit, build an OTLP
resourceMetrics payload, and POST it. Cumulative: series are never reset.
The snapshot copies each series' mutable state (counts/count/sum) while holding
the lock, so a concurrent record-histogram cannot tear a datapoint."
  (let ((groups (make-hash-table :test 'equal))   ; (name . unit) -> list of snapshot plists
        (now-nano (current-unix-nano)))
    (bt2:with-lock-held ((metric-registry-lock registry))
      (maphash (lambda (key st)
                 (declare (ignore key))
                 (let ((gk (cons (hist-state-name st) (hist-state-unit st)))
                       (snap (list :attrs (hist-state-attrs st)
                                   :bounds (hist-state-bounds st)
                                   :counts (copy-list (hist-state-counts st))
                                   :count (hist-state-count st)
                                   :sum (hist-state-sum st)
                                   :start-nano (hist-state-start-nano st))))
                   (push snap (gethash gk groups))))
               (metric-registry-table registry)))
    (when (zerop (hash-table-count groups))
      (return-from export-metrics t))
    (let ((metrics nil))
      (maphash
       (lambda (gk snaps)
         (let ((datapoints
                 (mapcar (lambda (snap)
                           (build-histogram-datapoint
                            (%series-attrs->otlp (getf snap :attrs))
                            (getf snap :bounds)
                            (getf snap :counts)
                            (getf snap :sum)
                            (getf snap :count)
                            (getf snap :start-nano)
                            now-nano))
                         snaps)))
           (push (build-otlp-metric (car gk) (cdr gk) datapoints) metrics)))
       groups)
      (let ((payload (jzon:stringify
                      (build-otlp-metrics-payload metrics
                                                  (config-service-name config)
                                                  (config-service-version config)))))
        (post-with-retry config (config-metrics-endpoint config) payload
                         auth-headers "metrics" (length metrics))))))
