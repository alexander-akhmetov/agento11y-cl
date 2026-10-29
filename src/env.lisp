(in-package :agento11y-cl)

;;; Canonical AGENTO11Y_* environment variable resolution.
;;;
;;; Mirrors the env schema shipped in the canonical agento11y SDKs
;;; (Go: go/agento11y/env.go, Python: python/agento11y/config.py).
;;; Resolution precedence: explicit caller config > env var > schema defaults.
;;;
;;; Branded variables have a preferred AGENTO11Y_<SUFFIX> name and a legacy
;;; SIGIL_<SUFFIX> fallback. Selection happens before parsing, so a nonblank
;;; preferred value wins even when it later fails validation and stale legacy
;;; config cannot silently resurface. This matches envPair/envTrimmed in
;;; go/agento11y/env.go.

;;; --- String helpers ---

(defun %trim (s)
  (string-trim '(#\Space #\Tab #\Newline #\Return) s))

(defun env-trimmed (env-fn name)
  "Read NAME via ENV-FN, trim whitespace. Return nil for missing or empty."
  (let ((raw (funcall env-fn name)))
    (when (and raw (stringp raw))
      (let ((trimmed (%trim raw)))
        (unless (zerop (length trimmed))
          trimmed)))))

(defun env-branded* (env-fn suffix)
  "Read the AGENTO11Y_<SUFFIX> variable, falling back to SIGIL_<SUFFIX>.
Returns (VALUE . NAME) for the first nonblank value, so a warning can name the
spelling the caller actually set. Returns (NIL . NIL) when neither is set. The
two spellings are never merged; the selected value is used whole."
  (loop for name in (list (concatenate 'string "AGENTO11Y_" suffix)
                          (concatenate 'string "SIGIL_" suffix))
        for value = (env-trimmed env-fn name)
        when value return (cons value name)
        finally (return (cons nil nil))))

(defun env-branded (env-fn suffix)
  "Value half of ENV-BRANDED*, for the vars whose warnings never name them."
  (car (env-branded* env-fn suffix)))

(defun parse-bool (s)
  "Map common truthy strings (\"1\", \"true\", \"yes\", \"on\", case-insensitive) to T,
falsy strings to NIL. Unknown values return NIL."
  (when (stringp s)
    (let ((lower (string-downcase (%trim s))))
      (cond
        ((member lower '("1" "true" "yes" "on" "t") :test #'string=) t)
        (t nil)))))

(defun parse-strict-bool (s config var-name)
  "Strictly map a bool env value: 1/true/yes/on/t -> T, 0/false/no/off/nil -> NIL
(case-insensitive). Any other value logs a warning via agento11y-log and returns
:invalid so a typo can't silently flip behavior. The warning never includes the
value: these vars gate redaction, and a value pasted there by mistake could be a
secret."
  (let ((normalized (string-downcase (%trim s))))
    (cond
      ((member normalized '("1" "true" "yes" "on" "t") :test #'string=) t)
      ((member normalized '("0" "false" "no" "off" "nil") :test #'string=) nil)
      (t
       (agento11y-log config :warn "env"
                  (format nil "ignoring ~a (unsupported value)" var-name))
       :invalid))))

(defun parse-csv-kv (s)
  "Parse \"k=v,k2=v2\" into an alist. Trims whitespace around keys/values.
Skips empty entries and entries with no '='. Returns a fresh list in source order."
  (let ((result nil))
    (when (and s (stringp s))
      (let ((start 0)
            (len (length s)))
        (loop while (< start len)
              for end = (or (position #\, s :start start) len)
              for entry = (%trim (subseq s start end))
              do (when (plusp (length entry))
                   (let ((eq-pos (position #\= entry)))
                     (when (and eq-pos (plusp eq-pos))
                       (let ((k (%trim (subseq entry 0 eq-pos)))
                             (v (%trim (subseq entry (1+ eq-pos)))))
                         (when (plusp (length k))
                           (push (cons k v) result))))))
                 (setf start (1+ end)))))
    (nreverse result)))

(defun parse-content-capture-mode (s config &optional (var-name "AGENTO11Y_CONTENT_CAPTURE_MODE"))
  "Map canonical content capture string to an agento11y-cl keyword.
Accepts \"full\", \"no_tool_content\", \"full_with_metadata_spans\", and
\"metadata_only\". Unknown values produce a warning via agento11y-log and
return :invalid. VAR-NAME names the variable the value came from, so the
warning points at the spelling the caller actually set."
  (let ((normalized (string-downcase (%trim s))))
    (cond
      ((string= normalized "full") :full)
      ((string= normalized "no_tool_content") :no-tool-content)
      ((string= normalized "full_with_metadata_spans") :full-with-metadata-spans)
      ((string= normalized "metadata_only") :metadata-only)
      (t
       (agento11y-log config :warn "env"
                  (format nil "ignoring ~a=~a (unsupported value)" var-name s))
       :invalid))))

(defun %parse-auth-mode (s config &optional (var-name "AGENTO11Y_AUTH_MODE"))
  "Map a canonical auth-mode string to a keyword. Unknown values warn and return :invalid."
  (let ((normalized (string-downcase (%trim s))))
    (cond
      ((string= normalized "none")   :none)
      ((string= normalized "tenant") :tenant)
      ((string= normalized "bearer") :bearer)
      ((string= normalized "basic")  :basic)
      (t
       (agento11y-log config :warn "env"
                  (format nil "ignoring ~a=~a (unsupported value)" var-name s))
       :invalid))))

(defun %merge-alist-env-base (env-list caller-list key-test)
  "Merge env-derived alist as the base layer; caller alist overrides on key
collision. KEY-TEST compares keys (use #'equal for tags, #'string-equal for
HTTP headers). Returns a fresh alist."
  (let ((merged (copy-alist env-list)))
    (dolist (kv caller-list)
      (let ((existing (assoc (car kv) merged :test key-test)))
        (if existing
            (setf (cdr existing) (cdr kv))
            (setf merged (append merged (list (cons (car kv) (cdr kv))))))))
    merged))

;;; --- Slot copying via MOP ---
;;;
;;; To avoid losing values when agento11y-config grows new slots, the resolver
;;; reflects over the class to copy every initarg-bound slot from the input
;;; config into a fresh instance, then layers env overrides on top. CLOS
;;; resolves duplicate initargs by taking the first occurrence, so overrides
;;; appear before the copied slots in the argument list.

(defun %copy-config-initargs (config)
  "Return a plist of (initarg value ...) covering every initarg-bound slot of CONFIG."
  (let ((result nil))
    (dolist (slot-def (c2mop:class-slots (class-of config)))
      (let ((name (c2mop:slot-definition-name slot-def))
            (initarg (first (c2mop:slot-definition-initargs slot-def))))
        (when (and initarg (slot-boundp config name))
          (push (slot-value config name) result)
          (push initarg result))))
    result))

(defun %otlp-signal-endpoint (base signal)
  (let* ((end (or (position-if (lambda (c) (find c "?#")) base) (length base)))
         (path (string-right-trim "/" (subseq base 0 end))))
    (concatenate 'string path "/v1/" signal (subseq base end))))

(defun %parse-otlp-protocol (value name)
  (unless (string-equal value "http/json")
    (error "Unsupported ~a; only http/json OTLP is supported" name))
  :http/json)

;;; --- Public resolver ---

(defun resolve-config-from-env (config &key (env-fn #'uiop:getenv))
  "Copy CONFIG, resolving caller options > environment > defaults.
Explicit NIL and explicit default values win, including through repeated copies.
Branded variables prefer AGENTO11Y_ over SIGIL_ before parsing.

OTLP endpoint precedence: explicit signal option > branded exact signal URL >
branded OTLP base > standard signal URL > standard OTLP base. Only base URLs
receive /v1/traces or /v1/metrics. Standard signal headers replace generic OTLP
headers. API extra headers are separate. Only HTTP JSON OTLP is supported.

A metrics-specific endpoint enables metrics unless metrics-enabled was supplied.
A generic OTLP base alone does not enable metrics or ordinary trace recording.
USE_EXPERIMENTAL_OTEL controls experiment telemetry, not generation transport."
  (let ((overrides nil))
    (labels ((supplied (key) (config-option-supplied-p config key))
             (override (key value)
               (setf (getf overrides key) value))
             (effective (key reader)
               (getf overrides key (funcall reader config)))
             (branded (suffix) (env-branded env-fn suffix))
             (standard (name) (env-trimmed env-fn name))
             (set-env (key suffix &optional parser)
               (unless (supplied key)
                 (let* ((pair (env-branded* env-fn suffix))
                        (value (car pair)))
                   (when value
                     (let ((parsed (if parser
                                       (funcall parser value config (cdr pair))
                                       value)))
                       (unless (eq parsed :invalid)
                         (override key parsed)))))))
             (set-alist (key text test)
               ;; Retain the caller's map separately so earlier env values cannot
               ;; override a later environment when a resolved config is copied.
               (let ((caller (getf (slot-value config 'supplied-option-values) key)))
                 (unless (and (supplied key) (null caller))
                   (when text
                     (override key (%merge-alist-env-base (parse-csv-kv text) caller test)))))))
      (dolist (entry '((:generation-endpoint "ENDPOINT")
                       (:eval-endpoint "EVAL_ENDPOINT")
                       (:eval-path-prefix "EVAL_PATH_PREFIX")
                       (:eval-auth-token "EVAL_AUTH_TOKEN")
                       (:ingest-actor "INGEST_ACTOR")
                       (:experiment-url-template "EXPERIMENT_URL_TEMPLATE")
                       (:auth-password "AUTH_TOKEN")
                       (:tenant-id "AUTH_TENANT_ID")
                       (:agent-name "AGENT_NAME")
                       (:agent-version "AGENT_VERSION")
                       (:user-id "USER_ID")))
        (set-env (first entry) (second entry)))
      (set-env :auth-mode "AUTH_MODE" #'%parse-auth-mode)
      (set-env :content-capture-mode "CONTENT_CAPTURE_MODE" #'parse-content-capture-mode)
      (dolist (entry '((:redact-secrets "REDACT_SECRETS")
                       (:redact-input-messages "REDACT_INPUT_MESSAGES")
                       (:redact-email-addresses "REDACT_EMAIL_ADDRESSES")
                       (:debug "DEBUG")
                       (:experimental-features "ENABLE_EXPERIMENTAL_FEATURES")
                       (:use-experimental-otel "USE_EXPERIMENTAL_OTEL")
                       (:metrics-enabled "METRICS_ENABLED")))
        (set-env (first entry) (second entry) #'parse-strict-bool))
      (set-alist :tags (branded "TAGS") #'equal)
      (set-alist :extra-headers (branded "HEADERS") #'string-equal)
      (unless (supplied :generation-protocol)
        (let* ((pair (env-branded* env-fn "PROTOCOL"))
               (protocol (car pair)))
          (when protocol
            (cond ((string-equal protocol "otel") (override :generation-protocol :otel))
                  ((member protocol '("http" "https") :test #'string-equal)
                   (override :generation-protocol :http))
                  (t (agento11y-log config :warn "env"
                                   (format nil "ignoring ~a (supported values are http, https, and otel)"
                                           (cdr pair))))))))
      (unless (valid-content-capture-mode-p
               (effective :content-capture-mode #'config-content-capture-mode))
        (agento11y-log config :warn "config"
                       (format nil "ignoring :content-capture-mode ~s (unsupported value), using :metadata-only"
                               (effective :content-capture-mode #'config-content-capture-mode)))
        (override :content-capture-mode :metadata-only))
      (let* ((branded-base (branded "OTEL_EXPORTER_OTLP_ENDPOINT"))
             (base (or branded-base (standard "OTEL_EXPORTER_OTLP_ENDPOINT")))
             (metrics-specific nil))
        (dolist (signal '("traces" "metrics"))
          (let* ((traces-p (string= signal "traces"))
                 (key (if traces-p :traces-endpoint :metrics-endpoint))
                 (header-key (if traces-p :traces-headers :metrics-headers))
                 (protocol-key (if traces-p :traces-protocol :metrics-protocol))
                 (upper (string-upcase signal))
                 (exact (branded (concatenate 'string upper "_ENDPOINT")))
                 (standard-exact (standard (concatenate 'string "OTEL_EXPORTER_OTLP_" upper "_ENDPOINT")))
                 (endpoint (or exact
                               (when branded-base (%otlp-signal-endpoint branded-base signal))
                               standard-exact
                               (when base (%otlp-signal-endpoint base signal))))
                 (headers (or (standard (concatenate 'string "OTEL_EXPORTER_OTLP_" upper "_HEADERS"))
                              (standard "OTEL_EXPORTER_OTLP_HEADERS")))
                 (protocol-name (concatenate 'string "OTEL_EXPORTER_OTLP_" upper "_PROTOCOL"))
                 (signal-protocol (standard protocol-name))
                 (protocol (or signal-protocol (standard "OTEL_EXPORTER_OTLP_PROTOCOL"))))
            (unless (supplied key)
              (when endpoint (override key endpoint)))
            (unless traces-p
              (setf metrics-specific
                    (if (supplied key)
                        (config-metrics-endpoint config)
                        (or exact (and (not branded-base) standard-exact)))))
            (unless (supplied header-key)
              (when headers (override header-key (parse-csv-kv headers))))
            (unless (supplied protocol-key)
              (when protocol
                (override protocol-key
                          (%parse-otlp-protocol protocol
                                                (if signal-protocol protocol-name
                                                    "OTEL_EXPORTER_OTLP_PROTOCOL")))))))
        ;; A base shared with generation export must not start a new signal.
        (when (and metrics-specific (not (supplied :metrics-enabled))
                   (not (branded "METRICS_ENABLED")))
          (override :metrics-enabled t)))
      (set-alist :resource-attributes (standard "OTEL_RESOURCE_ATTRIBUTES") #'equal)
      (let ((resources (effective :resource-attributes #'config-resource-attributes)))
        (unless (supplied :service-name)
          (let ((name (or (cdr (assoc "service.name"
                                     (getf (slot-value config 'supplied-option-values)
                                           :resource-attributes) :test #'equal))
                          (standard "OTEL_SERVICE_NAME")
                          (cdr (assoc "service.name" resources :test #'equal)))))
            (when name (override :service-name name))))
        (dolist (entry '((:service-version "service.version")
                         (:service-instance-id "service.instance.id")))
          (unless (supplied (first entry))
            (let ((value (cdr (assoc (second entry) resources :test #'equal))))
              (when value (override (first entry) value))))))
      (let* ((*copying-config-p* t)
             (copy (apply #'make-instance 'agento11y-config
                          (append overrides (%copy-config-initargs config)))))
        (setf (slot-value copy 'supplied-options)
              (copy-list (slot-value config 'supplied-options))
              (slot-value copy 'supplied-option-values)
              (copy-list (slot-value config 'supplied-option-values)))
        copy))))
