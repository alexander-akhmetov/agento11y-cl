(in-package :sigil-cl)

;;; Canonical SIGIL_* environment variable resolution.
;;;
;;; Mirrors the env schema shipped in the canonical sigil-sdk
;;; (Go: sigil-sdk/go/sigil/env.go, Python: sigil-sdk/python/sigil_sdk/config.py).
;;; Resolution precedence: explicit caller config > env var > schema defaults.

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
(case-insensitive). Any other value logs a warning via sigil-log and returns
:invalid so a typo can't silently flip behavior. The warning never includes the
value: these vars gate redaction, and a value pasted there by mistake could be a
secret."
  (let ((normalized (string-downcase (%trim s))))
    (cond
      ((member normalized '("1" "true" "yes" "on" "t") :test #'string=) t)
      ((member normalized '("0" "false" "no" "off" "nil") :test #'string=) nil)
      (t
       (sigil-log config :warn "env"
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

(defun parse-content-capture-mode (s config)
  "Map canonical content capture string to a sigil-cl keyword.
Accepts \"full\", \"no_tool_content\", and \"metadata_only\". The
:metadata-with-system-prompt mode is a CL-only extension and is not exposed
via env. Unknown values produce a warning via sigil-log and return :invalid."
  (let ((normalized (string-downcase (%trim s))))
    (cond
      ((string= normalized "full") :full)
      ((string= normalized "no_tool_content") :no-tool-content)
      ((string= normalized "metadata_only") :metadata-only)
      (t
       (sigil-log config :warn "env"
                  (format nil "ignoring SIGIL_CONTENT_CAPTURE_MODE=~a (unsupported value)"
                          s))
       :invalid))))

(defun %parse-auth-mode (s config)
  "Map a canonical auth-mode string to a keyword. Unknown values warn and return :invalid."
  (let ((normalized (string-downcase (%trim s))))
    (cond
      ((string= normalized "none")   :none)
      ((string= normalized "tenant") :tenant)
      ((string= normalized "bearer") :bearer)
      ((string= normalized "basic")  :basic)
      (t
       (sigil-log config :warn "env"
                  (format nil "ignoring SIGIL_AUTH_MODE=~a (unsupported value)" s))
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
;;; To avoid losing values when sigil-config grows new slots, the resolver
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

;;; --- Public resolver ---

(defun resolve-config-from-env (config &key (env-fn #'uiop:getenv))
  "Return a fresh sigil-config with SIGIL_* env vars layered on top of defaults
but below explicit caller-supplied values. CONFIG is the user's config; ENV-FN
is called with each variable name and must return a string or nil.

Resolution precedence per slot: caller value (explicit) > env > schema default.

Recognized variables:
  SIGIL_ENDPOINT, SIGIL_EVAL_ENDPOINT, SIGIL_EVAL_PATH_PREFIX,
  SIGIL_EVAL_AUTH_TOKEN, SIGIL_INGEST_ACTOR,
  SIGIL_EXPERIMENT_URL_TEMPLATE, SIGIL_HEADERS, SIGIL_AUTH_MODE,
  SIGIL_AUTH_TENANT_ID, SIGIL_AUTH_TOKEN, SIGIL_AGENT_NAME,
  SIGIL_AGENT_VERSION, SIGIL_USER_ID, SIGIL_TAGS,
  SIGIL_CONTENT_CAPTURE_MODE, SIGIL_REDACT_SECRETS,
  SIGIL_REDACT_INPUT_MESSAGES, SIGIL_DEBUG,
  SIGIL_ENABLE_EXPERIMENTAL_FEATURES.

SIGIL_ENABLE_EXPERIMENTAL_FEATURES also reads its agento11y spelling,
AGENTO11Y_ENABLE_EXPERIMENTAL_FEATURES, so a polyglot harness that already
exports the Go gate unlocks this SDK too. The SIGIL_ name wins when both are
set.

SIGIL_PROTOCOL is not supported (sigil-cl is HTTP-only); a warning is logged
when set to anything other than http/https. SIGIL_INSECURE is a no-op since
TLS is controlled by the URL scheme.

Note: an explicit caller value that equals the slot's schema default is
indistinguishable from \"unset\" and WILL be overridden by env. This applies to
:none for :auth-mode, :metadata-only for :content-capture-mode, and nil for
:redact-secrets / :redact-input-messages. For the redaction flags the asymmetry
fails safe: env can only turn redaction on when the caller did not ask for it.
Callers that need to enforce these defaults against deployment env must either
set the matching SIGIL_* var or unset it.
:eval-path-prefix and :ingest-actor do not have that problem: their slots hold
NIL until a caller sets them, so an explicit value equal to the default still
wins over env."
  (let* ((endpoint  (env-trimmed env-fn "SIGIL_ENDPOINT"))
         (eval-endpoint (env-trimmed env-fn "SIGIL_EVAL_ENDPOINT"))
         (eval-path-prefix (env-trimmed env-fn "SIGIL_EVAL_PATH_PREFIX"))
         (eval-auth-token (env-trimmed env-fn "SIGIL_EVAL_AUTH_TOKEN"))
         (ingest-actor (env-trimmed env-fn "SIGIL_INGEST_ACTOR"))
         (experiment-url-template (env-trimmed env-fn "SIGIL_EXPERIMENT_URL_TEMPLATE"))
         (headers   (env-trimmed env-fn "SIGIL_HEADERS"))
         (auth-mode (env-trimmed env-fn "SIGIL_AUTH_MODE"))
         (tenant    (env-trimmed env-fn "SIGIL_AUTH_TENANT_ID"))
         (token     (env-trimmed env-fn "SIGIL_AUTH_TOKEN"))
         (agent     (env-trimmed env-fn "SIGIL_AGENT_NAME"))
         (agent-ver (env-trimmed env-fn "SIGIL_AGENT_VERSION"))
         (user      (env-trimmed env-fn "SIGIL_USER_ID"))
         (tags      (env-trimmed env-fn "SIGIL_TAGS"))
         (capture   (env-trimmed env-fn "SIGIL_CONTENT_CAPTURE_MODE"))
         (redact-secrets (env-trimmed env-fn "SIGIL_REDACT_SECRETS"))
         (redact-inputs  (env-trimmed env-fn "SIGIL_REDACT_INPUT_MESSAGES"))
         (debug     (env-trimmed env-fn "SIGIL_DEBUG"))
         (experimental (or (env-trimmed env-fn +experimental-features-env-var+)
                           (env-trimmed env-fn "AGENTO11Y_ENABLE_EXPERIMENTAL_FEATURES")))
         (protocol  (env-trimmed env-fn "SIGIL_PROTOCOL")))
    ;; Warn when SIGIL_PROTOCOL is set to something we don't support.
    (when (and protocol
               (not (member (string-downcase protocol) '("http" "https") :test #'string=)))
      (sigil-log config :warn "env"
                 (format nil "ignoring SIGIL_PROTOCOL=~a (sigil-cl is HTTP-only)" protocol)))
    (let ((overrides nil))
      (flet ((override (k v) (push k overrides) (push v overrides)))
        ;; Endpoint.
        (when (and endpoint (null (config-generation-endpoint config)))
          (override :generation-endpoint endpoint))
        ;; Eval endpoint and links.
        (when (and eval-endpoint (null (config-eval-endpoint config)))
          (override :eval-endpoint eval-endpoint))
        ;; These two ask the slot itself rather than comparing against the
        ;; default, so a caller who explicitly passes the default value keeps it.
        (when (and eval-path-prefix
                   (not (config-eval-path-prefix-supplied-p config)))
          (override :eval-path-prefix eval-path-prefix))
        (when (and ingest-actor
                   (not (config-ingest-actor-supplied-p config)))
          (override :ingest-actor ingest-actor))
        (when (and eval-auth-token (null (config-eval-auth-token config)))
          (override :eval-auth-token eval-auth-token))
        (when (and experiment-url-template
                   (null (config-experiment-url-template config)))
          (override :experiment-url-template experiment-url-template))
        ;; Auth mode (caller :none is treated as "not set").
        (when (and auth-mode (eq (config-auth-mode config) :none))
          (let ((parsed (%parse-auth-mode auth-mode config)))
            (unless (eq parsed :invalid)
              (override :auth-mode parsed))))
        ;; Auth password.
        (when (and token (null (config-auth-password config)))
          (override :auth-password token))
        ;; Tenant ID.
        (when (and tenant (null (config-tenant-id config)))
          (override :tenant-id tenant))
        ;; Content capture mode (caller :metadata-only is treated as "not set").
        (when (and capture (eq (config-content-capture-mode config) :metadata-only))
          (let ((parsed (parse-content-capture-mode capture config)))
            (unless (eq parsed :invalid)
              (override :content-capture-mode parsed))))
        ;; Secret redaction master enable (sigil-cl extension).
        (when (and redact-secrets (null (config-redact-secrets config)))
          (let ((parsed (parse-strict-bool redact-secrets config "SIGIL_REDACT_SECRETS")))
            (unless (eq parsed :invalid)
              (override :redact-secrets parsed))))
        ;; Input-message redaction gating. Name kept aligned with the Python SDK env var.
        (when (and redact-inputs (null (config-redact-input-messages config)))
          (let ((parsed (parse-strict-bool redact-inputs config "SIGIL_REDACT_INPUT_MESSAGES")))
            (unless (eq parsed :invalid)
              (override :redact-input-messages parsed))))
        ;; Agent identity.
        (when (and agent (null (config-agent-name config)))
          (override :agent-name agent))
        (when (and agent-ver (null (config-agent-version config)))
          (override :agent-version agent-ver))
        ;; User ID.
        (when (and user (null (config-user-id config)))
          (override :user-id user))
        ;; Tags: env tags are base layer, caller tags win on key collision.
        (let ((env-tags (when tags (parse-csv-kv tags))))
          (when env-tags
            (override :tags
                      (%merge-alist-env-base env-tags (config-tags config) #'equal))))
        ;; Extra headers: case-insensitive collision so caller wins.
        (let ((env-headers (when headers (parse-csv-kv headers))))
          (when env-headers
            (override :extra-headers
                      (%merge-alist-env-base env-headers (config-extra-headers config)
                                             #'string-equal))))
        ;; Debug.
        (when (and debug (null (config-debug config)))
          (override :debug (parse-bool debug)))
        ;; Experimental feature gate.
        (when (and experimental (null (config-experimental-features config)))
          (override :experimental-features (parse-bool experimental))))
      ;; Overrides come first so make-instance picks them over the copied slots
      ;; (CLOS uses the first occurrence of an initarg).
      (apply #'make-instance 'sigil-config
             (append (nreverse overrides) (%copy-config-initargs config))))))
