(in-package :agento11y-cl)

;;; Canonical AGENTO11Y_* environment variable resolution.
;;;
;;; Mirrors the env schema shipped in the canonical agento11y SDKs
;;; (Go: go/agento11y/env.go, Python: python/agento11y/config.py).
;;; Resolution precedence: explicit caller config > env var > schema defaults.
;;;
;;; Every variable has a preferred AGENTO11Y_<SUFFIX> name and a legacy
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

;;; --- Public resolver ---

(defun resolve-config-from-env (config &key (env-fn #'uiop:getenv))
  "Return a fresh agento11y-config with AGENTO11Y_* env vars layered on top of
defaults but below explicit caller-supplied values. CONFIG is the user's config;
ENV-FN is called with each variable name and must return a string or nil.

Resolution precedence per slot: caller value (explicit) > env > schema default.

Recognized suffixes, each read as AGENTO11Y_<SUFFIX> with a legacy
SIGIL_<SUFFIX> fallback:
  ENDPOINT, EVAL_ENDPOINT, EVAL_PATH_PREFIX, EVAL_AUTH_TOKEN, INGEST_ACTOR,
  EXPERIMENT_URL_TEMPLATE, HEADERS, AUTH_MODE, AUTH_TENANT_ID, AUTH_TOKEN,
  AGENT_NAME, AGENT_VERSION, USER_ID, TAGS, CONTENT_CAPTURE_MODE,
  REDACT_SECRETS, REDACT_INPUT_MESSAGES, DEBUG,
  ENABLE_EXPERIMENTAL_FEATURES.

The AGENTO11Y_ spelling wins whenever both are set, and the choice is made
before parsing, so a stale SIGIL_ value cannot resurface when the preferred one
fails validation. The two spellings are never merged: the selected value is
used whole, including for TAGS and HEADERS.

AGENTO11Y_PROTOCOL is not supported (agento11y-cl is HTTP-only); a warning is
logged when set to anything other than http/https. AGENTO11Y_INSECURE is a
no-op since TLS is controlled by the URL scheme.

Note: an explicit caller value that equals the slot's schema default is
indistinguishable from \"unset\" and WILL be overridden by env. This applies to
:none for :auth-mode, :no-tool-content for :content-capture-mode, and nil for
:redact-secrets / :redact-input-messages. For the redaction flags the asymmetry
fails safe: env can only turn redaction on when the caller did not ask for it.
Callers that need to enforce these defaults against deployment env must either
set the matching variable or unset it.
:eval-path-prefix and :ingest-actor do not have that problem: their slots hold
NIL until a caller sets them, so an explicit value equal to the default still
wins over env."
  (let* ((endpoint  (env-branded env-fn "ENDPOINT"))
         (eval-endpoint (env-branded env-fn "EVAL_ENDPOINT"))
         (eval-path-prefix (env-branded env-fn "EVAL_PATH_PREFIX"))
         (eval-auth-token (env-branded env-fn "EVAL_AUTH_TOKEN"))
         (ingest-actor (env-branded env-fn "INGEST_ACTOR"))
         (experiment-url-template (env-branded env-fn "EXPERIMENT_URL_TEMPLATE"))
         (headers   (env-branded env-fn "HEADERS"))
         (agent     (env-branded env-fn "AGENT_NAME"))
         (agent-ver (env-branded env-fn "AGENT_VERSION"))
         (user      (env-branded env-fn "USER_ID"))
         (tags      (env-branded env-fn "TAGS"))
         (debug     (env-branded env-fn "DEBUG"))
         (experimental (env-branded env-fn +experimental-features-env-suffix+))
         (tenant    (env-branded env-fn "AUTH_TENANT_ID"))
         (token     (env-branded env-fn "AUTH_TOKEN"))
         ;; These five keep the variable name next to the value, so a warning
         ;; about an unsupported value names the spelling the caller set.
         (auth-mode-pair (env-branded* env-fn "AUTH_MODE"))
         (auth-mode (car auth-mode-pair))
         (auth-mode-var (cdr auth-mode-pair))
         (capture-pair (env-branded* env-fn "CONTENT_CAPTURE_MODE"))
         (capture (car capture-pair))
         (capture-var (cdr capture-pair))
         (redact-secrets-pair (env-branded* env-fn "REDACT_SECRETS"))
         (redact-secrets (car redact-secrets-pair))
         (redact-secrets-var (cdr redact-secrets-pair))
         (redact-inputs-pair (env-branded* env-fn "REDACT_INPUT_MESSAGES"))
         (redact-inputs (car redact-inputs-pair))
         (redact-inputs-var (cdr redact-inputs-pair))
         (protocol-pair (env-branded* env-fn "PROTOCOL"))
         (protocol (car protocol-pair))
         (protocol-var (cdr protocol-pair)))
    ;; Warn when the protocol var is set to something we don't support.
    (when (and protocol
               (not (member (string-downcase protocol) '("http" "https") :test #'string=)))
      (agento11y-log config :warn "env"
                 (format nil "ignoring ~a=~a (agento11y-cl is HTTP-only)"
                         protocol-var protocol)))
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
          (let ((parsed (%parse-auth-mode auth-mode config auth-mode-var)))
            (unless (eq parsed :invalid)
              (override :auth-mode parsed))))
        ;; Auth password.
        (when (and token (null (config-auth-password config)))
          (override :auth-password token))
        ;; Tenant ID.
        (when (and tenant (null (config-tenant-id config)))
          (override :tenant-id tenant))
        ;; Content capture mode (caller :no-tool-content, the schema default,
        ;; is treated as "not set").
        (when (and capture (eq (config-content-capture-mode config) :no-tool-content))
          (let ((parsed (parse-content-capture-mode capture config capture-var)))
            (unless (eq parsed :invalid)
              (override :content-capture-mode parsed))))
        ;; A caller mode outside the supported set falls back to :metadata-only.
        ;; The env branch above cannot also have fired: it requires the caller
        ;; mode to be :no-tool-content, which is supported. Serialization redacts
        ;; an unsupported mode on its own; this warning is the diagnostic.
        (unless (valid-content-capture-mode-p (config-content-capture-mode config))
          (agento11y-log config :warn "config"
                     (format nil "ignoring :content-capture-mode ~s (unsupported value), using :metadata-only"
                             (config-content-capture-mode config)))
          (override :content-capture-mode :metadata-only))
        ;; Secret redaction master enable (agento11y-cl extension).
        (when (and redact-secrets (null (config-redact-secrets config)))
          (let ((parsed (parse-strict-bool redact-secrets config redact-secrets-var)))
            (unless (eq parsed :invalid)
              (override :redact-secrets parsed))))
        ;; Input-message redaction gating. Name kept aligned with the Python SDK env var.
        (when (and redact-inputs (null (config-redact-input-messages config)))
          (let ((parsed (parse-strict-bool redact-inputs config redact-inputs-var)))
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
      (apply #'make-instance 'agento11y-config
             (append (nreverse overrides) (%copy-config-initargs config))))))
