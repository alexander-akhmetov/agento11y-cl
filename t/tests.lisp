(in-package :sigil-cl/t)

;;; --- Helper: make a test client with captured requests ---

(defun make-test-client (&key (generation-enabled t) (traces-enabled t)
                               (workflow-steps-enabled nil)
                               (metrics-enabled nil)
                               (capture :metadata-only)
                               (generation-endpoint "http://test-sigil:4318/api/v1/generations:export")
                               eval-endpoint
                               (eval-path-prefix "/api/v1")
                               (scores-export-path "/api/v1/scores:export")
                               eval-auth-token
                               (ingest-actor "ingest:sdk/lisp")
                               experiment-url-template
                               (traces-endpoint "http://test-sigil:4318/v1/traces")
                               (workflow-steps-endpoint
                                "http://test-sigil:4318/api/v1/workflow-steps:export")
                               (metrics-endpoint "http://test-sigil:4318/v1/metrics")
                               (auth-mode :bearer)
                               (auth-password "test-token")
                               (max-retries 5)
                               http-fn log-fn)
  "Create a client with mock HTTP that captures requests."
  (let ((requests nil))
    (values
     (make-client
      (make-config
       :generation-endpoint generation-endpoint
       :generation-enabled generation-enabled
       :eval-endpoint eval-endpoint
       :eval-path-prefix eval-path-prefix
       :scores-export-path scores-export-path
       :eval-auth-token eval-auth-token
       :ingest-actor ingest-actor
       :experiment-url-template experiment-url-template
       :traces-endpoint traces-endpoint
       :traces-enabled traces-enabled
       :workflow-steps-endpoint workflow-steps-endpoint
       :workflow-steps-enabled workflow-steps-enabled
       :metrics-endpoint metrics-endpoint
       :metrics-enabled metrics-enabled
       :content-capture-mode capture
       :service-name "test-service"
       :service-version "1.0.0"
       :auth-mode auth-mode
       :auth-password auth-password
       :max-retries max-retries
       :log-fn log-fn
       :http-fn (or http-fn
                    (lambda (url &key method headers content &allow-other-keys)
                      (declare (ignore method headers))
                      (push (list url content) requests)
                      (values "{}" 200))))
      :env-fn (constantly nil))
     (lambda () (reverse requests)))))

;;; --- Helpers shared by the experiment, trial, and suite suites ---
;;;
;;; One definition each. Two copies of ROUTED-HTTP had already drifted into
;;; disagreeing about what the backend returns.

(defun signals-condition-p (thunk expected-type)
  (handler-case
      (progn (funcall thunk) nil)
    (sigil-error (e) (typep e expected-type))))

(defun payload (call)
  (jzon:parse (third call)))

(defun score-call-p (call)
  (and (search "scores:export" (second call)) t))

(defun make-score-response (content)
  (let* ((parsed (jzon:parse content))
         (scores (jget parsed "scores"))
         (results nil))
    (loop for score across scores
          do (push (jobj "score_id" (jget score "score_id") "accepted" t)
                   results))
    (jzon:stringify (jobj "results" (coerce (nreverse results) 'vector)))))

(defun run-http-response (experiment-id status)
  (jzon:stringify (jobj "experiment_id" experiment-id
                        "name" "prompt A"
                        "status" status)))

(defun routed-http (calls-place experiment-id &key (score-status 202) score-body)
  "One stub covering every route an experiment touches. Calls are pushed onto
the cdr of CALLS-PLACE as (method url content), newest first."
  (lambda (url &key method headers content &allow-other-keys)
    (declare (ignore headers))
    (push (list method url content) (cdr calls-place))
    (cond
      ((search "scores:export" url)
       (values (or score-body (make-score-response content)) score-status))
      ((search "generations:export" url) (values "{}" 200))
      ((and (eq method :get) (search "/report" url))
       (values (jzon:stringify (jobj "experiment_id" experiment-id)) 200))
      ((search "experiment-runs" url)
       (values (run-http-response experiment-id "running") 200))
      (t (values "{}" 200)))))

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
    (check "backoff capped" (= (sigil-cl::backoff-seconds 10 0.1 5.0) 5.0))

    ;; UTF-8 encoding
    (check "utf8 ascii"
           (equalp (sigil-cl::string-to-utf8-octets "abc") #(97 98 99)))
    (check "utf8 two-byte codepoint"
           (equalp (sigil-cl::string-to-utf8-octets "é") #(195 169)))
    (check "utf8 three-byte codepoint"
           (equalp (sigil-cl::string-to-utf8-octets "€") #(226 130 172)))

    ;; SHA-1 (FIPS 180 test vectors)
    (check "sha1 empty"
           (equal (sigil-cl::sha1-hex (sigil-cl::string-to-utf8-octets ""))
                  "da39a3ee5e6b4b0d3255bfef95601890afd80709"))
    (check "sha1 abc"
           (equal (sigil-cl::sha1-hex (sigil-cl::string-to-utf8-octets "abc"))
                  "a9993e364706816aba3e25717850c26c9cd0d89d"))
    (check "sha1 two-block message"
           (equal (sigil-cl::sha1-hex
                   (sigil-cl::string-to-utf8-octets
                    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
                  "84983e441c3bd26ebaae4aa1f95129e5e54670f1"))
    (check "sha1 multi-block input"
           (equal (sigil-cl::sha1-hex
                   (sigil-cl::string-to-utf8-octets
                    (make-string 200 :initial-element #\a)))
                  "e61cfffe0d9195a525fc6cf06ca2d77119c24a40"))))

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
             (null (sigil-cl::build-traces-auth-headers cfg))))

    ;; Extra headers merge: appended to auth-derived headers
    (let* ((cfg (make-config :auth-mode :bearer :auth-password "tok"
                             :extra-headers '(("X-Custom" . "hello")
                                              ("X-Trace" . "abc"))))
           (headers (sigil-cl::build-auth-headers cfg)))
      (check "extra-headers: keeps Authorization"
             (equal (cdr (assoc "Authorization" headers :test #'equal))
                    "Bearer tok"))
      (check "extra-headers: includes custom"
             (equal (cdr (assoc "X-Custom" headers :test #'equal)) "hello"))
      (check "extra-headers: includes second"
             (equal (cdr (assoc "X-Trace" headers :test #'equal)) "abc")))

    ;; Extra headers wins on case-insensitive Authorization collision
    (let* ((cfg (make-config :auth-mode :bearer :auth-password "tok"
                             :extra-headers '(("authorization" . "Bearer override"))))
           (headers (sigil-cl::build-auth-headers cfg))
           (auth-vals (mapcar #'cdr
                              (remove-if-not (lambda (kv)
                                               (string-equal (car kv) "authorization"))
                                             headers))))
      (check "extra-headers: user Authorization wins"
             (and (= (length auth-vals) 1)
                  (equal (first auth-vals) "Bearer override"))))

    ;; Case-insensitive duplicates inside extras themselves are collapsed
    (let* ((cfg (make-config :auth-mode :none
                             :extra-headers '(("Authorization" . "first")
                                              ("authorization" . "second")
                                              ("X-Custom" . "keep"))))
           (headers (sigil-cl::build-auth-headers cfg))
           (auth-vals (mapcar #'cdr
                              (remove-if-not (lambda (kv)
                                               (string-equal (car kv) "authorization"))
                                             headers))))
      (check "extra-headers: duplicate names collapsed to one entry"
             (= (length auth-vals) 1))
      (check "extra-headers: last duplicate wins"
             (equal (first auth-vals) "second"))
      (check "extra-headers: non-duplicate kept"
             (equal (cdr (assoc "X-Custom" headers :test #'equal)) "keep")))))

(defun run-env-tests ()
  (with-test-suite ("Env")
    (labels ((env-from-alist (alist)
               (lambda (name)
                 (cdr (assoc name alist :test #'string=))))
             (resolve (env-alist &rest config-args)
               (sigil-cl::resolve-config-from-env
                (apply #'make-config config-args)
                :env-fn (env-from-alist env-alist))))
      ;; --- env-trimmed ---
      (let ((env (env-from-alist '(("A" . "  hi  ") ("B" . "")))))
        (check "env-trimmed strips whitespace"
               (equal (sigil-cl::env-trimmed env "A") "hi"))
        (check "env-trimmed nil for empty"
               (null (sigil-cl::env-trimmed env "B")))
        (check "env-trimmed nil for missing"
               (null (sigil-cl::env-trimmed env "C"))))

      ;; --- parse-bool ---
      (check "parse-bool 1"   (sigil-cl::parse-bool "1"))
      (check "parse-bool true"   (sigil-cl::parse-bool "TRUE"))
      (check "parse-bool yes"  (sigil-cl::parse-bool "yes"))
      (check "parse-bool on"   (sigil-cl::parse-bool "on"))
      (check "parse-bool no"   (null (sigil-cl::parse-bool "no")))
      (check "parse-bool empty"(null (sigil-cl::parse-bool "")))
      (check "parse-bool nil"  (null (sigil-cl::parse-bool nil)))

      ;; --- parse-csv-kv ---
      (let ((kvs (sigil-cl::parse-csv-kv "k=v,k2=v2")))
        (check "parse-csv-kv simple count" (= (length kvs) 2))
        (check "parse-csv-kv first" (equal (assoc "k" kvs :test #'equal) '("k" . "v")))
        (check "parse-csv-kv second" (equal (assoc "k2" kvs :test #'equal) '("k2" . "v2"))))
      (let ((kvs (sigil-cl::parse-csv-kv "  a = 1 , b=2 ,, c=3")))
        (check "parse-csv-kv whitespace trimmed (a)"
               (equal (cdr (assoc "a" kvs :test #'equal)) "1"))
        (check "parse-csv-kv empty entries skipped"
               (= (length kvs) 3))
        (check "parse-csv-kv trailing entry" (equal (cdr (assoc "c" kvs :test #'equal)) "3")))
      (let ((kvs (sigil-cl::parse-csv-kv "noequals,k=v")))
        (check "parse-csv-kv missing = skipped"
               (and (= (length kvs) 1)
                    (equal (assoc "k" kvs :test #'equal) '("k" . "v")))))
      (check "parse-csv-kv nil input" (null (sigil-cl::parse-csv-kv nil)))
      (check "parse-csv-kv empty string" (null (sigil-cl::parse-csv-kv "")))

      ;; --- SIGIL_ENDPOINT ---
      (let ((cfg (resolve '(("SIGIL_ENDPOINT" . "https://example/api/v1/generations:export")))))
        (check "SIGIL_ENDPOINT sets generation-endpoint"
               (equal (sigil-cl::config-generation-endpoint cfg)
                      "https://example/api/v1/generations:export")))

      ;; --- SIGIL_EVAL_* ---
      (let ((cfg (resolve '(("SIGIL_EVAL_ENDPOINT" . "https://eval.example/api")
                            ("SIGIL_EVAL_PATH_PREFIX" . "/custom/v1")
                            ("SIGIL_EVAL_AUTH_TOKEN" . "env-eval-token")
                            ("SIGIL_EXPERIMENT_URL_TEMPLATE" . "https://ui/runs/{run_id}")))))
        (check "SIGIL_EVAL_ENDPOINT -> eval-endpoint"
               (equal (config-eval-endpoint cfg) "https://eval.example/api"))
        (check "SIGIL_EVAL_PATH_PREFIX -> eval-path-prefix"
               (equal (config-eval-path-prefix cfg) "/custom/v1"))
        (check "SIGIL_EVAL_AUTH_TOKEN -> eval-auth-token"
               (equal (config-eval-auth-token cfg) "env-eval-token"))
        (check "SIGIL_EXPERIMENT_URL_TEMPLATE -> experiment-url-template"
               (equal (config-experiment-url-template cfg) "https://ui/runs/{run_id}")))
      (let ((cfg (resolve '(("SIGIL_EVAL_ENDPOINT" . "https://env.example")
                            ("SIGIL_EVAL_PATH_PREFIX" . "/env")
                            ("SIGIL_EVAL_AUTH_TOKEN" . "env-eval-token")
                            ("SIGIL_EXPERIMENT_URL_TEMPLATE" . "https://env/{run_id}"))
                          :eval-endpoint "https://caller.example/path"
                          :eval-path-prefix "/caller"
                          :eval-auth-token "caller-eval-token"
                          :experiment-url-template "https://caller/{run_id}")))
        (check "caller eval-endpoint beats env"
               (equal (config-eval-endpoint cfg) "https://caller.example/path"))
        (check "caller eval-path-prefix beats env"
               (equal (config-eval-path-prefix cfg) "/caller"))
        (check "caller eval-auth-token beats env"
               (equal (config-eval-auth-token cfg) "caller-eval-token"))
        (check "caller experiment-url-template beats env"
               (equal (config-experiment-url-template cfg) "https://caller/{run_id}")))
      (let ((cfg (resolve '(("SIGIL_EVAL_PATH_PREFIX" . "/env")
                            ("SIGIL_EXPERIMENT_URL_TEMPLATE" . "https://env/{run_id}"))
                          :eval-endpoint "https://caller.example")))
        (check "env still fills unset eval-path-prefix"
               (equal (config-eval-path-prefix cfg) "/env"))
        (check "env still fills unset experiment-url-template"
               (equal (config-experiment-url-template cfg) "https://env/{run_id}")))
      ;; An explicit value equal to the default must survive env. The slot has
      ;; no initform, so "supplied" is distinguishable from "left at default".
      (let ((cfg (resolve '(("SIGIL_EVAL_PATH_PREFIX" . "/env"))
                          :eval-path-prefix "/api/v1")))
        (check "explicit default-valued eval-path-prefix beats env"
               (equal (config-eval-path-prefix cfg) "/api/v1")))
      (let ((cfg (resolve nil)))
        (check "scores-export-path defaults when neither caller nor env set it"
               (equal (config-scores-export-path cfg) "/api/v1/scores:export")))
      ;; An explicit NIL means "unset", so the default still applies and the
      ;; export URL cannot degrade to the host root.
      (let ((cfg (resolve nil :scores-export-path nil :eval-path-prefix nil
                              :ingest-actor nil)))
        (check "explicit nil scores-export-path falls back to the default"
               (equal (config-scores-export-path cfg) "/api/v1/scores:export"))
        (check "explicit nil eval-path-prefix falls back to the default"
               (equal (config-eval-path-prefix cfg) "/api/v1"))
        (check "explicit nil ingest-actor falls back to the default"
               (equal (config-ingest-actor cfg) "ingest:sdk/lisp")))
      (let ((cfg (resolve '(("SIGIL_EVAL_PATH_PREFIX" . "/env/v1"))
                          :eval-path-prefix nil)))
        (check "env still fills an explicitly nil eval-path-prefix"
               (equal (config-eval-path-prefix cfg) "/env/v1")))
      (let ((cfg (resolve '(("SIGIL_INGEST_ACTOR" . "ingest:runner/harbor")))))
        (check "SIGIL_INGEST_ACTOR -> ingest-actor"
               (equal (config-ingest-actor cfg) "ingest:runner/harbor")))
      (let ((cfg (resolve '(("SIGIL_INGEST_ACTOR" . "ingest:env"))
                          :ingest-actor "ingest:sdk/lisp")))
        (check "explicit default-valued ingest-actor beats env"
               (equal (config-ingest-actor cfg) "ingest:sdk/lisp")))

      ;; --- SIGIL_AUTH_* (env layered when caller does not set) ---
      (let ((cfg (resolve '(("SIGIL_AUTH_MODE" . "basic")
                            ("SIGIL_AUTH_TENANT_ID" . "t1")
                            ("SIGIL_AUTH_TOKEN" . "tok")))))
        (check "SIGIL_AUTH_MODE -> :basic"
               (eq (sigil-cl::config-auth-mode cfg) :basic))
        (check "SIGIL_AUTH_TENANT_ID -> tenant-id"
               (equal (sigil-cl::config-tenant-id cfg) "t1"))
        (check "SIGIL_AUTH_TOKEN -> auth-password"
               (equal (sigil-cl::config-auth-password cfg) "tok"))
        ;; And basic Authorization header is built downstream
        (let ((headers (sigil-cl::build-auth-headers cfg)))
          (check "Authorization built from env"
                 (search "Basic " (cdr (assoc "Authorization" headers :test #'equal))))))

      ;; --- Caller value beats env ---
      (let ((cfg (resolve '(("SIGIL_AUTH_MODE" . "basic"))
                          :auth-mode :bearer :auth-password "explicit")))
        (check "caller auth-mode beats env"
               (eq (sigil-cl::config-auth-mode cfg) :bearer))
        (check "caller auth-password beats env"
               (equal (sigil-cl::config-auth-password cfg) "explicit")))

      ;; --- Unknown SIGIL_AUTH_MODE warns and falls through ---
      (let* ((warn-count 0)
             (cfg (sigil-cl::resolve-config-from-env
                   (make-config :log-fn (lambda (level component message &rest kvs)
                                          (declare (ignore component message kvs))
                                          (when (eq level :warn) (incf warn-count))))
                   :env-fn (env-from-alist '(("SIGIL_AUTH_MODE" . "garbage")
                                             ("SIGIL_AUTH_TOKEN" . "tok"))))))
        (check "unknown SIGIL_AUTH_MODE keeps default"
               (eq (sigil-cl::config-auth-mode cfg) :none))
        (check "unknown SIGIL_AUTH_MODE warned"
               (>= warn-count 1))
        (check "other env vars still applied alongside bad auth-mode"
               (equal (sigil-cl::config-auth-password cfg) "tok")))

      ;; --- SIGIL_TAGS merges with caller tags, caller wins on collision ---
      (let ((cfg (resolve '(("SIGIL_TAGS" . "env=prod,layer=router"))
                          :tags '(("layer" . "agent")))))
        (let ((tags (sigil-cl::config-tags cfg)))
          (check "SIGIL_TAGS contributes env tag"
                 (equal (cdr (assoc "env" tags :test #'equal)) "prod"))
          (check "caller tags win on collision"
                 (equal (cdr (assoc "layer" tags :test #'equal)) "agent"))))
      (let ((cfg (resolve '(("SIGIL_TAGS" . "env=prod")))))
        (check "SIGIL_TAGS with no caller tags -> env tags only"
               (equal (cdr (assoc "env" (sigil-cl::config-tags cfg) :test #'equal)) "prod")))

      ;; --- SIGIL_HEADERS -> extra-headers, merged into auth ---
      (let ((cfg (resolve '(("SIGIL_HEADERS" . "X-Foo=bar,X-Baz=qux")))))
        (let ((extras (config-extra-headers cfg)))
          (check "SIGIL_HEADERS parsed into extra-headers"
                 (and (equal (cdr (assoc "X-Foo" extras :test #'equal)) "bar")
                      (equal (cdr (assoc "X-Baz" extras :test #'equal)) "qux")))))

      ;; SIGIL_HEADERS + caller extra-headers collide case-insensitively.
      (let* ((cfg (resolve '(("SIGIL_HEADERS" . "authorization=env"))
                           :extra-headers '(("Authorization" . "caller"))))
             (extras (config-extra-headers cfg))
             (auth-entries (remove-if-not (lambda (kv)
                                            (string-equal (car kv) "authorization"))
                                          extras)))
        (check "case-insensitive merge collapses to one entry"
               (= (length auth-entries) 1))
        (check "caller header wins case-insensitively over env"
               (equal (cdr (first auth-entries)) "caller")))

      ;; --- SIGIL_AGENT_NAME / VERSION ---
      (let ((cfg (resolve '(("SIGIL_AGENT_NAME" . "router")
                            ("SIGIL_AGENT_VERSION" . "1.2.3")))))
        (check "SIGIL_AGENT_NAME -> agent-name"
               (equal (config-agent-name cfg) "router"))
        (check "SIGIL_AGENT_VERSION -> agent-version"
               (equal (config-agent-version cfg) "1.2.3")))

      ;; --- Caller agent fields beat env ---
      (let ((cfg (resolve '(("SIGIL_AGENT_NAME" . "from-env"))
                          :agent-name "explicit")))
        (check "caller agent-name beats env"
               (equal (config-agent-name cfg) "explicit")))

      ;; --- SIGIL_USER_ID ---
      (let ((cfg (resolve '(("SIGIL_USER_ID" . "u-42")))))
        (check "SIGIL_USER_ID -> user-id"
               (equal (sigil-cl::config-user-id cfg) "u-42")))

      ;; --- SIGIL_CONTENT_CAPTURE_MODE ---
      (let ((cfg (resolve '(("SIGIL_CONTENT_CAPTURE_MODE" . "full")))))
        (check "SIGIL_CONTENT_CAPTURE_MODE=full"
               (eq (sigil-cl::config-content-capture-mode cfg) :full)))
      (let ((cfg (resolve '(("SIGIL_CONTENT_CAPTURE_MODE" . "no_tool_content")))))
        (check "SIGIL_CONTENT_CAPTURE_MODE=no_tool_content"
               (eq (sigil-cl::config-content-capture-mode cfg) :no-tool-content)))
      (let* ((warns 0)
             (cfg (sigil-cl::resolve-config-from-env
                   (make-config :log-fn (lambda (l c m &rest kvs)
                                          (declare (ignore c m kvs))
                                          (when (eq l :warn) (incf warns))))
                   :env-fn (env-from-alist
                            '(("SIGIL_CONTENT_CAPTURE_MODE" . "garbage"))))))
        (check "bad SIGIL_CONTENT_CAPTURE_MODE keeps default"
               (eq (sigil-cl::config-content-capture-mode cfg) :metadata-only))
        (check "bad SIGIL_CONTENT_CAPTURE_MODE warns"
               (>= warns 1)))

      ;; --- SIGIL_DEBUG ---
      (let ((cfg (resolve '(("SIGIL_DEBUG" . "1")))))
        (check "SIGIL_DEBUG=1 -> t" (config-debug cfg)))
      (let ((cfg (resolve '(("SIGIL_DEBUG" . "false")))))
        (check "SIGIL_DEBUG=false -> nil" (null (config-debug cfg))))

      ;; --- SIGIL_PROTOCOL warning when non-http ---
      (let* ((warns 0)
             (cfg (sigil-cl::resolve-config-from-env
                   (make-config :log-fn (lambda (l c m &rest kvs)
                                          (declare (ignore c m kvs))
                                          (when (eq l :warn) (incf warns))))
                   :env-fn (env-from-alist '(("SIGIL_PROTOCOL" . "grpc"))))))
        (declare (ignore cfg))
        (check "non-http SIGIL_PROTOCOL warns" (>= warns 1)))

      ;; --- make-client honours :env-fn so deployments and tests can stub the env ---
      (let* ((env (env-from-alist '(("SIGIL_AGENT_NAME" . "auto"))))
             (client (make-client (make-config) :env-fn env)))
        (check "make-client :env-fn layers env into resolved config"
               (equal (config-agent-name (sigil-cl::client-config client)) "auto")))

      ;; --- Resolver preserves slots it doesn't touch ---
      ;; Regression guard: the MOP-based copy means new slots Just Work; this
      ;; test catches accidental regressions to a manually enumerated resolver
      ;; that would silently drop unrelated caller-supplied values.
      (let* ((http-fn (lambda (&rest _) (declare (ignore _)) (values "" 200)))
             (cfg (make-config :batch-size 7
                               :max-retries 11
                               :http-fn http-fn
                               :traces-forward-auth nil))
             (resolved (sigil-cl::resolve-config-from-env
                        cfg :env-fn (env-from-alist '(("SIGIL_AGENT_NAME" . "x"))))))
        (check "resolver preserves batch-size"
               (= (sigil-cl::config-batch-size resolved) 7))
        (check "resolver preserves max-retries"
               (= (sigil-cl::config-max-retries resolved) 11))
        (check "resolver preserves http-fn"
               (eq (sigil-cl::config-http-fn resolved) http-fn))
        (check "resolver preserves traces-forward-auth=nil"
               (null (sigil-cl::config-traces-forward-auth resolved)))))))

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

    ;; Metadata-with-system-prompt: redact message content but keep system prompt
    (multiple-value-bind (client get-requests)
        (make-test-client :capture :metadata-with-system-prompt)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m"
                                          :system-prompt "Be helpful"
                                          :input-messages
                                          (list (make-message
                                                 :role :user
                                                 :parts (list (make-text-part "Hello secret")))
                                                (make-message
                                                 :role :assistant
                                                 :parts (list (make-tool-call-part
                                                               :id "tc1"
                                                               :name "search"
                                                               :input-json "{\"q\":\"secret\"}")))
                                                (make-message
                                                 :role :tool
                                                 :parts (list (make-tool-result-part
                                                               :tool-call-id "tc1"
                                                               :name "search"
                                                               :content "private result")))))))
        (set-result rec :output-messages (list (make-message
                                                :role :assistant
                                                :parts (list (make-text-part "Also secret")))))
        (recorder-end rec)
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client)))))
          (check "metadata-with-system-prompt: system_prompt populated"
                 (equal (jget gen "system_prompt") "Be helpful"))
          (check "metadata-with-system-prompt: input present" (jget gen "input"))
          (check "metadata-with-system-prompt: output present" (jget gen "output"))
          (let ((user-msg (aref (jget gen "input") 0)))
            (check "metadata-with-system-prompt: user role preserved"
                   (equal (jget user-msg "role") "MESSAGE_ROLE_USER"))
            (check "metadata-with-system-prompt: text redacted"
                   (equal (jget (aref (jget user-msg "parts") 0) "text") "")))
          (let* ((asst-msg (aref (jget gen "input") 1))
                 (tc-part (aref (jget asst-msg "parts") 0)))
            (check "metadata-with-system-prompt: tool_call shape preserved"
                   (and (equal (jget* tc-part "tool_call" "id") "tc1")
                        (equal (jget* tc-part "tool_call" "name") "search")))
            (check "metadata-with-system-prompt: tool_call input_json redacted"
                   (equal (jget* tc-part "tool_call" "input_json") "")))
          (let* ((tool-msg (aref (jget gen "input") 2))
                 (tr-part (aref (jget tool-msg "parts") 0)))
            (check "metadata-with-system-prompt: tool_result shape preserved"
                   (and (equal (jget* tr-part "tool_result" "tool_call_id") "tc1")
                        (equal (jget* tr-part "tool_result" "name") "search")))
            (check "metadata-with-system-prompt: tool_result content redacted"
                   (equal (jget* tr-part "tool_result" "content") "")))
          (let ((out-msg (aref (jget gen "output") 0)))
            (check "metadata-with-system-prompt: output role preserved"
                   (equal (jget out-msg "role") "MESSAGE_ROLE_ASSISTANT"))
            (check "metadata-with-system-prompt: output text redacted"
                   (equal (jget (aref (jget out-msg "parts") 0) "text") ""))))))

    ;; No-tool-content: full generation content but redact tool spans
    (multiple-value-bind (client get-requests) (make-test-client :capture :no-tool-content)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "test" :model-name "m"
                                          :system-prompt "Be helpful"
                                          :input-messages (list (make-message
                                                                 :role :user
                                                                 :parts (list (make-text-part "Hello")))))))
        (set-result rec :output-messages (list (make-message
                                                :role :assistant
                                                :parts (list (make-text-part "Hi")))))
        (set-call-error rec "rate limit")
        (recorder-end rec)
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client)))))
          (check "no-tool-content: system prompt included"
                 (equal (jget gen "system_prompt") "Be helpful"))
          (check "no-tool-content: input text kept"
                 (equal (jget (aref (jget (aref (jget gen "input") 0) "parts") 0) "text")
                        "Hello"))
          (check "no-tool-content: output text kept"
                 (equal (jget (aref (jget (aref (jget gen "output") 0) "parts") 0) "text")
                        "Hi"))
          (check "no-tool-content: call_error not redacted"
                 (equal (jget gen "call_error") "rate limit")))))

    ;; No-tool-content: tool execution span args/results redacted
    (multiple-value-bind (client get-requests)
        (make-test-client :capture :no-tool-content :generation-enabled nil)
      (declare (ignore get-requests))
      (let ((*trace-context* (list :trace-id "parent-trace" :span-id "parent-span")))
        (let ((rec (start-tool-execution client
                     :tool-name "search"
                     :tool-call-id "tc-1"
                     :tool-type "function")))
          (set-result rec :arguments "{\"q\":\"secret\"}"
                          :result "found something private"
                          :duration-seconds 0.5d0)
          (recorder-end rec)
          (let* ((span (first (sigil-cl::queue-drain-all
                               (sigil-cl::client-trace-queue client))))
                 (attrs (jget span "attributes"))
                 (args-attr (find "gen_ai.tool.call.arguments" (coerce attrs 'list)
                                  :key (lambda (a) (jget a "key")) :test #'equal))
                 (result-attr (find "gen_ai.tool.call.result" (coerce attrs 'list)
                                    :key (lambda (a) (jget a "key")) :test #'equal))
                 (args-len (find "gen_ai.tool.call.arguments.length" (coerce attrs 'list)
                                 :key (lambda (a) (jget a "key")) :test #'equal)))
            (check "no-tool-content: tool args redacted"
                   (equal (jget* args-attr "value" "stringValue") "<redacted>"))
            (check "no-tool-content: tool result redacted"
                   (equal (jget* result-attr "value" "stringValue") "<redacted>"))
            (check "no-tool-content: arg length still recorded"
                   (= (parse-integer (jget* args-len "value" "intValue"))
                      (length "{\"q\":\"secret\"}")))))))

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
        (set-result rec :input-count 5 :input-tokens 100 :dimensions 1536
                        :duration-seconds 0.2d0)
        (recorder-end rec)
        (let ((span (first (sigil-cl::queue-drain-all
                            (sigil-cl::client-trace-queue client)))))
          (check "embedding span enqueued" (not (null span)))
          (check "embedding span name"
                 (search "embeddings" (jget span "name")))
          (flet ((span-int-attr (span key)
                   (let ((found nil))
                     (loop for a across (jget span "attributes")
                           when (equal (jget a "key") key)
                             do (setf found (jget* a "value" "intValue")))
                     found)))
            (check "embedding span dimension.count"
                   (equal (span-int-attr span "gen_ai.embeddings.dimension.count")
                          "1536"))))))

    ;; Embedding without dimensions omits the attribute
    (multiple-value-bind (client get-requests) (make-test-client :generation-enabled nil)
      (declare (ignore get-requests))
      (let ((rec (start-embedding client
                   :model-provider "openai"
                   :model-name "text-embedding-3-small")))
        (set-result rec :input-count 5 :input-tokens 100)
        (recorder-end rec)
        (let ((span (first (sigil-cl::queue-drain-all
                            (sigil-cl::client-trace-queue client)))))
          (check "embedding span omits dimension.count when unset"
                 (loop for a across (jget span "attributes")
                       never (equal (jget a "key")
                                    "gen_ai.embeddings.dimension.count"))))))))

(defun run-workflow-step-tests ()
  (with-test-suite ("WorkflowStep")
    ;; Util: generate-workflow-step-id
    (let ((id (sigil-cl::generate-workflow-step-id)))
      (check "wfs id starts with wfs_"
             (and (stringp id) (eql 0 (search "wfs_" id))))
      (check "wfs id unique"
             (not (equal id (sigil-cl::generate-workflow-step-id)))))

    ;; Auto-generated IDs and started-at
    (multiple-value-bind (client get-requests)
        (make-test-client :workflow-steps-enabled t)
      (declare (ignore get-requests))
      (let ((rec (sigil-cl::start-workflow-step client
                   :conversation-id "conv-1"
                   :step-name "classify")))
        (check "wfs has step-id" (search "wfs_" (sigil-cl::wfs-rec-step-id rec)))
        (check "wfs has 32-hex trace-id"
               (= (length (sigil-cl::wfs-rec-trace-id rec)) 32))
        (check "wfs has 16-hex span-id"
               (= (length (sigil-cl::wfs-rec-span-id rec)) 16))
        (check "wfs has started-at"
               (and (stringp (sigil-cl::recorder-started-at rec))
                    (plusp (length (sigil-cl::recorder-started-at rec)))))))

    ;; set-result mutates slots
    (multiple-value-bind (client get-requests)
        (make-test-client :workflow-steps-enabled t)
      (declare (ignore get-requests))
      (let ((rec (sigil-cl::start-workflow-step client
                   :conversation-id "conv-1"
                   :step-name "classify")))
        (let ((in (jobj "x" 1))
              (out (jobj "y" 2))
              (md (jobj "k" "v")))
          (sigil-cl::set-result rec
                                :input-state in
                                :output-state out
                                :metadata md
                                :tags '(("env" . "prod"))
                                :linked-generation-ids '("gen_a" "gen_b")
                                :parent-step-ids '("wfs_prev")
                                :duration-seconds 0.25d0)
          (check "set-result: input-state" (eq (sigil-cl::wfs-rec-input-state rec) in))
          (check "set-result: output-state" (eq (sigil-cl::wfs-rec-output-state rec) out))
          (check "set-result: metadata" (eq (sigil-cl::wfs-rec-metadata rec) md))
          (check "set-result: tags"
                 (equal (sigil-cl::wfs-rec-tags rec) '(("env" . "prod"))))
          (check "set-result: linked-generation-ids"
                 (equal (sigil-cl::wfs-rec-linked-generation-ids rec)
                        '("gen_a" "gen_b")))
          (check "set-result: parent-step-ids"
                 (equal (sigil-cl::wfs-rec-parent-step-ids rec) '("wfs_prev")))
          (check "set-result: duration-seconds"
                 (= (sigil-cl::wfs-rec-duration-seconds rec) 0.25d0)))))

    ;; recorder-end enqueues to both queues
    (multiple-value-bind (client get-requests)
        (make-test-client :workflow-steps-enabled t)
      (declare (ignore get-requests))
      (let ((rec (sigil-cl::start-workflow-step client
                   :conversation-id "conv-1"
                   :step-name "classify"
                   :framework "custom"
                   :agent-name "agent-1"
                   :agent-version "v1"
                   :parent-step-ids '("wfs_prev")
                   :linked-generation-ids '("gen_a"))))
        (sigil-cl::recorder-end rec)
        ;; Workflow queue payload
        (let ((wfs (first (sigil-cl::queue-drain-all
                            (sigil-cl::client-workflow-queue client)))))
          (check "wfs payload enqueued" (not (null wfs)))
          (check "wfs payload id non-empty"
                 (and (stringp (jget wfs "id")) (plusp (length (jget wfs "id")))))
          (check "wfs payload conversation_id"
                 (equal (jget wfs "conversation_id") "conv-1"))
          (check "wfs payload step_name"
                 (equal (jget wfs "step_name") "classify"))
          (check "wfs payload framework"
                 (equal (jget wfs "framework") "custom"))
          (check "wfs payload agent_name"
                 (equal (jget wfs "agent_name") "agent-1"))
          (check "wfs payload agent_version"
                 (equal (jget wfs "agent_version") "v1"))
          (check "wfs payload started_at present"
                 (and (stringp (jget wfs "started_at"))
                      (= (length (jget wfs "started_at")) 20)))
          (check "wfs payload completed_at present"
                 (and (stringp (jget wfs "completed_at"))
                      (= (length (jget wfs "completed_at")) 20)))
          (check "wfs payload trace_id present"
                 (= (length (jget wfs "trace_id")) 32))
          (check "wfs payload span_id present"
                 (= (length (jget wfs "span_id")) 16))
          (check "wfs payload parent_step_ids vector"
                 (and (vectorp (jget wfs "parent_step_ids"))
                      (equal (aref (jget wfs "parent_step_ids") 0) "wfs_prev")))
          (check "wfs payload linked_generation_ids vector"
                 (and (vectorp (jget wfs "linked_generation_ids"))
                      (equal (aref (jget wfs "linked_generation_ids") 0) "gen_a"))))
        ;; Trace queue span
        (let ((span (first (sigil-cl::queue-drain-all
                             (sigil-cl::client-trace-queue client)))))
          (check "wfs span enqueued" (not (null span)))
          (check "wfs span name"
                 (equal (jget span "name") "workflow_step classify"))
          (check "wfs span kind=INTERNAL" (= (jget span "kind") 1))
          (check "wfs span no parent (top-level)"
                 (null (jget span "parentSpanId")))
          (let* ((attrs (coerce (jget span "attributes") 'list))
                 (op (find "gen_ai.operation.name" attrs
                           :key (lambda (a) (jget a "key")) :test #'equal))
                 (sname (find "sigil.workflow.step.name" attrs
                              :key (lambda (a) (jget a "key")) :test #'equal))
                 (sid (find "sigil.workflow.step.id" attrs
                            :key (lambda (a) (jget a "key")) :test #'equal))
                 (fw (find "sigil.workflow.framework" attrs
                           :key (lambda (a) (jget a "key")) :test #'equal)))
            (check "wfs span op-name=workflow_step"
                   (equal (jget* op "value" "stringValue") "workflow_step"))
            (check "wfs span step.name attr"
                   (equal (jget* sname "value" "stringValue") "classify"))
            (check "wfs span step.id attr"
                   (and sid
                        (search "wfs_" (jget* sid "value" "stringValue"))))
            (check "wfs span framework attr"
                   (equal (jget* fw "value" "stringValue") "custom"))))))

    ;; Endpoint disabled: payload not enqueued, span still emitted
    (multiple-value-bind (client get-requests)
        (make-test-client :workflow-steps-enabled nil :traces-enabled t)
      (declare (ignore get-requests))
      (let ((rec (sigil-cl::start-workflow-step client
                   :conversation-id "c"
                   :step-name "noop")))
        (sigil-cl::recorder-end rec)
        (check "disabled: no workflow payload"
               (sigil-cl::queue-empty-p (sigil-cl::client-workflow-queue client)))
        (check "disabled: traces still emitted"
               (not (sigil-cl::queue-empty-p (sigil-cl::client-trace-queue client))))))

    ;; Both disabled: nothing enqueued
    (multiple-value-bind (client get-requests)
        (make-test-client :workflow-steps-enabled nil :traces-enabled nil)
      (declare (ignore get-requests))
      (let ((rec (sigil-cl::start-workflow-step client
                   :conversation-id "c"
                   :step-name "noop")))
        (sigil-cl::recorder-end rec)
        (check "both disabled: no workflow payload"
               (sigil-cl::queue-empty-p (sigil-cl::client-workflow-queue client)))
        (check "both disabled: no trace span"
               (sigil-cl::queue-empty-p (sigil-cl::client-trace-queue client)))))

    ;; Error path: error-message via set-call-error
    (multiple-value-bind (client get-requests)
        (make-test-client :workflow-steps-enabled t :capture :full)
      (declare (ignore get-requests))
      (let ((rec (sigil-cl::start-workflow-step client
                   :conversation-id "c"
                   :step-name "boom")))
        (sigil-cl::set-call-error rec "kaboom")
        (sigil-cl::recorder-end rec)
        (let ((wfs (first (sigil-cl::queue-drain-all
                            (sigil-cl::client-workflow-queue client))))
              (span (first (sigil-cl::queue-drain-all
                             (sigil-cl::client-trace-queue client)))))
          (check "error path: wfs error field"
                 (equal (jget wfs "error") "kaboom"))
          (check "error path: span status code = 2 (ERROR)"
                 (= (jget* span "status" "code") 2)))))

    ;; Capture mode :metadata-only redacts error and omits state fields
    (dolist (mode '(:metadata-only :metadata-with-system-prompt))
      (multiple-value-bind (client get-requests)
          (make-test-client :workflow-steps-enabled t :capture mode)
        (declare (ignore get-requests))
        (let ((rec (sigil-cl::start-workflow-step client
                     :conversation-id "c"
                     :step-name "s"
                     :input-state (jobj "secret" "in")
                     :output-state (jobj "secret" "out"))))
          (sigil-cl::set-call-error rec "boom")
          (sigil-cl::recorder-end rec)
          (let ((wfs (first (sigil-cl::queue-drain-all
                              (sigil-cl::client-workflow-queue client)))))
            (check (format nil "~a: error redacted" mode)
                   (equal (jget wfs "error") "<redacted>"))
            (check (format nil "~a: input_state omitted" mode)
                   (null (nth-value 1 (gethash "input_state" wfs))))
            (check (format nil "~a: output_state omitted" mode)
                   (null (nth-value 1 (gethash "output_state" wfs))))))))

    ;; Capture mode :full keeps state and error verbatim
    (multiple-value-bind (client get-requests)
        (make-test-client :workflow-steps-enabled t :capture :full)
      (declare (ignore get-requests))
      (let ((in (jobj "x" 1))
            (out (jobj "y" 2)))
        (let ((rec (sigil-cl::start-workflow-step client
                     :conversation-id "c"
                     :step-name "s"
                     :input-state in
                     :output-state out)))
          (sigil-cl::set-call-error rec "boom")
          (sigil-cl::recorder-end rec)
          (let ((wfs (first (sigil-cl::queue-drain-all
                              (sigil-cl::client-workflow-queue client)))))
            (check ":full: error verbatim" (equal (jget wfs "error") "boom"))
            (check ":full: input_state present" (eq (jget wfs "input_state") in))
            (check ":full: output_state present" (eq (jget wfs "output_state") out))))))

    ;; Nested generation parents under workflow span
    (multiple-value-bind (client get-requests)
        (make-test-client :workflow-steps-enabled t :traces-enabled t
                          :generation-enabled t)
      (declare (ignore get-requests))
      (let ((wfs-span-id nil)
            (wfs-trace-id nil))
        (with-workflow-step (wfs client :conversation-id "conv-1"
                                         :step-name "wrapper")
          (setf wfs-span-id (sigil-cl::wfs-rec-span-id wfs))
          (setf wfs-trace-id (sigil-cl::wfs-rec-trace-id wfs))
          (with-generation (gen client :mode :sync :model-provider "p" :model-name "m")
            (sigil-cl::set-result gen :usage (make-token-usage :input 1 :output 1))))
        ;; Drain trace queue: we expect at least 2 spans (gen + wfs)
        (let* ((spans (sigil-cl::queue-drain-all
                        (sigil-cl::client-trace-queue client)))
               (gen-span (find-if (lambda (s)
                                    (search "generateText" (jget s "name")))
                                  spans))
               (wfs-span (find-if (lambda (s)
                                    (search "workflow_step" (jget s "name")))
                                  spans)))
          (check "nesting: gen span found" (not (null gen-span)))
          (check "nesting: wfs span found" (not (null wfs-span)))
          (check "nesting: gen span trace-id matches workflow"
                 (equal (jget gen-span "traceId") wfs-trace-id))
          (check "nesting: gen span parentSpanId is workflow span-id"
                 (equal (jget gen-span "parentSpanId") wfs-span-id))
          (check "nesting: workflow span has no parent"
                 (null (jget wfs-span "parentSpanId"))))))

    ;; Standalone generation: independent trace-id, no parent
    (multiple-value-bind (client get-requests)
        (make-test-client :traces-enabled t :generation-enabled nil)
      (declare (ignore get-requests))
      (let ((rec (sigil-cl::start-generation client :mode :sync
                                              :model-provider "p"
                                              :model-name "m")))
        (sigil-cl::recorder-end rec)
        (let ((span (first (sigil-cl::queue-drain-all
                             (sigil-cl::client-trace-queue client)))))
          (check "standalone: gen span has no parent"
                 (null (jget span "parentSpanId")))
          (check "standalone: gen trace-id is fresh 32-hex"
                 (= (length (jget span "traceId")) 32)))))

    ;; HTTP path: end-to-end via client-flush
    (multiple-value-bind (client get-requests)
        (make-test-client :workflow-steps-enabled t)
      (with-workflow-step (wfs client :conversation-id "conv-1"
                                       :step-name "classify")
        wfs)
      (client-flush client)
      (let* ((reqs (funcall get-requests))
             (wfs-req (find "workflow-steps:export" reqs
                            :key #'first :test #'search)))
        (check "HTTP: workflow-steps:export URL was called"
               (not (null wfs-req)))
        (check "HTTP: URL exact match"
               (equal (first wfs-req)
                      "http://test-sigil:4318/api/v1/workflow-steps:export"))
        (let ((parsed (jzon:parse (second wfs-req))))
          (check "HTTP: body has workflow_steps array"
                 (vectorp (jget parsed "workflow_steps")))
          (check "HTTP: workflow_steps non-empty"
                 (plusp (length (jget parsed "workflow_steps")))))))))

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
      (check "worker thread nil" (null (sigil-cl::client-worker-thread client))))

    ;; Client config tags promoted onto the generation span as sigil.tag.*
    (flet ((span-attr (span key)
             (let ((found nil))
               (loop for a across (jget span "attributes")
                     when (equal (jget a "key") key)
                       do (setf found (jget* a "value" "stringValue")))
               found)))
      (let ((client (make-client
                     (make-config :traces-endpoint "http://x/v1/traces"
                                  :traces-enabled t
                                  :tags '(("env" . "prod") (1 . 2) ("team" . "ai")))
                     :env-fn (constantly nil))))
        (let ((rec (start-generation client :mode :sync
                                            :model-provider "openai" :model-name "gpt-4")))
          (set-result rec :usage (make-token-usage :input 10 :output 5))
          (recorder-end rec))
        (let ((span (first (sigil-cl::queue-drain-all
                            (sigil-cl::client-trace-queue client)))))
          (check "span: sigil.tag.env present"
                 (equal (span-attr span "sigil.tag.env") "prod"))
          (check "span: sigil.tag.team present"
                 (equal (span-attr span "sigil.tag.team") "ai"))
          (check "span: invalid tag cons skipped"
                 (null (span-attr span "sigil.tag.1"))))))

    ;; Cache-write span attr renamed; JSON payload keeps cache_creation_input_tokens
    (flet ((span-int-attr (span key)
             (let ((found nil))
               (loop for a across (jget span "attributes")
                     when (equal (jget a "key") key)
                       do (setf found (jget* a "value" "intValue")))
               found)))
      (multiple-value-bind (client get-requests) (make-test-client)
        (declare (ignore get-requests))
        (let ((rec (start-generation client :mode :sync
                                            :model-provider "anthropic" :model-name "claude")))
          (set-result rec :usage (make-token-usage :input 10 :output 5 :cache-creation 7))
          (recorder-end rec))
        (let ((gen (first (sigil-cl::queue-drain-all
                           (sigil-cl::client-generation-queue client))))
              (span (first (sigil-cl::queue-drain-all
                            (sigil-cl::client-trace-queue client)))))
          (check "span: cache_write_input_tokens set"
                 (equal (span-int-attr span "gen_ai.usage.cache_write_input_tokens") "7"))
          (check "span: no cache_creation_input_tokens attr"
                 (null (span-int-attr span "gen_ai.usage.cache_creation_input_tokens")))
          (check "payload: keeps cache_creation_input_tokens"
                 (= (jget* gen "usage" "cache_creation_input_tokens") 7)))))))

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
      (check "trace-context nil after" (null *trace-context*)))

    ;; with-span: gen_ai.agent.name uses agent-name when set, falling back to service-name
    (flet ((span-attr (span key)
             (let ((found nil))
               (loop for a across (jget span "attributes")
                     when (equal (jget a "key") key)
                       do (setf found (jget* a "value" "stringValue")))
               found)))
      ;; agent-name/version set -> win over service-name/version
      (let ((client (make-client (make-config :traces-endpoint "http://x/v1/traces"
                                              :traces-enabled t
                                              :service-name "my-app"
                                              :service-version "1.0"
                                              :agent-name "router"
                                              :agent-version "2.0")
                                 :env-fn (constantly nil))))
        (with-span (client "test-op")
          nil)
        (let* ((spans (sigil-cl::queue-drain-all (sigil-cl::client-trace-queue client)))
               (span (first spans)))
          (check "with-span: agent-name wins over service-name"
                 (equal (span-attr span "gen_ai.agent.name") "router"))
          (check "with-span: agent-version wins over service-version"
                 (equal (span-attr span "gen_ai.agent.version") "2.0"))))
      ;; agent-name/version unset -> service-name/version fallback (back-compat)
      (let ((client (make-client (make-config :traces-endpoint "http://x/v1/traces"
                                              :traces-enabled t
                                              :service-name "legacy-app"
                                              :service-version "0.9")
                                 :env-fn (constantly nil))))
        (with-span (client "test-op")
          nil)
        (let* ((spans (sigil-cl::queue-drain-all (sigil-cl::client-trace-queue client)))
               (span (first spans)))
          (check "with-span: falls back to service-name"
                 (equal (span-attr span "gen_ai.agent.name") "legacy-app"))
          (check "with-span: falls back to service-version"
                 (equal (span-attr span "gen_ai.agent.version") "0.9"))))

      ;; Config tags reach every span kind: generation, embedding, tool, workflow-step, with-span
      (let ((client (make-client
                     (make-config :traces-endpoint "http://x/v1/traces"
                                  :traces-enabled t
                                  :workflow-steps-endpoint "http://x/api/v1/workflow-steps:export"
                                  :workflow-steps-enabled t
                                  :tags '(("env" . "prod")))
                     :env-fn (constantly nil))))
        (with-generation (g client :mode :sync :model-provider "openai" :model-name "gpt-4")
          g)
        (with-embedding (e client :model-provider "openai" :model-name "text-embedding-3-small")
          e)
        (with-tool-execution (tr client :tool-name "search" :tool-call-id "tc1")
          tr)
        (with-workflow-step (w client :step-name "step-1")
          w)
        (with-span (client "rerank")
          nil)
        (let ((spans (sigil-cl::queue-drain-all (sigil-cl::client-trace-queue client))))
          (check "all span kinds enqueued" (= (length spans) 5))
          (check "every span carries sigil.tag.env"
                 (every (lambda (s) (equal (span-attr s "sigil.tag.env") "prod"))
                        spans)))))

    ;; --- Telemetry context: capture and rebind ---
    ;;
    ;; Sentinels rather than real values: nothing here dereferences either
    ;; special, and EQ then proves the capture carried the object itself.
    (let* ((captured-run (list :captured-run))
           (captured-trace (list :trace-id "trace-parent" :span-id "span-parent"))
           (context (let ((*experiment-run* captured-run)
                          (*trace-context* captured-trace))
                      (capture-telemetry-context)))
           (outer-run (list :outer-run))
           (outer-trace (list :outer-trace)))
      (let ((*experiment-run* outer-run)
            (*trace-context* outer-trace))
        (with-telemetry-context (context)
          (check "captured context rebinds *experiment-run* by identity"
                 (eq *experiment-run* captured-run))
          (check "captured context rebinds *trace-context* by identity"
                 (eq *trace-context* captured-trace)))
        (check "with-telemetry-context restores the surrounding *experiment-run*"
               (eq *experiment-run* outer-run))
        (check "with-telemetry-context restores the surrounding *trace-context*"
               (eq *trace-context* outer-trace))))

    ;; A NIL context is the no-run case, not an error.
    (let ((outer-run (list :outer-run))
          (outer-trace (list :outer-trace))
          (inside-run :unset)
          (inside-trace :unset)
          (signaled nil))
      (let ((*experiment-run* outer-run)
            (*trace-context* outer-trace))
        (handler-case
            (with-telemetry-context (nil)
              (setf inside-run *experiment-run*
                    inside-trace *trace-context*))
          (error () (setf signaled t)))
        (check "a NIL telemetry context binds both specials to NIL"
               (and (null inside-run) (null inside-trace)))
        (check "a NIL telemetry context signals nothing" (not signaled))
        (check "a NIL telemetry context restores both surrounding bindings"
               (and (eq *experiment-run* outer-run)
                    (eq *trace-context* outer-trace)))))

    ;; The context form is evaluated exactly once.
    (let ((evaluations 0))
      (flet ((context ()
               (incf evaluations)
               (list :experiment-run nil :trace-context nil)))
        (with-telemetry-context ((context))
          nil)
        (check "with-telemetry-context evaluates its context form once"
               (= evaluations 1))))

    ;; --- telemetry-context-thunk captures at wrap time, on the caller ---
    (let* ((wrap-run (list :wrap-run))
           (wrap-trace (list :wrap-trace))
           (thunk (let ((*experiment-run* wrap-run)
                        (*trace-context* wrap-trace))
                    (telemetry-context-thunk
                     (lambda () (list *experiment-run* *trace-context*)))))
           (seen nil))
      ;; Called outside the wrapping scope and under different bindings: what
      ;; the closure sees can only have come from wrap time.
      (let ((*experiment-run* (list :call-run))
            (*trace-context* (list :call-trace)))
        (setf seen (funcall thunk)))
      (check "telemetry-context-thunk binds the run captured at wrap time"
             (eq (first seen) wrap-run))
      (check "telemetry-context-thunk binds the trace captured at wrap time"
             (eq (second seen) wrap-trace))
      ;; And on a child thread, which starts at the global NIL values.
      (let ((from-child (bt2:join-thread
                         (bt2:make-thread thunk :name "sigil-test-wrap-time"))))
        (check "telemetry-context-thunk carries the wrap-time run onto a child thread"
               (eq (first from-child) wrap-run))
        (check "telemetry-context-thunk carries the wrap-time trace onto a child thread"
               (eq (second from-child) wrap-trace))))

    ;; --- The run and the trace cross a thread the caller spawns ---
    ;;
    ;; The unwrapped child is the control: it proves the tracking below comes
    ;; from the propagated context and not from ambient state.
    (flet ((exported-generation (calls id)
             (let ((found nil))
               (dolist (call (reverse (cdr calls)) found)
                 (when (search "generations:export" (second call))
                   (loop for gen across (jget (payload call) "generations")
                         when (equal (jget gen "id") id)
                           do (setf found gen)))))))
      (let ((calls (cons :calls nil))
            (run-object nil)
            (conversation-id nil)
            (child-rec nil))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :traces-enabled nil
                              :http-fn (routed-http calls "exp-threads"))
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-threads" :name "threads")
            (setf run-object run)
            (let ((*trace-context* (list :trace-id "trace-parent"
                                         :span-id "span-parent")))
              (let ((context (capture-telemetry-context)))
                (setf child-rec
                      (bt2:join-thread
                       (bt2:make-thread
                        (lambda ()
                          (with-telemetry-context (context)
                            (let ((rec (start-generation
                                        client
                                        :mode :sync
                                        :generation-id "generation-child"
                                        :model-provider "openai"
                                        :model-name "gpt-4")))
                              (recorder-end rec)
                              rec)))
                        :name "sigil-test-child-generation")))
                ;; Control: same thread constructor, no context carried.
                (bt2:join-thread
                 (bt2:make-thread
                  (lambda ()
                    (recorder-end (start-generation client
                                                    :mode :sync
                                                    :generation-id "generation-orphan"
                                                    :model-provider "openai"
                                                    :model-name "gpt-4")))
                  :name "sigil-test-orphan-generation"))))
            (setf conversation-id (experiment-run-active-conversation-id run)))
          (client-flush client))
        (check "child-thread generation is tracked by the run"
               (equal (experiment-run-produced-generation-ids run-object)
                      (list "generation-child")))
        (check "an uncarried child-thread generation is not tracked"
               (not (member "generation-orphan"
                            (experiment-run-produced-generation-ids run-object)
                            :test #'equal)))
        (check "child-thread generation inherits the captured trace id"
               (equal (gen-rec-trace-id child-rec) "trace-parent"))
        (check "child-thread generation parents to the captured span"
               (equal (sigil-cl::gen-rec-parent-span-id child-rec) "span-parent"))
        (let ((carried (exported-generation calls "generation-child"))
              (orphan (exported-generation calls "generation-orphan")))
          (check "child-thread generation carries the run-id tag"
                 (equal (jget* carried "tags" "experiment.run_id") "exp-threads"))
          (check "child-thread generation carries the run's conversation id"
                 (equal (jget carried "conversation_id") conversation-id))
          (check "an uncarried child-thread generation has no run-id tag"
                 (null (jget* orphan "tags" "experiment.run_id")))))

      ;; The same hop through telemetry-context-thunk, started from a call
      ;; site that has itself lost the run: only the wrap-time capture can
      ;; explain the registration.
      (let ((calls (cons :calls nil))
            (run-object nil)
            (child-id nil))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :traces-enabled nil
                              :http-fn (routed-http calls "exp-thunk"))
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-thunk" :name "thunk")
            (setf run-object run)
            (let ((wrapped (telemetry-context-thunk
                            (lambda ()
                              (let ((rec (start-generation
                                          client
                                          :mode :sync
                                          :generation-id "generation-wrapped"
                                          :model-provider "openai"
                                          :model-name "gpt-4")))
                                (recorder-end rec)
                                (gen-rec-generation-id rec))))))
              (let ((*experiment-run* nil)
                    (*trace-context* nil))
                (setf child-id
                      (bt2:join-thread
                       (bt2:make-thread wrapped :name "sigil-test-wrapped"))))))
          (client-flush client))
        (check "a wrapped thunk records the generation it was asked to"
               (equal child-id "generation-wrapped"))
        (check "a wrapped thunk registers its generation with the captured run"
               (member "generation-wrapped"
                       (experiment-run-produced-generation-ids run-object)
                       :test #'equal))
        (check "a wrapped thunk's generation carries the captured run-id tag"
               (equal (jget* (exported-generation calls "generation-wrapped")
                             "tags" "experiment.run_id")
                      "exp-thunk")))

      ;; A captured context outlives the scope it was taken in, so a thread
      ;; joined too late reaches a run that already finalized. The generation
      ;; is still tracked - it happened, and dropping the id would lose what a
      ;; later score needs - but the run says the reported counts predate it.
      (let ((calls (cons :calls nil))
            (logged nil)
            (wrapped nil)
            (run-object nil))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :traces-enabled nil
                              :http-fn (routed-http calls "exp-late")
                              :log-fn (lambda (level component message &rest kvs)
                                        (declare (ignore component kvs))
                                        (push (list level message) logged)))
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-late" :name "late")
            (setf run-object run)
            (setf wrapped
                  (telemetry-context-thunk
                   (lambda ()
                     (recorder-end (start-generation client
                                                     :mode :sync
                                                     :generation-id "generation-late"
                                                     :model-provider "openai"
                                                     :model-name "gpt-4"))))))
          ;; Only what the late generation emits is counted.
          (setf logged nil)
          (bt2:join-thread (bt2:make-thread wrapped :name "sigil-test-late"))
          (client-flush client))
        (check "a generation recorded after the run finalized is still tracked"
               (member "generation-late"
                       (experiment-run-produced-generation-ids run-object)
                       :test #'equal))
        (let ((warning (find :warn (reverse logged) :key #'first)))
          (check "a generation recorded after the run finalized is reported"
                 (and warning
                      (search "generation-late" (second warning))
                      (search "after the run finalized" (second warning))))))

      ;; The same generation inside the run's scope logs nothing.
      (let ((calls (cons :calls nil))
            (logged nil))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :traces-enabled nil
                              :http-fn (routed-http calls "exp-in-time")
                              :log-fn (lambda (level component message &rest kvs)
                                        (declare (ignore component kvs))
                                        (push (list level message) logged)))
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-in-time" :name "in time")
            (declare (ignore run))
            (setf logged nil)
            (recorder-end (start-generation client
                                            :mode :sync
                                            :generation-id "generation-in-time"
                                            :model-provider "openai"
                                            :model-name "gpt-4"))))
        (check "a generation recorded before the run finalized is not reported"
               (null (find :warn logged :key #'first)))))))

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
;;; Experiment tests
;;; ================================================================

(defun run-experiment-tests ()
  (with-test-suite ("Experiments")
    (labels ((empty-jobj-p (obj)
               (and (hash-table-p obj) (zerop (hash-table-count obj))))
             (ends-with-p (suffix text)
               (let ((pos (search suffix text :from-end t)))
                 (and pos (= (+ pos (length suffix)) (length text)))))
             (finalize-call-p (call)
               (and (eq (first call) :post)
                    (ends-with-p ":finalize" (second call))))
             (trial-create-call-p (call)
               (and (eq (first call) :post)
                    (ends-with-p "/trials" (second call))))
             (trial-patch-call-p (call)
               (eq (first call) :patch))
             (finalize-statuses (calls)
               (loop for call in calls
                     when (finalize-call-p call)
                       collect (jget (payload call) "status"))))

      ;; --- The thread-propagation API is public ---
      (check "*experiment-run* is exported"
             (eq :external (nth-value 1 (find-symbol "*EXPERIMENT-RUN*" :sigil-cl))))
      (check "the telemetry context helpers are exported"
             (every (lambda (name)
                      (eq :external (nth-value 1 (find-symbol name :sigil-cl))))
                    '("CAPTURE-TELEMETRY-CONTEXT"
                      "WITH-TELEMETRY-CONTEXT"
                      "TELEMETRY-CONTEXT-THUNK")))
      (check "*experiment-run* documents that a spawned thread sees NIL"
             (let ((doc (documentation (find-symbol "*EXPERIMENT-RUN*" :sigil-cl)
                                       'variable)))
               (and doc
                    (search "spawned thread" doc)
                    (search "NIL" doc))))

      ;; --- Eval base derivation and URL generation ---
      (let ((cfg (make-config
                  :generation-endpoint "https://sigil.example.test/api/v1/generations:export")))
        (check "eval-base-url derives scheme and host from generation-endpoint"
               (equal (sigil-cl::eval-base-url cfg) "https://sigil.example.test")))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test/ignored/path"
                            :generation-enabled nil
                            :traces-enabled nil)
        (declare (ignore get-requests))
        (check "experiment-url default uses eval base"
               (equal (experiment-url client "exp-prompt-a")
                      "https://sigil.example.test/a/grafana-sigil-app/evaluation/experiments/exp-prompt-a")))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :experiment-url-template "{base}/custom/{run_id}"
                            :generation-enabled nil
                            :traces-enabled nil)
        (declare (ignore get-requests))
        (check "experiment-url template replaces base and run_id"
               (equal (experiment-url client "exp-prompt-a")
                      "https://sigil.example.test/custom/exp-prompt-a")))
      (let* ((array (sigil-cl::%jsonify '("good" "bad")))
             (object (sigil-cl::%jsonify '(("good" . "bad"))))
             (nested (sigil-cl::%jsonify '(("tags" . ("good" "bad"))))))
        (check "%jsonify keeps plain string lists as arrays"
               (and (vectorp array)
                    (= (length array) 2)
                    (equal (aref array 0) "good")
                    (equal (aref array 1) "bad")))
        (check "%jsonify keeps cons alists as objects"
               (and (hash-table-p object)
                    (equal (jget object "good") "bad")))
        (check "%jsonify keeps nested plain lists as arrays"
               (let ((tags (jget nested "tags")))
                 (and (vectorp tags)
                      (equal (aref tags 0) "good")
                      (equal (aref tags 1) "bad")))))
      (let ((plist (sigil-cl::%jsonify '(:question "what?" :n 2))))
        (check "%jsonify turns a plist into an object"
               (and (hash-table-p plist)
                    (equal (jget plist "question") "what?")
                    (eql (jget plist "n") 2))))
      (check "%jsonify keeps an odd-length keyword list an array"
             (vectorp (sigil-cl::%jsonify '(:a :b :c))))
      (check "%jsonify keeps a keyword-free list an array"
             (vectorp (sigil-cl::%jsonify '("a" "b"))))

      ;; stable-id matches the SHA-1-based StableID in the Go/Python SDKs.
      (check "stable-id matches reference SDKs"
             (equal (stable-id "score" "exp-1" "item-1") "score-c612c8bbaefe5e8c"))
      (check "stable-id nil and empty parts match reference"
             (equal (stable-id "x" nil "") "x-953efe8f531a5a87"))
      (check "stable-id without parts matches reference"
             (equal (stable-id "x") "x-da39a3ee5e6b4b0d"))
      (check "stable-id multibyte parts match reference"
             (equal (stable-id "u" "héllo") "u-35b5ea45c5e41f78"))

      ;; --- Auth headers and ingest actor ---
      (let ((cfg (make-config :auth-mode :bearer :auth-password "gen-token"
                              :eval-auth-token "eval-token")))
        (check "eval auth token becomes bearer header"
               (equal (cdr (assoc "Authorization" (sigil-cl::build-eval-auth-headers cfg)
                                  :test #'equal))
                      "Bearer eval-token")))
      (let ((cfg (make-config :eval-auth-token "Bearer already")))
        (check "eval auth token keeps existing bearer prefix"
               (equal (cdr (assoc "Authorization" (sigil-cl::build-eval-auth-headers cfg)
                                  :test #'equal))
                      "Bearer already")))
      (let ((cfg (make-config :auth-mode :bearer :auth-password "gen-token")))
        (check "eval auth falls back to generation auth"
               (equal (cdr (assoc "Authorization" (sigil-cl::build-eval-auth-headers cfg)
                                  :test #'equal))
                      "Bearer gen-token")))
      (let ((cfg (make-config :auth-mode :none)))
        (check "default ingest actor is the lisp sdk"
               (equal (config-ingest-actor cfg) "ingest:sdk/lisp"))
        (check "eval auth headers carry the ingest actor"
               (equal (cdr (assoc "X-Agento11y-Ingest-Actor"
                                  (sigil-cl::build-eval-auth-headers cfg) :test #'equal))
                      "ingest:sdk/lisp"))
        (check "score export headers carry the ingest actor"
               (equal (cdr (assoc "X-Agento11y-Ingest-Actor"
                                  (sigil-cl::build-score-export-headers cfg) :test #'equal))
                      "ingest:sdk/lisp"))
        (check "no request uses the alternate actor header spelling"
               (and (null (assoc "X-Sigil-Ingest-Actor"
                                 (sigil-cl::build-eval-auth-headers cfg) :test #'string-equal))
                    (null (assoc "X-Sigil-Ingest-Actor"
                                 (sigil-cl::build-score-export-headers cfg)
                                 :test #'string-equal)))))
      (let ((cfg (make-config :auth-mode :none :ingest-actor "")))
        (check "blank ingest actor sends no actor header"
               (null (assoc "X-Agento11y-Ingest-Actor"
                            (sigil-cl::build-eval-auth-headers cfg) :test #'equal))))
      ;; Every lifecycle request carries the configured actor value.
      (let ((actor-headers nil))
        (multiple-value-bind (client get-requests)
            (make-test-client
             :eval-endpoint "https://sigil.example.test"
             :generation-enabled nil
             :traces-enabled nil
             :ingest-actor "ingest:sdk/custom"
             :http-fn (lambda (url &key method headers content &allow-other-keys)
                        (declare (ignore method))
                        (push (cons url (cdr (assoc "X-Agento11y-Ingest-Actor" headers
                                                    :test #'equal)))
                              actor-headers)
                        (cond
                          ((search "scores:export" url)
                           (values (make-score-response content) 202))
                          (t (values (run-http-response "exp-actor" "running") 200)))))
          (declare (ignore get-requests))
          (upsert-experiment-run client :experiment-id "exp-actor" :name "actor")
          (create-trial client "exp-actor" :trial-id "trial-1" :test-case-id "case-1")
          (finalize-trial client "exp-actor" "trial-1" :status "completed")
          (export-scores client (list (jobj "score_id" "s1"
                                            "trial_id" "trial-1"
                                            "evaluator_id" "judge"
                                            "evaluator_version" "1"
                                            "score_key" "quality"
                                            "value" 1)))
          (finalize-experiment-run client "exp-actor" :status "completed")
          (check "all five lifecycle requests send the configured actor"
                 (and (= (length actor-headers) 5)
                      (every (lambda (pair) (equal (cdr pair) "ingest:sdk/custom"))
                             actor-headers)))))

      ;; Score export is a tenant ingest write: generation host and generation
      ;; auth, never the eval token or eval endpoint.
      (let ((score-call nil)
            (upsert-call nil))
        (multiple-value-bind (client get-requests)
            (make-test-client
             :eval-endpoint "https://eval.example.test"
             :eval-auth-token "eval-tok"
             :generation-enabled nil
             :traces-enabled nil
             :http-fn (lambda (url &key method headers content &allow-other-keys)
                        (declare (ignore method))
                        (cond
                          ((search "scores:export" url)
                           (setf score-call (list url headers))
                           (values (make-score-response content) 202))
                          ((search "experiment-runs" url)
                           (setf upsert-call (list url headers))
                           (values (run-http-response "exp-auth" "running") 200))
                          (t (values "{}" 200)))))
          (declare (ignore get-requests))
          (upsert-experiment-run client :experiment-id "exp-auth" :name "auth")
          (let ((run (sigil-cl::%make-experiment-run :client client
                                                    :run-id "exp-auth"
                                                    :name "auth")))
            (experiment-run-add-scores
             run
             (list (make-score :evaluator-id "judge" :evaluator-version "1"
                               :score-key "quality" :value 1))
             :generation-ids '("gen-auth")))
          (check "control-plane request uses eval token"
                 (equal (cdr (assoc "Authorization" (second upsert-call) :test #'equal))
                        "Bearer eval-tok"))
          (check "control-plane request uses eval host"
                 (eql 0 (search "https://eval.example.test/" (first upsert-call))))
          (check "score export uses generation auth"
                 (equal (cdr (assoc "Authorization" (second score-call) :test #'equal))
                        "Bearer test-token"))
          (check "score export uses generation host"
                 (eql 0 (search "http://test-sigil:4318/api/v1/scores:export"
                                (first score-call))))))

      ;; --- Transport parsing, typed errors, and retry ---
      (let ((cfg (make-config :auth-mode :none
                              :max-retries 1
                              :http-fn (lambda (url &key method headers content &allow-other-keys)
                                         (declare (ignore url method headers content))
                                         (values "{\"ok\":true}" 200)))))
        (check "request-eval-json parses 2xx JSON body"
               (eq (jget (sigil-cl::request-eval-json cfg :get "https://x" nil "ok")
                         "ok")
                   t)))
      (let ((cfg (make-config :auth-mode :none
                              :max-retries 1
                              :http-fn (lambda (url &key method headers content &allow-other-keys)
                                         (declare (ignore url method headers content))
                                         (values "" 204)))))
        (check "request-eval-json returns empty object for empty 2xx"
               (empty-jobj-p (sigil-cl::request-eval-json cfg :get "https://x" nil "empty"))))
      (dolist (case '((400 sigil-validation-error)
                      (422 sigil-validation-error)
                      (404 sigil-not-found-error)
                      (409 sigil-conflict-error)))
        (destructuring-bind (status condition-type) case
          (let ((cfg (make-config :auth-mode :none
                                  :max-retries 1
                                  :http-fn (lambda (url &key method headers content &allow-other-keys)
                                             (declare (ignore url method headers content))
                                             (values "error" status)))))
            (check (format nil "request-eval-json maps ~d to ~a" status condition-type)
                   (signals-condition-p
                    (lambda ()
                      (sigil-cl::request-eval-json cfg :get "https://x" nil "status"))
                    condition-type)))))
      (let ((attempts 0)
            (cfg nil))
        (setf cfg (make-config :auth-mode :none
                               :max-retries 2
                               :initial-backoff-sec 0
                               :max-backoff-sec 0
                               :http-fn (lambda (url &key method headers content &allow-other-keys)
                                          (declare (ignore url method headers content))
                                          (incf attempts)
                                          (if (= attempts 1)
                                              (values "rate limited" 429)
                                              (values "{\"ok\":true}" 200)))))
        (check "request-eval-json retries 429"
               (and (eq (jget (sigil-cl::request-eval-json cfg :get "https://x" nil "retry")
                              "ok")
                        t)
                    (= attempts 2))))
      (let ((attempts 0)
            (cfg nil))
        (setf cfg (make-config :auth-mode :none
                               :max-retries 2
                               :initial-backoff-sec 0
                               :max-backoff-sec 0
                               :http-fn (lambda (url &key method headers content &allow-other-keys)
                                          (declare (ignore url method headers content))
                                          (incf attempts)
                                          (if (= attempts 1)
                                              (values "server error" 500)
                                              (values "{\"ok\":true}" 200)))))
        (check "request-eval-json retries 5xx"
               (and (eq (jget (sigil-cl::request-eval-json cfg :get "https://x" nil "retry")
                              "ok")
                        t)
                    (= attempts 2))))

      ;; A 401 naming actor ownership is a caller error, not a transient one.
      (let ((attempts 0)
            (cfg nil))
        (setf cfg (make-config :auth-mode :none
                               :max-retries 3
                               :initial-backoff-sec 0
                               :max-backoff-sec 0
                               :http-fn (lambda (url &key method headers content &allow-other-keys)
                                          (declare (ignore url method headers content))
                                          (incf attempts)
                                          (values "experiment is owned by another actor" 401))))
        (check "401 actor ownership signals a mismatch condition"
               (signals-condition-p
                (lambda () (sigil-cl::request-eval-json cfg :post "https://x" (jobj) "trial create"))
                'sigil-actor-mismatch-error))
        (check "401 actor ownership is not retried" (= attempts 1)))
      (let ((cfg (make-config :auth-mode :none
                              :max-retries 1
                              :http-fn (lambda (url &key method headers content &allow-other-keys)
                                         (declare (ignore url method headers content))
                                         (values "experiment is owned by another actor" 401)))))
        (check "actor mismatch message names the mismatch"
               (handler-case
                   (progn (sigil-cl::request-eval-json cfg :post "https://x" (jobj) "trial create")
                          nil)
                 (sigil-actor-mismatch-error (e)
                   (and (search "owned by another ingest actor" (sigil-error-message e)) t)))))
      (let ((cfg (make-config :auth-mode :none
                              :max-retries 1
                              :http-fn (lambda (url &key method headers content &allow-other-keys)
                                         (declare (ignore url method headers content))
                                         (values "token expired" 401)))))
        (check "an unrelated 401 stays an export error"
               (signals-condition-p
                (lambda () (sigil-cl::request-eval-json cfg :get "https://x" nil "get"))
                'sigil-export-error)))

      ;; --- 409 conflict classification (ported from errors.py) ---
      (dolist (case '(("expected 12 scores, found 11" :score-count-mismatch)
                      ("score_count mismatch" :score-count-mismatch)
                      ("cannot complete experiment with 2 pending evaluation(s)"
                       :pending-evaluations)
                      ("cannot complete experiment with 2 running trial(s)" :running-trials)
                      ("experiment \"run_1\" is already finalized as completed" :terminal)
                      ("suite draft is already published" :terminal)
                      ("planned_trial_count conflicts with the existing experiment"
                       :immutable-field)
                      ("suite version is not a draft" :immutable-field)
                      ("suite has an open draft" :open-draft)
                      ("something else entirely" :unknown)))
        (destructuring-bind (message kind) case
          (check (format nil "classify-conflict ~a -> ~a" message kind)
                 (eq (classify-conflict message) kind))))
      (check "score-count mismatch is not classified as terminal"
             (and (eq (classify-conflict "expected 12 scores, found 11") :score-count-mismatch)
                  (not (eq (classify-conflict "expected 12 scores, found 11") :terminal))))
      (check "conflict-recoverable-p marks score-count mismatch recoverable"
             (and (conflict-recoverable-p :score-count-mismatch)
                  (conflict-recoverable-p :running-trials)
                  (not (conflict-recoverable-p :terminal))))
      (let ((cfg (make-config :auth-mode :none
                              :max-retries 1
                              :http-fn (lambda (url &key method headers content &allow-other-keys)
                                         (declare (ignore url method headers content))
                                         (values "expected 12 scores, found 11" 409)))))
        (check "409 score-count conflict carries kind and backend text"
               (handler-case
                   (progn (sigil-cl::request-eval-json cfg :post "https://x" (jobj) "finalize")
                          nil)
                 (sigil-conflict-error (e)
                   (and (eq (sigil-conflict-error-kind e) :score-count-mismatch)
                        (search "expected 12 scores, found 11" (sigil-error-message e))
                        t)))))
      (let ((cfg (make-config :auth-mode :none
                              :max-retries 1
                              :http-fn (lambda (url &key method headers content &allow-other-keys)
                                         (declare (ignore url method headers content))
                                         (values "experiment \"run_1\" is already finalized as completed" 409)))))
        (check "409 terminal-run conflict carries the terminal kind"
               (handler-case
                   (progn (sigil-cl::request-eval-json cfg :post "https://x" (jobj) "finalize")
                          nil)
                 (sigil-conflict-error (e)
                   (eq (sigil-conflict-error-kind e) :terminal)))))

      ;; --- Run upsert ---
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client
             :eval-endpoint "https://sigil.example.test/ignored/path"
             :generation-enabled nil
             :traces-enabled nil
             :http-fn (routed-http calls "exp-prompt-a"))
          (declare (ignore get-requests))
          (let ((created (upsert-experiment-run client
                                                :experiment-id "exp-prompt-a"
                                                :name "prompt A"
                                                :tags '("smoke")
                                                :suite-id "suite-1"
                                                :suite-version "v2"
                                                :candidate '(("agent_name" . "agent-a"))
                                                :planned-trial-count 30
                                                :metadata '(("git_sha" . "abc")))))
            (check "upsert-experiment-run returns the parsed run"
                   (equal (jget created "experiment_id") "exp-prompt-a"))
            (let* ((call (first (reverse (cdr calls))))
                   (posted (payload call)))
              (check "run create POSTs to experiment-runs:upsert"
                     (and (eq (first call) :post)
                          (equal (second call)
                                 "https://sigil.example.test/api/v1/experiment-runs:upsert")))
              (check "run payload has experiment_id"
                     (equal (jget posted "experiment_id") "exp-prompt-a"))
              (check "run payload has no run_id"
                     (null (nth-value 1 (gethash "run_id" posted))))
              (check "run payload source is an sdk object"
                     (and (hash-table-p (jget posted "source"))
                          (equal (jget* posted "source" "kind") "sdk")
                          (equal (jget* posted "source" "id") "lisp")))
              (check "run payload keeps name"
                     (equal (jget posted "name") "prompt A"))
              (check "run payload carries suite id and version"
                     (and (equal (jget posted "suite_id") "suite-1")
                          (equal (jget posted "suite_version") "v2")))
              (check "run payload carries candidate and planned_trial_count"
                     (and (equal (jget* posted "candidate" "agent_name") "agent-a")
                          (eql (jget posted "planned_trial_count") 30)))
              (check "run payload omits collection_id and evaluators"
                     (and (null (nth-value 1 (gethash "collection_id" posted)))
                          (null (nth-value 1 (gethash "evaluators" posted)))))))))
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-x"))
          (declare (ignore get-requests))
          (check "upsert rejects a blank name before any request"
                 (and (signals-condition-p
                       (lambda () (upsert-experiment-run client :experiment-id "x" :name "  "))
                       'sigil-validation-error)
                      (null (cdr calls))))
          (check "upsert rejects a negative planned_trial_count before any request"
                 (and (signals-condition-p
                       (lambda () (upsert-experiment-run client :name "x"
                                                         :planned-trial-count -1))
                       'sigil-validation-error)
                      (null (cdr calls))))))

      ;; --- Run finalization ---
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-fin"))
          (declare (ignore get-requests))
          (finalize-experiment-run client "exp-fin" :status "succeeded" :score-count 3)
          (let* ((call (first (reverse (cdr calls))))
                 (posted (payload call)))
            (check "finalize POSTs the finalize route"
                   (and (eq (first call) :post)
                        (equal (second call)
                               "https://sigil.example.test/api/v1/experiment-runs/exp-fin:finalize")))
            (check "finalize normalizes succeeded to completed"
                   (equal (jget posted "status") "completed"))
            (check "finalize payload carries source and score_count"
                   (and (equal (jget* posted "source" "kind") "sdk")
                        (eql (jget posted "score_count") 3))))))
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-fin"))
          (declare (ignore get-requests))
          (finalize-experiment-run client "exp-fin" :status "failed" :error "boom")
          (let ((posted (payload (first (reverse (cdr calls))))))
            (check "finalize accepts failed" (equal (jget posted "status") "failed"))
            (check "finalize carries the error text" (equal (jget posted "error") "boom")))
          (setf (cdr calls) nil)
          (check "finalize rejects an unknown status before any request"
                 (and (signals-condition-p
                       (lambda () (finalize-experiment-run client "exp-fin" :status "canceled"))
                       'sigil-validation-error)
                      (null (cdr calls))))
          (check "finalize rejects running before any request"
                 (and (signals-condition-p
                       (lambda () (finalize-experiment-run client "exp-fin" :status "running"))
                       'sigil-validation-error)
                      (null (cdr calls))))))

      ;; --- Read plane stays on /eval/experiments ---
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client
             :eval-endpoint "https://sigil.example.test"
             :generation-enabled nil :traces-enabled nil
             :http-fn (lambda (url &key method headers content &allow-other-keys)
                        (declare (ignore headers))
                        (push (list method url content) (cdr calls))
                        (values "{\"experiment_id\":\"exp-read\"}" 200)))
          (declare (ignore get-requests))
          (get-experiment client "exp-read")
          (get-experiment-report client "exp-read")
          (list-experiment-scores client "exp-read" :limit 10)
          (let ((urls (mapcar #'second (reverse (cdr calls))))
                (methods (mapcar #'first (reverse (cdr calls)))))
            (check "get-experiment reads /eval/experiments/{id}"
                   (equal (first urls)
                          "https://sigil.example.test/api/v1/eval/experiments/exp-read"))
            (check "report reads /eval/experiments/{id}/report"
                   (equal (second urls)
                          "https://sigil.example.test/api/v1/eval/experiments/exp-read/report"))
            (check "scores list reads /eval/experiments/{id}/scores"
                   (eql 0 (search "https://sigil.example.test/api/v1/eval/experiments/exp-read/scores"
                                  (third urls))))
            (check "all three readers use GET" (every (lambda (m) (eq m :get)) methods))
            (check "reads are not redirected to experiment-runs"
                   (notany (lambda (u) (search "experiment-runs" u)) urls)))))

      ;; --- Score serialization ---
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client
             :eval-endpoint "https://sigil.example.test"
             :generation-enabled nil :traces-enabled nil
             :http-fn (routed-http calls "exp-score"))
          (declare (ignore get-requests))
          (let ((accepted (export-scores
                           client
                           (list (jobj "score_id" "score-1"
                                       "generation_id" "gen-1"
                                       "evaluator_id" "judge"
                                       "evaluator_version" "1"
                                       "score_key" "quality"
                                       "experiment_id" "exp-prompt-a"
                                       "value" 0.9)))))
            (check "export-scores returns accepted count" (= accepted 1))
            (let* ((call (first (reverse (cdr calls))))
                   (posted (payload call))
                   (score (aref (jget posted "scores") 0)))
              (check "export-scores POSTs scores path on the generation host"
                     (equal (second call) "http://test-sigil:4318/api/v1/scores:export"))
              (check "numeric score value serializes as number"
                     (let ((value (jget* score "value" "number")))
                       (and (numberp value)
                            (< (abs (- value 0.9)) 0.001))))
              (check "serialized score sends experiment_id"
                     (equal (jget score "experiment_id") "exp-prompt-a"))
              (check "serialized score sends no run_id"
                     (null (nth-value 1 (gethash "run_id" score))))))))
      ;; A trial-anchored score needs no generation.
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client
             :eval-endpoint "https://sigil.example.test"
             :generation-enabled nil :traces-enabled nil
             :http-fn (routed-http calls "exp-score"))
          (declare (ignore get-requests))
          (let ((accepted (export-scores
                           client
                           (list (jobj "score_id" "score-t"
                                       "trial_id" "trial-abc"
                                       "test_case_id" "case-1"
                                       "evaluator_id" "judge"
                                       "evaluator_version" "1"
                                       "score_key" "final"
                                       "experiment_id" "exp-prompt-a"
                                       "grader_conversation_id" "gconv"
                                       "grader_generation_id" "ggen"
                                       "grader_trace_id" "gtrace"
                                       "value" 1)))))
            (check "trial-anchored score exports without a generation" (= accepted 1))
            (let ((score (aref (jget (payload (first (reverse (cdr calls)))) "scores") 0)))
              (check "trial-anchored score sends top-level trial_id"
                     (equal (jget score "trial_id") "trial-abc"))
              (check "trial-anchored score sends no generation_id"
                     (null (nth-value 1 (gethash "generation_id" score))))
              (check "trial-anchored score sends test_case_id"
                     (equal (jget score "test_case_id") "case-1"))
              (check "trial-anchored score sends grader ids"
                     (and (equal (jget score "grader_conversation_id") "gconv"
                                 )
                          (equal (jget score "grader_generation_id") "ggen")
                          (equal (jget score "grader_trace_id") "gtrace")))))))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil)
        (declare (ignore get-requests))
        (check "score without a generation or trial signals validation"
               (signals-condition-p
                (lambda ()
                  (export-scores client
                                 (list (jobj "score_id" "score-anchorless"
                                             "evaluator_id" "judge"
                                             "evaluator_version" "1"
                                             "score_key" "quality"
                                             "value" 1))))
                'sigil-validation-error))
        (check "export-scores without value signals validation"
               (signals-condition-p
                (lambda ()
                  (export-scores client
                                 (list (jobj "score_id" "score-missing"
                                             "generation_id" "gen-1"
                                             "evaluator_id" "judge"
                                             "evaluator_version" "1"
                                             "score_key" "quality"))))
                'sigil-validation-error)))
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client
             :eval-endpoint "https://sigil.example.test"
             :generation-enabled nil :traces-enabled nil
             :http-fn (routed-http calls "exp-rej"
                                   :score-body "{\"results\":[{\"score_id\":\"score-bad\",\"accepted\":false,\"error\":\"bad\"}]}"))
          (declare (ignore get-requests))
          (check "export-scores signals rejected score ids"
                 (signals-condition-p
                  (lambda ()
                    (export-scores client
                                   (list (jobj "score_id" "score-bad"
                                               "generation_id" "gen-1"
                                               "evaluator_id" "judge"
                                               "evaluator_version" "1"
                                               "score_key" "quality"
                                               "value" 0.9))))
                  'sigil-export-error))))

      ;; --- Score metadata and identity on a run ---
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-meta"))
          (declare (ignore get-requests))
          (let ((run (sigil-cl::%make-experiment-run :client client
                                                    :run-id "exp-prompt-a"
                                                    :name "prompt A")))
            (experiment-run-add-scores
             run
             (list (make-score :evaluator-id "judge"
                               :evaluator-version "1"
                               :score-key "quality"
                               :value 0.9))
             :item (jobj "id" "it1"
                         "input" "what is the capital of France?"
                         "expected" "Paris")
             :generation-ids '("gen-1"))
            (let* ((posted (payload (first (reverse (cdr calls)))))
                   (score (aref (jget posted "scores") 0))
                   (metadata (jget score "metadata")))
              (check "score metadata keeps item_id"
                     (equal (jget metadata "item_id") "it1"))
              (check "score metadata does not copy item input"
                     (null (jget metadata "input")))
              (check "score metadata does not copy item expected"
                     (null (jget metadata "expected")))
              (check "score metadata omits trial_id, which is top level now"
                     (null (jget metadata "trial_id")))))))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil)
        (declare (ignore get-requests))
        (let ((run (sigil-cl::%make-experiment-run :client client
                                                  :run-id "exp-prompt-a"
                                                  :name "prompt A")))
          (check "make-score without value signals validation"
                 (signals-condition-p
                  (lambda ()
                    (experiment-run-add-scores
                     run
                     (list (make-score :evaluator-id "judge"
                                       :evaluator-version "1"
                                       :score-key "quality"))
                     :generation-ids '("gen-1")))
                  'sigil-validation-error))))

      ;; --- Upload mode validation ---
      (let ((create-count 0))
        (flet ((http (url &key method headers content &allow-other-keys)
                 (declare (ignore url method headers content))
                 (incf create-count)
                 (values "{}" 200)))
          (multiple-value-bind (client get-requests)
              (make-test-client :eval-endpoint "https://sigil.example.test"
                                :generation-enabled nil
                                :traces-enabled nil
                                :http-fn #'http)
            (declare (ignore get-requests))
            (let ((signaled
                    (signals-condition-p
                     (lambda ()
                       (with-experiment (run client :run-id "exp-weird"
                                             :name "prompt A"
                                             :upload :weird)
                         (declare (ignore run))))
                     'sigil-validation-error)))
              (check "with-experiment rejects unknown upload mode before create"
                     (and signaled (zerop create-count)))))))

      ;; --- :bulk buffers during the body and publishes on exit ---
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-bulk"))
          (declare (ignore get-requests))
          (let ((buffered nil)
                (buffered-count nil)
                (exports-during-body nil)
                (run-object nil))
            (with-experiment (run client :run-id "exp-bulk" :name "prompt A"
                                  :upload :bulk)
              (setf run-object run)
              (setf buffered
                    (experiment-run-add-scores
                     run
                     (list (make-score :evaluator-id "judge" :evaluator-version "1"
                                       :score-key "quality" :value 0.5)
                           (make-score :evaluator-id "judge" :evaluator-version "1"
                                       :score-key "other" :value 0.7))
                     :generation-ids '("gen-bulk")))
              (setf buffered-count (experiment-run-buffered-score-count run)
                    exports-during-body (count-if #'score-call-p (cdr calls))))
            (check "bulk add-scores returns buffered count" (eql buffered 2))
            (check "bulk scores buffered during body" (eql buffered-count 2))
            (check "bulk no export during body" (zerop exports-during-body))
            (check "bulk publishes once on exit"
                   (= 1 (count-if #'score-call-p (cdr calls))))
            (check "bulk buffer cleared after publish"
                   (zerop (experiment-run-buffered-score-count run-object)))
            (check "bulk accepted count after publish"
                   (= (experiment-run-accepted-count run-object) 2))
            (check "bulk finalizes completed"
                   (member "completed" (finalize-statuses (reverse (cdr calls)))
                           :test #'equal)))))

      ;; A failing publish on exit finalizes the run failed, keeps the buffer,
      ;; and propagates the error.
      (let ((calls (cons :calls nil))
            (run-object nil)
            (signaled nil))
        (multiple-value-bind (client get-requests)
            (make-test-client
             :eval-endpoint "https://sigil.example.test"
             :generation-enabled nil :traces-enabled nil
             :http-fn (routed-http calls "exp-pubfail"
                                   :score-body "{\"results\":[{\"score_id\":\"sc1\",\"accepted\":false,\"error\":\"bad score\"}]}"))
          (declare (ignore get-requests))
          (handler-case
              (with-experiment (run client :run-id "exp-pubfail" :name "prompt A"
                                    :upload :bulk)
                (setf run-object run)
                (experiment-run-add-scores
                 run
                 (list (make-score :evaluator-id "judge" :evaluator-version "1"
                                   :score-key "quality" :value 1))
                 :generation-ids '("gen-pubfail")))
            (sigil-error () (setf signaled t)))
          (check "publish failure propagates from with-experiment" signaled)
          (check "publish failure finalizes failed"
                 (member "failed" (finalize-statuses (reverse (cdr calls))) :test #'equal))
          (check "publish failure does not finalize completed"
                 (not (member "completed" (finalize-statuses (reverse (cdr calls)))
                              :test #'equal)))
          (check "publish failure keeps the buffer"
                 (= (experiment-run-buffered-score-count run-object) 1))))

      ;; --- :manual leaves the run open ---
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-manual"))
          (declare (ignore get-requests))
          (let ((run-object nil))
            (with-experiment (run client :run-id "exp-manual" :name "prompt A"
                                  :upload :manual)
              (setf run-object run)
              (experiment-run-add-scores
               run
               (list (make-score :evaluator-id "judge" :evaluator-version "1"
                                 :score-key "quality" :value 1))
               :generation-ids '("gen-manual")))
            (check "manual run left open on exit"
                   (null (finalize-statuses (reverse (cdr calls)))))
            (check "manual no export on exit"
                   (notany #'score-call-p (cdr calls)))
            (check "manual buffer persists after exit"
                   (= (experiment-run-buffered-score-count run-object) 1))
            (check "manual publish exports buffered scores"
                   (= (experiment-run-publish run-object) 1))
            (experiment-run-finalize run-object)
            (check "manual finalize completes after publish"
                   (member "completed" (finalize-statuses (reverse (cdr calls)))
                           :test #'equal)))))

      ;; --- run-experiment creates one trial per dataset item ---
      (let ((calls (cons :calls nil))
            (conversation-ids nil))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :traces-enabled nil
                              :http-fn (routed-http calls "exp-runner"))
          (declare (ignore get-requests))
          (let* ((items (list (make-dataset-item :id "it1" :input "2+2?")
                              (make-dataset-item :id "it2" :input "3+3?")))
                 (result
                   (run-experiment
                    client items
                    (lambda (item run)
                      (declare (ignore item))
                      (push (experiment-run-active-conversation-id run)
                            conversation-ids)
                      (let ((rec (start-generation client :mode :sync
                                                   :model-provider "openai"
                                                   :model-name "gpt-4")))
                        (recorder-end rec))
                      nil)
                    (list (lambda (item target-result)
                            (declare (ignore target-result))
                            (list (make-score :evaluator-id "judge"
                                              :evaluator-version "1"
                                              :score-key "quality"
                                              :value 1.0
                                              :metadata (jobj "item" (jget item "id"))))))
                    :run-id "exp-runner" :name "runner A")))
            (check "run-experiment returns run id"
                   (equal (getf result :run-id) "exp-runner"))
            (check "run-experiment accepted both scores"
                   (= (getf result :accepted-scores) 2))
            (check "run-experiment fetches report"
                   (equal (jget (getf result :report) "experiment_id") "exp-runner"))
            (check "run-experiment assigns stable per-item conversations"
                   (equal (reverse conversation-ids)
                          (list (stable-id "conv" "exp-runner" "it1")
                                (stable-id "conv" "exp-runner" "it2"))))
            (let* ((ordered (reverse (cdr calls)))
                   (trial-creates (remove-if-not #'trial-create-call-p ordered))
                   (trial-patches (remove-if-not #'trial-patch-call-p ordered)))
              (check "run-experiment creates exactly two trials"
                     (= (length trial-creates) 2))
              (check "run-experiment closes both trials"
                     (= (length trial-patches) 2))
              (check "each trial snapshots its own dataset item"
                     (let ((snapshots (mapcar (lambda (c)
                                                (jget* (payload c) "test_case" "test_case_id"))
                                              trial-creates)))
                       (equal snapshots '("it1" "it2"))))
              (check "trial snapshot wraps a scalar item input"
                     (equal (jget* (payload (first trial-creates))
                                   "test_case" "input" "value")
                            "2+2?"))
              (check "trial ids are deterministic"
                     (equal (jget (payload (first trial-creates)) "trial_id")
                            (stable-id "trial" "exp-runner" "it1" 1)))
              (check "run finalizes only after both trials close"
                     (let ((last-patch (position-if #'trial-patch-call-p ordered :from-end t))
                           (finalize (position-if #'finalize-call-p ordered)))
                       (and last-patch finalize (< last-patch finalize))))
              (let ((score-calls (remove-if-not #'score-call-p ordered)))
                (check "run-experiment exports per item"
                       (= (length score-calls) 2))
                (let* ((first-payload (payload (first score-calls)))
                       (score (aref (jget first-payload "scores") 0)))
                  (check "run-experiment score has item metadata"
                         (equal (jget* score "metadata" "item_id") "it1"))
                  (check "run-experiment score anchors to its trial"
                         (equal (jget score "trial_id")
                                (stable-id "trial" "exp-runner" "it1" 1)))
                  (check "run-experiment score carries test_case_id"
                         (equal (jget score "test_case_id") "it1"))
                  (check "run-experiment score uses stable conversation id"
                         (equal (jget score "conversation_id")
                                (stable-id "conv" "exp-runner" "it1")))
                  (check "run-experiment score uses captured generation id"
                         (let ((gid (jget score "generation_id")))
                           (and (stringp gid) (eql 0 (search "gen_" gid)))))
                  (check "run-experiment score id matches reference parity"
                         (equal (jget score "score_id")
                                (stable-id "score" "exp-runner"
                                           (stable-id "trial" "exp-runner" "it1" 1)
                                           "quality" "judge")))))))))

      ;; Two items sharing an id would mint the same trial id.
      (let ((calls (cons :calls nil))
            (signaled nil))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-dup"))
          (declare (ignore get-requests))
          (handler-case
              (run-experiment client
                              (list (make-dataset-item :id "same" :input "a")
                                    (make-dataset-item :id "same" :input "b"))
                              (lambda (item run) (declare (ignore item run)) nil)
                              nil
                              :run-id "exp-dup" :name "dupes" :fetch-report nil)
            (sigil-validation-error () (setf signaled t)))
          (check "run-experiment rejects duplicate item ids" signaled)
          (check "run-experiment creates only the first trial"
                 (= 1 (count-if #'trial-create-call-p (cdr calls))))
          (check "a rejected duplicate still finalizes the run failed"
                 (member "failed" (finalize-statuses (reverse (cdr calls)))
                         :test #'equal))))

      ;; --- A plain run-experiment sends exactly these request bodies ---
      ;;
      ;; Every request body a plain RUN-EXPERIMENT sends, pinned whole. The
      ;; fixture records no generation, so the only byte that varies between
      ;; runs is the upsert's created_at, which is read back and spliced into
      ;; the expected body. The trial, conversation, and score ids are SHA-1
      ;; derived and stay literal here on purpose: a changed hash is exactly
      ;; the drift this check exists to catch.
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-stable"))
          (declare (ignore get-requests))
          (run-experiment
           client
           (list (make-dataset-item :id "it1" :input "2+2?"))
           (lambda (item run) (declare (ignore item run)) nil)
           (list (lambda (item target-result)
                   (declare (ignore item target-result))
                   (list (make-score :evaluator-id "judge"
                                     :evaluator-version "1"
                                     :score-key "quality"
                                     :value 1.0
                                     :passed t))))
           :run-id "exp-stable" :name "stable" :fetch-report nil))
        (let* ((ordered (reverse (cdr calls)))
               (bodies (mapcar #'third ordered))
               (created-at (when ordered
                             (jget* (payload (first ordered)) "metadata" "created_at"))))
          (check "a plain run-experiment issues exactly five requests"
                 (= (length bodies) 5))
          (check "upsert body matches the pinned bytes"
                 (equal (first bodies)
                        (format nil "{\"name\":\"stable\",\"source\":{\"kind\":\"sdk\",\"id\":\"lisp\"},\"experiment_id\":\"exp-stable\",\"planned_trial_count\":1,\"metadata\":{\"created_at\":\"~a\"}}"
                                created-at)))
          (check "trial create body matches the pinned bytes"
                 (equal (second bodies)
                        "{\"trial_id\":\"trial-011f75bdcfbbe2dc\",\"test_case_id\":\"it1\",\"attempt\":1,\"status\":\"running\",\"test_case\":{\"test_case_id\":\"it1\",\"name\":\"\",\"description\":\"\",\"tags\":[],\"category\":\"\",\"input\":{\"value\":\"2+2?\"},\"expected\":{}}}"))
          (check "score export body matches the pinned bytes"
                 (equal (third bodies)
                        "{\"scores\":[{\"score_id\":\"score-f97882d5fc838573\",\"evaluator_id\":\"judge\",\"evaluator_version\":\"1\",\"score_key\":\"quality\",\"value\":{\"number\":1.0},\"trial_id\":\"trial-011f75bdcfbbe2dc\",\"experiment_id\":\"exp-stable\",\"conversation_id\":\"conv-e3921792bf7015da\",\"test_case_id\":\"it1\",\"passed\":true,\"metadata\":{\"item_id\":\"it1\"},\"source\":{\"kind\":\"experiment\",\"id\":\"exp-stable\"}}]}"))
          (check "trial patch body matches the pinned bytes"
                 (equal (fourth bodies)
                        "{\"status\":\"completed\",\"conversation_id\":\"conv-e3921792bf7015da\"}"))
          (check "finalize body matches the pinned bytes"
                 (equal (fifth bodies)
                        "{\"status\":\"completed\",\"source\":{\"kind\":\"sdk\",\"id\":\"lisp\"},\"score_count\":1}"))))

      ;; --- with-experiment: generation tagging, value shapes, completion ---
      (let ((calls (cons :calls nil))
            (accepted nil)
            (recorded-id nil)
            (run-object nil))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :traces-enabled nil
                              :http-fn (routed-http calls "exp-prompt-a"))
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-prompt-a" :name "prompt A"
                                :agent-name "judge-agent"
                                :agent-version "1.0"
                                :extra-tags '(("candidate" . "a"))
                                :extra-metadata '(("run.note" . "yes")))
            (setf run-object run)
            (let ((rec (start-generation client
                           :mode :sync
                           :model-provider "openai"
                           :model-name "gpt-4"
                           :tags '(("caller" . "tag"))
                           :metadata '(("caller.meta" . "yes")))))
              (setf recorded-id (gen-rec-generation-id rec))
              (recorder-end rec)
              (setf accepted
                    (experiment-run-add-scores
                     run
                     (list (make-score :evaluator-id "judge"
                                       :evaluator-version "1"
                                       :score-key "passed"
                                       :value t
                                       :passed t)
                           (make-score :evaluator-id "judge"
                                       :evaluator-version "1"
                                       :score-key "label"
                                       :value "pass")
                           (make-score :evaluator-id "judge"
                                       :evaluator-version "1"
                                       :score-key "failed"
                                       :value nil)))))))
        (check "experiment-run-add-scores accepted all scores" (= accepted 3))
        (check "run captured generation id"
               (equal (experiment-run-produced-generation-ids run-object)
                      (list recorded-id)))
        (let* ((ordered (reverse (cdr calls)))
               (gen-call (find-if (lambda (call) (search "generations:export" (second call)))
                                  ordered))
               (gen-payload (payload gen-call))
               (generation (aref (jget gen-payload "generations") 0))
               (score-call (find-if #'score-call-p ordered))
               (score-payload (payload score-call))
               (first-score (aref (jget score-payload "scores") 0))
               (second-score (aref (jget score-payload "scores") 1))
               (third-score (aref (jget score-payload "scores") 2)))
          (check "generation has experiment.run_id tag"
                 (equal (jget* generation "tags" "experiment.run_id") "exp-prompt-a"))
          (check "generation has experiment_run_id metadata"
                 (equal (jget* generation "metadata" "experiment_run_id") "exp-prompt-a"))
          (check "generation keeps caller tag"
                 (equal (jget* generation "tags" "caller") "tag"))
          (check "generation uses experiment agent identity"
                 (and (equal (jget generation "agent_name") "judge-agent")
                      (equal (jget generation "agent_version") "1.0")))
          (check "score uses captured generation id"
                 (equal (jget first-score "generation_id") recorded-id))
          (check "score includes experiment_id"
                 (equal (jget first-score "experiment_id") "exp-prompt-a"))
          (check "score omits run_id"
                 (null (nth-value 1 (gethash "run_id" first-score))))
          (check "score includes experiment source"
                 (and (equal (jget* first-score "source" "kind") "experiment")
                      (equal (jget* first-score "source" "id") "exp-prompt-a")))
          (check "boolean score value serializes as bool"
                 (eq (jget* first-score "value" "bool") t))
          (check "string score value serializes as string"
                 (equal (jget* second-score "value" "string") "pass"))
          (check "explicit nil score value serializes as false bool"
                 (multiple-value-bind (value found-p)
                     (gethash "bool" (jget third-score "value"))
                   (and found-p (null value))))
          (check "score conversation_id matches generation"
                 (equal (jget first-score "conversation_id")
                        (jget generation "conversation_id")))
          (check "with-experiment finalizes completed"
                 (member "completed" (finalize-statuses ordered) :test #'equal))))

      ;; --- Conflict on upsert ---
      ;;
      ;; :reopen continues only for a conflict the caller can still work
      ;; through. A run the backend calls terminal would take trials and a
      ;; finalize it has already refused, so that one propagates.
      (let ((calls (cons :calls nil))
            (body-ran nil))
        (flet ((http (url &key method headers content &allow-other-keys)
                 (declare (ignore headers))
                 (push (list method url content) (cdr calls))
                 (cond
                   ((and (eq method :post) (search "experiment-runs:upsert" url))
                    (values "an open draft already exists for this experiment" 409))
                   (t (values "{}" 200)))))
          (multiple-value-bind (client get-requests)
              (make-test-client :eval-endpoint "https://sigil.example.test"
                                :generation-enabled nil :traces-enabled nil
                                :http-fn #'http)
            (declare (ignore get-requests))
            (with-experiment (run client :run-id "exp-conflict" :name "prompt A")
              (declare (ignore run))
              (setf body-ran t))
            (check "with-experiment body runs after a recoverable upsert conflict"
                   body-ran)
            (check "conflict reopen issues no PATCH to /eval/experiments"
                   (notany (lambda (call)
                             (and (search "/eval/experiments" (second call))
                                  (member (first call) '(:post :patch))))
                           (cdr calls))))))
      (let ((calls (cons :calls nil))
            (signaled nil))
        (flet ((http (url &key method headers content &allow-other-keys)
                 (declare (ignore headers))
                 (push (list method url content) (cdr calls))
                 (cond
                   ((and (eq method :post) (search "experiment-runs:upsert" url))
                    (values "experiment \"exp-terminal\" is already finalized as completed" 409))
                   (t (values "{}" 200)))))
          (multiple-value-bind (client get-requests)
              (make-test-client :eval-endpoint "https://sigil.example.test"
                                :generation-enabled nil :traces-enabled nil
                                :http-fn #'http)
            (declare (ignore get-requests))
            (handler-case
                (with-experiment (run client :run-id "exp-terminal" :name "prompt A")
                  (declare (ignore run)))
              (sigil-conflict-error (e)
                (setf signaled (sigil-conflict-error-kind e))))
            (check "a terminal conflict propagates even under :reopen"
                   (eq signaled :terminal))
            (check "a terminal conflict writes nothing into the refused run"
                   (notany (lambda (call)
                             (or (search "/trials" (second call))
                                 (search ":finalize" (second call))))
                           (cdr calls))))))
      (let ((calls (cons :calls nil))
            (signaled nil))
        (flet ((http (url &key method headers content &allow-other-keys)
                 (declare (ignore headers))
                 (push (list method url content) (cdr calls))
                 (if (search "experiment-runs:upsert" url)
                     (values "conflict" 409)
                     (values "{}" 200))))
          (multiple-value-bind (client get-requests)
              (make-test-client :eval-endpoint "https://sigil.example.test"
                                :generation-enabled nil :traces-enabled nil
                                :http-fn #'http)
            (declare (ignore get-requests))
            (handler-case
                (with-experiment (run client :run-id "exp-conflict2" :name "prompt A"
                                      :on-conflict :error)
                  (declare (ignore run)))
              (sigil-conflict-error () (setf signaled t)))
            (check "on-conflict :error propagates the conflict" signaled))))

      ;; --- Error and non-local exits both finalize failed ---
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-fail"))
          (declare (ignore get-requests))
          (handler-case
              (with-experiment (run client :run-id "exp-fail" :name "prompt A")
                (declare (ignore run))
                (error "boom"))
            (error () nil))
          (check "with-experiment finalizes failed on error"
                 (member "failed" (finalize-statuses (reverse (cdr calls))) :test #'equal))))
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-abort"))
          (declare (ignore get-requests))
          (catch 'abort
            (with-experiment (run client :run-id "exp-abort" :name "prompt A")
              (declare (ignore run))
              (throw 'abort :done)))
          (check "non-local exit finalizes failed instead of cancelling"
                 (member "failed" (finalize-statuses (reverse (cdr calls))) :test #'equal))
          (check "non-local exit issues no cancel request"
                 (notany (lambda (call) (search ":cancel" (second call))) (cdr calls)))))

      ;; --- A trial that will not close blocks the score count ---
      (let ((calls (cons :calls nil)))
        (flet ((http (url &key method headers content &allow-other-keys)
                 (declare (ignore headers))
                 (push (list method url content) (cdr calls))
                 (cond
                   ;; Every score is rejected, so each trial's flush raises.
                   ((search "scores:export" url)
                    (values "{\"results\":[{\"score_id\":\"x\",\"accepted\":false,\"error\":\"bad\"}]}"
                            202))
                   (t (values (jzon:stringify (jobj "experiment_id" "exp-partial")) 200)))))
          (multiple-value-bind (client get-requests)
              (make-test-client :eval-endpoint "https://sigil.example.test"
                                :generation-enabled nil :traces-enabled nil
                                :http-fn #'http)
            (declare (ignore get-requests))
            (with-experiment (run client :run-id "exp-partial" :name "partial")
              ;; Two trials are left open; both must be attempted at exit.
              (dolist (case-id '("case-1" "case-2"))
                (let ((trial (experiment-run-open-trial run case-id)))
                  (trial-add-scores trial
                                    (list (make-score :evaluator-id "judge"
                                                      :evaluator-version "1"
                                                      :score-key "final"
                                                      :value 1))))))
            (let* ((ordered (reverse (cdr calls)))
                   (finalize (find-if #'finalize-call-p ordered)))
              (check "both open trials are attempted before finalization"
                     (= 2 (count-if #'score-call-p ordered)))
              (check "a run with a failed trial close finalizes failed"
                     (equal (jget (payload finalize) "status") "failed"))
              (check "a run with a failed trial close omits score_count"
                     (null (nth-value 1 (gethash "score_count" (payload finalize)))))
              (check "the finalize error names the trial close failure"
                     (search "trial close failed" (jget (payload finalize) "error")))))))

      ;; A close failure in manual mode is still a failure: the trials that
      ;; would not close took their scores with them, so the run cannot be
      ;; left open reporting a buffered count.
      (let ((calls (cons :calls nil)))
        (flet ((http (url &key method headers content &allow-other-keys)
                 (declare (ignore headers))
                 (push (list method url content) (cdr calls))
                 (if (eq method :patch)
                     (values "trial gone" 404)
                     (values (jzon:stringify (jobj "experiment_id" "exp-manual")) 200))))
          (multiple-value-bind (client get-requests)
              (make-test-client :eval-endpoint "https://sigil.example.test"
                                :generation-enabled nil :traces-enabled nil
                                :http-fn #'http)
            (declare (ignore get-requests))
            (with-experiment (run client :run-id "exp-manual" :name "manual"
                                  :upload :manual)
              (experiment-run-open-trial run "case-1"))
            (let ((finalize (find-if #'finalize-call-p (reverse (cdr calls)))))
              (check "manual mode still finalizes failed when a trial will not close"
                     (and finalize (equal (jget (payload finalize) "status") "failed")))))))

      ;; --- Score export never falls back to the eval auth headers ---
      (let ((score-headers :none))
        (multiple-value-bind (client get-requests)
            (make-test-client
             :eval-endpoint "https://eval.test"
             :generation-endpoint "http://gen.test/api/v1/generations:export"
             :generation-enabled nil :traces-enabled nil
             :eval-auth-token "SECRET-EVAL-TOKEN"
             :auth-mode :none :auth-password nil :ingest-actor ""
             :http-fn (lambda (url &key method headers content &allow-other-keys)
                        (declare (ignore method content))
                        (when (search "scores:export" url)
                          (setf score-headers headers))
                        (values (jzon:stringify (jobj "experiment_id" "exp-auth")) 200)))
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-auth" :name "auth")
            (experiment-run-add-scores
             run
             (list (make-score :evaluator-id "judge" :evaluator-version "1"
                               :score-key "final" :value 1
                               :generation-id "gen-1"))))
          (check "score export reached the generation host"
                 (not (eq score-headers :none)))
          (check "score export sends no eval Authorization header"
                 (null (assoc "Authorization" score-headers :test #'equal)))))

      ;; --- Trial-anchored scores do not need a generation ---
      ;;
      ;; The trial is the anchor, so an item that produced several generations
      ;; does not have to name one, and a trial does not inherit the previous
      ;; trial's captured generations.
      (let ((calls (cons :calls nil))
            (buffered nil))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-multi"))
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-multi" :name "multi")
            (let ((trial (experiment-run-open-trial run "case-multi")))
              (experiment-run-track-generation-id run "gen-a")
              (experiment-run-track-generation-id run "gen-b")
              (setf buffered
                    (trial-add-scores trial
                                      (list (make-score :evaluator-id "judge"
                                                        :evaluator-version "1"
                                                        :score-key "final"
                                                        :value 1))))
              (trial-close trial)))
          (check "a trial-anchored score survives several captured generations"
                 (eql buffered 1))
          (let ((score (aref (jget (payload (find-if #'score-call-p (reverse (cdr calls))))
                                   "scores")
                             0)))
            (check "a score with no chosen generation omits generation_id"
                   (null (nth-value 1 (gethash "generation_id" score)))))))
      (let ((calls (cons :calls nil))
            (second-score nil))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-carry"))
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-carry" :name "carry")
            (let ((first-trial (experiment-run-open-trial run "case-1")))
              (experiment-run-track-generation-id run "gen-first")
              (trial-close first-trial))
            (let ((second-trial (experiment-run-open-trial run "case-2")))
              (trial-add-scores second-trial
                                (list (make-score :evaluator-id "judge"
                                                  :evaluator-version "1"
                                                  :score-key "final" :value 1)))
              (trial-close second-trial)
              (setf second-score
                    (aref (jget (payload (find-if #'score-call-p (reverse (cdr calls))))
                                "scores")
                          0))))
          (check "a new trial does not inherit the previous trial's generation"
                 (null (nth-value 1 (gethash "generation_id" second-score))))))

      ;; --- Whitespace around an item id does not collapse two trials ---
      (let ((calls (cons :calls nil))
            (first-id nil)
            (second-id nil))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-ws"))
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-ws" :name "whitespace")
            (setf first-id (trial-id (experiment-run-open-trial
                                      run (make-dataset-item :id "a"))))
            (check "a whitespace-only variant of a used id is rejected"
                   (signals-condition-p
                    (lambda () (experiment-run-open-trial run (make-dataset-item :id " a ")))
                    'sigil-validation-error))
            (setf second-id (trial-id (experiment-run-open-trial
                                       run (make-dataset-item :id " b ")))))
          (check "a trimmed id mints the trimmed trial"
                 (equal second-id (stable-id "trial" "exp-ws" "b" 1)))
          (check "trials with distinct ids do not collide"
                 (not (equal first-id second-id)))
          (check "every opened trial closed"
                 (= 2 (count-if #'trial-patch-call-p (cdr calls))))))

      ;; --- A failed trial create releases its claim ---
      (let ((calls (cons :calls nil))
            (fail-create t)
            (retried nil))
        (flet ((http (url &key method headers content &allow-other-keys)
                 (declare (ignore headers))
                 (push (list method url content) (cdr calls))
                 (if (and fail-create (search "/trials" url) (eq method :post))
                     (values "boom" 500)
                     (values (jzon:stringify (jobj "experiment_id" "exp-retry")) 200))))
          (multiple-value-bind (client get-requests)
              (make-test-client :eval-endpoint "https://sigil.example.test"
                                :generation-enabled nil :traces-enabled nil
                                :max-retries 1
                                :http-fn #'http)
            (declare (ignore get-requests))
            (with-experiment (run client :run-id "exp-retry" :name "retry")
              (handler-case
                  (experiment-run-open-trial run "case-1")
                (sigil-error () nil))
              (setf fail-create nil)
              (setf retried (experiment-run-open-trial run "case-1")))
            (check "a trial create that failed can be retried at the same attempt"
                   (equal (trial-id retried) (stable-id "trial" "exp-retry" "case-1" 1))))))

      ;; --- A closed trial refuses further scores ---
      (let ((calls (cons :calls nil)))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-closed"))
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-closed" :name "closed")
            (let ((trial (experiment-run-open-trial run "case-1")))
              (trial-close trial)
              (check "adding scores to a closed trial signals"
                     (signals-condition-p
                      (lambda ()
                        (trial-add-scores trial
                                          (list (make-score :evaluator-id "judge"
                                                            :evaluator-version "1"
                                                            :score-key "final"
                                                            :value 1))))
                      'sigil-validation-error))))))

      ;; --- A rejected batch does not consume score ids ---
      (let ((calls (cons :calls nil))
            (trial-id nil))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn (routed-http calls "exp-ids"))
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-ids" :name "ids")
            (let ((trial (experiment-run-open-trial run "case-1")))
              (setf trial-id (trial-id trial))
              ;; The second score has no value, so the whole batch is rejected.
              (check "a malformed score rejects the batch"
                     (signals-condition-p
                      (lambda ()
                        (trial-add-scores trial
                                          (list (make-score :evaluator-id "judge"
                                                            :evaluator-version "1"
                                                            :score-key "final"
                                                            :value 1)
                                                (list :evaluator-id "judge"
                                                      :evaluator-version "1"
                                                      :score-key "final"))))
                      'sigil-validation-error))
              (trial-add-scores trial
                                (list (make-score :evaluator-id "judge"
                                                  :evaluator-version "1"
                                                  :score-key "final" :value 1)))
              (trial-close trial)))
          (let ((score (aref (jget (payload (find-if #'score-call-p (reverse (cdr calls))))
                                   "scores")
                             0)))
            (check "the corrected retry mints the canonical first score id"
                   (equal (jget score "score_id")
                          (stable-id "score" "exp-ids" trial-id "final" "judge")))))))))

;;; ================================================================
;;; Trial tests
;;; ================================================================

(defun run-trial-tests ()
  (with-test-suite ("Trials")

    ;; --- Deterministic identity ---
    (check "trial id matches stable-id inputs"
           (equal (trial-mint-id "exp-1" "case-1" 1)
                  (stable-id "trial" "exp-1" "case-1" 1)))
    (check "trial id varies by attempt"
           (not (equal (trial-mint-id "exp-1" "case-1" 1)
                       (trial-mint-id "exp-1" "case-1" 2))))
    (check "trial id varies by test case"
           (not (equal (trial-mint-id "exp-1" "case-1" 1)
                       (trial-mint-id "exp-1" "case-2" 1))))

    ;; --- Trial create request body ---
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (let ((trial (trial-open client "exp-1" "case-1"
                                 :attempt 1
                                 :test-case (make-test-case :id "case-1"
                                                            :name "capital"
                                                            :tags '("smoke")
                                                            :category "qa"
                                                            :input "hello"
                                                            :expected "world")
                                 :suite-id "suite-1"
                                 :suite-version "v2")))
          (let* ((call (first (reverse (cdr calls))))
                 (posted (payload call)))
            (check "trial create POSTs the trials route"
                   (and (eq (first call) :post)
                        (equal (second call)
                               "https://sigil.example.test/api/v1/experiment-runs/exp-1/trials")))
            (check "trial create sends status running"
                   (equal (jget posted "status") "running"))
            (check "trial create sends the deterministic trial id"
                   (equal (jget posted "trial_id")
                          (stable-id "trial" "exp-1" "case-1" 1)))
            (check "trial create sends test_case_id and attempt"
                   (and (equal (jget posted "test_case_id") "case-1")
                        (eql (jget posted "attempt") 1)))
            (check "trial create sends a test_case snapshot"
                   (hash-table-p (jget posted "test_case")))
            (check "snapshot input is an object wrapping the scalar"
                   (equal (jget* posted "test_case" "input" "value") "hello"))
            (check "snapshot expected is an object wrapping the scalar"
                   (equal (jget* posted "test_case" "expected" "value") "world"))
            (check "snapshot carries suite id and version"
                   (and (equal (jget* posted "test_case" "suite_id") "suite-1")
                        (equal (jget* posted "test_case" "suite_version") "v2")))
            (check "snapshot carries name, tags, and category"
                   (and (equal (jget* posted "test_case" "name") "capital")
                        (equal (aref (jget* posted "test_case" "tags") 0) "smoke")
                        (equal (jget* posted "test_case" "category") "qa")))
            (check "trial-created-p is set after create" (trial-created-p trial)))
          ;; --- Sparse close ---
          (setf (cdr calls) nil)
          (trial-close trial)
          (let* ((call (first (reverse (cdr calls))))
                 (posted (payload call)))
            (check "trial close PATCHes the trial route"
                   (and (eq (first call) :patch)
                        (equal (second call)
                               (format nil "https://sigil.example.test/api/v1/experiment-runs/exp-1/trials/~a"
                                       (trial-id trial)))))
            (check "clean close sends status completed"
                   (equal (jget posted "status") "completed"))
            (check "clean close body is sparse"
                   (= (hash-table-count posted) 1))
            (check "trial-closed-p is set after close" (trial-closed-p trial)))
          (setf (cdr calls) nil)
          (trial-close trial)
          (check "closing twice issues no second request" (null (cdr calls))))))

    ;; --- Error close ---
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (let ((trial (trial-open client "exp-1" "case-err")))
          (setf (cdr calls) nil)
          (trial-close trial :error "boom")
          (let ((posted (payload (first (reverse (cdr calls))))))
            (check "error close sends status failed"
                   (equal (jget posted "status") "failed"))
            (check "error close sends the error text"
                   (equal (jget posted "error") "boom"))))))

    ;; A locally failed assertion still closes the trial as completed; the
    ;; verdict rides on the final score's `passed`, not the trial status.
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (let ((trial (trial-open client "exp-1" "case-verdict")))
          (setf (cdr calls) nil)
          (trial-close trial :status "passed")
          (check "a passed trial closes as completed"
                 (equal (jget (payload (first (reverse (cdr calls)))) "status")
                        "completed")))))
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (let ((trial (trial-open client "exp-1" "case-errored")))
          (setf (cdr calls) nil)
          (trial-close trial :status "errored")
          (check "an errored trial closes as failed"
                 (equal (jget (payload (first (reverse (cdr calls)))) "status")
                        "failed")))))
    ;; Only an errored trial is failed. A trial that ran fine and failed its
    ;; assertion is completed, so it counts as executed in the report.
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (let ((trial (trial-open client "exp-1" "case-assert")))
          (setf (cdr calls) nil)
          (trial-close trial :status "failed")
          (check "a locally failed assertion closes as completed"
                 (equal (jget (payload (first (reverse (cdr calls)))) "status")
                        "completed")))))

    ;; --- Bind helpers are local only ---
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (let ((trial (trial-open client "exp-1" "case-bind")))
          (setf (cdr calls) nil)
          (trial-bind-generation trial "gen-9")
          (trial-bind-conversation trial "conv-9" :trace-id "tr-9" :span-id "sp-9")
          (check "bind helpers issue no request" (null (cdr calls)))
          (check "bind-generation sets the generation id"
                 (equal (trial-generation-id trial) "gen-9"))
          (check "bind-conversation sets conversation, trace, and span"
                 (and (equal (trial-conversation-id trial) "conv-9")
                      (equal (trial-trace-id trial) "tr-9")
                      (equal (trial-span-id trial) "sp-9")))
          (trial-close trial)
          (let ((posted (payload (first (reverse (cdr calls))))))
            (check "close forwards bound correlation ids"
                   (and (equal (jget posted "conversation_id") "conv-9")
                        (equal (jget posted "trace_id") "tr-9")
                        (equal (jget posted "span_id") "sp-9")))))))

    ;; --- Scores flush before the closing PATCH ---
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (with-experiment (run client :run-id "exp-1" :name "flush order")
          (let ((trial (experiment-run-open-trial run "case-flush")))
            (setf (cdr calls) nil)
            (trial-add-scores trial
                              (list (make-score :evaluator-id "judge"
                                                :evaluator-version "1"
                                                :score-key "final"
                                                :value 1
                                                :passed t)))
            (check "add-scores alone exports nothing"
                   (notany #'score-call-p (cdr calls)))
            (check "add-scores alone does not close the trial"
                   (and (not (trial-closed-p trial))
                        (notany (lambda (c) (eq (first c) :patch)) (cdr calls))))
            (trial-close trial)
            (let* ((ordered (reverse (cdr calls)))
                   (score-index (position-if #'score-call-p ordered))
                   (patch-index (position-if (lambda (c) (eq (first c) :patch)) ordered)))
              (check "continuous mode exports the trial's scores at close"
                     (and score-index t))
              (check "scores are exported before the closing PATCH"
                     (and score-index patch-index (< score-index patch-index)))
              (let ((score (aref (jget (payload (nth score-index ordered)) "scores") 0)))
                (check "flushed score anchors to the trial"
                       (equal (jget score "trial_id") (trial-id trial)))
                (check "flushed score needs no generation"
                       (null (nth-value 1 (gethash "generation_id" score))))))))))

    ;; In :bulk and :manual, closing a trial moves its scores to the run
    ;; buffer instead of exporting them; the run publishes at exit.
    (let ((calls (cons :calls nil))
          (buffered-after-close nil))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (with-experiment (run client :run-id "exp-1" :name "bulk trial"
                              :upload :bulk)
          (let ((trial (experiment-run-open-trial run "case-bulk")))
            (trial-add-scores trial
                              (list (make-score :evaluator-id "judge"
                                                :evaluator-version "1"
                                                :score-key "final" :value 1)))
            (setf (cdr calls) nil)
            (trial-close trial)
            (setf buffered-after-close (experiment-run-buffered-score-count run))
            (check "bulk trial close exports nothing"
                   (notany #'score-call-p (cdr calls)))
            (check "bulk trial close still PATCHes the trial"
                   (some (lambda (c) (eq (first c) :patch)) (cdr calls)))))
        (check "bulk trial scores land in the run buffer"
               (eql buffered-after-close 1))
        (check "bulk publishes the trial's scores at run exit"
               (= 1 (count-if #'score-call-p (cdr calls))))))

    ;; --- Deterministic score ids and occurrence counters ---
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (with-experiment (run client :run-id "exp-1" :name "score ids")
          (let ((trial (experiment-run-open-trial run "case-ids")))
            (setf (cdr calls) nil)
            (trial-add-scores trial
                              (list (make-score :evaluator-id "judge"
                                                :evaluator-version "1"
                                                :score-key "final" :value 1)
                                    (make-score :evaluator-id "judge"
                                                :evaluator-version "1"
                                                :score-key "final" :value 0)
                                    (make-score :evaluator-id "judge"
                                                :evaluator-version "1"
                                                :score-key "final" :value 1)))
            (trial-close trial)
            (let* ((score-call (find-if #'score-call-p (reverse (cdr calls))))
                   (scores (jget (payload score-call) "scores"))
                   (first-id (jget (aref scores 0) "score_id"))
                   (second-id (jget (aref scores 1) "score_id"))
                   (third-id (jget (aref scores 2) "score_id")))
              (check "first score id matches reference parity"
                     (equal first-id
                            (stable-id "score" "exp-1" (trial-id trial) "final" "judge")))
              (check "a repeated pair appends an occurrence counter"
                     (equal second-id
                            (stable-id "score" "exp-1" (trial-id trial) "final" "judge" 2)))
              (check "the third occurrence gets its own counter"
                     (equal third-id
                            (stable-id "score" "exp-1" (trial-id trial) "final" "judge" 3)))
              (check "repeated scores get distinct ids"
                     (and (not (equal first-id second-id))
                          (not (equal second-id third-id)))))))))

    ;; --- Duplicate (test-case-id, attempt) is rejected locally ---
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (with-experiment (run client :run-id "exp-1" :name "dupes")
          (experiment-run-open-trial run "case-dup")
          (setf (cdr calls) nil)
          (check "a duplicate attempt signals a validation error"
                 (signals-condition-p
                  (lambda () (experiment-run-open-trial run "case-dup"))
                  'sigil-validation-error))
          (check "a duplicate attempt records no second trial-create request"
                 (null (cdr calls)))
          (check "the collision error names the test case and attempt"
                 (handler-case
                     (progn (experiment-run-open-trial run "case-dup") nil)
                   (sigil-validation-error (e)
                     (and (search "case-dup" (sigil-error-message e))
                          (search "already exists" (sigil-error-message e))
                          t))))
          (check "a second attempt of the same case is allowed"
                 (let ((trial (experiment-run-open-trial run "case-dup" :attempt 2)))
                   (equal (trial-id trial)
                          (stable-id "trial" "exp-1" "case-dup" 2)))))))

    ;; --- with-trial ---
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (with-experiment (run client :run-id "exp-1" :name "with-trial")
          (setf (cdr calls) nil)
          (with-trial (trial run "case-macro")
            (check "with-trial binds an open trial"
                   (and (typep trial 'experiment-trial)
                        (not (trial-closed-p trial)))))
          (check "with-trial closes the trial on normal exit"
                 (some (lambda (c) (eq (first c) :patch)) (cdr calls)))
          (check "with-trial closes completed on normal exit"
                 (equal (jget (payload (find-if (lambda (c) (eq (first c) :patch))
                                                (reverse (cdr calls))))
                              "status")
                        "completed")))))
    (let ((calls (cons :calls nil))
          (signaled nil))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (with-experiment (run client :run-id "exp-1" :name "with-trial error")
          (setf (cdr calls) nil)
          (handler-case
              (with-trial (trial run "case-macro-err")
                (declare (ignore trial))
                (error "trial boom"))
            (error () (setf signaled t)))
          (check "with-trial propagates the body error" signaled)
          (let ((posted (payload (find-if (lambda (c) (eq (first c) :patch))
                                          (reverse (cdr calls))))))
            (check "with-trial closes failed on error"
                   (equal (jget posted "status") "failed"))
            (check "with-trial sends the error text"
                   (search "trial boom" (jget posted "error")))))))
    ;; A close that signals must not swallow the body's own error, and must
    ;; leave the trial closable again so run teardown can report it.
    (let ((calls (cons :calls nil))
          (seen nil))
      (flet ((http (url &key method headers content &allow-other-keys)
               (declare (ignore headers))
               (push (list method url content) (cdr calls))
               (if (search "scores:export" url)
                   (values "{\"results\":[{\"score_id\":\"x\",\"accepted\":false,\"error\":\"bad\"}]}"
                           202)
                   (values (jzon:stringify (jobj "experiment_id" "exp-1")) 200))))
        (multiple-value-bind (client get-requests)
            (make-test-client :eval-endpoint "https://sigil.example.test"
                              :generation-enabled nil :traces-enabled nil
                              :http-fn #'http)
          (declare (ignore get-requests))
          (with-experiment (run client :run-id "exp-1" :name "close failure")
            (handler-case
                (with-trial (trial run "case-close-fail")
                  (trial-add-scores trial
                                    (list (make-score :evaluator-id "judge"
                                                      :evaluator-version "1"
                                                      :score-key "final" :value 1)))
                  (error "body boom"))
              (error (e) (setf seen (princ-to-string e))))
            (check "a failing close keeps the body's error"
                   (and seen (search "body boom" seen)))
            (check "a trial whose close failed is still open"
                   (= 1 (length (experiment-run-open-trials-list run))))))))

    ;; A throw out of the body never verified the case, so the trial is failed
    ;; rather than completed.
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-1"))
        (declare (ignore get-requests))
        (with-experiment (run client :run-id "exp-1" :name "with-trial throw")
          (setf (cdr calls) nil)
          (catch 'skip
            (with-trial (trial run "case-macro-throw")
              (declare (ignore trial))
              (throw 'skip :done)))
          (let ((posted (payload (find-if (lambda (c) (eq (first c) :patch))
                                          (reverse (cdr calls))))))
            (check "a non-local exit closes the trial failed"
                   (equal (jget posted "status") "failed"))
            (check "a non-local exit names itself in the trial error"
                   (search "non-local exit" (jget posted "error")))))))

    ;; --- Opening a trial over one that is already open is reported ---
    ;;
    ;; Capture is held per run, so opening a trial clears what the already-open
    ;; trial captured. The open is allowed anyway: a failed close legitimately
    ;; leaves a trial open, and signalling here would break that recovery.
    (let ((calls (cons :calls nil))
          (logged nil)
          (trial-a nil)
          (trial-b nil)
          (open-ids nil)
          (open-logs nil)
          (signaled nil))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-overlap")
                            :log-fn (lambda (level component message &rest kvs)
                                      (declare (ignore kvs))
                                      (push (list level component message) logged)))
        (declare (ignore get-requests))
        (with-experiment (run client :run-id "exp-overlap" :name "overlap")
          (setf trial-a (experiment-run-open-trial run "case-a"))
          (setf logged nil)
          (handler-case
              (setf trial-b (experiment-run-open-trial run "case-b"))
            (condition () (setf signaled t)))
          ;; Snapshot before the closes and the finalize, so only what the
          ;; second open emitted is counted.
          (setf open-logs (reverse logged))
          (setf open-ids (mapcar #'trial-id (experiment-run-open-trials-list run)))
          (trial-close trial-a)
          (trial-close trial-b)))
      (check "an open trial logs exactly one message on the next open"
             (= 1 (length open-logs)))
      (let ((warning (first open-logs)))
        (check "the overlap message is a warning"
               (eq (first warning) :warn))
        (check "the overlap warning names the newly opened trial"
               (and warning (search (trial-id trial-b) (third warning))))
        (check "the overlap warning names the already-open trial"
               (and warning (search (trial-id trial-a) (third warning))))
        (check "the overlap warning states the per-run capture constraint"
               (and warning
                    (search "per run, not per trial" (third warning))
                    (search "sequentially" (third warning)))))
      (check "the second trial still opens" (and trial-b (trial-created-p trial-b)))
      (check "the first trial stays open alongside the second"
             (and (member (trial-id trial-a) open-ids :test #'equal)
                  (member (trial-id trial-b) open-ids :test #'equal)))
      (check "opening over an open trial signals nothing" (not signaled)))

    ;; --- A trial that opens while another is still being created ---
    ;;
    ;; A trial reaches OPEN-TRIALS only once its create returns, so this is the
    ;; overlap that reading OPEN-TRIALS at claim time cannot see, and it is the
    ;; shape two worker threads produce. Trial A's create blocks until trial
    ;; B's create starts, which puts B's claim strictly inside A's round trip.
    (let* ((calls (cons :calls nil))
           (http-lock (bt2:make-lock :name "sigil-test-race-http"))
           (log-lock (bt2:make-lock :name "sigil-test-race-log"))
           (a-creating (bt2:make-semaphore :name "sigil-test-a-creating"))
           (b-creating (bt2:make-semaphore :name "sigil-test-b-creating"))
           (logged nil)
           (overlap-logs nil)
           (trial-a nil)
           (trial-b nil)
           (open-ids nil))
      (multiple-value-bind (client get-requests)
          (make-test-client
           :eval-endpoint "https://sigil.example.test"
           :generation-enabled nil :traces-enabled nil
           :log-fn (lambda (level component message &rest kvs)
                     (declare (ignore component kvs))
                     (bt2:with-lock-held (log-lock) (push (list level message) logged)))
           :http-fn (let ((base (routed-http calls "exp-race")))
                      (lambda (url &rest args &key method content &allow-other-keys)
                        (when (and (eq method :post) (search "/trials" url))
                          (cond ((search "case-a" content)
                                 (bt2:signal-semaphore a-creating)
                                 (bt2:wait-on-semaphore b-creating :timeout 5))
                                ((search "case-b" content)
                                 (bt2:signal-semaphore b-creating))))
                        ;; The stub's call list is shared by both threads.
                        (bt2:with-lock-held (http-lock) (apply base url args)))))
        (declare (ignore get-requests))
        (with-experiment (run client :run-id "exp-race" :name "race")
          (bt2:with-lock-held (log-lock) (setf logged nil))
          (let ((opener (bt2:make-thread
                         (lambda () (setf trial-a (experiment-run-open-trial run "case-a")))
                         :name "sigil-test-trial-a")))
            (bt2:wait-on-semaphore a-creating :timeout 5)
            (setf trial-b (experiment-run-open-trial run "case-b"))
            (bt2:join-thread opener))
          ;; Snapshot before the closes and the finalize, so only what the
          ;; second open emitted is counted.
          (setf overlap-logs (bt2:with-lock-held (log-lock) (reverse logged)))
          (setf open-ids (mapcar #'trial-id (experiment-run-open-trials-list run)))
          (trial-close trial-a)
          (trial-close trial-b)))
      (check "a trial opened mid-create logs exactly one message"
             (= 1 (length overlap-logs)))
      (let ((warning (first overlap-logs)))
        (check "the mid-create overlap message is a warning"
               (eq (first warning) :warn))
        (check "the mid-create overlap warning names the newly opened trial"
               (and warning trial-b (search (trial-id trial-b) (second warning))))
        (check "the mid-create overlap warning names the trial still being created"
               (and warning trial-a (search (trial-id trial-a) (second warning)))))
      (check "both trials opened despite the overlap"
             (and trial-a trial-b
                  (trial-created-p trial-a) (trial-created-p trial-b)))
      (check "both trials are open after the race"
             (and (member (trial-id trial-a) open-ids :test #'equal)
                  (member (trial-id trial-b) open-ids :test #'equal))))

    ;; --- A log-fn that signals does not leave the trial id claimed ---
    ;;
    ;; The overlap warning runs between the claim and the create. A caller's
    ;; log-fn is arbitrary code; if it throws, the claim has to be released or
    ;; the retry fails with "already exists" and names the wrong cause.
    (let* ((calls (cons :calls nil))
           (boom t)
           (trial-a nil)
           (first-error nil)
           (retry-error nil)
           (retried nil))
      (multiple-value-bind (client get-requests)
          (make-test-client :eval-endpoint "https://sigil.example.test"
                            :generation-enabled nil :traces-enabled nil
                            :http-fn (routed-http calls "exp-logfn")
                            :log-fn (lambda (level component message &rest kvs)
                                      (declare (ignore level component message kvs))
                                      (when boom (error "log-fn boom"))))
        (declare (ignore get-requests))
        (with-experiment (run client :run-id "exp-logfn" :name "log-fn")
          (setf trial-a (experiment-run-open-trial run "case-a"))
          (handler-case
              (experiment-run-open-trial run "case-b")
            (error (e) (setf first-error (princ-to-string e))))
          (setf boom nil)
          (handler-case
              (setf retried (experiment-run-open-trial run "case-b"))
            (error (e) (setf retry-error (princ-to-string e))))
          (trial-close trial-a)
          (when retried (trial-close retried))))
      (check "a signalling log-fn surfaces its own error"
             (and first-error (search "log-fn boom" first-error)))
      (check "a signalling log-fn leaves the trial id free to retry"
             (and (null retry-error) retried (trial-created-p retried))))

    ;; --- Transport validation ---
    (multiple-value-bind (client get-requests)
        (make-test-client :eval-endpoint "https://sigil.example.test"
                          :generation-enabled nil :traces-enabled nil)
      (declare (ignore get-requests))
      (check "create-trial requires a trial id"
             (signals-condition-p
              (lambda () (create-trial client "exp-1" :test-case-id "c"))
              'sigil-validation-error))
      (check "create-trial requires a test case id"
             (signals-condition-p
              (lambda () (create-trial client "exp-1" :trial-id "t"))
              'sigil-validation-error))
      (check "finalize-trial requires a trial id"
             (signals-condition-p
              (lambda () (finalize-trial client "exp-1" "" :status "completed"))
              'sigil-validation-error))
      (check "trial routes require an experiment id"
             (signals-condition-p
              (lambda () (create-trial client "" :trial-id "t" :test-case-id "c"))
              'sigil-validation-error)))))

;;; ================================================================
;;; Local suite tests
;;; ================================================================

(defun run-suite-tests ()
  (with-test-suite ("Suites")

    ;; --- Test cases ---
    (let ((tc (make-test-case :id "case-1" :name "capital"
                              :description "a description"
                              :tags '("smoke" "fast")
                              :category "qa"
                              :input "hello" :expected "world"
                              :metadata (jobj "owner" "me"))))
      (check "test case keeps its id" (equal (test-case-test-case-id tc) "case-1"))
      (check "test case keeps name, description, and category"
             (and (equal (test-case-name tc) "capital")
                  (equal (test-case-description tc) "a description")
                  (equal (test-case-category tc) "qa")))
      (check "test case keeps tags" (equal (test-case-tags tc) '("smoke" "fast")))
      (check "test case keeps input and expected"
             (and (equal (test-case-input tc) "hello")
                  (equal (test-case-expected tc) "world"))))
    (check "test-case-id is an accepted spelling for id"
           (equal (test-case-test-case-id (make-test-case :test-case-id "c2")) "c2"))
    (check "a test case requires an id"
           (signals-condition-p (lambda () (make-test-case :id "")) 'sigil-validation-error))

    ;; --- Snapshots ---
    (let ((snapshot (test-case-snapshot
                     (make-test-case :id "case-1" :input "hello" :expected "world")
                     :suite-id "suite-1" :suite-version "v2")))
      (check "scalar input serializes as {\"value\": ...}"
             (and (hash-table-p (jget snapshot "input"))
                  (equal (jget* snapshot "input" "value") "hello")))
      (check "scalar expected serializes as {\"value\": ...}"
             (and (hash-table-p (jget snapshot "expected"))
                  (equal (jget* snapshot "expected" "value") "world")))
      (check "snapshot carries suite id and version"
             (and (equal (jget snapshot "suite_id") "suite-1")
                  (equal (jget snapshot "suite_version") "v2"))))
    (let ((snapshot (test-case-snapshot
                     (make-test-case :id "case-2"
                                     :input (jobj "question" "2+2?")
                                     :expected (jobj "answer" "4")))))
      (check "object input stays an object without wrapping"
             (and (equal (jget* snapshot "input" "question") "2+2?")
                  (null (jget* snapshot "input" "value"))))
      (check "object expected stays an object without wrapping"
             (equal (jget* snapshot "expected" "answer") "4")))
    (let ((snapshot (test-case-snapshot (make-test-case :id "case-3"))))
      (check "absent input serializes as an empty object"
             (and (hash-table-p (jget snapshot "input"))
                  (zerop (hash-table-count (jget snapshot "input")))))
      (check "absent expected serializes as an empty object"
             (and (hash-table-p (jget snapshot "expected"))
                  (zerop (hash-table-count (jget snapshot "expected"))))))
    (let ((snapshot (test-case-snapshot
                     (make-test-case :id "case-4" :input '("a" "b")))))
      (check "a plain list input is wrapped as a value array"
             (let ((value (jget* snapshot "input" "value")))
               (and (vectorp value) (equal (aref value 0) "a")))))
    (let ((snapshot (test-case-snapshot
                     (make-test-case :id "case-5" :input '(("q" . "2+2?"))))))
      (check "an alist input is treated as a mapping"
             (and (equal (jget* snapshot "input" "q") "2+2?")
                  (null (jget* snapshot "input" "value")))))
    (let ((snapshot (test-case-snapshot
                     (make-test-case :id "case-6"
                                     :input (list :question "what?" :n 2)
                                     :expected (list :answer "42")
                                     :metadata (list :owner "me")))))
      (check "a plist input is treated as a mapping"
             (and (equal (jget* snapshot "input" "question") "what?")
                  (eql (jget* snapshot "input" "n") 2)
                  (null (jget* snapshot "input" "value"))))
      (check "a plist expected is treated as a mapping"
             (equal (jget* snapshot "expected" "answer") "42"))
      (check "plist metadata survives the snapshot"
             (equal (jget* snapshot "metadata" "owner") "me")))
    (check "snapshot of nil is nil" (null (test-case-snapshot nil)))

    ;; --- Suites ---
    (let ((suite (make-test-suite :suite-id "suite-1" :version "v2" :name "smoke"
                                  :cases (list (make-test-case :id "case-1")
                                               (jobj "id" "case-2" "input" "hi")
                                               (list :id "case-3")))))
      (check "suite keeps id and version"
             (and (equal (test-suite-suite-id suite) "suite-1")
                  (equal (test-suite-version suite) "v2")))
      (check "suite builds cases from a test-case value, JSON, and a plist"
             (equal (mapcar #'test-case-test-case-id (test-suite-cases suite))
                    '("case-1" "case-2" "case-3")))
      (check "suite case built from JSON keeps its input"
             (equal (test-case-input (test-suite-case suite "case-2")) "hi"))
      (check "test-suite-case finds a case by id"
             (equal (test-case-test-case-id (test-suite-case suite "case-3")) "case-3"))
      (check "test-suite-case returns nil for an unknown id"
             (null (test-suite-case suite "nope"))))
    (check "a suite requires an id"
           (signals-condition-p (lambda () (make-test-suite :suite-id " "))
                                'sigil-validation-error))

    ;; --- A run carries its suite's id and version, and trials snapshot cases ---
    (let ((calls (cons :calls nil)))
      (multiple-value-bind (client get-requests)
          (make-test-client
           :eval-endpoint "https://sigil.example.test"
           :generation-enabled nil :traces-enabled nil
           :http-fn (lambda (url &key method headers content &allow-other-keys)
                      (declare (ignore headers))
                      (push (list method url content) (cdr calls))
                      (values (jzon:stringify (jobj "experiment_id" "exp-suite")) 200)))
        (declare (ignore get-requests))
        (let ((suite (make-test-suite
                      :suite-id "suite-1" :version "v2"
                      :cases (list (make-test-case :id "case-1"
                                                   :input "hello"
                                                   :expected "world")))))
          (with-experiment (run client :run-id "exp-suite" :name "suite run"
                                :suite suite)
            (experiment-run-open-trial run "case-1"))
          (let* ((ordered (reverse (cdr calls)))
                 (upsert (find-if (lambda (c) (search "experiment-runs:upsert" (second c)))
                                  ordered))
                 (trial (find-if (lambda (c) (search "/trials" (second c))) ordered)))
            (check "run payload includes the local suite id and version"
                   (and (equal (jget (jzon:parse (third upsert)) "suite_id") "suite-1")
                        (equal (jget (jzon:parse (third upsert)) "suite_version") "v2")))
            (check "a trial resolves its case from the run's suite"
                   (equal (jget* (jzon:parse (third trial)) "test_case" "input" "value")
                          "hello"))
            (check "the trial snapshot carries the suite reference"
                   (and (equal (jget* (jzon:parse (third trial)) "test_case" "suite_id")
                               "suite-1")
                        (equal (jget* (jzon:parse (third trial)) "test_case" "suite_version")
                               "v2")))))))))


;;; ================================================================
;;; Conversations tests
;;; ================================================================

(defun run-conversations-tests ()
  (with-test-suite ("Conversations")
    (labels ((message (role text)
               (jobj "role" role
                     "parts" (jarr (jobj "text" text))))
             (conversation (&rest generations)
               (jobj "generations" (coerce generations 'vector)))
             (signals-validation-p (thunk)
               (handler-case (progn (funcall thunk) nil)
                 (sigil-validation-error () t))))
      ;; Members response normalization.
      (check "member list from bare array"
             (= 1 (length (sigil-cl::%member-list (jarr (jobj "conversation_id" "c1"))))))
      (check "member list from members wrapper"
             (= 1 (length (sigil-cl::%member-list
                           (jobj "members" (jarr (jobj "conversation_id" "c1")))))))
      (check "member list from items wrapper"
             (= 1 (length (sigil-cl::%member-list
                           (jobj "items" (jarr (jobj "conversation_id" "c1")))))))
      (check "member list drops non-objects"
             (null (sigil-cl::%member-list (jarr "junk" 42))))
      (check "member list empty for unexpected body"
             (null (sigil-cl::%member-list "oops")))
      (check "member list empty members falls through to items"
             (= 1 (length (sigil-cl::%member-list
                           (jobj "members" (jarr)
                                 "items" (jarr (jobj "conversation_id" "c1")))))))

      ;; initial-user-prompt.
      (let ((conv (conversation
                   (jobj "started_at" "2026-01-02T00:00:00Z"
                         "input" (jarr (message "user" "later")))
                   (jobj "started_at" "2026-01-01T00:00:00Z"
                         "input" (jarr (message "system" "sys")
                                       (message "user" "first prompt")
                                       (message "user" "second prompt"))))))
        (check "initial-user-prompt picks earliest generation, last user message"
               (equal (initial-user-prompt conv) "second prompt")))
      (let ((conv (conversation
                   (jobj "started_at" "2026-01-01T00:00:00Z"
                         "input" (jarr (message "system" "sys")
                                       (message "assistant" "hi"))))))
        (check "initial-user-prompt falls back to first non-system"
               (equal (initial-user-prompt conv) "hi")))
      (check "initial-user-prompt accepts proto-style roles"
             (equal (initial-user-prompt
                     (conversation
                      (jobj "input" (jarr (message "MESSAGE_ROLE_SYSTEM" "sys")
                                          (message "MESSAGE_ROLE_USER" "proto prompt")))))
                    "proto prompt"))
      (check "initial-user-prompt empty for missing generations"
             (equal (initial-user-prompt (jobj)) ""))
      (check "initial-user-prompt concatenates text parts"
             (equal (initial-user-prompt
                     (conversation
                      (jobj "input" (jarr (jobj "role" "user"
                                                "parts" (jarr (jobj "text" "a ")
                                                              (jobj "text" "b")))))))
                    "a b"))

      ;; HTTP plumbing and dataset building against a mock eval API.
      (let ((urls nil))
        (multiple-value-bind (client get-requests)
            (make-test-client
             :eval-endpoint "https://sigil.example.test"
             :generation-enabled nil :traces-enabled nil
             :http-fn (lambda (url &key method headers content &allow-other-keys)
                        (declare (ignore method headers content))
                        (push url urls)
                        (cond
                          ((search "/eval/collections/" url)
                           (values (jzon:stringify
                                    (jobj "members"
                                          (jarr (jobj "conversation_id" "conv 1"
                                                      "saved_id" "sv1"
                                                      "name" "first")
                                                (jobj "conversation_id" "conv2")
                                                (jobj "name" "no conversation id"))))
                                   200))
                          ((search "/query/conversations/conv%201" url)
                           (values (jzon:stringify
                                    (jobj "generations"
                                          (jarr (jobj "started_at" "2026-01-01T00:00:00Z"
                                                      "input" (jarr (jobj "role" "user"
                                                                          "parts" (jarr (jobj "text" "prompt one"))))))))
                                   200))
                          ((search "/query/conversations/conv2" url)
                           (values (jzon:stringify (jobj "generations" (jarr))) 200))
                          (t (values "{}" 200)))))
          (declare (ignore get-requests))
          (let ((members (list-collection-members client "col-1")))
            (check "list-collection-members returns member objects"
                   (and (= (length members) 3)
                        (equal (jget (first members) "saved_id") "sv1"))))
          (check "collection members URL uses eval prefix"
                 (find-if (lambda (u) (search "/api/v1/eval/collections/col-1/members" u))
                          urls))
          (let ((conv (get-conversation client "conv 1")))
            (check "get-conversation parses conversation"
                   (vectorp (jget conv "generations"))))
          (check "get-conversation URL-encodes the id"
                 (find-if (lambda (u) (search "/api/v1/query/conversations/conv%201" u))
                          urls))
          (let ((items (dataset-from-collection client "col-1")))
            (check "dataset-from-collection builds one item" (= (length items) 1))
            (let ((item (first items)))
              (check "dataset item id prefers saved_id"
                     (equal (jget item "id") "sv1"))
              (check "dataset item input is initial prompt"
                     (equal (jget item "input") "prompt one"))
              (check "dataset item metadata links collection"
                     (and (equal (jget* item "metadata" "collection_id") "col-1")
                          (equal (jget* item "metadata" "conversation_id") "conv 1")
                          (equal (jget* item "metadata" "task_id") "sv1")
                          (equal (jget* item "metadata" "saved_name") "first")))))
          (let ((items (dataset-from-collection client "col-1" :skip-empty nil)))
            (check "dataset-from-collection keeps empty prompts when asked"
                   (= (length items) 2)))
          (let ((items (dataset-from-collection client "col-1" :limit 1)))
            (check "dataset-from-collection respects limit" (= (length items) 1)))
          (check "dataset-from-collection rejects :golden"
                 (signals-validation-p
                  (lambda () (dataset-from-collection client "col-1" :mode :golden))))
          (check "dataset-from-collection rejects unknown mode"
                 (signals-validation-p
                  (lambda () (dataset-from-collection client "col-1" :mode :weird))))
          (check "get-conversation requires id"
                 (signals-validation-p (lambda () (get-conversation client " "))))
          (check "list-collection-members requires id"
                 (signals-validation-p (lambda () (list-collection-members client "")))))))))

;;; ================================================================
;;; Metrics tests
;;; ================================================================

(defun %registry-series (registry)
  "Return all hist-state structs in REGISTRY as a list."
  (let ((out nil))
    (maphash (lambda (k st) (declare (ignore k)) (push st out))
             (sigil-cl::metric-registry-table registry))
    out))

(defun %series-named (registry name)
  "Return hist-states in REGISTRY whose metric name is NAME."
  (remove-if-not (lambda (st) (equal (sigil-cl::hist-state-name st) name))
                 (%registry-series registry)))

(defun %series-attr (st key)
  "Return the value of attribute KEY in hist-state ST, or NIL."
  (cdr (assoc key (sigil-cl::hist-state-attrs st) :test #'equal)))

(defun run-metrics-tests ()
  (with-test-suite ("Metrics")
    ;; --- OTLP datapoint field types ---
    (let* ((bounds sigil-cl::+duration-buckets+)
           (counts (append (make-list (length bounds) :initial-element 0) (list 0)))
           (dp (sigil-cl::build-histogram-datapoint
                (vector (otel-string-attr "gen_ai.operation.name" "generateText"))
                bounds counts 2.5d0 1 "100" "200")))
      (check "datapoint count is uint64 string" (equal (jget dp "count") "1"))
      (check "datapoint sum is a number" (numberp (jget dp "sum")))
      (check "datapoint bucketCounts are strings"
             (every #'stringp (jget dp "bucketCounts")))
      (check "datapoint explicitBounds length matches buckets"
             (= (length (jget dp "explicitBounds")) (length bounds)))
      (check "datapoint bucketCounts length = bounds + 1"
             (= (length (jget dp "bucketCounts")) (1+ (length bounds))))
      (check "datapoint startTimeUnixNano set" (equal (jget dp "startTimeUnixNano") "100"))
      (check "datapoint timeUnixNano set" (equal (jget dp "timeUnixNano") "200")))

    ;; --- OTLP resourceMetrics envelope shape ---
    (let* ((dp (sigil-cl::build-histogram-datapoint
                (vector) sigil-cl::+duration-buckets+
                (append (make-list (length sigil-cl::+duration-buckets+) :initial-element 0)
                        (list 0))
                1.0d0 1 "0" "1"))
           (metric (sigil-cl::build-otlp-metric "gen_ai.client.operation.duration" "s" (list dp)))
           (payload (sigil-cl::build-otlp-metrics-payload (list metric) "my-svc" "1.0")))
      (check "payload has resourceMetrics" (jget payload "resourceMetrics"))
      (let* ((rm (aref (jget payload "resourceMetrics") 0))
             (sm (aref (jget rm "scopeMetrics") 0)))
        (check "scope name is sigil-cl"
               (equal (jget* sm "scope" "name") "sigil-cl"))
        (let ((m (aref (jget sm "metrics") 0)))
          (check "metric name" (equal (jget m "name") "gen_ai.client.operation.duration"))
          (check "metric unit" (equal (jget m "unit") "s"))
          (check "metric histogram cumulative temporality"
                 (= (jget* m "histogram" "aggregationTemporality") 2))
          (check "metric histogram has dataPoints"
                 (plusp (length (jget* m "histogram" "dataPoints")))))))

    ;; --- Aggregation + bucket assignment ---
    (let ((reg (sigil-cl::make-metric-registry)))
      (sigil-cl::record-histogram reg "gen_ai.client.operation.duration" "s"
                                  sigil-cl::+duration-buckets+ nil 2.5d0)
      (let ((st (first (%series-named reg "gen_ai.client.operation.duration"))))
        (check "series created" (not (null st)))
        (check "count incremented" (= (sigil-cl::hist-state-count st) 1))
        (check "sum accumulated" (= (sigil-cl::hist-state-sum st) 2.5d0))
        (check "bucketCounts length = bounds + 1"
               (= (length (sigil-cl::hist-state-counts st))
                  (1+ (length sigil-cl::+duration-buckets+))))
        ;; 2.5s -> first bound >= 2.5 is 2.56 at index 8
        (check "2.5s lands in bucket index 8"
               (= (nth 8 (sigil-cl::hist-state-counts st)) 1))
        (check "no other bucket incremented"
               (= 1 (reduce #'+ (sigil-cl::hist-state-counts st))))))

    ;; --- Aggregation across events with same attrs collapses to one series ---
    (let ((reg (sigil-cl::make-metric-registry)))
      (sigil-cl::record-histogram reg "m" "s" sigil-cl::+duration-buckets+
                                  (list (cons "a" "1")) 0.5d0)
      (sigil-cl::record-histogram reg "m" "s" sigil-cl::+duration-buckets+
                                  (list (cons "a" "1")) 0.5d0)
      (check "same attrs -> one series" (= (length (%series-named reg "m")) 1))
      (check "count summed across events"
             (= (sigil-cl::hist-state-count (first (%series-named reg "m"))) 2)))

    ;; --- Generation produces all four histograms ---
    (multiple-value-bind (client get-requests) (make-test-client :metrics-enabled t)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :stream
                                          :model-provider "openai"
                                          :model-name "gpt-4"
                                          :agent-name "test-agent"
                                          :agent-version "1.0")))
        (set-result rec
                    :usage (make-token-usage :input 100 :output 50 :reasoning 10
                                             :cache-read 20 :cache-creation 5)
                    :output-messages (list (make-message
                                            :role :assistant
                                            :parts (list (make-tool-call-part
                                                          :id "tc1" :name "search"
                                                          :input-json "{}"))))
                    :duration-seconds 2.5d0
                    :ttft-seconds 0.3d0)
        (recorder-end rec)
        (let ((reg (sigil-cl::client-metric-registry client)))
          (check "operation.duration recorded"
                 (= (length (%series-named reg "gen_ai.client.operation.duration")) 1))
          (check "time_to_first_token recorded"
                 (= (length (%series-named reg "gen_ai.client.time_to_first_token")) 1))
          (check "tool_calls_per_operation recorded"
                 (= (length (%series-named reg "gen_ai.client.tool_calls_per_operation")) 1))
          ;; one token.usage series per non-zero token type (all 5 here)
          (let ((token-series (%series-named reg "gen_ai.client.token.usage")))
            (check "token.usage: one series per non-zero token type"
                   (= (length token-series) 5))
            (let ((types (mapcar (lambda (st) (%series-attr st "gen_ai.token.type"))
                                 token-series)))
              (check "token types present"
                     (and (member "input" types :test #'equal)
                          (member "output" types :test #'equal)
                          (member "reasoning" types :test #'equal)
                          (member "cache_read" types :test #'equal)
                          (member "cache_write" types :test #'equal)))))
          ;; identity + operation attrs on duration series
          (let ((dur (first (%series-named reg "gen_ai.client.operation.duration"))))
            (check "duration operation.name = streamText"
                   (equal (%series-attr dur "gen_ai.operation.name") "streamText"))
            (check "duration provider attr" (equal (%series-attr dur "gen_ai.provider.name") "openai"))
            (check "duration model attr" (equal (%series-attr dur "gen_ai.request.model") "gpt-4"))
            (check "duration agent name attr" (equal (%series-attr dur "gen_ai.agent.name") "test-agent"))
            (check "duration error.type empty (no error)"
                   (equal (%series-attr dur "error.type") ""))
            (check "duration 2.5s in bucket index 8"
                   (= (nth 8 (sigil-cl::hist-state-counts dur)) 1)))
          ;; tool_calls value = 1 -> bucket index 1 (bound 1)
          (let ((tc (first (%series-named reg "gen_ai.client.tool_calls_per_operation"))))
            (check "tool_calls sum = 1" (= (sigil-cl::hist-state-sum tc) 1d0))))))

    ;; --- Token type: only non-zero types produce series ---
    (multiple-value-bind (client get-requests) (make-test-client :metrics-enabled t)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "openai" :model-name "gpt-4")))
        (set-result rec :usage (make-token-usage :input 100 :output 0)
                        :duration-seconds 1.0d0)
        (recorder-end rec)
        (let* ((reg (sigil-cl::client-metric-registry client))
               (token-series (%series-named reg "gen_ai.client.token.usage")))
          (check "only input token.usage series (output zero)"
                 (= (length token-series) 1))
          (check "the single series is input type"
                 (equal (%series-attr (first token-series) "gen_ai.token.type") "input")))))

    ;; --- Export POSTs a resourceMetrics envelope to metrics-endpoint ---
    (multiple-value-bind (client get-requests) (make-test-client :metrics-enabled t)
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "openai" :model-name "gpt-4")))
        (set-result rec :usage (make-token-usage :input 10 :output 5)
                        :duration-seconds 1.0d0)
        (recorder-end rec)
        (client-flush client)
        (let* ((reqs (funcall get-requests))
               (metric-posts (remove-if-not
                              (lambda (r) (search "/v1/metrics" (first r)))
                              reqs)))
          (check "one metrics POST" (= (length metric-posts) 1))
          (let ((parsed (jzon:parse (second (first metric-posts)))))
            (check "POST body has resourceMetrics" (jget parsed "resourceMetrics"))
            (let* ((rm (aref (jget parsed "resourceMetrics") 0))
                   (sm (aref (jget rm "scopeMetrics") 0)))
              (check "POST scope name sigil-cl"
                     (equal (jget* sm "scope" "name") "sigil-cl"))
              (check "POST has metrics" (plusp (length (jget sm "metrics")))))))))

    ;; --- Embedding records duration + input token.usage ---
    (multiple-value-bind (client get-requests) (make-test-client :metrics-enabled t)
      (declare (ignore get-requests))
      (let ((rec (start-embedding client :model-provider "openai"
                                         :model-name "text-embedding-3-small")))
        (set-result rec :input-count 3 :input-tokens 42 :duration-seconds 0.5d0)
        (recorder-end rec)
        (let ((reg (sigil-cl::client-metric-registry client)))
          (check "embedding duration recorded"
                 (= (length (%series-named reg "gen_ai.client.operation.duration")) 1))
          (let ((dur (first (%series-named reg "gen_ai.client.operation.duration"))))
            (check "embedding op name" (equal (%series-attr dur "gen_ai.operation.name") "embeddings")))
          (let ((tok (%series-named reg "gen_ai.client.token.usage")))
            (check "embedding token.usage input series" (= (length tok) 1))
            (check "embedding token type input"
                   (equal (%series-attr (first tok) "gen_ai.token.type") "input"))))))

    ;; --- Tool execution records duration only ---
    (multiple-value-bind (client get-requests) (make-test-client :metrics-enabled t)
      (declare (ignore get-requests))
      (let ((rec (start-tool-execution client :tool-name "search" :tool-call-id "tc1")))
        (set-result rec :result "ok" :duration-seconds 0.2d0)
        (recorder-end rec)
        (let ((reg (sigil-cl::client-metric-registry client)))
          (check "tool execution duration recorded"
                 (= (length (%series-named reg "gen_ai.client.operation.duration")) 1))
          (let ((dur (first (%series-named reg "gen_ai.client.operation.duration"))))
            (check "tool op name" (equal (%series-attr dur "gen_ai.operation.name") "execute_tool"))
            (check "tool name attr" (equal (%series-attr dur "gen_ai.tool.name") "search"))))))

    ;; --- Duration falls back to started-at/completed-at when not set ---
    (multiple-value-bind (client get-requests) (make-test-client :metrics-enabled t)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "openai" :model-name "gpt-4")))
        (set-result rec :usage (make-token-usage :input 10 :output 5))
        (setf (sigil-cl::recorder-started-at rec) "2024-01-01T00:00:00Z")
        (setf (sigil-cl::recorder-completed-at rec) "2024-01-01T00:00:02Z")
        (recorder-end rec)
        (let* ((reg (sigil-cl::client-metric-registry client))
               (dur (first (%series-named reg "gen_ai.client.operation.duration"))))
          (check "generation duration from timestamps recorded" (not (null dur)))
          (check "generation duration from timestamps = 2s"
                 (= (sigil-cl::hist-state-sum dur) 2d0)))))
    (multiple-value-bind (client get-requests) (make-test-client :metrics-enabled t)
      (declare (ignore get-requests))
      (let ((rec (start-tool-execution client :tool-name "search" :tool-call-id "tc1")))
        (set-result rec :result "ok")
        (setf (sigil-cl::recorder-started-at rec) "2024-01-01T00:00:00Z")
        (setf (sigil-cl::recorder-completed-at rec) "2024-01-01T00:00:01Z")
        (recorder-end rec)
        (let* ((reg (sigil-cl::client-metric-registry client))
               (dur (first (%series-named reg "gen_ai.client.operation.duration"))))
          (check "tool duration from timestamps recorded" (not (null dur)))
          (check "tool duration from timestamps = 1s"
                 (= (sigil-cl::hist-state-sum dur) 1d0)))))
    (multiple-value-bind (client get-requests) (make-test-client :metrics-enabled t)
      (declare (ignore get-requests))
      (let ((rec (start-embedding client :model-provider "openai"
                                         :model-name "text-embedding-3-small")))
        (set-result rec :input-count 1 :input-tokens 10)
        (setf (sigil-cl::recorder-started-at rec) "2024-01-01T00:00:00Z")
        (setf (sigil-cl::recorder-completed-at rec) "2024-01-01T00:00:03Z")
        (recorder-end rec)
        (let* ((reg (sigil-cl::client-metric-registry client))
               (dur (first (%series-named reg "gen_ai.client.operation.duration"))))
          (check "embedding duration from timestamps recorded" (not (null dur)))
          (check "embedding duration from timestamps = 3s"
                 (= (sigil-cl::hist-state-sum dur) 3d0)))))

    (multiple-value-bind (client get-requests)
        (make-test-client :metrics-enabled t :workflow-steps-enabled t)
      (declare (ignore get-requests))
      (let ((rec (sigil-cl::start-workflow-step client :conversation-id "conv-1"
                                                       :step-name "classify")))
        (setf (sigil-cl::recorder-started-at rec) "2024-01-01T00:00:00Z")
        (setf (sigil-cl::recorder-completed-at rec) "2024-01-01T00:00:01Z")
        (sigil-cl::recorder-end rec)
        (let* ((reg (sigil-cl::client-metric-registry client))
               (dur (first (%series-named reg "gen_ai.client.operation.duration"))))
          (check "workflow step duration from timestamps recorded" (not (null dur)))
          (check "workflow step duration from timestamps = 1s"
                 (= (sigil-cl::hist-state-sum dur) 1d0)))))

    ;; --- Explicit duration-seconds wins over timestamps ---
    (multiple-value-bind (client get-requests) (make-test-client :metrics-enabled t)
      (declare (ignore get-requests))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "openai" :model-name "gpt-4")))
        (set-result rec :usage (make-token-usage :input 10 :output 5)
                        :duration-seconds 0.5d0)
        (setf (sigil-cl::recorder-started-at rec) "2024-01-01T00:00:00Z")
        (setf (sigil-cl::recorder-completed-at rec) "2024-01-01T00:00:02Z")
        (recorder-end rec)
        (let* ((reg (sigil-cl::client-metric-registry client))
               (dur (first (%series-named reg "gen_ai.client.operation.duration"))))
          (check "explicit duration wins over timestamps"
                 (= (sigil-cl::hist-state-sum dur) 0.5d0)))))

    ;; --- Client config tags promoted onto metric series; per-call tags are not ---
    (let ((client (make-client
                   (make-config :metrics-endpoint "http://x/v1/metrics"
                                :metrics-enabled t
                                :service-name "test-service"
                                :tags '(("env" . "prod")))
                   :env-fn (constantly nil))))
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "openai" :model-name "gpt-4"
                                          :tags '(("request_id" . "r-1")))))
        (set-result rec :usage (make-token-usage :input 100 :output 50)
                        :duration-seconds 1.0d0)
        (recorder-end rec)
        (let ((reg (sigil-cl::client-metric-registry client)))
          (let ((dur (first (%series-named reg "gen_ai.client.operation.duration"))))
            (check "duration series carries sigil.tag.env"
                   (equal (%series-attr dur "sigil.tag.env") "prod"))
            (check "duration series omits per-call tag"
                   (null (%series-attr dur "sigil.tag.request_id"))))
          (let ((tok (first (%series-named reg "gen_ai.client.token.usage"))))
            (check "token.usage series carries sigil.tag.env"
                   (equal (%series-attr tok "sigil.tag.env") "prod"))
            (check "token.usage series omits per-call tag"
                   (null (%series-attr tok "sigil.tag.request_id")))))))

    ;; --- Disabled by default: no recording, no POST ---
    (multiple-value-bind (client get-requests) (make-test-client) ; metrics-enabled nil
      (let ((rec (start-generation client :mode :sync
                                          :model-provider "openai" :model-name "gpt-4")))
        (set-result rec :usage (make-token-usage :input 100 :output 50)
                        :duration-seconds 1.0d0)
        (recorder-end rec)
        (client-flush client)
        (let ((reg (sigil-cl::client-metric-registry client)))
          (check "registry empty when metrics disabled"
                 (zerop (hash-table-count (sigil-cl::metric-registry-table reg)))))
        (let ((metric-posts (remove-if-not
                             (lambda (r) (search "/v1/metrics" (first r)))
                             (funcall get-requests))))
          (check "no metrics POST when disabled" (null metric-posts)))))))

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
                           #'run-env-tests
                           #'run-queue-tests
                           #'run-otel-tests
                           #'run-recorder-tests
                           #'run-workflow-step-tests
                           #'run-client-tests
                           #'run-macro-tests
                           #'run-normalize-tests
                           #'run-experiment-tests
                           #'run-trial-tests
                           #'run-suite-tests
                           #'run-conversations-tests
                           #'run-metrics-tests))
      (multiple-value-bind (ok pass fail) (funcall test-fn)
        (declare (ignore ok))
        (incf total-pass pass)
        (incf total-fail fail)))
    (format t "~%============================~%")
    (format t "TOTAL: ~d passed, ~d failed~%" total-pass total-fail)
    (values (zerop total-fail) total-pass total-fail)))
