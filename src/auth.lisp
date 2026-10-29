(in-package :agento11y-cl)

(defun encode-basic-auth (user password)
  "Encode USER:PASSWORD as Base64 for HTTP Basic Auth."
  (cl-base64:string-to-base64-string (format nil "~a:~a" user password)))

(defun %header-name-eq (a b)
  "Case-insensitive string equality for HTTP header names."
  (and (stringp a) (stringp b)
       (string-equal a b)))

(defun %has-header-p (headers name)
  "True when NAME is already present in HEADERS (alist), case-insensitive."
  (some (lambda (kv) (%header-name-eq (car kv) name)) headers))

(defun %dedupe-headers (headers)
  "Collapse case-insensitive duplicate header names in HEADERS (alist).
The last occurrence of each name wins; the surviving entry keeps the
position of the first occurrence so caller-defined ordering is preserved."
  (let ((result nil))
    (dolist (kv headers)
      (let ((existing (assoc (car kv) result :test #'%header-name-eq)))
        (if existing
            (setf (cdr existing) (cdr kv))
            (setf result (append result (list (cons (car kv) (cdr kv))))))))
    result))

(defun build-auth-headers (config &key (include-extra-headers t))
  "Build auth headers based on config auth-mode. Returns alist.
For :basic mode, uses tenant-id as username when auth-user is omitted
(standard Grafana Cloud configuration). Any user-supplied
config-extra-headers are merged in afterwards; on case-insensitive name
collisions the user-supplied header wins (so AGENTO11Y_HEADERS=Authorization=...
can override the auth-mode-derived value)."
  (let ((headers nil))
    (case (config-auth-mode config)
      (:basic
       (let ((password (config-auth-password config)))
         (when password
           (let ((user (or (config-auth-user config)
                           (config-tenant-id config))))
             (when user
               (push (cons "Authorization"
                           (format nil "Basic ~a" (encode-basic-auth user password)))
                     headers)))))
       (when (config-tenant-id config)
         (push (cons "X-Scope-OrgID" (config-tenant-id config)) headers)))
      (:bearer
       (when (config-auth-password config)
         (push (cons "Authorization"
                     (format nil "Bearer ~a" (config-auth-password config)))
               headers)))
      (:tenant
       (when (config-tenant-id config)
         (push (cons "X-Scope-OrgID" (config-tenant-id config)) headers))))
    (let ((extras (when (and include-extra-headers (config-extra-headers config))
                    (%dedupe-headers (config-extra-headers config)))))
      (when extras
        (let ((merged (remove-if (lambda (kv)
                                   (%has-header-p extras (car kv)))
                                 headers)))
          (setf headers (append merged extras)))))
    headers))

(defun %http-origin (url)
  "Return a normalized HTTP origin, or NIL when URL is not safe to compare."
  (when (stringp url)
    (let ((separator (search "://" url)))
      (when separator
        (let* ((scheme (string-downcase (subseq url 0 separator)))
               (start (+ separator 3))
               (end (or (position-if (lambda (c) (find c "/?#")) url :start start)
                        (length url)))
               (authority (string-downcase (subseq url start end))))
          (when (and (member scheme '("http" "https") :test #'string=)
                     (plusp (length authority))
                     (not (find-if (lambda (c) (or (find c "@\\")
                                                  (find c '(#\Space #\Tab #\Newline #\Return))))
                                   authority)))
            (let ((port (if (string= scheme "https") ":443" ":80")))
              (when (and (>= (length authority) (length port))
                         (string= port authority :start2 (- (length authority) (length port))))
                (setf authority (subseq authority 0 (- (length authority) (length port))))))
            (cons scheme authority)))))))

(defun %build-signal-auth-headers (config endpoint headers header-key forward forward-key)
  (let* ((explicit-forward (config-option-supplied-p config forward-key))
         (signal-headers-p (or headers (config-option-supplied-p config header-key)))
         (api-origin (%http-origin (or (config-generation-endpoint config)
                                       (config-api-endpoint config))))
         (signal-origin (%http-origin endpoint))
         ;; Ambient API credentials must not cross an origin boundary.
         (inherit (and forward
                       (or explicit-forward
                           (and (not signal-headers-p) api-origin signal-origin
                                (equal api-origin signal-origin))))))
    (%dedupe-headers
     (append (when inherit
               ;; Forwarding API extras requires the caller's explicit opt-in.
               (build-auth-headers config :include-extra-headers explicit-forward))
             headers))))

(defun build-traces-auth-headers (config)
  "Build trace headers. Signal headers win over explicitly forwarded API auth.
Without an explicit forwarding choice, only same-origin auth is inherited;
API extra headers are never inherited automatically. NIL disables forwarding,
not the independently configured trace headers."
  (%build-signal-auth-headers config (config-traces-endpoint config)
                              (config-traces-headers config) :traces-headers
                              (config-traces-forward-auth config) :traces-forward-auth))

(defun build-metrics-auth-headers (config)
  "Build metric headers using the same isolation rules as trace export."
  (%build-signal-auth-headers config (config-metrics-endpoint config)
                              (config-metrics-headers config) :metrics-headers
                              (config-metrics-forward-auth config) :metrics-forward-auth))
