(in-package :sigil-cl)

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

(defun build-auth-headers (config)
  "Build auth headers based on config auth-mode. Returns alist.
For :basic mode, uses tenant-id as username when auth-user is omitted
(standard Grafana Cloud configuration). Any user-supplied
config-extra-headers are merged in afterwards; on case-insensitive name
collisions the user-supplied header wins (so SIGIL_HEADERS=Authorization=...
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
    (let ((extras (config-extra-headers config)))
      (when extras
        (let ((merged (remove-if (lambda (kv)
                                   (%has-header-p extras (car kv)))
                                 headers)))
          (setf headers (append merged (copy-list extras))))))
    headers))

(defun build-traces-auth-headers (config)
  "Build auth headers for trace export.
When traces-forward-auth is true (default), forwards the same auth headers.
When false, sends no auth for traces."
  (if (config-traces-forward-auth config)
      (build-auth-headers config)
      nil))
