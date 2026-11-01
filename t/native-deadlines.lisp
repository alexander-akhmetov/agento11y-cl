(in-package :agento11y-cl)

(defmacro native-deadline-regression-cases ()
  '(labels ((check-case (name value) (agento11y-cl/t::check name value))
             (config (&rest options)
               (apply #'make-config
                      (append options (list :http-fn nil :max-retries 3
                                            :export-timeout-sec 3
                                            :log-fn (lambda (&rest args)
                                                      (declare (ignore args)))))))
             (read-request (stream)
               ;; The test sends only ASCII JSON, with Content-Length.
               (let ((line (read-line stream nil nil)) (length 0))
                 (unless line (error "Receiver saw EOF"))
                 (loop for header = (string-right-trim '(#\Return)
                                                       (read-line stream))
                       until (zerop (length header))
                       do (when (search "Content-Length:" header :test #'char-equal)
                            (setf length (parse-integer header :start 15))))
                 (dotimes (i length) (read-char stream))))
             (reply (stream &optional (body "{}"))
               (format stream "HTTP/1.1 200 OK~c~cContent-Length: ~d~c~cContent-Type: application/json~c~c~c~c~a"
                       #\Return #\Newline (length body) #\Return #\Newline
                       #\Return #\Newline #\Return #\Newline body)
               (finish-output stream))
             (receiver-case (name mode &key (warm t) outer (budget 0.12d0)
                                              (attempt-timeout 3))
               (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                                      :reuse-address t
                                                      :element-type 'character))
                      (port (usocket:get-local-port listener))
                      (url (format nil "~a://127.0.0.1:~d/v1/traces"
                                   (if (eq mode :tls-blocked) "https" "http") port))
                      (accepted 0) (requests 0) (server-error nil)
                      (dexador:*connection-pool* (dexador:make-connection-pool))
                      (dexador.util:*default-proxy* nil)
                      (thread
                        (sb-thread:make-thread
                         (lambda ()
                           (handler-case
                               (sb-sys:with-deadline (:seconds 4)
                                 (unless (usocket:wait-for-input listener :timeout 3 :ready-only t)
                                   (error "Receiver timed out waiting for a connection"))
                                 (let ((socket (usocket:socket-accept listener)))
                                   (incf accepted)
                                   (unwind-protect
                                        (let ((stream (usocket:socket-stream socket)))
                                          (when warm
                                            (read-request stream)
                                            (incf requests)
                                            (reply stream))
                                          (unless (eq mode :tls-blocked)
                                            (read-request stream)
                                            (incf requests))
                                          (ecase mode
                                            (:tls-blocked (sleep 0.65))
                                            (:retry
                                             (format stream "HTTP/1.1 503 Service Unavailable~c~cContent-Length: 0~c~c~c~c"
                                                     #\Return #\Newline #\Return #\Newline
                                                     #\Return #\Newline)
                                             (finish-output stream))
                                            (:blocked (sleep 0.65) (reply stream))
                                            (:drip
                                             (format stream "HTTP/1.1 200 OK~c~cContent-Length: 12~c~c~c~c"
                                                     #\Return #\Newline #\Return #\Newline
                                                     #\Return #\Newline)
                                             (finish-output stream)
                                             (dotimes (i 12)
                                               (write-char #\x stream)
                                               (finish-output stream)
                                               (sleep 0.055)))
                                            (:ok (reply stream))))
                                     (usocket:socket-close socket))))
                             ;; A client deadline may close the connection mid-reply.
                             (serious-condition (e) (setf server-error e))))
                         :name "native deadline loopback receiver")))
                 (unwind-protect
                      (progn
                        (when warm
                          (check-case (concatenate 'string name " warm request succeeds")
                                      (nth-value 0 (post-with-retry (config) url "{}" nil
                                                                   "generations" 1))))
                        (let* ((*export-deadline* (monotonic-deadline budget))
                               (start (get-internal-real-time))
                               (result
                                 (if outer
                                     (sb-sys:with-deadline (:seconds outer)
                                       (multiple-value-list
                                        (post-with-retry (config :max-retries 3
                                                                 :initial-backoff-sec 0.5
                                                                 :max-backoff-sec 0.5
                                                                 :export-timeout-sec attempt-timeout)
                                                         url "{}" nil
                                                         "generations" 1)))
                                     (multiple-value-list
                                      (post-with-retry (config :export-timeout-sec attempt-timeout)
                                                       url "{}" nil "generations" 1))))
                               (elapsed (/ (- (get-internal-real-time) start)
                                           (float internal-time-units-per-second 1d0))))
                          (format t "~&  ~a: ~,3fs ~s~%" name elapsed (third result))
                          (if (eq mode :ok)
                              (check-case name (first result))
                              (progn
                                (check-case (concatenate 'string name " classified timeout")
                                            (and (not (first result))
                                                 (eq :timeout (getf (third result) :kind))
                                                 (= 1 (getf (third result) :attempts))))
                                (check-case (concatenate 'string name " bounded elapsed time")
                                            (< elapsed (if outer 0.30d0 0.40d0)))))))
                   (sb-thread:join-thread thread)
                   (dexador:clear-connection-pool)
                   (usocket:socket-close listener))
                 (check-case (concatenate 'string name " uses native connection")
                             (and (= accepted 1)
                                  (= requests (cond (warm 2)
                                                    ((eq mode :tls-blocked) 0)
                                                    (t 1)))))
                 (when (eq mode :ok)
                   (check-case "receiver completed without errors" (null server-error))))))
      (let ((original (symbol-function 'dexador:request))
            (dexador:*dexador-backend* :usocket))
        (unwind-protect
             (dolist (budget '(nil 2))
               (let ((*export-deadline* (monotonic-deadline budget)) (calls 0))
                 (setf (symbol-function 'dexador:request)
                       (lambda (&rest args)
                         (declare (ignore args))
                         (incf calls)
                         (when (= calls 1) (error 'usocket:timeout-error))
                         (values "{}" 200)))
                 (multiple-value-bind (ok body diagnostic)
                     (post-with-retry (config) "http://127.0.0.1:1/" "{}" nil
                                      "generations" 1)
                   (declare (ignore body))
                   (check-case (format nil "native socket timeout is terminal with budget ~s" budget)
                               (and (not ok) (= calls 1)
                                    (eq :timeout (getf diagnostic :kind))
                                    (eql 1 (getf diagnostic :attempts)))))))
          (setf (symbol-function 'dexador:request) original)))
      (receiver-case "warm pooled blocked read" :blocked)
      (receiver-case "warm pooled slow drip" :drip)
      (receiver-case "outer earlier deadline" :blocked :outer 0.06d0 :budget 1d0)
      (receiver-case "native success" :ok)
      (receiver-case "fresh blocked read" :blocked :warm nil)
      (receiver-case "configured timeout without export deadline" :drip
                     :budget nil :attempt-timeout 0.12d0)
      (receiver-case "outer deadline during native retry backoff" :retry
                     :outer 0.06d0 :budget 1d0)
      (receiver-case "blocked TLS handshake" :tls-blocked :warm nil)
      ;; A private pool lock prevents any socket operation from starting.
      (let* ((dexador:*connection-pool* (dexador:make-connection-pool))
             (dexador.util:*default-proxy* nil)
             (lock (dexador.connection-cache::lru-pool-lock dexador:*connection-pool*))
             (ready (sb-thread:make-semaphore))
             (holder (sb-thread:make-thread
                      (lambda ()
                        (bt2:with-lock-held (lock)
                          (sb-thread:signal-semaphore ready)
                          (sleep 0.65))))))
        (unwind-protect
             (progn
               (sb-thread:wait-on-semaphore ready)
               (let* ((*export-deadline* (monotonic-deadline 0.1d0))
                      (start (get-internal-real-time))
                      (result (multiple-value-list
                               (post-with-retry (config :max-retries 3)
                                                "http://127.0.0.1:1/" "{}" nil
                                                "generations" 1)))
                      (elapsed (/ (- (get-internal-real-time) start)
                                  (float internal-time-units-per-second 1d0))))
                 (check-case "native pool lock wait expires without retry"
                             (and (not (first result))
                                  (eq :timeout (getf (third result) :kind))
                                  (= 1 (getf (third result) :attempts))
                                  (< elapsed 0.35d0)))))
          (sb-thread:join-thread holder)))
      ;; No HTTP function is installed here; an expired budget must not connect.
      (let ((*export-deadline* (monotonic-deadline 0)))
        (multiple-value-bind (ok body diagnostic)
            (post-with-retry (config) "http://127.0.0.1:1/" "{}" nil "generations" 1)
          (declare (ignore body))
          (check-case "expired deadline makes zero attempts"
                      (and (not ok) (eq :timeout (getf diagnostic :kind))
                           (zerop (getf diagnostic :attempts))))))
      ;; Custom transports retain their existing signature and own cancellation.
      (let ((called nil) (*export-deadline* (monotonic-deadline 0.01d0)))
        (check-case "custom transport remains caller responsibility"
                    (post-with-retry
                     (config :http-fn (lambda (url &key headers content)
                                        (declare (ignore url headers content))
                                        (sleep 0.03) (setf called t) (values "{}" 200)))
                     "unused" "{}" nil "generations" 1))
        (check-case "custom transport called" called))
      ;; Exercise reader branches on SBCL; this is not a non-SBCL runtime test.
      (let ((native (symbol-function 'do-http-post)))
        (unwind-protect
             (dolist (features (list (remove :sbcl *features*)
                                    (adjoin :windows *features*)))
               (let ((*features* features)
                     (*package* (find-package :agento11y-cl)))
                 (with-open-file (source "src/exporter.lisp")
                   (read source) ; IN-PACKAGE
                   (read source) ; DEFVAR
                   (eval (read source))))
               (check-case "unsupported Lisp/backend signals explicit condition"
                           (handler-case
                               (progn (do-http-post (config) "http://127.0.0.1:1/" nil "{}")
                                      nil)
                             (agento11y-export-error (e)
                               (eq :unsupported-deadline
                                   (getf (agento11y-export-error-diagnostics e) :kind)))))
               (let ((diagnostic
                       (nth-value 2 (post-with-retry (config :max-retries 3)
                                                    "http://127.0.0.1:1/" "{}" nil
                                                    "generations" 1))))
                 (check-case "unsupported deadline is terminal and not transport"
                             (and (eq :unsupported-deadline (getf diagnostic :kind))
                                  (= 1 (getf diagnostic :attempts)))))
               (check-case "unsupported Lisp/backend permits custom HTTP function"
                           (post-with-retry
                            (config :http-fn (lambda (&rest args)
                                              (declare (ignore args)) (values "{}" 200)))
                            "unused" "{}" nil "generations" 1)))
          (setf (symbol-function 'do-http-post) native)))))
