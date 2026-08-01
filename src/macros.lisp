(in-package :sigil-cl)

(defmacro with-generation ((var client &rest initargs) &body body)
  "Execute BODY with a generation recorder bound to VAR.
Binds *trace-context* per-thread so child tool/embedding spans
are correctly parented to this generation."
  `(let ((,var (start-generation ,client ,@initargs)))
     (let ((*trace-context* (list :trace-id (gen-rec-trace-id ,var)
                                  :span-id (gen-rec-span-id ,var))))
       (unwind-protect
            (progn ,@body)
         (recorder-end ,var)))))

(defmacro with-tool-execution ((var client &rest initargs) &body body)
  "Execute BODY with a tool execution recorder bound to VAR.
Calls recorder-end in unwind-protect."
  `(let ((,var (start-tool-execution ,client ,@initargs)))
     (unwind-protect
          (progn ,@body)
       (recorder-end ,var))))

(defmacro with-embedding ((var client &rest initargs) &body body)
  "Execute BODY with an embedding recorder bound to VAR.
Calls recorder-end in unwind-protect."
  `(let ((,var (start-embedding ,client ,@initargs)))
     (unwind-protect
          (progn ,@body)
       (recorder-end ,var))))

(defmacro with-workflow-step ((var client &rest initargs) &body body)
  "Execute BODY with a workflow step recorder bound to VAR.
Binds *trace-context* per-thread so child generation/tool/embedding spans
are correctly parented to this workflow step."
  `(let ((,var (start-workflow-step ,client ,@initargs)))
     (let ((*trace-context* (list :trace-id (wfs-rec-trace-id ,var)
                                  :span-id (wfs-rec-span-id ,var))))
       (unwind-protect
            (progn ,@body)
         (recorder-end ,var)))))

;;; --- Carrying telemetry onto a thread the caller spawns ---
;;;
;;; *EXPERIMENT-RUN* and *TRACE-CONTEXT* are both thread-confined, and
;;; START-GENERATION reads them together. A thread that keeps the run but
;;; loses the trace context produces a tracked generation on an orphan trace,
;;; which is worse than losing both, so they are captured and replayed as a
;;; single unit.

(defun capture-telemetry-context ()
  "Snapshot the calling thread's telemetry specials for replay on another thread.
Call this on the thread that owns the context. A capture taken inside a
spawned thread's own body runs after that thread already started at the
global values, so it carries nothing."
  (list :experiment-run *experiment-run*
        :trace-context *trace-context*))

(defmacro with-telemetry-context ((context) &body body)
  "Execute BODY with the specials captured by CAPTURE-TELEMETRY-CONTEXT rebound.
CONTEXT is evaluated once. A NIL context binds both to NIL, which is what a
fresh thread would see anyway."
  (let ((ctx (gensym "CTX-")))
    `(let* ((,ctx ,context)
            (*experiment-run* (getf ,ctx :experiment-run))
            (*trace-context* (getf ,ctx :trace-context)))
       ,@body)))

(defun telemetry-context-thunk (thunk)
  "Wrap THUNK so it runs with the calling thread's telemetry context.
Capture happens now, at wrap time, on the caller. Rebinding happens when the
returned closure is called, wherever that is. Pass the result straight to a
thread constructor."
  (let ((context (capture-telemetry-context)))
    (lambda ()
      (with-telemetry-context (context)
        (funcall thunk)))))

(defmacro with-span ((client name &key (kind 1) attributes-var) &body body)
  "Execute BODY wrapped in a Sigil OTel span.
Zero overhead when traces are disabled on CLIENT's config.
NAME is a string (evaluated). KIND: 1=INTERNAL (default), 3=CLIENT.
ATTRIBUTES-VAR: lexical variable (list) the body can push otel-*-attr items onto."
  (let ((attrs-var (or attributes-var (gensym "ATTRS-")))
        (start-nano (gensym "START-"))
        (ok (gensym "OK-"))
        (err-type (gensym "ERR-"))
        (vals (gensym "VALS-"))
        (client-var (gensym "CLIENT-")))
    `(let ((,attrs-var nil)
           (,client-var ,client))
       (declare (ignorable ,attrs-var))
       (if (not (config-traces-enabled (client-config ,client-var)))
           (progn ,@body)
           (let ((,start-nano (current-unix-nano))
                 (,ok t)
                 (,err-type nil)
                 (,vals nil))
             (unwind-protect
                  (handler-case
                      (progn
                        (setf ,vals (multiple-value-list (progn ,@body)))
                        (values-list ,vals))
                    (error (e)
                      (setf ,ok nil
                            ,err-type (princ-to-string (type-of e)))
                      (error e)))
               (handler-case
                   (let* ((end-nano (current-unix-nano))
                          (cfg (client-config ,client-var))
                          (trace-id (let ((ctx *trace-context*))
                                      (or (getf ctx :trace-id) (generate-trace-id))))
                          (span-id (generate-span-id))
                          (parent-span-id (getf *trace-context* :span-id))
                          (base-attrs (list (otel-string-attr "sigil.sdk.name" +sdk-name+))))
                     (let ((agent (or (config-agent-name cfg)
                                      (config-service-name cfg)))
                           (agent-ver (or (config-agent-version cfg)
                                          (config-service-version cfg))))
                       (when (and agent (plusp (length agent)))
                         (push (otel-string-attr "gen_ai.agent.name" agent) base-attrs))
                       (when (and agent-ver (plusp (length agent-ver)))
                         (push (otel-string-attr "gen_ai.agent.version" agent-ver) base-attrs)))
                     (dolist (kv (prefixed-tag-pairs (config-tags cfg)))
                       (push (otel-string-attr (car kv) (cdr kv)) base-attrs))
                     (dolist (a ,attrs-var)
                       (push a base-attrs))
                     (queue-enqueue
                      (client-trace-queue ,client-var)
                      (build-span :trace-id trace-id
                                  :span-id span-id
                                  :parent-span-id parent-span-id
                                  :name ,name
                                  :kind ,kind
                                  :start-time-unix-nano ,start-nano
                                  :end-time-unix-nano end-nano
                                  :attributes (coerce (nreverse base-attrs) 'vector)
                                  :status-code (if ,ok 1 2)
                                  :status-message (or ,err-type "")))
                     (bt2:with-lock-held ((client-lock ,client-var))
                       (bt2:condition-notify (client-wake-cv ,client-var))))
                 (error (e)
                   (handler-case
                       (sigil-log (client-config ,client-var) :warn "span"
                                 (format nil "span recording failed: ~a" (princ-to-string e)))
                     (error () nil))))))))))
