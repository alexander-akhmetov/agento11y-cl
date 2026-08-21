(in-package :agento11y-cl)

(defmacro with-generation ((var client &rest initargs) &body body)
  "Execute BODY with a generation recorder bound to VAR.
Binds *trace-context* per-thread so child tool/embedding spans
are correctly parented to this generation."
  `(let ((,var (start-generation ,client ,@initargs)))
     (let ((*trace-context* (child-trace-context
                             (gen-rec-trace-id ,var) (gen-rec-span-id ,var)
                             :content-capture-mode
                             (recorder-content-capture-mode ,var))))
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
     (let ((*trace-context* (child-trace-context
                             (wfs-rec-trace-id ,var) (wfs-rec-span-id ,var))))
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

(defmacro with-span ((client name &key (kind 1) attributes-var links) &body body)
  "Execute BODY wrapped in an OTel span.
Zero overhead when CLIENT exports no spans at all. The test is
SPANS-EXPORT-ACTIVE-P rather than the traces-enabled flag, because in otel
generation mode the traces endpoint is the generation destination and the flag
is unset: reading the flag alone left BODY without a *trace-context* and every
generation inside it a root span.
NAME is a string, evaluated once before BODY. KIND: 1=INTERNAL (default),
3=CLIENT.
ATTRIBUTES-VAR: lexical variable (list) the body can push otel-*-attr items onto.
LINKS: a form yielding a list of trace-context plists naming related spans in
other traces. It is evaluated once, in the unwind, so a caller can build it from
whatever BODY learned. A link never changes this span's trace, parent or
sampling, so an unusable one costs the link alone.

The span's identifiers are minted BEFORE BODY runs, and *trace-context* is bound
to them for BODY's extent, so anything opened inside -- a nested with-span, a
generation, a tool execution, an embedding -- parents under this span. Minting
them in the unwind instead published no context at all: every nested span read
the same ambient value this one did and came out a sibling, so a caller that
nested spans got a flat trace whose shape said nothing about what called what.

A span that completes reports status Unset, not Ok. The conventions reserve Ok
for an application that has decided the operation succeeded on its own terms;
an instrumentation library reporting it for every span that did not signal
leaves a caller no way to say otherwise. A span whose body signalled reports
Error, with the condition's own message as the status description -- the type
name alone said only that something of that class was raised, which for a
caller that signals one condition type to mark its spans is no information at
all. Rendering the condition can itself signal -- a report method that reads a
slot the condition was built without does -- so the type name remains the
fallback.

The exported span reads its trace id, span id and parent back OUT of that bound
context rather than from the lexicals it was minted into, so BODY can re-root
the span it is inside by SETF-ing the plist. A server span that must adopt an
inbound trace only after authenticating the caller is why: reading the lexicals
made such a write a silent no-op, exporting the span under the local trace while
the caller was told it had been adopted."
  (let ((attrs-var (or attributes-var (gensym "ATTRS-")))
        (start-nano (gensym "START-"))
        (ok (gensym "OK-"))
        (err-message (gensym "ERR-"))
        (vals (gensym "VALS-"))
        (ctx-var (gensym "CTX-"))
        (links-var (gensym "LINKS-"))
        (client-var (gensym "CLIENT-"))
        (name-var (gensym "NAME-"))
        (trace-id-var (gensym "TRACE-ID-"))
        (span-id-var (gensym "SPAN-ID-"))
        (parent-var (gensym "PARENT-SPAN-ID-")))
    `(let ((,attrs-var nil)
           (,client-var ,client))
       (declare (ignorable ,attrs-var))
       (if (not (spans-export-active-p (client-config ,client-var)))
           (progn ,@body)
           (let* ((,name-var ,name)
                  (,trace-id-var (or (getf *trace-context* :trace-id)
                                     (generate-trace-id)))
                  (,parent-var (getf *trace-context* :span-id))
                  (,span-id-var (generate-span-id))
                  (,ctx-var (child-trace-context ,trace-id-var ,span-id-var
                                                 :parent-span-id ,parent-var))
                  (,start-nano (current-unix-nano))
                  (,ok t)
                  (,err-message nil)
                  (,vals nil))
             (unwind-protect
                  (let ((*trace-context* ,ctx-var))
                    (handler-case
                        (progn
                          (setf ,vals (multiple-value-list (progn ,@body)))
                          (values-list ,vals))
                      (error (e)
                        (setf ,ok nil
                              ,err-message (condition-status-message e))
                        (error e))))
               (handler-case
                   (let* ((end-nano (current-unix-nano))
                          (,links-var ,links)
                          (cfg (client-config ,client-var))
                          (trace-id (getf ,ctx-var :trace-id))
                          (span-id (getf ,ctx-var :span-id))
                          (parent-span-id (getf ,ctx-var :parent-span-id))
                          (base-attrs (list (otel-string-attr "agento11y.sdk.name" +sdk-name+))))
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
                                  :name ,name-var
                                  :kind ,kind
                                  :start-time-unix-nano ,start-nano
                                  :end-time-unix-nano end-nano
                                  :attributes (coerce (nreverse base-attrs) 'vector)
                                  :status-code (if ,ok :unset 2)
                                  :status-message (or ,err-message "")
                                  :links ,links-var))
                     (bt2:with-lock-held ((client-lock ,client-var))
                       (bt2:condition-notify (client-wake-cv ,client-var))))
                 (error (e)
                   (handler-case
                       (agento11y-log (client-config ,client-var) :warn "span"
                                 (format nil "span recording failed: ~a" (princ-to-string e)))
                     (error () nil))))))))))
