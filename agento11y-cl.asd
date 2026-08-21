(defsystem :agento11y-cl
  :description "Common Lisp SDK for Grafana Agent Observability"
  :version "0.1.0"
  :author "Alexander Akhmetov"
  :license "Apache-2.0"
  :depends-on (:dexador :com.inuoe.jzon :bordeaux-threads :alexandria :cl-base64 :closer-mop :cl-ppcre :babel)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "util")
               (:file "conditions")
               (:file "json")
               (:file "types")
               (:file "config")
               (:file "redaction")
               (:file "env")
               (:file "auth")
               (:file "otel")
               ;; Before "queue": the GenAI-semconv export layer builds on the
               ;; OTLP attribute and span helpers in "otel", and the adapter
               ;; needs the capture vocabulary from "config". It loads ahead of
               ;; "recorder" so the recorder can call into it.
               (:file "otel-genai")
               (:file "otel-genai-hook")
               (:file "otel-export")
               (:file "queue")
               (:file "exporter")
               (:file "metrics")
               (:file "recorder")
               ;; Before "rating": SUBMIT-CONVERSATION-RATING derives its base
               ;; URL with %RESOLVE-API-BASE-URL, defined here.
               (:file "hooks")
               (:file "rating")
               (:file "client")
               (:file "eval")
               (:file "suite")
               (:file "trial")
               (:file "macros")
               ;; Before "experiment": TRIAL-RECORD-EVALUATION reads the
               ;; EVALUATION-RESULT accessors defined here.
               (:file "evaluators")
               (:file "experiment")
               (:file "conversations")
               (:file "normalize")))

(defsystem :agento11y-cl/t
  :description "Tests for agento11y-cl"
  :depends-on (:agento11y-cl)
  :serial t
  :pathname "t/"
  :components ((:file "package")
               (:file "suite")
               ;; Before "tests": RUN-TESTS calls the conformance suites.
               (:file "conformance")
               (:file "tests")))
