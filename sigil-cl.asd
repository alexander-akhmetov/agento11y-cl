(defsystem :sigil-cl
  :description "Common Lisp SDK for Grafana Sigil AI observability"
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
               (:file "queue")
               (:file "exporter")
               (:file "metrics")
               (:file "recorder")
               (:file "rating")
               (:file "hooks")
               (:file "client")
               (:file "eval")
               (:file "suite")
               (:file "trial")
               (:file "macros")
               (:file "experiment")
               (:file "conversations")
               (:file "normalize")))

(defsystem :sigil-cl/t
  :description "Tests for sigil-cl"
  :depends-on (:sigil-cl)
  :serial t
  :pathname "t/"
  :components ((:file "package")
               (:file "suite")
               (:file "tests")))
