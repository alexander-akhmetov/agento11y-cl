(defsystem :sigil-cl
  :description "Common Lisp SDK for Grafana Sigil AI observability"
  :version "0.1.0"
  :author "Alexander Akhmetov"
  :license "Apache-2.0"
  :depends-on (:dexador :com.inuoe.jzon :bordeaux-threads :alexandria :cl-base64 :closer-mop)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "util")
               (:file "conditions")
               (:file "json")
               (:file "types")
               (:file "config")
               (:file "env")
               (:file "auth")
               (:file "otel")
               (:file "queue")
               (:file "exporter")
               (:file "recorder")
               (:file "rating")
               (:file "client")
               (:file "macros")
               (:file "normalize")))

(defsystem :sigil-cl/t
  :description "Tests for sigil-cl"
  :depends-on (:sigil-cl)
  :serial t
  :pathname "t/"
  :components ((:file "package")
               (:file "suite")
               (:file "tests")))
