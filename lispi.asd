(defsystem "lispi"
  :description "Lisp data mode tools for agentic Common Lisp environments"
  :version "0.1.0"
  :licence "AGPL-3.0-only"
  :depends-on ("flexi-streams")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "lispi-packages")
                             (:file "lispi")))))
