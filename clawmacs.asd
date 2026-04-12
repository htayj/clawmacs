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

(defsystem "clawmacs"
  :description "A Lisp-native Emacs-inspired LLM chat interface"
  :version "0.1.0"
  :licence "AGPL-3.0-only"
  :depends-on ("lispi" "croatoan" "alexandria" "drakma" "cl-json"
               "bordeaux-threads" "coalton" "named-readtables")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "package-manager")
                             (:file "faces")
                             (:file "message")
                             (:file "buffer")
                             (:file "commands")
                             (:file "keymap")
                             (:file "projects")
                             (:file "skills")
                             (:file "matching-core")
                             (:file "matching")
                             (:file "llm")
                             (:file "tools")
                             (:file "compaction")
                             (:file "ui-protocol")
                             (:file "render-core")
                             (:file "croatoan-backend")
                             (:file "main")
                             (:file "reference")
                             (:file "sexed")
                             (:file "docs")))))

(defsystem "clawmacs/mcclim"
  :description "McCLIM graphical backend for clawmacs"
  :licence "AGPL-3.0-only"
  :depends-on ("clawmacs" "mcclim")
  :components ((:module "src"
                :components ((:file "mcclim-backend")))))

(defsystem "clawmacs/tests"
  :description "Tests for clawmacs"
  :licence "AGPL-3.0-only"
  :depends-on ("clawmacs" "fiveam")
  :serial t
  :components ((:module "tests"
                :serial t
                 :components ((:file "packages")
                              (:file "package-manager-test")
                              (:file "reference-test")
                              (:file "faces-test")
                              (:file "message-test")
                              (:file "buffer-test")
                              (:file "commands-test")
                              (:file "matching-test")
                              (:file "projects-test")
                              (:file "skills-test")
                              (:file "sexed-test")
                              (:file "llm-test")
                              (:file "keymap-test")
                              (:file "render-test")))))
