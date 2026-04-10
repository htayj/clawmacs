(defsystem "clawmacs"
  :description "A Lisp-native Emacs-inspired LLM chat interface"
  :version "0.1.0"
  :licence "AGPL-3.0-only"
  :depends-on ("croatoan" "alexandria" "drakma" "cl-json" "bordeaux-threads")
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
                             (:file "llm")
                             (:file "tools")
                             (:file "ui-protocol")
                             (:file "render-core")
                             (:file "croatoan-backend")
                             (:file "main")
                             (:file "reference")
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
                              (:file "llm-test")
                              (:file "keymap-test")
                              (:file "render-test")))))
