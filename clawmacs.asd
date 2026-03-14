(defsystem "clawmacs"
  :description "A Lisp-native Emacs-inspired LLM chat interface"
  :version "0.1.0"
  :depends-on ("croatoan" "alexandria")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "faces")
                             (:file "message")
                             (:file "buffer")
                             (:file "commands")
                             (:file "keymap")
                             (:file "render")
                             (:file "main")))))

(defsystem "clawmacs/tests"
  :description "Tests for clawmacs"
  :depends-on ("clawmacs" "fiveam")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "packages")
                             (:file "faces-test")
                             (:file "message-test")
                             (:file "buffer-test")
                             (:file "commands-test")
                             (:file "keymap-test")
                             (:file "render-test")))))
