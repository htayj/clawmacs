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
  :depends-on ("lispi" "mcclim" "esa-mcclim" "drei-mcclim"
               "alexandria" "drakma" "cl-json" "bordeaux-threads")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "faces")
                             (:file "message")
                             (:file "session")
                             (:file "buffer")
                             (:file "windows")
                             (:file "debug")
                             (:file "package-manager")
                             (:file "modelaria-core")
                             (:file "artifactum-core")
                             (:file "hooks")
                             (:file "commands")
                             (:file "keymap")
                             (:file "projects")
                             (:file "skills")
                             (:file "templata")
                             (:file "matching-core")
                             (:file "matching")
                             (:file "llm")
                             (:file "tools")
                             (:file "prompt-core")
                             (:file "subagents")
                             (:file "compaction")
                             (:file "prompt-runner")
                             (:file "pipelines")
                             (:file "render-core")
                             (:file "minibuffer")
                             (:file "prefix")
                             (:file "listener")
                             (:file "main")
                             (:file "mcclim-app")
                             (:file "speculum")
                             (:file "reference")
                             (:file "sexed")
                             (:file "slop")
                             (:file "docs")))))

(defsystem "clawmacs/tests"
  :description "Tests for clawmacs"
  :licence "AGPL-3.0-only"
  :depends-on ("clawmacs" "fiveam")
  :serial t
  :components ((:module "tests"
                :serial t
                 :components ((:file "packages")
                              (:file "package-manager-test")
                              (:file "git-package-test")
                              (:file "organa-package-test")
                              (:file "netcons-package-test")
                              (:file "prove-package-test")
                              (:file "speculum-package-test")
                              (:file "subagent-package-test")
                              (:file "templata-package-test")
                              (:file "quaestor-package-test")
                              (:file "modelaria-package-test")
                              (:file "artifactum-package-test")
                              (:file "reference-test")
                              (:file "faces-test")
                              (:file "message-test")
                              (:file "buffer-test")
                              (:file "windows-test")
                              (:file "commands-test")
                              (:file "matching-test")
                              (:file "projects-test")
                              (:file "skills-test")
                              (:file "sexed-test")
                              (:file "slop-test")
                              (:file "llm-test")
                              (:file "guard-test")
                              (:file "keymap-test")
                              (:file "render-test")))))
