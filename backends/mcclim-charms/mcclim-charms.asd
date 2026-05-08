(in-package #:asdf-user)

(defsystem "mcclim-charms"
  :description "Experimental McCLIM backend backed by cl-charms/curses."
  :version "0.1.0"
  :licence "AGPL-3.0-only"
  :depends-on ("mcclim" "cl-charms" "alexandria")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "classes")
                             (:file "lifecycle")
                             (:file "graft")
                             (:file "frame-manager")
                             (:file "mirror")
                             (:file "events")
                             (:file "medium")
                             (:file "examples")
                             (:file "port")))))

(defsystem "mcclim-charms/tests"
  :description "Unit tests for the experimental McCLIM charms backend."
  :licence "AGPL-3.0-only"
  :depends-on ("mcclim-charms" "fiveam")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "unit-tests")))))

(defsystem "mcclim-charms/examples-tests"
  :description "McCLIM examples coverage manifest and PTY runner integration."
  :licence "AGPL-3.0-only"
  :depends-on ("mcclim-charms/tests")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "examples-tests")))))
