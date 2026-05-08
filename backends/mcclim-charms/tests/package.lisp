(defpackage #:mcclim-charms/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:backend #:mcclim-charms)))

(in-package #:mcclim-charms/tests)

(def-suite mcclim-charms-suite
  :description "Unit tests for the McCLIM charms backend.")
