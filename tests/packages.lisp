(defpackage :clawmacs/tests
  (:use :cl :fiveam :clawmacs))

(in-package :clawmacs/tests)

(def-suite clawmacs-suite
  :description "All clawmacs tests")

(def-suite faces-suite
  :description "Face system tests"
  :in clawmacs-suite)

(def-suite message-suite
  :description "Message and line tests"
  :in clawmacs-suite)

(def-suite buffer-suite
  :description "Buffer tests"
  :in clawmacs-suite)

(def-suite commands-suite
  :description "Command system tests"
  :in clawmacs-suite)
