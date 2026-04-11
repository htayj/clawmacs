(require :asdf)

(load "scripts/build-cache.lisp" :verbose nil :print nil)

(let ((*standard-output* *error-output*)
      (*trace-output* *error-output*))
  (let ((setup (uiop:getenv "CLAWMACS_QUICKLISP_SETUP")))
    (unless (and setup (plusp (length setup)))
      (error "Missing CLAWMACS_QUICKLISP_SETUP"))
    (load setup))
  (clawmacs/build-cache:maybe-clean-build-cache
   :environment-variable "CLAWMACS_RUN_CLEAN_BUILD")
  (push (truename ".") asdf:*central-registry*)
  (funcall (symbol-function (find-symbol "QUICKLOAD" "QL")) :clawmacs))

(clawmacs:clawmacs-main)
