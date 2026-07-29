(require :asdf)

(load "scripts/build-cache.lisp" :verbose nil :print nil)

(let ((*standard-output* *error-output*)
      (*trace-output* *error-output*))
  (let ((setup (uiop:getenv "RPLACA_QUICKLISP_SETUP")))
    (unless (and setup (plusp (length setup)))
      (error "Missing RPLACA_QUICKLISP_SETUP"))
    (load setup))
  (rplaca/build-cache:maybe-clean-build-cache
   :environment-variable "RPLACA_PROMPT_CLEAN_BUILD")
  (push (truename ".") asdf:*central-registry*)
  (funcall (symbol-function (find-symbol "QUICKLOAD" "QL")) :rplaca))

(rplaca:rplaca-prompt-main)
