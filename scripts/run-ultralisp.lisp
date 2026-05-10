(require :asdf)

(load "scripts/build-cache.lisp" :verbose nil :print nil)

(defun clawmacs-ultralisp-setup-path ()
  (or (let ((setup (uiop:getenv "CLAWMACS_ULTRALISP_SETUP")))
        (when (and setup (plusp (length setup)))
          setup))
      (let ((setup (uiop:getenv "CLAWMACS_QUICKLISP_SETUP")))
        (when (and setup (plusp (length setup)))
          setup))
      (namestring (merge-pathnames #P"quicklisp/setup.lisp"
                                   (user-homedir-pathname)))))

(defun clawmacs-ensure-ultralisp-dist ()
  ;; Ultralisp is a Quicklisp-compatible dist.  The local setup file loads the
  ;; Quicklisp client; dependency resolution below comes from the Ultralisp dist.
  (let ((dist (find-symbol "FIND-DIST" "QL-DIST"))
        (install-dist (find-symbol "INSTALL-DIST" "QL-DIST")))
    (unless (and dist install-dist)
      (error "Quicklisp dist support is unavailable; cannot enable Ultralisp."))
    (unless (funcall (symbol-function dist) "ultralisp")
      (format *error-output* "Installing Ultralisp dist...~%")
      (funcall (symbol-function install-dist)
               "http://dist.ultralisp.org/"
               :prompt nil))))

(let ((*standard-output* *error-output*)
      (*trace-output* *error-output*))
  (let ((setup (clawmacs-ultralisp-setup-path)))
    (unless (probe-file setup)
      (error "Missing Quicklisp-compatible setup file for Ultralisp: ~A~%~A"
             setup
             "Set CLAWMACS_ULTRALISP_SETUP, CLAWMACS_QUICKLISP_SETUP, or install ~/quicklisp/setup.lisp."))
    (load setup))
  (clawmacs-ensure-ultralisp-dist)
  (clawmacs/build-cache:maybe-clean-build-cache
   :environment-variable "CLAWMACS_RUN_CLEAN_BUILD")
  (push (truename ".") asdf:*central-registry*)
  (funcall (symbol-function (find-symbol "QUICKLOAD" "QL")) :clawmacs))

(clawmacs:clawmacs-main
 :session-name (or (let ((name (uiop:getenv "CLAWMACS_SESSION_NAME")))
                     (when (and name (plusp (length name)))
                       name))
                   "clawmacs:native-session-01"))
