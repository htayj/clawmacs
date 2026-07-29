(require :asdf)

(defpackage :rplaca/build-cache
  (:use :cl)
  (:export :clean-build-argument-p
           :maybe-clean-build-cache))

(in-package :rplaca/build-cache)

(defun truthy-env-value-p (value)
  "Return true when VALUE is an affirmative shell-style boolean."
  (and value
       (not (member (string-downcase value)
                    '("" "0" "false" "no" "nil" "off")
                    :test #'string=))))

(defun cache-directory (root relative)
  "Return RELATIVE cache directory under ROOT when ROOT is non-empty."
  (when (and root (plusp (length root)))
    (merge-pathnames relative (uiop:ensure-directory-pathname root))))

(defun clean-build-cache-directories ()
  "Return ASDF output-cache directories cleared for clean builds."
  (remove-duplicates
   (remove nil
           (list
            (cache-directory (uiop:getenv "XDG_CACHE_HOME")
                             #P"common-lisp/")
            (cache-directory (uiop:getenv "HOME")
                             #P".cache/common-lisp/")))
   :test #'equal))

(defun clean-build-argument-p
    (&key (args (uiop:command-line-arguments))
          (flag-options '("--show-tools" "--show-tool-calls"
                          "--show-reasoning" "--show-metadata"
                          "--json"
                          "--no-init" "--help" "-h"))
          (value-options '("--agent" "--provider" "--model" "--think"
                           "--reasoning-effort" "--prompt" "-p"
                           "--max-tool-iterations" "--debug-log")))
  "Return true when ARGS request a clean build before an argument separator."
  (let ((remaining (copy-list args)))
    (loop :while remaining
          :for arg := (pop remaining)
          :do (cond
                ((string= arg "--")
                 (return nil))
                ((or (string= arg "--clean-build")
                     (string= arg "--force-clean-build"))
                 (return t))
                ((member arg value-options :test #'string=)
                 (when remaining
                   (pop remaining)))
                ((member arg flag-options :test #'string=)
                 nil)
                (t
                 (return nil)))
          :finally (return nil))))

(defun maybe-clean-build-cache (&key environment-variable
                                     (arguments (uiop:command-line-arguments)))
  "Clear cached Common Lisp build artifacts when explicitly requested."
  (when (or (and environment-variable
                 (truthy-env-value-p (uiop:getenv environment-variable)))
            (clean-build-argument-p :args arguments))
    (format *error-output* ";; clean-build: clearing Common Lisp output cache~%")
    (dolist (directory (clean-build-cache-directories))
      (when (probe-file directory)
        (format *error-output* ";; clean-build: deleting ~A~%" directory)
        (uiop:delete-directory-tree directory :validate t)))))
