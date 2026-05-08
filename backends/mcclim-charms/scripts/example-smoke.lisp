(require :asdf)

(defun configure-registry ()
  (let* ((script (or *load-pathname* *compile-file-pathname*))
         (backend-root (truename
                        (merge-pathnames "../"
                                         (make-pathname :directory (pathname-directory script)))))
         (repo-root (truename (merge-pathnames "../../" backend-root)))
         (mcclim-root (or (uiop:getenv "MCCLIM_SOURCE_ROOT")
                          "/home/tay/reference/external_src/McCLIM/")))
    (pushnew backend-root asdf:*central-registry* :test #'equal)
    (pushnew repo-root asdf:*central-registry* :test #'equal)
    (when (probe-file mcclim-root)
      (asdf:initialize-source-registry
       `(:source-registry
         (:tree ,(namestring backend-root))
         (:tree ,(namestring (truename mcclim-root)))
         :inherit-configuration))
      (pushnew (truename mcclim-root) asdf:*central-registry* :test #'equal)
      (pushnew (truename (merge-pathnames "Examples/" mcclim-root))
               asdf:*central-registry*
               :test #'equal))))

(defun main ()
  (configure-registry)
  (asdf:load-system :mcclim-charms)
  (setf (symbol-value (find-symbol "*DEFAULT-SERVER-PATH*" "CLIM")) '(:charms))
  (asdf:load-system :clim-examples)
  (format t "loaded clim-examples on ~S~%"
          (symbol-value (find-symbol "*DEFAULT-SERVER-PATH*" "CLIM")))
  (force-output)
  (uiop:quit 0))

(handler-case
    (main)
  (error (condition)
    (format *error-output* "~&example smoke failed: ~A~%" condition)
    (uiop:quit 1)))
