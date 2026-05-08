(require :asdf)

(let* ((backend-root (truename
                      (merge-pathnames "../"
                                       (make-pathname :directory (pathname-directory *load-pathname*)))))
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
             :test #'equal)))

(asdf:load-system :mcclim-charms/examples-tests)
(let ((result (fiveam:run 'mcclim-charms/tests::mcclim-charms-suite)))
  (fiveam:explain! result)
  (uiop:quit (if (fiveam:results-status result) 0 1)))
