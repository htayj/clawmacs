(in-package :rplaca/tests)

(in-suite reference-suite)

(defmacro with-common-lisp-spec-cache-reset (&body body)
  `(let ((rplaca::*common-lisp-spec-index-cache* nil)
         (rplaca::*common-lisp-spec-index-cache-path* nil))
     ,@body))

(defmacro with-temp-directory ((var prefix) &body body)
  `(let ((,var (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "~A-~36R/" ,prefix (random (expt 36 6)))
                 (uiop:temporary-directory)))))
     (ensure-directories-exist (merge-pathnames #P".keep" ,var))
     (unwind-protect
          (progn ,@body)
       (when (probe-file ,var)
         (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore)))))

(defun write-text-file (pathname content)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string content stream))
  pathname)

(defmacro with-common-lisp-spec-fixture ((root-var index-var) &body body)
  `(with-temp-directory (,root-var "rplaca-spec-fixture")
     (let* ((pages-dir (merge-pathnames #P"pages/" ,root-var))
            (,index-var (merge-pathnames #P"searchable_terms.json" ,root-var)))
       (ensure-directories-exist (merge-pathnames #P".keep" pages-dir))
       (write-text-file
        (merge-pathnames #P"format.html" pages-dir)
        "<html><head><title>format</title></head><body><div class=\"section topmost\"><h2>format</h2><p>Writes formatted output.</p><pre>(format destination control-string &rest args)</pre></div></body></html>")
       (write-text-file
        (merge-pathnames #P"handler_002dcase.html" pages-dir)
        "<html><head><title>handler-case</title></head><body><div class=\"section topmost\"><h2>handler-case</h2><p>Establishes condition handlers.</p></div></body></html>")
       (write-text-file
        ,index-var
        (format nil "{~%  ~S: ~S,~%  ~S: ~S~%}~%"
                "format" "format.html"
                "handler-case" "handler_002dcase.html"))
       (with-common-lisp-spec-cache-reset
         (let ((rplaca::*common-lisp-spec-root* ,root-var)
               (rplaca::*common-lisp-spec-index-path* ,index-var))
           ,@body)))))

(test common-lisp-spec-available-from-bundled-assets
  "The vendored search index and pages archive are present."
  (is (rplaca::common-lisp-spec-available-p)))

(test vendored-common-lisp-spec-smoke-test
  "The vendored CL Community Spec resolves stable standard symbols end-to-end."
  (with-common-lisp-spec-cache-reset
    (let ((format-entry (rplaca::find-common-lisp-spec-entry 'format))
          (handler-description (rplaca::describe-common-lisp-symbol-to-string 'handler-case
                                                                                :max-chars 4000)))
      (is (string= "format" (getf format-entry :term)))
      (is (probe-file (getf format-entry :path)))
      (is (search "format" (getf format-entry :page) :test #'char-equal))
      (is (search "Reference: CL Community Spec" handler-description))
      (is (search "handler-case" handler-description :test #'char-equal)))))

(test find-common-lisp-spec-entry-uses-index
  "Exact CL Community Spec lookup resolves to a page path and URL."
  (with-common-lisp-spec-fixture (root index)
    (declare (ignore root index))
    (let ((entry (rplaca::find-common-lisp-spec-entry 'format)))
      (is (string= "format" (getf entry :term)))
      (is (string= "format.html" (getf entry :page)))
      (is (search "/pages/format.html" (namestring (getf entry :path))))
      (is (search "https://cl-community-spec.github.io/pages/format.html"
                  (getf entry :url))))))

(test describe-common-lisp-symbol-to-string-uses-fixture-page
  "Spec descriptions render plain text from the vendored page content."
  (with-common-lisp-spec-fixture (root index)
    (declare (ignore root index))
    (let ((description (rplaca::describe-common-lisp-symbol-to-string 'format
                                                                        :max-chars 5000)))
      (is (search "Reference: CL Community Spec" description))
      (is (search "format.html" description))
      (is (search "Writes formatted output." description))
      (is (search "(format destination control-string &rest args)" description)))))

(test search-common-lisp-spec-to-string-ranks-fixture-hits
  "Spec search returns matching terms from the JSON index."
  (with-common-lisp-spec-fixture (root index)
    (declare (ignore root index))
    (let ((search-output (rplaca::search-common-lisp-spec-to-string "handler")))
      (is (search "handler-case" search-output))
      (is (search "handler_002dcase.html" search-output)))))

(test list-project-systems-reads-direct-dependencies
  "list-project-systems returns rplaca and the systems imported in rplaca.asd."
  (let ((systems (rplaca::list-project-systems)))
    (is (member "rplaca" systems :test #'string=))
    (is (member "alexandria" systems :test #'string=))
    (is (member "mcclim" systems :test #'string=))
    (is (member "drakma" systems :test #'string=))))

(test describe-system-to-string-summarizes-rplaca
  "describe-system-to-string reports metadata and package names for rplaca."
  (let ((description (rplaca::describe-system-to-string "rplaca")))
    (is (search "rplaca" description))
    (is (search "A Lisp-native Emacs-inspired LLM chat interface" description))
    (is (search "mcclim" description))
    (is (search "rplaca, rplaca/matching-core" description))))

(test search-system-docs-finds-rplaca-source
  "search-system-docs finds local matches in the rplaca source tree."
  (let ((search-output (rplaca::search-system-docs "rplaca" "lisp_eval" :limit 5)))
    (is (search "lisp_eval" search-output))
    (is (or (search "src/llm.lisp" search-output)
            (search "src/docs.lisp" search-output)
            (search "README.org" search-output)))))

(test describe-library-symbol-to-string-summarizes-rplaca-symbol
  "describe-library-symbol-to-string combines runtime docs with local source hits."
  (let ((description (rplaca::describe-library-symbol-to-string
                      "rplaca"
                      'describe-function-to-string
                      "rplaca")))
    (is (search "RPLACA::DESCRIBE-FUNCTION-TO-STRING" description))
    (is (search "Kinds: Function" description))
    (is (search "describe-function-to-string" description))))
