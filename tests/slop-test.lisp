(in-package :clawmacs/tests)

(in-suite slop-suite)

(defun slop-test-directory ()
  "Return a fresh temporary directory for slop tests."
  (let ((dir (merge-pathnames
              (format nil "clawmacs-slop-tests-~D-~D-~36R/"
                      (get-universal-time)
                      (get-internal-real-time)
                      (random (expt 36 8)))
              #P"/tmp/")))
    (ensure-directories-exist (merge-pathnames #P".keep" dir))
    (truename dir)))

(defun slop-demo-source ()
  "Return a small package-qualified Lisp fixture for slop tests."
  "(defpackage :slop-demo
  (:use :cl)
  (:export #:double #:triple #:caller))

(in-package :slop-demo)

(defun double (x)
  (+ x x))

(defun triple (value)
  (+ value (double value)))

(defun caller (n)
  (let ((total (double n))
        (shadow 0))
    (incf total)
    (setf shadow (triple total))
    (+ total shadow)))

(defun quoted ()
  '(double \"double\")
  #'double
  (double 4))
")

(defmacro with-slop-project ((project-name root-var source-var) &body body)
  "Create a temporary project containing SOURCE-VAR in src/main.lisp."
  `(let* ((base (slop-test-directory))
          (,root-var (merge-pathnames #P"root/" base))
          (definitions (merge-pathnames #P"defs/" base))
          (,source-var (slop-demo-source))
          (*project-registry* (make-hash-table :test #'equal))
          (*project-definitions-directory* definitions)
          (clawmacs::*project-definitions-loaded-p* nil)
          (clawmacs::*slop-project-index-cache*
           (make-hash-table :test #'equal)))
     (ensure-directories-exist (merge-pathnames #P".keep" ,root-var))
     (ensure-directories-exist (merge-pathnames #P".keep" definitions))
     (define-project ,project-name :root ,root-var)
     (project-create-file ,project-name "src/main.lisp" :content ,source-var)
     ,@body))

(test slop-finds-definitions-references-callers-and-callees
  "Slop indexes definitions, calls, caller groups, and callees in Lisp source."
  (with-slop-project ("slop-demo" root source)
    (let* ((definition-result
             (slop-find-definitions "slop-demo"
                                    "double"
                                    :namespace "function"))
           (definitions (getf definition-result :definitions))
           (definition (first definitions))
           (definition-id (getf definition :id)))
      (is (= 1 (getf definition-result :count)))
      (is (string= "double" (getf definition :name)))
      (is (string= "slop-demo" (getf definition :package)))
      (is (eq :function (getf definition :kind)))
      (let ((references (slop-find-references "slop-demo"
                                              :definition-id definition-id
                                              :role "call")))
        (is (= 3 (getf references :count)))
        (is (every (lambda (reference)
                     (eq :call (getf reference :role)))
                   (getf references :references))))
      (let ((tool-definitions
              (clawmacs::slop-tool-find-definitions
               '(:project "slop-demo"
                 :symbol "double"
                 :path "src/main.lisp"
                 :package ""
                 :namespace ""
                 :kind ""
                 :limit 10))))
        (is (= 1 (getf tool-definitions :count))))
      (let ((tool-callers
              (clawmacs::slop-tool-find-callers
               '(:project "slop-demo"
                 :path "src/main.lisp"
                 :symbol "double"
                 :definition-id ""
                 :limit 10))))
        (is (<= 1 (getf tool-callers :count))))
      (let* ((callers (slop-find-callers "slop-demo"
                                         :definition-id definition-id))
             (caller-names
               (mapcar (lambda (group)
                         (getf (getf group :caller) :name))
                       (getf callers :callers))))
        (is (member "triple" caller-names :test #'string=))
        (is (member "caller" caller-names :test #'string=))
        (is (member "quoted" caller-names :test #'string=)))
      (let* ((callees (slop-find-callees "slop-demo"
                                         :symbol "caller"))
             (callee-names
               (mapcar (lambda (callee)
                         (getf callee :name))
                       (getf callees :callees))))
        (is (member "double" callee-names :test #'string=))
        (is (member "triple" callee-names :test #'string=))))))

(test slop-skips-quoted-data-strings-and-finds-function-references
  "Quoted data and strings are not counted as calls; #' references are."
  (with-slop-project ("slop-quoted" root source)
    (let ((calls (slop-find-references "slop-quoted"
                                       :symbol "double"
                                       :namespace "function"
                                       :role "call"))
          (references (slop-find-references "slop-quoted"
                                            :symbol "double"
                                            :namespace "function"
                                            :role "reference")))
      (is (= 3 (getf calls :count)))
      (is (= 1 (getf references :count)))
      (is (search "#'double"
                  (getf (first (getf references :references)) :preview))))))

(test slop-indexes-explicit-paths-under-ignored-directories
  "A direct path is indexable even when project scans would skip its directory."
  (with-slop-project ("slop-ignored" root source)
    (is (probe-file root))
    (project-create-file "slop-ignored" ".cache/hidden.lisp" :content source)
    (is-false (member ".cache/hidden.lisp"
                      (project-list-files "slop-ignored" :limit nil)
                      :test #'string=))
    (let ((definitions (slop-find-definitions
                        "slop-ignored"
                        "double"
                        :path ".cache/hidden.lisp")))
      (is (= 1 (getf definitions :count)))
      (is (string= ".cache/hidden.lisp"
                   (getf (first (getf definitions :definitions)) :path))))))

(test slop-finds-variable-uses-and-renames-lexical-variable
  "Lexical variable use lookup distinguishes a binding from shadowing names."
  (with-slop-project ("slop-vars" root source)
    (let* ((binding-offset (search "total (double n)" source))
           (uses (slop-find-variable-uses "slop-vars"
                                          :path "src/main.lisp"
                                          :offset binding-offset))
           (binding (getf uses :binding))
           (binding-id (getf binding :id))
           (roles (mapcar (lambda (use)
                            (getf use :role))
                          (getf uses :uses))))
      (is (string= "total" (getf binding :name)))
      (is (member :binding roles))
      (is (member :set roles))
      (is (member :access roles))
      (signals error
        (slop-rename-variable "slop-vars"
                              :binding-id binding-id
                              :new-name "shadow"))
      (let ((summary (slop-rename-variable "slop-vars"
                                           :binding-id binding-id
                                           :new-name "sum")))
        (is (eq :ok (getf summary :status)))
        (is (string= "sum" (getf summary :new-name))))
      (let ((text (project-read-file "slop-vars" "src/main.lisp")))
        (is (search "(let ((sum (double n))" text))
        (is (search "(incf sum)" text))
        (is (search "(setf shadow (triple sum))" text))
        (is (search "(+ sum shadow)" text))
        (is-false (search "(let ((total (double n))" text))))))

(defmacro with-slop-package-state (&body body)
  "Run BODY with isolated package and tool registries."
  `(let* ((root (slop-test-directory))
          (clawmacs::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (clawmacs::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (clawmacs::*tool-table* (make-hash-table :test #'equal))
          (clawmacs::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (clawmacs::*package-configuration* nil)
          (clawmacs::*package-channels*
           (list (clawmacs:make-package-channel
                  :name "default"
                  :root clawmacs:*default-package-channel-directory*
                  :description "Bundled Clawmacs packages"
                  :source :builtin)))
          (clawmacs::*available-packages* nil)
          (clawmacs::*package-registry-loaded-p* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (clawmacs::*package-prompt-sections* nil))
     ,@body))

(test slop-package-registers-agent-tools-and-prompt
  "Enabled slop packages expose provider tools and package instructions."
  (with-slop-package-state
    (set-package-enablement-scope "slop" :global)
    (load-active-packages)
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<))
           (prompt (render-package-prompt-sections)))
      (is (member "slop_project_symbols" tool-names :test #'string=))
      (is (member "slop_find_references" tool-names :test #'string=))
      (is (member "slop_rename_variable" tool-names :test #'string=))
      (is (search "Symbol lookup with slop" prompt))
      (is (search "slop_find_definitions" prompt))
      (is-false (search "lisp_eval" prompt :test #'char=)))))
