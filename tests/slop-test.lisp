(in-package :rplaca/tests)

(in-suite slop-suite)

(defun slop-test-directory ()
  "Return a fresh temporary directory for slop tests."
  (let ((dir (merge-pathnames
              (format nil "rplaca-slop-tests-~D-~D-~36R/"
                      (get-universal-time)
                      (get-internal-real-time)
                      (random (expt 36 8)))
              #P"/tmp/")))
    (ensure-directories-exist (merge-pathnames #P".keep" dir))
    (truename dir)))

(defun write-slop-test-file (path text)
  "Write TEXT to PATH for tests that intentionally bypass projects."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string text stream)))

(defmacro with-slop-env-var ((var value) &body body)
  "Temporarily set environment variable VAR to VALUE."
  (let ((gvar (gensym "VAR-"))
        (gval (gensym "VAL-"))
        (gold (gensym "OLD-")))
    `(let* ((,gvar ,var)
            (,gval ,value)
            (,gold (uiop:getenv ,gvar)))
       (unwind-protect
            (progn
              (if ,gval
                  (setf (uiop:getenv ,gvar) ,gval)
                  (setf (uiop:getenv ,gvar) ""))
              ,@body)
         (if ,gold
             (setf (uiop:getenv ,gvar) ,gold)
             (setf (uiop:getenv ,gvar) ""))))))

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
          (rplaca::*project-definitions-loaded-p* nil)
          (rplaca::*slop-project-index-cache*
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
              (rplaca::slop-tool-find-definitions
               '(:project "slop-demo"
                 :symbol "double"
                 :path "src/main.lisp"
                 :package ""
                 :namespace ""
                 :kind ""
                 :limit 10))))
        (is (= 1 (getf tool-definitions :count))))
      (let ((tool-callers
              (rplaca::slop-tool-find-callers
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

(test slop-batches-definition-lookups
  "Batch lookup resolves multiple symbols from one indexed project pass."
  (with-slop-project ("slop-batch" root source)
    (let* ((result (slop-find-definitions-batch
                    "slop-batch"
                    '("double" "caller")
                    :namespace "function"
                    :per-symbol-limit 1))
           (results (getf result :results)))
      (is (= 2 (getf result :count)))
      (is (= 2 (getf result :total-definitions)))
      (is (every (lambda (entry)
                   (= 1 (getf entry :count)))
                 results))
      (is (equal '("double" "caller")
                 (mapcar (lambda (entry)
                           (getf entry :symbol))
                         results))))
    (let ((tool-result
            (rplaca::slop-tool-find-definitions-batch
             '(:project "slop-batch"
               :symbols #("double" "triple")
               :namespace "function"
               :per-symbol-limit 1))))
      (is (= 2 (getf tool-result :count)))
      (is (= 2 (getf tool-result :total-definitions))))))

(test slop-resolves-default-and-path-projects
  "Slop accepts omitted projects and explicit source root paths."
  (with-slop-project ("rplaca" root source)
    (is (search "(defun double" source))
    (with-slop-env-var ("RPLACA_PROMPT_PROJECT_ROOT" nil)
      (let* ((default-result (slop-find-definitions nil
                                                    "double"
                                                    :namespace "function"))
             (definition (first (getf default-result :definitions))))
        (is (= 1 (getf default-result :count)))
        (is (string= "rplaca" (getf default-result :project)))
        (is (string= "rplaca" (getf definition :project))))
      (let ((tool-result
              (rplaca::slop-tool-find-definitions
               '(:symbol "triple"
                 :namespace "function"
                 :path "src/main.lisp"))))
        (is (= 1 (getf tool-result :count)))
        (is (string= "rplaca" (getf tool-result :project)))))
    (let ((by-path (slop-find-definitions (namestring root)
                                          "caller"
                                          :path "src/main.lisp"
                                          :namespace "function")))
      (is (= 1 (getf by-path :count)))
      (is (string= "rplaca" (getf by-path :project))))))

(test slop-registers-transient-workspace-for-omitted-project
  "Omitted project arguments auto-register the current prompt workspace."
  (let* ((base (slop-test-directory))
         (root (merge-pathnames #P"workspace/" base))
         (definitions (merge-pathnames #P"defs/" base))
         (*project-registry* (make-hash-table :test #'equal))
         (*project-definitions-directory* definitions)
         (rplaca::*project-definitions-loaded-p* nil)
         (rplaca::*slop-project-index-cache*
          (make-hash-table :test #'equal)))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (ensure-directories-exist (merge-pathnames #P".keep" definitions))
    (write-slop-test-file (merge-pathnames #P"src/main.lisp" root)
                          (slop-demo-source))
    (with-slop-env-var ("RPLACA_PROMPT_PROJECT_ROOT" (namestring root))
      (let ((current (slop-current-project)))
        (is (string= "workspace" (getf current :project)))
        (is (string= (namestring (truename root))
                     (getf current :root))))
      (let ((definitions-result
              (rplaca::slop-tool-find-definitions
               '(:symbol "double"
                 :path "src/main.lisp"
                 :namespace "function"))))
        (is (= 1 (getf definitions-result :count)))
        (is (string= "workspace" (getf definitions-result :project))))
      (let ((by-dot (slop-find-definitions "."
                                           "triple"
                                           :namespace "function"))
            (by-path (slop-find-definitions (namestring root)
                                            "caller"
                                            :namespace "function")))
        (is (= 1 (getf by-dot :count)))
        (is (= 1 (getf by-path :count)))
        (is (string= "workspace" (getf by-dot :project)))
        (is (string= "workspace" (getf by-path :project))))
      (let* ((projects (rplaca::slop-tool-list-projects '()))
             (names (mapcar (lambda (project)
                              (getf project :name))
                            (getf projects :projects))))
        (is (string= "workspace" (getf projects :current-project)))
        (is (member "workspace" names :test #'string=))))))

(test slop-traces-call-flow-from-entrypoint
  "Trace call flow follows callees and returns resolved graph edges."
  (with-slop-project ("slop-trace" root source)
    (let* ((trace (slop-trace-calls "slop-trace"
                                    :symbol "caller"
                                    :direction "callees"
                                    :max-depth 2))
           (edges (getf trace :edges))
           (callee-names (mapcar (lambda (edge)
                                   (getf edge :callee))
                                 edges)))
      (is (string= "caller" (getf (getf trace :root) :name)))
      (is (member "slop-demo:double" callee-names :test #'string=))
      (is (member "slop-demo:triple" callee-names :test #'string=))
      (is (find-if (lambda (edge)
                     (and (string= "slop-demo:triple"
                                   (or (getf edge :caller) ""))
                          (string= "slop-demo:double"
                                   (or (getf edge :callee) ""))))
                   edges))
      (is (>= (getf trace :node-count) 3)))))

(test slop-stale-definition-ids-return-hints-or-fallback
  "Reference lookup handles stale ids without forcing a failed tool call."
  (with-slop-project ("slop-stale" root source)
    (let ((fallback (slop-find-references
                     "slop-stale"
                     :definition-id "src/main.lisp#def-999999"
                     :symbol "double"
                     :namespace "function"
                     :role "call")))
      (is (eq :definition-id-not-found (getf fallback :status)))
      (is (eq :symbol (getf fallback :fallback)))
      (is (= 3 (getf fallback :count)))
      (is (= 1 (length (getf fallback :matching-definitions)))))
    (let ((hint (slop-find-references
                 "slop-stale"
                 :definition-id "src/main.lisp#def-999999")))
      (is (eq :definition-id-not-found (getf hint :status)))
      (is (= 0 (getf hint :count)))
      (is (null (getf hint :references)))
      (is (plusp (length (getf hint :matching-definitions)))))))

(test slop-finds-text-mentions-outside-source-references
  "Mention search includes docs/tests/config style text and quoted data."
  (with-slop-project ("slop-mentions" root source)
    (project-create-file "slop-mentions"
                         "docs/commands.md"
                         :content "The caller command is documented here. callers-extra is separate.")
    (let* ((mentions (slop-find-mentions "slop-mentions"
                                         "caller"
                                         :path "docs"))
           (items (getf mentions :mentions)))
      (is (= 1 (getf mentions :count)))
      (is (string= "docs/commands.md" (getf (first items) :path)))
      (is (search "caller command" (getf (first items) :preview))))
    (let ((quoted (slop-find-mentions "slop-mentions"
                                      "double"
                                      :path "src/main.lisp")))
      (is (plusp (getf quoted :count))))))

(test slop-reads-definition-with-local-context
  "Definition context returns body, nearby top-level forms, and package forms."
  (with-slop-project ("slop-context" root source)
    (let* ((context (slop-definition-context "slop-context"
                                             :symbol "caller"
                                             :before-forms 1
                                             :after-forms 1))
           (definition (getf context :definition))
           (forms (getf context :nearby-forms))
           (roles (mapcar (lambda (form)
                            (getf form :role))
                          forms)))
      (is (string= "caller" (getf definition :name)))
      (is (search "(defun caller" (getf definition :body)))
      (is (equal '(:before :definition :after) roles))
      (is (search "(defun triple" (getf (first forms) :text)))
      (is (search "(defun quoted" (getf (third forms) :text)))
      (is (find :in-package
                (getf context :package-forms)
                :key (lambda (form)
                       (getf form :role))))
      (is (plusp (getf (getf context :line-range) :start-line))))))

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
          (rplaca::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (rplaca::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (rplaca::*tool-table* (make-hash-table :test #'equal))
          (rplaca::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (rplaca::*package-configuration* nil)
          (rplaca::*package-channels*
           (list (rplaca:make-package-channel
                  :name "default"
                  :root rplaca:*default-package-channel-directory*
                  :description "Bundled RPLACA packages"
                  :source :builtin)))
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil))
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
      (is (member "slop_current_project" tool-names :test #'string=))
      (is (member "slop_list_projects" tool-names :test #'string=))
      (is (member "slop_project_symbols" tool-names :test #'string=))
      (is (member "slop_find_definitions_batch" tool-names :test #'string=))
      (is (member "slop_find_references" tool-names :test #'string=))
      (is (member "slop_trace_calls" tool-names :test #'string=))
      (is (member "slop_find_mentions" tool-names :test #'string=))
      (is (member "slop_definition_context" tool-names :test #'string=))
      (is (member "slop_rename_variable" tool-names :test #'string=))
      (is (search "Symbol lookup with slop" prompt))
      (is (search "slop_find_definitions" prompt))
      (is (search "slop_trace_calls" prompt))
      (is (search "slop_find_mentions" prompt))
      (is (search "Omit it for the current workspace" prompt))
      (is (search "Avoid `lisp_eval`" prompt))
      (is (search "command-to-helper" prompt)))))
