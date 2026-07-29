(in-package :rplaca/tests)

(in-suite prove-package-suite)

(defmacro with-prove-package-state (&body body)
  "Run BODY with isolated package, project, tool, and agent registries."
  `(let* ((root (temp-package-test-directory "prove-config"))
          (rplaca::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (rplaca::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (rplaca::*tool-table* (make-hash-table :test #'equal))
          (rplaca::*agent-definition-registry*
           (make-hash-table :test #'equal))
          (rplaca::*project-registry* (make-hash-table :test #'equal))
          (rplaca::*project-definitions-loaded-p* nil)
          (rplaca::*pipeline-test-profile-registry*
           (make-hash-table :test #'equal))
          (rplaca::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (rplaca::*package-configuration* nil)
          (rplaca::*package-channels* (default-package-test-channels))
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil)
          (rplaca::*enabled-builtin-packages* nil))
     ,@body))

(defun load-test-prove-package ()
  "Enable and load the bundled prove package."
  (set-package-enablement-scope "prove" :global)
  (load-active-packages))

(defun prove-package-tool-result (tool-name args)
  "Execute TOOL-NAME with ARGS and read its Lisp data result."
  (nth-value 0
    (rplaca::lisp-data-read
     (rplaca:execute-tool tool-name args))))

(test prove-package-registers-tools-prompt-and-tester-agent
  "Enabling prove exposes self-testing tools, prompt guidance, and a tester agent."
  (with-prove-package-state
    (load-test-prove-package)
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<))
           (prompt (render-package-prompt-sections))
           (tester (find-agent-definition "tester")))
      (is (equal '("prove_list_methods" "prove_run") tool-names))
      (is (search "Self-testing with prove" prompt))
      (is (search "prove_list_methods" prompt))
      (is (search "prove_run" prompt))
      (is (search "`tester` agent" prompt))
      (is (not (null tester)))
      (is (equal '("prove_list_methods" "prove_run")
                 (sort (copy-list (agent-definition-tool-names tester))
                       #'string<))))))

(test prove-package-tools-list-and-run-methods
  "prove tools list deterministic test methods and run selected methods."
  (with-prove-package-state
    (let ((project-root (temp-package-test-directory "prove-project")))
      (ensure-directories-exist (merge-pathnames #P".keep" project-root))
      (register-pipeline-test-profile
       "pass"
       :description "Passing probe"
       :command "printf 'ok\\n'")
      (register-pipeline-test-profile
       "fail"
       :description "Failing probe"
       :command "printf '0123456789\\n' >&2; exit 7")
      (define-project "prove-project" :root project-root)
      (load-test-prove-package)
      (let* ((listed (prove-package-tool-result
                      "prove_list_methods"
                      '(:project "prove-project")))
             (methods (coerce (getf listed :methods) 'list)))
        (is (getf listed :ok))
        (is (string= (namestring project-root)
                     (getf listed :directory)))
        (is (equal '("fail" "pass")
                   (mapcar (lambda (method) (getf method :name))
                           methods)))
        (is (search "printf" (getf (first methods) :command))))
      (let* ((report (prove-package-tool-result
                      "prove_run"
                      '(:project "prove-project"
                        :methods #("pass" "fail")
                        :max_chars 4)))
             (results (coerce (getf report :results) 'list))
             (pass-result (first results))
             (fail-result (second results)))
        (is (getf report :ok))
        (is-false (getf report :passed-p))
        (is (equalp #("pass" "fail") (getf report :methods)))
        (is (search "pass: PASS" (getf report :summary) :test #'char-equal))
        (is (search "fail: FAIL" (getf report :summary) :test #'char-equal))
        (is (string= "pass" (getf pass-result :name)))
        (is (string= (format nil "ok~%")
                     (getf pass-result :stdout)))
        (is-false (getf pass-result :stdout-truncated-p))
        (is (string= "fail" (getf fail-result :name)))
        (is (= 7 (getf fail-result :exit-code)))
        (is (search "[truncated to last 4 chars]" (getf fail-result :stderr)
                    :test #'char-equal))
        (is-true (getf fail-result :stderr-truncated-p))
        (is (= 11 (getf fail-result :stderr-length)))))))
