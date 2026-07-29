(in-package :rplaca)

(defparameter *prove-tool-default-max-chars* 4000
  "Default maximum stdout/stderr characters returned per test method.")

(defun prove-blank-string-p (value)
  "Return true when VALUE is NIL or only ASCII whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))))

(defun prove-string (value field-name &key allow-nil)
  "Normalize VALUE as a string argument named FIELD-NAME."
  (cond
    ((null value)
     (if allow-nil
         nil
         (error "~A is required." field-name)))
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                 value)))
       (cond
         ((and (not allow-nil) (prove-blank-string-p trimmed))
          (error "~A must be a non-empty string." field-name))
         ((prove-blank-string-p trimmed) nil)
         (t trimmed))))
    ((symbolp value)
     (prove-string (string-downcase (symbol-name value))
                   field-name
                   :allow-nil allow-nil))
    (t
     (error "~A must be a string, got ~S." field-name value))))

(defun prove-positive-integer (value field-name default)
  "Return VALUE as a positive integer, or DEFAULT when VALUE is NIL."
  (cond
    ((null value) default)
    ((and (integerp value) (plusp value)) value)
    (t
     (error "~A must be a positive integer, got ~S." field-name value))))

(defun prove-existing-directory (path context)
  "Return PATH as an existing directory pathname, or signal using CONTEXT."
  (let ((directory (uiop:ensure-directory-pathname path)))
    (or (uiop:directory-exists-p directory)
        (error "~A does not name an existing directory: ~A"
               context
               directory))))

(defun prove-directory-from-project (project-designator)
  "Return the root directory for PROJECT-DESIGNATOR."
  (project-root (ensure-project project-designator)))

(defun prove-directory-from-path (directory)
  "Resolve DIRECTORY and return it as an existing directory."
  (prove-existing-directory
   (lispi:resolve-tool-path directory)
   "Test directory"))

(defun prove-buffer-working-directory ()
  "Return the current tool buffer working directory, when it exists."
  (when (and (boundp '*current-tool-buffer*)
             *current-tool-buffer*
             (buffer-working-directory *current-tool-buffer*))
    (uiop:directory-exists-p
     (uiop:ensure-directory-pathname
      (buffer-working-directory *current-tool-buffer*)))))

(defun prove-tool-working-directory ()
  "Return the effective tool working directory."
  (lispi:tool-working-directory-pathname))

(defun prove-resolve-directory (args)
  "Resolve ARGS to the directory where test methods should run."
  (let ((project (tool-arg args :project "project"))
        (directory (tool-arg args :directory "directory")))
    (cond
      ((not (prove-blank-string-p project))
       (prove-directory-from-project project))
      ((not (prove-blank-string-p directory))
       (prove-directory-from-path directory))
      ((prove-buffer-working-directory))
      (t
       (prove-existing-directory (prove-tool-working-directory)
                                 "Test directory")))))

(defun prove-method-names (args)
  "Return a normalized non-empty list of selected test method names."
  (let ((names (normalize-pipeline-test-profile-list
                (tool-arg args
                          :methods "methods"
                          :tests "tests"
                          :profiles "profiles"
                          :method "method"
                          :test "test"))))
    (unless names
      (error "methods is required and must contain at least one test method name."))
    names))

(defun prove-truncate-string (text max-chars)
  "Return TEXT truncated to MAX-CHARS, preserving the trailing portion."
  (let ((value (or text "")))
    (if (<= (length value) max-chars)
        (values value nil (length value))
        (values (format nil "[truncated to last ~D chars]~%~A"
                        max-chars
                        (subseq value (- (length value) max-chars)))
                t
                (length value)))))

(defun prove-profile-method-data (profile directory)
  "Return PROFILE as agent-readable method metadata for DIRECTORY."
  (let ((command (pipeline-test-profile-command-value profile directory)))
    (list :name (pipeline-test-profile-name profile)
          :description (or (pipeline-test-profile-description profile) "")
          :command (pipeline-command-display-string command))))

(defun prove-list-methods-tool (args)
  "Return available deterministic test methods for ARGS."
  (let* ((directory (prove-resolve-directory args))
         (methods
           (mapcar (lambda (profile)
                     (prove-profile-method-data profile directory))
                   (list-pipeline-test-profiles))))
    (lisp-data-string
     (list :ok t
           :directory (namestring directory)
           :methods (coerce methods 'vector)))))

(defun prove-report-result-data (result max-chars)
  "Return RESULT with agent-facing truncated stdout and stderr."
  (multiple-value-bind (stdout stdout-truncated-p stdout-length)
      (prove-truncate-string (getf result :stdout) max-chars)
    (multiple-value-bind (stderr stderr-truncated-p stderr-length)
        (prove-truncate-string (getf result :stderr) max-chars)
      (list :name (getf result :name)
            :command (getf result :command)
            :directory (getf result :directory)
            :exit-code (getf result :exit-code)
            :passed-p (getf result :passed-p)
            :stdout stdout
            :stderr stderr
            :stdout-length stdout-length
            :stderr-length stderr-length
            :stdout-truncated-p stdout-truncated-p
            :stderr-truncated-p stderr-truncated-p))))

(defun prove-report-data (report max-chars)
  "Return REPORT normalized for agent-facing tool output."
  (list :ok t
        :passed-p (getf report :passed-p)
        :directory (getf report :directory)
        :methods (coerce (copy-list (getf report :profiles)) 'vector)
        :summary (getf report :summary)
        :results (coerce (mapcar (lambda (result)
                                   (prove-report-result-data result max-chars))
                                 (getf report :results))
                         'vector)))

(defun prove-run-tool (args)
  "Run selected deterministic test methods and return a structured report."
  (let* ((directory (prove-resolve-directory args))
         (max-chars (prove-positive-integer
                     (tool-arg args :max-chars "max_chars" "max-chars")
                     "max_chars"
                     *prove-tool-default-max-chars*))
         (report (run-pipeline-test-profiles
                  (prove-method-names args)
                  :directory directory)))
    (lisp-data-string (prove-report-data report max-chars))))

(register-package-prompt-section
 "prove"
 "## Self-testing with prove

- Use `prove_list_methods` to inspect the deterministic test methods available
  in the current project, a named project, or an explicit directory path.
- Use `prove_run` to run one or more named test methods and get a structured
  report with truncated stdout and stderr suitable for agent use.
- The bundled `tester` agent is configured to use only these tools for
  verification work.
- Prefer these tools over raw shell workarounds or `lisp_eval` when the
  available test methods cover the verification task."
 :title "Self-testing with prove"
 :package "prove")

(deftool prove-list-methods-tool
  :name "prove_list_methods"
  :description "List deterministic self-test methods for the current project, a named project, or an explicit directory path."
  :call-style :raw-args
  :args ((project :type "string" :required nil
                  :description "Optional RPLACA project name. Takes precedence over directory.")
         (directory :type "string" :required nil
                    :description "Optional working directory path.")))

(deftool prove-run-tool
  :name "prove_run"
  :description "Run one or more deterministic self-test methods and return a structured, agent-readable report."
  :call-style :raw-args
  :args ((methods :type "array"
                  :items ((:type . "string"))
                  :description "Non-empty array of test method names to run. Also accepted as tests or profiles.")
         (project :type "string" :required nil
                  :description "Optional RPLACA project name. Takes precedence over directory.")
         (directory :type "string" :required nil
                    :description "Optional working directory path.")
         (max-chars :type "integer" :required nil
                    :description "Maximum stdout/stderr characters returned per method. Defaults to 4000.")))

(register-agent-definition
 "tester"
 :core-prompt
 "You are RPLACA's tester. Use prove_list_methods and prove_run to verify code changes with the available deterministic test methods. Do not claim a test passed unless prove_run reports :passed-p true. Summaries should stay concrete and grounded in the reported failures."
 :tool-names '("prove_list_methods" "prove_run"))
