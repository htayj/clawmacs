(in-package :clawmacs)

(register-package-prompt-section
 "pipelines"
 "## Deterministic pipelines

- Clawmacs pipelines are user-defined, deterministic routing graphs that run
  one or more agent stages for a user request.
- A pipeline stage can use the original user input, the previous stage output,
  or a named prior stage output. Clawmacs handles that routing outside the
  model; do not simulate the routing yourself unless the user explicitly asks.
- When a buffer has an active pipeline, normal message sending runs the
  configured pipeline instead of a single agent response.
- Pipeline stages are configured in Lisp with `define-pipeline` or
  `defpipeline`. Stages support static `:next` targets and function-valued
  `:next` routers for deterministic retry or repair loops.
- Stages may also compute `:packages`, `:skills`, and `:tools` from earlier
  stage output, and may use a deterministic `:runner` for non-LLM work such as
  test execution.
- The bundled `self-modify` pipeline plans a code change, injects needed
  packages and skills for implementation, uses the `prove` self-testing
  package plus the `tester` agent to enforce selected tests, loops back
  through planning on test failure, then updates docs and `init.lisp` when the
  plan says that is needed."
 :title "Deterministic pipelines"
 :package "pipelines")

(defun self-modify-blank-string-p (value)
  "Return true when VALUE is NIL or only ASCII whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))))

(defun self-modify-json-key-name (value)
  "Normalize VALUE into the underscore-preserving JSON lookup key shape."
  (string-downcase
   (json-name-to-lisp
    (etypecase value
      (string value)
      (symbol (symbol-name value))))))

(defun self-modify-json-value (alist key)
  "Look up KEY in decoded JSON ALIST using string-insensitive matching."
  (let ((target (self-modify-json-key-name key)))
    (cdr (find target alist
               :key (lambda (entry)
                      (self-modify-json-key-name (car entry)))
               :test #'string=))))

(defun self-modify-json-list (value)
  "Return VALUE coerced to a simple list."
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t (list value))))

(defun self-modify-extract-json-string (text)
  "Return the most likely JSON object string embedded in TEXT."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (or text "")))
         (fenced-p (and (<= 3 (length trimmed))
                        (string= "```" trimmed :end2 3))))
    (labels ((object-subseq (source)
               (let ((start (position #\{ source))
                     (end (position #\} source :from-end t)))
                 (when (and start end (< start end))
                   (subseq source start (1+ end))))))
      (cond
        ((zerop (length trimmed))
         "{}")
        (fenced-p
         (let* ((after-first-line
                  (or (position #\Newline trimmed)
                      (1- (length trimmed))))
                (closing (search "```" trimmed :start2 (1+ after-first-line)
                                 :from-end t))
                (inner (if (and closing (> closing after-first-line))
                           (subseq trimmed (1+ after-first-line) closing)
                           trimmed)))
           (or (object-subseq inner)
               inner)))
        (t
         (or (object-subseq trimmed)
             trimmed))))))

(defun self-modify-parse-plan-output (text context stage stage-result)
  "Parse TEXT from the planning stage into a normalized property list."
  (declare (ignore context stage stage-result))
  (let* ((json-text (self-modify-extract-json-string text))
         (data (api-json-decode json-text))
         (raw-tests (self-modify-json-list (self-modify-json-value data "tests")))
         (tests
           (or (remove-if-not #'find-pipeline-test-profile
                              (normalize-pipeline-test-profile-list raw-tests))
               '("unit")))
         (packages
           (remove-if-not #'find-installed-package
                          (normalize-package-name-list
                           (self-modify-json-list
                            (self-modify-json-value data "packages")))))
         (skills
           (remove-if-not #'find-skill
                          (normalize-pipeline-skill-name-list
                           (self-modify-json-list
                            (self-modify-json-value data "skills")))))
         (plan (or (self-modify-json-value data "plan")
                   (self-modify-json-value data "summary")
                   ""))
         (implementation
           (or (self-modify-json-value data "implementation")
               (self-modify-json-value data "implementation_instructions")
               plan))
         (docs
           (or (self-modify-json-value data "docs")
               (self-modify-json-value data "documentation")
               ""))
         (update-init
           (not (null (self-modify-json-value data "update_init"))))
         (init
           (or (self-modify-json-value data "init")
               (self-modify-json-value data "init_instructions")
               "")))
    (list :plan (princ-to-string plan)
          :implementation (princ-to-string implementation)
          :packages packages
          :skills skills
          :tests tests
          :docs (princ-to-string docs)
          :update-init update-init
          :init (princ-to-string init))))

(defun self-modify-plan-data (context)
  "Return the parsed plan output for CONTEXT, or a clear error."
  (or (pipeline-stage-parsed-output context "plan")
      (error "self-modify requires parsed output from the plan stage.")))

(defun self-modify-test-data (context)
  "Return the parsed test-stage output for CONTEXT, or NIL."
  (pipeline-stage-parsed-output context "test"))

(defun self-modify-test-feedback (context)
  "Return the best available human-readable test feedback for CONTEXT."
  (let ((test-data (self-modify-test-data context)))
    (or (and test-data
             (not (self-modify-blank-string-p (getf test-data :feedback)))
             (getf test-data :feedback))
        (and test-data
             (not (self-modify-blank-string-p (getf test-data :summary)))
             (getf test-data :summary))
        (pipeline-stage-output context "test"))))

(defun self-modify-format-package-options ()
  "Return installed package choices for the self-modify planner."
  (with-output-to-string (out)
    (dolist (definition (sort (copy-list (list-installed-packages))
                              #'string<
                              :key #'package-definition-name))
      (format out "- ~A: ~A~%"
              (package-definition-name definition)
              (or (package-definition-description definition)
                  "")))))

(defun self-modify-format-skill-options ()
  "Return enabled skill choices for the self-modify planner."
  (with-output-to-string (out)
    (dolist (skill (list-skills))
      (format out "- ~A: ~A~%"
              (skill-name skill)
              (or (skill-short-description skill)
                  (skill-description skill)
                  "")))))

(defun self-modify-format-test-profile-options ()
  "Return deterministic test profile choices for the self-modify planner."
  (with-output-to-string (out)
    (dolist (profile (list-pipeline-test-profiles))
      (format out "- ~A: ~A~%"
              (pipeline-test-profile-name profile)
              (or (pipeline-test-profile-description profile)
                  "")))))

(defun self-modify-plan-stage-prompt (context)
  "Return the planning-stage prompt for the bundled self-modify pipeline."
  (let ((previous-plan (pipeline-stage-parsed-output context "plan"))
        (previous-test (self-modify-test-feedback context)))
    (format nil
            "You are planning a Clawmacs self-modification workflow. Do not implement yet.~%~%
Return JSON only with these keys:~%
- \"plan\": short high-signal plan summary~%
- \"implementation\": implementation-stage instructions~%
- \"packages\": array of installed Clawmacs package names to enable~%
- \"skills\": array of enabled skill names to inject~%
- \"tests\": array of deterministic self-test method names to run after implementation~%
- \"docs\": documentation update instructions to apply only after tests pass~%
- \"update_init\": boolean~%
- \"init\": init.lisp update instructions used only when update_init is true~%~%
Choose only from these installed packages:~%~A~%
Choose only from these enabled skills:~%~A~%
Choose only from these deterministic self-test methods:~%~A~%
User request:~%~A~%~%
~@[Previous plan:~%~A~%~%~]~@[Latest test feedback:~%~A~%~%~]JSON only."
            (self-modify-format-package-options)
            (self-modify-format-skill-options)
            (self-modify-format-test-profile-options)
            (pipeline-context-original-prompt context)
            (and previous-plan (getf previous-plan :plan))
            (and (not (self-modify-blank-string-p previous-test))
                 previous-test))))

(defun self-modify-implement-stage-prompt (context)
  "Return the implementation-stage prompt for the bundled self-modify pipeline."
  (let ((plan (self-modify-plan-data context)))
    (format nil
            "Implement the user's request in the current project.~%~%
User request:~%~A~%~%
Plan summary:~%~A~%~%
Implementation instructions:~%~A~%~%
Selected test methods that will run after this stage: ~{~A~^, ~}.~%~%
Do the code changes now. Do not update docs or init.lisp yet unless doing so is
strictly necessary to make the code build or test."
            (pipeline-context-original-prompt context)
            (getf plan :plan)
            (getf plan :implementation)
            (getf plan :tests))))

(defun self-modify-test-stage-prompt (context)
  "Return the self-testing stage prompt for the bundled self-modify pipeline."
  (let ((plan (self-modify-plan-data context)))
    (format nil
            "You are verifying a completed Clawmacs code change. Do not modify code in this stage.~%~%
User request:~%~A~%~%
Plan summary:~%~A~%~%
Selected deterministic test methods: ~{~A~^, ~}.~%~%
Use `prove_run` to run those exact test methods in the current project. Use
`prove_list_methods` first only if you need to confirm the available method
names. After running the tests, return JSON only with these keys:~%
- \"passed\": boolean~%
- \"summary\": short summary sentence~%
- \"feedback\": concise high-signal feedback for a repair planner; include the
  failing method names and the most relevant stdout/stderr excerpts when tests
  fail~%
- \"tests\": array of the method names you actually ran~%~%
Do not claim success unless `prove_run` reports :passed-p true. JSON only."
            (pipeline-context-original-prompt context)
            (getf plan :plan)
            (getf plan :tests))))

(defun self-modify-parse-test-output (text context stage stage-result)
  "Parse TEXT from the self-testing stage into a normalized property list."
  (declare (ignore context stage stage-result))
  (handler-case
      (let* ((json-text (self-modify-extract-json-string text))
             (data (api-json-decode json-text))
             (passed (not (null (or (self-modify-json-value data "passed")
                                    (self-modify-json-value data "ok")))))
             (summary (or (self-modify-json-value data "summary")
                          (self-modify-json-value data "feedback")
                          ""))
             (feedback (or (self-modify-json-value data "feedback")
                           summary))
             (tests (ignore-errors
                      (normalize-pipeline-test-profile-list
                       (self-modify-json-list
                        (or (self-modify-json-value data "tests")
                            (self-modify-json-value data "methods")))))))
        (list :passed passed
              :summary (princ-to-string summary)
              :feedback (princ-to-string feedback)
              :tests tests))
    (error ()
      (let* ((body (princ-to-string (or text "")))
             (passed (and (search "pass" body :test #'char-equal)
                          (not (search "fail" body :test #'char-equal)))))
        (list :passed (not (null passed))
              :summary body
              :feedback body
              :tests nil)))))

(defun self-modify-docs-stage-prompt (context)
  "Return the documentation-stage prompt for the bundled self-modify pipeline."
  (let ((plan (self-modify-plan-data context))
        (test-report (self-modify-test-feedback context)))
    (format nil
            "Update the documentation affected by the completed change.~%~%
User request:~%~A~%~%
Plan summary:~%~A~%~%
Documentation instructions:~%~A~%~%
Tests passed with this report:~%~A~%~%
Make the necessary documentation changes now. If no documentation needs to
change, state that clearly and make no unrelated edits."
            (pipeline-context-original-prompt context)
            (getf plan :plan)
            (or (getf plan :docs) "Update the relevant docs for the change.")
            (or test-report ""))))

(defun self-modify-init-stage-prompt (context)
  "Return the init update stage prompt for the bundled self-modify pipeline."
  (let ((plan (self-modify-plan-data context)))
    (format nil
            "Update the user's init file only if the implemented feature requires it.~%~%
User request:~%~A~%~%
Plan summary:~%~A~%~%
init.lisp instructions:~%~A~%~%
The init file path is ~A. Workspace file tools may not reach it; use lisp_eval
if you must modify that file from the running process."
            (pipeline-context-original-prompt context)
            (getf plan :plan)
            (or (getf plan :init) "No init change should be made unless required.")
            (namestring *user-init-file*))))

(defun self-modify-docs-next-stage (_context _stage-result)
  "Route the docs stage to init only when the plan requires it."
  (declare (ignore _stage-result))
  (let ((plan (self-modify-plan-data _context)))
    (and (getf plan :update-init) "init")))

(defun self-modify-selected-packages (context _stage)
  "Return package enablement selected by the planning stage."
  (declare (ignore _stage))
  (copy-list (getf (self-modify-plan-data context) :packages)))

(defun self-modify-selected-skills (context _stage)
  "Return skill injection selected by the planning stage."
  (declare (ignore _stage))
  (copy-list (getf (self-modify-plan-data context) :skills)))

(defun register-bundled-pipeline-test-profiles ()
  "Register the deterministic test profiles shipped with the pipelines package."
  (register-pipeline-test-profile
   "unit"
   :description "Run the full FiveAM unit suite in the Guix runtime container."
   :command
   '("./scripts/guix-container.sh" "--mode" "run" "--" "sh" "-lc"
     "sbcl --noinform --load \"$CLAWMACS_QUICKLISP_SETUP\" --eval \"(push (truename \\\".\\\") asdf:*central-registry*)\" --eval \"(ql:quickload :clawmacs/tests)\" --eval \"(fiveam:run! (quote clawmacs/tests::clawmacs-suite))\" --eval \"(quit)\""))
  (dolist (group '(("mcclim-offline" "offline" "Run the offline McCLIM e2e suite.")
                   ("mcclim-smoke" "smoke" "Run the smoke McCLIM e2e suite.")
                   ("mcclim-packages" "packages" "Run the package-focused McCLIM e2e suite.")
                   ("mcclim-windows" "windows" "Run the logical-window McCLIM e2e suite.")
                   ("mcclim-readline" "readline" "Run the readline-oriented McCLIM e2e suite.")
                   ("mcclim-online" "online" "Run the live-provider McCLIM e2e suite.")
                   ("mcclim-online-zai" "online-zai" "Run the live ZAI McCLIM e2e suite.")
                   ("mcclim-online-openai-codex" "online-openai-codex"
                    "Run the live OpenAI Codex McCLIM e2e suite.")))
    (destructuring-bind (name only description) group
      (register-pipeline-test-profile
       name
       :description description
       :command (list "./scripts/mcclim-e2e.sh" "--only" only))))
  (register-pipeline-test-profile
   "prompt-probes"
   :description "Run the full live prompt.sh probe harness."
   :command '("./scripts/prompt-probes.sh"))
  (dolist (probe '(("prompt-probes-docs" "docs" "Run the docs prompt probe.")
                   ("prompt-probes-skills" "skills" "Run the skills prompt probe.")
                   ("prompt-probes-transaction" "transaction" "Run the transaction prompt probe.")
                   ("prompt-probes-scratch" "scratch" "Run the scratch prompt probe.")))
    (destructuring-bind (name only description) probe
      (register-pipeline-test-profile
       name
       :description description
       :command (list "./scripts/prompt-probes.sh" "--only" only)))))

(defun register-bundled-self-modify-pipeline ()
  "Register the bundled self-modify deterministic pipeline."
  (define-pipeline
   "self-modify"
   :description
   "Plan a code change, inject needed packages and skills, implement, verify with the prove self-testing package until tests pass, then update docs and init when required."
   :max-steps 12
   :stages
   `((:name "plan"
      :prompt ,#'self-modify-plan-stage-prompt
      :output-parser ,#'self-modify-parse-plan-output
      :next "implement")
     (:name "implement"
      :prompt ,#'self-modify-implement-stage-prompt
      :package-names ,#'self-modify-selected-packages
      :skill-names ,#'self-modify-selected-skills
      :next "test")
     (:name "test"
      :agent "tester"
      :prompt ,#'self-modify-test-stage-prompt
      :package-names ("prove")
      :tool-names ("prove_list_methods" "prove_run")
      :output-parser ,#'self-modify-parse-test-output
      :next ,(lambda (_context stage-result)
               (declare (ignore _context))
               (if (getf (pipeline-stage-result-parsed-output stage-result)
                         :passed)
                   "docs"
                   "plan")))
     (:name "docs"
      :prompt ,#'self-modify-docs-stage-prompt
      :package-names ,#'self-modify-selected-packages
      :skill-names ,#'self-modify-selected-skills
      :next ,#'self-modify-docs-next-stage)
     (:name "init"
      :prompt ,#'self-modify-init-stage-prompt
      :package-names ,#'self-modify-selected-packages
      :skill-names ,#'self-modify-selected-skills))))

(register-bundled-pipeline-test-profiles)
(register-bundled-self-modify-pipeline)

(defcommand set-buffer-pipeline
  :prompts ((pipeline-name :prompt "Pipeline name"))
  :docstring "Set the current buffer's deterministic pipeline by name.")

(defcommand clear-buffer-pipeline
  :docstring "Clear the current buffer's deterministic pipeline.")

(defdoc define-pipeline
  :category "pipelines"
  :usage "(define-pipeline NAME :stages '((:name \"plan\" :agent \"planner\" :prompt \"Plan {{input}}\" :next \"implement\") ...))"
  :returns "pipeline-definition - The registered deterministic pipeline."
  :side-effects "Updates the process-local pipeline registry. Stages may use computed packages/skills/tools, parse structured stage output, and run deterministic non-LLM runners. Buffers run a pipeline when BUFFER-PIPELINE-NAME names one, and prompt.sh can run one with --pipeline NAME."
  :see-also (defpipeline register-pipeline-definition set-buffer-pipeline run-pipeline-prompt))

(defdoc defpipeline
  :category "pipelines"
  :usage "(defpipeline plan-implement-test :stages '((:name \"plan\" ...)))"
  :returns "pipeline-definition - The registered deterministic pipeline."
  :see-also (define-pipeline register-pipeline-definition))

(defdoc set-buffer-pipeline
  :category "pipelines"
  :usage "(set-buffer-pipeline BUFFER PIPELINE-NAME) - (clear-buffer-pipeline BUFFER)"
  :returns "buffer - The mutated buffer."
  :side-effects "Causes SEND-MESSAGE to run the named deterministic pipeline instead of a single streaming agent response."
  :see-also (define-pipeline run-pipeline-for-buffer buffer-pipeline-name))

(defdoc run-pipeline-prompt
  :category "pipelines"
  :usage "(run-pipeline-prompt PROMPT PIPELINE-NAME &key :session-name :agent-name :provider :model :think-level :max-tool-iterations :auto-approve-tools-p :package-names)"
  :returns "prompt-run-result - The final pipeline stage summarized as prompt-mode output."
  :side-effects "Loads or creates a prompt buffer, records each pipeline stage as context, runs provider requests, executes allowed tools, and persists session snapshots when SESSION-NAME is supplied."
  :see-also (define-pipeline run-pipeline-on-buffer pipeline-stage-result-final-text))

(defdoc pipeline-stage-parsed-output
  :category "pipelines"
  :usage "(pipeline-stage-parsed-output CONTEXT \"plan\")"
  :returns "t or nil — The most recent parsed output stored for the named stage."
  :see-also (pipeline-stage-output define-pipeline))

(defdoc register-pipeline-test-profile
  :category "pipelines"
  :usage "(register-pipeline-test-profile \"unit\" :description \"Run unit tests\" :command '(\"./scripts/mcclim-e2e.sh\" \"--only\" \"smoke\"))"
  :returns "pipeline-test-profile - The registered deterministic test profile."
  :side-effects "Updates the process-local test profile registry used by run-pipeline-test-profiles and shipped pipelines such as self-modify."
  :see-also (define-pipeline-test-profile list-pipeline-test-profiles run-pipeline-test-profiles))

(defdoc define-pipeline-test-profile
  :category "pipelines"
  :usage "(define-pipeline-test-profile \"prompt-probes\" :description \"Run prompt harness\" :command '(\"./scripts/prompt-probes.sh\"))"
  :returns "pipeline-test-profile - The registered deterministic test profile."
  :see-also (register-pipeline-test-profile list-pipeline-test-profiles))

(defdoc list-pipeline-test-profiles
  :category "pipelines"
  :usage "(list-pipeline-test-profiles)"
  :returns "list - Registered deterministic test profiles."
  :see-also (register-pipeline-test-profile run-pipeline-test-profiles))

(defdoc run-pipeline-test-profiles
  :category "pipelines"
  :usage "(run-pipeline-test-profiles '(\"unit\" \"mcclim-offline\") :directory #P\"/path/to/repo/\")"
  :returns "plist - Structured test report with :passed-p, :summary, and per-profile command results."
  :side-effects "Runs registered shell command profiles sequentially in the given directory."
  :see-also (register-pipeline-test-profile list-pipeline-test-profiles define-pipeline))
