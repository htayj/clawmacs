(in-package :clawmacs/tests)

(in-suite templata-package-suite)

(defmacro with-templata-package-state ((channels-form) &body body)
  "Run BODY with isolated package and slash-command registries."
  `(let* ((root (temp-package-test-directory "templata-config"))
          (clawmacs::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (clawmacs::*package-configuration* nil)
          (clawmacs::*package-channels* ,channels-form)
          (clawmacs::*available-packages* nil)
          (clawmacs::*package-registry-loaded-p* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (clawmacs::*package-prompt-sections* nil)
          (clawmacs::*slash-command-table* (make-hash-table :test #'equal)))
     ,@body))

(test templata-parses-slash-command-line
  "Slash invocations split command name and shell-like quoted arguments."
  (multiple-value-bind (name args)
      (clawmacs::parse-slash-command-line
       "/component Button \"click handler\" 'disabled support'")
    (is (string= "component" name))
    (is (equal '("Button" "click handler" "disabled support") args))))

(test templata-ignores-non-slash-input
  "Plain chat input does not parse as a slash invocation."
  (multiple-value-bind (name args)
      (clawmacs::parse-slash-command-line "plain chat text")
    (declare (ignore args))
    (is-false name)))

(test templata-parses-markdown-frontmatter-and-description
  "Prompt templates read YAML-like description frontmatter."
  (let ((template
          (clawmacs::parse-prompt-template-markdown
           "review"
           "---
description: Review staged changes
---
Review the staged diff for bugs.")))
    (is (string= "review" (clawmacs::prompt-template-name template)))
    (is (string= "Review staged changes"
                 (clawmacs::prompt-template-description template)))
    (is (string= "Review the staged diff for bugs."
                 (clawmacs::prompt-template-body template)))))

(test templata-uses-first-non-empty-line-as-fallback-description
  "Templates without frontmatter fall back to the first non-empty line."
  (let ((template
          (clawmacs::parse-prompt-template-markdown
           "review"
           "

Review the staged diff for bugs.

More detail follows.")))
    (is (string= "Review the staged diff for bugs."
                 (clawmacs::prompt-template-description template)))))

(test templata-expands-positional-and-sliced-arguments
  "Template placeholders expand positional args, all args, and slices."
  (let ((expanded
          (clawmacs::expand-prompt-template-body
           "Name: $1
Second: $2
All: $@
Also all: $ARGUMENTS
Tail: ${@:2}
Slice: ${@:2:2}"
           '("Button" "click handler" "disabled support" "aria labels"))))
    (is (search "Name: Button" expanded))
    (is (search "Second: click handler" expanded))
    (is (search "All: Button click handler disabled support aria labels"
                expanded))
    (is (search "Also all: Button click handler disabled support aria labels"
                expanded))
    (is (search "Tail: click handler disabled support aria labels"
                expanded))
    (is (search "Slice: click handler disabled support" expanded))))

(test templata-discovers-nonrecursive-markdown-templates
  "Discovery loads only top-level .md templates from a directory."
  (let* ((root (temp-package-test-directory "templata-discovery"))
         (prompts (merge-pathnames "prompts/" root))
         (nested (merge-pathnames "nested/" prompts)))
    (ensure-directories-exist (merge-pathnames ".keep" nested))
    (with-open-file (stream (merge-pathnames "review.md" prompts)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string "---
description: Review code
---
Review the code." stream))
    (with-open-file (stream (merge-pathnames "nested/ignored.md" prompts)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string "Ignored." stream))
    (let ((templates (clawmacs::discover-prompt-templates-in-directory
                      prompts
                      :scope :project)))
      (is (= 1 (length templates)))
      (is (string= "review"
                   (clawmacs::prompt-template-name (first templates))))
      (is (eq :project
              (clawmacs::prompt-template-scope (first templates)))))))

(test templata-discovers-project-local-templates-for-buffer
  "Buffer-scoped discovery uses the working directory's .clawmacs/prompts root."
  (let* ((root (temp-package-test-directory "templata-buffer"))
         (project-root (merge-pathnames "project/" root))
         (prompt-root (merge-pathnames ".clawmacs/prompts/" project-root))
         (buffer (make-buffer "templata-buffer"
                              :working-directory project-root)))
    (ensure-directories-exist (merge-pathnames ".keep" prompt-root))
    (with-open-file (stream (merge-pathnames "review.md" prompt-root)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string "---
description: Review code
---
Review the code." stream))
    (let ((templates (clawmacs::discover-prompt-templates :buffer buffer)))
      (is (= 1 (length templates)))
      (is (string= "review"
                   (clawmacs::prompt-template-name (first templates)))))))

(test templata-discovers-package-templates-for-enabled-packages
  "Discovery includes prompts/*.md from enabled package roots."
  (let* ((channel-root
           (make-package-channel-root
            :label "templata-channel"
            :package-name "prompt-package"
            :manifest "(:name \"prompt-package\"
                        :entrypoint \"entry.lisp\"
                        :prompt-template-directory \"templates/\")"
            :entrypoint-content "(in-package :clawmacs)"))
         (package-root (merge-pathnames "prompt-package/" channel-root))
         (prompt-root (merge-pathnames "templates/" package-root))
         (buffer (make-buffer "templata-package")))
    (write-test-file (merge-pathnames "review.md" prompt-root)
                     "---
description: Package review
---
Review from package.")
    (with-templata-package-state
        ((list (make-package-channel
                :name "templata-channel"
                :root channel-root
                :description "Templata test channel"
                :source :test)))
      (set-package-enablement-scope "prompt-package" :global)
      (let ((templates (clawmacs::discover-prompt-templates :buffer buffer)))
        (is (= 1 (length templates)))
        (is (string= "prompt-package"
                     (clawmacs::prompt-template-package (first templates))))
        (is (string= "review"
                     (clawmacs::prompt-template-name (first templates))))))))

(test templata-prefers-project-over-package-over-global
  "Template lookup uses project, then package, then global precedence."
  (let* ((channel-root
           (make-package-channel-root
            :label "templata-precedence"
            :package-name "precedence-package"
            :manifest "(:name \"precedence-package\"
                        :entrypoint \"entry.lisp\"
                        :prompt-template-directory \"package-prompts/\")"
            :entrypoint-content "(in-package :clawmacs)"))
         (package-root (merge-pathnames "precedence-package/" channel-root))
         (package-prompt-root (merge-pathnames "package-prompts/" package-root))
         (global-root (temp-package-test-directory "templata-global"))
         (project-root (merge-pathnames "project/" (temp-package-test-directory
                                                    "templata-project")))
         (project-prompt-root (merge-pathnames ".clawmacs/prompts/" project-root))
         (buffer (make-buffer "templata-precedence"
                              :working-directory project-root)))
    (write-test-file (merge-pathnames "review.md" package-prompt-root)
                     "---
description: Package review
---
Package body.")
    (write-test-file (merge-pathnames "review.md" global-root)
                     "---
description: Global review
---
Global body.")
    (write-test-file (merge-pathnames "review.md" project-prompt-root)
                     "---
description: Project review
---
Project body.")
    (with-templata-package-state
        ((list (make-package-channel
                :name "templata-precedence"
                :root channel-root
                :description "Templata precedence channel"
                :source :test)))
      (let ((clawmacs::*prompt-template-user-directory*
              (uiop:ensure-directory-pathname global-root)))
        (set-package-enablement-scope "precedence-package" :global)
        (let ((template (clawmacs::find-prompt-template "review" :buffer buffer)))
          (is (not (null template)))
          (is (eq :project (clawmacs::prompt-template-scope template)))
          (is (search "Project body"
                      (clawmacs::prompt-template-body template))))))))

(test templata-slash-completion-items-dedupe-template-behind-command
  "Slash completion shows one entry when a command shadows a same-name template."
  (let* ((root (temp-package-test-directory "templata-complete"))
         (project-root (merge-pathnames "project/" root))
         (prompt-root (merge-pathnames ".clawmacs/prompts/" project-root))
         (buffer (make-buffer "templata-complete"
                              :working-directory project-root)))
    (ensure-directories-exist (merge-pathnames ".keep" prompt-root))
    (write-test-file (merge-pathnames "review.md" prompt-root)
                     "---
description: Review prompt
---
Review body.")
    (let ((clawmacs::*slash-command-table* (make-hash-table :test #'equal)))
      (clawmacs:register-slash-command
       "review" (lambda (buffer args input-text)
                  (declare (ignore buffer args input-text))
                  :ok)
       :description "Review command."
       :argument-hint "<target>")
      (let ((items (clawmacs::slash-command-selector-items :buffer buffer)))
        (is (= 1 (length items)))
        (is (eq :command (getf (first items) :kind)))
        (is (search "/review" (getf (first items) :display)))
        (is (search "<target>" (getf (first items) :display)))))))

(test templata-package-registers-built-in-slash-commands
  "Enabling templata registers the built-in slash wrappers."
  (with-templata-package-state ((default-package-test-channels))
    (set-package-enablement-scope "templata" :global)
    (load-active-packages)
    (is (equal '("export" "model" "new" "reload" "resume" "session")
               (mapcar #'clawmacs::slash-command-name
                       (clawmacs:list-slash-commands))))))

(test templata-package-manifest-declares-runtime-resources
  "The bundled templata package advertises slash commands in its manifest."
  (with-templata-package-state ((default-package-test-channels))
    (let ((definition (clawmacs:find-installed-package "templata")))
      (is (not (null definition)))
      (is (null (clawmacs::package-definition-prompt-template-directory
                 definition)))
      (is (= 6 (length (clawmacs::package-definition-slash-commands
                        definition))))
      (is (equal '("export" "model" "new" "reload" "resume" "session")
                 (sort (copy-list
                        (mapcar #'clawmacs::package-slash-command-spec-name
                                (clawmacs::package-definition-slash-commands
                                 definition)))
                       #'string<))))))

(test templata-reload-refreshes-package-slash-commands
  "Reloading active packages drops stale slash commands and loads new ones."
  (let* ((channel-root
           (make-package-channel-root
            :label "templata-reload"
            :package-name "reload-package"
            :manifest "(:name \"reload-package\"
                        :entrypoint \"entry.lisp\"
                        :slash-commands ((:name \"hello\"
                                          :handler \"reload-package-hello\"
                                          :description \"Initial command.\")))"
            :entrypoint-content
            "(in-package :clawmacs)

(defun reload-package-hello (buffer args input-text)
  (declare (ignore buffer args input-text))
  :hello)"))
         (buffer (make-buffer "templata-reload")))
    (with-templata-package-state
        ((list (make-package-channel
                :name "templata-reload"
                :root channel-root
                :description "Templata reload test channel"
                :source :test)))
      (set-package-enablement-scope "reload-package" :global)
      (load-active-packages :buffer buffer)
      (is (not (null (clawmacs:find-slash-command "hello" :buffer buffer))))
      (is (null (clawmacs:find-slash-command "bye" :buffer buffer)))
      (write-test-file
       (merge-pathnames "reload-package/manifest.lisp" channel-root)
       "(:name \"reload-package\"
         :entrypoint \"entry.lisp\"
         :slash-commands ((:name \"bye\"
                           :handler \"reload-package-bye\"
                           :description \"Reloaded command.\")))")
      (write-test-file
       (merge-pathnames "reload-package/entry.lisp" channel-root)
       "(in-package :clawmacs)

(defun reload-package-bye (buffer args input-text)
  (declare (ignore buffer args input-text))
  :bye)")
      (clawmacs::reload-package-channels)
      (clawmacs::reload-active-packages :buffer buffer)
      (is (null (clawmacs:find-slash-command "hello" :buffer buffer)))
      (is (not (null (clawmacs:find-slash-command "bye" :buffer buffer)))))))

(test templata-slash-resume-without-args-uses-load-session-selector
  "Bare /resume defers to the existing interactive load-session flow."
  (let ((called nil)
        (buffer (make-buffer "templata-resume")))
    (let ((original (symbol-function 'clawmacs:load-session-command)))
      (unwind-protect
           (progn
             (setf (symbol-function 'clawmacs:load-session-command)
                   (lambda (target-buffer)
                     (setf called target-buffer)
                     :selector-opened))
             (is (eq :selector-opened
                     (clawmacs::templata-slash-resume buffer nil "/resume")))
             (is (eq buffer called)))
        (setf (symbol-function 'clawmacs:load-session-command)
              original)))))
