(in-package :rplaca/tests)

(in-suite templata-package-suite)

(defmacro with-templata-package-state ((channels-form) &body body)
  "Run BODY with isolated package and slash-command registries."
  `(let* ((root (temp-package-test-directory "templata-config"))
          (rplaca::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (rplaca::*package-configuration* nil)
          (rplaca::*package-channels* ,channels-form)
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil)
          (rplaca::*slash-command-table* (make-hash-table :test #'equal)))
     ,@body))

(test templata-parses-slash-command-line
  "Slash invocations split command name and shell-like quoted arguments."
  (multiple-value-bind (name args)
      (rplaca::parse-slash-command-line
       "/component Button \"click handler\" 'disabled support'")
    (is (string= "component" name))
    (is (equal '("Button" "click handler" "disabled support") args))))

(test templata-ignores-non-slash-input
  "Plain chat input does not parse as a slash invocation."
  (multiple-value-bind (name args)
      (rplaca::parse-slash-command-line "plain chat text")
    (declare (ignore args))
    (is-false name)))

(test templata-parses-markdown-frontmatter-and-description
  "Prompt templates read YAML-like description frontmatter."
  (let ((template
          (rplaca::parse-prompt-template-markdown
           "review"
           "---
description: Review staged changes
---
Review the staged diff for bugs.")))
    (is (string= "review" (rplaca::prompt-template-name template)))
    (is (string= "Review staged changes"
                 (rplaca::prompt-template-description template)))
    (is (string= "Review the staged diff for bugs."
                 (rplaca::prompt-template-body template)))))

(test templata-uses-first-non-empty-line-as-fallback-description
  "Templates without frontmatter fall back to the first non-empty line."
  (let ((template
          (rplaca::parse-prompt-template-markdown
           "review"
           "

Review the staged diff for bugs.

More detail follows.")))
    (is (string= "Review the staged diff for bugs."
                 (rplaca::prompt-template-description template)))))

(test templata-expands-positional-and-sliced-arguments
  "Template placeholders expand positional args, all args, and slices."
  (let ((expanded
          (rplaca::expand-prompt-template-body
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
    (let ((templates (rplaca::discover-prompt-templates-in-directory
                      prompts
                      :scope :project)))
      (is (= 1 (length templates)))
      (is (string= "review"
                   (rplaca::prompt-template-name (first templates))))
      (is (eq :project
              (rplaca::prompt-template-scope (first templates)))))))

(test templata-discovers-project-local-templates-for-buffer
  "Buffer-scoped discovery uses the working directory's .rplaca/prompts root."
  (let* ((root (temp-package-test-directory "templata-buffer"))
         (project-root (merge-pathnames "project/" root))
         (prompt-root (merge-pathnames ".rplaca/prompts/" project-root))
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
    (let ((templates (rplaca::discover-prompt-templates :buffer buffer)))
      (is (= 1 (length templates)))
      (is (string= "review"
                   (rplaca::prompt-template-name (first templates)))))))

(test templata-discovers-package-templates-for-enabled-packages
  "Discovery includes prompts/*.md from enabled package roots."
  (let* ((channel-root
           (make-package-channel-root
            :label "templata-channel"
            :package-name "prompt-package"
            :manifest "(:name \"prompt-package\"
                        :entrypoint \"entry.lisp\"
                        :prompt-template-directory \"templates/\")"
            :entrypoint-content "(in-package :rplaca)"))
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
      (let ((templates (rplaca::discover-prompt-templates :buffer buffer)))
        (is (= 1 (length templates)))
        (is (string= "prompt-package"
                     (rplaca::prompt-template-package (first templates))))
        (is (string= "review"
                     (rplaca::prompt-template-name (first templates))))))))

(test templata-prefers-project-over-package-over-global
  "Template lookup uses project, then package, then global precedence."
  (let* ((channel-root
           (make-package-channel-root
            :label "templata-precedence"
            :package-name "precedence-package"
            :manifest "(:name \"precedence-package\"
                        :entrypoint \"entry.lisp\"
                        :prompt-template-directory \"package-prompts/\")"
            :entrypoint-content "(in-package :rplaca)"))
         (package-root (merge-pathnames "precedence-package/" channel-root))
         (package-prompt-root (merge-pathnames "package-prompts/" package-root))
         (global-root (temp-package-test-directory "templata-global"))
         (project-root (merge-pathnames "project/" (temp-package-test-directory
                                                    "templata-project")))
         (project-prompt-root (merge-pathnames ".rplaca/prompts/" project-root))
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
      (let ((rplaca::*prompt-template-user-directory*
              (uiop:ensure-directory-pathname global-root)))
        (set-package-enablement-scope "precedence-package" :global)
        (let ((template (rplaca::find-prompt-template "review" :buffer buffer)))
          (is (not (null template)))
          (is (eq :project (rplaca::prompt-template-scope template)))
          (is (search "Project body"
                      (rplaca::prompt-template-body template))))))))

(test templata-slash-completion-items-dedupe-template-behind-command
  "Slash completion shows one entry when a command shadows a same-name template."
  (let* ((root (temp-package-test-directory "templata-complete"))
         (project-root (merge-pathnames "project/" root))
         (prompt-root (merge-pathnames ".rplaca/prompts/" project-root))
         (buffer (make-buffer "templata-complete"
                              :working-directory project-root)))
    (ensure-directories-exist (merge-pathnames ".keep" prompt-root))
    (write-test-file (merge-pathnames "review.md" prompt-root)
                     "---
description: Review prompt
---
Review body.")
    (let ((rplaca::*slash-command-table* (make-hash-table :test #'equal)))
      (rplaca:register-slash-command
       "review" (lambda (buffer args input-text)
                  (declare (ignore buffer args input-text))
                  :ok)
       :description "Review command."
       :argument-hint "<target>")
      (let ((items (rplaca::slash-command-selector-items :buffer buffer)))
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
               (mapcar #'rplaca::slash-command-name
                       (rplaca:list-slash-commands))))))

(test templata-package-manifest-declares-runtime-resources
  "The bundled templata package advertises slash commands in its manifest."
  (with-templata-package-state ((default-package-test-channels))
    (let ((definition (rplaca:find-installed-package "templata")))
      (is (not (null definition)))
      (is (null (rplaca::package-definition-prompt-template-directory
                 definition)))
      (is (= 6 (length (rplaca::package-definition-slash-commands
                        definition))))
      (is (equal '("export" "model" "new" "reload" "resume" "session")
                 (sort (copy-list
                        (mapcar #'rplaca::package-slash-command-spec-name
                                (rplaca::package-definition-slash-commands
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
            "(in-package :rplaca)

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
      (is (not (null (rplaca:find-slash-command "hello" :buffer buffer))))
      (is (null (rplaca:find-slash-command "bye" :buffer buffer)))
      (write-test-file
       (merge-pathnames "reload-package/manifest.lisp" channel-root)
       "(:name \"reload-package\"
         :entrypoint \"entry.lisp\"
         :slash-commands ((:name \"bye\"
                           :handler \"reload-package-bye\"
                           :description \"Reloaded command.\")))")
      (write-test-file
       (merge-pathnames "reload-package/entry.lisp" channel-root)
       "(in-package :rplaca)

(defun reload-package-bye (buffer args input-text)
  (declare (ignore buffer args input-text))
  :bye)")
      (rplaca::reload-package-channels)
      (rplaca::reload-active-packages :buffer buffer)
      (is (null (rplaca:find-slash-command "hello" :buffer buffer)))
      (is (not (null (rplaca:find-slash-command "bye" :buffer buffer)))))))

(test templata-slash-resume-without-args-uses-load-session-selector
  "Bare /resume defers to the existing interactive load-session flow."
  (let ((called nil)
        (buffer (make-buffer "templata-resume")))
    (let ((original (symbol-function 'rplaca:load-session-command)))
      (unwind-protect
           (progn
             (setf (symbol-function 'rplaca:load-session-command)
                   (lambda (target-buffer)
                     (setf called target-buffer)
                     :selector-opened))
             (is (eq :selector-opened
                     (rplaca::templata-slash-resume buffer nil "/resume")))
             (is (eq buffer called)))
        (setf (symbol-function 'rplaca:load-session-command)
              original)))))

(test templata-slash-export-writes-html-and-reports-the-path
  "The bundled /export command writes HTML instead of only saving JSON."
  (let* ((*sessions-dir* (temp-session-test-directory "templata-export"))
         (export-path (merge-pathnames "slash-export.html" *sessions-dir*))
         (buffer (make-buffer "templata-export" :agent-name "echo")))
    (rplaca::attach-buffer-session
     buffer
     (load-or-create-session "templata-export"))
    (set-message-text (buffer-input-message buffer) "Need export")
    (buffer-finalize-input buffer)
    (buffer-insert-agent-message
     buffer
     "Exported."
     :raw-content '(((:type . "text") (:text . "Exported."))
                    ((:type . "reasoning") (:text . "Shown reasoning.")))
     :metadata '((:provider . :openai-codex)
                 (:model . "gpt-5.4")
                 (:think-level . "high")))
    (let* ((result (rplaca::templata-slash-export
                    buffer
                    (list "--reasoning" "--metadata" (namestring export-path))
                    (format nil "/export --reasoning --metadata ~A"
                            (namestring export-path))))
           (html (uiop:read-file-string export-path))
           (notice (message-prev (buffer-input-message buffer))))
      (is (equal (namestring export-path) (namestring result)))
      (is (probe-file export-path))
      (is (search "Shown reasoning." html))
      (is (search "provider/model: openai-codex/gpt-5.4" html))
      (is (not (null notice)))
      (is (eq :system (message-sender notice)))
      (is (search "Session exported to" (message-text notice))))))
