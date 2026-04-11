(in-package :clawmacs/tests)

(in-suite skills-suite)

(defun temp-skills-test-directory (&optional (name "skills"))
  (let* ((leaf (format nil "clawmacs-~A-test-~D-~D-~A"
                       name
                       (get-universal-time)
                       (get-internal-real-time)
                       (gensym)))
         (base (make-pathname :directory (list :absolute "tmp" leaf))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    base))

(defun write-skill-test-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream))
  path)

(defmacro with-isolated-skills ((root) &body body)
  `(let* ((,root (temp-skills-test-directory))
          (clawmacs::*skill-user-directory*
            (merge-pathnames #P"user-skills/" ,root))
          (clawmacs::*skill-agents-directory*
            (merge-pathnames #P"agents-skills/" ,root))
          (clawmacs::*skill-system-directory*
            (merge-pathnames #P"system-skills/" ,root))
          (clawmacs::*skill-configuration-path*
            (merge-pathnames #P"skills.json" ,root))
          (clawmacs::*skill-roots* nil)
          (clawmacs::*programmatic-skills* nil)
          (clawmacs::*skill-registry* nil)
          (clawmacs::*skill-disabled-paths* nil)
          (clawmacs::*minibuffer-active* nil)
          (clawmacs::*minibuffer-items* nil)
          (clawmacs::*minibuffer-filtered-items* nil)
          (clawmacs::*minibuffer-selected-index* 0)
          (clawmacs::*minibuffer-callback* nil))
     (ensure-directories-exist
      (merge-pathnames #P".keep" clawmacs::*skill-user-directory*))
     (ensure-directories-exist
      (merge-pathnames #P".keep" clawmacs::*skill-agents-directory*))
     (ensure-directories-exist
      (merge-pathnames #P".keep" clawmacs::*skill-system-directory*))
     ,@body))

(defun write-demo-skill (root &key (name "demo") (description "Demo skill"))
  (let* ((dir (merge-pathnames (make-pathname :directory `(:relative ,name))
                               root))
         (skill-path (merge-pathnames #P"SKILL.md" dir))
         (metadata-path (merge-pathnames #P"agents/openai.yaml" dir))
         (reference-path (merge-pathnames #P"references/guide.md" dir)))
    (write-skill-test-file
     skill-path
     (format nil "---~%name: ~A~%description: \"~A\"~%metadata:~%  short-description: \"Short ~A\"~%---~%Use this skill. The magic word is needle.~%"
             name description name))
    (write-skill-test-file
     metadata-path
     "interface:
  display_name: Demo Display
  short_description: Demo interface description
  default_prompt: Use demo by default
policy:
  allow_implicit_invocation: false
")
    (write-skill-test-file reference-path "A referenced guide with needle inside.")
    skill-path))

(test skill-root-discovery-loads-frontmatter-metadata-and-resources
  "Skills load from SKILL.md roots and expose local resource helpers."
  (with-isolated-skills (root)
    (write-demo-skill root)
    (register-skill-root root)
    (let ((skill (find-skill "demo")))
      (is (typep skill 'skill))
      (is (string= "demo" (skill-name skill)))
      (is (string= "Demo skill" (skill-description skill)))
      (is (string= "Short demo" (skill-short-description skill)))
      (is (string= "Demo interface description"
                   (clawmacs::skill-display-description skill)))
      (is-false (skill-allow-implicit-invocation-p skill))
      (is (member "SKILL.md" (skill-list-files skill) :test #'string=))
      (is (member "references/guide.md" (skill-list-files skill)
                  :test #'string=))
      (is (search "Use this skill" (read-skill-instructions skill)))
      (is (search "referenced guide" (skill-read-file skill "references/guide.md")))
      (is (search "references/guide.md:1"
                  (skill-search-to-string skill "needle"))))))

(test skill-enable-state-persists-by-path
  "Disabling a file-backed skill hides it until it is re-enabled."
  (with-isolated-skills (root)
    (let ((skill-path (write-demo-skill root)))
      (register-skill-root root)
      (is (= 1 (length (list-skills))))
      (disable-skill "demo")
      (is (= 0 (length (list-skills))))
      (is (= 1 (length (list-skills :include-disabled t))))
      (setf clawmacs::*skill-registry* nil
            clawmacs::*skill-disabled-paths* nil)
      (is (= 0 (length (list-skills))))
      (enable-skill skill-path)
      (is (= 1 (length (list-skills)))))))

(test programmatic-skills-are-available-without-files
  "init.lisp and packages can register skills directly in Lisp."
  (with-isolated-skills (root)
    root
    (register-skill-definition
     "inline"
     :description "Inline skill"
     :contents "---\nname: inline\ndescription: Inline skill\n---\nUse inline.")
    (let ((skill (find-skill "inline")))
      (is (string= "inline" (skill-name skill)))
      (is (equal '("SKILL.md") (skill-list-files skill)))
      (is (search "Use inline" (read-skill-instructions skill)))
      (is (equal '("inline")
                 (mapcar #'skill-name
                         (collect-skill-mentions "please use $inline")))))))

(test skill-mentions-ignore-env-vars-and-require-unambiguous-plain-names
  "Plain $name mentions resolve only when enabled and unambiguous."
  (with-isolated-skills (root)
    (let ((other-root (merge-pathnames #P"other/" root)))
      (write-demo-skill root :name "demo")
      (write-demo-skill other-root :name "demo")
      (register-skill-root root)
      (register-skill-root other-root)
      (is (null (collect-skill-mentions "inspect $PATH and $demo")))
      (let* ((skills (list-skills :include-disabled t))
             (first-skill (first skills)))
        (disable-skill (second skills))
        (is (equal '("demo")
                   (mapcar #'skill-name
                           (collect-skill-mentions "use $demo now"))))
        (is (equal (list (clawmacs::skill-path-key first-skill))
                   (mapcar #'clawmacs::skill-path-key
                           (collect-skill-mentions
                            (format nil "use [$demo](skill://~A)"
                                    (namestring (skill-path first-skill)))))))))))

(test skill-injection-messages-render-codex-style-blocks
  "Mentioned skills render as contextual <skill> blocks."
  (with-isolated-skills (root)
    (write-demo-skill root)
    (register-skill-root root)
    (let ((messages (skill-injection-messages "please use $demo")))
      (is (= 1 (length messages)))
      (is (search "<skill>" (first messages)))
      (is (search "<name>demo</name>" (first messages)))
      (is (search "Use this skill" (first messages))))))

(test build-conversation-messages-injects-mentioned-skills
  "Provider messages include skill context before the mentioning user message."
  (with-isolated-skills (root)
    (write-demo-skill root)
    (register-skill-root root)
    (let ((buf (make-buffer "skill-chat")))
      (clawmacs::set-message-text (buffer-input-message buf) "please use $demo")
      (buffer-finalize-input buf)
      (let* ((messages (build-conversation-messages buf))
             (first-content (cdr (assoc :content (first messages))))
             (second-content (cdr (assoc :content (second messages))))
             (first-text (cdr (assoc :text (aref first-content 0))))
             (second-text (cdr (assoc :text (aref second-content 0)))))
        (is (= 2 (length messages)))
        (is (search "<skill>" first-text))
        (is (search "please use $demo" second-text))))))

(test render-skills-section-lists-enabled-skills
  "The system prompt skill section lists enabled skills and lisp_eval usage."
  (with-isolated-skills (root)
    (write-demo-skill root)
    (register-skill-root root)
    (let ((section (render-skills-section)))
      (is (search "<skills_instructions>" section))
      (is (search "demo: Demo interface description" section))
      (is (search "Use `lisp_eval`" section)))))

(test prompt-args-accept-repeatable-skill-roots
  "prompt.sh can provide temporary skill roots for a prompt run."
  (let ((options (clawmacs::parse-clawmacs-prompt-args
                  '("--skill-root" "/tmp/a"
                    "--skill-root" "/tmp/b"
                    "hello"))))
    (is (equal '("/tmp/a" "/tmp/b")
               (clawmacs::prompt-options-skill-roots options)))
    (is (string= "hello" (clawmacs::prompt-options-prompt options)))))

(test minibuffer-insert-skill-command-inserts-linked-mention
  "The skill insert command inserts a precise linked mention into input."
  (with-isolated-skills (root)
    (write-demo-skill root)
    (register-skill-root root)
    (let ((buf (make-buffer "skill-ui")))
      (clawmacs::minibuffer-insert-skill-command buf)
      (is (eq t clawmacs::*minibuffer-active*))
      (is (string= "Insert Skill" clawmacs::*minibuffer-prompt*))
      (clawmacs::minibuffer-confirm)
      (is (search "[$demo](skill://"
                  (message-text (buffer-input-message buf)))))))

(test minibuffer-toggle-skill-command-persists-selection
  "The skill toggle command disables the selected skill."
  (with-isolated-skills (root)
    (write-demo-skill root)
    (register-skill-root root)
    (let ((buf (make-buffer "skill-toggle")))
      (clawmacs::minibuffer-toggle-skill-command buf)
      (is (string= "Toggle Skill" clawmacs::*minibuffer-prompt*))
      (clawmacs::minibuffer-confirm)
      (is (= 0 (length (list-skills))))
      (is (search "disabled" (message-text (message-prev (buffer-input-message buf))))))))
