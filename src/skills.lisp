(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Skill Registry
;;; --------------------------------------------------------------------------

(defparameter +default-skill-user-directory+
  (merge-pathnames #P".rplaca.d/skills/" (user-homedir-pathname))
  "Canonical user skill directory.")
(defparameter +legacy-skill-user-directory+
  (merge-pathnames #P".clawmacs.d/skills/" (user-homedir-pathname))
  "Legacy executable skill directory, never scanned automatically.")
(defvar *skill-user-directory* +default-skill-user-directory+
  "Default user skill directory.")

(defvar *skill-agents-directory*
  (merge-pathnames #P".agents/skills/" (user-homedir-pathname))
  "Default AGENTS-compatible user skill directory.")

(defparameter +default-skill-system-directory+
  (merge-pathnames #P".rplaca.d/skills/.system/" (user-homedir-pathname))
  "Canonical system skill directory.")
(defparameter +legacy-skill-system-directory+
  (merge-pathnames #P".clawmacs.d/skills/.system/" (user-homedir-pathname))
  "Legacy executable system skill directory, never scanned automatically.")
(defvar *skill-system-directory* +default-skill-system-directory+
  "Directory for bundled or locally installed system skills.")

(defparameter +default-skill-configuration-path+
  (merge-pathnames #P".rplaca.d/skills.json" (user-homedir-pathname))
  "Canonical persisted skill configuration.")
(defparameter +legacy-skill-configuration-path+
  (merge-pathnames #P".clawmacs.d/skills.json" (user-homedir-pathname))
  "Legacy behavioral skill registry, never read automatically.")
(defvar *skill-configuration-path* +default-skill-configuration-path+
  "Path to persisted skill enable/disable configuration.")

(defvar *skill-roots* nil
  "Additional skill roots registered by init.lisp, packages, or prompt flags.
Each entry is a SKILL-ROOT.")

(defvar *programmatic-skills* nil
  "Programmatic skill definitions registered by init.lisp or packages.")

(defvar *skill-registry* nil
  "Cached SKILL-LOAD-OUTCOME, or NIL when skills need to be reloaded.")

(defvar *skill-disabled-paths* nil
  "Hash table of canonical skill path keys disabled by configuration.")

(defvar *skill-scan-max-depth* 6
  "Maximum directory depth scanned below each skill root.")

(defvar *skill-scan-max-directories-per-root* 2000
  "Maximum number of directories scanned under a single skill root.")

(defvar *skill-list-file-limit* 1000
  "Maximum number of files returned by SKILL-LIST-FILES.")

(defvar *skill-search-result-limit* 100
  "Maximum number of matches returned by SKILL-SEARCH-TO-STRING.")

(defstruct skill
  "A local skill definition loaded from SKILL.md or registered programmatically."
  name
  description
  short-description
  display-name
  interface-short-description
  default-prompt
  icon-small
  icon-large
  brand-color
  allow-implicit-invocation-p
  path
  root
  scope
  source
  contents)

(defstruct (skill-root-definition
             (:constructor make-skill-root)
             (:predicate skill-root-p)
             (:conc-name skill-root-))
  "A directory root scanned for SKILL.md files."
  path
  scope
  source)

(defstruct skill-error
  "A recoverable skill loading error."
  path
  message)

(defstruct skill-load-outcome
  "Result of scanning skill roots."
  (skills nil)
  (errors nil)
  (disabled-paths nil))

(defun skill-blank-string-p (value)
  "Return true when VALUE is NIL or contains only ASCII whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))))

(defun skill-trim (value)
  "Return VALUE trimmed as a string, or NIL for blank values."
  (when value
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (string value))))
      (unless (zerop (length trimmed))
        trimmed))))

(defun normalize-skill-name (name)
  "Normalize NAME for matching and registry lookup."
  (let ((trimmed (skill-trim name)))
    (unless trimmed
      (error "Skill name must be a non-empty string"))
    (string-downcase trimmed)))

(defun ensure-directory-pathname* (path)
  "Return PATH as a directory pathname."
  (uiop:ensure-directory-pathname (pathname path)))

(defun canonical-skill-path-key (path)
  "Return a stable path key for PATH, canonicalizing when possible."
  (let* ((pathname (pathname path))
         (resolved (or (ignore-errors (truename pathname))
                       pathname)))
    (namestring resolved)))

(defun skill-path-key (skill)
  "Return the canonical persisted key for SKILL's path, or NIL."
  (when (skill-path skill)
    (canonical-skill-path-key (skill-path skill))))

(defun clear-skills-cache ()
  "Clear cached skills and disabled-path configuration."
  (setf *skill-registry* nil
        *skill-disabled-paths* nil))

(defun register-skill-root (path &key (scope :user) (source :init))
  "Register PATH as an additional skill root and clear the skill cache."
  (let ((root (make-skill-root :path (ensure-directory-pathname* path)
                               :scope scope
                               :source source)))
    (setf *skill-roots*
          (cons root
                (remove (canonical-skill-path-key (skill-root-path root))
                        *skill-roots*
                        :key (lambda (entry)
                               (canonical-skill-path-key (skill-root-path entry)))
                        :test #'string=)))
    (clear-skills-cache)
    root))

(defun register-skill-definition (name &key description short-description
                                       display-name default-prompt
                                       contents
                                       (scope :user)
                                       (source :programmatic)
                                       allow-implicit-invocation-p)
  "Register a programmatic skill definition and clear the skill cache."
  (let* ((normalized-name (normalize-skill-name name))
         (body (or contents
                   (format nil "---~%name: ~A~%description: ~A~%---~%"
                           normalized-name
                           (or description ""))))
         (skill (make-skill :name normalized-name
                            :description (or description "")
                            :short-description short-description
                            :display-name display-name
                            :default-prompt default-prompt
                            :allow-implicit-invocation-p
                            allow-implicit-invocation-p
                            :scope scope
                            :source source
                            :contents body)))
    (setf *programmatic-skills*
          (cons skill
                (remove normalized-name
                        *programmatic-skills*
                        :key #'skill-name
                        :test #'string=)))
    (clear-skills-cache)
    skill))

(defun skill-json-key-string (key)
  "Return KEY as a lowercase JSON field name string."
  (cond
    ((keywordp key) (string-downcase (symbol-name key)))
    ((symbolp key) (string-downcase (symbol-name key)))
    ((stringp key) key)
    (t (string key))))

(defun skill-lookup-json-value (alist key)
  "Look up KEY in a decoded JSON ALIST using string-insensitive key matching."
  (let ((target (string-downcase key)))
    (cdr (find target alist
               :key (lambda (entry)
                      (string-downcase (skill-json-key-string (car entry))))
               :test #'string=))))

(defun load-skill-disabled-paths ()
  "Load the persisted disabled skill path set."
  (let ((table (make-hash-table :test #'equal))
        (read-path
          (configured-migration-read-path
           *skill-configuration-path*
           +default-skill-configuration-path+
           +legacy-skill-configuration-path+
           :label "skill registry"
           :executable-p t)))
    (when (and read-path (probe-file read-path))
      (handler-case
          (let* ((json (uiop:read-file-string read-path))
                 (data (cl-json:decode-json-from-string json))
                 (disabled (or (skill-lookup-json-value data "disabled")
                               (skill-lookup-json-value data "disabled_paths"))))
            (dolist (path (coerce (or disabled #()) 'list))
              (when (stringp path)
                (setf (gethash (canonical-skill-path-key path) table) t))))
        (error (e)
          (format *error-output*
                  "~&;; Warning: error loading skill configuration ~A: ~A~%"
                  read-path e))))
    (setf *skill-disabled-paths* table)))

(defun ensure-skill-disabled-paths-loaded ()
  "Return the disabled skill path table, loading it if needed."
  (or *skill-disabled-paths*
      (load-skill-disabled-paths)))

(defun save-skill-configuration ()
  "Persist disabled skill path configuration."
  (ensure-skill-disabled-paths-loaded)
  (let ((disabled nil))
    (maphash (lambda (path disabled-p)
               (when disabled-p
                 (push path disabled)))
             *skill-disabled-paths*)
    (ensure-directories-exist *skill-configuration-path*)
    (with-open-file (stream *skill-configuration-path*
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string
       (cl-json:encode-json-to-string
        `((:disabled . ,(coerce (sort disabled #'string<) 'vector))))
       stream))
    *skill-configuration-path*))

(defun strip-optional-quotes (value)
  "Strip matching simple or double quotes around VALUE."
  (let ((trimmed (skill-trim value)))
    (cond
      ((and trimmed
            (>= (length trimmed) 2)
            (or (and (char= (char trimmed 0) #\")
                     (char= (char trimmed (1- (length trimmed))) #\"))
                (and (char= (char trimmed 0) #\')
                     (char= (char trimmed (1- (length trimmed))) #\'))))
       (subseq trimmed 1 (1- (length trimmed))))
      (t trimmed))))

(defun parse-yaml-scalar (value)
  "Parse the small one-line YAML scalar subset used by skill metadata."
  (let ((trimmed (strip-optional-quotes value)))
    (cond
      ((null trimmed) nil)
      ((string-equal trimmed "true") t)
      ((string-equal trimmed "false") nil)
      (t trimmed))))

(defun yaml-line-indent (line)
  "Return the number of leading spaces in LINE."
  (loop :for char :across line
        :while (char= char #\Space)
        :count t))

(defun parse-yaml-key-value-line (line)
  "Parse LINE as KEY: VALUE and return values KEY, VALUE, INDENT."
  (let* ((indent (yaml-line-indent line))
         (trimmed (string-trim '(#\Space #\Tab #\Return) line))
         (colon (position #\: trimmed)))
    (when (and colon (plusp colon))
      (values (string-downcase (subseq trimmed 0 colon))
              (parse-yaml-scalar (subseq trimmed (1+ colon)))
              indent))))

(defun split-lines (text)
  "Split TEXT into lines without retaining newline characters."
  (loop :for start := 0 :then (1+ pos)
        :for pos := (position #\Newline text :start start)
        :collect (subseq text start (or pos (length text)))
        :while pos))

(defun extract-skill-frontmatter (contents)
  "Return values FRONTMATTER and BODY from CONTENTS, or NIL when missing."
  (let ((lines (split-lines contents)))
    (when (and lines (string= "---" (string-trim '(#\Space #\Tab #\Return)
                                                 (first lines))))
      (let ((front nil)
            (body nil)
            (in-frontmatter-p t))
        (dolist (line (rest lines))
          (cond
            ((and in-frontmatter-p
                  (string= "---" (string-trim '(#\Space #\Tab #\Return) line)))
             (setf in-frontmatter-p nil))
            (in-frontmatter-p
             (push line front))
            (t
             (push line body))))
        (unless in-frontmatter-p
          (values (nreverse front)
                  (format nil "~{~A~^~%~}" (nreverse body))))))))

(defun parse-skill-frontmatter-lines (lines)
  "Parse the supported SKILL.md frontmatter subset into a plist."
  (let ((plist nil)
        (section nil))
    (dolist (line lines)
      (multiple-value-bind (key value indent)
          (parse-yaml-key-value-line line)
        (when key
          (cond
            ((zerop indent)
             (setf section key)
             (when value
               (setf (getf plist (intern (string-upcase key) :keyword))
                     value)))
            ((and section (string= section "metadata"))
             (when (string= key "short-description")
               (setf (getf plist :short-description) value)))))))
    plist))

(defun parse-skill-metadata-file (path)
  "Parse the supported agents/openai.yaml subset for PATH."
  (let ((metadata nil))
    (when (probe-file path)
      (handler-case
          (let ((section nil))
            (dolist (line (split-lines (uiop:read-file-string path)))
              (multiple-value-bind (key value indent)
                  (parse-yaml-key-value-line line)
                (when key
                  (cond
                    ((zerop indent)
                     (setf section key))
                    ((and section (string= section "interface"))
                     (cond
                       ((string= key "display_name")
                        (setf (getf metadata :display-name) value))
                       ((string= key "short_description")
                        (setf (getf metadata :interface-short-description) value))
                       ((string= key "default_prompt")
                        (setf (getf metadata :default-prompt) value))
                       ((string= key "icon_small")
                        (setf (getf metadata :icon-small) value))
                       ((string= key "icon_large")
                        (setf (getf metadata :icon-large) value))
                       ((string= key "brand_color")
                        (setf (getf metadata :brand-color) value))))
                    ((and section (string= section "policy"))
                     (when (string= key "allow_implicit_invocation")
                       (setf (getf metadata :allow-implicit-invocation-p)
                             value))))))))
        (error (e)
          (format *error-output*
                  "~&;; Warning: ignoring invalid skill metadata ~A: ~A~%"
                  path e))))
    metadata))

(defun default-skill-name-from-path (path)
  "Return a fallback skill name from PATH's parent directory."
  (let ((parent-name (car (last (pathname-directory
                                 (uiop:pathname-directory-pathname path))))))
    (if parent-name
        (normalize-skill-name (string parent-name))
        "skill")))

(defun parse-skill-file (path root scope source)
  "Parse PATH as a SKILL.md file."
  (let* ((contents (uiop:read-file-string path))
         (frontmatter-lines nil)
         (body nil))
    (multiple-value-setq (frontmatter-lines body)
      (extract-skill-frontmatter contents))
    (unless frontmatter-lines
      (error "missing YAML frontmatter delimited by ---"))
    (let* ((frontmatter (parse-skill-frontmatter-lines frontmatter-lines))
           (metadata-path (merge-pathnames #P"agents/openai.yaml"
                                           (uiop:pathname-directory-pathname path)))
           (metadata (parse-skill-metadata-file metadata-path))
           (name (or (skill-trim (getf frontmatter :name))
                     (default-skill-name-from-path path)))
           (description (or (getf frontmatter :description) ""))
           (short-description (getf frontmatter :short-description)))
      (make-skill :name (normalize-skill-name name)
                  :description description
                  :short-description short-description
                  :display-name (getf metadata :display-name)
                  :interface-short-description
                  (getf metadata :interface-short-description)
                  :default-prompt (getf metadata :default-prompt)
                  :icon-small (getf metadata :icon-small)
                  :icon-large (getf metadata :icon-large)
                  :brand-color (getf metadata :brand-color)
                  :allow-implicit-invocation-p
                  (getf metadata :allow-implicit-invocation-p)
                  :path (or (ignore-errors (truename path)) path)
                  :root root
                  :scope scope
                  :source source
                  :contents contents))))

(defun hidden-pathname-p (path)
  "Return true when PATH's final component starts with a dot."
  (let* ((dir (pathname-directory (uiop:ensure-directory-pathname path)))
         (name (or (pathname-name path)
                   (car (last dir)))))
    (and name
         (plusp (length (string name)))
         (char= #\. (char (string name) 0)))))

(defun discover-skills-under-root (root)
  "Discover skills under ROOT and return values SKILLS, ERRORS."
  (let* ((root-path (skill-root-path root))
         (root-dir (ignore-errors
                     (uiop:ensure-directory-pathname
                      (or (probe-file root-path)
                          root-path)))))
    (unless (and root-dir (probe-file root-dir))
      (return-from discover-skills-under-root (values nil nil)))
    (let ((queue (list (cons root-dir 0)))
          (visited (make-hash-table :test #'equal))
          (skills nil)
          (errors nil)
          (scanned 0))
      (loop :while queue
            :for item := (pop queue)
            :for dir := (car item)
            :for depth := (cdr item)
            :for key := (canonical-skill-path-key dir)
            :unless (gethash key visited)
              :do (setf (gethash key visited) t)
                  (incf scanned)
                  (when (> scanned *skill-scan-max-directories-per-root*)
                    (return))
                  (let ((skill-path (merge-pathnames #P"SKILL.md" dir)))
                    (when (probe-file skill-path)
                      (handler-case
                          (push (parse-skill-file skill-path
                                                  root-dir
                                                  (skill-root-scope root)
                                                  (skill-root-source root))
                                skills)
                        (error (e)
                          (push (make-skill-error :path skill-path
                                                  :message (format nil "~A" e))
                                errors)))))
                  (when (< depth *skill-scan-max-depth*)
                    (dolist (subdir (ignore-errors (uiop:subdirectories dir)))
                      (unless (hidden-pathname-p subdir)
                        (push (cons subdir (1+ depth)) queue)))))
      (values (nreverse skills) (nreverse errors)))))

(defun directory-ancestors (directory)
  "Return DIRECTORY and its parents from root to leaf."
  (let ((current (uiop:ensure-directory-pathname directory))
        (ancestors nil))
    (loop
      (push current ancestors)
      (let ((parent (uiop:pathname-parent-directory-pathname current)))
        (when (or (null parent)
                  (string= (namestring parent) (namestring current)))
          (return))
        (setf current parent)))
    (nreverse ancestors)))

(defun repo-skill-roots (&optional (cwd (truename ".")))
  "Return existing repo-local .agents/skills roots from CWD ancestors."
  (let ((roots nil))
    (dolist (dir (directory-ancestors (uiop:ensure-directory-pathname cwd)))
      (let ((candidate (merge-pathnames #P".agents/skills/" dir)))
        (when (probe-file candidate)
          (push (make-skill-root :path candidate
                                 :scope :repo
                                 :source :repo)
                roots))))
    (nreverse roots)))

(defun default-skill-roots ()
  "Return the default ordered skill root list."
  (append
   (repo-skill-roots)
   (append
    (mapcar (lambda (path)
              (make-skill-root :path path :scope :user :source :user))
            (configured-migration-read-roots
             *skill-user-directory*
             +default-skill-user-directory+
             +legacy-skill-user-directory+
             :label "user skill directory"
             :executable-p t))
    (list (make-skill-root :path *skill-agents-directory*
                           :scope :user
                           :source :agents))
    (mapcar (lambda (path)
              (make-skill-root :path path :scope :system :source :system))
            (configured-migration-read-roots
             *skill-system-directory*
             +default-skill-system-directory+
             +legacy-skill-system-directory+
             :label "system skill directory"
             :executable-p t)))
   (remove-if-not #'skill-root-p *skill-roots*)))

(defun dedupe-skill-roots (roots)
  "Return ROOTS with duplicate paths removed, preserving first occurrence."
  (let ((seen (make-hash-table :test #'equal))
        (out nil))
    (dolist (root roots)
      (let ((key (canonical-skill-path-key (skill-root-path root))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push root out))))
    (nreverse out)))

(defun compare-skills (a b)
  "Sort skills by scope precedence, name, and path."
  (labels ((scope-rank (scope)
             (case scope
               (:repo 0)
               (:user 1)
               (:system 2)
               (:admin 3)
               (otherwise 9))))
    (let ((scope-a (scope-rank (skill-scope a)))
          (scope-b (scope-rank (skill-scope b))))
      (cond
        ((< scope-a scope-b) t)
        ((> scope-a scope-b) nil)
        ((string< (skill-name a) (skill-name b)) t)
        ((string< (skill-name b) (skill-name a)) nil)
        (t
         (string< (or (skill-path-key a) "")
                  (or (skill-path-key b) "")))))))

(defun dedupe-skills-by-path (skills)
  "Dedupe file-backed SKILLS by path, preserving first occurrence."
  (let ((seen (make-hash-table :test #'equal))
        (out nil))
    (dolist (skill skills)
      (let ((key (or (skill-path-key skill)
                     (format nil "programmatic:~A" (skill-name skill)))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push skill out))))
    (nreverse out)))

(defun programmatic-skills ()
  "Return programmatically registered skills."
  (copy-list *programmatic-skills*))

(defun reload-skills ()
  "Reload skills from all configured roots and return a SKILL-LOAD-OUTCOME."
  (let ((skills (programmatic-skills))
        (errors nil)
        (disabled (ensure-skill-disabled-paths-loaded)))
    (dolist (root (dedupe-skill-roots (default-skill-roots)))
      (multiple-value-bind (root-skills root-errors)
          (discover-skills-under-root root)
        (setf skills (append skills root-skills)
              errors (append errors root-errors))))
    (let ((outcome (make-skill-load-outcome
                    :skills (sort (dedupe-skills-by-path skills) #'compare-skills)
                    :errors errors
                    :disabled-paths disabled)))
      (setf *skill-registry* outcome))))

(defun ensure-skills-loaded ()
  "Return the cached skill outcome, loading it if needed."
  (or *skill-registry*
      (reload-skills)))

(defun skill-enabled-p (skill-or-designator)
  "Return true when SKILL-OR-DESIGNATOR is enabled."
  (let ((skill (etypecase skill-or-designator
                 (skill skill-or-designator)
                 ((or string pathname)
                  (find-skill skill-or-designator :include-disabled t)))))
    (cond
      ((null skill) nil)
      ((null (skill-path skill)) t)
      (t
       (not (gethash (skill-path-key skill)
                     (ensure-skill-disabled-paths-loaded)))))))

(defun list-skills (&key include-disabled)
  "Return loaded skills, excluding disabled skills unless INCLUDE-DISABLED is true."
  (let ((skills (copy-list (skill-load-outcome-skills (ensure-skills-loaded)))))
    (if include-disabled
        skills
        (remove-if-not #'skill-enabled-p skills))))

(defun list-skill-errors ()
  "Return recoverable skill load errors from the last load."
  (copy-list (skill-load-outcome-errors (ensure-skills-loaded))))

(defun find-skills-by-name (name &key include-disabled)
  "Return all loaded skills named NAME."
  (let ((normalized (normalize-skill-name name)))
    (remove normalized (list-skills :include-disabled include-disabled)
            :key #'skill-name
            :test-not #'string=)))

(defun find-skill-by-path (path &key include-disabled)
  "Return the skill loaded from PATH, or NIL."
  (let ((key (canonical-skill-path-key path)))
    (find key (list-skills :include-disabled include-disabled)
          :key #'skill-path-key
          :test #'string=)))

(defun find-skill (designator &key include-disabled)
  "Find a skill by name, pathname, absolute SKILL.md path, or skill:// path."
  (cond
    ((skill-p designator) designator)
    ((pathnamep designator)
     (find-skill-by-path designator :include-disabled include-disabled))
    ((stringp designator)
     (let ((path (if (alexandria:starts-with-subseq "skill://" designator)
                     (subseq designator (length "skill://"))
                     designator)))
       (or (and (or (alexandria:starts-with-subseq "/" path)
                    (search "SKILL.md" path :test #'char-equal))
                (find-skill-by-path path :include-disabled include-disabled))
           (first (find-skills-by-name designator
                                       :include-disabled include-disabled)))))
    (t nil)))

(defun set-skill-enabled (skill-or-designator enabled-p)
  "Enable or disable SKILL-OR-DESIGNATOR and persist the setting."
  (let ((skill (find-skill skill-or-designator :include-disabled t)))
    (unless skill
      (error "Unknown skill: ~A" skill-or-designator))
    (unless (skill-path skill)
      (error "Programmatic skill ~A cannot be persisted by path"
             (skill-name skill)))
    (let ((key (skill-path-key skill)))
      (if enabled-p
          (remhash key (ensure-skill-disabled-paths-loaded))
          (setf (gethash key (ensure-skill-disabled-paths-loaded)) t)))
    (setf *skill-registry* nil)
    (save-skill-configuration)
    enabled-p))

(defun enable-skill (skill-or-designator)
  "Enable SKILL-OR-DESIGNATOR."
  (set-skill-enabled skill-or-designator t))

(defun disable-skill (skill-or-designator)
  "Disable SKILL-OR-DESIGNATOR."
  (set-skill-enabled skill-or-designator nil))

(defun read-skill-instructions (skill-or-designator)
  "Return the full SKILL.md contents for SKILL-OR-DESIGNATOR."
  (let ((skill (find-skill skill-or-designator :include-disabled t)))
    (unless skill
      (error "Unknown skill: ~A" skill-or-designator))
    (or (skill-contents skill)
        (and (skill-path skill)
             (uiop:read-file-string (skill-path skill)))
        "")))

(defun skill-directory (skill)
  "Return SKILL's containing directory, or NIL for programmatic skills."
  (when (skill-path skill)
    (uiop:pathname-directory-pathname (skill-path skill))))

(defun validate-skill-resource-path (relative-path)
  "Validate RELATIVE-PATH is a skill-local resource path."
  (let ((path (pathname relative-path)))
    (when (or (uiop:absolute-pathname-p path)
              (member :up (pathname-directory path)))
      (error "Skill resource paths must be relative and may not contain ..: ~A"
             relative-path))
    path))

(defun skill-resource-path (skill relative-path)
  "Resolve RELATIVE-PATH under SKILL's directory."
  (let ((dir (or (skill-directory skill)
                 (error "Skill ~A is programmatic and has no resource directory"
                        (skill-name skill)))))
    (merge-pathnames (validate-skill-resource-path relative-path) dir)))

(defun pathname-relative-to-directory (path directory)
  "Return PATH relative to DIRECTORY as a slash-separated string."
  (let ((relative (enough-namestring path directory)))
    (substitute #\/ #\\ relative)))

(defun skill-list-files (skill-or-designator)
  "Return skill-local files for SKILL-OR-DESIGNATOR."
  (let ((skill (find-skill skill-or-designator :include-disabled t)))
    (unless skill
      (error "Unknown skill: ~A" skill-or-designator))
    (if (null (skill-directory skill))
        '("SKILL.md")
        (let ((files nil)
              (queue (list (skill-directory skill)))
              (visited (make-hash-table :test #'equal)))
          (loop :while queue
                :while (< (length files) *skill-list-file-limit*)
                :for dir := (pop queue)
                :for key := (canonical-skill-path-key dir)
                :unless (gethash key visited)
                  :do (setf (gethash key visited) t)
                      (dolist (file (ignore-errors (uiop:directory-files dir)))
                        (unless (hidden-pathname-p file)
                          (push (pathname-relative-to-directory
                                 file (skill-directory skill))
                                files)))
                      (dolist (subdir (ignore-errors (uiop:subdirectories dir)))
                        (unless (hidden-pathname-p subdir)
                          (push subdir queue))))
          (sort files #'string<)))))

(defun skill-read-file (skill-or-designator relative-path)
  "Read RELATIVE-PATH from SKILL-OR-DESIGNATOR."
  (let ((skill (find-skill skill-or-designator :include-disabled t)))
    (unless skill
      (error "Unknown skill: ~A" skill-or-designator))
    (if (and (null (skill-directory skill))
             (string= relative-path "SKILL.md"))
        (read-skill-instructions skill)
        (uiop:read-file-string (skill-resource-path skill relative-path)))))

(defun search-lines-in-text (path text query)
  "Return path:line matches for QUERY in TEXT."
  (let ((matches nil)
        (line-number 0))
    (dolist (line (split-lines text))
      (incf line-number)
      (when (search query line :test #'char-equal)
        (push (format nil "~A:~D: ~A" path line-number line)
              matches)))
    (nreverse matches)))

(defun skill-search-to-string (skill-or-designator query)
  "Search SKILL-OR-DESIGNATOR resources for QUERY and return display text."
  (let ((results nil))
    (dolist (path (skill-list-files skill-or-designator))
      (when (< (length results) *skill-search-result-limit*)
        (handler-case
            (setf results
                  (append results
                          (search-lines-in-text
                           path
                           (skill-read-file skill-or-designator path)
                           query)))
          (error () nil))))
    (if results
        (format nil "~{~A~^~%~}"
                (subseq results 0 (min (length results)
                                       *skill-search-result-limit*)))
        (format nil "No matches for ~S in skill ~A."
                query
                (skill-name (find-skill skill-or-designator
                                        :include-disabled t))))))

(defun skill-effective-display-name (skill)
  "Return SKILL's user-facing display name."
  (or (skill-display-name skill)
      (skill-name skill)))

(defun skill-display-description (skill)
  "Return SKILL's short display description."
  (or (skill-interface-short-description skill)
      (skill-short-description skill)
      (skill-description skill)
      ""))

(defun describe-skill-to-string (skill-or-designator)
  "Return a human-readable description of SKILL-OR-DESIGNATOR."
  (let ((skill (find-skill skill-or-designator :include-disabled t)))
    (unless skill
      (error "Unknown skill: ~A" skill-or-designator))
    (with-output-to-string (s)
      (format s "Skill: ~A~%" (skill-name skill))
      (format s "Enabled: ~A~%" (if (skill-enabled-p skill) "yes" "no"))
      (format s "Scope: ~(~A~)~%" (or (skill-scope skill) :unknown))
      (format s "Source: ~(~A~)~%" (or (skill-source skill) :unknown))
      (when (skill-path skill)
        (format s "Path: ~A~%" (namestring (skill-path skill))))
      (let ((description (skill-display-description skill)))
        (when (plusp (length description))
          (format s "~%~A~%" description)))
      (format s "~%Files:~%~{  ~A~%~}" (skill-list-files skill)))))

(defun mention-name-char-p (char)
  "Return true when CHAR can appear in a $skill mention."
  (or (alphanumericp char)
      (member char '(#\_ #\- #\:) :test #'char=)))

(defun common-env-var-mention-p (name)
  "Return true when NAME is a common environment variable, not a skill mention."
  (member (string-upcase name)
          '("PATH" "HOME" "USER" "SHELL" "PWD" "TMPDIR" "TEMP" "TMP"
            "LANG" "TERM" "XDG_CONFIG_HOME")
          :test #'string=))

(defun parse-linked-skill-mention (text start)
  "Parse [$name](path) at START. Return values NAME, PATH, END or NIL."
  (when (and (< (+ start 2) (length text))
             (char= #\[ (char text start))
             (char= #\$ (char text (1+ start))))
    (let* ((name-start (+ start 2))
           (name-end name-start))
      (loop :while (and (< name-end (length text))
                        (mention-name-char-p (char text name-end)))
            :do (incf name-end))
      (when (and (> name-end name-start)
                 (< name-end (length text))
                 (char= #\] (char text name-end)))
        (let ((open (position #\( text :start (1+ name-end))))
          (when (and open
                     (loop :for i :from (1+ name-end) :below open
                           :always (member (char text i)
                                           '(#\Space #\Tab #\Newline #\Return)
                                           :test #'char=)))
            (let ((close (position #\) text :start (1+ open))))
              (when close
                (let ((name (subseq text name-start name-end))
                      (path (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (subseq text (1+ open) close))))
                  (unless (skill-blank-string-p path)
                    (values name path (1+ close))))))))))))

(defun collect-raw-skill-mentions (text)
  "Return values PLAIN-NAMES and LINKED-PATHS mentioned in TEXT."
  (let ((plain nil)
        (paths nil)
        (index 0))
    (loop :while (< index (length text))
          :do (multiple-value-bind (linked-name linked-path linked-end)
                  (parse-linked-skill-mention text index)
                (cond
                  (linked-end
                   (unless (common-env-var-mention-p linked-name)
                     (push linked-path paths))
                   (setf index linked-end))
                  ((char= #\$ (char text index))
                   (let ((start (1+ index)))
                     (if (and (< start (length text))
                              (mention-name-char-p (char text start)))
                         (let ((end start))
                           (loop :while (and (< end (length text))
                                             (mention-name-char-p (char text end)))
                                 :do (incf end))
                           (let ((name (subseq text start end)))
                             (unless (common-env-var-mention-p name)
                               (push name plain)))
                           (setf index end))
                         (incf index))))
                  (t
                   (incf index)))))
    (values (nreverse plain) (nreverse paths))))

(defun unambiguous-skill-by-name (name skills)
  "Return the unique skill named NAME in SKILLS, or NIL."
  (let* ((normalized (normalize-skill-name name))
         (matches (remove normalized skills :key #'skill-name :test-not #'string=)))
    (when (= 1 (length matches))
      (first matches))))

(defun collect-skill-mentions (text &key (skills (list-skills)))
  "Return enabled skills explicitly mentioned by TEXT."
  (multiple-value-bind (plain-names linked-paths)
      (collect-raw-skill-mentions text)
    (let ((selected nil)
          (seen-paths (make-hash-table :test #'equal))
          (seen-names (make-hash-table :test #'equal)))
      (labels ((select (skill)
                 (when skill
                   (let ((key (or (skill-path-key skill)
                                  (format nil "programmatic:~A"
                                          (skill-name skill)))))
                     (unless (gethash key seen-paths)
                       (setf (gethash key seen-paths) t
                             (gethash (skill-name skill) seen-names) t)
                       (push skill selected))))))
        (dolist (path linked-paths)
          (let ((normalized-path (if (alexandria:starts-with-subseq
                                      "skill://" path)
                                     (subseq path (length "skill://"))
                                     path)))
            (select (find-skill-by-path normalized-path))))
        (dolist (name plain-names)
          (unless (gethash (normalize-skill-name name) seen-names)
            (select (unambiguous-skill-by-name name skills)))))
      (nreverse selected))))

(defun render-skill-instructions-block (skill)
  "Render SKILL as a Codex-compatible contextual skill block."
  (format nil "<skill>~%<name>~A</name>~%<path>~A</path>~%~A~%</skill>"
          (skill-name skill)
          (or (and (skill-path skill)
                   (namestring (skill-path skill)))
              (format nil "programmatic:~A" (skill-name skill)))
          (read-skill-instructions skill)))

(defun skill-injection-messages (text &key (skills (list-skills)))
  "Return contextual skill instruction strings for mentions in TEXT."
  (mapcar #'render-skill-instructions-block
          (collect-skill-mentions text :skills skills)))

(defun render-skills-section (&optional (skills (list-skills)))
  "Render the system-prompt skill discovery section."
  (when skills
    (with-output-to-string (s)
      (format s "<skills_instructions>~%")
      (format s "## Skills~%")
      (format s "A skill is a set of local instructions in a `SKILL.md` file. Available skills are listed by name, description, and path. Use `lisp_eval` to inspect the skill file and any referenced local resources before relying on it.~%")
      (format s "### Available skills~%")
      (dolist (skill skills)
        (format s "- ~A: ~A (file: ~A)~%"
                (skill-name skill)
                (skill-display-description skill)
                (or (and (skill-path skill)
                         (namestring (skill-path skill)))
                    (format nil "programmatic:~A" (skill-name skill)))))
      (format s "### How to use skills~%")
      (format s "- If the user names a skill with `$SkillName` or the task clearly matches a skill description, use that skill for the turn.~%")
      (format s "- Do not carry skills across turns unless re-mentioned.~%")
      (format s "- Read only the needed parts of `SKILL.md` and referenced files. Resolve relative paths from the skill directory.~%")
      (format s "- Prefer skill scripts and assets as references; do not assume they expose or execute new tools.~%")
      (format s "- If a skill cannot be read or applied cleanly, say so briefly and continue with the best fallback.~%")
      (format s "</skills_instructions>"))))

(defun list-skills-to-string (&key include-disabled)
  "Return a human-readable list of loaded skills and load errors."
  (with-output-to-string (s)
    (format s "Skills~%======~%~%")
    (dolist (skill (list-skills :include-disabled include-disabled))
      (format s "~A ~A  [~(~A~)]~%"
              (if (skill-enabled-p skill) "[x]" "[ ]")
              (skill-name skill)
              (or (skill-scope skill) :unknown))
      (let ((description (skill-display-description skill)))
        (when (plusp (length description))
          (format s "    ~A~%" description)))
      (when (skill-path skill)
        (format s "    ~A~%" (namestring (skill-path skill)))))
    (let ((errors (list-skill-errors)))
      (when errors
        (format s "~%Load errors~%-----------~%")
        (dolist (error errors)
          (format s "~A: ~A~%"
                  (skill-error-path error)
                  (skill-error-message error)))))))
