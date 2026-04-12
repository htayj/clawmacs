(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Project/resource abstraction
;;; --------------------------------------------------------------------------

(defstruct project
  "A named collection of resources backed by an implementation-specific store."
  name
  root
  description
  source
  systems
  check-functions
  reload-function)

(defstruct change-set
  "A staged set of project resource mutations."
  id
  name
  description
  entries
  status
  created-at
  applied-at)

(defstruct change-set-entry
  "One staged project resource mutation."
  kind
  project-name
  path
  new-path
  new-text
  old-exists-p
  old-text
  target-old-exists-p
  target-old-text
  applied-p)

(defvar *project-definitions-directory*
  (merge-pathnames #P".clawmacs.projects.d/" (user-homedir-pathname))
  "Directory containing inert project definition manifests.")

(defvar *project-registry* (make-hash-table :test #'equal)
  "Project registry keyed by normalized project name.")

(defvar *project-definitions-loaded-p* nil
  "True after project manifests have been loaded for this process.")

(defvar *project-manifest-extension* "project"
  "Extension used for inert project definition manifests.")

(defvar *project-ignored-directory-names*
  '(".git" ".hg" ".svn" ".cache" ".direnv" ".claude" ".serena"
    ".worktrees" "__pycache__" "node_modules" "target")
  "Directory names ignored by project listing and search.")

(defvar *project-bulk-directory-names*
  '("vendor" "reference" "external_src" "external-src" "screenshots")
  "Bulky reference/artifact directories hidden from default project traversal.
Agents can pass :INCLUDE-BULK T to PROJECT-LIST-FILES, PROJECT-SEARCH, and
PROJECT-OUTLINE-TO-STRING when these resources are specifically needed.")

(defvar *project-ignored-file-names*
  '("debug.log" "debug-prompt.log" ".DS_Store")
  "File names ignored by project listing and search.")

(defvar *project-ignored-file-types*
  '("fasl" "fas" "o" "so" "dylib" "dll")
  "File extensions ignored by project listing and search.")

(defvar *project-list-file-limit* 500
  "Default maximum number of files returned by PROJECT-LIST-FILES.")

(defvar *project-search-result-limit* 100
  "Default maximum number of matches returned by PROJECT-SEARCH.")

(defvar *project-outline-file-limit* 80
  "Default maximum number of Lisp source files outlined by PROJECT-OUTLINE-TO-STRING.")

(defvar *project-write-events* nil
  "Dynamic list of project write events captured during prompt-mode runs.")

(defvar *change-set-registry* (make-hash-table :test #'equal)
  "Registry of staged project change sets keyed by id.")

(defvar *current-change-set* nil
  "The current change set used by staging helpers when none is supplied.")

(defvar *change-set-counter* 0
  "Monotonic counter used to create human-readable change set ids.")

(defun record-project-write-event (kind project path &key new-path bytes)
  "Record a project write event in *PROJECT-WRITE-EVENTS* and return it."
  (let ((event (list :kind kind
                     :project (project-name (ensure-project project))
                     :path (project-resource-name path))))
    (when new-path
      (setf (getf event :new-path) (project-resource-name new-path)))
    (when bytes
      (setf (getf event :bytes) bytes))
    (push event *project-write-events*)
    event))

(defun normalize-project-name (name)
  "Normalize NAME to a non-empty registry key."
  (let ((normalized
          (string-downcase
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (etypecase name
                          (string name)
                          (symbol (symbol-name name)))))))
    (unless (plusp (length normalized))
      (error "Project name cannot be empty."))
    normalized))

(defun project-display-name (name)
  "Return NAME as a trimmed display string."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (etypecase name
                 (string name)
                 (symbol (symbol-name name)))))

(defun project-designator-string (designator)
  "Return DESIGNATOR as a printable comparison string."
  (typecase designator
    (string designator)
    (symbol (symbol-name designator))
    (pathname (namestring designator))
    (t (prin1-to-string designator))))

(defun normalize-project-system-list (systems)
  "Normalize SYSTEMS to a list of ASDF system designators."
  (cond
    ((null systems) nil)
    ((and (listp systems)
          (not (stringp systems))
          (not (pathnamep systems)))
     systems)
    (t (list systems))))

(defun normalize-project-function-list (functions)
  "Normalize FUNCTIONS to a list of function designators."
  (cond
    ((null functions) nil)
    ((and (listp functions)
          (not (functionp functions)))
     functions)
    (t (list functions))))

(defun ensure-directory-path-exists (directory)
  "Ensure DIRECTORY exists and return its truename as a directory pathname."
  (let ((dir (uiop:ensure-directory-pathname directory)))
    (ensure-directories-exist (merge-pathnames #P".keep" dir))
    (truename dir)))

(defun normalize-project-root (root &key create-if-missing)
  "Return ROOT as a true directory pathname."
  (let ((dir (uiop:ensure-directory-pathname (pathname root))))
    (if create-if-missing
        (ensure-directory-path-exists dir)
        (or (probe-file dir)
            (error "Project root does not exist: ~A" dir)))))

(defun project-definition-directory ()
  "Return the active project definitions directory, creating it if needed."
  (ensure-directory-path-exists *project-definitions-directory*))

(defun project-manifest-path (name &optional (directory *project-definitions-directory*))
  "Return the manifest pathname for project NAME in DIRECTORY."
  (merge-pathnames
   (make-pathname :name (filesystem-safe-component (normalize-project-name name))
                  :type *project-manifest-extension*)
   (uiop:ensure-directory-pathname directory)))

(defun project->manifest-plist (project)
  "Return PROJECT as an inert manifest property list."
  (list :name (project-name project)
        :root (namestring (project-root project))
        :description (or (project-description project) "")
        :systems (or (project-systems project) '())))

(defun valid-project-manifest-p (manifest)
  "Return true when MANIFEST is a proper project property list."
  (and (listp manifest)
       (handler-case
           (evenp (length manifest))
         (type-error () nil))
       (getf manifest :name)
       (getf manifest :root)))

(defun read-project-manifest (path)
  "Read PATH as inert data and return a project plist."
  (let ((*read-eval* nil))
    (with-open-file (stream path :direction :input)
      (let ((manifest (read stream nil :eof)))
        (when (eq manifest :eof)
          (error "Project manifest is empty: ~A" path))
        (unless (valid-project-manifest-p manifest)
          (error "Project manifest must be a plist with :NAME and :ROOT: ~A"
                 path))
        manifest))))

(defun write-project-manifest (project &optional
                                       (directory *project-definitions-directory*))
  "Persist PROJECT as an inert manifest and return the manifest path."
  (let ((path (project-manifest-path (project-name project) directory)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write (project->manifest-plist project)
             :stream stream
             :pretty t
             :case :downcase)
      (terpri stream))
    path))

(defun register-project (project &key (replace t))
  "Register PROJECT and return it.
When REPLACE is NIL, an existing project with the same name is preserved."
  (let ((key (normalize-project-name (project-name project))))
    (cond
      ((and (not replace) (gethash key *project-registry*))
       (gethash key *project-registry*))
      (t
       (setf (project-name project) (project-display-name (project-name project))
             (project-root project) (normalize-project-root
                                     (project-root project)))
       (setf (gethash key *project-registry*) project)))))

(defun define-project (name &key root description systems check-functions
                              reload-function (create-if-missing nil)
                              (source :programmatic) (replace t))
  "Define a project named NAME rooted at ROOT."
  (unless root
    (error "DEFINE-PROJECT requires :ROOT."))
  (register-project
   (make-project :name (project-display-name name)
                 :root (normalize-project-root root
                                               :create-if-missing
                                               create-if-missing)
                 :description description
                 :source source
                 :systems (normalize-project-system-list systems)
                 :check-functions (normalize-project-function-list
                                   check-functions)
                 :reload-function reload-function)
   :replace replace))

(defun create-project (name &key root description systems check-functions
                              reload-function (persist t)
                              &allow-other-keys)
  "Create a project and optionally persist its manifest.
When ROOT is omitted, create a directory under *PROJECT-DEFINITIONS-DIRECTORY*."
  (let* ((default-root
           (merge-pathnames
            (format nil "~A/" (filesystem-safe-component
                               (normalize-project-name name)))
            (project-definition-directory)))
         (project (define-project name
                    :root (or root default-root)
                    :description description
                    :systems systems
                    :check-functions check-functions
                    :reload-function reload-function
                    :create-if-missing t
                    :source (if persist :manifest :programmatic)
                    :replace t)))
    (when persist
      (write-project-manifest project))
    project))

(defun find-project (name)
  "Return the project named NAME, or NIL."
  (gethash (normalize-project-name name) *project-registry*))

(defun ensure-project (project-designator)
  "Resolve PROJECT-DESIGNATOR to a project object."
  (etypecase project-designator
    (project project-designator)
    ((or string symbol)
     (or (find-project project-designator)
         (error "Unknown project: ~A" project-designator)))))

(defun list-projects ()
  "Return registered projects sorted by name."
  (let (projects)
    (maphash (lambda (key project)
               (declare (ignore key))
               (push project projects))
             *project-registry*)
    (sort projects #'string< :key #'project-name)))

(defun load-project-manifest (path &key (replace nil))
  "Load one inert project manifest from PATH."
  (let ((manifest (read-project-manifest path)))
    (define-project (getf manifest :name)
      :root (getf manifest :root)
      :description (getf manifest :description)
      :systems (getf manifest :systems)
      :source :manifest
      :replace replace)))

(defun config-project-root ()
  "Return the configured Clawmacs init directory."
  (if (boundp '*user-init-directory*)
      (symbol-value '*user-init-directory*)
      (merge-pathnames #P".clawmacs.d/" (user-homedir-pathname))))

(defun ensure-config-project ()
  "Ensure the user configuration directory is available as project \"config\"."
  (define-project "config"
    :root (config-project-root)
    :description "Clawmacs user configuration"
    :create-if-missing t
    :source :builtin
    :replace nil))

(defun project-manifest-paths (&optional (directory *project-definitions-directory*))
  "Return project manifest paths under DIRECTORY."
  (let ((dir (ensure-directory-path-exists directory)))
    (sort (directory
           (merge-pathnames
            (make-pathname :name :wild :type *project-manifest-extension*)
            dir))
          #'string< :key #'namestring)))

(defun load-project-definitions ()
  "Load built-in and manifest-backed project definitions.
Existing projects, usually from init.lisp, are not overwritten."
  (ensure-config-project)
  (dolist (path (project-manifest-paths *project-definitions-directory*))
    (handler-case
        (load-project-manifest path :replace nil)
      (error (e)
        (format *error-output*
                "~&;; Warning: error loading project manifest ~A:~%;; ~A~%"
                path e))))
  (setf *project-definitions-loaded-p* t)
  (list-projects))

(defun remove-projects-by-source (sources)
  "Remove projects whose source is a member of SOURCES."
  (maphash (lambda (key project)
             (when (member (project-source project) sources :test #'eq)
               (remhash key *project-registry*)))
           *project-registry*))

(defun reload-projects ()
  "Reload built-in and manifest-backed projects, preserving programmatic ones."
  (remove-projects-by-source '(:manifest :builtin))
  (setf *project-definitions-loaded-p* nil)
  (load-project-definitions))

(defun project-relative-pathname (path &key allow-directory)
  "Return PATH as a safe relative pathname."
  (let* ((pathname (pathname path))
         (directory (pathname-directory pathname))
         (namestring (namestring pathname)))
    (when (or (zerop (length namestring))
              (uiop:absolute-pathname-p pathname)
              (wild-pathname-p pathname)
              (member :absolute directory :test #'eq)
              (member :up directory :test #'eq)
              (member :back directory :test #'eq))
      (error "Project resource paths must be relative and cannot escape the project: ~A"
             path))
    (unless (or allow-directory (pathname-name pathname) (pathname-type pathname))
      (error "Project resource path names a directory, not a file: ~A" path))
    pathname))

(defun project-resource-name (path &key allow-directory)
  "Return PATH as a normalized project-relative namestring."
  (namestring (project-relative-pathname path :allow-directory allow-directory)))

(defun path-under-directory-p (directory path)
  "Return true when PATH's namestring starts with DIRECTORY's namestring."
  (alexandria:starts-with-subseq (namestring directory)
                                 (namestring path)))

(defun validate-project-existing-prefixes (project relative-path)
  "Reject symlinked existing path prefixes that escape PROJECT."
  (let ((current (project-root project)))
    (dolist (component (rest (or (pathname-directory relative-path)
                                 '(:relative))))
      (setf current (merge-pathnames
                     (make-pathname :directory (list :relative component))
                     current))
      (when (probe-file current)
        (let ((true (truename current)))
          (unless (path-under-directory-p (project-root project) true)
            (error "Project path escapes root through ~A" true)))))))

(defun project-resolve-path (project-designator path
                              &key require-exists allow-directory)
  "Resolve project-relative PATH inside PROJECT-DESIGNATOR."
  (let* ((project (ensure-project project-designator))
         (relative (project-relative-pathname path
                                             :allow-directory allow-directory))
         (target (merge-pathnames relative (project-root project))))
    (validate-project-existing-prefixes project relative)
    (cond
      ((probe-file target)
       (let ((true (truename target)))
         (unless (path-under-directory-p (project-root project) true)
           (error "Project path escapes root: ~A" path))
         true))
      (require-exists
       (error "Project file does not exist: ~A:~A"
              (project-name project)
              path))
      (t
       target))))

(defun project-relative-namestring (project pathname)
  "Return PATHNAME relative to PROJECT root."
  (let ((root (namestring (project-root project)))
        (full (namestring pathname)))
    (if (alexandria:starts-with-subseq root full)
        (subseq full (length root))
        full)))

(defun ignored-project-directory-p (directory &key include-ignored include-bulk)
  "Return true when DIRECTORY should be skipped during project traversal."
  (let* ((components (pathname-directory (uiop:ensure-directory-pathname directory)))
         (name (car (last components))))
    (or (and (not include-ignored)
             (member name *project-ignored-directory-names* :test #'string=))
        (and (not include-bulk)
             (member name *project-bulk-directory-names* :test #'string=)))))

(defun ignored-project-file-p (path &key include-ignored)
  "Return true when PATH should be skipped during project traversal."
  (let* ((name (file-namestring path))
         (type (pathname-type path)))
    (and (not include-ignored)
         (or (member name *project-ignored-file-names* :test #'string=)
             (and type
                  (member (string-downcase type)
                          *project-ignored-file-types*
                          :test #'string=))
             (alexandria:ends-with-subseq "~" name)
             (and (alexandria:starts-with-subseq "#" name)
                  (alexandria:ends-with-subseq "#" name))
             (alexandria:starts-with-subseq ".#" name)))))

(defun project-files-recursively (project &key include-ignored include-bulk)
  "Return all non-ignored files under PROJECT."
  (labels ((ignored-directory-p (directory)
             (ignored-project-directory-p directory
                                          :include-ignored include-ignored
                                          :include-bulk include-bulk))
           (ignored-file-p (path)
             (ignored-project-file-p path :include-ignored include-ignored))
           (walk (directory)
             (unless (ignored-directory-p directory)
               (nconc (remove-if #'ignored-file-p
                                  (uiop:directory-files directory))
                      (loop :for child :in (uiop:subdirectories directory)
                            :unless (ignored-directory-p child)
                              :nconc (walk child))))))
    (walk (project-root project))))

(defun project-list-files (project-designator &key
                              (limit *project-list-file-limit*)
                              include-ignored
                              include-bulk)
  "Return sorted project-relative file paths for PROJECT-DESIGNATOR."
  (let* ((project (ensure-project project-designator))
         (files (sort (mapcar (lambda (path)
                                (project-relative-namestring project path))
                              (project-files-recursively
                               project
                               :include-ignored include-ignored
                               :include-bulk include-bulk))
                      #'string<)))
    (if limit
        (subseq files 0 (min limit (length files)))
        files)))

(defun project-read-file (project-designator path)
  "Read a project resource as text."
  (uiop:read-file-string
   (project-resolve-path project-designator path :require-exists t)))

(defun synchronize-open-project-file-buffer (project-designator path text
                                             &key (dirty-p nil)
                                                  (original-text text))
  "Update an already-open file buffer after a direct project resource write."
  (let* ((project (ensure-project project-designator))
         (resource-path (project-resource-name path))
         (buffer
           (find-if (lambda (candidate)
                      (and (file-buffer-p candidate)
                           (string= (or (buffer-project-name candidate) "")
                                    (project-name project))
                           (string= (or (buffer-resource-path candidate) "")
                                    resource-path)))
                    *buffer-ring*)))
    (when buffer
      (set-message-text (buffer-input-message buffer) text)
      (setf (buffer-original-text buffer) original-text
            (buffer-dirty-p buffer) dirty-p)
      buffer)))

(defun remove-open-project-file-buffer (project-designator path)
  "Close an open file buffer for PROJECT-DESIGNATOR/PATH, if one exists."
  (let* ((project (ensure-project project-designator))
         (resource-path (project-resource-name path))
         (buffer
           (find-if (lambda (candidate)
                      (and (file-buffer-p candidate)
                           (string= (or (buffer-project-name candidate) "")
                                    (project-name project))
                           (string= (or (buffer-resource-path candidate) "")
                                    resource-path)))
                    *buffer-ring*)))
    (when buffer
      (kill-buffer-from-ring buffer)
      buffer)))

(defun retarget-open-project-file-buffer (project-designator old-path new-path text)
  "Retarget an open file buffer from OLD-PATH to NEW-PATH after a rename."
  (let* ((project (ensure-project project-designator))
         (old-resource-path (project-resource-name old-path))
         (new-resource-path (project-resource-name new-path))
         (buffer
           (find-if (lambda (candidate)
                      (and (file-buffer-p candidate)
                           (string= (or (buffer-project-name candidate) "")
                                    (project-name project))
                           (string= (or (buffer-resource-path candidate) "")
                                    old-resource-path)))
                    *buffer-ring*)))
    (when buffer
      (set-message-text (buffer-input-message buffer) text)
      (setf (buffer-name buffer) (format nil "~A:~A"
                                         (project-name project)
                                         new-resource-path)
            (buffer-resource-path buffer) new-resource-path
            (buffer-original-text buffer) text
            (buffer-dirty-p buffer) nil
            (buffer-major-mode buffer)
            (let ((type (pathname-type (pathname new-resource-path))))
              (cond
                ((and type (member (string-downcase type) '("lisp" "asd")
                                   :test #'string=))
                 "lisp")
                (type (string-downcase type))
                (t "text"))))
      buffer)))

(defun project-write-file (project-designator path text
                            &key (if-exists :supersede))
  "Write TEXT to project resource PATH and return a summary plist."
  (let* ((project (ensure-project project-designator))
         (resource-path (project-resource-name path))
         (resolved (project-resolve-path project resource-path)))
    (ensure-directories-exist resolved)
    (with-open-file (stream resolved
                            :direction :output
                            :if-exists if-exists
                            :if-does-not-exist :create)
      (write-string text stream))
    (synchronize-open-project-file-buffer project resource-path text)
    (record-project-write-event :write project resource-path
                                :bytes (length text))
    (list :status :ok
          :project (project-name project)
          :path resource-path
          :bytes-written (length text))))

(defun project-save-file (project-designator path text)
  "Save TEXT to project resource PATH, replacing any existing contents."
  (project-write-file project-designator path text :if-exists :supersede))

(defun replace-text-occurrences (text old new &key count)
  "Return TEXT with OLD replaced by NEW up to COUNT times and the replacement count."
  (when (zerop (length old))
    (error "Replacement text OLD cannot be empty."))
  (let ((start 0)
        (replacements 0))
    (values
     (with-output-to-string (out)
       (loop :for position := (and (or (null count)
                                       (< replacements count))
                                   (search old text
                                           :start2 start
                                           :test #'char=))
             :while position
             :do (write-string text out :start start :end position)
                 (write-string new out)
                 (incf replacements)
                 (setf start (+ position (length old)))
             :finally
                (write-string text out :start start)))
     replacements)))

(defun project-replace-text (project-designator path old new
                             &key (count 1))
  "Replace exact OLD text with NEW in PROJECT-DESIGNATOR/PATH."
  (let ((resource-path (project-resource-name path)))
    (multiple-value-bind (new-text replacements)
        (replace-text-occurrences
         (project-read-file project-designator resource-path)
         old
         new
         :count count)
      (unless (plusp replacements)
        (error "Text not found in project file: ~A:~A"
               project-designator
               resource-path))
      (let ((summary (project-save-file project-designator resource-path new-text)))
        (setf (getf summary :replacements) replacements)
        summary))))

(defun project-replace-text-between (project-designator path start-marker
                                      end-marker replacement
                                      &key (include-start-marker t)
                                        (include-end-marker nil))
  "Replace text between START-MARKER and END-MARKER in PROJECT-DESIGNATOR/PATH."
  (when (zerop (length start-marker))
    (error "START-MARKER cannot be empty."))
  (when (zerop (length end-marker))
    (error "END-MARKER cannot be empty."))
  (let* ((resource-path (project-resource-name path))
         (text (project-read-file project-designator resource-path))
         (start (search start-marker text :test #'char=)))
    (unless start
      (error "Start marker not found in project file: ~A:~A"
             project-designator
             resource-path))
    (let* ((after-start (+ start (length start-marker)))
           (end (search end-marker text
                        :start2 after-start
                        :test #'char=)))
      (unless end
        (error "End marker not found in project file after start marker: ~A:~A"
               project-designator
               resource-path))
      (let* ((replace-start (if include-start-marker start after-start))
             (replace-end (if include-end-marker
                              (+ end (length end-marker))
                              end))
             (new-text (concatenate 'string
                                    (subseq text 0 replace-start)
                                    replacement
                                    (subseq text replace-end)))
             (summary (project-save-file project-designator
                                         resource-path
                                         new-text)))
        (setf (getf summary :start-position) replace-start
              (getf summary :end-position) replace-end
              (getf summary :bytes-replaced) (- replace-end replace-start))
        summary))))

(defun stage-project-replace-text (project-designator path old new
                                   &key (count 1) change-set)
  "Stage exact OLD-to-NEW text replacement in PROJECT-DESIGNATOR/PATH."
  (let* ((resource-path (project-resource-name path))
         (target-change-set (ensure-change-set change-set)))
    (multiple-value-bind (new-text replacements)
        (replace-text-occurrences
         (change-set-project-file-text project-designator
                                       resource-path
                                       target-change-set)
         old
         new
         :count count)
      (unless (plusp replacements)
        (error "Text not found in project file: ~A:~A"
               project-designator
               resource-path))
      (let ((entry (stage-project-file project-designator resource-path new-text
                                       :change-set target-change-set)))
        (list :status :staged
              :change-set (change-set-id target-change-set)
              :project (change-set-entry-project-name entry)
              :path (change-set-entry-path entry)
              :replacements replacements
              :bytes-staged (length new-text))))))

(defun project-create-file (project-designator path &key (content "")
                                             (if-exists :error))
  "Create a new project resource PATH containing CONTENT."
  (project-write-file project-designator path content :if-exists if-exists))

;;; --------------------------------------------------------------------------
;;; Transactional change sets
;;; --------------------------------------------------------------------------

(defun next-change-set-id ()
  "Return a fresh human-readable change set id."
  (format nil "change-~D" (incf *change-set-counter*)))

(defun begin-change-set (&key name description)
  "Create and select a new staged project change set."
  (let* ((id (next-change-set-id))
         (change-set (make-change-set :id id
                                      :name (or name id)
                                      :description description
                                      :entries nil
                                      :status :open
                                      :created-at (get-universal-time))))
    (setf (gethash id *change-set-registry*) change-set
          *current-change-set* change-set)
    change-set))

(defun current-change-set ()
  "Return the current staged project change set, or NIL."
  *current-change-set*)

(defun find-change-set (designator)
  "Return the change set named by DESIGNATOR, or NIL."
  (etypecase designator
    (change-set designator)
    (string (gethash designator *change-set-registry*))
    (symbol (gethash (string-downcase (symbol-name designator))
                     *change-set-registry*))))

(defun ensure-change-set (&optional change-set-designator)
  "Return CHANGE-SET-DESIGNATOR, the current change set, or a new change set."
  (or (and change-set-designator
           (or (find-change-set change-set-designator)
               (error "Unknown change set: ~A" change-set-designator)))
      *current-change-set*
      (begin-change-set)))

(defun change-set-open-p (change-set)
  "Return true when CHANGE-SET can still accept staged entries."
  (eq :open (change-set-status change-set)))

(defun ensure-open-change-set (change-set)
  "Signal unless CHANGE-SET is open."
  (unless (change-set-open-p change-set)
    (error "Change set ~A is not open; status is ~A."
           (change-set-id change-set)
           (change-set-status change-set)))
  change-set)

(defun list-change-sets ()
  "Return known change sets sorted by creation time."
  (let (change-sets)
    (maphash (lambda (key change-set)
               (declare (ignore key))
               (push change-set change-sets))
             *change-set-registry*)
    (sort change-sets #'< :key (lambda (change-set)
                                 (or (change-set-created-at change-set) 0)))))

(defun project-file-snapshot (project-designator path)
  "Return values EXISTS-P and TEXT for PROJECT-DESIGNATOR/PATH."
  (let* ((project (ensure-project project-designator))
         (resource-path (project-resource-name path))
         (resolved (project-resolve-path project resource-path)))
    (if (probe-file resolved)
        (values t (uiop:read-file-string (truename resolved)))
        (values nil nil))))

(defun make-staged-write-entry (project path text)
  "Return a staged write entry for PROJECT/PATH."
  (multiple-value-bind (exists-p old-text)
      (project-file-snapshot project path)
    (make-change-set-entry :kind :write
                           :project-name (project-name (ensure-project project))
                           :path (project-resource-name path)
                           :new-text text
                           :old-exists-p exists-p
                           :old-text old-text)))

(defun append-change-set-entry (change-set entry)
  "Append ENTRY to CHANGE-SET and return ENTRY."
  (ensure-open-change-set change-set)
  (setf (change-set-entries change-set)
        (append (change-set-entries change-set) (list entry)))
  entry)

(defun stage-project-file (project-designator path text
                            &key change-set)
  "Stage TEXT as the new contents of PROJECT-DESIGNATOR/PATH."
  (let* ((target-change-set (ensure-change-set change-set))
         (project (ensure-project project-designator))
         (entry (make-staged-write-entry project path text)))
    (append-change-set-entry target-change-set entry)
    entry))

(defun stage-project-delete (project-designator path &key change-set)
  "Stage deletion of PROJECT-DESIGNATOR/PATH."
  (let* ((target-change-set (ensure-change-set change-set))
         (project (ensure-project project-designator))
         (resource-path (project-resource-name path)))
    (multiple-value-bind (exists-p old-text)
        (project-file-snapshot project resource-path)
      (unless exists-p
        (error "Cannot stage delete for missing project file: ~A:~A"
               (project-name project)
               resource-path))
      (append-change-set-entry
       target-change-set
       (make-change-set-entry :kind :delete
                              :project-name (project-name project)
                              :path resource-path
                              :old-exists-p exists-p
                              :old-text old-text)))))

(defun stage-project-rename (project-designator old-path new-path
                              &key change-set)
  "Stage a file rename inside PROJECT-DESIGNATOR."
  (let* ((target-change-set (ensure-change-set change-set))
         (project (ensure-project project-designator))
         (old-resource-path (project-resource-name old-path))
         (new-resource-path (project-resource-name new-path)))
    (multiple-value-bind (source-exists-p source-text)
        (project-file-snapshot project old-resource-path)
      (unless source-exists-p
        (error "Cannot stage rename for missing project file: ~A:~A"
               (project-name project)
               old-resource-path))
      (multiple-value-bind (target-exists-p target-text)
          (project-file-snapshot project new-resource-path)
        (append-change-set-entry
         target-change-set
         (make-change-set-entry :kind :rename
                                :project-name (project-name project)
                                :path old-resource-path
                                :new-path new-resource-path
                                :new-text source-text
                                :old-exists-p source-exists-p
                                :old-text source-text
                                :target-old-exists-p target-exists-p
                                :target-old-text target-text))))))

(defun latest-staged-file-entry (change-set project-designator path)
  "Return the latest staged entry affecting PROJECT-DESIGNATOR/PATH."
  (let* ((project (ensure-project project-designator))
         (project-key (normalize-project-name (project-name project)))
         (resource-path (project-resource-name path)))
    (find-if
     (lambda (entry)
       (and (string= project-key
                     (normalize-project-name
                      (change-set-entry-project-name entry)))
            (or (string= resource-path (change-set-entry-path entry))
                (and (change-set-entry-new-path entry)
                     (string= resource-path
                              (change-set-entry-new-path entry))))))
     (reverse (change-set-entries change-set)))))

(defun change-set-project-file-text (project-designator path
                                      &optional change-set-designator)
  "Return PROJECT-DESIGNATOR/PATH text including the latest staged write."
  (let ((change-set (or (and change-set-designator
                             (find-change-set change-set-designator))
                        *current-change-set*)))
    (when change-set
      (let ((entry (latest-staged-file-entry change-set project-designator path)))
        (when entry
          (case (change-set-entry-kind entry)
            (:write (return-from change-set-project-file-text
                      (change-set-entry-new-text entry)))
            (:rename
             (if (and (change-set-entry-new-path entry)
                      (string= (project-resource-name path)
                               (change-set-entry-new-path entry)))
                 (return-from change-set-project-file-text
                   (change-set-entry-new-text entry))
                 (error "Project file is staged for rename: ~A"
                        (change-set-entry-path entry))))
            (:delete
             (error "Project file is staged for deletion: ~A"
                    (change-set-entry-path entry)))))))
    (project-read-file project-designator path)))

(defun diff-line-list (text)
  "Split TEXT into display lines for change set diffs."
  (if text
      (split-text-lines text)
      nil))

(defun simple-text-diff-to-string (old-text new-text)
  "Return a compact line-oriented diff from OLD-TEXT to NEW-TEXT."
  (with-output-to-string (out)
    (let ((old-lines (diff-line-list old-text))
          (new-lines (diff-line-list new-text)))
      (cond
        ((and (null old-text) new-text)
         (dolist (line new-lines)
           (format out "+~A~%" line)))
        ((and old-text (null new-text))
         (dolist (line old-lines)
           (format out "-~A~%" line)))
        (t
         (let ((old-set (make-hash-table :test #'equal))
               (new-set (make-hash-table :test #'equal)))
           (dolist (line old-lines)
             (setf (gethash line old-set) t))
           (dolist (line new-lines)
             (setf (gethash line new-set) t))
           (dolist (line old-lines)
             (unless (gethash line new-set)
               (format out "-~A~%" line)))
           (dolist (line new-lines)
             (unless (gethash line old-set)
               (format out "+~A~%" line)))))))))

(defun change-set-entry-diff-to-string (entry)
  "Return a display diff for one staged ENTRY."
  (with-output-to-string (out)
    (ecase (change-set-entry-kind entry)
      (:write
       (format out "--- ~A:~A~%+++ ~A:~A~%"
               (change-set-entry-project-name entry)
               (change-set-entry-path entry)
               (change-set-entry-project-name entry)
               (change-set-entry-path entry))
       (write-string
        (simple-text-diff-to-string (change-set-entry-old-text entry)
                                    (change-set-entry-new-text entry))
        out))
      (:delete
       (format out "--- ~A:~A~%+++ /dev/null~%"
               (change-set-entry-project-name entry)
               (change-set-entry-path entry))
       (write-string
        (simple-text-diff-to-string (change-set-entry-old-text entry) nil)
        out))
      (:rename
       (format out "rename ~A:~A -> ~A:~A~%"
               (change-set-entry-project-name entry)
               (change-set-entry-path entry)
               (change-set-entry-project-name entry)
               (change-set-entry-new-path entry))))))

(defun change-set-diff-to-string (&optional change-set-designator)
  "Return an agent-readable diff for CHANGE-SET-DESIGNATOR."
  (let ((change-set (ensure-change-set change-set-designator)))
    (with-output-to-string (out)
      (format out "Change Set: ~A  Status: ~(~A~)~@[  Name: ~A~]~%~%"
              (change-set-id change-set)
              (change-set-status change-set)
              (change-set-name change-set))
      (if (change-set-entries change-set)
          (dolist (entry (change-set-entries change-set))
            (write-string (change-set-entry-diff-to-string entry) out)
            (terpri out))
          (format out "No staged changes.~%")))))

(defun restore-project-file-snapshot (project-designator path exists-p text)
  "Restore PROJECT-DESIGNATOR/PATH to EXISTS-P/TEXT."
  (if exists-p
      (project-save-file project-designator path (or text ""))
      (let ((resolved (project-resolve-path project-designator path)))
        (when (probe-file resolved)
          (delete-file (truename resolved)))
        (remove-open-project-file-buffer project-designator path)))
  t)

(defun apply-change-set-entry (entry)
  "Apply one staged change set ENTRY."
  (ecase (change-set-entry-kind entry)
    (:write
     (project-save-file (change-set-entry-project-name entry)
                        (change-set-entry-path entry)
                        (change-set-entry-new-text entry)))
    (:delete
     (let ((resolved (project-resolve-path (change-set-entry-project-name entry)
                                           (change-set-entry-path entry)
                                           :require-exists t)))
       (delete-file resolved)
       (remove-open-project-file-buffer (change-set-entry-project-name entry)
                                        (change-set-entry-path entry))
       (record-project-write-event :delete
                                   (change-set-entry-project-name entry)
                                   (change-set-entry-path entry))))
    (:rename
     (let ((source (project-resolve-path (change-set-entry-project-name entry)
                                         (change-set-entry-path entry)
                                         :require-exists t))
           (target (project-resolve-path (change-set-entry-project-name entry)
                                         (change-set-entry-new-path entry))))
       (ensure-directories-exist target)
       (remove-open-project-file-buffer (change-set-entry-project-name entry)
                                        (change-set-entry-new-path entry))
       (rename-file source target)
       (retarget-open-project-file-buffer (change-set-entry-project-name entry)
                                          (change-set-entry-path entry)
                                          (change-set-entry-new-path entry)
                                          (change-set-entry-new-text entry))
       (record-project-write-event :rename
                                   (change-set-entry-project-name entry)
                                   (change-set-entry-path entry)
                                   :new-path
                                   (change-set-entry-new-path entry)))))
  (setf (change-set-entry-applied-p entry) t)
  entry)

(defun revert-change-set-entry (entry)
  "Restore the pre-apply state for one ENTRY."
  (ecase (change-set-entry-kind entry)
    (:write
     (restore-project-file-snapshot (change-set-entry-project-name entry)
                                    (change-set-entry-path entry)
                                    (change-set-entry-old-exists-p entry)
                                    (change-set-entry-old-text entry)))
    (:delete
     (restore-project-file-snapshot (change-set-entry-project-name entry)
                                    (change-set-entry-path entry)
                                    (change-set-entry-old-exists-p entry)
                                    (change-set-entry-old-text entry)))
    (:rename
     (restore-project-file-snapshot (change-set-entry-project-name entry)
                                    (change-set-entry-new-path entry)
                                    (change-set-entry-target-old-exists-p entry)
                                    (change-set-entry-target-old-text entry))
     (restore-project-file-snapshot (change-set-entry-project-name entry)
                                    (change-set-entry-path entry)
                                    (change-set-entry-old-exists-p entry)
                                    (change-set-entry-old-text entry))))
  (setf (change-set-entry-applied-p entry) nil)
  entry)

(defun apply-change-set (&optional change-set-designator)
  "Apply CHANGE-SET-DESIGNATOR, rolling back already applied entries on error."
  (let ((change-set (ensure-change-set change-set-designator))
        (applied nil))
    (ensure-open-change-set change-set)
    (handler-case
        (progn
          (dolist (entry (change-set-entries change-set))
            (apply-change-set-entry entry)
            (push entry applied))
          (setf (change-set-status change-set) :applied
                (change-set-applied-at change-set) (get-universal-time))
          change-set)
      (error (condition)
        (dolist (entry applied)
          (ignore-errors (revert-change-set-entry entry)))
        (setf (change-set-status change-set) :failed)
        (error "Failed applying change set ~A; rolled back applied entries: ~A"
               (change-set-id change-set)
               condition)))))

(defun discard-change-set (&optional change-set-designator)
  "Discard an unapplied staged change set."
  (let ((change-set (ensure-change-set change-set-designator)))
    (when (eq :applied (change-set-status change-set))
      (error "Cannot discard applied change set ~A; use REVERT-CHANGE-SET."
             (change-set-id change-set)))
    (setf (change-set-status change-set) :discarded)
    (when (eq change-set *current-change-set*)
      (setf *current-change-set* nil))
    change-set))

(defun revert-change-set (&optional change-set-designator)
  "Revert an applied CHANGE-SET-DESIGNATOR using stored snapshots."
  (let ((change-set (ensure-change-set change-set-designator)))
    (unless (eq :applied (change-set-status change-set))
      (error "Change set ~A is not applied; status is ~A."
             (change-set-id change-set)
             (change-set-status change-set)))
    (dolist (entry (reverse (change-set-entries change-set)))
      (revert-change-set-entry entry))
    (setf (change-set-status change-set) :reverted)
    (when (eq change-set *current-change-set*)
      (setf *current-change-set* nil))
    change-set))

(defun change-set-summary-to-string (&optional change-set-designator)
  "Return a compact summary of staged entries in CHANGE-SET-DESIGNATOR."
  (let ((change-set (ensure-change-set change-set-designator)))
    (with-output-to-string (out)
      (format out "~A (~(~A~)): ~D entr~:@P~%"
              (change-set-id change-set)
              (change-set-status change-set)
              (length (change-set-entries change-set)))
      (dolist (entry (change-set-entries change-set))
        (ecase (change-set-entry-kind entry)
          (:write
           (format out "  write ~A:~A~%"
                   (change-set-entry-project-name entry)
                   (change-set-entry-path entry)))
          (:delete
           (format out "  delete ~A:~A~%"
                   (change-set-entry-project-name entry)
                   (change-set-entry-path entry)))
          (:rename
           (format out "  rename ~A:~A -> ~A~%"
                   (change-set-entry-project-name entry)
                   (change-set-entry-path entry)
                   (change-set-entry-new-path entry))))))))

(defun split-text-lines (text)
  "Split TEXT into lines without retaining newline characters."
  (loop :for start := 0 :then (1+ pos)
        :for pos := (position #\Newline text :start start)
        :collect (subseq text start (or pos (length text)))
        :while pos))

(defun bounded-project-line-range (line-count start end max-lines)
  "Return a sane 1-based inclusive line range for LINE-COUNT."
  (let* ((safe-start (max 1 (or start 1)))
         (safe-end (or end
                       (if max-lines
                           (+ safe-start max-lines -1)
                           line-count)))
         (bounded-start (min safe-start (max 1 line-count)))
         (bounded-end (max bounded-start (min safe-end line-count))))
    (when (and max-lines
               (> (1+ (- bounded-end bounded-start)) max-lines))
      (setf bounded-end (+ bounded-start max-lines -1)))
    (values bounded-start (min bounded-end line-count))))

(defun normalize-project-read-file-lines-args (args)
  "Normalize PROJECT-READ-FILE-LINES ARGS to a keyword plist."
  (cond
    ((or (null args)
         (keywordp (first args)))
     args)
    ((numberp (first args))
     (list* :start (first args)
            :end (second args)
            (cddr args)))
    (t
     (error "PROJECT-READ-FILE-LINES arguments must be keywords or positional START END, got ~S."
            args))))

(defun project-read-file-lines (project-designator path &rest args)
  "Read a line-numbered slice of a project resource as text."
  (destructuring-bind (&key line start end (context 20) (max-lines 120))
      (normalize-project-read-file-lines-args args)
    (let* ((resource-path (project-resource-name path))
           (lines (split-text-lines
                   (project-read-file project-designator resource-path)))
           (line-count (length lines))
           (range-start (if line
                            (max 1 (- line context))
                            start))
           (range-end (if line
                          (+ line context)
                          end)))
      (multiple-value-bind (bounded-start bounded-end)
          (bounded-project-line-range line-count range-start range-end max-lines)
        (with-output-to-string (out)
          (format out "~A: lines ~D-~D of ~D~%"
                  resource-path
                  bounded-start
                  bounded-end
                  line-count)
          (loop :for text :in lines
                :for line-number :from 1
                :when (and (>= line-number bounded-start)
                           (<= line-number bounded-end))
                  :do (format out "~D: ~A~%" line-number text)))))))

(defun project-search (project-designator query &key
                          (limit *project-search-result-limit*)
                          (case-sensitive nil)
                          include-ignored
                          include-bulk)
  "Search PROJECT-DESIGNATOR text files for QUERY.
Returns plists containing :PATH, :LINE, and :TEXT."
  (let* ((project (ensure-project project-designator))
         (needle (if case-sensitive query (string-downcase query)))
         (results nil))
    (dolist (path (project-list-files project
                                      :limit nil
                                      :include-ignored include-ignored
                                      :include-bulk include-bulk))
      (when (or (null limit) (< (length results) limit))
        (handler-case
            (let ((text (project-read-file project path)))
              (loop :for line :in (split-text-lines text)
                    :for line-number :from 1
                    :for haystack := (if case-sensitive
                                         line
                                         (string-downcase line))
                    :when (search needle haystack)
                      :do (push (list :path path
                                      :line line-number
                                      :text line)
                                results)
                    :when (and limit (>= (length results) limit))
                      :do (return)))
          (error () nil))))
    (nreverse results)))

(defun project-search-to-string (project-designator query &rest args
                                  &key (limit *project-search-result-limit*)
                                       case-sensitive
                                       include-ignored include-bulk)
  "Return PROJECT-SEARCH results as an agent-readable string."
  (declare (ignore case-sensitive include-ignored include-bulk))
  (let ((results (apply #'project-search project-designator query args)))
    (if results
        (with-output-to-string (out)
          (dolist (result results)
            (format out "~A:~D: ~A~%"
                    (getf result :path)
                    (getf result :line)
                    (getf result :text)))
          (when (and limit (= (length results) limit))
            (format out ";;; limited to ~D matches; pass :LIMIT NIL or a larger value if needed.~%"
                    limit)))
        "No matches.")))

(defun file-buffer-name (project path)
  "Return a display buffer name for PROJECT/PATH."
  (format nil "~A:~A" (project-name project) path))

(defun file-major-mode-for-path (path)
  "Return a simple major-mode label for PATH."
  (let ((type (pathname-type (pathname path))))
    (cond
      ((and type (member (string-downcase type) '("lisp" "asd")
                         :test #'string=))
       "lisp")
      (type (string-downcase type))
      (t "text"))))

(defun find-project-file-buffer (project-designator path)
  "Return an open file buffer for PROJECT-DESIGNATOR/PATH, if present."
  (let* ((project (ensure-project project-designator))
         (resource-path (project-resource-name path)))
    (find-if (lambda (buffer)
               (and (file-buffer-p buffer)
                    (string= (or (buffer-project-name buffer) "")
                             (project-name project))
                    (string= (or (buffer-resource-path buffer) "")
                             resource-path)))
             *buffer-ring*)))

(defun project-open-file (project-designator path)
  "Open PROJECT-DESIGNATOR/PATH as an editable Clawmacs file buffer."
  (let* ((project (ensure-project project-designator))
         (resource-path (project-resource-name path))
         (existing (find-project-file-buffer project resource-path)))
    (if existing
        (switch-to-buffer existing)
        (let* ((text (project-read-file project resource-path))
               (buffer (make-buffer (file-buffer-name project resource-path)
                                    :agent-name "file"
                                    :kind :file
                                    :working-directory (project-root project)
                                    :project-name (project-name project)
                                    :resource-path resource-path
                                    :original-text text
                                    :dirty-p nil)))
          (set-message-text (buffer-input-message buffer) text)
          (setf (buffer-major-mode buffer) (file-major-mode-for-path resource-path))
          (unless *default-keymap*
            (init-default-keymap))
          (setf (buffer-keymap buffer) (or *scratch-keymap* *default-keymap*))
          (add-buffer-to-ring buffer)
          (switch-to-buffer buffer)))))

(defun project-save-buffer (&optional (buffer (current-buffer)))
  "Save a project-backed file BUFFER to its project resource."
  (unless buffer
    (error "No current buffer."))
  (unless (file-buffer-p buffer)
    (error "Not a project file buffer: ~A" (buffer-name buffer)))
  (let* ((project (ensure-project (buffer-project-name buffer)))
         (path (buffer-resource-path buffer))
         (text (file-buffer-text buffer))
         (summary (project-save-file project path text)))
    (setf (buffer-original-text buffer) text
          (buffer-dirty-p buffer) nil)
    summary))

;;; --------------------------------------------------------------------------
;;; Project checks, reload, and code intelligence
;;; --------------------------------------------------------------------------

(defun call-project-function (function-designator project)
  "Call FUNCTION-DESIGNATOR with PROJECT."
  (funcall (etypecase function-designator
             (function function-designator)
             (symbol (symbol-function function-designator)))
           project))

(defun run-project-checks (project-designator)
  "Run check callbacks registered on PROJECT-DESIGNATOR.
Each check function is called with the project object and its result is
captured in a plist. Conditions are caught so agents can report all failures."
  (let* ((project (ensure-project project-designator))
         (checks (project-check-functions project)))
    (if checks
        (loop :for check :in checks
              :collect
              (handler-case
                  (list :check check
                        :status :passed
                        :result (call-project-function check project))
                (error (condition)
                  (list :check check
                        :status :failed
                        :condition (format nil "~A" condition)))))
        (list (list :project (project-name project)
                    :status :no-checks
                    :message "No project check functions are registered.")))))

(defun compile-project-file (project-designator path)
  "Compile PROJECT-DESIGNATOR/PATH with COMPILE-FILE and return a summary plist."
  (let ((resolved (project-resolve-path project-designator path
                                        :require-exists t)))
    (multiple-value-bind (output-path warnings-p failure-p)
        (compile-file resolved)
      (list :project (project-name (ensure-project project-designator))
            :path (project-resource-name path)
            :output-path output-path
            :warnings-p warnings-p
            :failure-p failure-p
            :status (if failure-p :failed :ok)))))

(defun load-project-file (project-designator path)
  "Load PROJECT-DESIGNATOR/PATH and return a summary plist."
  (let ((resolved (project-resolve-path project-designator path
                                        :require-exists t)))
    (load resolved :verbose nil :print nil)
    (list :project (project-name (ensure-project project-designator))
          :path (project-resource-name path)
          :status :ok)))

(defun reload-project-system (project-designator &optional system-designator)
  "Reload PROJECT-DESIGNATOR's registered systems or SYSTEM-DESIGNATOR."
  (let* ((project (ensure-project project-designator))
         (reload-function (project-reload-function project))
         (systems (or (and system-designator (list system-designator))
                      (project-systems project))))
    (cond
      (reload-function
       (list (list :project (project-name project)
                   :status :ok
                   :result (call-project-function reload-function project))))
      (systems
       (loop :for system :in systems
             :collect
             (handler-case
                 (progn
                   (asdf:load-system system)
                   (list :system system :status :ok))
               (error (condition)
                 (list :system system
                       :status :failed
                       :condition (format nil "~A" condition))))))
      (t
       (list (list :project (project-name project)
                   :status :no-systems
                   :message "No project systems or reload function are registered."))))))

(defun project-lisp-source-path-p (path)
  "Return true when PATH names a Lisp source resource."
  (let ((type (string-downcase (or (pathname-type (pathname path)) ""))))
    (member type '("lisp" "lsp" "cl" "asd") :test #'string=)))

(defun keyword-option-list (&rest pairs)
  "Return PAIRS without keyword/value pairs whose value is NIL."
  (loop :for (key value) :on pairs :by #'cddr
        :when value
          :append (list key value)))

(defun project-sexed-outline (project path options)
  "Call SEXED-OUTLINE-TO-STRING for PROJECT/PATH with OPTIONS."
  (apply (symbol-function 'sexed-outline-to-string)
         (project-read-file project path)
         options))

(defun project-outline-to-string (project-designator &key path depth max-depth
                                                     head limit
                                                     (preview-chars 96)
                                                     (file-limit *project-outline-file-limit*)
                                                     include-ignored
                                                     include-bulk)
  "Return a structural outline for one Lisp project resource or all Lisp files."
  (let* ((project (ensure-project project-designator))
         (options (keyword-option-list :depth depth
                                       :max-depth max-depth
                                       :head head
                                       :limit limit
                                       :preview-chars preview-chars))
         (paths (if path
                    (list (project-resource-name path))
                    (let ((source-paths
                            (remove-if-not
                             #'project-lisp-source-path-p
                             (project-list-files
                              project
                              :limit nil
                              :include-ignored include-ignored
                              :include-bulk include-bulk))))
                      (if file-limit
                          (subseq source-paths
                                  0
                                  (min file-limit (length source-paths)))
                          source-paths)))))
    (with-output-to-string (out)
      (if paths
          (dolist (resource-path paths)
            (format out ";;; ~A~%" resource-path)
            (handler-case
                (write-string (project-sexed-outline project resource-path options)
                              out)
              (error (condition)
                (format out "Error: ~A~%" condition)))
            (terpri out))
          (format out "No Lisp source files found.~%")))))

(defun definition-head-p (head)
  "Return true when HEAD names a common Lisp definition form."
  (and head
       (member (string-downcase head)
               '("defun" "defmacro" "defgeneric" "defmethod" "defclass"
                 "defstruct" "defvar" "defparameter" "defconstant"
                 "defcommand" "defdoc" "defpackage")
               :test #'string=)))

(defun project-find-definitions (project-designator &key name head
                                                    (limit 50))
  "Return definition plists found in PROJECT-DESIGNATOR."
  (let ((project (ensure-project project-designator))
        (results nil))
    (dolist (path (remove-if-not #'project-lisp-source-path-p
                                 (project-list-files project :limit nil)))
      (when (or (null limit) (< (length results) limit))
        (handler-case
            (dolist (form (funcall (symbol-function 'sexed-find-forms)
                                   (project-read-file project path)
                                   :max-depth 0
                                   :limit nil))
              (let ((form-head (getf form :head))
                    (form-name (getf form :name)))
                (when (and (definition-head-p form-head)
                           (or (null head)
                               (string-equal (project-designator-string head)
                                             form-head))
                           (or (null name)
                               (and form-name
                                    (string-equal
                                     (project-designator-string name)
                                     form-name))))
                  (push (append (list :path path) form) results)
                  (when (and limit (>= (length results) limit))
                    (return)))))
          (error () nil))))
    (nreverse results)))

(defun project-find-definitions-to-string (project-designator &rest args
                                            &key name head limit)
  "Return PROJECT-FIND-DEFINITIONS results as text."
  (declare (ignore name head limit))
  (let ((definitions (apply #'project-find-definitions project-designator args)))
    (if definitions
        (with-output-to-string (out)
          (dolist (definition definitions)
            (format out "~A:~A ~@[~A~] ~(~A~) [id ~A]~%  ~A~%"
                    (getf definition :path)
                    (getf definition :head)
                    (getf definition :name)
                    (getf definition :type)
                    (getf definition :id)
                    (getf definition :preview))))
        "No definitions found.")))

(defun project-find-references-to-string (project-designator query
                                           &key (limit *project-search-result-limit*))
  "Return text search hits for QUERY in PROJECT-DESIGNATOR."
  (project-search-to-string project-designator query :limit limit))

(defun project-package-map-to-string (project-designator)
  "Return package-related forms found in PROJECT-DESIGNATOR."
  (let ((project (ensure-project project-designator))
        (hits nil))
    (dolist (path (remove-if-not #'project-lisp-source-path-p
                                 (project-list-files project :limit nil)))
      (handler-case
          (dolist (form (funcall (symbol-function 'sexed-find-forms)
                                 (project-read-file project path)
                                 :max-depth 0
                                 :limit nil))
            (when (member (string-downcase (or (getf form :head) ""))
                          '("defpackage" "in-package")
                          :test #'string=)
              (push (append (list :path path) form) hits)))
        (error () nil)))
    (if hits
        (with-output-to-string (out)
          (dolist (hit (nreverse hits))
            (format out "~A: ~A ~@[~A~]~%  ~A~%"
                    (getf hit :path)
                    (getf hit :head)
                    (getf hit :name)
                    (getf hit :preview))))
        "No package forms found.")))

(defun project-describe-definition-to-string (project-designator name
                                               &key head)
  "Return the source text and location for the first matching definition."
  (let* ((project (ensure-project project-designator))
         (definition (first (project-find-definitions project
                                                      :name name
                                                      :head head
                                                      :limit 1))))
    (if definition
        (let* ((path (getf definition :path))
               (selector (list :id (getf definition :id)))
               (text (funcall (symbol-function 'sexed-form-text)
                              (project-read-file project path)
                              selector)))
          (format nil "~A:~A ~A [id ~A]~%~%~A"
                  path
                  (getf definition :head)
                  (getf definition :name)
                  (getf definition :id)
                  text))
        (format nil "No definition found for ~A in project ~A."
                name
                (project-name project)))))
