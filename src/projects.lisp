(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Project/resource abstraction
;;; --------------------------------------------------------------------------

(defstruct project
  "A named collection of resources backed by an implementation-specific store."
  name
  root
  description
  source)

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
  '(".git" ".hg" ".svn" ".cache" ".direnv" "node_modules" "target")
  "Directory names ignored by project listing and search.")

(defvar *project-list-file-limit* 5000
  "Default maximum number of files returned by PROJECT-LIST-FILES.")

(defvar *project-search-result-limit* 100
  "Default maximum number of matches returned by PROJECT-SEARCH.")

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
        :description (or (project-description project) "")))

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

(defun define-project (name &key root description (create-if-missing nil)
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
                 :source source)
   :replace replace))

(defun create-project (name &key root description (persist t)
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

(defun ignored-project-directory-p (directory)
  "Return true when DIRECTORY should be skipped during project traversal."
  (let* ((components (pathname-directory (uiop:ensure-directory-pathname directory)))
         (name (car (last components))))
    (member name *project-ignored-directory-names* :test #'string=)))

(defun project-files-recursively (project)
  "Return all non-ignored files under PROJECT."
  (labels ((walk (directory)
             (unless (ignored-project-directory-p directory)
               (nconc (uiop:directory-files directory)
                      (loop :for child :in (uiop:subdirectories directory)
                            :unless (ignored-project-directory-p child)
                              :nconc (walk child))))))
    (walk (project-root project))))

(defun project-list-files (project-designator &key
                              (limit *project-list-file-limit*))
  "Return sorted project-relative file paths for PROJECT-DESIGNATOR."
  (let* ((project (ensure-project project-designator))
         (files (sort (mapcar (lambda (path)
                                (project-relative-namestring project path))
                              (project-files-recursively project))
                      #'string<)))
    (if limit
        (subseq files 0 (min limit (length files)))
        files)))

(defun project-read-file (project-designator path)
  "Read a project resource as text."
  (uiop:read-file-string
   (project-resolve-path project-designator path :require-exists t)))

(defun project-write-file (project-designator path text
                            &key (if-exists :supersede))
  "Write TEXT to project resource PATH and return a summary plist."
  (let ((resolved (project-resolve-path project-designator path)))
    (ensure-directories-exist resolved)
    (with-open-file (stream resolved
                            :direction :output
                            :if-exists if-exists
                            :if-does-not-exist :create)
      (write-string text stream))
    (list :status :ok
          :project (project-name (ensure-project project-designator))
          :path (project-resource-name path)
          :bytes-written (length text))))

(defun project-save-file (project-designator path text)
  "Save TEXT to project resource PATH, replacing any existing contents."
  (project-write-file project-designator path text :if-exists :supersede))

(defun project-create-file (project-designator path &key (content "")
                                             (if-exists :error))
  "Create a new project resource PATH containing CONTENT."
  (project-write-file project-designator path content :if-exists if-exists))

(defun split-text-lines (text)
  "Split TEXT into lines without retaining newline characters."
  (loop :for start := 0 :then (1+ pos)
        :for pos := (position #\Newline text :start start)
        :collect (subseq text start (or pos (length text)))
        :while pos))

(defun project-search (project-designator query &key
                          (limit *project-search-result-limit*)
                          (case-sensitive nil))
  "Search PROJECT-DESIGNATOR text files for QUERY.
Returns plists containing :PATH, :LINE, and :TEXT."
  (let* ((project (ensure-project project-designator))
         (needle (if case-sensitive query (string-downcase query)))
         (results nil))
    (dolist (path (project-list-files project :limit nil))
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
                                  &key limit case-sensitive)
  "Return PROJECT-SEARCH results as an agent-readable string."
  (declare (ignore limit case-sensitive))
  (let ((results (apply #'project-search project-designator query args)))
    (if results
        (with-output-to-string (out)
          (dolist (result results)
            (format out "~A:~D: ~A~%"
                    (getf result :path)
                    (getf result :line)
                    (getf result :text))))
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
