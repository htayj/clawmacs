(in-package :rplaca)

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
  packages
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

(defstruct (project-buffer-effect
            (:constructor make-project-buffer-effect
                (operation project-name path
                 &key new-path text dirty-p original-text)))
  "Immutable frame-process synchronization after a project file mutation."
  (operation :synchronize :type keyword :read-only t)
  (project-name "" :type string :read-only t)
  (path "" :type string :read-only t)
  (new-path nil :type (or null string) :read-only t)
  (text nil :type (or null string) :read-only t)
  (dirty-p nil :type boolean :read-only t)
  (original-text nil :type (or null string) :read-only t))

;; TOOLS.LISP initializes the process value later in serial ASDF order.
(defvar *tool-effect-recorder*)

(defvar *project-buffer-effect-collector* nil
  "Transaction-local callback collecting project buffer effects until commit.")

(defparameter +default-project-definitions-directory+
  (merge-pathnames #P".rplaca.projects.d/" (user-homedir-pathname))
  "Canonical directory containing project definition manifests.")

(defparameter +legacy-project-definitions-directory+
  (merge-pathnames #P".clawmacs.projects.d/" (user-homedir-pathname))
  "Legacy behavioral project registry, never loaded automatically.")

(defvar *project-definitions-directory*
  +default-project-definitions-directory+
  "Directory containing inert project definition manifests.")

(defvar *project-registry* (make-hash-table :test #'equal)
  "Project registry keyed by normalized project name.")

(defvar *process-project-registry* *project-registry*
  "Process-global project registry, distinct from dynamic test bindings.")

(defvar *project-registry-lock*
  (bt:make-lock "rplaca project registry")
  "Lock guarding bounded access to the process-global project registry.")

(defun call-with-project-registry-lock
    (function &optional (table *project-registry*))
  "Call FUNCTION under the project lock when TABLE is process-global.

Manifest I/O, package installation, filesystem traversal, and project callback
execution must happen after the lock has been released."
  (if (eq table *process-project-registry*)
      (bt:with-lock-held (*project-registry-lock*)
        (funcall function))
      (funcall function)))

(defun project-registry-snapshot (&optional (table *project-registry*))
  "Return a stable alist snapshot of project TABLE."
  (call-with-project-registry-lock
   (lambda ()
     (let ((entries nil))
       (maphash (lambda (name project)
                  (push (cons name project) entries))
                table)
       entries))
   table))

(defvar *project-definitions-loaded-p* nil
  "True after project manifests have been loaded for this process.")

(defun project-definitions-loaded-p ()
  "Return a synchronized snapshot of project-definition load state."
  (call-with-project-registry-lock
   (lambda () *project-definitions-loaded-p*)
   *project-registry*))

(defun set-project-definitions-loaded-p (value)
  "Publish project-definition load state VALUE."
  (call-with-project-registry-lock
   (lambda ()
     (setf *project-definitions-loaded-p* (not (null value))))
   *project-registry*))

(defvar *project-manifest-extension* "project"
  "Extension used for inert project definition manifests.")

(defvar *project-manifest-serialization-lock*
  (bt:make-lock "rplaca project manifest serialization")
  "Lock guarding bounded ownership changes for manifest writers.")

(defvar *project-manifest-serialization-condition*
  (bt:make-condition-variable :name "rplaca project manifest serialization")
  "Condition signaled when the logical manifest writer releases ownership.")

(defvar *project-manifest-serialization-owner* nil
  "Exact token for the active logical manifest writer.")

(defvar *project-manifest-serialization-token* nil
  "Dynamically bound token permitting reentrant manifest helper calls.")

(defvar *project-manifest-write-function* nil
  "Optional test override called with a temporary path and manifest plist.")

(defun call-with-project-manifest-serialization (function)
  "Serialize manifest replacement without holding a mutex across file I/O."
  (if *project-manifest-serialization-token*
      (funcall function)
      (let ((token (list :project-manifest-writer)))
        (bt:with-lock-held (*project-manifest-serialization-lock*)
          (loop :while *project-manifest-serialization-owner*
                :do (bt:condition-wait
                     *project-manifest-serialization-condition*
                     *project-manifest-serialization-lock*
                     :timeout 0.1))
          (setf *project-manifest-serialization-owner* token))
        (let ((*project-manifest-serialization-token* token))
          (unwind-protect
               (funcall function)
            (bt:with-lock-held (*project-manifest-serialization-lock*)
              (when (eq token *project-manifest-serialization-owner*)
                (setf *project-manifest-serialization-owner* nil)
                #+sbcl
                (sb-thread:condition-broadcast
                 *project-manifest-serialization-condition*)
                #-sbcl
                (bt:condition-notify
                 *project-manifest-serialization-condition*))))))))

(defvar *project-ignored-directory-names*
  '(".git" ".hg" ".svn" ".cache" ".direnv" "node_modules" "target")
  "Directory names ignored by project listing and search.")

(defvar *project-list-file-limit* 5000
  "Default maximum number of files returned by PROJECT-LIST-FILES.")

(defvar *project-search-result-limit* 100
  "Default maximum number of matches returned by PROJECT-SEARCH.")

(defvar *change-set-registry* (make-hash-table :test #'equal)
  "Registry of staged project change sets keyed by id.")

(defvar *process-change-set-registry* *change-set-registry*
  "Process-global change-set registry, distinct from dynamic test bindings.")

(defvar *change-set-registry-lock*
  (bt:make-lock "rplaca change set registry")
  "Lock guarding change-set registration and mutable change-set state.")

(defstruct (change-set-transaction-state
            (:constructor make-change-set-transaction-state
                (&key owner condition)))
  "Logical filesystem transaction state protected by the registry lock."
  owner
  (condition (bt:make-condition-variable
              :name "rplaca change set transaction")))

(defvar *change-set-transaction-state*
  (make-change-set-transaction-state)
  "Logical owner state serializing process-global change-set file operations.")

(defvar *process-change-set-transaction-state* *change-set-transaction-state*
  "Process-global transaction state, distinct from dynamic test bindings.")

(defvar *change-set-transaction-token* nil
  "Dynamically bound exact owner token for a change-set transaction.")

(defvar *change-set-entry-apply-function* nil
  "Optional test override for applying one detached change-set entry.")

(defvar *change-set-entry-revert-function* nil
  "Optional test override for reverting one detached change-set entry.")

(defun call-with-change-set-registry-lock
    (function &optional (table *change-set-registry*))
  "Call FUNCTION under the change-set lock when TABLE is process-global.

Filesystem reads, writes, project callbacks, and buffer synchronization must
run after this lock is released."
  (if (eq table *process-change-set-registry*)
      (bt:with-lock-held (*change-set-registry-lock*)
        (funcall function))
      (funcall function)))

(defun process-change-set-transaction-p ()
  "Return true when the active registry and transaction state are global."
  (and (eq *change-set-registry* *process-change-set-registry*)
       (eq *change-set-transaction-state*
           *process-change-set-transaction-state*)))

(defun call-with-change-set-transaction (function)
  "Serialize process-global apply/revert transactions without locking over I/O."
  (cond
    ((not (process-change-set-transaction-p))
     (funcall function))
    (*change-set-transaction-token*
     (funcall function))
    (t
     (let ((token (list :change-set-transaction))
           (state *change-set-transaction-state*))
       (bt:with-lock-held (*change-set-registry-lock*)
         (loop :while (change-set-transaction-state-owner state)
               :do (bt:condition-wait
                    (change-set-transaction-state-condition state)
                    *change-set-registry-lock*
                    :timeout 0.1))
         (setf (change-set-transaction-state-owner state) token))
       (let ((*change-set-transaction-token* token))
         (unwind-protect
              (funcall function)
           (bt:with-lock-held (*change-set-registry-lock*)
             (when (eq token (change-set-transaction-state-owner state))
               (setf (change-set-transaction-state-owner state) nil)
               #+sbcl
               (sb-thread:condition-broadcast
                (change-set-transaction-state-condition state))
               #-sbcl
               (bt:condition-notify
                (change-set-transaction-state-condition state))))))))))

(defun copy-change-set-snapshot (change-set)
  "Return a detached snapshot of mutable CHANGE-SET metadata."
  (make-change-set
   :id (change-set-id change-set)
   :name (change-set-name change-set)
   :description (change-set-description change-set)
   :entries (mapcar #'copy-change-set-entry (change-set-entries change-set))
   :status (change-set-status change-set)
   :created-at (change-set-created-at change-set)
   :applied-at (change-set-applied-at change-set)))

(defun change-set-registry-snapshot (&optional (table *change-set-registry*))
  "Return a stable alist snapshot of change-set TABLE."
  (call-with-change-set-registry-lock
   (lambda ()
     (let ((entries nil))
       (maphash (lambda (id change-set)
                  (push (cons id (copy-change-set-snapshot change-set)) entries))
                table)
       entries))
   table))

(defvar *current-change-set* nil
  "The current change set used by staging helpers when none is supplied.")

(defvar *change-set-counter* 0
  "Monotonic counter used to create human-readable change set ids.")

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

(defun normalize-project-package-install-request (value)
  "Normalize VALUE to a project package install request plist."
  (cond
    ((null value) nil)
    ((and (listp value) (evenp (length value)))
     (let ((name (getf value :name)))
       (when name
         (list :name (project-display-name name)
               :src-type (rplaca::normalize-package-source-type
                          (getf value :src-type))
               :repo (getf value :repo)
               :path (getf value :path)
               :source (getf value :source)
               :ref (getf value :ref)
               :scope (or (getf value :scope) :project)
               :resource-types
               (rplaca::normalize-package-resource-type-list
                (getf value :resource-types))
               :description (getf value :description)))))
    ((or (stringp value) (symbolp value))
     (list :name (project-display-name value)
           :scope :project))
    (t nil)))

(defun normalize-project-package-install-request-list (packages)
  "Normalize PACKAGES to a list of project package install request plists."
  (loop :for package :in (or packages '())
        :for request := (normalize-project-package-install-request package)
        :when request
          :collect request))

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
        :systems (or (project-systems project) '())
        :packages (or (project-packages project) '())))

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

(defun project-manifest-temporary-path (path)
  "Return a fresh same-directory temporary pathname for manifest PATH."
  (let ((directory (uiop:pathname-directory-pathname path)))
    (loop
      :for candidate =
        (merge-pathnames
         (make-pathname
          :name (format nil ".~A-~36R"
                        (or (pathname-name path) "project")
                        (random (expt 36 10)))
          :type "tmp")
         directory)
      :unless (probe-file candidate)
        :return candidate)))

(defun write-project-manifest-generation (path manifest)
  "Write detached MANIFEST data to temporary PATH."
  (with-open-file (stream path
                          :direction :output
                          :if-exists :error
                          :if-does-not-exist :create)
    (write manifest
           :stream stream
           :pretty t
           :case :downcase)
    (terpri stream))
  path)

(defun write-project-manifest (project &optional
                                       (directory *project-definitions-directory*))
  "Atomically persist PROJECT as an inert manifest and return its path."
  ;; Snapshot the mutable project before entering the logical file transaction.
  (let ((path (project-manifest-path (project-name project) directory))
        (manifest (copy-tree (project->manifest-plist project))))
    (call-with-project-manifest-serialization
     (lambda ()
       (ensure-directories-exist path)
       (let ((temporary (project-manifest-temporary-path path)))
         (unwind-protect
              (progn
                (funcall (or *project-manifest-write-function*
                             #'write-project-manifest-generation)
                         temporary manifest)
                (uiop:rename-file-overwriting-target temporary path)
                path)
           (when (probe-file temporary)
             (ignore-errors (delete-file temporary)))))))))

(defun register-project (project &key (replace t))
  "Register PROJECT and return it.
When REPLACE is NIL, an existing project with the same name is preserved."
  (let ((key (normalize-project-name (project-name project))))
    ;; Root normalization may touch the filesystem and therefore precedes the
    ;; bounded publication critical section.
    (setf (project-name project) (project-display-name (project-name project))
          (project-root project) (normalize-project-root (project-root project)))
    (call-with-project-registry-lock
     (lambda ()
       (let ((existing (gethash key *project-registry*)))
         (if (and existing (not replace))
             existing
             (setf (gethash key *project-registry*) project))))
     *project-registry*)))

(defun define-project (name &key root description systems check-functions
                              packages
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
                 :packages (normalize-project-package-install-request-list
                            packages)
                 :check-functions (normalize-project-function-list
                                   check-functions)
                 :reload-function reload-function)
   :replace replace))

(defun create-project (name &key root description systems packages check-functions
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
                    :packages packages
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
  (let ((key (normalize-project-name name)))
    (call-with-project-registry-lock
     (lambda () (gethash key *project-registry*))
     *project-registry*)))

(defun ensure-project (project-designator)
  "Resolve PROJECT-DESIGNATOR to a project object."
  (etypecase project-designator
    (project project-designator)
    ((or string symbol)
     (or (find-project project-designator)
         (error "Unknown project: ~A" project-designator)))))

(defun list-projects ()
  "Return registered projects sorted by name."
  (sort (mapcar #'cdr (project-registry-snapshot))
        #'string< :key #'project-name))

(defun load-project-manifest (path &key (replace nil))
  "Load one inert project manifest from PATH."
  (let ((manifest (read-project-manifest path)))
    (define-project (getf manifest :name)
      :root (getf manifest :root)
      :description (getf manifest :description)
      :systems (getf manifest :systems)
      :packages (getf manifest :packages)
      :source :manifest
      :replace replace)))

(defun project-package-installed-p (project request)
  "Return true when REQUEST's package is already installed in its scope."
  (let* ((name (getf request :name))
         (scope (or (getf request :scope) :project))
         (definition (find-installed-package
                      name
                      :project (and (eq scope :project) project))))
    (and definition
         (or (and (eq scope :project)
                  (eq :project (package-definition-source-tier definition)))
             (and (eq scope :global)
                  (member (package-definition-source-tier definition)
                          '(:builtin :channel :third-party)
                          :test #'eq))))))

(defun project-package-install-request-valid-p (request)
  "Return true when REQUEST includes enough information to install."
  (and (getf request :name)
       (or (getf request :repo)
           (getf request :path)
           (getf request :source))))

(defun project-package-install-request->use-package-args (request)
  "Return keyword arguments for RPLACA-USE-PACKAGE from REQUEST."
  (let ((src-type (or (getf request :src-type)
                      (getf request :source-type)
                      :git))
        (repo (or (getf request :repo)
                  (getf request :path)
                  (getf request :source)))
        (ref (getf request :ref))
        (resource-types (getf request :resource-types))
        (scope (or (getf request :scope) :project)))
    (list :src-type src-type
          :repo repo
          :ref ref
          :resource-types resource-types
          :scope scope)))

(defun load-project-declared-packages (&optional project-designator)
  "Install missing project-declared packages for PROJECT-DESIGNATOR or all projects."
  (let ((loaded nil)
        (projects (if project-designator
                      (list (ensure-project project-designator))
                      (list-projects))))
    (dolist (project projects)
      (dolist (request (project-packages project))
        (let ((name (getf request :name)))
          (cond
            ((not (project-package-install-request-valid-p request))
             (emit-package-warning
              "Project ~A declared package ~S without install source details"
              (project-name project)
              request))
            ((project-package-installed-p project request)
             nil)
            (t
             (let ((definition
                     (apply #'rplaca-use-package
                            (append (project-package-install-request->use-package-args
                                     request)
                                    (list :project project)))))
               (when definition
                 (push definition loaded))))))))
    (nreverse loaded)))

(defun config-project-root ()
  "Return the configured RPLACA init directory."
  (if (boundp '*user-init-directory*)
      (symbol-value '*user-init-directory*)
      (merge-pathnames #P".rplaca.d/" (user-homedir-pathname))))

(defun ensure-config-project ()
  "Ensure the user configuration directory is available as project \"config\"."
  (define-project "config"
    :root (config-project-root)
    :description "RPLACA user configuration"
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
  (let ((roots
          (configured-migration-read-roots
           *project-definitions-directory*
           +default-project-definitions-directory+
           +legacy-project-definitions-directory+
           :label "project definition registry"
           :executable-p t)))
    (ensure-config-project)
    (dolist (root roots)
      (dolist (path (project-manifest-paths root))
        (handler-case
            (load-project-manifest path :replace nil)
          (error (e)
            (format *error-output*
                    "~&;; Warning: error loading project manifest ~A:~%;; ~A~%"
                    path e))))))
  (load-project-declared-packages)
  (set-project-definitions-loaded-p t)
  (list-projects))

(defun remove-projects-by-source (sources)
  "Remove projects whose source is a member of SOURCES."
  (call-with-project-registry-lock
   (lambda ()
     (let ((keys nil))
       (maphash (lambda (key project)
                  (when (member (project-source project) sources :test #'eq)
                    (push key keys)))
                *project-registry*)
       (dolist (key keys)
         (remhash key *project-registry*))))
   *project-registry*))

(defun reload-projects ()
  "Reload built-in and manifest-backed projects, preserving programmatic ones."
  (remove-projects-by-source '(:manifest :builtin))
  (set-project-definitions-loaded-p nil)
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

(defun publish-project-buffer-effect (effect)
  "Collect, defer, or immediately apply immutable project buffer EFFECT."
  (cond
    (*project-buffer-effect-collector*
     (funcall *project-buffer-effect-collector* effect))
    ((and (boundp '*tool-effect-recorder*) *tool-effect-recorder*)
     (funcall *tool-effect-recorder* :project-buffer effect))
    (t
     (apply-project-buffer-effect effect)))
  effect)

(defun call-with-project-buffer-effect-transaction (function)
  "Publish project buffer effects only after FUNCTION commits successfully."
  (if *project-buffer-effect-collector*
      (funcall function)
      (let ((effects nil)
            (values nil)
            (committed-p nil))
        (unwind-protect
             (let ((*project-buffer-effect-collector*
                     (lambda (effect) (push effect effects))))
               (setf values (multiple-value-list (funcall function))
                     committed-p t))
          (when committed-p
            (dolist (effect (nreverse effects))
              ;; Filesystem state is authoritative; UI sync failure cannot
              ;; invalidate an already committed project transaction.
              (handler-case
                  (let ((*project-buffer-effect-collector* nil))
                    (publish-project-buffer-effect effect))
                (error (condition)
                  (file-debug-event
                   "project-buffer-effect-error"
                   :operation (project-buffer-effect-operation effect)
                   :project (project-buffer-effect-project-name effect)
                   :path (project-buffer-effect-path effect)
                   :condition (format nil "~A" condition)))))))
        (values-list values))))

(defun synchronize-open-project-file-buffer (project-designator path text
                                             &key (dirty-p nil)
                                                  (original-text text))
  "Update an already-open file buffer after a direct project resource write."
  (let* ((project (ensure-project project-designator))
         (resource-path (project-resource-name path))
         (effect (make-project-buffer-effect
                  :synchronize (copy-seq (project-name project))
                  (copy-seq resource-path)
                  :text (copy-seq text)
                  :dirty-p (not (null dirty-p))
                  :original-text (copy-seq original-text))))
    (when (or *project-buffer-effect-collector*
              (and (boundp '*tool-effect-recorder*) *tool-effect-recorder*))
      (return-from synchronize-open-project-file-buffer
        (publish-project-buffer-effect effect)))
    (let ((buffer
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
        buffer))))

(defun remove-open-project-file-buffer (project-designator path)
  "Close an open file buffer for PROJECT-DESIGNATOR/PATH, if one exists."
  (let* ((project (ensure-project project-designator))
         (resource-path (project-resource-name path))
         (effect (make-project-buffer-effect
                  :remove (copy-seq (project-name project))
                  (copy-seq resource-path))))
    (when (or *project-buffer-effect-collector*
              (and (boundp '*tool-effect-recorder*) *tool-effect-recorder*))
      (return-from remove-open-project-file-buffer
        (publish-project-buffer-effect effect)))
    (let ((buffer
            (find-if (lambda (candidate)
                       (and (file-buffer-p candidate)
                            (string= (or (buffer-project-name candidate) "")
                                     (project-name project))
                            (string= (or (buffer-resource-path candidate) "")
                                     resource-path)))
                     *buffer-ring*)))
      (when buffer
        (kill-buffer-from-ring buffer)
        buffer))))

(defun retarget-open-project-file-buffer (project-designator old-path new-path text)
  "Retarget an open file buffer from OLD-PATH to NEW-PATH after a rename."
  (let* ((project (ensure-project project-designator))
         (old-resource-path (project-resource-name old-path))
         (new-resource-path (project-resource-name new-path))
         (effect (make-project-buffer-effect
                  :retarget (copy-seq (project-name project))
                  (copy-seq old-resource-path)
                  :new-path (copy-seq new-resource-path)
                  :text (copy-seq text))))
    (when (or *project-buffer-effect-collector*
              (and (boundp '*tool-effect-recorder*) *tool-effect-recorder*))
      (return-from retarget-open-project-file-buffer
        (publish-project-buffer-effect effect)))
    (let ((buffer
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
        buffer))))

(defun apply-project-buffer-effect (effect)
  "Apply immutable project buffer EFFECT on the current frame process."
  (let ((*project-buffer-effect-collector* nil)
        (*tool-effect-recorder* nil))
    (ecase (project-buffer-effect-operation effect)
      (:synchronize
       (synchronize-open-project-file-buffer
        (project-buffer-effect-project-name effect)
        (project-buffer-effect-path effect)
        (project-buffer-effect-text effect)
        :dirty-p (project-buffer-effect-dirty-p effect)
        :original-text (project-buffer-effect-original-text effect)))
      (:remove
       (remove-open-project-file-buffer
        (project-buffer-effect-project-name effect)
        (project-buffer-effect-path effect)))
      (:retarget
       (retarget-open-project-file-buffer
        (project-buffer-effect-project-name effect)
        (project-buffer-effect-path effect)
        (project-buffer-effect-new-path effect)
        (project-buffer-effect-text effect))))))

(defun project-write-file (project-designator path text
                            &key (if-exists :supersede))
  "Atomically write TEXT to project resource PATH and return a summary plist."
  (let* ((project (ensure-project project-designator))
         (resource-path (project-resource-name path))
         (resolved (project-resolve-path project resource-path)))
    (unless (member if-exists '(:error :supersede) :test #'eq)
      (error "PROJECT-WRITE-FILE supports only :ERROR or :SUPERSEDE, got ~S."
             if-exists))
    (call-with-change-set-transaction
     (lambda ()
       (when (and (eq if-exists :error) (probe-file resolved))
         (error "Project file already exists: ~A:~A"
                (project-name project) resource-path))
       (ensure-directories-exist resolved)
       (let ((temporary (project-manifest-temporary-path resolved)))
         (unwind-protect
              (progn
                (with-open-file (stream temporary
                                        :direction :output
                                        :if-exists :error
                                        :if-does-not-exist :create)
                  (write-string text stream))
                ;; Recheck :ERROR immediately before publication.  Atomic
                ;; no-clobber against another OS process remains platform-
                ;; dependent and is documented as a residual limitation.
                (when (and (eq if-exists :error) (probe-file resolved))
                  (error "Project file appeared while writing: ~A:~A"
                         (project-name project) resource-path))
                (if (eq if-exists :supersede)
                    (uiop:rename-file-overwriting-target temporary resolved)
                    (rename-file temporary resolved)))
           (when (probe-file temporary)
             (ignore-errors (delete-file temporary)))))
       (handler-case
           (synchronize-open-project-file-buffer project resource-path text)
         (error (condition)
           (file-debug-event
            "project-buffer-effect-error"
            :operation :synchronize
            :project (project-name project)
            :path resource-path
            :condition (format nil "~A" condition))))
       (list :status :ok
             :project (project-name project)
             :path resource-path
             :bytes-written (length text))))))

(defun project-save-file (project-designator path text)
  "Save TEXT to project resource PATH, replacing any existing contents."
  (project-write-file project-designator path text :if-exists :supersede))

(defun project-create-file (project-designator path &key (content "")
                                             (if-exists :error))
  "Create a new project resource PATH containing CONTENT."
  (project-write-file project-designator path content :if-exists if-exists))

;;; --------------------------------------------------------------------------
;;; Transactional change sets
;;; --------------------------------------------------------------------------

(defun next-change-set-id ()
  "Return a fresh human-readable change set id."
  (call-with-change-set-registry-lock
   (lambda ()
     (format nil "change-~D" (incf *change-set-counter*)))
   *change-set-registry*))

(defun begin-change-set-locked (name description created-at)
  "Create and select a change set while the registry lock is held."
  (let* ((id (format nil "change-~D" (incf *change-set-counter*)))
         (change-set (make-change-set :id id
                                      :name (or name id)
                                      :description description
                                      :entries nil
                                      :status :open
                                      :created-at created-at)))
    (setf (gethash id *change-set-registry*) change-set
          *current-change-set* change-set)
    change-set))

(defun begin-change-set (&key name description)
  "Create and select a new staged project change set."
  (let ((created-at (get-universal-time)))
    (call-with-change-set-registry-lock
     (lambda ()
       (begin-change-set-locked name description created-at))
     *change-set-registry*)))

(defun current-change-set ()
  "Return the current staged project change set, or NIL."
  (call-with-change-set-registry-lock
   (lambda () *current-change-set*)
   *change-set-registry*))

(defun find-change-set (designator)
  "Return the change set named by DESIGNATOR, or NIL."
  (etypecase designator
    (change-set designator)
    (string
     (call-with-change-set-registry-lock
      (lambda () (gethash designator *change-set-registry*))
      *change-set-registry*))
    (symbol
     (let ((key (string-downcase (symbol-name designator))))
       (call-with-change-set-registry-lock
        (lambda () (gethash key *change-set-registry*))
        *change-set-registry*)))))

(defun ensure-change-set (&optional change-set-designator)
  "Return CHANGE-SET-DESIGNATOR, the current change set, or a new change set."
  (if change-set-designator
      (or (find-change-set change-set-designator)
          (error "Unknown change set: ~A" change-set-designator))
      (let ((created-at (get-universal-time)))
        (call-with-change-set-registry-lock
         (lambda ()
           (or *current-change-set*
               (begin-change-set-locked nil nil created-at)))
         *change-set-registry*))))

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
  (sort (mapcar #'cdr (change-set-registry-snapshot))
        #'< :key (lambda (change-set)
                   (or (change-set-created-at change-set) 0))))

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
  (call-with-change-set-registry-lock
   (lambda ()
     (ensure-open-change-set change-set)
     (setf (change-set-entries change-set)
           (append (change-set-entries change-set) (list entry))))
   *change-set-registry*)
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
         (resource-path (project-resource-name path))
         (entries
           (call-with-change-set-registry-lock
            (lambda () (copy-list (change-set-entries change-set)))
            *change-set-registry*)))
    (find-if
     (lambda (entry)
       (and (string= project-key
                     (normalize-project-name
                      (change-set-entry-project-name entry)))
            (or (string= resource-path (change-set-entry-path entry))
                (and (change-set-entry-new-path entry)
                     (string= resource-path
                              (change-set-entry-new-path entry))))))
     (reverse entries))))

(defun change-set-project-file-text (project-designator path
                                      &optional change-set-designator)
  "Return PROJECT-DESIGNATOR/PATH text including the latest staged write."
  (let ((change-set (or (and change-set-designator
                             (find-change-set change-set-designator))
                        (current-change-set))))
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
  (let* ((change-set (ensure-change-set change-set-designator))
         (snapshot
           (call-with-change-set-registry-lock
            (lambda () (copy-change-set-snapshot change-set))
            *change-set-registry*)))
    (with-output-to-string (out)
      (format out "Change Set: ~A  Status: ~(~A~)~@[  Name: ~A~]~%~%"
              (change-set-id snapshot)
              (change-set-status snapshot)
              (change-set-name snapshot))
      (if (change-set-entries snapshot)
          (dolist (entry (change-set-entries snapshot))
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
                                          (change-set-entry-new-text entry)))))
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

(defun invoke-change-set-entry-apply (entry)
  "Apply detached ENTRY, restoring its stored snapshot on any failure."
  (handler-case
      (funcall (or *change-set-entry-apply-function*
                   #'apply-change-set-entry)
               entry)
    (error (condition)
      (handler-case
          (revert-change-set-entry entry)
        (error (compensation-condition)
          (error "Entry apply failed (~A) and compensation failed (~A)."
                 condition compensation-condition)))
      (error condition))))

(defun invoke-change-set-entry-revert (entry)
  "Revert detached ENTRY, restoring its applied state on any failure."
  (handler-case
      (funcall (or *change-set-entry-revert-function*
                   #'revert-change-set-entry)
               entry)
    (error (condition)
      (handler-case
          (apply-change-set-entry entry)
        (error (compensation-condition)
          (error "Entry revert failed (~A) and compensation failed (~A)."
                 condition compensation-condition)))
      (error condition))))

(defun merge-change-set-entry-applied-state-locked (change-set entries)
  "Merge detached ENTRIES' applied flags into CHANGE-SET while locked."
  (let ((actual (change-set-entries change-set)))
    (unless (= (length actual) (length entries))
      (error "Change set ~A entries changed during its transaction."
             (change-set-id change-set)))
    (mapc (lambda (actual-entry detached-entry)
            (setf (change-set-entry-applied-p actual-entry)
                  (change-set-entry-applied-p detached-entry)))
          actual entries))
  change-set)

(defun apply-change-set (&optional change-set-designator)
  "Apply CHANGE-SET-DESIGNATOR, rolling back already applied entries on error."
  (let ((change-set (ensure-change-set change-set-designator)))
    (call-with-project-buffer-effect-transaction
     (lambda ()
       (call-with-change-set-transaction
        (lambda ()
          (let ((entries nil)
                (applied nil))
            ;; Only detached entries cross the bounded registry section.
            (call-with-change-set-registry-lock
             (lambda ()
               (ensure-open-change-set change-set)
               (setf (change-set-status change-set) :applying
                     entries
                     (mapcar #'copy-change-set-entry
                             (change-set-entries change-set))))
             *change-set-registry*)
            (handler-case
                (progn
                  (dolist (entry entries)
                    (invoke-change-set-entry-apply entry)
                    (push entry applied))
                  (call-with-change-set-registry-lock
                   (lambda ()
                     (merge-change-set-entry-applied-state-locked
                      change-set entries)
                     (setf (change-set-status change-set) :applied
                           (change-set-applied-at change-set)
                           (get-universal-time)))
                   *change-set-registry*)
                  change-set)
              (error (condition)
                (let ((rollback-errors nil))
                  ;; APPLIED is already in reverse application order.
                  (dolist (entry applied)
                    (handler-case
                        (invoke-change-set-entry-revert entry)
                      (error (rollback-condition)
                        (push rollback-condition rollback-errors))))
                  (call-with-change-set-registry-lock
                   (lambda ()
                     (merge-change-set-entry-applied-state-locked
                      change-set entries)
                     (setf (change-set-status change-set)
                           (if rollback-errors :rollback-failed :failed)
                           (change-set-applied-at change-set) nil))
                   *change-set-registry*)
                  (error
                   "Failed applying change set ~A; rollback errors: ~D; cause: ~A"
                   (change-set-id change-set)
                   (length rollback-errors)
                   condition)))))))))))

(defun discard-change-set (&optional change-set-designator)
  "Discard an unapplied staged change set."
  (let ((change-set (ensure-change-set change-set-designator)))
    (call-with-change-set-registry-lock
     (lambda ()
       (unless (eq :open (change-set-status change-set))
         (error "Cannot discard change set ~A with status ~A."
                (change-set-id change-set)
                (change-set-status change-set)))
       (setf (change-set-status change-set) :discarded)
       (when (eq change-set *current-change-set*)
         (setf *current-change-set* nil)))
     *change-set-registry*)
    change-set))

(defun revert-change-set (&optional change-set-designator)
  "Revert an applied CHANGE-SET-DESIGNATOR using stored snapshots."
  (let ((change-set (ensure-change-set change-set-designator)))
    (call-with-project-buffer-effect-transaction
     (lambda ()
       (call-with-change-set-transaction
        (lambda ()
          (let ((entries nil)
                (reverted nil))
            (call-with-change-set-registry-lock
             (lambda ()
               (unless (eq :applied (change-set-status change-set))
                 (error "Change set ~A is not applied; status is ~A."
                        (change-set-id change-set)
                        (change-set-status change-set)))
               (setf (change-set-status change-set) :reverting
                     entries
                     (mapcar #'copy-change-set-entry
                             (change-set-entries change-set))))
             *change-set-registry*)
            (handler-case
                (progn
                  (dolist (entry (reverse (copy-list entries)))
                    (invoke-change-set-entry-revert entry)
                    (push entry reverted))
                  (call-with-change-set-registry-lock
                   (lambda ()
                     (merge-change-set-entry-applied-state-locked
                      change-set entries)
                     (setf (change-set-status change-set) :reverted)
                     (when (eq change-set *current-change-set*)
                       (setf *current-change-set* nil)))
                   *change-set-registry*)
                  change-set)
              (error (condition)
                (let ((restore-errors nil))
                  ;; REVERTED is already in original application order.
                  (dolist (entry reverted)
                    (handler-case
                        (invoke-change-set-entry-apply entry)
                      (error (restore-condition)
                        (push restore-condition restore-errors))))
                  (call-with-change-set-registry-lock
                   (lambda ()
                     (merge-change-set-entry-applied-state-locked
                      change-set entries)
                     (setf (change-set-status change-set) :revert-failed))
                   *change-set-registry*)
                  (error
                   "Failed reverting change set ~A; restore errors: ~D; cause: ~A"
                   (change-set-id change-set)
                   (length restore-errors)
                   condition)))))))))))

(defun change-set-summary-to-string (&optional change-set-designator)
  "Return a compact summary of staged entries in CHANGE-SET-DESIGNATOR."
  (let* ((change-set (ensure-change-set change-set-designator))
         (snapshot
           (call-with-change-set-registry-lock
            (lambda () (copy-change-set-snapshot change-set))
            *change-set-registry*)))
    (with-output-to-string (out)
      (format out "~A (~(~A~)): ~D entr~:@P~%"
              (change-set-id snapshot)
              (change-set-status snapshot)
              (length (change-set-entries snapshot)))
      (dolist (entry (change-set-entries snapshot))
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
  "Open PROJECT-DESIGNATOR/PATH as an editable RPLACA file buffer."
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
          (setf (buffer-keymap buffer) (ensure-file-keymap-initialized))
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
                                                     (preview-chars 96))
  "Return a structural outline for one Lisp project resource or all Lisp files."
  (let* ((project (ensure-project project-designator))
         (options (keyword-option-list :depth depth
                                       :max-depth max-depth
                                       :head head
                                       :limit limit
                                       :preview-chars preview-chars))
         (paths (if path
                    (list (project-resource-name path))
                    (remove-if-not #'project-lisp-source-path-p
                                   (project-list-files project)))))
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
