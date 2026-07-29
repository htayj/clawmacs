(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Provider-neutral generated media
;;; --------------------------------------------------------------------------

(defstruct media-provider
  "A package-owned generated-media provider.

START-FN receives one MEDIA-REQUEST and returns a MEDIA-PROVIDER-OUTCOME.
POLL-FN and CANCEL-FN, when present, receive one MEDIA-OPERATION.  This keeps
the public contract suitable for immediate image results and later asynchronous
video jobs without exposing provider or billing selection to an agent tool."
  (id "" :type string :read-only t)
  (kinds nil :type list :read-only t)
  (start-fn nil :type function :read-only t)
  (poll-fn nil :type (or null function) :read-only t)
  (cancel-fn nil :type (or null function) :read-only t)
  (package nil :type (or null string) :read-only t))

(defstruct media-request
  "A provider-neutral request for generated media."
  (id "" :type string :read-only t)
  (kind :image :type keyword :read-only t)
  (prompt "" :type string :read-only t)
  (referenced-image-paths nil :type list :read-only t))

(defstruct media-asset
  "One completed binary asset returned by a media provider."
  (name "generated-image.png" :type string :read-only t)
  (mime-type "application/octet-stream" :type string :read-only t)
  (octets #() :type (vector (unsigned-byte 8)) :read-only t)
  (metadata nil :type list :read-only t))

(defstruct media-provider-outcome
  "One provider result.  RUNNING outcomes carry a backend job identifier."
  (status :failed :type keyword :read-only t)
  (assets nil :type list :read-only t)
  (revised-prompt nil :type (or null string) :read-only t)
  (backend-id nil :type (or null string) :read-only t)
  (public-error nil :type (or null string) :read-only t))

(defstruct media-operation
  "Tracked lifecycle state for one provider-neutral media request."
  (id "" :type string :read-only t)
  (provider-id "" :type string :read-only t)
  (request nil :type media-request :read-only t)
  (status :pending :type keyword)
  (backend-id nil :type (or null string))
  (outcome nil :type (or null media-provider-outcome))
  (error nil :type (or null string))
  (artifact-records nil :type list)
  (persisting-p nil :type boolean)
  (started-at 0 :type integer :read-only t)
  (updated-at 0 :type integer))

(define-condition media-artifact-write-failed (error)
  ((operation :initarg :operation :reader media-artifact-write-failed-operation)
   (cause :initarg :cause :reader media-artifact-write-failed-cause))
  (:report (lambda (condition stream)
             (format stream "Media artifact write failed for operation ~A: ~A"
                     (media-operation-id
                      (media-artifact-write-failed-operation condition))
                     (media-artifact-write-failed-cause condition)))))

(defvar *media-provider-registry* (make-hash-table :test #'equal)
  "Registry of installed package-owned MEDIA-PROVIDER definitions.")

(defvar *media-provider-registry-lock* (bt:make-lock "rplaca media providers")
  "Lock guarding the media provider and operation registries.")

(defvar *media-operation-registry* (make-hash-table :test #'equal)
  "Registry of active and completed MEDIA-OPERATION values for this process.")

(defvar *media-default-provider* nil
  "User-selected default generated-media provider id, or NIL when unconfigured.")

(defvar *media-operation-counter* 0
  "Monotonic counter used with time to construct media operation identifiers.")

(defparameter *media-supported-kinds* '(:image :video)
  "Media kinds understood by the provider-neutral contract.")

(defparameter *media-outcome-statuses* '(:succeeded :running :failed :cancelled)
  "Terminal and non-terminal provider outcome statuses.")

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; STAT resolves symlinks without opening their target.  That lets request
  ;; validation reject FIFOs, sockets, devices, and directories before a media
  ;; adapter could block while trying to read an alleged reference image.
  (require :sb-posix))

#+sbcl
(defvar *media-posix-stat-function*
  (let ((symbol (and (find-package "SB-POSIX")
                     (find-symbol "STAT" "SB-POSIX"))))
    (and symbol (fboundp symbol) (symbol-function symbol)))
  "Resolved SB-POSIX STAT function used for fail-closed media file checks.")

#+sbcl
(defvar *media-posix-stat-mode-function*
  (let ((symbol (and (find-package "SB-POSIX")
                     (find-symbol "STAT-MODE" "SB-POSIX"))))
    (and symbol (fboundp symbol) (symbol-function symbol)))
  "Resolved SB-POSIX STAT-MODE accessor used for media file checks.")

#+sbcl
(defvar *media-posix-file-type-mask*
  (let ((symbol (and (find-package "SB-POSIX")
                     (find-symbol "S-IFMT" "SB-POSIX"))))
    (and symbol (boundp symbol) (symbol-value symbol)))
  "SB-POSIX bit mask selecting the POSIX file type from a stat mode.")

#+sbcl
(defvar *media-posix-regular-file-type*
  (let ((symbol (and (find-package "SB-POSIX")
                     (find-symbol "S-IFREG" "SB-POSIX"))))
    (and symbol (boundp symbol) (symbol-value symbol)))
  "SB-POSIX mode value representing a regular file.")

(defun normalize-media-provider-id (value)
  "Return VALUE as a non-empty lowercase provider id."
  (let ((text (string-downcase
               (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (string value)))))
    (unless (plusp (length text))
      (error "Media provider id must be a non-empty string."))
    text))

(defun normalize-media-kind (value)
  "Return VALUE as one of the media contract's supported kinds."
  (let ((kind (if (keywordp value)
                  value
                  (intern (string-upcase (string value)) :keyword))))
    (unless (member kind *media-supported-kinds* :test #'eq)
      (error "Unsupported media kind ~S. Expected one of ~S."
             value *media-supported-kinds*))
    kind))

(defun media-registry-owner-name ()
  "Return the package name that owns a registration in the current load."
  (and *current-rplaca-package*
       (manifest-package-name *current-rplaca-package*)))

(defun normalize-media-provider-package-owner (package)
  "Return PACKAGE as a canonical package owner, or signal for an invalid name."
  (when package
    (or (manifest-package-name package)
        (error "Media provider package owner must be a non-empty package name, got ~S."
               package))))

(defun media-regular-file-pathname-p (pathname)
  "Return true only when PATHNAME resolves to a POSIX regular file.

Symlinks are intentionally accepted when their resolved target is regular.
This check performs stat metadata lookup only; it never opens the candidate.
Non-SBCL implementations fail closed because the supported runtime is SBCL."
  #+sbcl
  (and *media-posix-stat-function*
       *media-posix-stat-mode-function*
       *media-posix-file-type-mask*
       *media-posix-regular-file-type*
       (handler-case
           (let* ((status (funcall *media-posix-stat-function*
                                   (namestring pathname)))
                  (mode (funcall *media-posix-stat-mode-function* status)))
             (= (logand mode *media-posix-file-type-mask*)
                *media-posix-regular-file-type*))
         (error () nil)))
  #-sbcl
  (declare (ignore pathname))
  #-sbcl
  nil)

(defun normalize-media-reference-image-paths (paths)
  "Return PATHS as resolved absolute regular files for a media request.

NIL means no references.  Any nonempty list or vector must contain at most
five nonblank absolute paths whose resolved targets are regular files."
  (cond
    ((null paths) nil)
    ((not (or (listp paths) (vectorp paths)))
     (error "referenced_image_paths must be a list or vector of absolute regular files."))
    (t
     (let ((items (coerce paths 'list)))
       (when (null items)
         (error "referenced_image_paths must be omitted or contain at least one path."))
       (when (> (length items) 5)
         (error "referenced_image_paths accepts at most 5 paths."))
       (mapcar
        (lambda (item)
          (unless (stringp item)
            (error "referenced image path must be a string, got ~S." item))
          (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return) item))
                 (pathname (pathname text)))
            (unless (plusp (length text))
              (error "referenced image path must be non-empty."))
            (unless (uiop:absolute-pathname-p pathname)
              (error "referenced image path must be absolute: ~A" text))
            (let ((resolved (probe-file pathname)))
              (unless resolved
                (error "referenced image path does not exist: ~A" text))
              (let ((true-path (truename resolved)))
                (unless (media-regular-file-pathname-p true-path)
                  (error "referenced image path must resolve to a regular file: ~A"
                         text))
                (namestring true-path)))))
        items)))))

(defun media-next-operation-id-unlocked ()
  "Return one registry-unique media operation id while the registry is locked."
  (format nil "media-~36R-~36R"
          (get-universal-time)
          (incf *media-operation-counter*)))

(defun register-media-provider (id kinds start-fn &key poll-fn cancel-fn package)
  "Register ID as a package-owned generated-media provider.

START-FN is mandatory.  POLL-FN and CANCEL-FN are optional hooks for
asynchronous providers such as video generation services."
  (let* ((normalized-id (normalize-media-provider-id id))
         (normalized-kinds (remove-duplicates
                            (mapcar #'normalize-media-kind kinds)
                            :test #'eq))
         (owner (or (normalize-media-provider-package-owner package)
                    (media-registry-owner-name))))
    (unless normalized-kinds
      (error "Media provider ~A must declare at least one kind." normalized-id))
    (unless (functionp start-fn)
      (error "Media provider ~A requires a START-FN function." normalized-id))
    (unless (or (null poll-fn) (functionp poll-fn))
      (error "Media provider ~A POLL-FN must be a function or NIL." normalized-id))
    (unless (or (null cancel-fn) (functionp cancel-fn))
      (error "Media provider ~A CANCEL-FN must be a function or NIL." normalized-id))
    (let ((provider (make-media-provider :id normalized-id
                                         :kinds normalized-kinds
                                         :start-fn start-fn
                                         :poll-fn poll-fn
                                         :cancel-fn cancel-fn
                                         :package owner)))
      (bt:with-lock-held (*media-provider-registry-lock*)
        (setf (gethash normalized-id *media-provider-registry*) provider))
      provider)))

(defun find-media-provider (id)
  "Return the registered provider named ID, or NIL."
  (let ((normalized-id (and id (normalize-media-provider-id id))))
    (when normalized-id
      (bt:with-lock-held (*media-provider-registry-lock*)
        (gethash normalized-id *media-provider-registry*)))))

(defun list-media-providers ()
  "Return a stable snapshot of all registered media providers."
  (bt:with-lock-held (*media-provider-registry-lock*)
    (sort (loop :for provider :being :the :hash-values :of *media-provider-registry*
                :collect provider)
          #'string< :key #'media-provider-id)))

(defun remove-media-providers-for-package (package-name)
  "Remove provider registrations owned by PACKAGE-NAME during package reset."
  (let ((normalized (and package-name (manifest-package-name package-name))))
    (when normalized
      (bt:with-lock-held (*media-provider-registry-lock*)
        (let ((removed nil))
          (maphash (lambda (id provider)
                     (when (string= normalized (or (media-provider-package provider) ""))
                       (remhash id *media-provider-registry*)
                       (push id removed)))
                   *media-provider-registry*)
          (when (and *media-default-provider*
                     (member *media-default-provider* removed :test #'string=))
            (setf *media-default-provider* nil))
          (nreverse removed))))))

(defun media-default-provider ()
  "Return the user-configured default provider id, or NIL."
  *media-default-provider*)

(defun set-media-default-provider (id)
  "Set the user-selected default media provider to registered ID.

Passing NIL clears the selection.  Tool callers never select providers: this
function is intentionally configuration-facing."
  (let ((normalized (and id (normalize-media-provider-id id))))
    (when (and normalized (null (find-media-provider normalized)))
      (error "Media provider ~A is not registered." normalized))
    (setf *media-default-provider* normalized)))

(defun make-media-generation-request (kind prompt &key referenced-image-paths)
  "Create one validated media request for KIND and PROMPT."
  (let ((normalized-kind (normalize-media-kind kind))
        (normalized-prompt
          (string-trim '(#\Space #\Tab #\Newline #\Return) (or prompt ""))))
    (unless (plusp (length normalized-prompt))
      (error "Media prompt must be a non-empty string."))
    (make-media-request :id (format nil "request-~36R-~36R"
                                    (get-universal-time)
                                    (get-internal-real-time))
                        :kind normalized-kind
                        :prompt normalized-prompt
                        :referenced-image-paths
                        (normalize-media-reference-image-paths
                         referenced-image-paths))))

(defun validate-media-provider-outcome (outcome)
  "Return OUTCOME after checking the narrow provider callback contract."
  (unless (typep outcome 'media-provider-outcome)
    (error "Media provider callback must return a MEDIA-PROVIDER-OUTCOME, got ~S."
           outcome))
  (unless (member (media-provider-outcome-status outcome)
                  *media-outcome-statuses* :test #'eq)
    (error "Media provider returned unsupported outcome status ~S."
             (media-provider-outcome-status outcome)))
  (when (eq (media-provider-outcome-status outcome) :running)
    (let ((backend-id (media-provider-outcome-backend-id outcome)))
      (unless (and (stringp backend-id)
                   (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                               backend-id))))
        (error "Running media provider outcomes require a non-empty backend id."))))
  (dolist (asset (media-provider-outcome-assets outcome))
    (unless (typep asset 'media-asset)
      (error "Media provider outcomes must contain MEDIA-ASSET values, got ~S."
             asset)))
  (when (and (eq (media-provider-outcome-status outcome) :succeeded)
             (null (media-provider-outcome-assets outcome)))
    (error "Successful media provider outcome must contain at least one asset."))
  outcome)

(defun apply-media-provider-outcome (operation outcome)
  "Apply OUTCOME to OPERATION and return OPERATION."
  (let ((validated (validate-media-provider-outcome outcome)))
    (bt:with-lock-held (*media-provider-registry-lock*)
      ;; Cancellation wins over a late START-FN response.  An adapter may have
      ;; completed remotely after the caller cancelled, but that result must
      ;; never become a successful local operation or durable artifact.
      (unless (and (eq (media-operation-status operation) :cancelled)
                   (not (eq (media-provider-outcome-status validated)
                            :cancelled)))
        (setf (media-operation-status operation)
              (media-provider-outcome-status validated)
              (media-operation-outcome operation) validated
              (media-operation-backend-id operation)
              (media-provider-outcome-backend-id validated)
              (media-operation-error operation)
              (media-provider-outcome-public-error validated)
              (media-operation-updated-at operation) (get-universal-time))))
    operation))

(defun begin-media-operation (request &key provider-id)
  "Publish a pending operation for REQUEST without invoking its provider.

PROVIDER-ID is for trusted configuration and adapter code.  Agent-facing tools
must call this without it, thereby using *MEDIA-DEFAULT-PROVIDER*."
  (unless (typep request 'media-request)
    (error "Expected a MEDIA-REQUEST, got ~S." request))
  (let* ((selected (or provider-id (media-default-provider)))
         (provider (and selected (find-media-provider selected))))
    (unless selected
      (error "No default media provider is configured."))
    (unless provider
      (error "Configured media provider ~A is not registered." selected))
    (unless (member (media-request-kind request) (media-provider-kinds provider)
                    :test #'eq)
      (error "Media provider ~A does not support ~A generation."
             (media-provider-id provider) (media-request-kind request)))
    (bt:with-lock-held (*media-provider-registry-lock*)
      (let ((created (make-media-operation
                      :id (media-next-operation-id-unlocked)
                      :provider-id (media-provider-id provider)
                      :request request
                      :started-at (get-universal-time)
                      :updated-at (get-universal-time))))
        (setf (gethash (media-operation-id created) *media-operation-registry*)
              created)
        created))))

(defun run-media-operation (operation)
  "Invoke OPERATION's START-FN once unless cancellation arrived first."
  (unless (typep operation 'media-operation)
    (error "Expected a MEDIA-OPERATION, got ~S." operation))
  (let ((provider (find-media-provider (media-operation-provider-id operation))))
    (unless provider
      (error "Media provider ~A is no longer registered."
             (media-operation-provider-id operation)))
    (unless (bt:with-lock-held (*media-provider-registry-lock*)
              (when (eq (media-operation-status operation) :pending)
                (setf (media-operation-status operation) :running
                      (media-operation-updated-at operation) (get-universal-time))
                t))
      (return-from run-media-operation operation))
    (handler-case
        (apply-media-provider-outcome
         operation
         (funcall (media-provider-start-fn provider)
                  (media-operation-request operation)))
      (error (condition)
        (apply-media-provider-outcome
         operation
         (make-media-provider-outcome
          :status :failed
          :public-error (format nil "Media provider request failed: ~A" condition))))))
  operation)

(defun start-media-operation (request &key provider-id)
  "Publish and immediately start REQUEST through the configured provider."
  (run-media-operation (begin-media-operation request :provider-id provider-id)))

(defun find-media-operation (id)
  "Return a tracked media operation by ID, or NIL."
  (bt:with-lock-held (*media-provider-registry-lock*)
    (gethash id *media-operation-registry*)))

(defun poll-media-operation (operation-or-id)
  "Poll one running operation when its provider supplies a poll callback."
  (let* ((operation (if (typep operation-or-id 'media-operation)
                        operation-or-id
                        (find-media-operation operation-or-id))))
    (unless operation
      (error "Unknown media operation ~S." operation-or-id))
    (unless (eq (media-operation-status operation) :running)
      (return-from poll-media-operation operation))
    (let* ((provider (find-media-provider (media-operation-provider-id operation)))
           (poll-fn (and provider (media-provider-poll-fn provider))))
      (unless poll-fn
        (error "Media provider ~A does not support polling."
               (media-operation-provider-id operation)))
      (handler-case
          (apply-media-provider-outcome operation (funcall poll-fn operation))
        (error (condition)
          (apply-media-provider-outcome
           operation
           (make-media-provider-outcome
            :status :failed
            :public-error (format nil "Media provider poll failed: ~A" condition))))))))

(defun cancel-media-operation (operation-or-id)
  "Cooperatively cancel one running operation when its provider allows it."
  (let* ((operation (if (typep operation-or-id 'media-operation)
                        operation-or-id
                        (find-media-operation operation-or-id))))
    (unless operation
      (error "Unknown media operation ~S." operation-or-id))
    (let ((status (bt:with-lock-held (*media-provider-registry-lock*)
                    (media-operation-status operation))))
      (when (eq status :pending)
        (return-from cancel-media-operation
          (apply-media-provider-outcome
           operation
           (make-media-provider-outcome :status :cancelled))))
      ;; Until Artifactum persistence begins, cancellation wins over a just
      ;; returned successful START-FN outcome.  Once bytes are being written,
      ;; success wins: the durable write is irreversible and never races a
      ;; cancellation into a half-published operation.
      (when (eq status :succeeded)
        (bt:with-lock-held (*media-provider-registry-lock*)
          (when (and (eq (media-operation-status operation) :succeeded)
                     (not (media-operation-persisting-p operation))
                     (null (media-operation-artifact-records operation)))
            (setf (media-operation-status operation) :cancelled
                  (media-operation-outcome operation)
                  (make-media-provider-outcome :status :cancelled)
                  (media-operation-error operation) nil
                  (media-operation-updated-at operation) (get-universal-time))))
        (return-from cancel-media-operation operation))
      (unless (eq status :running)
        (return-from cancel-media-operation operation)))
    (let* ((provider (find-media-provider (media-operation-provider-id operation)))
           (cancel-fn (and provider (media-provider-cancel-fn provider))))
      (unless cancel-fn
        (error "Media provider ~A does not support cancellation."
               (media-operation-provider-id operation)))
      (handler-case
          (apply-media-provider-outcome operation (funcall cancel-fn operation))
        (error (condition)
          (apply-media-provider-outcome
           operation
           (make-media-provider-outcome
            :status :failed
            :public-error (format nil "Media provider cancellation failed: ~A" condition))))))))

(defun media-operation-artifact-metadata (operation asset)
  "Return durable artifact metadata for ASSET produced by OPERATION."
  (append (copy-list (media-asset-metadata asset))
          `((:provider . ,(media-operation-provider-id operation))
            (:media_kind . ,(string-downcase
                             (symbol-name (media-request-kind
                                           (media-operation-request operation))))))))

(defun media-operation-artifact-provenance (operation)
  "Return durable provenance for one generated media artifact."
  `((:operation_id . ,(media-operation-id operation))
    (:request_id . ,(media-request-id (media-operation-request operation)))
    (:backend_id . ,(media-operation-backend-id operation))))

(defun persist-media-operation-assets (buffer operation)
  "Persist successful OPERATION assets into BUFFER's Artifactum session.

The operation retains the normalized artifact records so callers can safely
report them without recalculating filenames or source paths."
  (unless (typep operation 'media-operation)
    (error "Expected a MEDIA-OPERATION, got ~S." operation))
  (let ((existing
          (bt:with-lock-held (*media-provider-registry-lock*)
            (unless (eq (media-operation-status operation) :succeeded)
              (error "Only successful media operations can be persisted, got ~S."
                     (media-operation-status operation)))
            (or (media-operation-artifact-records operation)
                (progn
                  (when (media-operation-persisting-p operation)
                    (error "Media operation ~A is already persisting artifacts."
                           (media-operation-id operation)))
                  (setf (media-operation-persisting-p operation) t)
                  nil)))))
    (when existing
      (return-from persist-media-operation-assets existing))
    (handler-case
        (let ((records
                (mapcar (lambda (asset)
                          (artifactum-create-from-octets
                           buffer
                           (media-asset-name asset)
                           (media-asset-octets asset)
                           :mime-type (media-asset-mime-type asset)
                           :kind "generated-media"
                           :author "agent"
                           :metadata (media-operation-artifact-metadata operation asset)
                           :provenance (media-operation-artifact-provenance operation)))
                        (media-provider-outcome-assets
                         (media-operation-outcome operation)))))
          (bt:with-lock-held (*media-provider-registry-lock*)
            (setf (media-operation-artifact-records operation) records
                  (media-operation-persisting-p operation) nil))
          records)
      (media-artifact-write-failed (condition)
        (bt:with-lock-held (*media-provider-registry-lock*)
          (setf (media-operation-persisting-p operation) nil))
        (error condition))
      (error (condition)
        (bt:with-lock-held (*media-provider-registry-lock*)
          (setf (media-operation-persisting-p operation) nil))
        (error 'media-artifact-write-failed
               :operation operation
               :cause condition)))))

(defun media-operation-data (operation &key include-artifacts-p)
  "Return provider-safe data describing OPERATION for a tool result."
  (let ((outcome (media-operation-outcome operation)))
    (list :operation-id (media-operation-id operation)
          :provider (media-operation-provider-id operation)
          :kind (string-downcase (symbol-name
                                  (media-request-kind
                                   (media-operation-request operation))))
          :status (string-downcase (symbol-name (media-operation-status operation)))
          :backend-id (media-operation-backend-id operation)
          :revised-prompt (and outcome (media-provider-outcome-revised-prompt outcome))
          :error (media-operation-error operation)
          :artifacts (and include-artifacts-p
                          (coerce (copy-list (media-operation-artifact-records operation))
                                  'vector)))))
