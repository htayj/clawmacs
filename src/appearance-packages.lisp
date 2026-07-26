(in-package :clawmacs)

;;; Package-owned appearance declarations deliberately live above the pure
;;; appearance model.  The model knows only immutable catalogs; this file owns
;;; the package admission and publication transaction which supplies one.

(defstruct (package-appearance-declarations
            (:constructor %make-package-appearance-declarations
                (&key owner roles themes defaults))
            (:conc-name %package-appearance-declarations-))
  "Immutable declarations contributed by one package owner."
  (owner nil :type string :read-only t)
  (roles nil :type list :read-only t)
  (themes nil :type list :read-only t)
  (defaults nil :type list :read-only t))

(defstruct (package-appearance-staging
            (:constructor make-package-appearance-staging (&key definition owner))
            (:conc-name package-appearance-staging-))
  "Entry-point-local declaration batch, never a published catalog."
  definition
  owner
  (roles nil :type list)
  (themes nil :type list)
  (defaults nil :type list))

(defstruct (package-appearance-publication-batch
            (:constructor %make-package-appearance-publication-batch
                (&key base-catalog base-declarations replacements))
            (:conc-name package-appearance-publication-batch-))
  "Opaque all-owner candidate accumulated by one outer package reload."
  base-catalog
  base-declarations
  replacements)

(defstruct (appearance-package-transition-token
            (:constructor make-appearance-package-transition-token ()))
  "Barrier shared by every owning frame in one catalog transaction."
  (state :preparing :type keyword)
  (lock (bt:make-lock "appearance package transition"))
  (condition (bt:make-condition-variable
              :name "appearance package transition"))
  (expected-count 0 :type (integer 0 *))
  (ready-count 0 :type (integer 0 *))
  (applied-count 0 :type (integer 0 *))
  failure)

(defvar *package-appearance-declarations* (make-hash-table :test #'equal)
  "Published owner -> immutable package appearance declarations.")

(defvar *package-appearance-catalog* nil
  "Process-wide declaration catalog.  It never denotes an active profile.")

(defvar *package-appearance-catalog-lock*
  (bt:make-lock "package appearance catalog")
  "Serializes owner-scoped catalog candidate construction and publication.")

(defvar *package-appearance-entrypoint-staging* nil
  "Dynamically bound only while one package entrypoint is loading.")

(defvar *appearance-package-live-frame-provider* (constantly nil)
  "Function returning the frames which must classify a catalog transition.

The McCLIM adapter installs the real provider after its frame class is loaded.
Tests may bind this seam with ordinary objects; declaration publication itself
does not depend on CLIM implementation details.")

(defvar *appearance-package-frame-transition-planner*
  (lambda (frame catalog)
    (declare (ignore frame catalog))
    (list :status :ready))
  "Function which classifies one prospective catalog transition without mutation.")

(defvar *appearance-package-frame-transition-reserver*
  (lambda (frame plan catalog token)
    (list frame plan catalog token))
  "Side-effect-free admission for one frame catalog transition.")

(defvar *appearance-package-frame-transition-publisher*
  (lambda (reservation)
    (declare (ignore reservation))
    t)
  "Release a committed reservation through the frame's canonical event boundary.")

(defvar *appearance-package-frame-transition-finalizer*
  (lambda (token reservations commit-function rollback-function)
    (declare (ignore reservations rollback-function))
    (funcall commit-function)
    (setf (appearance-package-transition-token-state token) :committed)
    t)
  "Coordinate queued frame reservations with process-global publication.")

(defvar *appearance-package-batch-checkpoint-function*
  (lambda (reservations) (declare (ignore reservations)) nil)
  "Record one committed package transition in an enclosing reload batch.")

(defun abort-appearance-package-transition (token)
  "Release every participant in an unpublished transition."
  (bt:with-lock-held ((appearance-package-transition-token-lock token))
    (setf (appearance-package-transition-token-state token) :aborted)
    #+sbcl
    (sb-thread:condition-broadcast
     (appearance-package-transition-token-condition token))
    #-sbcl
    (dotimes (index (appearance-package-transition-token-expected-count token))
      (declare (ignore index))
      (bt:condition-notify
       (appearance-package-transition-token-condition token)))))

(defun current-package-appearance-catalog ()
  "Return the currently published immutable declaration catalog.

The returned catalog has no frame, port, profile, or bundle state."
  (bt:with-lock-held (*package-appearance-catalog-lock*)
    (or *package-appearance-catalog*
        (setf *package-appearance-catalog* (make-classic-appearance-catalog)))))

(defun package-appearance-current-catalog-under-lock ()
  "Return the catalog while *PACKAGE-APPEARANCE-CATALOG-LOCK* is already held."
  (or *package-appearance-catalog*
      (setf *package-appearance-catalog* (make-classic-appearance-catalog))))

(defun copy-package-appearance-id (id)
  "Copy a tagged package appearance identifier without interning any name."
  (list :package (copy-seq (second id)) (copy-seq (third id))))

(defun package-appearance-id-owner (id)
  (and (package-appearance-id-p id) (second id)))

(defun package-appearance-id-owned-by-p (id owner)
  (and (package-appearance-id-p id)
       (string= (second id) owner)))

(defun package-appearance-reject (axis value)
  (error-appearance-condition 'invalid-appearance-component
                              :origin :package-appearance
                              :axis axis :value value))

(defun require-package-appearance-id (id owner axis)
  "Return a copied package ID only when its exact spelling belongs to OWNER."
  (unless (and (package-appearance-id-p id)
               ;; PACKAGE-APPEARANCE-ID-P intentionally recognizes only the
               ;; normalized ASCII form.  Do not silently normalize input.
               (string= (second id) owner))
    (package-appearance-reject axis id))
  (copy-package-appearance-id id))

(defun package-appearance-reference-allowed-p (id owner core-p)
  (or (funcall core-p id)
      (package-appearance-id-owned-by-p id owner)))

(defun portable-appearance-leaf-p (value &optional (depth 0))
  "Return true only for bounded inert appearance data.

This is intentionally narrower than COPY-APPEARANCE-VALUE.  Package
declarations cannot smuggle functions, CLIM designs, streams, pathnames, or
other executable/backend objects through a typography or ink substructure."
  (and (<= depth 16)
       (cond
         ;; The inheritance sentinel is private immutable appearance data.  It
         ;; occurs inside otherwise portable partial typography/decoration
         ;; structures and must not be rejected merely because its
         ;; implementation representation is opaque.
         ((appearance-unspecified-p value) t)
         ((or (null value) (keywordp value) (stringp value) (numberp value)) t)
         ((or (functionp value) (pathnamep value) (streamp value)
              (typep value 'standard-object)) nil)
         ((consp value)
          (and (portable-appearance-leaf-p (car value) (1+ depth))
               (portable-appearance-leaf-p (cdr value) (1+ depth))))
         ((vectorp value)
          (every (lambda (item) (portable-appearance-leaf-p item (1+ depth))) value))
         (t nil))))

(defun package-appearance-style-portable-p (style)
  "Reject executable or backend-owned values in package style declarations."
  (and (typep style 'appearance-role-style)
       (let ((typography (appearance-role-style-typography style))
             (ink (appearance-role-style-foreground-ink style))
             (surface (appearance-role-style-surface style))
             (decoration (appearance-role-style-decoration style)))
         (and (or (appearance-unspecified-p typography)
                  (and (typep typography 'appearance-typography-spec)
                       (every #'portable-appearance-leaf-p
                              (list (appearance-typography-spec-family typography)
                                    (appearance-typography-spec-face typography)
                                    (appearance-typography-spec-size typography)))))
              (or (appearance-unspecified-p ink)
                  (and (typep ink 'appearance-ink-spec)
                       (portable-appearance-leaf-p
                        (appearance-ink-spec-foreground ink))))
              (or (appearance-unspecified-p surface)
                  (and (typep surface 'appearance-surface-spec)
                       (portable-appearance-leaf-p
                        (appearance-surface-spec-background surface))))
              (or (appearance-unspecified-p decoration)
                  (and (typep decoration 'appearance-decoration-spec)
                       (portable-appearance-leaf-p
                        (appearance-decoration-spec-kind decoration))
                       (portable-appearance-leaf-p
                        (appearance-decoration-spec-parameters decoration))))))))

(defun package-appearance-copy-role (role owner)
  (unless (typep role 'appearance-role-definition)
    (package-appearance-reject :role-definition role))
  (let ((id (require-package-appearance-id
             (appearance-role-definition-id role) owner :role-id))
        (fallback (appearance-role-definition-fallback-role role)))
    (when (and fallback
               (not (package-appearance-reference-allowed-p
                     fallback owner #'core-role-id-p)))
      (package-appearance-reject :fallback-role fallback))
    (make-appearance-role-definition
     :id id
     :kind (appearance-role-definition-kind role)
     :documentation (appearance-role-definition-documentation role)
     :fallback-role (and fallback (copy-appearance-value fallback))
     :supported-axes (appearance-role-definition-supported-axes role)
     :owner (copy-seq owner))))

(defun package-appearance-copy-theme (theme owner)
  (unless (typep theme 'appearance-theme-definition)
    (package-appearance-reject :theme-definition theme))
  (let ((id (require-package-appearance-id
             (appearance-theme-definition-id theme) owner :theme-id))
        (parent (appearance-theme-definition-parent-theme theme))
        (overlays (appearance-theme-definition-role-overlays theme)))
    (when (and parent
               (not (package-appearance-reference-allowed-p
                     parent owner #'core-theme-id-p)))
      (package-appearance-reject :parent-theme parent))
    (dolist (entry overlays)
      (unless (and (package-appearance-reference-allowed-p
                    (car entry) owner #'core-role-id-p)
                   (package-appearance-style-portable-p (cdr entry)))
        (package-appearance-reject :role-overlays entry)))
    (make-appearance-theme-definition
     :id id
     :documentation (appearance-theme-definition-documentation theme)
     :parent-theme (and parent (copy-appearance-value parent))
     :role-overlays (copy-appearance-value overlays)
     :owner (copy-seq owner))))

(defun package-appearance-copy-defaults (defaults owner)
  "Copy only portable defaults for roles owned by OWNER."
  (unless (listp defaults)
    (package-appearance-reject :package-defaults defaults))
  (let ((seen nil) (result nil))
    (dolist (entry defaults)
      (unless (and (consp entry)
                   (package-appearance-id-owned-by-p (car entry) owner)
                   (package-appearance-style-portable-p (cdr entry))
                   (not (member (car entry) seen :test #'equal)))
        (package-appearance-reject :package-defaults entry))
      (push (copy-appearance-value (car entry)) seen)
      (push (cons (copy-appearance-value (car entry))
                  (copy-appearance-value (cdr entry)))
            result))
    (nreverse result)))

(defun package-appearance-current-owner ()
  "Return the verified owner for the current entrypoint, or signal.

This deliberately requires both the package definition dynamic binding and the
resource allowlist.  Calling a registration API from an init file, hook, or a
later package callback is not an entrypoint declaration transaction."
  (let ((staging *package-appearance-entrypoint-staging*))
    (unless (and staging
                 (package-appearance-staging-definition staging)
                 *current-clawmacs-package*
                 ;; NIL is the legacy permissive policy for old resource
                 ;; categories.  Appearance declarations are deliberately
                 ;; opt-in: the package record must name :APPEARANCE itself.
                 (member :appearance *current-package-resource-types*
                         :test #'eq))
      (package-appearance-reject :package-appearance-registration :outside-entrypoint))
    (let ((canonical (package-definition-name
                      (package-appearance-staging-definition staging))))
      (unless (and (stringp canonical)
                   (string= canonical *current-clawmacs-package*)
                   (string= canonical (package-appearance-staging-owner staging))
                   (valid-package-owner-p canonical))
        (package-appearance-reject :package-owner canonical))
      canonical)))

(defun begin-package-appearance-entrypoint-staging (definition)
  "Create the batch used for one package entrypoint load."
  (let ((owner (package-definition-name definition)))
    (unless (valid-package-owner-p owner)
      (package-appearance-reject :package-owner owner))
    (make-package-appearance-staging :definition definition :owner (copy-seq owner))))

(defun register-package-appearance-declarations (&key roles themes defaults)
  "Stage immutable owner-qualified appearance declarations for this entrypoint.

All declarations in the entrypoint are validated together at commit time, so
same-owner role/theme references can be written in either source order."
  (let* ((owner (package-appearance-current-owner))
         (staging *package-appearance-entrypoint-staging*)
         (copied-roles (mapcar (lambda (role)
                                 (package-appearance-copy-role role owner))
                               (or roles nil)))
         (copied-themes (mapcar (lambda (theme)
                                  (package-appearance-copy-theme theme owner))
                                (or themes nil)))
         (copied-defaults (package-appearance-copy-defaults (or defaults nil) owner)))
    (setf (package-appearance-staging-roles staging)
          (nconc (package-appearance-staging-roles staging) copied-roles)
          (package-appearance-staging-themes staging)
          (nconc (package-appearance-staging-themes staging) copied-themes)
          (package-appearance-staging-defaults staging)
          (nconc (package-appearance-staging-defaults staging) copied-defaults))
    staging))

(defun register-package-appearance-role (role &key defaults)
  "Stage one package-owned ROLE and optional portable own-role DEFAULTS."
  (register-package-appearance-declarations :roles (list role) :defaults defaults))

(defun register-package-appearance-theme (theme)
  "Stage one package-owned THEME for the current package entrypoint."
  (register-package-appearance-declarations :themes (list theme)))

(defun register-package-appearance-defaults (defaults)
  "Stage portable default overlays for roles owned by the current package."
  (register-package-appearance-declarations :defaults defaults))

(defun package-appearance-duplicate-id-p (definitions accessor)
  (let ((seen nil))
    (some (lambda (definition)
            (let ((id (funcall accessor definition)))
              (if (member id seen :test #'equal)
                  t
                  (progn (push id seen) nil))))
          definitions)))

(defun package-appearance-candidate-catalog (owner declarations &key remove-p old-catalog)
  "Build, but never publish, OWNER's replacement declaration catalog."
  (let* ((old (or old-catalog (current-package-appearance-catalog)))
         (old-roles (appearance-catalog-role-definitions old))
         (old-themes (appearance-catalog-theme-definitions old))
         (old-defaults (appearance-catalog-built-in-overlays old))
         (owned-p (lambda (definition accessor)
                    (string= owner (funcall accessor definition))))
         (roles (remove-if (lambda (role)
                             (funcall owned-p role #'appearance-role-definition-owner))
                           old-roles))
         (themes (remove-if (lambda (theme)
                              (funcall owned-p theme #'appearance-theme-definition-owner))
                            old-themes))
         (defaults (remove-if (lambda (entry)
                                (package-appearance-id-owned-by-p (car entry) owner))
                              old-defaults)))
    (unless remove-p
      (setf roles (append roles (%package-appearance-declarations-roles declarations))
            themes (append themes (%package-appearance-declarations-themes declarations))
            defaults (append defaults (%package-appearance-declarations-defaults declarations))))
    (when (or (package-appearance-duplicate-id-p roles #'appearance-role-definition-id)
              (package-appearance-duplicate-id-p themes #'appearance-theme-definition-id))
      (package-appearance-reject :catalog-collision owner))
    (let ((catalog
            (make-appearance-catalog
             :role-definitions roles
             :theme-definitions themes
             :built-in-overlays defaults
             :generation (1+ (appearance-catalog-generation old)))))
      ;; MAKE-APPEARANCE-CATALOG validates declaration and inheritance
      ;; topology.  Overlay applicability is deliberately a resolver concern
      ;; in the pure model, so package publication must explicitly validate
      ;; every contributed overlay against the complete candidate catalog.
      (dolist (theme themes)
        (validate-appearance-overlays-for-catalog
         catalog
         (appearance-theme-definition-role-overlays theme)
         (appearance-theme-definition-id theme)))
      (validate-appearance-overlays-for-catalog
       catalog defaults :package-defaults)
      catalog)))

(defun copy-package-appearance-declaration-table (table)
  "Return a fresh owner table retaining immutable declaration values."
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (owner declarations)
               (setf (gethash owner copy) declarations))
             table)
    copy))

(defun package-appearance-declaration-tables-equal-p (left right)
  "Compare immutable owner tables without depending on hash iteration order."
  (and (= (hash-table-count left) (hash-table-count right))
       (block equal
         (maphash (lambda (owner declarations)
                    (unless (eq declarations (gethash owner right))
                      (return-from equal nil)))
                  left)
         t)))

(defun begin-package-appearance-publication-batch ()
  "Capture the immutable base for one deferred all-owner reload candidate."
  (bt:with-lock-held (*package-appearance-catalog-lock*)
    (%make-package-appearance-publication-batch
     :base-catalog (package-appearance-current-catalog-under-lock)
     :base-declarations
     (copy-package-appearance-declaration-table
      *package-appearance-declarations*)
     :replacements (make-hash-table :test #'equal))))

(defun stage-package-appearance-publication-batch
    (batch owner declarations remove-p)
  "Record OWNER's complete replacement without touching published state."
  (setf (gethash owner
                 (package-appearance-publication-batch-replacements batch))
        (unless remove-p declarations))
  batch)

(defun package-appearance-batch-candidate (batch)
  "Build and validate one catalog containing every staged owner replacement."
  (let* ((old (package-appearance-publication-batch-base-catalog batch))
         (replacements
           (package-appearance-publication-batch-replacements batch))
         (declaration-table
           (copy-package-appearance-declaration-table
            (package-appearance-publication-batch-base-declarations batch)))
         (changed-owner-p
           (lambda (owner)
             (nth-value 1 (gethash owner replacements))))
         (roles
           (remove-if
            (lambda (role)
              (funcall changed-owner-p
                       (appearance-role-definition-owner role)))
            (appearance-catalog-role-definitions old)))
         (themes
           (remove-if
            (lambda (theme)
              (funcall changed-owner-p
                       (appearance-theme-definition-owner theme)))
            (appearance-catalog-theme-definitions old)))
         (defaults
           (remove-if
            (lambda (entry)
              (funcall changed-owner-p
                       (package-appearance-id-owner (car entry))))
            (appearance-catalog-built-in-overlays old))))
    (dolist
        (owner
         (sort
          (loop :for key :being :the :hash-keys :of replacements
                :collect key)
          #'string<))
      (let ((declarations (gethash owner replacements)))
        (if declarations
            (progn
              (setf (gethash owner declaration-table) declarations
                    roles
                    (append roles
                            (%package-appearance-declarations-roles declarations))
                    themes
                    (append themes
                            (%package-appearance-declarations-themes declarations))
                    defaults
                    (append defaults
                            (%package-appearance-declarations-defaults
                             declarations))))
            (remhash owner declaration-table))))
    (when (or (package-appearance-duplicate-id-p
               roles #'appearance-role-definition-id)
              (package-appearance-duplicate-id-p
               themes #'appearance-theme-definition-id))
      (package-appearance-reject :catalog-collision :batch))
    (let ((catalog
            (make-appearance-catalog
             :role-definitions roles
             :theme-definitions themes
             :built-in-overlays defaults
             :generation (1+ (appearance-catalog-generation old)))))
      ;; Validate only after all owners are present, so the candidate graph is
      ;; admitted as one unit rather than as order-dependent intermediates.
      (dolist (theme themes)
        (validate-appearance-overlays-for-catalog
         catalog
         (appearance-theme-definition-role-overlays theme)
         (appearance-theme-definition-id theme)))
      (validate-appearance-overlays-for-catalog
       catalog defaults :package-defaults)
      (values catalog declaration-table))))

(defun package-appearance-frame-plan (frames catalog &key removing-owner)
  "Classify every FRAME before any catalog or frame state can be published."
  (let ((plans nil))
    (dolist (frame frames (nreverse plans))
      (let ((plan (funcall *appearance-package-frame-transition-planner* frame catalog)))
        (unless (and (listp plan) (member (getf plan :status)
                                          '(:ready :no-op)
                                          :test #'eq))
          (package-appearance-reject
           :frame-transition
           (list :frame frame :owner removing-owner :plan plan)))
        (push (cons frame plan) plans)))))

(defun publish-package-appearance-catalog
    (catalog plans owner declarations &key declaration-table)
  "Publish CATALOG only after complete classification, then queue frame work.

The frame publisher is the McCLIM process-boundary adapter.  It is called only
after every plan exists; failures before that point leave both registries and
frames unchanged."
  (let ((old-catalog *package-appearance-catalog*)
        (old-declaration-table *package-appearance-declarations*)
        (old-declarations (gethash owner *package-appearance-declarations*))
        (published-p nil)
        (reservations nil)
        (token (make-appearance-package-transition-token)))
    (unwind-protect
         (progn
           ;; Reserve every event/process transition before exposing the new
           ;; catalog.  Reservation is deliberately side-effect-free: no frame
           ;; can see the candidate until every live frame is admitted.
           (dolist (entry plans)
             (let ((reservation
                     (funcall *appearance-package-frame-transition-reserver*
                              (car entry) (cdr entry) catalog token)))
               (unless reservation
                 (package-appearance-reject :frame-transition (car entry)))
               (push reservation reservations)))
           (setf (appearance-package-transition-token-expected-count token)
                 (length reservations))
           (setf reservations (nreverse reservations))
           ;; Queue each frame at its owning-process barrier before exposing
           ;; any new global declaration state.
           (dolist (reservation reservations)
             (unless (funcall *appearance-package-frame-transition-publisher*
                              reservation)
               (error "Appearance catalog transition release failed.")))
           (funcall
            *appearance-package-frame-transition-finalizer*
            token reservations
            (lambda ()
              (setf *package-appearance-catalog* catalog)
              (if declaration-table
                  (setf *package-appearance-declarations* declaration-table)
                  (if declarations
                      (setf (gethash owner *package-appearance-declarations*)
                            declarations)
                      (remhash owner *package-appearance-declarations*))))
            (lambda ()
              (setf *package-appearance-catalog* old-catalog)
              (if declaration-table
                  (setf *package-appearance-declarations*
                        old-declaration-table)
                  (if old-declarations
                      (setf (gethash owner *package-appearance-declarations*)
                            old-declarations)
                      (remhash owner *package-appearance-declarations*)))))
           (funcall *appearance-package-batch-checkpoint-function*
                    reservations)
           (setf published-p t)
           catalog)
      (unless published-p
        (unless
            (member (appearance-package-transition-token-state token)
                    '(:committing :committed)
                    :test #'eq)
          ;; Only a pre-publication admission failure restores old globals.
          ;; Once COMMIT-FUNCTION ran, every prepared frame owns the immutable
          ;; target and recovery is strictly forward.
          (abort-appearance-package-transition token)
          (setf *package-appearance-catalog* old-catalog)
          (if declaration-table
              (setf *package-appearance-declarations*
                    old-declaration-table)
              (if old-declarations
                  (setf (gethash owner *package-appearance-declarations*)
                        old-declarations)
                  (remhash owner *package-appearance-declarations*))))))))

(defun commit-package-appearance-publication-batch (batch)
  "Validate and publish all deferred owner replacements as one transition."
  (check-type batch package-appearance-publication-batch)
  (bt:with-lock-held (*package-appearance-catalog-lock*)
    (unless (and
             (eq *package-appearance-catalog*
                 (package-appearance-publication-batch-base-catalog batch))
             (package-appearance-declaration-tables-equal-p
              *package-appearance-declarations*
              (package-appearance-publication-batch-base-declarations batch)))
      (package-appearance-reject :catalog-transition :stale-batch))
    (when (plusp
           (hash-table-count
            (package-appearance-publication-batch-replacements batch)))
      (multiple-value-bind (catalog declarations)
          (package-appearance-batch-candidate batch)
        (let ((plans
                (package-appearance-frame-plan
                 (funcall *appearance-package-live-frame-provider*) catalog)))
          (publish-package-appearance-catalog
           catalog plans nil nil :declaration-table declarations))))))

(defun commit-package-appearance-entrypoint-staging (staging)
  "Atomically replace one owner's declarations after its entrypoint returns."
  (when (and staging
             (or *package-appearance-entrypoint-reload-p*
                 (package-appearance-staging-roles staging)
                 (package-appearance-staging-themes staging)
                 (package-appearance-staging-defaults staging)))
    (let* ((owner (package-appearance-staging-owner staging))
           (declarations
             (%make-package-appearance-declarations
              :owner (copy-seq owner)
              :roles (copy-list (package-appearance-staging-roles staging))
              :themes (copy-list (package-appearance-staging-themes staging))
              :defaults (copy-appearance-value
                         (package-appearance-staging-defaults staging)))))
      (let ((remove-p (and *package-appearance-entrypoint-reload-p*
                           (null (package-appearance-staging-roles staging))
                           (null (package-appearance-staging-themes staging))
                           (null (package-appearance-staging-defaults staging)))))
        (if *package-appearance-publication-batch*
            (stage-package-appearance-publication-batch
             *package-appearance-publication-batch*
             owner declarations remove-p)
            (bt:with-lock-held (*package-appearance-catalog-lock*)
              (let* ((old (package-appearance-current-catalog-under-lock))
               (catalog (package-appearance-candidate-catalog
                         owner declarations :remove-p remove-p :old-catalog old))
               (plans (package-appearance-frame-plan
                       (funcall *appearance-package-live-frame-provider*) catalog)))
                (publish-package-appearance-catalog
                 catalog plans owner (unless remove-p declarations)))))))))

(defun prepare-package-appearance-removal (package)
  "Preflight PACKAGE removal without mutating its declarations or loaded state.

When a removed active theme needs :CLASSIC fallback, the frame planner must
classify that fallback as safe.  Any refusal leaves the old catalog untouched."
  (let ((owner (if (typep package 'package-definition)
                   (package-definition-name package)
                   (manifest-package-name package))))
    (when (and owner (gethash owner *package-appearance-declarations*))
      (bt:with-lock-held (*package-appearance-catalog-lock*)
        (let* ((old (package-appearance-current-catalog-under-lock))
               (catalog (package-appearance-candidate-catalog
                         owner nil :remove-p t :old-catalog old))
               (plans (package-appearance-frame-plan
                       (funcall *appearance-package-live-frame-provider*) catalog
                       :removing-owner owner)))
          (values catalog plans owner))))))

(defun commit-package-appearance-removal (catalog plans owner)
  "Publish a removal already admitted by PREPARE-PACKAGE-APPEARANCE-REMOVAL."
  (when catalog
    (bt:with-lock-held (*package-appearance-catalog-lock*)
      (publish-package-appearance-catalog catalog plans owner nil))))
