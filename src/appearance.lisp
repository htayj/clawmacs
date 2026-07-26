(in-package :clawmacs)

;;;; Appearance declarations and resolution
;;;;
;;;; This file deliberately contains no drawing, frame mutation, or configuration
;;;; I/O.  It describes immutable appearance data which later UI code resolves at
;;;; a frame and port boundary.

(defparameter *appearance-unspecified* (gensym "APPEARANCE-UNSPECIFIED-"))

(defun appearance-unspecified-p (value)
  "Return true when VALUE is the internal inheritance sentinel."
  (eq value *appearance-unspecified*))

(defparameter +appearance-logical-sizes+
  '(:tiny :very-small :small :normal :large :very-large :huge))

(defparameter +appearance-relative-sizes+ '(:smaller :larger))

(defparameter +appearance-role-kinds+ '(:surface :content :state))

(defparameter +appearance-role-axes+
  '((:surface . (:typography :foreground-ink :surface :decoration))
    (:content . (:typography :foreground-ink :decoration))
    (:state . (:typography :foreground-ink :decoration))))

(defun appearance-logical-sizes ()
  "Return a fresh stable-order copy of supported logical text sizes."
  (copy-list +appearance-logical-sizes+))

(defun appearance-relative-sizes ()
  "Return a fresh stable-order copy of supported relative text sizes."
  (copy-list +appearance-relative-sizes+))

(defun appearance-role-kinds ()
  "Return a fresh stable-order copy of supported role kinds."
  (copy-list +appearance-role-kinds+))

(define-condition appearance-condition (condition)
  ((origin :initarg :origin :reader appearance-condition-origin :initform nil)
   (role :initarg :role :reader appearance-condition-role :initform nil)
   (axis :initarg :axis :reader appearance-condition-axis :initform nil)
   (value :initarg :value :reader appearance-condition-value :initform nil)
   (path :initarg :path :reader appearance-condition-path :initform nil)
   (port :initarg :port :reader appearance-condition-port :initform nil)
   (available-choices :initarg :available-choices
                      :reader appearance-condition-available-choices
                      :initform nil)
   (fatal-p :initarg :fatal-p :reader appearance-condition-fatal-p :initform t)
   (suggested-repairs :initarg :suggested-repairs
                      :reader appearance-condition-suggested-repairs
                      :initform nil))
  (:documentation "Base condition for an appearance diagnostic.")
  (:report (lambda (condition stream)
             (format stream "Appearance condition~@[ for role ~S~]~@[ at axis ~S~]: ~S"
                     (appearance-condition-role condition)
                     (appearance-condition-axis condition)
                     (appearance-condition-value condition)))))

(defmethod initialize-instance :after ((condition appearance-condition) &key)
  "Defensively copy diagnostic payloads before a condition becomes observable."
  (dolist (slot '(origin role axis value path port available-choices
                  suggested-repairs))
    (setf (slot-value condition slot)
          (copy-appearance-value (slot-value condition slot)))))

(defmethod appearance-condition-origin :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-role :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-axis :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-value :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-path :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-port :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-available-choices :around
    ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-suggested-repairs :around
    ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(define-condition appearance-error (error appearance-condition) ())

(define-condition unknown-appearance-role (appearance-error) ())
(define-condition invalid-appearance-component (appearance-error) ())
(define-condition appearance-theme-cycle (appearance-error) ())
(define-condition appearance-role-cycle (appearance-error) ())
(define-condition missing-appearance-parent (appearance-error) ())
(define-condition unsupported-role-axis (appearance-error) ())
(define-condition ambiguous-font-family (appearance-error) ())
(define-condition ambiguous-font-face (appearance-error) ())
(define-condition font-unavailable (appearance-error) ())
(define-condition font-size-unavailable (appearance-error) ())
(define-condition relative-size-base-invalid (appearance-error) ())
(define-condition device-font-port-mismatch (appearance-error) ())
(define-condition appearance-contrast-warning (warning appearance-condition) ())
(define-condition appearance-live-update-unsupported (appearance-error) ())
(define-condition appearance-activation-failed (appearance-error) ())

(defun copy-appearance-value (value)
  "Return a deep copy of VALUE's mutable Common Lisp containers."
  (typecase value
    (string (copy-seq value))
    (cons (cons (copy-appearance-value (car value))
                (copy-appearance-value (cdr value))))
    (vector (map 'vector #'copy-appearance-value value))
    (hash-table
     (let ((copy (make-hash-table :test (hash-table-test value)
                                  :size (hash-table-count value))))
       (maphash (lambda (key entry)
                  (setf (gethash (copy-appearance-value key) copy)
                        (copy-appearance-value entry)))
                value)
       copy))
    (t value)))

(defun validate-appearance-component (value axis)
  "Reject NIL as an appearance style component and return VALUE."
  (when (null value)
    (error 'invalid-appearance-component :axis axis :value value))
  value)

(defstruct (appearance-typography-spec
            (:constructor %make-appearance-typography-spec
                (&key (family *appearance-unspecified*)
                      (face *appearance-unspecified*)
                      (size *appearance-unspecified*)))
            (:conc-name %appearance-typography-spec-))
  "Partial typography declaration.  Each component inherits independently."
  (family *appearance-unspecified* :read-only t)
  (face *appearance-unspecified* :read-only t)
  (size *appearance-unspecified* :read-only t))

(defstruct (appearance-ink-spec
            (:constructor %make-appearance-ink-spec
                (&key (foreground *appearance-unspecified*)))
            (:conc-name %appearance-ink-spec-))
  "Partial foreground-ink declaration."
  (foreground *appearance-unspecified* :read-only t))

(defstruct (appearance-surface-spec
            (:constructor %make-appearance-surface-spec
                (&key (background *appearance-unspecified*)))
            (:conc-name %appearance-surface-spec-))
  "Partial surface/background declaration."
  (background *appearance-unspecified* :read-only t))

(defstruct (appearance-decoration-spec
            (:constructor %make-appearance-decoration-spec
                (&key (kind *appearance-unspecified*)
                      (parameters *appearance-unspecified*)))
            (:conc-name %appearance-decoration-spec-))
  "Small tagged decoration policy; arbitrary drawing recipes are not accepted."
  (kind *appearance-unspecified* :read-only t)
  (parameters *appearance-unspecified* :read-only t))

(defstruct (appearance-role-style
            (:constructor %make-appearance-role-style
                (&key (typography *appearance-unspecified*)
                      (foreground-ink *appearance-unspecified*)
                      (surface *appearance-unspecified*)
                      (decoration *appearance-unspecified*)))
            (:conc-name %appearance-role-style-))
  "Partial, orthogonal appearance axes for one semantic role."
  (typography *appearance-unspecified* :read-only t)
  (foreground-ink *appearance-unspecified* :read-only t)
  (surface *appearance-unspecified* :read-only t)
  (decoration *appearance-unspecified* :read-only t))

(defstruct (appearance-role-definition
            (:constructor %make-appearance-role-definition
                (&key id kind documentation fallback-role
                      (supported-axes *appearance-unspecified*) owner))
            (:conc-name %appearance-role-definition-))
  "Immutable semantic role declaration owned by the catalog, not a frame."
  id
  kind
  documentation
  fallback-role
  (supported-axes *appearance-unspecified* :read-only t)
  owner)

(defstruct (appearance-theme-definition
            (:constructor %make-appearance-theme-definition
                (&key id documentation parent-theme role-overlays owner))
            (:conc-name %appearance-theme-definition-))
  "Immutable coordinated collection of partial role overlays."
  id
  documentation
  parent-theme
  (role-overlays nil :type list :read-only t)
  owner)

(defstruct (appearance-profile
            (:constructor %make-appearance-profile
                (&key (selected-theme :classic) (strict-contrast nil)
                      (role-overrides nil)))
            (:conc-name %appearance-profile-))
  "Immutable frame-selectable profile; it contains no resolved CLIM objects."
  (selected-theme :classic :read-only t)
  (strict-contrast nil :read-only t)
  (role-overrides nil :type list :read-only t))

(defun validate-appearance-leaf (value axis &key allow-none-p)
  "Return VALUE when it is a permitted explicit leaf for AXIS."
  (validate-appearance-component value axis)
  (when (and (eq value :none) (not allow-none-p))
    (error 'invalid-appearance-component :axis axis :value value))
  value)

(defun make-appearance-typography-spec
    (&key (family *appearance-unspecified*) (face *appearance-unspecified*)
       (size *appearance-unspecified*))
  "Construct an immutable partial typography declaration."
  (%make-appearance-typography-spec
   :family (if (appearance-unspecified-p family)
               *appearance-unspecified*
               (copy-appearance-value
                (validate-appearance-leaf family :typography-family)))
   :face (if (appearance-unspecified-p face)
             *appearance-unspecified*
             (copy-appearance-value
              (validate-appearance-leaf face :typography-face)))
   :size (if (appearance-unspecified-p size)
             *appearance-unspecified*
             (copy-appearance-value
              (validate-appearance-leaf size :typography-size)))))

(defun appearance-typography-spec-family (spec)
  (copy-appearance-value (%appearance-typography-spec-family spec)))

(defun appearance-typography-spec-face (spec)
  (copy-appearance-value (%appearance-typography-spec-face spec)))

(defun appearance-typography-spec-size (spec)
  (copy-appearance-value (%appearance-typography-spec-size spec)))

(defun make-appearance-ink-spec (&key (foreground *appearance-unspecified*))
  "Construct an immutable partial foreground declaration."
  (%make-appearance-ink-spec
   :foreground (if (appearance-unspecified-p foreground)
                   *appearance-unspecified*
                   (copy-appearance-value
                    (validate-appearance-leaf foreground :foreground-ink)))))

(defun appearance-ink-spec-foreground (spec)
  (copy-appearance-value (%appearance-ink-spec-foreground spec)))

(defun make-appearance-surface-spec (&key (background *appearance-unspecified*))
  "Construct an immutable partial surface declaration."
  (%make-appearance-surface-spec
   :background (if (appearance-unspecified-p background)
                   *appearance-unspecified*
                   (copy-appearance-value
                    (validate-appearance-leaf background :surface)))))

(defun appearance-surface-spec-background (spec)
  (copy-appearance-value (%appearance-surface-spec-background spec)))

(defun make-appearance-decoration-spec
    (&key (kind *appearance-unspecified*) (parameters *appearance-unspecified*))
  "Construct an immutable decoration declaration with no executable payload."
  (%make-appearance-decoration-spec
   :kind (if (appearance-unspecified-p kind)
             *appearance-unspecified*
             (copy-appearance-value
              (validate-appearance-leaf kind :decoration :allow-none-p t)))
   :parameters (if (appearance-unspecified-p parameters)
                   *appearance-unspecified*
                   (copy-appearance-value
                    (validate-appearance-leaf parameters :decoration-parameters)))))

(defun appearance-decoration-spec-kind (spec)
  (copy-appearance-value (%appearance-decoration-spec-kind spec)))

(defun appearance-decoration-spec-parameters (spec)
  (copy-appearance-value (%appearance-decoration-spec-parameters spec)))

(defun validate-appearance-style-axis (value expected-type axis)
  "Validate VALUE for one role-style AXIS and return an immutable copy."
  (cond
    ((appearance-unspecified-p value) value)
    ((typep value expected-type) value)
    (t (error 'invalid-appearance-component :axis axis :value value))))

(defun make-appearance-role-style
    (&key (typography *appearance-unspecified*)
       (foreground-ink *appearance-unspecified*)
       (surface *appearance-unspecified*)
       (decoration *appearance-unspecified*))
  "Construct an immutable orthogonal role style from typed axis specifications."
  (%make-appearance-role-style
   :typography (validate-appearance-style-axis typography
                                              'appearance-typography-spec
                                              :typography)
   :foreground-ink (validate-appearance-style-axis foreground-ink
                                                   'appearance-ink-spec
                                                   :foreground-ink)
   :surface (validate-appearance-style-axis surface 'appearance-surface-spec
                                             :surface)
   :decoration (validate-appearance-style-axis decoration
                                                'appearance-decoration-spec
                                                :decoration)))

(defun appearance-role-style-typography (style)
  (%appearance-role-style-typography style))

(defun appearance-role-style-foreground-ink (style)
  (%appearance-role-style-foreground-ink style))

(defun appearance-role-style-surface (style)
  (%appearance-role-style-surface style))

(defun appearance-role-style-decoration (style)
  (%appearance-role-style-decoration style))

(defun validate-appearance-role-kind (kind)
  "Return KIND if it is one of the fixed semantic role kinds."
  (unless (member kind +appearance-role-kinds+ :test #'eq)
    (error 'invalid-appearance-component :axis :role-kind :value kind))
  kind)

(defun validate-appearance-supported-axes (kind supported-axes)
  "Validate an explicit supported-axis set for KIND and return a copy."
  (when (and (not (appearance-unspecified-p supported-axes))
             (or (not (listp supported-axes))
                 (not (every (lambda (axis)
                               (member axis (cdr (assoc kind
                                                        +appearance-role-axes+))
                                       :test #'eq))
                             supported-axes))
                 (/= (length supported-axes)
                     (length (remove-duplicates supported-axes :test #'eq)))))
    (error 'invalid-appearance-component :axis :supported-axes
           :value supported-axes))
  (copy-appearance-value supported-axes))

(defun make-appearance-role-definition
    (&key id kind documentation fallback-role
       (supported-axes *appearance-unspecified*) owner)
  "Construct an immutable semantic role declaration with validated kind/axes."
  (validate-appearance-component id :role-id)
  (validate-appearance-role-kind kind)
  (%make-appearance-role-definition
   :id (copy-appearance-value id)
   :kind kind
   :documentation (copy-appearance-value documentation)
   :fallback-role (copy-appearance-value fallback-role)
   :supported-axes (validate-appearance-supported-axes kind supported-axes)
   :owner (copy-appearance-value owner)))

(defun appearance-role-definition-id (definition)
  (copy-appearance-value (%appearance-role-definition-id definition)))

(defun appearance-role-definition-kind (definition)
  (%appearance-role-definition-kind definition))

(defun appearance-role-definition-documentation (definition)
  (copy-appearance-value (%appearance-role-definition-documentation definition)))

(defun appearance-role-definition-fallback-role (definition)
  (copy-appearance-value (%appearance-role-definition-fallback-role definition)))

(defun appearance-role-definition-supported-axes (definition)
  (copy-appearance-value (%appearance-role-definition-supported-axes definition)))

(defun appearance-role-definition-owner (definition)
  (copy-appearance-value (%appearance-role-definition-owner definition)))

(defun validate-appearance-overlays (overlays axis)
  "Validate a role-to-style overlay alist and return a deep immutable copy."
  (unless (listp overlays)
    (error 'invalid-appearance-component :axis axis :value overlays))
  (let ((seen nil))
    (dolist (entry overlays)
      (unless (and (consp entry)
                   (not (null (car entry)))
                   (typep (cdr entry) 'appearance-role-style)
                   (not (member (car entry) seen :test #'equal)))
        (error 'invalid-appearance-component :axis axis :value entry))
      (push (car entry) seen)))
  (copy-appearance-value overlays))

(defun make-appearance-theme-definition
    (&key id documentation parent-theme role-overlays owner)
  "Construct an immutable theme declaration without resolving its references."
  (validate-appearance-component id :theme-id)
  (%make-appearance-theme-definition
   :id (copy-appearance-value id)
   :documentation (copy-appearance-value documentation)
   :parent-theme (copy-appearance-value parent-theme)
   :role-overlays (validate-appearance-overlays role-overlays :role-overlays)
   :owner (copy-appearance-value owner)))

(defun appearance-theme-definition-id (definition)
  (copy-appearance-value (%appearance-theme-definition-id definition)))

(defun appearance-theme-definition-documentation (definition)
  (copy-appearance-value (%appearance-theme-definition-documentation definition)))

(defun appearance-theme-definition-parent-theme (definition)
  (copy-appearance-value (%appearance-theme-definition-parent-theme definition)))

(defun appearance-theme-definition-role-overlays (definition)
  (copy-appearance-value (%appearance-theme-definition-role-overlays definition)))

(defun appearance-theme-definition-owner (definition)
  (copy-appearance-value (%appearance-theme-definition-owner definition)))

(defun make-appearance-profile
    (&key (selected-theme :classic) (strict-contrast nil) (role-overrides nil))
  "Construct an immutable frame profile with an explicit contrast policy."
  (validate-appearance-component selected-theme :selected-theme)
  (unless (typep strict-contrast 'boolean)
    (error 'invalid-appearance-component :axis :strict-contrast
           :value strict-contrast))
  (%make-appearance-profile
   :selected-theme (copy-appearance-value selected-theme)
   :strict-contrast strict-contrast
   :role-overrides (validate-appearance-overlays role-overrides :role-overrides)))

(defun appearance-profile-selected-theme (profile)
  (copy-appearance-value (%appearance-profile-selected-theme profile)))

(defun appearance-profile-strict-contrast (profile)
  (%appearance-profile-strict-contrast profile))

(defun appearance-profile-role-overrides (profile)
  (copy-appearance-value (%appearance-profile-role-overrides profile)))

(defun appearance-role-supported-axes (role-definition)
  "Return ROLE-DEFINITION's accepted axes in stable order."
  (or (unless (appearance-unspecified-p
               (appearance-role-definition-supported-axes role-definition))
        (appearance-role-definition-supported-axes role-definition))
      (cdr (assoc (appearance-role-definition-kind role-definition)
                  +appearance-role-axes+))))

(defun appearance-role-supports-axis-p (role-definition axis)
  "Return true when AXIS is meaningful for ROLE-DEFINITION."
  (not (null (member axis (appearance-role-supported-axes role-definition)
                     :test #'eq))))

(defun validate-appearance-role-style (role-definition style &key origin)
  "Validate STYLE's axis applicability for ROLE-DEFINITION and return STYLE."
  (unless (typep role-definition 'appearance-role-definition)
    (error 'invalid-appearance-component :origin origin :axis :role-definition
           :value role-definition))
  (unless (typep style 'appearance-role-style)
    (error 'invalid-appearance-component :origin origin :axis :role-style
           :value style))
  (dolist (axis '((:typography . appearance-role-style-typography)
                  (:foreground-ink . appearance-role-style-foreground-ink)
                  (:surface . appearance-role-style-surface)
                  (:decoration . appearance-role-style-decoration)))
    (unless (appearance-unspecified-p
             (funcall (cdr axis) style))
      (unless (appearance-role-supports-axis-p role-definition (car axis))
        (error 'unsupported-role-axis
               :origin origin
               :role (appearance-role-definition-id role-definition)
               :axis (car axis)
               :value (funcall (cdr axis) style)))))
  style)
