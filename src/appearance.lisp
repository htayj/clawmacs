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

(define-condition appearance-condition (error)
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
  (:documentation "Base condition for a rejected appearance declaration."))

(define-condition unknown-appearance-role (appearance-condition) ())
(define-condition invalid-appearance-component (appearance-condition) ())
(define-condition appearance-theme-cycle (appearance-condition) ())
(define-condition appearance-role-cycle (appearance-condition) ())
(define-condition missing-appearance-parent (appearance-condition) ())
(define-condition unsupported-role-axis (appearance-condition) ())
(define-condition ambiguous-font-family (appearance-condition) ())
(define-condition ambiguous-font-face (appearance-condition) ())
(define-condition font-unavailable (appearance-condition) ())
(define-condition font-size-unavailable (appearance-condition) ())
(define-condition relative-size-base-invalid (appearance-condition) ())
(define-condition device-font-port-mismatch (appearance-condition) ())
(define-condition appearance-contrast-warning (appearance-condition) ())
(define-condition appearance-live-update-unsupported (appearance-condition) ())
(define-condition appearance-activation-failed (appearance-condition) ())

(defstruct (appearance-typography-spec
            (:constructor make-appearance-typography-spec
                (&key (family *appearance-unspecified*)
                      (face *appearance-unspecified*)
                      (size *appearance-unspecified*))))
  "Partial typography declaration.  Each component inherits independently."
  (family *appearance-unspecified* :read-only t)
  (face *appearance-unspecified* :read-only t)
  (size *appearance-unspecified* :read-only t))

(defstruct (appearance-ink-spec
            (:constructor make-appearance-ink-spec
                (&key (foreground *appearance-unspecified*))))
  "Partial foreground-ink declaration."
  (foreground *appearance-unspecified* :read-only t))

(defstruct (appearance-surface-spec
            (:constructor make-appearance-surface-spec
                (&key (background *appearance-unspecified*))))
  "Partial surface/background declaration."
  (background *appearance-unspecified* :read-only t))

(defstruct (appearance-decoration-spec
            (:constructor make-appearance-decoration-spec
                (&key (kind *appearance-unspecified*)
                      (parameters *appearance-unspecified*))))
  "Small tagged decoration policy; arbitrary drawing recipes are not accepted."
  (kind *appearance-unspecified* :read-only t)
  (parameters *appearance-unspecified* :read-only t))

(defstruct (appearance-role-style
            (:constructor make-appearance-role-style
                (&key (typography *appearance-unspecified*)
                      (foreground-ink *appearance-unspecified*)
                      (surface *appearance-unspecified*)
                      (decoration *appearance-unspecified*))))
  "Partial, orthogonal appearance axes for one semantic role."
  (typography *appearance-unspecified* :read-only t)
  (foreground-ink *appearance-unspecified* :read-only t)
  (surface *appearance-unspecified* :read-only t)
  (decoration *appearance-unspecified* :read-only t))

(defstruct (appearance-role-definition
            (:constructor make-appearance-role-definition
                (&key id kind documentation fallback-role
                      (supported-axes *appearance-unspecified*) owner)))
  "Immutable semantic role declaration owned by the catalog, not a frame."
  id
  kind
  documentation
  fallback-role
  (supported-axes *appearance-unspecified* :read-only t)
  owner)

(defstruct (appearance-theme-definition
            (:constructor make-appearance-theme-definition
                (&key id documentation parent-theme role-overlays owner)))
  "Immutable coordinated collection of partial role overlays."
  id
  documentation
  parent-theme
  (role-overlays nil :type list :read-only t)
  owner)

(defstruct (appearance-profile
            (:constructor make-appearance-profile
                (&key (selected-theme :classic) (role-overrides nil))))
  "Immutable frame-selectable profile; it contains no resolved CLIM objects."
  (selected-theme :classic :read-only t)
  (role-overrides nil :type list :read-only t))

(defun appearance-role-supported-axes (role-definition)
  "Return ROLE-DEFINITION's accepted axes in stable order."
  (or (unless (appearance-unspecified-p
               (appearance-role-definition-supported-axes role-definition))
        (appearance-role-definition-supported-axes role-definition))
      (cdr (assoc (appearance-role-definition-kind role-definition)
                  +appearance-role-axes+))))

(defun appearance-role-supports-axis-p (role-definition axis)
  "Return true when AXIS is meaningful for ROLE-DEFINITION."
  (member axis (appearance-role-supported-axes role-definition)))

(defun validate-appearance-role-style (role-definition style &key origin)
  "Validate STYLE's axis applicability for ROLE-DEFINITION and return STYLE."
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
