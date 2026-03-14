(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Color Spec
;;; --------------------------------------------------------------------------

(defstruct (color-spec (:constructor %make-color-spec))
  "A color specification supporting CGA (0-15), 256-color (0-255), and hex."
  (color-type :cga :type keyword :read-only t)
  (value      0    :read-only t))

(declaim (ftype (function (keyword t) color-spec) make-color-spec))
(defun make-color-spec (color-type value)
  "Create a color-spec. COLOR-TYPE is :CGA, :256, or :HEX.
VALUE is an integer (CGA/256) or a string (hex)."
  (ecase color-type
    (:cga (check-type value (integer 0 15))
          (%make-color-spec :color-type :cga :value value))
    (:256 (check-type value (integer 0 255))
          (%make-color-spec :color-type :256 :value value))
    (:hex (check-type value string)
          (%make-color-spec :color-type :hex :value value))))

(declaim (ftype (function (color-spec) keyword) color-spec-type))
(defun color-spec-type (cs)
  "Return the type of a color-spec."
  (color-spec-color-type cs))

;;; --------------------------------------------------------------------------
;;; Face
;;; --------------------------------------------------------------------------

(defclass face ()
  ((name       :initarg :name
               :reader face-name
               :type keyword)
   (foreground :initarg :foreground
               :accessor face-foreground
               :initform nil
               :type (or null color-spec))
   (background :initarg :background
               :accessor face-background
               :initform nil
               :type (or null color-spec))
   (bold-p     :initarg :bold-p
               :accessor face-bold-p
               :initform nil
               :type (or null boolean))
   (underline-p :initarg :underline-p
                :accessor face-underline-p
                :initform nil
                :type (or null boolean))
   (reverse-p  :initarg :reverse-p
               :accessor face-reverse-p
               :initform nil
               :type (or null boolean))
   (parent     :initarg :parent
               :accessor face-parent
               :initform nil
               :type (or null face))
   (transform  :initarg :transform
               :accessor face-transform
               :initform nil
               :documentation "Stub for future color transforms. Ignored by resolve-face."))
  (:documentation "A named set of visual attributes with optional parent for inheritance."))

;;; --------------------------------------------------------------------------
;;; Resolved Face
;;; --------------------------------------------------------------------------

(defstruct resolved-face
  "A fully-resolved face with no nil attributes. Result of resolve-face."
  (foreground  (error "foreground required") :type color-spec :read-only t)
  (background  (error "background required") :type color-spec :read-only t)
  (bold-p      nil                           :type boolean    :read-only t)
  (underline-p nil                           :type boolean    :read-only t)
  (reverse-p   nil                           :type boolean    :read-only t))

(declaim (ftype (function (face) resolved-face) resolve-face))
(defun resolve-face (face)
  "Walk the parent chain of FACE, filling in nil attributes from ancestors.
Returns a RESOLVED-FACE with all attributes set. Ignores transform specs.
nil for boolean slots means 'inherit from parent'."
  (let (foreground background bold-p underline-p reverse-p
        (bold-set nil) (underline-set nil) (reverse-set nil))
    (loop :for current := face :then (face-parent current)
          :while current
          :do (when (and (null foreground) (face-foreground current))
                (setf foreground (face-foreground current)))
              (when (and (null background) (face-background current))
                (setf background (face-background current)))
              (when (and (not bold-set) (not (null (slot-value current 'bold-p))))
                (setf bold-p (face-bold-p current)
                      bold-set t))
              (when (and (not underline-set) (not (null (slot-value current 'underline-p))))
                (setf underline-p (face-underline-p current)
                      underline-set t))
              (when (and (not reverse-set) (not (null (slot-value current 'reverse-p))))
                (setf reverse-p (face-reverse-p current)
                      reverse-set t)))
    (make-resolved-face :foreground foreground
                        :background background
                        :bold-p (or bold-p nil)
                        :underline-p (or underline-p nil)
                        :reverse-p (or reverse-p nil))))

;;; --------------------------------------------------------------------------
;;; Face Set
;;; --------------------------------------------------------------------------

(defclass face-set ()
  ((owner :initarg :owner
          :reader face-set-owner
          :type keyword)
   (faces :initarg :faces
          :reader face-set-faces
          :type hash-table))
  (:documentation "A collection of named faces belonging to a sender."))

(declaim (ftype (function (keyword list) face-set) make-face-set))
(defun make-face-set (owner face-list)
  "Create a face-set for OWNER from a list of face objects.
Each face is stored by its name in a hash-table."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (f face-list)
      (setf (gethash (face-name f) ht) f))
    (make-instance 'face-set :owner owner :faces ht)))

(declaim (ftype (function (face-set keyword) (or null face)) get-face))
(defun get-face (face-set name)
  "Look up a face by NAME in FACE-SET. Returns nil if not found."
  (gethash name (face-set-faces face-set)))
