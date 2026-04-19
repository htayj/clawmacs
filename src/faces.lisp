(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; CLIM Inks
;;; --------------------------------------------------------------------------

(defstruct (color-spec (:constructor %make-color-spec))
  "Legacy color specification. New code should store CLIM inks directly."
  (color-type :cga :type keyword :read-only t)
  (value      0    :read-only t))

(declaim (ftype (function (keyword t) color-spec) make-color-spec))
(defun make-color-spec (color-type value)
  "Create a legacy color-spec. Prefer MAKE-CLIM-INK for new code."
  (ecase color-type
    (:cga (check-type value (integer 0 15))
          (%make-color-spec :color-type :cga :value value))
    (:256 (check-type value (integer 0 255))
          (%make-color-spec :color-type :256 :value value))
    (:hex (check-type value string)
          (%make-color-spec :color-type :hex :value value))))

(declaim (ftype (function (color-spec) keyword) color-spec-type))
(defun color-spec-type (cs)
  "Return the type of a legacy color-spec."
  (color-spec-color-type cs))

(defun make-cga-ink (value)
  "Return a CLIM ink for a CGA color VALUE in the range 0-15."
  (check-type value (integer 0 15))
  (case value
    (0  (clim:make-rgb-color 0.0 0.0 0.0))
    (1  (clim:make-rgb-color 0.67 0.0 0.0))
    (2  (clim:make-rgb-color 0.0 0.67 0.0))
    (3  (clim:make-rgb-color 0.67 0.33 0.0))
    (4  (clim:make-rgb-color 0.0 0.0 0.67))
    (5  (clim:make-rgb-color 0.67 0.0 0.67))
    (6  (clim:make-rgb-color 0.0 0.67 0.67))
    (7  (clim:make-rgb-color 0.67 0.67 0.67))
    (8  (clim:make-rgb-color 0.33 0.33 0.33))
    (9  (clim:make-rgb-color 1.0 0.33 0.33))
    (10 (clim:make-rgb-color 0.33 1.0 0.33))
    (11 (clim:make-rgb-color 1.0 1.0 0.33))
    (12 (clim:make-rgb-color 0.33 0.33 1.0))
    (13 (clim:make-rgb-color 1.0 0.33 1.0))
    (14 (clim:make-rgb-color 0.33 1.0 1.0))
    (15 (clim:make-rgb-color 1.0 1.0 1.0))))

(defun make-256-color-ink (value)
  "Return a CLIM ink for an xterm 256-color palette VALUE."
  (check-type value (integer 0 255))
  (cond
    ((< value 16)
     (make-cga-ink value))
    ((< value 232)
     (let* ((idx (- value 16))
            (r-idx (floor idx 36))
            (g-idx (floor (mod idx 36) 6))
            (b-idx (mod idx 6))
            (r (if (zerop r-idx) 0.0 (/ (+ 55 (* 40 r-idx)) 255.0)))
            (g (if (zerop g-idx) 0.0 (/ (+ 55 (* 40 g-idx)) 255.0)))
            (b (if (zerop b-idx) 0.0 (/ (+ 55 (* 40 b-idx)) 255.0))))
       (clim:make-rgb-color r g b)))
    (t
     (let ((level (/ (+ 8 (* 10 (- value 232))) 255.0)))
       (clim:make-rgb-color level level level)))))

(defun make-hex-ink (hex)
  "Return a CLIM ink for HEX in #RRGGBB format."
  (check-type hex string)
  (unless (and (= (length hex) 7) (char= (char hex 0) #\#))
    (error "Expected hex color in #RRGGBB format, got ~S" hex))
  (let ((r (/ (parse-integer hex :start 1 :end 3 :radix 16) 255.0))
        (g (/ (parse-integer hex :start 3 :end 5 :radix 16) 255.0))
        (b (/ (parse-integer hex :start 5 :end 7 :radix 16) 255.0)))
    (clim:make-rgb-color r g b)))

(defun make-clim-ink (color-type value)
  "Return a CLIM ink from a compact color specification.
COLOR-TYPE is :CGA, :256, or :HEX. New style definitions should store the
returned ink directly instead of storing COLOR-SPEC objects."
  (ecase color-type
    (:cga (make-cga-ink value))
    (:256 (make-256-color-ink value))
    (:hex (make-hex-ink value))))

(defun legacy-color-spec-to-ink (color-spec)
  "Convert a legacy COLOR-SPEC to a CLIM ink."
  (make-clim-ink (color-spec-type color-spec)
                 (color-spec-value color-spec)))

(defun coerce-clim-ink (value)
  "Accept a CLIM ink directly, converting legacy color-specs for init files."
  (cond
    ((null value) nil)
    ((typep value 'color-spec) (legacy-color-spec-to-ink value))
    (t value)))

;;; --------------------------------------------------------------------------
;;; CLIM Text Styles and Drawing Options
;;; --------------------------------------------------------------------------

(defun make-clim-text-style (family face size)
  "Return a CLIM text-style using portable or McCLIM-extended arguments."
  (clim:make-text-style family face size))

(defun normalize-text-style (value)
  "Accept NIL, a CLIM text-style, or a three-item MAKE-TEXT-STYLE argument list."
  (cond
    ((null value) nil)
    ((and (listp value) (= (length value) 3))
     (apply #'make-clim-text-style value))
    (t value)))

(defun default-clawmacs-text-style ()
  "Return the default Clawmacs text style."
  (make-clim-text-style :fix :roman :normal))

(defun bold-clawmacs-text-style ()
  "Return the default bold Clawmacs text style."
  (make-clim-text-style :fix :bold :normal))

(defun normalize-drawing-options (options)
  "Return a copy of OPTIONS after validating that it is a plist or NIL."
  (when options
    (unless (and (listp options) (evenp (length options)))
      (error "Drawing options must be a plist, got ~S" options)))
  (copy-list options))

(defun plist-has-key-p (plist key)
  "Return true if PLIST contains KEY."
  (loop :for tail :on plist :by #'cddr
        :thereis (eq (first tail) key)))

(defun merge-drawing-option-plists (child parent)
  "Merge CHILD and PARENT drawing option plists.
Keys in CHILD override keys in PARENT."
  (let ((result (copy-list child)))
    (loop :for (key value) :on parent :by #'cddr
          :unless (plist-has-key-p result key)
            :do (setf (getf result key) value))
    result))

;;; --------------------------------------------------------------------------
;;; Drawing Styles
;;; --------------------------------------------------------------------------

(defclass drawing-style ()
  ((name :initarg :name
         :reader drawing-style-name
         :type keyword
         :documentation "Keyword identifying this drawing style.")
   (ink :initarg :ink
        :initarg :foreground
        :accessor %drawing-style-ink
        :initform nil
        :documentation "Foreground CLIM ink, or nil to inherit from parent.")
   (background-ink :initarg :background-ink
                   :initarg :background
                   :accessor %drawing-style-background-ink
                   :initform nil
                   :documentation "Background CLIM ink used for row fills, or nil to inherit.")
   (text-style :initarg :text-style
               :accessor %drawing-style-text-style
               :initform nil
               :documentation "CLIM text-style, or nil to inherit from parent.")
   (drawing-options :initarg :drawing-options
                    :accessor %drawing-style-drawing-options
                    :initform nil
                    :documentation "Additional CLIM drawing option plist.")
   (bold-p :initarg :bold-p
           :accessor %drawing-style-bold-p
           :initform nil
           :documentation "Legacy compatibility. Prefer TEXT-STYLE.")
   (underline-p :initarg :underline-p
                :accessor %drawing-style-underline-p
                :initform nil
                :documentation "Clawmacs underline flag; CLIM text-style has no portable underline face.")
   (reverse-p :initarg :reverse-p
              :accessor %drawing-style-reverse-p
              :initform nil
              :documentation "Legacy compatibility. Prefer explicit inks.")
   (parent :initarg :parent
           :accessor drawing-style-parent
           :initform nil
           :documentation "Parent drawing-style for inheritance.")
   (transform :initarg :transform
              :accessor drawing-style-transform
              :initform nil
              :documentation "Legacy compatibility slot. Ignored by resolution."))
  (:documentation
   "A named CLIM drawing configuration.
Drawing styles store CLIM inks, text styles, and drawing-options directly.
They intentionally do not redefine CLIM's text-style, ink, or medium concepts."))

(defclass face (drawing-style) ()
  (:documentation "Compatibility name for older init files. Use DRAWING-STYLE."))

(defmethod initialize-instance :after ((style drawing-style) &key)
  (setf (%drawing-style-ink style)
        (coerce-clim-ink (%drawing-style-ink style))
        (%drawing-style-background-ink style)
        (coerce-clim-ink (%drawing-style-background-ink style))
        (%drawing-style-text-style style)
        (normalize-text-style (%drawing-style-text-style style))
        (%drawing-style-drawing-options style)
        (normalize-drawing-options
         (%drawing-style-drawing-options style))))

(defun make-drawing-style (name &rest initargs)
  "Create a DRAWING-STYLE named NAME."
  (apply #'make-instance 'drawing-style :name name initargs))

(defun drawing-style-ink (style)
  "Return STYLE's foreground CLIM ink, or NIL if inherited."
  (%drawing-style-ink style))

(defun (setf drawing-style-ink) (value style)
  (setf (%drawing-style-ink style) (coerce-clim-ink value)))

(defun drawing-style-background-ink (style)
  "Return STYLE's background CLIM ink, or NIL if inherited."
  (%drawing-style-background-ink style))

(defun (setf drawing-style-background-ink) (value style)
  (setf (%drawing-style-background-ink style) (coerce-clim-ink value)))

(defun drawing-style-text-style (style)
  "Return STYLE's CLIM text-style, or NIL if inherited."
  (%drawing-style-text-style style))

(defun (setf drawing-style-text-style) (value style)
  (setf (%drawing-style-text-style style) (normalize-text-style value)))

(defun drawing-style-drawing-options (style)
  "Return STYLE's additional CLIM drawing option plist."
  (%drawing-style-drawing-options style))

(defun (setf drawing-style-drawing-options) (value style)
  (setf (%drawing-style-drawing-options style)
        (normalize-drawing-options value)))

(defun drawing-style-underline-p (style)
  "Return STYLE's Clawmacs underline flag, or NIL if inherited."
  (%drawing-style-underline-p style))

(defun (setf drawing-style-underline-p) (value style)
  (setf (%drawing-style-underline-p style) value))

;;; Legacy face accessors. These map to CLIM-native slots.

(defun face-name (style)
  (drawing-style-name style))

(defun face-foreground (style)
  (drawing-style-ink style))

(defun (setf face-foreground) (value style)
  (setf (drawing-style-ink style) value))

(defun face-background (style)
  (drawing-style-background-ink style))

(defun (setf face-background) (value style)
  (setf (drawing-style-background-ink style) value))

(defun face-bold-p (style)
  (%drawing-style-bold-p style))

(defun (setf face-bold-p) (value style)
  (setf (%drawing-style-bold-p style) value))

(defun face-underline-p (style)
  (drawing-style-underline-p style))

(defun (setf face-underline-p) (value style)
  (setf (drawing-style-underline-p style) value))

(defun face-reverse-p (style)
  (%drawing-style-reverse-p style))

(defun (setf face-reverse-p) (value style)
  (setf (%drawing-style-reverse-p style) value))

(defun face-parent (style)
  (drawing-style-parent style))

(defun (setf face-parent) (value style)
  (setf (drawing-style-parent style) value))

(defun face-transform (style)
  (drawing-style-transform style))

(defun (setf face-transform) (value style)
  (setf (drawing-style-transform style) value))

;;; --------------------------------------------------------------------------
;;; Resolved Drawing Styles
;;; --------------------------------------------------------------------------

(defstruct resolved-drawing-style
  "A drawing-style with inherited values resolved."
  (ink (make-cga-ink 0) :read-only t)
  (background-ink (make-cga-ink 15) :read-only t)
  (text-style (default-clawmacs-text-style) :read-only t)
  (drawing-options nil :read-only t)
  (underline-p nil :type boolean :read-only t))

(defstruct resolved-face
  "Compatibility resolved face. Values are CLIM-native drawing values."
  (foreground (make-cga-ink 0) :read-only t)
  (background (make-cga-ink 15) :read-only t)
  (text-style (default-clawmacs-text-style) :read-only t)
  (drawing-options nil :read-only t)
  (bold-p nil :type boolean :read-only t)
  (underline-p nil :type boolean :read-only t)
  (reverse-p nil :type boolean :read-only t))

(defun drawing-style-effective-bold-p (style)
  "Return the inherited legacy bold flag for STYLE."
  (let ((set-p nil)
        (value nil))
    (loop :for current := style :then (drawing-style-parent current)
          :while current
          :until set-p
          :do (when (not (null (slot-value current 'bold-p)))
                (setf value (face-bold-p current)
                      set-p t)))
    value))

(declaim (ftype (function (drawing-style) resolved-drawing-style)
                resolve-drawing-style))
(defun resolve-drawing-style (style)
  "Resolve STYLE inheritance into CLIM drawing values."
  (let (ink background-ink text-style drawing-options underline-p
        (underline-set nil)
        (reverse-p nil)
        (reverse-set nil))
    (loop :for current := style :then (drawing-style-parent current)
          :while current
          :do (when (and (null ink) (drawing-style-ink current))
                (setf ink (drawing-style-ink current)))
              (when (and (null background-ink)
                         (drawing-style-background-ink current))
                (setf background-ink
                      (drawing-style-background-ink current)))
              (when (and (null text-style)
                         (drawing-style-text-style current))
                (setf text-style
                      (drawing-style-text-style current)))
              (setf drawing-options
                    (merge-drawing-option-plists
                     (or drawing-options nil)
                     (drawing-style-drawing-options current)))
              (when (and (not underline-set)
                         (not (null (slot-value current 'underline-p))))
                (setf underline-p (drawing-style-underline-p current)
                      underline-set t))
              (when (and (not reverse-set)
                         (not (null (slot-value current 'reverse-p))))
                (setf reverse-p (face-reverse-p current)
                      reverse-set t)))
    (unless text-style
      (setf text-style
            (or (getf drawing-options :text-style)
                (if (drawing-style-effective-bold-p style)
                    (bold-clawmacs-text-style)
                    (default-clawmacs-text-style)))))
    (setf ink (or ink
                  (getf drawing-options :ink)
                  (make-cga-ink 0))
          background-ink (or background-ink (make-cga-ink 15))
          text-style (or text-style
                         (getf drawing-options :text-style)
                         (default-clawmacs-text-style)))
    (when reverse-p
      (rotatef ink background-ink))
    (setf (getf drawing-options :ink) ink
          (getf drawing-options :text-style) text-style)
    (make-resolved-drawing-style
     :ink ink
     :background-ink background-ink
     :text-style text-style
     :drawing-options drawing-options
     :underline-p (or underline-p nil))))

(declaim (ftype (function (drawing-style) resolved-face) resolve-face))
(defun resolve-face (face)
  "Compatibility wrapper around RESOLVE-DRAWING-STYLE."
  (let* ((resolved (resolve-drawing-style face))
         (ink (resolved-drawing-style-ink resolved))
         (background-ink (resolved-drawing-style-background-ink resolved))
         (reverse-p (let ((set-p nil)
                          (value nil))
                      (loop :for current := face :then (drawing-style-parent current)
                            :while current
                            :until set-p
                            :do (when (not (null (slot-value current 'reverse-p)))
                                  (setf value (face-reverse-p current)
                                        set-p t)))
                      value)))
    (make-resolved-face
     :foreground ink
     :background background-ink
     :text-style (resolved-drawing-style-text-style resolved)
     :drawing-options (resolved-drawing-style-drawing-options resolved)
     :bold-p (or (drawing-style-effective-bold-p face) nil)
     :underline-p (resolved-drawing-style-underline-p resolved)
     :reverse-p (or reverse-p nil))))

;;; --------------------------------------------------------------------------
;;; Drawing Style Sets
;;; --------------------------------------------------------------------------

(defclass drawing-style-set ()
  ((owner :initarg :owner
          :reader drawing-style-set-owner
          :type keyword
          :documentation "Keyword identifying the owner of this style set.")
   (styles :initarg :styles
           :reader drawing-style-set-styles
           :type hash-table
           :documentation "Hash table mapping style name keywords to drawing styles."))
  (:documentation "A collection of named drawing styles belonging to a sender."))

(defclass face-set (drawing-style-set) ()
  (:documentation "Compatibility name for older init files. Use DRAWING-STYLE-SET."))

(declaim (ftype (function (keyword list) drawing-style-set)
                make-drawing-style-set))
(defun make-drawing-style-set (owner style-list)
  "Create a drawing-style-set for OWNER from STYLE-LIST."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (style style-list)
      (setf (gethash (drawing-style-name style) ht) style))
    (make-instance 'drawing-style-set :owner owner :styles ht)))

(declaim (ftype (function (keyword list) face-set) make-face-set))
(defun make-face-set (owner face-list)
  "Compatibility wrapper creating a face-set for OWNER from FACE-LIST."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (style face-list)
      (setf (gethash (drawing-style-name style) ht) style))
    (make-instance 'face-set :owner owner :styles ht)))

(declaim (ftype (function (drawing-style-set keyword) (or null drawing-style))
                get-drawing-style))
(defun get-drawing-style (style-set name)
  "Look up a drawing style by NAME in STYLE-SET."
  (gethash name (drawing-style-set-styles style-set)))

(defun face-set-owner (face-set)
  (drawing-style-set-owner face-set))

(defun face-set-faces (face-set)
  (drawing-style-set-styles face-set))

(declaim (ftype (function (drawing-style-set keyword) (or null drawing-style))
                get-face))
(defun get-face (face-set name)
  "Compatibility wrapper around GET-DRAWING-STYLE."
  (get-drawing-style face-set name))
