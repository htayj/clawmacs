(in-package #:mcclim-charms)

(defvar *initialize-curses-on-port-create* t
  "When true, a CHARMS-PORT attempts to own curses immediately.
Tests that run without a controlling terminal may bind this to NIL and call
INITIALIZE-CHARMS-PORT inside a PTY-backed process.")

(defclass charms-pointer (standard-pointer)
  ((x :initform 0 :accessor charms-pointer-x)
   (y :initform 0 :accessor charms-pointer-y)
   (button-state :initform 0
                 :accessor charms-pointer-button-state)))

(defmethod pointer-position ((pointer charms-pointer))
  (values (charms-pointer-x pointer)
          (charms-pointer-y pointer)))

(defmethod* (setf pointer-position) (x y (pointer charms-pointer))
  (setf (charms-pointer-x pointer) (round x)
        (charms-pointer-y pointer) (round y))
  (values x y))

(defmethod pointer-button-state ((pointer charms-pointer))
  (charms-pointer-button-state pointer))

(defclass charms-port (basic-port)
  ((id :initform (gensym "CHARMS-PORT-") :reader charms-port-id)
   (window :initform nil :accessor charms-port-window)
   (initialized-p :initform nil :accessor charms-port-initialized-p)
   (width :initform 0 :accessor charms-port-width)
   (height :initform 0 :accessor charms-port-height)
   (event-queue :initform '() :accessor charms-port-event-queue)
   (keyboard-focus :initform nil :accessor charms-port-keyboard-focus)
   (modifier-state :initform 0 :accessor charms-port-modifier-state)
   (color-pairs :initform (make-hash-table :test #'equal)
                :accessor charms-port-color-pairs)
   (next-color-pair :initform 1 :accessor charms-port-next-color-pair))
  (:default-initargs :pointer (make-instance 'charms-pointer)))

(defclass charms-graft (graft)
  ())

(defclass charms-frame-manager (standard-frame-manager)
  ())

(defclass charms-medium (basic-medium)
  ())

(defclass charms-mirror ()
  ((sheet :initarg :sheet :reader charms-mirror-sheet)
   (parent :initarg :parent :initform nil :accessor charms-mirror-parent)
   (x :initarg :x :initform 0 :accessor charms-mirror-x)
   (y :initarg :y :initform 0 :accessor charms-mirror-y)
   (width :initarg :width :initform 0 :accessor charms-mirror-width)
   (height :initarg :height :initform 0 :accessor charms-mirror-height)
   (enabled-p :initarg :enabled-p :initform t :accessor charms-mirror-enabled-p)
   (invalid-p :initform t :accessor charms-mirror-invalid-p)))

(defclass charms-pixmap ()
  ((width :initarg :width :reader pixmap-width)
   (height :initarg :height :reader pixmap-height)
   (depth :initarg :depth :initform 1 :reader pixmap-depth)
   (cells :initarg :cells :accessor charms-pixmap-cells)))

(defun charms-port-size (port)
  (values (charms-port-width port)
          (charms-port-height port)))
