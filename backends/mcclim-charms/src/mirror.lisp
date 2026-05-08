(in-package #:mcclim-charms)

(defun %mirror-geometry-from-region (region)
  (multiple-value-bind (left top right bottom)
      (bounding-rectangle* region)
    (values (round left)
            (round top)
            (max 0 (round (- right left)))
            (max 0 (round (- bottom top))))))

(defun %make-mirror-for-sheet (sheet)
  (multiple-value-bind (x y width height)
      (%mirror-geometry-from-region (sheet-region sheet))
    (make-instance 'charms-mirror
                   :sheet sheet
                   :x x
                   :y y
                   :width width
                   :height height)))

(defmethod set-mirror-geometry ((port charms-port) sheet region)
  (declare (ignore port))
  (multiple-value-bind (x y width height)
      (%mirror-geometry-from-region region)
    (when-let ((mirror (sheet-direct-mirror sheet)))
      (setf (charms-mirror-x mirror) x
            (charms-mirror-y mirror) y
            (charms-mirror-width mirror) width
            (charms-mirror-height mirror) height
            (charms-mirror-invalid-p mirror) t)))
  (bounding-rectangle* region))

(defmethod set-mirror-name ((port charms-port) (sheet mirrored-sheet-mixin) name)
  (declare (ignore port sheet name))
  nil)

(defmethod realize-mirror ((port charms-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port))
  (%make-mirror-for-sheet sheet))

(defmethod destroy-mirror ((port charms-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port sheet))
  nil)

(defmethod enable-mirror ((port charms-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (setf (charms-mirror-enabled-p mirror) t
          (charms-mirror-invalid-p mirror) t)))

(defmethod disable-mirror ((port charms-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (setf (charms-mirror-enabled-p mirror) nil
          (charms-mirror-invalid-p mirror) t)))

(defmethod shrink-mirror ((port charms-port) (mirror charms-mirror))
  (declare (ignore port))
  (setf (charms-mirror-width mirror) 0
        (charms-mirror-height mirror) 0
        (charms-mirror-invalid-p mirror) t))
