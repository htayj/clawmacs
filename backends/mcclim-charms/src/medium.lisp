(in-package #:mcclim-charms)

(defun %medium-window (medium)
  (let ((port (port medium)))
    (and (typep port 'charms-port)
         (charms-port-window port))))

(defun %window-cell-bounds (window)
  (multiple-value-bind (width height)
      (terminal-size-from-window window)
    (values width height)))

(defun %drawable-cell-p (window x y)
  (multiple-value-bind (width height)
      (%window-cell-bounds window)
    (and (<= 0 x)
         (< x width)
         (<= 0 y)
         (< y height)
         ;; Many curses implementations report ERR when writing the lower
         ;; right cell because that would require an automatic scroll.
         (not (and (= x (1- width))
                   (= y (1- height)))))))

(defun %put-char (window x y char)
  (when window
    (let ((x (round x))
          (y (round y)))
      (when (%drawable-cell-p window x y)
        (multiple-value-bind (cy cx) (clim->curses x y)
          (safe-call
           (lambda ()
             (charms:write-char-at-point window char cx cy))))))))

(defun %device-position (medium x y)
  (transform-position (medium-device-transformation medium) x y))

(defun %draw-line (window x1 y1 x2 y2 char)
  (let* ((dx (abs (- x2 x1)))
         (dy (- (abs (- y2 y1))))
         (sx (if (< x1 x2) 1 -1))
         (sy (if (< y1 y2) 1 -1))
         (err (+ dx dy)))
    (loop
      (%put-char window x1 y1 char)
      (when (and (= x1 x2) (= y1 y2))
        (return))
      (let ((e2 (* 2 err)))
        (when (>= e2 dy)
          (incf err dy)
          (incf x1 sx))
        (when (<= e2 dx)
          (incf err dx)
          (incf y1 sy))))))

(defun %coord (coord-seq index)
  (elt coord-seq index))

(defun %coord-count (coord-seq)
  (length coord-seq))

(defmethod (setf medium-text-style) :before (text-style (medium charms-medium))
  (declare (ignore text-style))
  nil)

(defmethod (setf medium-line-style) :before (line-style (medium charms-medium))
  (declare (ignore line-style))
  nil)

(defmethod (setf medium-clipping-region) :after (region (medium charms-medium))
  (declare (ignore region))
  nil)

(defmethod allocate-pixmap ((medium charms-medium) width height)
  (make-instance 'charms-pixmap
                 :width width
                 :height height
                 :depth 1
                 :cells (make-array (list height width)
                                    :initial-element #\space)))

(defmethod deallocate-pixmap ((pixmap charms-pixmap))
  (declare (ignore pixmap))
  nil)

(macrolet ((copy-area-method (from-class to-class)
             `(defmethod medium-copy-area ((from-drawable ,from-class)
                                           from-x from-y width height
                                           (to-drawable ,to-class)
                                           to-x to-y)
                (declare (ignore from-drawable from-x from-y width height
                                 to-drawable to-x to-y))
                nil)))
  (copy-area-method charms-medium charms-medium)
  (copy-area-method charms-medium charms-pixmap)
  (copy-area-method charms-pixmap charms-medium)
  (copy-area-method charms-pixmap charms-pixmap))

(defmethod medium-draw-point* ((medium charms-medium) x y)
  (multiple-value-bind (x y)
      (%device-position medium x y)
    (%put-char (%medium-window medium) x y #\*)))

(defmethod medium-draw-points* ((medium charms-medium) coord-seq)
  (loop for index below (%coord-count coord-seq) by 2
        for y-index = (1+ index)
        while (< y-index (%coord-count coord-seq))
        do (medium-draw-point* medium
                               (%coord coord-seq index)
                               (%coord coord-seq y-index))))

(defmethod medium-draw-line* ((medium charms-medium) x1 y1 x2 y2)
  (multiple-value-bind (x1 y1)
      (%device-position medium x1 y1)
    (multiple-value-bind (x2 y2)
        (%device-position medium x2 y2)
      (%draw-line (%medium-window medium)
                  (round x1) (round y1) (round x2) (round y2) #\*))))

(defmethod medium-draw-lines* ((medium charms-medium) coord-seq)
  (loop for index below (%coord-count coord-seq) by 4
        for y2-index = (+ index 3)
        while (< y2-index (%coord-count coord-seq))
        do (medium-draw-line* medium
                              (%coord coord-seq index)
                              (%coord coord-seq (+ index 1))
                              (%coord coord-seq (+ index 2))
                              (%coord coord-seq y2-index))))

(defmethod medium-draw-polygon* ((medium charms-medium) coord-seq closed filled)
  (declare (ignore filled))
  (let ((count (%coord-count coord-seq)))
    (loop for index below count by 2
          for y2-index = (+ index 3)
          while (< y2-index count)
          do (medium-draw-line* medium
                                (%coord coord-seq index)
                                (%coord coord-seq (+ index 1))
                                (%coord coord-seq (+ index 2))
                                (%coord coord-seq y2-index)))
    (when (and closed (>= count 4))
      (medium-draw-line* medium
                         (%coord coord-seq (- count 2))
                         (%coord coord-seq (- count 1))
                         (%coord coord-seq 0)
                         (%coord coord-seq 1)))))

(defmethod medium-draw-rectangle* ((medium charms-medium) left top right bottom filled)
  (multiple-value-bind (left top)
      (%device-position medium left top)
    (multiple-value-bind (right bottom)
        (%device-position medium right bottom)
      (let ((window (%medium-window medium))
            (left (round (min left right)))
            (top (round (min top bottom)))
            (right (round (max left right)))
            (bottom (round (max top bottom))))
        (if filled
            (loop for y from top below bottom
                  do (loop for x from left below right
                           do (%put-char window x y #\space)))
            (progn
              (%draw-line window left top right top #\-)
              (%draw-line window left bottom right bottom #\-)
              (%draw-line window left top left bottom #\|)
              (%draw-line window right top right bottom #\|)))))))

(defmethod medium-draw-rectangles* ((medium charms-medium) position-seq filled)
  (loop for index below (%coord-count position-seq) by 4
        for bottom-index = (+ index 3)
        while (< bottom-index (%coord-count position-seq))
        do (medium-draw-rectangle* medium
                                   (%coord position-seq index)
                                   (%coord position-seq (+ index 1))
                                   (%coord position-seq (+ index 2))
                                   (%coord position-seq bottom-index)
                                   filled)))

(defmethod medium-draw-ellipse* ((medium charms-medium) center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 start-angle end-angle filled)
  (declare (ignore radius-2-dx radius-2-dy start-angle end-angle filled))
  (let ((rx (max 1 (round (abs radius-1-dx))))
        (ry (max 1 (round (abs radius-1-dy))))
        (cx center-x)
        (cy center-y))
    (multiple-value-setq (cx cy)
      (%device-position medium cx cy))
    (setf cx (round cx)
          cy (round cy))
    (loop for degrees from 0 below 360 by 10
          for radians = (* pi (/ degrees 180.0d0))
          do (%put-char (%medium-window medium)
                        (+ cx (round (* rx (cos radians))))
                        (+ cy (round (* ry (sin radians))))
                        #\o))))

(defmethod text-style-ascent (text-style (medium charms-medium))
  (declare (ignore text-style))
  1)

(defmethod text-style-descent (text-style (medium charms-medium))
  (declare (ignore text-style))
  0)

(defmethod text-style-height (text-style (medium charms-medium))
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))

(defmethod text-style-character-width (text-style (medium charms-medium) char)
  (declare (ignore text-style char))
  1)

(defmethod text-style-width (text-style (medium charms-medium))
  (text-style-character-width text-style medium #\m))

(defmethod text-size ((medium charms-medium) string &key text-style (start 0) end)
  (declare (ignore text-style))
  (let* ((slice (string-slice string start end))
         (width (length slice))
         (height 1)
         (baseline 1))
    (values width height width 0 baseline)))

(defmethod climb:text-bounding-rectangle*
    ((medium charms-medium) string &key text-style (start 0) end)
  (multiple-value-bind (width height x y baseline)
      (text-size medium string :text-style text-style :start start :end end)
    (declare (ignore baseline))
    (values x y (+ x width) (+ y height) width 0)))

(defmethod medium-draw-text* ((medium charms-medium) string x y
                              start end align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore align-y toward-x toward-y transform-glyphs))
  (when-let ((window (%medium-window medium)))
    (multiple-value-bind (x y)
        (%device-position medium x y)
      (let* ((slice (string-slice string start end))
             (x (case align-x
                  (:center (- x (/ (length slice) 2)))
                  (:right (- x (length slice)))
                  (otherwise x)))
             (x (round x))
             (y (round y)))
      (multiple-value-bind (width height)
          (%window-cell-bounds window)
        (when (and (<= 0 x) (< x width) (<= 0 y) (< y height))
          (let* ((limit (max 0 (- width x (if (= y (1- height)) 1 0))))
                 (text (subseq slice 0 (min limit (length slice)))))
            (when (plusp (length text))
              (multiple-value-bind (cy cx) (clim->curses x y)
                (safe-call
                 (lambda ()
                   (charms:write-string-at-point window text cx cy))))))))))))

(defmethod medium-finish-output ((medium charms-medium))
  (when-let ((window (%medium-window medium)))
    (charms:refresh-window window)))

(defmethod medium-force-output ((medium charms-medium))
  (medium-finish-output medium))

(defmethod medium-clear-area ((medium charms-medium) left top right bottom)
  (let ((window (%medium-window medium)))
    (multiple-value-bind (left top)
        (%device-position medium left top)
      (multiple-value-bind (right bottom)
          (%device-position medium right bottom)
        (loop for y from (round (min top bottom)) below (round (max top bottom))
              do (loop for x from (round (min left right)) below (round (max left right))
                       do (%put-char window x y #\space)))))))

(defmethod medium-beep ((medium charms-medium))
  (declare (ignore medium))
  (charms:beep-console))

(defmethod medium-miter-limit ((medium charms-medium))
  0)
