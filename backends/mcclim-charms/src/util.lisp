(in-package #:mcclim-charms)

(defun clamp-integer (value low high)
  (min high (max low (round value))))

(defun clim->curses (x y)
  "Return curses Y/X values for CLIM X/Y values."
  (values (round y) (round x)))

(defun string-slice (string start end)
  (subseq (etypecase string
            (character (string string))
            (string string))
          (or start 0)
          (or end (length string))))

(defun terminal-size-from-window (window)
  (multiple-value-bind (width height)
      (charms:window-dimensions window)
    (values width height)))

(defun charms-window-pointer (window)
  (charms::window-pointer window))

(defun safe-call (thunk)
  (handler-case
      (values (funcall thunk) nil)
    (error (condition)
      (values nil condition))))
