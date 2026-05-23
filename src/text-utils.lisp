(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Text Utilities
;;; --------------------------------------------------------------------------

(declaim (ftype (function (string) list) split-string-by-newline)
         (ftype (function (list) string) join-lines-with-newlines))

(defun split-string-by-newline (str)
  "Split STR by newlines into a list of strings."
  (declare (type string str))
  (loop :for start := 0 :then (1+ pos)
        :for pos := (position #\Newline str :start start)
        :collect (subseq str start (or pos (length str)))
        :while pos))

(defun join-lines-with-newlines (lines)
  "Join LINES into a newline-delimited string."
  (declare (type list lines))
  (with-output-to-string (stream)
    (loop :for line :in lines
          :for first := t :then nil
          :do (unless first (write-char #\Newline stream))
              (write-string line stream))))
