(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Bitmap Font Editor
;;; --------------------------------------------------------------------------

(defvar *font-editor-buffer-name* "*font-editor*"
  "Default name for the in-buffer bitmap font editor.")

(defvar *font-editor-default-sample* "The quick brown fox 0123456789"
  "Default sample string shown by the bitmap font editor preview.")

(defvar *font-editor-default-extract-directory*
  #P"/tmp/clawmacs-genera-fonts/"
  "Default extraction target for proprietary Genera BDF fonts.")

(defvar *font-editor-states* (make-hash-table :test #'eq)
  "Per-buffer state for bitmap font editor buffers.")

(defparameter *font-editor-repo-root*
  (ignore-errors
    (truename
     (merge-pathnames
      #P"../"
      (or *load-truename*
          *compile-file-truename*
          *default-pathname-defaults*))))
  "Clawmacs repository root when it can be inferred during load/compile.")

(defun resolve-font-editor-directory (path)
  "Return PATH as a truename when it exists, otherwise NIL."
  (ignore-errors
    (truename path)))

(defparameter *font-editor-genera-bdf-source-directories*
  (remove nil
          (list
           (and *font-editor-repo-root*
                (resolve-font-editor-directory
                 (merge-pathnames
                  #P"reference/external_src/symbolics-potentially-proprietary/extracted/genera2_0_iso/sys.sct/x11/fonts/bdf/genera/"
                  *font-editor-repo-root*)))
           (resolve-font-editor-directory
            (merge-pathnames
             #P"reference/external_src/symbolics-potentially-proprietary/extracted/genera2_0_iso/sys.sct/x11/fonts/bdf/genera/"
             (user-homedir-pathname))))))

(defstruct (bitmap-glyph
            (:constructor %make-bitmap-glyph
                (&key code
                      name
                      (advance-width 8)
                      (x-offset 0)
                      (y-offset 0)
                      bitmap)))
  "One editable bitmap glyph in a font editor font."
  (code 0 :type integer)
  (name nil :type (or null string))
  (advance-width 8 :type integer)
  (x-offset 0 :type integer)
  (y-offset 0 :type integer)
  (bitmap (make-array '(0 0) :element-type 'bit)
          :type array))

(defstruct (bitmap-font
            (:constructor %make-bitmap-font
                (&key (name "UNTITLED")
                      (line-spacing 12)
                      (baseline 10)
                      (default-width 8)
                      (blinker-width 8)
                      (blinker-height 12)
                      glyphs
                      metadata)))
  "An editor-facing bitmap font model, close to BDF semantics."
  (name "UNTITLED" :type string)
  (line-spacing 12 :type integer)
  (baseline 10 :type integer)
  (default-width 8 :type integer)
  (blinker-width 8 :type integer)
  (blinker-height 12 :type integer)
  (glyphs (make-hash-table :test #'eql) :type hash-table)
  (metadata nil :type list))

(defstruct (font-editor-state
            (:constructor make-font-editor-state
                (&key
                  (font (make-empty-bitmap-font))
                  (selected-code 65)
                  (cursor-x 0)
                  (cursor-y 0)
                  (catalog-offset 0)
                  (sample-string *font-editor-default-sample*)
                  source-path
                  save-path
                  status-text
                  (dirty-p nil))))
  "Editor state for a bitmap font editor buffer."
  (font (make-empty-bitmap-font) :type bitmap-font)
  (selected-code 65 :type integer)
  (cursor-x 0 :type integer)
  (cursor-y 0 :type integer)
  (catalog-offset 0 :type integer)
  (sample-string *font-editor-default-sample* :type string)
  (source-path nil :type (or null pathname))
  (save-path nil :type (or null pathname))
  (status-text "" :type string)
  (dirty-p nil :type boolean))

(defun make-empty-bitmap-font (&key (name "UNTITLED")
                                    (line-spacing 12)
                                    (baseline 10)
                                    (default-width 8)
                                    (blinker-width default-width)
                                    (blinker-height line-spacing))
  "Return a fresh empty bitmap font."
  (%make-bitmap-font
   :name name
   :line-spacing line-spacing
   :baseline baseline
   :default-width default-width
   :blinker-width blinker-width
   :blinker-height blinker-height
   :glyphs (make-hash-table :test #'eql)
   :metadata nil))

(defun make-bitmap-glyph (&key (code 0)
                               name
                               (advance-width 8)
                               (x-offset 0)
                               (y-offset 0)
                               (width 0)
                               (height 0)
                               bitmap)
  "Return a glyph with either BITMAP or WIDTH/HEIGHT dimensions."
  (%make-bitmap-glyph
   :code code
   :name name
   :advance-width advance-width
   :x-offset x-offset
   :y-offset y-offset
   :bitmap (or bitmap
               (make-array (list (max 0 height) (max 0 width))
                           :element-type 'bit
                           :initial-element 0))))

(defun bitmap-glyph-width (glyph)
  "Return GLYPH's bitmap width."
  (array-dimension (bitmap-glyph-bitmap glyph) 1))

(defun bitmap-glyph-height (glyph)
  "Return GLYPH's bitmap height."
  (array-dimension (bitmap-glyph-bitmap glyph) 0))

(defun copy-bit-grid (grid)
  "Return a deep copy of bit array GRID."
  (let* ((height (array-dimension grid 0))
         (width (array-dimension grid 1))
         (copy (make-array (list height width)
                           :element-type 'bit
                           :initial-element 0)))
    (dotimes (y height copy)
      (dotimes (x width)
        (setf (aref copy y x) (aref grid y x))))))

(defun copy-bitmap-glyph (glyph)
  "Return a deep copy of GLYPH."
  (make-bitmap-glyph
   :code (bitmap-glyph-code glyph)
   :name (bitmap-glyph-name glyph)
   :advance-width (bitmap-glyph-advance-width glyph)
   :x-offset (bitmap-glyph-x-offset glyph)
   :y-offset (bitmap-glyph-y-offset glyph)
   :bitmap (copy-bit-grid (bitmap-glyph-bitmap glyph))))

(defun copy-bitmap-font (font)
  "Return a deep copy of FONT."
  (let ((copy (make-empty-bitmap-font
               :name (bitmap-font-name font)
               :line-spacing (bitmap-font-line-spacing font)
               :baseline (bitmap-font-baseline font)
               :default-width (bitmap-font-default-width font)
               :blinker-width (bitmap-font-blinker-width font)
               :blinker-height (bitmap-font-blinker-height font))))
    (setf (bitmap-font-metadata copy)
          (copy-tree (bitmap-font-metadata font)))
    (maphash (lambda (code glyph)
               (setf (gethash code (bitmap-font-glyphs copy))
                     (copy-bitmap-glyph glyph)))
             (bitmap-font-glyphs font))
    copy))

(defun bitmap-font-glyph (font code)
  "Return FONT's glyph for CODE, or NIL."
  (gethash code (bitmap-font-glyphs font)))

(defun (setf bitmap-font-glyph) (glyph font code)
  "Store GLYPH in FONT under CODE and return GLYPH."
  (setf (gethash code (bitmap-font-glyphs font)) glyph))

(defun bitmap-font-glyph-codes (font)
  "Return FONT's defined glyph codes in ascending order."
  (let ((codes nil))
    (maphash (lambda (code glyph)
               (declare (ignore glyph))
               (push code codes))
             (bitmap-font-glyphs font))
    (sort codes #'<)))

(defun bitmap-font-first-glyph-code (font)
  "Return a reasonable default selected glyph code for FONT."
  (or (find (char-code #\A) (bitmap-font-glyph-codes font))
      (find (char-code #\a) (bitmap-font-glyph-codes font))
      (find (char-code #\Space) (bitmap-font-glyph-codes font))
      (first (bitmap-font-glyph-codes font))
      65))

(defun ensure-bitmap-font-glyph (font code)
  "Ensure FONT contains a glyph at CODE and return it."
  (or (bitmap-font-glyph font code)
      (setf (bitmap-font-glyph font code)
            (make-bitmap-glyph
             :code code
             :advance-width (bitmap-font-default-width font)
             :x-offset 0
             :y-offset (- (bitmap-font-baseline font)
                          (bitmap-font-line-spacing font))
             :width (max 1 (bitmap-font-default-width font))
             :height (max 1 (bitmap-font-line-spacing font))))))

(defun printable-code-char (code)
  "Return CODE as a printable character, or NIL."
  (let ((char (ignore-errors (code-char code))))
    (and char
         (graphic-char-p char)
         char)))

(defun default-glyph-name (code)
  "Return a default glyph name for CODE."
  (let ((char (printable-code-char code)))
    (if char
        (string char)
        (format nil "code-~D" code))))

(defun resize-glyph-bitmap (glyph width height)
  "Resize GLYPH's bitmap to WIDTH x HEIGHT, preserving existing bits."
  (let* ((old (bitmap-glyph-bitmap glyph))
         (old-height (array-dimension old 0))
         (old-width (array-dimension old 1))
         (new (make-array (list (max 0 height) (max 0 width))
                          :element-type 'bit
                          :initial-element 0)))
    (dotimes (y (min old-height height))
      (dotimes (x (min old-width width))
        (setf (aref new y x) (aref old y x))))
    (setf (bitmap-glyph-bitmap glyph) new)
    glyph))

(defun glyph-rows-as-strings (glyph)
  "Return GLYPH's bitmap as a vector of `*' / space rows."
  (let* ((bitmap (bitmap-glyph-bitmap glyph))
         (height (array-dimension bitmap 0))
         (width (array-dimension bitmap 1))
         (rows (make-array height)))
    (dotimes (y height rows)
      (let ((line (make-string width :initial-element #\Space)))
        (dotimes (x width)
          (when (= 1 (aref bitmap y x))
            (setf (char line x) #\*)))
        (setf (aref rows y) line)))))

(defun rows-to-bit-grid (rows width)
  "Return a bit grid from ROWS using WIDTH columns."
  (let* ((height (length rows))
         (bitmap (make-array (list height width)
                             :element-type 'bit
                             :initial-element 0)))
    (dotimes (y height bitmap)
      (let ((row (or (elt rows y) "")))
        (dotimes (x (min width (length row)))
          (unless (char= (char row x) #\Space)
            (setf (aref bitmap y x) 1)))))))

(defun serialize-bitmap-glyph (glyph)
  "Return a plist representation of GLYPH."
  (list :code (bitmap-glyph-code glyph)
        :name (bitmap-glyph-name glyph)
        :advance-width (bitmap-glyph-advance-width glyph)
        :x-offset (bitmap-glyph-x-offset glyph)
        :y-offset (bitmap-glyph-y-offset glyph)
        :rows (glyph-rows-as-strings glyph)))

(defun deserialize-bitmap-glyph (data)
  "Return a bitmap glyph parsed from DATA."
  (let* ((rows (coerce (or (getf data :rows) #()) 'list))
         (width (loop :for row :in rows :maximize (length row) :into max :finally (return (or max 0))))
         (bitmap (rows-to-bit-grid rows width)))
    (make-bitmap-glyph
     :code (or (getf data :code) 0)
     :name (getf data :name)
     :advance-width (or (getf data :advance-width) 8)
     :x-offset (or (getf data :x-offset) 0)
     :y-offset (or (getf data :y-offset) 0)
     :bitmap bitmap)))

(defun serialize-bitmap-font (font)
  "Return a plist representation of FONT."
  (list :name (bitmap-font-name font)
        :line-spacing (bitmap-font-line-spacing font)
        :baseline (bitmap-font-baseline font)
        :default-width (bitmap-font-default-width font)
        :blinker-width (bitmap-font-blinker-width font)
        :blinker-height (bitmap-font-blinker-height font)
        :metadata (copy-tree (bitmap-font-metadata font))
        :glyphs (coerce
                 (mapcar (lambda (code)
                           (serialize-bitmap-glyph
                            (bitmap-font-glyph font code)))
                         (bitmap-font-glyph-codes font))
                 'vector)))

(defun deserialize-bitmap-font (data)
  "Return a bitmap font parsed from DATA."
  (let ((font (make-empty-bitmap-font
               :name (or (getf data :name) "UNTITLED")
               :line-spacing (or (getf data :line-spacing) 12)
               :baseline (or (getf data :baseline) 10)
               :default-width (or (getf data :default-width) 8)
               :blinker-width (or (getf data :blinker-width)
                                  (or (getf data :default-width) 8))
               :blinker-height (or (getf data :blinker-height)
                                   (or (getf data :line-spacing) 12)))))
    (setf (bitmap-font-metadata font)
          (copy-tree (getf data :metadata)))
    (dolist (entry (coerce (or (getf data :glyphs) #()) 'list))
      (let ((glyph (deserialize-bitmap-glyph entry)))
        (setf (bitmap-font-glyph font (bitmap-glyph-code glyph)) glyph)))
    font))

(defun write-clawfont-file (font path)
  "Write FONT to PATH in Clawmacs' native `.clawfont' format."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (let ((*print-readably* t)
          (*print-pretty* t))
      (prin1 (list :clawfont (serialize-bitmap-font font)) stream)
      (terpri stream)))
  path)

(defun read-clawfont-file (path)
  "Read and return a bitmap font from PATH."
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((*read-eval* nil))
      (destructuring-bind (tag payload) (read stream nil nil)
        (unless (eq tag :clawfont)
          (error "Unsupported Clawmacs font file: ~A" path))
        (deserialize-bitmap-font payload)))))

(defun split-string-on-page (text)
  "Split TEXT on `#\\Page' and return a list of page strings."
  (let ((pages nil)
        (start 0))
    (loop
      :for pos := (position #\Page text :start start)
      :do (if pos
              (progn
                (push (subseq text start pos) pages)
                (setf start (1+ pos)))
              (progn
                (push (subseq text start) pages)
                (return))))
    (nreverse pages)))

(defun ast-page-lines (page)
  "Return PAGE split into lines."
  (with-input-from-string (stream page)
    (loop :for line := (read-line stream nil nil)
          :while line
          :collect line)))

(defun parse-leading-integer (line)
  "Parse and return the leading integer from LINE."
  (parse-integer line :junk-allowed t))

(defun ast-header-name (line path)
  "Return a font name from AST header LINE, or PATH."
  (let* ((parts (remove "" (uiop:split-string line :separator " ")
                        :test #'string=))
         (token (find-if (lambda (item)
                           (and (search ";" item)
                                (search "KST" item)))
                         parts)))
    (or (when token
          (let* ((semi (position #\; token))
                 (suffix (and semi (subseq token (1+ semi))))
                 (space (and suffix (position #\Space suffix))))
            (and suffix
                 (string-upcase
                  (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (if space
                                   (subseq suffix 0 space)
                                   suffix))))))
        (string-upcase (pathname-name path))
        "UNTITLED")))

(defun import-ast-font (path)
  "Return a bitmap font imported from CADR AST PATH."
  (let* ((text (uiop:read-file-string path :external-format :utf-8))
         (pages (split-string-on-page text)))
    (when (endp pages)
      (error "Empty AST font file: ~A" path))
    (let* ((header-lines (ast-page-lines (first pages)))
           (line-spacing (if (second header-lines)
                             (parse-leading-integer (second header-lines))
                             12))
           (baseline (if (third header-lines)
                         (parse-leading-integer (third header-lines))
                         (max 1 (1- line-spacing))))
           (font (make-empty-bitmap-font
                  :name (ast-header-name (first header-lines) path)
                  :line-spacing line-spacing
                  :baseline baseline
                  :default-width 8
                  :blinker-width 8
                  :blinker-height line-spacing)))
      (dolist (page (rest pages))
        (let ((lines (ast-page-lines page)))
          (when (>= (length lines) 4)
            (let* ((code (parse-leading-integer (first lines)))
                   (raster-width (parse-leading-integer (second lines)))
                   (advance-width (parse-leading-integer (third lines)))
                   (x-offset (parse-leading-integer (fourth lines)))
                   (rows (copy-list (nthcdr 4 lines))))
              (unless rows
                (setf rows nil))
              (loop :while (< (length rows) line-spacing)
                    :do (setf rows (append rows (list ""))))
              (let* ((bitmap (rows-to-bit-grid rows raster-width))
                     (glyph (make-bitmap-glyph
                             :code code
                             :name (default-glyph-name code)
                             :advance-width advance-width
                             :x-offset x-offset
                             :y-offset (- baseline (length rows))
                             :bitmap bitmap)))
                (setf (bitmap-font-glyph font code) glyph))))))
      (let ((space (bitmap-font-glyph font (char-code #\Space))))
        (setf (bitmap-font-default-width font)
              (or (and space (bitmap-glyph-advance-width space))
                  (loop :for code :in (bitmap-font-glyph-codes font)
                        :for glyph := (bitmap-font-glyph font code)
                        :maximize (bitmap-glyph-advance-width glyph)
                        :into max
                        :finally (return (or max 8))))
              (bitmap-font-blinker-width font)
              (bitmap-font-default-width font)
              (bitmap-font-blinker-height font)
              (bitmap-font-line-spacing font)))
      font)))

(defun decode-bdf-bitmap-row (hex width)
  "Return a bit row from BDF HEX text using WIDTH bits."
  (let* ((text (string-trim '(#\Space #\Tab) hex))
         (nibbles (max 1 (length text)))
         (value (parse-integer text :radix 16))
         (source-width (* 4 nibbles))
         (row (make-array width :element-type 'bit :initial-element 0)))
    (dotimes (x width row)
      (let ((bit-index (- source-width 1 x)))
        (when (and (>= bit-index 0)
                   (logbitp bit-index value))
          (setf (aref row x) 1))))))

(defun import-bdf-font (path)
  "Return a bitmap font imported from BDF PATH."
  (let ((font (make-empty-bitmap-font
               :name (string-upcase (pathname-name path))))
        (in-properties-p nil))
    (with-open-file (stream path :direction :input :external-format :utf-8)
      (loop
        :for line := (read-line stream nil nil)
        :while line
        :do (cond
              ((uiop:string-prefix-p "STARTPROPERTIES " line)
               (setf in-properties-p t))
              ((string= line "ENDPROPERTIES")
               (setf in-properties-p nil))
              ((uiop:string-prefix-p "FONT " line)
               (setf (bitmap-font-name font)
                     (string-upcase
                      (string-trim '(#\Space #\Tab)
                                   (subseq line 5)))))
              ((uiop:string-prefix-p "FONTBOUNDINGBOX " line)
               (let ((parts (uiop:split-string (subseq line 16)
                                               :separator " ")))
                 (when (first parts)
                   (setf (bitmap-font-default-width font)
                         (parse-integer (first parts))))))
              ((and in-properties-p
                    (uiop:string-prefix-p "FONT_ASCENT " line))
               (setf (bitmap-font-baseline font)
                     (parse-integer (subseq line 12))))
              ((and in-properties-p
                    (uiop:string-prefix-p "FONT_DESCENT " line))
               (let ((descent (parse-integer (subseq line 13))))
                 (setf (bitmap-font-line-spacing font)
                       (+ (bitmap-font-baseline font) descent)
                       (bitmap-font-blinker-height font)
                       (+ (bitmap-font-baseline font) descent))))
              ((uiop:string-prefix-p "STARTCHAR " line)
               (let ((glyph-name (string-trim '(#\Space #\Tab)
                                              (subseq line 10)))
                     (code nil)
                     (advance-width nil)
                     (bbx-width 0)
                     (bbx-height 0)
                     (x-offset 0)
                     (y-offset 0)
                     (rows nil))
                 (loop
                   :for glyph-line := (read-line stream nil nil)
                   :while glyph-line
                   :do (cond
                         ((uiop:string-prefix-p "ENCODING " glyph-line)
                          (let* ((parts (remove "" (uiop:split-string (subseq glyph-line 9)
                                                                      :separator " ")
                                                :test #'string=))
                                 (token (car (last parts))))
                            (setf code (parse-integer token))))
                         ((uiop:string-prefix-p "DWIDTH " glyph-line)
                          (setf advance-width
                                (parse-integer
                                 (first (uiop:split-string
                                         (subseq glyph-line 7)
                                         :separator " ")))))
                         ((uiop:string-prefix-p "BBX " glyph-line)
                          (let ((parts (remove "" (uiop:split-string
                                                   (subseq glyph-line 4)
                                                   :separator " ")
                                               :test #'string=)))
                            (setf bbx-width (parse-integer (first parts))
                                  bbx-height (parse-integer (second parts))
                                  x-offset (parse-integer (third parts))
                                  y-offset (parse-integer (fourth parts)))))
                         ((string= "BITMAP" glyph-line)
                          (setf rows
                                (loop :repeat bbx-height
                                      :collect (read-line stream nil ""))))
                         ((string= "ENDCHAR" glyph-line)
                          (let ((bitmap (make-array (list bbx-height bbx-width)
                                                    :element-type 'bit
                                                    :initial-element 0)))
                            (dotimes (y bbx-height)
                              (let ((bit-row (decode-bdf-bitmap-row
                                              (nth y rows)
                                              bbx-width)))
                                (dotimes (x bbx-width)
                                  (setf (aref bitmap y x)
                                        (aref bit-row x)))))
                            (let ((glyph (make-bitmap-glyph
                                          :code (or code 0)
                                          :name glyph-name
                                          :advance-width
                                          (or advance-width
                                              (bitmap-font-default-width font))
                                          :x-offset x-offset
                                          :y-offset y-offset
                                          :bitmap bitmap)))
                              (setf (bitmap-font-glyph font (bitmap-glyph-code glyph))
                                    glyph)))
                          (return))))))))
      (let ((space (bitmap-font-glyph font (char-code #\Space))))
        (setf (bitmap-font-default-width font)
              (or (and space (bitmap-glyph-advance-width space))
                  (loop :for code :in (bitmap-font-glyph-codes font)
                        :for glyph := (bitmap-font-glyph font code)
                        :maximize (bitmap-glyph-advance-width glyph)
                        :into max
                        :finally (return (or max (bitmap-font-default-width font)))))
              (bitmap-font-blinker-width font)
              (bitmap-font-default-width font)
              (bitmap-font-blinker-height font)
              (bitmap-font-line-spacing font)))
    font)))

(defun bdf-row-hex (glyph y)
  "Return GLYPH row Y formatted as uppercase BDF hex."
  (let* ((width (bitmap-glyph-width glyph))
         (digits (max 1 (ceiling width 4)))
         (value 0)
         (bitmap (bitmap-glyph-bitmap glyph)))
    (dotimes (x width)
      (when (= 1 (aref bitmap y x))
        (setf value (logior value (ash 1 (- width 1 x))))))
    (format nil (format nil "~~~D,'0X" digits) value)))

(defun glyph-display-name-for-bdf (glyph)
  "Return the BDF `STARTCHAR' label for GLYPH."
  (let ((name (or (bitmap-glyph-name glyph)
                  (default-glyph-name (bitmap-glyph-code glyph)))))
    (if (blank-string-p name)
        (format nil "code-~D" (bitmap-glyph-code glyph))
        (substitute #\- #\Space name))))

(defun write-bdf-font (font path)
  "Write FONT to PATH as BDF."
  (let* ((ascent (bitmap-font-baseline font))
         (descent (max 0 (- (bitmap-font-line-spacing font) ascent)))
         (max-width (loop :for code :in (bitmap-font-glyph-codes font)
                          :for glyph := (bitmap-font-glyph font code)
                          :maximize (bitmap-glyph-width glyph)
                          :into max
                          :finally (return (or max (bitmap-font-default-width font))))))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (format stream "STARTFONT 2.1~%")
      (format stream "FONT ~A~%" (bitmap-font-name font))
      (format stream "SIZE ~D 72 72~%" (bitmap-font-line-spacing font))
      (format stream "FONTBOUNDINGBOX ~D ~D 0 ~D~%"
              max-width
              (bitmap-font-line-spacing font)
              (- descent))
      (format stream "STARTPROPERTIES 2~%")
      (format stream "FONT_ASCENT ~D~%" ascent)
      (format stream "FONT_DESCENT ~D~%" descent)
      (format stream "ENDPROPERTIES~%")
      (format stream "CHARS ~D~%" (length (bitmap-font-glyph-codes font)))
      (dolist (code (bitmap-font-glyph-codes font))
        (let ((glyph (bitmap-font-glyph font code)))
          (format stream "STARTCHAR ~A~%"
                  (glyph-display-name-for-bdf glyph))
          (format stream "ENCODING ~D~%" code)
          (format stream "SWIDTH ~D 0~%"
                  (* 1000 (bitmap-glyph-advance-width glyph)))
          (format stream "DWIDTH ~D 0~%"
                  (bitmap-glyph-advance-width glyph))
          (format stream "BBX ~D ~D ~D ~D~%"
                  (bitmap-glyph-width glyph)
                  (bitmap-glyph-height glyph)
                  (bitmap-glyph-x-offset glyph)
                  (bitmap-glyph-y-offset glyph))
          (format stream "BITMAP~%")
          (dotimes (y (bitmap-glyph-height glyph))
            (format stream "~A~%" (bdf-row-hex glyph y)))
          (format stream "ENDCHAR~%")))
      (format stream "ENDFONT~%"))
    path))

(defun detect-font-file-format (path)
  "Return the import/export file format keyword for PATH."
  (let ((name (string-downcase (namestring path))))
    (cond
      ((search ".clawfont" name :from-end t) :clawfont)
      ((search ".bdf" name :from-end t) :bdf)
      ((search ".ast" name :from-end t) :ast)
      (t
       (error "Unsupported font file format: ~A" path)))))

(defun read-font-file (path)
  "Read and return a bitmap font from PATH."
  (ecase (detect-font-file-format path)
    (:clawfont (read-clawfont-file path))
    (:bdf (import-bdf-font path))
    (:ast (import-ast-font path))))

(defun sanitize-genera-font-target-name (source-path)
  "Return a stable output filename for SOURCE-PATH."
  (let* ((name (file-namestring source-path))
         (lower (string-downcase name))
         (pos (search ".bdf" lower :from-end t)))
    (if pos
        (concatenate 'string (subseq name 0 (+ pos 4)))
        name)))

(defun directory-bdf-source-files (directory)
  "Return BDF source files from DIRECTORY."
  (sort
   (remove-if-not (lambda (path)
                    (search ".bdf" (string-downcase (file-namestring path))
                            :from-end t))
                  (uiop:directory-files directory))
   #'string<
   :key #'namestring))

(defun extract-genera-bdf-fonts (&key
                                   (target-directory
                                    *font-editor-default-extract-directory*)
                                   (source-directories
                                    *font-editor-genera-bdf-source-directories*))
  "Copy Genera X11 BDF fonts to TARGET-DIRECTORY and return their paths.
These extracted files remain outside the repository."
  (let* ((target (uiop:ensure-directory-pathname target-directory))
         (sources (remove nil
                          (mapcan #'directory-bdf-source-files
                                  (remove-if-not #'probe-file
                                                 source-directories)))))
    (ensure-directories-exist (merge-pathnames #P".keep" target))
    (unless sources
      (let ((existing (directory-bdf-source-files target)))
        (when existing
          (return-from extract-genera-bdf-fonts existing))
        (error "No Genera BDF source directories were found.")))
    (let ((results nil))
      (dolist (source sources (nreverse results))
        (let ((destination
                (merge-pathnames
                 (sanitize-genera-font-target-name source)
                 target)))
          (uiop:copy-file source destination)
          (push destination results))))))

(defun font-editor-buffer-p (buf)
  "Return true when BUF is a font editor buffer."
  (and buf (eq (buffer-kind buf) :font-editor)))

(defun font-editor-buffer-state (buf)
  "Return BUF's font editor state, creating it when needed."
  (unless (font-editor-buffer-p buf)
    (error "Not a font editor buffer: ~A" (and buf (buffer-name buf))))
  (or (gethash buf *font-editor-states*)
      (setf (gethash buf *font-editor-states*)
            (make-font-editor-state))))

(defun font-editor-current-font (buf)
  "Return BUF's current bitmap font."
  (font-editor-state-font (font-editor-buffer-state buf)))

(defun font-editor-current-glyph (buf)
  "Return BUF's currently selected glyph, creating it if missing."
  (let* ((state (font-editor-buffer-state buf))
         (font (font-editor-state-font state))
         (code (font-editor-state-selected-code state)))
    (ensure-bitmap-font-glyph font code)))

(defun font-editor-canvas-width (buf)
  "Return the editable canvas width for BUF's current glyph."
  (let* ((glyph (font-editor-current-glyph buf))
         (font (font-editor-current-font buf)))
    (max 16
         (bitmap-font-default-width font)
         (bitmap-glyph-width glyph))))

(defun font-editor-canvas-height (buf)
  "Return the editable canvas height for BUF's current glyph."
  (let* ((glyph (font-editor-current-glyph buf))
         (font (font-editor-current-font buf)))
    (max 16
         (bitmap-font-line-spacing font)
         (bitmap-glyph-height glyph))))

(defun font-editor-set-status (buf control &rest args)
  "Set BUF's transient status text from CONTROL and ARGS."
  (setf (font-editor-state-status-text (font-editor-buffer-state buf))
        (apply #'format nil control args))
  (notify-buffer-display-change buf :font-editor)
  buf)

(defun font-editor-mark-dirty (buf)
  "Mark BUF's font state dirty and request redisplay."
  (setf (font-editor-state-dirty-p (font-editor-buffer-state buf)) t)
  (notify-buffer-display-change buf :font-editor))

(defun clamp-font-editor-cursor (buf)
  "Clamp BUF's cursor to the current editor canvas."
  (let* ((state (font-editor-buffer-state buf))
         (width (font-editor-canvas-width buf))
         (height (font-editor-canvas-height buf)))
    (setf (font-editor-state-cursor-x state)
          (max 0 (min (font-editor-state-cursor-x state) (1- width)))
          (font-editor-state-cursor-y state)
          (max 0 (min (font-editor-state-cursor-y state) (1- height)))))
  buf)

(defun ensure-current-glyph-fits-cursor (buf)
  "Expand BUF's current glyph bitmap so the current cursor cell exists."
  (let* ((state (font-editor-buffer-state buf))
         (glyph (font-editor-current-glyph buf))
         (need-width (1+ (font-editor-state-cursor-x state)))
         (need-height (1+ (font-editor-state-cursor-y state))))
    (when (or (> need-width (bitmap-glyph-width glyph))
              (> need-height (bitmap-glyph-height glyph)))
      (resize-glyph-bitmap glyph
                           (max need-width (bitmap-glyph-width glyph))
                           (max need-height (bitmap-glyph-height glyph)))
      (font-editor-mark-dirty buf)))
  buf)

(defun font-editor-select-glyph (buf code)
  "Select CODE in BUF and keep the glyph catalog page aligned."
  (let* ((state (font-editor-buffer-state buf))
         (font (font-editor-state-font state))
         (codes (bitmap-font-glyph-codes font))
         (resolved (if (and codes (member code codes))
                       code
                       (bitmap-font-first-glyph-code font))))
    (setf (font-editor-state-selected-code state) resolved
          (font-editor-state-cursor-x state) 0
          (font-editor-state-cursor-y state) 0)
    (let ((page-size 32))
      (when (or (< resolved (font-editor-state-catalog-offset state))
                (>= resolved (+ (font-editor-state-catalog-offset state)
                                page-size)))
        (setf (font-editor-state-catalog-offset state)
              (* page-size (floor resolved page-size)))))
    (clamp-font-editor-cursor buf)
    (notify-buffer-display-change buf :font-editor)
    buf))

(defun font-editor-load-font-into-buffer (buf font path &key save-path)
  "Replace BUF's editor font with FONT loaded from PATH."
  (let ((state (font-editor-buffer-state buf)))
    (setf (font-editor-state-font state) (copy-bitmap-font font)
          (font-editor-state-selected-code state)
          (bitmap-font-first-glyph-code font)
          (font-editor-state-catalog-offset state) 0
          (font-editor-state-cursor-x state) 0
          (font-editor-state-cursor-y state) 0
          (font-editor-state-source-path state) path
          (font-editor-state-save-path state) save-path
          (font-editor-state-dirty-p state) nil)
    (setf (buffer-name buf)
          (format nil "*font:~A*" (bitmap-font-name font)))
    (font-editor-set-status buf "Loaded ~A" (file-namestring path))
    (notify-buffer-display-change buf :font-editor)
    buf))

(defun font-editor-serialize-buffer-state (buf)
  "Return BUF's durable font editor state."
  (let ((state (font-editor-buffer-state buf)))
    `((:font . ,(serialize-bitmap-font (font-editor-state-font state)))
      (:selected-code . ,(font-editor-state-selected-code state))
      (:cursor-x . ,(font-editor-state-cursor-x state))
      (:cursor-y . ,(font-editor-state-cursor-y state))
      (:catalog-offset . ,(font-editor-state-catalog-offset state))
      (:sample-string . ,(font-editor-state-sample-string state))
      (:source-path . ,(and (font-editor-state-source-path state)
                            (namestring (font-editor-state-source-path state))))
      (:save-path . ,(and (font-editor-state-save-path state)
                          (namestring (font-editor-state-save-path state))))
      (:dirty-p . ,(font-editor-state-dirty-p state))
      (:status-text . ,(font-editor-state-status-text state)))))

(defun font-editor-restore-buffer-state (buf persisted-state)
  "Restore PERSISTED-STATE into BUF's font editor state."
  (let ((state (font-editor-buffer-state buf)))
    (setf (font-editor-state-font state)
          (deserialize-bitmap-font (cdr (assoc :font persisted-state)))
          (font-editor-state-selected-code state)
          (or (cdr (assoc :selected-code persisted-state)) 65)
          (font-editor-state-cursor-x state)
          (or (cdr (assoc :cursor-x persisted-state)) 0)
          (font-editor-state-cursor-y state)
          (or (cdr (assoc :cursor-y persisted-state)) 0)
          (font-editor-state-catalog-offset state)
          (or (cdr (assoc :catalog-offset persisted-state)) 0)
          (font-editor-state-sample-string state)
          (or (cdr (assoc :sample-string persisted-state))
              *font-editor-default-sample*)
          (font-editor-state-source-path state)
          (let ((value (cdr (assoc :source-path persisted-state))))
            (and value (pathname value)))
          (font-editor-state-save-path state)
          (let ((value (cdr (assoc :save-path persisted-state))))
            (and value (pathname value)))
          (font-editor-state-dirty-p state)
          (not (null (cdr (assoc :dirty-p persisted-state))))
          (font-editor-state-status-text state)
          (or (cdr (assoc :status-text persisted-state)) ""))
    (clamp-font-editor-cursor buf))
  buf)

(defun make-font-editor-buffer (&key (name *font-editor-buffer-name*)
                                     (working-directory (truename "."))
                                     (add-to-ring-p t))
  "Create a new font editor buffer."
  (let ((buf (make-buffer name
                          :agent-name "font-editor"
                          :kind :font-editor
                          :working-directory working-directory
                          :major-mode "font-editor"
                          :session-persistence-mode :ephemeral)))
    (initialize-buffer-display-defaults buf)
    (setf (gethash buf *font-editor-states*)
          (make-font-editor-state))
    (font-editor-set-status buf
                            "o open  s save  e export-bdf  g extract-genera  arrows move  space toggle")
    (when add-to-ring-p
      (add-buffer-to-ring buf))
    buf))

(defun ensure-font-editor-buffer ()
  "Return an existing font editor buffer or create one."
  (or (find-if #'font-editor-buffer-p *buffer-ring*)
      (make-font-editor-buffer)))

(defun font-editor-visible-catalog-codes (buf &optional (page-size 32))
  "Return BUF's visible glyph catalog codes."
  (let* ((state (font-editor-buffer-state buf))
         (codes (bitmap-font-glyph-codes (font-editor-state-font state)))
         (offset (font-editor-state-catalog-offset state)))
    (subseq codes
            (min offset (length codes))
            (min (+ offset page-size) (length codes)))))

(defun font-editor-toggle-pixel (buf x y)
  "Toggle the current glyph pixel at X/Y in BUF."
  (let* ((state (font-editor-buffer-state buf))
         (glyph (font-editor-current-glyph buf)))
    (setf (font-editor-state-cursor-x state) x
          (font-editor-state-cursor-y state) y)
    (ensure-current-glyph-fits-cursor buf)
    (let ((bitmap (bitmap-glyph-bitmap glyph)))
      (setf (aref bitmap y x)
            (if (zerop (aref bitmap y x)) 1 0)))
    (font-editor-mark-dirty buf)
    (notify-buffer-display-change buf :font-editor)
    buf))

(defun font-editor-clear-glyph (buf)
  "Clear the current glyph bitmap in BUF."
  (let* ((glyph (font-editor-current-glyph buf))
         (bitmap (bitmap-glyph-bitmap glyph)))
    (dotimes (y (array-dimension bitmap 0))
      (dotimes (x (array-dimension bitmap 1))
        (setf (aref bitmap y x) 0))))
  (font-editor-mark-dirty buf)
  (font-editor-set-status buf "Cleared glyph ~D"
                          (font-editor-state-selected-code
                           (font-editor-buffer-state buf))))

(defun font-editor-move-cursor (buf dx dy)
  "Move BUF's edit cursor by DX and DY."
  (let ((state (font-editor-buffer-state buf)))
    (incf (font-editor-state-cursor-x state) dx)
    (incf (font-editor-state-cursor-y state) dy)
    (clamp-font-editor-cursor buf)
    (notify-buffer-display-change buf :font-editor))
  buf)

(defun font-editor-step-glyph (buf step)
  "Select the next or previous glyph in BUF by STEP."
  (let* ((font (font-editor-current-font buf))
         (codes (bitmap-font-glyph-codes font))
         (state (font-editor-buffer-state buf))
         (current (font-editor-state-selected-code state))
         (index (or (position current codes) 0)))
    (when codes
      (font-editor-select-glyph
       buf
       (nth (max 0 (min (+ index step) (1- (length codes)))) codes)))))

(defun font-editor-page-catalog (buf step &optional (page-size 32))
  "Page BUF's glyph catalog by STEP pages."
  (let* ((state (font-editor-buffer-state buf))
         (font (font-editor-state-font state))
         (codes (bitmap-font-glyph-codes font))
         (max-offset (max 0 (- (length codes) page-size))))
    (incf (font-editor-state-catalog-offset state) (* step page-size))
    (setf (font-editor-state-catalog-offset state)
          (max 0
               (min (font-editor-state-catalog-offset state)
                    max-offset))))
  (notify-buffer-display-change buf :font-editor)
  buf)

(defun font-editor-adjust-advance-width (buf delta)
  "Adjust the current glyph advance width in BUF by DELTA."
  (let ((glyph (font-editor-current-glyph buf)))
    (setf (bitmap-glyph-advance-width glyph)
          (max 0 (+ (bitmap-glyph-advance-width glyph) delta))))
  (font-editor-mark-dirty buf)
  buf)

(defun font-editor-adjust-x-offset (buf delta)
  "Adjust the current glyph x-offset in BUF by DELTA."
  (let ((glyph (font-editor-current-glyph buf)))
    (incf (bitmap-glyph-x-offset glyph) delta))
  (font-editor-mark-dirty buf)
  buf)

(defun font-editor-adjust-y-offset (buf delta)
  "Adjust the current glyph y-offset in BUF by DELTA."
  (let ((glyph (font-editor-current-glyph buf)))
    (incf (bitmap-glyph-y-offset glyph) delta))
  (font-editor-mark-dirty buf)
  buf)

(defun font-editor-suggest-save-path (buf extension)
  "Return a suggested save PATH for BUF with EXTENSION."
  (let* ((state (font-editor-buffer-state buf))
         (source (or (font-editor-state-save-path state)
                     (font-editor-state-source-path state)))
         (font (font-editor-state-font state)))
    (or (and source
             (make-pathname :type extension
                            :defaults source))
        (merge-pathnames
         (make-pathname :name (string-downcase (bitmap-font-name font))
                        :type extension)
         (buffer-working-directory buf)))))

(defun font-editor-save-current-font (buf path)
  "Save BUF's font to PATH in native `.clawfont' format."
  (write-clawfont-file (font-editor-state-font (font-editor-buffer-state buf))
                       path)
  (setf (font-editor-state-save-path (font-editor-buffer-state buf)) path
        (font-editor-state-dirty-p (font-editor-buffer-state buf)) nil)
  (font-editor-set-status buf "Saved ~A" (namestring path))
  path)

(defun font-editor-export-current-bdf (buf path)
  "Export BUF's font to PATH as BDF."
  (write-bdf-font (font-editor-state-font (font-editor-buffer-state buf))
                  path)
  (font-editor-set-status buf "Exported BDF ~A" (namestring path))
  path)

(defun font-editor-prompt-path (buf prompt initial-path callback)
  "Prompt for a file path from BUF and call CALLBACK with the pathname."
  (minibuffer-prompt
   prompt
   (lambda (input)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) input)))
       (if (not (plusp (length trimmed)))
           (buffer-insert-system-message buf "[Blank path ignored]")
           (handler-case
               (funcall callback (pathname trimmed))
             (error (condition)
               (buffer-insert-system-message
                buf
                (format nil "[Font editor: ~A]" condition)))))))
   :initial-input (if initial-path
                      (namestring initial-path)
                      "")))

(defun font-editor-open-file-command (buffer)
  "Prompt for a bitmap font file and load it into a font editor buffer."
  (let ((target (if (font-editor-buffer-p buffer)
                    buffer
                    (make-font-editor-buffer))))
    (font-editor-prompt-path
     target
     "Open font file"
     (font-editor-state-source-path (font-editor-buffer-state target))
     (lambda (path)
       (let ((font (read-font-file path)))
         (font-editor-load-font-into-buffer
          target font path
          :save-path (and (eq (detect-font-file-format path) :clawfont)
                          path))
         (switch-to-buffer target)))))
  nil)
(defcommand font-editor-open-file-command)

(defun font-editor-save-font-command (buffer)
  "Save the current font editor buffer as a `.clawfont' file."
  (unless (font-editor-buffer-p buffer)
    (error "Current buffer is not a font editor."))
  (let* ((state (font-editor-buffer-state buffer))
         (path (or (font-editor-state-save-path state)
                   (and (font-editor-state-source-path state)
                        (eq (detect-font-file-format
                             (font-editor-state-source-path state))
                            :clawfont)
                        (font-editor-state-source-path state)))))
    (if path
        (font-editor-save-current-font buffer path)
        (font-editor-prompt-path
         buffer
         "Save .clawfont as"
         (font-editor-suggest-save-path buffer "clawfont")
         (lambda (candidate)
           (font-editor-save-current-font buffer candidate)))))
  nil)
(defcommand font-editor-save-font-command)

(defun font-editor-export-bdf-command (buffer)
  "Prompt for a BDF destination and export the current editor font."
  (unless (font-editor-buffer-p buffer)
    (error "Current buffer is not a font editor."))
  (font-editor-prompt-path
   buffer
   "Export BDF as"
   (font-editor-suggest-save-path buffer "bdf")
   (lambda (candidate)
     (font-editor-export-current-bdf buffer candidate)))
  nil)
(defcommand font-editor-export-bdf-command)

(defun font-editor-set-sample-string-command (buffer)
  "Prompt for a new sample string in the current font editor."
  (unless (font-editor-buffer-p buffer)
    (error "Current buffer is not a font editor."))
  (let ((state (font-editor-buffer-state buffer)))
    (minibuffer-prompt
     "Sample string"
     (lambda (input)
       (setf (font-editor-state-sample-string state) input)
       (font-editor-set-status buffer "Updated sample preview"))
     :initial-input (font-editor-state-sample-string state)))
  nil)
(defcommand font-editor-set-sample-string-command)

(defun font-editor-extract-genera-fonts-command (buffer)
  "Extract locally available Genera BDF fonts outside the repository."
  (declare (ignore buffer))
  (let ((paths (extract-genera-bdf-fonts)))
    (switch-to-buffer
     (make-help-buffer
      "*help:genera-fonts*"
      (format nil
              "Extracted ~D Genera BDF fonts to ~A~%~%Use `font-editor-open-file-command` to import any of them."
              (length paths)
              (namestring *font-editor-default-extract-directory*)))))
  nil)
(defcommand font-editor-extract-genera-fonts-command)

(defun font-editor-command (buffer)
  "Open or switch to a bitmap font editor buffer."
  (declare (ignore buffer))
  (switch-to-buffer (ensure-font-editor-buffer)))
(defcommand font-editor-command)

(defun font-editor-handle-action (buf action)
  "Apply ACTION to BUF and return a redraw marker when appropriate."
  (case action
    (:open (invoke-command buf 'font-editor-open-file-command))
    (:save (invoke-command buf 'font-editor-save-font-command))
    (:export-bdf (invoke-command buf 'font-editor-export-bdf-command))
    (:extract-genera (invoke-command buf 'font-editor-extract-genera-fonts-command))
    (:sample (invoke-command buf 'font-editor-set-sample-string-command))
    (:prev-glyph (font-editor-step-glyph buf -1))
    (:next-glyph (font-editor-step-glyph buf 1))
    (:prev-page (font-editor-page-catalog buf -1))
    (:next-page (font-editor-page-catalog buf 1))
    (:advance-dec (font-editor-adjust-advance-width buf -1))
    (:advance-inc (font-editor-adjust-advance-width buf 1))
    (:xoff-dec (font-editor-adjust-x-offset buf -1))
    (:xoff-inc (font-editor-adjust-x-offset buf 1))
    (:yoff-dec (font-editor-adjust-y-offset buf -1))
    (:yoff-inc (font-editor-adjust-y-offset buf 1))
    (:clear-glyph (font-editor-clear-glyph buf)))
  :redraw)

(defun font-editor-preview-raster (font text)
  "Return a bit grid previewing TEXT in FONT."
  (let* ((chars (coerce text 'list))
         (height (max 1 (bitmap-font-line-spacing font)))
         (width (max 1
                     (loop :with total := 0
                           :for char :in chars
                           :for glyph := (bitmap-font-glyph font (char-code char))
                           :do (incf total (if glyph
                                               (bitmap-glyph-advance-width glyph)
                                               (bitmap-font-default-width font)))
                           :finally (return total))))
         (bitmap (make-array (list height width)
                             :element-type 'bit
                             :initial-element 0))
         (baseline (bitmap-font-baseline font))
         (x-origin 0))
    (dolist (char chars bitmap)
      (let ((glyph (bitmap-font-glyph font (char-code char))))
        (if (null glyph)
            (incf x-origin (bitmap-font-default-width font))
            (let* ((glyph-bitmap (bitmap-glyph-bitmap glyph))
                   (glyph-height (bitmap-glyph-height glyph))
                   (glyph-width (bitmap-glyph-width glyph))
                   (top (- baseline
                           (+ glyph-height
                              (bitmap-glyph-y-offset glyph))))
                   (left (+ x-origin (bitmap-glyph-x-offset glyph))))
              (dotimes (gy glyph-height)
                (dotimes (gx glyph-width)
                  (when (= 1 (aref glyph-bitmap gy gx))
                    (let ((dx (+ left gx))
                          (dy (+ top gy)))
                      (when (and (<= 0 dx) (< dx width)
                                 (<= 0 dy) (< dy height))
                        (setf (aref bitmap dy dx) 1))))))
              (incf x-origin (bitmap-glyph-advance-width glyph))))))))

(defun font-editor-glyph-heading (buf)
  "Return the label for BUF's current glyph."
  (let* ((state (font-editor-buffer-state buf))
         (code (font-editor-state-selected-code state))
         (glyph (font-editor-current-glyph buf))
         (char (printable-code-char code))
         (name (or (bitmap-glyph-name glyph)
                   (default-glyph-name code))))
    (if char
        (format nil "Glyph ~D (~X) ~C  ~A" code code char name)
        (format nil "Glyph ~D (~X)  ~A" code code name))))

(defun font-editor-mini-preview (glyph)
  "Return a compact single-line preview string for GLYPH."
  (let* ((width (min 8 (bitmap-glyph-width glyph)))
         (height (min 3 (bitmap-glyph-height glyph)))
         (bitmap (bitmap-glyph-bitmap glyph)))
    (with-output-to-string (out)
      (dotimes (y height)
        (when (plusp y)
          (write-char #\/ out))
        (dotimes (x width)
          (write-char (if (= 1 (aref bitmap y x)) #\# #\.) out))))))

(defun font-editor-catalog-line (glyph)
  "Return one catalog display line for GLYPH."
  (let* ((code (bitmap-glyph-code glyph))
         (char (printable-code-char code))
         (preview (font-editor-mini-preview glyph)))
    (format nil "~3D ~:[ ~;~C~]  ~A"
            code
            (not (null char))
            (or char #\Space)
            preview)))

(defun font-editor-write-action (pane label action)
  "Write one font-editor action as a native CLIM presentation."
  (clim:with-output-as-presentation
      (pane action 'font-editor-action-ref :single-box t)
    (write-string label pane)))

(defun font-editor-write-raster-lines (pane glyph)
  "Write GLYPH bitmap as text rows; no custom pixel canvas is drawn."
  (let* ((bitmap (bitmap-glyph-bitmap glyph))
         (height (array-dimension bitmap 0))
         (width (array-dimension bitmap 1)))
    (dotimes (y height)
      (dotimes (x width)
        (let ((cell (list :x x :y y)))
          (clim:with-output-as-presentation
              (pane cell 'font-editor-pixel-ref :single-box t)
            (write-char (if (= 1 (aref bitmap y x)) #\# #\.) pane))))
      (terpri pane))))

(defun font-editor-write-catalog (pane buf)
  "Write the visible glyph catalog as native CLIM stream presentations."
  (let* ((state (font-editor-buffer-state buf))
         (selected-code (font-editor-state-selected-code state)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane) (write-string "Code" pane))
        (clim:formatting-cell (pane) (write-string "Glyph" pane))
        (clim:formatting-cell (pane) (write-string "Preview" pane)))
      (dolist (code (font-editor-visible-catalog-codes buf))
        (let* ((glyph (bitmap-font-glyph (font-editor-state-font state) code))
               (char (printable-code-char code)))
          (clim:with-output-as-presentation
              (pane code 'font-editor-glyph-ref :single-box t)
            (clim:formatting-row (pane)
              (clim:formatting-cell (pane)
                (format pane "~:[ ~;>~] ~D"
                        (= code selected-code)
                        code))
              (clim:formatting-cell (pane)
                (format pane "~:[ ~;~C~]"
                        (not (null char))
                        (or char #\Space)))
              (clim:formatting-cell (pane)
                (write-string (font-editor-mini-preview glyph) pane)))))))))

(defun mcclim-render-font-editor-buffer (pane buf rows cols char-w char-h)
  "Render BUF's font editor as a native CLIM presentation-based view.

This intentionally sacrifices the hand-drawn CADR-style pixel canvas. Pixels,
glyphs, and actions remain semantic presentations rendered as stream text."
  (declare (ignore char-w char-h))
  (let* ((state (font-editor-buffer-state buf))
         (font (font-editor-state-font state))
         (glyph (font-editor-current-glyph buf)))
    (write-string (font-editor-glyph-heading buf) pane)
    (terpri pane)
    (format pane "font ~A  baseline ~D  line ~D  dirty ~:[no~;yes~]~%"
            (bitmap-font-name font)
            (bitmap-font-baseline font)
            (bitmap-font-line-spacing font)
            (font-editor-state-dirty-p state))
    (format pane "advance ~D  x-offset ~D  y-offset ~D  source ~A~%"
            (bitmap-glyph-advance-width glyph)
            (bitmap-glyph-x-offset glyph)
            (bitmap-glyph-y-offset glyph)
            (or (and (font-editor-state-source-path state)
                     (file-namestring
                      (font-editor-state-source-path state)))
                "none"))
    (write-string (font-editor-state-status-text state) pane)
    (terpri pane)
    (terpri pane)
    (dolist (entry '(("Open" . :open)
                     ("Save" . :save)
                     ("Export BDF" . :export-bdf)
                     ("Extract Genera BDF" . :extract-genera)
                     ("Sample" . :sample)
                     ("Prev Glyph" . :prev-glyph)
                     ("Next Glyph" . :next-glyph)))
      (font-editor-write-action pane (car entry) (cdr entry))
      (write-string "  " pane))
    (terpri pane)
    (terpri pane)
    (write-string "Bitmap" pane)
    (terpri pane)
    (font-editor-write-raster-lines pane glyph)
    (terpri pane)
    (write-string "Glyph Catalog" pane)
    (terpri pane)
    (font-editor-write-catalog pane buf)
    (when (fboundp 'mcclim-record-render-snapshot)
      (funcall (symbol-function 'mcclim-record-render-snapshot)
               (clim:pane-frame pane)
               pane
               buf
               :font-editor
               rows
               cols
               :input-start-row -1
               :history-height 0
               :visible-messages nil))))

(defun font-editor-empty-input-pane (pane buf rows cols char-w char-h)
  "Render the empty input area for a font editor buffer."
  (declare (ignore pane buf rows cols char-w char-h))
  nil)

(defun font-editor-handle-key (buf key)
  "Handle BUF-local key input for the bitmap font editor."
  (unless (font-editor-buffer-p buf)
    (return-from font-editor-handle-key nil))
  (case key
    (:left (font-editor-move-cursor buf -1 0))
    (:right (font-editor-move-cursor buf 1 0))
    (:up (font-editor-move-cursor buf 0 -1))
    (:down (font-editor-move-cursor buf 0 1))
    (#\Space
     (let ((state (font-editor-buffer-state buf)))
       (font-editor-toggle-pixel buf
                                 (font-editor-state-cursor-x state)
                                 (font-editor-state-cursor-y state))))
    (#\p (font-editor-step-glyph buf -1))
    (#\n (font-editor-step-glyph buf 1))
    (#\< (font-editor-page-catalog buf -1))
    (#\> (font-editor-page-catalog buf 1))
    (#\+ (font-editor-adjust-advance-width buf 1))
    (#\- (font-editor-adjust-advance-width buf -1))
    (#\{ (font-editor-adjust-x-offset buf -1))
    (#\} (font-editor-adjust-x-offset buf 1))
    (#\[ (font-editor-adjust-y-offset buf -1))
    (#\] (font-editor-adjust-y-offset buf 1))
    (#\c (font-editor-clear-glyph buf))
    (#\o (invoke-command buf 'font-editor-open-file-command))
    (#\s (invoke-command buf 'font-editor-save-font-command))
    (#\e (invoke-command buf 'font-editor-export-bdf-command))
    (#\g (invoke-command buf 'font-editor-extract-genera-fonts-command))
    (#\t (invoke-command buf 'font-editor-set-sample-string-command))
    (otherwise
     (return-from font-editor-handle-key nil)))
  :redraw)

(register-buffer-type
 :font-editor
 :description "Interactive CADR-style bitmap font editor."
 :major-mode "font-editor"
 :serialize-state-function 'font-editor-serialize-buffer-state
 :restore-state-function 'font-editor-restore-buffer-state)
