(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; McCLIM Graphical Backend
;;;
;;; A graphical UI backend using McCLIM that mirrors the terminal interface:
;;; monospace font, dark background, three-pane layout (main, modeline,
;;; minibuffer). Optional dependency — load via (asdf:load-system :clawmacs/mcclim).
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; Color Mapping — color-spec → CLIM ink
;;; --------------------------------------------------------------------------

(defun color-spec-to-clim-ink (cs)
  "Convert a color-spec to a CLIM ink (color object).
Handles :CGA (0-15), :256 (xterm-256 palette), and :HEX (#RRGGBB) types."
  (ecase (color-spec-type cs)
    (:cga
     (let ((val (color-spec-value cs)))
       (case val
         (0  (clim:make-rgb-color 0.0 0.0 0.0))         ; black
         (1  (clim:make-rgb-color 0.67 0.0 0.0))        ; red
         (2  (clim:make-rgb-color 0.0 0.67 0.0))        ; green
         (3  (clim:make-rgb-color 0.67 0.33 0.0))       ; brown
         (4  (clim:make-rgb-color 0.0 0.0 0.67))        ; blue
         (5  (clim:make-rgb-color 0.67 0.0 0.67))       ; magenta
         (6  (clim:make-rgb-color 0.0 0.67 0.67))       ; cyan
         (7  (clim:make-rgb-color 0.67 0.67 0.67))      ; gray
         (8  (clim:make-rgb-color 0.33 0.33 0.33))      ; dark gray
         (9  (clim:make-rgb-color 1.0 0.33 0.33))       ; bright red
         (10 (clim:make-rgb-color 0.33 1.0 0.33))       ; bright green
         (11 (clim:make-rgb-color 1.0 1.0 0.33))        ; bright yellow
         (12 (clim:make-rgb-color 0.33 0.33 1.0))       ; bright blue
         (13 (clim:make-rgb-color 1.0 0.33 1.0))        ; bright magenta
         (14 (clim:make-rgb-color 0.33 1.0 1.0))        ; bright cyan
         (15 (clim:make-rgb-color 1.0 1.0 1.0))         ; bright white
         (otherwise (clim:make-rgb-color 0.67 0.67 0.67)))))
    (:256
     (let ((val (color-spec-value cs)))
       (cond
         ;; Standard colors 0-15: recurse with CGA mapping
         ((< val 16)
          (color-spec-to-clim-ink (make-color-spec :cga val)))
         ;; 6x6x6 color cube: indices 16-231
         ((< val 232)
          (let* ((idx (- val 16))
                 (r-idx (floor idx 36))
                 (g-idx (floor (mod idx 36) 6))
                 (b-idx (mod idx 6))
                 (r (if (zerop r-idx) 0.0 (/ (+ 55 (* 40 r-idx)) 255.0)))
                 (g (if (zerop g-idx) 0.0 (/ (+ 55 (* 40 g-idx)) 255.0)))
                 (b (if (zerop b-idx) 0.0 (/ (+ 55 (* 40 b-idx)) 255.0))))
            (clim:make-rgb-color r g b)))
         ;; Grayscale ramp: indices 232-255
         (t
          (let ((level (/ (+ 8 (* 10 (- val 232))) 255.0)))
            (clim:make-rgb-color level level level))))))
    (:hex
     (let ((hex (color-spec-value cs)))
       (if (and (stringp hex) (= (length hex) 7) (char= (char hex 0) #\#))
           (let ((r (/ (parse-integer hex :start 1 :end 3 :radix 16) 255.0))
                 (g (/ (parse-integer hex :start 3 :end 5 :radix 16) 255.0))
                 (b (/ (parse-integer hex :start 5 :end 7 :radix 16) 255.0)))
             (clim:make-rgb-color r g b))
           (clim:make-rgb-color 1.0 1.0 1.0))))))

;;; --------------------------------------------------------------------------
;;; Face → Ink Resolution
;;; --------------------------------------------------------------------------

(defun resolve-face-inks (resolved-face)
  "Convert a resolved-face to CLIM drawing parameters.
Returns (values fg-ink bg-ink text-style) where text-style encodes bold."
  (let* ((fg (color-spec-to-clim-ink (resolved-face-foreground resolved-face)))
         (bg (color-spec-to-clim-ink (resolved-face-background resolved-face)))
         (bold-p (resolved-face-bold-p resolved-face))
         (ts (clim:make-text-style :fix (if bold-p :bold :roman) :normal)))
    ;; Handle reverse: swap fg/bg
    (when (resolved-face-reverse-p resolved-face)
      (rotatef fg bg))
    (values fg bg ts)))

(defun resolve-global-face-inks (face-name)
  "Resolve a global face by keyword NAME to CLIM inks.
Returns (values fg-ink bg-ink text-style), or nil values if face not found."
  (let ((face (global-face face-name)))
    (if face
        (resolve-face-inks (resolve-face face))
        (values (clim:make-rgb-color 0.67 0.67 0.67)
                (clim:make-rgb-color 0.0 0.0 0.0)
                (clim:make-text-style :fix :roman :normal)))))

;;; --------------------------------------------------------------------------
;;; Application Frame
;;; --------------------------------------------------------------------------

(clim:define-application-frame clawmacs-gui ()
  ((backend :initarg :backend :accessor frame-backend)
   (char-width :accessor frame-char-width :initform 0)
   (char-height :accessor frame-char-height :initform 0)
   (quit-flag :accessor frame-quit-flag :initform nil))
  (:panes
   (main-pane :application
              :display-function 'display-main-pane
              :text-style (clim:make-text-style :fix :roman :normal)
              :scroll-bars nil
              :background (clim:make-rgb-color 0.0 0.0 0.0)
              :foreground (clim:make-rgb-color 0.67 0.67 0.67))
   (modeline-pane :application
                  :display-function 'display-modeline-pane
                  :text-style (clim:make-text-style :fix :roman :normal)
                  :scroll-bars nil
                  :background (clim:make-rgb-color 0.67 0.67 0.67)
                  :foreground (clim:make-rgb-color 0.0 0.0 0.0))
   (minibuffer-pane :application
                    :display-function 'display-minibuffer-pane
                    :text-style (clim:make-text-style :fix :roman :normal)
                    :scroll-bars nil
                    :background (clim:make-rgb-color 0.0 0.0 0.0)
                    :foreground (clim:make-rgb-color 0.67 0.67 0.67)))
  (:layouts
   (default
    (clim:vertically ()
      main-pane
      modeline-pane
      minibuffer-pane))))

;;; --------------------------------------------------------------------------
;;; Drawing Primitives
;;; --------------------------------------------------------------------------

(defun ensure-char-metrics (frame pane)
  "Compute character width and height lazily on first display call.
The pane's medium must be connected to the backend (CLX) at this point."
  (when (zerop (frame-char-width frame))
    (let* ((ts (clim:make-text-style :fix :roman :normal))
           (medium (clim:sheet-medium pane)))
      (multiple-value-bind (width height)
          (clim:text-size medium "M" :text-style ts)
        (setf (frame-char-width frame) width)
        (setf (frame-char-height frame) height)))))

(defun draw-text-at (pane row col text fg-ink bg-ink text-style char-w char-h)
  "Draw TEXT at character grid position (ROW, COL) in PANE.
Fills a background rectangle first, then draws the text on top."
  (let* ((x (* col char-w))
         (y (* row char-h))
         (text-width (* (length text) char-w)))
    ;; Background rectangle
    (clim:draw-rectangle* pane x y (+ x text-width) (+ y char-h)
                          :ink bg-ink)
    ;; Text — baseline is at y + ascent
    (clim:draw-text* pane text x y
                     :text-style text-style
                     :ink fg-ink
                     :align-y :top)))

(defun draw-underline-at (pane row col length fg-ink char-w char-h)
  "Draw an underline under LENGTH characters starting at (ROW, COL)."
  (let* ((x (* col char-w))
         (y (+ (* (1+ row) char-h) -1))
         (x2 (+ x (* length char-w))))
    (clim:draw-line* pane x y x2 y :ink fg-ink)))

(defun clear-pane-with-ink (pane ink)
  "Fill the entire PANE with a solid color INK."
  (let ((region (clim:sheet-region pane)))
    (clim:draw-rectangle* pane
                          (clim:bounding-rectangle-min-x region)
                          (clim:bounding-rectangle-min-y region)
                          (clim:bounding-rectangle-max-x region)
                          (clim:bounding-rectangle-max-y region)
                          :ink ink)))

(defun fill-row (pane row cols bg-ink char-w char-h)
  "Fill an entire row with background color."
  (clim:draw-rectangle* pane
                        0 (* row char-h)
                        (* cols char-w) (* (1+ row) char-h)
                        :ink bg-ink))

(defun pane-grid-dimensions (pane char-w char-h)
  "Return (values cols rows) — the character grid size of PANE."
  (let ((region (clim:sheet-region pane)))
    (values (max 1 (floor (clim:bounding-rectangle-width region) char-w))
            (max 1 (floor (clim:bounding-rectangle-height region) char-h)))))

;;; --------------------------------------------------------------------------
;;; Modeline Display
;;; --------------------------------------------------------------------------

(defun display-modeline-pane (frame pane)
  "Display function for the modeline pane."
  (ensure-char-metrics frame pane)
  (let* ((char-w (frame-char-width frame))
         (char-h (frame-char-height frame))
         (buf (current-buffer)))
    (when (zerop char-w) (return-from display-modeline-pane))
    (multiple-value-bind (cols rows) (pane-grid-dimensions pane char-w char-h)
      (declare (ignore rows))
      (mcclim-render-modeline pane buf cols char-w char-h))))

(defun mcclim-render-modeline (pane buf cols char-w char-h)
  "Render the modeline string into PANE using the modeline face."
  (let* ((ml-face (make-modeline-face))
         (resolved (resolve-face ml-face))
         (text (format-modeline buf cols :major-mode (buffer-major-mode buf))))
    (multiple-value-bind (fg bg ts) (resolve-face-inks resolved)
      (fill-row pane 0 cols bg char-w char-h)
      (draw-text-at pane 0 0 text fg bg ts char-w char-h))))

;;; --------------------------------------------------------------------------
;;; Main Pane Display
;;; --------------------------------------------------------------------------

(defun display-main-pane (frame pane)
  "Display function for the main pane. Dispatches to buffer/selector rendering."
  (ensure-char-metrics frame pane)
  (let* ((char-w (frame-char-width frame))
         (char-h (frame-char-height frame)))
    (when (zerop char-w) (return-from display-main-pane))
    (multiple-value-bind (cols rows) (pane-grid-dimensions pane char-w char-h)
      (let ((modeline-pane (clim:find-pane-named frame 'modeline-pane)))
        (cond
          (*buffer-selector-active*
           (mcclim-render-buffer-selector pane modeline-pane rows cols
                                          char-w char-h frame))
          (*model-selector-active*
           (mcclim-render-model-selector pane modeline-pane rows cols
                                         char-w char-h frame))
          (t
           (mcclim-render-buffer pane (current-buffer) rows cols
                                 char-w char-h)))))))

;;; --------------------------------------------------------------------------
;;; Buffer Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-buffer (pane buf rows cols char-w char-h)
  "Render the full buffer: history + input into PANE."
  (let* ((total-height rows)
         (width cols)
         (input-height (calculate-input-height buf total-height width))
         (history-height (- total-height input-height))
         (input-start-row history-height))
    ;; Clear pane
    (clear-pane-with-ink pane (clim:make-rgb-color 0.0 0.0 0.0))
    ;; Collect history messages
    (let ((history-messages nil)
          (hide-tool-results (not (buffer-show-tool-results-p buf))))
      (loop :for msg := (buffer-first-message buf) :then (message-next msg)
            :while (and msg (not (eq msg (buffer-input-message buf))))
            :do (unless (and hide-tool-results
                             (or (eq :tool-result (message-sender msg))
                                 (and (message-raw-content msg)
                                      (not (eq :user (message-sender msg)))
                                      (every (lambda (block)
                                               (let ((btype (cdr (assoc :type block))))
                                                 (not (string= "text" (or btype "")))))
                                             (message-raw-content msg)))))
                  (push msg history-messages)))
      (setf history-messages (nreverse history-messages))
      ;; Calculate visual heights
      (let* ((msg-heights (mapcar (lambda (m) (message-visual-height m width))
                                  history-messages))
             (total-history-rows (reduce #'+ msg-heights :initial-value 0))
             (max-scroll (max 0 (- total-history-rows history-height)))
             (scroll-offset (min (buffer-scroll-offset buf) max-scroll))
             (visible-bottom (- total-history-rows scroll-offset))
             (visible-top (- visible-bottom history-height)))
        (setf (buffer-scroll-offset buf) scroll-offset)
        ;; Render visible history messages
        (let ((virtual-row 0))
          (loop :for msg :in history-messages
                :for msg-h :in msg-heights
                :for msg-top := virtual-row
                :for msg-bottom := (+ virtual-row msg-h)
                :do (setf virtual-row msg-bottom)
                    (when (and (< msg-top visible-bottom)
                               (> msg-bottom visible-top))
                      (let ((screen-row (- msg-top visible-top)))
                        (mcclim-render-message-lines pane msg screen-row width
                                                     char-w char-h
                                                     :max-rows history-height)))))))
    ;; Render input area
    (if (buffer-approval-pending buf)
        (mcclim-render-approval-prompt pane buf input-start-row cols char-w char-h rows)
        (mcclim-render-message-lines pane (buffer-input-message buf)
                                     input-start-row cols char-w char-h
                                     :show-cursor t :max-rows rows))))

;;; --------------------------------------------------------------------------
;;; Message Line Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-message-lines (pane msg start-row width char-w char-h
                                    &key show-cursor (max-rows 1000))
  "Render MSG's lines into PANE starting at START-ROW with line wrapping.
Returns the number of visual rows consumed."
  (let* ((prefix (message-sender-prefix msg))
         (prefix-len (length prefix))
         (display-width (max 1 (- width prefix-len)))
         (face-set (message-face-set msg))
         (face (if face-set
                   (or (get-face face-set :default) (make-default-text-face))
                   (make-default-text-face)))
         (resolved (resolve-face face))
         (row start-row)
         (cursor-y nil)
         (cursor-x nil))
    (multiple-value-bind (fg bg ts) (resolve-face-inks resolved)
      (let ((underline-p (resolved-face-underline-p resolved)))
        (loop :for line := (message-first-line msg) :then (line-next line)
              :for line-idx :from 0
              :while (and line (< row max-rows))
              :do
                 (let* ((content (line-content line))
                        (content-len (length content))
                        (num-wraps (wrapped-line-count content display-width)))
                   (dotimes (wrap-idx num-wraps)
                     (when (< row max-rows)
                       (let* ((chunk-start (* wrap-idx display-width))
                              (chunk-end (min (* (1+ wrap-idx) display-width) content-len))
                              (chunk (subseq content chunk-start chunk-end)))
                         ;; Fill entire row background
                         (fill-row pane row width bg char-w char-h)
                         ;; Show prefix on first visual row of message
                         (when (and (= line-idx 0) (= wrap-idx 0))
                           (draw-text-at pane row 0
                                         (concatenate 'string prefix chunk)
                                         fg bg ts char-w char-h)
                           (when underline-p
                             (draw-underline-at pane row 0
                                                (+ prefix-len (length chunk))
                                                fg char-w char-h))
                           (setf chunk nil))
                         (when chunk
                           (draw-text-at pane row prefix-len chunk
                                         fg bg ts char-w char-h)
                           (when underline-p
                             (draw-underline-at pane row prefix-len (length chunk)
                                                fg char-w char-h)))
                         ;; Track cursor position
                         (when (and show-cursor (eq line (message-point-line msg)))
                           (let ((point-off (message-point-offset msg)))
                             (when (and (>= point-off chunk-start)
                                        (or (< point-off chunk-end)
                                            (and (= wrap-idx (1- num-wraps))
                                                 (= point-off chunk-end))))
                               (setf cursor-y row
                                     cursor-x (+ prefix-len (- point-off chunk-start))))))
                         (incf row))))))
        ;; Render cursor as reverse-video block
        (when (and show-cursor cursor-y cursor-x)
          (let* ((cx (min cursor-x (1- width)))
                 (point-line (message-point-line msg))
                 (point-off (message-point-offset msg))
                 (content (when point-line (line-content point-line)))
                 (char-at-point (if (and content (< point-off (length content)))
                                    (char content point-off)
                                    #\Space)))
            ;; Draw with swapped colors (reverse video)
            (draw-text-at pane cursor-y cx (string char-at-point)
                          bg fg
                          (clim:make-text-style :fix :bold :normal)
                          char-w char-h)))))
    (- row start-row)))

;;; --------------------------------------------------------------------------
;;; Approval Prompt Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-approval-prompt (pane buf start-row width char-w char-h max-rows)
  "Render the permission approval prompt in the input area."
  (let* ((approval (buffer-approval-pending buf))
         (tool-name (cdr (assoc :tool-name approval)))
         (raw-sexpr (cdr (assoc :display-raw approval)))
         (expanded (cdr (assoc :display-expanded approval)))
         (row start-row))
    ;; Header
    (multiple-value-bind (fg bg ts) (resolve-global-face-inks :approval-header)
      (when (< row max-rows)
        (fill-row pane row width bg char-w char-h)
        (let ((header-text (format nil "-- PERMISSION REQUIRED: ~A " tool-name)))
          ;; Draw separator dashes
          (draw-text-at pane row 0
                        (make-string width :initial-element #\-)
                        fg bg ts char-w char-h)
          ;; Draw header text over it
          (draw-text-at pane row 0
                        (subseq header-text 0 (min (length header-text) width))
                        fg bg ts char-w char-h))
        (incf row)))
    ;; Raw sexpr
    (multiple-value-bind (fg bg ts) (resolve-global-face-inks :approval-code)
      (dolist (line (split-string-by-newline raw-sexpr))
        (when (< row max-rows)
          (fill-row pane row width bg char-w char-h)
          (draw-text-at pane row 0
                        (subseq line 0 (min (length line) width))
                        fg bg ts char-w char-h)
          (incf row))))
    ;; Expanded form
    (multiple-value-bind (fg bg ts) (resolve-global-face-inks :approval-text)
      (dolist (line (split-string-by-newline expanded))
        (when (< row max-rows)
          (fill-row pane row width bg char-w char-h)
          (draw-text-at pane row 0
                        (subseq line 0 (min (length line) width))
                        fg bg ts char-w char-h)
          (incf row))))
    ;; Extra display (diff)
    (let ((extra (cdr (assoc :display-extra approval))))
      (when extra
        (dolist (line (split-string-by-newline extra))
          (when (< row max-rows)
            (multiple-value-bind (fg bg ts)
                (cond
                  ((and (plusp (length line)) (char= (char line 0) #\+))
                   (resolve-global-face-inks :approval-diff-add))
                  ((and (plusp (length line)) (char= (char line 0) #\-))
                   (resolve-global-face-inks :approval-diff-remove))
                  (t
                   (resolve-global-face-inks :approval-text)))
              (fill-row pane row width bg char-w char-h)
              (draw-text-at pane row 0
                            (subseq line 0 (min (length line) width))
                            fg bg ts char-w char-h))
            (incf row)))))
    ;; Options
    (when (< (1+ row) max-rows)
      (incf row) ; blank line
      (multiple-value-bind (fg bg ts) (resolve-global-face-inks :approval-options)
        (fill-row pane row width bg char-w char-h)
        (draw-text-at pane row 0
                      "[a]pprove  [d]eny  [m]essage (deny with note to agent)"
                      fg bg ts char-w char-h)))))

;;; --------------------------------------------------------------------------
;;; Buffer Selector Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-buffer-selector (pane modeline-pane rows cols
                                      char-w char-h frame)
  "Render the buffer selector overlay."
  (let* ((width cols)
         (height rows)
         (buffers *buffer-ring*)
         (num-buffers (length buffers))
         (current (first *buffer-ring*))
         (max-entries (max 1 (- height 7)))
         (scroll (cond
                   ((< *buffer-selector-index* *buffer-selector-scroll*)
                    *buffer-selector-index*)
                   ((>= *buffer-selector-index*
                        (+ *buffer-selector-scroll* max-entries))
                    (max 0 (1+ (- *buffer-selector-index* max-entries))))
                   (t *buffer-selector-scroll*))))
    (setf *buffer-selector-scroll* scroll)
    (clear-pane-with-ink pane (clim:make-rgb-color 0.0 0.0 0.0))
    ;; Title
    (when (< 1 height)
      (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-title)
        (draw-text-at pane 1 2 "Agent Sessions" fg bg ts char-w char-h)))
    ;; Separator
    (when (< 2 height)
      (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-separator)
        (draw-text-at pane 2 2
                      (make-string (min (- width 4) 50) :initial-element #\─)
                      fg bg ts char-w char-h)))
    ;; Column headers
    (when (< 3 height)
      (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-header)
        (let ((header (format-selector-line "  " "NAME" "AGENT" "STATUS" "MSGS" width)))
          (fill-row pane 3 width bg char-w char-h)
          (draw-text-at pane 3 0
                        (subseq header 0 (min (length header) width))
                        fg bg ts char-w char-h))))
    ;; Buffer entries
    (loop :for absolute-idx :from scroll
          :below (min (+ scroll max-entries) num-buffers)
          :for buf := (nth absolute-idx buffers)
          :for row := (+ 5 (- absolute-idx scroll))
          :while (< row (- height 2))
          :for selected-p := (= absolute-idx *buffer-selector-index*)
          :for current-p := (eq buf current)
          :for marker := (cond ((and selected-p current-p) "▸*")
                               (selected-p "▸ ")
                               (current-p " *")
                               (t "  "))
          :for name := (buffer-name buf)
          :for agent := (buffer-agent-name buf)
          :for status := (string-downcase (symbol-name (buffer-status buf)))
          :for msg-count := (max 0 (1- (buffer-message-count buf)))
          :for count-str := (format nil "~D" msg-count)
          :for line := (format-selector-line marker name agent status count-str width)
          :do (multiple-value-bind (fg bg ts)
                  (resolve-global-face-inks (if selected-p
                                                :selector-selected
                                                :selector-entry))
                (fill-row pane row width bg char-w char-h)
                (draw-text-at pane row 0
                              (subseq line 0 (min (length line) width))
                              fg bg ts char-w char-h)))
    ;; Scroll indicator
    (when (> num-buffers max-entries)
      (let ((indicator (format nil "[~D-~D of ~D]"
                               (1+ scroll)
                               (min (+ scroll max-entries) num-buffers)
                               num-buffers))
            (ind-row (+ 5 (min max-entries (- num-buffers scroll)))))
        (when (< ind-row (- height 1))
          (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-scroll)
            (draw-text-at pane ind-row 2
                          (subseq indicator 0 (min (length indicator) (- width 4)))
                          fg bg ts char-w char-h)))))
    ;; Footer
    (let ((footer-row (1- height)))
      (when (plusp footer-row)
        (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-footer)
          (draw-text-at pane footer-row 2
                        "[RET] select  [C-g/q] cancel  [n] new  [k] kill"
                        fg bg ts char-w char-h))))
    ;; Custom modeline for selector
    (let* ((ml-face (make-modeline-face))
           (resolved (resolve-face ml-face))
           (ml-text (format nil " [buffer-selector] Agent Sessions | ~D session~:[s~;~]"
                            num-buffers (= num-buffers 1))))
      (multiple-value-bind (cols-ml _rows-ml)
          (pane-grid-dimensions modeline-pane char-w char-h)
        (declare (ignore _rows-ml))
        (multiple-value-bind (fg bg ts) (resolve-face-inks resolved)
          (let ((padded (if (<= (length ml-text) cols-ml)
                            (concatenate 'string ml-text
                                         (make-string (- cols-ml (length ml-text))
                                                      :initial-element #\Space))
                            (subseq ml-text 0 cols-ml))))
            (fill-row modeline-pane 0 cols-ml bg char-w char-h)
            (draw-text-at modeline-pane 0 0 padded fg bg ts char-w char-h)))))))

;;; --------------------------------------------------------------------------
;;; Model Selector Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-model-selector (pane modeline-pane rows cols
                                     char-w char-h frame)
  "Render the model selector overlay."
  (let* ((width cols)
         (height rows)
         (entries *model-selector-entries*)
         (num-entries (length entries))
         (current-buf (current-buffer))
         (max-visible (max 1 (- height 7)))
         (scroll (cond
                   ((< *model-selector-index* *model-selector-scroll*)
                    *model-selector-index*)
                   ((>= *model-selector-index*
                        (+ *model-selector-scroll* max-visible))
                    (max 0 (1+ (- *model-selector-index* max-visible))))
                   (t *model-selector-scroll*))))
    (setf *model-selector-scroll* scroll)
    (clear-pane-with-ink pane (clim:make-rgb-color 0.0 0.0 0.0))
    ;; Title
    (when (< 1 height)
      (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-title)
        (draw-text-at pane 1 2 "Select Model" fg bg ts char-w char-h)))
    ;; Separator
    (when (< 2 height)
      (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-separator)
        (draw-text-at pane 2 2
                      (make-string (min (- width 4) 50) :initial-element #\─)
                      fg bg ts char-w char-h)))
    ;; Column headers
    (when (< 3 height)
      (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-header)
        (let ((header (format-model-selector-line "  " "PROVIDER" "MODEL" width)))
          (fill-row pane 3 width bg char-w char-h)
          (draw-text-at pane 3 0
                        (subseq header 0 (min (length header) width))
                        fg bg ts char-w char-h))))
    ;; Model entries
    (loop :for absolute-idx :from scroll
          :below (min (+ scroll max-visible) num-entries)
          :for entry := (nth absolute-idx entries)
          :for row := (+ 5 (- absolute-idx scroll))
          :while (< row (- height 2))
          :for selected-p := (= absolute-idx *model-selector-index*)
          :for active-p := (getf entry :active-p)
          :for provider := (string-downcase (symbol-name (getf entry :provider)))
          :for model := (getf entry :model)
          :for marker := (cond ((and selected-p active-p) "▸*")
                               (selected-p "▸ ")
                               (active-p " *")
                               (t "  "))
          :for line := (format-model-selector-line marker provider model width)
          :do (multiple-value-bind (fg bg ts)
                  (resolve-global-face-inks (if selected-p
                                                :selector-selected
                                                :selector-entry))
                (fill-row pane row width bg char-w char-h)
                (draw-text-at pane row 0
                              (subseq line 0 (min (length line) width))
                              fg bg ts char-w char-h)))
    ;; Scroll indicator
    (when (> num-entries max-visible)
      (let ((indicator (format nil "[~D-~D of ~D]"
                               (1+ scroll)
                               (min (+ scroll max-visible) num-entries)
                               num-entries))
            (ind-row (+ 5 (min max-visible (- num-entries scroll)))))
        (when (< ind-row (- height 1))
          (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-scroll)
            (draw-text-at pane ind-row 2
                          (subseq indicator 0 (min (length indicator) (- width 4)))
                          fg bg ts char-w char-h)))))
    ;; Footer
    (let ((footer-row (1- height)))
      (when (plusp footer-row)
        (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-footer)
          (draw-text-at pane footer-row 2
                        "[RET] select  [C-g/q] cancel  * = active"
                        fg bg ts char-w char-h))))
    ;; Custom modeline
    (let* ((ml-face (make-modeline-face))
           (resolved (resolve-face ml-face))
           (pm (resolve-modeline-provider-model current-buf))
           (ml-text (format nil " [model-selector] ~A | ~D model~:[s~;~] available"
                            pm num-entries (= num-entries 1))))
      (multiple-value-bind (cols-ml _rows-ml)
          (pane-grid-dimensions modeline-pane char-w char-h)
        (declare (ignore _rows-ml))
        (multiple-value-bind (fg bg ts) (resolve-face-inks resolved)
          (let ((padded (if (<= (length ml-text) cols-ml)
                            (concatenate 'string ml-text
                                         (make-string (- cols-ml (length ml-text))
                                                      :initial-element #\Space))
                            (subseq ml-text 0 cols-ml))))
            (fill-row modeline-pane 0 cols-ml bg char-w char-h)
            (draw-text-at modeline-pane 0 0 padded fg bg ts char-w char-h)))))))

;;; --------------------------------------------------------------------------
;;; Minibuffer Display
;;; --------------------------------------------------------------------------

(defun display-minibuffer-pane (frame pane)
  "Display function for the minibuffer pane."
  (ensure-char-metrics frame pane)
  (let* ((char-w (frame-char-width frame))
         (char-h (frame-char-height frame)))
    (when (zerop char-w) (return-from display-minibuffer-pane))
    (multiple-value-bind (cols rows) (pane-grid-dimensions pane char-w char-h)
      (if *minibuffer-active*
          (mcclim-render-minibuffer-active pane cols rows char-w char-h)
          (mcclim-render-minibuffer-inactive pane cols char-w char-h)))))

(defun mcclim-render-minibuffer-inactive (pane cols char-w char-h)
  "Render the inactive minibuffer: a blank line."
  (multiple-value-bind (fg bg ts) (resolve-global-face-inks :minibuffer-prompt)
    (declare (ignore fg ts))
    (fill-row pane 0 cols bg char-w char-h)))

(defun mcclim-render-minibuffer-active (pane cols rows char-w char-h)
  "Render the active minibuffer with prompt, input, cursor, and candidates."
  (let* ((prompt-str (format nil "~A: " *minibuffer-prompt*))
         (input *minibuffer-input*)
         (prompt-line (concatenate 'string prompt-str input))
         (cursor-col (+ (length prompt-str) *minibuffer-point*)))
    ;; Prompt line
    (multiple-value-bind (fg bg ts) (resolve-global-face-inks :minibuffer-prompt)
      (fill-row pane 0 cols bg char-w char-h)
      (draw-text-at pane 0 0
                    (subseq prompt-line 0 (min (length prompt-line) cols))
                    fg bg ts char-w char-h))
    ;; Block cursor
    (when (< cursor-col cols)
      (let ((char-at-cursor (if (< *minibuffer-point* (length input))
                                (char input *minibuffer-point*)
                                #\Space)))
        (multiple-value-bind (fg bg ts) (resolve-global-face-inks :minibuffer-cursor)
          (draw-text-at pane 0 cursor-col (string char-at-cursor)
                        fg bg ts char-w char-h))))
    ;; Candidate list
    (let* ((items *minibuffer-filtered-items*)
           (positions *minibuffer-match-positions*)
           (selected *minibuffer-selected-index*)
           (scroll *minibuffer-scroll-offset*)
           (visible-rows (1- rows))
           (total (length items)))
      (loop :for row-idx :from 0 :below visible-rows
            :for item-idx := (+ scroll row-idx)
            :while (< item-idx total)
            :for item := (nth item-idx items)
            :for row := (1+ row-idx)
            :for display := (minibuffer-item-display item)
            :for match-pos := (nth item-idx positions)
            :for selected-p := (= item-idx selected)
            :do (mcclim-render-candidate-row pane row display match-pos
                                             selected-p cols char-w char-h)))))

(defun mcclim-render-candidate-row (pane row display match-positions
                                    selected-p cols char-w char-h)
  "Render a single minibuffer candidate with fuzzy match highlighting."
  (let* ((base-face-name (if selected-p :minibuffer-selected :minibuffer-candidate))
         (match-face-name (if selected-p :minibuffer-selected-match :minibuffer-match))
         (match-set (when match-positions
                      (let ((ht (make-hash-table :test #'eql)))
                        (dolist (p match-positions ht)
                          (setf (gethash p ht) t))))))
    ;; Clear row background
    (multiple-value-bind (fg bg ts) (resolve-global-face-inks base-face-name)
      (declare (ignore fg ts))
      (fill-row pane row cols bg char-w char-h))
    ;; Write "  " indent + each character with appropriate face
    (multiple-value-bind (base-fg base-bg base-ts) (resolve-global-face-inks base-face-name)
      (draw-text-at pane row 0 "  " base-fg base-bg base-ts char-w char-h)
      (loop :for i :from 0 :below (length display)
            :for col :from 2
            :while (< col cols)
            :for ch := (char display i)
            :for matched-p := (and match-set (gethash i match-set))
            :do (if matched-p
                    (multiple-value-bind (mfg mbg mts)
                        (resolve-global-face-inks match-face-name)
                      (draw-text-at pane row col (string ch)
                                    mfg mbg mts char-w char-h))
                    (draw-text-at pane row col (string ch)
                                  base-fg base-bg base-ts char-w char-h))))))

;;; --------------------------------------------------------------------------
;;; Key Normalization (McCLIM-specific)
;;; --------------------------------------------------------------------------

(defun mcclim-normalize-key (key-event)
  "Normalize a McCLIM key-press-event to the same abstract format as croatoan.
Returns a character, a keyword, a list (:alt key), (:ctrl-x key), etc."
  (let* ((char (clim:keyboard-event-character key-event))
         (key-name (clim:keyboard-event-key-name key-event))
         (modifiers (clim:event-modifier-state key-event))
         (ctrl-p (plusp (logand modifiers clim:+control-key+)))
         (meta-p (plusp (logand modifiers clim:+meta-key+)))
         ;; Map CLIM key names to our abstract keywords
         (key (case key-name
                ((:up) :up)
                ((:down) :down)
                ((:left) :left)
                ((:right) :right)
                ((:prior :page-up) :page_up)
                ((:next :page-down) :page_down)
                ((:home) :home)
                ((:end) :end)
                ((:return :newline) #\Newline)
                ((:tab) #\Tab)
                ((:backspace :delete) :backspace)
                ((:escape) #\Esc)
                (otherwise
                 (cond
                   ;; Control + letter: produce the control character
                   ((and ctrl-p char (alpha-char-p char))
                    (code-char (- (char-code (char-upcase char)) 64)))
                   ;; Regular character
                   (char char)
                   ;; Named key with no character → keyword
                   (t key-name))))))
    (cond
      ;; Ctrl+Backspace
      ((and ctrl-p (eq key :backspace))
       (list :ctrl :backspace))
      ;; Alt+Backspace
      ((and meta-p (eq key :backspace))
       (list :alt :backspace))
      ;; Pending prefix resolution (must come before raw prefix detection)
      (*meta-pending*
       (setf *meta-pending* nil)
       (list :alt key))
      (*cx-pending*
       (setf *cx-pending* nil)
       (list :ctrl-x key))
      (*cc-pending*
       (setf *cc-pending* nil)
       (list :ctrl-c key))
      (*ch-pending*
       (setf *ch-pending* nil)
       (list :ctrl-h key))
      ;; Meta delivered directly by CLIM (Alt+key)
      (meta-p
       (list :alt key))
      ;; ESC prefix
      ((and (characterp key) (char= key #\Esc))
       (setf *meta-pending* t)
       nil)
      ;; C-x prefix (ASCII 24)
      ((and (characterp key) (char= key (code-char 24)))
       (setf *cx-pending* t)
       nil)
      ;; C-c prefix (ASCII 3)
      ((and (characterp key) (char= key #\Etx))
       (setf *cc-pending* t)
       nil)
      ;; C-h prefix (ASCII 8)
      ((and (characterp key) (char= key (code-char 8)))
       (setf *ch-pending* t)
       nil)
      ;; Normal key
      (t key))))

;;; --------------------------------------------------------------------------
;;; Redisplay Helper
;;; --------------------------------------------------------------------------

(defun redisplay-all (frame)
  "Force redisplay of all three panes."
  (dolist (pane-name '(main-pane modeline-pane minibuffer-pane))
    (let ((pane (clim:find-pane-named frame pane-name)))
      (when pane
        (clim:redisplay-frame-pane frame pane :force-p t)))))

;;; --------------------------------------------------------------------------
;;; Custom Top-Level Event Loop
;;; --------------------------------------------------------------------------

(defmethod clim:default-frame-top-level ((frame clawmacs-gui) &key)
  "Custom event loop for clawmacs — mirrors the croatoan event loop structure.
Blocking reads when idle, polling with sleep when streaming.
Called by the standard run-frame-top-level AFTER the frame is adopted/enabled
and all mediums are connected to the X11 backend."
  ;; Initialize char metrics now that mediums are fully realized
  (let ((main-pane (clim:find-pane-named frame 'main-pane)))
    (ensure-char-metrics frame main-pane))
  ;; Set initial scroll page size
  (let* ((main-pane (clim:find-pane-named frame 'main-pane))
         (char-w (frame-char-width frame))
         (char-h (frame-char-height frame)))
    (when (and (plusp char-w) (plusp char-h))
      (multiple-value-bind (cols rows) (pane-grid-dimensions main-pane char-w char-h)
        (declare (ignore cols))
        (setf *scroll-page-size* (max 1 (- rows 3))))))
  ;; Initial render
  (redisplay-all frame)
  ;; Main event loop
  (loop :until (frame-quit-flag frame)
        :for buf := (current-buffer)
        :for streaming := (buffer-pending-stream buf)
        :do (let ((event (if streaming
                             (clim:event-read-no-hang
                              (clim:frame-top-level-sheet frame))
                             (clim:event-read
                              (clim:frame-top-level-sheet frame)))))
              (cond
                ;; Timeout path: poll streaming, redisplay, sleep briefly
                ((null event)
                 (when streaming
                   (update-streaming-response buf)
                   (redisplay-all frame))
                 (sleep 0.1))
                ;; Key press event
                ((typep event 'clim:key-press-event)
                 (let* ((key (mcclim-normalize-key event)))
                   (when key
                     (let ((result (handle-key-event buf key)))
                       (when (eq result :quit)
                         (setf (frame-quit-flag frame) t)))))
                 ;; Poll streaming if active after key handling
                 (let ((cur (current-buffer)))
                   (when (buffer-pending-stream cur)
                     (update-streaming-response cur)))
                 ;; Update scroll page size (window may have resized)
                 (let* ((main-pane (clim:find-pane-named frame 'main-pane))
                        (char-w (frame-char-width frame))
                        (char-h (frame-char-height frame)))
                   (when (and (plusp char-w) (plusp char-h))
                     (multiple-value-bind (cols rows)
                         (pane-grid-dimensions main-pane char-w char-h)
                       (declare (ignore cols))
                       (setf *scroll-page-size* (max 1 (- rows 3))))))
                 (redisplay-all frame))
                ;; Other events (pointer, etc.) — handle normally
                (t
                 (clim:handle-event (clim:event-sheet event) event))))))

;;; --------------------------------------------------------------------------
;;; Backend Class and Entry Point
;;; --------------------------------------------------------------------------

(defclass mcclim-backend (ui-backend)
  ((frame :accessor backend-frame :initform nil))
  (:documentation "McCLIM graphical backend with monospace grid rendering.
Three-pane GUI layout (main, modeline, minibuffer) that visually mirrors
the terminal UI. Load via (asdf:load-system :clawmacs/mcclim)."))

(defmethod backend-run ((b mcclim-backend) initial-buffer)
  "Run the McCLIM graphical UI.
Creates a three-pane frame (main, modeline, minibuffer), then enters
the event loop reading input and rendering until :QUIT."
  (let ((frame (clim:make-application-frame 'clawmacs-gui
                 :backend b
                 :width 900
                 :height 700)))
    (setf (backend-frame b) frame)
    (clim:run-frame-top-level frame)))
