(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Croatoan Terminal Backend
;;; --------------------------------------------------------------------------

(defclass croatoan-backend (ui-backend)
  ((screen :accessor backend-screen :initform nil
           :documentation "The croatoan screen object.")
   (main-win :accessor backend-main-win :initform nil
             :documentation "Main window for chat history and input.")
   (modeline-win :accessor backend-modeline-win :initform nil
                 :documentation "Single-row modeline window.")
   (minibuf-win :accessor backend-minibuf-win :initform nil
                :documentation "Minibuffer window at bottom of screen."))
  (:documentation "Default terminal UI backend using croatoan (ncurses).
Three-window layout: main (chat), modeline (1 row), minibuffer (bottom)."))

;;; --------------------------------------------------------------------------
;;; Croatoan Color Helpers
;;; --------------------------------------------------------------------------

(defvar *terminal-color-count* 8
  "Number of colors the terminal supports. Detected at startup by
backend-run from ncurses COLORS. Used by color-spec-to-croatoan to decide
whether bright CGA 8-15 and 256-color values can be passed directly.")

(defun hex-to-256 (hex-string)
  "Approximate a #RRGGBB hex color as a 256-color index (16-231 color cube
or 232-255 grayscale). Returns an integer suitable for ncurses."
  (let* ((r (parse-integer hex-string :start 1 :end 3 :radix 16))
         (g (parse-integer hex-string :start 3 :end 5 :radix 16))
         (b (parse-integer hex-string :start 5 :end 7 :radix 16)))
    ;; Check if it's close to a grayscale value (232-255 = 24 shades)
    (if (and (<= (abs (- r g)) 8) (<= (abs (- g b)) 8))
        ;; Grayscale ramp: index 232 = rgb(8,8,8) .. 255 = rgb(238,238,238)
        ;; Each step is ~10. Map average luminance to nearest index.
        (let* ((avg (round (+ r g b) 3))
               (idx (min 23 (max 0 (round (- avg 8) 10)))))
          (+ 232 idx))
        ;; 6x6x6 color cube: indices 16-231
        ;; Each channel maps 0-255 to 0-5
        (let ((ri (round (* r 5) 255))
              (gi (round (* g 5) 255))
              (bi (round (* b 5) 255)))
          (+ 16 (* 36 ri) (* 6 gi) bi)))))

(defun color-spec-to-croatoan (cs)
  "Convert a color-spec to a croatoan color keyword or integer.
Croatoan's color-pair setter accepts:
  - Keywords (:black, :red, etc.) for the 8 standard colors.
  - Integers for extended 256-color indices (passed through color-to-number).
Behavior depends on *terminal-color-count*:
  8 colors:   CGA 8-15 collapse to base 0-7 keywords; 256/:hex → keyword fallback.
  256 colors: CGA 0-7 as keywords, CGA 8-15 as integers; :256 as integers;
              :hex approximated to 256."
  (ecase (color-spec-type cs)
    (:cga
     (let ((val (color-spec-value cs)))
       ;; CGA 0-7 always map to keywords (works on all terminals).
       ;; CGA 8-15 use integers on 256-color terminals, collapse on 8-color.
       (case val
         (0  :black)
         (1  :red)
         (2  :green)
         (3  :yellow)
         (4  :blue)
         (5  :magenta)
         (6  :cyan)
         (7  :white)
         ;; Bright variants (CGA 8-15): use (:number N) for Croatoan's
         ;; color-to-number dispatch on 256-color terminals.
         (8  (if (>= *terminal-color-count* 256) '(:number 8)  :black))
         (9  (if (>= *terminal-color-count* 256) '(:number 9)  :red))
         (10 (if (>= *terminal-color-count* 256) '(:number 10) :green))
         (11 (if (>= *terminal-color-count* 256) '(:number 11) :yellow))
         (12 (if (>= *terminal-color-count* 256) '(:number 12) :blue))
         (13 (if (>= *terminal-color-count* 256) '(:number 13) :magenta))
         (14 (if (>= *terminal-color-count* 256) '(:number 14) :cyan))
         (15 (if (>= *terminal-color-count* 256) '(:number 15) :white))
         (otherwise (if (>= *terminal-color-count* 256) (list :number val) :white)))))
    (:256
     (if (>= *terminal-color-count* 256)
         (list :number (color-spec-value cs))
         ;; Degrade to nearest base color keyword
         (let ((val (color-spec-value cs)))
           (cond ((<= val 15) (color-spec-to-croatoan (make-color-spec :cga val)))
                 (t :white)))))
    (:hex
     (if (>= *terminal-color-count* 256)
         (list :number (hex-to-256 (color-spec-value cs)))
         :white))))

(defun apply-face-to-window (window resolved-face)
  "Set WINDOW's color pair and attributes from RESOLVED-FACE."
  (let ((fg (color-spec-to-croatoan (resolved-face-foreground resolved-face)))
        (bg (color-spec-to-croatoan (resolved-face-background resolved-face))))
    (setf (croatoan:color-pair window) (list fg bg))
    (setf (croatoan:attributes window)
          (remove nil
                  (list (when (resolved-face-bold-p resolved-face) :bold)
                        (when (resolved-face-underline-p resolved-face) :underline)
                        (when (resolved-face-reverse-p resolved-face) :reverse))))))

(defun apply-global-face (window face-name)
  "Apply a face from the global face registry to WINDOW by keyword NAME.
Resolves the face and sets WINDOW's colors and attributes."
  (let ((face (global-face face-name)))
    (when face
      (apply-face-to-window window (resolve-face face)))))

;;; --------------------------------------------------------------------------
;;; Modeline Rendering
;;; --------------------------------------------------------------------------

(defun render-modeline (buf modeline-window)
  "Render the modeline for BUF into MODELINE-WINDOW."
  (let* ((width (croatoan:width modeline-window))
         (text (format-modeline buf width :major-mode (buffer-major-mode buf)))
         (ml-face (make-modeline-face))
         (resolved (resolve-face ml-face)))
    (apply-face-to-window modeline-window resolved)
    (croatoan:clear modeline-window)
    (croatoan:move modeline-window 0 0)
    (croatoan:add-string modeline-window text)
    (croatoan:refresh modeline-window)))

;;; --------------------------------------------------------------------------
;;; Message Rendering
;;; --------------------------------------------------------------------------

(defun render-wrapped-row (window row col text width)
  "Write TEXT at (ROW, COL) in WINDOW, not exceeding WIDTH total columns.
Fills the rest of the row with spaces for background color."
  (when (< row (croatoan:height window))
    (croatoan:move window row 0)
    (croatoan:add-string window (make-string width :initial-element #\Space))
    (croatoan:move window row col)
    (let ((max-chars (- width col)))
      (when (plusp max-chars)
        (croatoan:add-string window (subseq text 0 (min (length text) max-chars)))))))

(defun render-faced-row (window row col spans width base-face-name)
  "Write SPANS at (ROW, COL) using global faces derived from BASE-FACE-NAME."
  (when (< row (croatoan:height window))
    (let* ((base-face (or (global-face base-face-name)
                          (make-default-text-face)))
           (base-resolved (resolve-face base-face))
           (cursor-col col))
      (apply-face-to-window window base-resolved)
      (croatoan:move window row 0)
      (croatoan:add-string window (make-string width :initial-element #\Space))
      (dolist (span spans)
        (when (< cursor-col width)
          (let* ((face-name (car span))
                 (text (cdr span))
                 (max-chars (- width cursor-col)))
            (when (plusp max-chars)
              (let* ((visible (subseq text 0 (min (length text) max-chars)))
                     (face (or (global-face face-name) base-face))
                     (resolved (resolve-face face)))
                (apply-face-to-window window resolved)
                (croatoan:move window row cursor-col)
                (croatoan:add-string window visible)
                (incf cursor-col (length visible)))))))
      (apply-face-to-window window base-resolved))))

(defun render-message-lines (window msg start-row width &key show-cursor)
  "Render MSG's lines into WINDOW starting at START-ROW with line wrapping.
Returns the number of visual rows consumed.
If SHOW-CURSOR is true, positions the cursor at MSG's point."
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
    (apply-face-to-window window resolved)
    (loop :for line := (message-first-line msg) :then (line-next line)
          :for line-idx :from 0
          :while (and line (< row (croatoan:height window)))
          :do
             (let* ((content (line-content line))
                    (tool-face-name (tool-line-base-face-name msg content))
                    (content-len (length content))
                    (num-wraps (wrapped-line-count content display-width)))
               ;; Render each visual row of this line
               (dotimes (wrap-idx num-wraps)
                 (when (< row (croatoan:height window))
                   (let* ((chunk-start (* wrap-idx display-width))
                          (chunk-end (min (* (1+ wrap-idx) display-width) content-len))
                          (chunk (subseq content chunk-start chunk-end))
                          (col prefix-len))
                     ;; Show prefix only on the very first visual row of the message
                     (when (and (= line-idx 0) (= wrap-idx 0))
                       (if tool-face-name
                           (render-faced-row window row 0
                                             (tool-line-display-spans
                                              content tool-face-name
                                              :start chunk-start :end chunk-end
                                              :prefix prefix)
                                             width tool-face-name)
                           (render-wrapped-row window row 0
                                               (concatenate 'string prefix chunk) width))
                       (setf col nil)) ; already rendered
                     (when col
                       (if tool-face-name
                           (render-faced-row window row col
                                             (tool-line-display-spans
                                              content tool-face-name
                                              :start chunk-start :end chunk-end)
                                             width tool-face-name)
                           (render-wrapped-row window row col chunk width)))
                     ;; Track cursor position
                     (when (and show-cursor (eq line (message-point-line msg)))
                       (let ((point-off (message-point-offset msg)))
                         (when (and (>= point-off chunk-start)
                                    (or (< point-off chunk-end)
                                        ;; cursor at end of last chunk
                                        (and (= wrap-idx (1- num-wraps))
                                             (= point-off chunk-end))))
                           (setf cursor-y row
                                 cursor-x (+ prefix-len (- point-off chunk-start))))))
                     (incf row))))))
    ;; Render point face: reverse-video on the character at cursor
    (when (and show-cursor cursor-y cursor-x)
      (let ((cx (min cursor-x (1- width))))
        ;; Get the character at point (or space if at end of line)
        (let* ((point-line (message-point-line msg))
               (point-off (message-point-offset msg))
               (content (when point-line (line-content point-line)))
               (char-at-point (if (and content (< point-off (length content)))
                                  (char content point-off)
                                  #\Space)))
          ;; Draw the character with reverse-video face
          (let ((fg (color-spec-to-croatoan (resolved-face-foreground resolved)))
                (bg (color-spec-to-croatoan (resolved-face-background resolved))))
            ;; Swap fg/bg for reverse video
            (setf (croatoan:color-pair window) (list bg fg))
            (setf (croatoan:attributes window) '(:bold))
            (croatoan:move window cursor-y cx)
            (croatoan:add-string window (string char-at-point))
            ;; Restore normal face
            (apply-face-to-window window resolved)))
        ;; Position the terminal cursor at point
        (croatoan:move window cursor-y cx)))
    (- row start-row)))

;;; --------------------------------------------------------------------------
;;; Buffer Rendering
;;; --------------------------------------------------------------------------

(defun render-buffer (buf main-window modeline-window)
  "Render the entire buffer: history + input into MAIN-WINDOW, modeline.
Respects buffer-scroll-offset for history scrolling."
  (let* ((total-height (croatoan:height main-window))
         (width (croatoan:width main-window))
         (input-height (calculate-input-height buf total-height width))
         (history-height (- total-height input-height))
         (input-start-row history-height))
    (croatoan:clear main-window)
    ;; Collect history messages (all except the input message)
    ;; Optionally filter out tool-related messages
    (let ((history-messages nil)
          (hide-tool-results (not (buffer-show-tool-results-p buf))))
      (loop :for msg := (buffer-first-message buf) :then (message-next msg)
            :while (and msg (not (eq msg (buffer-input-message buf))))
            :do (unless (and hide-tool-results
                             (or ;; Hide tool-result messages
                                 (eq :tool-result (message-sender msg))
                                 ;; Hide assistant messages that are purely tool calls
                                 ;; (raw-content has tool_use blocks but no text)
                                 (and (message-raw-content msg)
                                      (not (eq :user (message-sender msg)))
                                      (every (lambda (block)
                                               (let ((btype (cdr (assoc :type block))))
                                                 (not (string= "text" (or btype "")))))
                                             (message-raw-content msg)))))
                  (push msg history-messages)))
      (setf history-messages (nreverse history-messages))
      ;; Calculate visual heights for all history messages
      (let* ((msg-heights (mapcar (lambda (m) (message-visual-height m width))
                                  history-messages))
             (total-history-rows (reduce #'+ msg-heights :initial-value 0))
             ;; Clamp scroll-offset: can't scroll past the beginning of history
             (max-scroll (max 0 (- total-history-rows history-height)))
             (scroll-offset (min (buffer-scroll-offset buf) max-scroll))
             ;; The bottom of the visible history (in virtual row space)
             (visible-bottom (- total-history-rows scroll-offset))
             (visible-top (- visible-bottom history-height)))
        ;; Write clamped value back to buffer
        (setf (buffer-scroll-offset buf) scroll-offset)
        ;; Walk through messages, render those whose virtual rows
        ;; overlap the visible window [visible-top, visible-bottom)
        (let ((virtual-row 0))
          (loop :for msg :in history-messages
                :for msg-h :in msg-heights
                :for msg-top := virtual-row
                :for msg-bottom := (+ virtual-row msg-h)
                :do (setf virtual-row msg-bottom)
                    ;; Skip messages entirely above or below visible area
                    (when (and (< msg-top visible-bottom)
                               (> msg-bottom visible-top))
                      ;; This message is at least partially visible
                      (let ((screen-row (- msg-top visible-top)))
                        (render-message-lines main-window msg screen-row width)))))))
    ;; Render input area: either approval prompt or normal input
    (if (buffer-approval-pending buf)
        (render-approval-prompt main-window buf input-start-row width)
        (render-message-lines main-window (buffer-input-message buf)
                              input-start-row width
                              :show-cursor t))
    (croatoan:refresh main-window)
    (render-modeline buf modeline-window)))

;;; --------------------------------------------------------------------------
;;; Approval Prompt Rendering
;;; --------------------------------------------------------------------------

(defun render-approval-prompt (window buf start-row width)
  "Render the permission approval prompt in the input area.
Uses global faces: :approval-header, :approval-code, :approval-text,
:approval-diff-add, :approval-diff-remove, :approval-options."
  (let* ((approval (buffer-approval-pending buf))
         (tool-name (cdr (assoc :tool-name approval)))
         (raw-sexpr (cdr (assoc :display-raw approval)))
         (expanded (cdr (assoc :display-expanded approval)))
         (row start-row))
    ;; Header
    (apply-global-face window :approval-header)
    (when (< row (croatoan:height window))
      (croatoan:move window row 0)
      (croatoan:add-string window (make-string width :initial-element #\-))
      (croatoan:move window row 0)
      (croatoan:add-string window
                           (format nil "-- PERMISSION REQUIRED: ~A " tool-name))
      (incf row))
    ;; Raw sexpr
    (apply-global-face window :approval-code)
    (dolist (line (split-string-by-newline raw-sexpr))
      (when (< row (croatoan:height window))
        (croatoan:move window row 0)
        (croatoan:add-string window (make-string width :initial-element #\Space))
        (croatoan:move window row 0)
        (croatoan:add-string window (subseq line 0 (min (length line) width)))
        (incf row)))
    ;; Expanded form
    (apply-global-face window :approval-text)
    (dolist (line (split-string-by-newline expanded))
      (when (< row (croatoan:height window))
        (croatoan:move window row 0)
        (croatoan:add-string window (make-string width :initial-element #\Space))
        (croatoan:move window row 0)
        (croatoan:add-string window (subseq line 0 (min (length line) width)))
        (incf row)))
    ;; Extra display (e.g., file diff, command preview)
    (let ((extra (cdr (assoc :display-extra approval))))
      (when extra
        (dolist (line (split-string-by-newline extra))
          (when (< row (croatoan:height window))
            (croatoan:move window row 0)
            (croatoan:add-string window (make-string width :initial-element #\Space))
            (croatoan:move window row 0)
            ;; Color-code diff lines using global faces
            (cond
              ((and (plusp (length line)) (char= (char line 0) #\+))
               (apply-global-face window :approval-diff-add))
              ((and (plusp (length line)) (char= (char line 0) #\-))
               (apply-global-face window :approval-diff-remove))
              (t
               (apply-global-face window :approval-text)))
            (croatoan:add-string window (subseq line 0 (min (length line) width)))
            (incf row)))))
    ;; Options
    (when (< row (croatoan:height window))
      (incf row) ; blank line
      (apply-global-face window :approval-options)
      (croatoan:move window row 0)
      (croatoan:add-string window (make-string width :initial-element #\Space))
      (croatoan:move window row 0)
      (croatoan:add-string window
                           "[a]pprove  [d]eny  [m]essage (deny with note to agent)")
      (incf row))))

;;; --------------------------------------------------------------------------
;;; Buffer Selector Rendering
;;; --------------------------------------------------------------------------

(defun render-buffer-selector (main-window modeline-window)
  "Render the buffer selector overlay showing all agent sessions.
Handles scrolling when there are more buffers than visible rows.
Uses global faces: :selector-title, :selector-separator, :selector-header,
:selector-entry, :selector-selected, :selector-scroll, :selector-footer."
  (let* ((width (croatoan:width main-window))
         (height (croatoan:height main-window))
         (buffers *buffer-ring*)
         (num-buffers (length buffers))
         (current (first *buffer-ring*))
         ;; Rows 0=blank, 1=title, 2=separator, 3=headers, 4=blank, 5..=entries
         ;; Last row = footer
         (max-entries (max 1 (- height 7)))
         ;; Auto-scroll to keep the selected index visible
         (scroll (cond
                   ((< *buffer-selector-index* *buffer-selector-scroll*)
                    *buffer-selector-index*)
                   ((>= *buffer-selector-index*
                        (+ *buffer-selector-scroll* max-entries))
                    (max 0 (1+ (- *buffer-selector-index* max-entries))))
                   (t *buffer-selector-scroll*))))
    (setf *buffer-selector-scroll* scroll)
    (croatoan:clear main-window)
    ;; Title
    (when (< 1 height)
      (apply-global-face main-window :selector-title)
      (croatoan:move main-window 1 2)
      (let ((title "Agent Sessions"))
        (croatoan:add-string main-window
                             (subseq title 0 (min (length title) (- width 4))))))
    ;; Separator
    (when (< 2 height)
      (apply-global-face main-window :selector-separator)
      (croatoan:move main-window 2 2)
      (let ((sep (make-string (min (- width 4) 50) :initial-element #\─)))
        (croatoan:add-string main-window sep)))
    ;; Column headers
    (when (< 3 height)
      (apply-global-face main-window :selector-header)
      (let ((header (format-selector-line "  " "NAME" "AGENT" "STATUS" "MSGS" width)))
        (croatoan:move main-window 3 0)
        (croatoan:add-string main-window
                             (subseq header 0 (min (length header) width)))))
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
          :do (progn
                (if selected-p
                    (apply-global-face main-window :selector-selected)
                    (apply-global-face main-window :selector-entry))
                ;; Clear row with background color
                (croatoan:move main-window row 0)
                (croatoan:add-string main-window
                                     (make-string width :initial-element #\Space))
                ;; Write entry
                (croatoan:move main-window row 0)
                (croatoan:add-string main-window
                                     (subseq line 0 (min (length line) width)))))
    ;; Scroll indicator (when list exceeds visible area)
    (when (> num-buffers max-entries)
      (let ((indicator (format nil "[~D-~D of ~D]"
                               (1+ scroll)
                               (min (+ scroll max-entries) num-buffers)
                               num-buffers))
            (ind-row (+ 5 (min max-entries (- num-buffers scroll)))))
        (when (< ind-row (- height 1))
          (apply-global-face main-window :selector-scroll)
          (croatoan:move main-window ind-row 2)
          (croatoan:add-string main-window
                               (subseq indicator 0
                                       (min (length indicator) (- width 4)))))))
    ;; Footer with keybinding hints
    (let ((footer-row (1- height)))
      (when (plusp footer-row)
        (apply-global-face main-window :selector-footer)
        (croatoan:move main-window footer-row 2)
        (let ((footer "[RET] select  [C-g/q] cancel  [n] new  [k] kill"))
          (croatoan:add-string main-window
                               (subseq footer 0
                                       (min (length footer) (- width 4)))))))
    (croatoan:refresh main-window)
    ;; Custom modeline for selector
    (let* ((ml-face (make-modeline-face))
           (resolved (resolve-face ml-face))
           (ml-text (format nil " [buffer-selector] Agent Sessions | ~D session~:[s~;~]"
                            num-buffers (= num-buffers 1)))
           (ml-width (croatoan:width modeline-window))
           (padded (if (<= (length ml-text) ml-width)
                       (concatenate 'string ml-text
                                    (make-string (- ml-width (length ml-text))
                                                 :initial-element #\Space))
                       (subseq ml-text 0 ml-width))))
      (apply-face-to-window modeline-window resolved)
      (croatoan:clear modeline-window)
      (croatoan:move modeline-window 0 0)
      (croatoan:add-string modeline-window padded)
      (croatoan:refresh modeline-window))))

;;; --------------------------------------------------------------------------
;;; Model Selector Rendering
;;; --------------------------------------------------------------------------

(defun render-model-selector (main-window modeline-window)
  "Render the model selector overlay showing available models across providers.
Handles scrolling when there are more models than visible rows."
  (let* ((width (croatoan:width main-window))
         (height (croatoan:height main-window))
         (entries *model-selector-entries*)
         (num-entries (length entries))
         (current-buf (current-buffer))
         ;; Rows: 0=blank, 1=title, 2=separator, 3=headers, 4=blank, 5..=entries
         ;; Last row = footer
         (max-visible (max 1 (- height 7)))
         ;; Auto-scroll to keep selected index visible
         (scroll (cond
                   ((< *model-selector-index* *model-selector-scroll*)
                    *model-selector-index*)
                   ((>= *model-selector-index*
                        (+ *model-selector-scroll* max-visible))
                    (max 0 (1+ (- *model-selector-index* max-visible))))
                   (t *model-selector-scroll*))))
    (setf *model-selector-scroll* scroll)
    (croatoan:clear main-window)
    ;; Title
    (when (< 1 height)
      (apply-global-face main-window :selector-title)
      (croatoan:move main-window 1 2)
      (let ((title "Select Model"))
        (croatoan:add-string main-window
                             (subseq title 0 (min (length title) (- width 4))))))
    ;; Separator
    (when (< 2 height)
      (apply-global-face main-window :selector-separator)
      (croatoan:move main-window 2 2)
      (let ((sep (make-string (min (- width 4) 50) :initial-element #\─)))
        (croatoan:add-string main-window sep)))
    ;; Column headers
    (when (< 3 height)
      (apply-global-face main-window :selector-header)
      (let ((header (format-model-selector-line "  " "PROVIDER" "MODEL" width)))
        (croatoan:move main-window 3 0)
        (croatoan:add-string main-window
                             (subseq header 0 (min (length header) width)))))
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
          :do (progn
                (if selected-p
                    (apply-global-face main-window :selector-selected)
                    (apply-global-face main-window :selector-entry))
                ;; Clear row with background color
                (croatoan:move main-window row 0)
                (croatoan:add-string main-window
                                     (make-string width :initial-element #\Space))
                ;; Write entry
                (croatoan:move main-window row 0)
                (croatoan:add-string main-window
                                     (subseq line 0 (min (length line) width)))))
    ;; Scroll indicator
    (when (> num-entries max-visible)
      (let ((indicator (format nil "[~D-~D of ~D]"
                               (1+ scroll)
                               (min (+ scroll max-visible) num-entries)
                               num-entries))
            (ind-row (+ 5 (min max-visible (- num-entries scroll)))))
        (when (< ind-row (- height 1))
          (apply-global-face main-window :selector-scroll)
          (croatoan:move main-window ind-row 2)
          (croatoan:add-string main-window
                               (subseq indicator 0
                                       (min (length indicator) (- width 4)))))))
    ;; Footer with keybinding hints
    (let ((footer-row (1- height)))
      (when (plusp footer-row)
        (apply-global-face main-window :selector-footer)
        (croatoan:move main-window footer-row 2)
        (let ((footer "[RET] select  [C-g/q] cancel  * = active"))
          (croatoan:add-string main-window
                               (subseq footer 0
                                       (min (length footer) (- width 4)))))))
    (croatoan:refresh main-window)
    ;; Custom modeline for selector
    (let* ((ml-face (make-modeline-face))
           (resolved (resolve-face ml-face))
           (pm (resolve-modeline-provider-model current-buf))
           (ml-text (format nil " [model-selector] ~A | ~D model~:[s~;~] available"
                            pm num-entries (= num-entries 1)))
           (ml-width (croatoan:width modeline-window))
           (padded (if (<= (length ml-text) ml-width)
                       (concatenate 'string ml-text
                                    (make-string (- ml-width (length ml-text))
                                                 :initial-element #\Space))
                       (subseq ml-text 0 ml-width))))
      (apply-face-to-window modeline-window resolved)
      (croatoan:clear modeline-window)
      (croatoan:move modeline-window 0 0)
      (croatoan:add-string modeline-window padded)
      (croatoan:refresh modeline-window))))

;;; --------------------------------------------------------------------------
;;; Think Selector Rendering
;;; --------------------------------------------------------------------------

(defun render-think-selector (main-window modeline-window)
  "Render the think-level selector overlay for the active model."
  (let* ((width (croatoan:width main-window))
         (height (croatoan:height main-window))
         (entries *think-selector-entries*)
         (num-entries (length entries))
         (current-buf (current-buffer))
         (max-visible (max 1 (- height 7)))
         (scroll (cond
                   ((< *think-selector-index* *think-selector-scroll*)
                    *think-selector-index*)
                   ((>= *think-selector-index*
                        (+ *think-selector-scroll* max-visible))
                    (max 0 (1+ (- *think-selector-index* max-visible))))
                   (t *think-selector-scroll*))))
    (setf *think-selector-scroll* scroll)
    (croatoan:clear main-window)
    (when (< 1 height)
      (apply-global-face main-window :selector-title)
      (croatoan:move main-window 1 2)
      (let ((title "Select Think Level"))
        (croatoan:add-string main-window
                             (subseq title 0 (min (length title) (- width 4))))))
    (when (< 2 height)
      (apply-global-face main-window :selector-separator)
      (croatoan:move main-window 2 2)
      (let ((sep (make-string (min (- width 4) 50) :initial-element #\─)))
        (croatoan:add-string main-window sep)))
    (when (< 3 height)
      (apply-global-face main-window :selector-header)
      (let ((header (format-think-selector-line "  " "THINK LEVEL" width)))
        (croatoan:move main-window 3 0)
        (croatoan:add-string main-window
                             (subseq header 0 (min (length header) width)))))
    (loop :for absolute-idx :from scroll
          :below (min (+ scroll max-visible) num-entries)
          :for entry := (nth absolute-idx entries)
          :for row := (+ 5 (- absolute-idx scroll))
          :while (< row (- height 2))
          :for selected-p := (= absolute-idx *think-selector-index*)
          :for active-p := (getf entry :active-p)
          :for label := (getf entry :display)
          :for marker := (cond ((and selected-p active-p) "▸*")
                               (selected-p "▸ ")
                               (active-p " *")
                               (t "  "))
          :for line := (format-think-selector-line marker label width)
          :do (progn
                (if selected-p
                    (apply-global-face main-window :selector-selected)
                    (apply-global-face main-window :selector-entry))
                (croatoan:move main-window row 0)
                (croatoan:add-string main-window
                                     (make-string width :initial-element #\Space))
                (croatoan:move main-window row 0)
                (croatoan:add-string main-window
                                     (subseq line 0 (min (length line) width)))))
    (when (> num-entries max-visible)
      (let ((indicator (format nil "[~D-~D of ~D]"
                               (1+ scroll)
                               (min (+ scroll max-visible) num-entries)
                               num-entries))
            (ind-row (+ 5 (min max-visible (- num-entries scroll)))))
        (when (< ind-row (- height 1))
          (apply-global-face main-window :selector-scroll)
          (croatoan:move main-window ind-row 2)
          (croatoan:add-string main-window
                               (subseq indicator 0
                                       (min (length indicator) (- width 4)))))))
    (let ((footer-row (1- height)))
      (when (plusp footer-row)
        (apply-global-face main-window :selector-footer)
        (croatoan:move main-window footer-row 2)
        (let ((footer "[RET] select  [C-g/q] cancel  default = clear  * = active"))
          (croatoan:add-string main-window
                               (subseq footer 0
                                       (min (length footer) (- width 4)))))))
    (croatoan:refresh main-window)
    (let* ((ml-face (make-modeline-face))
           (resolved (resolve-face ml-face))
           (pm (resolve-modeline-provider-model current-buf))
           (ml-text (format nil " [think-selector] ~A | ~D level~:[s~;~] available"
                            pm num-entries (= num-entries 1)))
           (ml-width (croatoan:width modeline-window))
           (padded (if (<= (length ml-text) ml-width)
                       (concatenate 'string ml-text
                                    (make-string (- ml-width (length ml-text))
                                                 :initial-element #\Space))
                       (subseq ml-text 0 ml-width))))
      (apply-face-to-window modeline-window resolved)
      (croatoan:clear modeline-window)
      (croatoan:move modeline-window 0 0)
      (croatoan:add-string modeline-window padded)
      (croatoan:refresh modeline-window))))

;;; --------------------------------------------------------------------------
;;; Minibuffer Rendering
;;; --------------------------------------------------------------------------

(defun render-minibuffer (minibuffer-window)
  "Render the minibuffer. When inactive, shows a blank line.
When active, shows the prompt with input and filtered completion candidates."
  (let ((width (croatoan:width minibuffer-window))
        (height (croatoan:height minibuffer-window)))
    (croatoan:clear minibuffer-window)
    (if *minibuffer-active*
        (render-minibuffer-active minibuffer-window width height)
        (render-minibuffer-inactive minibuffer-window width))
    (croatoan:refresh minibuffer-window)))

(defun render-minibuffer-inactive (window width)
  "Render the inactive minibuffer: a single blank line.
Uses global face :minibuffer-prompt."
  (apply-global-face window :minibuffer-prompt)
  (croatoan:move window 0 0)
  (croatoan:add-string window (make-string width :initial-element #\Space)))

(defun render-minibuffer-candidate-row (window row display match-positions selected-p width)
  "Render a single minibuffer candidate at ROW in WINDOW.
DISPLAY is the candidate text.  MATCH-POSITIONS is a sorted list of character
indices (into DISPLAY) that were matched by the current query — these are
drawn with a highlight face so the user can see exactly what matched.
SELECTED-P is T for the currently selected candidate.  WIDTH is the column
count of the window.

Face mapping:
  base chars in unselected item  -> :minibuffer-candidate
  matched chars in unselected    -> :minibuffer-match        (bright yellow)
  base chars in selected item    -> :minibuffer-selected
  matched chars in selected      -> :minibuffer-selected-match"
  (let* ((base-face  (if selected-p :minibuffer-selected      :minibuffer-candidate))
         (match-face (if selected-p :minibuffer-selected-match :minibuffer-match))
         ;; Build a hash set of matched positions for O(1) lookup.
         (match-set  (when match-positions
                       (let ((ht (make-hash-table :test #'eql)))
                         (dolist (p match-positions ht)
                           (setf (gethash p ht) t))))))
    ;; Clear row background.
    (apply-global-face window base-face)
    (croatoan:move window row 0)
    (croatoan:add-string window (make-string width :initial-element #\Space))
    ;; Two-space indent.
    (apply-global-face window base-face)
    (croatoan:move window row 0)
    (croatoan:add-string window "  ")
    ;; Write each character, switching to the match face for matched positions.
    (loop :for i :from 0 :below (length display)
          :for col :from 2
          :while (< col width)
          :for ch := (char display i)
          :for matched-p := (and match-set (gethash i match-set))
          :do (apply-global-face window (if matched-p match-face base-face))
              (croatoan:move window row col)
              (croatoan:add-string window (string ch)))))

(defun render-minibuffer-active (window width height)
  "Render the active minibuffer with prompt, input, cursor, and filtered items.
The first row shows the prompt and user input with a block cursor.
Subsequent rows show the filtered candidates, with the selected one in
inverse video. Matched characters within each candidate are highlighted.
Uses global faces: :minibuffer-prompt, :minibuffer-cursor,
:minibuffer-candidate, :minibuffer-selected,
:minibuffer-match, :minibuffer-selected-match."
  (let* ((prompt-str (format nil "~A: " *minibuffer-prompt*))
         (input *minibuffer-input*)
         (prompt-line (concatenate 'string prompt-str input))
         (cursor-col (+ (length prompt-str) *minibuffer-point*)))
    ;; -- Prompt line --
    (apply-global-face window :minibuffer-prompt)
    (croatoan:move window 0 0)
    (croatoan:add-string window (make-string width :initial-element #\Space))
    (croatoan:move window 0 0)
    (croatoan:add-string window
                         (subseq prompt-line 0 (min (length prompt-line) width)))
    ;; -- Block cursor --
    (when (< cursor-col width)
      (let ((char-at-cursor (if (< *minibuffer-point* (length input))
                                (char input *minibuffer-point*)
                                #\Space)))
        ;; Cursor face
        (apply-global-face window :minibuffer-cursor)
        (croatoan:move window 0 cursor-col)
        (croatoan:add-string window (string char-at-cursor))
        ;; Restore prompt face
        (apply-global-face window :minibuffer-prompt)))
    ;; -- Candidate list (with scroll offset and fuzzy-match highlighting) --
    (let* ((items     *minibuffer-filtered-items*)
           (positions *minibuffer-match-positions*)
           (selected  *minibuffer-selected-index*)
           (scroll    *minibuffer-scroll-offset*)
           (visible-rows (1- height))
           (total     (length items)))
      (loop :for row-idx :from 0 :below visible-rows
            :for item-idx := (+ scroll row-idx)
            :while (< item-idx total)
            :for item := (nth item-idx items)
            :for row := (1+ row-idx)
            :for display := (minibuffer-item-display item)
            :for match-pos := (nth item-idx positions)
            :for selected-p := (= item-idx selected)
            :do (render-minibuffer-candidate-row
                 window row display match-pos selected-p width)))))

;;; --------------------------------------------------------------------------
;;; Key Normalization (croatoan-specific)
;;; --------------------------------------------------------------------------

(defun normalize-key (event)
  "Extract and normalize a key from a croatoan EVENT.
Returns a character, a keyword (for special keys), a list (:alt <key>)
for Meta combinations, a list (:ctrl-x <key>) for C-x prefix (global
commands), or a list (:ctrl-c <key>) for C-c prefix (mode-specific commands)."
  (let* ((raw-key (if (typep event 'croatoan:event)
                      (croatoan:event-key event)
                      event))
         (ctrl-p (and (typep raw-key 'croatoan:key)
                      (croatoan:key-ctrl raw-key)))
         (alt-p (and (typep raw-key 'croatoan:key)
                     (croatoan:key-alt raw-key)))
         (key (if (typep raw-key 'croatoan:key)
                   (croatoan:key-name raw-key)
                   raw-key)))
    (cond
      ;; -- Modifier flags from Croatoan key structs --
      ;; Croatoan reports Ctrl/Alt on special keys (backspace, arrows, etc.)
      ;; via key-ctrl / key-alt flags. Handle all combinations here.
      ((and ctrl-p alt-p)
       (list :ctrl :alt key))
      ((and ctrl-p (eql key :backspace))
       (list :ctrl :backspace))
      (ctrl-p
       (list :ctrl key))
      ((and alt-p (eql key :backspace))
       (list :alt :backspace))
      (alt-p
       (list :alt key))
      ;; -- Pending prefix resolution (must come BEFORE raw prefix detection) --
      ;; When a prefix key was already pressed, the NEXT keystroke completes the
      ;; chord.  We must check these first, otherwise a raw C-c/C-x/ESC that
      ;; arrives as the second key would start a *new* prefix instead of
      ;; completing the chord (e.g. C-x C-c would never produce (:ctrl-x #\Etx)).
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
      ;; -- Raw prefix detection (first key of a chord) --
      ;; ESC received: set meta-pending, return nil (consume the ESC)
      ((and (characterp key) (char= key #\Esc))
       (setf *meta-pending* t)
       nil)
      ;; C-x received (ASCII 24): set cx-pending, return nil
      ((and (characterp key) (char= key (code-char 24)))
       (setf *cx-pending* t)
       nil)
      ;; C-c received (ASCII 3 = ETX): set cc-pending, return nil.
      ;; C-c is the prefix for buffer-mode-specific commands.
      ((and (characterp key) (char= key #\Etx))
       (setf *cc-pending* t)
       nil)
      ;; C-h received (ASCII 8 = Backspace): set ch-pending, return nil.
      ;; C-h is the help prefix (e.g. C-h b = describe bindings).
      ((and (characterp key) (char= key (code-char 8)))
       (setf *ch-pending* t)
       nil)
      ;; Normal key
      (t key))))

;;; --------------------------------------------------------------------------
;;; Window Layout (croatoan-specific)
;;; --------------------------------------------------------------------------

(defun update-window-layout (scr main-win modeline-win minibuffer-win)
  "Resize and reposition all windows based on screen dimensions and minibuffer state.
Layout from top to bottom: main-win, modeline-win (1 row), minibuffer-win."
  (let* ((h (croatoan:height scr))
         (w (croatoan:width scr))
         (mb-h (minibuffer-current-height))
         (main-h (max 1 (- h 1 mb-h))))
    (croatoan:resize main-win main-h w)
    (croatoan:resize modeline-win 1 w)
    (croatoan:move-window modeline-win main-h 0)
    (croatoan:resize minibuffer-win mb-h w)
    (croatoan:move-window minibuffer-win (1+ main-h) 0)
    ;; Update scroll page size based on available history area
    (setf *scroll-page-size* (max 1 (- main-h 3)))))

;;; --------------------------------------------------------------------------
;;; Backend Implementation
;;; --------------------------------------------------------------------------

(defun ensure-croatoan-locale ()
  "Initialize the libc locale from the process environment for ncurses.
Croatoan's Unicode path expects setlocale to be called explicitly; SBCL no
longer does this for us."
  (ncurses:setlocale ncurses:+lc-all+ ""))

(defmethod backend-run ((b croatoan-backend) initial-buffer)
  "Run the croatoan terminal UI.
Creates a three-window layout (main, modeline, minibuffer), then enters
the event loop reading input and rendering until :QUIT is returned."
  (declare (ignore initial-buffer))
  (ensure-croatoan-locale)
  ;; :process-control-chars nil puts the terminal into raw mode (ncurses:raw)
  ;; instead of cbreak mode, so C-c is delivered as a keystroke (ASCII 3)
  ;; rather than generating SIGINT.  This is required for C-c to work as
  ;; a prefix key (e.g. C-c t).  Quit is C-x C-c.
  (croatoan:with-screen (scr :input-echoing nil
                             :input-blocking t
                             :cursor-visible t
                             :enable-colors t
                             :process-control-chars nil)
    (setf (backend-screen b) scr)
    ;; Mouse support: scroll wheel (buttons 4 & 5) for history navigation.
    ;; NOTE: Disabled — the Quicklisp 2024-10 version of Croatoan crashes in
    ;; make-key when decoding mouse events whose bitmask returns nil from
    ;; get-mouse-event. The event loop handler below is ready for when
    ;; Croatoan's mouse decoding is fixed.
    ;; (ignore-errors (croatoan:set-mouse-event '(:button-4-press :button-5-press)))
    ;; Detect terminal color support via the ncurses COLORS C global.
    ;; Croatoan's defcvar exports COLORS (not *COLORS*) from the ncurses package.
    (setf *terminal-color-count*
          (max 8 (or (ignore-errors
                       (cffi:foreign-symbol-pointer "COLORS")
                       (cffi:mem-ref (cffi:foreign-symbol-pointer "COLORS") :int))
                     8)))
    (let* ((screen-height (croatoan:height scr))
           (screen-width (croatoan:width scr))
           ;; Three-window layout: main (chat), modeline (1 row), minibuffer (bottom)
           (main-win (make-instance 'croatoan:window
                       :height (- screen-height 2)
                       :width screen-width
                       :position '(0 0)))
           (modeline-win (make-instance 'croatoan:window
                           :height 1
                           :width screen-width
                           :position (list (- screen-height 2) 0)))
           (minibuffer-win (make-instance 'croatoan:window
                             :height 1
                             :width screen-width
                             :position (list (1- screen-height) 0))))
      (setf (backend-main-win b) main-win
            (backend-modeline-win b) modeline-win
            (backend-minibuf-win b) minibuffer-win)
      ;; Set scroll page size based on available history area
      (setf *scroll-page-size* (max 1 (- (- screen-height 2) 3)))
      ;; Flush stdscr's pending clear before our first render
      (croatoan:refresh scr)
      ;; Local render helper: updates window layout (for dynamic minibuffer height)
      ;; then renders all three windows. Centralizes the render dispatch.
      (labels ((do-render (buf &key force-p)
                 (when force-p
                   (croatoan:clear scr)
                   (croatoan:refresh scr))
                 (update-window-layout scr main-win modeline-win minibuffer-win)
                 (cond
                   (*buffer-selector-active*
                    (render-buffer-selector main-win modeline-win))
                   (*model-selector-active*
                    (render-model-selector main-win modeline-win))
                   (*think-selector-active*
                    (render-think-selector main-win modeline-win))
                   (t
                    (render-buffer buf main-win modeline-win)))
                 (render-minibuffer minibuffer-win)))
        ;; Initial render
        (do-render (current-buffer))
        ;; Event loop: current-buffer may change between iterations.
        ;; Short timeout when streaming or OAuth login is active for polling updates.
        (loop :named main-loop
            :for buf := (current-buffer)
            :for streaming := (buffer-pending-stream buf)
            :for oauth-pending := *openai-oauth-pending*
            :do (progn
                  (setf (croatoan:input-blocking scr)
                        (if (or streaming oauth-pending) 100 t))
                  (let ((event (croatoan:get-wide-event scr)))
                    (cond
                        ;; No event (timeout) -- poll streaming/login and re-render
                        ((null event)
                         (when streaming
                           (update-streaming-response buf))
                         (when oauth-pending
                           (update-openai-oauth-login))
                         (when (or streaming oauth-pending)
                           (do-render (current-buffer))))
                        ;; Window resize event -- re-layout and re-render
                        ((and (typep event 'croatoan:event)
                              (typep (croatoan:event-key event) 'croatoan:key)
                              (eq :resize (croatoan:key-name
                                           (croatoan:event-key event))))
                         (do-render (current-buffer)))
                        ;; Mouse event -- scroll wheel for history navigation.
                        ;; Croatoan mouse-events have event-key as a key struct
                        ;; with :name being a keyword like :button-4-press.
                        ((typep event 'croatoan:mouse-event)
                         (let* ((raw-key (croatoan:event-key event))
                                (btn (when (typep raw-key 'croatoan:key)
                                       (croatoan:key-name raw-key))))
                           (cond
                             ;; Scroll wheel up (button 4)
                             ((member btn '(:button-4-press :button-4-click))
                              (handle-key-event buf :page-up))
                             ;; Scroll wheel down (button 5)
                             ((member btn '(:button-5-press :button-5-click))
                              (handle-key-event buf :page-down))))
                         (do-render (current-buffer)))
                        ;; Key event -- normalize, dispatch, then re-render
                        (t
                         (let ((result (handle-key-event buf (normalize-key event))))
                           (when (eq result :quit)
                             (return-from main-loop))
                           (let ((cur (current-buffer)))
                             (when (buffer-pending-stream cur)
                               (update-streaming-response cur))
                             (when *openai-oauth-pending*
                               (update-openai-oauth-login))
                             (do-render cur :force-p (eq result :redraw)))))))))))))
