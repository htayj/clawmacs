(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Default Face Definitions
;;; --------------------------------------------------------------------------

(defun make-default-user-face-set ()
  "Create the default face set for user messages.
Background: CGA blue (#4), foreground: white.
Spec calls for dark-gray (#8) but 8-color terminals map that to black,
making user and agent messages indistinguishable. Blue provides clear
visual distinction until 256-color detection is added."
  (make-face-set
   :user
   (list (make-instance 'face
           :name :default
           :background (make-color-spec :cga 4)
           :foreground (make-color-spec :cga 7)
           :bold-p nil
           :underline-p nil
           :reverse-p nil))))

(defun make-default-agent-face-set (agent-keyword)
  "Create the default face set for an agent.
Background: black (#0), foreground: white."
  (make-face-set
   agent-keyword
   (list (make-instance 'face
           :name :default
           :background (make-color-spec :cga 0)
           :foreground (make-color-spec :cga 15)
           :bold-p nil
           :underline-p nil
           :reverse-p nil))))

(defun make-modeline-face ()
  "Create the modeline face. Background: CGA white (#7), foreground: black, bold."
  (make-instance 'face
    :name :modeline
    :background (make-color-spec :cga 7)
    :foreground (make-color-spec :cga 0)
    :bold-p t
    :underline-p nil
    :reverse-p nil))

;;; --------------------------------------------------------------------------
;;; Croatoan Color Helpers
;;; --------------------------------------------------------------------------

(defun color-spec-to-croatoan (cs)
  "Convert a color-spec to a croatoan color keyword or integer.
CGA 8-15 (bright variants) are mapped to their base 0-7 keyword colors
because not all terminals support 256-color mode. When 256-color detection
is added, these can use the integer values directly for terminals that
support it."
  (ecase (color-spec-type cs)
    (:cga
     (let ((val (color-spec-value cs)))
       (case val
         ((0 8)   :black)       ; 8 = dark gray / bright black
         ((1 9)   :red)         ; 9 = bright red
         ((2 10)  :green)       ; 10 = bright green
         ((3 11)  :yellow)      ; 11 = bright yellow
         ((4 12)  :blue)        ; 12 = bright blue
         ((5 13)  :magenta)     ; 13 = bright magenta
         ((6 14)  :cyan)        ; 14 = bright cyan
         ((7 15)  :white)       ; 15 = bright white
         (otherwise val))))
    (:256
     (color-spec-value cs))
    (:hex
     :white)))

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

;;; --------------------------------------------------------------------------
;;; Modeline Rendering
;;; --------------------------------------------------------------------------

(defun format-modeline (buf width)
  "Format the modeline string for BUF, fitting within WIDTH columns."
  (let* ((left (format nil " ~A | ~A | ~A"
                       (buffer-name buf)
                       (buffer-agent-name buf)
                       (namestring (buffer-working-directory buf))))
         (right (format nil "~A/~A | ~A "
                        (buffer-token-count buf)
                        (buffer-context-limit buf)
                        (buffer-status buf)))
         (padding (max 1 (- width (length left) (length right))))
         (pad-str (make-string padding :initial-element #\Space))
         (padded (concatenate 'string left pad-str right)))
    (if (>= (length padded) width)
        (subseq padded 0 width)
        (let ((extra (- width (length padded))))
          (concatenate 'string padded (make-string extra :initial-element #\Space))))))

(defun render-modeline (buf modeline-window)
  "Render the modeline for BUF into MODELINE-WINDOW."
  (let* ((width (croatoan:width modeline-window))
         (text (format-modeline buf width))
         (ml-face (make-modeline-face))
         (resolved (resolve-face ml-face)))
    (apply-face-to-window modeline-window resolved)
    (croatoan:clear modeline-window)
    (croatoan:move modeline-window 0 0)
    (croatoan:add-string modeline-window text)
    (croatoan:refresh modeline-window)))

;;; --------------------------------------------------------------------------
;;; Line Wrapping Helpers
;;; --------------------------------------------------------------------------

(defun wrapped-line-count (content display-width)
  "Return the number of visual rows needed to display CONTENT within DISPLAY-WIDTH.
An empty string takes 1 row. A string exactly DISPLAY-WIDTH chars takes 1 row."
  (if (or (zerop (length content)) (<= (length content) display-width))
      1
      (ceiling (length content) display-width)))

(defun message-visual-height (msg width)
  "Return the total visual rows MSG needs at the given terminal WIDTH.
Accounts for the sender prefix and line wrapping."
  (let* ((prefix-len (length (message-sender-prefix msg)))
         (display-width (max 1 (- width prefix-len))))
    (loop :for line := (message-first-line msg) :then (line-next line)
          :while line
          :sum (wrapped-line-count (line-content line) display-width))))

;;; --------------------------------------------------------------------------
;;; Message Rendering
;;; --------------------------------------------------------------------------

(defun message-sender-prefix (msg)
  "Return the display prefix for MSG's sender."
  (format nil "~A> " (string-downcase (symbol-name (message-sender msg)))))

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

(defun render-message-lines (window msg start-row width &key show-cursor)
  "Render MSG's lines into WINDOW starting at START-ROW with line wrapping.
Returns the number of visual rows consumed.
If SHOW-CURSOR is true, positions the cursor at MSG's point."
  (let* ((prefix (message-sender-prefix msg))
         (prefix-len (length prefix))
         (display-width (max 1 (- width prefix-len)))
         (face-set (message-face-set msg))
         (face (if face-set
                   (or (get-face face-set :default) (make-modeline-face))
                   (make-modeline-face)))
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
                       (render-wrapped-row window row 0
                                           (concatenate 'string prefix chunk) width)
                       (setf col nil)) ; already rendered
                     (when col
                       (render-wrapped-row window row col chunk width))
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
    (when (and show-cursor cursor-y cursor-x)
      (croatoan:move window cursor-y (min cursor-x (1- width))))
    (- row start-row)))

;;; --------------------------------------------------------------------------
;;; Buffer Rendering
;;; --------------------------------------------------------------------------

(defun calculate-input-height (buf terminal-height width)
  "Calculate the visual height of the input area in rows.
Minimum 3, maximum (floor terminal-height 3). Accounts for line wrapping."
  (let* ((input (buffer-input-message buf))
         (visual-height (message-visual-height input width))
         (min-height 3)
         (max-height (floor terminal-height 3)))
    (max min-height (min visual-height max-height))))

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
    ;; Optionally filter out tool-result messages
    (let ((history-messages nil)
          (hide-tool-results (not (buffer-show-tool-results-p buf))))
      (loop :for msg := (buffer-first-message buf) :then (message-next msg)
            :while (and msg (not (eq msg (buffer-input-message buf))))
            :do (unless (and hide-tool-results
                             (eq :tool-result (message-sender msg)))
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
    ;; Render input message
    (render-message-lines main-window (buffer-input-message buf)
                          input-start-row width
                          :show-cursor t)
    (croatoan:refresh main-window)
    (render-modeline buf modeline-window)))
