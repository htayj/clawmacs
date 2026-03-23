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

(defun resolve-modeline-provider-model (buf)
  "Resolve the provider/model string for the modeline.
Returns a string like \"anthropic/claude-haiku-4-5-20251001\" or \"??\" on error."
  (handler-case
      (multiple-value-bind (provider model)
          (resolve-buffer-provider-and-model buf)
        (format nil "~(~A~)/~A" provider model))
    (error () "??")))

(defun format-modeline (buf width &key (major-mode "chat") provider-model)
  "Format the modeline string for BUF, fitting within WIDTH columns.
MAJOR-MODE is displayed on the far left (e.g. \"chat\", \"buffer-selector\").
PROVIDER-MODEL is the provider/model string (e.g. \"anthropic/claude-haiku-4-5\");
when nil it is resolved from the buffer's agent defaults."
  (let* ((pm (or provider-model (resolve-modeline-provider-model buf)))
         (left (format nil " [~A] ~A | ~A | ~A | ~A"
                       major-mode
                       (buffer-name buf)
                       (buffer-agent-name buf)
                       pm
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

(defun calculate-input-height (buf terminal-height width)
  "Calculate the visual height of the input area in rows.
Minimum 3, maximum (floor terminal-height 3). Accounts for line wrapping.
During approval prompts, allows up to 2/3 of terminal height."
  (let* ((input (buffer-input-message buf))
         (visual-height (message-visual-height input width))
         (min-height 3)
         (approval-active (buffer-approval-pending buf))
         (max-height (if approval-active
                         (floor (* terminal-height 2) 3)
                         (floor terminal-height 3))))
    (if approval-active
        ;; During approval, give enough room for the full prompt
        (max min-height (min (max 12 visual-height) max-height))
        (max min-height (min visual-height max-height)))))

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
  "Render the permission approval prompt in the input area."
  (let* ((approval (buffer-approval-pending buf))
         (tool-name (cdr (assoc :tool-name approval)))
         (raw-sexpr (cdr (assoc :display-raw approval)))
         (expanded (cdr (assoc :display-expanded approval)))
         (row start-row)
         ;; Use a distinct face: yellow on black for warnings
         (warn-fg '(:yellow))
         (prompt-fg '(:white)))
    (declare (ignore warn-fg prompt-fg))
    ;; Set colors: yellow text for the prompt
    (setf (croatoan:color-pair window) '(:yellow :black))
    (setf (croatoan:attributes window) '(:bold))
    ;; Header
    (when (< row (croatoan:height window))
      (croatoan:move window row 0)
      (croatoan:add-string window (make-string width :initial-element #\-))
      (croatoan:move window row 0)
      (croatoan:add-string window
                           (format nil "-- PERMISSION REQUIRED: ~A " tool-name))
      (incf row))
    ;; Raw sexpr
    (setf (croatoan:attributes window) nil)
    (setf (croatoan:color-pair window) '(:cyan :black))
    (dolist (line (split-string-by-newline raw-sexpr))
      (when (< row (croatoan:height window))
        (croatoan:move window row 0)
        (croatoan:add-string window (make-string width :initial-element #\Space))
        (croatoan:move window row 0)
        (croatoan:add-string window (subseq line 0 (min (length line) width)))
        (incf row)))
    ;; Expanded form
    (setf (croatoan:color-pair window) '(:white :black))
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
            ;; Color-code diff lines: green for +, red for -, white for context
            (cond
              ((and (plusp (length line)) (char= (char line 0) #\+))
               (setf (croatoan:color-pair window) '(:green :black)))
              ((and (plusp (length line)) (char= (char line 0) #\-))
               (setf (croatoan:color-pair window) '(:red :black)))
              (t
               (setf (croatoan:color-pair window) '(:white :black))))
            (croatoan:add-string window (subseq line 0 (min (length line) width)))
            (incf row)))))
    ;; Options
    (when (< row (croatoan:height window))
      (incf row) ; blank line
      (setf (croatoan:color-pair window) '(:green :black))
      (setf (croatoan:attributes window) '(:bold))
      (croatoan:move window row 0)
      (croatoan:add-string window (make-string width :initial-element #\Space))
      (croatoan:move window row 0)
      (croatoan:add-string window
                           "[a]pprove  [d]eny  [m]essage (deny with note to agent)")
      (incf row))))

(defun split-string-by-newline (str)
  "Split STR by newlines into a list of strings."
  (loop :for start := 0 :then (1+ pos)
        :for pos := (position #\Newline str :start start)
        :collect (subseq str start (or pos (length str)))
        :while pos))

;;; --------------------------------------------------------------------------
;;; Buffer Selector Rendering
;;; --------------------------------------------------------------------------

(defun format-selector-line (marker name agent status count-str width)
  "Format a single line for the buffer selector with aligned columns.
Adapts column widths to the terminal WIDTH."
  (let* ((name-width (max 8 (min 30 (floor width 4))))
         (agent-width (max 6 (min 15 (floor width 6))))
         (status-width (max 6 (min 12 (floor width 8))))
         (line (format nil "~A~VA  ~VA  ~VA  ~A"
                       marker
                       name-width name
                       agent-width agent
                       status-width status
                       count-str)))
    (if (<= (length line) width)
        (concatenate 'string line
                     (make-string (- width (length line)) :initial-element #\Space))
        (subseq line 0 width))))

(defun render-buffer-selector (main-window modeline-window)
  "Render the buffer selector overlay showing all agent sessions.
Handles scrolling when there are more buffers than visible rows."
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
      (setf (croatoan:color-pair main-window) '(:cyan :black))
      (setf (croatoan:attributes main-window) '(:bold))
      (croatoan:move main-window 1 2)
      (let ((title "Agent Sessions"))
        (croatoan:add-string main-window
                             (subseq title 0 (min (length title) (- width 4))))))
    ;; Separator
    (when (< 2 height)
      (setf (croatoan:color-pair main-window) '(:white :black))
      (setf (croatoan:attributes main-window) nil)
      (croatoan:move main-window 2 2)
      (let ((sep (make-string (min (- width 4) 50) :initial-element #\─)))
        (croatoan:add-string main-window sep)))
    ;; Column headers
    (when (< 3 height)
      (setf (croatoan:color-pair main-window) '(:yellow :black))
      (setf (croatoan:attributes main-window) '(:bold))
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
                    (progn
                      (setf (croatoan:color-pair main-window) '(:black :cyan))
                      (setf (croatoan:attributes main-window) '(:bold)))
                    (progn
                      (setf (croatoan:color-pair main-window) '(:white :black))
                      (setf (croatoan:attributes main-window) nil)))
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
          (setf (croatoan:color-pair main-window) '(:yellow :black))
          (setf (croatoan:attributes main-window) nil)
          (croatoan:move main-window ind-row 2)
          (croatoan:add-string main-window
                               (subseq indicator 0
                                       (min (length indicator) (- width 4)))))))
    ;; Footer with keybinding hints
    (let ((footer-row (1- height)))
      (when (plusp footer-row)
        (setf (croatoan:color-pair main-window) '(:green :black))
        (setf (croatoan:attributes main-window) nil)
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
