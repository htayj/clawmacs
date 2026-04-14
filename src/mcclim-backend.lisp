(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; McCLIM Graphical Backend — Genera-Style Interface
;;;
;;; A graphical UI backend using McCLIM with a Symbolics Genera-inspired
;;; design: white background, italic buffer title bars, a who-line for
;;; context-dependent action hints, modeline at the absolute bottom, and
;;; popup completion overlay instead of an inline minibuffer.
;;; Three-pane layout: main, who-line, modeline.
;;; Optional dependency — load via (asdf:load-system :clawmacs/mcclim).
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; Genera Theme — McCLIM-only white background override
;;; --------------------------------------------------------------------------

(defvar *mcclim-bg-ink* (clim:make-rgb-color 1.0 1.0 1.0)
  "Default background ink for the McCLIM backend. White for Genera theme.")

(defun mcclim-apply-genera-theme ()
  "Patch *global-face-registry* for Genera-style white background.
Called once from backend-run before frame creation. Only affects the McCLIM
backend — the terminal backend keeps its dark theme since it initializes
its own face registry on startup."
  (let ((white-bg (make-color-spec :cga 15))
        (black-fg (make-color-spec :cga 0))
        (light-blue-bg (make-color-spec :hex "#D0E0F0"))
        (dark-blue-fg (make-color-spec :cga 4))
        (dark-green-fg (make-color-spec :cga 2))
        (dark-red-fg (make-color-spec :cga 1))
        (dark-yellow-fg (make-color-spec :cga 3))
        (dark-magenta-fg (make-color-spec :cga 5))
        (dark-cyan-fg (make-color-spec :cga 6))
        (gray-fg (make-color-spec :cga 8)))
    ;; Walk all faces: black bg → white bg, white/gray fg → black fg
    (maphash (lambda (name face)
               (let ((bg-val (when (face-background face)
                               (color-spec-value (face-background face))))
                     (fg-val (when (face-foreground face)
                               (color-spec-value (face-foreground face)))))
                 ;; Skip faces with special overrides below
                 (unless (member name '(:modeline :selector-selected
                                        :minibuffer-selected :minibuffer-match
                                        :minibuffer-selected-match
                                        :approval-diff-add :approval-diff-remove
                                        :tool-call :tool-call-paren
                                        :tool-call-keyword :tool-call-string
                                        :tool-call-comment :tool-call-number
                                        :tool-result :tool-result-paren
                                        :tool-result-keyword :tool-result-string
                                        :tool-result-comment :tool-result-number))
                   ;; Black bg → white bg
                   (when (and (face-background face)
                              (eq :cga (color-spec-type (face-background face)))
                              (eql bg-val 0))
                     (setf (face-background face) white-bg))
                   ;; White/gray fg → black fg
                   (when (and (face-foreground face)
                              (eq :cga (color-spec-type (face-foreground face)))
                              (member fg-val '(7 15)))
                     (setf (face-foreground face) black-fg)))))
             *global-face-registry*)
    ;; Special overrides
    ;; :modeline — keep gray bg, black fg, bold (already correct)
    ;; :selector-selected — light blue bg, black fg
    (let ((f (global-face :selector-selected)))
      (when f
        (setf (face-background f) light-blue-bg
              (face-foreground f) black-fg)))
    ;; :minibuffer-selected — light blue bg, black fg
    (let ((f (global-face :minibuffer-selected)))
      (when f
        (setf (face-background f) light-blue-bg
              (face-foreground f) black-fg)))
    ;; :minibuffer-match — dark blue fg on white bg, bold
    (let ((f (global-face :minibuffer-match)))
      (when f
        (setf (face-background f) white-bg
              (face-foreground f) dark-blue-fg
              (face-bold-p f) t)))
    ;; :minibuffer-selected-match — dark blue fg on light blue bg, bold
    (let ((f (global-face :minibuffer-selected-match)))
      (when f
        (setf (face-background f) light-blue-bg
              (face-foreground f) dark-blue-fg
              (face-bold-p f) t)))
    ;; :approval-diff-add — green fg on white bg
    (let ((f (global-face :approval-diff-add)))
      (when f
        (setf (face-background f) white-bg)))
    ;; :approval-diff-remove — red fg on white bg
    (let ((f (global-face :approval-diff-remove)))
      (when f
        (setf (face-background f) white-bg)))
    ;; Tool call/result faces — use darker inks for contrast on white.
    (dolist (spec `((:tool-call ,white-bg ,dark-blue-fg t)
                    (:tool-call-paren ,white-bg ,dark-blue-fg t)
                    (:tool-call-keyword ,white-bg ,dark-red-fg t)
                    (:tool-call-string ,white-bg ,dark-green-fg nil)
                    (:tool-call-comment ,white-bg ,dark-cyan-fg nil)
                    (:tool-call-number ,white-bg ,dark-magenta-fg nil)
                    (:tool-result ,white-bg ,dark-green-fg t)
                    (:tool-result-paren ,white-bg ,dark-blue-fg t)
                    (:tool-result-keyword ,white-bg ,dark-yellow-fg t)
                    (:tool-result-string ,white-bg ,dark-cyan-fg nil)
                    (:tool-result-comment ,white-bg ,gray-fg nil)
                    (:tool-result-number ,white-bg ,dark-magenta-fg nil)))
      (let ((f (global-face (first spec))))
        (when f
          (setf (face-background f) (second spec)
                (face-foreground f) (third spec)
                (face-bold-p f) (fourth spec)))))
    ;; Patch per-buffer face-sets on existing buffers
    (let ((user-bg (make-color-spec :hex "#D0D8E8"))
          (agent-bg white-bg))
      (dolist (buf *buffer-ring*)
        (maphash (lambda (sender-kw fs)
                   (let ((default-face (get-face fs :default)))
                     (when default-face
                       (if (eq sender-kw :user)
                           (setf (face-background default-face) user-bg
                                 (face-foreground default-face) black-fg)
                           (progn
                             (setf (face-background default-face) agent-bg)
                             (when (and (face-foreground default-face)
                                        (eq :cga (color-spec-type
                                                  (face-foreground default-face)))
                                        (member (color-spec-value
                                                 (face-foreground default-face))
                                                '(7 15)))
                               (setf (face-foreground default-face) black-fg)))))))
                 (buffer-face-registry buf))))))

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
        (values (clim:make-rgb-color 0.0 0.0 0.0)
                *mcclim-bg-ink*
                (clim:make-text-style :fix :roman :normal)))))

;;; --------------------------------------------------------------------------
;;; Presentation Types — semantic mouse interaction
;;;
;;; CLIM presentation types make rendered objects clickable. Each type
;;; defines what kind of object is displayed, and translators define
;;; what actions are available when the user interacts with them.
;;; --------------------------------------------------------------------------

(clim:define-presentation-type chat-message ()
  :description "a chat message")

(clim:define-presentation-type tool-call ()
  :description "a tool call")

(clim:define-presentation-type buffer-ref ()
  :description "a buffer reference")

(clim:define-presentation-type model-ref ()
  :description "a model reference")

;;; --------------------------------------------------------------------------
;;; Application Frame
;;; --------------------------------------------------------------------------

(clim:define-application-frame clawmacs-gui ()
  ((backend :initarg :backend :accessor frame-backend)
   (always-poll-p :initarg :always-poll-p :accessor frame-always-poll-p :initform nil)
   (char-width :accessor frame-char-width :initform 0)
   (char-height :accessor frame-char-height :initform 0)
   (quit-flag :accessor frame-quit-flag :initform nil))
  (:panes
   (main-pane :application
              :display-function 'display-main-pane
              :text-style (clim:make-text-style :fix :roman :normal)
              :scroll-bars nil
              :background (clim:make-rgb-color 1.0 1.0 1.0)
              :foreground (clim:make-rgb-color 0.0 0.0 0.0))
   (modeline-pane :application
                  :display-function 'display-modeline-pane
                  :text-style (clim:make-text-style :fix :roman :normal)
                  :scroll-bars nil
                  :background (clim:make-rgb-color 0.67 0.67 0.67)
                  :foreground (clim:make-rgb-color 0.0 0.0 0.0))
   (who-line-pane :application
                  :display-function 'display-who-line-pane
                  :text-style (clim:make-text-style :fix :roman :normal)
                  :scroll-bars nil
                  :background (clim:make-rgb-color 0.93 0.93 0.93)
                  :foreground (clim:make-rgb-color 0.0 0.0 0.0)))
  (:layouts
   (default
    (clim:vertically ()
      (:fill main-pane)
      who-line-pane
      modeline-pane))))

;;; --------------------------------------------------------------------------
;;; Drawing Primitives
;;; --------------------------------------------------------------------------

(defun ensure-char-metrics (frame pane)
  "Compute character width and height lazily on first display call.
Called from inside display functions where the pane's medium is ready."
  (when (zerop (frame-char-width frame))
    (handler-case
        (let ((ts (clim:make-text-style :fix :roman :normal)))
          ;; Use a 10-char string to get accurate advance width per character
          ;; (single-char text-size may include bearing/rounding artifacts)
          (multiple-value-bind (width10 height)
              (clim:text-size pane "MMMMMMMMMM" :text-style ts)
            (setf (frame-char-width frame) (/ width10 10))
            (setf (frame-char-height frame) height)))
      (error ()
        ;; Medium not ready yet — use defaults, will retry next call
        (setf (frame-char-width frame) 7
              (frame-char-height frame) 14)))))

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
  (multiple-value-bind (width height) (pane-pixel-size pane)
    (clim:draw-rectangle* pane 0 0 width height :ink ink)))

(defun fill-row (pane row cols bg-ink char-w char-h)
  "Fill an entire row with background color."
  (clim:draw-rectangle* pane
                        0 (* row char-h)
                        (* cols char-w) (* (1+ row) char-h)
                        :ink bg-ink))

(defun draw-faced-spans (pane row start-col spans char-w char-h)
  "Draw SPANS starting at (ROW, START-COL) using global face definitions."
  (let ((col start-col))
    (dolist (span spans)
      (multiple-value-bind (fg bg ts)
          (resolve-global-face-inks (car span))
        (draw-text-at pane row col (cdr span) fg bg ts char-w char-h))
      (incf col (length (cdr span))))))

(defun pane-pixel-size (pane)
  "Return (values width height) — the allocated pixel size of PANE.
Clamps sheet-region (which grows with output records) to the frame's
top-level sheet dimensions to prevent exponential growth."
  (let* ((frame (clim:pane-frame pane))
         (top-sheet (clim:frame-top-level-sheet frame))
         (top-region (clim:sheet-region top-sheet))
         (max-w (floor (clim:bounding-rectangle-width top-region)))
         (max-h (floor (clim:bounding-rectangle-height top-region)))
         (region (clim:sheet-region pane)))
    (values (min max-w (floor (clim:bounding-rectangle-width region)))
            (min max-h (floor (clim:bounding-rectangle-height region))))))

(defun pane-grid-dimensions (pane char-w char-h)
  "Return (values cols rows) — the character grid size of PANE."
  (multiple-value-bind (width height) (pane-pixel-size pane)
    (values (max 1 (floor width char-w))
            (max 1 (floor height char-h)))))

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
  "Render the modeline string into PANE using the modeline face.
Wrapped in updating-output so CLIM skips redraw when the text hasn't changed."
  (let* ((ml-face (make-modeline-face))
         (resolved (resolve-face ml-face))
         (text (format-modeline buf cols :major-mode (buffer-major-mode buf))))
    (clim:updating-output (pane :unique-id 'modeline-content
                                :cache-value text
                                :cache-test #'string=)
      (multiple-value-bind (fg bg ts) (resolve-face-inks resolved)
        (fill-row pane 0 cols bg char-w char-h)
        (draw-text-at pane 0 0 text fg bg ts char-w char-h)))))

;;; --------------------------------------------------------------------------
;;; Who-Line Display
;;; --------------------------------------------------------------------------

(defun display-who-line-pane (frame pane)
  "Display function for the who-line pane. Shows 2 rows of context-dependent hints."
  (ensure-char-metrics frame pane)
  (let* ((char-w (frame-char-width frame))
         (char-h (frame-char-height frame))
         (buf (current-buffer)))
    (when (zerop char-w) (return-from display-who-line-pane))
    (multiple-value-bind (cols rows) (pane-grid-dimensions pane char-w char-h)
      (declare (ignore rows))
      (multiple-value-bind (row1 row2) (format-who-line buf cols)
        (let ((cache-key (concatenate 'string row1 "|" row2)))
          (clim:updating-output (pane :unique-id 'who-line-content
                                      :cache-value cache-key
                                      :cache-test #'string=)
            (multiple-value-bind (wl-fg wl-bg wl-ts) (resolve-global-face-inks :who-line)
              (fill-row pane 0 cols wl-bg char-w char-h)
              (fill-row pane 1 cols wl-bg char-w char-h)
              (draw-text-at pane 0 0
                            (subseq row1 0 (min (length row1) cols))
                            wl-fg wl-bg wl-ts char-w char-h)
              (draw-text-at pane 1 0
                            (subseq row2 0 (min (length row2) cols))
                            wl-fg wl-bg wl-ts char-w char-h))))))))

;;; --------------------------------------------------------------------------
;;; Main Pane Display
;;; --------------------------------------------------------------------------

(defun display-main-pane (frame pane)
  "Display function for the main pane. Dispatches to buffer/selector rendering.
When the minibuffer is active, draws a centered popup overlay on top."
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
          (*think-selector-active*
           (mcclim-render-think-selector pane modeline-pane rows cols
                                         char-w char-h frame))
          (t
           (mcclim-render-buffer pane (current-buffer) rows cols
                                 char-w char-h)))
        ;; Popup overlay for minibuffer and automatic skill completion.
        (when (or *minibuffer-active* *skill-completion-active*)
          (mcclim-render-completion-popup pane cols rows char-w char-h))))))

;;; --------------------------------------------------------------------------
;;; Buffer Title Bar
;;; --------------------------------------------------------------------------

(defun mcclim-render-buffer-title (pane buf cols char-w char-h)
  "Render the buffer name in italics at row 0, with a thin gray rule underneath."
  (let ((title (buffer-name buf))
        (italic-ts (clim:make-text-style :fix :italic :normal)))
    (fill-row pane 0 cols *mcclim-bg-ink* char-w char-h)
    (draw-text-at pane 0 1 title (clim:make-rgb-color 0.0 0.0 0.0)
                  *mcclim-bg-ink* italic-ts char-w char-h)
    ;; Thin rule under title
    (clim:draw-line* pane 0 (1- char-h) (* cols char-w) (1- char-h)
                      :ink (clim:make-rgb-color 0.67 0.67 0.67))))

;;; --------------------------------------------------------------------------
;;; Buffer Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-buffer (pane buf rows cols char-w char-h)
  "Render the full buffer: title bar at row 0, then history + input below."
  (when (document-buffer-p buf)
    (return-from mcclim-render-buffer
      (mcclim-render-document-buffer pane buf rows cols char-w char-h)))
  (let* ((total-height (1- rows))
         (width cols)
         (input-height (calculate-input-height buf total-height width))
         (history-height (- total-height input-height))
         (input-start-row (1+ history-height)))
    ;; Clear pane and render title bar
    (clear-pane-with-ink pane *mcclim-bg-ink*)
    (mcclim-render-buffer-title pane buf cols char-w char-h)
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
        ;; Only write scroll-offset if this is the primary frame (not a popup),
        ;; to avoid cross-thread writes from read-only popup viewers.
        (unless (frame-always-poll-p (clim:pane-frame pane))
          (setf (buffer-scroll-offset buf) scroll-offset))
        ;; Render visible history messages — wrapped in presentation types
        ;; so they are clickable objects in CLIM's semantic interaction model.
        (let ((virtual-row 0))
          (loop :for msg :in history-messages
                :for msg-h :in msg-heights
                :for msg-top := virtual-row
                :for msg-bottom := (+ virtual-row msg-h)
                :do (setf virtual-row msg-bottom)
                    (when (and (< msg-top visible-bottom)
                               (> msg-bottom visible-top))
                      (let ((screen-row (+ 1 (- msg-top visible-top)))
                            (ptype (if (eq :tool-result (message-sender msg))
                                       'tool-call
                                       'chat-message)))
                        (clim:with-output-as-presentation (pane msg ptype)
                          (mcclim-render-message-lines pane msg screen-row width
                                                       char-w char-h
                                                       :max-rows history-height))))))))
    ;; Render input area
    (if (buffer-approval-pending buf)
        (mcclim-render-approval-prompt pane buf input-start-row cols char-w char-h rows)
        (mcclim-render-message-lines pane (buffer-input-message buf)
                                     input-start-row cols char-w char-h
                                     :show-cursor t :max-rows rows))))

(defun mcclim-render-document-buffer (pane buf rows cols char-w char-h)
  "Render BUF as a full-pane editable document buffer."
  (clear-pane-with-ink pane *mcclim-bg-ink*)
  (let ((row 0))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))) (< row rows))
          :do (incf row (mcclim-render-message-lines pane msg row cols
                                                     char-w char-h
                                                     :max-rows rows)))
    (let ((text-height (max 1 (- rows row))))
      (multiple-value-bind (start-row scroll-offset)
          (scratch-buffer-scroll-geometry buf text-height cols)
        (unless (frame-always-poll-p (clim:pane-frame pane))
          (setf (buffer-scroll-offset buf) scroll-offset))
        (mcclim-render-message-lines pane
                                     (buffer-input-message buf)
                                     (+ row start-row)
                                     cols
                                     char-w
                                     char-h
                                     :show-cursor t
                                     :max-rows rows
                                     :prefix "")))))

;;; --------------------------------------------------------------------------
;;; Message Line Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-message-lines (pane msg start-row width char-w char-h
                                    &key show-cursor (max-rows 1000)
                                      (prefix (message-sender-prefix msg)))
  "Render MSG's lines into PANE starting at START-ROW with line wrapping.
Returns the number of visual rows consumed."
  (let* ((prefix-len (length prefix))
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
              :do (let* ((content (line-content line))
                         (tool-face-name (tool-line-base-face-name msg content))
                         (content-len (length content))
                         (num-wraps (wrapped-line-count content display-width)))
                    (dotimes (wrap-idx num-wraps)
                      (when (< row max-rows)
                        (let* ((chunk-start (* wrap-idx display-width))
                               (chunk-end (min (* (1+ wrap-idx) display-width) content-len))
                               (chunk (subseq content chunk-start chunk-end))
                               (first-row-p (and (= line-idx 0) (= wrap-idx 0))))
                          (when (>= row 0)
                            (if tool-face-name
                                (multiple-value-bind (_tool-fg tool-bg _tool-ts)
                                    (resolve-global-face-inks tool-face-name)
                                  (declare (ignore _tool-fg _tool-ts))
                                  (fill-row pane row width tool-bg char-w char-h)
                                  (draw-faced-spans
                                   pane row (if first-row-p 0 prefix-len)
                                   (tool-line-display-spans
                                    content tool-face-name
                                    :start chunk-start :end chunk-end
                                    :prefix (and first-row-p prefix))
                                   char-w char-h))
                                (progn
                                  (fill-row pane row width bg char-w char-h)
                                  (when first-row-p
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
                                                         fg char-w char-h))))))
                          (when (and show-cursor
                                     (>= row 0)
                                     (eq line (message-point-line msg)))
                            (let ((point-off (message-point-offset msg)))
                              (when (and (>= point-off chunk-start)
                                         (or (< point-off chunk-end)
                                             (and (= wrap-idx (1- num-wraps))
                                                  (= point-off chunk-end))))
                                (setf cursor-y row
                                      cursor-x (+ prefix-len
                                                  (- point-off chunk-start))))))
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
    (clear-pane-with-ink pane *mcclim-bg-ink*)
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
    ;; Buffer entries — wrapped as buffer-name presentations for clickability
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
          :do (clim:with-output-as-presentation (pane buf 'buffer-ref)
                (multiple-value-bind (fg bg ts)
                    (resolve-global-face-inks (if selected-p
                                                  :selector-selected
                                                  :selector-entry))
                  (fill-row pane row width bg char-w char-h)
                  (draw-text-at pane row 0
                                (subseq line 0 (min (length line) width))
                                fg bg ts char-w char-h))))
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
    (clear-pane-with-ink pane *mcclim-bg-ink*)
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
    ;; Model entries — wrapped as model-name presentations for clickability
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
          :do (clim:with-output-as-presentation (pane entry 'model-ref)
                (multiple-value-bind (fg bg ts)
                    (resolve-global-face-inks (if selected-p
                                                  :selector-selected
                                                  :selector-entry))
                  (fill-row pane row width bg char-w char-h)
                  (draw-text-at pane row 0
                                (subseq line 0 (min (length line) width))
                                fg bg ts char-w char-h))))
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
;;; Think Selector Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-think-selector (pane modeline-pane rows cols
                                     char-w char-h frame)
  "Render the think-level selector overlay."
  (declare (ignore frame))
  (let* ((width cols)
         (height rows)
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
    (clear-pane-with-ink pane *mcclim-bg-ink*)
    (when (< 1 height)
      (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-title)
        (draw-text-at pane 1 2 "Select Think Level" fg bg ts char-w char-h)))
    (when (< 2 height)
      (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-separator)
        (draw-text-at pane 2 2
                      (make-string (min (- width 4) 50) :initial-element #\─)
                      fg bg ts char-w char-h)))
    (when (< 3 height)
      (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-header)
        (let ((header (format-think-selector-line "  " "THINK LEVEL" width)))
          (fill-row pane 3 width bg char-w char-h)
          (draw-text-at pane 3 0
                        (subseq header 0 (min (length header) width))
                        fg bg ts char-w char-h))))
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
          :do (clim:with-output-as-presentation (pane entry 'model-ref)
                (multiple-value-bind (fg bg ts)
                    (resolve-global-face-inks (if selected-p
                                                  :selector-selected
                                                  :selector-entry))
                  (fill-row pane row width bg char-w char-h)
                  (draw-text-at pane row 0
                                (subseq line 0 (min (length line) width))
                                fg bg ts char-w char-h))))
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
    (let ((footer-row (1- height)))
      (when (plusp footer-row)
        (multiple-value-bind (fg bg ts) (resolve-global-face-inks :selector-footer)
          (draw-text-at pane footer-row 2
                        "[RET] select  [C-g/q] cancel  default = clear  * = active"
                        fg bg ts char-w char-h))))
    (let* ((ml-face (make-modeline-face))
           (resolved (resolve-face ml-face))
           (pm (resolve-modeline-provider-model current-buf))
           (ml-text (format nil " [think-selector] ~A | ~D level~:[s~;~] available"
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
;;; Popup Completion Overlay
;;; --------------------------------------------------------------------------

(defun draw-fuzzy-match-spans (pane row start-col text match-set
                               base-fg base-bg base-ts
                               match-fg match-bg match-ts
                               char-w char-h)
  "Draw TEXT at (ROW, START-COL) with match-highlighted characters in spans.
Instead of drawing per-character, collects consecutive same-style characters
into spans and draws each span as a single draw-text-at call."
  (let ((len (length text))
        (span-start 0)
        (span-matched nil))
    (labels ((flush-span (end)
               (when (> end span-start)
                 (let ((span-text (subseq text span-start end))
                       (col (+ start-col span-start)))
                   (if span-matched
                       (draw-text-at pane row col span-text
                                     match-fg match-bg match-ts char-w char-h)
                       (draw-text-at pane row col span-text
                                     base-fg base-bg base-ts char-w char-h))))))
      (loop :for i :from 0 :below len
            :for cur-matched := (and match-set (gethash i match-set))
            :do (when (and (plusp i) (not (eq (not cur-matched) (not span-matched))))
                  (flush-span i)
                  (setf span-start i))
                (setf span-matched cur-matched))
      (flush-span len))))

(defun mcclim-render-completion-popup (pane cols rows char-w char-h)
  "Render a centered popup overlay for completion on the main pane.
Drawn on top of existing buffer content when minibuffer or skill completion is
active.
Uses span-batched drawing for fuzzy-match highlighting instead of per-character."
  (let* ((skill-popup-p (and (not *minibuffer-active*) *skill-completion-active*))
         (items (if skill-popup-p
                    *skill-completion-filtered-items*
                    *minibuffer-filtered-items*))
         (positions (if skill-popup-p
                        *skill-completion-match-positions*
                        *minibuffer-match-positions*))
         (selected (if skill-popup-p
                       *skill-completion-selected-index*
                       *minibuffer-selected-index*))
         (scroll (if skill-popup-p
                     *skill-completion-scroll-offset*
                     *minibuffer-scroll-offset*))
         (total (length items))
         ;; Popup dimensions
         (popup-w (min (- cols 4) (max 40 (floor (* cols 3) 5))))
         (max-height (if skill-popup-p
                         *skill-completion-max-height*
                         *minibuffer-max-height*))
         (max-item-rows (max 0 (min (or max-height 12) (- rows 4))))
         (display-total (if skill-popup-p (max 1 total) total))
         (item-rows (min display-total max-item-rows))
         (popup-h (+ 1 item-rows))  ; 1 prompt row + items
         ;; Center position
         (popup-left (floor (- cols popup-w) 2))
         (popup-top (floor (- rows popup-h) 2))
         ;; Popup background from face system (candidate face bg or light gray)
         (popup-bg (multiple-value-bind (fg bg ts)
                       (resolve-global-face-inks :minibuffer-candidate)
                     (declare (ignore fg ts))
                     bg))
         (border-ink (clim:make-rgb-color 0.4 0.4 0.4))
         ;; Pixel coordinates for border
         (px-left (* popup-left char-w))
         (px-top (* popup-top char-h))
         (px-right (* (+ popup-left popup-w) char-w))
         (px-bottom (* (+ popup-top popup-h) char-h)))
    ;; Draw popup background
    (clim:draw-rectangle* pane px-left px-top px-right px-bottom
                          :ink popup-bg)
    ;; Draw border
    (clim:draw-rectangle* pane px-left px-top px-right px-bottom
                          :ink border-ink :filled nil)
    ;; Prompt row: "prompt: input" with block cursor
    (let* ((prompt-str (if skill-popup-p "Skill: " (format nil "~A: " *minibuffer-prompt*)))
           (input (if skill-popup-p
                      (format nil "$~A" *skill-completion-query*)
                      *minibuffer-input*))
           (point (if skill-popup-p
                      (length input)
                      *minibuffer-point*))
           (prompt-line (concatenate 'string prompt-str input))
           (display-width (- popup-w 2))
           (visible (subseq prompt-line 0 (min (length prompt-line) display-width)))
           (row popup-top)
           (col (1+ popup-left)))
      (multiple-value-bind (fg bg ts) (resolve-global-face-inks :minibuffer-prompt)
        (declare (ignore bg))
        ;; Fill prompt row background
        (clim:draw-rectangle* pane
                              (+ px-left char-w) (* row char-h)
                              (- px-right char-w) (* (1+ row) char-h)
                              :ink popup-bg)
        (draw-text-at pane row col visible fg popup-bg ts char-w char-h))
      ;; Block cursor
      (let ((cursor-col (+ col (length prompt-str) point)))
        (when (< cursor-col (+ popup-left popup-w -1))
          (let ((char-at-cursor (if (< point (length input))
                                    (char input point)
                                    #\Space)))
            (multiple-value-bind (fg bg ts) (resolve-global-face-inks :minibuffer-cursor)
              (draw-text-at pane row cursor-col (string char-at-cursor)
                            fg bg ts char-w char-h))))))
    ;; Candidate rows — batched span drawing instead of per-character
    (if (and skill-popup-p (zerop total) (plusp item-rows))
        (multiple-value-bind (base-fg base-bg base-ts)
            (resolve-global-face-inks :minibuffer-candidate)
          (let ((row (+ popup-top 1)))
            (clim:draw-rectangle* pane
                                  (+ px-left char-w) (* row char-h)
                                  (- px-right char-w) (* (1+ row) char-h)
                                  :ink base-bg)
            (draw-text-at pane row (+ popup-left 3) "No matching skills"
                          base-fg base-bg base-ts char-w char-h)))
        (loop :for row-idx :from 0 :below item-rows
              :for item-idx := (+ scroll row-idx)
              :while (< item-idx total)
              :for item := (nth item-idx items)
              :for row := (+ popup-top 1 row-idx)
              :for display := (minibuffer-item-display item)
              :for display-trimmed := (subseq display 0 (min (length display) (- popup-w 4)))
              :for match-pos := (nth item-idx positions)
              :for selected-p := (= item-idx selected)
              :do
                 (let* ((base-face-name (if selected-p :minibuffer-selected :minibuffer-candidate))
                        (match-face-name (if selected-p :minibuffer-selected-match :minibuffer-match))
                        (match-set (when match-pos
                                     (let ((ht (make-hash-table :test #'eql)))
                                       (dolist (p match-pos ht)
                                         (setf (gethash p ht) t))))))
                   (multiple-value-bind (base-fg base-bg base-ts)
                       (resolve-global-face-inks base-face-name)
                     ;; Fill row within popup
                     (clim:draw-rectangle* pane
                                           (+ px-left char-w) (* row char-h)
                                           (- px-right char-w) (* (1+ row) char-h)
                                           :ink base-bg)
                     ;; Indent
                     (draw-text-at pane row (+ popup-left 1) "  "
                                   base-fg base-bg base-ts char-w char-h)
                     ;; Draw text with span-batched fuzzy match highlighting
                     (multiple-value-bind (match-fg match-bg match-ts)
                         (resolve-global-face-inks match-face-name)
                       (draw-fuzzy-match-spans pane row (+ popup-left 3)
                                               display-trimmed match-set
                                               base-fg base-bg base-ts
                                               match-fg match-bg match-ts
                                               char-w char-h))))))))

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
      ;; C-h prefix — McCLIM delivers Control+h with key-name :backspace
      ;; (ASCII 8 = BS), so we must check the modifier+character explicitly
      ;; before the generic backspace handling below.
      ((and ctrl-p (characterp char) (char-equal char #\h)
            (not *meta-pending*) (not *cx-pending*) (not *cc-pending*) (not *ch-pending*))
       (setf *ch-pending* t)
       nil)
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
      ;; Normal key
      (t key))))


;;; --------------------------------------------------------------------------
;;; Redisplay Helper
;;; --------------------------------------------------------------------------

(defun update-pane-sizes (frame)
  "Resize modeline (1 row) and who-line (2 rows) panes based on char metrics."
  (let ((char-h (frame-char-height frame)))
    (when (plusp char-h)
      (let ((ml-pane (clim:find-pane-named frame 'modeline-pane)))
        (when ml-pane
          (clim:change-space-requirements ml-pane
                                          :height char-h
                                          :min-height char-h
                                          :max-height char-h)))
      (let ((wl-pane (clim:find-pane-named frame 'who-line-pane)))
        (when wl-pane
          (clim:change-space-requirements wl-pane
                                          :height (* 2 char-h)
                                          :min-height (* 2 char-h)
                                          :max-height (* 2 char-h)))))))



;;; --------------------------------------------------------------------------
;;; Custom Top-Level Event Loop
;;; --------------------------------------------------------------------------

(defmethod clim:default-frame-top-level ((frame clawmacs-gui) &key &allow-other-keys)
  "Custom event loop for clawmacs — mirrors the croatoan event loop structure.
Blocking reads when idle, polling with sleep when streaming.
Called by the standard run-frame-top-level AFTER the frame is adopted/enabled
and all mediums are connected to the X11 backend."
  ;; Char metrics are initialized lazily by ensure-char-metrics inside
  ;; the display functions when the medium is ready.
  ;; Let CLIM handle initial display via its own redisplay machinery,
  ;; then force a redisplay once we enter the event loop.
  ;; Main event loop
  (let ((poll-mode (frame-always-poll-p frame)))
    (loop :until (frame-quit-flag frame)
          :for buf := (current-buffer)
          :for streaming := (buffer-pending-stream buf)
          :for oauth-pending := *openai-oauth-pending*
          :for need-redisplay := nil
          :for force-redisplay := nil
          :for event := (if (or streaming poll-mode oauth-pending)
                            (clim:event-read-no-hang
                             (clim:frame-top-level-sheet frame))
                            (clim:event-read
                             (clim:frame-top-level-sheet frame)))
          :do (cond
                ;; Timeout path: sleep before next poll
                ((null event)
                 (sleep (if (and poll-mode (not streaming) (not oauth-pending))
                            0.2
                            0.05)))
                ;; Key press event — suppress in popup to avoid corrupting
                ;; shared prefix key state (*meta-pending* etc.)
                ((typep event 'clim:key-press-event)
                 (unless poll-mode
                   (let* ((key (mcclim-normalize-key event)))
                     (when key
                       (let ((result (handle-key-event buf key)))
                         (when (eq result :quit)
                           (setf (frame-quit-flag frame) t)
                           (clim:frame-exit frame))
                         (when (eq result :redraw)
                           (setf force-redisplay t))
                         (setf need-redisplay t)))))
                 ;; Keep CLIM's input context machinery alive for presentations.
                 ;; This mirrors the standard top-level behavior while preserving
                 ;; clawmacs key dispatch.
                 (clim:handle-event (clim:event-sheet event) event))
                ;; Explicit redraw triggers
                ((or (typep event 'clim:window-repaint-event)
                     (typep event 'clim:window-configuration-event))
                 (setf force-redisplay t
                       need-redisplay t)
                 (clim:handle-event (clim:event-sheet event) event))
                ;; Other events (pointer, exposure, menu, etc.) — let CLIM handle
                ;; and schedule a redisplay after handling.
                (t
                 (clim:handle-event (clim:event-sheet event) event)
                 (setf need-redisplay t)))
              ;; Always poll streaming when active — regardless of event type.
              ;; This prevents X11 events (exposure, pointer) from starving
              ;; the streaming poll, and ensures the final response is
              ;; displayed immediately when the stream completes.
              (unless poll-mode
                (let ((cur (current-buffer)))
                  (when (buffer-pending-stream cur)
                    (update-streaming-response cur)
                    (setf need-redisplay t))
                  (when *openai-oauth-pending*
                    (update-openai-oauth-login)
                    (setf need-redisplay t))))
              ;; Redisplay when something changed
              (when (or need-redisplay streaming poll-mode oauth-pending)
                ;; Update scroll page size (window may have resized)
                ;; — but only for the primary frame, not popups
                (unless poll-mode
                  (let* ((main-pane (clim:find-pane-named frame 'main-pane))
                         (char-w (frame-char-width frame))
                         (char-h (frame-char-height frame)))
                    (when (and (plusp char-w) (plusp char-h))
                      (multiple-value-bind (cols rows)
                          (pane-grid-dimensions main-pane char-w char-h)
                        (declare (ignore cols))
                        (setf *scroll-page-size* (max 1 (- rows 3)))))))
                ;; Keep who-line/modeline fixed heights in character rows.
                (update-pane-sizes frame)
                ;; Use CLIM's frame-level redisplay dispatch rather than
                ;; manually iterating panes.
                (clim:redisplay-frame-panes frame :force-p force-redisplay)))))

;;; --------------------------------------------------------------------------
;;; Popup Frame Lifecycle (read-only X11 viewer from terminal mode)
;;; --------------------------------------------------------------------------

(defvar *popup-frames* nil
  "List of (frame . thread) pairs for active popup viewers.")

(defun cleanup-popup-frames ()
  "Remove entries from *popup-frames* whose threads are no longer alive."
  (setf *popup-frames*
        (remove-if-not (lambda (pair)
                         (bt:thread-alive-p (cdr pair)))
                       *popup-frames*)))

(defun close-all-popup-frames ()
  "Signal all popup frames to quit and wait for their threads to finish."
  (dolist (pair *popup-frames*)
    (let ((frame (car pair)))
      (setf (frame-quit-flag frame) t)))
  ;; Give threads a moment to exit, then clean up
  (sleep 0.5)
  (cleanup-popup-frames))

(defun spawn-mcclim-popup ()
  "Spawn a read-only McCLIM popup window in a background thread.
Uses dark theme (black background) and polls for redisplay at ~200ms.
Keyboard input is suppressed to avoid corrupting shared prefix key state."
  (cleanup-popup-frames)
  (let ((thread
          (bt:make-thread
           (lambda ()
             (let ((*mcclim-bg-ink* (clim:make-rgb-color 0.0 0.0 0.0)))
               (handler-case
                   (let ((frame (clim:make-application-frame
                                 'clawmacs-gui
                                 :backend nil
                                 :always-poll-p t
                                 :width 900
                                 :height 700)))
                     (push (cons frame (bt:current-thread)) *popup-frames*)
                     (clim:run-frame-top-level frame))
                 (error (c)
                   (ignore-errors
                     (format *error-output*
                             "~&Popup frame error: ~A~%" c))))))
           :name "clawmacs-popup")))
    (declare (ignore thread))
    t))

;;; --------------------------------------------------------------------------
;;; Backend Class and Entry Point
;;; --------------------------------------------------------------------------

(defclass mcclim-backend (ui-backend)
  ((frame :accessor backend-frame :initform nil))
  (:documentation "McCLIM graphical backend with Genera-style interface.
Three-pane layout (main, who-line, modeline) with white background, italic
buffer title bars, context-dependent who-line hints, and popup completion.
Load via (asdf:load-system :clawmacs/mcclim)."))

(defmethod backend-run ((b mcclim-backend) initial-buffer)
  "Run the McCLIM graphical UI.
Creates the Genera-style frame (main, who-line, modeline), then enters
the event loop reading input and rendering until :QUIT."
  (mcclim-apply-genera-theme)
  (let ((frame (clim:make-application-frame 'clawmacs-gui
                 :backend b
                 :width 900
                 :height 700)))
    (setf (backend-frame b) frame)
    (clim:run-frame-top-level frame)))
