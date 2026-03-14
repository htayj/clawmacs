(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Default Face Definitions
;;; --------------------------------------------------------------------------

(defun make-default-user-face-set ()
  "Create the default face set for user messages.
Background: CGA dark-gray (#8), foreground: white."
  (make-face-set
   :user
   (list (make-instance 'face
           :name :default
           :background (make-color-spec :cga 8)
           :foreground (make-color-spec :cga 15)
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
  "Convert a color-spec to a croatoan color keyword or integer."
  (ecase (color-spec-type cs)
    (:cga
     (let ((val (color-spec-value cs)))
       (case val
         (0  :black)
         (1  :red)
         (2  :green)
         (3  :yellow)
         (4  :blue)
         (5  :magenta)
         (6  :cyan)
         (7  :white)
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
;;; Message Rendering
;;; --------------------------------------------------------------------------

(defun message-sender-prefix (msg)
  "Return the display prefix for MSG's sender."
  (format nil "~A> " (string-downcase (symbol-name (message-sender msg)))))

(defun render-message-lines (window msg start-row width &key show-cursor)
  "Render MSG's lines into WINDOW starting at START-ROW.
Returns the number of rows consumed."
  (let* ((prefix (message-sender-prefix msg))
         (prefix-len (length prefix))
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
             (croatoan:move window row 0)
             (croatoan:add-string window (make-string width :initial-element #\Space))
             (croatoan:move window row 0)
             (when (= line-idx 0)
               (croatoan:add-string window prefix))
             ;; NOTE: Lines longer than display-width are truncated, not wrapped.
             (let* ((indent (if (= line-idx 0) 0 prefix-len))
                    (content (line-content line))
                    (display-width (- width prefix-len))
                    (truncated (if (> (length content) display-width)
                                   (subseq content 0 display-width)
                                   content)))
               (when (> line-idx 0)
                 (croatoan:move window row indent))
               (croatoan:add-string window truncated))
             (when (and show-cursor (eq line (message-point-line msg)))
               (setf cursor-y row
                      cursor-x (+ prefix-len (message-point-offset msg))))
             (incf row))
    (when (and show-cursor cursor-y cursor-x)
      (croatoan:move window cursor-y (min cursor-x (1- width))))
    (- row start-row)))

;;; --------------------------------------------------------------------------
;;; Buffer Rendering
;;; --------------------------------------------------------------------------

(defun calculate-input-height (buf terminal-height)
  "Calculate the height of the input area in rows.
Minimum 3, maximum (floor terminal-height 3)."
  (let* ((input (buffer-input-message buf))
         (line-count (message-line-count input))
         (min-height 3)
         (max-height (floor terminal-height 3)))
    (max min-height (min line-count max-height))))

(defun render-buffer (buf main-window modeline-window)
  "Render the entire buffer: history + input into MAIN-WINDOW, modeline."
  (let* ((total-height (croatoan:height main-window))
         (width (croatoan:width main-window))
         (input-height (calculate-input-height buf total-height))
         (history-height (- total-height input-height))
         (input-start-row history-height))
    (croatoan:clear main-window)
    (let ((history-messages nil))
      (loop :for msg := (buffer-first-message buf) :then (message-next msg)
            :while (and msg (not (eq msg (buffer-input-message buf))))
            :do (push msg history-messages))
      (setf history-messages (nreverse history-messages))
      (let ((rows-used 0)
            (messages-to-render nil))
        (loop :for msg :in (reverse history-messages)
              :for msg-lines := (message-line-count msg)
              :while (<= (+ rows-used msg-lines) history-height)
              :do (incf rows-used msg-lines)
                  (push msg messages-to-render))
        (let ((row (- history-height rows-used)))
          (dolist (msg messages-to-render)
            (let ((consumed (render-message-lines main-window msg row width)))
              (incf row consumed))))))
    (render-message-lines main-window (buffer-input-message buf)
                          input-start-row width
                          :show-cursor t)
    (croatoan:refresh main-window)
    (render-modeline buf modeline-window)))
