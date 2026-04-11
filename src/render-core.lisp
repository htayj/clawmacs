(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Global Face Registry
;;; --------------------------------------------------------------------------

(defvar *global-face-registry* (make-hash-table :test #'eq)
  "Hash table mapping keyword names to face objects for theme-level faces.
These are faces not tied to any specific buffer or sender, such as modeline,
system messages, minibuffer, approval prompts, and selector overlays.")

(defun global-face (name)
  "Look up a face by keyword NAME in the global face registry.
Returns the face object, or nil if not found."
  (gethash name *global-face-registry*))

(defun (setf global-face) (face name)
  "Store FACE under keyword NAME in the global face registry."
  (setf (gethash name *global-face-registry*) face))

(defun init-global-faces ()
  "Populate the global face registry with all theme-level faces.
Call once at startup. These faces are customizable via customize-face."
  (let ((r *global-face-registry*))
    (clrhash r)
    ;; Modeline
    (setf (gethash :modeline r)
          (make-instance 'face :name :modeline
            :background (make-color-spec :cga 7)
            :foreground (make-color-spec :cga 0)
            :bold-p t :underline-p nil :reverse-p nil))
    ;; System messages (e.g. shell prefix output, notifications)
    (setf (gethash :system r)
          (make-instance 'face :name :system
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 6)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Compaction summaries are model-visible handoff context.
    (setf (gethash :compaction-summary r)
          (make-instance 'face :name :compaction-summary
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 3)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Debug messages — API request/response log when *debug-mode* is t.
    ;; Bright magenta distinguishes debug output from regular system messages.
    (setf (gethash :debug r)
          (make-instance 'face :name :debug
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 13)   ; bright magenta
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Minibuffer faces
    (setf (gethash :minibuffer-prompt r)
          (make-instance 'face :name :minibuffer-prompt
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 15)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :minibuffer-cursor r)
          (make-instance 'face :name :minibuffer-cursor
            :background (make-color-spec :cga 15)
            :foreground (make-color-spec :cga 0)
            :bold-p t :underline-p nil :reverse-p nil))
    (setf (gethash :minibuffer-candidate r)
          (make-instance 'face :name :minibuffer-candidate
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 7)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :minibuffer-selected r)
          (make-instance 'face :name :minibuffer-selected
            :background (make-color-spec :cga 15)
            :foreground (make-color-spec :cga 0)
            :bold-p t :underline-p nil :reverse-p nil))
    ;; Fuzzy-match highlight faces — matched characters are rendered in bright
    ;; yellow so they stand out against both dark and light backgrounds.
    (setf (gethash :minibuffer-match r)
          (make-instance 'face :name :minibuffer-match
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 11)   ; bright yellow on black
            :bold-p t :underline-p nil :reverse-p nil))
    (setf (gethash :minibuffer-selected-match r)
          (make-instance 'face :name :minibuffer-selected-match
            :background (make-color-spec :cga 15)
            :foreground (make-color-spec :cga 3)    ; dark yellow on white
            :bold-p t :underline-p nil :reverse-p nil))
    ;; Selector overlay faces (buffer selector, model selector)
    (setf (gethash :selector-title r)
          (make-instance 'face :name :selector-title
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 14)
            :bold-p t :underline-p nil :reverse-p nil))
    (setf (gethash :selector-separator r)
          (make-instance 'face :name :selector-separator
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 7)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :selector-header r)
          (make-instance 'face :name :selector-header
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 11)
            :bold-p t :underline-p nil :reverse-p nil))
    (setf (gethash :selector-entry r)
          (make-instance 'face :name :selector-entry
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 7)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :selector-selected r)
          (make-instance 'face :name :selector-selected
            :background (make-color-spec :cga 6)
            :foreground (make-color-spec :cga 0)
            :bold-p t :underline-p nil :reverse-p nil))
    (setf (gethash :selector-footer r)
          (make-instance 'face :name :selector-footer
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 2)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :selector-scroll r)
          (make-instance 'face :name :selector-scroll
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 11)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Approval prompt faces
    (setf (gethash :approval-header r)
          (make-instance 'face :name :approval-header
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 11)
            :bold-p t :underline-p nil :reverse-p nil))
    (setf (gethash :approval-code r)
          (make-instance 'face :name :approval-code
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 14)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :approval-text r)
          (make-instance 'face :name :approval-text
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 15)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :approval-diff-add r)
          (make-instance 'face :name :approval-diff-add
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 2)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :approval-diff-remove r)
          (make-instance 'face :name :approval-diff-remove
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 1)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :approval-options r)
          (make-instance 'face :name :approval-options
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 2)
            :bold-p t :underline-p nil :reverse-p nil))
    ;; Tool call faces
    (setf (gethash :tool-call r)
          (make-instance 'face :name :tool-call
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 14)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-call-paren r)
          (make-instance 'face :name :tool-call-paren
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 12)
            :bold-p t :underline-p nil :reverse-p nil))
    (setf (gethash :tool-call-keyword r)
          (make-instance 'face :name :tool-call-keyword
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 11)
            :bold-p t :underline-p nil :reverse-p nil))
    (setf (gethash :tool-call-string r)
          (make-instance 'face :name :tool-call-string
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 10)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-call-comment r)
          (make-instance 'face :name :tool-call-comment
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 6)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-call-number r)
          (make-instance 'face :name :tool-call-number
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 13)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Tool result faces
    (setf (gethash :tool-result r)
          (make-instance 'face :name :tool-result
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 10)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-result-paren r)
          (make-instance 'face :name :tool-result-paren
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 12)
            :bold-p t :underline-p nil :reverse-p nil))
    (setf (gethash :tool-result-keyword r)
          (make-instance 'face :name :tool-result-keyword
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 11)
            :bold-p t :underline-p nil :reverse-p nil))
    (setf (gethash :tool-result-string r)
          (make-instance 'face :name :tool-result-string
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 14)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-result-comment r)
          (make-instance 'face :name :tool-result-comment
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 6)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-result-number r)
          (make-instance 'face :name :tool-result-number
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 13)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Default text face — fallback for messages without a face set.
    ;; This should NEVER be the modeline face; it should be a sensible
    ;; text-on-dark-background face for generic content.
    (setf (gethash :default-text r)
          (make-instance 'face :name :default-text
            :background (make-color-spec :cga 0)
            :foreground (make-color-spec :cga 7)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Who-line face — light gray bg, black fg (McCLIM only, Genera-style)
    (setf (gethash :who-line r)
          (make-instance 'face :name :who-line
            :background (make-color-spec :hex "#EDEDED")
            :foreground (make-color-spec :cga 0)
            :bold-p nil :underline-p nil :reverse-p nil))
    r))

(defun collect-global-faces ()
  "Collect all faces from the global face registry.
Returns a list of plists with :face, :owner, :name, and :label keys."
  (let ((result nil))
    (maphash (lambda (name face)
               (push (list :face face
                           :owner :global
                           :name name
                           :label (format nil "global:~(~A~)" name))
                     result))
             *global-face-registry*)
    (sort result #'string< :key (lambda (p) (getf p :label)))))

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

(defun make-default-system-face-set ()
  "Create the default face set for system messages.
Uses the global :system face if available, otherwise cyan on black."
  (let ((sys-face (or (global-face :system)
                      (make-instance 'face :name :default
                        :background (make-color-spec :cga 0)
                        :foreground (make-color-spec :cga 6)
                        :bold-p nil :underline-p nil :reverse-p nil))))
    (make-face-set
     :system
     (list (make-instance 'face
             :name :default
             :parent sys-face
             :background nil :foreground nil
             :bold-p nil :underline-p nil :reverse-p nil)))))

(defun make-default-compaction-summary-face-set ()
  "Create the default face set for compaction summary messages."
  (let ((summary-face (or (global-face :compaction-summary)
                          (make-instance 'face :name :default
                            :background (make-color-spec :cga 0)
                            :foreground (make-color-spec :cga 3)
                            :bold-p nil :underline-p nil :reverse-p nil))))
    (make-face-set
     :compaction-summary
     (list (make-instance 'face
             :name :default
             :parent summary-face
             :background nil :foreground nil
             :bold-p nil :underline-p nil :reverse-p nil)))))

(defun make-modeline-face ()
  "Return the modeline face from the global registry, or create a default.
Prefers the stored global face for customizability."
  (or (global-face :modeline)
      (make-instance 'face
        :name :modeline
        :background (make-color-spec :cga 7)
        :foreground (make-color-spec :cga 0)
        :bold-p t
        :underline-p nil
        :reverse-p nil)))

(defun make-default-text-face ()
  "Return the default text face from the global registry, or create a default.
This is the fallback face for messages that lack a face set — white text on
black background. This must NOT be the modeline face."
  (or (global-face :default-text)
      (make-instance 'face
        :name :default-text
        :background (make-color-spec :cga 0)
        :foreground (make-color-spec :cga 7)
        :bold-p nil
        :underline-p nil
        :reverse-p nil)))

;;; --------------------------------------------------------------------------
;;; Modeline Formatting (pure string functions)
;;; --------------------------------------------------------------------------

(defun resolve-modeline-provider-model (buf)
  "Resolve the provider/model string for the modeline.
Returns a string like \"zai/glm-5\" or \"??\" on error."
  (handler-case
      (multiple-value-bind (provider model think-level)
          (resolve-buffer-provider-and-model buf)
        (format nil "~(~A~)/~A~@[ [think:~A]~]" provider model think-level))
    (error () "??")))

(defun format-modeline (buf width &key (major-mode "chat") provider-model)
  "Format the modeline string for BUF, fitting within WIDTH columns.
MAJOR-MODE is displayed on the far left (e.g. \"chat\", \"buffer-selector\").
PROVIDER-MODEL is the provider/model string (e.g. \"zai/glm-5\");
when nil it is resolved from the buffer's agent defaults."
  (let* ((pm (or provider-model (resolve-modeline-provider-model buf)))
         (dirty-marker (if (and buf (buffer-dirty-p buf)) "*" ""))
         (left (format nil " [~A~A] ~A | ~A | ~A | ~A"
                       major-mode
                       dirty-marker
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

;;; --------------------------------------------------------------------------
;;; Who-Line Formatting (pure string functions, McCLIM Genera-style)
;;; --------------------------------------------------------------------------

(defun format-who-line (buf width)
  "Return two strings (row1 row2) for the who-line, context-dependent hints.
BUF is the current buffer. WIDTH is the available character columns.
Mode dispatch priority matches handle-key-event in main.lisp."
  (flet ((pad (str)
           (if (>= (length str) width)
               (subseq str 0 width)
               (concatenate 'string str
                            (make-string (- width (length str))
                                         :initial-element #\Space)))))
    (let ((row1 "")
          (row2 ""))
      (cond
        (*minibuffer-active*
         (setf row1 " C-n/C-p: navigate  RET: confirm  C-g: cancel"
               row2 " Type to filter candidates"))
        (*buffer-selector-active*
         (setf row1 " RET: select  n: new  k: kill  C-g/q: cancel"
               row2 " C-p/C-n: navigate"))
        (*model-selector-active*
         (setf row1 " RET: select  C-g/q: cancel"
               row2 " C-p/C-n: navigate  *=active"))
        (*think-selector-active*
         (setf row1 " RET: select  C-g/q: cancel"
               row2 " C-p/C-n: navigate  default=clear  *=active"))
        (*customize-face-state*
         (setf row1 " C-n/C-p: next/prev  RET: cycle value  C-c C-c: save"
               row2 " C-g: cancel  Navigate fields with C-n/C-p"))
        (*openai-oauth-pending*
         (setf row1 " Browser login in progress"
               row2 " C-g: cancel"))
        (*deny-message-mode*
         (setf row1 " Type denial reason, then RET to send"
               row2 " Normal editing keys available"))
        ((and (current-buffer) (buffer-approval-pending (current-buffer)))
         (let* ((approval (buffer-approval-pending (current-buffer)))
                (tool-name (or (cdr (assoc :tool-name approval)) "")))
           (setf row1 " a: approve  d: deny  m: deny with message"
                 row2 (format nil " ~A" tool-name))))
        ((and buf (scratch-buffer-p buf))
         (setf row1 " Scratch buffer: edit freely  RET: newline"
               row2 " C-x b/C-x C-b: switch  C-l: redraw"))
        ((and buf (file-buffer-p buf))
         (setf row1 " File buffer: edit freely  RET: newline  C-x C-s: save"
               row2 " C-x C-f: open project file  C-x b/C-x C-b: switch"))
        (t
         (setf row1 " RET: send  C-o: newline  C-k: kill  C-y: yank  PgUp/Dn: scroll"
               row2 " C-x C-b: buffers  C-c A: agent  C-c C-m: model  C-c C-r: think  C-l: redraw  C-x C-c: quit")))
      (values (pad row1) (pad row2)))))

;;; --------------------------------------------------------------------------
;;; Line Wrapping Helpers (pure geometry)
;;; --------------------------------------------------------------------------

(defun wrapped-line-count (content display-width)
  "Return the number of visual rows needed to display CONTENT within DISPLAY-WIDTH.
An empty string takes 1 row. A string exactly DISPLAY-WIDTH chars takes 1 row."
  (if (or (zerop (length content)) (<= (length content) display-width))
      1
      (ceiling (length content) display-width)))

(defun message-visual-height (msg width &key (prefix (message-sender-prefix msg)))
  "Return the total visual rows MSG needs at the given terminal WIDTH.
Accounts for the sender prefix and line wrapping."
  (let* ((prefix-len (length prefix))
         (display-width (max 1 (- width prefix-len))))
    (loop :for line := (message-first-line msg) :then (line-next line)
          :while line
          :sum (wrapped-line-count (line-content line) display-width))))

(defun scratch-buffer-scroll-geometry (buf viewport-height width)
  "Return scratch render geometry as values START-ROW, SCROLL-OFFSET, MAX-SCROLL.
Scratch buffers use the same scroll offset semantics as chat history: zero
means the bottom of the document is visible."
  (let* ((height (max 1 viewport-height))
         (total-rows (message-visual-height (buffer-input-message buf)
                                            width
                                            :prefix ""))
         (max-scroll (max 0 (- total-rows height)))
         (scroll-offset (min (max 0 (buffer-scroll-offset buf)) max-scroll))
         (visible-bottom (- total-rows scroll-offset))
         (visible-top (max 0 (- visible-bottom height)))
         (start-row (- visible-top)))
    (values start-row scroll-offset max-scroll)))

;;; --------------------------------------------------------------------------
;;; Message Formatting (pure string functions)
;;; --------------------------------------------------------------------------

(defun message-sender-prefix (msg)
  "Return the display prefix for MSG's sender."
  (format nil "~A> " (string-downcase (symbol-name (message-sender msg)))))

(defun tool-call-message-p (msg)
  "Return non-nil when MSG contains one or more tool_use blocks."
  (and (message-raw-content msg)
       (some (lambda (block)
               (string= "tool_use" (or (cdr (assoc :type block)) "")))
             (message-raw-content msg))))

(defun tool-line-base-face-name (msg line-content)
  "Return the base global face name for a tool display line, or nil."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line-content)))
    (cond
      ((eq :tool-result (message-sender msg))
       :tool-result)
      ((and (tool-call-message-p msg)
            (plusp (length trimmed))
            (char= (char trimmed 0) #\())
       :tool-call)
      (t nil))))

(defun tool-line-face-name (base-face-name category)
  "Map BASE-FACE-NAME and CATEGORY to a concrete global face keyword."
  (ecase base-face-name
    (:tool-call
     (ecase category
       (:base :tool-call)
       (:paren :tool-call-paren)
       (:keyword :tool-call-keyword)
       (:string :tool-call-string)
       (:comment :tool-call-comment)
       (:number :tool-call-number)))
    (:tool-result
     (ecase category
       (:base :tool-result)
       (:paren :tool-result-paren)
       (:keyword :tool-result-keyword)
       (:string :tool-result-string)
       (:comment :tool-result-comment)
       (:number :tool-result-number)))))

(defun lisp-token-face-category (token)
  "Return the syntax face category for TOKEN."
  (cond
    ((zerop (length token)) :base)
    ((char= (char token 0) #\:) :keyword)
    ((ignore-errors
       (multiple-value-bind (value pos)
           (read-from-string token nil nil)
         (and (= pos (length token))
              (numberp value))))
     :number)
    (t :base)))

(defun lisp-line-face-vector (text base-face-name)
  "Return a per-character face-name vector for TEXT using BASE-FACE-NAME."
  (let* ((len (length text))
         (base-face (tool-line-face-name base-face-name :base))
         (paren-face (tool-line-face-name base-face-name :paren))
         (keyword-face (tool-line-face-name base-face-name :keyword))
         (string-face (tool-line-face-name base-face-name :string))
         (comment-face (tool-line-face-name base-face-name :comment))
         (number-face (tool-line-face-name base-face-name :number))
         (faces (make-array len :initial-element base-face)))
    (labels ((assign-range (start end face-name)
               (loop :for idx :from start :below end
                     :do (setf (aref faces idx) face-name)))
             (delimiter-char-p (char)
               (or (member char '(#\Space #\Tab #\( #\) #\[ #\] #\" #\; #\' #\` #\,)
                           :test #'char=)
                   (char= char #\Newline))))
      (loop :for idx := 0 :then next-idx
            :while (< idx len)
            :for char := (char text idx)
            :for next-idx :=
              (cond
                ((char= char #\;)
                 (assign-range idx len comment-face)
                 len)
                ((char= char #\")
                 (let ((scan (1+ idx)))
                   (loop :while (< scan len)
                         :for cur := (char text scan)
                         :do (if (char= cur #\\)
                                 (incf scan 2)
                                 (progn
                                   (incf scan)
                                   (when (char= cur #\")
                                     (return)))))
                   (assign-range idx (min scan len) string-face)
                   (min scan len)))
                ((member char '(#\( #\) #\[ #\] #\' #\` #\,) :test #'char=)
                 (setf (aref faces idx) paren-face)
                 (1+ idx))
                ((or (char= char #\Space) (char= char #\Tab))
                 (1+ idx))
                (t
                 (let ((end idx))
                   (loop :while (and (< end len)
                                     (not (delimiter-char-p (char text end))))
                         :do (incf end))
                   (let* ((token (subseq text idx end))
                          (face-name
                            (case (lisp-token-face-category token)
                              (:keyword keyword-face)
                              (:number number-face)
                              (t base-face))))
                     (assign-range idx end face-name))
                   end))))
      faces)))

(defun tool-line-display-spans (text base-face-name &key (start 0) end prefix)
  "Return contiguous face runs for TEXT between START and END.
When PREFIX is provided, prepend it using the base face for BASE-FACE-NAME."
  (let* ((limit (or end (length text)))
         (base-face (tool-line-face-name base-face-name :base))
         (faces (lisp-line-face-vector text base-face-name))
         (spans nil))
    (when prefix
      (push (cons base-face prefix) spans))
    (when (< start limit)
      (let ((span-start start)
            (span-face (aref faces start)))
        (loop :for idx :from start :below limit
              :for face-name := (aref faces idx)
              :do (unless (eq face-name span-face)
                    (push (cons span-face (subseq text span-start idx)) spans)
                    (setf span-start idx
                          span-face face-name)))
        (push (cons span-face (subseq text span-start limit)) spans)))
    (nreverse spans)))

;;; --------------------------------------------------------------------------
;;; Buffer Geometry (pure functions)
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

;;; --------------------------------------------------------------------------
;;; String Utilities
;;; --------------------------------------------------------------------------

(defun split-string-by-newline (str)
  "Split STR by newlines into a list of strings."
  (loop :for start := 0 :then (1+ pos)
        :for pos := (position #\Newline str :start start)
        :collect (subseq str start (or pos (length str)))
        :while pos))

;;; --------------------------------------------------------------------------
;;; Selector Formatting (pure string functions)
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

(defun format-model-selector-line (marker provider model width)
  "Format a single line for the model selector with aligned columns.
MARKER is a 2-char prefix (e.g. \"*\" or \"  \").
Adapts column widths to the terminal WIDTH."
  (let* ((provider-width (max 10 (min 20 (floor width 4))))
         (model-width (max 15 (- width (length marker) provider-width 4)))
         (line (format nil "~A~VA  ~A"
                       marker
                       provider-width provider
                       model)))
    (if (<= (length line) width)
        (concatenate 'string line
                     (make-string (- width (length line)) :initial-element #\Space))
        (subseq line 0 width))))

(defun format-think-selector-line (marker label width)
  "Format a single line for the think-level selector.
LABEL is typically \"default\", \"low\", or another reasoning-effort value."
  (let ((line (format nil "~A~A" marker label)))
    (if (<= (length line) width)
        (concatenate 'string line
                     (make-string (- width (length line)) :initial-element #\Space))
        (subseq line 0 width))))

;;; --------------------------------------------------------------------------
;;; Minibuffer State Query (pure function)
;;; --------------------------------------------------------------------------

(defun minibuffer-current-height ()
  "Return the current height of the minibuffer in rows.
When inactive: 1 row. When active: prompt line + number of filtered items,
capped at *MINIBUFFER-MAX-HEIGHT*. Automatic skill completion uses the same
bottom pane area, capped by *SKILL-COMPLETION-MAX-HEIGHT*."
  (cond
    (*minibuffer-active*
     (let ((item-count (length *minibuffer-filtered-items*)))
       (min *minibuffer-max-height*
            (1+ item-count))))
    (*skill-completion-active*
     (let ((item-count (length *skill-completion-filtered-items*)))
       (min *skill-completion-max-height*
            (1+ (max 1 item-count)))))
    (t 1)))
