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
          (make-instance 'drawing-style :name :modeline
            :background-ink (make-clim-ink :cga 7)
            :ink (make-clim-ink :cga 0)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    ;; System messages (e.g. shell prefix output, notifications)
    (setf (gethash :system r)
          (make-instance 'drawing-style :name :system
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 6)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Compaction summaries are model-visible handoff context.
    (setf (gethash :compaction-summary r)
          (make-instance 'drawing-style :name :compaction-summary
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 3)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Debug messages — API request/response log when *debug-mode* is t.
    ;; Bright magenta distinguishes debug output from regular system messages.
    (setf (gethash :debug r)
          (make-instance 'drawing-style :name :debug
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 13)   ; bright magenta
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Minibuffer faces
    (setf (gethash :minibuffer-prompt r)
          (make-instance 'drawing-style :name :minibuffer-prompt
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 15)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :minibuffer-cursor r)
          (make-instance 'drawing-style :name :minibuffer-cursor
            :background-ink (make-clim-ink :cga 15)
            :ink (make-clim-ink :cga 0)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    (setf (gethash :minibuffer-candidate r)
          (make-instance 'drawing-style :name :minibuffer-candidate
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 7)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :minibuffer-selected r)
          (make-instance 'drawing-style :name :minibuffer-selected
            :background-ink (make-clim-ink :cga 15)
            :ink (make-clim-ink :cga 0)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    ;; Fuzzy-match highlight faces — matched characters are rendered in bright
    ;; yellow so they stand out against both dark and light backgrounds.
    (setf (gethash :minibuffer-match r)
          (make-instance 'drawing-style :name :minibuffer-match
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 11)   ; bright yellow on black
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    (setf (gethash :minibuffer-selected-match r)
          (make-instance 'drawing-style :name :minibuffer-selected-match
            :background-ink (make-clim-ink :cga 15)
            :ink (make-clim-ink :cga 3)    ; dark yellow on white
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    ;; Selector overlay faces (buffer selector, model selector)
    (setf (gethash :selector-title r)
          (make-instance 'drawing-style :name :selector-title
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 14)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    (setf (gethash :selector-separator r)
          (make-instance 'drawing-style :name :selector-separator
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 7)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :selector-header r)
          (make-instance 'drawing-style :name :selector-header
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 11)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    (setf (gethash :selector-entry r)
          (make-instance 'drawing-style :name :selector-entry
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 7)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :selector-selected r)
          (make-instance 'drawing-style :name :selector-selected
            :background-ink (make-clim-ink :cga 6)
            :ink (make-clim-ink :cga 0)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    (setf (gethash :selector-footer r)
          (make-instance 'drawing-style :name :selector-footer
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 2)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :selector-scroll r)
          (make-instance 'drawing-style :name :selector-scroll
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 11)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Approval prompt faces
    (setf (gethash :approval-header r)
          (make-instance 'drawing-style :name :approval-header
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 11)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    (setf (gethash :approval-code r)
          (make-instance 'drawing-style :name :approval-code
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 14)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :approval-text r)
          (make-instance 'drawing-style :name :approval-text
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 15)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :approval-diff-add r)
          (make-instance 'drawing-style :name :approval-diff-add
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 2)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :approval-diff-remove r)
          (make-instance 'drawing-style :name :approval-diff-remove
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 1)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :approval-options r)
          (make-instance 'drawing-style :name :approval-options
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 2)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    ;; Tool call faces
    (setf (gethash :tool-call r)
          (make-instance 'drawing-style :name :tool-call
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 14)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-call-paren r)
          (make-instance 'drawing-style :name :tool-call-paren
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 12)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    (setf (gethash :tool-call-keyword r)
          (make-instance 'drawing-style :name :tool-call-keyword
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 11)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    (setf (gethash :tool-call-string r)
          (make-instance 'drawing-style :name :tool-call-string
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 10)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-call-comment r)
          (make-instance 'drawing-style :name :tool-call-comment
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 6)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-call-number r)
          (make-instance 'drawing-style :name :tool-call-number
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 13)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Tool result faces
    (setf (gethash :tool-result r)
          (make-instance 'drawing-style :name :tool-result
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 10)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-result-paren r)
          (make-instance 'drawing-style :name :tool-result-paren
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 12)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    (setf (gethash :tool-result-keyword r)
          (make-instance 'drawing-style :name :tool-result-keyword
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 11)
            :text-style (bold-clawmacs-text-style) :underline-p nil :reverse-p nil))
    (setf (gethash :tool-result-string r)
          (make-instance 'drawing-style :name :tool-result-string
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 14)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-result-comment r)
          (make-instance 'drawing-style :name :tool-result-comment
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 6)
            :bold-p nil :underline-p nil :reverse-p nil))
    (setf (gethash :tool-result-number r)
          (make-instance 'drawing-style :name :tool-result-number
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 13)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Default text face — fallback for messages without a face set.
    ;; This should NEVER be the modeline face; it should be a sensible
    ;; text-on-dark-background face for generic content.
    (setf (gethash :default-text r)
          (make-instance 'drawing-style :name :default-text
            :background-ink (make-clim-ink :cga 0)
            :ink (make-clim-ink :cga 7)
            :bold-p nil :underline-p nil :reverse-p nil))
    ;; Who-line face — light gray bg, black fg (McCLIM only, Genera-style)
    (setf (gethash :who-line r)
          (make-instance 'drawing-style :name :who-line
            :background-ink (make-clim-ink :hex "#EDEDED")
            :ink (make-clim-ink :cga 0)
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
Background: light blue, ink: black."
  (make-face-set
   :user
   (list (make-instance 'drawing-style
           :name :default
           :background-ink (make-hex-ink "#D0D8E8")
           :ink (make-cga-ink 0)
           :bold-p nil
           :underline-p nil
           :reverse-p nil))))

(defun make-default-agent-face-set (agent-keyword)
  "Create the default face set for an agent.
Background: white, ink: black."
  (make-face-set
   agent-keyword
   (list (make-instance 'drawing-style
           :name :default
           :background-ink (make-cga-ink 15)
           :ink (make-cga-ink 0)
           :bold-p nil
           :underline-p nil
           :reverse-p nil))))

(defun make-default-system-face-set ()
  "Create the default face set for system messages.
Uses the global :system drawing style if available."
  (let ((sys-face (or (global-face :system)
                      (make-instance 'drawing-style :name :default
                        :background-ink (make-cga-ink 15)
                        :ink (make-cga-ink 6)
                        :bold-p nil :underline-p nil :reverse-p nil))))
    (make-face-set
     :system
     (list (make-instance 'drawing-style
             :name :default
             :parent sys-face
             :background-ink nil :ink nil
             :bold-p nil :underline-p nil :reverse-p nil)))))

(defun make-default-compaction-summary-face-set ()
  "Create the default face set for compaction summary messages."
  (let ((summary-face (or (global-face :compaction-summary)
                          (make-instance 'drawing-style :name :default
                            :background-ink (make-cga-ink 15)
                            :ink (make-cga-ink 3)
                            :bold-p nil :underline-p nil :reverse-p nil))))
    (make-face-set
     :compaction-summary
     (list (make-instance 'drawing-style
             :name :default
             :parent summary-face
             :background-ink nil :ink nil
             :bold-p nil :underline-p nil :reverse-p nil)))))

(defun ensure-buffer-agent-face-set
    (buf &optional (agent-name (buffer-agent-name buf)))
  "Ensure BUF has a face set for AGENT-NAME and return it."
  (let* ((registry (buffer-face-registry buf))
         (agent-kw (intern (string-upcase agent-name) :keyword)))
    (or (gethash agent-kw registry)
        (setf (gethash agent-kw registry)
              (make-default-agent-face-set agent-kw)))))

(defun init-face-registry (buf)
  "Populate BUF's face registry with default user, agent, and system faces."
  (let* ((registry (buffer-face-registry buf))
         (user-fs (make-default-user-face-set))
         (system-fs (make-default-system-face-set))
         (summary-fs (make-default-compaction-summary-face-set)))
    (setf (gethash :user registry) user-fs
          (gethash :system registry) system-fs
          (gethash :compaction-summary registry) summary-fs)
    (ensure-buffer-agent-face-set buf)
    (setf (message-face-set (buffer-input-message buf)) user-fs)
    buf))

(defun make-modeline-face ()
  "Return the modeline face from the global registry, or create a default.
Prefers the stored global face for customizability."
  (or (global-face :modeline)
      (make-instance 'drawing-style
        :name :modeline
        :background-ink (make-clim-ink :cga 7)
        :ink (make-clim-ink :cga 0)
        :text-style (bold-clawmacs-text-style)
        :underline-p nil
        :reverse-p nil)))

(defun make-default-text-face ()
  "Return the default text face from the global registry, or create a default.
This is the fallback face for messages that lack a face set — white text on
black background. This must NOT be the modeline face."
  (or (global-face :default-text)
      (make-instance 'drawing-style
        :name :default-text
        :background-ink (make-cga-ink 15)
        :ink (make-cga-ink 0)
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
         (input (and buf (buffer-input-message buf)))
         (position (and input
                        (document-buffer-p buf)
                        (format nil "L~D:C~D~@[ Mark~]"
                                (message-current-line-number input)
                                (message-current-column-number input)
                                (message-mark-active-p input))))
         (left (format nil " [~A~A] ~A | ~A | ~A | ~A"
                       major-mode
                       dirty-marker
                       (buffer-name buf)
                       (buffer-agent-name buf)
                       pm
                       (namestring (buffer-working-directory buf))))
         (position-prefix (if position
                              (format nil "~A | " position)
                              ""))
         (right (format nil "~A~A/~A | ~A "
                        position-prefix
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
;;; Who-Line Formatting (pure string functions, Lisp-machine style)
;;; --------------------------------------------------------------------------

(defun who-line-clock-string (&optional (time (get-universal-time)))
  "Return TIME formatted for the who-line."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~2,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            month date (mod year 100) hour minute second)))

(defun who-line-machine-identity ()
  "Return the user@host identity string for the who-line."
  (format nil "~A@~A"
          (or (uiop:getenv "USER") "user")
          (machine-instance)))

(defun who-line-memory-summary ()
  "Return a compact implementation-specific memory summary."
  #+sbcl
  (let* ((pkg (find-package :sb-kernel))
         (sym (and pkg (find-symbol "DYNAMIC-USAGE" pkg)))
         (usage (and sym (fboundp sym)
                     (ignore-errors (funcall sym)))))
    (if usage
        (format nil "~D MB live"
                (round usage 1048576))
        (format nil "~D MB consed"
                (round (sb-ext:get-bytes-consed) 1048576))))
  #-sbcl
  "memory n/a")

(defun who-line-abbreviate-directory (pathname)
  "Return PATHNAME as a compact namestring for who-line display."
  (let* ((namestring (namestring (uiop:ensure-directory-pathname pathname)))
         (home (namestring (uiop:ensure-directory-pathname
                            (user-homedir-pathname)))))
    (if (and (>= (length namestring) (length home))
             (string= home namestring :end1 (length home) :end2 (length home)))
        (concatenate 'string "~/" (subseq namestring (length home)))
        namestring)))

(defun who-line-status-string (buf)
  "Return a concise status tag for BUF and current modal state."
  (cond
    (*openai-oauth-pending* "oauth")
    (*minibuffer-active* "minibuffer")
    (*buffer-selector-active* "buffers")
    (*model-selector-active* "models")
    (*think-selector-active* "think")
    (*deny-message-mode* "deny")
    ((and buf (buffer-approval-pending buf))
     (let ((tool-name (or (cdr (assoc :tool-name
                                      (buffer-approval-pending buf)))
                          "")))
       (if (blank-string-p tool-name)
           "approval"
           (format nil "approval:~A" tool-name))))
    ((and buf (buffer-user-input-pending buf))
     "question")
    ((and buf (buffer-llm-running-p buf))
     "running")
    ((and buf (buffer-status buf) (not (eq (buffer-status buf) :idle)))
     (string-downcase (symbol-name (buffer-status buf))))
    (t
     "ready")))

(defun who-line-field-data (buf)
  "Return ordered who-line field plists for BUF."
  (let ((fields nil))
    (labels ((add-field (id text &key object presentation-type)
               (unless (blank-string-p text)
                 (push (list :id id
                             :text text
                             :object object
                             :presentation-type presentation-type)
                       fields))))
      (add-field :clock (who-line-clock-string))
      (add-field :identity (who-line-machine-identity))
      (when buf
        (add-field :buffer
                   (format nil "buf:~A" (buffer-name buf))
                   :object buf
                   :presentation-type 'buffer-ref)
        (add-field :mode
                   (format nil "mode:~A" (buffer-major-mode buf)))
        (when (and (eq (buffer-kind buf) :chat)
                   (not (blank-string-p (buffer-agent-name buf))))
          (add-field :agent
                     (format nil "agent:~A" (buffer-agent-name buf))))
        (when (buffer-pipeline-name buf)
          (add-field :pipeline
                     (format nil "pipe:~A" (buffer-pipeline-name buf))))
        (when (buffer-working-directory buf)
          (add-field :directory
                     (format nil "dir:~A"
                             (who-line-abbreviate-directory
                              (buffer-working-directory buf)))
                     :object (buffer-working-directory buf)
                     :presentation-type 'pathname)))
      (add-field :state
                 (format nil "state:~A" (who-line-status-string buf)))
      (add-field :memory
                 (who-line-memory-summary)))
    (nreverse fields)))

(defun format-who-line (buf width)
  "Return two strings for the who-line.

The first value is the single visible status row.  The second value is blank
for compatibility with older callers and snapshots that still expect two
values."
  (flet ((pad (str)
           (if (>= (length str) width)
               (subseq str 0 width)
               (concatenate 'string str
                            (make-string (- width (length str))
                                         :initial-element #\Space)))))
    (let* ((texts (mapcar (lambda (field) (getf field :text))
                          (who-line-field-data buf)))
           (row1 (format nil "~{~A~^  ~}" texts)))
      (values (pad row1)
              (pad "")))))

;;; --------------------------------------------------------------------------
;;; Line Wrapping Helpers (pure geometry)
;;; --------------------------------------------------------------------------

(defun wrapped-line-count (content display-width)
  "Return the number of visual rows needed to display CONTENT within DISPLAY-WIDTH.
An empty string takes 1 row. A string exactly DISPLAY-WIDTH chars takes 1 row."
  (if (or (zerop (length content)) (<= (length content) display-width))
      1
      (ceiling (length content) display-width)))

(defun message-line-entries (msg)
  "Return display line entries for MSG as (TEXT . SOURCE-LINE)."
  (loop :for line := (message-first-line msg) :then (line-next line)
        :while line
        :collect (cons (line-content line) line)))

(defstruct (display-image-reference
            (:constructor make-display-image-reference
                (&key path alt raw-text source-line)))
  "Display-only reference to a local image embedded in message text."
  (path "" :type string :read-only t)
  (alt "" :type string :read-only t)
  (raw-text "" :type string :read-only t)
  (source-line nil :type (or null line) :read-only t))

(defun trim-display-image-path (path)
  "Normalize a Markdown image PATH for local display."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) path)))
    (if (alexandria:starts-with-subseq "file://" trimmed)
        (subseq trimmed 7)
        trimmed)))

(defun parse-display-image-line (text &key source-line)
  "Parse TEXT as a standalone Markdown image line.
Returns a DISPLAY-IMAGE-REFERENCE or NIL.  The supported form is:

  ![alt text](path/to/image.png)

The renderer intentionally treats image references as display-only; the
underlying message text remains ordinary Markdown for provider round-tripping."
  (let* ((trimmed (string-trim '(#\Space #\Tab) text))
         (len (length trimmed)))
    (when (and (>= len 5)
               (alexandria:starts-with-subseq "![" trimmed)
               (char= (char trimmed (1- len)) #\)))
      (let ((separator (search "](" trimmed :start2 2)))
        (when separator
          (let* ((alt (subseq trimmed 2 separator))
                 (path-start (+ separator 2))
                 (path-end (1- len))
                 (path (and (< path-start path-end)
                            (trim-display-image-path
                             (subseq trimmed path-start path-end)))))
            (when (and path (plusp (length path)))
              (make-display-image-reference
               :path path
               :alt alt
               :raw-text text
               :source-line source-line))))))))

(defun message-display-blocks (msg &key show-reasoning-p show-metadata-p)
  "Return display blocks for MSG.
Each block is a plist.  Text blocks have :TYPE :TEXT, :TEXT, and
:SOURCE-LINE.  Image blocks have :TYPE :IMAGE and :REFERENCE."
  (loop :for entry :in (message-display-line-entries
                        msg
                        :show-reasoning-p show-reasoning-p
                        :show-metadata-p show-metadata-p)
        :for text := (car entry)
        :for line := (cdr entry)
        :for image := (parse-display-image-line text :source-line line)
        :collect (if image
                     (list :type :image :reference image)
                     (list :type :text :text text :source-line line))))

(defun message-has-content-block-type-p (msg block-type)
  "Return non-nil when MSG has a raw content block of BLOCK-TYPE."
  (and (message-raw-content msg)
       (some (lambda (block)
               (string= block-type (or (cdr (assoc :type block)) "")))
             (message-raw-content msg))))

(defun message-reasoning-blocks (msg)
  "Return provider-supplied reasoning text blocks stored on MSG."
  (when (message-raw-content msg)
    (content-reasoning-blocks (message-raw-content msg))))

(defun display-text-from-line-strings (lines)
  "Join LINES into a newline-delimited display string."
  (with-output-to-string (stream)
    (loop :for line :in lines
          :for first := t :then nil
          :do (unless first (write-char #\Newline stream))
              (write-string line stream))))

(defun reasoning-block-display-lines (reasoning-blocks &key visible-text)
  "Return display lines for provider reasoning blocks.
VISIBLE-TEXT is used to avoid showing the same provider fallback text twice."
  (let ((visible (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (or visible-text "")))
        (lines nil))
    (dolist (reasoning reasoning-blocks)
      (let ((text (or reasoning "")))
        (unless (or (blank-string-p text)
                    (string= visible
                             (string-trim '(#\Space #\Tab #\Newline #\Return)
                                          text)))
          (push ";; reasoning" lines)
          (dolist (line (split-string-by-newline text))
            (push line lines)))))
    (nreverse lines)))

(defun message-reasoning-summary-requested-p (msg)
  "Return non-nil when MSG metadata says a reasoning summary was requested."
  (let ((metadata (message-metadata msg)))
    (and metadata
         (message-metadata-value metadata :reasoning-summary-mode))))

(defun message-reasoning-display-lines (msg &key visible-text)
  "Return reasoning sidecar lines for MSG, including an unavailable marker."
  (let ((blocks (message-reasoning-blocks msg)))
    (if blocks
        (reasoning-block-display-lines blocks :visible-text visible-text)
        (when (message-reasoning-summary-requested-p msg)
          (list ";; reasoning"
                ";; no provider-supplied reasoning blocks captured")))))

(defun message-metadata-display-lines (metadata)
  "Return display lines for provider/response METADATA."
  (when metadata
    (let ((agent (message-metadata-value metadata :agent))
          (provider (message-metadata-value metadata :provider))
          (model (message-metadata-value metadata :model))
          (think-level (message-metadata-value metadata :think-level))
          (reasoning-summary-mode
            (message-metadata-value metadata :reasoning-summary-mode))
          (stop-reason (message-metadata-value metadata :stop-reason))
          (content-block-count
            (message-metadata-value metadata :content-block-count))
          (tool-call-count
            (message-metadata-value metadata :tool-call-count))
          (reasoning-block-count
            (message-metadata-value metadata :reasoning-block-count))
          (usage-line
            (format-token-usage-summary
             (token-usage-from-metadata metadata))))
      (remove nil
              (list ";; metadata"
                    (and agent (format nil ";; agent: ~A" agent))
                    (and provider model
                         (format nil ";; provider/model: ~(~A~)/~A"
                                 provider model))
                    (format nil ";; think: ~A" (or think-level "default"))
                    (and reasoning-summary-mode
                         (format nil ";; reasoning-summary: ~A"
                                 reasoning-summary-mode))
                    (and stop-reason
                         (format nil ";; stop-reason: ~A" stop-reason))
                    (and content-block-count
                         (format nil ";; content-blocks: ~D"
                                 content-block-count))
                    (and tool-call-count
                         (format nil ";; tool-calls: ~D" tool-call-count))
                    (and reasoning-block-count
                         (format nil ";; reasoning-blocks: ~D"
                                 reasoning-block-count))
                    (and usage-line
                         (format nil ";; ~A" usage-line)))))))

(defun message-display-line-entries (msg &key show-reasoning-p show-metadata-p)
  "Return display line entries for MSG, including optional sidecar output."
  (let* ((entries (message-line-entries msg))
         (visible-text (display-text-from-line-strings (mapcar #'car entries)))
         (reasoning-lines
           (and show-reasoning-p
                (message-reasoning-display-lines
                 msg
                 :visible-text visible-text)))
         (metadata-lines
           (and show-metadata-p
                (message-metadata-display-lines (message-metadata msg))))
         (sidecar-lines (append reasoning-lines metadata-lines)))
    (if sidecar-lines
        (append (if (and (= (length entries) 1)
                         (blank-string-p (caar entries)))
                    nil
                    entries)
                (mapcar (lambda (line) (cons line nil)) sidecar-lines))
        entries)))

(defun message-display-line-strings (msg &key show-reasoning-p show-metadata-p)
  "Return rendered line strings for MSG."
  (mapcar #'car
          (message-display-line-entries msg
                                        :show-reasoning-p show-reasoning-p
                                        :show-metadata-p show-metadata-p)))

(defun message-hidden-by-tool-results-toggle-p (msg)
  "Return non-nil when MSG should disappear while tool output is hidden."
  (or (eq :tool-result (message-sender msg))
      (and (message-raw-content msg)
           (not (eq :user (message-sender msg)))
           (message-has-content-block-type-p msg "tool_use")
           (not (message-has-content-block-type-p msg "text"))
           (not (message-has-content-block-type-p msg "reasoning")))))

(defun message-hidden-by-reasoning-toggle-p (msg)
  "Return non-nil when MSG only contains hidden reasoning output."
  (and (message-raw-content msg)
       (not (eq :user (message-sender msg)))
       (message-has-content-block-type-p msg "reasoning")
       (not (message-has-content-block-type-p msg "text"))
       (not (message-has-content-block-type-p msg "tool_use"))))

(defun message-visible-for-buffer-p (msg buf)
  "Return non-nil when MSG should be rendered for BUF's display toggles."
  (and (or (buffer-show-tool-results-p buf)
           (not (message-hidden-by-tool-results-toggle-p msg)))
       (or (buffer-show-reasoning-p buf)
           (not (message-hidden-by-reasoning-toggle-p msg)))))

(defun message-visual-height (msg width &key (prefix (message-sender-prefix msg))
                                             show-reasoning-p
                                             show-metadata-p)
  "Return the total visual rows MSG needs at the given terminal WIDTH.
Accounts for the sender prefix and line wrapping."
  (let* ((prefix-len (length prefix))
         (display-width (max 1 (- width prefix-len))))
    (loop :for line :in (message-display-line-strings
                         msg
                         :show-reasoning-p show-reasoning-p
                         :show-metadata-p show-metadata-p)
          :sum (wrapped-line-count line display-width))))

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

(defun calculate-input-height (buf terminal-height width
                               &key
                                 (prefix
                                   (message-sender-prefix
                                    (buffer-input-message buf))))
  "Calculate the visual height of the input area in rows.
Minimum 3, maximum (floor terminal-height 2). Accounts for line wrapping.
During approval prompts, allows up to 2/3 of terminal height."
  (let* ((input (buffer-input-message buf))
         (visual-height (message-visual-height input width :prefix prefix))
         (min-height 3)
         (approval-active (buffer-approval-pending buf))
         (max-height (if approval-active
                         (floor (* terminal-height 2) 3)
                         (max min-height
                              (floor terminal-height 2)))))
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

(defun format-session-tree-selector-line (marker item width)
  "Format a single session tree selector ITEM."
  (let* ((prefix (or (getf item :tree-prefix) ""))
         (active (if (getf item :active-p) "*" " "))
         (kind (or (getf item :kind-label) "entry"))
         (label (getf item :label))
         (label-part (if label
                         (format nil " [~A]" label)
                         ""))
         (fold-part (cond
                      ((getf item :folded-p) "+ ")
                      ((plusp (or (getf item :children-count) 0))
                       "- ")
                      (t "  ")))
         (line (format nil "~A~A ~A~A~A ~A ~A"
                       marker
                       active
                       prefix
                       fold-part
                       kind
                       label-part
                       (or (getf item :content) ""))))
    (if (<= (length line) width)
        (concatenate 'string line
                     (make-string (- width (length line))
                                  :initial-element #\Space))
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
