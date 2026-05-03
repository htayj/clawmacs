(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; McCLIM Application — Genera-Style Interface
;;;
;;; The interactive McCLIM application frame.  The frame follows the
;;; McCLIM/ESA shape used by Climacs-style applications: an application
;;; frame owns panes and command tables, ESA supplies frame/minibuffer command
;;; infrastructure, Drei backs editable text panes, and Clawmacs core state
;;; remains outside the UI layer.
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; Genera Theme — McCLIM-only white background override
;;; --------------------------------------------------------------------------

(defvar *mcclim-bg-ink* (clim:make-rgb-color 1.0 1.0 1.0)
  "Default background ink for the McCLIM app. White for Genera theme.")

(defun set-drawing-style-values
    (style &key ink background-ink text-style drawing-options underline-p)
  "Update STYLE with CLIM drawing values, leaving omitted slots unchanged."
  (when ink
    (setf (drawing-style-ink style) ink))
  (when background-ink
    (setf (drawing-style-background-ink style) background-ink))
  (when text-style
    (setf (drawing-style-text-style style) text-style))
  (when drawing-options
    (setf (drawing-style-drawing-options style) drawing-options))
  (when (not (null underline-p))
    (setf (drawing-style-underline-p style) underline-p))
  style)

(defun set-global-drawing-style-values
    (name &key ink background-ink text-style drawing-options underline-p)
  "Update global drawing style NAME with CLIM drawing values."
  (let ((style (global-face name)))
    (when style
      (set-drawing-style-values style
                                :ink ink
                                :background-ink background-ink
                                :text-style text-style
                                :drawing-options drawing-options
                                :underline-p underline-p))))

(defun mcclim-apply-genera-theme ()
  "Patch global drawing styles for Genera-style white background.
Called once before the McCLIM application frame is created."
  (let ((white-bg (make-cga-ink 15))
        (black-fg (make-cga-ink 0))
        (light-blue-bg (make-hex-ink "#D0E0F0"))
        (user-bg (make-hex-ink "#D0D8E8"))
        (dark-blue-fg (make-cga-ink 4))
        (dark-green-fg (make-cga-ink 2))
        (dark-red-fg (make-cga-ink 1))
        (dark-yellow-fg (make-cga-ink 3))
        (dark-magenta-fg (make-cga-ink 5))
        (dark-cyan-fg (make-cga-ink 6))
        (gray-fg (make-cga-ink 8))
        (bold (bold-clawmacs-text-style))
        (roman (default-clawmacs-text-style)))
    (maphash (lambda (_name style)
               (declare (ignore _name))
               (set-drawing-style-values style
                                         :background-ink white-bg
                                         :ink black-fg
                                         :text-style roman))
             *global-face-registry*)
    (set-global-drawing-style-values :modeline
                                     :background-ink (make-cga-ink 7)
                                     :ink black-fg
                                     :text-style bold)
    (set-global-drawing-style-values :selector-selected
                                     :background-ink light-blue-bg
                                     :ink black-fg
                                     :text-style bold)
    (set-global-drawing-style-values :minibuffer-selected
                                     :background-ink light-blue-bg
                                     :ink black-fg
                                     :text-style bold)
    (set-global-drawing-style-values :minibuffer-match
                                     :background-ink white-bg
                                     :ink dark-blue-fg
                                     :text-style bold)
    (set-global-drawing-style-values :minibuffer-selected-match
                                     :background-ink light-blue-bg
                                     :ink dark-blue-fg
                                     :text-style bold)
    (set-global-drawing-style-values :approval-diff-add
                                     :background-ink white-bg
                                     :ink dark-green-fg)
    (set-global-drawing-style-values :approval-diff-remove
                                     :background-ink white-bg
                                     :ink dark-red-fg)
    (dolist (spec `((:tool-call ,white-bg ,dark-blue-fg ,bold)
                    (:tool-call-paren ,white-bg ,dark-blue-fg ,bold)
                    (:tool-call-keyword ,white-bg ,dark-red-fg ,bold)
                    (:tool-call-string ,white-bg ,dark-green-fg ,roman)
                    (:tool-call-comment ,white-bg ,dark-cyan-fg ,roman)
                    (:tool-call-number ,white-bg ,dark-magenta-fg ,roman)
                    (:tool-result ,white-bg ,dark-green-fg ,bold)
                    (:tool-result-paren ,white-bg ,dark-blue-fg ,bold)
                    (:tool-result-keyword ,white-bg ,dark-yellow-fg ,bold)
                    (:tool-result-string ,white-bg ,dark-cyan-fg ,roman)
                    (:tool-result-comment ,white-bg ,gray-fg ,roman)
                    (:tool-result-number ,white-bg ,dark-magenta-fg ,roman)))
      (set-global-drawing-style-values (first spec)
                                       :background-ink (second spec)
                                       :ink (third spec)
                                       :text-style (fourth spec)))
    ;; Patch per-buffer style sets on existing buffers.
    (let ((agent-bg white-bg))
      (dolist (buf *buffer-ring*)
        (maphash (lambda (sender-kw fs)
                   (let ((default-style (get-face fs :default)))
                     (when default-style
                       (if (eq sender-kw :user)
                           (set-drawing-style-values default-style
                                                     :background-ink user-bg
                                                     :ink black-fg
                                                     :text-style roman)
                           (set-drawing-style-values default-style
                                                     :background-ink agent-bg
                                                     :ink black-fg
                                                     :text-style roman)))))
                 (buffer-face-registry buf))))))

;;; --------------------------------------------------------------------------
;;; Drawing Style Resolution
;;; --------------------------------------------------------------------------

(defun resolve-face-inks (resolved-face)
  "Return CLIM drawing parameters from RESOLVED-FACE.
Values are ink, background-ink, text-style, drawing-options, and underline-p."
  (values (resolved-face-foreground resolved-face)
          (resolved-face-background resolved-face)
          (resolved-face-text-style resolved-face)
          (resolved-face-drawing-options resolved-face)
          (resolved-face-underline-p resolved-face)))

(defun resolve-global-face-inks (face-name)
  "Resolve a global drawing style by keyword NAME to CLIM drawing parameters."
  (let ((style (global-face face-name)))
    (if style
        (resolve-face-inks (resolve-face style))
        (let* ((ink (make-cga-ink 0))
               (background-ink *mcclim-bg-ink*)
               (text-style (default-clawmacs-text-style))
               (drawing-options (list :ink ink :text-style text-style)))
          (values ink background-ink text-style drawing-options nil)))))

;;; --------------------------------------------------------------------------
;;; Presentation Types — semantic mouse interaction
;;;
;;; CLIM presentation types make rendered objects clickable. Each type
;;; defines what kind of object is displayed, and translators define
;;; what actions are available when the user interacts with them.
;;; --------------------------------------------------------------------------

(clim:define-presentation-type rendered-message-ref ()
  :description "a rendered chat transcript message")

(clim:define-presentation-type chat-message ()
  :inherit-from 'rendered-message-ref
  :description "a chat message")

(clim:define-presentation-type tool-call ()
  :inherit-from 'rendered-message-ref
  :description "a tool call")

(clim:define-presentation-type tool-result ()
  :inherit-from 'rendered-message-ref
  :description "a tool result")


(clim:define-presentation-method clim:present
    (msg (type rendered-message-ref) stream (view clim:textual-view) &key)
  (mcclim-stream-render-message stream msg 100 1 1))
(clim:define-presentation-type image-reference ()
  :description "an inline image reference")

(clim:define-presentation-type approval-action ()
  :description "an approval prompt action")

(clim:define-presentation-type minibuffer-candidate-ref ()
  :description "a minibuffer completion candidate")

(clim:define-presentation-type skill-candidate-ref ()
  :description "a skill completion candidate")

(clim:define-presentation-type slash-candidate-ref ()
  :description "a slash completion candidate")

(clim:define-presentation-type buffer-ref ()
  :description "a buffer reference")

(clim:define-presentation-type package-dashboard-entry-ref ()
  :description "an installed package entry")

(clim:define-presentation-type clawmacs-window-ref ()
  :description "a Clawmacs logical window")

(clim:define-presentation-type selector-entry-ref ()
  :description "a selector entry")

(clim:define-presentation-type model-ref ()
  :inherit-from 'selector-entry-ref
  :description "a model reference")

(clim:define-presentation-type think-level-ref ()
  :inherit-from 'selector-entry-ref
  :description "a think-level reference")

(clim:define-presentation-type session-tree-entry-ref ()
  :inherit-from 'selector-entry-ref
  :description "a session tree entry")

(clim:define-presentation-type help-line-ref ()
  :description "a line in a help buffer")

(clim:define-presentation-type info-link-ref ()
  :description "an Info manual link")

(clim:define-presentation-type customize-field-ref ()
  :description "a customize buffer field")

(clim:define-presentation-type listener-entry-ref ()
  :description "a Common Lisp listener transcript entry")

(clim:define-presentation-type font-editor-glyph-ref ()
  :description "a font editor glyph entry")

(clim:define-presentation-type font-editor-pixel-ref ()
  :description "a font editor pixel cell")

(clim:define-presentation-type font-editor-action-ref ()
  :description "a font editor action")

;;; --------------------------------------------------------------------------
;;; CLIM Command Tables + Presentation Translators
;;;
;;; These provide canonical CLIM object interaction paths (presentation →
;;; command) for clickable buffer/model selector entries.
;;; --------------------------------------------------------------------------

(clim:define-command-table clawmacs-mcclim-command-table
  :inherit-from nil)

(clim:define-command (com-select-buffer
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((target-buffer 'buffer-ref))
  (let ((current (current-buffer)))
    (when (and target-buffer (member target-buffer *buffer-ring*))
      (switch-to-buffer target-buffer)
      (when (boundp 'clim:*application-frame*)
        (mcclim-set-selected-window-buffer clim:*application-frame*
                                           target-buffer))
      (setf *buffer-selector-active* nil)
      (unless (eq target-buffer current)
        (setf (buffer-scroll-offset target-buffer) 0)))))

(clim:define-command (com-select-window
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((target-window 'clawmacs-window-ref))
  (when target-window
    (mcclim-select-window clim:*application-frame* target-window)))

(clim:define-command (com-select-package-dashboard-entry
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((entry 'package-dashboard-entry-ref))
  (when (listp entry)
    (package-dashboard-toggle-entry (getf entry :dashboard-buffer)
                                    (getf entry :entry)
                                    :origin-buffer (getf entry :origin-buffer))))

(clim:define-command (com-describe-package-dashboard-entry
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((entry 'package-dashboard-entry-ref))
  (when (listp entry)
    (package-dashboard-describe-entry (getf entry :entry)
                                      :buffer (getf entry :origin-buffer))))

(clim:define-command (com-select-model-entry
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((entry 'model-ref))
  (let ((buf (frame-visible-buffer clim:*application-frame*)))
    (when (and *model-selector-active* entry (listp entry))
      (let ((provider (getf entry :provider))
            (model (getf entry :model)))
        (when (and provider model)
          (apply-buffer-model-selection buf provider model)
          (setf *model-selector-active* nil))))))

(clim:define-command (com-select-think-level-entry
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((entry 'think-level-ref))
  (let ((buf (frame-visible-buffer clim:*application-frame*)))
    (when (and *think-selector-active* entry (listp entry))
      (apply-buffer-think-level-selection buf entry)
      (setf *think-selector-active* nil))))

(defun mcclim-select-session-tree-item (item)
  "Select ITEM from the active session tree selector."
  (when (and item *session-tree-selector-active*)
    (let ((index (position item *session-tree-selector-filtered-items*
                           :test #'eq)))
      (when index
        (setf *session-tree-selector-index* index))
      (setf *session-tree-selector-active* nil)
      (when *session-tree-selector-callback*
        (funcall *session-tree-selector-callback* item)))))

(clim:define-command (com-select-session-tree-entry
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((entry 'session-tree-entry-ref))
  (mcclim-select-session-tree-item entry))

(defun mcclim-message-description (message)
  "Return a help-buffer description for rendered MESSAGE."
  (with-output-to-string (out)
    (format out "Message~%=======~%~%")
    (format out "Sender: ~(~A~)~%" (message-sender message))
    (when (message-entry-id message)
      (format out "Entry ID: ~A~%" (message-entry-id message)))
    (when (message-parent-entry-id message)
      (format out "Parent Entry ID: ~A~%" (message-parent-entry-id message)))
    (when (message-timestamp message)
      (format out "Timestamp: ~A~%" (message-timestamp message)))
    (when (message-metadata message)
      (format out "~%Metadata:~%~S~%" (message-metadata message)))
    (when (message-raw-content message)
      (format out "~%Raw Content:~%~S~%" (message-raw-content message)))
    (format out "~%Text:~%~A~%" (message-text message))))

(clim:define-command (com-describe-rendered-message
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((message 'rendered-message-ref))
  (when message
    (let ((help (make-help-buffer "*help:message*"
                                  (mcclim-message-description message))))
      (switch-to-buffer help))))

(clim:define-command (com-fork-chat-message-session
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((message 'chat-message))
  (let ((buffer (current-buffer)))
    (when (and message
               (message-entry-id message)
               (buffer-session buffer))
      (fork-session-from-entry-id buffer (message-entry-id message)))))

(defun mcclim-image-description (reference)
  "Return a help-buffer description for inline image REFERENCE."
  (with-output-to-string (out)
    (format out "Image~%=====~%~%")
    (format out "Path: ~A~%" (display-image-reference-path reference))
    (let ((alt (display-image-reference-alt reference)))
      (when (plusp (length alt))
        (format out "Alt: ~A~%" alt)))
    (let ((raw (display-image-reference-raw-text reference)))
      (when (plusp (length raw))
        (format out "~%Markdown:~%~A~%" raw)))))

(clim:define-command (com-describe-image-reference
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((reference 'image-reference))
  (when reference
    (let ((help (make-help-buffer "*help:image*"
                                  (mcclim-image-description reference))))
      (switch-to-buffer help))))

(defun mcclim-apply-approval-action (action)
  "Apply approval prompt ACTION in the visible buffer."
  (let ((buf (and (boundp 'clim:*application-frame*)
                  (frame-visible-buffer clim:*application-frame*))))
    (when (and buf (buffer-approval-pending buf))
      (ecase action
        (:approve
         (handle-approval-response buf :approve))
        (:deny
         (handle-approval-response buf :deny))
        (:message
         (set-message-text (buffer-input-message buf) "")
         (setf *deny-message-mode* t))))))

(clim:define-command (com-apply-approval-action
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((action 'approval-action))
  (mcclim-apply-approval-action action))

(defun mcclim-select-minibuffer-candidate (candidate)
  "Select CANDIDATE from the active custom minibuffer popup."
  (when (and *minibuffer-active* candidate)
    (let ((index (getf candidate :index)))
      (when (and index (<= 0 index)
                 (< index (length *minibuffer-filtered-items*)))
        (setf *minibuffer-selected-index* index)
        (minibuffer-confirm)))))

(clim:define-command (com-select-minibuffer-candidate
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((candidate 'minibuffer-candidate-ref))
  (mcclim-select-minibuffer-candidate candidate))

(clim:define-command (com-select-font-editor-glyph
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((code 'font-editor-glyph-ref))
  (let ((buf (frame-visible-buffer clim:*application-frame*)))
    (when (and buf (font-editor-buffer-p buf))
      (font-editor-select-glyph buf code))))

(clim:define-command (com-toggle-font-editor-pixel
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((cell 'font-editor-pixel-ref))
  (let ((buf (frame-visible-buffer clim:*application-frame*)))
    (when (and buf (font-editor-buffer-p buf) (listp cell))
      (font-editor-toggle-pixel buf
                                (or (getf cell :x) 0)
                                (or (getf cell :y) 0)))))

(clim:define-command (com-apply-font-editor-action
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((action 'font-editor-action-ref))
  (let ((buf (frame-visible-buffer clim:*application-frame*)))
    (when (and buf (font-editor-buffer-p buf))
      (font-editor-handle-action buf action))))

(defun mcclim-select-skill-candidate (candidate)
  "Select CANDIDATE from the active automatic skill completion popup."
  (when (and *skill-completion-active* candidate)
    (let ((index (getf candidate :index))
          (buf *skill-completion-buffer*))
      (when (and index buf (<= 0 index)
                 (< index (length *skill-completion-filtered-items*)))
        (setf *skill-completion-selected-index* index)
        (insert-selected-skill-completion buf)))))

(clim:define-command (com-select-skill-candidate
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((candidate 'skill-candidate-ref))
  (mcclim-select-skill-candidate candidate))

(defun mcclim-select-slash-candidate (candidate)
  "Select CANDIDATE from the active automatic slash completion popup."
  (when (and *slash-completion-active* candidate)
    (let ((index (getf candidate :index))
          (buf *slash-completion-buffer*))
      (when (and index buf (<= 0 index)
                 (< index (length *slash-completion-filtered-items*)))
        (setf *slash-completion-selected-index* index)
        (insert-selected-slash-completion buf)))))

(clim:define-command (com-select-slash-candidate
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((candidate 'slash-candidate-ref))
  (mcclim-select-slash-candidate candidate))

(defun mcclim-select-customize-field (field-ref)
  "Select and edit FIELD-REF from a customize buffer presentation."
  (when (and (listp field-ref)
             *customize-face-state*)
    (let ((buf (getf field-ref :buffer))
          (index (getf field-ref :index)))
      (when (and (eq buf (getf *customize-face-state* :buffer))
                 (customize-face-select-field index :edit-p t))
        field-ref))))

(clim:define-command (com-select-customize-field
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((field 'customize-field-ref))
  (mcclim-select-customize-field field))

(defun mcclim-follow-info-link (link)
  "Follow LINK from an Info buffer presentation."
  (let ((buf (and (boundp 'clim:*application-frame*)
                  (frame-visible-buffer clim:*application-frame*))))
    (when (and buf (info-buffer-p buf) link)
      (info-follow-link buf link))))

(defun mcclim-perform-background-update-command ()
  "Process pending asynchronous UI state on the current application frame."
  (let ((app-frame clim:*application-frame*))
    (when (typep app-frame 'clawmacs-gui)
      (file-debug-log "mcclim-update"
                      "command frame=~S buffer=~S pending=~S"
                      app-frame
                      (and (frame-visible-buffer app-frame)
                           (buffer-name (frame-visible-buffer app-frame)))
                      (frame-background-command-pending-p app-frame))
      (setf (frame-background-command-pending-p app-frame) nil)
      (mcclim-process-background-updates app-frame)
      (mcclim-sync-selected-window-from-current-buffer
       app-frame
       (frame-visible-buffer app-frame))
      (ignore-errors
        (mcclim-sync-main-pane-to-buffer-scroll app-frame)))))

(clim:define-command (com-follow-info-link
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((link 'info-link-ref))
  (mcclim-follow-info-link link))

(clim:define-command (com-process-background-updates
                      :command-table clawmacs-mcclim-command-table
                      :name nil)
    ()
  (mcclim-perform-background-update-command))

(defun mcclim-main-pane-sheet-p (frame event)
  "Return true when EVENT targets FRAME's transcript pane."
  (let ((main-pane (and frame (clim:find-pane-named frame 'main-pane)))
        (sheet (and event (ignore-errors (clim:event-sheet event)))))
    (and main-pane sheet
         (eq (mcclim-nearest-output-recording-pane sheet frame)
             main-pane))))

(defun mcclim-main-pane-blank-area-documentation (frame event)
  "Return pointer documentation for blank transcript areas.

Blank transcript space is no longer a synthetic character grid. Users move
point through the native Drei input pane and select semantic presentations in
the transcript."
  (declare (ignore frame event))
  nil)

(defun mcclim-selector-pane-active-p ()
  "Return true when any dedicated selector pane should be shown."
  (or *session-tree-selector-active*
      *buffer-selector-active*
      *model-selector-active*
      *think-selector-active*))

(defun mcclim-completion-pane-active-p ()
  "Return true when the completion pane should be shown."
  (or *minibuffer-active*
      *slash-completion-active*
      *skill-completion-active*))

(defun mcclim-handle-main-pane-blank-area-select (frame event)
  "Ignore blank transcript clicks.

Clawmacs no longer maps pointer pixels to synthetic text cells. Transcript
objects are selected through CLIM presentations instead."
  (declare (ignore frame event))
  nil)

(clim:define-command (com-main-pane-blank-area-select
                      :command-table clawmacs-mcclim-command-table
                      :name nil)
    ((event 'clim:blank-area))
  (mcclim-handle-main-pane-blank-area-select clim:*application-frame* event))

(clim:define-presentation-to-command-translator click-buffer-ref
    (buffer-ref com-select-buffer clawmacs-mcclim-command-table
                :gesture :select
                :priority 10
                :documentation "Switch to this buffer"
                :pointer-documentation "Switch to this buffer")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-clawmacs-window
    (clawmacs-window-ref com-select-window clawmacs-mcclim-command-table
                         :gesture :select
                         :priority 5
                         :tester
                         ((object)
                          (not (eql (ignore-errors
                                      (frame-selected-window-id
                                       clim:*application-frame*))
                                    (clawmacs-window-id object))))
                         :documentation "Select this window"
                         :pointer-documentation "Select this window")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-package-dashboard-entry
    (package-dashboard-entry-ref
     com-select-package-dashboard-entry
     clawmacs-mcclim-command-table
     :gesture :select
     :priority 20
     :documentation "Toggle package scope"
     :pointer-documentation "Toggle package scope")
    (object)
  (list object))

(clim:define-presentation-to-command-translator describe-package-dashboard-entry
    (package-dashboard-entry-ref
     com-describe-package-dashboard-entry
     clawmacs-mcclim-command-table
     :gesture :describe
     :priority 20
     :documentation "Describe this package"
     :pointer-documentation "Describe this package")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-model-ref
    (model-ref com-select-model-entry clawmacs-mcclim-command-table
               :gesture :select
               :priority 10
               :documentation "Apply selection"
               :pointer-documentation "Apply this model")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-think-level-ref
    (think-level-ref com-select-think-level-entry
                     clawmacs-mcclim-command-table
                     :gesture :select
                     :priority 10
                     :documentation "Apply think-level selection"
                     :pointer-documentation "Apply this think level")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-session-tree-entry
    (session-tree-entry-ref com-select-session-tree-entry
                            clawmacs-mcclim-command-table
                            :gesture :select
                            :priority 10
                            :documentation "Navigate to this session entry"
                            :pointer-documentation "Navigate to this session entry")
    (object)
  (list object))

(clim:define-presentation-to-command-translator describe-rendered-message
    (rendered-message-ref com-describe-rendered-message
                          clawmacs-mcclim-command-table
                          :gesture :describe
                          :priority 10
                          :documentation "Describe this message"
                          :pointer-documentation "Describe this message")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-chat-message
    (chat-message com-fork-chat-message-session
                  clawmacs-mcclim-command-table
                  :gesture :select
                  :priority 25
                  :tester ((message)
                           (not (null (message-entry-id message))))
                  :documentation "Fork a new session from this message"
                  :pointer-documentation "Fork a new session from this message")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-image-reference
    (image-reference com-describe-image-reference
                     clawmacs-mcclim-command-table
                     :gesture :select
                     :priority 20
                     :documentation "Describe this image"
                     :pointer-documentation "Describe this image")
    (object)
  (list object))

(clim:define-presentation-to-command-translator describe-image-reference
    (image-reference com-describe-image-reference
                     clawmacs-mcclim-command-table
                     :gesture :describe
                     :priority 10
                     :documentation "Describe this image"
                     :pointer-documentation "Describe this image")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-approval-action
    (approval-action com-apply-approval-action
                     clawmacs-mcclim-command-table
                     :gesture :select
                     :priority 30
                     :documentation "Choose approval action"
                     :pointer-documentation "Choose approval action")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-minibuffer-candidate
    (minibuffer-candidate-ref com-select-minibuffer-candidate
                              clawmacs-mcclim-command-table
                              :gesture :select
                              :priority 30
                              :documentation "Choose this completion candidate"
                              :pointer-documentation "Choose this candidate")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-skill-candidate
    (skill-candidate-ref com-select-skill-candidate
                         clawmacs-mcclim-command-table
                         :gesture :select
                         :priority 30
                         :documentation "Insert this skill mention"
                         :pointer-documentation "Insert this skill mention")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-slash-candidate
    (slash-candidate-ref com-select-slash-candidate
                         clawmacs-mcclim-command-table
                         :gesture :select
                         :priority 30
                         :documentation "Insert this slash command"
                         :pointer-documentation "Insert this slash command")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-customize-field
    (customize-field-ref com-select-customize-field
                         clawmacs-mcclim-command-table
                         :gesture :select
                         :priority 30
                         :documentation "Edit this customize field"
                         :pointer-documentation "Edit this customize field")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-info-link
    (info-link-ref com-follow-info-link
                   clawmacs-mcclim-command-table
                   :gesture :select
                   :priority 30
                   :documentation "Follow this Info link"
                    :pointer-documentation "Follow this Info link")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-font-editor-glyph
    (font-editor-glyph-ref com-select-font-editor-glyph
                           clawmacs-mcclim-command-table
                           :gesture :select
                           :priority 30
                           :documentation "Select this glyph"
                           :pointer-documentation "Select this glyph")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-font-editor-pixel
    (font-editor-pixel-ref com-toggle-font-editor-pixel
                           clawmacs-mcclim-command-table
                           :gesture :select
                           :priority 30
                           :documentation "Toggle this pixel"
                           :pointer-documentation "Toggle this pixel")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-font-editor-action
    (font-editor-action-ref com-apply-font-editor-action
                            clawmacs-mcclim-command-table
                            :gesture :select
                            :priority 30
                            :documentation "Invoke this action"
                            :pointer-documentation "Invoke this action")
    (object)
  (list object))

(clim:define-presentation-to-command-translator click-main-pane-blank-area
    (clim:blank-area com-main-pane-blank-area-select
                clawmacs-mcclim-command-table
                :gesture :select
                :priority 1
                :tester ((event)
                         (and (boundp 'clim:*application-frame*)
                              (mcclim-main-pane-sheet-p
                               clim:*application-frame*
                               event)
                              (mcclim-main-pane-blank-area-documentation
                               clim:*application-frame*
                               event)))
                :documentation ((event stream)
                                (let ((doc
                                        (mcclim-main-pane-blank-area-documentation
                                         clim:*application-frame*
                                         event)))
                                  (when doc
                                    (write-string doc stream))))
                :pointer-documentation ((event stream)
                                        (let ((doc
                                                (mcclim-main-pane-blank-area-documentation
                                                 clim:*application-frame*
                                                 event)))
                                          (when doc
                                            (write-string doc stream)))))
    (event)
  (list event))

(clim:define-command (com-clawmacs-dispatch-gestures
                      :command-table clawmacs-mcclim-command-table
                      :name nil)
    ((gestures 'clim:expression))
  (let ((frame clim:*application-frame*))
    (mcclim-sync-buffer-from-drei frame)
    (dolist (gesture gestures)
      (mcclim-dispatch-gesture frame gesture))
    (mcclim-sync-drei-from-buffer frame :force-p t)))

;;; --------------------------------------------------------------------------
;;; Frame-Owned UI State
;;;
;;; Core interaction code still uses special variables because commands,
;;; tests, and prompt-only entry points call it directly.  The McCLIM frame
;;; owns its own copy of those modal values and dynamically binds the existing
;;; specials while rendering or handling events.
;;; --------------------------------------------------------------------------

(defparameter *mcclim-ui-state-symbols*
  '(*buffer-selector-active*
    *buffer-selector-index*
    *buffer-selector-scroll*
    *model-selector-active*
    *model-selector-index*
    *model-selector-scroll*
    *model-selector-entries*
    *think-selector-active*
    *think-selector-index*
    *think-selector-scroll*
    *think-selector-entries*
    *session-tree-selector-active*
    *session-tree-selector-buffer*
    *session-tree-selector-items*
    *session-tree-selector-filtered-items*
    *session-tree-selector-index*
    *session-tree-selector-scroll*
    *session-tree-selector-search*
    *session-tree-selector-filter-mode*
    *session-tree-selector-folded-ids*
    *session-tree-selector-callback*
    *session-tree-selector-label-callback*
    *minibuffer-active*
    *minibuffer-mode*
    *minibuffer-prompt*
    *minibuffer-input*
    *minibuffer-point*
    *minibuffer-items*
    *minibuffer-filtered-items*
    *minibuffer-match-positions*
    *minibuffer-selected-index*
    *minibuffer-scroll-offset*
    *minibuffer-callback*
    *slash-completion-active*
    *slash-completion-buffer*
    *slash-completion-query*
    *slash-completion-token-start*
    *slash-completion-token-end*
    *slash-completion-token-text*
    *slash-completion-dismissed-token*
    *slash-completion-items*
    *slash-completion-filtered-items*
    *slash-completion-match-positions*
    *slash-completion-selected-index*
    *slash-completion-scroll-offset*
    *skill-completion-active*
    *skill-completion-buffer*
    *skill-completion-query*
    *skill-completion-token-start*
    *skill-completion-token-end*
    *skill-completion-token-text*
    *skill-completion-dismissed-token*
    *skill-completion-items*
    *skill-completion-filtered-items*
    *skill-completion-match-positions*
    *skill-completion-selected-index*
    *skill-completion-scroll-offset*
    *customize-face-state*
    *deny-message-mode*
    *meta-pending*
    *alt-pending*
    *cx-pending*
    *cc-pending*
    *ch-pending*
    *scroll-page-size*)
  "Special variables that are frame-local in the McCLIM interface.")

(defvar *mcclim-bound-ui-state* nil
  "The McCLIM UI state currently bound into interaction special variables.")

(defvar *mcclim-command-previous-buffer* nil
  "FRAME-visible buffer captured around one execute-frame-command call.")

(defstruct (mcclim-ui-state
            (:constructor %make-mcclim-ui-state (values)))
  (values nil :type list))

(defun mcclim-capture-ui-state-values ()
  "Return current values for McCLIM's frame-local interaction specials."
  (mapcar (lambda (symbol)
            (if (boundp symbol)
                (symbol-value symbol)
                nil))
          *mcclim-ui-state-symbols*))

(defun make-mcclim-ui-state ()
  "Create a UI state object initialized from the current interaction state."
  (%make-mcclim-ui-state (mcclim-capture-ui-state-values)))

(defun mcclim-ui-state-binding-values (state)
  "Return STATE values padded to the current frame-local symbol list."
  (let* ((values (mcclim-ui-state-values state))
         (symbols *mcclim-ui-state-symbols*)
         (missing (- (length symbols) (length values))))
    (if (plusp missing)
        (append values
                (subseq (mcclim-capture-ui-state-values)
                        (length values)))
        values)))

(defmacro with-mcclim-ui-state ((state) &body body)
  "Run BODY with McCLIM frame-local interaction specials bound from STATE."
  (let ((state-var (gensym "STATE-"))
        (symbols-var (gensym "SYMBOLS-")))
    `(let ((,state-var ,state))
       (if (or (null ,state-var)
               (eq *mcclim-bound-ui-state* ,state-var))
           (progn ,@body)
           (let ((,symbols-var *mcclim-ui-state-symbols*))
             (progv ,symbols-var (mcclim-ui-state-binding-values ,state-var)
               (unwind-protect
                    (let ((*mcclim-bound-ui-state* ,state-var))
                      ,@body)
                 (setf (mcclim-ui-state-values ,state-var)
                       (mapcar #'symbol-value ,symbols-var)))))))))

(defmacro with-mcclim-frame-ui-state ((frame) &body body)
  "Run BODY with FRAME's interaction specials dynamically bound."
  (let ((frame-var (gensym "FRAME-")))
    `(let ((,frame-var ,frame))
       (with-mcclim-ui-state
           ((and ,frame-var (frame-ui-state ,frame-var)))
         ,@body))))

;;; --------------------------------------------------------------------------
;;; McCLIM Pane Classes
;;; --------------------------------------------------------------------------

(defclass clawmacs-workspace-placeholder-pane
    (esa:esa-pane-mixin clim:application-pane)
  ()
  (:default-initargs
   :display-function (lambda (_frame _pane)
                       (declare (ignore _frame _pane))
                       nil)
   :display-time t
   :incremental-redisplay t
   :background (clim:make-rgb-color 1.0 1.0 1.0)
   :foreground (clim:make-rgb-color 0.0 0.0 0.0)
   :command-table 'clawmacs-mcclim-command-table))

(defclass clawmacs-transcript-pane (esa:esa-pane-mixin clim:application-pane)
  ((window-id :initarg :window-id
              :initform nil
              :reader transcript-pane-window-id))
  (:default-initargs
   :display-function 'display-main-pane
   :display-time :command-loop
   :incremental-redisplay nil
   :text-style (clim:make-text-style :fix :roman :normal)
   :background (clim:make-rgb-color 1.0 1.0 1.0)
   :foreground (clim:make-rgb-color 0.0 0.0 0.0)
   :command-table 'clawmacs-mcclim-command-table))

(defclass clawmacs-selector-pane (esa:esa-pane-mixin clim:application-pane)
  ()
  (:default-initargs
   :display-function 'display-selector-pane
   :display-time :command-loop
   :incremental-redisplay t
   :text-style (clim:make-text-style :fix :roman :normal)
   :background (clim:make-rgb-color 1.0 1.0 1.0)
   :foreground (clim:make-rgb-color 0.0 0.0 0.0)
   :command-table 'clawmacs-mcclim-command-table))

(defclass clawmacs-completion-pane (esa:esa-pane-mixin clim:application-pane)
  ()
  (:default-initargs
   :display-function 'display-completion-pane
   :display-time :command-loop
   :incremental-redisplay t
   :text-style (clim:make-text-style :fix :roman :normal)
   :background (clim:make-rgb-color 1.0 1.0 1.0)
   :foreground (clim:make-rgb-color 0.0 0.0 0.0)
   :command-table 'clawmacs-mcclim-command-table))

(defclass clawmacs-drei-input-view (drei:textual-drei-syntax-view)
  ()
  (:metaclass esa-utils:modual-class)
  (:default-initargs
   :name "Clawmacs Input"
   :use-editor-commands t))

(defclass clawmacs-drei-input-pane (drei:drei-pane esa:esa-pane-mixin)
  ((last-layout-width :accessor pane-last-layout-width
                      :initform nil)
   (last-layout-height :accessor pane-last-layout-height
                       :initform nil))
  (:metaclass esa-utils:modual-class)
  (:default-initargs
   :display-time :command-loop
   :background (clim:make-rgb-color 1.0 1.0 1.0)
   :foreground (clim:make-rgb-color 0.0 0.0 0.0)
   :text-style (clim:make-text-style :fix :roman :normal)
   :view (make-instance 'clawmacs-drei-input-view)))

(defclass clawmacs-minibuffer-pane (esa:minibuffer-pane)
  ()
  (:default-initargs
   :display-function 'display-clawmacs-minibuffer-pane
   :incremental-redisplay nil
   :height 0
   :min-height 0
   :max-height 0
   :text-style (clim:make-text-style :fix :roman :normal)
   :background (clim:make-rgb-color 0.98 0.98 0.98)
   :foreground (clim:make-rgb-color 0.0 0.0 0.0)))

(defparameter *mcclim-chat-input-pane-rows* 3
  "Stable row height for the shared chat compose pane.")

(defparameter *mcclim-listener-input-pane-rows* 3
  "Stable row height for the shared listener input pane.")

(defparameter *mcclim-document-input-pane-rows* 8
  "Stable row height for the shared document editor pane.")

(defun display-clawmacs-minibuffer-pane (frame pane)
  "Display Clawmacs' hidden ESA minibuffer service pane.
Clawmacs renders completion state in the main presentation pane. This pane is
kept as a minimal ESA-compatible service pane for commands that expect a real
`esa:minibuffer-pane', but it is intentionally not a visible UI region."
  (declare (ignore frame pane))
  nil)

(defun mcclim-buffer-uses-shared-input-pane-p (buf)
  "Return true when BUF should use the shared Drei compose/editor pane."
  (and buf
       (or (eq (buffer-kind buf) :chat)
           (listener-buffer-p buf)
           (and (document-buffer-p buf)
                (null (buffer-presentation-function buf))))))

(defun compose-pane-label-text (buf)
  "Return the label shown in the dedicated compose/input service pane."
  (cond
    ((null buf) "Input")
    ((font-editor-buffer-p buf) "Font Editor")
    ((listener-buffer-p buf) "Listener Input")
    ((mcclim-buffer-uses-shared-input-pane-p buf) "Editor")
    ((or (help-buffer-p buf)
         (info-buffer-p buf)
         (customize-buffer-p buf))
     "Read-Only")
    ((eq (buffer-kind buf) :organa) "Organa Commands")
    (t "Compose Message")))

(defun display-compose-pane (frame pane)
  "Display the compose/input section header as native CLIM stream output."
  (with-mcclim-frame-ui-state (frame)
    (let ((text (compose-pane-label-text (frame-visible-buffer frame))))
      (clim:updating-output (pane :unique-id 'compose-pane-content
                                  :cache-value text
                                  :cache-test #'string=)
        (write-string text pane)))))

;;; --------------------------------------------------------------------------
;;; Application Frame
;;; --------------------------------------------------------------------------

(clim:define-application-frame clawmacs-gui
    (esa:esa-frame-mixin clim:standard-application-frame)
  ((display-buffer :initarg :display-buffer
                   :initform nil
                   :accessor frame-display-buffer)
   (follow-current-buffer-p :initarg :follow-current-buffer-p
                            :initform t
                            :accessor frame-follow-current-buffer-p)
   (window-tree :initarg :window-tree
                :initform nil
                :accessor frame-window-tree)
   (workspace-parent-pane :initform nil
                          :accessor frame-workspace-parent-pane)
   (workspace-root-pane :initform nil
                        :accessor frame-workspace-root-pane)
   (window-pane-table :initform (make-hash-table :test #'eql)
                      :accessor frame-window-pane-table)
   (window-layout-signature :initform nil
                            :accessor frame-window-layout-signature)
   (workspace-syncing-p :initform nil
                        :accessor frame-workspace-syncing-p)
   (selected-window-id :initarg :selected-window-id
                       :initform nil
                       :accessor frame-selected-window-id)
   (char-width :accessor frame-char-width :initform 0)
   (char-height :accessor frame-char-height :initform 0)
   (last-render-snapshot :accessor frame-last-render-snapshot :initform nil)
   (render-sequence :accessor frame-render-sequence :initform 0)
   (quit-flag :accessor frame-quit-flag :initform nil)
   (background-command-pending-p
    :accessor frame-background-command-pending-p
    :initform nil)
   (syncing-drei-p :accessor frame-syncing-drei-p :initform nil)
   (last-drei-buffer :accessor frame-last-drei-buffer :initform nil)
   (hover-presentation :accessor frame-hover-presentation :initform nil)
   (hover-pane :accessor frame-hover-pane :initform nil)
   (pointer-documentation-text
    :accessor frame-pointer-documentation-text
    :initform "")
   (ui-state :accessor frame-ui-state
             :initform (make-mcclim-ui-state)))
  (:command-table (clawmacs-mcclim-command-table :inherit-from nil))
  (:panes
   (selector-pane clawmacs-selector-pane
                  :min-width 0
                  :height 0
                  :min-height 0)
   (workspace-pane clawmacs-workspace-placeholder-pane
                   :min-width 0)
   (compose-pane :application
                 :display-function 'display-compose-pane
                 :display-time :command-loop
                 :incremental-redisplay t
                 :min-width 0
                 :height 20
                 :min-height 20
                 :max-height 20
                 :text-style (clim:make-text-style :fix :roman :normal)
                 :scroll-bars nil
                 :background (clim:make-rgb-color 0.93 0.93 0.93)
                 :foreground (clim:make-rgb-color 0.0 0.0 0.0))
   (input-pane clawmacs-drei-input-pane
               :display-time :command-loop
               :min-width 0
               :height 42
               :min-height 20
               :max-height 90)
   (completion-pane clawmacs-completion-pane
                    :min-width 0
                    :height 0
                    :min-height 0)
   (minibuffer-pane clawmacs-minibuffer-pane
                    :min-width 0)
   (pointer-doc-pane :pointer-documentation
                     :min-width 0
                     :height 18
                     :min-height 18
                     :max-height 18)
   (modeline-pane :application
                  :display-function 'display-modeline-pane
                  :display-time :command-loop
                  :incremental-redisplay t
                  :min-width 0
                  :height 14
                  :min-height 14
                  :max-height 14
                  :text-style (clim:make-text-style :fix :roman :normal)
                  :scroll-bars nil
                  :background (clim:make-rgb-color 0.67 0.67 0.67)
                  :foreground (clim:make-rgb-color 0.0 0.0 0.0))
   (who-line-pane :application
                  :display-function 'display-who-line-pane
                  :display-time :command-loop
                  :incremental-redisplay t
                  :min-width 0
                  :height 28
                  :min-height 28
                  :max-height 28
                  :text-style (clim:make-text-style :fix :roman :normal)
                  :scroll-bars nil
                  :background (clim:make-rgb-color 0.93 0.93 0.93)
                  :foreground (clim:make-rgb-color 0.0 0.0 0.0)))
  (:layouts
   (default
    (clim:vertically ()
      (clim:horizontally ()
        (:fill selector-pane))
      (:fill
       (clim:horizontally ()
         (:fill workspace-pane)))
      (clim:horizontally ()
        (:fill compose-pane))
      (clim:horizontally ()
        (:fill input-pane))
      (clim:horizontally ()
        (:fill completion-pane))
      (clim:horizontally ()
        (:fill modeline-pane))
      (clim:horizontally ()
        (:fill who-line-pane))
      (clim:horizontally ()
        (:fill pointer-doc-pane)))))
  (:top-level (esa:esa-top-level))
  (:menu-bar nil)
  (:pointer-documentation t))

(defun mcclim-window-tree-signature (node)
  "Return a structural signature for logical window NODE."
  (cond
    ((null node) nil)
    ((clawmacs-window-node-leaf-p node)
     (list :leaf
           (clawmacs-window-id
            (clawmacs-window-node-window node))))
    ((clawmacs-window-node-split-p node)
     (list :split
           (clawmacs-window-node-orientation node)
           (mcclim-window-tree-signature
            (clawmacs-window-node-first node))
           (mcclim-window-tree-signature
            (clawmacs-window-node-second node))))
    (t nil)))

(defun mcclim-bind-workspace-parent (frame)
  "Record FRAME's stable workspace parent and current workspace child."
  (unless (frame-workspace-parent-pane frame)
    (let ((workspace-pane (clim:find-pane-named frame 'workspace-pane)))
      (when workspace-pane
        (setf (frame-workspace-parent-pane frame)
              (ignore-errors (clim:sheet-parent workspace-pane))
              (frame-workspace-root-pane frame)
              workspace-pane))))
  frame)

(defun frame-window-pane (frame window-or-id)
  "Return FRAME's transcript pane for WINDOW-OR-ID, or NIL."
  (let ((window-id (etypecase window-or-id
                     (integer window-or-id)
                     (clawmacs-window (clawmacs-window-id window-or-id)))))
    (gethash window-id (frame-window-pane-table frame))))

(defun frame-window-panes-in-display-order (frame)
  "Return FRAME transcript panes in logical display order."
  (let ((tree (frame-window-tree frame)))
    (remove nil
            (mapcar (lambda (window)
                      (frame-window-pane frame window))
                    (and tree
                         (clawmacs-window-tree-windows tree))))))

(defun frame-selected-window-pane (frame)
  "Return FRAME's selected transcript pane, or NIL."
  (let ((window-id (frame-selected-window-id frame)))
    (and window-id
         (frame-window-pane frame window-id))))

(defun transcript-pane-window (frame pane)
  "Return FRAME's logical window displayed by transcript PANE."
  (let ((window-id (and pane (transcript-pane-window-id pane))))
    (and window-id
         (clawmacs-window-tree-find-window
          (frame-window-tree frame)
          window-id))))

(defun transcript-pane-buffer (frame pane)
  "Return the buffer displayed by transcript PANE."
  (let ((window (transcript-pane-window frame pane)))
    (or (and window (clawmacs-window-buffer window))
        (frame-visible-buffer frame))))

(defun mcclim-maybe-make-box-adjuster ()
  "Return a McCLIM box adjuster gadget when available."
  (let* ((package (find-package :clim-extensions))
         (symbol (and package (find-symbol "BOX-ADJUSTER-GADGET" package))))
    (when (and symbol (ignore-errors (find-class symbol nil)))
      (clim:make-pane symbol))))

(defparameter +mcclim-default-pane-width+ 900
  "Preferred width for transcript/workspace panes in the initial frame layout.")

(defun mcclim-build-window-pane-constellation (window pane-table)
  "Return a native transcript pane subtree for WINDOW."
  (let ((pane (clim:make-pane 'clawmacs-transcript-pane
                              :width +mcclim-default-pane-width+
                              :window-id (clawmacs-window-id window))))
    (setf (gethash (clawmacs-window-id window) pane-table) pane)
    (clim:scrolling (:scroll-bars :vertical
                      :width +mcclim-default-pane-width+)
      pane)))

(defun mcclim-build-workspace-root (node pane-table)
  "Return a native CLIM pane subtree mirroring logical window NODE."
  (cond
    ((null node)
     (clim:make-pane 'clawmacs-workspace-placeholder-pane))
    ((clawmacs-window-node-leaf-p node)
     (mcclim-build-window-pane-constellation
      (clawmacs-window-node-window node)
      pane-table))
    ((clawmacs-window-node-split-p node)
     (let* ((first (mcclim-build-workspace-root
                    (clawmacs-window-node-first node)
                    pane-table))
            (second (mcclim-build-workspace-root
                     (clawmacs-window-node-second node)
                     pane-table))
            (adjuster (mcclim-maybe-make-box-adjuster)))
       (case (clawmacs-window-node-orientation node)
         (:vertical
          (if adjuster
              (clim:vertically ()
                (:fill first)
                adjuster
                (:fill second))
              (clim:vertically ()
                (:fill first)
                (:fill second))))
         (:horizontal
          (if adjuster
              (clim:horizontally ()
                (:fill first)
                adjuster
                (:fill second))
              (clim:horizontally ()
                (:fill first)
                (:fill second)))))))
    (t
     (clim:make-pane 'clawmacs-workspace-placeholder-pane))))

(defun mcclim-install-workspace-root (frame root)
  "Replace FRAME's current workspace child with ROOT.
Returns true when the replacement succeeded."
  (labels ((layout-child-panes (pane)
             (cond
               ((null pane) nil)
               ((typep pane 'climi::box-layout-mixin)
                (loop :for client :in (climi::box-layout-mixin-clients pane)
                      :for child := (climi::box-client-pane client)
                      :when child
                        :collect child))
               (t
                (remove nil
                        (copy-list
                         (ignore-errors (clim:sheet-children pane))))))))
    (let* ((parent (frame-workspace-parent-pane frame))
           (old-root (frame-workspace-root-pane frame))
           (children (and parent (layout-child-panes parent))))
      (when (and parent old-root children)
        (let ((index (or (position old-root children :test #'eq)
                         (max 0 (1- (length children))))))
          (ignore-errors
            (clim:sheet-disown-child parent old-root))
          (clim:sheet-adopt-child parent root)
          (if (typep parent 'climi::box-layout-mixin)
              (let* ((clients (copy-list (climi::box-layout-mixin-clients parent)))
                     (new-client (find root clients :key #'climi::box-client-pane))
                     (remaining (remove new-client clients :test #'eq)))
                (setf index (min index (length remaining)))
                (setf clients
                      (append (subseq remaining 0 index)
                              (list new-client)
                              (subseq remaining index)))
                (setf (climi::box-layout-mixin-clients parent) clients)
                (clim:change-space-requirements parent))
              (let ((new-order (remove old-root
                                       (remove root
                                               (layout-child-panes parent)
                                               :test #'eq)
                                       :test #'eq)))
                (setf index (min index (length new-order)))
                (setf new-order
                      (append (subseq new-order 0 index)
                              (list root)
                              (subseq new-order index)))
                (clim:reorder-sheets parent new-order)))
          (setf (frame-workspace-root-pane frame) root)
          (clim:layout-frame frame)
          t)))))

(defun mcclim-sync-workspace-from-window-tree (frame)
  "Ensure FRAME's workspace subtree mirrors its logical window tree."
  (unless (frame-workspace-syncing-p frame)
    (mcclim-bind-workspace-parent frame)
    (let* ((tree (frame-window-tree frame))
           (signature (mcclim-window-tree-signature tree))
           (needs-rebuild-p
             (and tree
                  (or (null (frame-workspace-root-pane frame))
                      (null (frame-workspace-parent-pane frame))
                      (not (equal signature
                                  (frame-window-layout-signature frame)))))))
      (when needs-rebuild-p
        (let ((pane-table (make-hash-table :test #'eql)))
          (clim:with-look-and-feel-realization ((clim:frame-manager frame) frame)
            (let ((root (mcclim-build-workspace-root tree pane-table)))
              (setf (frame-workspace-syncing-p frame) t)
              (unwind-protect
                  (when (mcclim-install-workspace-root frame root)
                    (setf (frame-window-pane-table frame) pane-table
                          (frame-window-layout-signature frame) signature))
                (setf (frame-workspace-syncing-p frame) nil))))))))
  (mcclim-sync-esa-windows frame)
  frame)

(defmethod clim:find-pane-named ((frame clawmacs-gui) pane-name)
  (if (eq pane-name 'main-pane)
      (or (frame-selected-window-pane frame)
          (call-next-method))
      (call-next-method)))

(defmethod clim:frame-standard-input ((frame clawmacs-gui))
  (or (and (mcclim-buffer-uses-shared-input-pane-p
            (frame-visible-buffer frame))
           (frame-drei-input-pane frame))
      (frame-selected-window-pane frame)
      (call-next-method)))

(defmethod clim:frame-standard-output ((frame clawmacs-gui))
  (or (frame-selected-window-pane frame)
      (call-next-method)))

(defun mcclim-focus-input-pane (frame)
  "Move keyboard focus to FRAME's Drei input pane when it exists."
  (let ((pane (and (mcclim-buffer-uses-shared-input-pane-p
                    (frame-visible-buffer frame))
                   (frame-drei-input-pane frame))))
    (when pane
      (ignore-errors
        (clim:stream-set-input-focus pane))))
  frame)

(defun mcclim-focused-sheet (frame)
  "Return FRAME's currently focused sheet, or NIL."
  (ignore-errors
    (clim:port-keyboard-input-focus (clim:port frame))))

(defun mcclim-focused-drei-input-pane (frame)
  "Return FRAME's focused shared Drei input pane, or NIL."
  (let ((focused (mcclim-focused-sheet frame)))
    (and focused
         (mcclim-sheet-ancestor-of-type focused 'clawmacs-drei-input-pane))))

(defun mcclim-clawmacs-command-table ()
  "Return Clawmacs' primary McCLIM command table object."
  (clim:find-command-table 'clawmacs-mcclim-command-table))

(defmethod esa:find-applicable-command-table ((frame clawmacs-gui))
  (declare (ignore frame))
  (mcclim-clawmacs-command-table))

(defmethod esa:minibuffer ((frame clawmacs-gui))
  "Return Clawmacs' hidden ESA service minibuffer pane."
  (clim:find-pane-named frame 'minibuffer-pane))

(defmethod esa:buffers ((frame clawmacs-gui))
  (declare (ignore frame))
  *buffer-ring*)

(defmethod esa:esa-current-buffer ((frame clawmacs-gui))
  (frame-visible-buffer frame))

(defmethod (setf esa:esa-current-buffer) (new-buffer (frame clawmacs-gui))
  (when (and new-buffer (member new-buffer *buffer-ring*))
    (setf (frame-display-buffer frame) new-buffer
          (frame-follow-current-buffer-p frame) nil)
    (switch-to-buffer new-buffer)
    (mcclim-set-selected-window-buffer frame new-buffer))
  new-buffer)

(defun mcclim-install-frame-command-table (frame)
  "Install the active CLIM command table on FRAME.

Presentation translators consult the frame command table for transcript and
pane objects. Keep that table stable even when the focused input pane uses
DREI's own keyboard command table."
  (setf (clim:frame-command-table frame)
        (mcclim-clawmacs-command-table))
  frame)

(defmethod esa:command-for-unbound-gestures ((frame clawmacs-gui) gestures)
  "Handle frame-level gestures that DREI and CLIM command tables do not own."
  (when gestures
    (file-debug-log "mcclim-input" "unbound gestures: ~S" gestures)
    (list 'com-clawmacs-dispatch-gestures (list 'quote gestures))))

(defmethod clim:adopt-frame :after (frame-manager (frame clawmacs-gui))
  (declare (ignore frame-manager))
  (with-mcclim-frame-ui-state (frame)
    (mcclim-install-frame-command-table frame)
    (mcclim-bind-workspace-parent frame)
    (mcclim-ensure-window-tree frame)
    (mcclim-sync-drei-from-buffer frame :force-p t)
    (mcclim-sync-main-pane-to-buffer-scroll frame)
    (mcclim-focus-input-pane frame)))

(defvar *mcclim-live-frames* nil
  "Application frames that should wake when shared buffer display state changes.")

(defvar *mcclim-live-frames-lock*
  (bt:make-lock "mcclim-live-frames"))

(defvar *mcclim-suppress-render-snapshot* nil
  "When non-nil, nested buffer renderers do not overwrite frame snapshots.")

(defvar *mcclim-render-window-id* nil
  "Logical window id currently being rendered, used for output-record keys.")

;;; --------------------------------------------------------------------------
;;; Render Snapshots
;;; --------------------------------------------------------------------------

(defun mcclim-render-snapshot-message (message)
  "Return a JSON-ready summary of MESSAGE as rendered by McCLIM."
  `((:sender . ,(string-downcase (symbol-name (message-sender message))))
    (:text . ,(message-text message))
    (:entry-id . ,(or (message-entry-id message) ""))))

(defun mcclim-record-render-snapshot
    (frame pane buf mode rows cols &key input-start-row history-height
                                  visible-messages windows)
  "Record the latest actual McCLIM display pass for e2e observation."
  (unless *mcclim-suppress-render-snapshot*
    (multiple-value-bind (pixel-width pixel-height)
        (pane-viewport-pixel-size pane)
      (setf (frame-last-render-snapshot frame)
            `((:ready . t)
              (:sequence . ,(incf (frame-render-sequence frame)))
              (:mode . ,(string-downcase (symbol-name mode)))
              (:buffer-name . ,(if buf (buffer-name buf) ""))
              (:rows . ,rows)
              (:cols . ,cols)
              (:pixel-width . ,pixel-width)
              (:pixel-height . ,pixel-height)
              (:input-start-row . ,(or input-start-row -1))
              (:history-height . ,(or history-height -1))
              (:visible-messages
               . ,(coerce (mapcar #'mcclim-render-snapshot-message
                                   (or visible-messages nil))
                          'vector))
              (:windows . ,(coerce (or windows nil) 'vector))))))
  frame)

;;; --------------------------------------------------------------------------
;;; Drawing Primitives
;;; --------------------------------------------------------------------------

;;; Character-cell metric probing was part of the removed fixed-grid renderer.



;;; Direct pixel drawing is intentionally absent here.  The McCLIM UI renders
;;; through panes, stream output, formatted output, output records, and
;;; presentations.

;;; --------------------------------------------------------------------------
;;; Inline Image Rendering
;;; --------------------------------------------------------------------------


(defstruct mcclim-image-cache-entry
  "Cached McCLIM image pattern and source metadata."
  pattern
  (width 1 :type integer)
  (height 1 :type integer)
  (write-date 0 :type integer)
  (path "" :type string))

(defvar *mcclim-image-cache* (make-hash-table :test #'equal)
  "Cache of loaded image patterns keyed by truename.")

(defun mcclim-resolve-display-image-path (path)
  "Resolve PATH as an existing sandbox-local image pathname."
  (let ((resolved (validate-sandbox-path path)))
    (or (probe-file resolved)
        (error "Image file not found: ~A" path))))

(defun mcclim-load-display-image-reference (reference)
  "Return cached image entry for REFERENCE, or NIL and an error string."
  (handler-case
      (let* ((pathname (mcclim-resolve-display-image-path
                        (display-image-reference-path reference)))
             (true-path (truename pathname))
             (key (namestring true-path))
             (write-date (or (file-write-date true-path) 0))
             (cached (gethash key *mcclim-image-cache*)))
        (if (and cached
                 (= write-date
                    (mcclim-image-cache-entry-write-date cached)))
            (values cached nil)
            (let* ((pattern (clim:make-pattern-from-bitmap-file true-path))
                   (width (max 1 (floor (clim:pattern-width pattern))))
                   (height (max 1 (floor (clim:pattern-height pattern))))
                   (entry (make-mcclim-image-cache-entry
                           :pattern pattern
                           :width width
                           :height height
                           :write-date write-date
                           :path key)))
              (setf (gethash key *mcclim-image-cache*) entry)
              (values entry nil))))
    (error (condition)
      (values nil (format nil "~A" condition)))))


(defun mcclim-inline-image-caption (reference entry error-text)
  "Return the one-line caption for an inline image block."
  (let* ((alt (display-image-reference-alt reference))
         (path (display-image-reference-path reference))
         (label (if (blank-string-p alt) "image" alt)))
    (cond
      (error-text
       (format nil "[image: ~A] ~A" label error-text))
      (entry
       (format nil "[image: ~A] ~A (~Dx~D)"
               label
               (mcclim-image-cache-entry-path entry)
               (mcclim-image-cache-entry-width entry)
               (mcclim-image-cache-entry-height entry)))
      (t
       (format nil "[image: ~A] ~A" label path)))))

(defun mcclim-image-block-visual-height (reference width prefix-len char-w char-h)
  "Return transcript rows consumed by an image reference presentation."
  (declare (ignore reference width prefix-len char-w char-h))
  1)

(defun mcclim-message-visual-height
    (msg width char-w char-h &key (prefix (message-sender-prefix msg))
       show-reasoning-p show-metadata-p render-images-p)
  "Return a coarse output-record row count for MSG.

CLIM owns line wrapping; this only counts logical displayed blocks for legacy
scroll metadata."
  (declare (ignore width char-w char-h prefix))
  (length (if render-images-p
              (message-display-blocks
               msg
               :show-reasoning-p show-reasoning-p
               :show-metadata-p show-metadata-p)
              (message-display-line-entries
               msg
               :show-reasoning-p show-reasoning-p
               :show-metadata-p show-metadata-p))))

(defun mcclim-region-size (region)
  "Return REGION's bounded pixel width and height, or NIL values."
  (handler-case
      (values (max 1 (floor (clim:bounding-rectangle-width region)))
              (max 1 (floor (clim:bounding-rectangle-height region))))
    (error ()
      (values nil nil))))

(defun mcclim-region-position (region)
  "Return REGION's bounded minimum x/y position, or NIL values."
  (handler-case
      (values (floor (clim:bounding-rectangle-min-x region))
              (floor (clim:bounding-rectangle-min-y region)))
    (error ()
      (values nil nil))))

(defun mcclim-sheet-visible-region-candidates (sheet)
  "Return bounded region candidates that may describe SHEET's visible extent."
  (remove nil
          (list (ignore-errors (clim:sheet-device-region sheet))
                (ignore-errors (clim:sheet-native-region sheet))
                (ignore-errors (clim:window-viewport sheet))
                (ignore-errors (clim:pane-viewport-region sheet))
                (ignore-errors (clim:sheet-region sheet)))))

(defun mcclim-effective-visible-region (pane)
  "Return the smallest bounded visible region found for PANE or its ancestors.

For panes inside a CLIM scroller, the application pane itself often retains its
full content width while an ancestor sheet carries the clipped viewport size.
Using the smallest bounded ancestor region tracks the actual visible extent."
  (let ((best-region nil)
        (best-area nil))
    (loop :for current := pane :then (ignore-errors (clim:sheet-parent current))
          :while current
          :do (dolist (region (mcclim-sheet-visible-region-candidates current))
                (multiple-value-bind (width height)
                    (mcclim-region-size region)
                  (when (and width height)
                    (let ((area (* width height)))
                      (when (or (null best-area) (< area best-area))
                        (setf best-region region
                              best-area area)))))))
    best-region))

(defun mcclim-pane-viewport-region (pane)
  "Return PANE's visible region for layout and render measurements.

Prefer the pane's currently visible native/device region before the stream
viewport object. After an external resize McCLIM updates those native regions
first, while WINDOW-VIEWPORT may still describe the previous output history
until the next normal CLIM redisplay pass."
  (or (mcclim-effective-visible-region pane)
      (ignore-errors (clim:sheet-device-region pane))
      (ignore-errors (clim:sheet-native-region pane))
      (ignore-errors (clim:window-viewport pane))
      (ignore-errors (clim:pane-viewport-region pane))
      (ignore-errors (clim:sheet-region pane))))

(defun mcclim-pane-native-region (pane)
  "Return PANE's visible region in native coordinates, or NIL."
  (or (mcclim-effective-visible-region pane)
      (ignore-errors (clim:sheet-device-region pane))
      (ignore-errors (clim:sheet-native-region pane))
      (mcclim-pane-viewport-region pane)))

(defun pane-pixel-size (pane)
  "Return (values width height) — the allocated pixel size of PANE."
  (multiple-value-bind (width height)
      (mcclim-region-size (mcclim-pane-viewport-region pane))
    (values (or width 1)
            (or height 1))))

(defun pane-grid-dimensions (pane char-w char-h)
  "Return (values cols rows) — the character grid size of PANE."
  (multiple-value-bind (width height) (pane-pixel-size pane)
    (values (max 1 (floor width char-w))
            (max 1 (floor height char-h)))))

(defun pane-viewport-pixel-size (pane)
  "Return (values width height) for PANE's visible viewport in pixels."
  (multiple-value-bind (width height)
      (mcclim-region-size (mcclim-pane-viewport-region pane))
    (values (or width 1)
            (or height 1))))

(defun pane-viewport-grid-dimensions (pane char-w char-h)
  "Return (values cols rows) for the visible viewport of PANE."
  (multiple-value-bind (width height) (pane-viewport-pixel-size pane)
    (values (max 1 (floor width char-w))
            (max 1 (floor height char-h)))))

(defun pane-viewport-grid-origin (pane char-w char-h)
  "Return (values col row) for the visible viewport origin of PANE."
  (multiple-value-bind (x y)
      (mcclim-region-position (mcclim-pane-viewport-region pane))
    (values (max 0 (floor (or x 0) char-w))
            (max 0 (floor (or y 0) char-h)))))

(defun mcclim-window-viewport-position* (pane)
  "Return PANE's viewport position, or NIL values when unavailable."
  (handler-case
      (clim:window-viewport-position pane)
    (error ()
      (values nil nil))))

(defun mcclim-transcript-history-messages (buf)
  "Return the visible finalized transcript messages for BUF."
  (let ((messages nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))))
          :do (when (message-visible-for-buffer-p msg buf)
                (push msg messages)))
    (nreverse messages)))

;;; Transcript row layout is delegated to CLIM output records and scrollers.

(defun mcclim-buffer-display-messages (buf)
  "Return the visible finalized messages for BUF's main-pane rendering."
  (mcclim-transcript-history-messages buf))

(defun mcclim-approval-prompt-rows (approval)
  "Return the transcript row count needed for APPROVAL."
  (let ((rows 0))
    (incf rows)
    (incf rows (length (split-string-by-newline
                        (or (cdr (assoc :display-raw approval)) ""))))
    (incf rows (length (split-string-by-newline
                        (or (cdr (assoc :display-expanded approval)) ""))))
    (let ((extra (cdr (assoc :display-extra approval))))
      (when extra
        (incf rows (length (split-string-by-newline extra)))))
    (+ rows 2)))

;;; Custom transcript row counts and visible-slice calculations were removed.

(defun mcclim-native-stream-buffer-p (buf)
  "Return true when BUF should render through a native CLIM stream/scroller."
  (and buf
       (or (eq (buffer-kind buf) :chat)
           (document-buffer-p buf)
           (help-buffer-p buf)
           (info-buffer-p buf)
           (customize-buffer-p buf)
           (listener-buffer-p buf))))

(defun mcclim-native-transcript-scroll-p (frame &optional (buf (frame-visible-buffer frame)))
  "Return true when BUF in FRAME should use a native CLIM scroller."
  (declare (ignore frame))
  (mcclim-native-stream-buffer-p buf))

;;; Native CLIM scrollers own content extents.

(defun mcclim-selector-pane-target-rows ()
  "Return the desired row count for the selector pane."
  (let* ((items (cond
                  (*session-tree-selector-active*
                   *session-tree-selector-filtered-items*)
                  (*buffer-selector-active*
                   *buffer-ring*)
                  (*model-selector-active*
                   *model-selector-entries*)
                  (*think-selector-active*
                   *think-selector-entries*)
                  (t nil)))
         (count (length items)))
    (max 8 (min 20 (+ 7 count)))))

(defun mcclim-completion-pane-target-rows ()
  "Return the desired row count for the completion pane."
  (let* ((mode (mcclim-completion-popup-mode))
         (items (and mode
                     (ecase mode
                       (:minibuffer *minibuffer-filtered-items*)
                       (:slash *slash-completion-filtered-items*)
                       (:skill *skill-completion-filtered-items*))))
         (total (length items))
         (max-height (and mode
                          (ecase mode
                            (:minibuffer *minibuffer-max-height*)
                            (:slash *slash-completion-max-height*)
                            (:skill *skill-completion-max-height*))))
         (item-rows (min (if (eq mode :minibuffer)
                             total
                             (max 1 total))
                         (or max-height 12))))
    (max 2 (1+ item-rows))))

(defun mcclim-sync-buffer-scroll-from-main-pane (frame)
  "Native CLIM scrolling owns transcript viewport state."
  frame)

(defun mcclim-sync-main-pane-to-buffer-scroll (frame)
  "Native CLIM scrolling owns transcript viewport state."
  (declare (ignore frame))
  nil)

(defun mcclim-handle-native-page-scroll (frame key)
  "Let normal key handling and native CLIM scrolling process page keys."
  (declare (ignore frame key))
  nil)

(declaim (special *clawmacs-frame*))

(defun mcclim-primary-frame-p (frame)
  "Return true when FRAME is the primary interactive Clawmacs frame."
  (and (boundp '*clawmacs-frame*)
       (eq frame *clawmacs-frame*)))

(defun mcclim-frame-fallback-buffer (frame)
  "Return the buffer to use when FRAME has no selected live window buffer."
  (or (and (frame-follow-current-buffer-p frame)
           (current-buffer))
      (frame-display-buffer frame)
      (current-buffer)
      (ensure-scratch-buffer)))

(defun mcclim-sync-esa-windows (frame)
  "Keep ESA's frame window slot pointed at CLIM panes.

ESA records command state on the current window and uses the first window as
`*standard-output*' in its top level, so this slot must contain McCLIM panes
rather than Clawmacs' logical application windows."
  (let* ((panes (frame-window-panes-in-display-order frame))
         (selected (frame-selected-window-pane frame)))
    (when panes
      (setf (esa:windows frame)
            (if (and selected (member selected panes :test #'eq))
                (cons selected (remove selected panes :test #'eq))
                panes)))))

(defun mcclim-ensure-window-tree (frame)
  "Ensure FRAME has a live logical window tree and selected window id."
  (let ((fallback (mcclim-frame-fallback-buffer frame)))
    (unless (frame-window-tree frame)
      (let* ((tree (make-clawmacs-window-tree fallback))
             (window (first (clawmacs-window-tree-windows tree))))
        (setf (frame-window-tree frame) tree
              (frame-selected-window-id frame) (clawmacs-window-id window))))
    (let* ((tree (frame-window-tree frame))
           (windows (clawmacs-window-tree-windows tree)))
      (clawmacs-window-tree-replace-dead-buffers tree *buffer-ring* fallback)
      (unless (clawmacs-window-tree-find-window
               tree (frame-selected-window-id frame))
        (let ((first-window (first windows)))
          (when first-window
            (setf (frame-selected-window-id frame)
                  (clawmacs-window-id first-window)))))
      (mcclim-sync-workspace-from-window-tree frame)
      tree)))

(defun frame-selected-window (frame)
  "Return FRAME's selected logical window."
  (let ((tree (mcclim-ensure-window-tree frame)))
    (or (clawmacs-window-tree-find-window tree (frame-selected-window-id frame))
        (first (clawmacs-window-tree-windows tree)))))

(defun frame-window-buffers (frame)
  "Return the buffers displayed by FRAME's logical windows."
  (remove-duplicates
   (remove nil
           (mapcar #'clawmacs-window-buffer
                   (clawmacs-window-tree-windows
                    (mcclim-ensure-window-tree frame))))
   :test #'eq))

(defun frame-visible-buffer (frame)
  "Return the buffer FRAME should display."
  (let ((window (frame-selected-window frame)))
    (or (and window (clawmacs-window-buffer window))
        (mcclim-frame-fallback-buffer frame))))

(defun mcclim-set-selected-window-buffer (frame buffer)
  "Set FRAME's selected logical window to display BUFFER."
  (when (and frame buffer (member buffer *buffer-ring*))
    (let ((window (frame-selected-window frame)))
      (when window
        (setf (clawmacs-window-buffer window) buffer
              (frame-display-buffer frame) buffer
              (frame-follow-current-buffer-p frame) nil)
        (unless (eq (current-buffer) buffer)
          (switch-to-buffer buffer))
        (mcclim-sync-esa-windows frame)
        (mcclim-sync-drei-from-buffer frame :force-p t)
        (mcclim-sync-main-pane-to-buffer-scroll frame)
        (mcclim-focus-input-pane frame)
        buffer))))

(defun mcclim-select-window (frame window)
  "Select WINDOW in FRAME and make its buffer the current buffer."
  (let* ((tree (mcclim-ensure-window-tree frame))
         (window-id (etypecase window
                      (clawmacs-window (clawmacs-window-id window))
                      (integer window)))
         (live-window (clawmacs-window-tree-find-window tree window-id)))
    (when live-window
      (setf (frame-selected-window-id frame) window-id)
      (mcclim-set-selected-window-buffer
       frame (or (clawmacs-window-buffer live-window)
                 (mcclim-frame-fallback-buffer frame)))
      live-window)))

(defun mcclim-select-window-pane (frame pane)
  "Select transcript PANE's logical window in FRAME."
  (let ((window (transcript-pane-window frame pane)))
    (when window
      (mcclim-select-window frame window))))

(defun mcclim-sync-selected-window-from-current-buffer (frame previous-buffer)
  "Update FRAME's selected logical window after a command changed current buffer."
  (mcclim-ensure-window-tree frame)
  (let ((current (current-buffer)))
    (cond
      ((and current (not (eq current previous-buffer)))
       (mcclim-set-selected-window-buffer frame current))
      ((not (member (frame-visible-buffer frame) *buffer-ring* :test #'eq))
       (mcclim-set-selected-window-buffer
        frame (mcclim-frame-fallback-buffer frame))))))

(defun mcclim-split-selected-window (frame orientation)
  "Split FRAME's selected logical window with ORIENTATION."
  (let* ((tree (mcclim-ensure-window-tree frame))
         (selected (frame-selected-window frame))
         (new-window (and selected
                          (split-clawmacs-window-tree
                           tree
                           (clawmacs-window-id selected)
                           orientation))))
    (when new-window
      (mcclim-sync-workspace-from-window-tree frame)
      (mcclim-sync-esa-windows frame)
      (notify-buffer-display-change (clawmacs-window-buffer selected)
                                    :windows))
    new-window))

(defun mcclim-delete-selected-window (frame)
  "Delete FRAME's selected logical window."
  (let* ((tree (mcclim-ensure-window-tree frame))
         (selected (frame-selected-window frame)))
    (when selected
      (multiple-value-bind (new-tree replacement deleted-p)
          (delete-clawmacs-window-from-tree
           tree (clawmacs-window-id selected))
        (setf (frame-window-tree frame) new-tree)
        (mcclim-sync-workspace-from-window-tree frame)
        (when replacement
          (setf (frame-selected-window-id frame)
                (clawmacs-window-id replacement))
          (mcclim-set-selected-window-buffer
           frame (clawmacs-window-buffer replacement)))
        (when deleted-p
          (mcclim-sync-esa-windows frame)
          (notify-buffer-display-change (frame-visible-buffer frame)
                                        :windows))
        deleted-p))))

(defun mcclim-delete-other-windows (frame)
  "Delete every logical window in FRAME except the selected window."
  (let* ((tree (mcclim-ensure-window-tree frame))
         (selected (frame-selected-window frame)))
    (when selected
      (multiple-value-bind (new-tree replacement deleted-p)
          (delete-other-clawmacs-windows tree (clawmacs-window-id selected))
        (declare (ignore replacement))
        (setf (frame-window-tree frame) new-tree
              (frame-selected-window-id frame)
              (clawmacs-window-id selected))
        (mcclim-sync-workspace-from-window-tree frame)
        (mcclim-sync-esa-windows frame)
        (when deleted-p
          (notify-buffer-display-change (frame-visible-buffer frame)
                                        :windows))
        deleted-p))))

(defun mcclim-select-other-window (frame)
  "Select the next logical window in FRAME."
  (let* ((tree (mcclim-ensure-window-tree frame))
         (next (clawmacs-window-tree-next-window
                tree (frame-selected-window-id frame))))
    (when next
      (mcclim-select-window frame next))))

(defun frame-drei-input-pane (frame)
  "Return FRAME's Drei-backed input pane, if it exists."
  (clim:find-pane-named frame 'input-pane))

(defun mcclim-drei-pane-text (pane)
  "Return the current string stored in PANE's Drei buffer."
  (handler-case
      (let* ((view (drei:current-view pane))
             (buffer (and view (drei:buffer view))))
        (if buffer
            (coerce (drei-buffer:buffer-sequence
                     buffer 0 (drei-buffer:size buffer))
                    'string)
            ""))
    (error ()
      "")))

(defun mcclim-drei-pane-point-offset (pane)
  "Return PANE's Drei point offset, or zero when unavailable."
  (handler-case
      (let ((view (drei:current-view pane)))
        (if view
            (drei-buffer:offset (drei:point view))
            0))
    (error ()
      0)))

(defun mcclim-message-point-absolute-offset (message)
  "Return MESSAGE point as a character offset in `message-text' coordinates."
  (message-point-absolute-offset message))

(defun mcclim-set-message-point-from-absolute-offset (message offset)
  "Move MESSAGE point to absolute text OFFSET and return MESSAGE."
  (set-message-point-from-absolute-offset message offset))

(defun mcclim-sync-drei-point-from-buffer (frame)
  "Copy the visible buffer's message point into FRAME's Drei input pane."
  (when frame
    (let ((pane (frame-drei-input-pane frame))
          (buf (frame-visible-buffer frame)))
      (when (and pane buf)
        (handler-case
            (let* ((view (drei:current-view pane))
                   (point (and view (drei:point view)))
                   (offset (mcclim-message-point-absolute-offset
                            (buffer-input-message buf))))
              (when point
                (setf (drei-buffer:offset point)
                      (max 0 (min offset
                                  (length (mcclim-drei-pane-text pane)))))))
          (error ()
            nil))))))

;;; Pixel-to-message point mapping was removed with the hand-rolled transcript
;;; grid renderer.  Native Drei input editing owns point movement.

(defun (setf mcclim-drei-pane-text) (text pane)
  "Replace PANE's Drei buffer with TEXT."
  (let* ((view (drei:current-view pane))
         (buffer (and view (drei:buffer view))))
    (when buffer
      (drei:performing-drei-operations (pane :with-undo nil)
        (drei-buffer:delete-buffer-range buffer 0 (drei-buffer:size buffer))
        (drei-buffer:insert-buffer-sequence buffer 0 text))))
  text)

(defun mcclim-sync-buffer-from-drei (frame)
  "Copy the Drei input pane text into the visible Clawmacs buffer."
  (when (and frame (not (frame-syncing-drei-p frame)))
    (let ((pane (frame-drei-input-pane frame))
          (buf (frame-visible-buffer frame)))
      (when (and pane buf)
        (let ((text (mcclim-drei-pane-text pane))
              (input (buffer-input-message buf)))
          (unless (string= text (message-text input))
            (let ((point-offset (mcclim-drei-pane-point-offset pane)))
              (set-message-text input text)
              (mcclim-set-message-point-from-absolute-offset
               input point-offset))))))))

;;; Native Drei input editing owns pointer/cursor mapping; Clawmacs does not
;;; translate pane pixels into buffer offsets.

(defun mcclim-sync-drei-from-buffer (frame &key force-p)
  "Copy the visible Clawmacs buffer input into FRAME's Drei input pane."
  (when frame
    (let ((pane (frame-drei-input-pane frame))
          (buf (frame-visible-buffer frame)))
      (when (and pane buf)
        (let* ((text (message-text (buffer-input-message buf)))
               (pane-text (mcclim-drei-pane-text pane)))
          (when (or force-p
                    (not (eq buf (frame-last-drei-buffer frame)))
                    (not (string= text pane-text)))
            (unwind-protect
                 (progn
                   (setf (frame-syncing-drei-p frame) t
                         (mcclim-drei-pane-text pane) text
                         (frame-last-drei-buffer frame) buf))
              (setf (frame-syncing-drei-p frame) nil)))
          (mcclim-sync-drei-point-from-buffer frame))))))

(defmethod clim:text-size ((medium clim:basic-medium) object
                           &rest drawing-options
                           &key text-style &allow-other-keys)
  "Compatibility shim for Drei cursor metrics on McCLIM basic mediums."
  (declare (ignore medium drawing-options text-style))
  (let* ((length (typecase object
                   (character 1)
                   (string (length object))
                   (t 1)))
         (width (* length 8))
         (height 14))
    (values width height width 0 11)))

;;; Drei owns input-pane cursor rendering and redisplay.

;;; Drei owns input-pane cursor rendering and redisplay.  Clawmacs no longer
;;; overlays a custom pixel cursor or mutates Drei's private display cache.

;;; --------------------------------------------------------------------------
;;; Modeline Display
;;; --------------------------------------------------------------------------

(defun display-modeline-pane (frame pane)
  "Display function for the modeline pane."
  (with-mcclim-frame-ui-state (frame)
    (mcclim-render-modeline pane (frame-visible-buffer frame))))

(defun mcclim-selector-modeline-fields (buf)
  "Return selector-specific modeline fields, or NIL for the normal modeline."
  (cond
    (*session-tree-selector-active*
     (list (list :id :mode :text "[session-tree]")
           (list :id :buffer
                 :text (if *session-tree-selector-buffer*
                           (buffer-name *session-tree-selector-buffer*)
                           "")
                 :object *session-tree-selector-buffer*
                 :presentation-type 'buffer-ref)
           (list :id :entries
                 :text (format nil "~D entr~:@P"
                               (length *session-tree-selector-filtered-items*)))
           (list :id :filter
                 :text (format nil "filter ~(~A~)"
                               *session-tree-selector-filter-mode*))))
    (*buffer-selector-active*
     (list (list :id :mode :text "[buffer-selector]")
           (list :id :title :text "Agent Sessions")
           (list :id :entries
                 :text (format nil "~D session~:[s~;~]"
                               (length *buffer-ring*)
                               (= (length *buffer-ring*) 1)))))
    (*model-selector-active*
     (list (list :id :mode :text "[model-selector]")
           (list :id :provider-model
                 :text (resolve-modeline-provider-model buf))
           (list :id :entries
                 :text (format nil "~D model~:[s~;~] available"
                               (length *model-selector-entries*)
                               (= (length *model-selector-entries*) 1)))))
    (*think-selector-active*
     (list (list :id :mode :text "[think-selector]")
           (list :id :provider-model
                 :text (resolve-modeline-provider-model buf))
           (list :id :entries
                 :text (format nil "~D level~:[s~;~] available"
                               (length *think-selector-entries*)
                               (= (length *think-selector-entries*) 1)))))
    (t nil)))

(defun mcclim-render-modeline (pane buf)
  "Render the modeline as native CLIM stream output with presentations."
  (let* ((fields (or (mcclim-selector-modeline-fields buf)
                     (modeline-field-data
                      buf :major-mode (if buf (buffer-major-mode buf) ""))))
         (cache-value (mapcar (lambda (field)
                                (list (getf field :id)
                                      (getf field :text)
                                      (getf field :object)
                                      (getf field :presentation-type)))
                              fields)))
    (clim:updating-output (pane :unique-id 'modeline-content
                                :cache-value cache-value
                                :cache-test #'equal)
      (multiple-value-bind (fg _bg ts opts)
          (resolve-face-inks (resolve-face (make-modeline-face)))
        (declare (ignore _bg))
        (apply #'clim:invoke-with-drawing-options
               pane
               (lambda (medium)
                 (declare (ignore medium))
                 (loop :for field :in fields
                       :for first-p := t :then nil
                       :do (unless first-p
                             (write-string " | " pane))
                           (mcclim-present-who-line-field pane field)))
               :ink fg
               :text-style ts
               opts)))))

;;; --------------------------------------------------------------------------
;;; Who-Line Display
;;; --------------------------------------------------------------------------

(defun mcclim-present-who-line-field (pane field)
  "Present who-line FIELD in PANE, using a CLIM presentation when available."
  (let ((text (getf field :text))
        (object (getf field :object))
        (presentation-type (getf field :presentation-type)))
    (if (and object presentation-type)
        (clim:with-output-as-presentation (pane object presentation-type
                                               :single-box t)
          (write-string text pane))
        (write-string text pane))))

(defun display-who-line-pane (frame pane)
  "Display function for the who-line pane.

Fields are written as ordinary CLIM stream output and semantic fields are
presented as CLIM presentations instead of being flattened into one prebuilt
string."
  (with-mcclim-frame-ui-state (frame)
    (let* ((buf (frame-visible-buffer frame))
           (fields (who-line-field-data buf))
           (cache-value (mapcar (lambda (field)
                                  (list (getf field :id)
                                        (getf field :text)
                                        (getf field :object)
                                        (getf field :presentation-type)))
                                fields)))
      (clim:updating-output (pane :unique-id 'who-line-content
                                  :cache-value cache-value
                                  :cache-test #'equal)
        (multiple-value-bind (wl-fg _wl-bg wl-ts wl-opts)
            (resolve-global-face-inks :who-line)
          (declare (ignore _wl-bg))
          (apply #'clim:invoke-with-drawing-options
                 pane
                 (lambda (medium)
                   (declare (ignore medium))
                   (loop :for field :in fields
                         :for first-p := t :then nil
                         :do (unless first-p
                               (write-string "  " pane))
                             (mcclim-present-who-line-field pane field)))
                 :ink wl-fg
                 :text-style wl-ts
                 wl-opts))))))

;;; --------------------------------------------------------------------------
;;; Main Pane Display
;;; --------------------------------------------------------------------------

(defun mcclim-message-cache-value
    (msg screen-row width show-reasoning-p show-metadata-p)
  "Return a stable cache value for MSG's CLIM output record."
  (list (message-entry-id msg)
        (message-sender msg)
        (message-text msg)
        (message-metadata msg)
        (message-raw-content msg)
        screen-row
        width
        show-reasoning-p
        show-metadata-p))

(defun mcclim-rendered-message-presentation-type (msg)
  "Return the presentation type used for transcript MSG."
  (cond
    ((eq :tool-result (message-sender msg))
     'tool-result)
    ((tool-call-message-p msg)
     'tool-call)
    (t
     'chat-message)))

(defun display-main-pane (frame pane)
  "Display function for the main pane."
  (with-mcclim-frame-ui-state (frame)
    (let ((buf (transcript-pane-buffer frame pane)))
      (mcclim-ensure-window-tree frame)
      (when buf
        (mcclim-render-buffer pane buf 100 100 1 1)))))

(defun display-selector-pane (frame pane)
  "Display function for the dedicated selector pane."
  (declare (ignore frame))
  (when (mcclim-selector-pane-active-p)
    (cond
      (*session-tree-selector-active*
       (mcclim-render-session-tree-selector pane 50 100 1 1 nil))
      (*buffer-selector-active*
       (mcclim-render-buffer-selector pane 50 100 1 1 clim:*application-frame*))
      (*model-selector-active*
       (mcclim-render-model-selector pane 50 100 1 1 nil))
      (*think-selector-active*
       (mcclim-render-think-selector pane 50 100 1 1 nil)))))

(defun display-completion-pane (frame pane)
  "Display function for the dedicated completion pane."
  (declare (ignore frame))
  (when (mcclim-completion-pane-active-p)
    (mcclim-render-completion-popup pane 100 20 1 1)))

(defun mcclim-call-with-drawing-style (pane fg text-style drawing-options thunk)
  "Run THUNK in PANE with FG, TEXT-STYLE, and DRAWING-OPTIONS active."
  (apply #'clim:invoke-with-drawing-options
         pane
         (lambda (medium)
           (declare (ignore medium))
           (funcall thunk))
         :ink fg
         :text-style text-style
         drawing-options))

(defun mcclim-call-with-global-face (pane face thunk)
  "Run THUNK in PANE using the resolved global FACE drawing style."
  (multiple-value-bind (fg _bg ts opts)
      (resolve-global-face-inks face)
    (declare (ignore _bg))
    (mcclim-call-with-drawing-style pane fg ts opts thunk)))

(defun mcclim-call-with-message-face (pane msg thunk)
  "Run THUNK in PANE using MSG's default drawing style."
  (let* ((face-set (message-face-set msg))
         (face (if face-set
                   (or (get-face face-set :default) (make-default-text-face))
                   (make-default-text-face)))
         (resolved (resolve-face face)))
    (multiple-value-bind (fg _bg ts opts _underline-p)
        (resolve-face-inks resolved)
      (declare (ignore _bg _underline-p))
      (mcclim-call-with-drawing-style pane fg ts opts thunk))))

(defun mcclim-stream-render-buffer-title (pane buf)
  "Render BUF's title as ordinary CLIM stream output."
  (mcclim-call-with-global-face
   pane
   :selector-title
   (lambda ()
     (write-string (buffer-name buf) pane)))
  (terpri pane)
  (mcclim-call-with-global-face
   pane
   :selector-separator
   (lambda ()
     (write-string (make-string (max 8 (length (buffer-name buf)))
                                :initial-element #\-)
                   pane)))
  (terpri pane)
  (terpri pane))

(defun mcclim-stream-render-text-block (pane text width prefix prefix-spaces)
  "Render TEXT to PANE as native CLIM stream output.

WIDTH and PREFIX-SPACES are accepted for renderer call compatibility; CLIM's
stream and scroll-pane machinery own wrapping."
  (declare (ignore width prefix-spaces))
  (let ((first-output-p t))
    (dolist (line (split-string-by-newline text))
      (write-string (if first-output-p prefix "") pane)
      (write-string line pane)
      (terpri pane)
      (setf first-output-p nil))
    (when first-output-p
      (write-string prefix pane)
      (terpri pane))))

(defun mcclim-stream-render-image-block (pane reference width prefix prefix-len char-w char-h)
  "Render image REFERENCE as a semantic text presentation.

Inline raster drawing is intentionally sacrificed; CLIM records the image
reference as a presentation and the UI stays in the native stream/output-record
path."
  (declare (ignore prefix-len char-w char-h))
  (let* ((prefix-spaces (make-string (length prefix) :initial-element #\Space)))
    (multiple-value-bind (entry error-text)
        (mcclim-load-display-image-reference reference)
      (let ((caption (mcclim-inline-image-caption reference entry error-text)))
        (clim:with-output-as-presentation
            (pane reference 'image-reference :single-box t)
          (mcclim-stream-render-text-block pane caption width prefix prefix-spaces)))
      entry)))

(defun mcclim-stream-render-message (pane msg width char-w char-h
                                     &key show-reasoning-p show-metadata-p
                                       (prefix (message-sender-prefix msg))
                                       (render-images-p t))
  "Render MSG to PANE as native CLIM stream output."
  (let* ((prefix-len (length prefix))
         (display-width (max 1 (- width prefix-len)))
         (prefix-spaces (make-string prefix-len :initial-element #\Space)))
    (mcclim-call-with-message-face
     pane
     msg
     (lambda ()
       (dolist (block (if render-images-p
                          (message-display-blocks
                           msg
                           :show-reasoning-p show-reasoning-p
                           :show-metadata-p show-metadata-p)
                          (mapcar (lambda (entry)
                                    (list :type :text
                                          :text (car entry)
                                          :source-line (cdr entry)))
                                  (message-display-line-entries
                                   msg
                                   :show-reasoning-p show-reasoning-p
                                   :show-metadata-p show-metadata-p))))
         (ecase (getf block :type)
           (:text
            (mcclim-stream-render-text-block pane
                                             (getf block :text)
                                             display-width
                                             prefix
                                             prefix-spaces))
           (:image
            (mcclim-stream-render-image-block pane
                                              (getf block :reference)
                                              width
                                              prefix
                                              prefix-len
                                              char-w
                                              char-h))))))))

(defun mcclim-stream-render-approval-prompt (pane approval width)
  "Render APPROVAL as native CLIM stream output."
  (let ((tool-name (cdr (assoc :tool-name approval)))
        (raw-sexpr (cdr (assoc :display-raw approval)))
        (expanded (cdr (assoc :display-expanded approval)))
        (extra (cdr (assoc :display-extra approval))))
    (mcclim-call-with-global-face
     pane
     :approval-header
     (lambda ()
       (write-string (format nil "-- PERMISSION REQUIRED: ~A --" tool-name)
                     pane)))
    (terpri pane)
    (terpri pane)
    (dolist (line (split-string-by-newline (or raw-sexpr "")))
      (mcclim-call-with-global-face
       pane
       :approval-raw
       (lambda ()
         (write-string line pane)))
      (terpri pane))
    (when expanded
      (terpri pane)
      (dolist (line (split-string-by-newline expanded))
        (mcclim-call-with-global-face
         pane
         :approval-expanded
         (lambda ()
           (write-string line pane)))
        (terpri pane)))
    (when extra
      (terpri pane)
      (dolist (line (split-string-by-newline extra))
        (mcclim-call-with-global-face
         pane
         :approval-extra
         (lambda ()
           (write-string line pane)))
        (terpri pane)))
    (terpri pane)
    (mcclim-call-with-global-face
     pane
     :approval-options
     (lambda ()
       (write-string "[a]pprove  [d]eny  [m]essage" pane)))
    (terpri pane)))

(defun mcclim-stream-render-styled-entry-list (pane entries)
  "Render styled ENTRIES as CLIM stream output."
  (dolist (entry entries)
    (let ((text (or (getf entry :text) ""))
          (face (or (getf entry :face) :default-text))
          (object (getf entry :object))
          (presentation-type (getf entry :presentation-type)))
      (mcclim-call-with-global-face
       pane
       face
       (lambda ()
         (if presentation-type
             (clim:with-output-as-presentation
                 (pane object presentation-type :single-box t)
               (write-string text pane))
             (write-string text pane))))
      (terpri pane))))

;;; Text wrapping is delegated to CLIM streams and panes.

(defun mcclim-ruler-line-p (line)
  "Return true when LINE is a simple help underline or separator."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (and (plusp (length trimmed))
         (every (lambda (ch)
                  (or (char= ch #\=)
                      (char= ch #\-)))
                trimmed))))

(defun mcclim-help-face-for-line (line next-line index)
  "Return the global drawing-style name for one help buffer LINE."
  (cond
    ((zerop index) :selector-title)
    ((mcclim-ruler-line-p line) :selector-separator)
    ((and next-line (mcclim-ruler-line-p next-line)) :selector-title)
    ((blank-string-p line) :default-text)
    ((and (plusp (length line))
          (not (find (char line 0) '(#\Space #\Tab))))
     :selector-header)
    (t :default-text)))

(defun mcclim-help-display-entries (buf cols)
  "Return styled entries for the dedicated help buffer presentation."
  (declare (ignore cols))
  (let ((lines (split-string-by-newline (help-buffer-text buf)))
        (entries nil))
    (loop :for line :in lines
          :for index :from 0
          :for tail :on lines
          :for next-line := (second tail)
          :for face := (mcclim-help-face-for-line line next-line index)
          :do (push (list :text line
                          :face face
                          :object (list :buffer buf
                                        :line index
                                        :text line)
                          :presentation-type 'help-line-ref)
                    entries))
    (nreverse entries)))

;;; Info lines are rendered as the document's native line segments. CLIM owns
;;; stream wrapping and scrolling.

(defun mcclim-info-display-rows (buf cols)
  "Return display rows for BUF's current Info document."
  (declare (ignore cols))
  (let* ((state (info-buffer-state buf))
         (document (and state (info-state-document state))))
    (copy-list (and document (info-document-lines document)))))

(defun mcclim-render-info-buffer (pane buf rows cols char-w char-h)
  "Render BUF as a dedicated Info/manual browser."
  (declare (ignore char-w char-h))
  (mcclim-stream-render-buffer-title pane buf)
  (let* ((state (info-buffer-state buf))
         (document (and state (info-state-document state)))
         (selected-index (and state
                              (info-state-selected-link-index state))))
    (dolist (row-segments (mcclim-info-display-rows buf cols))
      (dolist (segment row-segments)
        (let* ((text (or (info-segment-text segment) ""))
               (link-index (info-segment-link-index segment))
               (link (and document
                          link-index
                          (nth link-index (info-document-links document))))
               (face (cond
                       ((and link-index selected-index
                             (= link-index selected-index))
                        :selector-selected)
                       (link-index :selector-entry)
                       (t (or (info-segment-face segment) :default-text)))))
          (mcclim-call-with-global-face
           pane
           face
           (lambda ()
             (if link
                 (clim:with-output-as-presentation
                     (pane link 'info-link-ref :single-box t)
                   (write-string text pane))
                 (write-string text pane))))))
      (terpri pane)))
  (mcclim-record-render-snapshot (clim:pane-frame pane)
                                 pane
                                 buf
                                 :info-buffer
                                 rows
                                 cols
                                 :input-start-row -1
                                 :history-height (max 0 (1- rows))
                                 :visible-messages nil))

(defun mcclim-customize-field-line (face field index selected-p)
  "Return the display line for one customize FIELD."
  (let* ((label (format nil "~A:" (customize-face-field-label field)))
         (value (customize-face-field-display face field))
         (marker (if selected-p ">" " "))
         (left (format nil "~A ~2D. ~A" marker (1+ index) label)))
    (format nil "~26A ~A" left value)))

(defun mcclim-customize-preview-entries (face)
  "Return styled preview entries for FACE's resolved drawing values."
  (handler-case
      (let ((resolved (resolve-drawing-style face)))
        (list
         (list :text "Resolved CLIM drawing values"
               :face :selector-header)
         (list :text (format nil "Ink: ~A"
                             (format-clim-ink-display
                              (resolved-drawing-style-ink resolved)))
               :face :default-text)
         (list :text (format nil "Background Ink: ~A"
                             (format-clim-ink-display
                              (resolved-drawing-style-background-ink resolved)))
               :face :default-text)
         (list :text (format nil "Text Style: ~A"
                             (format-clim-text-style-display
                              (resolved-drawing-style-text-style resolved)))
               :face :default-text)
         (list :text (format nil "Drawing Options: ~A"
                             (format-drawing-options-display
                              (resolved-drawing-style-drawing-options resolved)))
               :face :default-text)
         (list :text (format nil "Underline: ~:[no~;yes~]"
                             (resolved-drawing-style-underline-p resolved))
               :face :default-text)))
    (error (condition)
      (list (list :text (format nil "Cannot resolve drawing style: ~A"
                                condition)
                  :face :system)))))

(defun mcclim-customize-display-entries (buf cols)
  "Return styled row entries for the dedicated customize buffer presentation."
  (declare (ignore cols))
  (let ((state *customize-face-state*))
    (if (and state (eq buf (getf state :buffer)))
        (let* ((face (getf state :face))
               (label (getf state :label))
               (field-index (getf state :field-index))
               (entries
                 (list
                  (list :text (format nil "Customize Drawing Style: ~A" label)
                        :face :selector-title)
                  (list :text "Use C-n/C-p or click a field. RET edits; SPC toggles booleans."
                        :face :selector-footer)
                  (list :text "" :face :default-text)
                  (list :text "Fields" :face :selector-header))))
          (loop :for field :in *customize-face-fields*
                :for index :from 0
                :for selected-p := (= index field-index)
                :do (setf entries
                          (append entries
                                  (list
                                   (list
                                    :text (mcclim-customize-field-line
                                           face field index selected-p)
                                    :face (if selected-p
                                              :selector-selected
                                              :selector-entry)
                                    :object (list :buffer buf
                                                  :index index
                                                  :field field)
                                    :presentation-type
                                    'customize-field-ref)))))
          (append entries
                  (list (list :text "" :face :default-text))
                  (mcclim-customize-preview-entries face)
                  (list (list :text "" :face :default-text)
                        (list :text "C-c C-c applies. C-c C-k, C-g, or q cancels. r reverts."
                              :face :selector-footer))))
        (list
         (list :text "No active customize state for this buffer."
               :face :system)
         (list :text "Use q or C-g to close this buffer."
               :face :selector-footer)))))

(defun mcclim-render-entry-buffer
    (pane buf rows cols char-w char-h entries mode)
  "Render BUF using precomputed styled ENTRIES."
  (declare (ignore char-w char-h))
  (mcclim-stream-render-buffer-title pane buf)
  (mcclim-stream-render-styled-entry-list pane entries)
  (mcclim-record-render-snapshot (clim:pane-frame pane)
                                 pane
                                 buf
                                 mode
                                 rows
                                 cols
                                 :input-start-row -1
                                 :history-height (max 0 (1- rows))
                                 :visible-messages nil))

(defun mcclim-render-help-buffer (pane buf rows cols char-w char-h)
  "Render BUF as a dedicated read-only help buffer presentation."
  (mcclim-render-entry-buffer pane buf rows cols char-w char-h
                              (mcclim-help-display-entries buf cols)
                              :help-buffer))

(defun mcclim-render-customize-buffer (pane buf rows cols char-w char-h)
  "Render BUF as a dedicated customize buffer presentation."
  (mcclim-render-entry-buffer pane buf rows cols char-w char-h
                              (mcclim-customize-display-entries buf cols)
                              :customize-buffer))

(defun mcclim-listener-message-prefix (buf msg)
  "Return the prompt prefix used to render MSG in listener BUF."
  (cond
    ((eq (message-sender msg) :user)
     (or (message-metadata-value (message-metadata msg) :listener-prompt)
         (listener-prompt-text buf)))
    ((eq (message-sender msg) :listener)
     "")
    (t
     (message-sender-prefix msg))))

(defun mcclim-listener-message-face (msg)
  "Return the global face used for listener MSG."
  (case (message-sender msg)
    (:user :selector-header)
    (:listener :default-text)
    (:system :system)
    (otherwise :default-text)))

(defun mcclim-listener-display-entries (buf cols)
  "Return styled entries for BUF's listener transcript."
  (declare (ignore cols))
  (let ((entries nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))))
          :do (let* ((prefix (mcclim-listener-message-prefix buf msg))
                     (face (mcclim-listener-message-face msg))
                     (first-row-p t))
                (dolist (line (message-display-line-strings msg))
                  (let ((line-prefix (if first-row-p prefix "")))
                    (push (list :text (concatenate 'string line-prefix line)
                                :face face
                                :object (list :buffer buf
                                              :message msg
                                              :text line)
                                :presentation-type 'listener-entry-ref)
                          entries))
                  (setf first-row-p nil))))
    (nreverse entries)))

(defun mcclim-render-listener-buffer (pane buf rows cols char-w char-h)
  "Render BUF as a McCLIM Listener-style buffer."
  (declare (ignore char-w char-h))
  (mcclim-stream-render-buffer-title pane buf)
  (let ((visible-messages nil))
    (dolist (entry (mcclim-listener-display-entries buf cols))
      (let ((object (getf entry :object)))
        (push (getf object :message) visible-messages))
      (mcclim-stream-render-styled-entry-list pane (list entry)))
    (terpri pane)
    (mcclim-call-with-global-face
     pane
     :who-line
     (lambda ()
       (write-string (listener-wholine-text buf) pane)))
    (terpri pane)
    (mcclim-record-render-snapshot (clim:pane-frame pane)
                                   pane
                                   buf
                                   :listener-buffer
                                   rows
                                   cols
                                   :input-start-row -1
                                   :history-height (max 0 (- rows 2))
                                   :visible-messages
                                   (remove-duplicates
                                    (nreverse visible-messages)
                                    :test #'eq))))

(defun register-mcclim-core-buffer-presentations ()
  "Install McCLIM presentation functions for built-in special buffers."
  (register-buffer-type
   :help
   :description "Read-only help buffer."
   :major-mode "help"
   :presentation-function 'mcclim-render-help-buffer)
  (register-buffer-type
   :info
   :description "Read-only Info manual browser."
   :major-mode "info"
   :presentation-function 'mcclim-render-info-buffer
   :serialize-state-function 'info-serialize-buffer-state
   :restore-state-function 'info-restore-buffer-state)
  (register-buffer-type
   :customize
   :description "Interactive customization buffer."
   :major-mode "customize"
   :presentation-function 'mcclim-render-customize-buffer)
  (register-buffer-type
   :listener
   :description "Interactive Common Lisp listener buffer."
   :major-mode "listener"
   :presentation-function 'mcclim-render-listener-buffer
   :serialize-state-function 'listener-serialize-buffer-state
   :restore-state-function 'listener-restore-buffer-state)
  (register-buffer-type
   :font-editor
   :description "Interactive CADR-style bitmap font editor."
   :major-mode "font-editor"
   :presentation-function 'mcclim-render-font-editor-buffer
   :serialize-state-function 'font-editor-serialize-buffer-state
   :restore-state-function 'font-editor-restore-buffer-state))

(register-mcclim-core-buffer-presentations)

(defun mcclim-stream-render-document-buffer (pane buf rows cols char-w char-h)
  "Render document BUF in the main pane using ordinary CLIM stream output."
  (declare (ignore char-w char-h))
  (mcclim-stream-render-buffer-title pane buf)
  (mcclim-call-with-global-face
   pane
   :default-text
   (lambda ()
     (mcclim-stream-render-text-block pane
                                      (message-text (buffer-input-message buf))
                                      cols
                                      ""
                                      "")))
  (mcclim-record-render-snapshot (clim:pane-frame pane)
                                 pane
                                 buf
                                 :document-buffer
                                 rows
                                 cols
                                 :input-start-row 3
                                 :history-height (max 0 (- rows 3))
                                 :visible-messages
                                 (list (buffer-input-message buf))))

(defun mcclim-render-native-stream-buffer (frame pane buf char-w char-h)
  "Render BUF in PANE through CLIM stream output and presentations."
  (declare (ignore char-w char-h))
  (let ((presentation-function (buffer-presentation-function buf))
        (viewport-cols 100)
        (viewport-rows 100))
    (cond
      (presentation-function
       (funcall presentation-function
                pane buf viewport-rows viewport-cols 1 1))
      ((document-buffer-p buf)
       (mcclim-stream-render-document-buffer
        pane buf viewport-rows viewport-cols 1 1))
      (t
       (let* ((history-messages (mcclim-transcript-history-messages buf))
              (show-reasoning-p (buffer-show-reasoning-p buf))
              (show-metadata-p (buffer-show-metadata-p buf)))
         (mcclim-stream-render-buffer-title pane buf)
         (loop :for remaining :on history-messages
               :for msg := (car remaining)
               :do (progn
                     (clim:present msg
                                   (mcclim-rendered-message-presentation-type msg)
                                   :stream pane)
                     (when (cdr remaining)
                       (terpri pane))))
         (let ((pending-approval (buffer-approval-pending buf)))
           (when pending-approval
             (mcclim-stream-render-approval-prompt
              pane pending-approval viewport-cols)))
         (mcclim-record-render-snapshot frame
                                        pane
                                        buf
                                        :buffer
                                        viewport-rows
                                        viewport-cols
                                        :input-start-row -1
                                        :history-height -1
                                        :visible-messages
                                        history-messages))))))

(defun mcclim-render-buffer (pane buf rows cols char-w char-h)
  "Render the transcript pane: title bar at row 0, then message history.
The editable input lives in the separate Drei input pane. Approval prompts
still render in the transcript because they are modal interaction state, not
ordinary input text."
  (let* ((frame (and pane (ignore-errors (clim:pane-frame pane))))
         (presentation-function (buffer-presentation-function buf)))
    (cond
      ((and frame (mcclim-native-transcript-scroll-p frame buf))
       (mcclim-render-native-stream-buffer frame pane buf char-w char-h))
      (presentation-function
       (funcall presentation-function pane buf rows cols char-w char-h))
      (t
       (error "Buffer ~A does not provide a native McCLIM renderer."
              (buffer-name buf))))))

;;; --------------------------------------------------------------------------
;;; Buffer Selector Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-stream-write-global-face (pane face text &key (newlinep t))
  "Write TEXT to PANE using global FACE as ordinary CLIM stream output."
  (mcclim-call-with-global-face pane face
                                (lambda ()
                                  (write-string text pane)))
  (when newlinep
    (terpri pane)))

(defun mcclim-selector-visible-scroll (selected-index scroll max-visible count)
  "Return a normalized selector scroll offset."
  (cond
    ((zerop count) 0)
    ((< selected-index scroll) selected-index)
    ((>= selected-index (+ scroll max-visible))
     (max 0 (1+ (- selected-index max-visible))))
    (t scroll)))

(defun mcclim-stream-render-fuzzy-text (pane text match-positions
                                        base-face match-face)
  "Write TEXT to PANE, highlighting MATCH-POSITIONS with MATCH-FACE."
  (let ((match-set (when match-positions
                     (let ((table (make-hash-table :test #'eql)))
                       (dolist (position match-positions table)
                         (setf (gethash position table) t))))))
    (labels ((emit-span (start end matchedp)
               (when (< start end)
                 (let ((span (subseq text start end)))
                   (mcclim-stream-write-global-face
                    pane
                    (if matchedp match-face base-face)
                    span
                    :newlinep nil)))))
      (let ((limit (length text))
            (span-start 0)
            (span-match-p nil))
        (loop :for index :from 0 :below limit
              :for matchedp := (and match-set (gethash index match-set))
              :do (cond
                    ((zerop index)
                     (setf span-match-p matchedp))
                    ((not (eql (not matchedp) (not span-match-p)))
                     (emit-span span-start index span-match-p)
                     (setf span-start index
                           span-match-p matchedp))))
        (emit-span span-start limit span-match-p)))))

(defun mcclim-render-buffer-selector (pane rows cols char-w char-h frame)
  "Render the buffer selector as a CLIM presentation table."
  (declare (ignore cols char-w char-h))
  (let* ((buffers *buffer-ring*)
         (num-buffers (length buffers))
         (current (frame-visible-buffer frame))
         (max-entries (max 1 (- rows 6)))
         (scroll (mcclim-selector-visible-scroll *buffer-selector-index*
                                                 *buffer-selector-scroll*
                                                 max-entries
                                                 num-buffers)))
    (setf *buffer-selector-scroll* scroll)
    (mcclim-stream-write-global-face pane :selector-title "Agent Sessions")
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (dolist (heading '("" "Name" "Agent" "Status" "Messages"))
          (clim:formatting-cell (pane)
            (mcclim-stream-write-global-face pane :selector-header heading
                                             :newlinep nil))))
      (loop :for absolute-idx :from scroll
            :below (min (+ scroll max-entries) num-buffers)
            :for buf := (nth absolute-idx buffers)
            :for selected-p := (= absolute-idx *buffer-selector-index*)
            :for current-p := (eq buf current)
            :do (clim:with-output-as-presentation
                    (pane buf 'buffer-ref :single-box t)
                  (clim:formatting-row (pane)
                    (clim:formatting-cell (pane)
                      (write-string (cond ((and selected-p current-p) ">*")
                                          (selected-p ">")
                                          (current-p "*")
                                          (t ""))
                                    pane))
                    (clim:formatting-cell (pane) (write-string (buffer-name buf) pane))
                    (clim:formatting-cell (pane) (write-string (buffer-agent-name buf) pane))
                    (clim:formatting-cell (pane)
                      (write-string (string-downcase
                                     (symbol-name (buffer-status buf)))
                                    pane))
                    (clim:formatting-cell (pane)
                      (format pane "~D" (max 0 (1- (buffer-message-count buf)))))))))
    (when (> num-buffers max-entries)
      (format pane "~%[~D-~D of ~D]"
              (1+ scroll)
              (min (+ scroll max-entries) num-buffers)
              num-buffers))
    (format pane "~%[RET] select  [C-g/q] cancel  [n] new  [k] kill")))

;;; --------------------------------------------------------------------------
;;; Model Selector Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-model-selector (pane rows cols char-w char-h frame)
  "Render the model selector as a CLIM presentation table."
  (declare (ignore cols char-w char-h frame))
  (let* ((entries *model-selector-entries*)
         (num-entries (length entries))
         (max-visible (max 1 (- rows 6)))
         (scroll (mcclim-selector-visible-scroll *model-selector-index*
                                                 *model-selector-scroll*
                                                 max-visible
                                                 num-entries)))
    (setf *model-selector-scroll* scroll)
    (mcclim-stream-write-global-face pane :selector-title "Select Model")
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (dolist (heading '("" "Provider" "Model"))
          (clim:formatting-cell (pane)
            (mcclim-stream-write-global-face pane :selector-header heading
                                             :newlinep nil))))
      (loop :for absolute-idx :from scroll
            :below (min (+ scroll max-visible) num-entries)
            :for entry := (nth absolute-idx entries)
            :for selected-p := (= absolute-idx *model-selector-index*)
            :for active-p := (getf entry :active-p)
            :do (clim:with-output-as-presentation
                    (pane entry 'model-ref :single-box t)
                  (clim:formatting-row (pane)
                    (clim:formatting-cell (pane)
                      (write-string (cond ((and selected-p active-p) ">*")
                                          (selected-p ">")
                                          (active-p "*")
                                          (t ""))
                                    pane))
                    (clim:formatting-cell (pane)
                      (write-string (string-downcase
                                     (symbol-name (getf entry :provider)))
                                    pane))
                    (clim:formatting-cell (pane)
                      (write-string (or (getf entry :model) "") pane))))))
    (when (> num-entries max-visible)
      (format pane "~%[~D-~D of ~D]"
              (1+ scroll)
              (min (+ scroll max-visible) num-entries)
              num-entries))
    (format pane "~%[RET] select  [C-g/q] cancel  * = active")))

;;; --------------------------------------------------------------------------
;;; Think Selector Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-think-selector (pane rows cols char-w char-h frame)
  "Render the think-level selector as CLIM presentation output."
  (declare (ignore cols char-w char-h frame))
  (let* ((entries *think-selector-entries*)
         (num-entries (length entries))
         (max-visible (max 1 (- rows 6)))
         (scroll (mcclim-selector-visible-scroll *think-selector-index*
                                                 *think-selector-scroll*
                                                 max-visible
                                                 num-entries)))
    (setf *think-selector-scroll* scroll)
    (mcclim-stream-write-global-face pane :selector-title "Select Think Level")
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane)
          (mcclim-stream-write-global-face pane :selector-header "Think Level"
                                           :newlinep nil)))
      (loop :for absolute-idx :from scroll
            :below (min (+ scroll max-visible) num-entries)
            :for entry := (nth absolute-idx entries)
            :for selected-p := (= absolute-idx *think-selector-index*)
            :for active-p := (getf entry :active-p)
            :do (clim:with-output-as-presentation
                    (pane entry 'think-level-ref :single-box t)
                  (clim:formatting-row (pane)
                    (clim:formatting-cell (pane)
                      (format pane "~A ~A"
                              (cond ((and selected-p active-p) ">*")
                                    (selected-p ">")
                                    (active-p "*")
                                    (t ""))
                              (or (getf entry :display) "")))))))
    (when (> num-entries max-visible)
      (format pane "~%[~D-~D of ~D]"
              (1+ scroll)
              (min (+ scroll max-visible) num-entries)
              num-entries))
    (format pane "~%[RET] select  [C-g/q] cancel  default = clear  * = active")))

(defun mcclim-render-session-tree-selector (pane rows cols char-w char-h frame)
  "Render the session tree selector as CLIM presentation output."
  (declare (ignore cols char-w char-h frame))
  (session-tree-selector-update-filter)
  (let* ((items *session-tree-selector-filtered-items*)
         (num-items (length items))
         (max-visible (max 1 (- rows 6)))
         (scroll (mcclim-selector-visible-scroll *session-tree-selector-index*
                                                 *session-tree-selector-scroll*
                                                 max-visible
                                                 num-items)))
    (setf *session-tree-selector-scroll* scroll)
    (format pane "Session Tree: ~A~%"
            (if *session-tree-selector-buffer*
                (buffer-name *session-tree-selector-buffer*)
                ""))
    (format pane "filter:~(~A~)  search:~A~%"
            *session-tree-selector-filter-mode*
            *session-tree-selector-search*)
    (loop :for absolute-idx :from scroll
          :below (min (+ scroll max-visible) num-items)
          :for item := (nth absolute-idx items)
          :for selected-p := (= absolute-idx *session-tree-selector-index*)
          :do (clim:with-output-as-presentation
                  (pane item 'session-tree-entry-ref :single-box t)
                (format pane "~A ~A~A~A~%"
                        (if selected-p ">" " ")
                        (or (getf item :tree-prefix) "")
                        (if (getf item :active-p) "*" " ")
                        (or (getf item :label)
                            (plist-display item)
                            ""))))
    (when (> num-items max-visible)
      (format pane "[~D-~D of ~D]~%"
              (1+ scroll)
              (min (+ scroll max-visible) num-items)
              num-items))
    (write-string "[RET] select  [C-g/q] cancel  L label  <- fold  -> unfold  C-o filter"
                  pane)))

;;; --------------------------------------------------------------------------
;;; Popup Completion Overlay
;;; --------------------------------------------------------------------------

(defun mcclim-completion-popup-mode ()
  "Return the active completion popup mode keyword, or NIL."
  (cond
    (*minibuffer-active* :minibuffer)
    (*slash-completion-active* :slash)
    (*skill-completion-active* :skill)
    (t nil)))

(defun mcclim-render-completion-popup (pane cols rows char-w char-h)
  "Render the active completion list as native CLIM presentation output."
  (declare (ignore cols char-w char-h))
  (let* ((mode (mcclim-completion-popup-mode))
         (items (ecase mode
                  (:minibuffer *minibuffer-filtered-items*)
                  (:slash *slash-completion-filtered-items*)
                  (:skill *skill-completion-filtered-items*)))
         (selected (ecase mode
                     (:minibuffer *minibuffer-selected-index*)
                     (:slash *slash-completion-selected-index*)
                     (:skill *skill-completion-selected-index*)))
         (scroll (ecase mode
                   (:minibuffer *minibuffer-scroll-offset*)
                   (:slash *slash-completion-scroll-offset*)
                   (:skill *skill-completion-scroll-offset*)))
         (prompt-str (ecase mode
                       (:minibuffer (format nil "~A: " *minibuffer-prompt*))
                       (:slash "Slash: ")
                       (:skill "Skill: ")))
         (input (ecase mode
                  (:minibuffer *minibuffer-input*)
                  (:slash (format nil "/~A" *slash-completion-query*))
                  (:skill (format nil "$~A" *skill-completion-query*))))
         (no-match-text (ecase mode
                          (:minibuffer "")
                          (:slash "No matching slash commands")
                          (:skill "No matching skills")))
         (total (length items))
         (item-rows (max 0 (1- rows))))
    (format pane "~A~A~%" prompt-str input)
    (if (and (not (eq mode :minibuffer)) (zerop total) (plusp item-rows))
        (write-string no-match-text pane)
        (loop :for row-idx :from 0 :below item-rows
              :for item-idx := (+ scroll row-idx)
              :while (< item-idx total)
              :for item := (nth item-idx items)
              :for candidate-object := (list :index item-idx :item item)
              :for candidate-ptype := (ecase mode
                                       (:minibuffer 'minibuffer-candidate-ref)
                                       (:slash 'slash-candidate-ref)
                                       (:skill 'skill-candidate-ref))
              :for selected-p := (= item-idx selected)
              :do (clim:with-output-as-presentation
                      (pane candidate-object candidate-ptype :single-box t)
                    (format pane "~A ~A~%"
                            (if selected-p ">" " ")
                            (minibuffer-item-display item)))))))

;;; --------------------------------------------------------------------------
;;; Key Normalization (McCLIM-specific)
;;; --------------------------------------------------------------------------

(defun mcclim-key-name-control-character (key-name)
  "Return the ASCII control character represented by symbolic KEY-NAME.
Some X backends report control events as symbolic control names such as :ETX
or :EM rather than as characters. Normalize those to the same control
characters used by Clawmacs keymaps."
  (when (keywordp key-name)
    (let ((code (cdr (assoc key-name
                            '((:soh . 1) (:stx . 2) (:etx . 3)
                              (:eot . 4) (:enq . 5) (:ack . 6)
                              (:bel . 7) (:bs . 8) (:ht . 9)
                              (:lf . 10) (:nl . 10) (:vt . 11)
                              (:ff . 12) (:cr . 13) (:so . 14)
                              (:si . 15) (:dle . 16) (:dc1 . 17)
                              (:dc2 . 18) (:dc3 . 19) (:dc4 . 20)
                              (:nak . 21) (:syn . 22) (:etb . 23)
                              (:can . 24) (:em . 25) (:sub . 26))
                            :test #'eq))))
      (and code (code-char code)))))

(defun mcclim-letter-key-name-control-character (key-name)
  "Return the ASCII control character represented by Control+letter KEY-NAME."
  (when (keywordp key-name)
    (let* ((name (symbol-name key-name)))
      (when (= (length name) 1)
        (let ((char (char name 0)))
          (when (alpha-char-p char)
            (code-char (- (char-code (char-upcase char)) 64))))))))

(defun mcclim-modifier-key-name-p (key-name)
  "Return true when KEY-NAME names a standalone modifier key."
  (when (keywordp key-name)
    (let ((name (symbol-name key-name)))
      (or (search "SHIFT" name)
          (search "CONTROL" name)
          (search "CTRL" name)
          (search "META" name)
          (search "ALT" name)
          (search "SUPER" name)
          (search "HYPER" name)))))

(defun mcclim-alt-modifier-key-name-p (key-name)
  "Return true when KEY-NAME names a physical Alt key."
  (when (keywordp key-name)
    (search "ALT" (symbol-name key-name))))

(defun mcclim-meta-modifier-key-name-p (key-name)
  "Return true when KEY-NAME names a physical Meta key."
  (when (keywordp key-name)
    (search "META" (symbol-name key-name))))

(defun mcclim-meta-or-alt-prefix-for-key-name (key-name)
  "Return the Clawmacs prefix implied by standalone Meta/Alt KEY-NAME."
  (cond
    ((mcclim-meta-modifier-key-name-p key-name) :meta)
    ((mcclim-alt-modifier-key-name-p key-name)
     (if *alt-emulates-meta* :meta :alt))
    (t nil)))

(defun mcclim-ctrl-meta-modifiers-p (modifiers)
  "Return true when CLIM reports both Control and Meta modifiers."
  (and (plusp (logand modifiers clim:+control-key+))
       (plusp (logand modifiers clim:+meta-key+))))

(defun mcclim-consume-meta-alt-prefix (key ctrl-p)
  "Return KEY wrapped with a pending Meta/Alt prefix, if one is active."
  (let ((prefix (cond
                  (*alt-pending* :alt)
                  (*meta-pending* :meta))))
    (when prefix
      (setf *alt-pending* nil
            *meta-pending* nil)
      (if (and ctrl-p (not (characterp key)))
          (list :ctrl prefix key)
          (list prefix key)))))

(defun mcclim-normalize-key (key-event &optional buffer)
  "Normalize a McCLIM key-press-event to Clawmacs' abstract key format.
Returns a character, a keyword, a list (:meta key), (:alt key), (:ctrl-x key), etc."
  (let* ((char (clim:keyboard-event-character key-event))
         (key-name (clim:keyboard-event-key-name key-event))
         (effective-buffer (or buffer (current-buffer)))
         (modifiers (clim:event-modifier-state key-event))
         (ctrl-p (plusp (logand modifiers clim:+control-key+)))
         (meta-p (plusp (logand modifiers clim:+meta-key+)))
         (ctrl-meta-p (mcclim-ctrl-meta-modifiers-p modifiers))
         ;; Map CLIM key names to our abstract keywords
         (key (case key-name
                ((:up) :up)
                ((:down) :down)
                ((:left) :left)
                ((:right) :right)
                ((:prior :page-up) :page-up)
                ((:next :page-down) :page-down)
                ((:home) :home)
                ((:end) :end)
                ((:return :newline) #\Newline)
                ((:tab) #\Tab)
                ((:backspace :delete) :backspace)
                ((:escape) #\Esc)
                (otherwise
                 (cond
                   ;; C-SPC is set-mark in Emacs-style file buffers.
                   ((and ctrl-p char (char= char #\Space))
                    (code-char 0))
                   ;; Control + letter: produce the control character
                   ((and ctrl-p char (alpha-char-p char))
                    (code-char (- (char-code (char-upcase char)) 64)))
                   ;; Some CLX/X11 paths provide only KEY-NAME for Ctrl+letter.
                   ((and ctrl-p
                         (mcclim-letter-key-name-control-character key-name))
                    (mcclim-letter-key-name-control-character key-name))
                   ;; Other paths provide symbolic control names like :EM.
                   ((mcclim-key-name-control-character key-name)
                    (mcclim-key-name-control-character key-name))
                   ;; Regular character
                   (char char)
                   ;; Named key with no character → keyword
                   (t key-name))))))
    (cond
      ;; X11 sends standalone modifier key-presses before modified keys. They
      ;; should not self-insert or consume a pending C-x/C-c/C-h/ESC prefix.
      ;; Some X server/window-manager combinations do not preserve the Meta
      ;; modifier on the following key event, so standalone Alt/Meta records
      ;; the physical prefix for the next gesture.
      ((and (null char) (mcclim-meta-or-alt-prefix-for-key-name key-name))
       (ecase (mcclim-meta-or-alt-prefix-for-key-name key-name)
         (:meta
          (setf *meta-pending* t
                *alt-pending* nil))
         (:alt
          (setf *alt-pending* t
                *meta-pending* nil)))
       nil)
      ((and (null char) (mcclim-modifier-key-name-p key-name))
       nil)
      ;; C-h prefix — McCLIM delivers Control+h with key-name :backspace
      ;; (ASCII 8 = BS), so we must check the modifier+character explicitly
      ;; before the generic backspace handling below.
      ((and ctrl-p (characterp char) (char-equal char #\h)
            (not *meta-pending*) (not *alt-pending*)
            (not *cx-pending*) (not *cc-pending*) (not *ch-pending*))
       (setf *ch-pending* t)
       nil)
      ;; Pending Meta/Alt prefix resolution must happen before direct modifier
      ;; handling so McCLIM's collapsed Alt/Meta bit can still be separated
      ;; when a standalone physical modifier event preceded this key.
      ((mcclim-consume-meta-alt-prefix key ctrl-p))
      ;; Ctrl+Meta on named keys mirrors the old Croatoan key struct path.
      ((and ctrl-meta-p (not (characterp key)))
       (list :ctrl :meta key))
      ;; Ctrl+Backspace
      ((and ctrl-p (eq key :backspace))
       (list :ctrl :backspace))
      ;; Control on other named keys.
      ((and ctrl-p (not (characterp key)))
       (list :ctrl key))
      ;; Meta+Backspace
      ((and meta-p (eq key :backspace))
       (list :meta :backspace))
      ;; Pending prefix resolution (must come before raw prefix detection)
      (*cx-pending*
       (setf *cx-pending* nil)
       (list :ctrl-x key))
      (*cc-pending*
       (setf *cc-pending* nil)
       (list :ctrl-c key))
      (*ch-pending*
       (setf *ch-pending* nil)
       (list :ctrl-h key))
      ;; Meta delivered directly by CLIM. McCLIM's CLX backend currently
      ;; collapses Alt and Meta into this same bit; standalone modifier events
      ;; above distinguish them when possible.
      (meta-p
       (list :meta key))
      ;; ESC prefix
      ((and (characterp key) (char= key #\Esc))
       (if (or *slash-completion-active*
               *skill-completion-active*
               (and effective-buffer
                    (buffer-llm-running-p effective-buffer)))
           key
           (progn
             (setf *meta-pending* t
                   *alt-pending* nil)
             nil)))
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

(defun mcclim-pointer-scroll-key (event)
  "Return the Clawmacs scroll key represented by CLIM pointer EVENT."
  (cond
    ((typep event 'clime:pointer-scroll-event)
     (let ((delta-y (clime:pointer-event-delta-y event)))
       (cond
         ((minusp delta-y) :page-up)
         ((plusp delta-y) :page-down)
         (t nil))))
    ((typep event 'clim:pointer-button-press-event)
     (case (clim:pointer-event-button event)
       (#.clim:+pointer-wheel-up+ :page-up)
       (#.clim:+pointer-wheel-down+ :page-down)
       (otherwise nil)))
    (t nil)))

(defun mcclim-select-pointer-event-p (event)
  "Return true when EVENT is the standard CLIM select button gesture."
  (and (typep event 'clim:pointer-button-press-event)
       (or (clim:event-matches-gesture-name-p event :select)
           (eql (clim:pointer-event-button event)
                clim:+pointer-left-button+))))

(defun mcclim-sheet-ancestor-of-type (sheet type)
  "Return SHEET or the nearest ancestor satisfying TYPE, or NIL."
  (loop :for current := sheet :then (ignore-errors (clim:sheet-parent current))
        :while current
        :when (typep current type)
          :return current))

(defun mcclim-event-position-in-pane (event pane)
  "Return EVENT coordinates transformed into PANE's local coordinate space."
  (let ((sheet (clim:event-sheet event)))
    (if (eq sheet pane)
        (values (clim:pointer-event-x event)
                (clim:pointer-event-y event))
        (handler-case
            (clim:transform-position
             (clim:sheet-delta-transformation sheet pane)
             (clim:pointer-event-x event)
             (clim:pointer-event-y event))
          (error ()
            (values (clim:pointer-event-x event)
                    (clim:pointer-event-y event)))))))

(defun mcclim-completion-popup-geometry (cols rows)
  "Return popup geometry matching `mcclim-render-completion-popup'."
  (let* ((mode (mcclim-completion-popup-mode))
         (items (ecase mode
                  (:minibuffer *minibuffer-filtered-items*)
                  (:slash *slash-completion-filtered-items*)
                  (:skill *skill-completion-filtered-items*)))
         (total (length items))
         (popup-w (min (- cols 4) (max 40 (floor (* cols 3) 5))))
         (max-height (ecase mode
                       (:minibuffer *minibuffer-max-height*)
                       (:slash *slash-completion-max-height*)
                       (:skill *skill-completion-max-height*)))
         (max-item-rows (max 0 (min (or max-height 12) (- rows 4))))
         (display-total (if (eq mode :minibuffer) total (max 1 total)))
         (item-rows (min display-total max-item-rows))
         (popup-h (+ 1 item-rows))
         (popup-left (floor (- cols popup-w) 2))
         (popup-top (floor (- rows popup-h) 2)))
    (values popup-left popup-top popup-w item-rows mode total)))

(defun mcclim-handle-input-pane-click (frame pane event)
  "Synchronize state after Drei handles a mouse click in the input pane."
  (declare (ignore pane))
  (when (mcclim-select-pointer-event-p event)
    (let ((buf (frame-visible-buffer frame)))
      (when buf
        (mcclim-sync-buffer-from-drei frame)
        (sync-slash-completion buf)
        (if *slash-completion-active*
            (deactivate-skill-completion)
            (sync-skill-completion buf))
        t))))

(defun mcclim-handle-pointer-scroll (frame event)
  "Let native McCLIM scrolling handle pointer-wheel gestures."
  (declare (ignore frame event))
  nil)

(defun mcclim-live-frames ()
  "Return a stable list of active McCLIM frames."
  (bt:with-lock-held (*mcclim-live-frames-lock*)
    (copy-list *mcclim-live-frames*)))

(defun mcclim-register-frame (frame)
  "Register FRAME as a live Clawmacs McCLIM frame."
  (when (and (fboundp 'add-hook)
             (boundp '*after-buffer-display-change-hook*))
    (add-hook '*after-buffer-display-change-hook*
              #'mcclim-handle-buffer-display-change))
  (bt:with-lock-held (*mcclim-live-frames-lock*)
    (pushnew frame *mcclim-live-frames* :test #'eq))
  frame)

(defun mcclim-unregister-frame (frame)
  "Unregister FRAME from the live Clawmacs McCLIM frame set."
  (bt:with-lock-held (*mcclim-live-frames-lock*)
    (setf *mcclim-live-frames*
          (remove frame *mcclim-live-frames* :test #'eq)))
  (when (and (null (mcclim-live-frames))
             (fboundp 'remove-hook)
             (boundp '*after-buffer-display-change-hook*))
    (remove-hook '*after-buffer-display-change-hook*
                 #'mcclim-handle-buffer-display-change))
  frame)

(defun mcclim-desired-input-pane-rows (buf body-rows width)
  "Return the stable shared-input-pane row count for BUF.

This remains as a pure policy helper for tests and static geometry decisions;
it no longer drives per-keystroke relayout."
  (declare (ignore body-rows width))
  (cond
    ((null buf)
     *mcclim-chat-input-pane-rows*)
    ((not (mcclim-buffer-uses-shared-input-pane-p buf))
     0)
    ((listener-buffer-p buf)
     *mcclim-listener-input-pane-rows*)
    ((document-buffer-p buf)
     *mcclim-document-input-pane-rows*)
    (t
     *mcclim-chat-input-pane-rows*)))

(defun mcclim-normalize-gesture (gesture &optional buffer)
  "Normalize an ESA/CLIM GESTURE to Clawmacs' abstract key format."
  (cond
    ((typep gesture 'clim:key-press-event)
     (mcclim-normalize-key gesture buffer))
    ((characterp gesture)
     (case gesture
       (#\Return #\Newline)
       (otherwise gesture)))
    ((symbolp gesture)
     gesture)
    (t nil)))

(defun mcclim-dispatch-gesture (frame gesture)
  "Dispatch one ESA/CLIM GESTURE through the Clawmacs keymap."
  (with-mcclim-frame-ui-state (frame)
    (if (not (mcclim-primary-frame-p frame))
        ;; Popup viewers are read-only; do not let keyboard input mutate shared
        ;; prefix state or fall through to CLIM's input editor.
        nil
        (let* ((buf (frame-visible-buffer frame))
               (previous-buffer (current-buffer))
               (key (mcclim-normalize-gesture gesture buf)))
          (file-debug-log "mcclim-input" "gesture ~S normalized to ~S"
                          gesture key)
          (when key
            (if (mcclim-handle-native-page-scroll frame key)
                t
                (let ((result (handle-key-event buf key)))
                  (when (eq result :quit)
                    (setf (frame-quit-flag frame) t)
                    (clim:frame-exit frame))
                  (when (eq result :redraw)
                    t))))
          (mcclim-sync-selected-window-from-current-buffer
           frame previous-buffer)
          nil))))

(defun mcclim-process-background-updates (frame)
  "Apply pending streaming/OAuth updates relevant to FRAME.
Returns true when application state may have changed."
  (when (mcclim-primary-frame-p frame)
    (let ((changed-p nil))
      (dolist (buf (frame-window-buffers frame))
        (when (buffer-pending-stream buf)
          (when (update-streaming-response buf)
            (setf changed-p t))
          (unless (buffer-pending-stream buf)
            (setf changed-p t))))
      (when *openai-oauth-pending*
        (when (update-openai-oauth-login)
          (setf changed-p t))
        (unless *openai-oauth-pending*
          (setf changed-p t)))
      changed-p)))

(defun mcclim-nearest-output-recording-pane (sheet frame)
  "Return SHEET or an ancestor that records output for FRAME, or NIL."
  (loop :for current := sheet
          :then (ignore-errors (clim:sheet-parent current))
        :while current
        :when (and (eq (ignore-errors (clim:pane-frame current)) frame)
                   (ignore-errors
                     (clim:output-recording-stream-p current)))
          :return current))

(defun mcclim-presentation-type-symbol (presentation)
  "Return PRESENTATION's primary type symbol."
  (let ((type (clim:presentation-type presentation)))
    (if (consp type)
        (car type)
        type)))

(defparameter *mcclim-named-panes*
  '(selector-pane main-pane compose-pane input-pane completion-pane
    who-line-pane minibuffer-pane modeline-pane pointer-doc-pane)
  "Named panes in the standard Clawmacs McCLIM frame.")

(defun mcclim-frame-pane-name (frame pane)
  "Return FRAME's symbolic pane name for PANE, or NIL."
  (when (and frame pane)
    (or (and (typep pane 'clawmacs-transcript-pane)
             'main-pane)
        (loop :for name :in *mcclim-named-panes*
              :for named-pane := (ignore-errors (clim:find-pane-named frame name))
              :when (eq named-pane pane)
                :return name))))

(defun mcclim-hyper-modifier-p (modifier-state)
  "Return true when MODIFIER-STATE includes Hyper."
  (and modifier-state
       (not (zerop (logand modifier-state clim:+hyper-key+)))))

;;; Pointer feedback is owned by McCLIM presentations and pointer-documentation.
;;; Clawmacs does not scan, highlight, unhighlight, or repaint presentation
;;; output records on pointer motion.

(defun mcclim-frame-displays-buffer-p (frame buffer)
  "Return true when FRAME currently shows BUFFER in any logical window."
  (and frame buffer
       (member buffer (frame-window-buffers frame) :test #'eq)))

(defclass clawmacs-frame-update-event (clim:window-manager-event)
  ((frame :initarg :frame
          :reader clawmacs-frame-update-event-frame)))

(defun mcclim-frame-command-ready-p (frame)
  "Return true when FRAME may safely accept a queued background wakeup."
  (let ((sheet (ignore-errors (clim:frame-top-level-sheet frame))))
    (and sheet
         (ignore-errors (clim:sheet-enabled-p sheet))
         (ignore-errors (clim:sheet-grafted-p sheet)))))

(defun mcclim-schedule-background-command (frame &key delay)
  "Queue one native CLIM command wakeup for FRAME.
The wakeup only transfers control back to the UI thread; normal McCLIM/ESA
command execution still owns redisplay."
  (when (and frame
             (not (frame-quit-flag frame))
             (mcclim-frame-command-ready-p frame))
    (unless (frame-background-command-pending-p frame)
      (file-debug-log "mcclim-update"
                      "schedule frame=~S buffer=~S"
                      frame
                      (and (frame-visible-buffer frame)
                           (buffer-name (frame-visible-buffer frame))))
      (setf (frame-background-command-pending-p frame) t)
      (unless (ignore-errors
                (let* ((sheet (clim:frame-top-level-sheet frame))
                       (event (make-instance 'clawmacs-frame-update-event
                                             :sheet sheet
                                             :frame frame)))
                  (if delay
                      (clime:schedule-event sheet event delay)
                      (clim:queue-event sheet event)))
                t)
        (setf (frame-background-command-pending-p frame) nil))))
  frame)

(defun mcclim-queue-frame-state-sync (frame)
  "Compatibility wrapper around the native CLIM command wakeup path."
  (mcclim-schedule-background-command frame))

(defun mcclim-handle-buffer-display-change (buffer _reason)
  "Queue native CLIM command wakeups for live frames showing BUFFER."
  (file-debug-log "mcclim-update"
                  "notify buffer=~S reason=~S live-frames=~D"
                  (and buffer (buffer-name buffer))
                  _reason
                  (length (mcclim-live-frames)))
  (let ((active-frame (and (boundp 'clim:*application-frame*)
                           clim:*application-frame*)))
    (dolist (frame (mcclim-live-frames))
      (when (and (mcclim-frame-displays-buffer-p frame buffer)
                 (not (and (eq frame active-frame)
                           (eq _reason :dirty))))
        (mcclim-queue-frame-state-sync frame))))
  nil)

(defun mcclim-activate-transcript-pane (frame pane)
  "Make transcript PANE the selected logical window in FRAME."
  (when (and frame (typep pane 'clawmacs-transcript-pane))
    (mcclim-select-window-pane frame pane)))

(defun mcclim-track-pane-pointer-feedback (pane event)
  "Let McCLIM presentation machinery own pointer feedback."
  (declare (ignore pane event))
  nil)

(defmethod clim:handle-event :before ((pane clawmacs-transcript-pane)
                                      (event clim:pointer-button-press-event))
  (let ((frame (clim:pane-frame pane)))
    (with-mcclim-frame-ui-state (frame)
      (mcclim-activate-transcript-pane frame pane))))

(defmethod clim:handle-event :after ((pane clawmacs-transcript-pane)
                                     (event clime:pointer-scroll-event))
  (let ((frame (clim:pane-frame pane)))
    (with-mcclim-frame-ui-state (frame)
      (mcclim-activate-transcript-pane frame pane)
      (mcclim-sync-buffer-scroll-from-main-pane frame))))

;;; Pointer motion is handled by McCLIM's native presentation machinery.

(defmethod clim:handle-event :around ((pane clim:application-pane)
                                      (event clime:pointer-scroll-event))
  (let ((frame (ignore-errors (clim:pane-frame pane))))
    (if (and (typep frame 'clawmacs-gui)
             (with-mcclim-frame-ui-state (frame)
               (mcclim-handle-pointer-scroll frame event)))
        nil
        (call-next-method))))

;;; Pointer motion is handled by Drei/McCLIM.

(defmethod clim:handle-event ((pane clim:sheet)
                              (event clawmacs-frame-update-event))
  (let ((frame (clawmacs-frame-update-event-frame event)))
    (when (typep frame 'clawmacs-gui)
      (file-debug-log "mcclim-update"
                      "event frame=~S buffer=~S pending=~S"
                      frame
                      (and (frame-visible-buffer frame)
                           (buffer-name (frame-visible-buffer frame)))
                      (frame-background-command-pending-p frame))
      (with-mcclim-frame-ui-state (frame)
        (setf (frame-background-command-pending-p frame) nil)
        (when (mcclim-frame-command-ready-p frame)
          (clim:execute-frame-command
           frame
           '(com-process-background-updates)))))))

(defmethod clim:handle-event ((pane clawmacs-drei-input-pane)
                              (event clim:pointer-button-press-event))
  (let ((frame (clim:pane-frame pane)))
    (with-mcclim-frame-ui-state (frame)
      (call-next-method)
      (mcclim-handle-input-pane-click frame pane event))))

(defmethod clim:handle-event :after ((pane clawmacs-drei-input-pane)
                                     (event clim:key-press-event))
  (declare (ignore event))
  (let ((frame (clim:pane-frame pane)))
    (with-mcclim-frame-ui-state (frame)
      (mcclim-sync-buffer-from-drei frame)
      (let ((buf (frame-visible-buffer frame))
            (current (current-buffer)))
        (if (and current (not (eq current buf)))
            (progn
              (deactivate-slash-completion)
              (deactivate-skill-completion))
            (progn
              (sync-slash-completion buf)
              (if *slash-completion-active*
                  (deactivate-skill-completion)
                  (sync-skill-completion buf))))))))

(defmethod clim:execute-frame-command :around ((frame clawmacs-gui) command)
  (declare (ignore command))
  (with-mcclim-frame-ui-state (frame)
    (let ((*mcclim-command-previous-buffer* (frame-visible-buffer frame)))
      (call-next-method))))

(defmethod clim:execute-frame-command :after ((frame clawmacs-gui) command)
  (declare (ignore command))
  (with-mcclim-frame-ui-state (frame)
    (mcclim-sync-selected-window-from-current-buffer
     frame *mcclim-command-previous-buffer*)
    (ignore-errors
      (mcclim-sync-main-pane-to-buffer-scroll frame))))

;;; --------------------------------------------------------------------------
;;; Application Entry Point
;;; --------------------------------------------------------------------------

(defvar *clawmacs-frame* nil
  "The currently running primary Clawmacs McCLIM frame, or NIL.")

(defun run-clawmacs-mcclim (initial-buffer &key (window-title "Clawmacs"))
  "Run the Clawmacs McCLIM application for INITIAL-BUFFER."
  (let ((frame (clim:make-application-frame 'clawmacs-gui
                 :pretty-name window-title
                 :display-buffer initial-buffer
                 :follow-current-buffer-p t
                 :width 900
                 :height 700)))
    (setf *clawmacs-frame* frame)
    (mcclim-register-frame frame)
    (unwind-protect
         (clim:run-frame-top-level frame)
      (mcclim-unregister-frame frame)
      (when (eq *clawmacs-frame* frame)
        (setf *clawmacs-frame* nil)))))
