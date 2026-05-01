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

(clim:define-command (com-follow-info-link
                      :command-table clawmacs-mcclim-command-table
                      :name t)
    ((link 'info-link-ref))
  (mcclim-follow-info-link link))

(defun mcclim-main-pane-sheet-p (frame event)
  "Return true when EVENT targets FRAME's transcript pane."
  (let ((main-pane (and frame (clim:find-pane-named frame 'main-pane)))
        (sheet (and event (ignore-errors (clim:event-sheet event)))))
    (and main-pane sheet
         (eq (mcclim-nearest-output-recording-pane sheet frame)
             main-pane))))

(defun mcclim-main-pane-blank-area-documentation (frame event)
  "Return blank-area pointer documentation for EVENT in FRAME, or NIL."
  (let ((buf (and frame (frame-visible-buffer frame)))
        (main-pane (and frame (clim:find-pane-named frame 'main-pane))))
    (when (and buf
               (document-buffer-p buf)
               main-pane
               (mcclim-main-pane-sheet-p frame event))
      (let ((start-row (or (cdr (assoc :input-start-row
                                       (frame-last-render-snapshot frame)))
                           -1)))
        (when (>= start-row 0)
          (ensure-char-metrics frame main-pane)
          (multiple-value-bind (local-x local-y)
              (mcclim-event-position-in-pane event main-pane)
            (declare (ignore local-x))
            (let ((row (mcclim-pixel-grid-index
                        local-y
                        (max 1 (frame-char-height frame)))))
              (when (>= row start-row)
                "L: Move point"))))))))

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
  "Handle a native CLIM blank-area select in FRAME's transcript pane."
  (let* ((buf (and frame (frame-visible-buffer frame)))
         (main-pane (and frame (clim:find-pane-named frame 'main-pane)))
         (snapshot (and frame (frame-last-render-snapshot frame)))
         (start-row (or (and snapshot
                             (cdr (assoc :input-start-row snapshot)))
                        -1)))
    (when (and buf
               main-pane
               (document-buffer-p buf)
               (mcclim-main-pane-sheet-p frame event)
               (>= start-row 0))
      (ensure-char-metrics frame main-pane)
      (multiple-value-bind (local-x local-y)
          (mcclim-event-position-in-pane event main-pane)
        (let* ((char-w (max 1 (frame-char-width frame)))
               (char-h (max 1 (frame-char-height frame)))
               (cols (max 1 (floor (nth-value 0 (pane-pixel-size main-pane))
                                   char-w)))
               (row (mcclim-pixel-grid-index local-y char-h))
               (col (mcclim-pixel-grid-index local-x char-w)))
          (when (>= row start-row)
            (mcclim-set-message-point-from-grid
             (buffer-input-message buf)
             (- row start-row)
             col
             cols)
            (mcclim-sync-drei-point-from-buffer frame)
            t))))))

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
    (mcclim-sync-drei-from-buffer frame :force-p t)
    (mcclim-ensure-polling frame)))

(clim:define-command (com-clawmacs-poll
                      :command-table clawmacs-mcclim-command-table
                      :name nil
                      :keystroke :clawmacs-poll)
    ()
  (let ((frame clim:*application-frame*))
    (when (mcclim-poll-external-updates frame)
      (mcclim-sync-drei-from-buffer frame :force-p t)
      (mcclim-redisplay-frame frame :force-p t))
    (mcclim-ensure-polling frame)))

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
   :display-time :command-loop
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
   :incremental-redisplay t
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

(defmethod clim:compose-space ((pane clawmacs-transcript-pane)
                               &key (width 900) height)
  "Expose ordinary chat transcripts as a scrollable scrollee.

Selectors, popup overlays, special buffers, and multiwindow layouts keep the
viewport-sized pane so they do not grow a transcript scrollbar."
  (let* ((frame (ignore-errors (clim:pane-frame pane)))
         (buf (and frame (transcript-pane-buffer frame pane)))
         (char-w (max 1 (or (and frame (ignore-errors (frame-char-width frame))) 7)))
         (char-h (max 1 (or (and frame (ignore-errors (frame-char-height frame))) 14)))
         (cols (max 1 (floor width char-w)))
         (native-scroll-p (and frame buf
                               (mcclim-native-transcript-scroll-p frame buf)))
         (rows (if native-scroll-p
                   (mcclim-transcript-total-content-rows
                    buf
                    cols
                    char-w
                    char-h)
                   0))
         (pixel-height (* rows char-h))
         (requested-height (if native-scroll-p
                               pixel-height
                               (or height pixel-height))))
    (clim:make-space-requirement
     :width width
     :min-width 0
     :height requested-height
     :min-height (if native-scroll-p pixel-height 0)
     :max-height (if native-scroll-p pixel-height clim:+fill+))))

(defclass clawmacs-drei-input-view (drei:textual-drei-syntax-view)
  ()
  (:metaclass esa-utils:modual-class)
  (:default-initargs
   :name "Clawmacs Input"
   :use-editor-commands t))

(defclass clawmacs-drei-input-pane (drei:drei-pane esa:esa-pane-mixin)
  ()
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

(defmethod clim:compose-space ((pane clawmacs-minibuffer-pane)
                               &key (width 900) height)
  (declare (ignore pane height))
  (clim:make-space-requirement :width width
                               :min-width 0
                               :height 0
                               :min-height 0
                               :max-height 0))

(defun display-clawmacs-minibuffer-pane (frame pane)
  "Display Clawmacs' hidden ESA minibuffer service pane.
Clawmacs renders completion state in the main presentation pane. This pane is
kept as a minimal ESA-compatible service pane for commands that expect a real
`esa:minibuffer-pane', but it is intentionally not a visible UI region."
  (declare (ignore frame))
  (clear-pane-with-ink pane (clim:make-rgb-color 0.98 0.98 0.98)))

(defun compose-pane-label-text (buf)
  "Return the label shown in the dedicated compose/input service pane."
  (cond
    ((null buf) "Input")
    ((font-editor-buffer-p buf) "Font Editor")
    ((listener-buffer-p buf) "Listener Input")
    ((document-buffer-p buf) "Editor")
    ((and (buffer-input-presentation-function buf)
          (eq (buffer-input-presentation-function buf)
              'mcclim-render-empty-input-pane))
     "Read-Only")
    (t "Compose Message")))

(defun display-compose-pane (frame pane)
  "Display the compose/input section header as its own CLIM pane."
  (with-mcclim-frame-ui-state (frame)
    (ensure-char-metrics frame pane)
    (let* ((char-w (frame-char-width frame))
           (char-h (frame-char-height frame))
           (buf (frame-visible-buffer frame)))
      (when (zerop char-w)
        (return-from display-compose-pane))
      (multiple-value-bind (cols rows)
          (pane-grid-dimensions pane char-w char-h)
        (declare (ignore rows))
        (let* ((text (compose-pane-label-text buf))
               (visible (subseq text 0 (min (length text) (max 0 (1- cols))))))
          (clim:updating-output (pane :unique-id 'compose-pane-content
                                      :cache-value visible
                                      :cache-test #'string=)
            (write-string visible pane)))))))

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
   (pane-space-char-height :accessor frame-pane-space-char-height :initform 0)
   (main-pane-space-height :accessor frame-main-pane-space-height :initform 0)
   (selector-pane-space-height :accessor frame-selector-pane-space-height
                               :initform -1)
   (input-pane-space-height :accessor frame-input-pane-space-height
                            :initform 0)
   (completion-pane-space-height :accessor frame-completion-pane-space-height
                                 :initform -1)
   (last-render-snapshot :accessor frame-last-render-snapshot :initform nil)
   (render-sequence :accessor frame-render-sequence :initform 0)
   (quit-flag :accessor frame-quit-flag :initform nil)
   (poll-pulse-event :accessor frame-poll-pulse-event :initform nil)
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
   (selector-pane clawmacs-selector-pane)
   (workspace-pane clawmacs-workspace-placeholder-pane)
   (compose-pane :application
                 :display-function 'display-compose-pane
                 :display-time :command-loop
                 :incremental-redisplay t
                 :height 20
                 :min-height 20
                 :max-height 20
                 :text-style (clim:make-text-style :fix :roman :normal)
                 :scroll-bars nil
                 :background (clim:make-rgb-color 0.93 0.93 0.93)
                 :foreground (clim:make-rgb-color 0.0 0.0 0.0))
   (input-pane clawmacs-drei-input-pane
               :display-time :command-loop
               :height 42
               :min-height 20
               :max-height 90)
   (completion-pane clawmacs-completion-pane)
   (minibuffer-pane clawmacs-minibuffer-pane)
   (pointer-doc-pane :pointer-documentation
                     :height 18
                     :min-height 18
                     :max-height 18)
   (modeline-pane :application
                  :display-function 'display-modeline-pane
                  :display-time :command-loop
                  :incremental-redisplay t
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
      selector-pane
      (:fill workspace-pane)
      compose-pane
      input-pane
      completion-pane
      modeline-pane
      who-line-pane
      pointer-doc-pane)))
  (:top-level (clawmacs-esa-top-level))
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

(defun mcclim-build-window-pane-constellation (window pane-table)
  "Return a native transcript pane subtree for WINDOW."
  (let ((pane (clim:make-pane 'clawmacs-transcript-pane
                              :window-id (clawmacs-window-id window))))
    (setf (gethash (clawmacs-window-id window) pane-table) pane)
    (clim:scrolling (:scroll-bars :vertical)
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
                first
                adjuster
                second)
              (clim:vertically ()
                first
                second)))
         (:horizontal
          (if adjuster
              (clim:horizontally ()
                first
                adjuster
                second)
              (clim:horizontally ()
                first
                second))))))
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

(defclass clawmacs-display-change-event (clim:window-event)
  ((buffer :initarg :buffer :reader display-change-event-buffer)
   (reason :initarg :reason :reader display-change-event-reason)
   (force-p :initarg :force-p :reader display-change-event-force-p
            :initform t)))

(clim:define-gesture-name :clawmacs-poll :timer :clawmacs-poll)

(defmethod clim:frame-standard-input ((frame clawmacs-gui))
  (or (frame-selected-window-pane frame)
      (call-next-method)))

(defmethod clim:frame-standard-output ((frame clawmacs-gui))
  (or (frame-selected-window-pane frame)
      (call-next-method)))

(defmethod esa:minibuffer ((frame clawmacs-gui))
  (or (clim:find-pane-named frame 'minibuffer-pane)
      (call-next-method)))

(defmethod esa:find-applicable-command-table ((frame clawmacs-gui))
  (declare (ignore frame))
  'clawmacs-mcclim-command-table)

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

Presentation translators consult the frame command table, so keep it aligned
with ESA's current command-table selection even though Clawmacs handles most
keyboard commands through its own defcommand/keymap layer."
  (let ((table-name (esa:find-applicable-command-table frame)))
    (setf (clim:frame-command-table frame)
          (clim:find-command-table table-name)))
  frame)

(defmethod esa:command-for-unbound-gestures ((frame clawmacs-gui) gestures)
  "Treat ordinary ESA gestures as Clawmacs keymap input.
Presentation translators still use CLIM commands; keyboard input falls through
to the existing Clawmacs command/keymap system."
  (when gestures
    (file-debug-log "mcclim-input" "unbound gestures: ~S" gestures)
    (list 'com-clawmacs-dispatch-gestures (list 'quote gestures))))

(defmethod clim:adopt-frame :after (frame-manager (frame clawmacs-gui))
  (declare (ignore frame-manager))
  (with-mcclim-frame-ui-state (frame)
    (mcclim-install-frame-command-table frame)
    (mcclim-sync-drei-from-buffer frame :force-p t)
    (mcclim-ensure-polling frame)))

(defmethod clim:frame-exit :before ((frame clawmacs-gui))
  (mcclim-stop-polling frame))

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
        (pane-pixel-size pane)
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

(defun clim-drawing-options-for-text (fg-ink text-style drawing-options)
  "Return a CLIM drawing-options plist for text rendering."
  (let ((options (copy-list drawing-options)))
    (setf (getf options :ink) fg-ink
          (getf options :text-style) text-style)
    options))

(defun draw-text-at
    (pane row col text fg-ink bg-ink text-style char-w char-h
     &key drawing-options)
  "Draw TEXT at character grid position (ROW, COL) in PANE.
Fills a background rectangle first, then draws the text on top."
  (let* ((x (* col char-w))
         (y (* row char-h))
         (text-width (* (length text) char-w))
         (options (clim-drawing-options-for-text
                   fg-ink text-style drawing-options)))
    ;; Background rectangle
    (clim:draw-rectangle* pane x y (+ x text-width) (+ y char-h)
                          :ink bg-ink)
    (apply #'clim:draw-text*
           pane text x y
           :align-y :top
           options)))

(defun text-pixel-width (pane text text-style)
  "Return TEXT's pixel width in PANE for TEXT-STYLE."
  (if (or (null text) (string= text ""))
      0
      (nth-value 0
                 (clim:text-size pane text
                                 :text-style text-style))))

(defun mcclim-pixel-grid-index (pixel cell-size)
  "Return the zero-based grid cell containing PIXEL at CELL-SIZE."
  (if (plusp cell-size)
      (max 0 (floor pixel cell-size))
      0))

(defun draw-text-at-pixels
    (pane x y text fg-ink bg-ink text-style char-h
     &key drawing-options background-width)
  "Draw TEXT at pixel position (X, Y) in PANE.
Uses TEXT-STYLE for both measurement and drawing so cursor overlays line up
with the rendered glyph advance."
  (let* ((text-width (max 1 (or background-width
                                (text-pixel-width pane text text-style))))
         (options (clim-drawing-options-for-text
                   fg-ink text-style drawing-options)))
    (clim:draw-rectangle* pane x y (+ x text-width) (+ y char-h)
                          :ink bg-ink)
    (apply #'clim:draw-text*
           pane text x y
           :align-y :top
           options)))

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

(defun clear-pane-region-with-ink (pane region ink)
  "Fill REGION of PANE with a solid color INK."
  (clim:with-bounding-rectangle* (x1 y1 x2 y2) region
    (clim:draw-rectangle* pane x1 y1 x2 y2 :ink ink)))

(defun fill-row (pane row cols bg-ink char-w char-h)
  "Fill an entire row with background color."
  (clim:draw-rectangle* pane
                        0 (* row char-h)
                        (* cols char-w) (* (1+ row) char-h)
                        :ink bg-ink))

(defun fill-grid-rect (pane row col rows cols ink char-w char-h)
  "Fill a grid rectangle in PANE."
  (clim:draw-rectangle* pane
                        (* col char-w)
                        (* row char-h)
                        (* (+ col cols) char-w)
                        (* (+ row rows) char-h)
                        :ink ink))

(defun draw-faced-spans (pane row start-col spans char-w char-h)
  "Draw SPANS starting at (ROW, START-COL) using global face definitions."
  (let ((col start-col))
    (dolist (span spans)
      (multiple-value-bind (fg bg ts opts)
          (resolve-global-face-inks (car span))
        (draw-text-at pane row col (cdr span) fg bg ts char-w char-h
                      :drawing-options opts))
      (incf col (length (cdr span))))))

;;; --------------------------------------------------------------------------
;;; Inline Image Rendering
;;; --------------------------------------------------------------------------

(defparameter *mcclim-inline-image-max-rows* 24
  "Maximum transcript rows consumed by an inline image.")

(defparameter *mcclim-inline-image-min-rows* 4
  "Minimum display rows used when a tiny inline image can be scaled up.")

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

(defun mcclim-inline-image-geometry (entry width prefix-len char-w char-h)
  "Return display geometry for cached image ENTRY.
Values are DISPLAY-WIDTH, DISPLAY-HEIGHT, ROWS, and SCALE."
  (let* ((image-width (mcclim-image-cache-entry-width entry))
         (image-height (mcclim-image-cache-entry-height entry))
         (available-width (max char-w
                               (* (max 1 (- width prefix-len 1)) char-w)))
         (max-height (* (max 1 *mcclim-inline-image-max-rows*) char-h))
         (min-height (* (max 1 *mcclim-inline-image-min-rows*) char-h))
         (max-scale (min (/ available-width image-width)
                         (/ max-height image-height)))
         (desired-scale (if (< image-height min-height)
                            (/ min-height image-height)
                            1))
         (scale (min max-scale desired-scale))
         (display-width (max 1 (floor (* image-width scale))))
         (display-height (max 1 (floor (* image-height scale))))
         (rows (max 1 (ceiling display-height char-h))))
    (values display-width display-height rows (float scale 1.0))))

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
  "Return transcript rows consumed by image REFERENCE."
  (multiple-value-bind (entry error-text)
      (mcclim-load-display-image-reference reference)
    (if (or error-text (null entry))
        1
        (multiple-value-bind (_display-width _display-height image-rows _scale)
            (mcclim-inline-image-geometry entry width prefix-len char-w char-h)
          (declare (ignore _display-width _display-height _scale))
          (1+ image-rows)))))

(defun mcclim-message-visual-height
    (msg width char-w char-h &key (prefix (message-sender-prefix msg))
       show-reasoning-p show-metadata-p render-images-p)
  "Return MSG height using McCLIM image geometry when requested."
  (let* ((prefix-len (length prefix))
         (display-width (max 1 (- width prefix-len))))
    (loop :for block :in (if render-images-p
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
                                      :show-metadata-p show-metadata-p)))
          :sum (ecase (getf block :type)
                 (:text
                  (wrapped-line-count (getf block :text) display-width))
                 (:image
                  (mcclim-image-block-visual-height
                   (getf block :reference)
                   width prefix-len char-w char-h))))))

(defun mcclim-render-image-block
    (pane reference row width prefix prefix-len fg bg ts drawing-options
     char-w char-h max-rows first-row-p)
  "Render image REFERENCE at ROW and return rows consumed."
  (multiple-value-bind (entry error-text)
      (mcclim-load-display-image-reference reference)
    (let* ((caption (mcclim-inline-image-caption reference entry error-text))
           (caption-col (if first-row-p 0 prefix-len))
           (caption-text (if first-row-p
                             (concatenate 'string prefix caption)
                             caption))
           (visible-caption (subseq caption-text
                                    0
                                    (min (length caption-text)
                                         (max 1 (- width caption-col))))))
      (when (and (>= row 0) (< row max-rows))
        (fill-row pane row width bg char-w char-h)
        (draw-text-at pane row caption-col visible-caption
                      fg bg ts char-w char-h
                      :drawing-options drawing-options))
      (if (or error-text (null entry))
          1
          (multiple-value-bind (display-width display-height image-rows scale)
              (mcclim-inline-image-geometry entry width prefix-len char-w char-h)
            (let* ((image-row (1+ row))
                   (image-end-row (+ image-row image-rows))
                   (x (* prefix-len char-w))
                   (y (* image-row char-h))
                   (clip-y1 (max 0 y))
                   (clip-y2 (min (* max-rows char-h) (+ y display-height)))
                   (pattern (mcclim-image-cache-entry-pattern entry))
                   (image-width (mcclim-image-cache-entry-width entry))
                   (image-height (mcclim-image-cache-entry-height entry)))
              (when (and (> image-end-row 0)
                         (< image-row max-rows)
                         (< clip-y1 clip-y2))
                (clim:draw-rectangle* pane
                                      0 clip-y1
                                      (* width char-w) clip-y2
                                      :ink bg)
                ;; DRAW-PATTERN* applies TRANSFORMATION to both the pattern
                ;; origin and the clipping region. Build the final translation
                ;; into the transformation and express clipping in source-image
                ;; coordinates so scaled inline images stay on-screen.
                (clim:draw-pattern*
                 pane
                 pattern
                 0 0
                 :clipping-region
                 (clim:make-rectangle*
                  0
                  (max 0 (/ (- clip-y1 y) scale))
                  image-width
                  (min image-height (/ (- clip-y2 y) scale)))
                 :transformation
                 (clim:compose-transformations
                  (clim:make-translation-transformation x y)
                  (clim:make-scaling-transformation scale scale)))))
            (1+ image-rows))))))

(defun pane-pixel-size (pane)
  "Return (values width height) — the allocated pixel size of PANE.
Caps the pane viewport by the current top-level sheet region so external
window-manager tiling/resizing remains authoritative even when stream output
history grows a pane's own sheet-region."
  (labels ((region-size (region)
             (values (max 1 (floor (clim:bounding-rectangle-width region)))
                     (max 1 (floor (clim:bounding-rectangle-height region))))))
    (let* ((frame (clim:pane-frame pane))
           (top-sheet (clim:frame-top-level-sheet frame))
           (top-region (clim:sheet-region top-sheet))
           (pane-region (handler-case
                            (clim:window-viewport pane)
                          (error ()
                            (clim:sheet-region pane)))))
      (multiple-value-bind (top-width top-height)
          (region-size top-region)
        (multiple-value-bind (pane-width pane-height)
            (region-size pane-region)
          (values (min top-width pane-width)
                  (min top-height pane-height)))))))

(defun pane-grid-dimensions (pane char-w char-h)
  "Return (values cols rows) — the character grid size of PANE."
  (multiple-value-bind (width height) (pane-pixel-size pane)
    (values (max 1 (floor width char-w))
            (max 1 (floor height char-h)))))

(defun pane-viewport-pixel-size (pane)
  "Return (values width height) for PANE's visible viewport in pixels."
  (labels ((region-size (region)
             (values (max 1 (floor (clim:bounding-rectangle-width region)))
                     (max 1 (floor (clim:bounding-rectangle-height region))))))
    (let ((region (or (ignore-errors (clim:pane-viewport-region pane))
                      (ignore-errors (clim:window-viewport pane))
                      (clim:sheet-region pane))))
      (region-size region))))

(defun pane-viewport-grid-dimensions (pane char-w char-h)
  "Return (values cols rows) for the visible viewport of PANE."
  (multiple-value-bind (width height) (pane-viewport-pixel-size pane)
    (values (max 1 (floor width char-w))
            (max 1 (floor height char-h)))))

(defun pane-viewport-grid-origin (pane char-w char-h)
  "Return (values col row) for the visible viewport origin of PANE."
  (let ((region (or (ignore-errors (clim:pane-viewport-region pane))
                    (ignore-errors (clim:window-viewport pane))
                    (clim:sheet-region pane))))
    (values (max 0 (floor (clim:bounding-rectangle-min-x region) char-w))
            (max 0 (floor (clim:bounding-rectangle-min-y region) char-h)))))

(defun mcclim-transcript-history-messages (buf)
  "Return the visible finalized transcript messages for BUF."
  (let ((messages nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))))
          :do (when (message-visible-for-buffer-p msg buf)
                (push msg messages)))
    (nreverse messages)))

(defun mcclim-transcript-history-layout (buf width char-w char-h)
  "Return transcript layout values for BUF at WIDTH.

Values are HISTORY-MESSAGES, MESSAGE-HEIGHTS, and TOTAL-HISTORY-ROWS."
  (let* ((show-reasoning-p (buffer-show-reasoning-p buf))
         (show-metadata-p (buffer-show-metadata-p buf))
         (history-messages (mcclim-transcript-history-messages buf))
         (msg-heights
           (mapcar (lambda (message)
                     (mcclim-message-visual-height
                      message
                      width
                      char-w
                      char-h
                      :show-reasoning-p show-reasoning-p
                      :show-metadata-p show-metadata-p
                      :render-images-p t))
                   history-messages)))
    (values history-messages
            msg-heights
            (reduce #'+ msg-heights :initial-value 0))))

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

(defun mcclim-transcript-total-content-rows (buf width char-w char-h)
  "Return the full transcript scrollee row count for BUF."
  (multiple-value-bind (_messages _heights total-history-rows)
      (mcclim-transcript-history-layout buf width char-w char-h)
    (declare (ignore _messages _heights))
    (+ 1
       total-history-rows
       (if (buffer-approval-pending buf)
           (mcclim-approval-prompt-rows (buffer-approval-pending buf))
           0))))

(defun mcclim-native-stream-buffer-p (buf)
  "Return true when BUF should render through a native CLIM stream/scroller."
  (and buf
       (or (eq (buffer-kind buf) :chat)
           (help-buffer-p buf)
           (info-buffer-p buf)
           (customize-buffer-p buf)
           (listener-buffer-p buf))))

(defun mcclim-native-transcript-scroll-p (frame &optional (buf (frame-visible-buffer frame)))
  "Return true when BUF in FRAME should use a native CLIM scroller."
  (declare (ignore frame))
  (mcclim-native-stream-buffer-p buf))

(defun mcclim-pane-content-rows (pane char-h)
  "Return PANE's current content height in character rows."
  (let ((region (or (ignore-errors (clim:sheet-region pane))
                    (ignore-errors (clim:window-viewport pane))
                    (ignore-errors (clim:pane-viewport-region pane)))))
    (if (and region (plusp char-h))
        (max 1 (ceiling (clim:bounding-rectangle-height region) char-h))
        1)))

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

(defun mcclim-update-main-pane-space-requirements (frame)
  "Refresh the main transcript pane scroller requirements for FRAME."
  (when (mcclim-primary-frame-p frame)
    (dolist (main-pane (frame-window-panes-in-display-order frame))
      (let* ((buf (transcript-pane-buffer frame main-pane))
             (pixel-height 0)
             (pixel-width 0)
             (native-scroll-p (mcclim-native-transcript-scroll-p frame buf))
             (char-w (max 1 (frame-char-width frame)))
             (char-h (max 1 (frame-char-height frame))))
        (multiple-value-bind (viewport-width viewport-height)
            (pane-viewport-pixel-size main-pane)
          (declare (ignore char-w char-h))
          (setf pixel-width viewport-width
                pixel-height viewport-height)
          (when (eq main-pane (frame-selected-window-pane frame))
            (setf (frame-main-pane-space-height frame) pixel-height))
          (clim:change-space-requirements
           main-pane
           :width pixel-width
           :min-width (if native-scroll-p pixel-width 0)
           :max-width (if native-scroll-p pixel-width clim:+fill+)
           :height pixel-height
           :min-height (if native-scroll-p pixel-height 0)
           :max-height (if native-scroll-p pixel-height clim:+fill+))
          (unless native-scroll-p
            (ignore-errors
              (clim:scroll-extent main-pane 0 0)))))))
  frame)

(defun mcclim-sync-buffer-scroll-from-main-pane (frame)
  "Mirror the native main-pane viewport position into the visible buffer."
  (when (mcclim-native-transcript-scroll-p frame)
    (let ((main-pane (clim:find-pane-named frame 'main-pane))
          (buf (frame-visible-buffer frame)))
      (when (and main-pane buf)
        (let ((char-w (max 1 (frame-char-width frame)))
              (char-h (max 1 (frame-char-height frame))))
          (multiple-value-bind (_cols viewport-rows)
              (pane-viewport-grid-dimensions main-pane char-w char-h)
            (declare (ignore _cols))
            (let* ((content-rows (mcclim-pane-content-rows main-pane char-h))
                   (max-scroll (max 0 (- content-rows viewport-rows)))
                   (viewport-y
                     (or (nth-value 1
                           (ignore-errors
                             (clim:window-viewport-position main-pane)))
                         (let ((region
                                 (or (ignore-errors
                                       (clim:pane-viewport-region main-pane))
                                     (ignore-errors
                                       (clim:window-viewport main-pane))
                                     (clim:sheet-region main-pane))))
                           (clim:bounding-rectangle-min-y region))
                         0))
                   (top-row (min max-scroll
                                 (max 0 (round viewport-y char-h)))))
              (setf (buffer-scroll-offset buf)
                    (max 0 (- content-rows viewport-rows top-row)))))))))
  frame)

(defun mcclim-sync-main-pane-to-buffer-scroll (frame)
  "Apply the visible buffer's logical scroll offset to FRAME's main pane."
  (when (mcclim-native-transcript-scroll-p frame)
    (let ((main-pane (clim:find-pane-named frame 'main-pane))
          (buf (frame-visible-buffer frame)))
      (when (and main-pane buf)
        (let ((char-w (max 1 (frame-char-width frame)))
              (char-h (max 1 (frame-char-height frame))))
          (multiple-value-bind (_cols viewport-rows)
              (pane-viewport-grid-dimensions main-pane char-w char-h)
            (declare (ignore _cols))
            (let* ((content-rows (mcclim-pane-content-rows main-pane char-h))
                   (max-scroll (max 0 (- content-rows viewport-rows)))
                   (desired-bottom-offset
                     (min (max 0 (buffer-scroll-offset buf)) max-scroll))
                   (desired-top-row
                     (max 0 (- content-rows viewport-rows
                                desired-bottom-offset)))
                   (desired-y (* desired-top-row char-h)))
              (multiple-value-bind (_viewport-x viewport-y)
                  (ignore-errors (clim:window-viewport-position main-pane))
                (declare (ignore _viewport-x))
                (when (/= desired-y (or viewport-y 0))
                  (clim:scroll-extent main-pane 0 desired-y)))))))))
  frame)

(defun mcclim-handle-native-page-scroll (frame key)
  "Handle KEY as a native transcript page scroll when appropriate."
  (when (and (member key '(:page-up :page-down) :test #'eq)
             (mcclim-native-transcript-scroll-p frame)
             (not (mcclim-selector-pane-active-p))
             (not (mcclim-completion-pane-active-p)))
    (let ((main-pane (clim:find-pane-named frame 'main-pane))
          (buf (frame-visible-buffer frame))
          (char-w (max 1 (frame-char-width frame)))
          (char-h (max 1 (frame-char-height frame))))
      (when (and main-pane buf (plusp char-w) (plusp char-h))
        (multiple-value-bind (_cols viewport-rows)
            (pane-viewport-grid-dimensions main-pane char-w char-h)
          (declare (ignore _cols))
          (let* ((content-rows (mcclim-pane-content-rows main-pane char-h))
                 (max-scroll (max 0 (- content-rows viewport-rows)))
                 (page-rows (max 1 (- viewport-rows 3)))
                 (delta-y (* page-rows char-h))
                 (viewport-y
                   (or (nth-value 1
                         (ignore-errors
                           (clim:window-viewport-position main-pane)))
                       0))
                 (desired-y
                   (min (* max-scroll char-h)
                        (max 0 (+ viewport-y
                                  (if (eq key :page-up)
                                      (- delta-y)
                                      delta-y))))))
            (ignore-errors
              (clim:scroll-extent main-pane 0 desired-y))
            (let ((top-row (min max-scroll
                                (max 0 (round desired-y char-h)))))
              (setf (buffer-scroll-offset buf)
                    (max 0 (- content-rows viewport-rows top-row))))
            t))))))

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
      (mcclim-sync-drei-from-buffer frame :force-p t)
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

(defun mcclim-set-message-point-from-grid
    (message grid-row grid-col width &key (prefix-len 0))
  "Move MESSAGE point to the source location displayed at GRID-ROW/GRID-COL.
WIDTH and PREFIX-LEN use the same wrapped grid geometry as
`mcclim-render-message-lines'."
  (let* ((target-row (max 0 grid-row))
         (target-col (max 0 (- grid-col prefix-len)))
         (display-width (max 1 (- width prefix-len)))
         (visual-row 0)
         (last-line nil))
    (labels ((move-point (line offset)
               (setf (message-point-line message) line
                     (message-point-offset message) offset)
               message))
      (loop :for line := (message-first-line message) :then (line-next line)
            :while line
            :do (let* ((content (line-content line))
                       (content-len (length content))
                       (wraps (wrapped-line-count content display-width)))
                  (setf last-line line)
                  (dotimes (wrap-index wraps)
                    (let* ((chunk-start (* wrap-index display-width))
                           (chunk-end (min (* (1+ wrap-index) display-width)
                                           content-len))
                           (chunk-width (- chunk-end chunk-start)))
                      (when (= visual-row target-row)
                        (return-from mcclim-set-message-point-from-grid
                          (move-point
                           line
                           (max 0
                                (min content-len
                                     (+ chunk-start
                                        (min target-col chunk-width)))))))
                      (incf visual-row)))))
      (when last-line
        (move-point last-line (length (line-content last-line))))
      message)))

(defun (setf mcclim-drei-pane-text) (text pane)
  "Replace PANE's Drei buffer with TEXT."
  (let* ((view (drei:current-view pane))
         (buffer (and view (drei:buffer view))))
    (when buffer
      (drei:performing-drei-operations (pane :with-undo nil :redisplay nil)
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

(defun mcclim-drei-offset-grid-position (pane offset)
  "Return OFFSET in PANE as wrapped grid column and row values."
  (let* ((frame (clim:pane-frame pane)))
    (ensure-char-metrics frame pane)
    (let* ((char-w (max 1 (frame-char-width frame)))
           (char-h (max 1 (frame-char-height frame)))
           (cols (max 1 (nth-value 0 (pane-grid-dimensions pane char-w char-h))))
           (text (mcclim-drei-pane-text pane))
           (limit (min (max 0 offset) (length text)))
           (row 0)
           (col 0))
      (loop :for index :from 0 :below limit
            :for ch := (char text index)
            :do (cond
                  ((char= ch #\Newline)
                   (incf row)
                   (setf col 0))
                  (t
                   (incf col)
                   (when (>= col cols)
                     (incf row)
                     (setf col 0)))))
      (values col row char-w char-h))))

(defun mcclim-set-input-point-from-pane-click (frame pane event)
  "Map EVENT in input PANE into the visible buffer's input message point."
  (let ((buf (frame-visible-buffer frame)))
    (when (and frame pane buf (not (document-buffer-p buf)))
      (ensure-char-metrics frame pane)
      (multiple-value-bind (local-x local-y)
          (mcclim-event-position-in-pane event pane)
        (let* ((char-w (max 1 (frame-char-width frame)))
               (char-h (max 1 (frame-char-height frame)))
               (cols (max 1 (nth-value 0
                              (pane-viewport-grid-dimensions
                               pane char-w char-h))))
               (row (mcclim-pixel-grid-index local-y char-h))
               (col (mcclim-pixel-grid-index local-x char-w)))
          (mcclim-set-message-point-from-grid
           (buffer-input-message buf)
           row
           col
           cols)
          (mcclim-sync-drei-point-from-buffer frame)
          t)))))

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

(defmethod drei:display-drei-view-cursor ((pane clawmacs-drei-input-pane)
                                          (view clawmacs-drei-input-view)
                                          (cursor drei:drei-cursor))
  "Render Drei's cursor in our fixed-pitch input pane without basic-medium text metrics."
  (declare (ignore view))
  (multiple-value-bind (col row char-w char-h)
      (mcclim-drei-offset-grid-position pane
                                        (drei-buffer:offset (drei:mark cursor)))
    (let ((x (* col char-w))
          (y (* row char-h)))
      (clim:draw-rectangle* pane
                            x y
                            (+ x 2)
                            (+ y char-h)
                            :ink (drei:ink cursor)))))

;;; --------------------------------------------------------------------------
;;; Modeline Display
;;; --------------------------------------------------------------------------

(defun display-modeline-pane (frame pane)
  "Display function for the modeline pane."
  (with-mcclim-frame-ui-state (frame)
    (ensure-char-metrics frame pane)
    (let* ((char-w (frame-char-width frame))
           (char-h (frame-char-height frame))
           (buf (frame-visible-buffer frame)))
      (when (zerop char-w) (return-from display-modeline-pane))
      (multiple-value-bind (cols rows) (pane-grid-dimensions pane char-w char-h)
        (declare (ignore rows))
        (mcclim-render-modeline pane buf cols)))))

(defun mcclim-fit-modeline-text (text cols)
  "Pad or truncate TEXT to exactly COLS display columns."
  (if (<= (length text) cols)
      (concatenate 'string text
                   (make-string (- cols (length text))
                                :initial-element #\Space))
      (subseq text 0 cols)))

(defun mcclim-selector-modeline-text (buf cols)
  "Return selector-specific modeline text, or NIL for the normal modeline."
  (let ((text
          (cond
            (*session-tree-selector-active*
             (format nil " [session-tree] ~A | ~D entr~:@P | filter ~(~A~)"
                     (if *session-tree-selector-buffer*
                         (buffer-name *session-tree-selector-buffer*)
                         "")
                     (length *session-tree-selector-filtered-items*)
                     *session-tree-selector-filter-mode*))
            (*buffer-selector-active*
             (format nil " [buffer-selector] Agent Sessions | ~D session~:[s~;~]"
                     (length *buffer-ring*) (= (length *buffer-ring*) 1)))
            (*model-selector-active*
             (format nil " [model-selector] ~A | ~D model~:[s~;~] available"
                     (resolve-modeline-provider-model buf)
                     (length *model-selector-entries*)
                     (= (length *model-selector-entries*) 1)))
            (*think-selector-active*
             (format nil " [think-selector] ~A | ~D level~:[s~;~] available"
                     (resolve-modeline-provider-model buf)
                     (length *think-selector-entries*)
                     (= (length *think-selector-entries*) 1)))
            (t nil))))
    (when text
      (mcclim-fit-modeline-text text cols))))

(defun mcclim-render-modeline (pane buf cols)
  "Render the modeline string into PANE using the modeline face.
Wrapped in updating-output so CLIM skips redraw when the text hasn't changed."
  (let* ((ml-face (make-modeline-face))
         (resolved (resolve-face ml-face))
         (text (or (mcclim-selector-modeline-text buf cols)
                   (format-modeline buf cols
                                    :major-mode (buffer-major-mode buf)))))
    (clim:updating-output (pane :unique-id 'modeline-content
                                :cache-value text
                                :cache-test #'string=)
      (multiple-value-bind (fg _bg ts opts) (resolve-face-inks resolved)
        (declare (ignore _bg))
        (apply #'clim:invoke-with-drawing-options
               pane
               (lambda (medium)
                 (declare (ignore medium))
                 (write-string text pane))
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
  "Display function for the who-line pane. Shows a single status wholine."
  (with-mcclim-frame-ui-state (frame)
    (ensure-char-metrics frame pane)
    (let* ((char-w (frame-char-width frame))
           (char-h (frame-char-height frame))
           (buf (frame-visible-buffer frame)))
      (when (zerop char-w)
        (return-from display-who-line-pane))
      (multiple-value-bind (cols rows) (pane-grid-dimensions pane char-w char-h)
        (declare (ignore rows))
        (multiple-value-bind (row1 _row2)
            (format-who-line buf cols)
          (declare (ignore _row2))
          (clim:updating-output (pane :unique-id 'who-line-content
                                      :cache-value row1
                                      :cache-test #'string=)
            (multiple-value-bind (wl-fg _wl-bg wl-ts wl-opts)
                (resolve-global-face-inks :who-line)
              (declare (ignore _wl-bg))
              (apply #'clim:invoke-with-drawing-options
                     pane
                     (lambda (medium)
                       (declare (ignore medium))
                       (write-string row1 pane))
                     :ink wl-fg
                     :text-style wl-ts
                     wl-opts))))))))

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

(defun mcclim-window-layout-entries (frame rows cols)
  "Return logical window layout entries and separators for FRAME."
  (clawmacs-window-tree-layout
   (mcclim-ensure-window-tree frame)
   rows
   cols))

(defun mcclim-render-window-separator (pane separator char-w char-h)
  "Render one logical window separator."
  (multiple-value-bind (fg bg ts opts)
      (resolve-global-face-inks :modeline)
    (declare (ignore fg ts opts))
    (fill-grid-rect pane
                    (clawmacs-window-separator-row separator)
                    (clawmacs-window-separator-col separator)
                    (clawmacs-window-separator-rows separator)
                    (clawmacs-window-separator-cols separator)
                    bg
                    char-w
                    char-h)))

(defun mcclim-render-logical-window-entry
    (frame pane entry char-w char-h selected-window-id)
  "Render one logical window ENTRY into PANE using CLIM clipping/translation."
  (declare (ignore frame))
  (let* ((window (clawmacs-window-layout-entry-window entry))
         (buf (and window (clawmacs-window-buffer window)))
         (row (clawmacs-window-layout-entry-row entry))
         (col (clawmacs-window-layout-entry-col entry))
         (rows (clawmacs-window-layout-entry-rows entry))
         (cols (clawmacs-window-layout-entry-cols entry))
         (x (* col char-w))
         (y (* row char-h))
         (x2 (* (+ col cols) char-w))
         (y2 (* (+ row rows) char-h))
         (selected-p (and window
                          (eql selected-window-id
                               (clawmacs-window-id window)))))
    (when (and window buf (plusp rows) (plusp cols))
      (clim:with-output-as-presentation
          (pane window 'clawmacs-window-ref :single-box t)
        (clim:with-drawing-options
            (pane :clipping-region (clim:make-rectangle* x y x2 y2))
          (clim:with-translation (pane x y)
            (let ((*mcclim-render-window-id* (clawmacs-window-id window))
                  (*mcclim-suppress-render-snapshot* (not selected-p)))
              (declare (special *mcclim-render-window-id*
                                *mcclim-suppress-render-snapshot*))
              (mcclim-render-buffer pane buf rows cols char-w char-h))))))))

(defun mcclim-render-logical-window-tree
    (frame pane rows cols char-w char-h)
  "Render FRAME's Emacs-style logical windows into the transcript pane."
  (clear-pane-with-ink pane *mcclim-bg-ink*)
  (multiple-value-bind (entries separators)
      (mcclim-window-layout-entries frame rows cols)
    (let ((selected-window-id (frame-selected-window-id frame)))
      (dolist (entry entries)
        (mcclim-render-logical-window-entry
         frame pane entry char-w char-h selected-window-id))
      (dolist (separator separators)
        (mcclim-render-window-separator pane separator char-w char-h)))))

(defun display-main-pane (frame pane)
  "Display function for the main pane."
  (with-mcclim-frame-ui-state (frame)
    (ensure-char-metrics frame pane)
    (let* ((char-w (frame-char-width frame))
           (char-h (frame-char-height frame))
           (buf (transcript-pane-buffer frame pane)))
      (when (zerop char-w) (return-from display-main-pane))
      (multiple-value-bind (cols rows) (pane-viewport-grid-dimensions pane char-w char-h)
        (mcclim-ensure-window-tree frame)
        (when buf
          (mcclim-render-buffer pane buf rows cols char-w char-h))))))

(defun display-selector-pane (frame pane)
  "Display function for the dedicated selector pane."
  (with-mcclim-frame-ui-state (frame)
    (ensure-char-metrics frame pane)
    (let ((char-w (frame-char-width frame))
          (char-h (frame-char-height frame)))
      (when (zerop char-w) (return-from display-selector-pane))
      (multiple-value-bind (cols rows) (pane-viewport-grid-dimensions pane char-w char-h)
        (if (mcclim-selector-pane-active-p)
            (cond
              (*session-tree-selector-active*
               (mcclim-render-session-tree-selector pane rows cols char-w char-h frame))
              (*buffer-selector-active*
               (mcclim-render-buffer-selector pane rows cols char-w char-h frame))
              (*model-selector-active*
               (mcclim-render-model-selector pane rows cols char-w char-h frame))
              (*think-selector-active*
               (mcclim-render-think-selector pane rows cols char-w char-h frame)))
            (clear-pane-with-ink pane *mcclim-bg-ink*))))))

(defun display-completion-pane (frame pane)
  "Display function for the dedicated completion pane."
  (with-mcclim-frame-ui-state (frame)
    (ensure-char-metrics frame pane)
    (let ((char-w (frame-char-width frame))
          (char-h (frame-char-height frame)))
      (when (zerop char-w) (return-from display-completion-pane))
      (multiple-value-bind (cols rows) (pane-viewport-grid-dimensions pane char-w char-h)
        (if (mcclim-completion-pane-active-p)
            (mcclim-render-completion-popup pane cols rows char-w char-h)
            (clear-pane-with-ink pane *mcclim-bg-ink*))))))

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
  "Render TEXT to PANE as wrapped CLIM stream output."
  (let ((first-output-p t))
    (dolist (line (split-string-by-newline text))
      (dolist (chunk (mcclim-wrap-display-line line width))
        (write-string (if first-output-p prefix prefix-spaces) pane)
        (write-string chunk pane)
        (terpri pane)
        (setf first-output-p nil)))
    (when first-output-p
      (write-string prefix pane)
      (terpri pane))))

(defun mcclim-stream-render-image-block (pane reference width prefix prefix-len char-w char-h)
  "Render image REFERENCE to PANE and return its source entry if loaded."
  (let* ((prefix-spaces (make-string prefix-len :initial-element #\Space)))
    (multiple-value-bind (entry error-text)
        (mcclim-load-display-image-reference reference)
      (let ((caption (mcclim-inline-image-caption reference entry error-text)))
        (mcclim-stream-render-text-block pane caption width prefix prefix-spaces))
      (when (and entry (null error-text))
        (multiple-value-bind (display-width display-height _rows scale)
            (mcclim-inline-image-geometry entry width prefix-len char-w char-h)
          (declare (ignore display-width _rows))
          (multiple-value-bind (_cursor-x cursor-y)
              (clim:stream-cursor-position pane)
            (declare (ignore _cursor-x))
            (clim:with-output-as-presentation
                (pane reference 'image-reference :single-box t)
              (clim:draw-pattern*
               pane
               (mcclim-image-cache-entry-pattern entry)
               0 0
               :transformation
               (clim:compose-transformations
                (clim:make-translation-transformation (* prefix-len char-w)
                                                      cursor-y)
                (clim:make-scaling-transformation scale scale))))
            (setf (clim:stream-cursor-position pane)
                  (values 0 (+ cursor-y display-height)))
            (terpri pane))))
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

;;; --------------------------------------------------------------------------
;;; Buffer Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-fit-line (line cols)
  "Return LINE truncated to COLS display columns."
  (let ((width (max 0 cols)))
    (if (> (length line) width)
        (subseq line 0 width)
        line)))

(defun mcclim-wrap-display-line (line cols)
  "Wrap LINE into fixed-width chunks."
  (let* ((width (max 1 cols))
         (len (length line)))
    (if (zerop len)
        (list "")
        (loop :for start :from 0 :below len :by width
              :collect (subseq line start (min len (+ start width)))))))

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
  "Return styled row entries for the dedicated help buffer presentation."
  (let ((lines (split-string-by-newline (help-buffer-text buf)))
        (entries nil))
    (loop :for line :in lines
          :for index :from 0
          :for tail :on lines
          :for next-line := (second tail)
          :for face := (mcclim-help-face-for-line line next-line index)
          :do (dolist (chunk (mcclim-wrap-display-line line cols))
                (push (list :text chunk
                            :face face
                            :object (list :buffer buf
                                          :line index
                                          :text line)
                            :presentation-type 'help-line-ref)
                      entries)))
    (nreverse entries)))

(defun mcclim-wrap-info-line-segments (segments cols)
  "Wrap one Info display line SEGMENTS into fixed-width rows."
  (let ((width (max 1 cols))
        (rows nil)
        (current-row nil)
        (current-col 0))
    (labels ((flush-row ()
               (push (nreverse current-row) rows)
               (setf current-row nil
                     current-col 0))
             (push-piece (text face link-index)
               (push (make-info-segment :text text
                                        :face face
                                        :link-index link-index)
                     current-row)
               (incf current-col (length text))))
      (dolist (segment segments)
        (let* ((text (or (info-segment-text segment) ""))
               (face (info-segment-face segment))
               (link-index (info-segment-link-index segment))
               (start 0)
               (length (length text)))
          (when (zerop length)
            (when (and (null current-row) (null rows))
              (flush-row)))
          (loop :while (< start length)
                :do (when (>= current-col width)
                      (flush-row))
                    (let* ((remaining (- width current-col))
                           (end (min length (+ start remaining))))
                      (push-piece (subseq text start end) face link-index)
                      (setf start end)))))
      (when (or current-row (null rows))
        (flush-row)))
    (nreverse rows)))

(defun mcclim-info-display-rows (buf cols)
  "Return wrapped display rows for BUF's current Info document."
  (let* ((state (info-buffer-state buf))
         (document (and state (info-state-document state)))
         (rows nil))
    (dolist (line (and document (info-document-lines document)))
      (setf rows
            (append rows
                    (mcclim-wrap-info-line-segments line cols))))
    rows))

(defun mcclim-draw-info-row (pane row cols row-segments selected-index document
                             char-w char-h)
  "Draw one wrapped Info ROW-SEGMENTS."
  (multiple-value-bind (default-fg default-bg default-ts default-opts)
      (resolve-global-face-inks :default-text)
    (declare (ignore default-fg default-ts default-opts))
    (fill-row pane row cols default-bg char-w char-h))
  (let ((col 0))
    (dolist (segment row-segments)
      (let* ((text (or (info-segment-text segment) ""))
             (link-index (info-segment-link-index segment))
             (link (and document
                        link-index
                        (nth link-index (info-document-links document))))
             (selected-p (and link-index
                              selected-index
                              (= link-index selected-index)))
             (face (cond
                     (selected-p :selector-selected)
                     (link-index :selector-entry)
                     (t (or (info-segment-face segment) :default-text)))))
        (multiple-value-bind (fg bg ts opts)
            (resolve-global-face-inks face)
          (flet ((draw ()
                   (draw-text-at pane row col text fg bg ts char-w char-h
                                 :drawing-options opts)))
            (if link
                (clim:with-output-as-presentation
                    (pane link 'info-link-ref :single-box t)
                  (draw))
                (draw))))
        (incf col (length text))))))

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

(defun mcclim-draw-styled-entry (pane row cols entry char-w char-h)
  "Draw one styled row ENTRY."
  (let ((text (getf entry :text))
        (face (or (getf entry :face) :default-text)))
    (multiple-value-bind (fg bg ts opts)
        (resolve-global-face-inks face)
      (fill-row pane row cols bg char-w char-h)
      (when (plusp cols)
        (draw-text-at pane row 0
                      (mcclim-fit-line (or text "") cols)
                      fg bg ts char-w char-h
                      :drawing-options opts)))))

(defun mcclim-entry-scroll-window (buf total-rows viewport-rows)
  "Return visible top and bottom row indices for a scrollable entry list."
  (let* ((height (max 0 viewport-rows))
         (max-scroll (max 0 (- total-rows height)))
         (scroll-offset (min (max 0 (buffer-scroll-offset buf)) max-scroll))
         (visible-bottom (- total-rows scroll-offset))
         (visible-top (max 0 (- visible-bottom height))))
    (setf (buffer-scroll-offset buf) scroll-offset)
    (values visible-top visible-bottom)))

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

(defun mcclim-render-empty-input-pane (pane buf rows cols char-w char-h)
  "Render no editable input for read-only/special-purpose buffers."
  (declare (ignore buf rows cols char-w char-h))
  (clear-pane-with-ink pane *mcclim-bg-ink*))

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
  (let ((entries nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))))
          :do (let* ((prefix (mcclim-listener-message-prefix buf msg))
                     (prefix-len (length prefix))
                     (prefix-spaces (make-string prefix-len
                                                 :initial-element #\Space))
                     (display-width (max 1 (- cols prefix-len)))
                     (face (mcclim-listener-message-face msg))
                     (first-row-p t))
                (dolist (line (message-display-line-strings msg))
                  (dolist (chunk (mcclim-wrap-display-line line display-width))
                    (let ((line-prefix (if first-row-p prefix prefix-spaces)))
                      (push (list :text (concatenate 'string line-prefix chunk)
                                  :face face
                                  :object (list :buffer buf
                                                :message msg
                                                :text line)
                                  :presentation-type 'listener-entry-ref)
                            entries))
                    (setf first-row-p nil)))))
    (nreverse entries)))

(defun mcclim-render-listener-footer (pane buf row cols char-w char-h)
  "Render a listener wholine-style footer at ROW."
  (multiple-value-bind (fg bg ts opts)
      (resolve-global-face-inks :who-line)
    (fill-row pane row cols bg char-w char-h)
    (draw-text-at pane row 0
                  (mcclim-fit-line (listener-wholine-text buf) cols)
                  fg bg ts char-w char-h
                  :drawing-options opts)))

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

(defun mcclim-render-listener-input-pane (pane buf rows cols char-w char-h)
  "Render BUF's listener input with a package-sensitive prompt."
  (clear-pane-with-ink pane *mcclim-bg-ink*)
  (when (plusp rows)
    (mcclim-render-message-lines pane (buffer-input-message buf)
                                 0 cols char-w char-h
                                 :show-cursor t
                                 :max-rows rows
                                 :prefix (listener-prompt-text buf)
                                 :render-images-p nil)))

(defun register-mcclim-core-buffer-presentations ()
  "Install McCLIM presentation functions for built-in special buffers."
  (register-buffer-type
   :help
   :description "Read-only help buffer."
   :major-mode "help"
   :presentation-function 'mcclim-render-help-buffer
   :input-presentation-function 'mcclim-render-empty-input-pane)
  (register-buffer-type
   :info
   :description "Read-only Info manual browser."
   :major-mode "info"
   :presentation-function 'mcclim-render-info-buffer
   :input-presentation-function 'mcclim-render-empty-input-pane
   :serialize-state-function 'info-serialize-buffer-state
   :restore-state-function 'info-restore-buffer-state)
  (register-buffer-type
   :customize
   :description "Interactive customization buffer."
   :major-mode "customize"
   :presentation-function 'mcclim-render-customize-buffer
   :input-presentation-function 'mcclim-render-empty-input-pane)
  (register-buffer-type
   :listener
   :description "Interactive Common Lisp listener buffer."
   :major-mode "listener"
   :presentation-function 'mcclim-render-listener-buffer
   :input-presentation-function 'mcclim-render-listener-input-pane
   :serialize-state-function 'listener-serialize-buffer-state
   :restore-state-function 'listener-restore-buffer-state)
  (register-buffer-type
   :font-editor
   :description "Interactive CADR-style bitmap font editor."
   :major-mode "font-editor"
   :presentation-function 'mcclim-render-font-editor-buffer
   :input-presentation-function 'font-editor-empty-input-pane
   :serialize-state-function 'font-editor-serialize-buffer-state
   :restore-state-function 'font-editor-restore-buffer-state))

(register-mcclim-core-buffer-presentations)

(defun mcclim-render-buffer (pane buf rows cols char-w char-h)
  "Render the transcript pane: title bar at row 0, then message history.
The editable input lives in the separate Drei input pane. Approval prompts
still render in the transcript because they are modal interaction state, not
ordinary input text."
  (let ((frame (and pane (ignore-errors (clim:pane-frame pane)))))
    (when (and frame
               (mcclim-native-transcript-scroll-p frame buf))
      (let ((presentation-function (buffer-presentation-function buf)))
        (multiple-value-bind (viewport-cols viewport-rows)
            (pane-viewport-grid-dimensions pane char-w char-h)
          (if presentation-function
              (progn
                (funcall presentation-function
                         pane buf viewport-rows viewport-cols char-w char-h)
                (return-from mcclim-render-buffer buf))
              (let ((approval (buffer-approval-pending buf))
                    (visible-messages nil)
                    (show-reasoning-p (buffer-show-reasoning-p buf))
                    (show-metadata-p (buffer-show-metadata-p buf)))
                (mcclim-stream-render-buffer-title pane buf)
                (dolist (msg (mcclim-transcript-history-messages buf))
                  (push msg visible-messages)
                  (let ((ptype
                          (mcclim-rendered-message-presentation-type msg)))
                    (clim:updating-output
                        (pane
                         :unique-id (list *mcclim-render-window-id* msg)
                         :id-test #'equal
                         :cache-value
                         (mcclim-message-cache-value
                          msg 0 viewport-cols
                          show-reasoning-p show-metadata-p)
                         :cache-test #'equal)
                      (clim:with-output-as-presentation
                          (pane msg ptype :single-box t)
                        (mcclim-stream-render-message pane
                                                      msg
                                                      viewport-cols
                                                      char-w
                                                      char-h
                                                      :show-reasoning-p show-reasoning-p
                                                      :show-metadata-p show-metadata-p))))
                  (terpri pane))
                (when approval
                  (mcclim-stream-render-approval-prompt pane approval viewport-cols))
                (mcclim-record-render-snapshot frame
                                               pane
                                               buf
                                               :buffer
                                               viewport-rows
                                               viewport-cols
                                               :input-start-row -1
                                               :history-height
                                               (max 0 (1- viewport-rows))
                                               :visible-messages
                                               (nreverse visible-messages))))
          (return-from mcclim-render-buffer buf)))))
  (let ((presentation-function (buffer-presentation-function buf)))
    (when presentation-function
      (return-from mcclim-render-buffer
        (funcall presentation-function pane buf rows cols char-w char-h))))
  (when (document-buffer-p buf)
    (return-from mcclim-render-buffer
      (mcclim-render-document-buffer pane buf rows cols char-w char-h)))
  (let* ((approval-p (buffer-approval-pending buf))
         (total-height (1- rows))
         (width cols)
         (input-height (if approval-p
                           (calculate-input-height buf total-height width)
                           0))
         (history-height (- total-height input-height))
         (input-start-row (if approval-p (1+ history-height) 0))
         (visible-messages nil))
    ;; Clear pane and render title bar
    (clear-pane-with-ink pane *mcclim-bg-ink*)
    (mcclim-render-buffer-title pane buf cols char-w char-h)
    ;; Collect history messages
    (let ((history-messages nil))
      (loop :for msg := (buffer-first-message buf) :then (message-next msg)
            :while (and msg (not (eq msg (buffer-input-message buf))))
            :do (when (message-visible-for-buffer-p msg buf)
                  (push msg history-messages)))
      (setf history-messages (nreverse history-messages))
      ;; Calculate visual heights
      (let* ((show-reasoning-p (buffer-show-reasoning-p buf))
             (show-metadata-p (buffer-show-metadata-p buf))
             (msg-heights (mapcar (lambda (m)
                                    (mcclim-message-visual-height
                                     m width char-w char-h
                                     :show-reasoning-p show-reasoning-p
                                     :show-metadata-p show-metadata-p
                                     :render-images-p t))
                                  history-messages))
             (total-history-rows (reduce #'+ msg-heights :initial-value 0))
             (max-scroll (max 0 (- total-history-rows history-height)))
             (scroll-offset (min (buffer-scroll-offset buf) max-scroll))
             (visible-bottom (- total-history-rows scroll-offset))
             (visible-top (- visible-bottom history-height))
             (history-end-row (1+ history-height)))
        (setf (buffer-scroll-offset buf) scroll-offset)
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
                            (ptype
                              (mcclim-rendered-message-presentation-type
                               msg)))
                        (push msg visible-messages)
                        (clim:updating-output
                            (pane
                             :unique-id (list *mcclim-render-window-id* msg)
                             :id-test #'equal
                             :cache-value
                             (mcclim-message-cache-value
                              msg screen-row width
                              show-reasoning-p show-metadata-p)
                             :cache-test #'equal)
                          (clim:with-output-as-presentation
                              (pane msg ptype :single-box t)
                            (mcclim-render-message-lines
                             pane msg screen-row width
                             char-w char-h
                             :max-rows history-end-row
                             :show-reasoning-p show-reasoning-p
                             :show-metadata-p show-metadata-p)))))))))
    (when approval-p
      (mcclim-render-approval-prompt pane buf input-start-row
                                     cols char-w char-h rows))
    (mcclim-record-render-snapshot (clim:pane-frame pane)
                                   pane
                                   buf
                                   :buffer
                                   rows
                                   cols
                                   :input-start-row input-start-row
                                   :history-height history-height
                                   :visible-messages (nreverse visible-messages))))

(defun mcclim-render-document-buffer (pane buf rows cols char-w char-h)
  "Render BUF as a full-pane editable document buffer."
  (clear-pane-with-ink pane *mcclim-bg-ink*)
  (let ((row 0)
        (visible-messages nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))) (< row rows))
          :do (push msg visible-messages)
              (incf row (mcclim-render-message-lines pane msg row cols
                                                     char-w char-h
                                                     :max-rows rows)))
    (let ((text-height (max 1 (- rows row))))
      (multiple-value-bind (start-row scroll-offset)
          (scratch-buffer-scroll-geometry buf text-height cols)
        (setf (buffer-scroll-offset buf) scroll-offset)
        (mcclim-render-message-lines pane
                                     (buffer-input-message buf)
                                     (+ row start-row)
                                     cols
                                     char-w
                                     char-h
                                     :show-cursor t
                                     :max-rows rows
                                     :prefix ""
                                     :render-images-p nil)
        (mcclim-record-render-snapshot (clim:pane-frame pane)
                                       pane
                                       buf
                                       :document-buffer
                                       rows
                                       cols
                                       :input-start-row (+ row start-row)
                                       :history-height row
                                       :visible-messages
                                       (nreverse visible-messages))))))

;;; --------------------------------------------------------------------------
;;; Message Line Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-text-block
    (pane msg content line row width display-width prefix prefix-len
     fg bg ts drawing-options char-w char-h underline-p show-cursor max-rows
     first-output-p cursor-y cursor-x)
  "Render one text display block and return updated row/cursor state."
  (let* ((tool-face-name (tool-line-base-face-name msg content))
         (content-len (length content))
         (num-wraps (wrapped-line-count content display-width)))
    (dotimes (wrap-idx num-wraps)
      (when (< row max-rows)
        (let* ((chunk-start (* wrap-idx display-width))
               (chunk-end (min (* (1+ wrap-idx) display-width)
                               content-len))
               (chunk-text (subseq content chunk-start chunk-end))
               (chunk chunk-text)
               (first-row-p first-output-p))
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
                    :start chunk-start
                    :end chunk-end
                    :prefix (and first-row-p prefix))
                   char-w char-h))
                (progn
                  (fill-row pane row width bg char-w char-h)
                  (when first-row-p
                    (draw-text-at pane row 0
                                  (concatenate 'string prefix chunk)
                                  fg bg ts char-w char-h
                                  :drawing-options drawing-options)
                    (when underline-p
                      (draw-underline-at pane row 0
                                         (+ prefix-len (length chunk))
                                         fg char-w char-h))
                    (setf chunk nil))
                  (when chunk
                    (draw-text-at pane row prefix-len chunk
                                  fg bg ts char-w char-h
                                  :drawing-options drawing-options)
                    (when underline-p
                      (draw-underline-at pane row prefix-len (length chunk)
                                         fg char-w char-h))))))
          (when (and show-cursor
                     line
                     (>= row 0)
                     (eq line (message-point-line msg)))
            (let ((point-off (message-point-offset msg)))
              (when (and (>= point-off chunk-start)
                         (or (< point-off chunk-end)
                             (and (= wrap-idx (1- num-wraps))
                                  (= point-off chunk-end))))
                (let* ((cursor-offset (- point-off chunk-start))
                       (base-col (if first-row-p 0 prefix-len))
                       (cursor-text
                         (if (< point-off content-len)
                             (string (char content point-off))
                             " "))
                       (cursor-x-pixels
                         (* (+ base-col cursor-offset) char-w))
                       (cursor-width char-w))
                  (setf cursor-y row
                        cursor-x (+ prefix-len cursor-offset))
                  (draw-text-at-pixels pane cursor-x-pixels (* row char-h)
                                       cursor-text bg fg ts char-h
                                       :drawing-options drawing-options
                                       :background-width cursor-width))))))
          (setf first-output-p nil)
          (incf row))))
  (values row first-output-p cursor-y cursor-x))

(defun mcclim-render-message-lines (pane msg start-row width char-w char-h
                                    &key show-cursor (max-rows 1000)
                                      (prefix (message-sender-prefix msg))
                                      show-reasoning-p
                                      show-metadata-p
                                      (render-images-p t))
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
    (multiple-value-bind (fg bg ts opts underline-p)
        (resolve-face-inks resolved)
      (let ()
        (let ((first-output-p t)
              (blocks (if render-images-p
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
                                   :show-metadata-p show-metadata-p)))))
          (loop :for block :in blocks
                :while (< row max-rows)
                :do (ecase (getf block :type)
                      (:text
                       (multiple-value-setq
                           (row first-output-p cursor-y cursor-x)
                         (mcclim-render-text-block
                          pane msg
                          (getf block :text)
                          (getf block :source-line)
                          row width display-width prefix prefix-len
                          fg bg ts opts char-w char-h underline-p show-cursor
                          max-rows first-output-p cursor-y cursor-x)))
                      (:image
                       (let ((reference (getf block :reference))
                             (consumed 0))
                         (clim:with-output-as-presentation
                             (pane reference 'image-reference :single-box t)
                           (setf consumed
                                 (mcclim-render-image-block
                                  pane
                                  reference
                                  row width prefix prefix-len
                                  fg bg ts opts char-w char-h
                                  max-rows first-output-p)))
                         (setf first-output-p nil)
                         (incf row consumed))))))
        nil))
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
    (multiple-value-bind (fg bg ts opts)
        (resolve-global-face-inks :approval-header)
      (when (< row max-rows)
        (fill-row pane row width bg char-w char-h)
        (let ((header-text (format nil "-- PERMISSION REQUIRED: ~A " tool-name)))
          ;; Draw separator dashes
          (draw-text-at pane row 0
                        (make-string width :initial-element #\-)
                        fg bg ts char-w char-h
                        :drawing-options opts)
          ;; Draw header text over it
          (draw-text-at pane row 0
                        (subseq header-text 0 (min (length header-text) width))
                        fg bg ts char-w char-h
                        :drawing-options opts))
        (incf row)))
    ;; Raw sexpr
    (multiple-value-bind (fg bg ts opts)
        (resolve-global-face-inks :approval-code)
      (dolist (line (split-string-by-newline raw-sexpr))
        (when (< row max-rows)
          (fill-row pane row width bg char-w char-h)
          (draw-text-at pane row 0
                        (subseq line 0 (min (length line) width))
                        fg bg ts char-w char-h
                        :drawing-options opts)
          (incf row))))
    ;; Expanded form
    (multiple-value-bind (fg bg ts opts)
        (resolve-global-face-inks :approval-text)
      (dolist (line (split-string-by-newline expanded))
        (when (< row max-rows)
          (fill-row pane row width bg char-w char-h)
          (draw-text-at pane row 0
                        (subseq line 0 (min (length line) width))
                        fg bg ts char-w char-h
                        :drawing-options opts)
          (incf row))))
    ;; Extra display (diff)
    (let ((extra (cdr (assoc :display-extra approval))))
      (when extra
        (dolist (line (split-string-by-newline extra))
          (when (< row max-rows)
            (multiple-value-bind (fg bg ts opts)
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
                            fg bg ts char-w char-h
                            :drawing-options opts))
            (incf row)))))
    ;; Options
    (when (< (1+ row) max-rows)
      (incf row) ; blank line
      (multiple-value-bind (fg bg ts opts)
          (resolve-global-face-inks :approval-options)
        (fill-row pane row width bg char-w char-h)
        (let ((segments `((:approve 0 "[a]pprove")
                          (:deny 11 "[d]eny")
                          (:message 20 "[m]essage (deny with note to agent)"))))
          (dolist (segment segments)
            (destructuring-bind (action col text) segment
              (when (< col width)
                (clim:with-output-as-presentation
                    (pane action 'approval-action :single-box t)
                  (draw-text-at pane row col
                                (subseq text 0
                                        (min (length text) (- width col)))
                                fg bg ts char-w char-h
                                :drawing-options opts))))))))))

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
  "Render the buffer selector pane using ordinary CLIM stream output."
  (declare (ignore char-w char-h))
  (let* ((width cols)
         (height rows)
         (buffers *buffer-ring*)
         (num-buffers (length buffers))
         (current (frame-visible-buffer frame))
         (max-entries (max 1 (- height 7)))
         (scroll (mcclim-selector-visible-scroll *buffer-selector-index*
                                                 *buffer-selector-scroll*
                                                 max-entries
                                                 num-buffers)))
    (setf *buffer-selector-scroll* scroll)
    (clear-pane-with-ink pane *mcclim-bg-ink*)
    (mcclim-stream-write-global-face pane :selector-title "Agent Sessions")
    (mcclim-stream-write-global-face
     pane
     :selector-separator
     (make-string (min (max 8 width) 50) :initial-element #\─))
    (mcclim-stream-write-global-face
     pane
     :selector-header
     (format-selector-line "  " "NAME" "AGENT" "STATUS" "MSGS" width))
    (terpri pane)
    (loop :for absolute-idx :from scroll
          :below (min (+ scroll max-entries) num-buffers)
          :for buf := (nth absolute-idx buffers)
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
          :do (clim:with-output-as-presentation
                  (pane buf 'buffer-ref :single-box t)
                (mcclim-stream-write-global-face
                 pane
                 (if selected-p :selector-selected :selector-entry)
                 line)))
    (when (> num-buffers max-entries)
      (terpri pane)
      (mcclim-stream-write-global-face
       pane
       :selector-scroll
       (format nil "[~D-~D of ~D]"
               (1+ scroll)
               (min (+ scroll max-entries) num-buffers)
               num-buffers)))
    (terpri pane)
    (mcclim-stream-write-global-face
     pane
     :selector-footer
     "[RET] select  [C-g/q] cancel  [n] new  [k] kill")))

;;; --------------------------------------------------------------------------
;;; Model Selector Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-model-selector (pane rows cols char-w char-h frame)
  "Render the model selector pane using ordinary CLIM stream output."
  (declare (ignore char-w char-h frame))
  (let* ((width cols)
         (height rows)
         (entries *model-selector-entries*)
         (num-entries (length entries))
         (max-visible (max 1 (- height 7)))
         (scroll (mcclim-selector-visible-scroll *model-selector-index*
                                                 *model-selector-scroll*
                                                 max-visible
                                                 num-entries)))
    (setf *model-selector-scroll* scroll)
    (clear-pane-with-ink pane *mcclim-bg-ink*)
    (mcclim-stream-write-global-face pane :selector-title "Select Model")
    (mcclim-stream-write-global-face
     pane
     :selector-separator
     (make-string (min (max 8 width) 50) :initial-element #\─))
    (mcclim-stream-write-global-face
     pane
     :selector-header
     (format-model-selector-line "  " "PROVIDER" "MODEL" width))
    (terpri pane)
    (loop :for absolute-idx :from scroll
          :below (min (+ scroll max-visible) num-entries)
          :for entry := (nth absolute-idx entries)
          :for selected-p := (= absolute-idx *model-selector-index*)
          :for active-p := (getf entry :active-p)
          :for provider := (string-downcase (symbol-name (getf entry :provider)))
          :for model := (getf entry :model)
          :for marker := (cond ((and selected-p active-p) "▸*")
                               (selected-p "▸ ")
                               (active-p " *")
                               (t "  "))
          :for line := (format-model-selector-line marker provider model width)
          :do (clim:with-output-as-presentation
                  (pane entry 'model-ref :single-box t)
                (mcclim-stream-write-global-face
                 pane
                 (if selected-p :selector-selected :selector-entry)
                 line)))
    (when (> num-entries max-visible)
      (terpri pane)
      (mcclim-stream-write-global-face
       pane
       :selector-scroll
       (format nil "[~D-~D of ~D]"
               (1+ scroll)
               (min (+ scroll max-visible) num-entries)
               num-entries)))
    (terpri pane)
    (mcclim-stream-write-global-face
     pane
     :selector-footer
     "[RET] select  [C-g/q] cancel  * = active")))

;;; --------------------------------------------------------------------------
;;; Think Selector Rendering
;;; --------------------------------------------------------------------------

(defun mcclim-render-think-selector (pane rows cols char-w char-h frame)
  "Render the think-level selector pane using ordinary CLIM stream output."
  (declare (ignore char-w char-h frame))
  (let* ((width cols)
         (height rows)
         (entries *think-selector-entries*)
         (num-entries (length entries))
         (max-visible (max 1 (- height 7)))
         (scroll (mcclim-selector-visible-scroll *think-selector-index*
                                                 *think-selector-scroll*
                                                 max-visible
                                                 num-entries)))
    (setf *think-selector-scroll* scroll)
    (clear-pane-with-ink pane *mcclim-bg-ink*)
    (mcclim-stream-write-global-face pane :selector-title "Select Think Level")
    (mcclim-stream-write-global-face
     pane
     :selector-separator
     (make-string (min (max 8 width) 50) :initial-element #\─))
    (mcclim-stream-write-global-face
     pane
     :selector-header
     (format-think-selector-line "  " "THINK LEVEL" width))
    (terpri pane)
    (loop :for absolute-idx :from scroll
          :below (min (+ scroll max-visible) num-entries)
          :for entry := (nth absolute-idx entries)
          :for selected-p := (= absolute-idx *think-selector-index*)
          :for active-p := (getf entry :active-p)
          :for label := (getf entry :display)
          :for marker := (cond ((and selected-p active-p) "▸*")
                               (selected-p "▸ ")
                               (active-p " *")
                               (t "  "))
          :for line := (format-think-selector-line marker label width)
          :do (clim:with-output-as-presentation
                  (pane entry 'think-level-ref :single-box t)
                (mcclim-stream-write-global-face
                 pane
                 (if selected-p :selector-selected :selector-entry)
                 line)))
    (when (> num-entries max-visible)
      (terpri pane)
      (mcclim-stream-write-global-face
       pane
       :selector-scroll
       (format nil "[~D-~D of ~D]"
               (1+ scroll)
               (min (+ scroll max-visible) num-entries)
               num-entries)))
    (terpri pane)
    (mcclim-stream-write-global-face
     pane
     :selector-footer
     "[RET] select  [C-g/q] cancel  default = clear  * = active")))

(defun mcclim-render-session-tree-selector (pane rows cols char-w char-h frame)
  "Render the session tree selector pane using ordinary CLIM stream output."
  (declare (ignore char-w char-h frame))
  (session-tree-selector-update-filter)
  (let* ((width cols)
         (height rows)
         (items *session-tree-selector-filtered-items*)
         (num-items (length items))
         (max-visible (max 1 (- height 7)))
         (scroll (mcclim-selector-visible-scroll *session-tree-selector-index*
                                                 *session-tree-selector-scroll*
                                                 max-visible
                                                 num-items)))
    (setf *session-tree-selector-scroll* scroll)
    (clear-pane-with-ink pane *mcclim-bg-ink*)
    (mcclim-stream-write-global-face
     pane
     :selector-title
     (format nil "Session Tree: ~A"
             (if *session-tree-selector-buffer*
                 (buffer-name *session-tree-selector-buffer*)
                 "")))
    (mcclim-stream-write-global-face
     pane
     :selector-separator
     (make-string (min (max 8 width) 50) :initial-element #\─))
    (mcclim-stream-write-global-face
     pane
     :selector-header
     (format nil "  filter:~(~A~)  search:~A"
             *session-tree-selector-filter-mode*
             *session-tree-selector-search*))
    (terpri pane)
    (loop :for absolute-idx :from scroll
          :below (min (+ scroll max-visible) num-items)
          :for item := (nth absolute-idx items)
          :for selected-p := (= absolute-idx *session-tree-selector-index*)
          :for marker := (if selected-p "> " "  ")
          :for line := (format-session-tree-selector-line marker item width)
          :do (clim:with-output-as-presentation
                  (pane item 'session-tree-entry-ref :single-box t)
                (mcclim-stream-write-global-face
                 pane
                 (if selected-p :selector-selected :selector-entry)
                 line)))
    (when (> num-items max-visible)
      (terpri pane)
      (mcclim-stream-write-global-face
       pane
       :selector-scroll
       (format nil "[~D-~D of ~D]"
               (1+ scroll)
               (min (+ scroll max-visible) num-items)
               num-items)))
    (terpri pane)
    (mcclim-stream-write-global-face
     pane
     :selector-footer
     "[RET] select  [C-g/q] cancel  L label  <- fold  -> unfold  C-o filter")))

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
  "Render the active completion list as ordinary CLIM stream output."
  (declare (ignore char-w char-h))
  (let* ((mode (mcclim-completion-popup-mode))
         (items (ecase mode
                  (:minibuffer *minibuffer-filtered-items*)
                  (:slash *slash-completion-filtered-items*)
                  (:skill *skill-completion-filtered-items*)))
         (positions (ecase mode
                      (:minibuffer *minibuffer-match-positions*)
                      (:slash *slash-completion-match-positions*)
                      (:skill *skill-completion-match-positions*)))
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
         (point (ecase mode
                  (:minibuffer *minibuffer-point*)
                  (:slash (length input))
                  (:skill (length input))))
         (no-match-text (ecase mode
                          (:minibuffer "")
                          (:slash "No matching slash commands")
                          (:skill "No matching skills")))
         (total (length items))
         (max-height (ecase mode
                       (:minibuffer *minibuffer-max-height*)
                       (:slash *slash-completion-max-height*)
                       (:skill *skill-completion-max-height*)))
         (max-item-rows (max 0 (min (or max-height 12) (- rows 4))))
         (display-total (if (eq mode :minibuffer) total (max 1 total)))
         (item-rows (min display-total (max 0 (1- rows)))))
    (clear-pane-with-ink pane *mcclim-bg-ink*)
    (let* ((prompt-line (concatenate 'string prompt-str input))
           (display-width (max 1 cols))
           (visible (subseq prompt-line 0 (min (length prompt-line)
                                               display-width))))
      (declare (ignore point))
      (mcclim-stream-write-global-face pane :minibuffer-prompt visible))
    (if (and (not (eq mode :minibuffer)) (zerop total) (plusp item-rows))
        (mcclim-stream-write-global-face pane :minibuffer-candidate
                                         no-match-text)
        (loop :for row-idx :from 0 :below item-rows
              :for item-idx := (+ scroll row-idx)
              :while (< item-idx total)
              :for item := (nth item-idx items)
              :for row := (1+ row-idx)
              :for candidate-object := (list :index item-idx :item item)
              :for candidate-ptype := (ecase mode
                                       (:minibuffer 'minibuffer-candidate-ref)
                                       (:slash 'slash-candidate-ref)
                                       (:skill 'skill-candidate-ref))
              :for display := (minibuffer-item-display item)
              :for display-trimmed := (subseq display 0
                                              (min (length display)
                                                   (max 1 (- cols 2))))
              :for match-pos := (nth item-idx positions)
              :for selected-p := (= item-idx selected)
              :do
                 (let* ((base-face-name (if selected-p :minibuffer-selected :minibuffer-candidate))
                        (match-face-name (if selected-p
                                             :minibuffer-selected-match
                                             :minibuffer-match))
                        (padding (max 0 (- cols 2 (length display-trimmed)))))
                   (clim:with-output-as-presentation
                       (pane candidate-object candidate-ptype :single-box t)
                     (mcclim-stream-write-global-face pane base-face-name "  "
                                                      :newlinep nil)
                     (mcclim-stream-render-fuzzy-text pane
                                                      display-trimmed
                                                      match-pos
                                                      base-face-name
                                                      match-face-name)
                     (when (plusp padding)
                       (mcclim-stream-write-global-face
                        pane
                        base-face-name
                        (make-string padding :initial-element #\Space)
                        :newlinep nil))
                     (terpri pane)))))))

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
  "Handle pointer scroll EVENT according to active Clawmacs UI mode."
  (let ((key (mcclim-pointer-scroll-key event)))
    (when key
      (cond
        (*slash-completion-active*
         (if (eq key :page-up)
             (slash-completion-prev-item)
             (slash-completion-next-item))
         t)
        (*skill-completion-active*
         (if (eq key :page-up)
             (skill-completion-prev-item)
             (skill-completion-next-item))
         t)
        (*minibuffer-active*
         (if (eq key :page-up)
             (minibuffer-prev-item)
             (minibuffer-next-item))
         t)
        (*buffer-selector-active*
         (handle-buffer-selector-key (if (eq key :page-up) :up :down))
         t)
        (*model-selector-active*
         (handle-model-selector-key (if (eq key :page-up) :up :down)
                                    (frame-visible-buffer frame))
         t)
        (*think-selector-active*
         (handle-think-selector-key (if (eq key :page-up) :up :down)
                                    (frame-visible-buffer frame))
         t)
        (*session-tree-selector-active*
         (handle-session-tree-selector-key (if (eq key :page-up) :up :down))
         t)
        (t nil)))))

;;; --------------------------------------------------------------------------
;;; Redisplay Helper
;;; --------------------------------------------------------------------------

(defun mcclim-live-frames ()
  "Return a stable list of active McCLIM frames."
  (bt:with-lock-held (*mcclim-live-frames-lock*)
    (copy-list *mcclim-live-frames*)))

(defun mcclim-register-frame (frame)
  "Register FRAME for queued redisplay notifications."
  (bt:with-lock-held (*mcclim-live-frames-lock*)
    (pushnew frame *mcclim-live-frames* :test #'eq))
  (add-hook '*after-buffer-display-change-hook*
            'mcclim-buffer-display-change-hook)
  frame)

(defun mcclim-unregister-frame (frame)
  "Stop sending queued redisplay notifications to FRAME."
  (bt:with-lock-held (*mcclim-live-frames-lock*)
    (setf *mcclim-live-frames*
          (remove frame *mcclim-live-frames* :test #'eq))
    (unless *mcclim-live-frames*
      (remove-hook '*after-buffer-display-change-hook*
                   'mcclim-buffer-display-change-hook)))
  frame)

(defun mcclim-queue-display-change (frame buffer reason &key (force-p t))
  "Wake FRAME's CLIM event loop because BUFFER changed for display REASON."
  (when (and frame (not (frame-quit-flag frame)))
    (ignore-errors
      (let ((sheet (clim:frame-top-level-sheet frame)))
        (clim:queue-event
         sheet
         (make-instance 'clawmacs-display-change-event
                        :sheet sheet
                        :buffer buffer
                        :reason reason
                        :force-p force-p)))))
  frame)

(defun mcclim-buffer-display-change-hook (buffer reason)
  "Hook function that turns buffer display changes into CLIM events."
  (dolist (frame (mcclim-live-frames))
    (mcclim-queue-display-change frame buffer reason :force-p t))
  nil)

(defun update-pane-sizes (frame)
  "Resize fixed panes and the input pane based on current wrapped input."
  (let ((char-h (frame-char-height frame))
        (char-w (frame-char-width frame))
        (changed-p nil))
    (when (plusp char-h)
      (mcclim-update-main-pane-space-requirements frame)
      (unless (= char-h (frame-pane-space-char-height frame))
        (setf (frame-pane-space-char-height frame) char-h)
        (setf changed-p t)
        (let ((compose-pane (clim:find-pane-named frame 'compose-pane)))
          (when compose-pane
            (clim:change-space-requirements compose-pane
                                            :height char-h
                                            :min-height char-h
                                            :max-height char-h)))
        (let ((ml-pane (clim:find-pane-named frame 'modeline-pane)))
          (when ml-pane
            (clim:change-space-requirements ml-pane
                                            :height char-h
                                            :min-height char-h
                                            :max-height char-h)))
        (let ((wl-pane (clim:find-pane-named frame 'who-line-pane)))
          (when wl-pane
            (clim:change-space-requirements wl-pane
                                            :height char-h
                                            :min-height char-h
                                            :max-height char-h))))
      (let* ((selector-pane (clim:find-pane-named frame 'selector-pane))
             (selector-height (if (mcclim-selector-pane-active-p)
                                  (* char-h
                                     (mcclim-selector-pane-target-rows))
                                  0))
             (completion-pane (clim:find-pane-named frame 'completion-pane))
             (completion-height (if (mcclim-completion-pane-active-p)
                                    (* char-h
                                       (mcclim-completion-pane-target-rows))
                                    0)))
        (when (and selector-pane
                   (/= selector-height (frame-selector-pane-space-height frame)))
          (setf (frame-selector-pane-space-height frame) selector-height)
          (setf changed-p t)
          (clim:change-space-requirements selector-pane
                                          :height selector-height
                                          :min-height selector-height
                                          :max-height selector-height))
        (when (and completion-pane
                   (/= completion-height
                       (frame-completion-pane-space-height frame)))
          (setf (frame-completion-pane-space-height frame) completion-height)
          (setf changed-p t)
          (clim:change-space-requirements completion-pane
                                          :height completion-height
                                          :min-height completion-height
                                          :max-height completion-height)))
      (when (plusp char-w)
        (let ((input-pane (frame-drei-input-pane frame))
              (main-pane (clim:find-pane-named frame 'main-pane))
              (buf (frame-visible-buffer frame)))
          (when (and input-pane main-pane buf)
            (multiple-value-bind (input-cols input-rows)
                (pane-viewport-grid-dimensions input-pane char-w char-h)
              (multiple-value-bind (_main-cols main-rows)
                  (pane-viewport-grid-dimensions main-pane char-w char-h)
                (declare (ignore _main-cols))
                (let* ((body-rows (max 3 (+ input-rows main-rows)))
                       (desired-rows
                         (mcclim-desired-input-pane-rows
                          buf body-rows input-cols))
                       (desired-height (* desired-rows char-h)))
                  (unless (= desired-height
                             (frame-input-pane-space-height frame))
                    (setf (frame-input-pane-space-height frame)
                          desired-height)
                    (setf changed-p t)
                    (clim:change-space-requirements input-pane
                                                    :height desired-height
                                                    :min-height desired-height
                                                    :max-height
                                                    desired-height)))))))))
    changed-p))

(defun mcclim-input-pane-prefix (buf)
  "Return the visual prefix rendered in BUF's input pane."
  (cond
    ((null buf) "")
    ((listener-buffer-p buf) (listener-prompt-text buf))
    (t "")))

(defun mcclim-desired-input-pane-rows (buf body-rows width)
  "Return the desired input-pane row count for BUF.

Document buffers keep the legacy fixed input strip because editing happens in
the main pane. Chat-like buffers grow and shrink with wrapped input so text
stays visible while composing."
  (if (or (null buf) (document-buffer-p buf))
      3
      (calculate-input-height buf body-rows width
                              :prefix (mcclim-input-pane-prefix buf))))

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
  "Dispatch one ESA/CLIM GESTURE through the Clawmacs keymap.
Returns (values need-redisplay-p force-redisplay-p)."
  (with-mcclim-frame-ui-state (frame)
    (if (not (mcclim-primary-frame-p frame))
        ;; Popup viewers are read-only; do not let keyboard input mutate shared
        ;; prefix state or fall through to CLIM's input editor.
        (values nil nil)
        (let* ((buf (frame-visible-buffer frame))
               (previous-buffer (current-buffer))
               (key (mcclim-normalize-gesture gesture buf))
               (force-redisplay-p nil))
          (file-debug-log "mcclim-input" "gesture ~S normalized to ~S"
                          gesture key)
          (when key
            (if (mcclim-handle-native-page-scroll frame key)
                (setf force-redisplay-p t)
                (let ((result (handle-key-event buf key)))
                  (when (eq result :quit)
                    (setf (frame-quit-flag frame) t)
                    (clim:frame-exit frame))
                  (when (eq result :redraw)
                    (setf force-redisplay-p t)))))
          (mcclim-sync-selected-window-from-current-buffer
           frame previous-buffer)
          ;; Even prefix-only keys return NIL from normalization but may change
          ;; who-line state, so the interactive frame should still redisplay.
          (values t force-redisplay-p)))))

(defun mcclim-poll-external-updates (frame)
  "Poll streaming/OAuth state for FRAME.
Returns true when application state may have changed."
  (when (mcclim-primary-frame-p frame)
    (let ((changed-p nil))
      (dolist (buf (frame-window-buffers frame))
        (when (buffer-pending-stream buf)
          (update-streaming-response buf)
          (setf changed-p t)))
      (when *openai-oauth-pending*
        (update-openai-oauth-login)
        (setf changed-p t))
      changed-p)))

(defun mcclim-update-scroll-page-size (frame)
  "Update the shared page-scroll size from FRAME's main pane geometry."
  (when (mcclim-primary-frame-p frame)
    (let* ((main-pane (clim:find-pane-named frame 'main-pane))
           (char-w (frame-char-width frame))
           (char-h (frame-char-height frame)))
      (when (and main-pane (plusp char-w) (plusp char-h))
        (multiple-value-bind (cols rows)
            (pane-viewport-grid-dimensions main-pane char-w char-h)
          (declare (ignore cols))
          (setf *scroll-page-size* (max 1 (- rows 3))))))))

(defun mcclim-frame-output-panes (frame)
  "Return FRAME panes that may have pending CLIM output."
  (remove nil
          (append
           (list (clim:find-pane-named frame 'selector-pane))
           (frame-window-panes-in-display-order frame)
           (list (clim:find-pane-named frame 'compose-pane)
                 (clim:find-pane-named frame 'input-pane)
                 (clim:find-pane-named frame 'completion-pane)
                 (clim:find-pane-named frame 'who-line-pane)
                 (clim:find-pane-named frame 'modeline-pane)
                 (clim:find-pane-named frame 'pointer-doc-pane)))))

(defun mcclim-flush-frame-output (frame)
  "Force pending output for FRAME's visible pane streams."
  (dolist (pane (mcclim-frame-output-panes frame))
    (ignore-errors
      (force-output pane))))

(defun mcclim-redisplay-frame (frame &key force-p)
  "Refresh FRAME through the standard CLIM redisplay path."
  (with-mcclim-frame-ui-state (frame)
    (mcclim-bind-workspace-parent frame)
    (mcclim-ensure-window-tree frame)
    (mcclim-sync-buffer-from-drei frame)
    (mcclim-update-scroll-page-size frame)
    (update-pane-sizes frame)
    (mcclim-sync-main-pane-to-buffer-scroll frame)
    (mcclim-sync-drei-from-buffer frame)
    (clim:redisplay-frame-panes frame :force-p force-p)
    (mcclim-sync-main-pane-to-buffer-scroll frame)
    (mcclim-flush-frame-output frame)
    (when (update-pane-sizes frame)
      (mcclim-sync-main-pane-to-buffer-scroll frame)
      (clim:redisplay-frame-panes frame :force-p force-p)
      (mcclim-sync-main-pane-to-buffer-scroll frame)
      (mcclim-flush-frame-output frame))))

;;; --------------------------------------------------------------------------
;;; ESA/Pulse Event Integration
;;; --------------------------------------------------------------------------

(defun mcclim-poll-needed-p (frame)
  "Return true when FRAME needs timer-driven provider/OAuth polling."
  (or (some #'buffer-pending-stream (frame-window-buffers frame))
      *openai-oauth-pending*))

(defun mcclim-input-event-sources (frame)
  "Return sheets/streams that may receive interactive gestures for FRAME.

McCLIM's click-to-focus policy can focus a child pane.  Window managers differ
in whether keyboard focus is restored to the top-level sheet or to the child
sheet that was clicked, so Clawmacs' custom top level has to watch the panes
that can receive keyboard/pointer gestures instead of blocking only on the
top-level sheet."
  (labels ((collect-sheet-and-descendants (sheet seen)
             (when (and sheet (not (gethash sheet seen)))
               (setf (gethash sheet seen) t)
               (cons sheet
                     (loop :for child :in (ignore-errors (clim:sheet-children sheet))
                           :nconc (collect-sheet-and-descendants child seen))))))
    (let ((seen (make-hash-table :test #'eq)))
      (remove nil
              (append
               (list (ignore-errors (clim:frame-standard-input frame)))
               (collect-sheet-and-descendants
                (clim:find-pane-named frame 'selector-pane)
                seen)
               (mapcan (lambda (pane)
                         (collect-sheet-and-descendants pane seen))
                       (frame-window-panes-in-display-order frame))
               (collect-sheet-and-descendants
                (clim:find-pane-named frame 'completion-pane)
                seen)
               (collect-sheet-and-descendants
                (frame-drei-input-pane frame)
                seen))))))

(defun mcclim-input-event-available-p (frame)
  "Return true when any interactive pane for FRAME has queued raw input."
  (some (lambda (source)
          (ignore-errors
            (clim:event-listen source)))
        (mcclim-input-event-sources frame)))

(defun mcclim-actionable-input-event-p (event)
  "Return true when EVENT is a gesture Clawmacs should dispatch directly."
  (or (typep event 'clim:key-press-event)
      (typep event 'clim:pointer-button-press-event)
      (typep event 'clim:pointer-motion-event)
      (typep event 'clime:pointer-scroll-event)))

(defun mcclim-nearest-output-recording-pane (sheet frame)
  "Return SHEET or an ancestor that records output for FRAME, or NIL."
  (loop :for current := sheet
          :then (ignore-errors (clim:sheet-parent current))
        :while current
        :when (and (eq (ignore-errors (clim:pane-frame current)) frame)
                   (ignore-errors
                     (clim:output-recording-stream-p current)))
          :return current))

(defun mcclim-pointer-tracking-pane (frame event)
  "Return the pane whose presentations should answer pointer feedback."
  (mcclim-nearest-output-recording-pane (clim:event-sheet event) frame))

(defun mcclim-pointer-button-target-sheet (frame event)
  "Return the meaningful pane/sheet that should receive EVENT."
  (let ((sheet (and event (ignore-errors (clim:event-sheet event)))))
    (or (mcclim-sheet-ancestor-of-type sheet 'clawmacs-drei-input-pane)
        (mcclim-pointer-tracking-pane frame event)
        sheet)))

(defun mcclim-handle-presentation-button-press (frame pane event)
  "Handle EVENT through CLIM's native presentation command path on PANE."
  (let ((command-table (clim:frame-command-table frame))
        (handled-p nil))
    (clim:with-input-context
        (`(clim:command :command-table ,command-table))
        (object)
        (ignore-errors
          (clim:frame-input-context-button-press-handler frame pane event))
      (t
       (setf handled-p t)
       (clim:execute-frame-command
        frame
        (climi::ensure-complete-command object command-table nil))))
    handled-p))

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

(defun mcclim-presentation-object-safe (presentation)
  "Return PRESENTATION's object, or NIL when the runtime does not expose it."
  (let* ((package (find-package :clim))
         (symbol (and package (find-symbol "PRESENTATION-OBJECT" package))))
    (when (and symbol (fboundp symbol))
      (ignore-errors
        (funcall symbol presentation)))))

(defun mcclim-hover-summary-text (text &optional (limit 48))
  "Return TEXT normalized to one line and bounded to LIMIT characters."
  (let* ((raw (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (or text "")))
         (single-line (substitute #\Space #\Newline raw)))
    (cond
      ((zerop (length single-line)) "")
      ((<= (length single-line) limit) single-line)
      (t (concatenate 'string (subseq single-line 0 limit) "...")))))

(defun mcclim-hover-object-summary (presentation-type object)
  "Return a concise object summary for PRESENTATION-TYPE and OBJECT."
  (labels ((plist-display (value)
             (cond
               ((null value) nil)
               ((stringp value) value)
               ((and (listp value) (getf value :display))
                (getf value :display))
               ((and (listp value) (getf value :label))
                (getf value :label))
               (t nil))))
    (let ((summary
            (case presentation-type
              (buffer-ref
               (and (typep object 'buffer)
                    (format nil "buffer ~S" (buffer-name object))))
              (clawmacs-window-ref
               (and (typep object 'clawmacs-window)
                    (let ((buffer (clawmacs-window-buffer object)))
                      (if buffer
                          (format nil "window ~D showing ~S"
                                  (clawmacs-window-id object)
                                  (buffer-name buffer))
                          (format nil "window ~D"
                                  (clawmacs-window-id object))))))
              (model-ref
               (or (and (listp object)
                        (getf object :provider)
                        (getf object :model)
                        (format nil "model ~A/~A"
                                (getf object :provider)
                                (getf object :model)))
                   (plist-display object)))
              (think-level-ref
               (or (and (listp object)
                        (getf object :think-level)
                        (format nil "think level ~A"
                                (getf object :think-level)))
                   (plist-display object)))
              (session-tree-entry-ref
               (or (plist-display object)
                   (and (typep object 'session-tree-node)
                        (let ((label (session-tree-node-label object)))
                          (and label
                               (format nil "session entry ~A" label))))))
              ((rendered-message-ref chat-message)
               (and (typep object 'message)
                    (format nil "~(~A~) message"
                            (message-sender object))))
              (tool-call
               (and (typep object 'message)
                    (let ((preview (mcclim-hover-summary-text
                                    (message-text object)
                                    32)))
                      (if (plusp (length preview))
                          (format nil "tool call ~A" preview)
                          "tool call"))))
              (tool-result
               (and (typep object 'message)
                    (let ((preview (mcclim-hover-summary-text
                                    (message-text object)
                                    32)))
                      (if (plusp (length preview))
                          (format nil "tool result ~A" preview)
                          "tool result"))))
              (image-reference
               (and (typep object 'display-image-reference)
                    (let ((alt (mcclim-hover-summary-text
                                (display-image-reference-alt object)
                                24))
                          (path (display-image-reference-path object)))
                      (if (plusp (length alt))
                          (format nil "image ~S (~A)" path alt)
                          (format nil "image ~S" path)))))
              (approval-action
               (format nil "approval action ~A" object))
              ((minibuffer-candidate-ref skill-candidate-ref slash-candidate-ref)
               (let ((display (ignore-errors (minibuffer-item-display object))))
                 (and display
                      (format nil "~A ~A"
                              (string-downcase
                               (symbol-name presentation-type))
                              display))))
              (help-line-ref
               (and (listp object)
                    (format nil "help line ~D"
                            (1+ (or (getf object :line) 0)))))
              (info-link-ref
               (and (typep object 'info-link)
                    (let ((label (mcclim-hover-summary-text
                                  (info-link-label object)
                                  32)))
                      (if (plusp (length label))
                          (format nil "Info link ~A" label)
                          "Info link"))))
              (customize-field-ref
               (and (listp object)
                    (let ((field (getf object :field)))
                      (format nil "customize field ~A"
                              (if field
                                  (customize-face-field-label field)
                                  (1+ (or (getf object :index) 0)))))))
              (listener-entry-ref
               (and (listp object)
                    (let* ((msg (getf object :message))
                           (preview (mcclim-hover-summary-text
                                     (or (getf object :text)
                                         (and msg (message-text msg)))
                                     32)))
                      (if (plusp (length preview))
                          (format nil "listener entry ~A" preview)
                          "listener entry"))))
              (font-editor-glyph-ref
               (and (integerp object)
                    (format nil "glyph ~D" object)))
              (font-editor-pixel-ref
               (and (listp object)
                    (format nil "pixel ~D,~D"
                            (or (getf object :x) 0)
                            (or (getf object :y) 0))))
              (font-editor-action-ref
               (and object
                    (format nil "font action ~A" object)))
              (otherwise
               (cond
                 ((null object) nil)
                 ((stringp object) object)
                 (t (mcclim-debug-object-summary object)))))))
      (when summary
        (mcclim-hover-summary-text summary 96)))))

(defun mcclim-hover-identity-documentation
    (pane-name presentation-type object)
  "Return Hyper-hover identity text for PANE-NAME and OBJECT."
  (let* ((pane-text (and pane-name
                         (string-downcase (symbol-name pane-name))))
         (object-text (mcclim-hover-object-summary presentation-type object)))
    (cond
      ((and pane-text object-text)
       (format nil "Pane: ~A | Object: ~A" pane-text object-text))
      (pane-text
       (format nil "Pane: ~A" pane-text))
      (object-text
       (format nil "Object: ~A" object-text))
      (t ""))))

(defun mcclim-hover-documentation-text
    (frame pane presentation modifier-state)
  "Return pointer documentation for PANE/PRESENTATION under MODIFIER-STATE."
  (let* ((presentation-type
           (and presentation
                (mcclim-presentation-type-symbol presentation)))
         (pane-name (mcclim-frame-pane-name frame pane))
         (pane-buffer (and frame
                           (if (typep pane 'clawmacs-transcript-pane)
                               (transcript-pane-buffer frame pane)
                               (frame-visible-buffer frame))))
         (object (and presentation
                      (mcclim-presentation-object-safe presentation)))
         (action-text
           (and presentation
                (mcclim-presentation-pointer-documentation presentation)))
         (blank-area-text
           (and (null presentation)
                (not (mcclim-hyper-modifier-p modifier-state))
                (eq pane-name 'main-pane)
                (let ((buf pane-buffer))
                  (and buf
                       (document-buffer-p buf)
                       "L: Move point")))))
    (if (mcclim-hyper-modifier-p modifier-state)
        (mcclim-hover-identity-documentation pane-name presentation-type object)
        (or action-text blank-area-text ""))))

(defun mcclim-presentation-pointer-documentation (presentation)
  "Return the pointer documentation string for PRESENTATION, or NIL."
  (case (mcclim-presentation-type-symbol presentation)
    (buffer-ref "L: Switch to this buffer")
    (clawmacs-window-ref "L: Select this window")
    (model-ref "L: Apply this model")
    (think-level-ref "L: Apply this think level")
    (session-tree-entry-ref "L: Navigate to this session entry")
    ((rendered-message-ref chat-message tool-call tool-result)
     "Describe this message")
    (image-reference "Describe this image")
    (approval-action "L: Choose approval action")
    (minibuffer-candidate-ref "L: Choose this candidate")
    (skill-candidate-ref "L: Insert this skill mention")
    (slash-candidate-ref "L: Insert this slash command")
    (customize-field-ref "L: Edit this customize field")
    (info-link-ref "L: Follow this Info link")
    (otherwise nil)))

(defun mcclim-find-interactive-presentation (pane x y)
  "Return the innermost interactive presentation at PANE position X/Y."
  (labels ((visit (record)
             (clim:map-over-output-records-containing-position
              #'visit record x y)
             (when (and (clim:presentationp record)
                        (mcclim-presentation-pointer-documentation record))
               (return-from mcclim-find-interactive-presentation record))))
    (visit (clim:stream-output-history pane))
    nil))

(defun mcclim-clear-hover-highlight (frame)
  "Remove any active hover highlight remembered on FRAME."
  (let ((presentation (frame-hover-presentation frame))
        (pane (frame-hover-pane frame)))
    (when (and presentation pane)
      (ignore-errors
        (clim:highlight-output-record presentation pane :unhighlight))))
  (setf (frame-hover-presentation frame) nil
        (frame-hover-pane frame) nil)
  frame)

(defun mcclim-refresh-pointer-doc-pane (frame)
  "Flush FRAME's native pointer documentation pane, if present."
  (let ((stream (ignore-errors (clim:frame-pointer-documentation-output frame))))
    (when stream
      (ignore-errors
        (force-output stream))))
  frame)

(defun mcclim-write-pointer-documentation (frame text)
  "Write TEXT to FRAME's native pointer documentation pane."
  (let ((stream (ignore-errors (clim:frame-pointer-documentation-output frame))))
    (when stream
      (ignore-errors
        (clim:window-clear stream))
      (when (and text (plusp (length text)))
        (ignore-errors
          (write-string text stream)))
      (ignore-errors
        (finish-output stream))))
  frame)

(defun mcclim-track-pointer-feedback (frame pane event)
  "Update FRAME's native pointer feedback and cache the semantic hover state."
  (multiple-value-bind (local-x local-y)
      (mcclim-event-position-in-pane event pane)
    (let* ((presentation
             (mcclim-find-interactive-presentation pane local-x local-y))
           (modifier-state
             (ignore-errors (clim:event-modifier-state event)))
           (doc-text
             (mcclim-hover-documentation-text
              frame pane presentation
              modifier-state))
           (doc-string (or doc-text ""))
           (highlight-changed-p
             (or (not (eq presentation (frame-hover-presentation frame)))
                 (not (eq (and presentation pane) (frame-hover-pane frame)))))
           (doc-changed-p
             (not (string= doc-string
                           (or (frame-pointer-documentation-text frame) "")))))
      (when (or (not (eq presentation (frame-hover-presentation frame)))
                (not (eq (and presentation pane) (frame-hover-pane frame))))
        (mcclim-clear-hover-highlight frame)
        (when (and presentation pane)
          (ignore-errors
            (clim:highlight-output-record presentation pane :highlight))))
      (setf (frame-hover-presentation frame) presentation
            (frame-hover-pane frame) (and presentation pane))
      (when doc-changed-p
        (setf (frame-pointer-documentation-text frame) doc-string)
        (mcclim-write-pointer-documentation frame doc-string))))
  t)

(defun mcclim-clear-pointer-feedback (frame)
  "Clear any highlighted presentation and pointer documentation on FRAME."
  (when (or (frame-hover-presentation frame)
            (plusp (length (or (frame-pointer-documentation-text frame) ""))))
    (mcclim-clear-hover-highlight frame)
    (setf (frame-pointer-documentation-text frame) "")
    (mcclim-write-pointer-documentation frame ""))
  t)

(defun mcclim-read-input-event-no-hang (frame)
  "Read one queued gesture from FRAME's interactive panes, if available.

Child panes may accumulate repaint, configure, pointer boundary, and other
window-system events while the window manager is resizing a tiled frame.  Those
events are not input gestures, and returning them here can starve key delivery
under StumpWM, so this reader drains pane queues until it finds an actionable
gesture or the queues are empty."
  (dolist (source (mcclim-input-event-sources frame))
    (loop
      :for event := (ignore-errors
                      (clim:event-read-no-hang source))
      :while event
      :do (when (mcclim-actionable-input-event-p event)
            (return-from mcclim-read-input-event-no-hang event)))))

(defun mcclim-read-event (frame)
  "Read the next CLIM event, timing out while provider/OAuth polling is needed.
McCLIM timer events are useful when the command loop reads application-pane
event queues directly.  Clawmacs reads the top-level sheet for window-manager
events and also watches focused child panes for raw gestures; the timeout keeps
provider streams moving even when no key or window event arrives."
  (let ((sheet (clim:frame-top-level-sheet frame)))
    (or (mcclim-read-input-event-no-hang frame)
        (multiple-value-bind (event reason)
            (clime:event-read-with-timeout
             sheet
             :timeout (and (mcclim-poll-needed-p frame) 0.05)
             :wait-function
             (lambda ()
               (mcclim-input-event-available-p frame)))
          (cond
            (event event)
            ((eq reason :wait-function)
             (mcclim-read-input-event-no-hang frame))
            (t nil))))))

(defun mcclim-poll-sheet (frame)
  "Return the sheet used for McCLIM pulse events."
  (or (clim:frame-standard-output frame)
      (clim:find-pane-named frame 'main-pane)
      (clim:frame-top-level-sheet frame)))

(defun mcclim-stop-polling (frame)
  "Cancel FRAME's provider/OAuth polling pulse, if any."
  (let ((event (frame-poll-pulse-event frame)))
    (when event
      (clime:delete-pulse-event event)
      (setf (frame-poll-pulse-event frame) nil)))
  frame)

(defun mcclim-ensure-polling (frame)
  "Start or stop FRAME's McCLIM pulse polling as runtime state requires."
  (cond
    ((and (not (frame-quit-flag frame))
          (mcclim-poll-needed-p frame)
          (null (frame-poll-pulse-event frame)))
     (setf (frame-poll-pulse-event frame)
           (clime:schedule-pulse-event
            (mcclim-poll-sheet frame) :clawmacs-poll 0.05)))
    ((and (not (mcclim-poll-needed-p frame))
          (frame-poll-pulse-event frame))
     (mcclim-stop-polling frame)))
  frame)

(defmethod clim:handle-event ((pane clawmacs-transcript-pane)
                              (event clim:pointer-button-press-event))
  (call-next-method))

(defun mcclim-activate-transcript-pane (frame pane)
  "Make transcript PANE the selected logical window in FRAME."
  (when (and frame (typep pane 'clawmacs-transcript-pane))
    (mcclim-select-window-pane frame pane)))

(defmethod clim:handle-event ((pane clawmacs-drei-input-pane)
                              (event clim:pointer-button-press-event))
  (let ((frame (clim:pane-frame pane)))
    (with-mcclim-frame-ui-state (frame)
      (call-next-method)
      (mcclim-handle-input-pane-click frame pane event))))

(defmethod clim:handle-event ((pane clawmacs-transcript-pane)
                              (event clawmacs-display-change-event))
  (let ((frame (clim:pane-frame pane)))
    (with-mcclim-frame-ui-state (frame)
      (mcclim-sync-drei-from-buffer frame :force-p t)
      (mcclim-poll-external-updates frame)
      (mcclim-redisplay-frame frame
                              :force-p (display-change-event-force-p event))
      (mcclim-ensure-polling frame))))

(defmethod clim:handle-event :after ((pane clawmacs-drei-input-pane)
                                     (event clim:key-press-event))
  (declare (ignore event))
  (let ((frame (clim:pane-frame pane)))
    (with-mcclim-frame-ui-state (frame)
      (mcclim-sync-buffer-from-drei frame)
      (mcclim-redisplay-frame frame :force-p t))))

(defmethod clim:redisplay-frame-panes :before ((frame clawmacs-gui)
                                               &key force-p)
  (declare (ignore force-p))
  (with-mcclim-frame-ui-state (frame)
    (mcclim-update-scroll-page-size frame)
    (update-pane-sizes frame)
    (mcclim-sync-drei-from-buffer frame)))

(defmethod clim:execute-frame-command :around ((frame clawmacs-gui) command)
  (declare (ignore command))
  (with-mcclim-frame-ui-state (frame)
    (mcclim-sync-buffer-from-drei frame)
    (prog1 (call-next-method)
      (mcclim-sync-drei-from-buffer frame :force-p t)
      (mcclim-ensure-polling frame))))

(defmethod clawmacs-esa-top-level ((frame clawmacs-gui)
                                   &key &allow-other-keys)
  "Run Clawmacs' McCLIM top level using ESA command processing.
The loop reads window-system events from the top-level sheet and raw gestures
from focused child panes. Keyboard gestures are dispatched through Clawmacs'
keymap so keys like C-u keep their editor meaning; CLIM presentation and window
events stay on the standard CLIM event path."
  (unless (eq (clim:frame-state frame) :enabled)
    (clim:enable-frame frame))
  (mcclim-bind-workspace-parent frame)
  (mcclim-ensure-window-tree frame)
  (mcclim-install-frame-command-table frame)
  (mcclim-sync-drei-from-buffer frame :force-p t)
  (mcclim-redisplay-frame frame :force-p t)
  (loop :until (frame-quit-flag frame)
        :for event := (mcclim-read-event frame)
        :do (with-mcclim-frame-ui-state (frame)
              (cond
                ((null event)
                 (when (mcclim-poll-external-updates frame)
                   (mcclim-sync-drei-from-buffer frame :force-p t)
                   (mcclim-redisplay-frame frame :force-p t)
                   (mcclim-ensure-polling frame)))
                ((typep event 'clawmacs-display-change-event)
                 (mcclim-sync-drei-from-buffer frame :force-p t)
                 (mcclim-poll-external-updates frame)
                 (mcclim-redisplay-frame
                  frame :force-p (display-change-event-force-p event))
                 (mcclim-ensure-polling frame))
                ((clim:event-matches-gesture-name-p event :clawmacs-poll)
                 (clim:execute-frame-command frame '(com-clawmacs-poll)))
                ((or (typep event 'clim:key-press-event)
                     (characterp event))
                 (clim:execute-frame-command
                  frame
                  (list 'com-clawmacs-dispatch-gestures (list event)))
                 (mcclim-redisplay-frame frame :force-p t))
                ((typep event 'clim:pointer-motion-event)
                 (let ((tracking-pane
                         (mcclim-pointer-tracking-pane frame event)))
                   (if tracking-pane
                       (mcclim-track-pointer-feedback
                        frame tracking-pane event)
                       (mcclim-clear-pointer-feedback frame))))
                ((typep event 'clim:pointer-button-press-event)
                 (let ((sheet (mcclim-pointer-button-target-sheet frame event)))
                   (when (typep sheet 'clawmacs-transcript-pane)
                     (mcclim-activate-transcript-pane frame sheet))
                   (unless (and (typep sheet 'clim:output-recording-stream)
                                (mcclim-handle-presentation-button-press
                                 frame sheet event))
                     (clim:handle-event sheet event))
                   (when (typep sheet 'clawmacs-drei-input-pane)
                     (mcclim-set-input-point-from-pane-click frame sheet event)
                     (mcclim-sync-buffer-from-drei frame)))
                 (mcclim-redisplay-frame frame))
                ((mcclim-handle-pointer-scroll frame event)
                 (mcclim-redisplay-frame frame))
                ((typep event 'clime:pointer-scroll-event)
                 (let ((tracking-pane
                         (mcclim-pointer-tracking-pane frame event)))
                   (when (typep tracking-pane 'clawmacs-transcript-pane)
                     (mcclim-activate-transcript-pane frame tracking-pane)))
                 (clim:handle-event (clim:event-sheet event) event)
                 (mcclim-sync-buffer-scroll-from-main-pane frame)
                 (mcclim-redisplay-frame frame))
                ((or (typep event 'clim:window-repaint-event)
                     (typep event 'clim:window-configuration-event))
                 (clim:handle-event (clim:event-sheet event) event)
                 (mcclim-redisplay-frame frame :force-p t))
                (t
                 (clim:handle-event (clim:event-sheet event) event)
                 (mcclim-redisplay-frame frame))))))

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
