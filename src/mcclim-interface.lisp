(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Fresh McCLIM Chat Interface
;;; --------------------------------------------------------------------------

(clim:define-presentation-type chat-message ()
  :inherit-from 'message)

(clim:define-presentation-method clim:presentation-typep
    (object (type chat-message))
  (typep object 'message))

(clim:define-gesture-name
    :describe-presentation :pointer-button-press (:left :control :shift))

(clim:define-presentation-type tool-approval ())
(clim:define-presentation-type tool-activity-summary ())
(clim:define-presentation-type minibuffer-command-candidate ())
(clim:define-presentation-type package-dashboard-entry-ref ())

(defparameter *buffer-presentation-default-columns* 100
  "Fallback display width passed to buffer presentation hooks.")

(defvar *buffer-input-presentation-text* nil
  "Compose text dynamically visible while rendering input presentation hooks.")

(defun buffer-input-presentation-text (buffer)
  "Return current compose text for BUFFER while rendering an input presentation."
  (or *buffer-input-presentation-text*
      (and buffer (message-text (buffer-input-message buffer)))
      ""))

(clim:define-presentation-method clim:presentation-typep
    (object (type tool-approval))
  (and (listp object)
       (not (null (assoc :tool-name object)))))

(clim:define-presentation-method clim:presentation-typep
    (object (type minibuffer-command-candidate))
  (and (listp object)
       (not (null (getf object :command)))))

(clim:define-presentation-method clim:presentation-typep
    (object (type package-dashboard-entry-ref))
  (and (listp object)
       (getf object :dashboard-buffer)
       (getf object :entry)))

(defclass clawmacs-chat-redisplay-event (clim:window-event)
  ())

(defclass clawmacs-chat-menu-refresh-event (clim:window-event)
  ())

(defvar *suppress-chat-redisplay-requests* nil
  "When non-nil, buffer display hooks should not queue chat redisplay events.
This is bound while a chat frame is already applying provider stream state to
avoid recursive update→notify→redisplay loops in the CLIM event thread.")

(defparameter *chat-transcript-follow-tail* t
  "When non-nil, the chat transcript scrolls to the bottom after redisplay.")

(clim:define-command-table clawmacs-chat-control-menu
  :menu (("Stop Response" :command com-chat-stop-response
          :documentation "Stop the active streaming response.")))

(defun chat-menu-check-label (enabled-p name)
  "Return NAME prefixed with a check mark when ENABLED-P."
  (format nil "~A ~A" (if enabled-p "✓" " ") name))

(defun chat-view-menu-items (&optional buffer)
  "Return dynamic menu items for transcript display options.
When BUFFER is nil, use the default buffer visibility settings."
  (let ((show-tool-results-p (if buffer
                                 (buffer-show-tool-results-p buffer)
                                 *default-show-tool-results*))
        (show-reasoning-p (if buffer
                              (buffer-show-reasoning-p buffer)
                              *default-show-reasoning-output*))
        (show-metadata-p (if buffer
                             (buffer-show-metadata-p buffer)
                             *default-show-metadata-output*)))
    `((,(chat-menu-check-label show-tool-results-p "Tool Results")
       :command com-chat-toggle-tool-results
       :documentation "Toggle tool result messages.")
      (,(chat-menu-check-label show-reasoning-p "Reasoning Output")
       :command com-chat-toggle-reasoning-output
       :documentation "Toggle provider reasoning blocks.")
      (,(chat-menu-check-label show-metadata-p "Metadata Output")
       :command com-chat-toggle-metadata-output
       :documentation "Toggle provider response metadata.")
      (,(chat-menu-check-label *debug-mode* "Debug Mode")
       :command com-chat-toggle-debug-mode
       :documentation "Toggle API debug messages."))))

(defun no-chat-menu-items-label (label)
  "Return a non-action menu item for an empty dynamic menu."
  `((,label :divider nil)))

(defun chat-skill-menu-items ()
  "Return dynamic menu items for file-backed skills."
  (let ((items
          (loop :for skill :in (list-skills :include-disabled t)
                :for key := (skill-path-key skill)
                :when key
                  :collect
                  `(,(chat-menu-check-label
                      (skill-enabled-p skill)
                      (skill-name skill))
                    :command (com-chat-toggle-skill ,key)
                    :documentation ,(skill-display-description skill)))))
    (or items (no-chat-menu-items-label "No skills available"))))

(defun chat-buffer-package-enabled-p (buffer package-name)
  "Return true when BUFFER explicitly enables PACKAGE-NAME."
  (and buffer
       (buffer-package-name-enabled-p buffer
                                      (manifest-package-name package-name))))

(defun chat-package-menu-items (buffer)
  "Return dynamic menu items for packages visible to BUFFER."
  (let ((items
          (loop :for definition :in (list-installed-packages :buffer buffer)
                :for name := (package-definition-name definition)
                :collect
                `(,(chat-menu-check-label
                    (chat-buffer-package-enabled-p buffer name)
                    name)
                  :command (com-chat-toggle-package ,name)
                  :documentation ,(package-display-description definition)))))
    (or items (no-chat-menu-items-label "No packages available"))))

(defun chat-effort-menu-label (buffer)
  "Return the top-level effort menu label for BUFFER."
  (if (null buffer)
      "Effort"
      (handler-case
          (let* ((entries (available-think-levels-for-selector buffer))
                 (active (and entries
                              (or (find-if (lambda (entry)
                                             (getf entry :active-p))
                                           entries)
                                  (first entries))))
                 (status (if active
                             (getf active :display)
                             "n/a")))
            (format nil "Effort: ~A" status))
        (error ()
          "Effort: n/a"))))

(defun chat-effort-unavailable-label (buffer)
  "Return the disabled effort menu label for BUFFER."
  (multiple-value-bind (provider model)
      (handler-case (resolve-buffer-provider-and-model buffer)
        (error () (values nil nil)))
    (if (and provider model)
        (format nil "Not available for ~A"
                (model-selector-display provider model))
        "Not available for the active model")))

(defun chat-effort-menu-items (buffer)
  "Return dynamic menu items for model effort selection."
  (cond
    ((null buffer)
     (no-chat-menu-items-label "No active chat buffer"))
    (t
     (let ((entries (available-think-levels-for-selector buffer)))
       (if entries
           (mapcar
            (lambda (entry)
              (let* ((provider (getf entry :provider))
                     (model (getf entry :model))
                     (display (getf entry :display))
                     (level (getf entry :level))
                     (model-display (model-selector-display provider model)))
                `(,(chat-menu-check-label (getf entry :active-p) display)
                  :command (com-chat-select-effort ,(or level ""))
                  :documentation
                  ,(if level
                       (format nil "Set reasoning effort to ~A for ~A."
                               display
                               model-display)
                       (format nil "Use the model default reasoning effort for ~A."
                               model-display)))))
            entries)
           (no-chat-menu-items-label
            (chat-effort-unavailable-label buffer)))))))

(defun chat-menu-context-buffer (context)
  "Return the chat buffer represented by CONTEXT."
  (cond
    ((null context) nil)
    ((typep context 'buffer) context)
    (t (chat-frame-buffer context))))

(defun chat-system-menu-items ()
  "Return dynamic menu items for system-level frame actions."
  '(("Recurse" :command com-chat-recurse
     :documentation "Open a fresh nested Clawmacs frame in a new process.")))

(defun make-chat-menu-bar-command-table (&optional context)
  "Return a frame-local chat menu command table for CONTEXT."
  (let* ((buffer (chat-menu-context-buffer context))
         (view-menu
           (clim:make-command-table
            nil
            :inherit-from nil
            :menu (chat-view-menu-items buffer)))
         (skills-menu
           (clim:make-command-table
            nil
            :inherit-from nil
            :menu (chat-skill-menu-items)))
         (packages-menu
           (clim:make-command-table
            nil
            :inherit-from nil
            :menu (chat-package-menu-items buffer)))
         (effort-menu
           (clim:make-command-table
            nil
            :inherit-from nil
            :menu (chat-effort-menu-items buffer)))
         (system-menu
           (clim:make-command-table
            nil
            :inherit-from nil
            :menu (chat-system-menu-items))))
    (clim:make-command-table
     nil
     :inherit-from '(clawmacs-chat-frame)
     :menu `(("Chat" :menu ,(clim:find-command-table
                             'clawmacs-chat-control-menu)
              :documentation "Chat controls.")
             ("View" :menu ,view-menu
              :documentation "Transcript display controls.")
             ("Skills" :menu ,skills-menu
              :documentation "Enable or disable skills.")
             ("Packages" :menu ,packages-menu
              :documentation "Enable or disable packages for this chat.")
             (,(chat-effort-menu-label buffer) :menu ,effort-menu
              :documentation "Select the model reasoning effort for this chat.")
             ("System" :menu ,system-menu
              :documentation "Launch nested Clawmacs instances and other system actions.")))))

(defun rebuild-chat-menu-bar-command-tables (&optional context)
  "Return a fresh chat menu command table for CONTEXT."
  (make-chat-menu-bar-command-table context))

(defun refresh-chat-frame-menu-bar (frame)
  "Refresh FRAME's frame-local dynamic menu-bar entries."
  (setf (clim:frame-command-table frame)
        (make-chat-menu-bar-command-table frame))
  frame)

(defun chat-message-kind (msg)
  "Return MSG's high-level display kind."
  (case (message-sender msg)
    (:user :user)
    (:system :system)
    (:tool-result :tool)
    (t :agent)))

(defun chat-message-ink (msg)
  "Return the CLIM ink used for MSG."
  (ecase (chat-message-kind msg)
    (:user (clim:make-rgb-color 0.10 0.25 0.55))
    (:agent (clim:make-rgb-color 0.10 0.10 0.10))
    (:tool (clim:make-rgb-color 0.12 0.34 0.18))
    (:system (clim:make-rgb-color 0.36 0.36 0.36))))

(defun chat-message-label (msg)
  "Return the display label for MSG."
  (ecase (chat-message-kind msg)
    (:user "user")
    (:agent "agent")
    (:tool "tool")
    (:system "system")))

(defstruct (chat-tool-activity-summary
            (:constructor make-chat-tool-activity-summary
                (&key messages tool-counts result-count first-id last-id)))
  "Display-only summary for one consecutive run of tool calls/results."
  (messages nil :type list)
  (tool-counts nil :type list)
  (result-count 0 :type integer)
  first-id
  last-id)

(defun chat-message-tool-use-blocks (msg)
  "Return tool_use blocks recorded in MSG's raw content."
  (content-tool-use-blocks (or (message-raw-content msg) nil)))

(defun chat-message-tool-result-blocks (msg)
  "Return tool_result blocks recorded in MSG's raw content."
  (remove-if-not (lambda (block)
                   (string= "tool_result" (content-block-type block)))
                 (or (message-raw-content msg) nil)))

(defun chat-tool-activity-message-p (msg)
  "Return true when MSG is a tool call/result display message."
  (or (chat-message-tool-use-blocks msg)
      (eq (message-sender msg) :tool-result)
      (chat-message-tool-result-blocks msg)))

(defun chat-increment-tool-count (name counts)
  "Return COUNTS with NAME incremented once, preserving first-seen order."
  (let ((cell (assoc name counts :test #'string=)))
    (if cell
        (progn
          (incf (cdr cell))
          counts)
        (append counts (list (cons name 1))))))

(defun chat-tool-activity-summary-from-run (messages)
  "Return a collapsed tool activity summary for consecutive MESSAGES."
  (let ((counts nil)
        (result-count 0))
    (dolist (msg messages)
      (dolist (tool-use (chat-message-tool-use-blocks msg))
        (let ((name (or (cdr (assoc :name tool-use)) "unknown")))
          (setf counts (chat-increment-tool-count name counts))))
      (incf result-count (length (chat-message-tool-result-blocks msg))))
    (make-chat-tool-activity-summary
     :messages messages
     :tool-counts counts
     :result-count result-count
     :first-id (chat-message-output-id (first messages))
     :last-id (chat-message-output-id (car (last messages))))))

(defun chat-transcript-messages (buf)
  "Return finalized, non-ephemeral messages for BUF."
  (loop :for msg := (buffer-first-message buf) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buf))))
        :unless (buffer-ephemeral-display-message-p msg)
          :collect msg))

(defun chat-transcript-display-items (buf)
  "Return transcript display items, collapsing consecutive tool activity by default."
  (let ((messages (chat-transcript-messages buf)))
    (if (not (buffer-collapse-tool-activity-p buf))
        messages
        (let ((items nil)
              (tool-run nil))
          (labels ((flush-tool-run ()
                     (when tool-run
                       (push (chat-tool-activity-summary-from-run
                              (nreverse tool-run))
                             items)
                       (setf tool-run nil))))
            (dolist (msg messages)
              (cond
                ((chat-tool-activity-message-p msg)
                 (unless (and (eq (message-sender msg) :tool-result)
                              (not (buffer-show-tool-results-p buf)))
                   (push msg tool-run)))
                (t
                 (flush-tool-run)
                 (push msg items))))
            (flush-tool-run)
            (nreverse items))))))

(defun chat-message-output-id (msg)
  "Return a stable incremental-redisplay id for MSG."
  (or (message-entry-id msg) msg))

(defun chat-tool-activity-summary-output-id (summary)
  "Return a stable incremental-redisplay id for SUMMARY."
  (list :tool-activity-summary
        (chat-tool-activity-summary-first-id summary)
        (chat-tool-activity-summary-last-id summary)))

(defun chat-display-item-output-id (item)
  "Return a stable incremental-redisplay id for ITEM."
  (if (chat-tool-activity-summary-p item)
      (chat-tool-activity-summary-output-id item)
      (chat-message-output-id item)))

(defun chat-message-cache-value (msg)
  "Return a cache value covering visible MSG state."
  (list (message-sender msg)
        (message-timestamp msg)
        (message-text msg)
        (message-metadata msg)
        (message-entry-id msg)
        (message-parent-entry-id msg)))

(defun chat-tool-activity-summary-cache-value (summary)
  "Return a cache value covering visible SUMMARY state."
  (list (chat-tool-activity-summary-tool-counts summary)
        (chat-tool-activity-summary-result-count summary)
        (mapcar #'chat-message-cache-value
                (chat-tool-activity-summary-messages summary))))

(defun chat-display-item-cache-value (item)
  "Return a cache value covering visible ITEM state."
  (if (chat-tool-activity-summary-p item)
      (chat-tool-activity-summary-cache-value item)
      (chat-message-cache-value item)))

(defun message-metadata-help-string (msg)
  "Return help-window text describing MSG metadata."
  (with-output-to-string (stream)
    (format stream "Message~2%")
    (format stream "Sender: ~A~%" (message-sender msg))
    (format stream "Timestamp: ~A~%" (or (message-timestamp msg) "none"))
    (format stream "Entry id: ~A~%" (or (message-entry-id msg) "none"))
    (format stream "Parent entry id: ~A~%"
            (or (message-parent-entry-id msg) "none"))
    (format stream "Read only: ~:[no~;yes~]~%" (message-read-only-p msg))
    (format stream "Text length: ~D~%" (length (message-text msg)))
    (format stream "Line count: ~D~%" (message-line-count msg))
    (format stream "Raw content blocks: ~D~%"
            (length (or (message-raw-content msg) nil)))
    (format stream "~%Metadata:~%~S~%" (message-metadata msg))))

(defun chat-approval-value (approval key)
  "Return KEY's value in APPROVAL."
  (cdr (assoc key approval)))

(defun chat-approval-display-string (approval)
  "Return a compact display string for a pending tool APPROVAL."
  (with-output-to-string (stream)
    (format stream "Approval required~%")
    (write-string
     (or (chat-approval-value approval :display-expanded)
         (chat-approval-value approval :display-raw)
         (chat-approval-value approval :tool-name)
         "")
     stream)
    (let ((extra (chat-approval-value approval :display-extra)))
      (unless (blank-string-p (or extra ""))
        (format stream "~2%~A" extra)))
    (format stream "~2%Decision: approve | deny")))

(defun display-chat-approval (stream approval)
  "Display pending APPROVAL as a semantic presentation."
  (clim:with-output-as-presentation
      (stream approval 'tool-approval :single-box t)
    (clim:with-drawing-options
        (stream :ink (clim:make-rgb-color 0.50 0.22 0.08))
      (write-string (chat-approval-display-string approval) stream)))
  (terpri stream)
  (terpri stream))

(defun chat-string-prefix-p (prefix string)
  "Return true when PREFIX is a prefix of STRING."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun chat-denial-reason-from-text (trimmed lower)
  "Return a denial reason encoded in TRIMMED/LOWER, or NIL."
  (dolist (prefix '("deny:" "deny " "no:" "no "))
    (when (chat-string-prefix-p prefix lower)
      (let ((reason
              (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (subseq trimmed (length prefix)))))
        (return (unless (blank-string-p reason)
                  reason))))))

(defun chat-approval-response-from-text (text)
  "Return an approval response encoded by compose TEXT."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (or text "")))
         (lower (string-downcase trimmed))
         (reason (chat-denial-reason-from-text trimmed lower)))
    (cond
      (reason (cons :deny-with-message reason))
      ((member lower '("d" "deny" "n" "no") :test #'string=) :deny)
      (t :approve))))

(defun handle-chat-compose-text (buf text)
  "Submit compose TEXT for BUF. Return true when TEXT was consumed."
  (cond
    ((buffer-approval-pending buf)
     (handle-approval-response buf (chat-approval-response-from-text text))
     t)
    ((buffer-user-input-pending buf)
     (set-message-text (buffer-input-message buf) text)
     (send-message buf)
     t)
    ((blank-string-p text)
     nil)
    (t
     (set-message-text (buffer-input-message buf) text)
     (send-message buf)
     t)))

(clim:define-application-frame clawmacs-message-help-frame ()
  ((message :initarg :message
            :reader help-frame-message))
  (:panes
   (help :application
         :display-function 'display-message-help
         :display-time :command-loop
         :width 520
         :height 360))
  (:layouts
   (default help)))

(defun display-message-help (frame stream)
  "Display FRAME's message metadata in STREAM."
  (write-string
   (message-metadata-help-string (help-frame-message frame))
   stream))

(defun open-message-help-window (msg)
  "Open a one-pane help frame for MSG metadata."
  (let ((frame (clim:make-application-frame
                'clawmacs-message-help-frame
                :message msg
                :pretty-name "Message Metadata")))
    (bt:make-thread
     (lambda ()
       (clim:run-frame-top-level frame))
     :name "clawmacs message metadata")
    frame))

(defun install-chat-compose-drei-keybindings ()
  "Install Clawmacs-specific Drei bindings expected by the compose pane.
Drei already binds C-j to newline-and-indent, but its gadget event bridge
ignores modifier key events by default. We also add the Clawmacs/Emacs-style
C-Backspace binding for backward word deletion, matching the old input pane."
  (esa:set-key 'drei-commands::com-newline-and-indent
               'drei:indent-table
               '((#\Newline :control)))
  (dolist (gesture '((#\Backspace :control)
                     (#\Rubout :control)))
    (esa:set-key `(drei-commands::com-backward-kill-word
                   ,clim:*numeric-argument-marker*)
                 'drei:deletion-table
                 (list gesture))))

(install-chat-compose-drei-keybindings)

(defun configure-chat-compose-pane (pane)
  "Enable CLIM stream soft wrapping for chat compose PANE when supported.
Drei compose panes implement the stream end-of-line protocol; if a pane does
not, leave the editor value untouched rather than inserting approximate hard
newlines."
  (when pane
    (ignore-errors
      (setf (clim:stream-end-of-line-action pane) :wrap*)))
  pane)

(defun chat-compose-pane-p (pane)
  "Return true when PANE is the compose pane of a Clawmacs chat frame."
  (let ((frame (ignore-errors (clim:pane-frame pane))))
    (and (typep frame 'clawmacs-chat-frame)
         (eq pane (ignore-errors (clim:find-pane-named frame 'compose))))))

(defun maybe-configure-chat-compose-pane (pane)
  "Configure PANE when it is the chat compose pane."
  (when (chat-compose-pane-p pane)
    (configure-chat-compose-pane pane)))

(defmethod clim:note-sheet-grafted :after ((pane clim:text-editor-pane))
  (maybe-configure-chat-compose-pane pane))

(defmethod clim:note-sheet-grafted :after ((pane drei:drei-gadget-pane))
  (maybe-configure-chat-compose-pane pane))

(defmethod clim:note-sheet-region-changed :after ((pane clim:text-editor-pane))
  (maybe-configure-chat-compose-pane pane))

(defmethod clim:note-sheet-region-changed :after ((pane drei:drei-gadget-pane))
  (maybe-configure-chat-compose-pane pane))

(defun chat-compose-submit-event-p (event)
  "Return true when EVENT should submit the compose pane.
Drei may report the Enter key as either Return or Newline depending on the
backend.  Control-modified newline remains an editor gesture for inserting a
line break."
  (let ((modifiers (clim:event-modifier-state event)))
    (and (zerop (logand modifiers clim:+control-key+))
         (zerop (logand modifiers clim:+meta-key+))
         (let ((key-name (clim:keyboard-event-key-name event))
               (key-character (clim:keyboard-event-character event)))
           (or (eql key-character #\Return)
               (eql key-character #\Newline)
               (member key-name '(:return :newline :linefeed)
                       :test #'eq))))))

(defun chat-compose-modifier-key-name-p (key-name)
  "Return true when KEY-NAME names a modifier key by itself."
  (member key-name
          '(:shift :control :ctrl :meta :alt :super :hyper
            :lshift :lctrl :lmeta :lalt :lsuper :lhyper
            :rshift :rctrl :rmeta :ralt :rsuper :rhyper
            :shift-left :shift-right :control-left :control-right
            :ctrl-left :ctrl-right :meta-left :meta-right
            :alt-left :alt-right :super-left :super-right
            :hyper-left :hyper-right :caps-lock :num-lock
            :scroll-lock :mode-switch :iso-level3-shift)
          :test #'eq))

(defun chat-compose-encoded-control-character (event)
  "Return EVENT's control-encoded character, or NIL.

Some CLIM backends report C-b as character code 2 with no modifier state
instead of reporting #\\b plus the Control modifier.  Drei's command tables are
bound to the latter representation, so compose input normalizes these encoded
control characters before command lookup.  Editing keys that are commonly also
represented as ASCII control characters, such as Backspace and Tab, are left as
their normal key events so Drei can handle them directly."
  (let ((char (clim:keyboard-event-character event))
        (key-name (clim:keyboard-event-key-name event)))
    (and (characterp char)
         (not (member key-name '(:backspace :delete :rubout :tab
                                 :return :newline :linefeed)
                      :test #'eq))
         (zerop (clim:event-modifier-state event))
         (let ((code (char-code char)))
           (and (<= 1 code 26)
                ;; Leave Return/Newline activation alone.  When a backend sends
                ;; plain Enter as LF/CR, submit handling has already consumed it;
                ;; when it cannot distinguish C-j from Enter, there is no safe
                ;; way to infer the user's intent here.  Likewise preserve
                ;; plain Backspace and Tab as editing keys.
                (not (member char '(#\Backspace #\Tab #\Return #\Newline)
                             :test #'eql))
                (code-char (+ (char-code #\a) code -1)))))))

(defun chat-compose-modified-key-event-p (event)
  "Return true when EVENT is a key Drei should process directly.

Drei's gadget event bridge normally turns key events into gestures before the
Drei command processor sees them.  The compose pane uses a small CLIM gesture
normalizer for backend-specific Control encodings, but modifier keys by
themselves are still ignored and Shift-only character input remains ordinary
text input."
  (or (chat-compose-encoded-control-character event)
      (let ((modifiers (clim:event-modifier-state event))
            (key-name (clim:keyboard-event-key-name event)))
        (and (not (chat-compose-modifier-key-name-p key-name))
             (not (zerop modifiers))
             (not (eql modifiers clim:+shift-key+))
             (let ((key-character (clim:keyboard-event-character event)))
               (or key-character key-name))))))

(defun chat-compose-drei-control-editing-event-p (event)
  "Return true when EVENT is a modified key Drei should handle here.

This compatibility wrapper keeps older tests and callers meaningful after the
compose pane fix was broadened from a few control editing keys to every
modified key event."
  (chat-compose-modified-key-event-p event))

(defun chat-compose-drei-gesture (pane event)
  "Return the CLIM gesture the compose Drei command processor should see.

This is a narrow backend-normalization adapter for `drei-gadget-pane'.  It does
not decide which Clawmacs command to run; it only preserves standard CLIM key
event information so Drei's command table machinery can resolve editor-table
and frame-table bindings."
  (let* ((key-name (clim:keyboard-event-key-name event))
         (key-character (clim:keyboard-event-character event))
         (modifiers (clim:event-modifier-state event))
         (encoded-control (chat-compose-encoded-control-character event))
         (replacement
           (cond
             ((chat-compose-modifier-key-name-p key-name) nil)
             (encoded-control encoded-control)
             ((and (null key-character)
                   (member key-name '(:escape :esc) :test #'eq))
              #\Esc)
             ((and (null key-character)
                   (member key-name '(:return) :test #'eq))
              #\Return)
             ((and (null key-character)
                   (member key-name '(:newline :linefeed) :test #'eq))
              #\Newline)
             ((and (null key-character)
                   (member key-name '(:backspace :delete :rubout) :test #'eq))
              #\Backspace))))
    (cond
      ((chat-compose-modifier-key-name-p key-name) nil)
      (replacement
       (make-instance 'clim:key-press-event
                      :sheet pane
                      :x 0
                      :y 0
                      :key-name nil
                      :key-character replacement
                      :modifier-state (if encoded-control
                                          clim:+control-key+
                                          modifiers)))
      ((and key-character
            (or (zerop modifiers)
                (eql modifiers clim:+shift-key+)))
       key-character)
      ((or key-character key-name)
       event)
      (t nil))))

(defun process-chat-compose-drei-event (pane event &key redisplay)
  "Process EVENT through Drei for compose PANE.
When REDISPLAY is nil, run just the command processor; this supports unit tests
without a grafted port. Live event handling uses Drei's normal handler so the
pane redraws and value callbacks propagate."
  (let ((gesture (chat-compose-drei-gesture pane event)))
    (when (and gesture (esa:proper-gesture-p gesture))
      (drei:with-bound-drei-special-variables
          (pane :prompt (format nil "~A " (esa:gesture-name gesture)))
        (if redisplay
            (drei:handle-gesture pane gesture)
            (esa:process-gesture pane gesture))))))

(defun chat-compose-application-input-active-p (&optional buffer)
  "Return true when Clawmacs modal input should receive compose keystrokes."
  (or *minibuffer-active*
      *session-tree-selector-active*
      *buffer-selector-active*
      *model-selector-active*
      *think-selector-active*
      *openai-oauth-pending*
      (and buffer (buffer-user-input-pending buffer))))

(defun chat-key-name-keyword (key-name)
  "Return the Clawmacs key keyword corresponding to CLIM KEY-NAME."
  (case key-name
    ((:left :right :up :down :home :end :page-up :page-down :tab :backspace)
     key-name)
    ((:backtab :shift-tab :iso-left-tab) :backtab)
    ((:prior) :page-up)
    ((:next) :page-down)
    ((:delete :rubout) :backspace)
    ((:return) #\Return)
    ((:newline :linefeed) #\Newline)
    (t key-name)))

(defun chat-control-key-character (char)
  "Return CHAR encoded as the control character expected by Clawmacs keymaps."
  (cond
    ((null char) nil)
    ((char-equal char #\Space) (code-char 0))
    ((alpha-char-p char)
     (code-char (1+ (- (char-code (char-downcase char))
                      (char-code #\a)))))
    ((eql char #\Return) #\Return)
    ((eql char #\Newline) #\Newline)
    (t char)))

(defun chat-compose-event-key (event)
  "Return EVENT in the normalized key syntax used by `handle-key-event'."
  (let* ((modifiers (clim:event-modifier-state event))
         (key-name (clim:keyboard-event-key-name event))
         (key-character (clim:keyboard-event-character event))
         (encoded-control (and (chat-compose-encoded-control-character event)
                               key-character))
         (shift-tab-p (and (not (zerop (logand modifiers clim:+shift-key+)))
                           (or (eql key-character #\Tab)
                               (eq key-name :tab))))
         (base (cond
                 (encoded-control encoded-control)
                 (shift-tab-p :backtab)
                 ((member key-name '(:backtab :shift-tab :iso-left-tab)
                          :test #'eq)
                  :backtab)
                 (key-character key-character)
                 (key-name (chat-key-name-keyword key-name))))
         (control-p (or encoded-control
                        (not (zerop (logand modifiers clim:+control-key+)))))
         (meta-p (not (zerop (logand modifiers clim:+meta-key+))))
         (base-key (if (and control-p (characterp base))
                       (chat-control-key-character base)
                       base)))
    (cond
      ((and meta-p base-key) (list :meta base-key))
      (base-key base-key)
      (t nil))))

(defun chat-compose-escape-event-p (event)
  "Return true when EVENT is a plain Escape key press."
  (let ((modifiers (clim:event-modifier-state event))
        (key-name (clim:keyboard-event-key-name event))
        (key-character (clim:keyboard-event-character event)))
    (and (zerop (logand modifiers clim:+control-key+))
         (zerop (logand modifiers clim:+meta-key+))
         (or (eql key-character #\Esc)
             (eq key-name :escape)))))

(defun sync-chat-frame-after-command-dispatch (frame &key focus-compose)
  "Synchronize FRAME after a command mutates buffer/UI state."
  (let ((current (current-buffer)))
    (when current
      (setf (chat-frame-buffer frame) current)))
  (request-chat-frame-menu-refresh frame)
  (request-chat-frame-redisplay frame)
  (when focus-compose
    (let ((compose (ignore-errors (clim:find-pane-named frame 'compose))))
      (when compose
        (ignore-errors
          (clim:stream-set-input-focus compose)))))
  frame)

(defun chat-compose-pane-point-offset (pane)
  "Return PANE's Drei point as a flat character offset, or NIL."
  (ignore-errors
    (drei-buffer:offset (drei:point (slot-value pane 'drei::%view)))))

(defun set-chat-compose-pane-point-offset (pane offset)
  "Set PANE's Drei point to OFFSET when Drei internals are available."
  (when offset
    (ignore-errors
      (setf (drei-buffer:offset (drei:point (slot-value pane 'drei::%view)))
            offset))))

(defun sync-chat-buffer-input-from-compose-pane (pane buffer)
  "Reflect compose PANE's text and point into BUFFER's input message."
  (when (and pane buffer)
    (let* ((value (ignore-errors (clim:gadget-value pane)))
           (offset (chat-compose-pane-point-offset pane))
           (message (buffer-input-message buffer)))
      (when (stringp value)
        (set-message-text message value)
        (when offset
          (set-message-point-from-absolute-offset message offset))))))

(defun sync-chat-compose-pane-from-buffer (pane buffer)
  "Reflect BUFFER's input editor text and point in compose PANE."
  (when (and pane buffer)
    (let* ((message (buffer-input-message buffer))
           (offset (message-point-absolute-offset message)))
      (setf (clim:gadget-value pane) (message-text message))
      (set-chat-compose-pane-point-offset pane offset))))

(defun chat-compose-buffer-owned-editing-event-p (pane event)
  "Return true when EVENT should use Clawmacs' buffer editing command.

Most editing keys are handled by Drei, but Drei reserves C-u as a universal
argument and C-w as kill-region.  Clawmacs documents those as kill-backward-line
and backward-kill-word in chat compose buffers, so route them through the
buffer keymap after synchronizing the Drei text/point."
  (let* ((frame (clim:pane-frame pane))
         (buf (and frame (chat-frame-buffer frame)))
         (key (chat-compose-event-key event))
         (command (and buf key (keymap-lookup (buffer-keymap buf) key))))
    (and (member key (list (code-char 21) (code-char 23)) :test #'eql)
         (member command '(kill-backward-line-command backward-kill-word-command)
                 :test #'eq))))

(defun dispatch-chat-compose-event-to-buffer (pane event)
  "Dispatch EVENT through Clawmacs' buffer key handler and refresh the frame."
  (let* ((frame (clim:pane-frame pane))
         (buf (chat-frame-buffer frame))
         (modal-input-p (chat-compose-application-input-active-p buf))
         (pending-input-p (and buf (buffer-user-input-pending buf)))
         (raw-key (chat-compose-event-key event))
         (key (cond
                ((and raw-key *meta-pending*)
                 (setf *meta-pending* nil)
                 (list :meta raw-key))
                (t raw-key))))
    (when key
      (unless modal-input-p
        (sync-chat-buffer-input-from-compose-pane pane buf))
      (let ((result (handle-key-event buf key)))
        (when (eq result :quit)
          (clim:frame-exit frame))
        (sync-chat-frame-after-command-dispatch frame)
        (cond
          (pending-input-p
           (sync-chat-compose-pane-from-buffer pane buf))
          ((not modal-input-p)
           (sync-chat-compose-pane-from-buffer pane (chat-frame-buffer frame))))
        t))))

(defclass clawmacs-chat-compose-pane (drei:drei-gadget-pane)
  ()
  (:metaclass esa-utils:modual-class)
  (:documentation "Drei-backed chat compose pane with Clawmacs frame commands."))

(defmethod drei-syntax:additional-command-tables append
    ((pane clawmacs-chat-compose-pane) (table drei::drei-command-table))
  "Let the compose pane resolve frame commands before Drei editor commands.

This follows the Climacs/Drei extension hook: add the frame-local Clawmacs
command table to the compose pane's effective Drei command table instead of
manually dispatching every key.  Editing keys stay in Drei because only
application-level Clawmacs keys are installed in the frame table."
  (declare (ignore table))
  (let ((frame (ignore-errors (clim:pane-frame pane))))
    (when frame
      (list (clim:frame-command-table frame)))))

(defun chat-compose-meta-prefix-event (pane event)
  "Return EVENT as a Meta-modified key press for an ESC prefix."
  (make-instance 'clim:key-press-event
                 :sheet pane
                 :x 0
                 :y 0
                 :key-name (clim:keyboard-event-key-name event)
                 :key-character (clim:keyboard-event-character event)
                 :modifier-state (logior (clim:event-modifier-state event)
                                          clim:+meta-key+)))

(defmethod clim:handle-event
    ((pane clawmacs-chat-compose-pane) (event clim:key-press-event))
  "Feed compose key events into Drei's command processor.

The only direct dispatches left here are Clawmacs' custom modal input overlays
and legacy compose editing keys that intentionally conflict with ESA/Drei's
standard meanings.  All normal application and editor commands are resolved by
Drei/ESA command tables."
  (let* ((frame (ignore-errors (clim:pane-frame pane)))
         (buf (and frame (chat-frame-buffer frame)))
         (result
           (cond
             ((and buf (chat-compose-escape-event-p event)
                   (buffer-llm-running-p buf))
              ;; Escape always means "stop the active stream" before it can
              ;; extend or consume a pending ESC-as-Meta prefix.
              (setf *meta-pending* nil)
              (dispatch-chat-compose-event-to-buffer pane event))
             ((and buf (chat-compose-application-input-active-p buf)
                   (chat-compose-escape-event-p event)
                   (not *meta-pending*))
              (setf *meta-pending* t)
              t)
             ((and buf (chat-compose-application-input-active-p buf))
              (dispatch-chat-compose-event-to-buffer pane event))
             ((and (chat-compose-escape-event-p event)
                   (not *meta-pending*))
              (setf *meta-pending* t)
              t)
             ((chat-compose-buffer-owned-editing-event-p pane event)
              (dispatch-chat-compose-event-to-buffer pane event))
             ((and (drei::currently-processing-p pane)
                   (esa:directly-processing-p pane))
              nil)
             (t
              (let* ((gesture-event (if *meta-pending*
                                        (prog1 (chat-compose-meta-prefix-event pane event)
                                          (setf *meta-pending* nil))
                                        event))
                     (gesture (chat-compose-drei-gesture pane gesture-event)))
                (when (and gesture (esa:proper-gesture-p gesture))
                  (unwind-protect
                       (progn
                         (setf (drei::currently-processing-p pane) t)
                         (drei:with-bound-drei-special-variables
                             (pane :prompt (format nil "~A "
                                                   (esa:gesture-name gesture)))
                           (drei:handle-gesture pane gesture)))
                    (setf (drei::currently-processing-p pane) nil))))))))
    (when frame
      (emit-chat-frame-e2e-snapshot frame :reason "compose-key" :pane "compose"))
    result))

(defclass clawmacs-transcript-pane (esa:esa-pane-mixin clim:application-pane)
  ()
  (:documentation "ESA window pane that displays the current chat transcript."))

(defclass clawmacs-chat-info-pane (esa:info-pane)
  ()
  (:documentation "Emacs-style status line for the Clawmacs chat frame.")
  (:default-initargs
   :height 22
   :min-height 22
   :max-height 22))

(defparameter *chat-minibuffer-line-height* 24
  "Approximate pixel height of one chat minibuffer row.")

(defparameter *chat-minibuffer-max-pixel-height* nil
  "Maximum pixel height for the expanded chat minibuffer pane.
When NIL, derive it from `*minibuffer-max-height*' and
`*chat-minibuffer-line-height*' so logical rows and reserved space agree.")

(defclass clawmacs-chat-minibuffer-pane (esa:minibuffer-pane)
  ()
  (:documentation "ESA minibuffer used for messages, command arguments, and M-x.")
  (:default-initargs
   :height 24
   :min-height 24
   :max-height 24))

(defun chat-frame-e2e-effective-frame (frame)
  "Return FRAME when it is a chat frame, otherwise the current application frame."
  (cond
    ((typep frame 'clawmacs-chat-frame) frame)
    ((typep clim:*application-frame* 'clawmacs-chat-frame)
     clim:*application-frame*)
    (t nil)))

(defun chat-frame-e2e-compose-text (frame)
  "Return semantic compose text for FRAME without inspecting pixels."
  (let* ((compose (and frame (ignore-errors (clim:find-pane-named frame 'compose))))
         (value (and compose (ignore-errors (clim:gadget-value compose))))
         (buf (and frame (chat-frame-buffer frame)))
         (fallback (and buf (message-text (buffer-input-message buf)))))
    (or (and (stringp value) value)
        fallback
        "")))

(defun minibuffer-selection-count-text ()
  "Return a compact selected/total completion count string, or NIL."
  (let ((count (length *minibuffer-filtered-items*)))
    (when (plusp count)
      (format nil "~D/~D" (1+ *minibuffer-selected-index*) count))))

(defun chat-minibuffer-display-input ()
  "Return minibuffer input with a visible point marker for display."
  (let ((point (max 0 (min *minibuffer-point* (length *minibuffer-input*)))))
    (concatenate 'string
                 (subseq *minibuffer-input* 0 point)
                 "|"
                 (subseq *minibuffer-input* point))))

(defun write-minibuffer-prompt-line (stream &key (display-cursor-p nil))
  "Write the minibuffer prompt line to STREAM."
  (format stream "~A: ~A"
          *minibuffer-prompt*
          (if display-cursor-p
              (chat-minibuffer-display-input)
              *minibuffer-input*))
  (when (and (eq *minibuffer-mode* :completion)
             *minibuffer-filtered-items*)
    (let ((item (nth *minibuffer-selected-index*
                     *minibuffer-filtered-items*))
          (count-text (minibuffer-selection-count-text)))
      (when item
        (format stream "  [~A]" (minibuffer-item-display item)))
      (when count-text
        (format stream "  (~A)" count-text)))))

(defun chat-frame-e2e-minibuffer-text ()
  "Return semantic minibuffer text for the current input state."
  (if *minibuffer-active*
      (with-output-to-string (stream)
        (write-minibuffer-prompt-line stream)
        (cond
          (*minibuffer-filtered-items*
           (dolist (row (minibuffer-visible-candidate-rows))
             (format stream "~%~A ~A"
                     (if (getf row :selected-p) ">" " ")
                     (getf row :display))))
          ((eq *minibuffer-mode* :completion)
           (format stream "~%  No matches"))))
      ""))

(defun chat-frame-e2e-info-line (frame)
  "Return the status/model line represented by FRAME."
  (let ((buf (and frame (chat-frame-buffer frame))))
    (if buf
        (multiple-value-bind (provider model)
            (handler-case (resolve-buffer-provider-and-model buf)
              (error () (values nil nil)))
          (format nil "~A  ~A  ~A  agent ~A  ~A"
                  (buffer-name buf)
                  (buffer-major-mode buf)
                  (string-downcase (symbol-name (buffer-status buf)))
                  (buffer-agent-name buf)
                  (if (and provider model)
                      (model-selector-display provider model)
                      "no model")))
        "")))

(defun chat-display-item-e2e-text (item)
  "Return semantic transcript text for one displayed ITEM."
  (if (chat-tool-activity-summary-p item)
      (chat-tool-activity-summary-text item)
      (format nil "~A>~%~A" (chat-message-label item) (message-text item))))

(defun call-buffer-presentation-function (function buffer columns)
  "Return entries from FUNCTION for BUFFER and COLUMNS."
  (when function
    (handler-case
        (or (funcall function buffer columns) nil)
      (error (condition)
        (list (list :text (format nil "[Presentation error: ~A]" condition)
                    :face :error
                    :unique-id (list :presentation-error function)))))))

(defun buffer-presentation-entries-e2e-text (entries)
  "Return semantic text for generic presentation ENTRIES."
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (entry) (getf entry :text "")) entries)))

(defun buffer-presentation-function-e2e-text (buffer function)
  "Return semantic text produced by BUFFER's presentation FUNCTION."
  (buffer-presentation-entries-e2e-text
   (call-buffer-presentation-function
    function
    buffer
    *buffer-presentation-default-columns*)))

(defun buffer-presentation-feedback-items (buffer)
  "Return transcript feedback messages to append after custom presentations."
  (remove-if-not (lambda (item)
                   (and (typep item 'message)
                        (eq (message-sender item) :system)
                        (not (buffer-system-prompt-display-message-p item))))
                 (chat-transcript-display-items buffer)))

(defun buffer-presentation-feedback-e2e-text (buffer)
  "Return semantic text for BUFFER feedback messages."
  (let ((items (and buffer (buffer-presentation-feedback-items buffer))))
    (and items
         (format nil "~{~A~^~%~%~}"
                 (mapcar #'chat-display-item-e2e-text items)))))

(defun chat-frame-e2e-transcript-text (buf)
  "Return semantic transcript text for BUF using the normal display item path."
  (let ((presentation-function (and buf (buffer-presentation-function buf))))
    (cond
      (presentation-function
       (format nil "~{~A~^~%~%~}"
               (remove-if #'blank-string-p
                          (list (buffer-presentation-function-e2e-text
                                 buf presentation-function)
                                (buffer-presentation-feedback-e2e-text buf)))))
      (t
       (let ((items (and buf (chat-transcript-display-items buf))))
         (if items
             (format nil "~{~A~^~%~%~}" (mapcar #'chat-display-item-e2e-text items))
             "No messages yet."))))))

(defun chat-frame-e2e-input-presentation-text (buf &optional input-text)
  "Return semantic text for BUF's input presentation overlay, if any."
  (let ((input-functions (and buf (buffer-input-presentation-functions buf)))
        (*buffer-input-presentation-text* input-text))
    (and input-functions
         (format nil "~{~A~^~%~}"
                 (mapcar (lambda (function)
                           (buffer-presentation-function-e2e-text buf function))
                         input-functions)))))

(defun chat-frame-e2e-screen-text (frame)
  "Return a semantic screen-text snapshot for FRAME."
  (let* ((buf (and frame (chat-frame-buffer frame)))
         (transcript (chat-frame-e2e-transcript-text buf))
         (input-panel (chat-frame-e2e-input-presentation-text
                       buf
                       (chat-frame-e2e-compose-text frame)))
         (approval (and buf (buffer-approval-pending buf)
                        (chat-approval-display-string
                         (buffer-approval-pending buf))))
         (info (chat-frame-e2e-info-line frame))
         (minibuffer (chat-frame-e2e-minibuffer-text))
         (parts (remove-if #'blank-string-p
                           (list transcript
                                 input-panel
                                 approval
                                 info
                                 minibuffer))))
    (format nil "~{~A~%~}" parts)))

(defun chat-frame-e2e-snapshot (frame)
  "Return semantic GUI state for FRAME as a plist for tests and E2E logs."
  (let* ((frame (chat-frame-e2e-effective-frame frame))
         (buf (and frame (chat-frame-buffer frame))))
    (multiple-value-bind (provider model)
        (if buf
            (handler-case (resolve-buffer-provider-and-model buf)
              (error () (values nil nil)))
            (values nil nil))
      (list :buffer-name (and buf (buffer-name buf))
            :agent (and buf (buffer-agent-name buf))
            :status (and buf (string-downcase (symbol-name (buffer-status buf))))
            :major-mode (and buf (buffer-major-mode buf))
            :provider (and provider (string-downcase (symbol-name provider)))
            :model model
            :message-count (and buf (buffer-message-count buf))
            :show-tool-results (and buf (buffer-show-tool-results-p buf))
            :show-reasoning (and buf (buffer-show-reasoning-p buf))
            :show-metadata (and buf (buffer-show-metadata-p buf))
            :debug-mode *debug-mode*
            :minibuffer-active *minibuffer-active*
            :buffer-selector-active *buffer-selector-active*
            :model-selector-active *model-selector-active*
            :think-selector-active *think-selector-active*
            :session-tree-selector-active *session-tree-selector-active*
            :compose-text (chat-frame-e2e-compose-text frame)
            :minibuffer-text (chat-frame-e2e-minibuffer-text)
            :info-text (chat-frame-e2e-info-line frame)
            :screen-text (chat-frame-e2e-screen-text frame)))))

(defun emit-chat-frame-e2e-snapshot (frame &key reason pane)
  "Emit a structured semantic GUI snapshot for FRAME when E2E logging is enabled."
  (let ((frame (chat-frame-e2e-effective-frame frame)))
    (when frame
      (ignore-errors
        (apply #'file-debug-event
               "ui-snapshot"
               (append (list :reason reason :pane pane)
                       (chat-frame-e2e-snapshot frame)))))))

(defun emit-chat-pane-rendered (frame pane-name &rest payload)
  "Emit E2E pane render and snapshot events for FRAME."
  (let ((frame (chat-frame-e2e-effective-frame frame)))
    (when frame
      (ignore-errors
        (apply #'file-debug-event
               "pane-rendered"
               :pane pane-name
               payload))
      (emit-chat-frame-e2e-snapshot frame :reason "pane-rendered" :pane pane-name))))

(defun display-minibuffer-candidate-row (stream row)
  "Display one minibuffer completion ROW on STREAM."
  (let* ((item (getf row :item))
         (display (getf row :display))
         (selected-p (getf row :selected-p))
         (marker (if selected-p ">" " ")))
    (flet ((emit-row ()
             (format stream " ~A " marker)
             (if selected-p
                 (clim:with-text-face (stream :bold)
                   (format stream "~A" display))
                 (format stream "~A" display))))
      (if (and (listp item) (getf item :command))
          (clim:with-output-as-presentation
              (stream item 'minibuffer-command-candidate :single-box t)
            (emit-row))
          (emit-row)))))

(defun display-chat-minibuffer-pane (frame stream)
  "Display Clawmacs' lightweight minibuffer state in STREAM."
  (when *minibuffer-active*
    (write-char #\Space stream)
    (write-minibuffer-prompt-line stream :display-cursor-p t)
    (cond
      (*minibuffer-filtered-items*
       (dolist (row (minibuffer-visible-candidate-rows))
         (terpri stream)
         (display-minibuffer-candidate-row stream row)))
      ((eq *minibuffer-mode* :completion)
       (format stream "~%   No matches"))))
  (emit-chat-pane-rendered frame "minibuffer"
                           :active *minibuffer-active*
                           :text (chat-frame-e2e-minibuffer-text)))

(defun display-chat-info-pane (frame stream)
  "Display an Emacs-style status line for FRAME."
  (declare (ignore frame))
  (let* ((pane (and (typep stream 'esa:info-pane) stream))
         (master (and pane (ignore-errors (esa:master-pane pane))))
         (frame (or (and master (ignore-errors (clim:pane-frame master)))
                    clim:*application-frame*))
         (buf (and (typep frame 'clawmacs-chat-frame)
                   (chat-frame-buffer frame))))
    (when buf
      (multiple-value-bind (provider model)
          (handler-case (resolve-buffer-provider-and-model buf)
            (error () (values nil nil)))
        (format stream " ~A  ~A  ~A  ~A"
                (buffer-name buf)
                (buffer-major-mode buf)
                (string-downcase (symbol-name (buffer-status buf)))
                (if (and provider model)
                    (model-selector-display provider model)
                    "no model")))
      (emit-chat-pane-rendered frame "info"
                               :text (chat-frame-e2e-info-line frame)))))

(clim:define-application-frame clawmacs-chat-frame
    (esa:esa-frame-mixin clim:standard-application-frame)
  ((buffer :initarg :buffer
           :accessor chat-frame-buffer)
   (redisplay-lock :initform (bt:make-lock "clawmacs chat redisplay")
                   :reader chat-frame-redisplay-lock)
   (redisplay-pending-p :initform nil
                        :accessor chat-frame-redisplay-pending-p)
   (redisplay-handling-p :initform nil
                         :accessor chat-frame-redisplay-handling-p)
   (redisplay-repeat-p :initform nil
                       :accessor chat-frame-redisplay-repeat-p))
  (:command-table (clawmacs-chat-frame
                   :inherit-from (esa:global-esa-table
                                  esa:keyboard-macro-table)))
  (:pointer-documentation t)
  (:menu-bar t)
  (:panes
   (transcript
    (let ((pane (clim:make-pane
                 'clawmacs-transcript-pane
                 :display-function 'display-chat-transcript
                 :display-time :command-loop
                 :incremental-redisplay t
                 :end-of-page-action :allow
                 :width 900
                 :height 640
                 :command-table 'clawmacs-chat-frame)))
      (setf (esa:windows clim:*application-frame*) (list pane))
      pane))
   (info
    (clim:make-pane
     'clawmacs-chat-info-pane
     :master-pane nil
     :display-function 'display-chat-info-pane
     :width 900))
   (compose
    (clim:make-pane
     'clawmacs-chat-compose-pane
     :initial-contents ""
     :ncolumns 90
     :nlines 6
     :minibuffer nil
     :scroll-bars nil
     :border-width 0
     :activation-gestures '(:return)
     :activate-callback #'compose-pane-activated))
   (minibuffer
    (clim:make-pane 'clawmacs-chat-minibuffer-pane
                    :display-function 'display-chat-minibuffer-pane
                    :display-time :command-loop
                    :width 900)))
  (:layouts
   (default
    (clim:vertically ()
      (clim:scrolling ()
        transcript)
      compose
      info
      minibuffer)))
  (:top-level (run-clawmacs-chat-top-level)))

(defmethod initialize-instance :after ((frame clawmacs-chat-frame) &key)
  "Keep ESA frame slots safely initialized before panes are generated."
  (unless (slot-boundp frame 'esa:windows)
    (setf (esa:windows frame) nil)))

(defmethod clim:frame-standard-input ((frame clawmacs-chat-frame))
  "Use the ESA minibuffer as FRAME's standard input stream when it exists."
  (or (ignore-errors (clim:find-pane-named frame 'minibuffer))
      (call-next-method)))

(defmethod esa:buffers ((frame clawmacs-chat-frame))
  "Return the Clawmacs buffers visible to the ESA command processor."
  (remove-duplicates
   (remove nil (cons (chat-frame-buffer frame) *buffer-ring*))
   :test #'eq))

(defmethod esa:esa-current-buffer ((frame clawmacs-chat-frame))
  "Return FRAME's current Clawmacs buffer."
  (chat-frame-buffer frame))

(defmethod (setf esa:esa-current-buffer) ((new-buffer buffer)
                                          (frame clawmacs-chat-frame))
  "Switch FRAME to NEW-BUFFER using Clawmacs buffer-ring semantics."
  (setf (chat-frame-buffer frame) new-buffer)
  (when (member new-buffer *buffer-ring* :test #'eq)
    (switch-to-buffer new-buffer))
  new-buffer)

(defmethod esa:esa-current-window ((frame clawmacs-chat-frame))
  "Return the current ESA window, falling back to FRAME before panes exist."
  (or (first (ignore-errors (esa:windows frame)))
      (ignore-errors (clim:find-pane-named frame 'transcript))
      frame))

(defmethod (setf esa:previous-command) (command (frame clawmacs-chat-frame))
  "Accept ESA's previous-command update before concrete window panes exist."
  command)

(defmethod esa:find-applicable-command-table ((frame clawmacs-chat-frame))
  "Use the frame-local command table so ESA M-x sees dynamic Clawmacs menus."
  (clim:frame-command-table frame))

(defun focus-chat-compose-pane (frame)
  "Give keyboard focus to FRAME's compose pane when it is available."
  (let ((compose (ignore-errors (clim:find-pane-named frame 'compose))))
    (when compose
      (ignore-errors
        (clim:stream-set-input-focus compose))
      compose)))

(defun run-clawmacs-chat-top-level (frame)
  "Run FRAME with ESA command processing and compose focused initially."
  (unless (eq (clim:frame-state frame) :enabled)
    (clim:enable-frame frame)
    (file-debug-event "frame-enabled"
                      :buffer-name (buffer-name (chat-frame-buffer frame))
                      :state (clim:frame-state frame)))
  (focus-chat-compose-pane frame)
  (file-debug-event "frame-ready"
                    :buffer-name (buffer-name (chat-frame-buffer frame))
                    :state (clim:frame-state frame))
  (emit-chat-frame-e2e-snapshot frame :reason "frame-ready")
  (esa:esa-top-level frame))

(defun mcclim-kill-items-vector (items)
  "Return ITEMS in the vector representation McCLIM's kill history expects."
  (if (vectorp items)
      items
      (coerce items 'vector)))

;; McCLIM's Edward kill history reinserts killed items with AREF, so the
;; list-producing kill commands need to hand it vectors.
(defmethod climi::ie-erase-word
    ((sheet clim:text-editor-pane) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore buffer event))
  (loop :with cursor := (climi::edit-cursor sheet)
        :repeat numeric-argument
        :do (loop :for item := (climi::smooth-erase-item cursor)
                  :when item
                    :collect item :into result
                  :until (or (cluffer:beginning-of-line-p cursor)
                             (cluffer:beginning-of-buffer-p cursor)
                             (char= (cluffer:item-before-cursor cursor) #\space))
                  :finally
                     (climi::edward-kill-object
                      sheet
                      (mcclim-kill-items-vector (nreverse result))
                      :front))))

(defmethod climi::ie-delete-word
    ((sheet clim:text-editor-pane) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore buffer event))
  (loop :with cursor := (climi::edit-cursor sheet)
        :repeat numeric-argument
        :do (loop :for item := (climi::smooth-delete-item cursor)
                  :when item
                    :collect item :into result
                  :until (or (cluffer:end-of-line-p cursor)
                             (cluffer:end-of-buffer-p cursor)
                             (char= (cluffer:item-after-cursor cursor) #\space))
                  :finally
                     (climi::edward-kill-object
                      sheet
                      (mcclim-kill-items-vector result)
                      :back))))

(defmethod climi::ie-kill-line
    ((sheet clim:text-editor-pane) (buffer cluffer:buffer) event numeric-argument)
  (declare (ignore buffer event))
  (handler-bind ((cluffer:end-of-buffer
                   (lambda (condition)
                     (declare (ignore condition))
                     (return-from climi::ie-kill-line))))
    (loop :with cursor := (climi::edit-cursor sheet)
          :repeat numeric-argument
          :for line := (climi::smooth-kill-line cursor)
          :do (climi::edward-kill-object
               sheet
               (mcclim-kill-items-vector line)
               :back))))

(defun display-chat-message (stream msg)
  "Display MSG as one chat-message presentation on STREAM."
  (let ((sender (chat-message-label msg))
        (text (message-text msg)))
    (clim:with-output-as-presentation
        (stream msg 'chat-message :single-box t)
      (clim:with-drawing-options (stream :ink (chat-message-ink msg))
        (format stream "~A>~%" sender)
        (write-string text stream)))
    (terpri stream)
    (terpri stream)))

(defun chat-tool-activity-summary-text (summary)
  "Return the collapsed display text for SUMMARY."
  (with-output-to-string (stream)
    (format stream "tools> ~D tool message~:P collapsed"
            (length (chat-tool-activity-summary-messages summary)))
    (let ((counts (chat-tool-activity-summary-tool-counts summary)))
      (if counts
          (dolist (entry counts)
            (format stream "~%  ~A × ~D" (car entry) (cdr entry)))
          (format stream "~%  no tool calls recorded")))
    (when (plusp (chat-tool-activity-summary-result-count summary))
      (format stream "~%  ~D tool result~:P"
              (chat-tool-activity-summary-result-count summary)))))

(defun display-chat-tool-activity-summary (stream summary)
  "Display SUMMARY as one collapsed tool-activity presentation."
  (clim:with-output-as-presentation
      (stream summary 'tool-activity-summary :single-box t)
    (clim:with-drawing-options (stream :ink (clim:make-rgb-color 0.12 0.34 0.18))
      (write-string (chat-tool-activity-summary-text summary) stream)))
  (terpri stream)
  (terpri stream))

(defun display-chat-display-item (stream item)
  "Display one transcript ITEM."
  (if (chat-tool-activity-summary-p item)
      (display-chat-tool-activity-summary stream item)
      (display-chat-message stream item)))

(defun buffer-presentation-entry-ink (entry)
  "Return the ink for one generic buffer presentation ENTRY."
  (case (getf entry :face)
    (:selector-title (clim:make-rgb-color 0.16 0.22 0.45))
    (:selector-header (clim:make-rgb-color 0.18 0.36 0.20))
    (:selector-footer (clim:make-rgb-color 0.35 0.35 0.35))
    (:selector-selected (clim:make-rgb-color 0.10 0.38 0.65))
    (:selector-entry (clim:make-rgb-color 0.20 0.20 0.20))
    (:tool-result (clim:make-rgb-color 0.12 0.34 0.18))
    (:system (clim:make-rgb-color 0.45 0.45 0.45))
    (:disabled (clim:make-rgb-color 0.45 0.45 0.45))
    (:error (clim:make-rgb-color 0.60 0.12 0.12))
    (t clim:+foreground-ink+)))

(defun display-buffer-presentation-entry (stream entry)
  "Display one generic buffer presentation ENTRY on STREAM."
  (let ((text (getf entry :text ""))
        (object (getf entry :object))
        (presentation-type (getf entry :presentation-type)))
    (flet ((emit ()
             (clim:with-drawing-options
                 (stream :ink (buffer-presentation-entry-ink entry))
               (if (eq (getf entry :face) :selector-title)
                   (clim:with-text-face (stream :bold)
                     (write-string text stream))
                   (write-string text stream)))))
      (if (and object presentation-type)
          (clim:with-output-as-presentation
              (stream object presentation-type :single-box t)
            (emit))
          (emit))))
  (terpri stream))

(defun display-buffer-presentation-entries (stream entries &key (namespace :buffer))
  "Display generic presentation ENTRIES on STREAM with incremental redisplay."
  (loop :for entry :in entries
        :for index :from 0
        :do
           (clim:updating-output
               (stream
                :unique-id (or (getf entry :unique-id)
                               (list namespace index (getf entry :text "")))
                :id-test #'equal
                :cache-value entry
                :cache-test #'equal)
             (display-buffer-presentation-entry stream entry))))

(defun stream-buffer-presentation-columns (stream)
  "Return an approximate text column count available on STREAM."
  (or (ignore-errors
        (let* ((width (clim:bounding-rectangle-width stream))
               (char-width (max 1 (or (ignore-errors
                                         (clim:text-style-width
                                          (clim:medium-text-style stream)
                                          stream))
                                       8))))
          (max 20 (floor width char-width))))
      *buffer-presentation-default-columns*))

(defun display-buffer-presentation-function
    (stream buffer function &key (namespace :buffer) columns)
  "Display BUFFER entries produced by FUNCTION and return the entry count."
  (let ((entries (call-buffer-presentation-function
                  function
                  buffer
                  (or columns *buffer-presentation-default-columns*))))
    (display-buffer-presentation-entries stream entries :namespace namespace)
    (length entries)))

(defun display-chat-transcript-feedback (stream buffer)
  "Display presentation-buffer feedback messages and return count."
  (let ((items (buffer-presentation-feedback-items buffer))
        (count 0))
    (dolist (item items count)
      (incf count)
      (clim:updating-output
          (stream
           :unique-id (chat-display-item-output-id item)
           :id-test #'equal
           :cache-value (chat-display-item-cache-value item)
           :cache-test #'equal)
        (display-chat-display-item stream item)))))

(defun display-chat-transcript (frame stream)
  "Display FRAME's transcript on STREAM."
  (let* ((buf (chat-frame-buffer frame))
         (columns (stream-buffer-presentation-columns stream))
         (presentation-function (and buf (buffer-presentation-function buf)))
         (input-functions (and buf (buffer-input-presentation-functions buf)))
         (items (and (not presentation-function)
                     (chat-transcript-display-items buf)))
         (item-count 0))
    (cond
      (presentation-function
       (incf item-count
             (display-buffer-presentation-function
              stream buf presentation-function
              :namespace :buffer-presentation
              :columns columns))
       (incf item-count (display-chat-transcript-feedback stream buf)))
      (items
       (dolist (item items)
         (incf item-count)
         (clim:updating-output
             (stream
              :unique-id (chat-display-item-output-id item)
              :id-test #'equal
              :cache-value (chat-display-item-cache-value item)
              :cache-test #'equal)
           (display-chat-display-item stream item))))
      (t
       (clim:with-drawing-options
           (stream :ink (clim:make-rgb-color 0.45 0.45 0.45))
         (format stream "No messages yet.~%"))))
    (when input-functions
      (let ((*buffer-input-presentation-text* (chat-frame-e2e-compose-text frame)))
        (dolist (input-function input-functions)
          (incf item-count
                (display-buffer-presentation-function
                 stream buf input-function
                 :namespace :buffer-input-presentation
                 :columns columns)))))
    (when (buffer-approval-pending buf)
      (display-chat-approval stream (buffer-approval-pending buf)))
    (emit-chat-pane-rendered frame "transcript"
                             :item-count item-count
                             :approval-pending (not (null (buffer-approval-pending buf))))))

(defun chat-transcript-pane (frame)
  "Return FRAME's transcript pane, or NIL when unavailable."
  (ignore-errors
    (clim:find-pane-named frame 'transcript)))

(defun chat-transcript-bottom-scroll-y-from-heights (content-height viewport-height)
  "Return the Y displacement that places CONTENT-HEIGHT's bottom in view."
  (max 0 (- (or content-height 0)
            (or viewport-height 0))))

(defun chat-transcript-output-height (pane)
  "Return PANE's recorded output height, or NIL when unavailable."
  (let ((history (ignore-errors (clim:stream-output-history pane))))
    (and history
         (ignore-errors
           (clim:bounding-rectangle-height history)))))

(defun chat-transcript-viewport-height (pane)
  "Return PANE's visible viewport height, or NIL when unavailable."
  (let ((viewport (or (ignore-errors (clim:pane-viewport pane)) pane)))
    (and viewport
         (ignore-errors
           (clim:bounding-rectangle-height viewport)))))

(defun chat-transcript-bottom-scroll-y (pane)
  "Return the Y displacement for keeping PANE scrolled to transcript tail."
  (let ((content-height (chat-transcript-output-height pane))
        (viewport-height (chat-transcript-viewport-height pane)))
    (and content-height
         viewport-height
         (chat-transcript-bottom-scroll-y-from-heights content-height viewport-height))))

(defun chat-transcript-scroll-to-bottom (pane)
  "Scroll transcript PANE to its bottom when McCLIM has viewport geometry."
  (let ((bottom-y (and pane (chat-transcript-bottom-scroll-y pane))))
    (when bottom-y
      (ignore-errors
        (clim:scroll-extent pane 0 bottom-y))
      bottom-y)))

(defun chat-frame-follow-transcript-tail (frame)
  "Scroll FRAME's transcript to the newest visible output when enabled."
  (when *chat-transcript-follow-tail*
    (chat-transcript-scroll-to-bottom (chat-transcript-pane frame))))

(defun submit-chat-compose-pane (frame compose-pane)
  "Submit COMPOSE-PANE through FRAME's chat command path."
  (configure-chat-compose-pane compose-pane)
  (let ((text (clim:gadget-value compose-pane))
        (buf (chat-frame-buffer frame)))
    (file-debug-event "compose-submitted"
                      :buffer-name (buffer-name buf)
                      :text text)
    (when (handle-chat-compose-text buf text)
      (setf (clim:gadget-value compose-pane) "")
      (request-chat-frame-redisplay frame)
      (emit-chat-frame-e2e-snapshot frame :reason "compose-submitted" :pane "compose")
      t)))

(defun compose-pane-activated (gadget)
  "Dispatch compose activation as a frame command."
  (declare (ignore gadget))
  (clim:with-application-frame (frame)
    (clim:execute-frame-command frame '(com-chat-submit-compose))))

(defun queue-chat-frame-redisplay-event (frame)
  "Queue one redisplay event for FRAME when its sheet is available."
  (let ((sheet (ignore-errors (clim:frame-top-level-sheet frame))))
    (when sheet
      (clim:queue-event
       sheet
       (make-instance 'clawmacs-chat-redisplay-event :sheet frame))
      t)))

(defun chat-frame-grafted-top-level-sheet (frame)
  "Return FRAME's grafted top-level sheet, or NIL before FRAME is running."
  (let ((sheet (ignore-errors (clim:frame-top-level-sheet frame))))
    (and sheet
         (ignore-errors (clim:sheet-grafted-p sheet))
         sheet)))

(defun queue-chat-frame-menu-refresh-event (frame)
  "Queue one menu refresh event for FRAME when its sheet is grafted."
  (let ((sheet (chat-frame-grafted-top-level-sheet frame)))
    (when sheet
      (clim:queue-event
       sheet
       (make-instance 'clawmacs-chat-menu-refresh-event :sheet frame))
      t)))

(defun request-chat-frame-menu-refresh (frame)
  "Refresh FRAME's dynamic menu bar outside active menu callbacks.

Unstarted frames have no grafted top-level sheet, so tests and pre-run frame
setup refresh synchronously. Running frames use the event queue so a menu command
does not replace McCLIM submenu sheets while pointer tracking is still unwinding."
  (unless (queue-chat-frame-menu-refresh-event frame)
    (refresh-chat-frame-menu-bar frame))
  frame)

(defun request-chat-frame-redisplay (frame)
  "Request one coalesced transcript redisplay for FRAME."
  (file-debug-event "redisplay-requested"
                    :buffer-name (buffer-name (chat-frame-buffer frame)))
  (let ((queue-now-p nil))
    (bt:with-lock-held ((chat-frame-redisplay-lock frame))
      (cond
        ((chat-frame-redisplay-handling-p frame)
         (setf (chat-frame-redisplay-repeat-p frame) t))
        ((not (chat-frame-redisplay-pending-p frame))
         (setf (chat-frame-redisplay-pending-p frame) t
               queue-now-p t))))
    (when queue-now-p
      (queue-chat-frame-redisplay-event frame))))

(defun chat-minibuffer-desired-row-count ()
  "Return the number of rows the chat minibuffer pane should reserve."
  (cond
    ((not *minibuffer-active*) 1)
    ((eq *minibuffer-mode* :completion)
     (+ 1 (max 1 (minibuffer-visible-item-count))))
    (t 1)))

(defun chat-minibuffer-max-pixel-height ()
  "Return the maximum expanded minibuffer pane height in pixels."
  (or *chat-minibuffer-max-pixel-height*
      (* *chat-minibuffer-line-height* *minibuffer-max-height*)))

(defun update-chat-minibuffer-space-requirements (frame)
  "Resize FRAME's minibuffer pane for active completion rows."
  (let* ((pane (ignore-errors (clim:find-pane-named frame 'minibuffer)))
         (height (min (chat-minibuffer-max-pixel-height)
                      (* *chat-minibuffer-line-height*
                         (chat-minibuffer-desired-row-count)))))
    (when pane
      (ignore-errors
        (clim:change-space-requirements pane
                                        :height height
                                        :min-height height
                                        :max-height height)))))

(defun handle-chat-frame-redisplay (frame)
  "Run the canonical redisplay step for FRAME's transcript pane."
  (let ((repeat-p nil))
    (bt:with-lock-held ((chat-frame-redisplay-lock frame))
      (setf (chat-frame-redisplay-pending-p frame) nil
            (chat-frame-redisplay-handling-p frame) t
            (chat-frame-redisplay-repeat-p frame) nil))
    (unwind-protect
         (let ((buf (chat-frame-buffer frame)))
           (when (and buf (buffer-pending-stream buf))
             (let ((*suppress-chat-redisplay-requests* t))
               (update-streaming-response buf)))
           (update-chat-minibuffer-space-requirements frame)
           (clim:redisplay-frame-pane frame 'transcript :force-p nil)
           (ignore-errors
             (clim:redisplay-frame-pane frame 'info :force-p t))
           (ignore-errors
             (clim:redisplay-frame-pane frame 'minibuffer :force-p t))
           (chat-frame-follow-transcript-tail frame))
      (bt:with-lock-held ((chat-frame-redisplay-lock frame))
        (setf repeat-p (chat-frame-redisplay-repeat-p frame)
              (chat-frame-redisplay-handling-p frame) nil
              (chat-frame-redisplay-repeat-p frame) nil)))
    (file-debug-event "redisplay-handled"
                      :buffer-name (buffer-name (chat-frame-buffer frame))
                      :repeat repeat-p)
    (emit-chat-frame-e2e-snapshot frame :reason "redisplay-handled")
    (when repeat-p
      (request-chat-frame-redisplay frame))))

(defmethod clim:handle-event
    ((frame clawmacs-chat-frame) (event clawmacs-chat-redisplay-event))
  (declare (ignore event))
  (handle-chat-frame-redisplay frame))

(defmethod clim:handle-event
    ((frame clawmacs-chat-frame) (event clawmacs-chat-menu-refresh-event))
  (declare (ignore event))
  (refresh-chat-frame-menu-bar frame))

(defun respond-to-chat-approval (frame response)
  "Apply RESPONSE to FRAME's pending approval and redisplay."
  (let ((buf (chat-frame-buffer frame)))
    (when (buffer-approval-pending buf)
      (handle-approval-response buf response)
      (request-chat-frame-redisplay frame)
      t)))

(defun run-chat-frame-buffer-command (frame command)
  "Run COMMAND on FRAME's buffer and refresh frame UI state."
  (funcall command (chat-frame-buffer frame))
  (request-chat-frame-menu-refresh frame)
  (request-chat-frame-redisplay frame))

(defun select-chat-effort-for-buffer (buffer level)
  "Set BUFFER's reasoning effort LEVEL and report the new selection."
  (let ((entries (available-think-levels-for-selector buffer)))
    (unless entries
      (error "Think levels are not available for the active model."))
    (let ((entry (find level
                       entries
                       :key (lambda (item)
                              (or (getf item :level) ""))
                       :test #'string=)))
      (unless entry
        (error "Unknown think level selection: ~S" level))
      (apply-buffer-think-level-selection buffer entry)
      entry)))

(defun toggle-chat-skill-for-buffer (buffer skill-key)
  "Toggle SKILL-KEY and record feedback in BUFFER."
  (let ((skill (find-skill-by-path skill-key :include-disabled t)))
    (unless skill
      (error "Unknown skill: ~A" skill-key))
    (let ((enabled-p (not (skill-enabled-p skill))))
      (set-skill-enabled skill enabled-p)
      (buffer-insert-system-message
       buffer
       (format nil "[Skill ~A ~A]"
               (skill-name skill)
               (if enabled-p "enabled" "disabled")))
      enabled-p)))

(defun toggle-chat-package-for-buffer (buffer package-name)
  "Toggle explicit BUFFER enablement for PACKAGE-NAME."
  (unless buffer
    (error "Package toolbar toggles require a chat buffer."))
  (let* ((name (manifest-package-name package-name))
         (agent (buffer-agent-name buffer))
         (definition (find-installed-package name :buffer buffer))
         (previous-scope (package-enablement-scope
                          name
                          :buffer buffer
                          :agent-name agent))
         (had-context-p (buffer-has-conversation-context-p buffer))
         (enabled-p (not (buffer-package-name-enabled-p buffer name))))
    (unless definition
      (error "Unknown package: ~A" package-name))
    (set-buffer-package-name-enabled buffer name enabled-p)
    (if enabled-p
        (maybe-insert-enabled-package-context
         buffer definition previous-scope :buffer had-context-p)
        (remove-package-context-messages buffer name))
    (sync-buffer-system-prompt-display buffer)
    (load-active-packages :buffer buffer)
    (maybe-run-hook-with-args
     '*package-enablement-changed-hook*
     name
     (package-enablement-scope name :buffer buffer :agent-name agent)
     buffer
     agent)
    (buffer-insert-system-message
     buffer
     (format nil "[Package ~A ~A for this buffer]"
             name
             (if enabled-p "enabled" "disabled")))
    enabled-p))

(defun chat-recurse-readable-form (form)
  "Return FORM printed safely for a child Lisp process command line."
  (let ((*print-readably* nil)
        (*print-escape* t)
        (*print-array* nil)
        (*print-pretty* nil))
    (prin1-to-string form)))

(defun chat-recurse-source-root ()
  "Return the Clawmacs source root used for recurse launches."
  (uiop:ensure-directory-pathname
   (or (ignore-errors (asdf:system-source-directory :clawmacs))
       (truename "."))))

(defun chat-recurse-quicklisp-setup ()
  "Return the Quicklisp setup file used for recurse launches."
  (or (let ((env (uiop:getenv "CLAWMACS_QUICKLISP_SETUP")))
        (and env
             (plusp (length env))
             (probe-file env)))
      (probe-file
       (merge-pathnames #P"quicklisp/setup.lisp"
                        (user-homedir-pathname)))
      (error 'simple-error
             :format-control
             "Cannot recurse without Quicklisp setup. Set CLAWMACS_QUICKLISP_SETUP or install ~/quicklisp/setup.lisp."
             :format-arguments nil)))

(defun chat-recurse-session-name (buffer)
  "Return a unique session name for BUFFER's recurse child."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (format nil "~A recurse ~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D-~D"
            (buffer-name buffer)
            year
            month
            date
            hour
            minute
            second
            (get-internal-real-time))))

(defun chat-recurse-window-title (buffer)
  "Return the window title for BUFFER's recurse child."
  (format nil "Clawmacs Recurse - ~A" (buffer-name buffer)))

(defun chat-recurse-startup-form
    (buffer &key session-name window-title working-directory)
  "Return the child-process startup form for BUFFER's recurse launch."
  (let ((session-name (or session-name (chat-recurse-session-name buffer)))
        (window-title (or window-title (chat-recurse-window-title buffer)))
        (working-directory
          (normalize-buffer-working-directory
           (or working-directory
               (buffer-working-directory buffer)))))
    (format nil
            "(clawmacs:clawmacs-main :session-name ~A :agent-name ~A :window-title ~A :working-directory ~A)"
            (chat-recurse-readable-form session-name)
            (chat-recurse-readable-form (buffer-agent-name buffer))
            (chat-recurse-readable-form window-title)
            (chat-recurse-readable-form (namestring working-directory)))))

(defun chat-recurse-launch-spec
    (buffer &key repo-root quicklisp-setup session-name window-title
                  working-directory)
  "Return a launch plist for a fresh child Clawmacs process for BUFFER."
  (let* ((source-root
           (uiop:ensure-directory-pathname
            (or repo-root (chat-recurse-source-root))))
         (quicklisp-setup
           (or quicklisp-setup (chat-recurse-quicklisp-setup)))
         (session-name (or session-name (chat-recurse-session-name buffer)))
         (window-title (or window-title (chat-recurse-window-title buffer)))
         (working-directory
           (normalize-buffer-working-directory
            (or working-directory
                (buffer-working-directory buffer))))
         (build-cache-script
           (merge-pathnames #P"scripts/build-cache.lisp" source-root))
         (argv
           (list "sbcl"
                 "--noinform"
                 "--eval" "(require :asdf)"
                 "--load" (namestring build-cache-script)
                 "--load" (namestring quicklisp-setup)
                 "--eval"
                 (format nil
                         "(clawmacs/build-cache:maybe-clean-build-cache :environment-variable ~S)"
                         "CLAWMACS_RUN_CLEAN_BUILD")
                 "--eval"
                 (format nil
                         "(push (truename ~S) asdf:*central-registry*)"
                         (namestring source-root))
                 "--eval" "(ql:quickload :clawmacs)"
                 "--eval" "(asdf:load-system :clawmacs :force t)"
                 "--eval"
                 (chat-recurse-startup-form
                  buffer
                  :session-name session-name
                  :window-title window-title
                  :working-directory working-directory)
                 "--eval" "(uiop:quit)")))
    (list :directory source-root
          :argv argv
          :session-name session-name
          :window-title window-title
          :working-directory working-directory)))

(defun launch-chat-recurse (buffer)
  "Spawn a fresh child Clawmacs process for BUFFER and return its launch plist."
  (let* ((spec (chat-recurse-launch-spec buffer))
         (process
           (uiop:launch-program
            (getf spec :argv)
            :directory (getf spec :directory)
            :input nil
            :output :interactive
            :error-output :interactive
            :ignore-error-status t)))
    (setf (getf spec :process) process)
    spec))

(define-clawmacs-chat-frame-command
    (com-show-message-metadata :name nil)
    ((msg 'chat-message))
  (open-message-help-window msg))

(define-clawmacs-chat-frame-command
    (com-chat-submit-compose :name nil)
    ()
  (clim:with-application-frame (frame)
    (submit-chat-compose-pane
     frame
     (clim:find-pane-named frame 'compose))))

(define-clawmacs-chat-frame-command
    (com-chat-select-minibuffer-command-candidate :name nil)
    ((item 'minibuffer-command-candidate))
  (clim:with-application-frame (frame)
    (let ((command (and (listp item) (getf item :command))))
      (when command
        (minibuffer-deactivate)
        (invoke-command (chat-frame-buffer frame) command)
        (sync-chat-frame-after-command-dispatch frame :focus-compose t)))))

(dolist (gesture '((#\Return) (#\Newline)))
  (clim:add-keystroke-to-command-table
   'clawmacs-chat-frame
   gesture
   :command '(com-chat-submit-compose)
   :errorp nil))

(defparameter *chat-compose-drei-owned-commands*
  '(send-message
    self-insert-command
    insert-newline-command
    insert-tab-command
    beginning-of-line-command
    end-of-line-command
    forward-char-command
    backward-char-command
    forward-word-command
    backward-word-command
    beginning-of-buffer-command
    end-of-buffer-command
    previous-line-command
    next-line-command
    set-mark-command
    exchange-point-and-mark-command
    mark-whole-buffer-command
    kill-region-command
    copy-region-command
    kill-line-command
    kill-backward-line-command
    kill-word-command
    backward-kill-word-command
    yank-command
    yank-pop-command
    yank-previous-command-first-arg-command
    yank-previous-command-last-arg-command
    delete-char-backward-command
    delete-char-forward-command
    search-forward-command
    search-backward-command)
  "Clawmacs keymap commands that the Drei compose editor should handle itself.

The compose pane inherits the chat frame command table before Drei's editor
command tables, so binding these in the frame table would steal ordinary text
editing gestures such as C-a, C-k, and M-f from Drei.  Application-level
bindings such as M-x, C-x b, C-h b, C-c t, and scrolling commands are installed
in the frame table and remain available while focus is in compose.")

(defun chat-compose-application-command-p (command)
  "Return true when COMMAND should be dispatched by the chat frame in compose."
  (and command
       (not (member command *chat-compose-drei-owned-commands* :test #'eq))))

(defun chat-control-character-gesture (char)
  "Return the CLIM gesture for Clawmacs control character CHAR."
  (let ((code (char-code char)))
    (cond
      ((= code 0) '(#\Space :control))
      ((or (eql char #\Return) (eql char #\Newline)) (list char :control))
      ((and (>= code 1) (<= code 26))
       (list (code-char (+ (char-code #\a) code -1)) :control))
      ((= code 27) '(#\Esc))
      ((= code 127) '(#\Rubout))
      (t (list char)))))

(defun chat-key-component-gesture (component &optional modifier)
  "Return a one-gesture CLIM key spec for one Clawmacs key COMPONENT."
  (let ((gesture
          (cond
            ((characterp component)
             (cond
               ((< (char-code component) 32)
                (chat-control-character-gesture component))
               ((upper-case-p component)
                (list component :shift))
               (t
                (list component))))
            ((keywordp component)
             (list component))
            (t
             (list component)))))
    (if modifier
        (append gesture (list modifier))
        gesture)))

(defun chat-keyspec-gestures (key)
  "Translate one Clawmacs keymap KEY into ESA/CLIM gesture sequence syntax."
  (cond
    ((characterp key)
     (list (chat-key-component-gesture key)))
    ((keywordp key)
     (list (chat-key-component-gesture key)))
    ((and (consp key) (eq (first key) :meta))
     (list (chat-key-component-gesture (second key) :meta)))
    ((and (consp key) (eq (first key) :alt))
     (list (chat-key-component-gesture (second key) :meta)))
    ((and (consp key) (eq (first key) :ctrl))
     (list (chat-key-component-gesture (second key) :control)))
    ((and (consp key) (eq (first key) :ctrl-x))
     (list (chat-key-component-gesture #\x :control)
           (chat-key-component-gesture (second key))))
    ((and (consp key) (eq (first key) :ctrl-c))
     (list (chat-key-component-gesture #\c :control)
           (chat-key-component-gesture (second key))))
    ((and (consp key) (eq (first key) :ctrl-h))
     (list (chat-key-component-gesture #\h :control)
           (chat-key-component-gesture (second key))))
    (t nil)))

(defun chat-keyspec-uppercase-aliases (gestures)
  "Return backend-compatible aliases for uppercase character gestures.

McCLIM backends differ in how a typed uppercase letter is represented in key
events: uppercase character plus :SHIFT, uppercase character without :SHIFT,
or lowercase character plus :SHIFT.  Install all three spellings for
application command keys so prefix bindings such as C-c V work consistently
from the Drei compose pane."
  (labels ((uppercase-shift-gesture-p (gesture)
             (and (consp gesture)
                  (characterp (first gesture))
                  (upper-case-p (first gesture))
                  (member :shift (rest gesture))))
           (without-shift (gesture)
             (if (uppercase-shift-gesture-p gesture)
                 (remove :shift gesture)
                 gesture))
           (lowercase-with-shift (gesture)
             (if (uppercase-shift-gesture-p gesture)
                 (cons (char-downcase (first gesture))
                       (rest gesture))
                 gesture))
           (maybe-alias (transform)
             (let ((alias (mapcar transform gestures)))
               (unless (equal alias gestures)
                 alias))))
    (remove nil
            (list (maybe-alias #'without-shift)
                  (maybe-alias #'lowercase-with-shift))
            :test #'equal)))

(defun install-chat-frame-keybindings (&optional (keymap *default-keymap*))
  "Install application-level Clawmacs key bindings into the chat command table.

Drei gadgets already inherit the frame command table.  Installing only
application-level bindings there lets global Clawmacs/ESA keys work from the
compose pane while leaving text editing keys to Drei's editor tables."
  (when keymap
    (maphash
     (lambda (key command)
       (when (chat-compose-application-command-p command)
         (let ((gestures (chat-keyspec-gestures key)))
           (when gestures
             (dolist (key-gestures
                      (cons gestures
                            (chat-keyspec-uppercase-aliases gestures)))
               (handler-case
                   ;; CLIM executes command table entries as command forms;
                   ;; quote the Clawmacs key object so list keys like
                   ;; (:meta #\x) are passed as data rather than called.
                   (esa:set-key `(com-chat-dispatch-key ',key)
                                'clawmacs-chat-frame
                                key-gestures)
                 (clim:command-already-present () nil)))))))
     (keymap-bindings keymap)))
  (dolist (gesture '((#\Return) (#\Newline)))
    (clim:add-keystroke-to-command-table
     'clawmacs-chat-frame
     gesture
     :command '(com-chat-submit-compose)
     :errorp nil)))

(define-clawmacs-chat-frame-command
    (com-chat-dispatch-key :name nil)
    ((key 't))
  (clim:with-application-frame (frame)
    (let ((buf (chat-frame-buffer frame)))
      (unless (member buf *buffer-ring* :test #'eq)
        (add-buffer-to-ring buf))
      (unless (eq (current-buffer) buf)
        (switch-to-buffer buf))
      (let ((result (handle-key-event buf key)))
        (when (eq result :quit)
          (clim:frame-exit frame))
        (let ((current (current-buffer)))
          (when current
            (setf (chat-frame-buffer frame) current)))
        (request-chat-frame-menu-refresh frame)
        (request-chat-frame-redisplay frame)))))

(install-chat-frame-keybindings)

(define-clawmacs-chat-frame-command
    (com-chat-stop-response :name "Stop Response")
    ()
  (clim:with-application-frame (frame)
    (when (stop-streaming-response (chat-frame-buffer frame))
      (request-chat-frame-redisplay frame))))

(define-clawmacs-chat-frame-command
    (com-chat-toggle-tool-results :name "Toggle Tool Results")
    ()
  (clim:with-application-frame (frame)
    (run-chat-frame-buffer-command frame #'toggle-tool-results-command)))

(define-clawmacs-chat-frame-command
    (com-chat-toggle-reasoning-output :name "Toggle Reasoning Output")
    ()
  (clim:with-application-frame (frame)
    (run-chat-frame-buffer-command frame #'toggle-reasoning-output-command)))

(define-clawmacs-chat-frame-command
    (com-chat-toggle-metadata-output :name "Toggle Metadata Output")
    ()
  (clim:with-application-frame (frame)
    (run-chat-frame-buffer-command frame #'toggle-metadata-output-command)))

(define-clawmacs-chat-frame-command
    (com-chat-toggle-debug-mode :name "Toggle Debug Mode")
    ()
  (clim:with-application-frame (frame)
    (run-chat-frame-buffer-command frame #'toggle-debug-mode-command)))

(define-clawmacs-chat-frame-command
    (com-chat-toggle-skill :name nil)
    ((skill-key 'string))
  (clim:with-application-frame (frame)
    (toggle-chat-skill-for-buffer (chat-frame-buffer frame) skill-key)
    (request-chat-frame-menu-refresh frame)
    (request-chat-frame-redisplay frame)))

(define-clawmacs-chat-frame-command
    (com-chat-toggle-package :name nil)
    ((package-name 'string))
  (clim:with-application-frame (frame)
    (toggle-chat-package-for-buffer (chat-frame-buffer frame) package-name)
    (request-chat-frame-menu-refresh frame)
    (request-chat-frame-redisplay frame)))

(define-clawmacs-chat-frame-command
    (com-chat-select-effort :name nil)
    ((level 'string))
  (clim:with-application-frame (frame)
    (select-chat-effort-for-buffer (chat-frame-buffer frame) level)
    (request-chat-frame-menu-refresh frame)
    (request-chat-frame-redisplay frame)))

(define-clawmacs-chat-frame-command
    (com-chat-recurse :name "Recurse")
    ()
  (clim:with-application-frame (frame)
    (let* ((buffer (chat-frame-buffer frame))
           (spec (launch-chat-recurse buffer)))
      (buffer-insert-system-message
       buffer
       (format nil
               "[Opened recurse frame ~A for session ~A in ~A]"
               (getf spec :window-title)
               (getf spec :session-name)
               (namestring (getf spec :working-directory))))
      (request-chat-frame-redisplay frame))))

(define-clawmacs-chat-frame-command
    (com-approve-tool :name "Approve Tool")
    ((approval 'tool-approval))
  (declare (ignore approval))
  (clim:with-application-frame (frame)
    (respond-to-chat-approval frame :approve)))

(define-clawmacs-chat-frame-command
    (com-toggle-package-dashboard-entry :name nil)
    ((ref 'package-dashboard-entry-ref))
  (let ((dashboard (getf ref :dashboard-buffer))
        (origin (getf ref :origin-buffer))
        (entry (getf ref :entry)))
    (when (and dashboard entry)
      (package-dashboard-toggle-entry dashboard entry :origin-buffer origin))))

(clim:define-presentation-to-command-translator describe-chat-message
    (chat-message com-show-message-metadata clawmacs-chat-frame
     :gesture :describe
     :documentation "View message metadata"
     :menu nil)
    (object)
  (list object))

(clim:define-presentation-to-command-translator select-minibuffer-command-candidate
    (minibuffer-command-candidate com-chat-select-minibuffer-command-candidate
     clawmacs-chat-frame
     :gesture :select
     :documentation "Run command"
     :menu nil)
    (object)
  (list object))

(clim:define-presentation-to-command-translator select-package-dashboard-entry
    (package-dashboard-entry-ref com-toggle-package-dashboard-entry
     clawmacs-chat-frame
     :gesture :select
     :documentation "Toggle package scope"
     :menu t)
    (object)
  (list object))

(clim:define-presentation-to-command-translator approve-tool-approval
    (tool-approval com-approve-tool clawmacs-chat-frame
     :gesture :select
     :documentation "Approve tool call"
     :menu t)
    (object)
  (list object))

(defmethod clim:run-frame-top-level :around ((frame clawmacs-chat-frame) &key)
  (refresh-chat-frame-menu-bar frame)
  (let ((transcript (clim:find-pane-named frame 'transcript)))
    (setf (esa:windows frame) (and transcript (list transcript))))
  (configure-chat-compose-pane (clim:find-pane-named frame 'compose))
  (let ((hook (lambda (buf reason)
                (when (and (not *suppress-chat-redisplay-requests*)
                           (eq buf (chat-frame-buffer frame)))
                  (when (member reason '(:routing :system-prompt)
                                :test #'eq)
                    (request-chat-frame-menu-refresh frame))
                  (request-chat-frame-redisplay frame)))))
    (add-hook '*after-buffer-display-change-hook* hook :append t)
    (unwind-protect
         (call-next-method)
      (remove-hook '*after-buffer-display-change-hook* hook))))

(defun run-clawmacs-chat-frame (buffer &key window-title)
  "Run the fresh McCLIM chat frame for BUFFER."
  (let ((frame (clim:make-application-frame
                'clawmacs-chat-frame
                :buffer buffer
                :pretty-name (or window-title "Clawmacs"))))
    (file-debug-event "frame-created"
                      :buffer-name (buffer-name buffer)
                      :window-title (or window-title "Clawmacs"))
    (clim:run-frame-top-level frame)))
