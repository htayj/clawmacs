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

(clim:define-presentation-method clim:presentation-typep
    (object (type tool-approval))
  (and (listp object)
       (not (null (assoc :tool-name object)))))

(defclass clawmacs-chat-redisplay-event (clim:window-event)
  ())

(defclass clawmacs-chat-menu-refresh-event (clim:window-event)
  ())

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

(defun chat-menu-context-buffer (context)
  "Return the chat buffer represented by CONTEXT."
  (cond
    ((null context) nil)
    ((typep context 'buffer) context)
    (t (chat-frame-buffer context))))

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
            :menu (chat-package-menu-items buffer))))
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
              :documentation "Enable or disable packages for this chat.")))))

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

(defun chat-transcript-messages (buf)
  "Return finalized, non-ephemeral messages for BUF."
  (loop :for msg := (buffer-first-message buf) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buf))))
        :unless (buffer-ephemeral-display-message-p msg)
          :collect msg))

(defun chat-message-output-id (msg)
  "Return a stable incremental-redisplay id for MSG."
  (or (message-entry-id msg) msg))

(defun chat-message-cache-value (msg)
  "Return a cache value covering visible MSG state."
  (list (message-sender msg)
        (message-timestamp msg)
        (message-text msg)
        (message-metadata msg)
        (message-entry-id msg)
        (message-parent-entry-id msg)))

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

(clim:define-application-frame clawmacs-chat-frame ()
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
  (:pointer-documentation t)
  (:menu-bar t)
  (:panes
   (transcript :application
               :display-function 'display-chat-transcript
               :display-time :command-loop
               :incremental-redisplay t
               :width 900
               :height 640)
   (compose :text-editor
            :value ""
            :ncolumns 90
            :nlines 6
            :activation-gestures '(:return)
            :activate-callback #'compose-pane-activated))
  (:layouts
   (default
    (clim:vertically ()
      transcript
      compose))))

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

(defun display-chat-transcript (frame stream)
  "Display FRAME's transcript on STREAM."
  (let* ((buf (chat-frame-buffer frame))
         (messages (chat-transcript-messages buf)))
    (if messages
        (dolist (msg messages)
          (clim:updating-output
              (stream
               :unique-id (chat-message-output-id msg)
               :id-test #'equal
               :cache-value (chat-message-cache-value msg)
               :cache-test #'equal)
            (display-chat-message stream msg)))
        (clim:with-drawing-options
            (stream :ink (clim:make-rgb-color 0.45 0.45 0.45))
          (format stream "No messages yet.~%")))
    (when (buffer-approval-pending buf)
      (display-chat-approval stream (buffer-approval-pending buf)))))

(defun submit-chat-compose-pane (frame compose-pane)
  "Submit COMPOSE-PANE through FRAME's chat command path."
  (let ((text (clim:gadget-value compose-pane))
        (buf (chat-frame-buffer frame)))
    (when (handle-chat-compose-text buf text)
      (setf (clim:gadget-value compose-pane) "")
      (request-chat-frame-redisplay frame)
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
             (update-streaming-response buf))
           (clim:redisplay-frame-pane frame 'transcript :force-p nil))
      (bt:with-lock-held ((chat-frame-redisplay-lock frame))
        (setf repeat-p (chat-frame-redisplay-repeat-p frame)
              (chat-frame-redisplay-handling-p frame) nil
              (chat-frame-redisplay-repeat-p frame) nil)))
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
    (com-approve-tool :name "Approve Tool")
    ((approval 'tool-approval))
  (declare (ignore approval))
  (clim:with-application-frame (frame)
    (respond-to-chat-approval frame :approve)))

(clim:define-presentation-to-command-translator describe-chat-message
    (chat-message com-show-message-metadata clawmacs-chat-frame
     :gesture :describe
     :documentation "View message metadata"
     :menu nil)
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
  (let ((hook (lambda (buf reason)
                (declare (ignore reason))
                (when (eq buf (chat-frame-buffer frame))
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
    (clim:run-frame-top-level frame)))
