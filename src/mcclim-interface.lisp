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

(defun compose-pane-activated (gadget)
  "Submit the compose pane contents."
  (clim:with-application-frame (frame)
    (let ((text (clim:gadget-value gadget))
          (buf (chat-frame-buffer frame)))
      (when (handle-chat-compose-text buf text)
        (setf (clim:gadget-value gadget) "")
        (request-chat-frame-redisplay frame)))))

(defun queue-chat-frame-redisplay-event (frame)
  "Queue one redisplay event for FRAME when its sheet is available."
  (let ((sheet (ignore-errors (clim:frame-top-level-sheet frame))))
    (when sheet
      (clim:queue-event
       sheet
       (make-instance 'clawmacs-chat-redisplay-event :sheet frame))
      t)))

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

(defun respond-to-chat-approval (frame response)
  "Apply RESPONSE to FRAME's pending approval and redisplay."
  (let ((buf (chat-frame-buffer frame)))
    (when (buffer-approval-pending buf)
      (handle-approval-response buf response)
      (request-chat-frame-redisplay frame)
      t)))

(define-clawmacs-chat-frame-command
    (com-show-message-metadata :name nil)
    ((msg 'chat-message))
  (open-message-help-window msg))

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
