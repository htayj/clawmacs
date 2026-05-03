(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Fresh McCLIM Chat Interface
;;; --------------------------------------------------------------------------

(clim:define-presentation-type chat-message ()
  :inherit-from 'message)

(clim:define-presentation-method clim:presentation-typep
    (object (type chat-message))
  (typep object 'message))

(defclass clawmacs-chat-redisplay-event (clim:window-event)
  ())

(defun chat-message-kind (msg)
  "Return MSG's high-level display kind."
  (case (message-sender msg)
    (:user :user)
    (:system :system)
    (t :agent)))

(defun chat-message-ink (msg)
  "Return the CLIM ink used for MSG."
  (ecase (chat-message-kind msg)
    (:user (clim:make-rgb-color 0.10 0.25 0.55))
    (:agent (clim:make-rgb-color 0.10 0.10 0.10))
    (:system (clim:make-rgb-color 0.36 0.36 0.36))))

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
  (let ((sender (string-downcase (symbol-name (message-sender msg))))
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
  (let ((messages (chat-transcript-messages (chat-frame-buffer frame))))
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
          (format stream "No messages yet.~%")))))

(defun compose-pane-activated (gadget)
  "Submit the compose pane contents."
  (clim:with-application-frame (frame)
    (let ((text (clim:gadget-value gadget)))
      (unless (blank-string-p text)
        (set-message-text (buffer-input-message (chat-frame-buffer frame)) text)
        (setf (clim:gadget-value gadget) "")
        (send-message (chat-frame-buffer frame))
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

(define-clawmacs-chat-frame-command
    (com-show-message-metadata :name nil)
    ((msg 'chat-message))
  (open-message-help-window msg))

(clim:define-presentation-to-command-translator describe-chat-message
    (chat-message com-show-message-metadata clawmacs-chat-frame
     :gesture :describe
     :documentation "View message metadata"
     :menu nil)
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
