(in-package :clawmacs)

(defvar *quaestor-package-name* "quaestor"
  "Bundled package name for structured questions and queued delivery.")

(defvar *quaestor-flushing-queue* nil
  "When non-nil, queued delivery is actively submitting a stored message.")

(defvar *quaestor-request-user-input-catch-active* nil
  "When non-nil, request_user_input may suspend the active agent turn.")

(defun quaestor-property-alist-p (value)
  "Return true when VALUE is a simple property alist."
  (and (listp value)
       (every #'consp value)))

(defun quaestor-package-active-p (&optional buffer)
  "Return true when the quaestor package is active for BUFFER."
  (package-active-p *quaestor-package-name* :buffer buffer))

(clim:define-presentation-type quaestor-option-ref ())
(clim:define-presentation-type quaestor-submit-ref ())

(clim:define-presentation-method clim:presentation-typep
    (object (type quaestor-option-ref))
  (and (listp object)
       (stringp (getf object :question-id))
       (plusp (length (getf object :question-id)))
       (stringp (getf object :label))
       (plusp (length (getf object :label)))
       (integerp (getf object :index))))

(clim:define-presentation-method clim:presentation-typep
    (object (type quaestor-submit-ref))
  (and (listp object)
       (getf object :quaestor-submit-p)))

(defun quaestor-trimmed-string (value)
  "Return VALUE trimmed of ASCII surrounding whitespace."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (or value "")))

(defun quaestor-nonblank-string-p (value)
  "Return true when VALUE contains non-whitespace text."
  (plusp (length (quaestor-trimmed-string value))))

(defun quaestor-alist-value (alist key &optional default)
  "Return KEY's value from ALIST, or DEFAULT."
  (let ((entry (assoc key alist :test #'eq)))
    (if entry
        (cdr entry)
        default)))

(defun quaestor-alist-put (alist key value)
  "Return ALIST with KEY set to VALUE."
  (acons key value
         (remove key alist :key #'car :test #'eq)))

(defun quaestor-boolean (value)
  "Return VALUE coerced to a boolean."
  (not (null value)))

(defun quaestor-normalize-option (raw)
  "Return RAW normalized as one request_user_input option alist."
  (unless (listp raw)
    (error "request_user_input options must be objects, got ~S." raw))
  (let ((label (tool-arg raw :label "label"))
        (description (tool-arg raw :description "description")))
    (unless (quaestor-nonblank-string-p label)
      (error "request_user_input option labels must be non-empty strings."))
    (unless (quaestor-nonblank-string-p description)
      (error "request_user_input option descriptions must be non-empty strings."))
    `((:label . ,(quaestor-trimmed-string label))
      (:description . ,(quaestor-trimmed-string description)))))

(defun quaestor-normalize-question (raw index)
  "Return RAW normalized as one request_user_input question alist."
  (unless (listp raw)
    (error "request_user_input questions must be objects, got ~S." raw))
  (let* ((header (tool-arg raw :header "header"))
         (id (tool-arg raw :id "id"))
         (question (tool-arg raw :question "question"))
         (options-raw (tool-arg raw :options "options"))
         (multiple-p (quaestor-boolean
                      (or (tool-arg raw :multiple "multiple")
                          (tool-arg raw :multi "multi"))))
         (allow-notes-p
           (or (null options-raw)
               (quaestor-boolean
                (or (tool-arg raw :allow-notes "allow-notes" "allow_notes")
                    (tool-arg raw :freeform "freeform")
                    (tool-arg raw :allow-freeform "allow_freeform")
                    (tool-arg raw :is-other "is_other")))))
         (options (and options-raw
                       (mapcar #'quaestor-normalize-option
                               (coerce options-raw 'list)))))
    (unless (quaestor-nonblank-string-p header)
      (error "request_user_input question ~D is missing a non-empty header." (1+ index)))
    (unless (quaestor-nonblank-string-p id)
      (error "request_user_input question ~D is missing a non-empty id." (1+ index)))
    (unless (quaestor-nonblank-string-p question)
      (error "request_user_input question ~D is missing a non-empty question." (1+ index)))
    (when (and multiple-p (null options))
      (error "request_user_input question ~A cannot be multi-choice without options."
             id))
    `((:header . ,(quaestor-trimmed-string header))
      (:id . ,(quaestor-trimmed-string id))
      (:question . ,(quaestor-trimmed-string question))
      (:options . ,options)
      (:multiple . ,multiple-p)
      (:allow-notes . ,allow-notes-p))))

(defun quaestor-normalize-questions (raw-questions)
  "Return RAW-QUESTIONS normalized as a non-empty list of questions."
  (let ((questions (coerce (or raw-questions #()) 'list)))
    (unless questions
      (error "request_user_input requires at least one question."))
    (loop :for question :in questions
          :for index :from 0
          :collect (quaestor-normalize-question question index))))

(defun quaestor-default-selected-options (questions)
  "Return the default selected options alist for QUESTIONS."
  (loop :for question :in questions
        :for id := (quaestor-alist-value question :id)
        :for options := (quaestor-alist-value question :options)
        :unless (or (quaestor-alist-value question :multiple)
                    (null options))
          :collect (cons id
                         (list (quaestor-alist-value (first options)
                                                     :label)))))

(defun quaestor-request-focus-for-question (question)
  "Return the preferred focus keyword for QUESTION."
  (if (quaestor-alist-value question :options)
      :options
      :notes))

(defun quaestor-request-allows-notes-p (question)
  "Return true when QUESTION accepts freeform notes."
  (quaestor-boolean (quaestor-alist-value question :allow-notes)))

(defun quaestor-current-request (buffer)
  "Return BUFFER's current quaestor request state."
  (buffer-user-input-pending buffer))

(defun quaestor-request-questions (request)
  "Return REQUEST's normalized question list."
  (copy-list (quaestor-alist-value request :questions)))

(defun quaestor-request-current-index (request)
  "Return REQUEST's active question index."
  (or (quaestor-alist-value request :current-index) 0))

(defun quaestor-request-current-question (request)
  "Return REQUEST's active question alist."
  (nth (quaestor-request-current-index request)
       (quaestor-request-questions request)))

(defun quaestor-request-notes (request question-id)
  "Return REQUEST's saved notes for QUESTION-ID."
  (cdr (assoc question-id
              (quaestor-alist-value request :notes)
              :test #'string=)))

(defun quaestor-request-selected-options (request question-id)
  "Return REQUEST's selected option labels for QUESTION-ID."
  (copy-list
   (cdr (assoc question-id
               (quaestor-alist-value request :selected-options)
               :test #'string=))))

(defun quaestor-request-put-notes (request question-id notes)
  "Return REQUEST with QUESTION-ID notes updated to NOTES."
  (quaestor-alist-put
   request
   :notes
   (acons question-id notes
          (remove question-id
                  (quaestor-alist-value request :notes)
                  :key #'car
                  :test #'string=))))

(defun quaestor-request-put-selected-options (request question-id selections)
  "Return REQUEST with QUESTION-ID selections updated to SELECTIONS."
  (quaestor-alist-put
   request
   :selected-options
   (acons question-id selections
          (remove question-id
                  (quaestor-alist-value request :selected-options)
                  :key #'car
                  :test #'string=))))

(defun quaestor-request-focus (request)
  "Return REQUEST's current focus keyword."
  (or (quaestor-alist-value request :focus) :options))

(defun quaestor-set-current-request (buffer request)
  "Install REQUEST as BUFFER's pending quaestor state."
  (setf (buffer-user-input-pending buffer) request)
  (notify-buffer-display-change buffer :question)
  request)

(defun quaestor-request-summary-lines (request)
  "Return REQUEST rendered as summary lines."
  (loop :for question :in (quaestor-request-questions request)
        :append
        (append
         (list (format nil "~A: ~A"
                       (quaestor-alist-value question :header)
                       (quaestor-alist-value question :question)))
         (loop :for option :in (quaestor-alist-value question :options)
               :collect (format nil "  - ~A: ~A"
                                (quaestor-alist-value option :label)
                                (quaestor-alist-value option :description))))))

(defun quaestor-answer-summary-lines (request)
  "Return REQUEST's current answers rendered as summary lines."
  (loop :for question :in (quaestor-request-questions request)
        :for qid := (quaestor-alist-value question :id)
        :for labels := (quaestor-request-selected-options request qid)
        :for notes := (quaestor-trimmed-string
                       (quaestor-request-notes request qid))
        :collect
        (format nil "~A: ~A"
                (quaestor-alist-value question :header)
                (or (and (or labels (quaestor-nonblank-string-p notes))
                         (format nil "~{~A~^, ~}~@[; ~A~]"
                                 labels
                                 (and (quaestor-nonblank-string-p notes)
                                      notes)))
                    "(skipped)"))))

(defun quaestor-request-response-payload (request)
  "Return REQUEST's normalized answer payload."
  (list :answers
        (loop :for question :in (quaestor-request-questions request)
              :for qid := (quaestor-alist-value question :id)
              :for labels := (quaestor-request-selected-options request qid)
              :for notes := (quaestor-trimmed-string
                             (quaestor-request-notes request qid))
              :for answers := (append labels
                                      (when (quaestor-nonblank-string-p notes)
                                        (list notes)))
              :collect
              (cons qid
                    (list :answers (coerce answers 'vector))))))

(defun quaestor-request-response-string (request)
  "Return REQUEST's answer payload as agent-readable Lisp data."
  (lisp-data-string (quaestor-request-response-payload request)))

(defun quaestor-request-display-string (request)
  "Return a concise display string for REQUEST's answer payload."
  (format nil "[request_user_input answered: ~{~A~^ | ~}]"
          (quaestor-answer-summary-lines request)))

(defun quaestor-serialize-function-symbol (symbol)
  "Return SYMBOL serialized as PACKAGE::NAME, or NIL."
  (when symbol
    (format nil "~A::~A"
            (package-name (symbol-package symbol))
            (symbol-name symbol))))

(defun quaestor-deserialize-function-symbol (string)
  "Return STRING resolved to a function symbol, or NIL."
  (when (quaestor-nonblank-string-p string)
    (let ((separator (search "::" string)))
      (when separator
        (let* ((package-name (subseq string 0 separator))
               (symbol-name (subseq string (+ separator 2)))
               (package (find-package package-name)))
          (when package
            (multiple-value-bind (symbol status)
                (find-symbol symbol-name package)
              (declare (ignore status))
              (and symbol (fboundp symbol) symbol))))))))

(defun quaestor-restore-question-input (buffer request)
  "Restore BUFFER's input editor from REQUEST's active question."
  (let* ((question (quaestor-request-current-question request))
         (notes (and question
                     (quaestor-request-allows-notes-p question)
                     (quaestor-request-notes
                      request
                      (quaestor-alist-value question :id)))))
    (set-message-text (buffer-input-message buffer) (or notes ""))
    (mark-buffer-dirty buffer))
  request)

(defun quaestor-save-current-question-input (buffer request)
  "Persist BUFFER's current input editor text into REQUEST and return it."
  (let ((question (quaestor-request-current-question request)))
    (if (or (null question)
            (not (quaestor-request-allows-notes-p question)))
        request
        (quaestor-request-put-notes
         request
         (quaestor-alist-value question :id)
         (message-text (buffer-input-message buffer))))))

(defun quaestor-select-option (buffer delta)
  "Move BUFFER's current quaestor option selection by DELTA."
  (let* ((request (quaestor-current-request buffer))
         (question (and request (quaestor-request-current-question request)))
         (options (and question (quaestor-alist-value question :options))))
    (when options
      (let* ((qid (quaestor-alist-value question :id))
             (current (quaestor-request-selected-options request qid))
             (labels (mapcar (lambda (option)
                               (quaestor-alist-value option :label))
                             options))
             (current-index (or (position (first current) labels :test #'string=)
                                0))
             (new-index (mod (+ current-index delta) (length labels)))
             (new-selection
               (if (quaestor-alist-value question :multiple)
                   current
                   (list (nth new-index labels)))))
        (unless (quaestor-alist-value question :multiple)
          (setf request
                (quaestor-request-put-selected-options request qid new-selection)))
        (setf request
              (quaestor-alist-put request :option-index new-index))
        (quaestor-set-current-request buffer request)))))

(defun quaestor-toggle-current-option (buffer)
  "Toggle BUFFER's currently highlighted quaestor option."
  (let* ((request (quaestor-current-request buffer))
         (question (and request (quaestor-request-current-question request)))
         (options (and question (quaestor-alist-value question :options))))
    (when options
      (let* ((qid (quaestor-alist-value question :id))
             (labels (mapcar (lambda (option)
                               (quaestor-alist-value option :label))
                             options))
             (index (or (quaestor-alist-value request :option-index) 0))
             (label (nth (max 0 (min index (1- (length labels)))) labels))
             (selected (quaestor-request-selected-options request qid)))
        (setf request
              (quaestor-request-put-selected-options
               request
               qid
               (if (quaestor-alist-value question :multiple)
                   (if (member label selected :test #'string=)
                       (remove label selected :test #'string=)
                       (append selected (list label)))
                   (list label))))
        (quaestor-set-current-request buffer request)))))

(defun quaestor-switch-question (buffer direction)
  "Move BUFFER's request view by DIRECTION questions."
  (let* ((request (quaestor-save-current-question-input
                   buffer
                   (quaestor-current-request buffer)))
         (questions (quaestor-request-questions request))
         (current (quaestor-request-current-index request))
         (next-index (max 0 (min (+ current direction)
                                 (1- (length questions)))))
         (next-question (nth next-index questions))
         (focus (quaestor-request-focus-for-question next-question)))
    (setf request
          (quaestor-alist-put
           (quaestor-alist-put request :current-index next-index)
           :focus focus))
    (quaestor-set-current-request buffer request)
    (quaestor-restore-question-input buffer request)))

(declaim (ftype (function (buffer) t) quaestor-submit-current-request))
(defun quaestor-next-question-or-submit (buffer)
  "Advance BUFFER's active request or submit it on the last question."
  (let* ((request (quaestor-save-current-question-input
                   buffer
                   (quaestor-current-request buffer)))
         (questions (quaestor-request-questions request)))
    (setf (buffer-user-input-pending buffer) request)
    (if (< (quaestor-request-current-index request)
           (1- (length questions)))
        (quaestor-switch-question buffer 1)
        (quaestor-submit-current-request buffer))))

(defun quaestor-begin-request
    (buffer questions &key source tool-id resume-function resume-state)
  "Start a structured user-input request on BUFFER."
  (when (buffer-user-input-pending buffer)
    (error "A request_user_input interaction is already active in this buffer."))
  (let* ((normalized (quaestor-normalize-questions questions))
         (request
           `((:source . ,source)
             (:tool-id . ,tool-id)
             (:current-index . 0)
             (:focus . ,(quaestor-request-focus-for-question (first normalized)))
             (:questions . ,normalized)
             (:selected-options . ,(quaestor-default-selected-options normalized))
             (:notes . nil)
             (:resume-function . ,(quaestor-serialize-function-symbol
                                   resume-function))
             (:resume-state . ,resume-state)
             (:requested-at . ,(get-universal-time)))))
    (when (and (null (buffer-stashed-input buffer))
               (quaestor-nonblank-string-p
                (message-text (buffer-input-message buffer))))
      (setf (buffer-stashed-input buffer)
            (message-text (buffer-input-message buffer))))
    (set-message-text (buffer-input-message buffer) "")
    (setf (buffer-status buffer) :question)
    (buffer-insert-system-message
     buffer
     (format nil "[request_user_input]~%~{~A~%~}"
             (quaestor-request-summary-lines request))
     :metadata `((:kind . "request-user-input")
                 (:request . ,request)))
    (quaestor-set-current-request buffer request)
    (quaestor-restore-question-input buffer request)
    request))

(defun quaestor-clear-request (buffer)
  "Clear BUFFER's active quaestor request and restore any stashed input."
  (setf (buffer-user-input-pending buffer) nil)
  (let ((stashed (buffer-stashed-input buffer)))
    (set-message-text (buffer-input-message buffer) (or stashed ""))
    (setf (buffer-stashed-input buffer) nil)
    (mark-buffer-dirty buffer))
  (notify-buffer-display-change buffer :question)
  nil)

(defun quaestor-complete-tool-request (buffer request)
  "Finish a tool-sourced REQUEST on BUFFER and resume tool processing."
  (let* ((tool-id (quaestor-alist-value request :tool-id))
         (result-text (quaestor-request-response-string request))
         (display (quaestor-request-display-string request)))
    (push `((:result . ,result-text)
            (:display . ,display)
            (:tool-id . ,tool-id))
          (buffer-tool-call-results buffer))
    (when (buffer-pending-tool-calls buffer)
      (pop (buffer-pending-tool-calls buffer)))
    (quaestor-clear-request buffer)
    (advance-tool-approval buffer)))

(defun quaestor-complete-lisp-request (buffer request)
  "Finish a Lisp-sourced REQUEST on BUFFER and invoke its resume hook."
  (let ((resume-symbol
          (quaestor-deserialize-function-symbol
           (quaestor-alist-value request :resume-function))))
    (quaestor-clear-request buffer)
    (setf (buffer-status buffer) :idle)
    (notify-buffer-display-change buffer :status)
    (when resume-symbol
      (funcall resume-symbol
               buffer
               request
               (quaestor-request-response-payload request)
               (quaestor-alist-value request :resume-state)))))

(defun quaestor-submit-current-request (buffer)
  "Submit BUFFER's current quaestor request."
  (let ((request (quaestor-save-current-question-input
                  buffer
                  (quaestor-current-request buffer))))
    (setf (buffer-user-input-pending buffer) request)
    (buffer-insert-system-message
     buffer
     (format nil "[request_user_input answered]~%~{~A~%~}"
             (quaestor-answer-summary-lines request))
     :metadata `((:kind . "request-user-input-answer")
                 (:response . ,(quaestor-request-response-payload request))))
    (if (eq (quaestor-alist-value request :source) :tool)
        (quaestor-complete-tool-request buffer request)
        (quaestor-complete-lisp-request buffer request))))

(defun quaestor-queue-entry-description (entry)
  "Return ENTRY rendered for queue inspection."
  (format nil "[~(~A~)] ~A"
          (or (getf entry :kind) :follow-up)
          (or (getf entry :text) "")))

(defun quaestor-pop-last-queued-message (buffer)
  "Pop and return BUFFER's most recently queued message, or NIL."
  (let ((follow-ups (buffer-queued-follow-up-messages buffer))
        (steering (buffer-queued-steering-messages buffer)))
    (cond
      (follow-ups
       (let* ((last-index (1- (length follow-ups)))
              (entry (nth last-index follow-ups)))
         (setf (buffer-queued-follow-up-messages buffer)
               (subseq follow-ups 0 last-index))
         (notify-buffer-display-change buffer :queued-message)
         entry))
      (steering
       (let* ((last-index (1- (length steering)))
              (entry (nth last-index steering)))
         (setf (buffer-queued-steering-messages buffer)
               (subseq steering 0 last-index))
         (notify-buffer-display-change buffer :queued-message)
         entry))
      (t nil))))

(defun quaestor-recall-last-queued-message-command (buffer)
  "Recall the most recent queued message into BUFFER's input editor."
  (let ((entry (quaestor-pop-last-queued-message buffer)))
    (if (null entry)
        (buffer-insert-system-message buffer "[No queued message to recall.]")
        (let* ((current (message-text (buffer-input-message buffer)))
               (restored (getf entry :text)))
          (set-message-text
           (buffer-input-message buffer)
           (if (quaestor-nonblank-string-p current)
               (format nil "~A~%~%~A" current restored)
               restored))
          (mark-buffer-dirty buffer)
          (notify-buffer-display-change buffer :queued-message)
          entry))))
(defcommand quaestor-recall-last-queued-message-command)

(defun quaestor-show-queued-messages-command (buffer)
  "Display BUFFER's queued steering and follow-up messages."
  (let ((messages (buffer-queued-messages buffer)))
    (switch-to-buffer
     (make-help-buffer
      "*help:quaestor-queue*"
      (if messages
          (format nil "Queued Messages~%===============~%~%~{~A~%~}"
                  (mapcar #'quaestor-queue-entry-description messages))
          (format nil "Queued Messages~%===============~%~%(no queued messages)~%"))))))
(defcommand quaestor-show-queued-messages-command)

(defun quaestor-queue-steering-message-command (buffer)
  "Queue BUFFER's current input as a steering message and interrupt when possible."
  (let ((text (message-text (buffer-input-message buffer))))
    (unless (quaestor-nonblank-string-p text)
      (buffer-insert-system-message buffer "[No steering text to queue.]")
      (return-from quaestor-queue-steering-message-command nil))
    (queue-buffer-message buffer :steering text)
    (set-message-text (buffer-input-message buffer) "")
    (mark-buffer-dirty buffer)
    (when (buffer-llm-running-p buffer)
      (stop-llm-command buffer))
    (buffer-insert-system-message buffer "[Queued steering message.]")
    t))
(defcommand quaestor-queue-steering-message-command)

(defun quaestor-cancel-and-restore-command (buffer)
  "Stop the current response when needed and restore queued drafts to input."
  (let ((current (message-text (buffer-input-message buffer))))
    (when (buffer-llm-running-p buffer)
      (stop-llm-command buffer))
    (restore-buffer-queued-messages-to-input buffer)
    (when (quaestor-nonblank-string-p current)
      (let ((restored (message-text (buffer-input-message buffer))))
        (set-message-text
         (buffer-input-message buffer)
         (if (quaestor-nonblank-string-p restored)
             (format nil "~A~%~%~A" restored current)
             current))))
    (mark-buffer-dirty buffer)
    (notify-buffer-display-change buffer :queued-message)
    t))
(defcommand quaestor-cancel-and-restore-command)

(defun quaestor-install-keybindings ()
  "Install global keybindings for quaestor queue commands."
  (when (and (boundp '*default-keymap*) *default-keymap*)
    (keymap-bind *default-keymap* '(:ctrl-c #\q)
                 'quaestor-show-queued-messages-command)
    (keymap-bind *default-keymap* '(:ctrl-c #\Q)
                 'quaestor-recall-last-queued-message-command)
    (keymap-bind *default-keymap* '(:ctrl-c #\j)
                 'quaestor-queue-steering-message-command)
    (keymap-bind *default-keymap* '(:ctrl-c #\J)
                 'quaestor-cancel-and-restore-command)))

(defun quaestor-flush-next-queued-message (buffer)
  "Submit BUFFER's next queued steering or follow-up message, when present."
  (let ((entry (or (dequeue-buffer-steering-message buffer)
                   (dequeue-buffer-follow-up-message buffer))))
    (when entry
      (let ((*quaestor-flushing-queue* t))
        (set-message-text (buffer-input-message buffer) (getf entry :text))
        (mark-buffer-dirty buffer)
        (send-message buffer)))))

(defun quaestor-maybe-flush-queue (buffer reason)
  "Submit queued messages for BUFFER when the active run has gone idle."
  (declare (ignore reason))
  (when (and (quaestor-package-active-p buffer)
             (not *quaestor-flushing-queue*)
             (eq (buffer-status buffer) :idle)
             (not (buffer-agent-busy-p buffer))
             (not (buffer-user-input-pending buffer))
             (buffer-has-queued-messages-p buffer))
    (quaestor-flush-next-queued-message buffer)))

(defun quaestor-request-tool-suspension-p (value)
  "Return true when VALUE is a caught quaestor suspension marker."
  (and (quaestor-property-alist-p value)
       (quaestor-alist-value value :quaestor-request)))

(defun quaestor-build-tool-request (buffer marker)
  "Start BUFFER's UI for a caught tool suspension MARKER."
  (let ((current-tool (first (buffer-pending-tool-calls buffer))))
    (quaestor-begin-request
     buffer
     (quaestor-alist-value marker :questions)
     :source :tool
     :tool-id (cdr (assoc :id current-tool)))))

(defun quaestor-handle-pending-request-key (buffer key)
  "Handle KEY for BUFFER's active quaestor request.
Returns true when the key was consumed."
  (let ((request (quaestor-current-request buffer)))
    (when request
      (let* ((question (quaestor-request-current-question request))
             (allow-notes-p (and question
                                 (quaestor-request-allows-notes-p question))))
        (cond
        ((or (equal key :page-up) (equal key :prior))
         (quaestor-switch-question buffer -1)
         t)
        ((or (equal key :page-down) (equal key :next))
         (quaestor-switch-question buffer 1)
         t)
        ((and allow-notes-p
              (characterp key)
              (char= key #\Tab))
         (quaestor-set-current-request
          buffer
          (quaestor-alist-put
           request
           :focus
           (if (eq (quaestor-request-focus request) :options)
               :notes
               :options)))
         t)
        ((and (eq (quaestor-request-focus request) :options)
              (or (equal key :up)
                  (and (characterp key) (char= key (code-char 16)))))
         (quaestor-select-option buffer -1)
         t)
        ((and (eq (quaestor-request-focus request) :options)
              (or (equal key :down)
                  (and (characterp key) (char= key (code-char 14)))))
         (quaestor-select-option buffer 1)
         t)
        ((and (eq (quaestor-request-focus request) :options)
              (characterp key)
              (char= key #\Space))
         (quaestor-toggle-current-option buffer)
         t)
        ((and (characterp key)
              (or (char= key #\Newline)
                  (char= key #\Return)))
         (quaestor-next-question-or-submit buffer)
         t)
        ((and allow-notes-p
              (eq (quaestor-request-focus request) :options)
              (characterp key)
              (graphic-char-p key))
         (quaestor-set-current-request
          buffer
          (quaestor-alist-put request :focus :notes))
         (let ((*self-insert-char* key))
           (self-insert-command buffer))
         t)
        (t
         (let ((cmd (keymap-lookup (buffer-keymap buffer) key)))
           (cond
             ((and cmd (not (eq cmd 'send-message)))
              (invoke-command buffer cmd)
              t)
             ((and allow-notes-p
                   (characterp key)
                   (graphic-char-p key))
              (let ((*self-insert-char* key))
                (self-insert-command buffer))
              t)
             (t nil)))))))))

(defun quaestor-activate-option (buffer option)
  "Select OPTION in BUFFER's current request and toggle/choose it."
  (let* ((request (quaestor-current-request buffer))
         (question (and request (quaestor-request-current-question request)))
         (question-id (and question (quaestor-alist-value question :id)))
         (index (and (listp option) (getf option :index))))
    (when (and request question question-id
               (listp option)
               (string= question-id (or (getf option :question-id) ""))
               (integerp index)
               (<= 0 index)
               (< index (length (quaestor-alist-value question :options))))
      (setf request (quaestor-alist-put request :focus :options))
      (setf request (quaestor-alist-put request :option-index index))
      (quaestor-set-current-request buffer request)
      (quaestor-toggle-current-option buffer))))

(defun quaestor-option-presentation-object (question option index)
  "Return a presentation object for OPTION in QUESTION."
  (list :question-id (quaestor-alist-value question :id)
        :label (quaestor-alist-value option :label)
        :index index))

(defun quaestor-submit-presentation-object (request)
  "Return a presentation object for advancing or submitting REQUEST."
  (list :quaestor-submit-p t
        :question-id (and (quaestor-request-current-question request)
                          (quaestor-alist-value
                           (quaestor-request-current-question request)
                           :id))))

(defun quaestor-option-entry (question option index selected-labels active-index)
  "Return one generic presentation entry for OPTION."
  (let* ((label (quaestor-alist-value option :label))
         (selected-p (member label selected-labels :test #'string=))
         (active-p (= index active-index)))
    (list :text (format nil "~A [~A] ~A — ~A"
                        (if active-p ">" " ")
                        (if selected-p "x" " ")
                        label
                        (quaestor-alist-value option :description))
          :face (if selected-p :selector-selected :selector-entry)
          :object (quaestor-option-presentation-object question option index)
          :presentation-type 'quaestor-option-ref
          :unique-id (list :quaestor-option
                           (quaestor-alist-value question :id)
                           label))))

(defun quaestor-input-presentation-entries (buffer columns)
  "Return generic McCLIM entries for BUFFER's active quaestor request."
  (declare (ignore columns))
  (let ((request (and (quaestor-package-active-p buffer)
                      (quaestor-current-request buffer))))
    (when request
      (let* ((questions (quaestor-request-questions request))
             (question (quaestor-request-current-question request))
             (question-id (and question (quaestor-alist-value question :id)))
             (index (quaestor-request-current-index request))
             (options (and question (quaestor-alist-value question :options)))
             (selected-labels (and question
                                   (quaestor-request-selected-options
                                    request question-id)))
             (active-index (or (quaestor-alist-value request :option-index) 0))
             (allow-notes-p (and question
                                  (quaestor-request-allows-notes-p question)))
             (notes (buffer-input-presentation-text buffer))
             (next-label (if (< index (1- (length questions))) "Next" "Submit")))
        (append
         (list
          (list :text "" :face :default-text)
          (list :text (format nil "Quaestor request ~D/~D: ~A"
                              (1+ index)
                              (length questions)
                              (quaestor-alist-value question :header))
                :face :selector-title
                :unique-id (list :quaestor-title question-id))
          (list :text (quaestor-alist-value question :question)
                :face :selector-header
                :unique-id (list :quaestor-question question-id))
          (list :text (format nil "Focus: ~(~A~). Tab switches focus; Return advances."
                              (quaestor-request-focus request))
                :face :selector-footer
                :unique-id (list :quaestor-focus question-id)))
         (loop :for option :in options
               :for option-index :from 0
               :collect (quaestor-option-entry question option option-index
                                               selected-labels active-index))
         (when allow-notes-p
           (list (list :text (format nil "Notes: ~A" notes)
                       :face (if (eq (quaestor-request-focus request) :notes)
                                 :selector-selected
                                 :selector-footer)
                       :unique-id (list :quaestor-notes question-id))))
         (list (list :text (format nil "[~A request]" next-label)
                     :face :selector-footer
                     :object (quaestor-submit-presentation-object request)
                     :presentation-type 'quaestor-submit-ref
                     :unique-id (list :quaestor-submit question-id))))))))

(define-clawmacs-chat-frame-command
    (com-quaestor-select-option :name nil)
    ((option 'quaestor-option-ref))
  (clim:with-application-frame (frame)
    (quaestor-activate-option (chat-frame-buffer frame) option)))

(define-clawmacs-chat-frame-command
    (com-quaestor-submit-request :name nil)
    ((submit 'quaestor-submit-ref))
  (declare (ignore submit))
  (clim:with-application-frame (frame)
    (quaestor-next-question-or-submit (chat-frame-buffer frame))))

(clim:define-presentation-to-command-translator select-quaestor-option
    (quaestor-option-ref com-quaestor-select-option
     clawmacs-chat-frame
     :gesture :select
     :documentation "Select answer option"
     :menu t)
    (object)
  (list object))

(clim:define-presentation-to-command-translator select-quaestor-submit
    (quaestor-submit-ref com-quaestor-submit-request
     clawmacs-chat-frame
     :gesture :select
     :documentation "Advance request"
     :menu t)
    (object)
  (list object))

(defun quaestor-request-user-input-tool (args)
  "Suspend the active agent turn and request structured user input."
  (unless *quaestor-request-user-input-catch-active*
    (error "request_user_input is only available during an active interactive agent turn."))
  (unless (and *current-tool-buffer*
               (quaestor-package-active-p *current-tool-buffer*))
    (error "request_user_input requires the quaestor package to be enabled."))
  (throw 'quaestor-suspend
         `((:quaestor-request . t)
           (:questions . ,(quaestor-normalize-questions
                           (tool-arg args :questions "questions"))))))

(deftool quaestor-request-user-input-tool
  :name "request_user_input"
  :description "Pause the interactive agent turn and ask the user one or more structured questions with optional choices and freeform notes."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((questions :type "array"
                    :items ((:type . "object"))
                    :description "Non-empty array of question objects. Each object requires header, id, and question. Optional fields: options (array of {label, description}), multiple (boolean), and freeform/is_other (boolean).")))

(defun quaestor-request-user-input (buffer questions
                                     &key resume-function resume-state)
  "Public Lisp API for package or pipeline code to ask QUESTIONS in BUFFER."
  (unless (quaestor-package-active-p buffer)
    (error "quaestor package is not active for buffer ~A." (buffer-name buffer)))
  (quaestor-begin-request buffer questions
                          :source :lisp
                          :resume-function resume-function
                          :resume-state resume-state))

(defadvice send-message quaestor-send-message :around (next buffer)
  (if (or *quaestor-flushing-queue*
          (not (quaestor-package-active-p buffer))
          (document-buffer-p buffer)
          (listener-buffer-p buffer))
      (funcall next buffer)
      (let ((input-text (message-text (buffer-input-message buffer))))
        (cond
          ((buffer-user-input-pending buffer)
           (quaestor-submit-current-request buffer))
          ((and (buffer-agent-busy-p buffer)
                (quaestor-nonblank-string-p input-text))
           (queue-buffer-message buffer :follow-up input-text)
           (set-message-text (buffer-input-message buffer) "")
           (mark-buffer-dirty buffer)
           (notify-buffer-display-change buffer :queued-message)
           (buffer-insert-system-message buffer "[Queued follow-up message.]"))
          (t
           (funcall next buffer))))))

(defadvice handle-key-event quaestor-handle-key-event :around (next buffer key)
  (if (and (quaestor-package-active-p buffer)
           (buffer-user-input-pending buffer)
           (quaestor-handle-pending-request-key buffer key))
      nil
      (funcall next buffer key)))

(defadvice advance-tool-approval quaestor-advance-tool-approval :around (next buffer)
  (let* ((*quaestor-request-user-input-catch-active* t)
         (outcome (catch 'quaestor-suspend
                    (multiple-value-list (funcall next buffer)))))
    (if (quaestor-request-tool-suspension-p outcome)
        (progn
          (quaestor-build-tool-request buffer outcome)
          nil)
        (values-list outcome))))

(defadvice execute-prompt-tool-call quaestor-prompt-tool-call :around
    (next buffer tool-use-block agent-kw auto-approve-tools-p
          &key event-callback &allow-other-keys)
  (let* ((*quaestor-request-user-input-catch-active* t)
         (outcome (catch 'quaestor-suspend
                    (multiple-value-list
                     (funcall next buffer tool-use-block agent-kw
                              auto-approve-tools-p
                              :event-callback event-callback)))))
    (if (quaestor-request-tool-suspension-p outcome)
        (let* ((tool-id (cdr (assoc :id tool-use-block)))
               (tool-input (cdr (assoc :input tool-use-block)))
               (display "[request_user_input unavailable in prompt mode]")
               (result-text
                 (tool-error-result-data
                  "request_user_input is unavailable in non-interactive prompt mode."))
               (result `((:result . ,result-text)
                         (:display . ,display)
                         (:tool-id . ,tool-id)))
               (event (make-prompt-tool-event
                       :id tool-id
                       :name "request_user_input"
                       :input tool-input
                       :result-text result-text
                       :display display
                       :denied-p t)))
          (when event-callback
            (funcall event-callback
                     (list :event "tool.call"
                           :id tool-id
                           :name "request_user_input"
                           :input tool-input))
            (funcall event-callback
                     (list :event "tool.result"
                           :id tool-id
                           :name "request_user_input"
                           :result result-text
                           :denied-p t)))
          (values result event))
        (values-list outcome))))

(defadvice init-default-keymap quaestor-init-default-keymap :after (result)
  (declare (ignore result))
  (quaestor-install-keybindings))

(register-buffer-type :chat
                      :input-presentation-function
                      'quaestor-input-presentation-entries
                      :package nil)

(register-package-prompt-section
 "quaestor"
 "## Structured user questions with quaestor

- Use `request_user_input` when the task genuinely needs a user decision or
  missing requirement before you can continue safely.
- Questions may be freeform, single-choice, or multi-choice. Keep them short,
  concrete, and grounded in the current task.
- While a response is running, Clawmacs can queue follow-up or steering
  messages instead of losing them. The user can inspect or recall the queue
  with the quaestor commands."
 :title "Structured user questions with quaestor"
 :package "quaestor")

(quaestor-install-keybindings)
(add-hook '*after-buffer-display-change-hook* 'quaestor-maybe-flush-queue
          :append t)
