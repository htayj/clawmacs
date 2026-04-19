(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Self-insert support (must be defined before commands that reference it)
;;; --------------------------------------------------------------------------

(defvar *self-insert-char* nil
  "The character to insert for self-insert-command. Bound by the event loop.")

;;; --------------------------------------------------------------------------
;;; Interactive Command Dispatch
;;; --------------------------------------------------------------------------

(defun command-display-name (command)
  "Return the display name used for COMMAND in the UI."
  (string-downcase (symbol-name command)))

(defun command-keybinding-hints (command)
  "Return formatted keybinding strings for COMMAND in the default keymap."
  (let ((bindings (find-keybindings-for-command command)))
    (sort (mapcar #'format-key-binding bindings) #'string<)))

(defun make-command-selector-items (&key buffer)
  "Build minibuffer items for command selection."
  (mapcar (lambda (command)
            (let* ((name (command-display-name command))
                   (keys (command-keybinding-hints command))
                   (display (if keys
                                (format nil "~A  [~{~A~^, ~}]" name keys)
                                name)))
              (list :command command
                    :display display
                    :match-text name)))
          (sort (copy-list (list-available-commands :buffer buffer))
                #'string<
                :key #'command-display-name)))

(defun call-command-function (buffer command args)
  "Invoke COMMAND with BUFFER and ARGS, running command hooks around it."
  (run-hook-with-args '*before-command-hook* buffer command)
  (let ((values (multiple-value-list
                 (apply (symbol-function command) buffer args))))
    (run-hook-with-args '*after-command-hook* buffer command (first values))
    (values-list values)))

(defun prompt-command-arguments (buffer command specs &optional (collected nil)
                                                   (initial-input ""))
  "Prompt for SPECS sequentially in the minibuffer, then invoke COMMAND."
  (if (endp specs)
      (call-command-function buffer command (nreverse collected))
      (let* ((spec (first specs))
             (arg-name (getf spec :name))
             (prompt (getf spec :prompt))
             (reader (resolve-command-prompt-reader (getf spec :reader))))
        (minibuffer-prompt
         prompt
         (lambda (input)
           (handler-case
               (let ((value (funcall reader input)))
                 (prompt-command-arguments buffer command (rest specs)
                                           (cons value collected)))
             (error (e)
               (buffer-insert-system-message
                buffer
                (format nil "[Invalid ~A for ~A: ~A]"
                        (command-display-name arg-name)
                        (command-display-name command)
                        e))
               (prompt-command-arguments buffer command specs
                                         collected input))))
         :initial-input initial-input))))

(defun invoke-command (buffer command)
  "Invoke COMMAND from the UI, prompting for command arguments when needed."
  (let* ((metadata (gethash command *command-table*))
         (required-args (and metadata (command-required-arguments command)))
         (prompts (and metadata
                       (command-metadata-prompts metadata))))
    (cond
      ((null metadata)
       (error "Unknown command: ~A" command))
      ((not (command-metadata-visible-p metadata :buffer buffer))
       (error "Command ~A belongs to an inactive package." command))
      ((null required-args)
       (call-command-function buffer command nil))
      (prompts
       (prompt-command-arguments buffer command prompts)
       nil)
      (t
       (error "Command ~A is missing prompt metadata for arguments ~S."
              command required-args)))))

;;; --------------------------------------------------------------------------
;;; Commands
;;; --------------------------------------------------------------------------

(defun send-message (buffer)
  "Send the current input message to the agent."
  (if (document-buffer-p buffer)
      (insert-newline-command buffer)
      (let ((input-text (message-text (buffer-input-message buffer))))
        (when (plusp (length (string-trim '(#\Space #\Tab #\Newline) input-text)))
          (run-hook-with-args '*before-send-message-hook* buffer input-text)
          (unless (find-prefix-handler input-text)
            (maybe-compact-buffer buffer
                                  :reason :pre-user-message
                                  :include-current-input-p t))
          (buffer-finalize-input buffer)
          (setf (message-face-set (buffer-input-message buffer))
                (gethash :user (buffer-face-registry buffer)))
          (let ((result
                  ;; Check for prefix commands before sending to the LLM
                  (or (process-prefix-command buffer input-text)
                      (if (buffer-pipeline-name buffer)
                          (run-pipeline-for-buffer buffer input-text)
                          (send-to-agent-with-context buffer)))))
            (run-hook-with-args '*after-send-message-hook*
                                buffer input-text result)
            result)))))
(defcommand send-message :keys (#\Return))

(defun insert-newline-command (buffer)
  "Insert a newline in the input message."
  (message-insert-newline (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand insert-newline-command :keys (#\Linefeed))

(defun beginning-of-line-command (buffer)
  "Move point to the beginning of the current line."
  (message-move-beginning-of-line (buffer-input-message buffer)))
(defcommand beginning-of-line-command)

(defun end-of-line-command (buffer)
  "Move point to the end of the current line."
  (message-move-end-of-line (buffer-input-message buffer)))
(defcommand end-of-line-command)

(defun kill-line-command (buffer)
  "Kill from point to the end of the line."
  (message-kill-line (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand kill-line-command)

(defun yank-command (buffer)
  "Yank the top of the kill ring at point."
  (message-yank (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand yank-command)

(defun delete-char-backward-command (buffer)
  "Delete the character before point."
  (message-delete-char-backward (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand delete-char-backward-command)

(defun delete-char-forward-command (buffer)
  "Delete the character after point."
  (message-delete-char-forward (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand delete-char-forward-command)

(defun forward-char-command (buffer)
  "Move point one character forward."
  (message-forward-char (buffer-input-message buffer)))
(defcommand forward-char-command)

(defun backward-char-command (buffer)
  "Move point one character backward."
  (message-backward-char (buffer-input-message buffer)))
(defcommand backward-char-command)

(defun forward-word-command (buffer)
  "Move point forward to end of next word."
  (message-forward-word (buffer-input-message buffer)))
(defcommand forward-word-command)

(defun backward-word-command (buffer)
  "Move point backward to beginning of previous word."
  (message-backward-word (buffer-input-message buffer)))
(defcommand backward-word-command)

(defun kill-backward-line-command (buffer)
  "Kill from start of line to point."
  (message-kill-backward-line (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand kill-backward-line-command)

(defun kill-word-command (buffer)
  "Kill from point to end of current word."
  (message-kill-word (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand kill-word-command)

(defun backward-kill-word-command (buffer)
  "Kill from beginning of current word to point."
  (message-backward-kill-word (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand backward-kill-word-command)

(defun yank-pop-command (buffer)
  "Replace just-yanked text with next kill ring entry."
  (message-yank-pop (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand yank-pop-command)

(defun message-insert-string (msg text)
  "Insert TEXT at point in MSG."
  (loop :for char :across text
        :do (if (char= char #\Newline)
                (message-insert-newline msg)
                (message-insert-char msg char)))
  msg)

(defun yank-previous-command-first-arg-command (buffer)
  "Insert the first argument of the previous user command."
  (let ((arg (buffer-previous-command-first-argument buffer)))
    (when arg
      (message-insert-string (buffer-input-message buffer) arg)
      (mark-buffer-dirty buffer))))
(defcommand yank-previous-command-first-arg-command)

(defun yank-previous-command-last-arg-command (buffer)
  "Insert the last argument of the previous user command."
  (let ((arg (buffer-previous-command-last-argument buffer)))
    (when arg
      (message-insert-string (buffer-input-message buffer) arg)
      (mark-buffer-dirty buffer))))
(defcommand yank-previous-command-last-arg-command)

(defun self-insert-command (buffer)
  "Insert a character at point. The character is passed via *self-insert-char*."
  (when *self-insert-char*
    (message-insert-char (buffer-input-message buffer) *self-insert-char*)
    (mark-buffer-dirty buffer)))

;;; --------------------------------------------------------------------------
;;; Scroll Commands
;;; --------------------------------------------------------------------------

(defvar *scroll-page-size* nil
  "Number of rows to scroll per page. Set by the event loop based on window height.")

(defun scroll-up-command (buffer)
  "Scroll history up (back) by one page."
  (when *scroll-page-size*
    (incf (buffer-scroll-offset buffer) *scroll-page-size*)))
(defcommand scroll-up-command)

(defun scroll-down-command (buffer)
  "Scroll history down (forward) by one page."
  (when *scroll-page-size*
    (decf (buffer-scroll-offset buffer) *scroll-page-size*)
    (when (minusp (buffer-scroll-offset buffer))
      (setf (buffer-scroll-offset buffer) 0))))
(defcommand scroll-down-command)

;;; --------------------------------------------------------------------------
;;; OpenAI Codex OAuth Command
;;; --------------------------------------------------------------------------

(defun openai-codex-oauth-command (buffer)
  "Start the OpenAI Codex OAuth login flow using a localhost browser callback."
  (handler-case
      (progn
        (when *openai-oauth-pending*
          (error "An OpenAI Codex OAuth login is already in progress"))
        (setf *openai-oauth-pending*
              (start-openai-codex-oauth-login :buffer buffer))
        (let* ((snapshot (openai-oauth-flow-snapshot *openai-oauth-pending*))
               (auth-url (getf snapshot :auth-url))
               (redirect-uri (getf snapshot :redirect-uri)))
          (buffer-insert-system-message
           buffer
           (format nil "[OpenAI Codex OAuth]~%~%A browser login was started for shared Codex auth.~%If the browser did not open, use this URL:~%~%  ~A~%~%The callback server is listening at:~%  ~A~%~%Press C-g to cancel."
                   auth-url redirect-uri))
          (setf (buffer-status buffer) :oauth)
          (notify-buffer-display-change buffer :status)))
    (error (e)
      (buffer-insert-system-message
       buffer
       (format nil "[OAuth error: ~A]" e)))))
(defcommand openai-codex-oauth-command)

;;; --------------------------------------------------------------------------
;;; Buffer Management Commands
;;; --------------------------------------------------------------------------

(defun list-buffers-command (buffer)
  "Open the buffer selector to switch between agent sessions."
  (declare (ignore buffer))
  (setf *buffer-selector-active* t
        *buffer-selector-index* 0
        *buffer-selector-scroll* 0))
(defcommand list-buffers-command)

;;; --------------------------------------------------------------------------
;;; Project Commands
;;; --------------------------------------------------------------------------

(defun ensure-projects-for-ui ()
  "Ensure project definitions are loaded before project UI commands run."
  (unless *project-definitions-loaded-p*
    (load-project-definitions))
  (list-projects))

(defun project-selector-items (&optional active-project-name)
  "Return minibuffer project selector items."
  (mapcar (lambda (project)
            (let* ((name (project-name project))
                   (active-p (and active-project-name
                                  (string= name active-project-name)))
                   (display (format nil "~A ~A  [~(~A~)] ~A"
                                    (if active-p "*" " ")
                                    name
                                    (or (project-source project) :unknown)
                                    (namestring (project-root project)))))
              (list :project project
                    :project-name name
                    :active-p active-p
                    :display display
                    :match-text (format nil "~A ~A ~A"
                                        name
                                        (or (project-description project) "")
                                        (namestring (project-root project))))))
          (ensure-projects-for-ui)))

(defun minibuffer-choose-project (buffer prompt callback)
  "Prompt for a project, then call CALLBACK with the selected project."
  (let ((items (project-selector-items (buffer-project-name buffer))))
    (if items
        (progn
          (minibuffer-activate prompt items
                               (lambda (item)
                                 (funcall callback (getf item :project))))
          (preselect-minibuffer-active-item items))
        (buffer-insert-system-message buffer "[No projects available.]"))))

(defun project-file-selector-items (project)
  "Return minibuffer file selector items for PROJECT."
  (mapcar (lambda (path)
            (list :project project
                  :path path
                  :display path
                  :match-text path))
          (project-list-files project)))

(defun minibuffer-open-project-file (buffer project)
  "Prompt for a file in PROJECT and open it."
  (let ((items (project-file-selector-items project)))
    (if items
        (minibuffer-activate
         (format nil "Open ~A" (project-name project))
         items
         (lambda (item)
           (handler-case
               (project-open-file (getf item :project) (getf item :path))
             (error (e)
               (buffer-insert-system-message
                buffer
                (format nil "[Open project file failed: ~A]" e))))))
        (buffer-insert-system-message
         buffer
         (format nil "[Project ~A has no files.]" (project-name project))))))

(defun current-buffer-project (buffer)
  "Return BUFFER's selected project, or NIL."
  (and (buffer-project-name buffer)
       (find-project (buffer-project-name buffer))))

(defun minibuffer-select-project-command (buffer)
  "Select the active project for the current buffer."
  (if (file-buffer-p buffer)
      (buffer-insert-system-message
       buffer
       "[File buffers keep the project of their backing resource.]")
      (minibuffer-choose-project
       buffer
       "Select Project"
       (lambda (project)
        (setf (buffer-project-name buffer) (project-name project)
              (buffer-working-directory buffer) (project-root project))
        (buffer-insert-system-message
         buffer
         (format nil "[Project changed to ~A]" (project-name project)))))))
(defcommand minibuffer-select-project-command)

(defun open-project-file-command (buffer)
  "Open a file from the current or selected project."
  (let ((project (current-buffer-project buffer)))
    (if project
        (minibuffer-open-project-file buffer project)
        (minibuffer-choose-project buffer
                                   "Select Project"
                                   (lambda (selected-project)
                                     (minibuffer-open-project-file
                                      buffer selected-project))))))
(defcommand open-project-file-command)

(defun create-project-file-command (buffer)
  "Create and open a new file in a selected project."
  (minibuffer-choose-project
   buffer
   "Select Project"
   (lambda (project)
     (minibuffer-prompt
      (format nil "Create in ~A" (project-name project))
      (lambda (path)
        (handler-case
            (progn
              (project-create-file project path)
              (project-open-file project path))
          (error (e)
            (buffer-insert-system-message
             buffer
             (format nil "[Create project file failed: ~A]" e)))))))))
(defcommand create-project-file-command)

(defun search-project-command (buffer)
  "Search a selected project and insert the result list."
  (minibuffer-choose-project
   buffer
   "Select Project"
   (lambda (project)
     (minibuffer-prompt
      (format nil "Search ~A" (project-name project))
      (lambda (query)
        (handler-case
            (buffer-insert-system-message
             buffer
             (project-search-to-string project query))
          (error (e)
            (buffer-insert-system-message
             buffer
             (format nil "[Search project failed: ~A]" e)))))))))
(defcommand search-project-command)

;;; --------------------------------------------------------------------------
;;; Skill Commands
;;; --------------------------------------------------------------------------

(defun skill-mention-text (skill)
  "Return the text inserted for selecting SKILL."
  (if (skill-path skill)
      (format nil "[$~A](skill://~A)"
              (skill-name skill)
              (namestring (skill-path skill)))
      (format nil "$~A" (skill-name skill))))

(defun make-skill-selector-item (skill &key include-enabled-marker)
  "Build one minibuffer item for SKILL."
  (let* ((enabled-p (skill-enabled-p skill))
         (marker (cond
                   ((not include-enabled-marker) "")
                   (enabled-p "[x] ")
                   (t "[ ] ")))
         (path (and (skill-path skill)
                    (namestring (skill-path skill))))
         (description (skill-display-description skill)))
    (list :skill skill
          :display (format nil "~A~A  [~(~A~)]~@[ ~A~]"
                           marker
                           (skill-name skill)
                           (or (skill-scope skill) :unknown)
                           description)
          :match-text (format nil "~A ~A ~A"
                              (skill-name skill)
                              description
                              (or path "")))))

(defun skill-selector-items (&key include-disabled include-enabled-marker)
  "Return minibuffer skill selector items."
  (mapcar (lambda (skill)
            (make-skill-selector-item
             skill
             :include-enabled-marker include-enabled-marker))
          (list-skills :include-disabled include-disabled)))

;;; --------------------------------------------------------------------------
;;; Automatic Skill Completion
;;; --------------------------------------------------------------------------

(defvar *automatic-skill-completion-enabled* t
  "When non-nil, typing $NAME in supported buffers opens skill completion.")

(defvar *skill-completion-enabled-buffer-kinds* '(:chat)
  "Buffer kinds where automatic skill completion is enabled.
Set this to T to enable automatic completion in every buffer kind, or NIL
to disable it without changing *AUTOMATIC-SKILL-COMPLETION-ENABLED*.")

(defvar *skill-completion-max-height* 12
  "Maximum rows used by automatic skill completion, including the prompt row.")

(defvar *skill-completion-active* nil
  "When non-nil, automatic skill completion candidates are visible.")

(defvar *skill-completion-buffer* nil
  "Buffer whose input currently owns automatic skill completion state.")

(defvar *skill-completion-query* ""
  "Current query text after the $ prefix.")

(defvar *skill-completion-token-start* 0
  "Start offset of the active $skill token on the current input line.")

(defvar *skill-completion-token-end* 0
  "End offset of the active $skill token on the current input line.")

(defvar *skill-completion-token-text* nil
  "Exact active $skill token text, including the leading $.")

(defvar *skill-completion-dismissed-token* nil
  "Exact token text dismissed by the user. Reopens after the token changes.")

(defvar *skill-completion-items* nil
  "All automatic skill completion candidate items.")

(defvar *skill-completion-filtered-items* nil
  "Automatic skill completion candidates matching *SKILL-COMPLETION-QUERY*.")

(defvar *skill-completion-match-positions* nil
  "Fuzzy match positions parallel to *SKILL-COMPLETION-FILTERED-ITEMS*.")

(defvar *skill-completion-selected-index* 0
  "Index of the selected automatic skill completion candidate.")

(defvar *skill-completion-scroll-offset* 0
  "First visible automatic skill completion candidate index.")

(defun skill-completion-buffer-kind-enabled-p (kind)
  "Return true when KIND is configured for automatic skill completion."
  (or (eq *skill-completion-enabled-buffer-kinds* t)
      (member kind *skill-completion-enabled-buffer-kinds* :test #'eq)))

(defun skill-completion-enabled-for-buffer-p (buffer)
  "Return true when automatic skill completion should scan BUFFER."
  (and *automatic-skill-completion-enabled*
       buffer
       (skill-completion-buffer-kind-enabled-p (buffer-kind buffer))
       (not *minibuffer-active*)
       (not *buffer-selector-active*)
       (not *model-selector-active*)
       (not *think-selector-active*)
       (not *customize-face-state*)
       (not *openai-oauth-pending*)
       (not *deny-message-mode*)
       (not (buffer-approval-pending buffer))))

(defun skill-completion-whitespace-char-p (char)
  "Return true when CHAR separates input tokens for automatic completion."
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun skill-completion-token-char-p (char)
  "Return true when CHAR can occur after $ in an automatic skill token."
  (mention-name-char-p char))

(defun current-skill-mention-token (message)
  "Return values QUERY START END TOKEN for the $token at MESSAGE point.
Returns NIL values when point is not inside a whitespace-delimited token that
starts with $ and contains only skill mention characters after it."
  (let* ((line (message-point-line message))
         (content (and line (line-content line)))
         (point (and content
                     (max 0 (min (message-point-offset message)
                                 (length content))))))
    (when content
      (let ((start point)
            (end point))
        (loop :while (and (plusp start)
                          (not (skill-completion-whitespace-char-p
                                (char content (1- start)))))
              :do (decf start))
        (loop :while (and (< end (length content))
                          (not (skill-completion-whitespace-char-p
                                (char content end))))
              :do (incf end))
        (when (and (< start end)
                   (char= (char content start) #\$)
                   (loop :for idx :from (1+ start) :below end
                         :always (skill-completion-token-char-p
                                  (char content idx))))
          (let ((token (subseq content start end)))
            (values (subseq token 1) start end token)))))))

(defun skill-completion-update-filter ()
  "Recompute automatic skill completion candidates for the active query."
  (let ((query *skill-completion-query*))
    (cond
      ((zerop (length query))
       (setf *skill-completion-filtered-items*
             (copy-list *skill-completion-items*)
             *skill-completion-match-positions*
             (make-list (length *skill-completion-items*)
                        :initial-element nil)))
      (t
       (let* ((matched (remove-if-not
                        (lambda (item)
                          (fuzzy-match-p query
                                         (minibuffer-item-match-text item)))
                        *skill-completion-items*))
              (scored (mapcar (lambda (item)
                                (cons (or (fuzzy-score
                                           query
                                           (minibuffer-item-match-text item))
                                          0)
                                      item))
                              matched))
              (sorted (stable-sort scored #'> :key #'car))
              (sorted-items (mapcar #'cdr sorted)))
         (setf *skill-completion-filtered-items* sorted-items
               *skill-completion-match-positions*
               (mapcar (lambda (item)
                         (fuzzy-match-positions
                          query
                          (minibuffer-item-match-text item)))
                       sorted-items))))))
  (setf *skill-completion-selected-index*
        (max 0 (min *skill-completion-selected-index*
                    (1- (max 1 (length *skill-completion-filtered-items*))))))
  (setf *skill-completion-scroll-offset* 0)
  (skill-completion-ensure-visible))

(defun skill-completion-visible-item-count ()
  "Return candidate rows visible in the automatic skill completion popup."
  (max 0
       (1- (min *skill-completion-max-height*
                (1+ (max 1 (length *skill-completion-filtered-items*)))))))

(defun skill-completion-ensure-visible ()
  "Adjust automatic skill completion scroll so the selection is visible."
  (let ((visible (skill-completion-visible-item-count)))
    (when (plusp visible)
      (when (< *skill-completion-selected-index*
               *skill-completion-scroll-offset*)
        (setf *skill-completion-scroll-offset*
              *skill-completion-selected-index*))
      (when (>= *skill-completion-selected-index*
                (+ *skill-completion-scroll-offset* visible))
        (setf *skill-completion-scroll-offset*
              (1+ (- *skill-completion-selected-index* visible)))))))

(defun skill-completion-next-item ()
  "Move automatic skill completion selection down one candidate."
  (when (< *skill-completion-selected-index*
           (1- (length *skill-completion-filtered-items*)))
    (incf *skill-completion-selected-index*)
    (skill-completion-ensure-visible)))

(defun skill-completion-prev-item ()
  "Move automatic skill completion selection up one candidate."
  (when (plusp *skill-completion-selected-index*)
    (decf *skill-completion-selected-index*)
    (skill-completion-ensure-visible)))

(defun deactivate-skill-completion (&key dismissed-token)
  "Hide automatic skill completion and optionally remember DISMISSED-TOKEN."
  (setf *skill-completion-active* nil
        *skill-completion-buffer* nil
        *skill-completion-query* ""
        *skill-completion-token-start* 0
        *skill-completion-token-end* 0
        *skill-completion-token-text* nil
        *skill-completion-items* nil
        *skill-completion-filtered-items* nil
        *skill-completion-match-positions* nil
        *skill-completion-selected-index* 0
        *skill-completion-scroll-offset* 0
        *skill-completion-dismissed-token* dismissed-token))

(defun sync-skill-completion (buffer)
  "Synchronize automatic skill completion state with BUFFER's current input."
  (unless (skill-completion-enabled-for-buffer-p buffer)
    (deactivate-skill-completion)
    (return-from sync-skill-completion nil))
  (multiple-value-bind (query start end token)
      (current-skill-mention-token (buffer-input-message buffer))
    (cond
      ((null token)
       (deactivate-skill-completion))
      ((and *skill-completion-dismissed-token*
            (string= token *skill-completion-dismissed-token*))
       (deactivate-skill-completion :dismissed-token token))
      (t
       (setf *skill-completion-dismissed-token* nil)
       (let ((items (skill-selector-items)))
         (if items
             (progn
               (setf *skill-completion-active* t
                     *skill-completion-buffer* buffer
                     *skill-completion-query* query
                     *skill-completion-token-start* start
                     *skill-completion-token-end* end
                     *skill-completion-token-text* token
                     *skill-completion-items* items)
               (skill-completion-update-filter))
             (deactivate-skill-completion)))))))

(defun insert-selected-skill-completion (buffer)
  "Replace the active $token in BUFFER with the selected skill mention."
  (let ((item (when (plusp (length *skill-completion-filtered-items*))
                (nth *skill-completion-selected-index*
                     *skill-completion-filtered-items*))))
    (unless item
      (deactivate-skill-completion)
      (return-from insert-selected-skill-completion nil))
    (multiple-value-bind (query start end token)
        (current-skill-mention-token (buffer-input-message buffer))
      (declare (ignore query token))
      (if (null start)
          (deactivate-skill-completion)
          (let* ((message (buffer-input-message buffer))
                 (line (message-point-line message))
                 (content (line-content line))
                 (mention (skill-mention-text (getf item :skill)))
                 (inserted (concatenate 'string mention " "))
                 (replacement (concatenate 'string
                                           (subseq content 0 start)
                                           inserted
                                           (subseq content end))))
            (setf (line-content line) replacement
                  (message-point-offset message) (+ start (length inserted)))
            (mark-buffer-dirty buffer)
            (deactivate-skill-completion)
            t)))))

(defun skill-completion-base-key (key)
  "Return KEY without simple prefix wrappers used by completion handlers."
  (if (and (listp key) (= (length key) 2)
           (member (first key) '(:meta :alt :ctrl-x :ctrl-c)))
      (second key)
      key))

(defun handle-skill-completion-key (buffer key)
  "Handle KEY for the automatic skill completion popup.
Returns true when KEY was consumed by completion."
  (unless (eq buffer *skill-completion-buffer*)
    (deactivate-skill-completion)
    (return-from handle-skill-completion-key nil))
  (let ((base-key (skill-completion-base-key key)))
    (cond
      ((or (eq base-key :escape)
           (and (characterp base-key)
                (or (char= base-key #\Esc)
                    (char= base-key (code-char 7)))))
       (deactivate-skill-completion
        :dismissed-token *skill-completion-token-text*)
       t)
      ((and (characterp base-key)
            (or (char= base-key #\Return)
                (char= base-key #\Newline)
                (char= base-key #\Tab)))
       (insert-selected-skill-completion buffer)
       t)
      ((eq base-key :tab)
       (insert-selected-skill-completion buffer)
       t)
      ((or (eq base-key :down)
           (and (characterp base-key) (char= base-key (code-char 14))))
       (skill-completion-next-item)
       t)
      ((or (eq base-key :up)
           (and (characterp base-key) (char= base-key (code-char 16))))
       (skill-completion-prev-item)
       t)
      (t nil))))

(defun minibuffer-insert-skill-command (buffer)
  "Select a skill and insert an exact $skill mention into the input."
  (let ((items (skill-selector-items)))
    (if items
        (minibuffer-activate
         "Insert Skill" items
         (lambda (item)
           (message-insert-string
            (buffer-input-message buffer)
            (skill-mention-text (getf item :skill)))
           (mark-buffer-dirty buffer)))
        (buffer-insert-system-message buffer "[No enabled skills available.]"))))
(defcommand minibuffer-insert-skill-command)

(defun minibuffer-toggle-skill-command (buffer)
  "Select a skill and toggle whether it is enabled."
  (let ((items (skill-selector-items :include-disabled t
                                     :include-enabled-marker t)))
    (if items
        (minibuffer-activate
         "Toggle Skill" items
         (lambda (item)
           (let* ((skill (getf item :skill))
                  (enabled-p (not (skill-enabled-p skill))))
             (handler-case
                 (progn
                   (set-skill-enabled skill enabled-p)
                   (buffer-insert-system-message
                    buffer
                    (format nil "[Skill ~A ~A]"
                            (skill-name skill)
                            (if enabled-p "enabled" "disabled"))))
               (error (e)
                  (buffer-insert-system-message
                   buffer
                   (format nil "[Skill toggle failed: ~A]" e)))))))
        (buffer-insert-system-message buffer "[No skills available.]"))))
(defcommand minibuffer-toggle-skill-command)

(defun list-skills-command (buffer)
  "Open a help buffer listing loaded skills and skill load errors."
  (declare (ignore buffer))
  (reload-skills)
  (let* ((buf-name "*help:skills*")
         (existing (find-buffer-by-name buf-name))
         (content (list-skills-to-string :include-disabled t)))
    (if existing
        (progn
          (set-message-text (message-prev (buffer-input-message existing))
                            content)
          (switch-to-buffer existing))
        (switch-to-buffer (make-help-buffer buf-name content)))))
(defcommand list-skills-command)

;;; --------------------------------------------------------------------------
;;; Package Commands
;;; --------------------------------------------------------------------------

(defun make-package-selector-item (definition buffer)
  "Build one minibuffer item for package DEFINITION."
  (let* ((name (package-definition-name definition))
         (scope (package-enablement-scope name :buffer buffer))
         (description (package-display-description definition))
         (display (format nil "[~A] ~A - ~A"
                          (package-scope-label scope)
                          name
                          description)))
    (list :package definition
          :package-name name
          :scope scope
          :display display
          :match-text (format nil "~A ~A ~A"
                              name
                              (package-scope-label scope)
                              description))))

(defun installed-package-selector-items (buffer)
  "Return installed packages as minibuffer selector items."
  (mapcar (lambda (definition)
            (make-package-selector-item definition buffer))
          (sort (copy-list (list-installed-packages))
                #'string<
                :key #'package-definition-name)))

(defun select-package-selector-item (package-name)
  "Select PACKAGE-NAME in the active minibuffer when present."
  (let ((index (position package-name *minibuffer-filtered-items*
                         :key (lambda (item)
                                (getf item :package-name))
                         :test #'string=)))
    (when index
      (setf *minibuffer-selected-index* index)
      (minibuffer-ensure-visible))))

(defun activate-package-toggle-minibuffer (buffer &optional selected-package-name)
  "Open the installed package enablement selector."
  (let ((items (installed-package-selector-items buffer)))
    (if items
        (progn
          (minibuffer-activate
           "Enable Package" items
           (lambda (item)
             (let* ((name (getf item :package-name))
                    (definition (getf item :package))
                    (previous-scope
                      (package-enablement-scope name :buffer buffer))
                    (had-context-p
                      (buffer-has-conversation-context-p buffer))
                    (scope (cycle-package-enablement-scope name :buffer buffer)))
               (load-active-packages :buffer buffer)
               (maybe-insert-enabled-package-context
                buffer definition previous-scope scope had-context-p)
               (buffer-insert-system-message
                buffer
                (format nil "[Package ~A ~A]"
                        name
                        (package-scope-message scope)))
               (activate-package-toggle-minibuffer buffer name))))
          (when selected-package-name
            (select-package-selector-item selected-package-name)))
        (buffer-insert-system-message buffer "[No installed packages available.]"))))

(defun minibuffer-toggle-package-command (buffer)
  "Select an installed package and cycle its enablement scope."
  (reload-package-channels)
  (activate-package-toggle-minibuffer buffer))
(defcommand minibuffer-toggle-package-command)

(defun describe-installed-package-command (buffer)
  "Select an installed package and open its help buffer."
  (reload-package-channels)
  (let ((items (installed-package-selector-items buffer)))
    (if items
        (minibuffer-activate
         "Describe Package" items
         (lambda (item)
           (let* ((definition (getf item :package))
                  (name (package-definition-name definition))
                  (content (describe-installed-package-to-string
                            definition buffer))
                  (buf-name (format nil "*help:package:~A*" name))
                  (existing (find-buffer-by-name buf-name)))
             (if existing
                 (progn
                   (set-message-text (message-prev (buffer-input-message existing))
                                     content)
                   (switch-to-buffer existing))
                 (switch-to-buffer (make-help-buffer buf-name content))))))
        (buffer-insert-system-message buffer "[No installed packages available.]"))))
(defcommand describe-installed-package-command)

;;; --------------------------------------------------------------------------
;;; Model Selection Commands
;;; --------------------------------------------------------------------------

(defun model-selector-display (provider model)
  "Return the display string used for model selection history and UI."
  (format nil "~(~A~)/~A" provider model))

(defun build-model-selector-items (entries)
  "Convert selector ENTRIES into minibuffer items with display strings."
  (mapcar (lambda (entry)
            (let ((provider (getf entry :provider))
                  (model (getf entry :model)))
              (list :provider provider
                    :model model
                    :active-p (getf entry :active-p)
                    :display (model-selector-display provider model))))
          entries))

(defun model-selection-status-suffix (think-status think-level)
  "Return a short status suffix describing the resulting think level."
  (case think-status
    (:kept
     (format nil "; kept think ~A" think-level))
    (:reset
     "; think reset to default")
    (t
     (if think-level
         (format nil "; think ~A" think-level)
         "; think default"))))

(defun apply-buffer-model-selection (buffer provider model)
  "Apply PROVIDER and MODEL to BUFFER, reconcile think level, and report status."
  (set-buffer-provider-override buffer provider)
  (set-buffer-model-override buffer model)
  (multiple-value-bind (think-status think-level)
      (reconcile-buffer-think-level-override buffer
                                             :provider provider
                                             :model model)
    (when (buffer-session buffer)
      (record-session-model-change (buffer-session buffer)
                                   provider
                                   model
                                   :think-level think-level))
    (values think-status think-level)))

(defun record-model-selection-history (display)
  "Record DISPLAY as the most recently selected model."
  (setf *model-selection-history*
        (cons display
              (remove display *model-selection-history* :test #'string=))))

(defun insert-model-selection-message (buffer provider model think-status think-level)
  "Insert a confirmation message for a model selection."
  (buffer-insert-system-message
   buffer
   (format nil "[Model changed to ~A~A]"
           (model-selector-display provider model)
           (model-selection-status-suffix think-status think-level))))

(defun available-think-levels-for-selector (buffer)
  "Build think-level selector entries for BUFFER's active model."
  (multiple-value-bind (provider model current-think)
      (handler-case (resolve-buffer-provider-and-model buffer)
        (error () (values nil nil nil)))
    (let ((levels (and provider model
                       (provider-model-supported-think-levels provider model))))
      (when levels
        (cons (list :provider provider
                    :model model
                    :level nil
                    :default-p t
                    :active-p (null current-think)
                    :display "default")
              (mapcar (lambda (level)
                        (list :provider provider
                              :model model
                              :level level
                              :default-p nil
                              :active-p (and current-think
                                             (string= level current-think))
                              :display level))
                      levels))))))

(defun insert-think-selection-message (buffer provider model think-level)
  "Insert a confirmation message for a think-level selection."
  (buffer-insert-system-message
   buffer
   (if think-level
       (format nil "[Think level set to ~A for ~A]"
               think-level
               (model-selector-display provider model))
       (format nil "[Think level reset to default for ~A]"
               (model-selector-display provider model)))))

(defun apply-buffer-think-level-selection (buffer entry)
  "Apply think-level ENTRY to BUFFER and insert a confirmation message."
  (let ((provider (getf entry :provider))
        (model (getf entry :model))
        (level (getf entry :level)))
    (if level
        (set-buffer-think-level-override buffer level)
        (clear-buffer-think-level-override buffer))
    (when (buffer-session buffer)
      (record-session-think-level-change (buffer-session buffer) level))
    (insert-think-selection-message buffer provider model level)))

(defun preselect-minibuffer-active-item (items)
  "Move the minibuffer selection to the active item in ITEMS when present."
  (let ((active-idx (position-if (lambda (item) (getf item :active-p)) items)))
    (when active-idx
      (setf *minibuffer-selected-index* active-idx)
      (minibuffer-ensure-visible))))

(defun resolve-agent-display-config (agent-name)
  "Return AGENT-NAME's effective provider, model, and think level for UI display."
  (let ((buf (make-buffer "agent-config-preview" :agent-name agent-name)))
    (resolve-buffer-provider-and-model buf)))

(defun format-agent-selection-message (agent-name)
  "Return a confirmation message after switching to AGENT-NAME."
  (handler-case
      (multiple-value-bind (provider model think-level)
          (resolve-agent-display-config agent-name)
        (format nil "[Agent changed to ~A (~(~A~)/~A~@[; think ~A~])]"
                agent-name provider model think-level))
    (error ()
      (format nil "[Agent changed to ~A]" agent-name))))

(defun switch-buffer-to-agent (buffer agent-name)
  "Switch BUFFER to AGENT-NAME, clear overrides, ensure faces, and confirm."
  (normalize-agent-name-key agent-name)
  (let* ((definition (find-agent-definition agent-name))
         (resolved-name (if definition
                            (agent-definition-name definition)
                            (string-trim '(#\Space #\Tab #\Newline #\Return) agent-name))))
    (setf (buffer-agent-name buffer) resolved-name)
    (clear-buffer-routing-overrides buffer)
    (ensure-buffer-agent-face-set buffer resolved-name)
    (buffer-insert-system-message buffer (format-agent-selection-message resolved-name))
    buffer))

(defun make-agent-selector-item (agent-name active-agent-name)
  "Build one minibuffer item for AGENT-NAME."
  (let ((active-p (string= agent-name active-agent-name)))
    (handler-case
        (multiple-value-bind (provider model think-level)
            (resolve-agent-display-config agent-name)
          (list :agent-name agent-name
                :active-p active-p
                :display (format nil "~A ~A  [~(~A~)/~A~@[ think:~A~]]"
                                 (if active-p "*" " ")
                                 agent-name
                                 provider
                                 model
                                 think-level)
                :match-text (format nil "~A ~(~A~) ~A~@[ ~A~]"
                                    agent-name provider model think-level)))
      (error ()
        (list :agent-name agent-name
              :active-p active-p
              :display (format nil "~A ~A" (if active-p "*" " ") agent-name)
              :match-text agent-name)))))

(defun sort-agent-selector-items (items)
  "Sort agent selector ITEMS with the active agent first, then alphabetically."
  (stable-sort (copy-list items)
               (lambda (a b)
                 (cond
                   ((and (getf a :active-p) (not (getf b :active-p))) t)
                   ((and (getf b :active-p) (not (getf a :active-p))) nil)
                   (t (string< (getf a :agent-name)
                               (getf b :agent-name)))))))

(defun minibuffer-select-agent-command (buffer)
  "Open the minibuffer agent selector for the current buffer."
  (let* ((active-agent (buffer-agent-name buffer))
         (known-agents (list-known-agent-names))
         (items (sort-agent-selector-items
                 (mapcar (lambda (agent-name)
                           (make-agent-selector-item agent-name active-agent))
                         known-agents))))
    (cond
      ((null items)
       (buffer-insert-system-message buffer "[No known agents available.]"))
      (t
       (minibuffer-activate
        "Select Agent" items
        (lambda (item)
          (switch-buffer-to-agent buffer (getf item :agent-name))))
       (preselect-minibuffer-active-item items)))))
(defcommand minibuffer-select-agent-command)

(defun select-model-command (buffer)
  "Open the model selector to change the LLM model for this session.
Builds the available model list based on configured API keys."
  (let ((entries (available-models-for-selector buffer)))
    (cond
      ((null entries)
       (buffer-insert-system-message
        buffer "[No API keys configured. Cannot list models.]"))
      (t
       ;; Pre-select the currently active model (if found)
       (let ((active-idx (position-if (lambda (e) (getf e :active-p)) entries)))
         (setf *model-selector-entries* entries
               *model-selector-active* t
               *model-selector-index* (or active-idx 0)
               *model-selector-scroll* 0))))))
(defcommand select-model-command)

(defun minibuffer-select-model-command (buffer)
  "Open the minibuffer model selector with fuzzy search (helm/ivy/vertico style).
Activates the minibuffer with all available models as candidates, sorted by
recency then alphabetically. The user can type to fuzzy-filter and use C-n/C-p
to navigate."
  (let ((entries (available-models-for-selector buffer)))
    (cond
      ((null entries)
       (buffer-insert-system-message
        buffer "[No API keys configured. Cannot list models.]"))
      (t
       (let* ((items (build-model-selector-items entries))
              ;; Sort: by recency (from history), then active, then alphabetical
              (sorted (sort-models-by-recency items)))
         (minibuffer-activate
          "Select Model" sorted
          (lambda (item)
            (let ((provider (getf item :provider))
                  (model (getf item :model)))
              (multiple-value-bind (think-status think-level)
                  (apply-buffer-model-selection buffer provider model)
                (record-model-selection-history (getf item :display))
                (insert-model-selection-message buffer
                                                provider
                                                model
                                                think-status
                                                think-level))))))))))
(defcommand minibuffer-select-model-command)

(defun select-think-level-command (buffer)
  "Open the think-level selector for the active model."
  (let ((entries (available-think-levels-for-selector buffer)))
    (cond
      ((null entries)
       (multiple-value-bind (provider model)
           (handler-case (resolve-buffer-provider-and-model buffer)
             (error () (values nil nil)))
         (buffer-insert-system-message
          buffer
          (if (and provider model)
              (format nil "[Think levels not available for ~A.]"
                      (model-selector-display provider model))
              "[Think levels are not available for the active model.]"))))
      (t
       (let ((active-idx (position-if (lambda (entry) (getf entry :active-p))
                                      entries)))
         (setf *think-selector-entries* entries
               *think-selector-active* t
               *think-selector-index* (or active-idx 0)
               *think-selector-scroll* 0))))))
(defcommand select-think-level-command)

(defun minibuffer-select-think-level-command (buffer)
  "Open the minibuffer think-level selector for the active model."
  (let ((entries (available-think-levels-for-selector buffer)))
    (cond
      ((null entries)
       (multiple-value-bind (provider model)
           (handler-case (resolve-buffer-provider-and-model buffer)
             (error () (values nil nil)))
         (buffer-insert-system-message
          buffer
          (if (and provider model)
              (format nil "[Think levels not available for ~A.]"
                      (model-selector-display provider model))
              "[Think levels are not available for the active model.]"))))
      (t
       (minibuffer-activate
        "Select Think Level" entries
        (lambda (item)
          (apply-buffer-think-level-selection buffer item)))
       (preselect-minibuffer-active-item entries)))))
(defcommand minibuffer-select-think-level-command)

;;; --------------------------------------------------------------------------
;;; Buffer Management Commands (continued)
;;; --------------------------------------------------------------------------

(defun minibuffer-select-buffer-command (buffer)
  "Open the minibuffer buffer selector with fuzzy search (helm/ivy/vertico style).
Activates the minibuffer with all open buffers as candidates, sorted by
recency then alphabetically. The user can type to fuzzy-filter and use C-n/C-p
to navigate. Shows buffer name, agent, status, and message count."
  (declare (ignore buffer))
  (let* ((current (current-buffer))
         (items (mapcar (lambda (buf)
                          (let* ((name (buffer-name buf))
                                 (agent (buffer-agent-name buf))
                                 (status (string-downcase
                                          (symbol-name (buffer-status buf))))
                                 (msgs (max 0 (1- (buffer-message-count buf))))
                                 (current-p (eq buf current))
                                 (marker (if current-p "*" " "))
                                 (display (format nil "~A ~A  [~A] ~A  msgs:~D"
                                                  marker name agent status msgs)))
                            (list :buffer buf
                                  :name name
                                  :current-p current-p
                                  :display display)))
                        *buffer-ring*))
         ;; Sort: by recency (from history), then current buffer, then alphabetical
         (sorted (sort-buffers-by-recency items)))
    (minibuffer-activate
     "Switch Buffer" sorted
     (lambda (item)
       (let ((selected-buf (getf item :buffer))
             (name (getf item :name)))
         (when selected-buf
           (switch-to-buffer selected-buf)
           ;; Record in history for recency sorting
           (setf *buffer-selection-history*
                 (cons name
                       (remove name *buffer-selection-history*
                               :test #'string=)))))))))
(defcommand minibuffer-select-buffer-command)

(defun new-buffer-command (buffer)
  "Create a new chat buffer and switch to it."
  (declare (ignore buffer))
  (let ((new-buf (make-chat-buffer (next-buffer-name)
                                   :agent-name *default-agent-name*
                                   :working-directory (truename ".")
                                   :add-to-ring-p t)))
    (autosave-session-snapshot new-buf)
    (switch-to-buffer new-buf)))
(defcommand new-buffer-command)

(defun next-buffer-command (buffer)
  "Switch to the next buffer in the ring."
  (declare (ignore buffer))
  (when (cdr *buffer-ring*)
    ;; Rotate: move first to end
    (let ((current (pop *buffer-ring*)))
      (setf *buffer-ring* (append *buffer-ring* (list current))))))
(defcommand next-buffer-command)

(defun kill-buffer-command (buffer)
  "Kill the current buffer. Switches to the next buffer in the ring."
  (declare (ignore buffer))
  (when (cdr *buffer-ring*)  ; Don't kill the last buffer
    (kill-buffer-from-ring (current-buffer))))
(defcommand kill-buffer-command)

;;; --------------------------------------------------------------------------
;;; Session Commands
;;; --------------------------------------------------------------------------

(defun save-session-command (buffer)
  "Save the current buffer's persistent state."
  (cond
    ((file-buffer-p buffer)
     (let ((summary (project-save-buffer buffer)))
       (buffer-insert-system-message
        buffer
        (format nil "[Saved ~A:~A]"
                (getf summary :project)
                (getf summary :path)))))
    ((scratch-buffer-p buffer)
     (buffer-insert-system-message
      buffer
      "[Scratch buffer is not saved; it lasts only until Clawmacs exits.]"))
    (t
     (let ((path (save-session buffer)))
       ;; Insert a system message confirming the save
       (buffer-insert-system-message
        buffer
        (format nil "[Session saved to ~A]" path))))))
(defcommand save-session-command)

(defun apply-session-branch-to-buffer
    (buffer leaf-id &key input-text (autosave-p t))
  "Move BUFFER's session to LEAF-ID and display that branch."
  (let ((session (or (buffer-session buffer)
                     (ensure-buffer-session buffer))))
    (set-session-current-leaf session leaf-id)
    (multiple-value-bind (provider model think-level)
        (session-branch-state session leaf-id)
      (setf (buffer-provider-override buffer) provider
            (buffer-model-override buffer) model
            (buffer-think-level-override buffer) think-level))
    (replace-buffer-history-with-serialized-messages
     buffer
     (session-active-branch-message-events session leaf-id)
     :input-text input-text
     :autosave-p autosave-p)
    buffer))

(defun session-branch-summary-source-text (buffer)
  "Return a text transcript of BUFFER's current visible branch."
  (with-output-to-string (out)
    (loop :for msg := (buffer-first-message buffer) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buffer))))
          :do (format out "~(~A~)> ~A~%~%"
                      (message-sender msg)
                      (message-text msg)))))

(defun generate-session-branch-summary (buffer &key custom-instructions)
  "Generate a branch summary for BUFFER using its active provider."
  (let* ((source (session-branch-summary-source-text buffer))
         (instructions
           (if (session-tree-blank-string-p custom-instructions)
               "Summarize this abandoned conversation branch. Preserve decisions, facts, files touched, and unresolved work. Keep it concise and useful as future context."
               custom-instructions))
         (prompt (format nil "~A~%~%Branch transcript:~%~A"
                         instructions source)))
    (multiple-value-bind (provider model think-level)
        (resolve-buffer-provider-and-model buffer)
      (let* ((state (provider-request-streaming
                     provider
                     (list `((:role . "user")
                             (:content . ,(coerce
                                           (canonicalize-message-content
                                            "user"
                                            prompt)
                                           'vector))))
                     (lambda (s) (declare (ignore s)))
                     :model model
                     :tools #()
                     :system-prompt "You summarize abandoned chat branches for future context."
                     :reasoning-effort think-level))
             (response (wait-for-compaction-stream-state state))
             (summary (content-text-blocks (response-content response))))
        (when (session-tree-blank-string-p summary)
          (error "Branch summary provider returned an empty summary"))
        summary))))

(defun complete-session-tree-navigation
    (buffer entry-id leaf-id input-text &key summarize custom-instructions)
  "Finish navigation to ENTRY-ID in BUFFER."
  (let* ((session (buffer-session buffer))
         (target-leaf leaf-id))
    (when summarize
      (let ((summary (generate-session-branch-summary
                      buffer
                      :custom-instructions custom-instructions)))
        (setf target-leaf
              (record-session-branch-summary session leaf-id summary))))
    (apply-session-branch-to-buffer buffer target-leaf :input-text input-text)
    (buffer-insert-system-message
     buffer
     (format nil "[Navigated session tree to ~A]" entry-id)
     :record-p nil)))

(defun session-tree-summary-choice-items ()
  "Return minibuffer choices for branch navigation summaries."
  (list (list :choice :none
              :display "No summary"
              :match-text "none no summary")
        (list :choice :summary
              :display "Summarize abandoned branch"
              :match-text "summarize abandoned branch")
        (list :choice :custom
              :display "Summarize with custom prompt"
              :match-text "summarize custom prompt")))

(defun prompt-session-tree-summary-choice (buffer entry-id leaf-id input-text)
  "Ask how to summarize before navigating the session tree."
  (minibuffer-activate
   "Branch Summary"
   (session-tree-summary-choice-items)
   (lambda (item)
     (case (getf item :choice)
       (:none
        (complete-session-tree-navigation
         buffer entry-id leaf-id input-text))
       (:summary
        (handler-case
            (complete-session-tree-navigation
             buffer entry-id leaf-id input-text :summarize t)
          (error (e)
            (buffer-insert-system-message
             buffer
             (format nil "[Branch summary failed: ~A]" e)
             :record-p nil))))
       (:custom
        (minibuffer-prompt
         "Summary Prompt"
         (lambda (custom)
           (handler-case
               (complete-session-tree-navigation
                buffer entry-id leaf-id input-text
                :summarize t
                :custom-instructions custom)
             (error (e)
               (buffer-insert-system-message
                buffer
                (format nil "[Branch summary failed: ~A]" e)
                :record-p nil))))))))))

(defun session-tree-navigation-needs-summary-p (session leaf-id)
  "Return true when moving to LEAF-ID abandons the current leaf."
  (let ((current (session-effective-leaf-id session)))
    (and current
         (not (and leaf-id (string= current leaf-id))))))

(defun navigate-session-tree-item (buffer item)
  "Navigate BUFFER's session according to selected tree ITEM."
  (let* ((session (buffer-session buffer))
         (entry-id (getf item :id))
         (leaf-id (session-navigation-leaf-for-entry session entry-id))
         (input-text (session-entry-user-message-text session entry-id)))
    (if (session-tree-navigation-needs-summary-p session leaf-id)
        (prompt-session-tree-summary-choice buffer entry-id leaf-id input-text)
        (complete-session-tree-navigation buffer entry-id leaf-id input-text))))

(defun edit-session-tree-label (buffer item)
  "Prompt for a label for selected session tree ITEM."
  (let ((entry-id (getf item :id))
        (current-label (or (getf item :label) "")))
    (minibuffer-prompt
     "Entry Label"
     (lambda (label)
       (record-session-label-change (buffer-session buffer) entry-id label)
       (buffer-insert-system-message
        buffer
        (if (session-tree-blank-string-p label)
            (format nil "[Cleared label on ~A]" entry-id)
            (format nil "[Labeled ~A: ~A]" entry-id label))
        :record-p nil)
       (session-tree-selector-activate
        buffer
        (lambda (selected)
          (navigate-session-tree-item buffer selected))
        :label-callback
        (lambda (selected)
          (edit-session-tree-label buffer selected))
        :initial-entry-id entry-id))
     :initial-input current-label)))

(defun session-tree-command (buffer)
  "Open the current session's tree selector."
  (let ((session (ensure-buffer-session buffer)))
    (if (null (session-normalized-tree-events session))
        (buffer-insert-system-message
         buffer
         "[Current session has no tree entries yet.]"
         :record-p nil)
        (session-tree-selector-activate
         buffer
         (lambda (item)
           (navigate-session-tree-item buffer item))
         :label-callback
         (lambda (item)
           (edit-session-tree-label buffer item))))))
(defcommand session-tree-command)

(defun fork-session-from-tree-item (buffer item)
  "Fork BUFFER's session from selected tree ITEM into a new buffer."
  (let* ((session (buffer-session buffer))
         (entry-id (getf item :id))
         (leaf-id (session-navigation-leaf-for-entry session entry-id))
         (input-text (session-entry-user-message-text session entry-id))
         (new-session (create-branched-session session leaf-id))
         (new-buffer (make-buffer (session-name new-session)
                                  :agent-name (buffer-agent-name buffer)
                                  :working-directory (buffer-working-directory buffer)
                                  :session new-session)))
    (multiple-value-bind (provider model think-level)
        (session-branch-state new-session leaf-id)
      (setf (buffer-provider-override new-buffer) provider
            (buffer-model-override new-buffer) model
            (buffer-think-level-override new-buffer) think-level))
    (initialize-buffer-display-defaults new-buffer)
    (replace-buffer-history-with-serialized-messages
     new-buffer
     (session-active-branch-message-events new-session leaf-id)
     :input-text input-text)
    (add-buffer-to-ring new-buffer)
    (switch-to-buffer new-buffer)
    (buffer-insert-system-message
     new-buffer
     (format nil "[Forked from ~A]" entry-id)
     :record-p nil)))

(defun fork-session-command (buffer)
  "Fork a selected session tree point into a new session buffer."
  (let ((session (ensure-buffer-session buffer)))
    (if (null (session-normalized-tree-events session))
        (buffer-insert-system-message
         buffer
         "[Current session has no tree entries to fork.]"
         :record-p nil)
        (session-tree-selector-activate
         buffer
         (lambda (item)
           (fork-session-from-tree-item buffer item))
         :label-callback
         (lambda (item)
           (edit-session-tree-label buffer item))))))
(defcommand fork-session-command)

(defun load-session-command (buffer)
  "Load a saved chat session into a new buffer via minibuffer completion."
  (labels ((record-selection (name)
             (setf *buffer-selection-history*
                   (cons name
                         (remove name *buffer-selection-history*
                                 :test #'string=))))
           (unique-loaded-buffer-name (base-name)
             (if (null (find-buffer-by-name base-name))
                 base-name
                 (loop :for suffix :from 2
                       :for candidate := (format nil "~A<~D>" base-name suffix)
                       :unless (find-buffer-by-name candidate)
                         :return candidate))))
    (let* ((session-names (sort (copy-list (or (list-saved-sessions) nil))
                                #'string<))
           (items (mapcar (lambda (session-name)
                            (let ((open-p (not (null (find-buffer-by-name
                                                      session-name)))))
                              (list :session-name session-name
                                    :open-p open-p
                                    :display (if open-p
                                                 (format nil "~A  [open]"
                                                         session-name)
                                                 session-name)
                                    :match-text session-name)))
                          session-names)))
      (if items
          (minibuffer-activate
           "Load Session"
           items
           (lambda (item)
             (let ((session-name (getf item :session-name)))
               (handler-case
                   (let ((loaded (load-session session-name)))
                     (if loaded
                         (progn
                           (setf (buffer-name loaded)
                                 (unique-loaded-buffer-name
                                  (buffer-name loaded)))
                           (initialize-buffer-display-defaults loaded)
                           (add-buffer-to-ring loaded)
                           (switch-to-buffer loaded)
                           (record-selection (buffer-name loaded)))
                         (buffer-insert-system-message
                          buffer
                          (format nil
                                  "[Saved session ~A is no longer available.]"
                                  session-name))))
                 (error (e)
                   (buffer-insert-system-message
                    buffer
                    (format nil "[Load session failed: ~A]" e)))))))
          (buffer-insert-system-message
           buffer
           "[No saved sessions available.]")))))
(defcommand load-session-command)

(defun execute-extended-command (buffer)
  "Select and run a command via the minibuffer. Bound to M-x."
  (let ((items (make-command-selector-items :buffer buffer)))
    (if (null items)
        (buffer-insert-system-message buffer "[No commands available]")
        (minibuffer-activate
         "M-x"
         items
         (lambda (item)
           (invoke-command buffer (getf item :command)))))))
(defcommand execute-extended-command :keys ((:meta #\x)))

;;; --------------------------------------------------------------------------
;;; Display Toggle Commands
;;; --------------------------------------------------------------------------

(defun toggle-tool-results-command (buffer)
  "Toggle visibility of tool-result messages in the chat."
  (setf (buffer-show-tool-results-p buffer)
        (not (buffer-show-tool-results-p buffer)))
  (notify-buffer-display-change buffer :visibility))
(defcommand toggle-tool-results-command)

(defun toggle-reasoning-output-command (buffer)
  "Toggle visibility of provider-supplied reasoning blocks in the chat."
  (setf (buffer-show-reasoning-p buffer)
        (not (buffer-show-reasoning-p buffer)))
  (notify-buffer-display-change buffer :visibility))
(defcommand toggle-reasoning-output-command)

(defun toggle-metadata-output-command (buffer)
  "Toggle visibility of provider/response metadata in the chat."
  (setf (buffer-show-metadata-p buffer)
        (not (buffer-show-metadata-p buffer)))
  (notify-buffer-display-change buffer :visibility))
(defcommand toggle-metadata-output-command)

(defun toggle-debug-mode-command (buffer)
  "Toggle API debug mode on/off. When enabled, every outgoing API request
(provider, model, full messages/tools JSON) and every completed response
(stop-reason, content blocks) is echoed into the chat window as a debug
message, rendered in magenta so it stands out from normal system output.
Bound to C-c C-d."
  (setf *debug-mode* (not *debug-mode*))
  (buffer-insert-system-message
   buffer
   (if *debug-mode*
       "[Debug mode ON — API calls will be shown in chat]"
       "[Debug mode OFF]")))
(defcommand toggle-debug-mode-command)

(defun redraw-screen-command (buffer)
  "Request a full screen redraw. Bound to C-l."
  (declare (ignore buffer))
  :redraw)
(defcommand redraw-screen-command)

;;; --------------------------------------------------------------------------
;;; Customize Face
;;; --------------------------------------------------------------------------

(defvar *customize-face-state* nil
  "When non-nil, a plist describing the face customization session:
  :face — the face object being customized
  :label — display label (e.g. \"user:default\")
  :field-index — 0-5 (which field is selected)
  :original-values — alist of original attribute values for revert
  :buffer — the customize buffer")

(defvar *customize-face-fields*
  '(:foreground :background :bold-p :underline-p :reverse-p :parent)
  "List of face attribute field keywords in display order for customize-face.")

(defun cga-color-name (value)
  "Return a human-readable name for a CGA color value (0-15)."
  (case value
    (0 "black") (1 "red") (2 "green") (3 "yellow")
    (4 "blue") (5 "magenta") (6 "cyan") (7 "white")
    (8 "bright-black") (9 "bright-red") (10 "bright-green") (11 "bright-yellow")
    (12 "bright-blue") (13 "bright-magenta") (14 "bright-cyan") (15 "bright-white")
    (t (format nil "~D" value))))

(defun format-color-spec-display (cs)
  "Format a color-spec for human-readable display."
  (if (null cs)
      "(inherit)"
      (ecase (color-spec-type cs)
        (:cga (format nil "~A (CGA ~D)" (cga-color-name (color-spec-value cs))
                       (color-spec-value cs)))
        (:256 (format nil "color-~D (256)" (color-spec-value cs)))
        (:hex (format nil "~A (hex)" (color-spec-value cs))))))

(defun format-boolean-display (val)
  "Format a boolean face attribute for display.
NIL means inherit from parent, T means yes."
  (if val "yes" "(inherit)"))

(defun format-face-parent-display (parent-face)
  "Format a face's parent for display."
  (if (null parent-face)
      "(none)"
      (format nil "~(~A~)" (face-name parent-face))))

(defun customize-face-field-value (face field)
  "Get the current value of FIELD on FACE."
  (ecase field
    (:foreground (face-foreground face))
    (:background (face-background face))
    (:bold-p (slot-value face 'bold-p))
    (:underline-p (slot-value face 'underline-p))
    (:reverse-p (slot-value face 'reverse-p))
    (:parent (face-parent face))))

(defun customize-face-set-field-value (face field value)
  "Set the value of FIELD on FACE to VALUE."
  (ecase field
    (:foreground (setf (face-foreground face) value))
    (:background (setf (face-background face) value))
    (:bold-p (setf (face-bold-p face) value))
    (:underline-p (setf (face-underline-p face) value))
    (:reverse-p (setf (face-reverse-p face) value))
    (:parent (setf (face-parent face) value))))

(defun customize-face-field-label (field)
  "Return the human-readable label for a face attribute field keyword."
  (ecase field
    (:foreground "Foreground")
    (:background "Background")
    (:bold-p "Bold")
    (:underline-p "Underline")
    (:reverse-p "Reverse")
    (:parent "Parent")))

(defun customize-face-field-display (face field)
  "Return the display string for FIELD's current value on FACE."
  (ecase field
    ((:foreground :background)
     (format-color-spec-display (customize-face-field-value face field)))
    ((:bold-p :underline-p :reverse-p)
     (format-boolean-display (customize-face-field-value face field)))
    (:parent
     (format-face-parent-display (customize-face-field-value face field)))))

(defun customize-face-snapshot (face)
  "Take a snapshot of FACE's current attribute values for revert.
Returns an alist of (field . value) pairs."
  (mapcar (lambda (field)
            (cons field (customize-face-field-value face field)))
          *customize-face-fields*))

(defun customize-face-restore-snapshot (face snapshot)
  "Restore FACE's attributes from SNAPSHOT (an alist from customize-face-snapshot)."
  (dolist (entry snapshot)
    (customize-face-set-field-value face (car entry) (cdr entry))))

(defun build-customize-face-content (face label field-index)
  "Build the text content for a customize-face buffer.
FACE is the face being customized, LABEL is its display name,
FIELD-INDEX is the currently selected field (0-5)."
  (with-output-to-string (s)
    (format s "Customize Face: ~A~%" label)
    (format s "~A~%~%"
            (make-string (min 50 (+ 16 (length label)))
                         :initial-element #\═))
    ;; Fields
    (loop :for field :in *customize-face-fields*
          :for idx :from 0
          :for selected-p := (= idx field-index)
          :for marker := (if selected-p "▸" " ")
          :for field-label := (customize-face-field-label field)
          :for value-str := (customize-face-field-display face field)
          :do (format s "~A ~A:~A~A~%"
                      marker
                      field-label
                      (make-string (max 1 (- 14 (length field-label)))
                                   :initial-element #\Space)
                      value-str))
    ;; Resolved preview
    (format s "~%Resolved attributes:~%")
    (handler-case
        (let ((resolved (resolve-face face)))
          (when resolved
            (format s "  FG: ~A  BG: ~A~%"
                    (format-color-spec-display (resolved-face-foreground resolved))
                    (format-color-spec-display (resolved-face-background resolved)))
            (format s "  Bold: ~:[no~;yes~]  Underline: ~:[no~;yes~]  Reverse: ~:[no~;yes~]~%"
                    (resolved-face-bold-p resolved)
                    (resolved-face-underline-p resolved)
                    (resolved-face-reverse-p resolved))))
      (error () (format s "  (cannot resolve — missing foreground or background)~%")))
    ;; Keybinding help
    (format s "~%~A~%" (make-string 40 :initial-element #\─))
    (format s "[RET] Edit  [SPC] Toggle  [C-n/C-p] Navigate~%")
    (format s "[C-c C-c] Apply  [C-c C-k] Cancel  [r] Revert")))

(defun rebuild-customize-face-display ()
  "Rebuild the customize buffer content from current *customize-face-state*.
Updates the form display message in-place."
  (when *customize-face-state*
    (let* ((face (getf *customize-face-state* :face))
           (label (getf *customize-face-state* :label))
           (field-index (getf *customize-face-state* :field-index))
           (buf (getf *customize-face-state* :buffer))
           (content (build-customize-face-content face label field-index)))
      ;; Find the first message (the form display) and update it
      (let ((msg (buffer-first-message buf)))
        (when (and msg (message-read-only-p msg))
          (set-message-text msg content))))))

(defun customize-face-next-field ()
  "Move to the next field in the customize form."
  (when *customize-face-state*
    (let ((idx (getf *customize-face-state* :field-index)))
      (when (< idx (1- (length *customize-face-fields*)))
        (setf (getf *customize-face-state* :field-index) (1+ idx))
        (rebuild-customize-face-display)))))

(defun customize-face-prev-field ()
  "Move to the previous field in the customize form."
  (when *customize-face-state*
    (let ((idx (getf *customize-face-state* :field-index)))
      (when (plusp idx)
        (setf (getf *customize-face-state* :field-index) (1- idx))
        (rebuild-customize-face-display)))))

(defun customize-face-toggle-field ()
  "Toggle a boolean field between yes (t) and inherit (nil).
Does nothing for non-boolean fields (foreground, background, parent)."
  (when *customize-face-state*
    (let* ((face (getf *customize-face-state* :face))
           (field-index (getf *customize-face-state* :field-index))
           (field (nth field-index *customize-face-fields*)))
      (when (member field '(:bold-p :underline-p :reverse-p))
        (let ((current (customize-face-field-value face field)))
          (customize-face-set-field-value face field (not current))
          (rebuild-customize-face-display))))))

(defun collect-all-faces ()
  "Collect all unique face objects from the global face registry and
all buffer face registries. Returns a sorted list of plists with
:face, :owner, :name, and :label keys."
  (let ((seen (make-hash-table :test #'eq))
        (result nil))
    ;; Global faces first
    (maphash (lambda (name face)
               (unless (gethash face seen)
                 (setf (gethash face seen) t)
                 (push (list :face face
                             :owner :global
                             :name name
                             :label (format nil "global:~(~A~)" name))
                       result)))
             *global-face-registry*)
    ;; Per-buffer faces
    (dolist (buf *buffer-ring*)
      (maphash (lambda (owner face-set)
                 (maphash (lambda (name face)
                            (unless (gethash face seen)
                              (setf (gethash face seen) t)
                              (push (list :face face
                                          :owner owner
                                          :name name
                                          :label (format nil "~(~A~):~(~A~)"
                                                         owner name))
                                    result)))
                          (face-set-faces face-set)))
               (buffer-face-registry buf)))
    (sort (nreverse result) #'string< :key (lambda (p) (getf p :label)))))

(defun make-color-selection-items ()
  "Build the list of items for color selection in the minibuffer.
Includes CGA colors 0-15 with names, plus an inherit option."
  (let ((items nil))
    (push (list :color-spec nil :display "(inherit / nil)") items)
    (loop :for i :from 0 :to 15
          :do (push (list :color-spec (make-color-spec :cga i)
                          :display (format nil "CGA ~2D: ~A" i (cga-color-name i)))
                    items))
    (nreverse items)))

(defun make-boolean-selection-items ()
  "Build the list of items for boolean field selection in the minibuffer."
  (list (list :value t :display "yes")
        (list :value nil :display "inherit (nil)")))

(defun make-parent-selection-items (current-face)
  "Build the list of parent face candidates for the minibuffer.
Excludes CURRENT-FACE to prevent inheritance cycles."
  (let ((items (list (list :face nil :display "(none)"))))
    (dolist (entry (collect-all-faces))
      (let ((face (getf entry :face)))
        (unless (eq face current-face)
          (push (list :face face
                      :display (getf entry :label))
                items))))
    (nreverse items)))

(defun customize-face-edit-field ()
  "Edit the currently selected field using the minibuffer.
Opens a field-appropriate minibuffer: color picker for foreground/background,
boolean selector for bold/underline/reverse, face selector for parent."
  (when *customize-face-state*
    (let* ((face (getf *customize-face-state* :face))
           (field-index (getf *customize-face-state* :field-index))
           (field (nth field-index *customize-face-fields*)))
      (ecase field
        ;; Color fields — pick from CGA palette
        ((:foreground :background)
         (let ((field-label (customize-face-field-label field)))
           (minibuffer-activate
            (format nil "Set ~A" field-label)
            (make-color-selection-items)
            (lambda (item)
              (customize-face-set-field-value face field (getf item :color-spec))
              (rebuild-customize-face-display)))))
        ;; Boolean fields — yes / inherit
        ((:bold-p :underline-p :reverse-p)
         (let ((field-label (customize-face-field-label field)))
           (minibuffer-activate
            (format nil "Set ~A" field-label)
            (make-boolean-selection-items)
            (lambda (item)
              (customize-face-set-field-value face field (getf item :value))
              (rebuild-customize-face-display)))))
        ;; Parent field — pick from available faces
        (:parent
         (minibuffer-activate
          "Set Parent"
          (make-parent-selection-items face)
          (lambda (item)
            (customize-face-set-field-value face :parent (getf item :face))
            (rebuild-customize-face-display))))))))

(defun customize-face-apply ()
  "Apply face customizations and close the customize buffer.
Changes are already applied to the face object (modified in-place),
so this just closes the buffer and confirms."
  (when *customize-face-state*
    (let ((buf (getf *customize-face-state* :buffer))
          (label (getf *customize-face-state* :label)))
      (setf *customize-face-state* nil)
      (kill-buffer-from-ring buf)
      ;; Show confirmation in the new current buffer
      (buffer-insert-system-message
       (current-buffer)
       (format nil "[Face ~A customized successfully]" label)))))

(defun customize-face-cancel ()
  "Cancel face customization, reverting all changes to original values.
Closes the customize buffer and switches to the previous buffer."
  (when *customize-face-state*
    (let ((face (getf *customize-face-state* :face))
          (snapshot (getf *customize-face-state* :original-values))
          (buf (getf *customize-face-state* :buffer)))
      (customize-face-restore-snapshot face snapshot)
      (setf *customize-face-state* nil)
      (kill-buffer-from-ring buf))))

(defun customize-face-revert-to-original ()
  "Revert all fields to their original values without closing the buffer."
  (when *customize-face-state*
    (let ((face (getf *customize-face-state* :face))
          (snapshot (getf *customize-face-state* :original-values)))
      (customize-face-restore-snapshot face snapshot)
      (rebuild-customize-face-display))))

(defun make-customize-face-buffer (face label)
  "Create a customize buffer for FACE with display LABEL.
Sets up the customize state and returns the new buffer."
  (let* ((buf-name (format nil "*customize:~A*" label))
         (existing (find-buffer-by-name buf-name)))
    ;; Kill any existing customize buffer for this face
    (when existing
      (kill-buffer-from-ring existing))
    (let* ((snapshot (customize-face-snapshot face))
           (buf (make-buffer buf-name :agent-name "customize")))
      (init-face-registry buf)
      (setf (buffer-keymap buf) *default-keymap*)
      (setf (buffer-major-mode buf) "customize")
      ;; Set up customize state
      (setf *customize-face-state*
            (list :face face
                  :label label
                  :field-index 0
                  :original-values snapshot
                  :buffer buf))
      ;; Build initial content
      (let ((content (build-customize-face-content face label 0)))
        (buffer-insert-agent-message buf content))
      (add-buffer-to-ring buf)
      buf)))

(defun handle-customize-key (key)
  "Handle a key event while in customize mode.
Supports field navigation (C-n/C-p), editing (Return), toggling (Space),
apply (C-c C-c), cancel (C-c C-k / C-g / q), revert (r), and passes
through global command bindings like C-x and M-x."
  (cond
    ;; C-c C-c: apply changes (C-c prefix then C-c = ETX = ASCII 3)
    ((equal key '(:ctrl-c #\Etx))
     (customize-face-apply))
    ;; C-c C-k: cancel (C-c prefix then C-k = VT = ASCII 11)
    ((equal key '(:ctrl-c #\Vt))
     (customize-face-cancel))
    ;; C-g: cancel
    ((and (characterp key) (char= key (code-char 7)))
     (customize-face-cancel))
    ;; q: cancel
    ((and (characterp key) (char= key #\q))
     (customize-face-cancel))
    ;; C-n or Down arrow: next field
    ((or (eq key :down)
         (and (characterp key) (char= key (code-char 14))))
     (customize-face-next-field))
    ;; C-p or Up arrow: previous field
    ((or (eq key :up)
         (and (characterp key) (char= key (code-char 16))))
     (customize-face-prev-field))
    ;; Return: edit selected field via minibuffer
    ((and (characterp key) (or (char= key #\Return) (char= key #\Newline)))
     (customize-face-edit-field))
    ;; Space: toggle boolean field
    ((and (characterp key) (char= key #\Space))
     (customize-face-toggle-field))
    ;; r: revert to original values
    ((and (characterp key) (char= key #\r))
     (customize-face-revert-to-original))
    ;; Pass through global bindings like C-x commands and M-x.
    ((and (listp key) (member (first key) '(:ctrl-x :meta :alt)))
     (let ((command (keymap-lookup *default-keymap* key)))
       (when command
         (invoke-command (current-buffer) command))))
    ;; Scroll keys
    ((or (eq key :page-up) (eq key :page-down))
     (let ((command (keymap-lookup *default-keymap* key)))
       (when command
         (invoke-command (current-buffer) command))))
    ;; Everything else: ignore
    (t nil)))

(defun customize-face-command (buffer)
  "Open a face selector in the minibuffer, then customize the selected face.
Lists all faces from all buffer face registries. When a face is selected,
opens a customize buffer where face attributes can be edited interactively.
Bound to C-h F."
  (declare (ignore buffer))
  (let ((faces (collect-all-faces)))
    (if (null faces)
        (buffer-insert-system-message
         (current-buffer)
         "[No faces found to customize]")
        (minibuffer-activate
         "Customize Face"
         (mapcar (lambda (entry)
                   (let* ((face (getf entry :face))
                          (label (getf entry :label))
                          (fg (format-color-spec-display (face-foreground face)))
                          (bg (format-color-spec-display (face-background face)))
                          (display (format nil "~A  fg:~A  bg:~A" label fg bg)))
                     (list :face face
                           :label label
                           :display display)))
                 faces)
         (lambda (item)
           (let* ((face (getf item :face))
                  (label (getf item :label))
                  (buf (make-customize-face-buffer face label)))
             (switch-to-buffer buf)))))))
(defcommand customize-face-command)

;;; --------------------------------------------------------------------------
;;; Introspection: list-functions & describe-function
;;; --------------------------------------------------------------------------

(defun list-functions ()
  "Return a sorted list of function symbols exported from the clawmacs package.
Includes all exported symbols that have function bindings (functions, generic
functions, commands, macros)."
  (let ((functions nil))
    (do-external-symbols (sym :clawmacs)
      (when (fboundp sym)
        (push sym functions)))
    (sort functions #'string< :key #'symbol-name)))

(defun find-keybindings-for-command (command-sym &optional (keymap *default-keymap*))
  "Return a list of key specifications bound to COMMAND-SYM in KEYMAP.
Walks only the direct keymap bindings (not the parent chain)."
  (let ((bindings nil))
    (when keymap
      (maphash (lambda (key cmd)
                 (when (eq cmd command-sym)
                   (push key bindings)))
               (keymap-bindings keymap)))
    bindings))

(defun format-key-binding (key)
  "Format a key binding specification as a human-readable string.
Converts raw characters, keywords, and prefix lists to standard Emacs notation."
  (cond
    ((characterp key)
     (let ((code (char-code key)))
       (cond
         ((= code 13) "RET")
         ((= code 10) "C-j")
         ((= code 27) "ESC")
         ((= code 127) "DEL")
         ((< code 32) (format nil "C-~A" (code-char (+ code 96))))
         ((char= key #\Space) "SPC")
         (t (string key)))))
    ((keywordp key)
     (string-downcase (symbol-name key)))
    ((and (listp key) (eq (first key) :ctrl-x))
     (format nil "C-x ~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :ctrl-c))
     (format nil "C-c ~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :ctrl-h))
     (format nil "C-h ~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :meta))
     (format nil "M-~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :alt))
     (format nil "A-~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :ctrl))
     (format nil "C-~A" (format-key-binding (second key))))
    (t (format nil "~S" key))))

(defun describe-function-to-string (fn-symbol)
  "Return a human-readable string describing FN-SYMBOL.
Includes: name, type, lambda list, docstring, and keybindings."
  (unless (and fn-symbol (fboundp fn-symbol))
    (return-from describe-function-to-string
      (format nil "~A is not a defined function." fn-symbol)))
  (with-output-to-string (s)
    ;; Header
    (format s "~A~%~A~%~%" fn-symbol
            (make-string (min 60 (length (symbol-name fn-symbol)))
                         :initial-element #\-))
    ;; Type
    (let* ((cmd-meta (gethash fn-symbol *command-table*))
           (fn-obj (fdefinition fn-symbol))
           (type-str (cond
                       ((macro-function fn-symbol) "Macro")
                       (cmd-meta "Command")
                       ((typep fn-obj 'generic-function) "Generic Function")
                       (t "Function"))))
      (format s "Type: ~A~%" type-str)
      ;; Lambda list
      (let ((lambda-list
              (handler-case
                  (cond
                    ((typep fn-obj 'generic-function)
                     #+sbcl (sb-mop:generic-function-lambda-list fn-obj)
                     #-sbcl nil)
                    (t
                     #+sbcl (sb-introspect:function-lambda-list fn-symbol)
                     #-sbcl nil))
                (error () nil))))
        (when lambda-list
          (format s "Arguments: (~{~A~^ ~})~%" lambda-list)))
      ;; Keybindings (from actual keymap scan)
      (let ((keybinds (find-keybindings-for-command fn-symbol)))
        (when keybinds
          (format s "Keybindings: ~{~A~^, ~}~%"
                  (mapcar #'format-key-binding keybinds))))
      ;; Docstring
      (let ((doc (or (documentation fn-symbol 'function) "")))
        (when (plusp (length doc))
          (format s "~%~A~%" doc)))
      ;; Extended documentation
      (let ((ext (extended-doc fn-symbol)))
        (when ext
          (let ((usage (getf ext :usage)))
            (when usage
              (format s "~%Usage:~%  ~A~%" usage)))
          (let ((returns (getf ext :returns)))
            (when returns
              (format s "~%Returns:~%  ~A~%" returns)))
          (let ((side-effects (getf ext :side-effects)))
            (when side-effects
              (format s "~%Side Effects:~%  ~A~%" side-effects)))
          (let ((see-also (getf ext :see-also)))
            (when see-also
              (format s "~%See Also: ~{~(~A~)~^, ~}~%" see-also)))
          (let ((category (getf ext :category)))
            (when category
              (format s "~%Category: ~A~%" category))))))))

(defun describe-function-command (buffer)
  "Open a minibuffer selector listing all functions.
On selection, displays detailed function description in a help buffer.
Bound to C-h f."
  (declare (ignore buffer))
  (let* ((fn-list (list-functions))
         (items (mapcar (lambda (sym)
                          (let* ((name (string-downcase (symbol-name sym)))
                                 (cmd-meta (gethash sym *command-table*))
                                 (fn-obj (fdefinition sym))
                                 (type-str (cond
                                             ((macro-function sym) "macro")
                                             (cmd-meta "command")
                                             ((typep fn-obj 'generic-function)
                                              "generic")
                                             (t "function")))
                                 (keybinds (find-keybindings-for-command sym))
                                 (kb-str (if keybinds
                                             (format nil "  [~{~A~^, ~}]"
                                                     (mapcar #'format-key-binding keybinds))
                                             ""))
                                 (display (format nil "~A  (~A)~A" name type-str kb-str)))
                            (list :symbol sym
                                  :name name
                                  :display display)))
                        fn-list)))
    (minibuffer-activate
     "Describe Function" items
     (lambda (item)
       (let* ((sym (getf item :symbol))
              (desc (describe-function-to-string sym))
              (buf-name (format nil "*help:~A*"
                                (string-downcase (symbol-name sym))))
              ;; Reuse existing help buffer for this function if one exists
              (existing (find-buffer-by-name buf-name)))
         (if existing
             (switch-to-buffer existing)
             (let ((help-buf (make-help-buffer buf-name desc)))
               (switch-to-buffer help-buf))))))))
(defcommand describe-function-command)

;;; --------------------------------------------------------------------------
;;; Introspection: list-variables & describe-variable
;;; --------------------------------------------------------------------------

(defun list-variables ()
  "Return a sorted list of variable symbols exported from the clawmacs package.
Includes all exported symbols that have global variable bindings (special
variables, constants, parameters)."
  (let ((variables nil))
    (do-external-symbols (sym :clawmacs)
      (when (boundp sym)
        (push sym variables)))
    (sort variables #'string< :key #'symbol-name)))

(defun variable-kind (sym)
  "Return a keyword describing the kind of variable SYM.
Returns :constant, :parameter, or :variable."
  (cond
    ((constantp sym) :constant)
    ;; Convention: *foo* with earmuffs is a special/dynamic variable.
    ;; defparameter defines a special variable with a default value — we call
    ;; those "parameter" to distinguish from plain defvar.  Since CL doesn't
    ;; store this distinction at runtime, we just rely on the earmuff naming.
    ((let ((name (symbol-name sym)))
       (and (> (length name) 2)
            (char= (char name 0) #\*)
            (char= (char name (1- (length name))) #\*)))
     :parameter)
    (t :variable)))

(defun truncate-value-string (value &optional (max-length 200))
  "Print VALUE to a string, truncating at MAX-LENGTH characters."
  (let* ((full (handler-case
                   (let ((*print-length* 20)
                         (*print-level* 3)
                         (*print-circle* t)
                         (*print-pretty* nil))
                     (prin1-to-string value))
                 (error (e)
                   (format nil "#<error printing value: ~A>" e))))
         (len (length full)))
    (if (> len max-length)
        (concatenate 'string (subseq full 0 max-length) "...")
        full)))

(defun describe-variable-to-string (var-symbol)
  "Return a human-readable string describing VAR-SYMBOL.
Includes: name, kind, type of current value, current value (truncated),
and docstring."
  (unless (and var-symbol (boundp var-symbol))
    (return-from describe-variable-to-string
      (format nil "~A is not a bound variable." var-symbol)))
  (with-output-to-string (s)
    ;; Header
    (format s "~A~%~A~%~%" var-symbol
            (make-string (min 60 (length (symbol-name var-symbol)))
                         :initial-element #\-))
    ;; Kind
    (let ((kind (variable-kind var-symbol)))
      (format s "Kind: ~A~%"
              (ecase kind
                (:constant  "Constant (defconstant)")
                (:parameter "Special Variable (defvar/defparameter)")
                (:variable  "Variable"))))
    ;; Value type
    (let ((val (symbol-value var-symbol)))
      (format s "Value Type: ~A~%" (type-of val))
      ;; Current value (truncated)
      (format s "Current Value: ~A~%" (truncate-value-string val)))
    ;; Docstring
    (let ((doc (or (documentation var-symbol 'variable) "")))
      (when (plusp (length doc))
        (format s "~%~A~%" doc)))
    ;; Extended documentation
    (let ((ext (extended-doc var-symbol)))
      (when ext
        (let ((side-effects (getf ext :side-effects)))
          (when side-effects
            (format s "~%Side Effects:~%  ~A~%" side-effects)))
        (let ((see-also (getf ext :see-also)))
          (when see-also
            (format s "~%See Also: ~{~(~A~)~^, ~}~%" see-also)))
        (let ((category (getf ext :category)))
          (when category
            (format s "~%Category: ~A~%" category)))))))

(defun describe-variable-command (buffer)
  "Open a minibuffer selector listing all exported variables.
On selection, displays detailed variable description in a help buffer.
Bound to C-h v."
  (declare (ignore buffer))
  (let* ((var-list (list-variables))
         (items (mapcar (lambda (sym)
                          (let* ((name (string-downcase (symbol-name sym)))
                                 (kind (variable-kind sym))
                                 (kind-str (ecase kind
                                             (:constant "const")
                                             (:parameter "special")
                                             (:variable "var")))
                                 (val-preview
                                   (handler-case
                                       (let ((val (symbol-value sym)))
                                         (truncate-value-string val 40))
                                     (error () "#<unreadable>")))
                                 (display (format nil "~A  (~A)  = ~A"
                                                  name kind-str val-preview)))
                            (list :symbol sym
                                  :name name
                                  :display display)))
                        var-list)))
    (minibuffer-activate
     "Describe Variable" items
     (lambda (item)
       (let* ((sym (getf item :symbol))
              (desc (describe-variable-to-string sym))
              (buf-name (format nil "*help:~A*"
                                (string-downcase (symbol-name sym))))
              ;; Reuse existing help buffer for this variable if one exists
              (existing (find-buffer-by-name buf-name)))
         (if existing
             (switch-to-buffer existing)
             (let ((help-buf (make-help-buffer buf-name desc)))
               (switch-to-buffer help-buf))))))))
(defcommand describe-variable-command)

;;; --------------------------------------------------------------------------
;;; Introspection: list-types & describe-type
;;; --------------------------------------------------------------------------

(defun list-types ()
  "Return a sorted list of type-name symbols exported from the clawmacs package.
Includes CLOS classes, structures, and conditions — any exported symbol that
names a class (via find-class)."
  (let ((types nil))
    (do-external-symbols (sym :clawmacs)
      (when (find-class sym nil)
        (push sym types)))
    (sort types #'string< :key #'symbol-name)))

(defun type-kind (sym)
  "Return a keyword describing the kind of type SYM names.
Returns :condition, :structure, :standard-class, or :class."
  (let ((class (find-class sym nil)))
    (cond
      ((null class) :unknown)
      ((subtypep sym 'condition) :condition)
      ((typep class 'structure-class) :structure)
      ((typep class 'standard-class) :standard-class)
      (t :class))))

(defun type-kind-label (kind)
  "Return a human-readable label for a type-kind keyword."
  (ecase kind
    (:condition "Condition")
    (:structure "Structure (defstruct)")
    (:standard-class "Class (defclass)")
    (:class "Built-in Class")
    (:unknown "Unknown")))

(defun type-slot-info (class)
  "Return a list of plists describing each slot in CLASS.
Each plist has :name, :type, :initform, :initargs, :readers, :writers,
:allocation, and :documentation."
  #+sbcl
  (handler-case
      (progn
        ;; Ensure the class is finalized so slots are available
        (unless (sb-mop:class-finalized-p class)
          (sb-mop:finalize-inheritance class))
        (mapcar
         (lambda (slot)
           (list :name (sb-mop:slot-definition-name slot)
                 :type (let ((ty (sb-mop:slot-definition-type slot)))
                         (if (eq ty t) nil ty))
                 :initform (if (sb-mop:slot-definition-initfunction slot)
                               (sb-mop:slot-definition-initform slot)
                               :no-initform)
                 :initargs (sb-mop:slot-definition-initargs slot)
                 :readers (when (typep slot 'sb-mop:direct-slot-definition)
                            (sb-mop:slot-definition-readers slot))
                 :writers (when (typep slot 'sb-mop:direct-slot-definition)
                            (sb-mop:slot-definition-writers slot))
                 :allocation (sb-mop:slot-definition-allocation slot)
                 :documentation (documentation slot t)))
         (sb-mop:class-direct-slots class)))
  (error () nil))
  #-sbcl nil)

(defun type-struct-slot-info (sym)
  "Return a list of plists describing each slot in structure type SYM.
Uses sb-kernel:dd-slots to get defstruct slot details."
  #+sbcl
  (handler-case
      (let* ((layout (sb-kernel:find-layout sym))
             (dd (when layout (sb-kernel:layout-info layout))))
        (when dd
          (mapcar
           (lambda (dsd)
             (list :name (sb-kernel:dsd-name dsd)
                   :type (let ((ty (sb-kernel:dsd-type dsd)))
                           (if (eq ty t) nil ty))
                   :read-only (sb-kernel:dsd-read-only dsd)
                   :accessor (let ((acc-name (sb-kernel:dsd-accessor-name dsd)))
                               (when (fboundp acc-name) acc-name))))
           (sb-kernel:dd-slots dd))))
    (error () nil))
  #-sbcl nil)

(defun describe-type-to-string (type-symbol)
  "Return a human-readable string describing the type named by TYPE-SYMBOL.
Includes: name, kind, superclasses, slots/fields with their types, initforms,
accessors, and documentation. Also shows class-level and extended documentation."
  (let ((class (find-class type-symbol nil)))
    (unless class
      (return-from describe-type-to-string
        (format nil "~A does not name a type." type-symbol))))
  (with-output-to-string (s)
    ;; Header
    (format s "~A~%~A~%~%" type-symbol
            (make-string (min 60 (length (symbol-name type-symbol)))
                         :initial-element #\-))
    (let* ((class (find-class type-symbol))
           (kind (type-kind type-symbol)))
      ;; Kind
      (format s "Kind: ~A~%" (type-kind-label kind))
      ;; Superclasses
      #+sbcl
      (handler-case
          (let* ((supers (sb-mop:class-direct-superclasses class))
                 (super-names (mapcar #'class-name supers)))
            (when (and super-names
                       (not (equal super-names '(structure-object)))
                       (not (equal super-names '(standard-object))))
              (format s "Superclasses: ~{~A~^, ~}~%" super-names)))
        (error () nil))
      ;; Class documentation
      (let ((doc (documentation class t)))
        (when (and doc (plusp (length doc)))
          (format s "~%~A~%" doc)))
      ;; Slots / Fields
      (cond
        ;; Structure type — use defstruct slot introspection
        ((eq kind :structure)
         (let ((slots (type-struct-slot-info type-symbol)))
           (when slots
             (format s "~%Fields:~%")
             (dolist (slot slots)
               (let ((name (getf slot :name))
                     (type (getf slot :type))
                     (read-only (getf slot :read-only))
                     (accessor (getf slot :accessor)))
                 (format s "  ~A" name)
                 (when type (format s " : ~A" type))
                 (when read-only (format s "  [read-only]"))
                 (when accessor (format s "  (accessor: ~A)" accessor))
                 (format s "~%"))))))
        ;; CLOS class or condition — use MOP
        ((member kind '(:standard-class :condition))
         (let ((slots (type-slot-info class)))
           (when slots
             (format s "~%Slots:~%")
             (dolist (slot slots)
               (let ((name (getf slot :name))
                     (type (getf slot :type))
                     (initform (getf slot :initform))
                     (initargs (getf slot :initargs))
                     (readers (getf slot :readers))
                     (writers (getf slot :writers))
                     (doc (getf slot :documentation)))
                 (format s "  ~A" name)
                 (when type (format s " : ~A" type))
                 (format s "~%")
                 (when initargs
                   (format s "    Initargs: ~{~S~^, ~}~%" initargs))
                 (unless (eq initform :no-initform)
                   (format s "    Default: ~S~%" initform))
                 (when readers
                   (format s "    Readers: ~{~A~^, ~}~%" readers))
                 (when writers
                   (format s "    Writers: ~{~A~^, ~}~%" writers))
                 (when (and doc (plusp (length doc)))
                   (format s "    ~A~%" doc))))))))
      ;; Extended documentation
      (let ((ext (extended-doc type-symbol)))
        (when ext
          (let ((usage (getf ext :usage)))
            (when usage
              (format s "~%Usage:~%  ~A~%" usage)))
          (let ((returns (getf ext :returns)))
            (when returns
              (format s "~%Returns:~%  ~A~%" returns)))
          (let ((side-effects (getf ext :side-effects)))
            (when side-effects
              (format s "~%Side Effects:~%  ~A~%" side-effects)))
          (let ((see-also (getf ext :see-also)))
            (when see-also
              (format s "~%See Also: ~{~(~A~)~^, ~}~%" see-also)))
          (let ((category (getf ext :category)))
            (when category
              (format s "~%Category: ~A~%" category))))))))

(defun undocumented-types ()
  "Return a list of exported type symbols that lack a defdoc entry.
Useful for finding types that still need extended documentation."
  (let ((missing nil))
    (dolist (sym (list-types))
      (unless (gethash sym *extended-docs*)
        (push sym missing)))
    (nreverse missing)))

(defun describe-type-command (buffer)
  "Open a minibuffer selector listing all defined types.
On selection, displays detailed type description in a help buffer.
Bound to C-h T."
  (declare (ignore buffer))
  (let* ((type-list (list-types))
         (items (mapcar (lambda (sym)
                          (let* ((name (string-downcase (symbol-name sym)))
                                 (kind (type-kind sym))
                                 (kind-str (ecase kind
                                             (:condition "condition")
                                             (:structure "struct")
                                             (:standard-class "class")
                                             (:class "built-in")
                                             (:unknown "unknown")))
                                 (class (find-class sym nil))
                                 (doc-preview
                                   (let ((doc (when class (documentation class t))))
                                     (if (and doc (plusp (length doc)))
                                         (let ((first-line
                                                 (subseq doc 0
                                                         (or (position #\Newline doc)
                                                             (min 50 (length doc))))))
                                           (if (> (length first-line) 50)
                                               (concatenate 'string (subseq first-line 0 47) "...")
                                               first-line))
                                         "")))
                                 (display (if (plusp (length doc-preview))
                                              (format nil "~A  (~A)  ~A" name kind-str doc-preview)
                                              (format nil "~A  (~A)" name kind-str))))
                            (list :symbol sym
                                  :name name
                                  :display display)))
                        type-list)))
    (minibuffer-activate
     "Describe Type" items
     (lambda (item)
       (let* ((sym (getf item :symbol))
              (desc (describe-type-to-string sym))
              (buf-name (format nil "*help:~A*"
                                (string-downcase (symbol-name sym))))
              ;; Reuse existing help buffer for this type if one exists
              (existing (find-buffer-by-name buf-name)))
         (if existing
             (switch-to-buffer existing)
             (let ((help-buf (make-help-buffer buf-name desc)))
               (switch-to-buffer help-buf))))))))
(defcommand describe-type-command)

;;; --------------------------------------------------------------------------
;;; Describe Bindings (C-h b)
;;; --------------------------------------------------------------------------

(defun categorize-command (command-sym)
  "Return a category string for COMMAND-SYM based on its name.
Used to group keybindings in the describe-bindings listing."
  (let ((name (string-downcase (symbol-name command-sym))))
    (cond
      ((or (search "scroll" name) (search "forward-char" name)
           (search "backward-char" name) (search "forward-word" name)
           (search "backward-word" name) (search "beginning-of-line" name)
           (search "end-of-line" name))
       "Movement")
      ((or (search "kill" name) (search "delete" name)
           (search "yank" name) (search "insert-newline" name)
           (search "self-insert" name))
       "Editing")
      ((or (search "buffer" name)
           (search "save-session" name)
           (search "load-session" name))
       "Buffers & Sessions")
      ((or (search "model" name) (search "select-model" name))
       "Model Selection")
      ((or (search "describe" name) (search "help" name)
           (search "customize" name)
           (search "execute-extended" name))
       "Help & Introspection")
      ((or (search "debug" name) (search "toggle" name)
           (search "oauth" name))
       "Toggles & Misc")
      ((search "send-message" name)
       "Chat")
      (t "Other"))))

(defun describe-bindings-to-string (&optional (keymap *default-keymap*))
  "Return a formatted string listing all keybindings in KEYMAP, grouped by category.
Each binding shows the key notation and the command name."
  (let ((entries nil))
    ;; Collect all bindings as (key-string command-sym category) triples
    (maphash (lambda (key cmd)
               (let ((key-str (format-key-binding key)))
                 (push (list key-str cmd (categorize-command cmd)) entries)))
             (keymap-bindings keymap))
    ;; Group by category
    (let ((groups (make-hash-table :test #'equal)))
      (dolist (entry entries)
        (destructuring-bind (key-str cmd category) entry
          (declare (ignore cmd))
          (push (list key-str (third entry) (second entry)) (gethash category groups nil))))
      ;; Deduplicate: multiple keys may map to the same command.
      ;; For each category, collect unique (command → list-of-keys) then format.
      (with-output-to-string (s)
        (format s "Key Bindings~%")
        (format s "============~%~%")
        (format s "Keymap: ~A~%~%" (keymap-name keymap))
        ;; Define a stable category order
        (let ((category-order '("Chat" "Movement" "Editing" "Buffers & Sessions"
                                "Model Selection" "Help & Introspection"
                                "Toggles & Misc" "Other")))
          (dolist (category category-order)
            (let ((cat-entries (gethash category groups)))
              (when cat-entries
                (format s "~A~%" category)
                (format s "~A~%" (make-string (length category) :initial-element #\-))
                ;; Group by command symbol within the category
                (let ((cmd-keys (make-hash-table :test #'eq)))
                  (dolist (entry cat-entries)
                    (let ((key-str (first entry))
                          (cmd (third entry)))
                      (push key-str (gethash cmd cmd-keys nil))))
                  ;; Sort commands alphabetically and format
                  (let ((cmd-list nil))
                    (maphash (lambda (cmd keys)
                               (push (cons cmd (sort (copy-list keys) #'string<)) cmd-list))
                             cmd-keys)
                    (setf cmd-list (sort cmd-list #'string<
                                         :key (lambda (c)
                                                (string-downcase (symbol-name (car c))))))
                    (dolist (item cmd-list)
                      (let* ((cmd (car item))
                             (keys (cdr item))
                             (cmd-name (string-downcase (symbol-name cmd)))
                             (keys-str (format nil "~{~A~^, ~}" keys)))
                        (format s "  ~20A  ~A~%" keys-str cmd-name)))))
                (format s "~%")))))))))

(defun describe-bindings-command (buffer)
  "Open a help buffer listing all keybindings in the default keymap.
Bound to C-h b."
  (declare (ignore buffer))
  (let* ((buf-name "*help:keybindings*")
         (existing (find-buffer-by-name buf-name)))
    (if existing
        (switch-to-buffer existing)
        (let ((help-buf (make-help-buffer buf-name
                                          (describe-bindings-to-string))))
          (switch-to-buffer help-buf)))))
(defcommand describe-bindings-command)

;;; --------------------------------------------------------------------------
;;; Event Loop
;;; --------------------------------------------------------------------------

(defvar *meta-pending* nil
  "When non-nil, the next key event is combined with Meta (ESC prefix).")

(defvar *alt-pending* nil
  "When non-nil, the next key event is combined with physical Alt.")

(defvar *alt-emulates-meta* t
  "When non-nil, physical Alt key events are treated as Meta in McCLIM.
Set this to NIL in user init to keep Alt and Meta separate when the backend
reports standalone Alt/Meta key events.")

(defvar *cx-pending* nil
  "When non-nil, the next key event is combined with C-x prefix.")

(defvar *cc-pending* nil
  "When non-nil, the next key event is combined with C-c prefix.
C-c is reserved for buffer-mode-specific commands (e.g. C-c t).
Quit is C-x C-c (global command, uses C-x prefix).")

(defvar *ch-pending* nil
  "When non-nil, the next key event is combined with C-h prefix.
C-h is the help prefix (e.g. C-h b = describe bindings).")

(defvar *deny-message-mode* nil
  "When non-nil, the input area is being used to type a denial message.")

;;; --------------------------------------------------------------------------
;;; Event Loop Dispatch
;;; --------------------------------------------------------------------------

(defun handle-key-event (buf key)
  "Dispatch a normalized key through the buffer's keymap.
Returns :QUIT if the application should exit, or nil otherwise.
Handles approval mode, deny-message mode, and normal dispatch.
KEY is already normalized by the interface before calling this."
  (flet ((redraw-key-p (candidate)
           (or (and (characterp candidate)
                    (char= candidate (code-char 12)))
               (equal candidate '(:ctrl #\l))
               (equal candidate '(:ctrl #\L))))
         (sync-current-skill-completion ()
           (let ((current (current-buffer)))
             (if (and current (not (eq current buf)))
                 (deactivate-skill-completion)
                 (sync-skill-completion buf)))))
  (let ((*current-caller* :user))
    (when (null key)
      (return-from handle-key-event nil))
    (cond
      ;; C-x C-c always quits (Emacs standard quit chord)
      ((equal key (list :ctrl-x #\Etx))
       :quit)

      ;; C-l requests a full redraw in every mode.
      ((redraw-key-p key)
       (redraw-screen-command buf))

      ;; === MINIBUFFER MODE ===
      ;; When the minibuffer is active, it captures all input
      (*minibuffer-active*
       (handle-minibuffer-key key)
       nil)

      ;; === SESSION TREE SELECTOR MODE ===
      ;; Navigation, folding, filtering, and branch selection.
      (*session-tree-selector-active*
       (handle-session-tree-selector-key key)
       nil)

      ;; === BUFFER SELECTOR MODE ===
      ;; Navigation and selection within the buffer list overlay
      (*buffer-selector-active*
       (handle-buffer-selector-key key)
       nil)

      ;; === MODEL SELECTOR MODE ===
      ;; Navigation and selection within the model list overlay
      (*model-selector-active*
       (handle-model-selector-key key buf)
       nil)

      ;; === THINK SELECTOR MODE ===
      ;; Navigation and selection within the think-level overlay
      (*think-selector-active*
       (handle-think-selector-key key buf)
       nil)

      ;; === CUSTOMIZE MODE ===
      ;; When the current buffer is a customize buffer, dispatch to
      ;; the customize key handler for field navigation and editing.
      ;; Clean up stale state if the customize buffer was killed.
      ((and *customize-face-state*
            (let ((cbuf (getf *customize-face-state* :buffer)))
              (if (member cbuf *buffer-ring*)
                  (eq buf cbuf)
                  (progn (setf *customize-face-state* nil) nil))))
       (handle-customize-key key)
       nil)

      ;; === OPENAI OAUTH MODE ===
      ;; OAuth is pending in a background localhost callback server; only C-g cancels.
      (*openai-oauth-pending*
       (cond
         ;; C-g: cancel OAuth flow
         ((and (characterp key) (char= key (code-char 7)))
          (cancel-openai-codex-oauth-login *openai-oauth-pending*)
          (setf *openai-oauth-pending* nil
                (buffer-status buf) :idle)
          (buffer-insert-system-message buf "[OAuth cancelled]"))
         ;; Ignore other input while the browser flow is pending.
         (t nil))
       nil)

      ;; === DENY MESSAGE MODE ===
      ;; User is typing a denial reason; Enter submits, normal editing works
      (*deny-message-mode*
       (cond
         ((and (characterp key) (char= key #\Newline))
         ;; Submit denial message
          (let ((reason (message-text (buffer-input-message buf))))
            (setf *deny-message-mode* nil)
            (handle-approval-response buf (cons :deny-with-message reason))))
         ;; Normal editing in the input area
         ((let ((cmd (keymap-lookup (buffer-keymap buf) key)))
            (when cmd
              (unless (eq cmd 'send-message)
                (invoke-command buf cmd))
              t)))
         ((and (characterp key) (graphic-char-p key))
          (let ((*self-insert-char* key))
            (self-insert-command buf))))
       nil)

      ;; === APPROVAL MODE ===
      ;; Waiting for a/d/m keypress
      ((buffer-approval-pending buf)
       (when (characterp key)
         (case key
           (#\a (handle-approval-response buf :approve))
           (#\d (handle-approval-response buf :deny))
           (#\m
            ;; Switch to deny-message mode: clear input for typing reason
            (set-message-text (buffer-input-message buf) "")
            (setf *deny-message-mode* t))))
       nil)

      ;; === AUTOMATIC SKILL COMPLETION ===
      ;; Completion is non-modal for normal typing, but selected navigation and
      ;; confirmation keys are consumed before the chat keymap can send input.
      ((and *skill-completion-active*
            (handle-skill-completion-key buf key))
       nil)

      ;; === NORMAL MODE ===
      ;; Keymap lookup
      ((let ((command (keymap-lookup (buffer-keymap buf) key)))
         (when command
           (let ((result (invoke-command buf command)))
             (when (eq result :redraw)
               (return-from handle-key-event :redraw)))
           (when (and (characterp key)
                      (not (member command '(scroll-up-command scroll-down-command))))
             (setf (buffer-scroll-offset buf) 0))
           (sync-current-skill-completion)
           t)))
      ;; Self-insert
      ((and (characterp key) (graphic-char-p key))
       (let ((*self-insert-char* key))
       (self-insert-command buf))
       (setf (buffer-scroll-offset buf) 0)
       (sync-current-skill-completion)
       nil)
      (t nil)))))

(defvar *user-init-directory*
  (merge-pathnames #P".clawmacs.d/" (user-homedir-pathname))
  "Directory for user Lisp configuration files.")

(defvar *user-init-file*
  (merge-pathnames "init.lisp" *user-init-directory*)
  "Path to the user init file, loaded at startup if it exists.")

(defvar *inhibit-user-init* nil
  "When non-nil, skip loading the user init file at startup.")

(defun load-user-init-file ()
  "Load ~/.clawmacs.d/init.lisp if it exists. Errors are caught and reported."
  (when *inhibit-user-init*
    (return-from load-user-init-file nil))
  (let ((init-path (probe-file *user-init-file*)))
    (when init-path
      (handler-case
          (let ((*package* (find-package :clawmacs)))
            (load init-path :verbose nil :print nil))
        (error (e)
          (format *error-output*
                  "~&;; Warning: error loading ~A:~%;; ~A~%"
                  init-path e)
          (file-debug-log "init" "error loading ~A: ~A" init-path e)
          nil)))))

(defun parse-clawmacs-args ()
  "Parse command-line arguments and environment variables.
Recognized flags:
  --debug-log <path>   Enable file-based debug logging to <path>.
  --clean-build        Clear cached Lisp build artifacts before loading.
  --no-init            Skip loading the user init file.
Environment variables:
  CLAWMACS_DEBUG_LOG   Same as --debug-log (CLI flag takes precedence)."
  ;; CLI args (everything after SBCL's -- separator)
  (let ((args (uiop:command-line-arguments)))
    (loop :while args
          :for arg := (pop args)
          :do (cond
                ((string= arg "--debug-log")
                 (let ((path (pop args)))
                   (when path
                     (setf *debug-log-file* (pathname path)))))
                ((or (string= arg "--clean-build")
                     (string= arg "--force-clean-build"))
                 nil)
                ((string= arg "--no-init")
                 (setf *inhibit-user-init* t)))))
  ;; Environment variable fallback
  (unless *debug-log-file*
    (let ((env (uiop:getenv "CLAWMACS_DEBUG_LOG")))
      (when (and env (plusp (length env)))
        (setf *debug-log-file* (pathname env)))))
  ;; Log startup marker
  (when *debug-log-file*
    (file-debug-log "startup" "debug log enabled, writing to ~A" *debug-log-file*)))

(defun initialize-clawmacs-runtime ()
  "Initialize shared runtime state before either UI or prompt execution."
  (init-default-keymap)
  (init-tools)
  (init-global-faces)
  ;; Load the configured personality prompt file before init.lisp so user init
  ;; may still override it directly or reload after changing the path.
  (load-personality-prompt-file)
  (load-user-init-file)
  (reload-package-channels)
  (load-autoload-packages)
  (load-project-definitions)
  (run-hooks '*startup-hook*))

(defun reset-interaction-state ()
  "Reset buffer selectors, minibuffer state, OAuth state, and key prefixes."
  (setf *buffer-ring* nil *buffer-counter* 0)
  (setf *buffer-selector-active* nil
        *buffer-selector-index* 0
        *buffer-selector-scroll* 0)
  (setf *model-selector-active* nil
        *model-selector-index* 0
        *model-selector-scroll* 0
        *model-selector-entries* nil)
  (setf *think-selector-active* nil
        *think-selector-index* 0
        *think-selector-scroll* 0
        *think-selector-entries* nil)
  (session-tree-selector-deactivate)
  (setf *minibuffer-active* nil
        *minibuffer-mode* :completion
        *minibuffer-prompt* ""
        *minibuffer-input* ""
        *minibuffer-point* 0
        *minibuffer-items* nil
        *minibuffer-filtered-items* nil
        *minibuffer-match-positions* nil
        *minibuffer-selected-index* 0
        *minibuffer-scroll-offset* 0
        *minibuffer-callback* nil)
  (deactivate-skill-completion)
  (setf *openai-oauth-pending* nil)
  (setf *meta-pending* nil
        *alt-pending* nil
        *cx-pending* nil
        *cc-pending* nil
        *ch-pending* nil))

(defun make-initial-chat-buffer (session-name agent-name)
  "Create and register the initial interactive chat buffer."
  (let ((buf (make-chat-buffer session-name
                               :agent-name agent-name
                               :working-directory (truename ".")
                               :add-to-ring-p t)))
    (setf *sandbox-root* (truename "."))
    (run-hook-with-args '*initial-buffer-hook* buf)
    (autosave-session-snapshot buf)
    buf))

(defun prompt-usage-string ()
  "Return command-line help for non-interactive prompt mode."
  (format nil "Usage: prompt.sh [options] PROMPT...

Options:
  --agent NAME              Use the named clawmacs agent.
  --provider PROVIDER       Override provider: openai-codex, zai, openrouter.
                            Default without --agent: ~A.
  --model MODEL             Override the model name.
                            Default without --agent: ~A.
  --think LEVEL             Override reasoning effort when supported.
  --show-tools              Print tool calls/results to stderr.
  --show-reasoning          Print provider-supplied reasoning blocks when present.
  --show-metadata           Print provider/model/iteration metadata to stderr.
  --json                    Emit a JSON result object to stdout.
  --auto-approve-tools      Allow permission-gated tools without an interactive prompt.
  --max-tool-iterations N   Stop after N tool-call turns (default: 20).
  --package NAME            Enable an installed package for this prompt run. May repeat.
  --skill-root PATH         Add a skill root for this prompt run. May repeat.
  --session NAME            Load/update a saved session instead of one-shot mode.
  --pipeline NAME           Run a deterministic pipeline defined in init.lisp.
  --debug-log PATH          Write low-level debug logs to PATH.
  --isolated                Use temporary prompt config/project/session dirs.
  --clean-build             Clear cached Lisp build artifacts before loading.
  --force-clean-build       Alias for --clean-build.
  --no-init                 Skip ~~/.clawmacs.d/init.lisp.
  --help                    Show this help.

If PROMPT is omitted, non-interactive stdin is read as the prompt."
          +prompt-default-provider+
          +prompt-default-model+))

(defun default-session-prompt-session-name ()
  "Return the default saved session name for session-prompt.sh."
  (let ((name (uiop:getenv "CLAWMACS_SESSION_PROMPT_SESSION")))
    (if (and name (not (blank-string-p name)))
        name
        +session-prompt-default-session-name+)))

(defun session-prompt-usage-string ()
  "Return command-line help for saved-session prompt mode."
  (format nil "Usage: session-prompt.sh [options] PROMPT...

Runs PROMPT against a saved prompt-mode session. The next invocation with the
same session name reloads the prior transcript before sending the new prompt.

Session options:
  --session NAME            Saved session to load/update.
                            Default: ~A
  CLAWMACS_SESSION_PROMPT_SESSION
                            Environment default for --session.

All prompt.sh routing/output options are also supported.

Example:
  ./session-prompt.sh \"Reply with exactly: CACHE-PROBE-ONE\"
  ./session-prompt.sh \"Reply with exactly: CACHE-PROBE-TWO\""
          (default-session-prompt-session-name)))

(defun require-option-value (option args)
  "Pop and return OPTION's value from ARGS, or signal a clear error."
  (let ((value (pop args)))
    (unless value
      (error "~A requires a value" option))
    (values value args)))

(defun parse-positive-integer-option (option value)
  "Parse VALUE as a positive integer for OPTION."
  (let ((parsed (parse-integer value :junk-allowed nil)))
    (unless (plusp parsed)
      (error "~A must be a positive integer, got ~A" option value))
    parsed))

(defun read-stdin-to-string ()
  "Read all available standard input into a string."
  (let ((out (make-string-output-stream)))
    (loop :for char := (read-char *standard-input* nil nil)
          :while char
          :do (write-char char out))
    (get-output-stream-string out)))

(defun finalize-prompt-option-text (prompt-parts)
  "Return prompt text from PROMPT-PARTS or non-interactive stdin."
  (let ((from-args (and prompt-parts
                        (format nil "~{~A~^ ~}" prompt-parts))))
    (cond
      ((and from-args (not (blank-string-p from-args)))
       from-args)
      ((not (interactive-stream-p *standard-input*))
       (string-trim '(#\Space #\Tab #\Newline #\Return)
                    (read-stdin-to-string)))
      (t
       nil))))

(defun parse-clawmacs-prompt-args (&optional (args (uiop:command-line-arguments)))
  "Parse ARGS for non-interactive prompt mode and return PROMPT-OPTIONS."
  (let ((options (make-prompt-options))
        (prompt-parts nil)
        (agent-supplied-p nil)
        (provider-supplied-p nil)
        (model-supplied-p nil)
        (remaining (copy-list args)))
    (loop :while remaining
          :for arg := (pop remaining)
          :do (cond
                ((string= arg "--")
                 (setf prompt-parts (append prompt-parts remaining)
                       remaining nil))
                ((or (string= arg "--help") (string= arg "-h"))
                 (setf (prompt-options-help-p options) t))
                ((string= arg "--agent")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-agent-name options) value
                         agent-supplied-p t
                         remaining rest)))
                ((string= arg "--provider")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-provider options) value
                         provider-supplied-p t
                         remaining rest)))
                ((string= arg "--model")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-model options) value
                         model-supplied-p t
                         remaining rest)))
                ((or (string= arg "--think")
                     (string= arg "--reasoning-effort"))
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-think-level options) value
                         remaining rest)))
                ((or (string= arg "--prompt") (string= arg "-p"))
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf prompt-parts (append prompt-parts (list value))
                         remaining rest)))
                ((or (string= arg "--show-tools")
                     (string= arg "--show-tool-calls"))
                 (setf (prompt-options-show-tools-p options) t))
                ((string= arg "--show-reasoning")
                 (setf (prompt-options-show-reasoning-p options) t))
                ((string= arg "--show-metadata")
                 (setf (prompt-options-show-metadata-p options) t))
                ((string= arg "--json")
                 (setf (prompt-options-json-p options) t))
                ((string= arg "--auto-approve-tools")
                 (setf (prompt-options-auto-approve-tools-p options) t))
                ((string= arg "--max-tool-iterations")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-max-tool-iterations options)
                         (parse-positive-integer-option arg value)
                         remaining rest)))
                ((string= arg "--skill-root")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-skill-roots options)
                         (append (prompt-options-skill-roots options)
                                 (list value))
                         remaining rest)))
                ((string= arg "--package")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-packages options)
                         (append (prompt-options-packages options)
                                 (list value))
                         remaining rest)))
                ((string= arg "--session")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-session-name options) value
                         remaining rest)))
                ((string= arg "--pipeline")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-pipeline-name options) value
                         remaining rest)))
                ((string= arg "--debug-log")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-debug-log-path options) value
                         remaining rest)))
                ((or (string= arg "--isolated")
                     (string= arg "--isolate"))
                 (setf (prompt-options-isolated-p options) t))
                ((or (string= arg "--clean-build")
                     (string= arg "--force-clean-build"))
                 nil)
                ((string= arg "--no-init")
                 (setf (prompt-options-inhibit-user-init-p options) t))
                ((and (plusp (length arg))
                      (char= #\- (char arg 0)))
                 (error "Unknown prompt option: ~A" arg))
                (t
                 (setf prompt-parts (append prompt-parts (cons arg remaining))
                       remaining nil))))
    (unless (prompt-options-help-p options)
      (unless (or agent-supplied-p provider-supplied-p)
        (setf (prompt-options-provider options) +prompt-default-provider+))
      (unless (or agent-supplied-p model-supplied-p provider-supplied-p)
        (setf (prompt-options-model options) +prompt-default-model+))
      (setf (prompt-options-prompt options)
            (finalize-prompt-option-text prompt-parts)))
    options))

(defun maybe-enable-prompt-debug-log (options)
  "Apply prompt-mode debug-log options and environment fallback."
  (let ((path (prompt-options-debug-log-path options)))
    (when path
      (setf *debug-log-file* (pathname path))))
  (unless *debug-log-file*
    (let ((env (uiop:getenv "CLAWMACS_DEBUG_LOG")))
      (when (and env (plusp (length env)))
        (setf *debug-log-file* (pathname env)))))
  (when *debug-log-file*
    (file-debug-log "startup" "prompt debug log enabled, writing to ~A"
                    *debug-log-file*)))

(defun prompt-isolation-root ()
  "Create and return a temporary root for isolated prompt execution."
  (let ((root (merge-pathnames
               (format nil "clawmacs-prompt-isolated-~D-~D/"
                       (get-universal-time)
                       (get-internal-real-time))
               #P"/tmp/")))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    root))

(defun apply-prompt-isolation ()
  "Redirect prompt-mode mutable config paths into a temporary directory."
  (let* ((root (prompt-isolation-root))
         (config-dir (merge-pathnames #P".clawmacs.d/" root)))
    (ensure-directories-exist (merge-pathnames #P".keep" config-dir))
    (setf *user-init-directory* config-dir
          *user-init-file* (merge-pathnames #P"init.lisp" config-dir)
          *project-definitions-directory*
          (merge-pathnames #P"projects.d/" root)
          *sessions-dir*
          (merge-pathnames #P"sessions/" root)
          *agent-defaults-path*
          (merge-pathnames #P"agent-defaults.json" root)
          *packages-directory*
          (merge-pathnames #P"packages/" root)
          *package-configuration-path*
          (merge-pathnames #P"packages.json" config-dir)
          *skill-user-directory*
          (merge-pathnames #P"skills/" config-dir)
          *skill-agents-directory*
          (merge-pathnames #P"agents-skills/" root)
          *skill-system-directory*
          (merge-pathnames #P"system-skills/" root)
          *skill-configuration-path*
          (merge-pathnames #P"skills.json" config-dir)
          *personality-prompt-path*
          (merge-pathnames #P"personality-prompt.txt" root))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *project-definitions-directory*))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *sessions-dir*))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *packages-directory*))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *skill-user-directory*))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *skill-agents-directory*))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *skill-system-directory*))
    root))

(defparameter *prompt-workspace-project-name* "clawmacs"
  "Project name used for the source tree mounted into prompt.sh runs.")

(defun prompt-workspace-project-root ()
  "Return the source root that prompt.sh should expose as a Clawmacs project."
  (let ((root (uiop:getenv "CLAWMACS_PROMPT_PROJECT_ROOT")))
    (if (and root (plusp (length root)))
        (truename (uiop:ensure-directory-pathname root))
        (truename "."))))

(defun ensure-prompt-workspace-project ()
  "Expose the prompt workspace source tree as project \"clawmacs\"."
  (define-project *prompt-workspace-project-name*
    :root (prompt-workspace-project-root)
    :description "Clawmacs source tree mounted for prompt-mode analysis"
    :systems '(:clawmacs :clawmacs/tests)
    :source :builtin
    :replace nil))

(defun prompt-tool-event-json (event)
  "Return EVENT as a JSON-ready alist."
  `((:id . ,(prompt-tool-event-id event))
    (:name . ,(prompt-tool-event-name event))
    (:input . ,(prompt-tool-event-input event))
    (:result . ,(prompt-tool-event-result-text event))
    (:display . ,(prompt-tool-event-display event))
    (:denied . ,(prompt-tool-event-denied-p event))))

(defun prompt-run-result-json (result)
  "Return RESULT as a JSON-ready alist."
  `((:prompt . ,(prompt-run-result-prompt result))
    (:final--text . ,(prompt-run-result-final-text result))
    (:agent . ,(prompt-run-result-agent-name result))
    (:provider . ,(and (prompt-run-result-provider result)
                       (string-downcase
                        (symbol-name (prompt-run-result-provider result)))))
    (:model . ,(prompt-run-result-model result))
    (:reasoning--effort . ,(prompt-run-result-think-level result))
    (:iterations . ,(prompt-run-result-iterations result))
    (:stop--reason . ,(prompt-run-result-stop-reason result))
    ,@(when (prompt-run-result-usage result)
        `((:usage . ,(token-usage-json (prompt-run-result-usage result)))))
    (:tool--events . ,(coerce (mapcar #'prompt-tool-event-json
                                       (prompt-run-result-tool-events result))
                              'vector))
    (:reasoning . ,(coerce (prompt-run-result-reasoning-blocks result) 'vector))))

(defun write-string-with-final-newline (text stream)
  "Write TEXT to STREAM and ensure it is newline-terminated."
  (write-string (or text "") stream)
  (unless (and text
               (plusp (length text))
               (char= #\Newline (char text (1- (length text)))))
    (terpri stream)))

(defun write-prompt-metadata (result stream)
  "Write prompt metadata comments to STREAM."
  (format stream ";; agent: ~A~%" (prompt-run-result-agent-name result))
  (format stream ";; provider/model: ~(~A~)/~A~%"
          (prompt-run-result-provider result)
          (prompt-run-result-model result))
  (format stream ";; think: ~A~%"
          (or (prompt-run-result-think-level result) "default"))
  (format stream ";; iterations: ~D~%"
          (prompt-run-result-iterations result))
  (format stream ";; stop-reason: ~A~%"
          (or (prompt-run-result-stop-reason result) "nil"))
  (let ((usage-line (format-token-usage-summary
                     (prompt-run-result-usage result))))
    (when usage-line
      (format stream ";; ~A~%" usage-line))))

(defun write-prompt-tool-events (result stream)
  "Write prompt tool events to STREAM in Lisp-oriented display form."
  (loop :for event :in (prompt-run-result-tool-events result)
        :for index :from 1
        :do (format stream ";; tool ~D: ~A~%" index
                    (prompt-tool-event-name event))
            (format stream "~A~%"
                    (format-tool-call-sexpr
                     (prompt-tool-event-name event)
                     (prompt-tool-event-input event)))
            (format stream "~A~%~%" (prompt-tool-event-display event))))

(defun write-prompt-tool-event-list (events stream)
  "Write prompt tool EVENTS to STREAM."
  (loop :for event :in events
        :for index :from 1
        :do (format stream ";; partial tool ~D: ~A~%" index
                    (prompt-tool-event-name event))
            (format stream "~A~%"
                    (format-tool-call-sexpr
                     (prompt-tool-event-name event)
                     (prompt-tool-event-input event)))
            (format stream "~A~%~%" (prompt-tool-event-display event))))

(defun write-prompt-reasoning (result stream)
  "Write provider-supplied reasoning blocks to STREAM when present."
  (let ((blocks (prompt-run-result-reasoning-blocks result)))
    (if blocks
        (dolist (block blocks)
          (write-string-with-final-newline block stream))
        (format stream ";; no provider-supplied reasoning blocks captured~%"))))

(defun write-prompt-run-result (result options)
  "Write RESULT according to OPTIONS."
  (cond
    ((prompt-options-json-p options)
     (write-string-with-final-newline
      (api-json-encode (prompt-run-result-json result))
      *standard-output*))
    (t
     (when (prompt-options-show-metadata-p options)
       (write-prompt-metadata result *error-output*))
     (when (prompt-options-show-tools-p options)
       (write-prompt-tool-events result *error-output*))
     (when (prompt-options-show-reasoning-p options)
       (write-prompt-reasoning result *error-output*))
     (write-string-with-final-newline
      (prompt-run-result-final-text result)
      *standard-output*))))

(defun run-prompt-options (options)
  "Run parsed prompt OPTIONS and return a PROMPT-RUN-RESULT."
  (if (prompt-options-pipeline-name options)
      (run-pipeline-prompt
       (prompt-options-prompt options)
       (prompt-options-pipeline-name options)
       :session-name (prompt-options-session-name options)
       :agent-name (prompt-options-agent-name options)
       :provider (prompt-options-provider options)
       :model (prompt-options-model options)
       :think-level (prompt-options-think-level options)
       :max-tool-iterations
       (prompt-options-max-tool-iterations options)
       :auto-approve-tools-p
       (prompt-options-auto-approve-tools-p options)
       :package-names
       (prompt-options-packages options))
      (if (prompt-options-session-name options)
          (run-session-prompt
           (prompt-options-prompt options)
           :session-name (prompt-options-session-name options)
           :agent-name (prompt-options-agent-name options)
           :provider (prompt-options-provider options)
           :model (prompt-options-model options)
           :think-level (prompt-options-think-level options)
           :max-tool-iterations
           (prompt-options-max-tool-iterations options)
           :auto-approve-tools-p
           (prompt-options-auto-approve-tools-p options)
           :package-names
           (prompt-options-packages options))
          (run-single-prompt
           (prompt-options-prompt options)
           :agent-name (prompt-options-agent-name options)
           :provider (prompt-options-provider options)
           :model (prompt-options-model options)
           :think-level (prompt-options-think-level options)
           :max-tool-iterations
           (prompt-options-max-tool-iterations options)
           :auto-approve-tools-p
           (prompt-options-auto-approve-tools-p options)
           :package-names
           (prompt-options-packages options)))))

(defun clawmacs-prompt-main* (&key default-session-name usage-string-function)
  "Shared CLI entry point for one-shot and saved-session prompt modes."
  (let ((options nil))
    (handler-case
      (progn
        (setf options (parse-clawmacs-prompt-args))
        (when (and default-session-name
                   (not (prompt-options-session-name options)))
          (setf (prompt-options-session-name options) default-session-name))
        (when (prompt-options-help-p options)
          (write-string-with-final-newline (funcall usage-string-function)
                                           *standard-output*)
          (uiop:quit 0))
        (unless (prompt-options-prompt options)
          (error "No prompt supplied.~%~A" (funcall usage-string-function)))
        (maybe-enable-prompt-debug-log options)
        (when (prompt-options-isolated-p options)
          (let ((root (apply-prompt-isolation)))
            (when (prompt-options-show-metadata-p options)
              (format *error-output* ";; isolated-root: ~A~%" root))))
        (dolist (skill-root (prompt-options-skill-roots options))
          (register-skill-root skill-root :scope :user :source :cli))
        (let ((*inhibit-user-init* (or (prompt-options-isolated-p options)
                                       (prompt-options-inhibit-user-init-p
                                        options))))
          (initialize-clawmacs-runtime)
          (reset-interaction-state)
          (setf *sandbox-root* (truename "."))
          (ensure-prompt-workspace-project)
          (let ((result (run-prompt-options options)))
            (write-prompt-run-result result options)))
        (uiop:quit 0))
      (prompt-run-error (e)
        (format *error-output* "~&clawmacs prompt error: ~A~%" e)
        (when options
          (when (prompt-options-show-metadata-p options)
            (format *error-output* ";; partial iterations: ~D~%"
                    (prompt-run-error-iterations e))
            (format *error-output* ";; partial provider/model: ~(~A~)/~A~%"
                    (or (prompt-run-error-provider e) :unknown)
                    (or (prompt-run-error-model e) "unknown"))
            (format *error-output* ";; partial think: ~A~%"
                    (or (prompt-run-error-think-level e) "default")))
          (when (prompt-run-error-tool-events e)
            (format *error-output* ";; partial tool trace follows~%")
            (write-prompt-tool-event-list
             (prompt-run-error-tool-events e)
             *error-output*)))
        (uiop:quit 1))
    (error (e)
      (format *error-output* "~&clawmacs prompt error: ~A~%" e)
      (uiop:quit 1)))))

(defun clawmacs-prompt-main ()
  "CLI entry point for one-shot prompt execution.
This function exits the Lisp image with status 0 on success and 1 on errors."
  (clawmacs-prompt-main*
   :usage-string-function #'prompt-usage-string))

(defun clawmacs-session-prompt-main ()
  "CLI entry point for saved-session prompt execution."
  (clawmacs-prompt-main*
   :default-session-name (default-session-prompt-session-name)
   :usage-string-function #'session-prompt-usage-string))

(declaim (ftype (function (buffer) *) run-clawmacs-mcclim))

(defun clawmacs-main (&key (session-name "clawmacs:session-01")
                           (agent-name *default-agent-name*))
  "Entry point for clawmacs. Initializes state and starts the McCLIM app."
  (parse-clawmacs-args)
  (initialize-clawmacs-runtime)
  ;; Create initial buffer and initialize global state
  (reset-interaction-state)
  (let ((buf (make-initial-chat-buffer session-name agent-name)))
    (ensure-scratch-buffer)
    (run-clawmacs-mcclim buf)))
