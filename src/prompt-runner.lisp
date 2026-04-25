(in-package :clawmacs)

(defvar *openai-oauth-pending* nil
  "When non-nil, an alist storing the active OAuth flow state:
(:code-verifier . string) (:state . string).")

;;; --------------------------------------------------------------------------
;;; Interactive Agent Runtime
;;; --------------------------------------------------------------------------

(defun insert-agent-message-from-content (buf content-blocks agent-kw)
  "Insert an agent message from API content blocks.
Shows text parts and tool call summaries. Stores raw-content for round-trip."
  (let* ((text-parts (content-text-blocks content-blocks))
          (tool-uses (content-tool-use-blocks content-blocks))
          (canonical-content (canonicalize-message-content "assistant" content-blocks))
          (display (with-output-to-string (s)
                     (when (plusp (length text-parts))
                       (write-string text-parts s))
                     (dolist (tu tool-uses)
                       (when (plusp (length text-parts))
                         (write-char #\Newline s))
                       (write-string (format-tool-call-display tu) s))))
          (agent-msg (buffer-insert-read-only-message
                      buf
                      agent-kw
                      (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   display)
                      :raw-content canonical-content
                      :face-set (gethash agent-kw
                                         (buffer-face-registry buf)))))
    agent-msg))

(defun begin-tool-approval (buf tool-use-blocks)
  "Start the tool approval process. Stashes user input and presents
the first tool needing permission, or auto-executes allowed tools.
Called from update-streaming-response when tool calls are detected."
  ;; Stash current input
  (setf (buffer-stashed-input buf) (message-text (buffer-input-message buf)))
  ;; Queue all tool calls for processing
  (setf (buffer-pending-tool-calls buf) (copy-list tool-use-blocks))
  (setf (buffer-tool-call-results buf) nil)
  ;; Process the first tool (or all auto-approved ones)
  (advance-tool-approval buf))

(defun advance-tool-approval (buf)
  "Process the next pending tool call. Auto-executes :agent-allowed tools,
shows approval prompt for :agent-with-permission tools.
When all tools are done, finalizes the results."
  (let ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword)))
    (loop :while (buffer-pending-tool-calls buf)
          :for tu := (first (buffer-pending-tool-calls buf))
          :for tool-name := (cdr (assoc :name tu))
          :for tool-input := (cdr (assoc :input tu))
          :for tool-id := (cdr (assoc :id tu))
          :do (cond
                ;; Tool needs permission: show approval prompt and return
                ((tool-requires-permission-p tool-name :buffer buf)
                 (let* ((raw-sexpr (format-tool-call-sexpr tool-name tool-input))
                        (expanded (format-tool-call-expanded tool-name tool-input))
                        (extra (tool-approval-extra-display tool-name tool-input)))
                   (setf (buffer-approval-pending buf)
                         `((:tool-name . ,tool-name)
                           (:tool-id . ,tool-id)
                           (:tool-input . ,tool-input)
                           (:display-raw . ,raw-sexpr)
                           (:display-expanded . ,expanded)
                           ,@(when extra `((:display-extra . ,extra)))
                           (:tool-use-block . ,tu)))
                   ;; Clear input area for the prompt
                   (set-message-text (buffer-input-message buf) "")
                   (setf (buffer-status buf) :approval)
                   (notify-buffer-display-change buf :approval)
                   (return)))  ; exit loop, wait for user
                ;; Tool is auto-approved: execute immediately
                (t
                 (let* ((*current-caller* agent-kw)
                        (*current-tool-buffer* buf)
                        (result-text
                          (handler-case
                              (execute-tool tool-name tool-input)
                            (error (e)
                              (tool-error-result-data e)))))
                   (push `((:result . ,result-text)
                           (:display . ,(format-tool-result-display tool-name result-text))
                           (:tool-id . ,tool-id))
                         (buffer-tool-call-results buf)))
                 (pop (buffer-pending-tool-calls buf)))))
    ;; If no more pending tools, finalize
    (unless (buffer-pending-tool-calls buf)
      (finalize-tool-results buf))))

(defun handle-approval-response (buf response)
  "Handle user's approval decision for the current pending tool.
RESPONSE is :approve, :deny, or (:deny-with-message . \"reason\")."
  (let* ((approval (buffer-approval-pending buf))
         (tool-name (cdr (assoc :tool-name approval)))
         (tool-id (cdr (assoc :tool-id approval)))
         (tool-input (cdr (assoc :tool-input approval)))
         (agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword)))
    ;; Clear approval state
    (setf (buffer-approval-pending buf) nil)
    (cond
      ;; Approved: execute the tool
      ((eq response :approve)
       (approval-policy-record-history-entry
        buf tool-name :approved
        :policy :interactive
        :reason "User approved interactive tool request"
        :entry tool-input)
       (let* ((*current-caller* agent-kw)
              (*current-tool-buffer* buf)
              (result-text
                (handler-case
                    (execute-tool tool-name tool-input)
                  (error (e)
                    (tool-error-result-data e)))))
         (push `((:result . ,result-text)
                 (:display . ,(format-tool-result-display tool-name result-text))
                 (:tool-id . ,tool-id))
               (buffer-tool-call-results buf))))
      ;; Denied with message
      ((and (consp response) (eq (car response) :deny-with-message))
       (let ((reason (cdr response)))
         (approval-policy-record-history-entry
          buf tool-name :denied
          :policy :interactive
          :reason (or reason "User denied this tool call")
          :entry tool-input)
         (push `((:result . ,(tool-denied-result-data
                              (or reason "User denied this tool call")))
                 (:display . ,(format nil "[~A DENIED: ~A]" tool-name (or reason "denied")))
                 (:tool-id . ,tool-id))
               (buffer-tool-call-results buf))))
      ;; Denied (no message)
      (t
       (approval-policy-record-history-entry
        buf tool-name :denied
        :policy :interactive
        :reason "User denied"
        :entry tool-input)
       (push `((:result . ,(tool-denied-result-data "User denied"))
               (:display . ,(format nil "[~A DENIED]" tool-name))
               (:tool-id . ,tool-id))
             (buffer-tool-call-results buf))))
    ;; Move to next tool
    (pop (buffer-pending-tool-calls buf))
    ;; Restore stashed input
    (when (buffer-stashed-input buf)
      (set-message-text (buffer-input-message buf) (buffer-stashed-input buf))
      (setf (buffer-stashed-input buf) nil))
    (notify-buffer-display-change buf :approval)
    ;; Continue with remaining tools
    (advance-tool-approval buf)))

(defun insert-tool-results-message (buf results)
  "Insert RESULTS as a tool-result message before BUF's input message.
RESULTS is a chronological list of alists containing :RESULT, :DISPLAY,
and :TOOL-ID entries. Returns the inserted message."
  (let* ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (display-parts (mapcar (lambda (r) (cdr (assoc :display r))) results))
         (result-blocks (mapcar (lambda (r)
                                  `((:type . "tool_result")
                                    (:tool--use--id . ,(cdr (assoc :tool-id r)))
                                    (:content . ,(cdr (assoc :result r)))))
                                results))
         (canonical-result-blocks (canonicalize-message-content "user" result-blocks))
         (display-text (format nil "~{~A~^~%~}" display-parts)))
    (buffer-insert-read-only-message
     buf
     :tool-result
     display-text
     :raw-content canonical-result-blocks
     :face-set (gethash agent-kw (buffer-face-registry buf)))))

(defun finalize-tool-results (buf)
  "Insert the accumulated tool results as a message and continue the conversation."
  (let ((results (nreverse (buffer-tool-call-results buf))))
    (insert-tool-results-message buf results)
    ;; Clear tool call state
    (setf (buffer-tool-call-results buf) nil
          (buffer-pending-tool-calls buf) nil)
    ;; Restore stashed input if still stashed
    (when (buffer-stashed-input buf)
      (set-message-text (buffer-input-message buf) (buffer-stashed-input buf))
      (setf (buffer-stashed-input buf) nil))
    (notify-buffer-display-change buf :tool-results)
    ;; Continue: inject any queued steering before the next provider turn.
    (or (deliver-next-buffer-steering-message buf)
        (start-streaming-response buf))))

(defun queued-buffer-message-text (entry)
  "Return ENTRY's normalized queued message text, or NIL when blank."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (or (getf entry :text) ""))))
    (unless (blank-string-p text)
      text)))

(defun deliver-buffer-queued-message (buf entry)
  "Finalize ENTRY into BUF as the next user turn and continue the agent run."
  (let ((text (queued-buffer-message-text entry)))
    (when text
      (run-hook-with-args '*before-send-message-hook* buf text)
      (set-message-text (buffer-input-message buf) text)
      (buffer-finalize-input buf)
      (setf (message-face-set (buffer-input-message buf))
            (gethash :user (buffer-face-registry buf)))
      (let ((result (send-to-agent-with-context buf)))
        (run-hook-with-args '*after-send-message-hook* buf text result)
        result))))

(defun deliver-next-buffer-steering-message (buf)
  "Deliver the next queued steering message for BUF, if any."
  (let ((entry (dequeue-buffer-steering-message buf)))
    (when entry
      (deliver-buffer-queued-message buf entry)
      t)))

(defun deliver-next-buffer-follow-up-message (buf)
  "Deliver the next queued follow-up message for BUF, if any."
  (let ((entry (dequeue-buffer-follow-up-message buf)))
    (when entry
      (deliver-buffer-queued-message buf entry)
      t)))

(defun start-streaming-response (buf)
  "Start a streaming API call. Creates a placeholder agent message and
stores the stream state on the buffer. Non-blocking -- the event loop
polls for updates via update-streaming-response."
  (load-active-packages :buffer buf)
  (maybe-compact-buffer buf :reason :pre-request)
  (let* ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (tools (let ((*current-caller* agent-kw))
                   (tool-definitions-for-api :buffer buf)))
         (messages (build-conversation-messages buf))
         (system-prompt (let ((*current-caller* agent-kw))
                          (build-agent-system-prompt (buffer-agent-name buf)
                                                     :buffer buf))))
    (handler-case
        (multiple-value-bind (provider model think-level)
            (resolve-buffer-provider-and-model buf)
          (let ((service-tier (resolve-buffer-service-tier buf)))
          ;; Debug: echo the outgoing request payload before sending
          (let ((req-json (api-json-encode
                           `((:messages . ,(coerce messages 'vector))
                             ,@(when think-level
                                 `((:reasoning . ((:effort . ,think-level)))))
                             ,@(when service-tier
                                 `((:service_tier . ,service-tier)))
                             ,@(when (and tools (plusp (length tools)))
                                 `((:tools . ,tools)))))))
            (debug-log buf
              (format nil "[API REQUEST → ~(~A~)/~A~@[ think=~A~]~@[ tier=~A~]  msg:~D  tools:~D]~%~A"
                      provider model
                      think-level
                      service-tier
                      (length messages)
                      (if tools (length tools) 0)
                      req-json))
            (file-debug-log "api-request"
                            "provider=~(~A~) model=~A think=~A tier=~A msgs=~D tools=~D payload=~A"
                            provider model
                            (or think-level "default")
                            (or service-tier "auto")
                            (length messages)
                            (if tools (length tools) 0)
                            req-json))
          (let* ((request-args (list :model model
                                     :tools tools
                                     :system-prompt system-prompt
                                     :reasoning-effort think-level))
                 (_ignored (when service-tier
                             (setf request-args
                                   (append request-args
                                           (list :service-tier service-tier)))))
                 (state (apply #'provider-request-streaming
                               provider
                               messages
                               (lambda (s) (declare (ignore s)))
                               request-args))
                 ;; Create placeholder message that will be updated as tokens arrive.
                 ;; It becomes durable only when the stream completes or errors.
                 (agent-msg (buffer-insert-agent-message
                             buf ""
                             :record-p nil
                             :run-hook-p nil)))
          (declare (ignore _ignored))
          (put-message-metadata agent-msg
                                :agent (buffer-agent-name buf)
                                :provider provider
                                :model model
                                :think-level think-level
                                :service-tier service-tier
                                :reasoning-summary-mode
                                (and (eq provider :openai-codex)
                                     *openai-codex-reasoning-summary*))
          (setf (message-face-set agent-msg)
                (gethash agent-kw (buffer-face-registry buf)))
          (setf (buffer-pending-stream buf) state
                (buffer-streaming-message buf) agent-msg
                (buffer-status buf) :thinking)
          (notify-buffer-display-change buf :stream-started))))
      (error (e)
        (setf (buffer-status buf) :error)
        (let ((err-msg (buffer-insert-agent-message
                         buf (format nil "[Error: ~A]" e))))
          (setf (message-face-set err-msg)
                (gethash agent-kw (buffer-face-registry buf))))
        (notify-buffer-display-change buf :status)))))

(defun latest-text-block-text (content-blocks)
  "Return the text of the last text block in CONTENT-BLOCKS, or NIL."
  (let ((latest nil))
    (dolist (block content-blocks latest)
      (when (string= "text" (content-block-type block))
        (setf latest (or (cdr (assoc :text block)) ""))))))

(defun stream-state-display-text (state &key show-reasoning-p)
  "Return STATE's in-progress text without double-counting accumulators.
Some providers keep the current partial text only in STREAM-STATE-TEXT, while
OpenAI-compatible providers also mirror it into CONTENT-BLOCKS on every delta."
  (bt:with-lock-held ((stream-state-lock state))
    (let* ((accumulator (stream-state-text state))
           (content-blocks (reverse (copy-list (stream-state-content-blocks state))))
           (content-text (content-text-blocks content-blocks))
           (reasoning-text
             (and show-reasoning-p
                  (display-text-from-line-strings
                   (reasoning-block-display-lines
                    (content-reasoning-blocks content-blocks)
                    :visible-text content-text))))
           (latest-text (latest-text-block-text content-blocks)))
      (cond
        ((and reasoning-text (not (blank-string-p reasoning-text)))
         (if (blank-string-p content-text)
             reasoning-text
             (format nil "~A~%~A" content-text reasoning-text)))
        ((zerop (length accumulator))
         content-text)
        ((and latest-text (string= latest-text accumulator))
         content-text)
        (t
         (concatenate 'string content-text accumulator))))))

(defun cancelled-stream-content-blocks (state)
  "Return cancellable response content, excluding unfinished tool calls."
  (remove-if (lambda (block)
               (string= "tool_use" (content-block-type block)))
             (stream-state-final-content-blocks state)))

(defun finalize-cancelled-streaming-response (buf state msg)
  "Finalize MSG after STATE is stopped by the user."
  (let* ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (content-blocks (cancelled-stream-content-blocks state))
         (canonical-content
           (canonicalize-message-content "assistant" content-blocks))
         (final-text
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (content-text-blocks canonical-content)))
         (usage
           (bt:with-lock-held ((stream-state-lock state))
             (copy-list (stream-state-usage state)))))
    (put-message-metadata
     msg
     :agent (buffer-agent-name buf)
     :stop-reason "cancelled"
     :content-block-count (length content-blocks)
     :tool-call-count 0
     :reasoning-block-count (length (content-reasoning-blocks
                                      canonical-content)))
    (when usage
      (apply #'put-message-metadata
             msg
             (token-usage-metadata-pairs usage)))
    (if (blank-string-p final-text)
        (progn
          (setf (message-sender msg) :system
                (message-raw-content msg) nil
                (message-face-set msg) (gethash :system
                                                (buffer-face-registry buf)))
          (set-message-text msg "[Response stopped by user]"))
        (progn
          (setf (message-face-set msg)
                (gethash agent-kw (buffer-face-registry buf))
                (message-raw-content msg) canonical-content)
          (set-message-text msg
                            (format nil "~A~%[Stopped by user]"
                                    final-text))))
    (record-buffer-message buf msg)
    (setf (buffer-pending-stream buf) nil
          (buffer-streaming-message buf) nil
          (buffer-status buf) :idle)
    (notify-buffer-display-change buf :stream-cancelled)
    nil))

(defun stop-streaming-response (buf)
  "Stop BUF's active LLM stream, preserving any partial response text."
  (let ((state (and buf (buffer-pending-stream buf))))
    (when state
      (cancel-stream-state state)
      (update-streaming-response buf)
      t)))

(defun update-streaming-response (buf)
  "Poll the active streaming response and update the display.
Returns T if still streaming, NIL if done."
  (let ((state (buffer-pending-stream buf))
        (msg (buffer-streaming-message buf)))
    (unless (and state msg)
      (return-from update-streaming-response nil))
    ;; Read state under lock
    (let ((done (bt:with-lock-held ((stream-state-lock state))
                  (stream-state-done-p state)))
          (err (bt:with-lock-held ((stream-state-lock state))
                 (stream-state-error-p state)))
          (cancelled (bt:with-lock-held ((stream-state-lock state))
                       (stream-state-cancelled-p state))))
      ;; While streaming: update display with in-progress text
      ;; (stream-state-text accumulates the CURRENT block's text;
      ;; completed blocks have their text finalized in content-blocks)
      (unless done
        (let ((all-text (stream-state-display-text
                         state
                         :show-reasoning-p (buffer-show-reasoning-p buf))))
          (when (plusp (length all-text))
            (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     all-text)))
              (unless (string= text (message-text msg))
                (set-message-text msg text)
                (notify-buffer-display-change buf :streaming))))))
      (cond
        ;; User-cancelled streams are normal stops, not provider errors.
        (cancelled
         (finalize-cancelled-streaming-response buf state msg))
        ;; Error during streaming
        (err
         (set-message-text msg (format nil "[Streaming error: ~A]" err))
         (record-buffer-message buf msg)
         (setf (buffer-pending-stream buf) nil
               (buffer-streaming-message buf) nil
               (buffer-status buf) :error)
         (notify-buffer-display-change buf :stream-error)
         nil)
        ;; Streaming complete
        (done
         (let* ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
                 (content-blocks
                   (bt:with-lock-held ((stream-state-lock state))
                     (nreverse (copy-list (stream-state-content-blocks state)))))
                 (canonical-content (canonicalize-message-content "assistant" content-blocks))
                 (stop-reason
                   (bt:with-lock-held ((stream-state-lock state))
                     (stream-state-stop-reason state)))
                 (usage
                   (bt:with-lock-held ((stream-state-lock state))
                     (copy-list (stream-state-usage state))))
                 (tool-uses (content-tool-use-blocks canonical-content))
                 (final-text (content-text-blocks canonical-content)))
            ;; Build final display from content-blocks (not the accumulator)
            (let ((display (with-output-to-string (s)
                             (write-string final-text s)
                             (dolist (tu tool-uses)
                               (write-char #\Newline s)
                               (write-string (format-tool-call-display tu) s)))))
              (set-message-text msg (string-trim '(#\Space #\Tab #\Newline #\Return) display))
              (setf (message-raw-content msg) canonical-content))
            (put-message-metadata
             msg
             :stop-reason (or stop-reason "nil")
             :content-block-count (length content-blocks)
             :tool-call-count (length tool-uses)
             :reasoning-block-count (length (content-reasoning-blocks
                                              canonical-content)))
            (when usage
              (apply #'put-message-metadata
                     msg
                     (token-usage-metadata-pairs usage)))
            (record-buffer-message buf msg)
            ;; Debug: echo the completed response
            (let ((resp-json (api-json-encode (coerce canonical-content 'vector)))
                  (usage-line (format-token-usage-summary usage)))
              (debug-log buf
                (format nil "[API RESPONSE  stop:~A  blocks:~D~@[  ~A~]]~%~A"
                        (or stop-reason "nil")
                        (length content-blocks)
                        usage-line
                        resp-json))
              (file-debug-log "api-response"
                              "stop=~A blocks=~D~@[ ~A~] content=~A"
                              (or stop-reason "nil")
                              (length content-blocks)
                              usage-line
                              resp-json))
            (let ((metadata (message-metadata msg)))
              (maybe-run-hook-with-args
               '*after-provider-response-hook*
               buf
               canonical-content
               (message-metadata-value metadata :provider)
               (message-metadata-value metadata :model)
               usage))
            ;; Clear streaming state
            (setf (buffer-pending-stream buf) nil
                  (buffer-streaming-message buf) nil)
            (notify-buffer-display-change buf :stream-complete)
           ;; Handle tool calls
            (if (and (string= "tool_use" (or stop-reason ""))
                     tool-uses)
                (progn
                  (begin-tool-approval buf tool-uses)
                  t)
               (or (deliver-next-buffer-steering-message buf)
                   (deliver-next-buffer-follow-up-message buf)
                   (progn
                     (setf (buffer-status buf) :idle)
                     (notify-buffer-display-change buf :status)
                     nil)))))
        ;; Still streaming
        (t t)))))

(defun update-openai-oauth-login ()
  "Poll the active OpenAI OAuth login flow and surface its result in the buffer."
  (let ((flow *openai-oauth-pending*))
    (unless flow
      (return-from update-openai-oauth-login nil))
    (let* ((snapshot (openai-oauth-flow-snapshot flow))
           (buf (openai-oauth-flow-buffer flow)))
      (unless (getf snapshot :done-p)
        (return-from update-openai-oauth-login t))
      (setf *openai-oauth-pending* nil)
      (when buf
        (setf (buffer-status buf) :idle)
        (buffer-insert-system-message
         buf
         (cond
           ((getf snapshot :success-p)
            "[OpenAI Codex OAuth: Login successful. Credentials saved to ~/.codex/auth.json.]")
           ((getf snapshot :cancelled-p)
            "[OAuth cancelled]")
           (t
            (format nil "[OAuth error: ~A]"
                    (or (getf snapshot :error) "Unknown error"))))))
      nil)))

(declaim (ftype (function (buffer) buffer) send-to-agent-with-context))
(defun send-to-agent-with-context (buf)
  "Start a streaming conversation with the LLM. Non-blocking --
the event loop polls for updates via update-streaming-response."
  (setf (buffer-status buf) :thinking)
  (notify-buffer-display-change buf :status)
  (start-streaming-response buf)
  buf)

;;; --------------------------------------------------------------------------
;;; Non-interactive Prompt Mode
;;; --------------------------------------------------------------------------

(defun make-prompt-buffer
    (prompt agent-name &key
                        (session-persistence-mode
                         *default-buffer-session-persistence-mode*))
  "Create a buffer seeded with PROMPT as the only finalized user message."
  (let ((buf (make-chat-buffer "clawmacs:prompt"
                               :agent-name agent-name
                               :working-directory (truename ".")
                               :session-persistence-mode
                               session-persistence-mode)))
    (set-message-text (buffer-input-message buf) prompt)
    (buffer-finalize-input buf)
    buf))

(defun make-empty-session-prompt-buffer (session-name agent-name)
  "Create an empty prompt-mode buffer attached to SESSION-NAME."
  (let ((buf (make-chat-buffer session-name
                               :agent-name agent-name
                               :working-directory (truename "."))))
    (autosave-session-snapshot buf)
    buf))

(defun make-session-prompt-buffer (session-name agent-name)
  "Load SESSION-NAME for prompt mode, or create it when missing."
  (let ((buf (or (load-session session-name :agent-name agent-name)
                 (make-empty-session-prompt-buffer session-name agent-name))))
    (initialize-buffer-display-defaults buf)
    buf))

(defun append-session-prompt-input (buf prompt)
  "Append PROMPT as BUF's next finalized user message."
  (set-message-text (buffer-input-message buf) prompt)
  (buffer-finalize-input buf)
  buf)

(defun prompt-cache-key-for-buffer (buf)
  "Return a stable OpenAI prompt cache routing key for BUF."
  (format nil "clawmacs-~(~A~)-~(~A~)"
          (buffer-agent-name buf)
          (session-name-hash (buffer-name buf))))

(defun maybe-apply-prompt-routing-overrides
    (buf provider model think-level &key model-role service-tier)
  "Apply optional routing overrides to BUF."
  (when provider
    (set-buffer-provider-override buf (normalize-provider provider)))
  (when model
    (set-buffer-model-override buf model))
  (when think-level
    (set-buffer-think-level-override buf think-level))
  (when model-role
    (set-buffer-model-role-override buf model-role))
  (when service-tier
    (set-buffer-service-tier-override buf service-tier))
  buf)

(defun prompt-stream-state-response (state)
  "Convert a completed streaming STATE into a canonical response."
  (bt:with-lock-held ((stream-state-lock state))
    (when (stream-state-error-p state)
      (error "Streaming error: ~A" (stream-state-error-p state)))
    (canonical-response
     (or (stream-state-stop-reason state) "end_turn")
     (nreverse (copy-list (stream-state-content-blocks state)))
     :usage (copy-list (stream-state-usage state)))))

(defun wait-for-prompt-stream-state (state)
  "Block until streaming STATE completes, then return its canonical response."
  (loop
    (when (bt:with-lock-held ((stream-state-lock state))
            (stream-state-done-p state))
      (return (prompt-stream-state-response state)))
    (sleep 0.02)))

(defun prompt-request-once (buf)
  "Send BUF's current conversation once via the streaming provider path.
Returns values RESPONSE, PROVIDER, MODEL, THINK-LEVEL."
  (load-active-packages :buffer buf)
  (maybe-compact-buffer buf :reason :prompt-request)
  (let* ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (tools (let ((*current-caller* agent-kw))
                  (tool-definitions-for-api :buffer buf)))
         (messages (build-conversation-messages buf))
         (system-prompt (let ((*current-caller* agent-kw))
                          (build-agent-system-prompt (buffer-agent-name buf)
                                                     :buffer buf))))
    (multiple-value-bind (provider model think-level)
        (resolve-buffer-provider-and-model buf)
      (let ((service-tier (resolve-buffer-service-tier buf))
            (prompt-cache-key (and (eq provider :openai-codex)
                                   (prompt-cache-key-for-buffer buf)))
            (prompt-cache-retention nil))
        (file-debug-log "prompt-request"
                        "provider=~(~A~) model=~A think=~A tier=~A msgs=~D tools=~D cache-key=~A cache-retention=~A"
                        provider model (or think-level "default")
                        (or service-tier "auto")
                        (length messages)
                        (if tools (length tools) 0)
                        (or prompt-cache-key "none")
                        (or prompt-cache-retention "none"))
        (let ((*openai-codex-prompt-cache-key* prompt-cache-key)
              (*openai-codex-prompt-cache-retention*
                prompt-cache-retention))
          (let* ((request-args (list :model model
                                     :tools tools
                                     :system-prompt system-prompt
                                     :reasoning-effort think-level))
                 (_ignored (when service-tier
                             (setf request-args
                                   (append request-args
                                           (list :service-tier service-tier)))))
                 (state (apply #'provider-request-streaming
                               provider messages
                               (lambda (state) (declare (ignore state)))
                               request-args))
                 (response (wait-for-prompt-stream-state state)))
            (declare (ignore _ignored))
            (maybe-run-hook-with-args
             '*after-provider-response-hook*
             buf
             response
             provider
             model
             (response-usage response))
            (values response
                    provider
                    model
                    think-level
                    service-tier)))))))

(defun denied-tool-result-data (reason)
  "Return a Lisp data denial payload for a non-interactive tool denial."
  (tool-denied-result-data reason))

(defun execute-prompt-tool-call (buf tool-use-block agent-kw auto-approve-tools-p)
  "Execute TOOL-USE-BLOCK for prompt mode and return values RESULT and EVENT.
RESULT is the alist consumed by INSERT-TOOL-RESULTS-MESSAGE. EVENT is a
PROMPT-TOOL-EVENT for terminal/debug output."
  (let* ((tool-name (cdr (assoc :name tool-use-block)))
         (tool-input (cdr (assoc :input tool-use-block)))
         (tool-id (cdr (assoc :id tool-use-block)))
         (requires-approval-p (tool-requires-permission-p tool-name
                                                          :buffer buf))
         (denied-p (and requires-approval-p
                        (not auto-approve-tools-p)))
         (result-text
           (if denied-p
               (progn
                 (approval-policy-record-history-entry
                  buf tool-name :denied
                  :policy :prompt-mode
                  :reason "Prompt mode denied interactive approval"
                  :entry tool-input)
                 (denied-tool-result-data
                  "Tool requires interactive approval; prompt mode denied it."))
               (let ((*current-caller* agent-kw)
                     (*current-tool-buffer* buf))
                 (handler-case
                     (execute-tool tool-name tool-input)
                   (error (e)
                     (tool-error-result-data e))))))
         (display (if denied-p
                      (format nil "[~A DENIED: non-interactive prompt mode]"
                              tool-name)
                      (format-tool-result-display tool-name result-text)))
         (result `((:result . ,result-text)
                   (:display . ,display)
                   (:tool-id . ,tool-id)))
         (event (make-prompt-tool-event
                 :id tool-id
                 :name tool-name
                 :input tool-input
                 :result-text result-text
                 :display display
                 :denied-p denied-p)))
    (values result event)))

(defun execute-prompt-tool-calls (buf tool-uses auto-approve-tools-p)
  "Execute TOOL-USES, insert their tool-result message into BUF, and return events."
  (let ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
        (results nil)
        (events nil))
    (dolist (tool-use tool-uses)
      (multiple-value-bind (result event)
          (execute-prompt-tool-call buf tool-use agent-kw auto-approve-tools-p)
        (push result results)
        (push event events)))
    (insert-tool-results-message buf (nreverse results))
    (nreverse events)))

(defun run-prompt-buffer-loop (buf prompt max-tool-iterations
                               auto-approve-tools-p)
  "Run BUF through prompt-mode provider/tool iterations for PROMPT."
  (let ((tool-events nil)
        (final-provider nil)
        (final-model nil)
        (final-think-level nil)
        (final-service-tier nil)
        (aggregate-usage nil)
        (iterations 0))
    (labels ((fail (format-string &rest format-args)
               (error 'prompt-run-error
                      :message (apply #'format nil format-string format-args)
                      :tool-events tool-events
                      :iterations iterations
                      :provider final-provider
                      :model final-model
                      :think-level final-think-level)))
      (loop
        (when (>= iterations max-tool-iterations)
          (fail "Exceeded maximum tool iterations (~D)"
                max-tool-iterations))
        (incf iterations)
        (multiple-value-bind (response provider* model* think-level* service-tier*)
            (handler-case
                (prompt-request-once buf)
              (error (condition)
                (fail "Prompt provider request failed: ~A" condition)))
          (setf final-provider provider*
                final-model model*
                final-think-level think-level*
                final-service-tier service-tier*)
          (setf aggregate-usage
                (merge-token-usage aggregate-usage
                                   (response-usage response)))
          (let* ((content-blocks (response-content response))
                 (canonical-content (canonicalize-message-content
                                     "assistant"
                                     content-blocks))
                 (tool-uses (content-tool-use-blocks canonical-content))
                 (stop-reason (response-stop-reason response))
                 (agent-kw (intern (string-upcase (buffer-agent-name buf))
                                   :keyword)))
            (insert-agent-message-from-content buf canonical-content agent-kw)
            (if tool-uses
                (setf tool-events
                      (append tool-events
                              (handler-case
                                  (execute-prompt-tool-calls
                                   buf tool-uses auto-approve-tools-p)
                                (error (condition)
                                  (fail "Prompt tool loop failed: ~A"
                                        condition)))))
                (return
                  (make-prompt-run-result
                   :prompt prompt
                   :final-text (content-text-blocks canonical-content)
                   :tool-events tool-events
                   :reasoning-blocks (content-reasoning-blocks
                                      canonical-content)
                   :agent-name (buffer-agent-name buf)
                   :provider final-provider
                   :model final-model
                   :think-level final-think-level
                   :service-tier final-service-tier
                   :iterations iterations
                   :stop-reason stop-reason
                   :usage aggregate-usage)))))))))

(defun run-prompt-with-buffer (buf prompt custom-tool-definitions
                               max-tool-iterations auto-approve-tools-p
                               tool-names tool-names-supplied-p)
  "Run a prepared prompt BUF with optional custom tools."
  (let* ((temporary-tool-table
           (temporary-tool-table-from-definitions custom-tool-definitions))
         (effective-tool-names
           (resolve-prompt-tool-names (buffer-agent-name buf)
                                      custom-tool-definitions
                                      tool-names
                                      tool-names-supplied-p)))
    (let ((*active-tool-names* effective-tool-names)
          (*temporary-tool-table* (or temporary-tool-table
                                      *temporary-tool-table*)))
      (run-prompt-buffer-loop buf
                              prompt
                              max-tool-iterations
                              auto-approve-tools-p))))

(defun run-single-prompt (prompt &key (agent-name *default-agent-name*)
                                 provider model think-level
                                 model-role service-tier
                                 (session-persistence-mode
                                  *default-buffer-session-persistence-mode*)
                                 (max-tool-iterations *prompt-max-tool-iterations*)
                                 auto-approve-tools-p
                                 package-names
                                 (tool-names nil tool-names-supplied-p)
                                 custom-tools)
  "Run PROMPT once without a UI and return a PROMPT-RUN-RESULT.
The request loops through tool_use responses until the provider returns a final
assistant response or MAX-TOOL-ITERATIONS is exceeded."
  (when (blank-string-p prompt)
    (error "Prompt must be non-empty"))
  (let* ((custom-tool-definitions (normalize-run-custom-tools custom-tools))
         (buf (make-prompt-buffer
               prompt agent-name
               :session-persistence-mode session-persistence-mode)))
    (maybe-apply-prompt-routing-overrides buf provider model think-level
                                          :model-role model-role
                                          :service-tier service-tier)
    (when package-names
      (setf (buffer-enabled-packages buf)
            (normalize-package-name-list package-names)))
    (run-prompt-with-buffer buf
                            prompt
                            custom-tool-definitions
                            max-tool-iterations
                            auto-approve-tools-p
                            tool-names
                            tool-names-supplied-p)))

(defun run-session-prompt (prompt &key session-name
                                  (agent-name *default-agent-name*)
                                  provider model think-level
                                  model-role service-tier
                                  (max-tool-iterations *prompt-max-tool-iterations*)
                                  auto-approve-tools-p
                                  package-names
                                  (tool-names nil tool-names-supplied-p)
                                  custom-tools)
  "Append PROMPT to SESSION-NAME, run the agent, and save the session."
  (when (blank-string-p prompt)
    (error "Prompt must be non-empty"))
  (unless (and (stringp session-name)
               (not (blank-string-p session-name)))
    (error "Session prompt mode requires a non-empty session name"))
  (let* ((custom-tool-definitions (normalize-run-custom-tools custom-tools))
         (buf (make-session-prompt-buffer session-name agent-name)))
    (maybe-apply-prompt-routing-overrides buf provider model think-level
                                          :model-role model-role
                                          :service-tier service-tier)
    (when package-names
      (setf (buffer-enabled-packages buf)
            (normalize-package-name-list package-names)))
    (append-session-prompt-input buf prompt)
    (run-prompt-with-buffer buf
                            prompt
                            custom-tool-definitions
                            max-tool-iterations
                            auto-approve-tools-p
                            tool-names
                            tool-names-supplied-p)))
