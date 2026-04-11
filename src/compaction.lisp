(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Conversation compaction
;;; --------------------------------------------------------------------------

(defvar *compaction-point* 9/10
  "Automatic compaction threshold.
NIL disables automatic compaction.  A real number from 0 to 1 means that
fraction of BUFFER-CONTEXT-LIMIT.  An integer greater than 1 means an absolute
token estimate.  A function is called as (FUNCTION BUFFER ESTIMATE LIMIT) and
may return NIL, T, a ratio, or an absolute token estimate.")

(defvar *compaction-function* nil
  "Function used by MAYBE-COMPACT-BUFFER.
The function is called as (FUNCTION BUFFER :REASON REASON) and should return
non-NIL when it compacted the buffer.")

(defvar *compaction-prompt*
  "You are performing a CONTEXT CHECKPOINT COMPACTION. Create a handoff summary for another LLM that will resume the task.

Include:
- Current progress and key decisions made
- Important context, constraints, or user preferences
- What remains to be done (clear next steps)
- Any critical data, examples, or references needed to continue

Be concise, structured, and focused on helping the next LLM seamlessly continue the work."
  "Prompt sent to the active provider when summarizing old conversation history.")

(defvar *compaction-summary-prefix*
  "Another language model started to solve this problem and produced a summary of its thinking process. You also have access to the state of the tools that were used by that language model. Use this to build on the work that has already been done and avoid duplicating work. Here is the summary produced by the other language model, use the information in this summary to assist with your own analysis:"
  "Prefix inserted before generated compaction summaries.")

(defvar *compaction-preserved-user-message-token-limit* 20000
  "Approximate token budget for exact recent user messages kept after compaction.")

(defun compaction-token-estimate-for-string (text)
  "Return a deterministic rough token estimate for TEXT."
  (let ((length (length (or text ""))))
    (if (zerop length)
        0
        (ceiling length 4))))

(defun compaction-json-estimate-text (value)
  "Return VALUE as stable text for token estimation."
  (handler-case
      (api-json-encode value)
    (error ()
      (prin1-to-string value))))

(defun compaction-provider-message-token-estimate (message)
  "Return a rough token estimate for a provider MESSAGE alist."
  (+ 4
     (compaction-token-estimate-for-string
      (or (cdr (assoc :role message)) ""))
     (compaction-token-estimate-for-string
      (compaction-json-estimate-text (cdr (assoc :content message))))))

(defun compaction-provider-messages-token-estimate (messages &key system-prompt)
  "Return a rough token estimate for provider MESSAGES and SYSTEM-PROMPT."
  (+ (compaction-token-estimate-for-string system-prompt)
     (loop :for message :in messages
           :sum (compaction-provider-message-token-estimate message))))

(defun compaction-text-provider-message (role text)
  "Return a canonical provider message with ROLE and TEXT."
  `((:role . ,role)
    (:content . ,(coerce (canonicalize-message-content role text) 'vector))))

(defun buffer-conversation-token-estimate (buf &key include-current-input-p)
  "Estimate model-visible tokens in BUF's current conversation."
  (let ((messages (build-conversation-messages buf)))
    (when include-current-input-p
      (let ((input-text (message-text (buffer-input-message buf))))
        (unless (blank-string-p input-text)
          (setf messages
                (append messages
                        (list (compaction-text-provider-message "user"
                                                                input-text)))))))
    (compaction-provider-messages-token-estimate
     messages
     :system-prompt (build-agent-system-prompt (buffer-agent-name buf)))))

(defun normalize-compaction-point-value (value limit estimate)
  "Normalize VALUE into an absolute token threshold or NIL."
  (cond
    ((null value) nil)
    ((eq value t) estimate)
    ((and (integerp value) (plusp value))
     value)
    ((and (realp value) (> value 0) (<= value 1))
     (max 1 (floor (* limit value))))
    ((and (realp value) (> value 1))
     (round value))
    (t
     (error "Invalid compaction point: ~S" value))))

(defun compaction-threshold-tokens (buf &key estimate)
  "Return BUF's automatic compaction threshold in estimated tokens, or NIL."
  (let* ((limit (max 1 (buffer-context-limit buf)))
         (estimate (or estimate (buffer-conversation-token-estimate buf)))
         (point (if (functionp *compaction-point*)
                    (funcall *compaction-point* buf estimate limit)
                    *compaction-point*)))
    (normalize-compaction-point-value point limit estimate)))

(defun compaction-needed-p (buf &key include-current-input-p)
  "Return values NEEDED-P, ESTIMATE, and THRESHOLD for BUF."
  (let* ((estimate (buffer-conversation-token-estimate
                    buf
                    :include-current-input-p include-current-input-p))
         (threshold (compaction-threshold-tokens buf :estimate estimate)))
    (values (and threshold (>= estimate threshold))
            estimate
            threshold)))

(defun compaction-summary-text-p (text)
  "Return true when TEXT appears to be a generated compaction summary."
  (let ((prefix (format nil "~A~%" *compaction-summary-prefix*)))
    (and text
         (<= (length prefix) (length text))
         (string= prefix text :end2 (length prefix)))))

(defun compaction-message-visible-p (msg)
  "Return true when MSG participates in provider-visible conversation history."
  (not (eq (message-sender msg) :system)))

(defun collect-recent-user-message-snapshots (buf token-limit)
  "Collect exact recent user messages from BUF within TOKEN-LIMIT."
  (let ((remaining (max 0 token-limit))
        (snapshots nil))
    (when (plusp remaining)
      (loop :for msg := (message-prev (buffer-input-message buf))
              :then (message-prev msg)
            :while msg
            :for text := (message-text msg)
            :when (and (eq (message-sender msg) :user)
                       (not (compaction-summary-text-p text)))
              :do (let ((tokens (compaction-token-estimate-for-string text)))
                    (cond
                      ((<= tokens remaining)
                       (push (list :sender :user
                                   :text text
                                   :timestamp (message-timestamp msg)
                                   :raw-content (copy-tree
                                                 (message-raw-content msg)))
                             snapshots)
                       (decf remaining tokens))
                      ((null snapshots)
                       (push (list :sender :user
                                   :text text
                                   :timestamp (message-timestamp msg)
                                   :raw-content (copy-tree
                                                 (message-raw-content msg)))
                             snapshots)
                       (return))
                      (t
                       (return))))))
    snapshots))

(defun compaction-summary-with-prefix (summary)
  "Return SUMMARY with the configured compaction prefix."
  (format nil "~A~%~A"
          *compaction-summary-prefix*
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (or summary ""))))

(defun buffer-insert-read-only-message (buf sender text &key raw-content timestamp)
  "Insert a read-only message before BUF's input message."
  (let* ((msg (make-message sender :read-only-p t))
         (input (buffer-input-message buf))
         (before-input (message-prev input)))
    (set-message-text msg (or text ""))
    (setf (message-timestamp msg) (or timestamp (get-universal-time))
          (message-raw-content msg) raw-content
          (message-face-set msg)
          (or (gethash sender (buffer-face-registry buf))
              (and (eq sender :compaction-summary)
                   (gethash :system (buffer-face-registry buf)))))
    (setf (message-prev msg) before-input
          (message-next msg) input
          (message-prev input) msg)
    (if before-input
        (setf (message-next before-input) msg)
        (setf (buffer-first-message buf) msg))
    msg))

(defun clear-buffer-history-before-input (buf)
  "Remove all finalized history messages from BUF, preserving the input message."
  (let ((input (buffer-input-message buf)))
    (setf (message-prev input) nil
          (buffer-first-message buf) input))
  buf)

(defun replace-buffer-with-compacted-history (buf summary-text user-snapshots)
  "Replace BUF history with SUMMARY-TEXT followed by USER-SNAPSHOTS."
  (clear-buffer-history-before-input buf)
  (buffer-insert-read-only-message
   buf
   :compaction-summary
   summary-text)
  (dolist (snapshot user-snapshots)
    (buffer-insert-read-only-message
     buf
     (getf snapshot :sender)
     (getf snapshot :text)
     :raw-content (getf snapshot :raw-content)
     :timestamp (getf snapshot :timestamp)))
  buf)

(defun compaction-stream-state-response (state)
  "Convert a completed streaming STATE into a canonical response."
  (bt:with-lock-held ((stream-state-lock state))
    (when (stream-state-error-p state)
      (error "Streaming error: ~A" (stream-state-error-p state)))
    (canonical-response
     (or (stream-state-stop-reason state) "end_turn")
     (nreverse (copy-list (stream-state-content-blocks state))))))

(defun wait-for-compaction-stream-state (state)
  "Block until streaming STATE completes and return its canonical response."
  (loop
    (when (bt:with-lock-held ((stream-state-lock state))
            (stream-state-done-p state))
      (return (compaction-stream-state-response state)))
    (sleep 0.02)))

(defun trim-compaction-request-messages (messages system-prompt limit)
  "Trim oldest MESSAGES until the compaction request fits LIMIT.
The final compaction prompt message is always retained.  Returns values
TRIMMED-MESSAGES and TRIMMED-COUNT."
  (let ((trimmed (copy-list messages))
        (trimmed-count 0))
    (loop :while (and (> (length trimmed) 1)
                      (> (compaction-provider-messages-token-estimate
                          trimmed
                          :system-prompt system-prompt)
                         limit))
          :do (pop trimmed)
              (incf trimmed-count))
    (values trimmed trimmed-count)))

(defun compaction-request-messages (buf)
  "Return provider messages and trim count for compacting BUF."
  (let* ((system-prompt (build-agent-system-prompt (buffer-agent-name buf)))
         (messages (append (build-conversation-messages buf)
                           (list (compaction-text-provider-message
                                  "user"
                                  *compaction-prompt*)))))
    (multiple-value-bind (trimmed trimmed-count)
        (trim-compaction-request-messages
         messages
         system-prompt
         (max 1 (buffer-context-limit buf)))
      (values trimmed trimmed-count system-prompt))))

(defun generate-compaction-summary (buf)
  "Ask BUF's active provider to summarize current conversation history."
  (multiple-value-bind (messages trimmed-count system-prompt)
      (compaction-request-messages buf)
    (multiple-value-bind (provider model think-level)
        (resolve-buffer-provider-and-model buf)
      (let* ((state (provider-request-streaming
                     provider
                     messages
                     (lambda (state) (declare (ignore state)))
                     :model model
                     :tools #()
                     :system-prompt system-prompt
                     :reasoning-effort think-level))
             (response (wait-for-compaction-stream-state state))
             (summary (content-text-blocks (response-content response))))
        (values summary provider model think-level trimmed-count)))))

(defun default-compact-buffer (buf &key (reason :auto))
  "Compact BUF using the active provider and return BUF on success."
  (declare (ignore reason))
  (unless (some #'compaction-message-visible-p
                (loop :for msg := (buffer-first-message buf) :then (message-next msg)
                      :while (and msg (not (eq msg (buffer-input-message buf))))
                      :collect msg))
    (return-from default-compact-buffer nil))
  (let ((recent-users (collect-recent-user-message-snapshots
                       buf
                       *compaction-preserved-user-message-token-limit*)))
    (multiple-value-bind (summary provider model think-level trimmed-count)
        (generate-compaction-summary buf)
      (declare (ignore think-level))
      (when (blank-string-p summary)
        (error "Compaction provider returned an empty summary"))
      (let ((summary-text (compaction-summary-with-prefix summary)))
        (replace-buffer-with-compacted-history buf summary-text recent-users)
        (let ((estimate (buffer-conversation-token-estimate buf)))
          (setf (buffer-token-count buf) estimate)
          (buffer-insert-system-message
           buf
           (format nil
                   "[Conversation compacted via ~(~A~)/~A: summary plus ~D recent user message~:P, estimate ~D/~D tokens~A.]"
                   provider
                   model
                   (length recent-users)
                   estimate
                   (buffer-context-limit buf)
                   (if (plusp trimmed-count)
                       (format nil
                               "; trimmed ~D old message~:P before summarizing"
                               trimmed-count)
                       "")))))
      buf)))

(defun maybe-compact-buffer (buf &key (reason :auto)
                                   include-current-input-p
                                   force-p)
  "Compact BUF when needed.
Returns values COMPACTED-P, ESTIMATE, and THRESHOLD."
  (multiple-value-bind (needed estimate threshold)
      (if force-p
          (values t
                  (buffer-conversation-token-estimate
                   buf
                   :include-current-input-p include-current-input-p)
                  nil)
          (compaction-needed-p buf
                               :include-current-input-p include-current-input-p))
    (setf (buffer-token-count buf) estimate)
    (if (and needed *compaction-function*)
        (handler-case
            (let ((result (funcall *compaction-function* buf :reason reason)))
              (values (not (null result))
                      (buffer-conversation-token-estimate
                       buf
                       :include-current-input-p include-current-input-p)
                      threshold))
          (error (e)
            (buffer-insert-system-message
             buf
             (format nil "[Compaction failed: ~A]" e))
            (values nil estimate threshold)))
        (values nil estimate threshold))))

(defcommand compact-buffer-command (:permission :user-only)
  "Compact the current chat buffer now."
  (buffer)
  (cond
    ((document-buffer-p buffer)
     (buffer-insert-system-message
      buffer
      "[Compaction is only available for chat buffers.]"))
    (t
     (multiple-value-bind (compacted-p estimate threshold)
         (maybe-compact-buffer buffer :reason :manual :force-p t)
       (unless compacted-p
         (buffer-insert-system-message
          buffer
          (format nil
                  "[Nothing compacted; estimate ~D~A tokens.]"
                  estimate
                  (if threshold
                      (format nil "/~D threshold" threshold)
                      ""))))))))

(unless *compaction-function*
  (setf *compaction-function* #'default-compact-buffer))
