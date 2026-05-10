(in-package :clawmacs/tests)
(in-suite commands-suite)

(defvar *command-tool-test-log* nil
  "Records command tool invocations during command tests.")

(defvar *slash-command-test-log* nil
  "Records slash command dispatch during command tests.")

(defmacro with-agent-tool-state (() &body body)
  `(let ((clawmacs::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
         (clawmacs::*agent-tool-name-table*
           (make-hash-table :test #'equal))
         (clawmacs::*tool-table*
           (make-hash-table :test #'equal)))
     ,@body))

(defmacro with-slash-command-state (() &body body)
  `(let ((clawmacs::*slash-command-table*
           (make-hash-table :test #'equal)))
     ,@body))

(clawmacs:defhook *commands-test-hook* (value)
  "Hook used by command-system tests.")

(defvar *advice-test-log* nil
  "Records advice execution order during command tests.")

(defvar *hook-dispatch-test-log* nil
  "Records command dispatch hook execution order during command tests.")

(defun advice-target (value)
  "Function used by advice tests."
  (push (list :body value) *advice-test-log*)
  (format nil "body-~A" value))

(defun advice-values-target (value)
  "Function used to verify advised functions preserve multiple values."
  (values value :second))

(defun hook-dispatch-test-command (buffer)
  "Command used by hook dispatch tests."
  (push (list :body buffer) *hook-dispatch-test-log*)
  :command-result)
(clawmacs:defcommand hook-dispatch-test-command)

(test defhook-registers-hook-metadata
  "defhook defines a hook var and records its argument metadata."
  (let ((metadata (clawmacs:find-hook-metadata
                   'clawmacs/tests::*commands-test-hook*)))
    (is (not (null metadata)))
    (is (equal '(value) (clawmacs:hook-metadata-args metadata)))
    (is (find 'clawmacs/tests::*commands-test-hook*
              (clawmacs:list-hooks)
              :key #'clawmacs:hook-metadata-name))))

(test run-hook-with-args-isolates-hook-errors
  "Hook errors are reported without preventing later hook functions."
  (let ((*commands-test-hook* nil)
        (log nil)
        (*error-output* (make-string-output-stream)))
    (clawmacs:add-hook
     '*commands-test-hook*
     (lambda (value)
       (declare (ignore value))
       (error "expected hook failure"))
     :append t)
    (clawmacs:add-hook
     '*commands-test-hook*
     (lambda (value)
       (push (list :after-error value) log))
     :append t)
    (clawmacs:run-hook-with-args '*commands-test-hook* "ok")
    (is (equal '((:after-error "ok")) (reverse log)))))

(test advice-before-after-and-around-wrap-function
  "Advice entries run around the preserved original fdefinition."
  (let ((*advice-test-log* nil))
    (unwind-protect
         (progn
           (clawmacs:clear-advices 'advice-target)
           (clawmacs:add-advice
            'advice-target
            :before
            (lambda (value)
              (push (list :before value) *advice-test-log*))
            :name 'advice-test-before
            :append t)
           (clawmacs:add-advice
            'advice-target
            :around
            (lambda (next value)
              (push (list :around-before value) *advice-test-log*)
              (let ((result (funcall next (format nil "~A!" value))))
                (push (list :around-after result) *advice-test-log*)
                (format nil "around-~A" result)))
            :name 'advice-test-around
            :append t)
           (clawmacs:add-advice
            'advice-target
            :after
            (lambda (result value)
              (push (list :after result value) *advice-test-log*))
            :name 'advice-test-after
            :append t)
           (is (string= "around-body-x!" (funcall 'advice-target "x")))
           (is (equal '((:before "x")
                        (:around-before "x")
                        (:body "x!")
                        (:around-after "body-x!")
                        (:after "around-body-x!" "x"))
                      (reverse *advice-test-log*))))
      (clawmacs:clear-advices 'advice-target))))

(test advice-removal-and-multiple-values
  "Removing the last advice restores the original function and values."
  (clawmacs:clear-advices 'advice-values-target)
  (let ((original (symbol-function 'advice-values-target)))
    (unwind-protect
         (progn
           (clawmacs:add-advice
            'advice-values-target
            :after
            (lambda (result value)
              (declare (ignore result value))
              nil)
            :name 'advice-values-after)
           (is (clawmacs:advice-member-p 'advice-values-target
                                          'advice-values-after))
           (is (equal '(:first :second)
                      (multiple-value-list
                       (funcall 'advice-values-target :first))))
           (is (not (null (clawmacs:remove-advice
                           'advice-values-target
                           'advice-values-after))))
           (is (null (clawmacs:list-advices 'advice-values-target)))
           (is (eq original (symbol-function 'advice-values-target))))
      (clawmacs:clear-advices 'advice-values-target))))

(test invoke-command-runs-command-hooks
  "Interactive command dispatch runs before and after command hooks."
  (let ((clawmacs::*before-command-hook* nil)
        (clawmacs::*after-command-hook* nil)
        (*hook-dispatch-test-log* nil))
    (clawmacs:add-hook
     'clawmacs:*before-command-hook*
     (lambda (buffer command)
       (push (list :before buffer command) *hook-dispatch-test-log*))
     :append t)
    (clawmacs:add-hook
     'clawmacs:*after-command-hook*
     (lambda (buffer command result)
       (push (list :after buffer command result) *hook-dispatch-test-log*))
     :append t)
    (let ((buffer (make-buffer "command-hook-test")))
      (is (eq :command-result
              (clawmacs:invoke-command
               buffer 'clawmacs/tests::hook-dispatch-test-command)))
      (is (equal `((:before ,buffer
                            clawmacs/tests::hook-dispatch-test-command)
                   (:body ,buffer)
                   (:after ,buffer
                           clawmacs/tests::hook-dispatch-test-command
                           :command-result))
                 (reverse *hook-dispatch-test-log*))))))

(test execute-tool-runs-tool-hooks
  "Tool execution runs before and after tool hooks."
  (let ((clawmacs::*tool-table* (make-hash-table :test #'equal))
        (clawmacs::*before-tool-hook* nil)
        (clawmacs::*after-tool-hook* nil)
        (log nil))
    (clawmacs:register-tool
     "hook_probe"
     "Probe tool hooks."
     '((:type . "object"))
     :agent-allowed
     (lambda (args)
       (push (list :body args) log)
       "tool-result"))
    (clawmacs:add-hook
     'clawmacs:*before-tool-hook*
     (lambda (tool-name args)
       (push (list :before tool-name args) log))
     :append t)
    (clawmacs:add-hook
     'clawmacs:*after-tool-hook*
     (lambda (tool-name args result)
       (push (list :after tool-name args result) log))
     :append t)
    (let ((args '(:value "ok")))
      (is (string= "tool-result"
                   (clawmacs:execute-tool "hook_probe" args)))
      (is (equal `((:before "hook_probe" ,args)
                   (:body ,args)
                   (:after "hook_probe" ,args "tool-result"))
                 (reverse log))))))

(test send-message-runs-send-hooks
  "Sending a non-empty input runs before and after send hooks."
  (let ((clawmacs::*before-send-message-hook* nil)
        (clawmacs::*after-send-message-hook* nil)
        (log nil))
    (clawmacs:add-hook
     'clawmacs:*before-send-message-hook*
     (lambda (buffer input-text)
       (push (list :before buffer input-text) log))
     :append t)
    (clawmacs:add-hook
     'clawmacs:*after-send-message-hook*
     (lambda (buffer input-text result)
       (push (list :after buffer input-text result) log))
     :append t)
    (let* ((buffer (make-buffer "send-hook-test"))
           (clawmacs::*prefix-handlers*
             (list (cons "?"
                         (lambda (buf remaining)
                           (push (list :handler buf remaining) log))))))
      (clawmacs::set-message-text (buffer-input-message buffer) "?payload")
      (is (eq t (clawmacs::send-message buffer)))
      (is (equal `((:before ,buffer "?payload")
                   (:handler ,buffer "payload")
                   (:after ,buffer "?payload" t))
                 (reverse log))))))

(defun slash-command-test-handler (buffer args full-text)
  "Record slash dispatch for command tests."
  (push (list :slash buffer args full-text) *slash-command-test-log*)
  :slash-dispatched)

(test send-message-dispatches-slash-command-before-normal-send
  "Known slash commands are handled in the composer instead of becoming chat history."
  (with-slash-command-state ()
    (let ((*slash-command-test-log* nil)
          (clawmacs::*before-send-message-hook* nil)
          (clawmacs::*after-send-message-hook* nil)
          (buffer (make-buffer "slash-dispatch-test")))
      (clawmacs:register-slash-command
       "demo"
       #'slash-command-test-handler
       :description "Demo slash command.")
      (clawmacs::set-message-text (buffer-input-message buffer) "/demo alpha beta")
      (is (eq :slash-dispatched (clawmacs::send-message buffer)))
      (is (equal `((:slash ,buffer ("alpha" "beta") "/demo alpha beta"))
                 *slash-command-test-log*))
      (is (string= "" (message-text (buffer-input-message buffer))))
      (is (eq (buffer-first-message buffer)
              (buffer-input-message buffer))))))

(test send-message-leaves-unknown-slash-text-on-normal-send-path
  "Unknown slash text still falls through to the normal agent send path."
  (with-slash-command-state ()
    (let ((clawmacs::*before-send-message-hook* nil)
          (clawmacs::*after-send-message-hook* nil)
          (buffer (make-buffer "unknown-slash-test"))
          (original-send (symbol-function 'clawmacs::send-to-agent-with-context)))
      (unwind-protect
           (progn
             (setf (symbol-function 'clawmacs::send-to-agent-with-context)
                   (lambda (buf)
                     (declare (ignore buf))
                     :agent-sent))
             (clawmacs::set-message-text (buffer-input-message buffer)
                                         "/unknown still-send")
             (is (eq :agent-sent (clawmacs::send-message buffer))))
        (setf (symbol-function 'clawmacs::send-to-agent-with-context)
              original-send)))))

(test send-message-expands-prompt-template-before-normal-send
  "Known slash templates expand into normal chat input before the agent send."
  (let* ((root (temp-package-test-directory "templata-send"))
         (project-root (merge-pathnames "project/" root))
         (prompt-root (merge-pathnames ".clawmacs/prompts/" project-root))
         (clawmacs::*before-send-message-hook* nil)
         (clawmacs::*after-send-message-hook* nil)
         (buffer (make-buffer "templata-send"
                              :working-directory project-root))
         (sent-text nil)
         (original-send (symbol-function 'clawmacs::send-to-agent-with-context)))
    (ensure-directories-exist (merge-pathnames ".keep" prompt-root))
    (with-open-file (stream (merge-pathnames "review.md" prompt-root)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string "---
description: Review prompt
---
Review target: $1
All args: $@" stream))
    (unwind-protect
         (progn
           (setf (symbol-function 'clawmacs::send-to-agent-with-context)
                 (lambda (buf)
                   (setf sent-text
                         (message-text
                          (message-prev (buffer-input-message buf))))
                   :agent-sent))
           (clawmacs::set-message-text (buffer-input-message buffer)
                                       "/review parser")
           (is (eq :agent-sent (clawmacs::send-message buffer)))
           (is (search "Review target: parser" sent-text))
           (is (search "All args: parser" sent-text)))
      (setf (symbol-function 'clawmacs::send-to-agent-with-context)
            original-send))))

(test command-metadata-registration
  "defcommand registers metadata in the command table."
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(defun test-cmd (buffer)
             "A test command."
             (declare (ignore buffer))
             :test-result))
    (eval '(clawmacs:defcommand test-cmd))
    (let ((meta (gethash 'test-cmd *command-table*)))
      (is (not (null meta)))
      (is (string= "A test command." (command-metadata-docstring meta))))))

(test command-metadata-captures-lambda-list-and-prompts
  "defcommand stores the command lambda list and prompt metadata."
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(defun prompted-cmd (buffer count label)
             "Prompted command."
             (declare (ignore buffer count label))
             :ok))
    (eval '(clawmacs:defcommand prompted-cmd
             :prompts ((count :prompt "Count" :reader parse-integer)
                       (label :prompt "Label"))))
    (let ((meta (gethash 'prompted-cmd *command-table*)))
      (is (equal '(buffer count label)
                 (command-metadata-lambda-list meta)))
      (is (equal '((:name count :prompt "Count" :reader parse-integer)
                   (:name label :prompt "Label" :reader nil))
                 (command-metadata-prompts meta))))))

(test deftool-metadata-registers-provider-tool
  "deftool stores metadata and exposes a provider-callable tool."
  (with-agent-tool-state ()
    (eval '(defun metadata-doc-tool (value)
             (format nil "value=~A" value)))
    (eval '(clawmacs:deftool metadata-doc-tool
             :name "metadata_doc_tool"
             :description "Return a tagged value."
             :args ((value :type "string"
                           :description "Value to echo."))))
    (let* ((metadata (find-agent-tool-metadata 'metadata-doc-tool))
           (definition (gethash "metadata_doc_tool" clawmacs::*tool-table*))
           (schema (and definition
                        (tool-definition-input-schema definition)))
           (properties (cdr (assoc :properties schema))))
      (is (not (null metadata)))
      (is (string= "metadata_doc_tool"
                   (agent-tool-metadata-name metadata)))
      (is (eq :agent-allowed
              (agent-tool-metadata-permission metadata)))
      (is (not (null definition)))
      (is (not (null (assoc "value" properties :test #'string=))))
      (is (string= "value=ok"
                   (execute-tool "metadata_doc_tool" '(:value "ok")))))))

(test deftool-metadata-replaces-same-symbol
  "Re-evaluating the same symbol's tool metadata replaces the provider entry."
  (with-agent-tool-state ()
    (eval '(defun replace-doc-tool (value)
             (format nil "replace=~A" value)))
    (eval '(clawmacs:deftool replace-doc-tool
             :name "replace_old"
             :description "Old tool."
             :args ((value :type "string"))))
    (eval '(clawmacs:deftool replace-doc-tool
             :name "replace_new"
             :description "New tool."
             :args ((value :type "string"))))
    (is (null (gethash "replace_old" clawmacs::*tool-table*)))
    (is (not (null (gethash "replace_new" clawmacs::*tool-table*))))
    (is (string= "replace_new"
                 (agent-tool-metadata-name
                  (find-agent-tool-metadata 'replace-doc-tool))))))

(test deftool-metadata-rejects-duplicate-provider-names
  "Different symbols cannot register the same provider tool name."
  (with-agent-tool-state ()
    (eval '(defun duplicate-doc-tool-a (value) value))
    (eval '(defun duplicate-doc-tool-b (value) value))
    (eval '(clawmacs:deftool duplicate-doc-tool-a
             :name "duplicate_doc_tool"
             :description "First tool."
             :args ((value :type "string"))))
    (signals error
      (eval '(clawmacs:deftool duplicate-doc-tool-b
               :name "duplicate_doc_tool"
               :description "Second tool."
               :args ((value :type "string")))))))

(test deftool-rejects-undefined-functions
  "deftool tags a separately defined function."
  (with-agent-tool-state ()
    (when (fboundp 'missing-doc-tool)
      (fmakunbound 'missing-doc-tool))
    (signals error
      (eval '(clawmacs:deftool missing-doc-tool
               :name "missing_doc_tool"
               :description "Missing function."
               :args ((value :type "string")))))))

(test deftool-command-uses-current-buffer
  "deftool infers command call style and supplies the active buffer."
  (with-agent-tool-state ()
    (let ((*command-table* (make-hash-table :test #'eq))
          (*command-tool-test-log* nil))
      (eval '(defun command-tool-test (buffer label)
               "Run a command as an agent tool."
               (setf clawmacs/tests::*command-tool-test-log*
                     (list (buffer-name buffer) label))
               (format nil "command=~A" label)))
      (eval '(clawmacs:defcommand command-tool-test
               :prompts ((label :prompt "Label"))))
      (eval '(clawmacs:deftool command-tool-test
               :name "command_tool_test"
               :description "Run a command as an agent tool."
               :permission :agent-allowed
               :args ((label :type "string"
                             :description "Label to record."))))
      (let* ((definition (gethash "command_tool_test" clawmacs::*tool-table*))
             (metadata (find-agent-tool-metadata 'command-tool-test))
             (schema (tool-definition-input-schema definition))
             (properties (cdr (assoc :properties schema)))
             (buf (make-buffer "tool-buffer")))
        (is (eq :agent-allowed
                (agent-tool-metadata-permission metadata)))
        (is (not (null (assoc "label" properties :test #'string=))))
        (is (null (assoc "buffer" properties :test #'string=)))
        (let ((*current-caller* :some-agent)
              (*current-tool-buffer* buf))
          (is (string= "command=ok"
                       (execute-tool "command_tool_test" '(:label "ok")))))
        (is (equal '("tool-buffer" "ok")
                   *command-tool-test-log*))))))

(test old-tool-keywords-are-rejected
  "Tool metadata belongs in deftool, not defdoc or defcommand."
  (signals error
    (macroexpand-1
     '(clawmacs:defdoc old-doc-tool
       :tool (:name "old_doc_tool"
              :args ((value :type "string"))))))
  (signals error
    (macroexpand-1
     '(clawmacs:defcommand old-command-tool
        :tool (:name "old_command_tool"
               :args ((label :type "string")))))))

(test defcommand-rejects-permission-keyword
  "Command permissions belong in deftool metadata, not defcommand."
  (signals error
    (macroexpand-1
     '(clawmacs:defcommand permission-command
        :permission :agent-allowed))))

(test defcommand-rejects-interactive-keyword
  "Prompt metadata belongs under :PROMPTS, not :INTERACTIVE."
  (signals error
    (macroexpand-1
     '(clawmacs:defcommand interactive-command
        :interactive nil))))

(test defcommand-rejects-undefined-functions
  "defcommand tags a separately defined function."
  (when (fboundp 'missing-command)
    (fmakunbound 'missing-command))
  (signals error
    (eval '(clawmacs:defcommand missing-command))))

(test list-available-commands-returns-registered-commands
  "list-available-commands returns registered commands for every caller."
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(defun first-cmd (buffer)
             "First." (declare (ignore buffer)) nil))
    (eval '(defun second-cmd (buffer)
             "Second." (declare (ignore buffer)) nil))
    (eval '(clawmacs:defcommand first-cmd))
    (eval '(clawmacs:defcommand second-cmd))
    (let ((*current-caller* :user))
      (let ((cmds (list-available-commands)))
        (is (member 'first-cmd cmds))
        (is (member 'second-cmd cmds))))
    (let ((*current-caller* :some-agent))
      (let ((cmds (list-available-commands)))
        (is (member 'first-cmd cmds))
        (is (member 'second-cmd cmds))))))

(test list-available-commands-includes-prompted-commands
  "All defcommand forms are UI commands."
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(defun zero-arg-cmd (buffer)
             "Default command." (declare (ignore buffer)) :ok))
    (eval '(defun listed-prompted-cmd (buffer count)
             "Prompted command." (declare (ignore buffer count)) :ok))
    (eval '(clawmacs:defcommand zero-arg-cmd))
    (eval '(clawmacs:defcommand listed-prompted-cmd
             :prompts ((count :prompt "Count" :reader parse-integer))))
    (let ((cmds (list-available-commands)))
      (is (member 'zero-arg-cmd cmds))
      (is (member 'listed-prompted-cmd cmds)))))

(test defcommand-rejects-unsupported-lambda-lists
  "Commands must use required positional arguments only."
  (eval '(defun unsupported-cmd (buffer &optional count)
           "Bad lambda list."
           (declare (ignore buffer count))
           nil))
  (signals error
    (eval '(clawmacs:defcommand unsupported-cmd))))

(test defcommand-rejects-missing-prompts-for-arguments
  "Commands with non-buffer arguments must declare minibuffer prompts."
  (eval '(defun missing-prompts-cmd (buffer count)
           "Missing prompts."
           (declare (ignore buffer count))
           nil))
  (signals error
    (eval '(clawmacs:defcommand missing-prompts-cmd))))

(test defcommand-rejects-prompt-arg-mismatches
  "Prompt specs must line up with the command parameters."
  (eval '(defun mismatched-cmd (buffer count label)
           "Mismatched."
           (declare (ignore buffer count label))
           nil))
  (signals error
    (eval '(clawmacs:defcommand mismatched-cmd
             :prompts ((count :prompt "Count" :reader parse-integer))))))
