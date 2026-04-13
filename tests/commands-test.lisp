(in-package :clawmacs/tests)
(in-suite commands-suite)

(defvar *command-tool-test-log* nil
  "Records command tool invocations during command tests.")

(defmacro with-agent-tool-state (() &body body)
  `(let ((clawmacs::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
         (clawmacs::*agent-tool-name-table*
           (make-hash-table :test #'equal))
         (clawmacs::*tool-table*
           (make-hash-table :test #'equal)))
     ,@body))

(test command-metadata-registration
  "defcommand registers metadata in the command table."
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(clawmacs:defcommand test-cmd ()
             "A test command."
             (buffer)
             (declare (ignore buffer))
             :test-result))
    (let ((meta (gethash 'test-cmd *command-table*)))
      (is (not (null meta)))
      (is (string= "A test command." (command-metadata-docstring meta))))))

(test command-metadata-captures-lambda-list-and-prompts
  "defcommand stores the command lambda list and prompt metadata."
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(clawmacs:defcommand prompted-cmd
               (:prompts ((count :prompt "Count" :reader parse-integer)
                          (label :prompt "Label")))
             "Prompted command."
             (buffer count label)
             (declare (ignore buffer count label))
             :ok))
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

(test deftool-command-uses-current-buffer
  "deftool infers command call style and supplies the active buffer."
  (with-agent-tool-state ()
    (let ((*command-table* (make-hash-table :test #'eq))
          (*command-tool-test-log* nil))
      (eval '(clawmacs:defcommand command-tool-test
                 (:prompts ((label :prompt "Label")))
               "Run a command as an agent tool."
               (buffer label)
               (setf clawmacs/tests::*command-tool-test-log*
                     (list (buffer-name buffer) label))
               (format nil "command=~A" label)))
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
          (:tool (:name "old_command_tool"
                  :args ((label :type "string"))))
        "Old command tool."
        (buffer label)
        (declare (ignore buffer label))
        nil))))

(test defcommand-rejects-permission-keyword
  "Command permissions belong in deftool metadata, not defcommand."
  (signals error
    (macroexpand-1
     '(clawmacs:defcommand permission-command
          (:permission :agent-allowed)
        "Old permission metadata."
        (buffer)
        (declare (ignore buffer))
        nil))))

(test defcommand-rejects-interactive-keyword
  "Prompt metadata belongs under :PROMPTS, not :INTERACTIVE."
  (signals error
    (macroexpand-1
     '(clawmacs:defcommand interactive-command
          (:interactive nil)
        "Old interactive metadata."
        (buffer)
        (declare (ignore buffer))
        nil))))

(test list-available-commands-returns-registered-commands
  "list-available-commands returns registered commands for every caller."
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(clawmacs:defcommand first-cmd ()
             "First." (buffer) (declare (ignore buffer)) nil))
    (eval '(clawmacs:defcommand second-cmd ()
             "Second." (buffer) (declare (ignore buffer)) nil))
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
    (eval '(clawmacs:defcommand zero-arg-cmd ()
             "Default command." (buffer) (declare (ignore buffer)) :ok))
    (eval '(clawmacs:defcommand listed-prompted-cmd
               (:prompts ((count :prompt "Count" :reader parse-integer)))
             "Prompted command." (buffer count)
             (declare (ignore buffer count)) :ok))
    (let ((cmds (list-available-commands)))
      (is (member 'zero-arg-cmd cmds))
      (is (member 'listed-prompted-cmd cmds)))))

(test defcommand-rejects-unsupported-lambda-lists
  "Interactive commands must use required positional arguments only."
  (signals error
    (macroexpand-1
     '(clawmacs:defcommand unsupported-cmd ()
       "Bad lambda list."
       (buffer &optional count)
       (declare (ignore buffer count))
       nil))))

(test defcommand-rejects-missing-prompts-for-arguments
  "Commands with non-buffer arguments must declare minibuffer prompts."
  (signals error
    (macroexpand-1
     '(clawmacs:defcommand missing-prompts-cmd ()
        "Missing prompts."
        (buffer count)
        (declare (ignore buffer count))
        nil))))

(test defcommand-rejects-prompt-arg-mismatches
  "Prompt specs must line up with the command parameters."
  (signals error
    (macroexpand-1
     '(clawmacs:defcommand mismatched-cmd
          (:prompts ((count :prompt "Count" :reader parse-integer)))
        "Mismatched."
        (buffer count label)
        (declare (ignore buffer count label))
        nil))))
