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
    (eval '(clawmacs:defcommand test-cmd (:permission :user-only)
             "A test command."
             (buffer)
             (declare (ignore buffer))
             :test-result))
    (let ((meta (gethash 'test-cmd *command-table*)))
      (is (not (null meta)))
      (is (eq :user-only (command-metadata-permission meta)))
      (is (string= "A test command." (command-metadata-docstring meta))))))

(test command-metadata-captures-lambda-list-and-interactive-spec
  "defcommand stores the command lambda list and interactive arg metadata."
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(clawmacs:defcommand interactive-cmd
               (:permission :user-only
                :interactive ((count :prompt "Count" :reader parse-integer)
                              (label :prompt "Label")))
             "Interactive command."
             (buffer count label)
             (declare (ignore buffer count label))
             :ok))
    (let ((meta (gethash 'interactive-cmd *command-table*)))
      (is (equal '(buffer count label)
                 (command-metadata-lambda-list meta)))
      (is (equal '((:name count :prompt "Count" :reader parse-integer)
                   (:name label :prompt "Label" :reader nil))
                 (command-metadata-interactive-spec meta))))))

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
                 (:permission :agent-allowed
                  :interactive nil)
               "Run a command as an agent tool."
               (buffer label)
               (setf clawmacs/tests::*command-tool-test-log*
                     (list (buffer-name buffer) label))
               (format nil "command=~A" label)))
      (eval '(clawmacs:deftool command-tool-test
               :name "command_tool_test"
               :description "Run a command as an agent tool."
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
          (:permission :agent-allowed
           :tool (:name "old_command_tool"
                  :args ((label :type "string"))))
        "Old command tool."
        (buffer label)
        (declare (ignore buffer label))
        nil))))

(test permission-denied-for-agent-on-user-only
  "An agent calling a :user-only command signals permission-denied."
  (let ((*command-table* (make-hash-table :test #'eq))
        (*current-caller* :some-agent))
    (eval '(clawmacs:defcommand restricted-cmd (:permission :user-only)
             "Restricted."
             (buffer)
             (declare (ignore buffer))
             :ok))
    (signals permission-denied
      (check-permission 'restricted-cmd))))

(test permission-passes-for-user-on-user-only
  "A user calling a :user-only command succeeds."
  (let ((*command-table* (make-hash-table :test #'eq))
        (*current-caller* :user))
    (eval '(clawmacs:defcommand allowed-cmd (:permission :user-only)
             "Allowed."
             (buffer)
             (declare (ignore buffer))
             :ok))
    (finishes (check-permission 'allowed-cmd))))

(test agent-allowed-passes-for-any-caller
  "An :agent-allowed command can be called by anyone."
  (let ((*command-table* (make-hash-table :test #'eq))
        (*current-caller* :some-agent))
    (eval '(clawmacs:defcommand open-cmd (:permission :agent-allowed)
             "Open."
             (buffer)
             (declare (ignore buffer))
             :ok))
    (finishes (check-permission 'open-cmd))))

(test list-available-commands-filters-by-caller
  "list-available-commands excludes :user-only commands for agent callers."
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(clawmacs:defcommand user-cmd (:permission :user-only)
             "User only." (buffer) (declare (ignore buffer)) nil))
    (eval '(clawmacs:defcommand agent-cmd (:permission :agent-allowed)
             "Agent ok." (buffer) (declare (ignore buffer)) nil))
    (let ((*current-caller* :user))
      (let ((cmds (list-available-commands)))
        (is (member 'user-cmd cmds))
        (is (member 'agent-cmd cmds))))
    (let ((*current-caller* :some-agent))
      (let ((cmds (list-available-commands)))
        (is (not (member 'user-cmd cmds)))
        (is (member 'agent-cmd cmds))))))

(test list-interactive-commands-excludes-programmatic-commands
  "Interactive command listing only returns commands exposed to the UI."
  (let ((*command-table* (make-hash-table :test #'eq)))
    (eval '(clawmacs:defcommand zero-arg-cmd (:permission :user-only)
             "Default interactive." (buffer) (declare (ignore buffer)) :ok))
    (eval '(clawmacs:defcommand prompted-cmd
               (:permission :user-only
                :interactive ((count :prompt "Count" :reader parse-integer)))
             "Prompted interactive." (buffer count)
             (declare (ignore buffer count)) :ok))
    (eval '(clawmacs:defcommand hidden-cmd
               (:permission :user-only :interactive nil)
             "Hidden." (buffer) (declare (ignore buffer)) :ok))
    (let ((cmds (list-interactive-commands)))
      (is (member 'zero-arg-cmd cmds))
      (is (member 'prompted-cmd cmds))
      (is (not (member 'hidden-cmd cmds))))))

(test defcommand-rejects-unsupported-lambda-lists
  "Interactive commands must use required positional arguments only."
  (signals error
    (macroexpand-1
     '(clawmacs:defcommand unsupported-cmd (:permission :user-only)
       "Bad lambda list."
       (buffer &optional count)
       (declare (ignore buffer count))
       nil))))

(test defcommand-rejects-interactive-arg-mismatches
  "Interactive arg specs must line up with the command parameters."
  (signals error
    (macroexpand-1
     '(clawmacs:defcommand mismatched-cmd
          (:permission :user-only
           :interactive ((count :prompt "Count" :reader parse-integer)))
        "Mismatched."
        (buffer count label)
        (declare (ignore buffer count label))
        nil))))
