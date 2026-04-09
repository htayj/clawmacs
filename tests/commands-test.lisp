(in-package :clawmacs/tests)
(in-suite commands-suite)

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
