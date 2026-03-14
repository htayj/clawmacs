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
