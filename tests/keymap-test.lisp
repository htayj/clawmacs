(in-package :clawmacs/tests)

(def-suite keymap-suite
  :description "Keymap tests"
  :in clawmacs-suite)

(in-suite keymap-suite)

(test keymap-bind-and-lookup
  "Binding a key and looking it up returns the command."
  (let ((km (make-keymap :test)))
    (keymap-bind km #\a 'some-command)
    (is (eq 'some-command (keymap-lookup km #\a)))
    (is (null (keymap-lookup km #\b)))))

(test keymap-parent-chain-lookup
  "Lookup falls back to parent keymap."
  (let* ((parent (make-keymap :parent))
         (child (make-keymap :child :parent parent)))
    (keymap-bind parent #\a 'parent-command)
    (keymap-bind child #\b 'child-command)
    (is (eq 'child-command (keymap-lookup child #\b)))
    (is (eq 'parent-command (keymap-lookup child #\a)))
    (is (null (keymap-lookup parent #\b)))))

(test keymap-child-overrides-parent
  "Child bindings shadow parent bindings for the same key."
  (let* ((parent (make-keymap :parent))
         (child (make-keymap :child :parent parent)))
    (keymap-bind parent #\a 'parent-version)
    (keymap-bind child #\a 'child-version)
    (is (eq 'child-version (keymap-lookup child #\a)))
    (is (eq 'parent-version (keymap-lookup parent #\a)))))

(test default-keymap-readline-argument-yank-bindings
  "Default keymap includes readline-style argument yank keybindings."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::yank-previous-command-first-arg-command
          (keymap-lookup *default-keymap* '(:alt #\Em))))
  (is (eq 'clawmacs::yank-previous-command-last-arg-command
          (keymap-lookup *default-keymap* '(:alt #\.))))
  (is (eq 'clawmacs::yank-previous-command-last-arg-command
          (keymap-lookup *default-keymap* '(:alt #\_)))))

(test default-keymap-ctrl-d-binding
  "Default keymap binds Ctrl+d to forward delete."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::delete-char-forward-command
          (keymap-lookup *default-keymap* (code-char 4)))))

(test default-keymap-backspace-bindings
  "Default keymap binds backspace variants to backward char delete."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::delete-char-backward-command
          (keymap-lookup *default-keymap* #\Backspace)))
  (is (eq 'clawmacs::delete-char-backward-command
          (keymap-lookup *default-keymap* #\Rubout)))
  (is (eq 'clawmacs::delete-char-backward-command
          (keymap-lookup *default-keymap* :backspace))))

(test default-keymap-backward-kill-word-backspace-bindings
  "Default keymap binds C-Backspace and M-Backspace to backward-kill-word."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:alt #\Backspace))))
  (is (eq 'clawmacs::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:alt #\Rubout))))
  (is (eq 'clawmacs::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:alt :backspace))))
  (is (eq 'clawmacs::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:ctrl #\Backspace))))
  (is (eq 'clawmacs::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:ctrl #\Rubout))))
  (is (eq 'clawmacs::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:ctrl :backspace)))))
