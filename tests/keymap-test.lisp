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
