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
          (keymap-lookup *default-keymap* '(:meta #\Em))))
  (is (eq 'clawmacs::yank-previous-command-last-arg-command
          (keymap-lookup *default-keymap* '(:meta #\.))))
  (is (eq 'clawmacs::yank-previous-command-last-arg-command
          (keymap-lookup *default-keymap* '(:meta #\_)))))

(test default-keymap-ctrl-d-binding
  "Default keymap binds Ctrl+d to forward delete."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::delete-char-forward-command
          (keymap-lookup *default-keymap* (code-char 4)))))

(test default-keymap-redraw-screen-binding
  "Default keymap binds Ctrl+l to request a full redraw."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::redraw-screen-command
          (keymap-lookup *default-keymap* (code-char 12)))))

(test default-keymap-mcclim-debug-status-binding
  "Default keymap binds C-h D and C-c D to McCLIM debug status."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::mcclim-debug-status-command
          (keymap-lookup *default-keymap* '(:ctrl-h #\D))))
  (is (eq 'clawmacs::mcclim-debug-status-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\D)))))

(test default-keymap-escape-stop-llm-binding
  "Default keymap binds Escape to stopping the active LLM response."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::stop-llm-command
          (keymap-lookup *default-keymap* #\Esc))))

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
          (keymap-lookup *default-keymap* '(:meta #\Backspace))))
  (is (eq 'clawmacs::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:meta #\Rubout))))
  (is (eq 'clawmacs::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:meta :backspace))))
  (is (null (keymap-lookup *default-keymap* '(:alt :backspace))))
  (is (eq 'clawmacs::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:ctrl #\Backspace))))
  (is (eq 'clawmacs::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:ctrl #\Rubout))))
  (is (eq 'clawmacs::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:ctrl :backspace)))))

(defun mcclim-test-key-event (key-name key-character &optional (modifier-state 0))
  "Build a McCLIM key event for key normalization tests."
  (make-instance 'clim:key-press-event
                 :key-name key-name
                 :key-character key-character
                 :modifier-state modifier-state
                 :sheet nil))

(test mcclim-normalize-alt-emulates-meta-by-default
  "By default, physical Alt is normalized as Meta for Emacs-style bindings."
  (let ((*alt-emulates-meta* t)
        (clawmacs::*meta-pending* nil)
        (clawmacs::*alt-pending* nil))
    (is (null (clawmacs::mcclim-normalize-key
               (mcclim-test-key-event :alt-left nil))))
    (is (equal '(:meta #\x)
               (clawmacs::mcclim-normalize-key
                (mcclim-test-key-event :|x| #\x clim:+meta-key+))))))

(test mcclim-normalize-alt-can-be-separate-from-meta
  "When disabled, Alt and Meta produce distinct Clawmacs key prefixes."
  (let ((*alt-emulates-meta* nil)
        (clawmacs::*meta-pending* nil)
        (clawmacs::*alt-pending* nil))
    (is (null (clawmacs::mcclim-normalize-key
               (mcclim-test-key-event :alt-left nil))))
    (is (equal '(:alt #\x)
               (clawmacs::mcclim-normalize-key
                (mcclim-test-key-event :|x| #\x clim:+meta-key+))))
    (is (null (clawmacs::mcclim-normalize-key
               (mcclim-test-key-event :meta-left nil))))
    (is (equal '(:meta #\x)
               (clawmacs::mcclim-normalize-key
                (mcclim-test-key-event :|x| #\x clim:+meta-key+))))))

(test mcclim-normalize-ctrl-space
  "Control-space normalizes to the file editor set-mark key."
  (is (eql (code-char 0)
           (clawmacs::mcclim-normalize-key
            (mcclim-test-key-event :space #\Space clim:+control-key+)))))

(test mcclim-normalize-escape-is-meta-prefix-when-not-streaming
  "Escape remains the Meta prefix when no provider stream is active."
  (let ((clawmacs::*meta-pending* nil)
        (clawmacs::*alt-pending* nil)
        (clawmacs::*skill-completion-active* nil))
    (is (null (clawmacs::mcclim-normalize-key
               (mcclim-test-key-event :escape #\Esc))))
    (is-true clawmacs::*meta-pending*)
    (is (null clawmacs::*alt-pending*))))

(test mcclim-normalize-escape-passes-through-when-streaming
  "Escape reaches the keymap as Esc while the visible buffer has an LLM run."
  (let* ((buf (make-buffer "streaming-escape"))
         (state (clawmacs::make-stream-state))
         (clawmacs::*meta-pending* nil)
         (clawmacs::*alt-pending* nil)
         (clawmacs::*skill-completion-active* nil))
    (setf (buffer-pending-stream buf) state)
    (is (eql #\Esc
             (clawmacs::mcclim-normalize-key
              (mcclim-test-key-event :escape #\Esc)
              buf)))
    (is (null clawmacs::*meta-pending*))
    (is (null clawmacs::*alt-pending*))))

(test mcclim-actionable-input-event-includes-pointer-motion
  "Pointer motion events are preserved for CLIM hover highlighting."
  (let ((event (make-instance 'clim:pointer-motion-event
                              :sheet nil
                              :pointer nil
                              :modifier-state 0
                              :x 0
                              :y 0)))
    (is (clawmacs::mcclim-actionable-input-event-p event))))

(test mcclim-ui-state-dynamically-binds-interaction-specials
  "McCLIM frame-local UI state shadows and persists modal interaction specials."
  (let ((clawmacs::*minibuffer-active* nil)
        (clawmacs::*minibuffer-input* "global")
        (clawmacs::*skill-completion-active* nil)
        (clawmacs::*meta-pending* nil)
        (clawmacs::*scroll-page-size* nil))
    (let ((state (clawmacs::make-mcclim-ui-state)))
      (clawmacs::with-mcclim-ui-state (state)
        (setf clawmacs::*minibuffer-active* t
              clawmacs::*minibuffer-input* "frame"
              clawmacs::*skill-completion-active* t
              clawmacs::*meta-pending* t
              clawmacs::*scroll-page-size* 17)
        (clawmacs::with-mcclim-ui-state (state)
          (setf clawmacs::*minibuffer-input* "nested-frame")))
      (is (null clawmacs::*minibuffer-active*))
      (is (string= "global" clawmacs::*minibuffer-input*))
      (is (null clawmacs::*skill-completion-active*))
      (is (null clawmacs::*meta-pending*))
      (is (null clawmacs::*scroll-page-size*))
      (clawmacs::with-mcclim-ui-state (state)
        (is (eq t clawmacs::*minibuffer-active*))
        (is (string= "nested-frame" clawmacs::*minibuffer-input*))
        (is (eq t clawmacs::*skill-completion-active*))
        (is (eq t clawmacs::*meta-pending*))
        (is (= 17 clawmacs::*scroll-page-size*))))))

(test mcclim-message-grid-click-moves-point-through-wrapped-lines
  "The McCLIM input-pane click mapper moves message point by visual grid row."
  (let ((message (make-message :user)))
    (clawmacs::set-message-text message "abcdef")
    (clawmacs::mcclim-set-message-point-from-grid message 1 1 4)
    (is (= 5 (message-point-offset message)))
    (is (eq (message-first-line message)
            (message-point-line message)))))

(test mcclim-message-grid-click-clamps-after-last-line
  "Clicking below the input text moves point to the end of the message."
  (let ((message (make-message :user)))
    (clawmacs::set-message-text message "ab
cde")
    (clawmacs::mcclim-set-message-point-from-grid message 20 10 80)
    (is (string= "cde" (line-content (message-point-line message))))
    (is (= 3 (message-point-offset message)))))

(test default-keymap-buffer-selector-binding
  "Default keymap binds C-x C-b to the minibuffer buffer selector,
and C-x b to the old overlay buffer selector."
  (clawmacs::init-default-keymap)
  ;; C-x C-b -> minibuffer buffer selector (new)
  (is (eq 'clawmacs::minibuffer-select-buffer-command
          (keymap-lookup *default-keymap* (list :ctrl-x (code-char 2)))))
  ;; C-x b -> old overlay buffer selector
  (is (eq 'clawmacs::list-buffers-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\b)))))

(test default-keymap-project-bindings
  "Default keymap binds project selection and project file opening."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::open-project-file-command
          (keymap-lookup *default-keymap* (list :ctrl-x (code-char 6)))))
  (is (eq 'clawmacs::minibuffer-select-project-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\p))))
  (is (eq 'clawmacs::load-session-command
          (keymap-lookup *default-keymap* (list :ctrl-x (code-char 18))))))

(test default-keymap-listener-binding
  "Default keymap binds C-x l to the in-buffer Lisp listener."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::new-listener-buffer-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\l)))))

(test default-keymap-window-bindings
  "Default keymap binds Emacs-style logical window commands."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::split-window-below-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\2))))
  (is (eq 'clawmacs::split-window-right-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\3))))
  (is (eq 'clawmacs::delete-window-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\0))))
  (is (eq 'clawmacs::delete-other-windows-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\1))))
  (is (eq 'clawmacs::other-window-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\o)))))

(test file-keymap-emacs-editor-bindings
  "File buffers have Emacs-style editor bindings over the global keymap."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::insert-newline-command
          (keymap-lookup clawmacs::*file-keymap* #\Return)))
  (is (eq 'clawmacs::insert-tab-command
          (keymap-lookup clawmacs::*file-keymap* #\Tab)))
  (is (eq 'clawmacs::previous-line-command
          (keymap-lookup clawmacs::*file-keymap* (code-char 16))))
  (is (eq 'clawmacs::next-line-command
          (keymap-lookup clawmacs::*file-keymap* (code-char 14))))
  (is (eq 'clawmacs::search-forward-command
          (keymap-lookup clawmacs::*file-keymap* (code-char 19))))
  (is (eq 'clawmacs::set-mark-command
          (keymap-lookup clawmacs::*file-keymap* (code-char 0))))
  (is (eq 'clawmacs::kill-region-command
          (keymap-lookup clawmacs::*file-keymap* (code-char 23))))
  (is (eq 'clawmacs::copy-region-command
          (keymap-lookup clawmacs::*file-keymap* '(:meta #\w))))
  (is (eq 'clawmacs::save-session-command
          (keymap-lookup clawmacs::*file-keymap*
                         (list :ctrl-x (code-char 19)))))
  (is (eq 'clawmacs::write-project-file-as-command
          (keymap-lookup clawmacs::*file-keymap*
                         (list :ctrl-x (code-char 23)))))
  (is (eq 'clawmacs::revert-file-buffer-command
          (keymap-lookup clawmacs::*file-keymap*
                         (list :ctrl-x (code-char 22))))))

(test default-keymap-toggle-tool-results-uses-c-c-prefix
  "Toggle tool results stays under C-c; C-x t is session-tree navigation."
  (clawmacs::init-default-keymap)
  ;; C-c t should be bound
  (is (eq 'clawmacs::toggle-tool-results-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\t))))
  (is (eq 'clawmacs::session-tree-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\t))))
  (is (eq 'clawmacs::fork-session-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\T)))))

(test default-keymap-toggle-reasoning-output-uses-c-c-prefix
  "Toggle reasoning output is bound under C-c (mode-specific), not C-x."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::toggle-reasoning-output-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\V))))
  (is (eq 'clawmacs::describe-variable-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\v))))
  (is (null (keymap-lookup *default-keymap* '(:ctrl-x #\V)))))

(test default-keymap-toggle-metadata-output-uses-c-c-prefix
  "Toggle metadata output is bound under C-c (mode-specific), not C-x."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::toggle-metadata-output-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\I))))
  (is (null (keymap-lookup *default-keymap* '(:ctrl-x #\I)))))

(test default-keymap-compaction-binding
  "Manual compaction is bound under the chat-mode C-c prefix."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::compact-buffer-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\c)))))

(test default-keymap-agent-selector-binding
  "Default keymap binds C-c A to the minibuffer agent selector."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::minibuffer-select-agent-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\A)))))

(test default-keymap-skill-bindings
  "Default keymap binds skill insertion and toggling under C-c."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::minibuffer-insert-skill-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\s))))
  (is (eq 'clawmacs::minibuffer-toggle-skill-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\S)))))

(test default-keymap-model-selector-binding
  "Default keymap binds C-c C-m (C-c Return) to the minibuffer model selector,
and C-c M (capital M) to the old overlay model selector."
  (clawmacs::init-default-keymap)
  ;; C-c C-m -> minibuffer model selector (new)
  (is (eq 'clawmacs::minibuffer-select-model-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\Return))))
  ;; Some terminals send #\Newline (LF) for Enter
  (is (eq 'clawmacs::minibuffer-select-model-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\Newline))))
  ;; C-c M (capital M) -> old overlay model selector
  (is (eq 'clawmacs::select-model-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\M)))))

(test default-keymap-think-selector-binding
  "Default keymap binds C-c C-r to the minibuffer think selector,
and C-c R to the old overlay think selector."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::minibuffer-select-think-level-command
          (keymap-lookup *default-keymap* (list :ctrl-c (code-char 18)))))
  (is (eq 'clawmacs::select-think-level-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\R)))))

(test default-keymap-describe-function-binding
  "Default keymap binds C-c f to describe-function-command."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::describe-function-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\f)))))

(test default-keymap-describe-bindings-binding
  "Default keymap binds C-c b to describe-bindings-command."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::describe-bindings-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\b)))))

(test default-keymap-describe-variable-binding
  "Default keymap binds C-c v to describe-variable-command."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::describe-variable-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\v)))))

(test default-keymap-describe-type-binding
  "Default keymap binds C-c T (capital T) to describe-type-command."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::describe-type-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\T)))))

(test default-keymap-customize-face-binding
  "Default keymap binds C-c F (capital F) to customize-face-command."
  (clawmacs::init-default-keymap)
  (is (eq 'clawmacs::customize-face-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\F)))))
