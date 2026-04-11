(register-package-prompt-section
 "sexed"
 "## Structural editing with sexed

- Use the `sexed-*` functions for Lisp source edits instead of raw string replacement.
- `(sexed-outline-to-string TEXT :max-depth 2)` shows stable form ids, depths, heads, names,
  spans, and previews.
- `(sexed-find-forms TEXT :head \"defun\" :name \"NAME\")` finds forms by structure.
- `(sexed-form-text TEXT '(:head \"defun\" :name \"NAME\"))` returns one selected form.
- `(sexed-replace-form TEXT SELECTOR NEW-TEXT)` and related pure edit functions return updated
  text only after validating that the full result remains balanced.
- Use `(sexed-balanced-p TEXT)` or `(balanced-parentheses-p TEXT)` for explicit balance checks.
- Prefer project-aware adapters for persistent source edits:
  `(sexed-project-outline-to-string \"PROJECT\" \"PATH\" :head \"defun\")`,
  `(sexed-project-form-text \"PROJECT\" \"PATH\" SELECTOR)`, and
  `(sexed-replace-project-form \"PROJECT\" \"PATH\" SELECTOR NEW-TEXT)`.
- For durable project edits, prefer staged adapters:
  `(sexed-stage-replace-project-form \"PROJECT\" \"PATH\" SELECTOR NEW-TEXT)`,
  then inspect `(change-set-diff-to-string)` and apply with `(apply-change-set)`.
- Direct file adapters such as `(sexed-file-outline-to-string \"PATH\")` remain available for
  sandbox-local compatibility, but project adapters are the default for agent work.
- For scratch buffer edits, prefer scratch adapters:
  `(sexed-replace-scratch-form SELECTOR NEW-TEXT)`,
  `(sexed-insert-after-scratch-form SELECTOR NEW-TEXT)`, and
  `(sexed-scratch-form-text SELECTOR)`.
- For `~/.clawmacs.d/init.lisp`, use the init-specific adapters:
  `(sexed-init-outline-to-string :max-depth 3)`,
  `(sexed-init-form-text SELECTOR)`,
  `(sexed-replace-init-form SELECTOR NEW-TEXT)`,
  `(sexed-insert-before-init-form SELECTOR NEW-TEXT)`,
  `(sexed-insert-after-init-form SELECTOR NEW-TEXT)`, or the staged variants
  `sexed-stage-replace-init-form`, `sexed-stage-insert-before-init-form`, and
  `sexed-stage-insert-after-init-form`.
- Do not guess a selector for `init.lisp`. First call `sexed-init-outline-to-string` or
  `sexed-project-outline-to-string` and choose a selector from the returned ids, heads, and names.
- Do not tell the user `init.lisp` was edited until you verify with
  `(project-read-file \"config\" \"init.lisp\")`, `(sexed-init-form-text SELECTOR)`, or both.
- If `init.lisp` is missing, call `(sexed-ensure-init-file :content \"...\")` or create it through
  the `config` project before attempting structural edits.
- To reset scratch contents, use `(setf (scratch-buffer-text) \"...\")`. Do not try to set
  `(buffer-input-message BUFFER)`; it returns the editable message object, not the text.
- Message adapters such as `sexed-replace-message-form` take a `message` object. Pure functions
  such as `sexed-replace-form` take a string.
- Insert helpers add separator whitespace when adjacent forms would otherwise touch.
- Selectors are plists such as `(:head \"defun\" :name \"foo\")`, `(:head \"let\" :nth 0)`,
  or `(:id 7)`. `:nth` is zero-based.
- Scratch example:
  `(progn
     (ensure-scratch-buffer)
     (setf (scratch-buffer-text) \"(workspace (todo alpha) (todo beta))\")
     (sexed-replace-scratch-form '(:head \"todo\" :nth 1) \"(done beta)\")
     (sexed-insert-after-scratch-form '(:head \"done\") \"(note \\\"checked\\\")\")
     (scratch-buffer-text))`
- init.lisp edit example:
  `(progn
     (sexed-init-outline-to-string :max-depth 3)
     (sexed-replace-init-form '(:id 0) \"(defvar *example* :after)\")
     (project-read-file \"config\" \"init.lisp\"))`
- Transactional project edit example:
  `(let* ((cs (begin-change-set :name \"rename-helper\")))
     (sexed-stage-replace-project-form
      \"PROJECT\" \"src/file.lisp\" '(:head \"defun\" :name \"old\")
      \"(defun old () :new)\")
     (values (change-set-diff-to-string cs)
             (apply-change-set cs)
             (run-project-checks \"PROJECT\")))`"
 :title "Structural editing with sexed"
 :package "sexed")
