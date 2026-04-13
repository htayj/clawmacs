(in-package :clawmacs)

(register-package-prompt-section
 "sexed"
 "## Structural editing with sexed

- Use the `sexed_*` provider tools for Lisp source edits. Route normal Sexed
  work through these tools.
- Get an outline before editing. Outlines provide stable form ids, depths, heads,
  names, spans, and previews.
- Selectors are tool input objects such as `{id: 7}`,
  `{head: \"defun\", name: \"foo\"}`, or `{head: \"let\", nth: 0}`. `nth` is
  zero-based. Do not guess selectors for persistent edits.
- For project files, default to `sexed_project_outline`,
  `sexed_project_form_text`, and `sexed_project_edit`.
- For durable edits that should be reviewed first, use
  `sexed_change_set_begin`, `sexed_stage_project_edit`,
  `sexed_change_set_diff`, then `sexed_change_set_apply` or
  `sexed_change_set_discard`.
- For `~/.clawmacs.d/init.lisp`, use `sexed_ensure_init_file`,
  `sexed_init_outline`, `sexed_init_form_text`, `sexed_init_edit`, or the
  staged init edit and change-set tools. Verify with `sexed_init_form_text` or
  `sexed_init_outline` before saying the init file was edited.
- For scratch work, use `sexed_scratch_outline`, `sexed_scratch_form_text`, and
  `sexed_scratch_edit`.
- Direct file tools operate inside the sandbox. Project tools are preferred for
  named project resources.
- Edit operations are `replace`, `delete`, `insert-before`, `insert-after`,
  `wrap`, `splice`, `raise`, `slurp-forward`, and `barf-forward`. Init edit
  tools intentionally expose only `replace`, `insert-before`, and
  `insert-after`.
- Insert helpers add separator whitespace when adjacent forms would otherwise
  touch, and every edit validates that the resulting Lisp text remains
  balanced."
 :title "Structural editing with sexed"
 :package "sexed")

(deftool sexed-tool-text-diagnostics
  :name "sexed_text_diagnostics"
  :description "Check Lisp source text for balanced s-expressions and return parser diagnostics."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((text :type "string"
               :description "Lisp source text to inspect.")))

(deftool sexed-tool-text-outline
  :name "sexed_text_outline"
  :description "Return a structural outline for Lisp source text."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((text :type "string"
               :description "Lisp source text to outline.")
         (depth :type "integer" :required nil
                :description "Only include forms at this depth.")
         (max-depth :type "integer" :required nil
                    :description "Only include forms at or above this depth.")
         (head :type "string" :required nil
               :description "Only include forms whose head symbol matches this value.")
         (limit :type "integer" :required nil
                :description "Maximum number of forms to return.")
         (preview-chars :type "integer" :required nil
                        :description "Maximum characters in each preview.")))

(deftool sexed-tool-text-form-text
  :name "sexed_text_form_text"
  :description "Return the selected form text from Lisp source text."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((text :type "string"
               :description "Lisp source text to inspect.")
         (selector :type "object"
                   :description "Selector object such as {id: 0} or {head: \"defun\", name: \"foo\"}.")))

(deftool sexed-tool-text-edit
  :name "sexed_text_edit"
  :description "Apply one structural edit to Lisp source text and return the edited text."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((text :type "string"
               :description "Lisp source text to edit.")
         (operation :type "string"
                    :description "One of replace, delete, insert-before, insert-after, wrap, splice, raise, slurp-forward, or barf-forward.")
         (selector :type "object"
                   :description "Selector object for the target form.")
         (new-text :type "string" :required nil
                   :description "Replacement or inserted Lisp text for replace and insert operations.")
         (prefix :type "string" :required nil
                 :description "Prefix for wrap.")
         (suffix :type "string" :required nil
                 :description "Suffix for wrap.")
         (child-selector :type "object" :required nil
                         :description "Child selector for raise.")
         (count :type "integer" :required nil
                :description "Sibling count for slurp-forward or barf-forward.")))

(deftool sexed-tool-file-outline
  :name "sexed_file_outline"
  :description "Return a structural outline for a sandbox-local Lisp file."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((path :type "string"
               :description "Sandbox-local path to outline.")
         (depth :type "integer" :required nil
                :description "Only include forms at this depth.")
         (max-depth :type "integer" :required nil
                    :description "Only include forms at or above this depth.")
         (head :type "string" :required nil
               :description "Only include forms whose head symbol matches this value.")
         (limit :type "integer" :required nil
                :description "Maximum number of forms to return.")
         (preview-chars :type "integer" :required nil
                        :description "Maximum characters in each preview.")))

(deftool sexed-tool-file-form-text
  :name "sexed_file_form_text"
  :description "Return the selected form text from a sandbox-local Lisp file."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((path :type "string"
               :description "Sandbox-local path to inspect.")
         (selector :type "object"
                   :description "Selector object for the target form.")))

(deftool sexed-tool-file-edit
  :name "sexed_file_edit"
  :description "Apply one structural edit to a sandbox-local Lisp file."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((path :type "string"
               :description "Sandbox-local path to edit.")
         (operation :type "string"
                    :description "One of replace, delete, insert-before, insert-after, wrap, splice, raise, slurp-forward, or barf-forward.")
         (selector :type "object"
                   :description "Selector object for the target form.")
         (new-text :type "string" :required nil
                   :description "Replacement or inserted Lisp text for replace and insert operations.")
         (prefix :type "string" :required nil
                 :description "Prefix for wrap.")
         (suffix :type "string" :required nil
                 :description "Suffix for wrap.")
         (child-selector :type "object" :required nil
                         :description "Child selector for raise.")
         (count :type "integer" :required nil
                :description "Sibling count for slurp-forward or barf-forward.")))

(deftool sexed-tool-project-outline
  :name "sexed_project_outline"
  :description "Return a structural outline for a project Lisp file."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Project name.")
         (path :type "string"
               :description "Project-relative path to outline.")
         (depth :type "integer" :required nil
                :description "Only include forms at this depth.")
         (max-depth :type "integer" :required nil
                    :description "Only include forms at or above this depth.")
         (head :type "string" :required nil
               :description "Only include forms whose head symbol matches this value.")
         (limit :type "integer" :required nil
                :description "Maximum number of forms to return.")
         (preview-chars :type "integer" :required nil
                        :description "Maximum characters in each preview.")))

(deftool sexed-tool-project-form-text
  :name "sexed_project_form_text"
  :description "Return the selected form text from a project Lisp file."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Project name.")
         (path :type "string"
               :description "Project-relative path to inspect.")
         (selector :type "object"
                   :description "Selector object for the target form.")))

(deftool sexed-tool-project-edit
  :name "sexed_project_edit"
  :description "Apply one structural edit directly to a project Lisp file."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Project name.")
         (path :type "string"
               :description "Project-relative path to edit.")
         (operation :type "string"
                    :description "One of replace, delete, insert-before, insert-after, wrap, splice, raise, slurp-forward, or barf-forward.")
         (selector :type "object"
                   :description "Selector object for the target form.")
         (new-text :type "string" :required nil
                   :description "Replacement or inserted Lisp text for replace and insert operations.")
         (prefix :type "string" :required nil
                 :description "Prefix for wrap.")
         (suffix :type "string" :required nil
                 :description "Suffix for wrap.")
         (child-selector :type "object" :required nil
                         :description "Child selector for raise.")
         (count :type "integer" :required nil
                :description "Sibling count for slurp-forward or barf-forward.")))

(deftool sexed-tool-stage-project-edit
  :name "sexed_stage_project_edit"
  :description "Stage one structural edit to a project Lisp file without writing it yet."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Project name.")
         (path :type "string"
               :description "Project-relative path to stage.")
         (operation :type "string"
                    :description "One of replace, delete, insert-before, insert-after, wrap, splice, raise, slurp-forward, or barf-forward.")
         (selector :type "object"
                   :description "Selector object for the target form.")
         (new-text :type "string" :required nil
                   :description "Replacement or inserted Lisp text for replace and insert operations.")
         (prefix :type "string" :required nil
                 :description "Prefix for wrap.")
         (suffix :type "string" :required nil
                 :description "Suffix for wrap.")
         (child-selector :type "object" :required nil
                         :description "Child selector for raise.")
         (count :type "integer" :required nil
                :description "Sibling count for slurp-forward or barf-forward.")
         (change-set :type "string" :required nil
                     :description "Optional change-set id. Omit to use the current change set or create one.")))

(deftool sexed-tool-ensure-init-file
  :name "sexed_ensure_init_file"
  :description "Ensure ~/.clawmacs.d/init.lisp exists, optionally creating it with content."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((content :type "string" :required nil
                  :description "Initial file content to use when init.lisp is missing.")))

(deftool sexed-tool-init-outline
  :name "sexed_init_outline"
  :description "Return a structural outline for ~/.clawmacs.d/init.lisp."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((depth :type "integer" :required nil
                :description "Only include forms at this depth.")
         (max-depth :type "integer" :required nil
                    :description "Only include forms at or above this depth.")
         (head :type "string" :required nil
               :description "Only include forms whose head symbol matches this value.")
         (limit :type "integer" :required nil
                :description "Maximum number of forms to return.")
         (preview-chars :type "integer" :required nil
                        :description "Maximum characters in each preview.")))

(deftool sexed-tool-init-form-text
  :name "sexed_init_form_text"
  :description "Return the selected form text from ~/.clawmacs.d/init.lisp."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((selector :type "object"
                   :description "Selector object for the target form.")))

(deftool sexed-tool-init-edit
  :name "sexed_init_edit"
  :description "Apply one structural edit directly to ~/.clawmacs.d/init.lisp."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((operation :type "string"
                    :description "One of replace, insert-before, or insert-after.")
         (selector :type "object"
                   :description "Selector object for the target form.")
         (new-text :type "string"
                   :description "Replacement or inserted Lisp text.")))

(deftool sexed-tool-stage-init-edit
  :name "sexed_stage_init_edit"
  :description "Stage one structural edit to ~/.clawmacs.d/init.lisp without writing it yet."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((operation :type "string"
                    :description "One of replace, insert-before, or insert-after.")
         (selector :type "object"
                   :description "Selector object for the target form.")
         (new-text :type "string"
                   :description "Replacement or inserted Lisp text.")
         (change-set :type "string" :required nil
                     :description "Optional change-set id. Omit to use the current change set or create one.")))

(deftool sexed-tool-scratch-outline
  :name "sexed_scratch_outline"
  :description "Return a structural outline for the scratch buffer."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((depth :type "integer" :required nil
                :description "Only include forms at this depth.")
         (max-depth :type "integer" :required nil
                    :description "Only include forms at or above this depth.")
         (head :type "string" :required nil
               :description "Only include forms whose head symbol matches this value.")
         (limit :type "integer" :required nil
                :description "Maximum number of forms to return.")
         (preview-chars :type "integer" :required nil
                        :description "Maximum characters in each preview.")))

(deftool sexed-tool-scratch-form-text
  :name "sexed_scratch_form_text"
  :description "Return the selected form text from the scratch buffer."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((selector :type "object"
                   :description "Selector object for the target form.")))

(deftool sexed-tool-scratch-edit
  :name "sexed_scratch_edit"
  :description "Apply one structural edit to the scratch buffer."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((operation :type "string"
                    :description "One of replace, delete, insert-before, insert-after, wrap, splice, raise, slurp-forward, or barf-forward.")
         (selector :type "object"
                   :description "Selector object for the target form.")
         (new-text :type "string" :required nil
                   :description "Replacement or inserted Lisp text for replace and insert operations.")
         (prefix :type "string" :required nil
                 :description "Prefix for wrap.")
         (suffix :type "string" :required nil
                 :description "Suffix for wrap.")
         (child-selector :type "object" :required nil
                         :description "Child selector for raise.")
         (count :type "integer" :required nil
                :description "Sibling count for slurp-forward or barf-forward.")))

(deftool sexed-tool-begin-change-set
  :name "sexed_change_set_begin"
  :description "Open a change set for staged sexed edits."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((name :type "string" :required nil
               :description "Optional short change-set name.")
         (description :type "string" :required nil
                      :description "Optional change-set description.")))

(deftool sexed-tool-change-set-diff
  :name "sexed_change_set_diff"
  :description "Return the diff for the current or named staged change set."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((change-set :type "string" :required nil
                     :description "Optional change-set id. Omit to use the current change set.")))

(deftool sexed-tool-apply-change-set
  :name "sexed_change_set_apply"
  :description "Apply the current or named staged change set."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((change-set :type "string" :required nil
                     :description "Optional change-set id. Omit to use the current change set.")))

(deftool sexed-tool-discard-change-set
  :name "sexed_change_set_discard"
  :description "Discard the current or named staged change set."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((change-set :type "string" :required nil
                     :description "Optional change-set id. Omit to use the current change set.")))
