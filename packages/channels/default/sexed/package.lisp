(in-package :clawmacs)

(register-package-prompt-section
 "sexed"
 "## Structural editing with sexed

- Use the `sexed_*` provider tools for Lisp source files and Lisp source text.
- For files, prefer the disk-backed tools. Use `sexed_file_*` for ordinary
  sandbox-local paths such as `.cache/...` or user-supplied relative paths.
  Use `sexed_project_outline`, `sexed_project_form_text`, `sexed_project_edit`,
  `sexed_project_read`, and `sexed_project_write` only when you have an actual
  Clawmacs project name and a project-relative path.
- Before editing an existing file, call an outline tool with no filters first.
  Outlines provide stable form ids, depths, heads, names, spans, and previews;
  add filters such as `head` or `max-depth` only after you have seen the broad
  outline.
- Structural edit and form-text tools require a `selector` input object. Use
  selectors such as `{id: 7}`, `{head: \"defun\", name: \"foo\"}`, or
  `{head: \"let\", nth: 0}`. `nth` is zero-based.
- Prefer `{head: \"defun\", name: \"foo\"}` or similar head/name selectors for
  named top-level forms. Use numeric ids only when you are copying an id that
  appeared in the outline result.
- Edit operations are `replace`, `delete`, `insert-before`, `insert-after`,
  `wrap`, `splice`, `raise`, `slurp-forward`, and `barf-forward`.
- For tasks with several changes in the same file, prefer `sexed_file_edits`
  or `sexed_project_edits` so the edits are applied in one balanced write.
- Use `sexed_project_write` or `sexed_file_write` to create or overwrite a
  complete Lisp file. Every write and edit validates that the resulting source
  has balanced parentheses before touching disk."
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
  :description "Apply one structural edit to Lisp source text and return the edited text. Requires selector, usually copied from sexed_text_outline."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((text :type "string"
               :description "Lisp source text to edit.")
         (operation :type "string"
                    :description "One of replace, delete, insert-before, insert-after, wrap, splice, raise, slurp-forward, or barf-forward.")
         (selector :type "object"
                   :description "Selector object for the target form.")
         (new-text :type "string" :required nil
                   :description "Replacement or inserted Lisp source for replace and insert operations.")
         (prefix :type "string" :required nil
                 :description "Prefix for wrap.")
         (suffix :type "string" :required nil
                 :description "Suffix for wrap.")
         (child-selector :type "object" :required nil
                         :description "Child selector for raise.")
         (count :type "integer" :required nil
                :description "Sibling count for slurp-forward or barf-forward.")))

(deftool sexed-tool-text-edits
  :name "sexed_text_edits"
  :description "Apply a batch of structural edits to Lisp source text and return the edited text. Prefer head/name selectors because edits are applied sequentially."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((text :type "string"
               :description "Lisp source text to edit.")
         (edits :type "array" :items ((:type . "object"))
                :description "Array of edit objects. Each edit has operation, selector, and for replace/insert operations new-text/newtext.")))

(deftool sexed-tool-file-read
  :name "sexed_file_read"
  :description "Read a sandbox-local Lisp source file from disk."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((path :type "string"
               :description "Sandbox-local path to read.")))

(deftool sexed-tool-file-write
  :name "sexed_file_write"
  :description "Create or overwrite a sandbox-local Lisp source file after balance validation."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((path :type "string"
               :description "Sandbox-local path to write.")
         (content :type "string"
                  :description "Complete Lisp source content to write.")))

(deftool sexed-tool-file-outline
  :name "sexed_file_outline"
  :description "Return a structural outline for a sandbox-local Lisp source file."
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
  :description "Return the selected form text from a sandbox-local Lisp source file."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((path :type "string"
               :description "Sandbox-local path to inspect.")
         (selector :type "object"
                   :description "Selector object for the target form.")))

(deftool sexed-tool-file-edit
  :name "sexed_file_edit"
  :description "Apply one structural edit directly to a sandbox-local Lisp source file. Requires selector, usually copied from sexed_file_outline."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((path :type "string"
               :description "Sandbox-local path to edit.")
         (operation :type "string"
                    :description "One of replace, delete, insert-before, insert-after, wrap, splice, raise, slurp-forward, or barf-forward.")
         (selector :type "object"
                   :description "Selector object for the target form.")
         (new-text :type "string" :required nil
                   :description "Replacement or inserted Lisp source for replace and insert operations.")
         (prefix :type "string" :required nil
                 :description "Prefix for wrap.")
         (suffix :type "string" :required nil
                 :description "Suffix for wrap.")
         (child-selector :type "object" :required nil
                         :description "Child selector for raise.")
         (count :type "integer" :required nil
                :description "Sibling count for slurp-forward or barf-forward.")))

(deftool sexed-tool-file-edits
  :name "sexed_file_edits"
  :description "Apply a batch of structural edits directly to a sandbox-local Lisp source file in one balanced write. Prefer this for multi-change file tasks."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((path :type "string"
               :description "Sandbox-local path to edit.")
         (edits :type "array" :items ((:type . "object"))
                :description "Array of edit objects. Each edit has operation, selector, and for replace/insert operations new-text/newtext. Prefer head/name selectors.")))

(deftool sexed-tool-project-read
  :name "sexed_project_read"
  :description "Read a project Lisp source file from disk."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Project name.")
         (path :type "string"
               :description "Project-relative path to read.")))

(deftool sexed-tool-project-write
  :name "sexed_project_write"
  :description "Create or overwrite a project Lisp source file after balance validation."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Project name.")
         (path :type "string"
               :description "Project-relative path to write.")
         (content :type "string"
                  :description "Complete Lisp source content to write.")))

(deftool sexed-tool-project-outline
  :name "sexed_project_outline"
  :description "Return a structural outline for a project Lisp source file."
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
  :description "Return the selected form text from a project Lisp source file."
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
  :description "Apply one structural edit directly to a project Lisp source file. Requires selector, usually copied from sexed_project_outline."
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
                   :description "Replacement or inserted Lisp source for replace and insert operations.")
         (prefix :type "string" :required nil
                 :description "Prefix for wrap.")
         (suffix :type "string" :required nil
                 :description "Suffix for wrap.")
         (child-selector :type "object" :required nil
                         :description "Child selector for raise.")
         (count :type "integer" :required nil
                :description "Sibling count for slurp-forward or barf-forward.")))

(deftool sexed-tool-project-edits
  :name "sexed_project_edits"
  :description "Apply a batch of structural edits directly to a project Lisp source file in one balanced write. Prefer this for multi-change project tasks."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Project name.")
         (path :type "string"
               :description "Project-relative path to edit.")
         (edits :type "array" :items ((:type . "object"))
                :description "Array of edit objects. Each edit has operation, selector, and for replace/insert operations new-text/newtext. Prefer head/name selectors.")))
