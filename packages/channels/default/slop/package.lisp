(in-package :clawmacs)

(register-package-prompt-section
 "slop"
 "## Symbol lookup with slop

- Use the `slop_*` provider tools for Common Lisp symbol lookup, origin
  probes, call/reference searches, lexical variable use searches, and safe
  lexical variable renames.
- `slop` is a static source analyzer for project Lisp files. Its results are
  source spans, ids, roles, namespaces, packages, enclosing definitions, and
  previews that are meant to guide precise follow-up reads or structural edits.
- Start with `slop_project_symbols`, `slop_find_definitions`, or
  `slop_find_definitions_batch` when locating definitions. Use
  `slop_find_references`, `slop_find_callers`, `slop_find_callees`, and
  `slop_trace_calls` for relationships between definitions.
- Use `slop_definition_context` when you need a definition body plus nearby
  top-level forms and package context without reading the whole file.
- Use `slop_find_mentions` for docs, tests, config, quoted strings, and other
  text mentions that are intentionally outside source-reference indexing.
- Use `slop_symbol_at` when you know a file location and need the symbol,
  binding, or enclosing definition at that point.
- Use `slop_find_variable_uses` with a `binding-id` or source location to
  inspect one lexical variable's binding, accesses, and sets.
- `slop_rename_variable` only renames lexical variables. It refuses ambiguous
  or unsafe renames, validates balanced Lisp source, and writes directly to the
  project file on disk.
- For exact source text or structural edits outside lexical variable rename,
  pair slop ids and spans with the `sexed_*` tools when that package is
  enabled."
 :title "Symbol lookup with slop"
 :package "slop")

(deftool slop-tool-project-symbols
  :name "slop_project_symbols"
  :description "Search Common Lisp definitions in a project by name, package, kind, or path."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (path :type "string" :required nil
               :description "Optional project-relative file or directory to restrict indexing.")
         (package :type "string" :required nil
                  :description "Optional package name to match.")
         (kind :type "string" :required nil
               :description "Optional definition kind such as function, macro, method, class, parameter, package, tool, or command.")
         (name-pattern :type "string" :required nil
                       :description "Optional symbol name pattern. Exact by default; set substring true for substring matching.")
         (substring :type "boolean" :required nil
                    :description "Use substring matching for name-pattern.")
         (include-body :type "boolean" :required nil
                       :description "Include full source body for matching definitions.")
         (limit :type "integer" :required nil
                :description "Maximum number of definitions to return.")))

(deftool slop-tool-symbol-at
  :name "slop_symbol_at"
  :description "Return the symbol, binding, and enclosing definition at a Common Lisp source location."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (path :type "string"
               :description "Project-relative Lisp source path.")
         (offset :type "integer" :required nil
                 :description "Optional 0-based character offset. Takes precedence over line/column.")
         (line :type "integer" :required nil
               :description "Optional 1-based line number.")
         (column :type "integer" :required nil
                 :description "Optional 1-based column number.")))

(deftool slop-tool-find-definitions
  :name "slop_find_definitions"
  :description "Find Common Lisp definitions for a symbol in a project."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (symbol :type "string"
                 :description "Symbol name such as foo, pkg:foo, or *option*.")
         (path :type "string" :required nil
               :description "Optional project-relative file or directory to restrict indexing.")
         (package :type "string" :required nil
                  :description "Optional package name for unqualified symbols.")
         (namespace :type "string" :required nil
                    :description "Optional namespace: function, variable, type, package, system, or documentation.")
         (kind :type "string" :required nil
               :description "Optional definition kind such as function, macro, method, class, parameter, package, tool, or command.")
         (include-body :type "boolean" :required nil
                       :description "Include full source body for matching definitions.")
         (limit :type "integer" :required nil
                :description "Maximum number of definitions to return.")))

(deftool slop-tool-find-definitions-batch
  :name "slop_find_definitions_batch"
  :description "Find Common Lisp definitions for multiple symbols in one project index pass."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (symbols :type "array" :items ((:type . "string"))
                  :description "Symbol names such as foo, pkg:foo, or *option*.")
         (path :type "string" :required nil
               :description "Optional project-relative file or directory to restrict indexing.")
         (package :type "string" :required nil
                  :description "Optional package name for unqualified symbols.")
         (namespace :type "string" :required nil
                    :description "Optional namespace: function, variable, type, package, system, or documentation.")
         (kind :type "string" :required nil
               :description "Optional definition kind such as function, macro, method, class, parameter, package, tool, or command.")
         (include-body :type "boolean" :required nil
                       :description "Include full source body for matching definitions.")
         (per-symbol-limit :type "integer" :required nil
                           :description "Maximum number of definitions to return for each symbol.")))

(deftool slop-tool-find-references
  :name "slop_find_references"
  :description "Find Common Lisp references for a symbol or slop definition id; stale ids fall back to symbol lookup when supplied."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (path :type "string" :required nil
               :description "Optional project-relative file or directory to restrict indexing.")
         (symbol :type "string" :required nil
                 :description "Optional symbol name such as foo or pkg:foo.")
         (package :type "string" :required nil
                  :description "Optional package name for unqualified symbols.")
         (namespace :type "string" :required nil
                    :description "Optional namespace: function or variable.")
         (definition-id :type "string" :required nil
                        :description "Optional slop definition id returned by slop_find_definitions or slop_project_symbols.")
         (role :type "string" :required nil
               :description "Optional role filter such as call, reference, access, set, or binding.")
         (substring :type "boolean" :required nil
                    :description "Use substring matching for symbol when no definition-id is supplied.")
         (limit :type "integer" :required nil
                :description "Maximum number of references to return.")))

(deftool slop-tool-find-callers
  :name "slop_find_callers"
  :description "Find functions or top-level forms that call a selected Common Lisp function definition."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (path :type "string" :required nil
               :description "Optional project-relative file or directory to restrict indexing.")
         (symbol :type "string" :required nil
                 :description "Function symbol when definition-id is not supplied.")
         (definition-id :type "string" :required nil
                        :description "Slop function definition id.")
         (limit :type "integer" :required nil
                :description "Maximum number of caller groups to return.")))

(deftool slop-tool-find-callees
  :name "slop_find_callees"
  :description "List function calls made from a selected Common Lisp definition."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (path :type "string" :required nil
               :description "Optional project-relative file or directory to restrict indexing.")
         (symbol :type "string" :required nil
                 :description "Function symbol when definition-id is not supplied.")
         (definition-id :type "string" :required nil
                        :description "Slop function definition id.")
         (limit :type "integer" :required nil
                :description "Maximum number of callees to return.")))

(deftool slop-tool-trace-calls
  :name "slop_trace_calls"
  :description "Trace Common Lisp call flow from an entry definition to a bounded depth."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (path :type "string" :required nil
               :description "Optional project-relative file or directory to restrict indexing.")
         (symbol :type "string" :required nil
                 :description "Function symbol when definition-id is not supplied.")
         (definition-id :type "string" :required nil
                        :description "Slop function definition id.")
         (direction :type "string" :required nil
                    :description "Trace direction: callees, callers, or both. Defaults to callees.")
         (max-depth :type "integer" :required nil
                    :description "Maximum call graph depth from the entry definition. Defaults to 2.")
         (include-body :type "boolean" :required nil
                       :description "Include full source bodies for traced definitions.")
         (limit :type "integer" :required nil
                :description "Maximum number of trace edges to return.")))

(deftool slop-tool-find-mentions
  :name "slop_find_mentions"
  :description "Find text mentions of a symbol or phrase across project source, docs, tests, and config."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (query :type "string"
                :description "Symbol or phrase to find in text.")
         (path :type "string" :required nil
               :description "Optional project-relative file or directory to restrict search.")
         (substring :type "boolean" :required nil
                    :description "Allow matches embedded inside larger symbols or words.")
         (case-sensitive :type "boolean" :required nil
                         :description "Use case-sensitive text matching.")
         (limit :type "integer" :required nil
                :description "Maximum number of mentions to return.")))

(deftool slop-tool-definition-context
  :name "slop_definition_context"
  :description "Read a Common Lisp definition body with nearby top-level forms and package context."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (path :type "string" :required nil
               :description "Optional project-relative file or directory to restrict indexing.")
         (symbol :type "string" :required nil
                 :description "Symbol name when definition-id is not supplied.")
         (definition-id :type "string" :required nil
                        :description "Slop definition id.")
         (package :type "string" :required nil
                  :description "Optional package name for unqualified symbols.")
         (namespace :type "string" :required nil
                    :description "Optional namespace: function, variable, type, package, system, or documentation.")
         (kind :type "string" :required nil
               :description "Optional definition kind such as function, macro, method, class, parameter, package, tool, or command.")
         (before-forms :type "integer" :required nil
                       :description "Number of preceding top-level forms to include. Defaults to 1.")
         (after-forms :type "integer" :required nil
                      :description "Number of following top-level forms to include. Defaults to 1.")))

(deftool slop-tool-find-variable-uses
  :name "slop_find_variable_uses"
  :description "Find the binding, accesses, and sets for one lexical Common Lisp variable."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (path :type "string" :required nil
               :description "Project-relative source path when selecting by location.")
         (binding-id :type "string" :required nil
                     :description "Slop lexical binding id.")
         (offset :type "integer" :required nil
                 :description "Optional 0-based character offset used with path.")
         (line :type "integer" :required nil
               :description "Optional 1-based line number used with path/column.")
         (column :type "integer" :required nil
                 :description "Optional 1-based column number used with path/line.")
         (limit :type "integer" :required nil
                :description "Maximum number of uses to return.")))

(deftool slop-tool-rename-variable
  :name "slop_rename_variable"
  :description "Safely rename one lexical Common Lisp variable and its known references directly on disk."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((project :type "string"
                  :description "Clawmacs project name.")
         (path :type "string" :required nil
               :description "Project-relative source path when selecting by location.")
         (binding-id :type "string" :required nil
                     :description "Slop lexical binding id.")
         (offset :type "integer" :required nil
                 :description "Optional 0-based character offset used with path.")
         (line :type "integer" :required nil
               :description "Optional 1-based line number used with path/column.")
         (column :type "integer" :required nil
                 :description "Optional 1-based column number used with path/line.")
         (new-name :type "string"
                   :description "New simple unqualified lexical variable name.")))
