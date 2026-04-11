(in-package :clawmacs)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (require :sb-bsd-sockets))

;;; --------------------------------------------------------------------------
;;; Configuration
;;; --------------------------------------------------------------------------

(defvar *default-provider* :zai
  "The default LLM provider to use when no agent-specific override is set.
Must be a keyword matching a known provider (:openai-codex, :zai, :openrouter).")

(defvar *default-model* "glm-5"
  "The default model to use when no agent-specific or provider-fallback model
is configured. Should be a valid model name for *default-provider*.")

(defvar *default-max-tokens* 8192
  "Default maximum tokens for LLM responses across all providers.")

(defvar *openai-codex-model* "gpt-5.3-codex"
  "The OpenAI Codex model to use for chat completions.")

(defvar *openai-codex-api-base-url* "https://api.openai.com/v1"
  "The OpenAI API base URL used for API-key style Codex requests.")

(defvar *openai-codex-chatgpt-base-url* "https://chatgpt.com/backend-api/codex"
  "The ChatGPT backend base URL used for Codex ChatGPT OAuth requests.")

;;; Z.AI (Zhipu AI) Configuration
(defvar *zai-model* "glm-5"
  "The Z.AI model to use for chat completions.
GLM-5 is the flagship model with 200K context window.")

(defvar *zai-api-url* "https://api.z.ai/api/coding/paas/v4/chat/completions"
  "The Z.AI Chat Completions API endpoint for GLM Coding plan subscribers.
Uses the coding-specific endpoint for subscription-based access.")

;;; OpenRouter Configuration
(defvar *openrouter-model* "openai/gpt-5.3-codex"
  "The default OpenRouter model to use for chat completions.
Model names follow the 'provider/model-name' format (e.g. 'openai/gpt-5.3-codex',
'google/gemini-2.5-pro', 'z-ai/glm-4.6'). These are OpenRouter
model identifiers and do not imply direct Anthropic provider support.")

(defvar *openrouter-api-url* "https://openrouter.ai/api/v1/chat/completions"
  "The OpenRouter Chat Completions API endpoint.
OpenRouter normalizes to the OpenAI-compatible chat completions schema.")

(defvar *openrouter-models-url* "https://openrouter.ai/api/v1/models"
  "The OpenRouter models listing endpoint.
Returns the full catalog of available models.")

(defvar *openrouter-env-var* "OPENROUTER_API_KEY"
  "Environment variable name for the OpenRouter API key.
When set, this takes highest priority over the static token file.")

(defvar *openrouter-cached-models* nil
  "Cached list of OpenRouter model ID strings fetched from the API.
Populated on first call to fetch-openrouter-models.  Set to nil to force refresh.")

;;; OpenAI Codex OAuth 2.0 Configuration
(defvar *openai-oauth-client-id* "app_EMoamEEZ73f0CkXaXp7hrann"
  "OpenAI Codex OAuth 2.0 public client identifier.")

(defvar *openai-oauth-auth-url* "https://auth.openai.com/oauth/authorize"
  "OpenAI OAuth 2.0 authorization endpoint.")

(defvar *openai-oauth-token-url* "https://auth.openai.com/oauth/token"
  "OpenAI OAuth 2.0 token exchange endpoint.")

(defvar *openai-oauth-default-port* 1455
  "Preferred localhost port for the OpenAI Codex OAuth callback server.")

(defvar *openai-oauth-callback-path* "/auth/callback"
  "HTTP path served by the local OpenAI Codex OAuth callback server.")

(defvar *openai-oauth-redirect-uri*
  (format nil "http://localhost:~D~A"
          *openai-oauth-default-port*
          *openai-oauth-callback-path*)
  "Default OAuth redirect URI used when the preferred callback port is available.")

(defvar *openai-oauth-scopes*
  "openid profile email offline_access api.connectors.read api.connectors.invoke"
  "OAuth scopes requested from OpenAI, aligned with Codex.")

(defvar *openai-oauth-originator* "codex_cli_rs"
  "Originator value sent in the OpenAI Codex OAuth browser flow and API requests.")

(defparameter +default-codex-auth-path+
  (merge-pathnames #P".codex/auth.json" (user-homedir-pathname))
  "Default path to the shared Codex auth.json credential store.")

(defvar *codex-auth-path* +default-codex-auth-path+
  "Path to the shared Codex auth.json credential store.")

(defparameter +default-personality-prompt-path+
  (merge-pathnames #P".config/clawmacs/system-prompt.txt" (user-homedir-pathname))
  "Default path for the optional personality prompt file.")

(defvar *personality-prompt-path*
  +default-personality-prompt-path+
  "Path to an optional personality prompt file.")

(defvar *agent-defaults-path*
  (merge-pathnames #P".config/clawmacs/agent-defaults.json" (user-homedir-pathname))
  "Path to the persisted agent defaults registry.")

(defvar *agent-defaults-registry* nil
  "Memoized agent defaults registry.")

(defvar *agent-definition-registry* (make-hash-table :test #'equal)
  "Programmatic agent definitions keyed by downcased agent name.")

(defvar *agent-prompt-overrides* nil
  "Dynamic prompt overrides keyed by normalized agent name.
Each entry is (NAME-KEY . PLIST) and is intended for transient subagent runs.")

(defstruct agent-definition
  "Programmatic agent definition used for per-buffer routing and prompt composition."
  name
  provider
  model
  think-level
  core-prompt
  personality-prompt
  tool-names)

(defparameter +default-core-system-prompt+
  "You are running inside clawmacs, a Lisp-native terminal chat interface.
The only built-in tool available by default is `lisp_eval`, which evaluates one Common Lisp
form inside the running clawmacs image. Use `lisp_eval` for concrete work.

## Subagents

- This system can run multiple agents. You are the most powerful type because you can call
  Common Lisp directly with `lisp_eval`; that power is flexible but not always efficient.
- Delegate focused work with `(run-subagent \"PROMPT\" ...)` when a constrained agent can answer
  faster, inspect a narrower surface, or enforce a tool policy.
- Use an existing registered agent with `(run-subagent \"PROMPT\" :agent-name \"docs\")`, then add
  overrides such as `:provider`, `:model`, `:think-level`, `:personality-prompt`, or `:tool-names`.
- Use a fully custom transient agent with `:core-prompt` and `:personality-prompt`; this does not
  permanently register a new agent definition.
- Use `(run-subagent-async \"PROMPT\" ...)` for concurrent background delegation. It returns a
  handle that you can inspect with `(subagent-snapshot HANDLE)`, poll with `(subagent-status HANDLE)`,
  wait on with `(wait-subagent HANDLE :timeout 120)`, or cancel with `(cancel-subagent HANDLE)`.
- Use `:tool-names '(\"doc_lookup\")` to constrain a subagent to specific registered tools instead
  of the default tool set. Parent agents can inspect `(prompt-run-result-tool-events RESULT)` or
  use `(prompt-run-used-tool-p RESULT \"doc_lookup\")`, `(prompt-run-tool-count RESULT)`, and
  `(prompt-run-tool-names RESULT)` to verify what happened.
- Use `(make-subagent-tool :name \"lookup\" :description \"...\" :input-schema '((:type . \"object\") ...)
  :execute-fn (lambda (args) ...))` with `:custom-tools` to expose temporary Lisp functions to one
  subagent run. Temporary tools do not mutate the global tool registry. If `:custom-tools` is supplied
  without `:tool-names`, only those temporary tools are exposed to that subagent.

## Default workflow

- Do not merely describe searches, inspections, calls, or updates. Perform them with
  `lisp_eval` first, then report the result.
- Never answer a concrete user request with a future-tense promise such as `I'll do it now`
  or `I will continue` when a `lisp_eval` action is available. If the user asks you to
  run, edit, inspect, test, or continue a concrete task, your next assistant action should
  normally be `lisp_eval`.
- Use `lisp_eval` for environment inspection, symbol search, documentation lookup,
  function calls, data transformation, and runtime changes.
- `lisp_eval` evaluates one form per call. When a task needs multiple steps, wrap them
  in a single form such as `progn`, `let`, or `let*`.
- Prefer batching related inspection/edit/check work into one well-structured Lisp form
  when the steps are already clear. This keeps prompt-mode reliable and avoids wasting
  tool iterations on tiny calls.
- In `let`/`let*`, each binding must be `(variable value-form)`. Put side-effect calls
  such as `(project-save-file ...)` in the body, or wrap them in `progn`; do not place
  raw call forms in the binding list.
- `(count-occurrences \"needle\" TEXT)` is available for simple non-overlapping substring
  counts; prefer it over inventing ad hoc counting helpers.
- Common Lisp strings do not treat `\\n` as a newline escape. Use `(string #\\Newline)`,
  `(format nil \"~%\")`, or a literal line break when constructing multi-line text.
- The tool's `:package` argument defaults to `CLAWMACS`. Set it explicitly when you need
  another package instead of relying on `in-package`.
- `lisp_eval` captures printed stdout/stderr. Use `(eval-history-to-string)` to inspect
  recent evals, `*last-eval-result*` for the last successful multiple-value list, and
  `*last-eval-condition*` for the last failed condition.
- If `lisp_eval` fails, inspect `*last-eval-condition*`, correct the form, and retry or
  report the concrete blocker. Do not ask the user to ask again, and do not claim work is
  done until a follow-up eval verifies the result.

## Searching the image

- Never guess Clawmacs symbol names. Before calling an unfamiliar function, variable, type, or
  command, use the list/describe helpers in this section to discover the exact symbol and calling
  convention.
- `(list-functions)` - returns a sorted list of exported function symbols.
- `(list-variables)` - returns a sorted list of exported variable symbols.
- `(list-types)` - returns a sorted list of exported type symbols.
- `(apropos \"SUBSTRING\")` - searches all visible symbols.
- `(apropos-list \"SUBSTRING\" :clawmacs)` - searches the `:clawmacs` package and returns matches.
- `(multiple-value-list (find-symbol \"NAME\" :clawmacs))` - checks whether a symbol exists and
  whether it is external or internal.
- `(list-project-systems)` - lists clawmacs and the ASDF systems it imports.
- `(describe-system-to-string \"SYSTEM\")` - summarizes an imported system from its local ASD,
  package definitions, and docs.
- `(list-system-packages \"SYSTEM\")` - lists package names defined by an imported system.
- `(undocumented-functions)`, `(undocumented-variables)`, and `(undocumented-types)` help find
  symbols that are missing extended docs.

## Finding documentation

- The bundled standard-language reference is `cl-community-spec`, a vendored offline snapshot of the
  open CL Community Spec. Use it for ANSI Common Lisp questions.
- `(describe-common-lisp-symbol-to-string 'SYMBOL)` - returns the local `cl-community-spec` entry for
  a standard Common Lisp symbol.
- `(search-common-lisp-spec-to-string \"QUERY\")` - searches the bundled `cl-community-spec` index.
- `(describe-function-to-string 'SYMBOL)` - returns detailed function docs, usage, and related symbols.
- `(describe-variable-to-string 'SYMBOL)` - returns detailed variable docs and current-value info.
- `(describe-type-to-string 'SYMBOL)` - returns type, class, struct, or condition docs.
- `(extended-doc 'SYMBOL :PROPERTY)` - returns a specific extended-doc property such as `:usage`,
  `:returns`, `:side-effects`, or `:see-also`.
- `(documentation 'SYMBOL 'function)` and `(documentation 'SYMBOL 'variable)` read the standard
  docstring when you only need the built-in documentation entry.
- `(describe-library-symbol-to-string \"SYSTEM\" 'SYMBOL)` - summarizes a library symbol using local
  package introspection, docstrings, and source/docs hits.
- `(search-system-docs \"SYSTEM\" \"QUERY\")` - searches local README/docs/source files for imported
  libraries and for clawmacs itself.
- Use `describe-common-lisp-symbol-to-string` for standard `COMMON-LISP` symbols, and use the
  system/package helpers for imported libraries or SBCL-specific APIs.

## Packages

- Clawmacs has a three-tier package model: lean core runtime, bundled packages from channels, and
  third-party packages.
- `(list-package-channels)` lists registered local channels.
- `(list-available-packages)` lists packages discovered from those channels.
- `(find-available-package \"NAME\")` returns the package definition for an available package.
- `(load-clawmacs-package \"NAME\")` loads a channel package and its dependencies.
- `(clawmacs-use-package :src-type :git :repo \"URL\")` installs and loads a third-party git package.
- Loaded packages may contribute additional system-prompt sections below the core instructions.

## Skills

- Skills are local instruction bundles stored in `SKILL.md` files and listed in the system prompt
  when available.
- Use `(list-skills)` to inspect enabled skill structures and
  `(mapcar #'skill-name (list-skills))` when you need just skill names.
- Use `(list-skills :include-disabled t)` to include disabled skills.
- `(describe-skill-to-string \"SKILL\")` summarizes a skill, its path, and its files.
- `(read-skill-instructions \"SKILL\")` returns the full `SKILL.md` instructions.
- `(skill-list-files \"SKILL\")`, `(skill-read-file \"SKILL\" \"references/file.md\")`, and
  `(skill-search-to-string \"SKILL\" \"QUERY\")` let you inspect referenced skill resources.
- If the user mentions `$skill-name`, use the skill for that turn. Do not carry skills across turns
  unless the user mentions them again.
- Skill scripts and assets are resources to inspect through `lisp_eval`; they do not grant new
  tools or permissions.

## Calling and inspecting

- Call known functions directly once you identify the right entry point.
- Use `funcall` or `apply` when the callee or argument list is dynamic.
- Use `symbol-value`, `boundp`, and `fboundp` to inspect runtime state before mutating it.
- Return a string or data structure from the evaluated form. Prefer `(format nil ...)` over
  `format t`, because printed stdout is not captured by `lisp_eval`.

## Project resources

- Use project resource functions for persistent workspace changes instead of direct filesystem
  reads or writes.
- `(list-projects)` returns registered projects. Each project has `project-name`, `project-root`,
  `project-description`, and `project-source`.
- `(define-project \"NAME\" :root #P\"/path/to/root/\")` registers a project for this process.
- `(create-project \"NAME\" :root #P\"/path/to/root/\")` creates a project and writes an inert
  manifest into `*project-definitions-directory*`; use `:persist nil` for temporary projects.
- Use `create-project` for temporary roots that may not exist. `define-project` registers an
  existing root and does not accept `:persist`.
- Project manifests are data files under `*project-definitions-directory*`, which defaults to
  `~/.clawmacs.projects.d/` and may be customized in init.lisp.
- The user's Clawmacs configuration directory is always available as the `config` project unless
  init.lisp defines a project named `config` first.
- The user's `init.lisp` is the `config` project resource `\"init.lisp\"`. Inspect it with
  `(project-read-file \"config\" \"init.lisp\")`; after any edit, read it again before claiming the
  edit succeeded.
- `(project-list-files \"PROJECT\")` lists project-relative resource paths.
- `(project-read-file \"PROJECT\" \"PATH\")` reads a project resource as text.
- `(project-read-file-lines \"PROJECT\" \"PATH\" 10 40)` reads a numbered line slice. It also
  accepts keyword arguments such as `:line 42 :context 8`.
- `(project-search-to-string \"PROJECT\" \"QUERY\")` searches project resources and returns
  `path:line: text` matches.
- `(project-create-file \"PROJECT\" \"PATH\" :content \"...\" :if-exists :supersede)` creates or
  replaces a project resource. Omit `:if-exists` when you need an error on existing files.
- `(project-save-file \"PROJECT\" \"PATH\" TEXT)` saves text to a project resource.
- `(project-replace-text \"PROJECT\" \"PATH\" OLD NEW)` replaces exact text once.
- `(project-replace-text-between \"PROJECT\" \"PATH\" START-MARKER END-MARKER REPLACEMENT)`
  replaces a marker-bounded span; use this instead of custom substring code for cleanup edits.
- `(project-open-file \"PROJECT\" \"PATH\")` opens a project resource as an editable file buffer.
- For open file buffers, use `(file-buffer-text BUFFER)` and `(setf (file-buffer-text BUFFER) ...)`,
  then `(project-save-buffer BUFFER)`. Use `(file-buffer-dirty-p BUFFER)` to check whether
  there are unsaved file-buffer edits.
- `project-open-file` returns the buffer object to edit; capture that return value. Do not use
  `project-list-files` results as buffers.
- Direct project writes such as `project-save-file`, `project-create-file`, and applied change
  sets synchronize any already-open buffer for the same resource so retries do not edit stale text.
- Project resource paths are relative to their project and cannot use absolute paths or `..`.
- Do not guess project file paths. Before editing an unfamiliar project area, call
  `(project-list-files \"PROJECT\")`, `(project-search-to-string \"PROJECT\" \"QUERY\")`, or the
  project code-intelligence helpers to find the exact resource.
- After any project edit, immediately read back each edited resource with
  `(project-read-file \"PROJECT\" \"PATH\")` or a structure-aware read helper and verify the expected
  text is present. If a TODO was completed, read back `todo.org` and verify that exact item is
  marked `DONE` before claiming completion.
- For durable coding work, prefer transactional change sets over immediate writes:
  `(begin-change-set :name \"short-name\")`,
  `(stage-project-file \"PROJECT\" \"PATH\" TEXT)`,
  `(change-set-diff-to-string)`,
  `(apply-change-set)`,
  `(discard-change-set)`, and `(revert-change-set)`.
- `(change-set-project-file-text \"PROJECT\" \"PATH\")` reads staged text when present, so
  multiple staged edits can compose before applying.
- Projects may expose validation and reload hooks. Use `(run-project-checks \"PROJECT\")`,
  `(compile-project-file \"PROJECT\" \"PATH\")`, `(load-project-file \"PROJECT\" \"PATH\")`,
  and `(reload-project-system \"PROJECT\")` when available.
- Project code-intelligence helpers include `(project-outline-to-string \"PROJECT\")`,
  `(project-find-definitions-to-string \"PROJECT\" :name \"NAME\")`,
  `(project-find-references-to-string \"PROJECT\" \"QUERY\")`,
  `(project-package-map-to-string \"PROJECT\")`, and
  `(project-describe-definition-to-string \"PROJECT\" \"NAME\")`.
- Project file-buffer example:
  `(progn
     (create-project \"tmp\" :root #P\"/tmp/work/\" :persist nil)
     (project-save-file \"tmp\" \"notes.lisp\" \"(note old)\")
     (let ((buf (project-open-file \"tmp\" \"notes.lisp\")))
       (setf (file-buffer-text buf) \"(note new)\")
       (project-save-buffer buf)))`

## Updating runtime state

- Use `(setf ...)` to change variables and slots when the task requires it.
- Use `defun`, `defvar`, and `defparameter` for runtime definitions when appropriate.
- New definitions persist for the lifetime of the clawmacs process.
- Inspect before mutating, and return the new value or a short confirmation summary from the form.
- Do not use `lisp_eval` to dump chain-of-thought. Use it to act on the running system.

## Useful examples

- `*default-model*`
- `(symbol-value '*default-provider*)`
- `(apropos-list \"buffer\" :clawmacs)`
- `(describe-common-lisp-symbol-to-string 'handler-case)`
- `(search-common-lisp-spec-to-string \"dynamic extent\")`
- `(describe-system-to-string \"croatoan\")`
- `(search-system-docs \"alexandria\" \"hash table\")`
- `(describe-function-to-string 'start-streaming-response)`
- `(let* ((syms (apropos-list \"model\" :clawmacs)))
     (format nil \"~{~A~^, ~}\" syms))`

### Common configuration variables

- `*default-model*` - the fallback model name.
- `*default-provider*` - the fallback provider keyword.
- `*default-personality-prompt*` - the default personality prompt.
- `*default-core-system-prompt*` - the default clawmacs operating prompt."
  "Built-in clawmacs operating instructions inserted ahead of the personality prompt.")

(defvar *default-core-system-prompt*
  +default-core-system-prompt+
  "Default clawmacs operating instructions inserted ahead of the personality prompt.")

(defparameter +default-personality-prompt+
  "You are a helpful assistant. Keep private reasoning private. Use normal assistant text only
for direct user-facing replies and concise explanations after you have done the work."
  "Built-in default personality prompt inserted after the clawmacs core system prompt.")

(defvar *default-personality-prompt*
  +default-personality-prompt+
  "Default personality prompt inserted after the clawmacs core system prompt.
Users may override this via *personality-prompt-path* or init.lisp.")

(defvar *boot-file-names*
  '("AGENTS.md" "SOUL.md" "USER.md" "IDENTITY.md" "TOOLS.md")
  "Boot markdown files to load, in order. Checked in the working directory
and ~/.config/clawmacs/. Compatible with OpenClaw workspace conventions.")

(defun load-personality-prompt-file (&optional (path *personality-prompt-path*))
  "Load PATH into the default personality prompt when the file exists.
Returns the trimmed prompt text on success, or NIL when PATH is NIL or missing."
  (let ((prompt-path (and path (probe-file path))))
    (when prompt-path
      (let ((prompt (string-trim '(#\Space #\Tab #\Newline #\Return)
                                 (uiop:read-file-string prompt-path))))
        (setf *default-personality-prompt* prompt)
        prompt))))

(defun load-boot-files ()
  "Load boot MD files from the working directory and ~/.config/clawmacs/.
Returns a concatenated string, or nil if no files found.
Files are loaded in the order specified by *boot-file-names*.
Project-local files take precedence over global ones."
  (let ((parts nil)
        (global-dir (merge-pathnames #P".config/clawmacs/" (user-homedir-pathname)))
        (local-dir (truename ".")))
    (dolist (name *boot-file-names*)
      (let ((local-path (merge-pathnames name local-dir))
            (global-path (merge-pathnames name global-dir)))
        ;; Project-local takes precedence
        (cond
          ((probe-file local-path)
           (push (uiop:read-file-string local-path) parts))
          ((probe-file global-path)
           (push (uiop:read-file-string global-path) parts)))))
    (when parts
      (format nil "~{~A~^~%~%---~%~%~}" (nreverse parts)))))

(defun agent-prompt-override (agent-name)
  "Return the dynamic prompt override plist for AGENT-NAME, or NIL."
  (let ((name-key (normalize-agent-name-key agent-name)))
    (cdr (assoc name-key *agent-prompt-overrides* :test #'string=))))

(defun agent-prompt-override-value (agent-name key)
  "Return dynamic prompt override KEY for AGENT-NAME, or NIL."
  (let ((override (agent-prompt-override agent-name)))
    (and override (getf override key))))

(defun agent-definition-core-prompt-or-default (agent-name)
  "Return AGENT-NAME's core prompt, falling back to the default core system prompt."
  (let ((override (agent-prompt-override-value agent-name :core-prompt))
        (definition (find-agent-definition agent-name)))
    (or override
        (and definition
             (agent-definition-core-prompt definition))
        *default-core-system-prompt*)))

(defun agent-definition-personality-prompt-or-default (agent-name)
  "Return AGENT-NAME's personality prompt, falling back to the default personality prompt."
  (let ((override (agent-prompt-override-value agent-name :personality-prompt))
        (definition (find-agent-definition agent-name)))
    (or override
        (and definition
             (agent-definition-personality-prompt definition))
        *default-personality-prompt*)))

(defun build-agent-system-prompt (agent-name)
  "Build the full system prompt for AGENT-NAME.
Composition order: boot-file prefix, core prompt, package sections, skills section, then personality prompt."
  (let ((parts (remove-if #'null
                          (list (load-boot-files)
                                (agent-definition-core-prompt-or-default agent-name)
                                (render-package-prompt-sections)
                                (render-skills-section)
                                (agent-definition-personality-prompt-or-default agent-name)))))
    (format nil "~{~A~^~%~%---~%~%~}" parts)))

(defun build-system-prompt ()
  "Compatibility wrapper that builds the default agent prompt."
  (build-agent-system-prompt *default-agent-name*))

;;; --------------------------------------------------------------------------
;;; JSON Helpers (underscore-preserving round-trip)
;;; --------------------------------------------------------------------------

(defun json-name-to-lisp (name)
  "Convert a JSON key name to a Lisp identifier string, preserving underscores
as double-dashes so they round-trip correctly through cl-json encoding.
E.g., \"tool_use\" -> \"TOOL--USE\" -> (interned as :TOOL--USE) -> \"tool_use\"."
  (string-upcase
   (with-output-to-string (s)
     (loop :for c :across name
           :do (if (char= c #\_)
                   (write-string "--" s)
                   (write-char c s))))))

(defun api-json-decode (string)
  "Decode a JSON string using underscore-preserving key mapping."
  (let ((cl-json:*json-identifier-name-to-lisp* #'json-name-to-lisp))
    (cl-json:decode-json-from-string string)))

(defun api-json-encode (object)
  "Encode an object to JSON using cl-json's default encoding.
Keys with double-dashes encode as underscores (e.g., :TOOL--USE -> tool_use)."
  (cl-json:encode-json-to-string object))

(defclass json-false-literal ()
  ()
  (:documentation "Marker object that encodes as a literal JSON false value."))

(defparameter +json-false+ (make-instance 'json-false-literal)
  "Shared marker object that encodes as a literal JSON false value.")

(defmethod cl-json:encode-json ((object json-false-literal)
                                &optional (stream cl-json:*json-output*))
  "Encode OBJECT as a literal JSON false value."
  (declare (ignore object))
  (write-string "false" stream))

(declaim (ftype (function (t) (or null list)) normalize-legacy-raw-content))

(defun canonical-text-block (text)
  "Return TEXT as a canonical text block." 
  `((:type . "text")
    (:text . ,(or text ""))))

(defun canonical-reasoning-block (text)
  "Return TEXT as a canonical provider-supplied reasoning block."
  `((:type . "reasoning")
    (:text . ,(or text ""))))

(defun canonicalize-content-block (role block)
  "Normalize BLOCK for ROLE and enforce valid role/block pairings."
  (let ((block-type (cdr (assoc :type block))))
    (cond
      ((string= "text" (or block-type ""))
       (unless (member role '("user" "assistant") :test #'string=)
         (error "text blocks are only allowed on user or assistant messages"))
       (canonical-text-block (cdr (assoc :text block))))
      ((string= "reasoning" (or block-type ""))
       (unless (string= role "assistant")
         (error "reasoning blocks are only allowed on assistant messages"))
       (canonical-reasoning-block (cdr (assoc :text block))))
      ((string= "tool_use" (or block-type ""))
       (unless (string= role "assistant")
         (error "tool_use blocks are only allowed on assistant messages"))
       `((:type . "tool_use")
         (:id . ,(cdr (assoc :id block)))
         (:name . ,(cdr (assoc :name block)))
         (:input . ,(cdr (assoc :input block)))))
      ((string= "tool_result" (or block-type ""))
       (unless (string= role "user")
         (error "tool_result blocks are only allowed on user messages"))
       (let ((tool-use-id (or (cdr (assoc :tool--use--id block))
                              (cdr (assoc :tool-use-id block)))))
         (unless tool-use-id
           (error "tool_result blocks require tool_use_id"))
        `((:type . "tool_result")
          (:tool--use--id . ,tool-use-id)
          (:content . ,(cdr (assoc :content block))))))
      (t
       (error "Unsupported content block type ~S" block-type)))))

(defun canonicalize-message-content (role content)
  "Normalize CONTENT into canonical content blocks for ROLE."
  (cond
    ((or (null content) (stringp content))
     (list (canonicalize-content-block role (canonical-text-block content))))
    ((vectorp content)
     (loop :for block :across content
           :collect (canonicalize-content-block role block)))
    ((listp content)
     (loop :for block :in content
           :collect (canonicalize-content-block role block)))
    (t
     (error "Unsupported message content ~S" content))))

(defun normalize-legacy-raw-content (raw-content)
  "Normalize persisted legacy session raw-content blocks to canonical blocks."
  (when raw-content
    (loop :for block :in (coerce raw-content 'list)
          :collect
          (let ((block-type (cdr (assoc :type block))))
            (cond
              ((string= "text" (or block-type ""))
               `((:type . "text")
                 (:text . ,(cdr (assoc :text block)))))
              ((string= "reasoning" (or block-type ""))
               `((:type . "reasoning")
                 (:text . ,(cdr (assoc :text block)))))
              ((string= "tool_use" (or block-type ""))
               `((:type . "tool_use")
                 (:id . ,(cdr (assoc :id block)))
                 (:name . ,(cdr (assoc :name block)))
                 (:input . ,(cdr (assoc :input block)))))
              ((string= "tool_result" (or block-type ""))
                `((:type . "tool_result")
                  (:tool--use--id . ,(or (cdr (assoc :tool--use--id block))
                                         (cdr (assoc :tool-use-id block))))
                  (:content . ,(cdr (assoc :content block)))))
              (t block))))))

;;; --------------------------------------------------------------------------
;;; Token Management
;;; --------------------------------------------------------------------------

(defun provider-token-path (provider)
  "Return the provider-specific token file path for PROVIDER."
  (merge-pathnames
   (case provider
     (:openai-codex #P".config/clawmacs/openai-codex-token")
     (:zai #P".config/clawmacs/zai-api-key")
     (:openrouter #P".config/clawmacs/openrouter-api-key")
     (otherwise
      (error "Unknown provider ~S. Supported providers: :OPENAI-CODEX, :ZAI, :OPENROUTER"
             provider)))
   (user-homedir-pathname)))

(defvar *zai-env-var* "ZAI_CODING_MAX_API_KEY"
  "Environment variable name for the Z.AI Coding Max API key.
When set, this takes highest priority over the static token file.")

(defun read-env-token (env-var)
  "Read a token from the environment variable named ENV-VAR.
Returns the trimmed token string if the variable is set and non-empty, nil otherwise."
  (let ((value (uiop:getenv env-var)))
    (when (and value (stringp value))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
        (when (plusp (length trimmed))
          trimmed)))))

(defun save-provider-token (provider token)
  "Save TOKEN to PROVIDER's provider-specific token file."
  (let ((token-path (provider-token-path provider)))
    (ensure-directories-exist token-path)
    (with-open-file (s token-path
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
      (write-string token s))
    (ignore-errors
      (uiop:run-program (list "chmod" "600" (namestring token-path))))
    token))

;;; --------------------------------------------------------------------------
;;; OpenAI Codex OAuth 2.0 (PKCE Flow)
;;; --------------------------------------------------------------------------

(defconstant +unix-to-universal-time-offset+ 2208988800
  "Seconds between the Unix epoch and Common Lisp universal time epoch.")

(defconstant +openai-oauth-refresh-staleness-days+ 8
  "Fallback staleness threshold used when JWT expiry cannot be parsed.")

(defconstant +openai-oauth-refresh-leeway-seconds+ 300
  "Refresh ChatGPT OAuth tokens this many seconds before JWT expiry.")

(defun trimmed-file-string (pathname)
  "Read PATHNAME and trim surrounding ASCII whitespace.
Returns NIL when the file is missing or blank."
  (when (probe-file pathname)
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (uiop:read-file-string pathname))))
      (when (plusp (length trimmed))
        trimmed))))

(defun read-provider-file-token (provider)
  "Read PROVIDER's static token file without consulting env vars or OAuth."
  (trimmed-file-string (provider-token-path provider)))

(defun url-like-string-p (string)
  "Return non-nil when STRING looks like an HTTP(S) URL."
  (and (stringp string)
       (or (and (<= 7 (length string))
                (string= "http://" string :end2 7))
           (and (<= 8 (length string))
                (string= "https://" string :end2 8)))))

(defun jwt-like-string-p (string)
  "Return non-nil when STRING looks like a JWT."
  (and (stringp string)
       (= 2 (count #\. string))
       (plusp (length string))))

(defun openai-codex-api-key-override-valid-p (token)
  "Return non-nil when TOKEN is a plausible API-key style override."
  (and (stringp token)
       (plusp (length token))
       (not (url-like-string-p token))
       (not (search "/auth/callback" token :test #'char=))
       (not (search "code=" token :test #'char=))
       (not (search "state=" token :test #'char=))
       (not (jwt-like-string-p token))))

(defun openai-codex-chatgpt-access-token-valid-p (token)
  "Return non-nil when TOKEN is a plausible ChatGPT OAuth access token."
  (and (stringp token)
       (plusp (length token))
       (not (url-like-string-p token))
       (jwt-like-string-p token)))

(defun current-codex-auth-path ()
  "Return the effective auth.json path."
  *codex-auth-path*)

(defun write-private-file (pathname contents)
  "Write CONTENTS to PATHNAME and best-effort chmod the result to 0600."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream))
  (ignore-errors
    (uiop:run-program (list "chmod" "600" (namestring pathname))))
  pathname)

(defun openai-oauth-redirect-uri (&optional (port *openai-oauth-default-port*))
  "Return the localhost callback URI for PORT."
  (format nil "http://localhost:~D~A" port *openai-oauth-callback-path*))

(defun current-rfc3339-timestamp ()
  "Return the current UTC time as a simple RFC 3339 timestamp."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun unix-time->universal-time (unix-time)
  "Convert UNIX-TIME seconds to Common Lisp universal time."
  (+ unix-time +unix-to-universal-time-offset+))

(defun alist-string-value (alist key)
  "Return the non-blank string value for KEY in ALIST."
  (let ((value (cdr (assoc key alist))))
    (when (and (stringp value) (plusp (length value)))
      value)))

(defun openai-codex-auth-json-tokens (auth-json)
  "Return the decoded tokens object from AUTH-JSON."
  (cdr (assoc :tokens auth-json)))

(defun normalize-openai-codex-auth-mode (value)
  "Normalize a persisted auth mode VALUE to :CHATGPT or :API-KEY."
  (cond
    ((or (null value) (eq value :chatgpt)) :chatgpt)
    ((eq value :api-key) :api-key)
    ((stringp value)
     (let ((normalized (string-downcase value)))
       (cond
         ((string= normalized "chatgpt") :chatgpt)
         ((or (string= normalized "api_key")
              (string= normalized "api-key")
              (string= normalized "apikey"))
          :api-key)
         (t :chatgpt))))
    (t :chatgpt)))

(defun openai-codex-resolved-auth-mode (auth-json)
  "Resolve AUTH-JSON's effective auth mode the same way Codex does."
  (let ((explicit-mode (cdr (assoc :auth--mode auth-json)))
        (api-key (alist-string-value auth-json :openai--api--key)))
    (cond
      (explicit-mode
       (normalize-openai-codex-auth-mode explicit-mode))
      (api-key
       :api-key)
      (t
       :chatgpt))))

(defun read-openai-codex-auth-json ()
  "Read the shared Codex auth.json store."
  (let ((auth-path (current-codex-auth-path)))
    (when (probe-file auth-path)
      (handler-case
          (api-json-decode (uiop:read-file-string auth-path))
        (error () nil)))))

(defun openai-codex-auth-payload (&key auth-mode openai-api-key
                                       id-token access-token refresh-token account-id
                                       last-refresh)
  "Build an auth.json payload compatible with Codex."
  (let ((payload nil)
        (tokens nil))
    (when (or id-token access-token refresh-token account-id)
      (setf tokens `((:id--token . ,id-token)
                     (:access--token . ,access-token)
                     (:refresh--token . ,refresh-token)
                     (:account--id . ,account-id))))
    (when auth-mode
      (push `(:auth--mode . ,(ecase auth-mode
                               (:chatgpt "chatgpt")
                               (:api-key "api_key")))
            payload))
    (push `(:openai--api--key . ,openai-api-key) payload)
    (when tokens
      (push `(:tokens . ,tokens) payload))
    (push `(:last--refresh . ,(or last-refresh (current-rfc3339-timestamp))) payload)
    (nreverse payload)))

(defun save-openai-codex-auth-json (auth-json)
  "Persist AUTH-JSON to the shared Codex auth store."
  (write-private-file (current-codex-auth-path)
                      (api-json-encode auth-json))
  auth-json)

(defun base64url-char-value (char)
  "Decode one base64url CHAR to its 6-bit integer value."
  (cond
    ((and (char>= char #\A) (char<= char #\Z))
     (- (char-code char) (char-code #\A)))
    ((and (char>= char #\a) (char<= char #\z))
     (+ 26 (- (char-code char) (char-code #\a))))
    ((and (char>= char #\0) (char<= char #\9))
     (+ 52 (- (char-code char) (char-code #\0))))
    ((char= char #\-) 62)
    ((char= char #\_) 63)
    (t
     (error "Invalid base64url character ~S" char))))

(defun base64url-decode-to-octets (string)
  "Decode base64url STRING into a fresh octet vector."
  (let ((buffer 0)
        (bits 0)
        (bytes (make-array 0
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer 0)))
    (loop :for char :across string
          :unless (char= char #\=)
            :do (progn
                  (setf buffer (logior (ash buffer 6)
                                       (base64url-char-value char))
                        bits (+ bits 6))
                  (loop :while (>= bits 8)
                        :do (decf bits 8)
                            (vector-push-extend (ldb (byte 8 bits) buffer) bytes)
                            (setf buffer (ldb (byte bits 0) buffer)))))
    bytes))

(defun jwt-payload-string (jwt)
  "Return JWT's decoded payload as a UTF-8 string."
  (let* ((parts (split-string-by-char jwt #\.))
         (payload (second parts)))
    (unless (and (= (length parts) 3)
                 payload
                 (plusp (length payload)))
      (error "Invalid JWT format"))
    (flexi-streams:octets-to-string
     (base64url-decode-to-octets payload)
     :external-format :utf-8)))

(defun parse-jwt-expiration (jwt)
  "Return JWT's `exp` claim as universal time, or NIL when unavailable."
  (handler-case
      (let* ((payload (api-json-decode (jwt-payload-string jwt)))
             (exp (cdr (assoc :exp payload))))
        (when (integerp exp)
          (unix-time->universal-time exp)))
    (error () nil)))

(defun json-string-field-value (json-string field-name)
  "Extract a simple quoted FIELD-NAME value from JSON-STRING."
  (let* ((needle (format nil "\"~A\"" field-name))
         (field-pos (search needle json-string)))
    (when field-pos
      (let* ((colon-pos (position #\: json-string :start (+ field-pos (length needle))))
             (value-start (and colon-pos
                               (position #\" json-string :start (1+ colon-pos)))))
        (when value-start
          (let ((value-end (position #\" json-string :start (1+ value-start))))
            (when value-end
              (subseq json-string (1+ value-start) value-end))))))))

(defun parse-rfc3339-timestamp (string)
  "Parse a simple RFC 3339 UTC or offset timestamp STRING to universal time."
  (when (and string (>= (length string) 20))
    (let* ((year (parse-integer string :start 0 :end 4))
           (month (parse-integer string :start 5 :end 7))
           (day (parse-integer string :start 8 :end 10))
           (hour (parse-integer string :start 11 :end 13))
           (minute (parse-integer string :start 14 :end 16))
           (second (parse-integer string :start 17 :end 19))
           (zone-pos (or (position #\Z string :start 19)
                         (position #\+ string :start 19)
                         (position #\- string :start 19)))
           (base (encode-universal-time second minute hour day month year 0)))
      (cond
        ((or (null zone-pos) (char= (char string zone-pos) #\Z))
         base)
        (t
         (let* ((sign (char string zone-pos))
                (offset-hour (parse-integer string :start (1+ zone-pos) :end (+ zone-pos 3)))
                (offset-minute (parse-integer string :start (+ zone-pos 4) :end (+ zone-pos 6)))
                (offset-seconds (+ (* offset-hour 3600) (* offset-minute 60))))
           (if (char= sign #\+)
               (- base offset-seconds)
               (+ base offset-seconds))))))))

(defun openai-codex-id-token-account-id (id-token)
  "Extract the ChatGPT account/workspace ID from ID-TOKEN when present."
  (handler-case
      (json-string-field-value (jwt-payload-string id-token) "chatgpt_account_id")
    (error () nil)))

(defun read-openai-codex-oauth-tokens ()
  "Read the shared OpenAI Codex auth store as a convenience plist."
  (let* ((auth-json (read-openai-codex-auth-json))
         (tokens (and auth-json (openai-codex-auth-json-tokens auth-json)))
         (access-token (and tokens (alist-string-value tokens :access--token))))
    (when auth-json
      (list :auth-mode (openai-codex-resolved-auth-mode auth-json)
            :openai-api-key (alist-string-value auth-json :openai--api--key)
            :id-token (and tokens (alist-string-value tokens :id--token))
            :access-token access-token
            :refresh-token (and tokens (alist-string-value tokens :refresh--token))
            :account-id (and tokens (alist-string-value tokens :account--id))
            :last-refresh (alist-string-value auth-json :last--refresh)
            :expires-at (and access-token (parse-jwt-expiration access-token))))))

(defun openai-codex-chatgpt-auth-stale-p (auth-json)
  "Return non-nil when AUTH-JSON should be proactively refreshed."
  (let* ((tokens (openai-codex-auth-json-tokens auth-json))
         (access-token (and tokens (alist-string-value tokens :access--token))))
    (cond
      ((and access-token
            (let ((expires-at (parse-jwt-expiration access-token)))
              (and expires-at
                   (<= expires-at
                       (+ (get-universal-time)
                          +openai-oauth-refresh-leeway-seconds+)))))
       t)
      (t
       (let ((last-refresh (parse-rfc3339-timestamp
                            (alist-string-value auth-json :last--refresh))))
         (and last-refresh
              (< last-refresh
                 (- (get-universal-time)
                    (* +openai-oauth-refresh-staleness-days+ 24 60 60)))))))))

(defun request-openai-codex-token-refresh (refresh-token)
  "Request a refreshed ChatGPT OAuth token set using REFRESH-TOKEN."
  (multiple-value-bind (body status-code)
      (drakma:http-request
       *openai-oauth-token-url*
       :method :post
       :content-type "application/json"
       :content (api-json-encode
                 `((:client--id . ,*openai-oauth-client-id*)
                   (:grant--type . "refresh_token")
                   (:refresh--token . ,refresh-token)))
       :want-stream nil
       :force-binary nil)
    (let ((body-string (http-body-string body)))
      (unless (= status-code 200)
        (error "OAuth refresh failed (~A): ~A" status-code body-string))
      (let ((response (api-json-decode body-string)))
        (list :id-token (alist-string-value response :id--token)
              :access-token (alist-string-value response :access--token)
              :refresh-token (or (alist-string-value response :refresh--token)
                                 refresh-token))))))

(defun refresh-openai-codex-auth-json (&optional auth-json)
  "Refresh AUTH-JSON in-place via the token endpoint and persist the result."
  (let* ((current-auth (or auth-json (read-openai-codex-auth-json)))
         (tokens (and current-auth (openai-codex-auth-json-tokens current-auth)))
         (refresh-token (and tokens (alist-string-value tokens :refresh--token))))
    (unless (and refresh-token (plusp (length refresh-token)))
      (return-from refresh-openai-codex-auth-json nil))
    (handler-case
        (let* ((refreshed (request-openai-codex-token-refresh refresh-token))
               (existing-id-token (alist-string-value tokens :id--token))
               (new-id-token (or (getf refreshed :id-token) existing-id-token))
               (account-id (or (and tokens (alist-string-value tokens :account--id))
                               (and new-id-token
                                    (openai-codex-id-token-account-id new-id-token))))
               (updated
                 (openai-codex-auth-payload
                  :auth-mode :chatgpt
                  :openai-api-key (alist-string-value current-auth :openai--api--key)
                  :id-token new-id-token
                  :access-token (getf refreshed :access-token)
                  :refresh-token (getf refreshed :refresh-token)
                  :account-id account-id
                  :last-refresh (current-rfc3339-timestamp))))
          (save-openai-codex-auth-json updated)
          updated)
      (error ()
        nil))))

(defun save-openai-codex-oauth-tokens (access-token refresh-token expires-in
                                        &key id-token account-id openai-api-key
                                          (auth-mode :chatgpt))
  "Persist an OpenAI Codex auth payload in the shared Codex auth.json store.
The legacy EXPIRES-IN argument is accepted for compatibility and ignored."
  (declare (ignore expires-in))
  (save-openai-codex-auth-json
   (openai-codex-auth-payload
    :auth-mode auth-mode
    :openai-api-key openai-api-key
    :id-token id-token
    :access-token access-token
    :refresh-token refresh-token
    :account-id account-id
    :last-refresh (current-rfc3339-timestamp)))
  access-token)

(defun read-openai-codex-oauth-token ()
  "Read a valid ChatGPT OAuth access token from the shared Codex auth store."
  (let* ((auth-json (read-openai-codex-auth-json))
         (tokens (and auth-json (openai-codex-auth-json-tokens auth-json)))
         (access-token (and tokens (alist-string-value tokens :access--token))))
    (cond
      ((null auth-json) nil)
      ((and (eq (openai-codex-resolved-auth-mode auth-json) :chatgpt)
            (openai-codex-chatgpt-auth-stale-p auth-json))
       (let ((refreshed (refresh-openai-codex-auth-json auth-json)))
         (or (and refreshed
                  (alist-string-value (openai-codex-auth-json-tokens refreshed)
                                      :access--token))
             access-token)))
      (t
       access-token))))

(defun openai-codex-auth-descriptor-from-auth-json (auth-json)
  "Resolve AUTH-JSON into a request descriptor."
  (let ((mode (openai-codex-resolved-auth-mode auth-json)))
    (ecase mode
      (:api-key
       (let ((api-key (alist-string-value auth-json :openai--api--key)))
         (when (openai-codex-api-key-override-valid-p api-key)
           (list :source :codex-api-key
                 :mode :api-key
                 :token api-key
                 :base-url *openai-codex-api-base-url*
                 :account-id nil
                 :refreshable-p nil
                 :auth-json auth-json))))
      (:chatgpt
       (let* ((tokens (openai-codex-auth-json-tokens auth-json))
              (access-token (and tokens (alist-string-value tokens :access--token)))
              (account-id (or (and tokens (alist-string-value tokens :account--id))
                              (let ((id-token (and tokens (alist-string-value tokens :id--token))))
                                (and id-token
                                     (openai-codex-id-token-account-id id-token))))))
         (unless (and (openai-codex-chatgpt-access-token-valid-p access-token)
                      account-id)
           (error "Codex ChatGPT auth requires a JWT access_token and account_id in auth.json"))
         (list :source :codex-chatgpt
               :mode :chatgpt
               :token access-token
               :base-url *openai-codex-chatgpt-base-url*
               :account-id account-id
               :refreshable-p t
               :auth-json auth-json))))))

(defun resolve-openai-codex-auth (&key (refresh-if-needed t))
  "Resolve the effective OpenAI Codex auth descriptor.
Precedence: clawmacs override token file, then shared ~/.codex/auth.json."
  (let ((override-token (read-provider-file-token :openai-codex)))
    (when (openai-codex-api-key-override-valid-p override-token)
      (return-from resolve-openai-codex-auth
        (list :source :token-override
              :mode :api-key
              :token override-token
              :base-url *openai-codex-api-base-url*
              :account-id nil
              :refreshable-p nil))))
  (let ((auth-json (read-openai-codex-auth-json)))
    (when auth-json
      (let ((effective-auth
              (if (and refresh-if-needed
                       (eq (openai-codex-resolved-auth-mode auth-json) :chatgpt)
                       (openai-codex-chatgpt-auth-stale-p auth-json))
                  (or (refresh-openai-codex-auth-json auth-json)
                      auth-json)
                  auth-json)))
        (openai-codex-auth-descriptor-from-auth-json effective-auth)))))

(defun refresh-openai-codex-oauth-token (refresh-token)
  "Refresh the OpenAI Codex ChatGPT OAuth token and return the new access token."
  (declare (ignore refresh-token))
  (let ((updated (refresh-openai-codex-auth-json)))
    (and updated
         (alist-string-value (openai-codex-auth-json-tokens updated)
                             :access--token))))

(defun read-provider-token (provider)
  "Read PROVIDER's token with provider-specific precedence rules.

  :OPENAI-CODEX  1) Static token override (~/.config/clawmacs/openai-codex-token)
                 2) Shared Codex auth.json (~/.codex/auth.json)

  :ZAI           1) ZAI_CODING_MAX_API_KEY env var
                 2) Static token file (~/.config/clawmacs/zai-api-key)

  :OPENROUTER    1) OPENROUTER_API_KEY env var
                 2) Static token file (~/.config/clawmacs/openrouter-api-key)"
  (or (when (eq provider :zai)
        (read-env-token *zai-env-var*))
      (when (eq provider :openrouter)
        (read-env-token *openrouter-env-var*))
      (when (eq provider :openai-codex)
        (getf (resolve-openai-codex-auth) :token))
      (read-provider-file-token provider)))

(defun generate-random-string (length)
  "Generate a random string of LENGTH alphanumeric characters.
Uses /dev/urandom for cryptographic randomness."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
        (num-chars 62))
    (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
      (with-output-to-string (s)
        (loop :repeat length
              :for byte := (read-byte urandom)
              :do (write-char (char chars (mod byte num-chars)) s))))))

(defun generate-code-verifier ()
  "Generate a 43-character PKCE code verifier (RFC 7636)."
  (generate-random-string 43))

(defun generate-oauth-state ()
  "Generate a base64url OAuth state token for CSRF protection."
  (let ((bytes (make-array 32 :element-type '(unsigned-byte 8))))
    (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
      (read-sequence bytes urandom))
    (with-output-to-string (out)
      (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
            (buffer 0)
            (bits 0))
        (loop :for byte :across bytes
              :do (setf buffer (logior (ash buffer 8) byte)
                        bits (+ bits 8))
                  (loop :while (>= bits 6)
                        :do (decf bits 6)
                            (write-char (char alphabet (ldb (byte 6 bits) buffer)) out)
                            (setf buffer (ldb (byte bits 0) buffer))))
        (when (plusp bits)
          (write-char (char alphabet (ash buffer (- 6 bits))) out))))))

(defun hex-string-to-bytes (hex-string)
  "Convert a HEX-STRING to a byte array."
  (let* ((len (/ (length hex-string) 2))
         (bytes (make-array len :element-type '(unsigned-byte 8))))
    (loop :for i :from 0 :below (length hex-string) :by 2
          :for j :from 0
          :do (setf (aref bytes j)
                    (parse-integer hex-string :start i :end (+ i 2) :radix 16)))
    bytes))

(defun compute-code-challenge (code-verifier)
  "Compute S256 PKCE code challenge from CODE-VERIFIER (RFC 7636 Section 4.2).
Uses openssl for SHA-256 and base64, then converts to base64url encoding."
  (string-trim
   '(#\Newline #\Space #\Return)
   (with-output-to-string (out)
     (uiop:run-program
      (format nil "printf '%s' '~A' | openssl dgst -sha256 -binary | openssl base64 -A | tr '+/' '-_' | tr -d '='"
              code-verifier)
      :output out))))

(defun url-encode-param (string)
  "Percent-encode STRING for use as a URL query parameter value."
  (with-output-to-string (s)
    (loop :for char :across string
          :do (cond
                ((or (alphanumericp char)
                     (find char "-_.~"))
                 (write-char char s))
                ((< (char-code char) 128)
                 (format s "%~2,'0X" (char-code char)))
                (t
                 ;; Multi-byte: encode each UTF-8 byte
                 (loop :for byte :across (flexi-streams:string-to-octets
                                          (string char) :external-format :utf-8)
                       :do (format s "%~2,'0X" byte)))))))

(defun split-string-by-char (string delimiter)
  "Split STRING into substrings at each occurrence of DELIMITER character."
  (loop :for start := 0 :then (1+ end)
        :for end := (position delimiter string :start start)
        :collect (subseq string start (or end (length string)))
        :while end))

(defun hex-digit-value (char)
  "Decode one hexadecimal digit CHAR to its integer value."
  (cond
    ((and (char>= char #\0) (char<= char #\9))
     (- (char-code char) (char-code #\0)))
    ((and (char>= char #\A) (char<= char #\F))
     (+ 10 (- (char-code char) (char-code #\A))))
    ((and (char>= char #\a) (char<= char #\f))
     (+ 10 (- (char-code char) (char-code #\a))))
    (t
     (error "Invalid hex digit ~S" char))))

(defun url-decode-param (string)
  "Percent-decode STRING from a URL query parameter."
  (with-output-to-string (out)
    (loop :for i :from 0 :below (length string)
          :for char := (char string i)
          :do (cond
                ((char= char #\+)
                 (write-char #\Space out))
                ((and (char= char #\%)
                      (<= (+ i 2) (1- (length string))))
                 (let ((byte (+ (* 16 (hex-digit-value (char string (1+ i))))
                                (hex-digit-value (char string (+ i 2))))))
                   (write-char (code-char byte) out)
                   (incf i 2)))
                (t
                 (write-char char out))))))

(defun parse-query-string (query)
  "Parse a URL QUERY string into an alist of (key . value) string pairs."
  (loop :for part :in (split-string-by-char query #\&)
        :for eq-pos := (position #\= part)
        :when eq-pos
          :collect (cons (url-decode-param (subseq part 0 eq-pos))
                         (url-decode-param (subseq part (1+ eq-pos))))))

(defun extract-oauth-callback-params (url)
  "Extract authorization code and state from an OAuth callback URL.
Returns (values code state). Signals an error if the code is missing."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) url))
         (query-start (position #\? trimmed)))
    (unless query-start
      (error "Invalid callback URL (no query parameters).~%Expected: ~A?code=...&state=..."
             (openai-oauth-redirect-uri)))
    (let* ((query (subseq trimmed (1+ query-start)))
           (params (parse-query-string query))
           (code (cdr (assoc "code" params :test #'string=)))
           (state (cdr (assoc "state" params :test #'string=))))
      (unless code
        (error "No authorization code in callback URL. Check that you copied the full URL."))
      (values code state))))

(defun build-openai-codex-authorize-url (redirect-uri code-challenge state)
  "Build a Codex-compatible browser authorization URL."
  (format nil
          "~A?response_type=code&client_id=~A&redirect_uri=~A&scope=~A&code_challenge=~A&code_challenge_method=S256&id_token_add_organizations=true&codex_cli_simplified_flow=true&state=~A&originator=~A"
          *openai-oauth-auth-url*
          *openai-oauth-client-id*
          (url-encode-param redirect-uri)
          (url-encode-param *openai-oauth-scopes*)
          (url-encode-param code-challenge)
          (url-encode-param state)
          (url-encode-param *openai-oauth-originator*)))

(defun openai-codex-oauth-start (&key (redirect-uri (openai-oauth-redirect-uri)))
  "Initiate an OpenAI Codex OAuth PKCE flow.
Generates PKCE pair and state, builds the authorization URL.
Returns (values auth-url code-verifier state)."
  (let* ((code-verifier (generate-code-verifier))
         (code-challenge (compute-code-challenge code-verifier))
         (state (generate-oauth-state))
         (auth-url (build-openai-codex-authorize-url redirect-uri code-challenge state)))
    (values auth-url code-verifier state)))

(defun exchange-openai-oauth-code (code code-verifier &key (redirect-uri (openai-oauth-redirect-uri)))
  "Exchange an OAuth authorization CODE for access/refresh tokens using CODE-VERIFIER.
Returns a plist (:id-token ... :access-token ... :refresh-token ... :account-id ...)."
  (multiple-value-bind (body status-code)
      (drakma:http-request
       *openai-oauth-token-url*
       :method :post
       :content-type "application/x-www-form-urlencoded"
       :content (format nil "grant_type=authorization_code&client_id=~A&code=~A&redirect_uri=~A&code_verifier=~A"
                        (url-encode-param *openai-oauth-client-id*)
                        (url-encode-param code)
                        (url-encode-param redirect-uri)
                        (url-encode-param code-verifier))
       :want-stream nil
       :force-binary nil)
    (let ((body-string (http-body-string body)))
      (unless (= status-code 200)
        (error "OAuth token exchange failed (~A): ~A" status-code body-string))
      (let* ((response (api-json-decode body-string))
             (id-token (alist-string-value response :id--token)))
        (list :id-token id-token
              :access-token (alist-string-value response :access--token)
              :refresh-token (alist-string-value response :refresh--token)
              :account-id (and id-token
                               (openai-codex-id-token-account-id id-token)))))))

(defun obtain-openai-codex-api-key (id-token)
  "Exchange ID-TOKEN for an API-key style token when the backend allows it."
  (multiple-value-bind (body status-code)
      (drakma:http-request
       *openai-oauth-token-url*
       :method :post
       :content-type "application/x-www-form-urlencoded"
       :content (format nil
                        "grant_type=~A&client_id=~A&requested_token=~A&subject_token=~A&subject_token_type=~A"
                        (url-encode-param "urn:ietf:params:oauth:grant-type:token-exchange")
                        (url-encode-param *openai-oauth-client-id*)
                        (url-encode-param "openai-api-key")
                        (url-encode-param id-token)
                        (url-encode-param "urn:ietf:params:oauth:token-type:id_token"))
       :want-stream nil
       :force-binary nil)
    (let ((body-string (http-body-string body)))
      (when (= status-code 200)
        (alist-string-value (api-json-decode body-string) :access--token)))))

(defun openai-codex-oauth-finish (callback-url code-verifier expected-state
                                   &key (redirect-uri (openai-oauth-redirect-uri)))
  "Complete the OAuth flow by exchanging the authorization code for tokens.
CALLBACK-URL is the full localhost redirect URL received by the callback server.
CODE-VERIFIER is the PKCE verifier from the initial request.
EXPECTED-STATE is the state parameter for CSRF validation.
Returns the access token on success."
  (multiple-value-bind (code state)
      (extract-oauth-callback-params callback-url)
    (when (and expected-state state (not (string= state expected-state)))
      (error "OAuth state mismatch (possible CSRF). Expected ~A, got ~A"
             expected-state state))
    (let* ((tokens (exchange-openai-oauth-code code code-verifier
                                               :redirect-uri redirect-uri))
           (id-token (getf tokens :id-token))
           (api-key (and id-token
                         (ignore-errors
                           (obtain-openai-codex-api-key id-token)))))
      (save-openai-codex-oauth-tokens
       (getf tokens :access-token)
       (getf tokens :refresh-token)
       nil
       :id-token id-token
       :account-id (getf tokens :account-id)
       :openai-api-key api-key
       :auth-mode :chatgpt)
      (getf tokens :access-token))))

(defstruct openai-oauth-flow
  "State for an in-progress localhost OpenAI Codex OAuth login."
  buffer
  auth-url
  redirect-uri
  port
  code-verifier
  state
  listener
  thread
  (done-p nil :type boolean)
  (success-p nil :type boolean)
  (cancelled-p nil :type boolean)
  error
  token
  (lock (bt:make-lock "openai-oauth-flow")))

(defun html-escape (string)
  "Escape STRING for safe insertion into a tiny HTML page."
  (with-output-to-string (out)
    (loop :for char :across (or string "")
          :do (case char
                (#\& (write-string "&amp;" out))
                (#\< (write-string "&lt;" out))
                (#\> (write-string "&gt;" out))
                (#\" (write-string "&quot;" out))
                (#\' (write-string "&#39;" out))
                (otherwise
                 (write-char char out))))))

(defun openai-oauth-success-page ()
  "Return the minimal success HTML shown in the browser after login."
  "<!doctype html><html><head><meta charset=\"utf-8\"><title>Codex Login Complete</title></head><body><h1>Login complete</h1><p>You can return to clawmacs.</p></body></html>")

(defun openai-oauth-error-page (message)
  "Return a minimal HTML error page for OAuth failures."
  (format nil "<!doctype html><html><head><meta charset=\"utf-8\"><title>Codex Login Failed</title></head><body><h1>Login failed</h1><pre>~A</pre></body></html>"
          (html-escape message)))

(defun openai-oauth-send-http-response (stream status reason body)
  "Write a small HTTP response with BODY to STREAM."
  (format stream "HTTP/1.1 ~D ~A~C~C" status reason #\Return #\Linefeed)
  (format stream "Content-Type: text/html; charset=utf-8~C~C" #\Return #\Linefeed)
  (format stream "Content-Length: ~D~C~C" (length body) #\Return #\Linefeed)
  (format stream "Connection: close~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed)
  (write-string body stream)
  (finish-output stream))

(defun openai-oauth-flow-set-result (flow &key success cancelled error token)
  "Record FLOW's completion state under its lock."
  (bt:with-lock-held ((openai-oauth-flow-lock flow))
    (when (openai-oauth-flow-done-p flow)
      (return-from openai-oauth-flow-set-result flow))
    (setf (openai-oauth-flow-done-p flow) t
          (openai-oauth-flow-success-p flow) success
          (openai-oauth-flow-cancelled-p flow) cancelled
          (openai-oauth-flow-error flow) error
          (openai-oauth-flow-token flow) token))
  flow)

(defun openai-oauth-flow-snapshot (flow)
  "Return a plist snapshot of FLOW for safe polling from the UI."
  (bt:with-lock-held ((openai-oauth-flow-lock flow))
    (list :done-p (openai-oauth-flow-done-p flow)
          :success-p (openai-oauth-flow-success-p flow)
          :cancelled-p (openai-oauth-flow-cancelled-p flow)
          :error (openai-oauth-flow-error flow)
          :token (openai-oauth-flow-token flow)
          :auth-url (openai-oauth-flow-auth-url flow)
          :redirect-uri (openai-oauth-flow-redirect-uri flow)
          :port (openai-oauth-flow-port flow))))

(defun maybe-open-url-in-browser (url)
  "Best-effort browser opener for URL. Returns non-nil when a command launched."
  (or (let ((browser (uiop:getenv "BROWSER")))
        (when (and browser (plusp (length browser)))
          (ignore-errors
            (uiop:launch-program (list browser url)
                                 :input nil :output nil :error-output nil
                                 :ignore-error-status t))))
      (ignore-errors
        (uiop:launch-program (list "xdg-open" url)
                             :input nil :output nil :error-output nil
                             :ignore-error-status t))
      (ignore-errors
        (uiop:launch-program (list "open" url)
                             :input nil :output nil :error-output nil
                             :ignore-error-status t))))

(defun bind-openai-oauth-listener (&optional (preferred-port *openai-oauth-default-port*))
  "Bind a localhost TCP listener, preferring PREFERRED-PORT and falling back to an ephemeral port."
  #+sbcl
  (labels ((try-bind (port)
             (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                            :type :stream
                                            :protocol :tcp)))
               (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
               (handler-case
                   (progn
                     (sb-bsd-sockets:socket-bind listener #(127 0 0 1) port)
                     (sb-bsd-sockets:socket-listen listener 5)
                     (multiple-value-bind (address actual-port)
                         (sb-bsd-sockets:socket-name listener)
                       (declare (ignore address))
                       (values listener actual-port)))
                 (error (e)
                   (ignore-errors (sb-bsd-sockets:socket-close listener))
                   (error e))))))
    (handler-case
        (try-bind preferred-port)
      (error ()
        (try-bind 0))))
  #-sbcl
  (error "Local OpenAI Codex OAuth login currently requires SBCL"))

(defun openai-oauth-read-request-target (stream)
  "Read a single HTTP request from STREAM and return its method and target."
  (let ((request-line (read-line stream nil nil)))
    (when request-line
      (loop :for line := (read-line stream nil nil)
            :for trimmed := (and line (string-trim '(#\Return) line))
            :while (and trimmed (plusp (length trimmed))))
      (let ((parts (split-string-by-char request-line #\Space)))
        (values (first parts) (second parts))))))

(defun openai-oauth-request-path (target)
  "Return TARGET's path portion without its query string."
  (let ((query-pos (position #\? target)))
    (subseq target 0 (or query-pos (length target)))))

(defun run-openai-oauth-server (flow)
  "Serve one localhost OAuth callback request for FLOW."
  #+sbcl
  (handler-case
      (let ((listener (openai-oauth-flow-listener flow)))
        (loop
          (multiple-value-bind (client-socket peer-address)
              (sb-bsd-sockets:socket-accept listener)
            (declare (ignore peer-address))
            (unwind-protect
                 (let ((stream (sb-bsd-sockets:socket-make-stream
                                client-socket
                                :input t
                                :output t
                                :element-type 'character
                                :external-format :utf-8
                                :buffering :line)))
                   (unwind-protect
                        (multiple-value-bind (method target)
                            (openai-oauth-read-request-target stream)
                          (cond
                            ((or (null method) (null target))
                             (openai-oauth-send-http-response
                              stream 400 "Bad Request"
                              (openai-oauth-error-page "Malformed callback request"))
                             (openai-oauth-flow-set-result flow :error "Malformed callback request")
                             (return))
                            ((not (string= method "GET"))
                             (openai-oauth-send-http-response
                              stream 405 "Method Not Allowed"
                              (openai-oauth-error-page "Only GET callbacks are supported"))
                             (return))
                            ((not (string= (openai-oauth-request-path target)
                                           *openai-oauth-callback-path*))
                             (openai-oauth-send-http-response
                              stream 404 "Not Found"
                              (openai-oauth-error-page "Unknown callback path"))
                             (return))
                            (t
                             (handler-case
                                 (let* ((callback-url
                                          (format nil "~A~A"
                                                  (openai-oauth-flow-redirect-uri flow)
                                                  (let ((query-pos (position #\? target)))
                                                    (if query-pos
                                                        (subseq target query-pos)
                                                        ""))))
                                        (token
                                          (openai-codex-oauth-finish
                                           callback-url
                                           (openai-oauth-flow-code-verifier flow)
                                           (openai-oauth-flow-state flow)
                                           :redirect-uri
                                           (openai-oauth-flow-redirect-uri flow))))
                                   (openai-oauth-flow-set-result flow
                                                                 :success t
                                                                 :token token)
                                   (openai-oauth-send-http-response
                                    stream 200 "OK" (openai-oauth-success-page))
                                   (return))
                               (error (e)
                                 (openai-oauth-flow-set-result
                                  flow :error (format nil "~A" e))
                                 (openai-oauth-send-http-response
                                  stream 400 "Bad Request"
                                  (openai-oauth-error-page (format nil "~A" e)))
                                 (return))))))
                     (ignore-errors
                       (close stream))))
              (ignore-errors
                (sb-bsd-sockets:socket-close client-socket))))))
    (error (e)
      (unless (openai-oauth-flow-cancelled-p flow)
        (openai-oauth-flow-set-result flow :error (format nil "~A" e)))))
  #-sbcl
  (declare (ignore flow)))

(defun start-openai-codex-oauth-login (&key buffer (open-browser-p t))
  "Start the localhost OpenAI Codex OAuth PKCE flow and return the flow object."
  (multiple-value-bind (listener actual-port)
      (bind-openai-oauth-listener)
    (let* ((redirect-uri (openai-oauth-redirect-uri actual-port))
           (auth-url nil)
           (code-verifier nil)
           (state nil))
      (multiple-value-setq (auth-url code-verifier state)
        (openai-codex-oauth-start :redirect-uri redirect-uri))
      (let ((flow (make-openai-oauth-flow :buffer buffer
                                          :auth-url auth-url
                                          :redirect-uri redirect-uri
                                          :port actual-port
                                          :code-verifier code-verifier
                                          :state state
                                          :listener listener)))
        (setf (openai-oauth-flow-thread flow)
              (bt:make-thread
               (lambda ()
                 (unwind-protect
                      (run-openai-oauth-server flow)
                   (ignore-errors
                     #+sbcl
                     (sb-bsd-sockets:socket-close (openai-oauth-flow-listener flow)))))
               :name "clawmacs-openai-oauth"))
        (when open-browser-p
          (maybe-open-url-in-browser auth-url))
        flow))))

(defun cancel-openai-codex-oauth-login (flow)
  "Cancel FLOW and shut down its listener."
  (openai-oauth-flow-set-result flow :cancelled t)
  (ignore-errors
    #+sbcl
    (sb-bsd-sockets:socket-close (openai-oauth-flow-listener flow)))
  flow)

(defparameter *provider-fallback-models*
  '((:openai-codex . *openai-codex-model*)
    (:zai . *zai-model*)
    (:openrouter . *openrouter-model*))
  "Alist mapping provider keywords to the variable holding their default model.
Each cdr is a symbol naming a special variable; provider-fallback-model
dereferences it at call time so that user customizations take effect.")

(defun known-provider-p (provider)
  "Return non-nil when PROVIDER is supported locally."
  (member provider '(:openai-codex :zai :openrouter) :test #'eq))

(defun canonicalize-provider-name (provider-name)
  "Return a normalized comparison key for PROVIDER-NAME."
  (string-downcase
   (remove-if-not #'alphanumericp
                  (string provider-name))))

(defun normalize-provider (provider)
  "Normalize PROVIDER to a supported keyword, or nil when absent."
  (cond
    ((null provider) nil)
    ((keywordp provider)
     (if (known-provider-p provider)
         provider
         (error "Unknown provider ~S. Supported providers: :OPENAI-CODEX, :ZAI, :OPENROUTER"
                provider)))
    ((stringp provider)
     (let ((normalized-name (canonicalize-provider-name provider)))
       (or (find normalized-name
                 '(:openai-codex :zai :openrouter)
                 :key (lambda (candidate)
                        (canonicalize-provider-name (symbol-name candidate)))
                 :test #'string=)
           (error "Unknown provider ~S. Supported providers: :OPENAI-CODEX, :ZAI, :OPENROUTER"
                  provider))))
    ((symbolp provider)
     (normalize-provider (symbol-name provider)))
    (t
     (error "Unknown provider ~S. Supported providers: :OPENAI-CODEX, :ZAI, :OPENROUTER"
            provider))))

(defun blank-string-p (value)
  "Return non-nil when VALUE is nil or all whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))))

(defun normalize-agent-name-key (agent-name)
  "Normalize AGENT-NAME for registry lookup."
  (unless (stringp agent-name)
    (error "Agent name must be a string, got ~S" agent-name))
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) agent-name)))
    (when (blank-string-p trimmed)
      (error "Agent name must be a non-empty string"))
    (string-downcase trimmed)))

(defun normalize-tool-name (tool-name)
  "Normalize TOOL-NAME into the API-facing tool name string."
  (let* ((raw (etypecase tool-name
                (string tool-name)
                (symbol (symbol-name tool-name))))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
    (when (blank-string-p trimmed)
      (error "Tool name must be non-empty"))
    (string-downcase
     (substitute #\_ #\- trimmed))))

(defun normalize-tool-name-list (tool-names)
  "Normalize TOOL-NAMES into a duplicate-free list of API-facing strings.
NIL means no explicit tool allowlist."
  (when tool-names
    (let ((names (cond
                   ((or (stringp tool-names)
                        (symbolp tool-names))
                    (list tool-names))
                   ((vectorp tool-names)
                    (coerce tool-names 'list))
                   ((listp tool-names)
                    tool-names)
                   (t
                    (error "Tool names must be a string, symbol, list, vector, or NIL")))))
      (remove-duplicates
       (mapcar #'normalize-tool-name names)
       :test #'string=))))

(defun register-agent-definition (name &key provider model think-level
                                       core-prompt personality-prompt tool-names)
  "Register or update an agent definition for NAME.
NAME is stored as given for display, while lookups are keyed case-insensitively."
  (let* ((trimmed-name (string-trim '(#\Space #\Tab #\Newline #\Return) name))
         (registry-key (normalize-agent-name-key trimmed-name))
         (normalized-provider (normalize-provider provider))
         (normalized-think-level (normalize-think-level-override think-level))
         (normalized-tool-names (normalize-tool-name-list tool-names))
         (definition (make-agent-definition :name trimmed-name
                                            :provider normalized-provider
                                            :model model
                                            :think-level normalized-think-level
                                            :core-prompt core-prompt
                                            :personality-prompt personality-prompt
                                            :tool-names normalized-tool-names)))
    (when (and model (blank-string-p model))
      (error "Agent model must be a non-empty string"))
    (setf (gethash registry-key *agent-definition-registry*) definition)
    definition))

(defun find-agent-definition (agent-name)
  "Return the registered agent definition for AGENT-NAME, or NIL."
  (when agent-name
    (gethash (normalize-agent-name-key agent-name)
             *agent-definition-registry*)))

(defun list-agent-definitions ()
  "Return all registered agent definitions sorted by name."
  (let ((definitions nil))
    (maphash (lambda (name definition)
               (declare (ignore name))
               (push definition definitions))
             *agent-definition-registry*)
    (sort definitions #'string<
          :key (lambda (definition)
                 (string-downcase (agent-definition-name definition))))))

(defun agent-definition-provider-for-name (agent-name)
  "Return AGENT-NAME's programmatic default provider, or NIL."
  (let ((definition (find-agent-definition agent-name)))
    (and definition
         (agent-definition-provider definition))))

(defun agent-definition-model-for-name (agent-name provider)
  "Return AGENT-NAME's programmatic model when it matches PROVIDER."
  (let ((definition (find-agent-definition agent-name)))
    (when (and definition
               (eq provider (agent-definition-provider definition)))
      (agent-definition-model definition))))

(defun agent-definition-think-level-for-name (agent-name provider model)
  "Return AGENT-NAME's programmatic think level when PROVIDER/MODEL support it."
  (let ((definition (find-agent-definition agent-name)))
    (when definition
      (let ((think-level (agent-definition-think-level definition)))
        (when (and think-level
                   (think-level-supported-p provider model think-level))
          think-level)))))

(defun agent-definition-tool-names-for-name (agent-name)
  "Return AGENT-NAME's programmatic tool allowlist, or NIL for default tools."
  (let ((definition (find-agent-definition agent-name)))
    (when definition
      (copy-list (agent-definition-tool-names definition)))))

(defun provider-fallback-model (provider)
  "Return the fallback model for PROVIDER.
Looks up the provider-specific variable in *provider-fallback-models* and
returns its current value, so user customizations (e.g. via init.lisp) are
respected."
  (let ((entry (cdr (assoc provider *provider-fallback-models*))))
    (when entry (symbol-value entry))))

;;; --------------------------------------------------------------------------
;;; Known Models Per Provider
;;; --------------------------------------------------------------------------

(defparameter *provider-known-models*
  '((:openai-codex
     "gpt-5.3-codex"
     "gpt-5.4"
     "gpt-5.2-codex"
     "gpt-5.1-codex-max"
     "gpt-5.1-codex-mini"
     "gpt-5.2")
    (:zai
     "glm-5"
     "glm-5-turbo"
     "glm-4.7"
     "glm-4.6"
     "glm-4.5"
     "glm-4.5-air")
    (:openrouter
     "openai/gpt-5.3-codex"
     "openai/gpt-5.2"
     "openai/gpt-5.1"
     "openai/gpt-4.1"
     "openai/gpt-4o"
     "google/gemini-2.5-pro"
     "google/gemini-2.5-flash"
     "z-ai/glm-4.6"
     "deepseek/deepseek-r1"
     "meta-llama/llama-4-maverick"))
  "Known model identifiers grouped by provider.
The first model in each list is the provider's default.
For :OPENROUTER, models are dynamically fetched by fetch-openrouter-models when
an API key is configured; this static list is used as a fallback.
These are used by the model selector overlay.")

(defparameter *openai-codex-model-think-levels*
  '(("gpt-5.4" "none" "low" "medium" "high" "xhigh")
    ("gpt-5.3-codex" "low" "medium" "high" "xhigh")
    ("gpt-5.2-codex" "low" "medium" "high" "xhigh")
    ("gpt-5.2" "none" "low" "medium" "high" "xhigh")
    ("gpt-5.1-codex" "none" "low" "medium" "high")
    ("gpt-5.1-codex-max" "none" "low" "medium" "high")
    ("gpt-5.1-codex-mini" "none" "low" "medium" "high"))
  "Supported reasoning effort values for known OpenAI-Codex models.
Values are ordered from lowest to highest effort, excluding the synthetic
\"default\" selector entry handled by the UI.")

(defun provider-known-models (provider)
  "Return the list of known model names for PROVIDER.
For :OPENROUTER, returns the dynamically-fetched model list when available,
falling back to the static *provider-known-models* entry."
  (if (and (eq provider :openrouter) *openrouter-cached-models*)
      *openrouter-cached-models*
      (cdr (assoc provider *provider-known-models*))))

(defun fetch-openrouter-models ()
  "Fetch the list of available models from the OpenRouter API.
Populates *openrouter-cached-models* and returns the model ID list.
Requires a valid OpenRouter API key to be configured.
Returns the cached list on subsequent calls; set *openrouter-cached-models* to
nil to force a refresh. Returns the static fallback list on any error."
  (when *openrouter-cached-models*
    (return-from fetch-openrouter-models *openrouter-cached-models*))
  (handler-case
      (let ((token (read-provider-token :openrouter)))
        (unless token
          (return-from fetch-openrouter-models
            (cdr (assoc :openrouter *provider-known-models*))))
        (multiple-value-bind (body status-code)
            (drakma:http-request
             *openrouter-models-url*
             :method :get
             :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token)))
             :want-stream nil
             :force-binary nil)
          (if (= status-code 200)
              (let* ((body-string (http-body-string body))
                     (response (api-json-decode body-string))
                     (models-data (cdr (assoc :data response)))
                     (ids (loop :for m :in (coerce models-data 'list)
                                :for id := (cdr (assoc :id m))
                                :when (and id (stringp id) (plusp (length id)))
                                  :collect id)))
                (when ids
                  (setf *openrouter-cached-models* ids))
                (or ids (cdr (assoc :openrouter *provider-known-models*))))
              (cdr (assoc :openrouter *provider-known-models*)))))
    (error ()
      (cdr (assoc :openrouter *provider-known-models*)))))

(defun provider-has-token-p (provider)
  "Return non-nil when PROVIDER has a usable API key or OAuth token configured."
  (handler-case
      (if (eq provider :openai-codex)
          (let ((auth (resolve-openai-codex-auth)))
            (and auth
                 (stringp (getf auth :token))
                 (plusp (length (getf auth :token)))))
          (let ((token (read-provider-token provider)))
            (and token (stringp token) (plusp (length token)))))
    (error () nil)))

(defun provider-model-supported-think-levels (provider model)
  "Return the supported think levels for PROVIDER and MODEL, or NIL."
  (when (and (eq provider :openai-codex)
             (stringp model))
    (copy-list (cdr (assoc model *openai-codex-model-think-levels*
                           :test #'string=)))))

(defun think-level-supported-p (provider model think-level)
  "Return non-nil when THINK-LEVEL is supported by PROVIDER and MODEL."
  (member think-level
          (provider-model-supported-think-levels provider model)
          :test #'string=))

(defun resolved-buffer-think-level (buf provider model)
  "Return BUF's validated think-level override for PROVIDER and MODEL, or NIL."
  (let ((think-level (normalize-think-level-override
                      (buffer-think-level-override buf))))
    (when (and think-level
               (think-level-supported-p provider model think-level))
      think-level)))

(defun reconcile-buffer-think-level-override (buf &key provider model)
  "Reconcile BUF's think-level override against PROVIDER and MODEL.
Returns two values: status keyword and resulting think level."
  (multiple-value-bind (resolved-provider resolved-model)
      (if (and provider model)
          (values provider model)
          (resolve-buffer-provider-and-model buf))
    (let* ((old-think (normalize-think-level-override
                       (buffer-think-level-override buf)))
           (new-think (resolved-buffer-think-level buf
                                                   resolved-provider
                                                   resolved-model)))
      (setf (buffer-think-level-override buf) new-think)
      (values (cond
                ((and old-think new-think
                      (string= old-think new-think))
                 :kept)
                ((and old-think (null new-think))
                 :reset)
                (t
                 :default))
              new-think))))

(defun available-models-for-selector (buf)
  "Build the model selector entry list for BUF.
Returns a list of plists: ((:provider :zai :model \"name\" :active-p t/nil) ...)
Only includes providers that have a valid API key. The entry matching BUF's
currently resolved provider/model is marked :active-p t.
For :OPENROUTER, dynamically-fetched models are used when an API key is present."
  (multiple-value-bind (current-provider current-model)
      (handler-case (resolve-buffer-provider-and-model buf)
        (error () (values nil nil)))
    (let ((entries nil))
      (dolist (provider-models *provider-known-models*)
        (let* ((provider (car provider-models))
               (models (if (eq provider :openrouter)
                           (fetch-openrouter-models)
                           (cdr provider-models))))
          (when (provider-has-token-p provider)
            (dolist (model models)
              (push (list :provider provider
                          :model model
                          :active-p (and (eq provider current-provider)
                                         (string= model current-model)))
                    entries)))))
      (nreverse entries))))

(defun json-key-string (key)
  "Convert a decoded JSON key into a lowercase string."
  (string-downcase
   (etypecase key
     (string key)
     (symbol (symbol-name key)))))

(defun lookup-json-value (alist key)
  "Find KEY in decoded JSON ALIST, handling string and symbol keys."
  (let ((name (string-downcase key)))
    (loop :for (entry-key . value) :in alist
          :when (string= name (json-key-string entry-key))
            :do (return value))))

(defun make-agent-defaults-registry ()
  "Create an empty in-memory agent defaults registry."
  (list :agents (make-hash-table :test #'equal)))

(defun registry-agents (registry)
  "Return the agent table stored in REGISTRY."
  (getf registry :agents))

(defun agent-default-spec (agent-name)
  "Return the stored default spec for AGENT-NAME, or nil."
  (ensure-agent-defaults-loaded)
  (gethash (string-downcase agent-name)
           (registry-agents *agent-defaults-registry*)))

(defun load-agent-defaults ()
  "Load and memoize persisted agent defaults, overlaying built-in fallbacks."
  (let ((registry (make-agent-defaults-registry)))
    (when (probe-file *agent-defaults-path*)
      (let* ((json (uiop:read-file-string *agent-defaults-path*))
             (data (cl-json:decode-json-from-string json))
             (agents (registry-agents registry)))
        (dolist (entry data)
          (let* ((agent-name (json-key-string (car entry)))
                 (spec (cdr entry))
                 (provider (normalize-provider (lookup-json-value spec "provider")))
                 (model (lookup-json-value spec "model")))
            (setf (gethash agent-name agents)
                  (list :provider provider
                        :model (and (stringp model)
                                    model)))))))
    (setf *agent-defaults-registry* registry)))

(defun save-agent-defaults ()
  "Persist the current agent defaults registry to disk."
  (ensure-agent-defaults-loaded)
  (let ((payload nil))
    (maphash
     (lambda (agent-name spec)
       (let ((provider (getf spec :provider))
             (model (getf spec :model)))
         (push `(,agent-name . ((:provider . ,(and provider
                                                   (string-downcase (symbol-name provider))))
                                ,@(when model
                                    `((:model . ,model)))))
               payload)))
     (registry-agents *agent-defaults-registry*))
    (ensure-directories-exist *agent-defaults-path*)
    (with-open-file (stream *agent-defaults-path*
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (api-json-encode (nreverse payload)) stream))
    *agent-defaults-path*))

(defun ensure-agent-defaults-loaded ()
  "Load the agent defaults registry on first use."
  (or *agent-defaults-registry*
      (load-agent-defaults)))

(defun agent-default (agent-name)
  "Return AGENT-NAME's default provider, or *default-provider*."
  (or (agent-definition-provider-for-name agent-name)
      (getf (agent-default-spec agent-name)
            :provider)
      *default-provider*))

(defun agent-default-model (agent-name provider)
  "Return AGENT-NAME's stored model when it matches PROVIDER."
  (or (agent-definition-model-for-name agent-name provider)
      (let ((spec (agent-default-spec agent-name)))
        (when (eq provider (getf spec :provider))
          (getf spec :model)))))

(defun agent-known-persisted-default-names ()
  "Return the agent names that exist in the persisted defaults registry."
  (ensure-agent-defaults-loaded)
  (let ((names nil))
    (maphash (lambda (name spec)
               (declare (ignore spec))
               (push name names))
             (registry-agents *agent-defaults-registry*))
    names))

(defun list-known-agent-names ()
  "Return known agent names from programmatic definitions and compatibility defaults."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (definition (list-agent-definitions))
      (setf (gethash (normalize-agent-name-key (agent-definition-name definition)) table)
            (agent-definition-name definition)))
    (dolist (name (agent-known-persisted-default-names))
      (setf (gethash (normalize-agent-name-key name) table) name))
    (when *default-agent-name*
      (setf (gethash (normalize-agent-name-key *default-agent-name*) table)
            *default-agent-name*))
    (let ((names nil))
      (maphash (lambda (name-key name)
                 (declare (ignore name-key))
                 (push name names))
               table)
      (sort names #'string< :key #'string-downcase))))

(defun set-agent-default (agent-name provider &key model)
  "Set AGENT-NAME's default provider and optional MODEL, then persist it."
  (ensure-agent-defaults-loaded)
  (let ((normalized-provider (normalize-provider provider)))
    (when (and model (blank-string-p model))
      (error "Resolved model must be a non-empty string"))
    (setf (gethash (string-downcase agent-name)
                   (registry-agents *agent-defaults-registry*))
          (list :provider normalized-provider
                :model model))
    (save-agent-defaults)
    normalized-provider))

(defun resolve-buffer-provider-and-model (buf)
  "Resolve BUF's effective provider, model, and think level.
Resolution order for provider: buffer override → agent definition → legacy agent
default → *default-provider*.
Resolution order for model: buffer override → agent definition → legacy agent
default → provider fallback → *default-model*.
Think level resolution: buffer override → agent definition → nil, with support
checked against the resolved provider/model."
  (ensure-agent-defaults-loaded)
  (let* ((provider (or (buffer-provider-override buf)
                       (agent-default (buffer-agent-name buf))
                       *default-provider*))
         (resolved-provider (normalize-provider provider))
         (model (or (buffer-model-override buf)
                    (agent-default-model (buffer-agent-name buf) resolved-provider)
                    (provider-fallback-model resolved-provider)
                    *default-model*))
         (think-level (or (resolved-buffer-think-level buf resolved-provider model)
                          (agent-definition-think-level-for-name (buffer-agent-name buf)
                                                                 resolved-provider
                                                                 model))))
    (when (blank-string-p model)
      (error "Resolved model must be a non-empty string"))
    (values resolved-provider model think-level)))

;;; --------------------------------------------------------------------------
;;; Conversation Building
;;; --------------------------------------------------------------------------

(defun build-conversation-messages (buf)
  "Build canonical provider messages from the buffer's chat history.
Uses raw-content when available (for tool_use/tool_result messages),
falls back to plain text content.
System messages (sender :system) are excluded — they are display-only
and should not be sent to the API."
  (let ((messages nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))))
          :do (let ((sender (message-sender msg)))
                ;; Skip system messages — they are display-only (shell output, etc.)
                (unless (eq sender :system)
                  (let* ((role (cond
                                 ((eq sender :user) "user")
                                 ((eq sender :tool-result) "user")
                                 ((eq sender :compaction-summary) "user")
                                 (t "assistant")))
                         (content (canonicalize-message-content
                                   role
                                   (or (message-raw-content msg)
                                       (message-text msg)))))
                    (when (and (eq sender :user)
                               (null (message-raw-content msg)))
                      (dolist (skill-text
                                (handler-case
                                    (skill-injection-messages (message-text msg))
                                  (error () nil)))
                        (let ((skill-content
                                (canonicalize-message-content "user" skill-text)))
                          (push `((:role . "user")
                                  (:content . ,(coerce skill-content 'vector)))
                                messages))))
                    (push `((:role . ,role)
                            (:content . ,(coerce content 'vector)))
                          messages)))))
    (nreverse messages)))

(defun canonical-tool-use-block (id name input)
  "Return ID/NAME/INPUT as a canonical tool_use block."
  `((:type . "tool_use")
    (:id . ,id)
    (:name . ,name)
    (:input . ,input)))

(defun canonical-response (stop-reason content-blocks &optional cache-performance)
  "Return a provider-agnostic response payload."
  `((:stop--reason . ,stop-reason)
    (:content . ,(coerce content-blocks 'vector))
    (:cache--performance . ,cache-performance)))

(defun usage-cache-performance (usage)
  "Normalize provider USAGE payloads into cache metrics."
  (when usage
    (let* ((prompt-tokens
             (or (cdr (assoc :prompt--tokens usage))
                 (cdr (assoc :input--tokens usage))))
           (details
             (or (cdr (assoc :prompt--tokens--details usage))
                 (cdr (assoc :input--tokens--details usage))))
           (cached-tokens
             (or (and details (cdr (assoc :cached--tokens details)))
                 (and details (cdr (assoc :cached--input--tokens details)))
                 0)))
      (when prompt-tokens
        `((:prompt--tokens . ,prompt-tokens)
          (:cached--tokens . ,cached-tokens))))))

(defun openai-response-cache-performance (response)
  "Extract normalized cache metrics from an OpenAI-compatible RESPONSE payload."
  (usage-cache-performance (cdr (assoc :usage response))))

(defun http-body-string (body)
  "Return BODY as a UTF-8 string."
  (if (stringp body)
      body
      (flexi-streams:octets-to-string body :external-format :utf-8)))

(defun utf8-character-input-stream (stream)
  "Return STREAM as a UTF-8 character input stream."
  (if (nth-value 0 (subtypep (stream-element-type stream) 'character))
      stream
      (flexi-streams:make-flexi-stream stream :external-format :utf-8)))

(defun read-stream-as-utf8-string (stream)
  "Read STREAM fully as UTF-8 text and return the resulting string."
  (let ((text-stream (utf8-character-input-stream stream)))
    (unwind-protect
         (let ((s (make-string-output-stream)))
           (loop :for c := (read-char text-stream nil nil)
                 :while c :do (write-char c s))
           (get-output-stream-string s))
      (when (not (eq text-stream stream))
        (ignore-errors (close text-stream))))))

(defun openai-finish-reason->stop-reason (finish-reason)
  "Normalize OpenAI FINISH-REASON to clawmacs stop reasons."
  (cond
    ((null finish-reason) nil)
    ((string= finish-reason "tool_calls") "tool_use")
    ((string= finish-reason "length") "max_tokens")
    ((string= finish-reason "stop") "end_turn")
    (t finish-reason)))

(defun openai-tool-call->canonical-block (tool-call)
  "Convert an OpenAI TOOL-CALL object into a canonical tool_use block."
  (let* ((function (cdr (assoc :function tool-call)))
         (arguments (cdr (assoc :arguments function)))
         (input (cond
                  ((blank-string-p arguments) nil)
                  ((stringp arguments) (api-json-decode arguments))
                  (t arguments))))
    (canonical-tool-use-block
     (cdr (assoc :id tool-call))
     (cdr (assoc :name function))
     input)))

(defun message-role-content-blocks (message)
  "Extract a decoded provider message into ROLE and canonical content blocks."
  (values (cdr (assoc :role message))
          (coerce (cdr (assoc :content message)) 'list)))

(defun content-blocks->openai-tool-calls (tool-uses)
  "Convert canonical TOOL-USES into OpenAI tool call objects."
  (coerce
   (loop :for tool-use :in tool-uses
         :collect `((:id . ,(cdr (assoc :id tool-use)))
                    (:type . "function")
                    (:function . ((:name . ,(cdr (assoc :name tool-use)))
                                  (:arguments . ,(api-json-encode
                                                  (or (cdr (assoc :input tool-use))
                                                      '())))))))
   'vector))

(defun tool-definitions->openai-tools (tools)
  "Translate clawmacs TOOLS to OpenAI-compatible tool definitions."
  (when (and tools (plusp (length tools)))
    (coerce
     (loop :for tool :across tools
           :collect `((:type . "function")
                      (:function . ((:name . ,(cdr (assoc :name tool)))
                                    (:description . ,(cdr (assoc :description tool)))
                                    (:parameters . ,(cdr (assoc :input--schema tool)))))))
     'vector)))

(defun conversation-messages->openai-messages (messages)
  "Translate canonical conversation MESSAGES into OpenAI chat messages."
  (loop :for message :in messages
        :append
        (multiple-value-bind (role content-blocks)
            (message-role-content-blocks message)
          (let ((text (content-text-blocks content-blocks))
                (tool-uses (content-tool-use-blocks content-blocks))
                (tool-results (remove-if-not (lambda (block)
                                               (string= "tool_result"
                                                        (content-block-type block)))
                                             content-blocks)))
            (cond
              ((string= role "assistant")
               (list `((:role . "assistant")
                       (:content . ,(unless (blank-string-p text) text))
                       ,@(when tool-uses
                           `((:tool--calls . ,(content-blocks->openai-tool-calls tool-uses)))))))
              ((and (string= role "user") tool-results)
               (append
                (when (not (blank-string-p text))
                  (list `((:role . "user")
                          (:content . ,text))))
                 (loop :for block :in tool-results
                       :collect `((:role . "tool")
                                 (:tool--call--id . ,(or (cdr (assoc :tool--use--id block))
                                                         (cdr (assoc :tool-use-id block))))
                                 (:content . ,(cdr (assoc :content block)))))))
              (t
               (list `((:role . ,role)
                       (:content . ,text)))))))))

(defun openai-messages-with-system-prompt (messages &key (system-prompt (build-system-prompt)))
  "Translate MESSAGES and prepend SYSTEM-PROMPT when present."
  (let ((openai-messages (conversation-messages->openai-messages messages)))
    (if system-prompt
        (cons `((:role . "system")
                (:content . ,system-prompt))
              openai-messages)
        openai-messages)))

(defun canonical-text-content-item (role text)
  "Return a Responses API content item for ROLE/TEXT."
  `((:type . ,(if (string= role "assistant")
                  "output_text"
                  "input_text"))
    (:text . ,text)))

(defun tool-result-output-string (content)
  "Normalize tool result CONTENT to a string for function_call_output."
  (cond
    ((null content) "")
    ((stringp content) content)
    (t (api-json-encode content))))

(defun content-blocks->responses-input-items (role content-blocks)
  "Convert canonical ROLE/CONTENT-BLOCKS into Responses API input items."
  (let ((text (content-text-blocks content-blocks))
        (tool-uses (content-tool-use-blocks content-blocks))
        (tool-results (remove-if-not (lambda (block)
                                       (string= "tool_result"
                                                (content-block-type block)))
                                     content-blocks)))
    (append
     (when (not (blank-string-p text))
       (list `((:type . "message")
               (:role . ,role)
               (:content . ,(vector (canonical-text-content-item role text))))))
     (when (string= role "assistant")
       (loop :for tool-use :in tool-uses
             :collect `((:type . "function_call")
                        (:call--id . ,(cdr (assoc :id tool-use)))
                        (:name . ,(cdr (assoc :name tool-use)))
                        (:arguments . ,(api-json-encode
                                        (or (cdr (assoc :input tool-use))
                                            '()))))))
     (when (string= role "user")
       (loop :for block :in tool-results
             :collect `((:type . "function_call_output")
                        (:call--id . ,(or (cdr (assoc :tool--use--id block))
                                          (cdr (assoc :tool-use-id block))))
                        (:output . ,(tool-result-output-string
                                     (cdr (assoc :content block))))))))))

(defun conversation-messages->responses-input (messages)
  "Translate canonical conversation MESSAGES into Responses API input items."
  (loop :for message :in messages
        :append
        (multiple-value-bind (role content-blocks)
            (message-role-content-blocks message)
          (content-blocks->responses-input-items role content-blocks))))

(defun tool-definitions->responses-tools (tools)
  "Translate clawmacs TOOLS to OpenAI Responses function tools."
  (when (and tools (plusp (length tools)))
    (coerce
     (loop :for tool :across tools
           :collect `((:type . "function")
                      (:name . ,(cdr (assoc :name tool)))
                      (:description . ,(cdr (assoc :description tool)))
                      (:parameters . ,(cdr (assoc :input--schema tool)))))
     'vector)))

(defun openai-choice->canonical-response (choice &optional cache-performance)
  "Normalize an OpenAI completion CHOICE to canonical response shape.
Handles reasoning models (Z.AI GLM, DeepSeek R1, etc.) that return
reasoning_content alongside content. When content is blank but
reasoning_content is present, falls back to reasoning_content."
  (let* ((message (cdr (assoc :message choice)))
         (content-blocks nil)
         (text (cdr (assoc :content message)))
         (reasoning (cdr (assoc :reasoning--content message)))
         (tool-calls (cdr (assoc :tool--calls message)))
         (effective-text (cond
                           ((not (blank-string-p text)) text)
                           ((not (blank-string-p reasoning)) reasoning)
                           (t nil))))
    (when effective-text
      (push (canonical-text-block effective-text) content-blocks))
    (when reasoning
      (push (canonical-reasoning-block reasoning) content-blocks))
    (dolist (tool-call (coerce (or tool-calls #()) 'list))
      (push (openai-tool-call->canonical-block tool-call) content-blocks))
    (canonical-response
     (openai-finish-reason->stop-reason (cdr (assoc :finish--reason choice)))
     (nreverse content-blocks)
     cache-performance)))

(defun responses-output-item-text (item)
  "Extract displayable assistant text from a Responses ITEM."
  (let ((content (cdr (assoc :content item))))
    (when content
      (let ((text
              (with-output-to-string (out)
                (dolist (part (coerce content 'list))
                  (let ((part-type (cdr (assoc :type part)))
                        (part-text (cdr (assoc :text part))))
                    (when (and part-text
                               (member part-type
                                       '("output_text" "text" "input_text")
                                       :test #'string=))
                      (write-string part-text out)))))))
        (unless (blank-string-p text)
          text)))))

(defun responses-function-call->canonical-block (item)
  "Convert a Responses API function_call ITEM to a canonical tool_use block."
  (let ((arguments (cdr (assoc :arguments item))))
    (canonical-tool-use-block
     (cdr (assoc :call--id item))
     (cdr (assoc :name item))
     (when (and arguments (not (blank-string-p arguments)))
       (handler-case
           (api-json-decode arguments)
         (error ()
           nil))))))

(defun responses-item->canonical-blocks (item)
  "Convert one Responses API output ITEM into canonical content blocks."
  (let ((item-type (cdr (assoc :type item))))
    (cond
      ((and (string= item-type "message")
            (string= (cdr (assoc :role item)) "assistant"))
       (let ((text (responses-output-item-text item)))
         (when text
           (list (canonical-text-block text)))))
      ((string= item-type "function_call")
       (list (responses-function-call->canonical-block item)))
      (t
       nil))))

(defun responses-api-response->canonical-response (response)
  "Normalize an OpenAI Responses API RESPONSE to the canonical clawmacs shape."
  (let ((content-blocks nil)
        (saw-tool-use nil)
        (cache-performance
          (usage-cache-performance (cdr (assoc :usage response)))))
    (dolist (item (coerce (or (cdr (assoc :output response)) #()) 'list))
      (dolist (block (responses-item->canonical-blocks item))
        (when (string= (cdr (assoc :type block)) "tool_use")
          (setf saw-tool-use t))
        (push block content-blocks)))
    (let ((fallback-text (cdr (assoc :output--text response))))
      (when (and (null content-blocks)
                 (stringp fallback-text)
                 (plusp (length fallback-text)))
        (push (canonical-text-block fallback-text) content-blocks)))
    (canonical-response (if saw-tool-use "tool_use" "end_turn")
                        (nreverse content-blocks)
                        cache-performance)))

(defun openai-codex-responses-request-body (messages model max-tokens tools
                                            &key stream reasoning-effort
                                                 (system-prompt (or (build-system-prompt) "")))
  "Build the request body for an OpenAI Responses API call."
  (declare (ignore max-tokens))
  (let* ((input-items (conversation-messages->responses-input messages))
         (response-tools (or (tool-definitions->responses-tools tools) #()))
         (body `((:model . ,model)
                 (:instructions . ,system-prompt)
                 (:input . ,(coerce input-items 'vector))
                 (:tools . ,response-tools)
                 (:tool--choice . "auto")
                 (:parallel--tool--calls . t)
                 (:store . ,+json-false+)
                 (:stream . ,(if stream t +json-false+))
                 (:include . #()))))
    (when reasoning-effort
      (setf body (append body
                         (list `(:reasoning . ((:effort . ,reasoning-effort)))))))
    (api-json-encode body)))

(defun openai-codex-request-headers (auth &key stream)
  "Build request headers for AUTH, optionally enabling streaming."
  (let ((headers `(("Authorization" . ,(format nil "Bearer ~A" (getf auth :token)))
                   ("originator" . ,*openai-oauth-originator*))))
    (when (getf auth :account-id)
      (push `("ChatGPT-Account-ID" . ,(getf auth :account-id)) headers))
    (when stream
      (push '("Accept" . "text/event-stream") headers))
    headers))

(defun openai-codex-responses-endpoint (auth)
  "Return AUTH's full Responses API endpoint URL."
  (format nil "~A/responses"
          (string-right-trim "/" (getf auth :base-url))))

(defun refresh-openai-codex-auth-descriptor ()
  "Force-refresh the shared ChatGPT OAuth auth descriptor and resolve it again."
  (let ((updated (refresh-openai-codex-auth-json)))
    (and updated
         (openai-codex-auth-descriptor-from-auth-json updated))))

(defun openai-codex-http-request (auth request-body &key stream (allow-refresh t))
  "Perform one OpenAI Codex HTTP request, refreshing ChatGPT auth once on 401."
  (multiple-value-bind (body status-code headers)
      (drakma:http-request
       (openai-codex-responses-endpoint auth)
       :method :post
       :content-type "application/json"
       :additional-headers (openai-codex-request-headers auth :stream stream)
       :external-format-in :utf-8
       :content request-body
       :want-stream stream
       :force-binary t)
    (declare (ignore headers))
    (if (and (= status-code 401)
             allow-refresh
             (getf auth :refreshable-p))
        (let ((refreshed (refresh-openai-codex-auth-descriptor)))
          (if refreshed
              (openai-codex-http-request refreshed request-body
                                         :stream stream
                                         :allow-refresh nil)
              (values body status-code nil auth)))
        (values body status-code nil auth))))

(defun stream-state-text-block-cell (state)
  "Return the cons cell holding STATE's text block, or NIL."
  (loop :for cell :on (stream-state-content-blocks state)
        :when (let ((block (car cell)))
                (and block
                     (string= "text" (cdr (assoc :type block)))))
          :return cell))

(defun set-stream-state-text-block (state text)
  "Set or create STATE's canonical text block to TEXT."
  (let ((cell (stream-state-text-block-cell state)))
    (if cell
        (setf (car cell) (canonical-text-block text))
        (push (canonical-text-block text) (stream-state-content-blocks state)))))

(defun provider-request (provider messages
                         &key model
                              (max-tokens *default-max-tokens*)
                              tools
                              reasoning-effort
                              (system-prompt (build-system-prompt)))
  "Dispatch a non-streaming request by resolved PROVIDER."
  (ecase provider
    (:openai-codex
     (openai-codex-request messages
                           :model model
                           :max-tokens max-tokens
                           :tools tools
                           :reasoning-effort reasoning-effort
                           :system-prompt system-prompt))
    (:zai
     (zai-request messages
                  :model model
                  :max-tokens max-tokens
                  :tools tools
                  :system-prompt system-prompt))
    (:openrouter
     (openrouter-request messages
                         :model model
                         :max-tokens max-tokens
                         :tools tools
                         :system-prompt system-prompt))))

(defun provider-request-streaming (provider messages callback
                                   &key model
                                        (max-tokens *default-max-tokens*)
                                        tools
                                        reasoning-effort
                                        (system-prompt (build-system-prompt)))
  "Dispatch a streaming request by resolved PROVIDER."
  (ecase provider
    (:openai-codex
     (openai-codex-request-streaming messages callback
                                     :model model
                                     :max-tokens max-tokens
                                     :tools tools
                                     :reasoning-effort reasoning-effort
                                     :system-prompt system-prompt))
    (:zai
     (zai-request-streaming messages callback
                            :model model
                            :max-tokens max-tokens
                            :tools tools
                            :system-prompt system-prompt))
    (:openrouter
     (openrouter-request-streaming messages callback
                                   :model model
                                   :max-tokens max-tokens
                                   :tools tools
                                   :system-prompt system-prompt))))

;;; --------------------------------------------------------------------------
;;; API Call
;;; --------------------------------------------------------------------------

(defun openai-codex-request (messages &key (model *openai-codex-model*)
                                           (max-tokens *default-max-tokens*)
                                           tools
                                           reasoning-effort
                                           (system-prompt (or (build-system-prompt) "")))
  "Call the OpenAI Responses API for Codex and normalize the response shape."
  (let* ((auth (or (resolve-openai-codex-auth)
                   (error 'simple-error
                          :format-control "No OpenAI Codex auth. Save a bearer token to ~/.config/clawmacs/openai-codex-token or sign in via ~/.codex/auth.json")))
         (request-body (openai-codex-responses-request-body
                        messages model max-tokens tools
                        :system-prompt system-prompt
                        :reasoning-effort reasoning-effort)))
    (multiple-value-bind (body status-code ignored effective-auth)
        (openai-codex-http-request auth request-body)
      (declare (ignore ignored effective-auth))
      (let ((body-string (http-body-string body)))
        (unless (= status-code 200)
          (error "API error (~A): ~A" status-code body-string))
        (responses-api-response->canonical-response
         (api-json-decode body-string))))))

;;; --------------------------------------------------------------------------
;;; Streaming API Call
;;; --------------------------------------------------------------------------

(defstruct stream-state
  "Mutable state for an in-progress streaming response."
  (text ""             :type string)
  (content-blocks nil  :type list)
  (tool-input-json ""  :type string)   ; accumulates input_json_delta for tool_use
  (openai-tool-call-states (make-hash-table :test #'equal))
  (openai-tool-call-order nil :type list)
  (stop-reason nil)
  (done-p nil          :type boolean)
  (error-p nil)
  (lock (bt:make-lock "stream-state")))

(defun parse-sse-line (line)
  "Parse a single SSE line. Returns (values field value) or nil.
SSE format: 'field: value' or just 'data: {...}'."
  (let ((colon-pos (position #\: line)))
    (when (and colon-pos (plusp colon-pos))
      (let ((field (subseq line 0 colon-pos))
            (value (if (< (1+ colon-pos) (length line))
                       (string-left-trim " " (subseq line (1+ colon-pos)))
                       "")))
        (values field value)))))

(defun stream-state-first-block (state)
  "Return STATE's current leading content block."
  (first (stream-state-content-blocks state)))

(defun ensure-openai-stream-text-block (state)
  "Ensure STATE has a current canonical text block."
  (let ((block (stream-state-first-block state)))
    (unless (and block (string= "text" (cdr (assoc :type block))))
      (push (canonical-text-block "") (stream-state-content-blocks state)))))

(defun openai-stream-tool-call-key (tool-call)
  "Return a stable key for a streamed OpenAI TOOL-CALL delta."
  (or (cdr (assoc :index tool-call))
      (cdr (assoc :id tool-call))
      (error "OpenAI streaming tool call missing index/id: ~S" tool-call)))

(defun upsert-openai-stream-tool-call (state tool-call)
  "Merge TOOL-CALL delta into STATE's OpenAI tool-call assembly tables."
  (let* ((key (openai-stream-tool-call-key tool-call))
         (function (cdr (assoc :function tool-call)))
         (existing (or (gethash key (stream-state-openai-tool-call-states state))
                       (list :id nil :name nil :arguments ""))))
    (unless (gethash key (stream-state-openai-tool-call-states state))
      (setf (stream-state-openai-tool-call-order state)
            (append (stream-state-openai-tool-call-order state) (list key))))
    (let ((updated (list :id (or (cdr (assoc :id tool-call))
                                 (getf existing :id))
                         :name (or (cdr (assoc :name function))
                                   (getf existing :name))
                         :arguments (let ((arguments (cdr (assoc :arguments function))))
                                      (if arguments
                                          (concatenate 'string
                                                       (getf existing :arguments)
                                                       arguments)
                                          (getf existing :arguments))))))
      (setf (gethash key (stream-state-openai-tool-call-states state)) updated)
      updated)))

(defun finalize-openai-stream-tool-blocks (state)
  "Finalize all assembled OpenAI streaming tool calls into canonical blocks."
  (let ((non-tool-blocks (remove-if (lambda (block)
                                      (string= "tool_use" (cdr (assoc :type block))))
                                    (stream-state-content-blocks state)))
        (tool-blocks
          (loop :for key :in (stream-state-openai-tool-call-order state)
                :for call-state := (gethash key (stream-state-openai-tool-call-states state))
                :collect (canonical-tool-use-block
                          (getf call-state :id)
                          (getf call-state :name)
                          (let ((arguments (getf call-state :arguments)))
                            (when (plusp (length arguments))
                              (handler-case
                                  (api-json-decode arguments)
                                (error () nil))))))))
    (let ((display-blocks (append (nreverse (copy-list non-tool-blocks))
                                  tool-blocks)))
      (setf (stream-state-content-blocks state)
            (nreverse display-blocks)))))

(defun process-openai-sse-event (data state)
  "Process a single OpenAI SSE DATA payload into STATE."
  (cond
    ((string= data "[DONE]")
     (bt:with-lock-held ((stream-state-lock state))
       (finalize-openai-stream-tool-blocks state)
       (when (plusp (length (stream-state-text state)))
         (ensure-openai-stream-text-block state)
         (setf (first (stream-state-content-blocks state))
               (canonical-text-block (stream-state-text state))))
       (unless (stream-state-stop-reason state)
         (setf (stream-state-stop-reason state) "end_turn"))
       (setf (stream-state-done-p state) t)))
    (t
     (let* ((event (api-json-decode data))
            (choice (first (coerce (cdr (assoc :choices event)) 'list)))
            (delta (and choice (cdr (assoc :delta choice))))
            (text (and delta (cdr (assoc :content delta))))
            (reasoning (and delta (cdr (assoc :reasoning--content delta))))
            (tool-calls (and delta (cdr (assoc :tool--calls delta))))
            (finish-reason (and choice (cdr (assoc :finish--reason choice))))
            ;; Use whichever text field is present in this chunk.
            ;; Reasoning models (Z.AI GLM, DeepSeek R1) stream
            ;; reasoning_content first, then content.
            (effective-text (or text reasoning)))
        (bt:with-lock-held ((stream-state-lock state))
          (when effective-text
            (ensure-openai-stream-text-block state)
            (setf (stream-state-text state)
                  (concatenate 'string (stream-state-text state) effective-text)
                  (first (stream-state-content-blocks state))
                  (canonical-text-block (stream-state-text state))))
          (dolist (tool-call (coerce (or tool-calls #()) 'list))
            (upsert-openai-stream-tool-call state tool-call))
          (when finish-reason
            (when (or tool-calls
                      (stream-state-openai-tool-call-order state))
              (finalize-openai-stream-tool-blocks state))
            (setf (stream-state-stop-reason state)
                  (openai-finish-reason->stop-reason finish-reason))))))))

(defun read-openai-sse-stream (stream state)
  "Read OpenAI SSE events from STREAM into STATE."
  (handler-case
      (loop :with data-buffer := nil
            :for line := (read-line stream nil nil)
            :while line
            :do (let ((trimmed (string-trim '(#\Return) line)))
                  (cond
                    ((zerop (length trimmed))
                     (when data-buffer
                       (process-openai-sse-event
                        (format nil "~{~A~}" (nreverse data-buffer))
                        state)
                       (setf data-buffer nil)))
                    (t
                     (multiple-value-bind (field value) (parse-sse-line trimmed)
                       (when (and field (string= "data" field))
                         (push value data-buffer)))))))
    (error (e)
      (bt:with-lock-held ((stream-state-lock state))
        (setf (stream-state-error-p state) (format nil "~A" e)
              (stream-state-done-p state) t))))
  (bt:with-lock-held ((stream-state-lock state))
    (setf (stream-state-done-p state) t)))

(defun responses-stream-tool-use-present-p (state)
  "Return non-nil when STATE already contains a tool_use block."
  (some (lambda (block)
          (string= "tool_use" (cdr (assoc :type block))))
        (stream-state-content-blocks state)))

(defun process-openai-codex-responses-sse-event (data state)
  "Process one Responses API SSE DATA payload into STATE."
  (let* ((event (api-json-decode data))
         (event-type (cdr (assoc :type event))))
    (cond
      ((string= event-type "response.output_text.delta")
       (let ((delta (cdr (assoc :delta event))))
         (when delta
           (bt:with-lock-held ((stream-state-lock state))
             (setf (stream-state-text state)
                   (concatenate 'string (stream-state-text state) delta))
             (set-stream-state-text-block state (stream-state-text state))))))
      ((string= event-type "response.output_item.done")
       (let ((item (cdr (assoc :item event))))
         (when item
           (let ((item-type (cdr (assoc :type item))))
             (bt:with-lock-held ((stream-state-lock state))
               (cond
                 ((and (string= item-type "message")
                       (string= (cdr (assoc :role item)) "assistant"))
                  (let ((text (responses-output-item-text item)))
                    (when text
                      (setf (stream-state-text state) text)
                      (set-stream-state-text-block state text))))
                 ((string= item-type "function_call")
                  (push (responses-function-call->canonical-block item)
                        (stream-state-content-blocks state))
                  (setf (stream-state-stop-reason state) "tool_use"))))))))
      ((string= event-type "response.completed")
       (bt:with-lock-held ((stream-state-lock state))
         (unless (stream-state-stop-reason state)
           (setf (stream-state-stop-reason state)
                 (if (responses-stream-tool-use-present-p state)
                     "tool_use"
                     "end_turn")))
         (setf (stream-state-done-p state) t)))
      ((or (string= event-type "response.failed")
           (string= event-type "error"))
       (let ((message (or (cdr (assoc :message event))
                          (cdr (assoc :error event))
                          "Unknown Responses API error")))
         (bt:with-lock-held ((stream-state-lock state))
           (setf (stream-state-error-p state) message
                 (stream-state-done-p state) t)))))))

(defun read-openai-codex-responses-sse-stream (stream state)
  "Read Responses API SSE events from STREAM into STATE."
  (handler-case
      (loop :with data-buffer := nil
            :for line := (read-line stream nil nil)
            :while line
            :do (let ((trimmed (string-trim '(#\Return) line)))
                  (cond
                    ((zerop (length trimmed))
                     (when data-buffer
                       (process-openai-codex-responses-sse-event
                        (format nil "~{~A~}" (nreverse data-buffer))
                        state)
                       (setf data-buffer nil)))
                    (t
                     (multiple-value-bind (field value) (parse-sse-line trimmed)
                       (when (and field (string= "data" field))
                         (push value data-buffer)))))))
    (error (e)
      (bt:with-lock-held ((stream-state-lock state))
        (setf (stream-state-error-p state) (format nil "~A" e)
              (stream-state-done-p state) t))))
  (bt:with-lock-held ((stream-state-lock state))
    (setf (stream-state-done-p state) t)))

(defun openai-codex-request-streaming (messages callback
                                       &key (model *openai-codex-model*)
                                            (max-tokens *default-max-tokens*)
                                            tools
                                            reasoning-effort
                                            (system-prompt (or (build-system-prompt) "")))
  "Call the OpenAI Responses API with SSE streaming enabled."
  (declare (ignore callback))
  (let* ((auth (or (resolve-openai-codex-auth)
                   (error 'simple-error
                          :format-control "No OpenAI Codex auth. Save a bearer token to ~/.config/clawmacs/openai-codex-token or sign in via ~/.codex/auth.json")))
         (request-body
           (openai-codex-responses-request-body
            messages model max-tokens tools
            :stream t
            :system-prompt system-prompt
            :reasoning-effort reasoning-effort))
         (state (make-stream-state)))
    (multiple-value-bind (body-stream status-code ignored effective-auth)
        (openai-codex-http-request auth request-body :stream t)
      (declare (ignore ignored effective-auth))
      (unless (= status-code 200)
        (let ((err (if (streamp body-stream)
                       (unwind-protect
                            (read-stream-as-utf8-string body-stream)
                         (ignore-errors (close body-stream)))
                       (format nil "~A" body-stream))))
          (error "API error (~A): ~A" status-code err)))
      (let ((sse-stream (utf8-character-input-stream body-stream)))
      (bt:make-thread
       (lambda ()
         (unwind-protect
              (read-openai-codex-responses-sse-stream sse-stream state)
           (close sse-stream)))
       :name "clawmacs-openai-codex-responses")
      state))))

;;; --------------------------------------------------------------------------
;;; OpenRouter API — OpenAI-compatible
;;; --------------------------------------------------------------------------
;;; OpenRouter proxies requests to 300+ models (OpenAI, Anthropic, Google,
;;; Meta, DeepSeek, etc.) through a single OpenAI-compatible endpoint.
;;; Authentication uses a Bearer API key obtained from openrouter.ai/keys.
;;; Model names follow the 'provider/model-name' format.

(defun openrouter-request (messages &key (model *openrouter-model*)
                                          (max-tokens *default-max-tokens*)
                                          tools
                                          (system-prompt (build-system-prompt)))
  "Call the OpenRouter Chat Completions API and normalize the response shape.
Uses the OpenAI-compatible chat completions protocol."
  (let* ((token (or (read-provider-token :openrouter)
                    (error 'simple-error
                           :format-control "No OpenRouter API key. Set OPENROUTER_API_KEY env var or save to ~/.config/clawmacs/openrouter-api-key")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--tokens . ,max-tokens)
                         (:messages . ,(coerce (openai-messages-with-system-prompt
                                               messages
                                               :system-prompt system-prompt)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(tool-definitions->openai-tools tools)) body))
              (api-json-encode body))))
    (multiple-value-bind (body status-code)
        (drakma:http-request
         *openrouter-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                               ("HTTP-Referer" . "https://github.com/clawmacs/clawmacs")
                               ("X-Title" . "clawmacs"))
         :content request-body
         :want-stream nil
         :force-binary nil)
      (let ((body-string (http-body-string body)))
        (unless (= status-code 200)
          (error "OpenRouter API error (~A): ~A" status-code body-string))
        (let* ((response (api-json-decode body-string))
               (choices (cdr (assoc :choices response)))
               (choice (first (coerce choices 'list))))
          (unless choice
            (error "OpenRouter response did not include a choice"))
          (openai-choice->canonical-response
           choice
           (openai-response-cache-performance response)))))))

(defun openrouter-request-streaming (messages callback
                                     &key (model *openrouter-model*)
                                          (max-tokens *default-max-tokens*)
                                          tools
                                          (system-prompt (build-system-prompt)))
  "Call the OpenRouter Chat Completions API with SSE streaming enabled.
Uses the same OpenAI-compatible streaming protocol."
  (declare (ignore callback))
  (let* ((token (or (read-provider-token :openrouter)
                    (error 'simple-error
                           :format-control "No OpenRouter API key. Set OPENROUTER_API_KEY env var or save to ~/.config/clawmacs/openrouter-api-key")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--tokens . ,max-tokens)
                          (:stream . t)
                         (:messages . ,(coerce (openai-messages-with-system-prompt
                                               messages
                                               :system-prompt system-prompt)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(tool-definitions->openai-tools tools)) body))
              (api-json-encode body)))
         (state (make-stream-state)))
    (multiple-value-bind (body-stream status-code headers)
        (drakma:http-request
         *openrouter-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                               ("HTTP-Referer" . "https://github.com/clawmacs/clawmacs")
                               ("X-Title" . "clawmacs"))
         :content request-body
         :want-stream t)
      (declare (ignore headers))
      (unless (= status-code 200)
        (let ((err (if (streamp body-stream)
                       (let ((s (make-string-output-stream)))
                         (loop :for c := (read-char body-stream nil nil)
                               :while c :do (write-char c s))
                         (get-output-stream-string s))
                       (format nil "~A" body-stream))))
          (error "OpenRouter API error (~A): ~A" status-code err)))
      (bt:make-thread
       (lambda ()
         (unwind-protect
              (read-openai-sse-stream body-stream state)
           (close body-stream)))
       :name "clawmacs-openrouter-sse-reader")
      state)))

;;; --------------------------------------------------------------------------
;;; Z.AI (Zhipu AI) API — OpenAI-compatible
;;; --------------------------------------------------------------------------

(defun zai-request (messages &key (model *zai-model*)
                                   (max-tokens *default-max-tokens*)
                                   tools
                                   (system-prompt (build-system-prompt)))
  "Call Z.AI Chat Completions API and normalize the response shape.
Uses the coding plan endpoint (api.z.ai/api/coding/paas/v4) which is
compatible with the GLM Coding Max-Monthly subscription.
The API follows the OpenAI Chat Completions format."
  (let* ((token (or (read-provider-token :zai)
                    (error 'simple-error
                           :format-control "No Z.AI API key. Set ZAI_CODING_MAX_API_KEY env var or save to ~/.config/clawmacs/zai-api-key")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--tokens . ,max-tokens)
                         (:messages . ,(coerce (openai-messages-with-system-prompt
                                               messages
                                               :system-prompt system-prompt)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(tool-definitions->openai-tools tools)) body))
              (api-json-encode body))))
    (multiple-value-bind (body status-code)
        (drakma:http-request
         *zai-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                               ("Accept-Language" . "en-US,en"))
         :content request-body
         :want-stream nil
         :force-binary nil)
      (let ((body-string (http-body-string body)))
        (unless (= status-code 200)
          (error "Z.AI API error (~A): ~A" status-code body-string))
        (let* ((response (api-json-decode body-string))
               (choices (cdr (assoc :choices response)))
               (choice (first (coerce choices 'list))))
          (unless choice
            (error "Z.AI response did not include a choice"))
          (openai-choice->canonical-response
           choice
           (openai-response-cache-performance response)))))))

(defun zai-request-streaming (messages callback
                               &key (model *zai-model*)
                                    (max-tokens *default-max-tokens*)
                                    tools
                                    (system-prompt (build-system-prompt)))
  "Call Z.AI Chat Completions API with SSE streaming enabled.
Uses the same OpenAI-compatible streaming protocol."
  (declare (ignore callback))
  (let* ((token (or (read-provider-token :zai)
                    (error 'simple-error
                           :format-control "No Z.AI API key. Set ZAI_CODING_MAX_API_KEY env var or save to ~/.config/clawmacs/zai-api-key")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--tokens . ,max-tokens)
                          (:stream . t)
                         (:messages . ,(coerce (openai-messages-with-system-prompt
                                               messages
                                               :system-prompt system-prompt)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(tool-definitions->openai-tools tools)) body))
              (api-json-encode body)))
         (state (make-stream-state)))
    (multiple-value-bind (body-stream status-code headers)
        (drakma:http-request
         *zai-api-url*
         :method :post
         :content-type "application/json"
         :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                               ("Accept-Language" . "en-US,en"))
         :content request-body
         :want-stream t)
      (declare (ignore headers))
      (unless (= status-code 200)
        (let ((err (if (streamp body-stream)
                       (let ((s (make-string-output-stream)))
                         (loop :for c := (read-char body-stream nil nil)
                               :while c :do (write-char c s))
                         (get-output-stream-string s))
                       (format nil "~A" body-stream))))
          (error "Z.AI API error (~A): ~A" status-code err)))
      (bt:make-thread
       (lambda ()
         (unwind-protect
              (read-openai-sse-stream body-stream state)
           (close body-stream)))
       :name "clawmacs-zai-sse-reader")
      state)))

;;; --------------------------------------------------------------------------
;;; Response Parsing Helpers
;;; --------------------------------------------------------------------------

(defun response-stop-reason (response)
  "Extract stop_reason from an API response."
  (cdr (assoc :stop--reason response)))
(defun response-cache-performance (response)
  "Extract cache-performance metrics from an API response."
  (cdr (assoc :cache--performance response)))

(defun cache-performance-prompt-tokens (cache-performance)
  "Return prompt tokens represented by CACHE-PERFORMANCE."
  (or (and cache-performance (cdr (assoc :prompt--tokens cache-performance)))
      0))

(defun cache-performance-cached-tokens (cache-performance)
  "Return cached prompt tokens represented by CACHE-PERFORMANCE."
  (or (and cache-performance (cdr (assoc :cached--tokens cache-performance)))
      0))

(defun response-cache-hit-rate (response)
  "Return RESPONSE cache hit rate percentage, or NIL when unavailable."
  (let* ((cache-performance (response-cache-performance response))
         (prompt-tokens (cache-performance-prompt-tokens cache-performance))
         (cached-tokens (cache-performance-cached-tokens cache-performance)))
    (when (plusp prompt-tokens)
      (* 100.0 (/ cached-tokens prompt-tokens)))))

(defun response-content (response)
  "Extract content blocks from an API response as a list."
  (let ((content (cdr (assoc :content response))))
    (coerce content 'list)))

(defun content-block-type (block)
  "Return the type string of a content block."
  (cdr (assoc :type block)))

(defun content-text-blocks (content-blocks)
  "Extract and concatenate all text from text-type content blocks."
  (with-output-to-string (s)
    (let ((first t))
      (dolist (block content-blocks)
        (when (string= "text" (content-block-type block))
          (unless first (write-char #\Newline s))
          (write-string (cdr (assoc :text block)) s)
          (setf first nil))))))

(defun content-reasoning-blocks (content-blocks)
  "Extract provider-supplied reasoning text blocks."
  (loop :for block :in content-blocks
        :when (string= "reasoning" (content-block-type block))
          :collect (or (cdr (assoc :text block)) "")))

(defun content-tool-use-blocks (content-blocks)
  "Extract tool_use blocks from content."
  (remove-if-not (lambda (b) (string= "tool_use" (content-block-type b)))
                 content-blocks))

(defun format-tool-call-display (tool-use-block)
  "Format a tool_use block for display in the chat."
  (let ((name (cdr (assoc :name tool-use-block)))
        (input (cdr (assoc :input tool-use-block))))
    (format-tool-call-sexpr name input)))

(defun format-lisp-eval-result-display (result-text)
  "Format RESULT-TEXT from lisp_eval as a Lisp-friendly display block."
  (handler-case
      (let* ((payload (api-json-decode result-text))
             (code (cdr (assoc :code payload)))
             (values-count (or (cdr (assoc :values payload)) 0))
             (result (cdr (assoc :result payload)))
             (error-text (cdr (assoc :error payload)))
             (denied-p (cdr (assoc :denied payload)))
             (reason (cdr (assoc :reason payload))))
        (with-output-to-string (s)
          (write-string ";; lisp_eval" s)
          (when code
            (format s "~%~A" code))
          (cond
            (denied-p
             (format s "~%;; denied~%~S" (or reason "User denied")))
            (error-text
             (format s "~%;; error~%~A"
                     (format nil "(error ~S)" error-text)))
            (t
             (format s "~%;; => ~D value~:P" values-count)
             (when result
               (format s "~%~A" result))))))
    (error ()
      nil)))

(defun format-tool-result-display (tool-name result-text)
  "Format a tool result for display in the chat."
  (or (and (string= tool-name "lisp_eval")
           (format-lisp-eval-result-display result-text))
      (let ((preview (if (> (length result-text) 200)
                         (concatenate 'string (subseq result-text 0 200) "...")
                         result-text)))
        (format nil "[~A result: ~A]" tool-name preview))))
