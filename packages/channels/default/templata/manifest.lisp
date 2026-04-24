(:name "templata"
 :description "Slash commands and prompt-template expansion."
 :entrypoint "package.lisp"
 :autoload t
 :system-prompt-section "## Slash commands and prompt templates

- Type `/name` in the composer to invoke a registered slash command.
- If no slash command is registered for that name, Clawmacs also checks for a
  prompt template in the current project's `.clawmacs/prompts/*.md`, any
  enabled package prompt-template directories declared in their manifests, and
  the global `~/.clawmacs.d/prompts/*.md`.
- Template placeholders support `$1`, `$2`, `$@`, `$ARGUMENTS`,
  `${@:N}`, and `${@:N:M}`."
 :slash-commands ((:name "model"
                   :handler "templata-slash-model"
                   :description "Change the active model or open the model selector."
                   :argument-hint "[provider/model]")
                  (:name "session"
                   :handler "templata-slash-session"
                   :description "Open the session tree for the current conversation.")
                  (:name "resume"
                   :handler "templata-slash-resume"
                   :description "Resume a saved session by exact name or unique prefix."
                   :argument-hint "<session>")
                  (:name "new"
                   :handler "templata-slash-new"
                   :description "Create and switch to a new chat buffer.")
                  (:name "export"
                   :handler "templata-slash-export"
                   :description "Save the current session snapshot to disk.")
                  (:name "reload"
                   :handler "templata-slash-reload"
                   :description "Reload skills, package manifests, and prompt-template files.")))
