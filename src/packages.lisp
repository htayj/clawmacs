(defpackage #:clawmacs/matching-core
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:list #:coalton-library/list)
   (#:str #:coalton-library/string))
  (:export
   #:split-query-tokens
   #:fuzzy-token-match-p
   #:fuzzy-match-p
   #:fuzzy-token-positions
   #:fuzzy-match-positions
   #:fuzzy-token-score-or-negative-one
   #:fuzzy-score-or-negative-one))

(defpackage :clawmacs
  (:use :cl)
  (:export
   ;; General utilities
   #:count-occurrences

   ;; Color specs
   #:color-spec
   #:make-color-spec
   #:color-spec-type
   #:color-spec-value

   ;; Faces
   #:face
   #:face-name
   #:face-foreground
   #:face-background
   #:face-bold-p
   #:face-underline-p
   #:face-reverse-p
   #:face-parent
   #:face-transform
   #:resolved-face
   #:resolved-face-foreground
   #:resolved-face-background
   #:resolved-face-bold-p
   #:resolved-face-underline-p
   #:resolved-face-reverse-p
   #:resolve-face

   ;; Face sets
   #:face-set
   #:face-set-owner
   #:face-set-faces
   #:make-face-set
   #:get-face

   ;; Global face registry
   #:*global-face-registry*
   #:global-face
   #:init-global-faces
   #:collect-global-faces
   #:apply-global-face
   #:make-default-system-face-set
   #:make-default-text-face

   ;; Lines
   #:line
   #:make-line
   #:line-content
   #:line-next
   #:line-prev

   ;; Messages
   #:message
   #:make-message
   #:message-first-line
   #:message-last-line
   #:message-point-line
   #:message-point-offset
   #:message-mark-line
   #:message-mark-offset
   #:message-sender
   #:message-timestamp
   #:message-face-set
   #:message-read-only-p
   #:message-next
   #:message-prev
   #:message-raw-content
   #:message-text
   #:message-line-count

   ;; Message editing
   #:message-insert-char
   #:message-insert-newline
   #:message-delete-char-backward
   #:message-delete-char-forward
   #:message-forward-char
   #:message-backward-char
   #:message-forward-word
   #:message-backward-word
   #:message-move-beginning-of-line
   #:message-move-end-of-line
   #:message-kill-line
   #:message-kill-backward-line
   #:message-kill-word
   #:message-backward-kill-word
   #:message-yank
   #:message-yank-pop

   ;; Kill ring
   #:*kill-ring*
   #:kill-ring-push
   #:kill-ring-top

   ;; Buffer
   #:*default-context-limit*
   #:*default-agent-name*
   #:*default-show-tool-results*
   #:*scratch-buffer-name*
   #:*scratch-buffer-initial-text*
   #:buffer
   #:make-buffer
   #:buffer-name
   #:buffer-first-message
   #:buffer-last-message
   #:buffer-input-message
   #:buffer-agent-name
   #:buffer-kind
   #:buffer-working-directory
   #:buffer-project-name
   #:buffer-resource-path
   #:buffer-original-text
   #:buffer-dirty-p
    #:buffer-token-count
    #:buffer-context-limit
    #:buffer-status
    #:buffer-provider-override
    #:buffer-model-override
    #:buffer-think-level-override
    #:set-buffer-provider-override
    #:set-buffer-model-override
    #:set-buffer-think-level-override
    #:clear-buffer-provider-override
    #:clear-buffer-model-override
    #:clear-buffer-think-level-override
    #:clear-buffer-routing-overrides
     #:buffer-face-registry
   #:buffer-keymap
   #:buffer-scroll-offset
   #:buffer-show-tool-results-p
   #:buffer-pending-stream
   #:buffer-streaming-message
   #:*buffer-ring*
   #:current-buffer
   #:add-buffer-to-ring
   #:switch-to-buffer
   #:kill-buffer-from-ring
   #:find-buffer-by-name
   #:scratch-buffer-p
   #:file-buffer-p
   #:document-buffer-p
   #:scratch-buffer
   #:ensure-scratch-buffer
   #:scratch-buffer-text
   #:file-buffer-text
   #:file-buffer-dirty-p
   #:mark-buffer-dirty
   #:next-buffer-name
   #:buffer-names
   #:buffer-finalize-input
   #:buffer-insert-agent-message
   #:buffer-insert-system-message
   #:buffer-message-count
   #:save-session
   #:load-session
   #:list-saved-sessions
   #:*sessions-dir*

   ;; Commands
   #:*current-caller*
   #:*sandbox-root*
   #:*command-table*
   #:command-metadata
   #:command-metadata-name
   #:command-metadata-permission
   #:command-metadata-docstring
   #:command-metadata-keybindings
   #:command-metadata-lambda-list
   #:command-metadata-interactive-spec
   #:defcommand
   #:check-permission
   #:permission-denied
   #:permission-required
   #:list-available-commands
   #:command-required-arguments
   #:command-interactive-p
   #:list-interactive-commands
   #:resolve-command-interactive-reader

   ;; Extended documentation
   #:*extended-docs*
   #:defdoc
   #:extended-doc
   #:undocumented-functions
   #:undocumented-variables

   ;; Keymaps
   #:keymap
   #:make-keymap
   #:keymap-name
   #:keymap-bindings
   #:keymap-parent
   #:keymap-bind
   #:keymap-lookup
   #:*default-keymap*

   ;; Projects
   #:project
   #:make-project
   #:project-name
   #:project-root
   #:project-description
   #:project-source
   #:project-systems
   #:project-check-functions
   #:project-reload-function
   #:change-set
   #:make-change-set
   #:change-set-id
   #:change-set-name
   #:change-set-description
   #:change-set-entries
   #:change-set-status
   #:change-set-created-at
   #:change-set-applied-at
   #:change-set-entry
   #:change-set-entry-kind
   #:change-set-entry-project-name
   #:change-set-entry-path
   #:change-set-entry-new-path
   #:change-set-entry-new-text
   #:change-set-entry-old-exists-p
   #:change-set-entry-old-text
   #:change-set-entry-target-old-exists-p
   #:change-set-entry-target-old-text
   #:change-set-entry-applied-p
   #:*project-definitions-directory*
   #:*project-registry*
   #:*project-manifest-extension*
   #:*project-ignored-directory-names*
   #:*project-list-file-limit*
   #:*project-search-result-limit*
   #:*change-set-registry*
   #:*current-change-set*
   #:define-project
   #:create-project
   #:register-project
   #:find-project
   #:list-projects
   #:load-project-definitions
   #:reload-projects
   #:project-resolve-path
   #:project-list-files
   #:project-read-file
   #:project-create-file
   #:project-save-file
   #:project-search
   #:project-search-to-string
   #:project-open-file
   #:find-project-file-buffer
   #:project-save-buffer
   #:begin-change-set
   #:current-change-set
   #:find-change-set
   #:list-change-sets
   #:stage-project-file
   #:stage-project-delete
   #:stage-project-rename
   #:change-set-project-file-text
   #:change-set-diff-to-string
   #:change-set-summary-to-string
   #:apply-change-set
   #:discard-change-set
   #:revert-change-set
   #:run-project-checks
   #:compile-project-file
   #:load-project-file
   #:reload-project-system
   #:project-outline-to-string
   #:project-find-definitions
   #:project-find-definitions-to-string
   #:project-find-references-to-string
   #:project-package-map-to-string
   #:project-describe-definition-to-string

    ;; LLM
    #:*zai-env-var*
    #:read-env-token
    #:provider-token-path
    #:read-provider-token
    #:save-provider-token
     #:agent-definition
     #:make-agent-definition
     #:agent-definition-name
     #:agent-definition-provider
     #:agent-definition-model
     #:agent-definition-think-level
     #:agent-definition-core-prompt
     #:agent-definition-personality-prompt
     #:register-agent-definition
     #:find-agent-definition
     #:list-agent-definitions
     #:load-agent-defaults
     #:save-agent-defaults
     #:agent-default
     #:set-agent-default
     #:ensure-agent-defaults-loaded
     #:resolve-buffer-provider-and-model
     #:build-agent-system-prompt
     #:*default-core-system-prompt*
     #:*default-personality-prompt*
     #:*personality-prompt-path*
     #:load-personality-prompt-file
     #:provider-model-supported-think-levels
     #:reconcile-buffer-think-level-override
     #:*boot-file-names*
     #:load-boot-files
     #:*agent-defaults-path*
     #:build-conversation-messages
   #:*default-provider*
   #:*default-model*
   #:*default-max-tokens*
   #:api-json-encode
   #:api-json-decode

   ;; Z.AI (Zhipu AI)
   #:*zai-model*
   #:*zai-api-url*
   #:zai-request
   #:zai-request-streaming

   ;; OpenRouter
   #:*openrouter-model*
   #:*openrouter-api-url*
   #:*openrouter-models-url*
   #:*openrouter-env-var*
   #:*openrouter-cached-models*
   #:fetch-openrouter-models
   #:openrouter-request
   #:openrouter-request-streaming

   ;; OpenAI Codex OAuth
   #:*openai-oauth-client-id*
   #:*openai-oauth-auth-url*
   #:*openai-oauth-token-url*
   #:*openai-oauth-redirect-uri*
   #:*openai-oauth-scopes*
   #:*codex-auth-path*
   #:generate-code-verifier
   #:compute-code-challenge
   #:generate-oauth-state
   #:url-encode-param
   #:extract-oauth-callback-params
   #:openai-codex-oauth-start
   #:exchange-openai-oauth-code
   #:save-openai-codex-oauth-tokens
   #:read-openai-codex-oauth-tokens
   #:refresh-openai-codex-oauth-token
   #:read-openai-codex-oauth-token
   #:openai-codex-oauth-finish
   #:*openai-oauth-pending*
   #:openai-codex-oauth-command

   ;; Tools
   #:*http-fetch-max-chars*
   #:*http-connection-timeout*
   #:*http-user-agent*
   #:*file-read-default-limit*
   #:*shell-exec-default-timeout*
   #:*diff-display-max-lines*
   #:*last-eval-result*
   #:*last-eval-condition*
   #:*lisp-eval-history*
   #:*lisp-eval-history-limit*
   #:*lisp-eval-max-output-chars*
   #:*tool-table*
   #:lisp-eval-record
   #:lisp-eval-record-code
   #:lisp-eval-record-package
   #:lisp-eval-record-result
   #:lisp-eval-record-stdout
   #:lisp-eval-record-stderr
   #:lisp-eval-record-condition
   #:lisp-eval-record-timestamp
   #:tool-definition
   #:register-tool
   #:execute-tool
   #:tool-requires-permission-p
   #:tool-definitions-for-api
   #:format-tool-call-sexpr
   #:format-tool-call-expanded
   #:tool-approval-extra-display
   #:eval-history-to-string
   #:init-tools

   ;; Standard reference / library discovery
   #:*common-lisp-spec-root*
   #:common-lisp-spec-available-p
   #:find-common-lisp-spec-entry
   #:describe-common-lisp-symbol-to-string
   #:search-common-lisp-spec-to-string
   #:list-project-systems
   #:describe-system-to-string
   #:list-system-packages
   #:list-package-functions
   #:list-package-variables
   #:list-package-types
   #:describe-library-symbol-to-string
   #:search-system-docs

   ;; Sexed structural editing
   #:sexed-balanced-p
   #:balanced-parentheses-p
   #:sexed-diagnostics
   #:sexed-find-forms
   #:sexed-form-text
   #:sexed-outline-to-string
   #:sexed-replace-form
   #:sexed-delete-form
   #:sexed-insert-before-form
   #:sexed-insert-after-form
   #:sexed-insert-form-before
   #:sexed-insert-form-after
   #:sexed-wrap-form
   #:sexed-splice-form
   #:sexed-raise-form
   #:sexed-slurp-forward
   #:sexed-barf-forward
   #:sexed-file-outline-to-string
   #:sexed-file-form-text
   #:sexed-replace-file-form
   #:sexed-delete-file-form
   #:sexed-insert-before-file-form
   #:sexed-insert-after-file-form
   #:sexed-insert-file-form-before
   #:sexed-insert-file-form-after
   #:sexed-wrap-file-form
   #:sexed-splice-file-form
   #:sexed-raise-file-form
   #:sexed-slurp-forward-file-form
   #:sexed-barf-forward-file-form
   #:sexed-project-outline-to-string
   #:sexed-project-form-text
   #:sexed-replace-project-form
   #:sexed-delete-project-form
   #:sexed-insert-before-project-form
   #:sexed-insert-after-project-form
   #:sexed-insert-project-form-before
   #:sexed-insert-project-form-after
   #:sexed-wrap-project-form
   #:sexed-splice-project-form
   #:sexed-raise-project-form
   #:sexed-slurp-forward-project-form
   #:sexed-barf-forward-project-form
   #:sexed-update-staged-project-file
   #:sexed-stage-replace-project-form
   #:sexed-stage-delete-project-form
   #:sexed-stage-insert-before-project-form
   #:sexed-stage-insert-after-project-form
   #:sexed-stage-insert-project-form-before
   #:sexed-stage-insert-project-form-after
   #:sexed-stage-wrap-project-form
   #:sexed-stage-splice-project-form
   #:sexed-stage-raise-project-form
   #:sexed-stage-slurp-forward-project-form
   #:sexed-stage-barf-forward-project-form
   #:sexed-replace-message-form
   #:sexed-delete-message-form
   #:sexed-insert-before-message-form
   #:sexed-insert-after-message-form
   #:sexed-insert-message-form-before
   #:sexed-insert-message-form-after
   #:sexed-wrap-message-form
   #:sexed-splice-message-form
   #:sexed-raise-message-form
   #:sexed-slurp-forward-message-form
   #:sexed-barf-forward-message-form
   #:sexed-scratch-message
   #:sexed-scratch-text
   #:sexed-scratch-outline-to-string
   #:sexed-scratch-form-text
   #:sexed-replace-scratch-form
   #:sexed-delete-scratch-form
   #:sexed-insert-before-scratch-form
   #:sexed-insert-after-scratch-form
   #:sexed-insert-scratch-form-before
   #:sexed-insert-scratch-form-after
   #:sexed-wrap-scratch-form
   #:sexed-splice-scratch-form
   #:sexed-raise-scratch-form
   #:sexed-slurp-forward-scratch-form
   #:sexed-barf-forward-scratch-form

   ;; Approval
   #:buffer-approval-pending
   #:buffer-approval-result
   #:buffer-stashed-input
   #:buffer-pending-tool-calls
   #:buffer-tool-call-results

   ;; Rendering
   #:resolve-modeline-provider-model
   #:render-modeline
   #:render-buffer

   ;; Buffer selector
   #:*buffer-selector-active*
   #:*buffer-selector-index*
   #:list-buffers-command
   #:minibuffer-select-project-command
   #:open-project-file-command
   #:create-project-file-command
   #:search-project-command
   #:render-buffer-selector

   ;; Model selector (overlay)
   #:*model-selector-active*
   #:*model-selector-index*
   #:*model-selector-entries*
   #:select-model-command
   #:render-model-selector
   #:handle-model-selector-key
   #:*think-selector-active*
   #:*think-selector-index*
   #:*think-selector-entries*
   #:select-think-level-command
   #:render-think-selector
   #:handle-think-selector-key

   ;; Minibuffer
   #:*minibuffer-active*
   #:*minibuffer-mode*
   #:*minibuffer-prompt*
   #:*minibuffer-input*
   #:*minibuffer-point*
   #:*minibuffer-items*
   #:*minibuffer-filtered-items*
   #:*minibuffer-selected-index*
   #:*minibuffer-scroll-offset*
   #:*minibuffer-match-positions*
   #:*minibuffer-callback*
   #:*minibuffer-max-height*
   #:*model-selection-history*
   #:matching-core-available-p
   #:split-query-tokens
   #:fuzzy-token-match-p
   #:fuzzy-token-positions
   #:fuzzy-match-positions
   #:fuzzy-token-score
   #:fuzzy-score
   #:fuzzy-match-p
   #:minibuffer-item-display
   #:minibuffer-item-match-text
   #:minibuffer-activate
   #:minibuffer-prompt
   #:minibuffer-deactivate
   #:minibuffer-update-filter
   #:minibuffer-visible-item-count
   #:minibuffer-ensure-visible
   #:minibuffer-confirm
   #:minibuffer-cancel
   #:handle-minibuffer-key
   #:sort-models-by-recency
   #:*buffer-selection-history*
   #:sort-buffers-by-recency
   #:minibuffer-current-height
   #:update-window-layout
   #:render-minibuffer
   #:invoke-command
   #:execute-extended-command
   #:minibuffer-select-agent-command
   #:minibuffer-select-model-command
   #:minibuffer-select-think-level-command
   #:minibuffer-select-buffer-command

   ;; Introspection / Help
   #:list-functions
   #:find-keybindings-for-command
   #:format-key-binding
   #:describe-function-to-string
   #:make-help-buffer
   #:describe-function-command
   #:buffer-major-mode
   #:list-variables
   #:variable-kind
   #:truncate-value-string
   #:describe-variable-to-string
   #:describe-variable-command

   ;; Describe bindings
   #:*ch-pending*
   #:categorize-command
   #:describe-bindings-to-string
   #:describe-bindings-command

   ;; Customize face
   #:*customize-face-state*
   #:*customize-face-fields*
   #:cga-color-name
   #:format-color-spec-display
   #:format-boolean-display
   #:format-face-parent-display
   #:customize-face-field-value
   #:customize-face-set-field-value
   #:customize-face-field-label
   #:customize-face-field-display
   #:customize-face-snapshot
   #:customize-face-restore-snapshot
   #:build-customize-face-content
   #:rebuild-customize-face-display
   #:customize-face-next-field
   #:customize-face-prev-field
   #:customize-face-toggle-field
   #:collect-all-faces
   #:make-color-selection-items
   #:make-boolean-selection-items
   #:make-parent-selection-items
   #:customize-face-edit-field
   #:customize-face-apply
   #:customize-face-cancel
   #:customize-face-revert-to-original
   #:make-customize-face-buffer
   #:handle-customize-key
   #:customize-face-command

   ;; Type introspection
   #:list-types
   #:type-kind
   #:type-kind-label
   #:type-slot-info
   #:type-struct-slot-info
   #:describe-type-to-string
   #:undocumented-types
   #:describe-type-command

   ;; Known models
   #:*provider-known-models*
   #:provider-known-models
   #:provider-has-token-p
   #:available-models-for-selector

   ;; Prefix processing
   #:*prefix-handlers*
   #:find-prefix-handler
   #:process-prefix-command
   #:shell-prefix-handler

   ;; Debug mode
   #:*debug-mode*
   #:*debug-log-file*
   #:debug-log
   #:file-debug-log
   #:toggle-debug-mode-command
   #:redraw-screen-command

   ;; Package loader
   #:*packages-directory*
   #:clawmacs-use-package

   ;; User init
   #:*user-init-directory*
   #:*user-init-file*
   #:*inhibit-user-init*
   #:*startup-hook*
   #:*initial-buffer-hook*
   #:add-hook
   #:remove-hook
   #:load-user-init-file

   ;; UI Backend Protocol
   #:ui-backend
   #:*ui-backend*
   #:backend-run
   #:croatoan-backend
   #:mcclim-backend

   ;; Croatoan color support
   #:*terminal-color-count*

   ;; McCLIM presentation types
   #:chat-message
   #:tool-call
   #:buffer-ref
   #:model-ref

   ;; Popup GUI
   #:*popup-frames*
   #:spawn-mcclim-popup
   #:cleanup-popup-frames
   #:close-all-popup-frames
   #:popup-gui-command

   ;; Main
   #:clawmacs-main
   #:clawmacs-prompt-main
   #:run-single-prompt
   #:*prompt-max-tool-iterations*
   #:prompt-run-result
   #:prompt-run-result-prompt
   #:prompt-run-result-final-text
   #:prompt-run-result-tool-events
   #:prompt-run-result-reasoning-blocks
   #:prompt-run-result-agent-name
   #:prompt-run-result-provider
   #:prompt-run-result-model
   #:prompt-run-result-think-level
   #:prompt-run-result-iterations
   #:prompt-run-result-stop-reason
   #:prompt-run-error
   #:prompt-run-error-message
   #:prompt-run-error-tool-events
   #:prompt-run-error-iterations
   #:prompt-run-error-provider
   #:prompt-run-error-model
   #:prompt-run-error-think-level
   #:prompt-tool-event
   #:prompt-tool-event-id
   #:prompt-tool-event-name
   #:prompt-tool-event-input
   #:prompt-tool-event-result-text
   #:prompt-tool-event-display
   #:prompt-tool-event-denied-p
   #:send-to-agent-with-context
   #:switch-buffer-to-agent
    #:scroll-up-command
    #:scroll-down-command
    #:*scroll-page-size*))
