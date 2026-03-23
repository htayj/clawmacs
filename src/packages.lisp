(defpackage :clawmacs
  (:use :cl)
  (:export
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
   #:buffer
   #:make-buffer
   #:buffer-name
   #:buffer-first-message
   #:buffer-last-message
   #:buffer-input-message
   #:buffer-agent-name
   #:buffer-working-directory
    #:buffer-token-count
    #:buffer-context-limit
    #:buffer-status
    #:buffer-provider-override
    #:buffer-model-override
    #:set-buffer-provider-override
    #:set-buffer-model-override
    #:clear-buffer-provider-override
    #:clear-buffer-model-override
    #:clear-buffer-provider/model-overrides
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
   #:defcommand
   #:check-permission
   #:permission-denied
   #:permission-required
   #:list-available-commands

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

    ;; LLM
    #:*claude-code-credentials-path*
    #:*anthropic-env-var*
    #:*zai-env-var*
    #:read-env-token
    #:read-claude-code-oauth-token
    #:provider-token-path
    #:read-provider-token
     #:save-provider-token
     #:read-token
     #:load-agent-defaults
     #:save-agent-defaults
     #:agent-default
     #:set-agent-default
     #:ensure-agent-defaults-loaded
     #:resolve-buffer-provider-and-model
     #:anthropic-request
     #:build-conversation-messages
    #:*anthropic-model*
   #:api-json-encode
   #:api-json-decode

   ;; Claude CLI Subprocess (stream-json protocol)
   #:*claude-cli-path*
   #:*claude-cli-models*
   #:claude-cli-model-p
   #:claude-cli-build-prompt
   #:claude-cli-build-ndjson-message
   #:claude-cli-spawn-args
   #:claude-cli-next-session-id
   #:claude-cli-request
   #:claude-cli-request-streaming

   ;; Z.AI (Zhipu AI)
   #:*zai-model*
   #:*zai-api-url*
   #:zai-request
   #:zai-request-streaming

   ;; OpenAI Codex OAuth
   #:*openai-oauth-client-id*
   #:*openai-oauth-auth-url*
   #:*openai-oauth-token-url*
   #:*openai-oauth-redirect-uri*
   #:*openai-oauth-scopes*
   #:*openai-codex-oauth-path*
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
   #:*tool-table*
   #:tool-definition
   #:register-tool
   #:execute-tool
   #:tool-requires-permission-p
   #:tool-definitions-for-api
   #:format-tool-call-sexpr
   #:format-tool-call-expanded
   #:tool-approval-extra-display
   #:init-tools

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
   #:render-buffer-selector

   ;; Model selector (overlay)
   #:*model-selector-active*
   #:*model-selector-index*
   #:*model-selector-entries*
   #:select-model-command
   #:render-model-selector
   #:handle-model-selector-key

   ;; Minibuffer
   #:*minibuffer-active*
   #:*minibuffer-prompt*
   #:*minibuffer-input*
   #:*minibuffer-point*
   #:*minibuffer-items*
   #:*minibuffer-filtered-items*
   #:*minibuffer-selected-index*
   #:*minibuffer-scroll-offset*
   #:*minibuffer-callback*
   #:*minibuffer-max-height*
   #:*model-selection-history*
   #:fuzzy-match-p
   #:minibuffer-item-display
   #:minibuffer-activate
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
   #:minibuffer-select-model-command
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

   ;; Main
   #:clawmacs-main
   #:send-to-agent-with-context
    #:scroll-up-command
    #:scroll-down-command
    #:*scroll-page-size*))
