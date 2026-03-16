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
   #:render-history
   #:render-modeline
   #:render-input
   #:render-buffer

   ;; Main
   #:clawmacs-main
   #:send-to-agent-with-context
    #:scroll-up-command
    #:scroll-down-command
    #:*scroll-page-size*))
