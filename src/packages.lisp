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
   #:message-text
   #:message-line-count

   ;; Message editing
   #:message-insert-char
   #:message-insert-newline
   #:message-delete-char-backward
   #:message-move-beginning-of-line
   #:message-move-end-of-line
   #:message-kill-line
   #:message-yank

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
   #:buffer-face-registry
   #:buffer-keymap
   #:buffer-scroll-offset
   #:buffer-finalize-input
   #:buffer-insert-agent-message
   #:buffer-message-count

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
   #:read-claude-credentials
   #:anthropic-chat
   #:build-conversation-messages
   #:*anthropic-model*

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
