(in-package :clawmacs)

;;; =========================================================================
;;; Extended Documentation Entries
;;;
;;; This file provides extended documentation for all exported symbols using
;;; the defdoc macro. Each entry may include:
;;;   :category     — grouping for organized browsing
;;;   :usage        — parameter types and example call
;;;   :returns      — return type and example value
;;;   :see-also     — list of related symbols
;;;   :side-effects — description of mutations, I/O, or global state changes
;;; =========================================================================

;;; --------------------------------------------------------------------------
;;; Undocumented Symbol Detection
;;; --------------------------------------------------------------------------

(defun undocumented-functions ()
  "Return a sorted list of exported function symbols that lack extended
documentation in *extended-docs*."
  (let ((undocumented nil))
    (dolist (sym (list-functions))
      (unless (gethash sym *extended-docs*)
        (push sym undocumented)))
    (sort undocumented #'string< :key #'symbol-name)))

(defun undocumented-variables ()
  "Return a sorted list of exported variable symbols that lack extended
documentation in *extended-docs*."
  (let ((undocumented nil))
    (dolist (sym (list-variables))
      (unless (gethash sym *extended-docs*)
        (push sym undocumented)))
    (sort undocumented #'string< :key #'symbol-name)))

;;; ==========================================================================
;;; Category: face — Colors, faces, resolved faces, face sets
;;; ==========================================================================

(defdoc color-spec
  :category "face"
  :usage "(make-color-spec :cga 7) to create a CGA white color spec."
  :returns "Structure — A color-spec with color-type and value slots."
  :see-also (make-color-spec color-spec-type color-spec-value face))

(defdoc make-color-spec
  :category "face"
  :usage "(make-color-spec COLOR-TYPE:keyword VALUE) — (make-color-spec :cga 7), (make-color-spec :256 128), (make-color-spec :hex \"#FF00FF\")"
  :returns "color-spec — #S(COLOR-SPEC :COLOR-TYPE :CGA :VALUE 7)"
  :see-also (color-spec color-spec-type color-spec-value))

(defdoc color-spec-type
  :category "face"
  :usage "(color-spec-type CS:color-spec) — (color-spec-type (make-color-spec :cga 7))"
  :returns "keyword — :CGA, :256, or :HEX"
  :see-also (color-spec make-color-spec color-spec-value))

(defdoc color-spec-value
  :category "face"
  :usage "(color-spec-value CS:color-spec) — (color-spec-value (make-color-spec :cga 7))"
  :returns "integer or string — 7 for CGA, 128 for 256-color, \"#FF00FF\" for hex"
  :see-also (color-spec make-color-spec color-spec-type))

(defdoc face
  :category "face"
  :usage "(make-instance 'face :name :default :foreground (make-color-spec :cga 7) :bold-p t)"
  :returns "Class — A face object with visual attributes and optional parent chain."
  :see-also (face-name face-foreground face-background face-bold-p face-parent resolve-face))

(defdoc face-name
  :category "face"
  :usage "(face-name FACE:face) — (face-name my-face)"
  :returns "keyword — :DEFAULT, :MODELINE, etc."
  :see-also (face face-foreground face-background))

(defdoc face-foreground
  :category "face"
  :usage "(face-foreground FACE:face) — (face-foreground my-face)"
  :returns "color-spec or nil — nil means inherit from parent."
  :see-also (face face-background face-bold-p resolve-face))

(defdoc face-background
  :category "face"
  :usage "(face-background FACE:face) — (face-background my-face)"
  :returns "color-spec or nil — nil means inherit from parent."
  :see-also (face face-foreground face-bold-p resolve-face))

(defdoc face-bold-p
  :category "face"
  :usage "(face-bold-p FACE:face) — (face-bold-p my-face)"
  :returns "boolean or nil — nil means inherit from parent."
  :see-also (face face-underline-p face-reverse-p resolve-face))

(defdoc face-underline-p
  :category "face"
  :usage "(face-underline-p FACE:face) — (face-underline-p my-face)"
  :returns "boolean or nil — nil means inherit from parent."
  :see-also (face face-bold-p face-reverse-p resolve-face))

(defdoc face-reverse-p
  :category "face"
  :usage "(face-reverse-p FACE:face) — (face-reverse-p my-face)"
  :returns "boolean or nil — nil means inherit from parent."
  :see-also (face face-bold-p face-underline-p resolve-face))

(defdoc face-parent
  :category "face"
  :usage "(face-parent FACE:face) — (face-parent my-face)"
  :returns "face or nil — The parent face for attribute inheritance."
  :see-also (face resolve-face face-foreground))

(defdoc face-transform
  :category "face"
  :usage "(face-transform FACE:face) — (face-transform my-face)"
  :returns "t or nil — Stub for future color transforms. Currently ignored."
  :see-also (face resolve-face))

(defdoc resolved-face
  :category "face"
  :usage "(resolve-face my-face) to get a resolved-face."
  :returns "Structure — A resolved-face with all attributes fully resolved (no nils)."
  :see-also (resolve-face resolved-face-foreground resolved-face-background))

(defdoc resolved-face-foreground
  :category "face"
  :usage "(resolved-face-foreground RF:resolved-face)"
  :returns "color-spec — Always non-nil."
  :see-also (resolved-face resolved-face-background resolve-face))

(defdoc resolved-face-background
  :category "face"
  :usage "(resolved-face-background RF:resolved-face)"
  :returns "color-spec — Always non-nil."
  :see-also (resolved-face resolved-face-foreground resolve-face))

(defdoc resolved-face-bold-p
  :category "face"
  :usage "(resolved-face-bold-p RF:resolved-face)"
  :returns "boolean — T or NIL (never nil-as-inherit)."
  :see-also (resolved-face resolved-face-underline-p resolve-face))

(defdoc resolved-face-underline-p
  :category "face"
  :usage "(resolved-face-underline-p RF:resolved-face)"
  :returns "boolean — T or NIL."
  :see-also (resolved-face resolved-face-bold-p resolve-face))

(defdoc resolved-face-reverse-p
  :category "face"
  :usage "(resolved-face-reverse-p RF:resolved-face)"
  :returns "boolean — T or NIL."
  :see-also (resolved-face resolved-face-bold-p resolve-face))

(defdoc resolve-face
  :category "face"
  :usage "(resolve-face FACE:face) — (resolve-face my-face)"
  :returns "resolved-face — All attributes filled in from parent chain."
  :see-also (face resolved-face face-parent))

(defdoc face-set
  :category "face"
  :usage "(make-face-set :user (list face1 face2)) to create a face set."
  :returns "Class — A collection of named faces belonging to a sender."
  :see-also (make-face-set face-set-owner face-set-faces get-face))

(defdoc face-set-owner
  :category "face"
  :usage "(face-set-owner FS:face-set) — (face-set-owner my-face-set)"
  :returns "keyword — :USER, :AGENT, etc."
  :see-also (face-set make-face-set face-set-faces))

(defdoc face-set-faces
  :category "face"
  :usage "(face-set-faces FS:face-set) — (face-set-faces my-face-set)"
  :returns "hash-table — Maps face name keywords to face objects."
  :see-also (face-set make-face-set get-face))

(defdoc make-face-set
  :category "face"
  :usage "(make-face-set OWNER:keyword FACE-LIST:list) — (make-face-set :user (list default-face))"
  :returns "face-set — A new face set with faces indexed by name."
  :see-also (face-set get-face face-set-owner))

(defdoc get-face
  :category "face"
  :usage "(get-face FACE-SET:face-set NAME:keyword) — (get-face my-set :default)"
  :returns "face or nil — The face with that name, or nil if not found."
  :see-also (face-set make-face-set face-set-faces))

;;; ==========================================================================
;;; Category: line — Line data structure
;;; ==========================================================================

(defdoc line
  :category "line"
  :usage "(make-line \"Hello world\") to create a line."
  :returns "Class — A line object in a doubly-linked list."
  :see-also (make-line line-content line-next line-prev message))

(defdoc make-line
  :category "line"
  :usage "(make-line &optional CONTENT:string) — (make-line \"Hello\"), (make-line)"
  :returns "line — A new unlinked line with the given content."
  :see-also (line line-content line-next line-prev))

(defdoc line-content
  :category "line"
  :usage "(line-content LINE:line) — (line-content my-line)"
  :returns "string — The text content of this line."
  :see-also (line make-line line-next line-prev))

(defdoc line-next
  :category "line"
  :usage "(line-next LINE:line) — (line-next my-line)"
  :returns "line or nil — Next line in the linked list, or nil if last."
  :see-also (line line-prev line-content))

(defdoc line-prev
  :category "line"
  :usage "(line-prev LINE:line) — (line-prev my-line)"
  :returns "line or nil — Previous line in the linked list, or nil if first."
  :see-also (line line-next line-content))

;;; ==========================================================================
;;; Category: message — Message structure and accessors
;;; ==========================================================================

(defdoc message
  :category "message"
  :usage "(make-message :user) to create a user message."
  :returns "Class — A message containing a doubly-linked list of lines."
  :see-also (make-message message-text message-sender message-first-line buffer))

(defdoc make-message
  :category "message"
  :usage "(make-message SENDER:keyword &key FACE-SET READ-ONLY-P) — (make-message :user), (make-message :agent :read-only-p t)"
  :returns "message — A new message with a single empty line."
  :see-also (message message-text message-sender message-first-line))

(defdoc message-first-line
  :category "message"
  :usage "(message-first-line MSG:message) — (message-first-line my-msg)"
  :returns "line — First line in the message's doubly-linked list."
  :see-also (message message-last-line message-text line))

(defdoc message-last-line
  :category "message"
  :usage "(message-last-line MSG:message) — (message-last-line my-msg)"
  :returns "line — Last line in the message's doubly-linked list."
  :see-also (message message-first-line message-text line))

(defdoc message-point-line
  :category "message"
  :usage "(message-point-line MSG:message) — (message-point-line my-msg)"
  :returns "line — The line containing the editing cursor (point)."
  :see-also (message message-point-offset message-first-line))

(defdoc message-point-offset
  :category "message"
  :usage "(message-point-offset MSG:message) — (message-point-offset my-msg)"
  :returns "fixnum — Character offset of the cursor within the point line."
  :see-also (message message-point-line message-forward-char message-backward-char))

(defdoc message-mark-line
  :category "message"
  :usage "(message-mark-line MSG:message) — (message-mark-line my-msg)"
  :returns "line or nil — The line containing the mark for selection, or nil."
  :see-also (message message-mark-offset message-point-line))

(defdoc message-mark-offset
  :category "message"
  :usage "(message-mark-offset MSG:message) — (message-mark-offset my-msg)"
  :returns "fixnum or nil — Character offset of the mark, or nil."
  :see-also (message message-mark-line message-point-offset))

(defdoc message-sender
  :category "message"
  :usage "(message-sender MSG:message) — (message-sender my-msg)"
  :returns "keyword — :USER, :AGENT, :SYSTEM, :TOOL-RESULT, etc."
  :see-also (message make-message message-text))

(defdoc message-timestamp
  :category "message"
  :usage "(message-timestamp MSG:message) — (message-timestamp my-msg)"
  :returns "integer or nil — Universal time when finalized, or nil if not yet."
  :see-also (message message-sender buffer-finalize-input))

(defdoc message-face-set
  :category "message"
  :usage "(message-face-set MSG:message) — (message-face-set my-msg)"
  :returns "face-set or nil — The face set used to render this message."
  :see-also (message face-set make-face-set))

(defdoc message-read-only-p
  :category "message"
  :usage "(message-read-only-p MSG:message) — (message-read-only-p my-msg)"
  :returns "boolean — T if the message cannot be edited."
  :see-also (message buffer-finalize-input buffer-insert-agent-message))

(defdoc message-next
  :category "message"
  :usage "(message-next MSG:message) — (message-next my-msg)"
  :returns "message or nil — Next message in the buffer, or nil if last."
  :see-also (message message-prev buffer-first-message))

(defdoc message-prev
  :category "message"
  :usage "(message-prev MSG:message) — (message-prev my-msg)"
  :returns "message or nil — Previous message in the buffer, or nil if first."
  :see-also (message message-next buffer-last-message))

(defdoc message-raw-content
  :category "message"
  :usage "(message-raw-content MSG:message) — (message-raw-content my-msg)"
  :returns "list or nil — API-format content blocks for round-tripping tool_use/tool_result."
  :see-also (message message-text build-conversation-messages))

(defdoc message-text
  :category "message"
  :usage "(message-text MSG:message) — (message-text my-msg)"
  :returns "string — Full text with newlines between lines. \"Hello\\nworld\""
  :see-also (message message-first-line message-line-count))

(defdoc message-line-count
  :category "message"
  :usage "(message-line-count MSG:message) — (message-line-count my-msg)"
  :returns "fixnum — Number of lines. (message-line-count (make-message :user)) => 1"
  :see-also (message message-text message-first-line))

;;; ==========================================================================
;;; Category: message-editing — Message editing operations
;;; ==========================================================================

(defdoc message-insert-char
  :category "message-editing"
  :usage "(message-insert-char MSG:message CHAR:character) — (message-insert-char msg #\\a)"
  :returns "message — The modified message."
  :side-effects "Mutates the point line's content and advances point by 1."
  :see-also (message-insert-newline message-delete-char-backward self-insert-command))

(defdoc message-insert-newline
  :category "message-editing"
  :usage "(message-insert-newline MSG:message) — (message-insert-newline msg)"
  :returns "message — The modified message."
  :side-effects "Splits the current line into two at point. Updates the line linked list."
  :see-also (message-insert-char insert-newline-command message-kill-line))

(defdoc message-delete-char-backward
  :category "message-editing"
  :usage "(message-delete-char-backward MSG:message) — (message-delete-char-backward msg)"
  :returns "message — The modified message."
  :side-effects "Deletes the character before point. Joins lines if at start."
  :see-also (message-delete-char-forward delete-char-backward-command message-insert-char))

(defdoc message-delete-char-forward
  :category "message-editing"
  :usage "(message-delete-char-forward MSG:message) — (message-delete-char-forward msg)"
  :returns "message — The modified message."
  :side-effects "Deletes the character after point. Joins lines if at end."
  :see-also (message-delete-char-backward delete-char-forward-command))

(defdoc message-forward-char
  :category "message-editing"
  :usage "(message-forward-char MSG:message) — (message-forward-char msg)"
  :returns "message — The modified message."
  :side-effects "Moves point one character forward. Wraps to next line at end."
  :see-also (message-backward-char message-forward-word forward-char-command))

(defdoc message-backward-char
  :category "message-editing"
  :usage "(message-backward-char MSG:message) — (message-backward-char msg)"
  :returns "message — The modified message."
  :side-effects "Moves point one character backward. Wraps to previous line at start."
  :see-also (message-forward-char message-backward-word backward-char-command))

(defdoc message-forward-word
  :category "message-editing"
  :usage "(message-forward-word MSG:message) — (message-forward-word msg)"
  :returns "message — The modified message."
  :side-effects "Moves point forward to end of next word."
  :see-also (message-backward-word message-forward-char forward-word-command))

(defdoc message-backward-word
  :category "message-editing"
  :usage "(message-backward-word MSG:message) — (message-backward-word msg)"
  :returns "message — The modified message."
  :side-effects "Moves point backward to beginning of previous word."
  :see-also (message-forward-word message-backward-char backward-word-command))

(defdoc message-move-beginning-of-line
  :category "message-editing"
  :usage "(message-move-beginning-of-line MSG:message) — (message-move-beginning-of-line msg)"
  :returns "message — The modified message."
  :side-effects "Sets point offset to 0."
  :see-also (message-move-end-of-line beginning-of-line-command))

(defdoc message-move-end-of-line
  :category "message-editing"
  :usage "(message-move-end-of-line MSG:message) — (message-move-end-of-line msg)"
  :returns "message — The modified message."
  :side-effects "Sets point offset to end of current line."
  :see-also (message-move-beginning-of-line end-of-line-command))

(defdoc message-kill-line
  :category "message-editing"
  :usage "(message-kill-line MSG:message) — (message-kill-line msg)"
  :returns "message — The modified message."
  :side-effects "Kills from point to end of line. Pushes killed text to kill ring."
  :see-also (message-kill-backward-line message-kill-word kill-line-command kill-ring-push))

(defdoc message-kill-backward-line
  :category "message-editing"
  :usage "(message-kill-backward-line MSG:message) — (message-kill-backward-line msg)"
  :returns "message — The modified message."
  :side-effects "Kills from start of line to point. Pushes killed text to kill ring."
  :see-also (message-kill-line kill-backward-line-command kill-ring-push))

(defdoc message-kill-word
  :category "message-editing"
  :usage "(message-kill-word MSG:message) — (message-kill-word msg)"
  :returns "message — The modified message."
  :side-effects "Kills from point to end of current word. Pushes to kill ring."
  :see-also (message-backward-kill-word kill-word-command kill-ring-push))

(defdoc message-backward-kill-word
  :category "message-editing"
  :usage "(message-backward-kill-word MSG:message) — (message-backward-kill-word msg)"
  :returns "message — The modified message."
  :side-effects "Kills from beginning of current word to point. Pushes to kill ring."
  :see-also (message-kill-word backward-kill-word-command kill-ring-push))

(defdoc message-yank
  :category "message-editing"
  :usage "(message-yank MSG:message) — (message-yank msg)"
  :returns "message — The modified message."
  :side-effects "Inserts the top of the kill ring at point."
  :see-also (message-yank-pop yank-command kill-ring-top))

(defdoc message-yank-pop
  :category "message-editing"
  :usage "(message-yank-pop MSG:message) — (message-yank-pop msg)"
  :returns "message — The modified message."
  :side-effects "Replaces the just-yanked text with the next kill ring entry."
  :see-also (message-yank yank-pop-command *kill-ring*))

;;; ==========================================================================
;;; Category: kill-ring — Kill ring operations
;;; ==========================================================================

(defdoc *kill-ring*
  :category "kill-ring"
  :see-also (kill-ring-push kill-ring-top message-kill-line))

(defdoc kill-ring-push
  :category "kill-ring"
  :usage "(kill-ring-push STRING:string) — (kill-ring-push \"deleted text\")"
  :returns "string — The pushed string."
  :side-effects "Pushes STRING onto *kill-ring*. Trims ring to *kill-ring-max* entries."
  :see-also (*kill-ring* kill-ring-top message-kill-line))

(defdoc kill-ring-top
  :category "kill-ring"
  :usage "(kill-ring-top) — (kill-ring-top)"
  :returns "string or nil — Most recent kill ring entry, or nil if empty."
  :see-also (*kill-ring* kill-ring-push message-yank))

;;; ==========================================================================
;;; Category: buffer — Buffer structure, ring, and operations
;;; ==========================================================================

(defdoc *default-context-limit*
  :category "buffer"
  :see-also (make-buffer buffer-context-limit))

(defdoc *default-agent-name*
  :category "buffer"
  :see-also (make-buffer buffer-agent-name load-session))

(defdoc *default-show-tool-results*
  :category "buffer"
  :see-also (buffer-show-tool-results-p))

(defdoc *scratch-buffer-name*
  :category "buffer"
  :see-also (ensure-scratch-buffer scratch-buffer))

(defdoc *scratch-buffer-initial-text*
  :category "buffer"
  :see-also (ensure-scratch-buffer scratch-buffer-text))

(defdoc buffer
  :category "buffer"
  :usage "(make-buffer \"session-01\" :agent-name \"agent\") to create a buffer."
  :returns "Class — A buffer with a doubly-linked list of messages."
  :see-also (make-buffer buffer-name buffer-input-message *buffer-ring*))

(defdoc make-buffer
  :category "buffer"
  :usage "(make-buffer NAME:string &key AGENT-NAME:string KIND:keyword WORKING-DIRECTORY:pathname CONTEXT-LIMIT:integer) — (make-buffer \"session-01\" :agent-name \"agent\")"
  :returns "buffer — A new buffer with a single empty input message."
  :side-effects "Allocates a buffer with an empty face registry."
  :see-also (buffer buffer-name buffer-kind add-buffer-to-ring current-buffer))

(defdoc buffer-name
  :category "buffer"
  :usage "(buffer-name BUF:buffer) — (buffer-name (current-buffer))"
  :returns "string — \"session-01\", \"*help:make-buffer*\", etc."
  :see-also (buffer make-buffer find-buffer-by-name buffer-names))

(defdoc buffer-first-message
  :category "buffer"
  :usage "(buffer-first-message BUF:buffer) — (buffer-first-message (current-buffer))"
  :returns "message — First message in the buffer's linked list."
  :see-also (buffer buffer-last-message buffer-input-message message))

(defdoc buffer-last-message
  :category "buffer"
  :usage "(buffer-last-message BUF:buffer) — (buffer-last-message (current-buffer))"
  :returns "message — Last message (always the input message)."
  :see-also (buffer buffer-first-message buffer-input-message))

(defdoc buffer-input-message
  :category "buffer"
  :usage "(buffer-input-message BUF:buffer) — (buffer-input-message (current-buffer))"
  :returns "message — The current editable input message (not read-only)."
  :see-also (buffer buffer-last-message buffer-finalize-input))

(defdoc buffer-agent-name
  :category "buffer"
  :usage "(buffer-agent-name BUF:buffer) — (buffer-agent-name (current-buffer))"
  :returns "string — \"agent\", \"help\", etc."
  :see-also (buffer make-buffer buffer-name))

(defdoc buffer-kind
  :category "buffer"
  :usage "(buffer-kind BUF:buffer) — (buffer-kind (current-buffer))"
  :returns "keyword — :CHAT, :SCRATCH, or another custom buffer kind."
  :see-also (buffer make-buffer scratch-buffer-p))

(defdoc buffer-working-directory
  :category "buffer"
  :usage "(buffer-working-directory BUF:buffer) — (buffer-working-directory (current-buffer))"
  :returns "pathname — The working directory for shell commands."
  :see-also (buffer make-buffer shell-prefix-handler))

(defdoc buffer-token-count
  :category "buffer"
  :usage "(buffer-token-count BUF:buffer) — (buffer-token-count (current-buffer))"
  :returns "integer — Running count of tokens used in this buffer's conversation."
  :see-also (buffer buffer-context-limit))

(defdoc buffer-context-limit
  :category "buffer"
  :usage "(buffer-context-limit BUF:buffer) — (buffer-context-limit (current-buffer))"
  :returns "integer — Maximum token context window size (default 200000)."
  :see-also (buffer buffer-token-count))

(defdoc buffer-status
  :category "buffer"
  :usage "(buffer-status BUF:buffer) — (buffer-status (current-buffer))"
  :returns "keyword — :IDLE, :THINKING, :STREAMING, :ERROR, :APPROVAL, or :OAUTH."
  :see-also (buffer send-to-agent-with-context))

(defdoc buffer-provider-override
  :category "buffer"
  :usage "(buffer-provider-override BUF:buffer) — (buffer-provider-override (current-buffer))"
  :returns "keyword or nil — :OPENAI-CODEX, :ZAI, :OPENROUTER, or nil."
  :see-also (set-buffer-provider-override clear-buffer-provider-override buffer-model-override))

(defdoc buffer-model-override
  :category "buffer"
  :usage "(buffer-model-override BUF:buffer) — (buffer-model-override (current-buffer))"
  :returns "string or nil — \"glm-5\" or nil to use default."
  :see-also (set-buffer-model-override clear-buffer-model-override buffer-provider-override))

(defdoc buffer-think-level-override
  :category "buffer"
  :usage "(buffer-think-level-override BUF:buffer) — (buffer-think-level-override (current-buffer))"
  :returns "string or nil — \"high\" or nil to use the model default."
  :see-also (set-buffer-think-level-override clear-buffer-think-level-override buffer-model-override))

(defdoc set-buffer-provider-override
  :category "buffer"
  :usage "(set-buffer-provider-override BUF:buffer PROVIDER:keyword) — (set-buffer-provider-override buf :zai)"
  :returns "buffer — The modified buffer."
  :side-effects "Sets the buffer's provider override."
  :see-also (buffer-provider-override clear-buffer-provider-override set-buffer-model-override))

(defdoc set-buffer-model-override
  :category "buffer"
  :usage "(set-buffer-model-override BUF:buffer MODEL:string) — (set-buffer-model-override buf \"glm-5\")"
  :returns "buffer — The modified buffer."
  :side-effects "Sets the buffer's model override."
  :see-also (buffer-model-override clear-buffer-model-override set-buffer-provider-override))

(defdoc set-buffer-think-level-override
  :category "buffer"
  :usage "(set-buffer-think-level-override BUF:buffer THINK-LEVEL:string) — (set-buffer-think-level-override buf \"high\")"
  :returns "buffer — The modified buffer."
  :side-effects "Sets the buffer's think-level override."
  :see-also (buffer-think-level-override clear-buffer-think-level-override))

(defdoc clear-buffer-provider-override
  :category "buffer"
  :usage "(clear-buffer-provider-override BUF:buffer) — (clear-buffer-provider-override buf)"
  :returns "buffer — The modified buffer."
  :side-effects "Clears the buffer's provider override to nil."
  :see-also (set-buffer-provider-override buffer-provider-override clear-buffer-routing-overrides))

(defdoc clear-buffer-model-override
  :category "buffer"
  :usage "(clear-buffer-model-override BUF:buffer) — (clear-buffer-model-override buf)"
  :returns "buffer — The modified buffer."
  :side-effects "Clears the buffer's model override to nil."
  :see-also (set-buffer-model-override buffer-model-override clear-buffer-routing-overrides))

(defdoc clear-buffer-think-level-override
  :category "buffer"
  :usage "(clear-buffer-think-level-override BUF:buffer) — (clear-buffer-think-level-override buf)"
  :returns "buffer — The modified buffer."
  :side-effects "Clears the buffer's think-level override to nil."
  :see-also (set-buffer-think-level-override buffer-think-level-override clear-buffer-routing-overrides))

(defdoc clear-buffer-routing-overrides
  :category "buffer"
  :usage "(clear-buffer-routing-overrides BUF:buffer) — (clear-buffer-routing-overrides buf)"
  :returns "buffer — The modified buffer."
  :side-effects "Clears provider, model, and think-level overrides."
  :see-also (clear-buffer-provider-override clear-buffer-model-override clear-buffer-think-level-override))

(defdoc buffer-face-registry
  :category "buffer"
  :usage "(buffer-face-registry BUF:buffer) — (buffer-face-registry (current-buffer))"
  :returns "hash-table — Maps sender keywords to face-set objects."
  :see-also (buffer face-set init-face-registry))

(defdoc buffer-keymap
  :category "buffer"
  :usage "(buffer-keymap BUF:buffer) — (buffer-keymap (current-buffer))"
  :returns "keymap or nil — The keymap for this buffer."
  :see-also (buffer keymap *default-keymap*))

(defdoc buffer-scroll-offset
  :category "buffer"
  :usage "(buffer-scroll-offset BUF:buffer) — (buffer-scroll-offset (current-buffer))"
  :returns "integer — Number of rows scrolled up. 0 means auto-scroll."
  :see-also (buffer scroll-up-command scroll-down-command))

(defdoc buffer-show-tool-results-p
  :category "buffer"
  :usage "(buffer-show-tool-results-p BUF:buffer) — (buffer-show-tool-results-p (current-buffer))"
  :returns "boolean — T if tool result messages are shown."
  :see-also (buffer toggle-tool-results-command))

(defdoc buffer-pending-stream
  :category "buffer"
  :usage "(buffer-pending-stream BUF:buffer) — (buffer-pending-stream (current-buffer))"
  :returns "stream-state or nil — The active streaming response state."
  :see-also (buffer buffer-streaming-message send-to-agent-with-context))

(defdoc buffer-streaming-message
  :category "buffer"
  :usage "(buffer-streaming-message BUF:buffer) — (buffer-streaming-message (current-buffer))"
  :returns "message or nil — The message being updated by streaming."
  :see-also (buffer buffer-pending-stream))

(defdoc buffer-major-mode
  :category "buffer"
  :usage "(buffer-major-mode BUF:buffer) — (buffer-major-mode (current-buffer))"
  :returns "string — \"chat\", \"scratch\", \"help\", etc."
  :see-also (buffer make-help-buffer render-modeline))

(defdoc *buffer-ring*
  :category "buffer"
  :see-also (current-buffer add-buffer-to-ring switch-to-buffer kill-buffer-from-ring))

(defdoc current-buffer
  :category "buffer"
  :usage "(current-buffer) — (current-buffer)"
  :returns "buffer — The current buffer (first in the ring)."
  :see-also (*buffer-ring* switch-to-buffer add-buffer-to-ring))

(defdoc add-buffer-to-ring
  :category "buffer"
  :usage "(add-buffer-to-ring BUF:buffer) — (add-buffer-to-ring new-buf)"
  :returns "buffer — The added buffer."
  :side-effects "Pushes BUF to the front of *buffer-ring*."
  :see-also (*buffer-ring* current-buffer switch-to-buffer kill-buffer-from-ring))

(defdoc switch-to-buffer
  :category "buffer"
  :usage "(switch-to-buffer BUF:buffer) — (switch-to-buffer other-buf)"
  :returns "buffer — The switched-to buffer."
  :side-effects "Moves BUF to front of *buffer-ring*, making it current."
  :see-also (*buffer-ring* current-buffer add-buffer-to-ring))

(defdoc kill-buffer-from-ring
  :category "buffer"
  :usage "(kill-buffer-from-ring BUF:buffer) — (kill-buffer-from-ring (current-buffer))"
  :returns "buffer or nil — The new current buffer, or nil if ring is now empty."
  :side-effects "Removes BUF from *buffer-ring* unless BUF is the scratch buffer."
  :see-also (*buffer-ring* add-buffer-to-ring kill-buffer-command scratch-buffer-p))

(defdoc find-buffer-by-name
  :category "buffer"
  :usage "(find-buffer-by-name NAME:string) — (find-buffer-by-name \"session-01\")"
  :returns "buffer or nil — The matching buffer, or nil."
  :see-also (buffer-name buffer-names *buffer-ring*))

(defdoc scratch-buffer-p
  :category "buffer"
  :usage "(scratch-buffer-p BUF:buffer) — (scratch-buffer-p (current-buffer))"
  :returns "boolean — T when BUF is the process-local scratch buffer."
  :see-also (scratch-buffer ensure-scratch-buffer buffer-kind))

(defdoc scratch-buffer
  :category "buffer"
  :usage "(scratch-buffer) — (scratch-buffer)"
  :returns "buffer or nil — The loaded scratch buffer."
  :see-also (ensure-scratch-buffer scratch-buffer-text *scratch-buffer-name*))

(defdoc ensure-scratch-buffer
  :category "buffer"
  :usage "(ensure-scratch-buffer) — (ensure-scratch-buffer)"
  :returns "buffer — The process-local scratch buffer."
  :side-effects "Creates and loads the scratch buffer when absent; preserves the current buffer."
  :see-also (scratch-buffer scratch-buffer-p *scratch-buffer-initial-text*))

(defdoc scratch-buffer-text
  :category "buffer"
  :usage "(scratch-buffer-text &optional BUF) — (setf (scratch-buffer-text) \"notes\")"
  :returns "string or nil — The editable scratch document text."
  :side-effects "SETF replaces the scratch buffer's editable text."
  :see-also (scratch-buffer ensure-scratch-buffer buffer-input-message))

(defdoc next-buffer-name
  :category "buffer"
  :usage "(next-buffer-name) — (next-buffer-name)"
  :returns "string — \"session-1\", \"session-2\", etc."
  :side-effects "Increments the global buffer counter."
  :see-also (make-buffer buffer-name new-buffer-command))

(defdoc buffer-names
  :category "buffer"
  :usage "(buffer-names) — (buffer-names)"
  :returns "list of strings — (\"session-01\" \"session-02\" \"*help:make-buffer*\")"
  :see-also (*buffer-ring* buffer-name find-buffer-by-name))

(defdoc buffer-finalize-input
  :category "buffer"
  :usage "(buffer-finalize-input BUF:buffer) — (buffer-finalize-input (current-buffer))"
  :returns "buffer — The modified buffer."
  :side-effects "Makes input message read-only, timestamps it, creates new empty input."
  :see-also (buffer buffer-input-message send-message))

(defdoc buffer-insert-agent-message
  :category "buffer"
  :usage "(buffer-insert-agent-message BUF:buffer TEXT:string) — (buffer-insert-agent-message buf \"Hello!\")"
  :returns "message — The newly inserted agent message."
  :side-effects "Creates a read-only message and inserts it before the input message."
  :see-also (buffer-insert-system-message buffer-finalize-input buffer-input-message))

(defdoc buffer-insert-system-message
  :category "buffer"
  :usage "(buffer-insert-system-message BUF:buffer TEXT:string) — (buffer-insert-system-message buf \"[Session saved]\")"
  :returns "message — The newly inserted system message."
  :side-effects "Creates a read-only :system message. Excluded from API conversation history."
  :see-also (buffer-insert-agent-message build-conversation-messages))

(defdoc buffer-message-count
  :category "buffer"
  :usage "(buffer-message-count BUF:buffer) — (buffer-message-count (current-buffer))"
  :returns "fixnum — Total number of messages including the input message."
  :see-also (buffer buffer-first-message message-next))

;;; ==========================================================================
;;; Category: session — Session persistence
;;; ==========================================================================

(defdoc *sessions-dir*
  :category "session"
  :see-also (save-session load-session list-saved-sessions))

(defdoc save-session
  :category "session"
  :usage "(save-session BUF:buffer) — (save-session (current-buffer))"
  :returns "pathname — Path to the saved session file."
  :side-effects "Writes buffer's conversation to a JSON file in *sessions-dir*."
  :see-also (load-session list-saved-sessions save-session-command *sessions-dir*))

(defdoc load-session
  :category "session"
  :usage "(load-session SESSION-NAME:string &key AGENT-NAME:string) — (load-session \"session-01\")"
  :returns "buffer or nil — The loaded buffer, or nil if session file not found."
  :side-effects "Reads a JSON session file and creates a new buffer with replayed messages."
  :see-also (save-session list-saved-sessions *sessions-dir*))

(defdoc list-saved-sessions
  :category "session"
  :usage "(list-saved-sessions) — (list-saved-sessions)"
  :returns "list of strings — (\"session-01\" \"session-02\") or nil."
  :see-also (save-session load-session *sessions-dir*))

;;; ==========================================================================
;;; Category: command — Command infrastructure and permissions
;;; ==========================================================================

(defdoc *current-caller*
  :category "command"
  :see-also (check-permission defcommand *command-table*))

(defdoc *sandbox-root*
  :category "command"
  :see-also (check-permission defcommand))

(defdoc *command-table*
  :category "command"
  :see-also (defcommand command-metadata list-available-commands check-permission))

(defdoc command-metadata
  :category "command"
  :usage "Created automatically by defcommand."
  :returns "Structure — Holds name, permission, docstring, keybindings, lambda list, and interactive metadata for a command."
  :see-also (defcommand *command-table* command-metadata-name command-metadata-permission))

(defdoc command-metadata-name
  :category "command"
  :usage "(command-metadata-name META:command-metadata)"
  :returns "symbol — The command's name."
  :see-also (command-metadata command-metadata-permission))

(defdoc command-metadata-permission
  :category "command"
  :usage "(command-metadata-permission META:command-metadata)"
  :returns "keyword — :USER-ONLY, :AGENT-ALLOWED, or :AGENT-WITH-PERMISSION."
  :see-also (command-metadata command-metadata-name check-permission))

(defdoc command-metadata-docstring
  :category "command"
  :usage "(command-metadata-docstring META:command-metadata)"
  :returns "string — The command's documentation string."
  :see-also (command-metadata command-metadata-name))

(defdoc command-metadata-keybindings
  :category "command"
  :usage "(command-metadata-keybindings META:command-metadata)"
  :returns "list — Key specifications declared in the defcommand form."
  :see-also (command-metadata find-keybindings-for-command))

(defdoc command-metadata-lambda-list
  :category "command"
  :usage "(command-metadata-lambda-list META:command-metadata)"
  :returns "list — The command's required positional lambda list."
  :see-also (command-required-arguments defcommand))

(defdoc command-metadata-interactive-spec
  :category "command"
  :usage "(command-metadata-interactive-spec META:command-metadata)"
  :returns "T, NIL, or a list of interactive argument specs."
  :see-also (command-interactive-p list-interactive-commands defcommand))

(defdoc defcommand
  :category "command"
  :usage "(defcommand NAME (&key PERMISSION KEYS INTERACTIVE) DOCSTRING (BUFFER &rest REQUIRED-ARGS) &body BODY)"
  :returns "symbol — The command name."
  :side-effects "Registers metadata in *command-table*, defines a generic function, :around method for access control, and primary method. INTERACTIVE controls M-x visibility and minibuffer prompting."
  :see-also (*command-table* command-metadata check-permission list-available-commands list-interactive-commands))

(defdoc check-permission
  :category "command"
  :usage "(check-permission COMMAND-NAME:symbol) — (check-permission 'send-message)"
  :returns "values — No meaningful return value."
  :side-effects "Signals PERMISSION-DENIED or PERMISSION-REQUIRED if access is denied."
  :see-also (*current-caller* permission-denied permission-required defcommand))

(defdoc permission-denied
  :category "command"
  :usage "Signaled automatically by check-permission."
  :returns "Condition — An error condition for denied access."
  :see-also (check-permission permission-required *current-caller*))

(defdoc permission-required
  :category "command"
  :usage "Signaled automatically by check-permission."
  :returns "Condition — An error condition requiring user approval."
  :see-also (check-permission permission-denied *current-caller*))

(defdoc list-available-commands
  :category "command"
  :usage "(list-available-commands) — (list-available-commands)"
  :returns "list of symbols — Commands available to *current-caller*."
  :see-also (*command-table* *current-caller* defcommand))

(defdoc command-required-arguments
  :category "command"
  :usage "(command-required-arguments COMMAND-NAME:symbol)"
  :returns "list — The non-buffer required argument names for the command."
  :see-also (command-metadata-lambda-list command-interactive-p))

(defdoc command-interactive-p
  :category "command"
  :usage "(command-interactive-p COMMAND-NAME:symbol)"
  :returns "T, NIL, or a list of interactive arg specs."
  :see-also (list-interactive-commands command-metadata-interactive-spec))

(defdoc list-interactive-commands
  :category "command"
  :usage "(list-interactive-commands) — (list-interactive-commands)"
  :returns "list of symbols — Commands available to the UI via M-x."
  :see-also (list-available-commands command-interactive-p))

(defdoc resolve-command-interactive-reader
  :category "command"
  :usage "(resolve-command-interactive-reader READER)"
  :returns "function — A callable reader for minibuffer text."
  :see-also (defcommand command-metadata-interactive-spec))

;;; ==========================================================================
;;; Category: docs — Extended documentation system
;;; ==========================================================================

(defdoc *extended-docs*
  :category "docs"
  :see-also (defdoc extended-doc undocumented-functions undocumented-variables))

(defdoc defdoc
  :category "docs"
  :usage "(defdoc SYMBOL :category \"cat\" :usage \"...\" :returns \"...\" :see-also (sym1 sym2) :side-effects \"...\")"
  :returns "plist — The documentation plist stored in *extended-docs*."
  :side-effects "Stores a documentation plist in *extended-docs* keyed by SYMBOL."
  :see-also (*extended-docs* extended-doc undocumented-functions))

(defdoc extended-doc
  :category "docs"
  :usage "(extended-doc SYMBOL &optional KEY) — (extended-doc 'make-buffer :usage)"
  :returns "plist or value — Full doc plist without KEY, specific value with KEY."
  :see-also (*extended-docs* defdoc))

(defdoc undocumented-functions
  :category "docs"
  :usage "(undocumented-functions) — (undocumented-functions)"
  :returns "list of symbols — Exported function symbols without extended docs."
  :see-also (list-functions *extended-docs* undocumented-variables))

(defdoc undocumented-variables
  :category "docs"
  :usage "(undocumented-variables) — (undocumented-variables)"
  :returns "list of symbols — Exported variable symbols without extended docs."
  :see-also (list-variables *extended-docs* undocumented-functions))

;;; ==========================================================================
;;; Category: keymap — Keymap structure and bindings
;;; ==========================================================================

(defdoc keymap
  :category "keymap"
  :usage "(make-keymap \"my-keymap\") to create a keymap."
  :returns "Class — A key-to-command mapping with optional parent chain."
  :see-also (make-keymap keymap-bind keymap-lookup *default-keymap*))

(defdoc make-keymap
  :category "keymap"
  :usage "(make-keymap NAME:string &optional PARENT:keymap) — (make-keymap \"buffer-keymap\" *default-keymap*)"
  :returns "keymap — A new empty keymap."
  :see-also (keymap keymap-bind keymap-lookup))

(defdoc keymap-name
  :category "keymap"
  :usage "(keymap-name KM:keymap) — (keymap-name *default-keymap*)"
  :returns "string — The keymap's name."
  :see-also (keymap make-keymap))

(defdoc keymap-bindings
  :category "keymap"
  :usage "(keymap-bindings KM:keymap) — (keymap-bindings *default-keymap*)"
  :returns "hash-table — Maps key specs to command symbols."
  :see-also (keymap keymap-bind keymap-lookup))

(defdoc keymap-parent
  :category "keymap"
  :usage "(keymap-parent KM:keymap) — (keymap-parent my-keymap)"
  :returns "keymap or nil — Parent keymap for fallback lookups."
  :see-also (keymap make-keymap keymap-lookup))

(defdoc keymap-bind
  :category "keymap"
  :usage "(keymap-bind KM:keymap KEY COMMAND:symbol) — (keymap-bind km #\\Return 'send-message)"
  :returns "symbol — The bound command."
  :side-effects "Stores the key-to-command binding in the keymap's hash table."
  :see-also (keymap keymap-lookup make-keymap))

(defdoc keymap-lookup
  :category "keymap"
  :usage "(keymap-lookup KM:keymap KEY) — (keymap-lookup *default-keymap* #\\Return)"
  :returns "symbol or nil — The command bound to KEY, or nil."
  :see-also (keymap keymap-bind keymap-parent))

(defdoc *default-keymap*
  :category "keymap"
  :see-also (keymap make-keymap keymap-bind keymap-lookup))

;;; ==========================================================================
;;; Category: llm — LLM configuration, authentication, and API
;;; ==========================================================================

(defdoc *default-provider*
  :category "llm"
  :usage "*default-provider* → :zai"
  :returns "keyword — The default LLM provider keyword."
  :see-also (*default-model* *provider-fallback-models* agent-default resolve-buffer-provider-and-model)
  :side-effects "Changing this affects all new buffers that lack an agent-specific provider override.")

(defdoc *default-model*
  :category "llm"
  :usage "*default-model* → \"glm-5\""
  :returns "string — The default model name."
  :see-also (*default-provider* *provider-fallback-models* resolve-buffer-provider-and-model)
  :side-effects "Changing this affects all new buffers that lack both an agent-specific and provider-fallback model.")

(defdoc *default-core-system-prompt*
  :category "llm"
  :usage "String — default clawmacs operating prompt inserted before the personality prompt."
  :see-also (*default-personality-prompt* build-agent-system-prompt)
  :side-effects "Changing this affects future requests for agents that do not provide their own core prompt.")

(defdoc *default-personality-prompt*
  :category "llm"
  :usage "String — default personality prompt inserted after the core system prompt."
  :see-also (*default-core-system-prompt* *personality-prompt-path* load-personality-prompt-file build-agent-system-prompt)
  :side-effects "Changing this affects future requests for agents that do not provide their own personality prompt.")

(defdoc *personality-prompt-path*
  :category "llm"
  :usage "Pathname — defaults to ~/.config/clawmacs/system-prompt.txt"
  :see-also (*default-personality-prompt* load-personality-prompt-file)
  :side-effects "clawmacs-main loads this file once during startup before init.lisp; init.lisp may then change the path and call load-personality-prompt-file again.")

(defdoc load-personality-prompt-file
  :category "llm"
  :usage "(load-personality-prompt-file &optional PATH) — (load-personality-prompt-file #P\"~/my-prompt.txt\")"
  :returns "string or nil — The trimmed prompt text, or NIL when PATH is missing."
  :side-effects "Reads PATH and stores its contents into *default-personality-prompt*."
  :see-also (*default-personality-prompt* *personality-prompt-path* build-agent-system-prompt))

(defdoc *boot-file-names*
  :category "llm"
  :usage "List of strings — boot markdown files loaded ahead of the core and personality prompts."
  :see-also (load-boot-files build-agent-system-prompt)
  :side-effects "Changing this affects which AGENTS.md/SOUL.md-style files are consulted for future prompt builds.")

(defdoc load-boot-files
  :category "llm"
  :usage "(load-boot-files)"
  :returns "string or nil — Concatenated boot-file content, or NIL when none are present."
  :see-also (*boot-file-names* build-agent-system-prompt))

(defdoc *agent-defaults-path*
  :category "llm"
  :usage "Pathname — defaults to ~/.config/clawmacs/agent-defaults.json"
  :see-also (load-agent-defaults save-agent-defaults))

(defdoc *default-max-tokens*
  :category "llm"
  :see-also (*default-model* *default-provider* provider-request))

(defdoc *zai-env-var*
  :category "llm"
  :see-also (read-env-token read-provider-token))

(defdoc *zai-model*
  :category "llm"
  :see-also (*zai-api-url* zai-request))

(defdoc *zai-api-url*
  :category "llm"
  :see-also (*zai-model* zai-request))

(defdoc *openai-oauth-client-id*
  :category "llm"
  :see-also (openai-codex-oauth-start exchange-openai-oauth-code))

(defdoc *openai-oauth-auth-url*
  :category "llm"
  :see-also (openai-codex-oauth-start *openai-oauth-client-id*))

(defdoc *openai-oauth-token-url*
  :category "llm"
  :see-also (exchange-openai-oauth-code refresh-openai-codex-oauth-token))

(defdoc *openai-oauth-redirect-uri*
  :category "llm"
  :see-also (openai-codex-oauth-start extract-oauth-callback-params))

(defdoc *openai-oauth-scopes*
  :category "llm"
  :see-also (openai-codex-oauth-start *openai-oauth-client-id*))

(defdoc *codex-auth-path*
  :category "llm"
  :see-also (read-openai-codex-oauth-tokens save-openai-codex-oauth-tokens))

(defdoc *openai-oauth-pending*
  :category "llm"
  :see-also (openai-codex-oauth-command openai-codex-oauth-start openai-codex-oauth-finish))

(defdoc *provider-known-models*
  :category "llm"
  :see-also (provider-known-models provider-has-token-p available-models-for-selector))

(defdoc read-env-token
  :category "llm"
  :usage "(read-env-token ENV-VAR:string) — (read-env-token \"ZAI_CODING_MAX_API_KEY\")"
  :returns "string or nil — Trimmed token value, or nil if unset/empty."
  :see-also (read-provider-token *zai-env-var* *openrouter-env-var*))

(defdoc provider-token-path
  :category "llm"
  :usage "(provider-token-path PROVIDER:keyword) — (provider-token-path :zai)"
  :returns "pathname — Provider-specific token file under ~/.config/clawmacs/."
  :see-also (read-provider-token save-provider-token))

(defdoc read-provider-token
  :category "llm"
  :usage "(read-provider-token PROVIDER:keyword) — (read-provider-token :zai)"
  :returns "string or nil — The token with highest priority source, or nil."
  :see-also (save-provider-token read-env-token provider-token-path))

(defdoc save-provider-token
  :category "llm"
  :usage "(save-provider-token PROVIDER:keyword TOKEN:string) — (save-provider-token :zai \"sk-...\")"
  :returns "string — The saved token."
  :side-effects "Writes TOKEN to the provider's token file. Sets file permissions to 600."
  :see-also (read-provider-token provider-token-path))

(defdoc generate-code-verifier
  :category "llm"
  :usage "(generate-code-verifier) — (generate-code-verifier)"
  :returns "string — A 43-character PKCE code verifier (RFC 7636)."
  :see-also (compute-code-challenge openai-codex-oauth-start))

(defdoc compute-code-challenge
  :category "llm"
  :usage "(compute-code-challenge CODE-VERIFIER:string) — (compute-code-challenge (generate-code-verifier))"
  :returns "string — S256 PKCE code challenge."
  :side-effects "Invokes openssl via subprocess for SHA-256 + base64url encoding."
  :see-also (generate-code-verifier openai-codex-oauth-start))

(defdoc generate-oauth-state
  :category "llm"
  :usage "(generate-oauth-state) — (generate-oauth-state)"
  :returns "string — A base64url random state token for CSRF protection."
  :see-also (openai-codex-oauth-start extract-oauth-callback-params))

(defdoc url-encode-param
  :category "llm"
  :usage "(url-encode-param STRING:string) — (url-encode-param \"hello world\")"
  :returns "string — \"hello%20world\""
  :see-also (openai-codex-oauth-start exchange-openai-oauth-code))

(defdoc extract-oauth-callback-params
  :category "llm"
  :usage "(extract-oauth-callback-params URL:string) — (extract-oauth-callback-params \"http://localhost:1455/auth/callback?code=abc&state=xyz\")"
  :returns "values code:string state:string — The authorization code and state parameter."
  :see-also (openai-codex-oauth-finish openai-codex-oauth-start))

(defdoc openai-codex-oauth-start
  :category "llm"
  :usage "(openai-codex-oauth-start) — (openai-codex-oauth-start)"
  :returns "values auth-url:string code-verifier:string state:string"
  :see-also (openai-codex-oauth-finish exchange-openai-oauth-code openai-codex-oauth-command))

(defdoc exchange-openai-oauth-code
  :category "llm"
  :usage "(exchange-openai-oauth-code CODE:string CODE-VERIFIER:string)"
  :returns "plist — (:id-token ... :access-token ... :refresh-token ... :account-id ...)"
  :side-effects "Makes an HTTP POST to the OAuth token endpoint."
  :see-also (openai-codex-oauth-start openai-codex-oauth-finish save-openai-codex-oauth-tokens))

(defdoc save-openai-codex-oauth-tokens
  :category "llm"
  :usage "(save-openai-codex-oauth-tokens ACCESS-TOKEN REFRESH-TOKEN EXPIRES-IN)"
  :returns "string — The access token."
  :side-effects "Writes shared Codex auth.json credentials to disk with 600 permissions."
  :see-also (read-openai-codex-oauth-tokens exchange-openai-oauth-code *codex-auth-path*))

(defdoc read-openai-codex-oauth-tokens
  :category "llm"
  :usage "(read-openai-codex-oauth-tokens) — (read-openai-codex-oauth-tokens)"
  :returns "plist or nil — (:auth-mode ... :openai-api-key ... :id-token ... :access-token ... :refresh-token ... :account-id ... :last-refresh ...)"
  :see-also (save-openai-codex-oauth-tokens read-openai-codex-oauth-token *codex-auth-path*))

(defdoc refresh-openai-codex-oauth-token
  :category "llm"
  :usage "(refresh-openai-codex-oauth-token REFRESH-TOKEN:string)"
  :returns "string or nil — The refreshed ChatGPT access token, or nil on failure."
  :side-effects "Refreshes shared Codex auth.json credentials and saves the updated payload to disk."
  :see-also (read-openai-codex-oauth-token save-openai-codex-oauth-tokens))

(defdoc read-openai-codex-oauth-token
  :category "llm"
  :usage "(read-openai-codex-oauth-token) — (read-openai-codex-oauth-token)"
  :returns "string or nil — A valid ChatGPT access token, auto-refreshed when stale."
  :side-effects "May refresh shared Codex auth.json via HTTP when the ChatGPT token is stale."
  :see-also (read-openai-codex-oauth-tokens refresh-openai-codex-oauth-token))

(defdoc openai-codex-oauth-finish
  :category "llm"
  :usage "(openai-codex-oauth-finish CALLBACK-URL CODE-VERIFIER EXPECTED-STATE)"
  :returns "string — The access token."
  :side-effects "Exchanges the authorization code for tokens, optionally exchanges the ID token for an API key, and saves the shared Codex auth.json payload."
  :see-also (openai-codex-oauth-start extract-oauth-callback-params openai-codex-oauth-command))

(defdoc load-agent-defaults
  :category "llm"
  :usage "(load-agent-defaults) — (load-agent-defaults)"
  :returns "list — The agent defaults registry."
  :side-effects "Reads ~/.config/clawmacs/agent-defaults.json and memoizes the result."
  :see-also (save-agent-defaults agent-default set-agent-default ensure-agent-defaults-loaded))

(defdoc save-agent-defaults
  :category "llm"
  :usage "(save-agent-defaults) — (save-agent-defaults)"
  :returns "pathname — Path to the saved defaults file."
  :side-effects "Writes the agent defaults registry to disk as JSON."
  :see-also (load-agent-defaults set-agent-default))

(defdoc agent-default
  :category "llm"
  :usage "(agent-default AGENT-NAME:string) — (agent-default \"coder\")"
  :returns "keyword — :OPENAI-CODEX, :ZAI, or :OPENROUTER."
  :see-also (set-agent-default resolve-buffer-provider-and-model load-agent-defaults))

(defdoc set-agent-default
  :category "llm"
  :usage "(set-agent-default AGENT-NAME:string PROVIDER:keyword &key MODEL:string) — (set-agent-default \"coder\" :zai :model \"glm-5\")"
  :returns "keyword — The normalized provider."
  :side-effects "Updates the agent defaults registry and persists to disk."
  :see-also (agent-default save-agent-defaults resolve-buffer-provider-and-model))

(defdoc ensure-agent-defaults-loaded
  :category "llm"
  :usage "(ensure-agent-defaults-loaded) — (ensure-agent-defaults-loaded)"
  :returns "list — The agent defaults registry."
  :side-effects "Loads defaults from disk on first call (lazy initialization)."
  :see-also (load-agent-defaults agent-default))

(defdoc resolve-buffer-provider-and-model
  :category "llm"
  :usage "(resolve-buffer-provider-and-model BUF:buffer) — (resolve-buffer-provider-and-model (current-buffer))"
  :returns "values provider:keyword model:string think-level:(or null string) — e.g. :ZAI, \"glm-5\", nil"
  :see-also (buffer-provider-override buffer-model-override buffer-think-level-override agent-default))

(defdoc provider-model-supported-think-levels
  :category "llm"
  :usage "(provider-model-supported-think-levels PROVIDER:keyword MODEL:string) — (provider-model-supported-think-levels :openai-codex \"gpt-5.4\")"
  :returns "list of strings or nil — e.g. (\"none\" \"low\" \"medium\" \"high\" \"xhigh\")"
  :see-also (resolve-buffer-provider-and-model select-think-level-command))

(defdoc reconcile-buffer-think-level-override
  :category "llm"
  :usage "(reconcile-buffer-think-level-override BUF:buffer &key PROVIDER MODEL)"
  :returns "values status:keyword think-level:(or null string)"
  :side-effects "Clears stale think-level overrides that are unsupported by the active model."
  :see-also (buffer-think-level-override provider-model-supported-think-levels))

(defdoc build-conversation-messages
  :category "llm"
  :usage "(build-conversation-messages BUF:buffer) — (build-conversation-messages (current-buffer))"
  :returns "list — API-format messages array (list of alists with :role and :content)."
  :see-also (provider-request buffer-first-message message-raw-content))

(defdoc api-json-encode
  :category "llm"
  :usage "(api-json-encode OBJECT) — (api-json-encode '((:key . \"value\")))"
  :returns "string — JSON string with double-dash keys encoded as underscores."
  :see-also (api-json-decode))

(defdoc api-json-decode
  :category "llm"
  :usage "(api-json-decode STRING:string) — (api-json-decode \"{\\\"key\\\": \\\"value\\\"}\")"
  :returns "alist — Decoded JSON with underscore-preserving key mapping."
  :see-also (api-json-encode))

(defdoc zai-request
  :category "llm"
  :usage "(zai-request MESSAGES:list CALLBACK:function &key MODEL TOOLS)"
  :returns "list — The API response as an alist."
  :side-effects "Makes HTTP POST to the Z.AI Chat Completions API."
  :see-also (zai-request-streaming *zai-model* *zai-api-url*))

(defdoc zai-request-streaming
  :category "llm"
  :usage "(zai-request-streaming MESSAGES:list CALLBACK:function &key MODEL TOOLS)"
  :returns "stream-state — State object for polling streaming progress."
  :side-effects "Spawns a background thread for streaming from Z.AI API."
  :see-also (zai-request *zai-model*))

(defdoc provider-known-models
  :category "llm"
  :usage "(provider-known-models PROVIDER:keyword) — (provider-known-models :zai)"
  :returns "list of strings — (\"glm-5\" \"glm-5-turbo\" ...)"
  :see-also (*provider-known-models* provider-has-token-p available-models-for-selector))

(defdoc provider-has-token-p
  :category "llm"
  :usage "(provider-has-token-p PROVIDER:keyword) — (provider-has-token-p :zai)"
  :returns "boolean — T if the provider has a usable API key or OAuth token."
  :see-also (read-provider-token provider-known-models available-models-for-selector))

(defdoc available-models-for-selector
  :category "llm"
  :usage "(available-models-for-selector BUF:buffer) — (available-models-for-selector (current-buffer))"
  :returns "list of plists — ((:provider :zai :model \"name\" :active-p t) ...)"
  :see-also (provider-known-models provider-has-token-p select-model-command minibuffer-select-model-command))

;;; ==========================================================================
;;; Category: tool — Tool registry and execution
;;; ==========================================================================

(defdoc *http-fetch-max-chars*
  :category "tool"
  :returns "integer - Default character limit for the dormant http_fetch helper implementation."
  :see-also (*http-connection-timeout* *http-user-agent*))

(defdoc *http-connection-timeout*
  :category "tool"
  :returns "integer - Connection timeout for the dormant http_fetch helper implementation."
  :see-also (*http-fetch-max-chars* *http-user-agent*))

(defdoc *http-user-agent*
  :category "tool"
  :returns "string - User-Agent header used by the dormant http_fetch helper implementation."
  :see-also (*http-fetch-max-chars* *http-connection-timeout*))

(defdoc *file-read-default-limit*
  :category "tool"
  :returns "integer - Default line limit for the dormant file_read helper implementation."
  :see-also (*tool-table* register-tool))

(defdoc *shell-exec-default-timeout*
  :category "tool"
  :returns "integer - Default timeout for the dormant shell_exec helper implementation."
  :see-also (*tool-table* register-tool))

(defdoc *diff-display-max-lines*
  :category "tool"
  :returns "integer - Maximum diff lines shown when approval UIs render non-default tool edits."
  :see-also (*tool-table* tool-approval-extra-display))

(defdoc *tool-table*
  :category "tool"
  :see-also (register-tool execute-tool tool-definitions-for-api init-tools))

(defdoc tool-definition
  :category "tool"
  :usage "Created by register-tool."
  :returns "Structure — Holds name, description, schema, permission, and execute-fn."
  :see-also (register-tool *tool-table* execute-tool))

(defdoc register-tool
  :category "tool"
  :usage "(register-tool NAME DESCRIPTION SCHEMA PERMISSION EXECUTE-FN ...)"
  :returns "tool-definition — The registered tool."
  :side-effects "Stores the tool definition in *tool-table*."
  :see-also (*tool-table* tool-definition execute-tool init-tools))

(defdoc execute-tool
  :category "tool"
  :usage "(execute-tool NAME:string ARGS:alist) — (execute-tool \"lisp_eval\" '((:code . \"(+ 1 2)\")))"
  :returns "string — The tool execution result as a JSON string."
  :side-effects "Executes the tool's function. Side effects depend on the registered tool implementation."
  :see-also (register-tool tool-requires-permission-p *tool-table*))

(defdoc tool-requires-permission-p
  :category "tool"
  :usage "(tool-requires-permission-p NAME:string) — (tool-requires-permission-p \"lisp_eval\")"
  :returns "boolean — T if the tool requires user approval."
  :see-also (execute-tool register-tool))

(defdoc tool-definitions-for-api
  :category "tool"
  :usage "(tool-definitions-for-api) — (tool-definitions-for-api)"
  :returns "list — Tool definitions formatted for provider adapters."
  :see-also (*tool-table* register-tool provider-request))

(defdoc format-tool-call-sexpr
  :category "tool"
  :usage "(format-tool-call-sexpr NAME:string ARGS:alist)"
  :returns "string — S-expression formatted tool call. \"(lisp_eval :code \\\"(+ 1 2)\\\")\""
  :see-also (format-tool-call-expanded tool-approval-extra-display))

(defdoc format-tool-call-expanded
  :category "tool"
  :usage "(format-tool-call-expanded NAME:string ARGS:alist)"
  :returns "string — Multi-line expanded display of a tool call."
  :see-also (format-tool-call-sexpr tool-approval-extra-display))

(defdoc tool-approval-extra-display
  :category "tool"
  :usage "(tool-approval-extra-display NAME:string ARGS:alist)"
  :returns "string or nil — Extra display content for approval prompts (e.g. file diffs)."
  :see-also (format-tool-call-sexpr format-tool-call-expanded))

(defdoc init-tools
  :category "tool"
  :usage "(init-tools) — Called once at startup."
  :returns "nil"
  :side-effects "Removes previously registered built-in tool entries and re-registers only the default built-in tool, lisp_eval."
  :see-also (*tool-table* register-tool))

;;; ==========================================================================
;;; Category: approval — Tool approval state
;;; ==========================================================================

(defdoc buffer-approval-pending
  :category "approval"
  :usage "(buffer-approval-pending BUF:buffer) — (buffer-approval-pending (current-buffer))"
  :returns "alist or nil — Describes the tool call awaiting approval."
  :see-also (buffer-approval-result buffer-stashed-input buffer-status))

(defdoc buffer-approval-result
  :category "approval"
  :usage "(buffer-approval-result BUF:buffer) — (buffer-approval-result (current-buffer))"
  :returns "keyword or cons — :APPROVE, :DENY, or (:DENY-WITH-MESSAGE . \"reason\")."
  :see-also (buffer-approval-pending))

(defdoc buffer-stashed-input
  :category "approval"
  :usage "(buffer-stashed-input BUF:buffer) — (buffer-stashed-input (current-buffer))"
  :returns "string or nil — The user's input text stashed during approval."
  :see-also (buffer-approval-pending))

(defdoc buffer-pending-tool-calls
  :category "approval"
  :usage "(buffer-pending-tool-calls BUF:buffer)"
  :returns "list — Tool_use blocks awaiting sequential approval."
  :see-also (buffer-tool-call-results buffer-approval-pending))

(defdoc buffer-tool-call-results
  :category "approval"
  :usage "(buffer-tool-call-results BUF:buffer)"
  :returns "list — Accumulated results from approved/denied tool calls."
  :see-also (buffer-pending-tool-calls buffer-approval-pending))

;;; ==========================================================================
;;; Category: global-face — Global face registry
;;; ==========================================================================

(defdoc *global-face-registry*
  :category "global-face"
  :usage "*global-face-registry* — hash table mapping keywords to face objects"
  :returns "hash-table"
  :side-effects "Populated at startup by init-global-faces."
  :see-also (global-face init-global-faces collect-global-faces collect-all-faces))

(defdoc global-face
  :category "global-face"
  :usage "(global-face NAME:keyword) — (global-face :modeline)"
  :returns "face or nil — the face object, or nil if not found."
  :see-also (*global-face-registry* init-global-faces apply-global-face))

(defdoc init-global-faces
  :category "global-face"
  :usage "(init-global-faces) — called once at startup in clawmacs-main"
  :returns "hash-table — the populated *global-face-registry*"
  :side-effects "Clears and repopulates *global-face-registry* with theme faces for modeline, system/debug messages, minibuffer and selector UI, approval prompts, default text, who-line, and tool-call/tool-result Lisp syntax highlighting."
  :see-also (*global-face-registry* global-face customize-face-command))

(defdoc collect-global-faces
  :category "global-face"
  :usage "(collect-global-faces)"
  :returns "list of plists — each with :face, :owner (:global), :name, :label"
  :see-also (*global-face-registry* collect-all-faces))

(defdoc apply-global-face
  :category "global-face"
  :usage "(apply-global-face WINDOW:croatoan-window NAME:keyword) — (apply-global-face win :modeline)"
  :returns "nil"
  :side-effects "Sets the window's color pair and attributes from the resolved global face."
  :see-also (global-face apply-face-to-window resolve-face))

(defdoc make-default-system-face-set
  :category "global-face"
  :usage "(make-default-system-face-set)"
  :returns "face-set — a face set for system messages with cyan-on-black default."
  :see-also (make-default-user-face-set make-default-agent-face-set init-face-registry))

(defdoc make-default-text-face
  :category "global-face"
  :usage "(make-default-text-face)"
  :returns "face — the default text face (white on black, no bold) from the global registry."
  :see-also (make-modeline-face make-default-system-face-set global-face)
  :side-effects "None — reads from the global face registry.")

;;; ==========================================================================
;;; Category: render — Rendering and display
;;; ==========================================================================

(defdoc resolve-modeline-provider-model
  :category "render"
  :usage "(resolve-modeline-provider-model BUF:buffer) — (resolve-modeline-provider-model (current-buffer))"
  :returns "string — \"zai/glm-5\" or \"openai-codex/gpt-5.4 [think:high]\", or \"??\" on error."
  :see-also (render-modeline resolve-buffer-provider-and-model))

(defdoc render-modeline
  :category "render"
  :usage "(render-modeline BUF:buffer MODELINE-WINDOW:croatoan-window)"
  :returns "nil"
  :side-effects "Clears and redraws the modeline window with buffer status info."
  :see-also (resolve-modeline-provider-model render-buffer buffer-major-mode))

(defdoc render-buffer
  :category "render"
  :usage "(render-buffer BUF:buffer MAIN-WINDOW MODELINE-WINDOW)"
  :returns "nil"
  :side-effects "Clears and redraws the main window with buffer history and input, then renders modeline."
  :see-also (render-modeline render-buffer-selector render-model-selector))

;;; ==========================================================================
;;; Category: selector — Buffer selector, model selector, minibuffer
;;; ==========================================================================

(defdoc *buffer-selector-active*
  :category "selector"
  :see-also (*buffer-selector-index* list-buffers-command render-buffer-selector))

(defdoc *buffer-selector-index*
  :category "selector"
  :see-also (*buffer-selector-active* list-buffers-command))

(defdoc list-buffers-command
  :category "selector"
  :usage "Bound to C-x b. Opens the overlay buffer selector."
  :returns "nil"
  :side-effects "Sets *buffer-selector-active* to T."
  :see-also (*buffer-selector-active* minibuffer-select-buffer-command render-buffer-selector))

(defdoc render-buffer-selector
  :category "selector"
  :usage "(render-buffer-selector WINDOW:croatoan-window)"
  :returns "nil"
  :side-effects "Renders the overlay buffer selector table."
  :see-also (list-buffers-command *buffer-selector-active*))

(defdoc *model-selector-active*
  :category "selector"
  :see-also (*model-selector-index* *model-selector-entries* select-model-command))

(defdoc *model-selector-index*
  :category "selector"
  :see-also (*model-selector-active* select-model-command))

(defdoc *model-selector-entries*
  :category "selector"
  :see-also (*model-selector-active* select-model-command available-models-for-selector))

(defdoc select-model-command
  :category "selector"
  :usage "Bound to C-c M. Opens the overlay model selector."
  :returns "nil"
  :side-effects "Sets *model-selector-active* to T and populates *model-selector-entries*."
  :see-also (*model-selector-active* minibuffer-select-model-command available-models-for-selector))

(defdoc render-model-selector
  :category "selector"
  :usage "(render-model-selector WINDOW ENTRIES:list SELECTED-INDEX:fixnum)"
  :returns "nil"
  :side-effects "Renders the overlay model selector table."
  :see-also (select-model-command *model-selector-entries*))

(defdoc handle-model-selector-key
  :category "selector"
  :usage "(handle-model-selector-key BUF KEY)"
  :returns "nil"
  :side-effects "Processes key input in the overlay model selector (navigate, confirm, cancel)."
  :see-also (select-model-command *model-selector-active*))

(defdoc *think-selector-active*
  :category "selector"
  :see-also (*think-selector-index* *think-selector-entries* select-think-level-command))

(defdoc *think-selector-index*
  :category "selector"
  :see-also (*think-selector-active* select-think-level-command))

(defdoc *think-selector-entries*
  :category "selector"
  :see-also (*think-selector-active* select-think-level-command provider-model-supported-think-levels))

(defdoc select-think-level-command
  :category "selector"
  :usage "Bound to C-c R. Opens the overlay think-level selector."
  :returns "nil"
  :side-effects "Sets *think-selector-active* to T and populates *think-selector-entries*."
  :see-also (*think-selector-active* minibuffer-select-think-level-command provider-model-supported-think-levels))

(defdoc render-think-selector
  :category "selector"
  :usage "(render-think-selector WINDOW ENTRIES:list SELECTED-INDEX:fixnum)"
  :returns "nil"
  :side-effects "Renders the overlay think-level selector table."
  :see-also (select-think-level-command *think-selector-entries*))

(defdoc handle-think-selector-key
  :category "selector"
  :usage "(handle-think-selector-key BUF KEY)"
  :returns "nil"
  :side-effects "Processes key input in the overlay think-level selector (navigate, confirm, cancel)."
  :see-also (select-think-level-command *think-selector-active*))

;;; ==========================================================================
;;; Category: minibuffer — Minibuffer completion system
;;; ==========================================================================

(defdoc *minibuffer-active*
  :category "minibuffer"
  :see-also (minibuffer-activate minibuffer-deactivate handle-minibuffer-key))

(defdoc *minibuffer-mode*
  :category "minibuffer"
  :see-also (minibuffer-activate minibuffer-prompt minibuffer-confirm))

(defdoc *minibuffer-prompt*
  :category "minibuffer"
  :see-also (minibuffer-activate *minibuffer-input*))

(defdoc *minibuffer-input*
  :category "minibuffer"
  :see-also (minibuffer-activate minibuffer-update-filter *minibuffer-point*))

(defdoc *minibuffer-point*
  :category "minibuffer"
  :see-also (*minibuffer-input* minibuffer-activate))

(defdoc *minibuffer-items*
  :category "minibuffer"
  :see-also (minibuffer-activate *minibuffer-filtered-items* minibuffer-update-filter))

(defdoc *minibuffer-filtered-items*
  :category "minibuffer"
  :see-also (*minibuffer-items* minibuffer-update-filter fuzzy-match-p))

(defdoc *minibuffer-selected-index*
  :category "minibuffer"
  :see-also (*minibuffer-filtered-items* minibuffer-ensure-visible))

(defdoc *minibuffer-scroll-offset*
  :category "minibuffer"
  :see-also (minibuffer-ensure-visible minibuffer-visible-item-count render-minibuffer))

(defdoc *minibuffer-callback*
  :category "minibuffer"
  :see-also (minibuffer-activate minibuffer-confirm))

(defdoc *minibuffer-max-height*
  :category "minibuffer"
  :see-also (minibuffer-activate minibuffer-current-height))

(defdoc *model-selection-history*
  :category "minibuffer"
  :see-also (sort-models-by-recency minibuffer-select-model-command))

(defdoc *buffer-selection-history*
  :category "minibuffer"
  :see-also (sort-buffers-by-recency minibuffer-select-buffer-command))

(defdoc fuzzy-match-p
  :category "minibuffer"
  :usage "(fuzzy-match-p QUERY:string CANDIDATE:string) — (fuzzy-match-p \"glm\" \"glm-5\")"
  :returns "boolean — T if all characters in QUERY appear in CANDIDATE in order."
  :see-also (minibuffer-update-filter *minibuffer-filtered-items*))

(defdoc minibuffer-item-display
  :category "minibuffer"
  :usage "(minibuffer-item-display ITEM) — (minibuffer-item-display '(:display \"foo\"))"
  :returns "string — The display string for the item."
  :see-also (minibuffer-activate fuzzy-match-p))

(defdoc minibuffer-item-match-text
  :category "minibuffer"
  :usage "(minibuffer-item-match-text ITEM)"
  :returns "string — The text used for fuzzy matching."
  :see-also (minibuffer-item-display minibuffer-update-filter))

(defdoc minibuffer-activate
  :category "minibuffer"
  :usage "(minibuffer-activate PROMPT:string ITEMS:list CALLBACK:function &key MODE INITIAL-INPUT)"
  :returns "nil"
  :side-effects "Sets all minibuffer state variables. Makes the minibuffer active and visible."
  :see-also (minibuffer-deactivate minibuffer-confirm minibuffer-cancel *minibuffer-active*))

(defdoc minibuffer-prompt
  :category "minibuffer"
  :usage "(minibuffer-prompt PROMPT:string CALLBACK:function &key INITIAL-INPUT)"
  :returns "nil"
  :side-effects "Activates the minibuffer in prompt mode and submits raw input to CALLBACK."
  :see-also (minibuffer-activate minibuffer-confirm))

(defdoc minibuffer-deactivate
  :category "minibuffer"
  :usage "(minibuffer-deactivate) — (minibuffer-deactivate)"
  :returns "nil"
  :side-effects "Clears all minibuffer state. Makes the minibuffer inactive (1 row)."
  :see-also (minibuffer-activate minibuffer-cancel *minibuffer-active*))

(defdoc minibuffer-update-filter
  :category "minibuffer"
  :usage "(minibuffer-update-filter) — (minibuffer-update-filter)"
  :returns "nil"
  :side-effects "Filters *minibuffer-items* by fuzzy matching *minibuffer-input*. Resets selection and scroll."
  :see-also (fuzzy-match-p *minibuffer-filtered-items* *minibuffer-input*))

(defdoc minibuffer-visible-item-count
  :category "minibuffer"
  :usage "(minibuffer-visible-item-count) — (minibuffer-visible-item-count)"
  :returns "fixnum — Number of candidate rows visible in the minibuffer."
  :see-also (minibuffer-ensure-visible *minibuffer-max-height* minibuffer-current-height))

(defdoc minibuffer-ensure-visible
  :category "minibuffer"
  :usage "(minibuffer-ensure-visible) — (minibuffer-ensure-visible)"
  :returns "nil"
  :side-effects "Adjusts *minibuffer-scroll-offset* so the selected item is on-screen."
  :see-also (minibuffer-visible-item-count *minibuffer-scroll-offset* *minibuffer-selected-index*))

(defdoc minibuffer-confirm
  :category "minibuffer"
  :usage "(minibuffer-confirm) — Called when user presses Return in the minibuffer."
  :returns "nil"
  :side-effects "Calls *minibuffer-callback* with the selected item, then deactivates."
  :see-also (minibuffer-cancel minibuffer-activate *minibuffer-callback*))

(defdoc minibuffer-cancel
  :category "minibuffer"
  :usage "(minibuffer-cancel) — Called when user presses C-g in the minibuffer."
  :returns "nil"
  :side-effects "Deactivates the minibuffer without calling the callback."
  :see-also (minibuffer-confirm minibuffer-deactivate))

(defdoc handle-minibuffer-key
  :category "minibuffer"
  :usage "(handle-minibuffer-key KEY) — Dispatches key events when minibuffer is active."
  :returns "nil"
  :side-effects "Updates minibuffer input, selection, or confirms/cancels based on key."
  :see-also (minibuffer-activate minibuffer-confirm minibuffer-cancel))

(defdoc invoke-command
  :category "minibuffer"
  :usage "(invoke-command BUFFER COMMAND)"
  :returns "The direct command result, or NIL when invocation switches to minibuffer prompts."
  :side-effects "Runs an interactive command immediately or prompts for its arguments in the minibuffer."
  :see-also (execute-extended-command list-interactive-commands))

(defdoc execute-extended-command
  :category "minibuffer"
  :usage "(execute-extended-command BUFFER)"
  :returns "nil"
  :side-effects "Opens the M-x minibuffer, lets the user choose an interactive command, and invokes it."
  :see-also (invoke-command list-interactive-commands))

(defdoc sort-models-by-recency
  :category "minibuffer"
  :usage "(sort-models-by-recency ITEMS:list) — (sort-models-by-recency model-items)"
  :returns "list — Items sorted by recency from *model-selection-history*, then alphabetically."
  :see-also (*model-selection-history* minibuffer-select-model-command))

(defdoc sort-buffers-by-recency
  :category "minibuffer"
  :usage "(sort-buffers-by-recency ITEMS:list) — (sort-buffers-by-recency buffer-items)"
  :returns "list — Items sorted by recency from *buffer-selection-history*, then alphabetically."
  :see-also (*buffer-selection-history* minibuffer-select-buffer-command))

(defdoc minibuffer-current-height
  :category "minibuffer"
  :usage "(minibuffer-current-height) — (minibuffer-current-height)"
  :returns "fixnum — Current height of the minibuffer in rows (1 when inactive)."
  :see-also (*minibuffer-active* *minibuffer-max-height* update-window-layout))

(defdoc update-window-layout
  :category "minibuffer"
  :usage "(update-window-layout MAIN-WINDOW MODELINE-WINDOW MINIBUFFER-WINDOW SCREEN-HEIGHT SCREEN-WIDTH)"
  :returns "nil"
  :side-effects "Resizes and repositions all three windows based on minibuffer height."
  :see-also (minibuffer-current-height render-minibuffer clawmacs-main))

(defdoc render-minibuffer
  :category "minibuffer"
  :usage "(render-minibuffer MINIBUFFER-WINDOW:croatoan-window)"
  :returns "nil"
  :side-effects "Clears and redraws the minibuffer. Shows candidates with inverse video selection."
  :see-also (minibuffer-current-height *minibuffer-active* update-window-layout))

(defdoc minibuffer-select-model-command
  :category "minibuffer"
  :usage "Bound to C-c C-m. Opens the minibuffer model selector with fuzzy search."
  :returns "nil"
  :side-effects "Activates the minibuffer with available models. On confirm, sets buffer overrides."
  :see-also (minibuffer-activate select-model-command available-models-for-selector *model-selection-history*))

(defdoc minibuffer-select-think-level-command
  :category "minibuffer"
  :usage "Bound to C-c C-r. Opens the minibuffer think-level selector."
  :returns "nil"
  :side-effects "Activates the minibuffer with think levels for the active model. On confirm, sets or clears the think override."
  :see-also (minibuffer-activate select-think-level-command provider-model-supported-think-levels))

(defdoc minibuffer-select-buffer-command
  :category "minibuffer"
  :usage "Bound to C-x C-b. Opens the minibuffer buffer selector with fuzzy search."
  :returns "nil"
  :side-effects "Activates the minibuffer with all buffers. On confirm, switches to selected buffer."
  :see-also (minibuffer-activate list-buffers-command switch-to-buffer *buffer-selection-history*))

;;; ==========================================================================
;;; Category: help — Introspection and help system
;;; ==========================================================================

(defdoc *common-lisp-spec-root*
  :category "help"
  :usage "*common-lisp-spec-root*"
  :returns "pathname — Directory containing the vendored CL Community Spec snapshot and `pages/` subtree."
  :see-also (common-lisp-spec-available-p describe-common-lisp-symbol-to-string search-common-lisp-spec-to-string))

(defdoc common-lisp-spec-available-p
  :category "help"
  :usage "(common-lisp-spec-available-p) — (common-lisp-spec-available-p)"
  :returns "boolean — Non-nil when the vendored CL Community Spec pages and index are available."
  :see-also (*common-lisp-spec-root* describe-common-lisp-symbol-to-string search-common-lisp-spec-to-string))

(defdoc find-common-lisp-spec-entry
  :category "help"
  :usage "(find-common-lisp-spec-entry DESIGNATOR) — (find-common-lisp-spec-entry 'format)"
  :returns "plist or nil — Includes :term, :page, :path, and :url for an exact CL Community Spec match."
  :see-also (describe-common-lisp-symbol-to-string search-common-lisp-spec-to-string common-lisp-spec-available-p))

(defdoc describe-common-lisp-symbol-to-string
  :category "help"
  :usage "(describe-common-lisp-symbol-to-string DESIGNATOR &key MAX-CHARS) — (describe-common-lisp-symbol-to-string 'handler-case)"
  :returns "string — Plain-text rendering of the vendored CL Community Spec page."
  :see-also (find-common-lisp-spec-entry search-common-lisp-spec-to-string documentation))

(defdoc search-common-lisp-spec-to-string
  :category "help"
  :usage "(search-common-lisp-spec-to-string QUERY &key LIMIT) — (search-common-lisp-spec-to-string \"dynamic extent\")"
  :returns "string — Ranked CL Community Spec search results with page names and upstream URLs."
  :see-also (describe-common-lisp-symbol-to-string find-common-lisp-spec-entry))

(defdoc list-functions
  :category "help"
  :usage "(list-functions) — (list-functions)"
  :returns "list of symbols — Sorted list of all exported function symbols from :clawmacs."
  :see-also (describe-function-to-string describe-function-command list-variables))

(defdoc find-keybindings-for-command
  :category "help"
  :usage "(find-keybindings-for-command COMMAND-SYM:symbol &optional KEYMAP:keymap) — (find-keybindings-for-command 'send-message)"
  :returns "list — Key specifications bound to COMMAND-SYM. e.g. (#\\Return)"
  :see-also (format-key-binding describe-function-to-string keymap-bindings))

(defdoc format-key-binding
  :category "help"
  :usage "(format-key-binding KEY) — (format-key-binding '(:ctrl-x #\\k))"
  :returns "string — Human-readable key notation: \"C-x k\", \"C-c C-m\", \"RET\", \"M-v\""
  :see-also (find-keybindings-for-command describe-function-to-string))

(defdoc describe-function-to-string
  :category "help"
  :usage "(describe-function-to-string FN-SYMBOL:symbol) — (describe-function-to-string 'make-buffer)"
  :returns "string — Multi-line human-readable description with type, args, docs, extended docs."
  :see-also (describe-function-command list-functions extended-doc))

(defdoc make-help-buffer
  :category "help"
  :usage "(make-help-buffer NAME:string CONTENT:string) — (make-help-buffer \"*help:foo*\" \"Description text\")"
  :returns "buffer — A new help buffer added to the buffer ring."
  :side-effects "Creates a buffer with major-mode \"help\" and inserts content as a read-only message."
  :see-also (describe-function-command describe-variable-command buffer-major-mode))

(defdoc describe-function-command
  :category "help"
  :usage "Bound to C-h f. Opens minibuffer listing all functions."
  :returns "nil"
  :side-effects "On selection, creates/switches to a help buffer with the function description."
  :see-also (describe-function-to-string list-functions make-help-buffer))

(defdoc list-variables
  :category "help"
  :usage "(list-variables) — (list-variables)"
  :returns "list of symbols — Sorted list of all exported variable symbols from :clawmacs."
  :see-also (describe-variable-to-string describe-variable-command list-functions))

(defdoc variable-kind
  :category "help"
  :usage "(variable-kind SYM:symbol) — (variable-kind '*buffer-ring*)"
  :returns "keyword — :CONSTANT, :PARAMETER, or :VARIABLE."
  :see-also (describe-variable-to-string list-variables))

(defdoc truncate-value-string
  :category "help"
  :usage "(truncate-value-string VALUE &optional MAX-LENGTH) — (truncate-value-string *buffer-ring* 40)"
  :returns "string — Printed representation of VALUE, truncated at MAX-LENGTH."
  :see-also (describe-variable-to-string))

(defdoc describe-variable-to-string
  :category "help"
  :usage "(describe-variable-to-string VAR-SYMBOL:symbol) — (describe-variable-to-string '*buffer-ring*)"
  :returns "string — Multi-line human-readable description with kind, type, value, docs."
  :see-also (describe-variable-command list-variables extended-doc))

(defdoc describe-variable-command
  :category "help"
  :usage "Bound to C-h v. Opens minibuffer listing all variables."
  :returns "nil"
  :side-effects "On selection, creates/switches to a help buffer with the variable description."
  :see-also (describe-variable-to-string list-variables make-help-buffer))

(defdoc list-types
  :category "help"
  :usage "(list-types) — (list-types)"
  :returns "list of symbols — Sorted list of all exported type symbols (classes, structs, conditions) from :clawmacs."
  :see-also (describe-type-to-string describe-type-command list-functions list-variables))

(defdoc type-kind
  :category "help"
  :usage "(type-kind SYM:symbol) — (type-kind 'buffer)"
  :returns "keyword — :CONDITION, :STRUCTURE, :STANDARD-CLASS, :CLASS, or :UNKNOWN."
  :see-also (type-kind-label describe-type-to-string list-types))

(defdoc type-kind-label
  :category "help"
  :usage "(type-kind-label KIND:keyword) — (type-kind-label :standard-class)"
  :returns "string — Human-readable label: \"Class (defclass)\", \"Structure (defstruct)\", etc."
  :see-also (type-kind describe-type-to-string))

(defdoc type-slot-info
  :category "help"
  :usage "(type-slot-info CLASS:class) — (type-slot-info (find-class 'buffer))"
  :returns "list of plists — Each plist has :name, :type, :initform, :initargs, :readers, :writers, :allocation, :documentation."
  :side-effects "Finalizes class inheritance if not already finalized (via sb-mop:finalize-inheritance)."
  :see-also (type-struct-slot-info describe-type-to-string))

(defdoc type-struct-slot-info
  :category "help"
  :usage "(type-struct-slot-info SYM:symbol) — (type-struct-slot-info 'stream-state)"
  :returns "list of plists — Each plist has :name, :type, :read-only, :accessor."
  :see-also (type-slot-info describe-type-to-string))

(defdoc describe-type-to-string
  :category "help"
  :usage "(describe-type-to-string TYPE-SYMBOL:symbol) — (describe-type-to-string 'buffer)"
  :returns "string — Multi-line human-readable description with kind, superclasses, slots/fields, docs."
  :see-also (describe-type-command list-types type-slot-info type-struct-slot-info extended-doc))

(defdoc undocumented-types
  :category "help"
  :usage "(undocumented-types) — (undocumented-types)"
  :returns "list of symbols — Exported type symbols missing a defdoc entry."
  :see-also (undocumented-functions undocumented-variables list-types))

(defdoc describe-type-command
  :category "help"
  :usage "Bound to C-h T. Opens minibuffer listing all types."
  :returns "nil"
  :side-effects "On selection, creates/switches to a help buffer with the type description."
  :see-also (describe-type-to-string list-types make-help-buffer))

(defdoc list-project-systems
  :category "help"
  :usage "(list-project-systems) — (list-project-systems)"
  :returns "list of strings — clawmacs plus the systems imported in clawmacs.asd."
  :see-also (describe-system-to-string list-system-packages search-system-docs))

(defdoc describe-system-to-string
  :category "help"
  :usage "(describe-system-to-string SYSTEM) — (describe-system-to-string \"croatoan\")"
  :returns "string — Local ASDF summary with root path, metadata, packages, and docs."
  :see-also (list-project-systems list-system-packages search-system-docs))

(defdoc list-system-packages
  :category "help"
  :usage "(list-system-packages SYSTEM) — (list-system-packages \"alexandria\")"
  :returns "list of strings — Package names defined by the system's package source files."
  :see-also (describe-system-to-string list-package-functions describe-library-symbol-to-string))

(defdoc list-package-functions
  :category "help"
  :usage "(list-package-functions PACKAGE) — (list-package-functions \"CLAWMACS\")"
  :returns "list of symbols — Exported function symbols from PACKAGE."
  :see-also (list-package-variables list-package-types describe-library-symbol-to-string))

(defdoc list-package-variables
  :category "help"
  :usage "(list-package-variables PACKAGE) — (list-package-variables \"CLAWMACS\")"
  :returns "list of symbols — Exported bound variable symbols from PACKAGE."
  :see-also (list-package-functions list-package-types describe-library-symbol-to-string))

(defdoc list-package-types
  :category "help"
  :usage "(list-package-types PACKAGE) — (list-package-types \"CLAWMACS\")"
  :returns "list of symbols — Exported class and type symbols from PACKAGE."
  :see-also (list-package-functions list-package-variables describe-library-symbol-to-string))

(defdoc describe-library-symbol-to-string
  :category "help"
  :usage "(describe-library-symbol-to-string SYSTEM SYMBOL &optional PACKAGE) — (describe-library-symbol-to-string \"clawmacs\" 'describe-function-to-string \"CLAWMACS\")"
  :returns "string — Runtime docstrings plus local source/doc hits for a library symbol."
  :see-also (list-system-packages list-package-functions search-system-docs))

(defdoc search-system-docs
  :category "help"
  :usage "(search-system-docs SYSTEM QUERY &key LIMIT) — (search-system-docs \"croatoan\" \"thread\")"
  :returns "string — Matching README/docs/source hits with relative paths and line numbers."
  :see-also (describe-system-to-string describe-library-symbol-to-string list-project-systems))

;;; ==========================================================================
;;; Category: type — Type definitions (classes, structures, conditions)
;;; ==========================================================================

(defdoc color-spec
  :category "type"
  :usage "(make-color-spec :color-type :cga :value 15)"
  :see-also (make-color-spec color-spec-type color-spec-value face))

(defdoc face
  :category "type"
  :usage "(make-instance 'face :name :default :foreground (make-color-spec ...) :bold-p t)"
  :see-also (resolve-face face-name face-foreground face-background face-bold-p face-parent resolved-face face-set))

(defdoc resolved-face
  :category "type"
  :usage "(resolve-face some-face fallback-face) => resolved-face"
  :see-also (resolve-face resolved-face-foreground resolved-face-background resolved-face-bold-p face))

(defdoc face-set
  :category "type"
  :usage "(make-face-set :user user-faces-ht)"
  :see-also (make-face-set face-set-owner face-set-faces get-face face))

(defdoc line
  :category "type"
  :usage "(make-line \"Hello world\")"
  :see-also (make-line line-content line-next line-prev message))

(defdoc message
  :category "type"
  :usage "(make-message :user)"
  :see-also (make-message message-sender message-text message-first-line message-point-line buffer))

(defdoc buffer
  :category "type"
  :usage "(make-buffer \"session-01\" :agent-name \"agent\")"
  :see-also (make-buffer buffer-name buffer-status buffer-first-message buffer-input-message))

(defdoc keymap
  :category "type"
  :usage "(make-keymap :default)"
  :see-also (make-keymap keymap-bind keymap-lookup keymap-name keymap-bindings keymap-parent *default-keymap*))

(defdoc command-metadata
  :category "type"
  :usage "(make-command-metadata :name 'send-message :permission :user-only)"
  :see-also (defcommand command-metadata-name command-metadata-permission *command-table*))

(defdoc tool-definition
  :category "type"
  :usage "(make-tool-definition :name \"lisp_eval\" :description \"Evaluate Lisp\" ...)"
  :see-also (register-tool execute-tool tool-definitions-for-api *tool-table*))

(defdoc stream-state
  :category "type"
  :usage "(make-stream-state)"
  :see-also (buffer-pending-stream provider-request-streaming zai-request-streaming))

(defdoc permission-denied
  :category "type"
  :usage "(error 'permission-denied :command 'some-command)"
  :see-also (check-permission permission-required *current-caller*))

(defdoc permission-required
  :category "type"
  :usage "(error 'permission-required :command 'some-command)"
  :see-also (check-permission permission-denied *current-caller*))

;;; ==========================================================================
;;; Category: prefix — Prefix command processing
;;; ==========================================================================

(defdoc *prefix-handlers*
  :category "prefix"
  :see-also (find-prefix-handler process-prefix-command shell-prefix-handler))

(defdoc find-prefix-handler
  :category "prefix"
  :usage "(find-prefix-handler TEXT:string) — (find-prefix-handler \"!ls -la\")"
  :returns "cons or nil — (\"!\" . #<FUNCTION SHELL-PREFIX-HANDLER>) or nil."
  :see-also (*prefix-handlers* process-prefix-command shell-prefix-handler))

(defdoc process-prefix-command
  :category "prefix"
  :usage "(process-prefix-command BUF:buffer INPUT-TEXT:string) — (process-prefix-command buf \"!ls\")"
  :returns "boolean — T if a prefix matched and handler was called, NIL otherwise."
  :side-effects "Calls the matching handler, which typically inserts a system message."
  :see-also (find-prefix-handler *prefix-handlers* send-message))

(defdoc shell-prefix-handler
  :category "prefix"
  :usage "(shell-prefix-handler BUF:buffer COMMAND-TEXT:string) — (shell-prefix-handler buf \"ls -la\")"
  :returns "message — The inserted system message containing the command output."
  :side-effects "Executes a shell command via /bin/sh in the buffer's working directory."
  :see-also (*prefix-handlers* process-prefix-command buffer-working-directory))

;;; ==========================================================================
;;; Category: editing — Interactive editing commands
;;; ==========================================================================

(defdoc send-message
  :category "editing"
  :usage "Bound to Return. Sends the current input to the agent."
  :returns "nil"
  :side-effects "Finalizes input, checks for prefix commands, starts streaming if no prefix."
  :see-also (buffer-finalize-input send-to-agent-with-context process-prefix-command))

(defdoc insert-newline-command
  :category "editing"
  :usage "Bound to C-j. Inserts a newline in the input message."
  :returns "nil"
  :side-effects "Splits the current line at point."
  :see-also (message-insert-newline send-message))

(defdoc beginning-of-line-command
  :category "editing"
  :usage "Bound to C-a. Moves point to the beginning of the current line."
  :returns "nil"
  :side-effects "Sets point offset to 0."
  :see-also (end-of-line-command message-move-beginning-of-line))

(defdoc end-of-line-command
  :category "editing"
  :usage "Bound to C-e. Moves point to the end of the current line."
  :returns "nil"
  :side-effects "Sets point offset to end of line."
  :see-also (beginning-of-line-command message-move-end-of-line))

(defdoc kill-line-command
  :category "editing"
  :usage "Bound to C-k. Kills from point to the end of the line."
  :returns "nil"
  :side-effects "Removes text and pushes it to the kill ring."
  :see-also (message-kill-line kill-backward-line-command yank-command))

(defdoc yank-command
  :category "editing"
  :usage "Bound to C-y. Yanks the top of the kill ring at point."
  :returns "nil"
  :side-effects "Inserts text from the kill ring."
  :see-also (message-yank yank-pop-command kill-line-command kill-ring-top))

(defdoc delete-char-backward-command
  :category "editing"
  :usage "Bound to Backspace/DEL. Deletes the character before point."
  :returns "nil"
  :side-effects "Removes one character or joins lines."
  :see-also (message-delete-char-backward delete-char-forward-command))

(defdoc delete-char-forward-command
  :category "editing"
  :usage "Bound to C-d. Deletes the character after point."
  :returns "nil"
  :side-effects "Removes one character or joins lines."
  :see-also (message-delete-char-forward delete-char-backward-command))

(defdoc forward-char-command
  :category "editing"
  :usage "Bound to C-f / Right. Moves point one character forward."
  :returns "nil"
  :side-effects "Advances point, wrapping to next line if at end."
  :see-also (message-forward-char backward-char-command forward-word-command))

(defdoc backward-char-command
  :category "editing"
  :usage "Bound to C-b / Left. Moves point one character backward."
  :returns "nil"
  :side-effects "Retreats point, wrapping to previous line if at start."
  :see-also (message-backward-char forward-char-command backward-word-command))

(defdoc forward-word-command
  :category "editing"
  :usage "Bound to M-f. Moves point forward to end of next word."
  :returns "nil"
  :side-effects "Advances point past non-word chars then word chars."
  :see-also (message-forward-word backward-word-command forward-char-command))

(defdoc backward-word-command
  :category "editing"
  :usage "Bound to M-b. Moves point backward to beginning of previous word."
  :returns "nil"
  :side-effects "Retreats point past non-word chars then word chars."
  :see-also (message-backward-word forward-word-command backward-char-command))

(defdoc kill-backward-line-command
  :category "editing"
  :usage "Bound to C-u. Kills from start of line to point."
  :returns "nil"
  :side-effects "Removes text and pushes it to the kill ring."
  :see-also (message-kill-backward-line kill-line-command yank-command))

(defdoc kill-word-command
  :category "editing"
  :usage "Bound to M-d. Kills from point to end of current word."
  :returns "nil"
  :side-effects "Removes text and pushes it to the kill ring."
  :see-also (message-kill-word backward-kill-word-command yank-command))

(defdoc backward-kill-word-command
  :category "editing"
  :usage "Bound to M-Backspace / C-w. Kills from beginning of current word to point."
  :returns "nil"
  :side-effects "Removes text and pushes it to the kill ring."
  :see-also (message-backward-kill-word kill-word-command yank-command))

(defdoc yank-pop-command
  :category "editing"
  :usage "Bound to M-y. Replaces just-yanked text with next kill ring entry."
  :returns "nil"
  :side-effects "Cycles through the kill ring."
  :see-also (message-yank-pop yank-command *kill-ring*))

(defdoc yank-previous-command-first-arg-command
  :category "editing"
  :usage "Inserts the first argument of the previous user command."
  :returns "nil"
  :side-effects "Inserts text at point."
  :see-also (yank-previous-command-last-arg-command))

(defdoc yank-previous-command-last-arg-command
  :category "editing"
  :usage "Bound to M-.. Inserts the last argument of the previous user command."
  :returns "nil"
  :side-effects "Inserts text at point."
  :see-also (yank-previous-command-first-arg-command))

(defdoc self-insert-command
  :category "editing"
  :usage "Invoked for printable characters. Inserts *self-insert-char* at point."
  :returns "nil"
  :side-effects "Inserts one character at point."
  :see-also (message-insert-char *self-insert-char*))

(defdoc *scroll-page-size*
  :category "editing"
  :see-also (scroll-up-command scroll-down-command buffer-scroll-offset))

(defdoc scroll-up-command
  :category "editing"
  :usage "Bound to M-v / Page Up. Scrolls history up (back) by one page."
  :returns "nil"
  :side-effects "Increases buffer-scroll-offset by *scroll-page-size*."
  :see-also (scroll-down-command *scroll-page-size* buffer-scroll-offset))

(defdoc scroll-down-command
  :category "editing"
  :usage "Bound to C-v / Page Down. Scrolls history down (forward) by one page."
  :returns "nil"
  :side-effects "Decreases buffer-scroll-offset by *scroll-page-size* (minimum 0)."
  :see-also (scroll-up-command *scroll-page-size* buffer-scroll-offset))

;;; ==========================================================================
;;; Category: buffer-command — Buffer management commands
;;; ==========================================================================

(defdoc new-buffer-command
  :category "buffer-command"
  :usage "Bound to C-x n. Creates a new chat buffer and switches to it."
  :returns "nil"
  :side-effects "Creates a new buffer, initializes face registry and keymap, adds to ring."
  :see-also (make-buffer add-buffer-to-ring kill-buffer-command))

(defdoc kill-buffer-command
  :category "buffer-command"
  :usage "Bound to C-x k. Kills the current buffer."
  :returns "nil"
  :side-effects "Removes the current buffer from the ring. Switches to the next buffer."
  :see-also (kill-buffer-from-ring new-buffer-command))

(defdoc next-buffer-command
  :category "buffer-command"
  :usage "Switches to the next buffer in the ring."
  :returns "nil"
  :side-effects "Rotates *buffer-ring*, moving current buffer to end."
  :see-also (*buffer-ring* minibuffer-select-buffer-command))

(defdoc save-session-command
  :category "buffer-command"
  :usage "Bound to C-x C-s. Saves the current buffer's conversation."
  :returns "nil"
  :side-effects "Writes conversation to JSON file. Inserts confirmation system message."
  :see-also (save-session load-session *sessions-dir*))

(defdoc toggle-tool-results-command
  :category "buffer-command"
  :usage "Bound to C-c t. Toggles visibility of tool result messages."
  :returns "nil"
  :side-effects "Flips buffer-show-tool-results-p."
  :see-also (buffer-show-tool-results-p))

(defdoc redraw-screen-command
  :category "buffer-command"
  :usage "Bound to C-l. Requests a full screen redraw in the active UI backend."
  :returns ":redraw"
  :side-effects "Causes the backend event loop to force a repaint of the current interface."
  :see-also (backend-run clawmacs-main))

(defdoc openai-codex-oauth-command
  :category "buffer-command"
  :usage "Starts the OpenAI Codex OAuth login flow."
  :returns "nil"
  :side-effects "Starts a localhost callback server, launches the browser login when possible, and sets the current buffer to :oauth status."
  :see-also (openai-codex-oauth-start openai-codex-oauth-finish *openai-oauth-pending*))

;;; ==========================================================================
;;; Category: packages — User-installed package loading
;;; ==========================================================================

(defdoc *packages-directory*
  :category "packages"
  :usage "Pathname — defaults to ~/.clawmacs.d/packages/"
  :see-also (clawmacs-use-package *user-init-file*))

(defdoc clawmacs-use-package
  :category "packages"
  :usage "(clawmacs-use-package :src-type :git :repo \"https://example.com/user/repo.git\")"
  :returns "Boolean — non-nil on successful install/load, NIL on warning or failure."
  :side-effects "Creates the package install directory when needed, runs git clone for missing packages, reads manifest.lisp from the package root, and loads the manifest entrypoint."
  :see-also (*packages-directory* *user-init-file* load-user-init-file))

;;; ==========================================================================
;;; Category: init — User init file
;;; ==========================================================================

(defdoc *user-init-directory*
  :category "init"
  :usage "Pathname — defaults to ~/.clawmacs.d/"
  :see-also (*user-init-file* *inhibit-user-init* load-user-init-file))

(defdoc *user-init-file*
  :category "init"
  :usage "Pathname — defaults to ~/.clawmacs.d/init.lisp"
  :see-also (*user-init-directory* *inhibit-user-init* load-user-init-file))

(defdoc *inhibit-user-init*
  :category "init"
  :usage "Boolean — set to T via --no-init flag to skip loading the user init file."
  :see-also (*user-init-file* load-user-init-file))

(defdoc *startup-hook*
  :category "init"
  :usage "List of function designators run after init.lisp loads and before backend startup."
  :see-also (add-hook remove-hook *initial-buffer-hook* clawmacs-main))

(defdoc *initial-buffer-hook*
  :category "init"
  :usage "List of function designators run with the initial buffer after it is created."
  :see-also (add-hook remove-hook *startup-hook* clawmacs-main))

(defdoc add-hook
  :category "init"
  :usage "(add-hook '*startup-hook* #'my-fn &key APPEND)"
  :returns "The function designator that was added."
  :side-effects "Mutates the hook variable named by HOOK-VAR."
  :see-also (remove-hook *startup-hook* *initial-buffer-hook*))

(defdoc remove-hook
  :category "init"
  :usage "(remove-hook '*startup-hook* #'my-fn)"
  :returns "The function designator that was removed."
  :side-effects "Mutates the hook variable named by HOOK-VAR."
  :see-also (add-hook *startup-hook* *initial-buffer-hook*))

(defdoc load-user-init-file
  :category "init"
  :usage "(load-user-init-file) — Called once during startup."
  :returns "T on success, NIL if skipped, inhibited, or on error."
  :side-effects "Loads and evaluates *user-init-file* in the :clawmacs package. Errors are caught, printed to stderr, and logged via file-debug-log. By the time init.lisp runs, the default keymap, tool registry, faces, and configured system prompt file are already loaded; use *startup-hook* or *initial-buffer-hook* for additional startup customization."
  :see-also (*user-init-file* *user-init-directory* *inhibit-user-init* *startup-hook* *initial-buffer-hook* clawmacs-main))

;;; ==========================================================================
;;; Category: ui-backend — Pluggable UI backend protocol
;;; ==========================================================================

(defdoc ui-backend
  :category "ui-backend"
  :usage "(defclass my-backend (clawmacs:ui-backend) ()) — Subclass to create a custom UI backend."
  :returns "CLOS class — Base class for all UI backends."
  :see-also (backend-run *ui-backend* croatoan-backend mcclim-backend))

(defdoc *ui-backend*
  :category "ui-backend"
  :usage "(setf clawmacs:*ui-backend* (make-instance 'my-backend)) — Set in ~/.clawmacs.d/init.lisp before startup."
  :see-also (ui-backend backend-run croatoan-backend mcclim-backend)
  :side-effects "When nil at startup, clawmacs-main sets it to a croatoan-backend instance.")

(defdoc backend-run
  :category "ui-backend"
  :usage "(backend-run BACKEND:ui-backend INITIAL-BUFFER:buffer) — Called by clawmacs-main to start the UI."
  :returns "nil — Returns when the user quits."
  :side-effects "Takes over the display. Sets *scroll-page-size*. Calls handle-key-event and update-streaming-response."
  :see-also (ui-backend *ui-backend* croatoan-backend mcclim-backend clawmacs-main))

(defdoc croatoan-backend
  :category "ui-backend"
  :usage "(make-instance 'croatoan-backend) — The default terminal backend using ncurses via croatoan."
  :returns "CLOS instance — Three-window terminal layout (main, modeline, minibuffer)."
  :see-also (ui-backend backend-run *ui-backend* mcclim-backend))

(defdoc mcclim-backend
  :category "ui-backend"
  :usage "(make-instance 'mcclim-backend) — McCLIM graphical backend. Load via (asdf:load-system :clawmacs/mcclim)."
  :returns "CLOS instance — Three-pane GUI layout (main, modeline, minibuffer)."
  :see-also (ui-backend backend-run *ui-backend* croatoan-backend))

;;; ==========================================================================
;;; Category: main — Application entry point
;;; ==========================================================================

(defdoc send-to-agent-with-context
  :category "main"
  :usage "(send-to-agent-with-context BUF:buffer) — (send-to-agent-with-context (current-buffer))"
  :returns "buffer — The buffer."
  :side-effects "Sets buffer status to :thinking. Starts a non-blocking streaming API call."
  :see-also (send-message build-conversation-messages provider-request-streaming))

(defdoc *prompt-max-tool-iterations*
  :category "main"
  :usage "Integer default used by run-single-prompt."
  :returns "integer — Default maximum non-interactive tool-call turns."
  :see-also (run-single-prompt clawmacs-prompt-main))

(defdoc run-single-prompt
  :category "main"
  :usage "(run-single-prompt PROMPT &key :agent-name :provider :model :think-level :max-tool-iterations :auto-approve-tools-p)"
  :returns "prompt-run-result — Final text, routing metadata, iteration count, and captured tool events."
  :side-effects "Creates an in-memory prompt buffer, sends non-streaming provider requests, executes agent-allowed tools, inserts tool_result messages into the prompt buffer, and loops until a final assistant response is returned."
  :see-also (clawmacs-prompt-main provider-request execute-tool build-conversation-messages))

(defdoc clawmacs-prompt-main
  :category "main"
  :usage "(clawmacs-prompt-main) — CLI entry point used by prompt.sh."
  :returns "does not return — Exits the Lisp image with status 0 or 1."
  :side-effects "Parses command-line options, initializes the clawmacs runtime without a UI backend, runs one prompt through run-single-prompt, and writes the final response or JSON result to stdout."
  :see-also (run-single-prompt *prompt-max-tool-iterations* clawmacs-main))

(defdoc clawmacs-main
  :category "main"
  :usage "(clawmacs-main) — Called once to start the application."
  :returns "nil — Returns when the user quits (C-x C-c)."
  :side-effects "Initializes state, loads the configured personality prompt file, loads user init, runs *startup-hook*, creates the initial buffer, runs *initial-buffer-hook*, then delegates to the UI backend via backend-run."
  :see-also (current-buffer *default-keymap* init-tools *ui-backend* *startup-hook* *initial-buffer-hook* backend-run))

;;; ==========================================================================
;;; Category: customize — Interactive face customization
;;; ==========================================================================

(defdoc *customize-face-state*
  :category "customize"
  :usage "Plist or nil. When non-nil: (:face FACE :label STRING :field-index INT :original-values ALIST :buffer BUFFER)"
  :see-also (customize-face-command make-customize-face-buffer handle-customize-key)
  :side-effects "Set by customize-face-command, cleared by customize-face-apply or customize-face-cancel.")

(defdoc *customize-face-fields*
  :category "customize"
  :usage "Constant list: (:foreground :background :bold-p :underline-p :reverse-p :parent)"
  :see-also (customize-face-field-value customize-face-field-label))

(defdoc cga-color-name
  :category "customize"
  :usage "(cga-color-name VALUE:integer) — (cga-color-name 4)"
  :returns "string — \"blue\""
  :see-also (format-color-spec-display make-color-spec))

(defdoc format-color-spec-display
  :category "customize"
  :usage "(format-color-spec-display CS:(or null color-spec)) — (format-color-spec-display (make-color-spec :cga 4))"
  :returns "string — \"blue (CGA 4)\" or \"(inherit)\" for nil"
  :see-also (cga-color-name color-spec))

(defdoc format-boolean-display
  :category "customize"
  :usage "(format-boolean-display VAL:(or null boolean)) — (format-boolean-display t)"
  :returns "string — \"yes\" or \"(inherit)\""
  :see-also (customize-face-field-display))

(defdoc format-face-parent-display
  :category "customize"
  :usage "(format-face-parent-display PARENT:(or null face)) — (format-face-parent-display nil)"
  :returns "string — \"(none)\" or face name like \"default\""
  :see-also (customize-face-field-display face-parent))

(defdoc customize-face-field-value
  :category "customize"
  :usage "(customize-face-field-value FACE:face FIELD:keyword) — (customize-face-field-value my-face :foreground)"
  :returns "t — The slot value (color-spec, boolean, face, or nil)"
  :see-also (customize-face-set-field-value customize-face-field-display))

(defdoc customize-face-set-field-value
  :category "customize"
  :usage "(customize-face-set-field-value FACE:face FIELD:keyword VALUE:t) — (customize-face-set-field-value my-face :bold-p t)"
  :returns "t — The new value"
  :side-effects "Modifies the face object in-place. Changes take effect immediately."
  :see-also (customize-face-field-value rebuild-customize-face-display))

(defdoc customize-face-field-label
  :category "customize"
  :usage "(customize-face-field-label FIELD:keyword) — (customize-face-field-label :bold-p)"
  :returns "string — \"Bold\""
  :see-also (*customize-face-fields* customize-face-field-display))

(defdoc customize-face-field-display
  :category "customize"
  :usage "(customize-face-field-display FACE:face FIELD:keyword) — (customize-face-field-display my-face :foreground)"
  :returns "string — Human-readable display of the field's current value"
  :see-also (format-color-spec-display format-boolean-display format-face-parent-display))

(defdoc customize-face-snapshot
  :category "customize"
  :usage "(customize-face-snapshot FACE:face)"
  :returns "alist — ((:foreground . #<COLOR-SPEC>) (:background . nil) ...)"
  :see-also (customize-face-restore-snapshot *customize-face-state*))

(defdoc customize-face-restore-snapshot
  :category "customize"
  :usage "(customize-face-restore-snapshot FACE:face SNAPSHOT:alist)"
  :returns "nil"
  :side-effects "Restores all face attributes from the snapshot alist."
  :see-also (customize-face-snapshot customize-face-cancel customize-face-revert-to-original))

(defdoc build-customize-face-content
  :category "customize"
  :usage "(build-customize-face-content FACE:face LABEL:string FIELD-INDEX:integer)"
  :returns "string — Multi-line text content for the customize buffer"
  :see-also (rebuild-customize-face-display make-customize-face-buffer))

(defdoc rebuild-customize-face-display
  :category "customize"
  :usage "(rebuild-customize-face-display)"
  :returns "nil"
  :side-effects "Updates the form message in the customize buffer from current state."
  :see-also (build-customize-face-content *customize-face-state*))

(defdoc customize-face-next-field
  :category "customize"
  :usage "(customize-face-next-field)"
  :returns "nil"
  :side-effects "Increments field-index in *customize-face-state* and rebuilds display."
  :see-also (customize-face-prev-field handle-customize-key))

(defdoc customize-face-prev-field
  :category "customize"
  :usage "(customize-face-prev-field)"
  :returns "nil"
  :side-effects "Decrements field-index in *customize-face-state* and rebuilds display."
  :see-also (customize-face-next-field handle-customize-key))

(defdoc customize-face-toggle-field
  :category "customize"
  :usage "(customize-face-toggle-field)"
  :returns "nil"
  :side-effects "Toggles boolean fields between t and nil. No-op for non-boolean fields."
  :see-also (customize-face-edit-field handle-customize-key))

(defdoc collect-all-faces
  :category "customize"
  :usage "(collect-all-faces)"
  :returns "list — Sorted list of plists with :face, :owner, :name, :label keys"
  :see-also (customize-face-command make-parent-selection-items))

(defdoc make-color-selection-items
  :category "customize"
  :usage "(make-color-selection-items)"
  :returns "list — Minibuffer items for CGA color palette selection"
  :see-also (customize-face-edit-field cga-color-name))

(defdoc make-boolean-selection-items
  :category "customize"
  :usage "(make-boolean-selection-items)"
  :returns "list — ((:value t :display \"yes\") (:value nil :display \"inherit (nil)\"))"
  :see-also (customize-face-edit-field))

(defdoc make-parent-selection-items
  :category "customize"
  :usage "(make-parent-selection-items CURRENT-FACE:face)"
  :returns "list — Minibuffer items for parent face selection, excluding CURRENT-FACE"
  :see-also (customize-face-edit-field collect-all-faces))

(defdoc customize-face-edit-field
  :category "customize"
  :usage "(customize-face-edit-field)"
  :returns "nil"
  :side-effects "Opens the minibuffer with field-appropriate options."
  :see-also (customize-face-toggle-field handle-customize-key minibuffer-activate))

(defdoc customize-face-apply
  :category "customize"
  :usage "(customize-face-apply)"
  :returns "nil"
  :side-effects "Closes the customize buffer, clears state. Changes persist on the face."
  :see-also (customize-face-cancel customize-face-revert-to-original))

(defdoc customize-face-cancel
  :category "customize"
  :usage "(customize-face-cancel)"
  :returns "nil"
  :side-effects "Reverts all face changes from snapshot, closes customize buffer."
  :see-also (customize-face-apply customize-face-restore-snapshot))

(defdoc customize-face-revert-to-original
  :category "customize"
  :usage "(customize-face-revert-to-original)"
  :returns "nil"
  :side-effects "Reverts all fields to their original values without closing the buffer."
  :see-also (customize-face-restore-snapshot customize-face-cancel))

(defdoc make-customize-face-buffer
  :category "customize"
  :usage "(make-customize-face-buffer FACE:face LABEL:string) — (make-customize-face-buffer my-face \"user:default\")"
  :returns "buffer — A new customize buffer added to the buffer ring"
  :side-effects "Sets *customize-face-state*. Kills existing customize buffer for this face."
  :see-also (customize-face-command *customize-face-state*))

(defdoc handle-customize-key
  :category "customize"
  :usage "(handle-customize-key KEY) — Called from handle-key-event when in customize mode."
  :returns "nil"
  :side-effects "Dispatches to field navigation, editing, apply, cancel, or revert."
  :see-also (*customize-face-state* customize-face-command))

(defdoc customize-face-command
  :category "customize"
  :usage "Bound to C-h F. Opens minibuffer face selector → customize buffer."
  :returns "nil"
  :side-effects "Activates minibuffer with face candidates. On selection, creates customize buffer."
  :see-also (collect-all-faces make-customize-face-buffer handle-customize-key))
