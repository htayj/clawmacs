(in-package :clawmacs)

(register-package-prompt-section
 "speculum"
 "## McCLIM self-visibility with speculum

- Use the `speculum_*` provider tools when you need to observe the running
  Clawmacs McCLIM window, capture a screenshot, or inspect current GUI state.
- `speculum_screenshot` captures the current McCLIM window as a PNG and
  returns the file path plus frame/render metadata.
- `speculum_window_state` returns structured McCLIM frame, pane, render,
  buffer, minibuffer, selector, and interaction state without needing pixels.
- `speculum_inspect` reads a fixed allowlist of McCLIM/Clawmacs state names.
  It does not evaluate arbitrary Lisp variables.
- Prefer these tools over `lisp_eval` for normal GUI self-observation and
  McCLIM frame/window inspection. Use `lisp_eval` only when the allowlisted
  self-visibility tools cannot answer the question."
 :title "McCLIM self-visibility with speculum"
 :package "speculum")

(deftool speculum-tool-screenshot
  :name "speculum_screenshot"
  :description "Capture the current Clawmacs McCLIM window as a PNG and return the path plus frame/render metadata."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((path :type "string" :required nil
               :description "Optional sandbox-local PNG path. Defaults to screenshots/speculum/speculum-<time>.png.")
         (refresh :type "boolean" :required nil
                  :description "When true or omitted, request a McCLIM redisplay before capture.")
         (window-id :type "string" :required nil
                    :description "Optional X11 window id for tests/debugging. Omit to find the Clawmacs window.")))

(deftool speculum-tool-window-state
  :name "speculum_window_state"
  :description "Return structured state for the current Clawmacs McCLIM frame, panes, render snapshot, and interaction state."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((scope :type "string" :required nil
                :description "One of summary, frame, panes, render, interaction, or all. Defaults to all.")
         (message-limit :type "integer" :required nil
                        :description "Maximum recent transcript messages to include. Defaults to 10.")))

(deftool speculum-tool-inspect
  :name "speculum_inspect"
  :description "Inspect allowlisted McCLIM/Clawmacs GUI state names without arbitrary Lisp evaluation."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((names :type "array" :required nil
                :items ((:type . "string"))
                :description "Allowlisted state names. Omit to return the allowlist.")
         (message-limit :type "integer" :required nil
                        :description "Maximum recent transcript messages for message-related state. Defaults to 10.")))
