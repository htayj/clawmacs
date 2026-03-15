# Clawmacs Next Features Plan

## Feature 1: More Tools (file read/write, shell exec with approval)

### 1a: file_read tool (:agent-allowed)
- Read a file within *sandbox-root* (defaults to working directory)
- Parameters: path (string), offset (optional int), limit (optional int)
- Validates path is within sandbox
- Returns file contents as text

### 1b: file_write tool (:agent-with-permission)
- Write content to a file within *sandbox-root*
- Parameters: path (string), content (string)
- Requires user approval (for now: auto-approve, add UI later)
- Validates path is within sandbox

### 1c: shell_exec tool (:agent-with-permission)
- Execute a shell command
- Parameters: command (string), timeout (optional int, default 30s)
- Requires user approval
- Returns stdout + stderr + exit code
- Runs within *sandbox-root* as working directory

## Feature 2: System Prompt Configuration

- Add *system-prompt* special variable (default: basic assistant prompt)
- Send as system parameter in the Anthropic API request
- Loadable from ~/.config/clawmacs/system-prompt.txt if it exists
- Buffer slot for per-session override

## Feature 3: Streaming Responses

- Use Anthropic streaming API (stream: true)
- Parse SSE events incrementally
- Update the agent message in the buffer as tokens arrive
- Requires moving the API call to a background thread (bordeaux-threads)
- Event loop polls for updates via short input timeout

## Feature 4: Multiple Buffers/Sessions

- Buffer ring (like Emacs buffer list)
- C-x b to switch buffers
- C-x k to kill buffer
- Each buffer has its own conversation, agent, face registry
- Modeline shows buffer name

## Feature 5: Persistent Conversation History

- Serialize buffer messages to JSON on exit / periodically
- Load on startup if session file exists
- Storage: ~/.config/clawmacs/sessions/<session-name>.json
- C-x C-s to save session
