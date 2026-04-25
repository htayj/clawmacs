# Clawmacs Parity TODO

This file tracks package-sized gaps between Clawmacs and the feature surfaces
in:

- `reference/external_src/codex`
- `reference/external_src/pi-mono`

I also looked at `reference/external_src/oh-my-pi`, but only as design
inspiration. It is not the parity baseline for this file.

Features Clawmacs already has and therefore are not listed here: session tree
navigation and forking, transcript persistence, hooks/advice, subagents,
pipelines, package enablement scopes, web search/fetch, basic git tools,
org-mode todo/project management, self-screenshot/debug visibility, image
rendering in assistant messages, Common Lisp structural editing, and Common
Lisp semantic lookup.

## P0

## `templata` - Slash Commands And Prompt Templates

Parity sources:
- `reference/external_src/pi-mono/packages/coding-agent/README.md`
- `reference/external_src/pi-mono/packages/coding-agent/docs/prompt-templates.md`
- `reference/external_src/codex/docs/slash_commands.md`

- [x] Add composer-level slash command dispatch before normal message send.
- [x] Add prompt-template resources discoverable from global, project, and
  package paths.
- [x] Support template metadata (`description`) and positional arguments
  (`$1`, `$2`, `$@`, slicing forms).
- [x] Add slash completion UI with descriptions and argument hints.
- [x] Ship slash wrappers for existing Clawmacs operations where that is more
  ergonomic than `M-x` (`/model`, `/session`, `/resume`, `/new`, `/export`,
  `/reload`).
- [x] Add reload support for prompt templates and slash resources without
  restarting Clawmacs.
- [x] Make package manifests able to contribute prompts/commands as first-class
  resources.
- [x] Add unit and McCLIM e2e coverage for template expansion, completion, and
  reload behavior.

## `quaestor` - Structured User Questions And Delivery Control

Parity sources:
- `reference/external_src/codex/docs/tui-request-user-input.md`
- `reference/external_src/pi-mono/packages/coding-agent/README.md`
- `reference/external_src/pi-mono/packages/coding-agent/docs/extensions.md`

- [ ] Add a built-in `request_user_input` tool contract for single-choice,
  multi-choice, and freeform prompts.
- [ ] Add a McCLIM overlay/buffer UI for answering one or more structured
  questions without dropping back to raw chat text.
- [ ] Persist answers as structured session entries so pipelines and subagents
  can resume cleanly.
- [ ] Add message queueing semantics while the agent is running:
  steering-message, follow-up-message, cancel-and-restore.
- [ ] Add commands/keybindings for queued-message inspection and recall.
- [ ] Expose a small Lisp API so packages/pipelines can suspend on user input
  and resume deterministically.
- [ ] Add tests for question flow, keyboard routing, answer persistence, and
  queued delivery behavior.

## `sessionarium` - Session Metadata, Resume Ergonomics, Export, Share

Parity sources:
- `reference/external_src/pi-mono/packages/coding-agent/README.md`
- `reference/external_src/codex/sdk/python/docs/getting-started.md`
- `reference/external_src/codex/sdk/typescript/README.md`

- [ ] Add persistent session display names separate from buffer names.
- [ ] Add "continue most recent session for this cwd" behavior in both CLI and
  UI command flows.
- [ ] Improve resume/fork selection by id prefix and explicit path.
- [ ] Add HTML export for the active session, including assistant/tool output,
  reasoning visibility choices, and attached artifacts.
- [ ] Add pluggable share handlers, with one initial implementation
  (private gist-like flow or user-provided hook).
- [ ] Add a session info view that shows path, ids, timestamps, token usage,
  cache stats, current branch/leaf, and model metadata.
- [ ] Support ephemeral/no-session runs as a first-class mode rather than a
  side-effect of ad hoc scripting.
- [ ] Add regression tests for export, resume, continue, fork-by-id, and
  session naming.

## `packrat` - External Package Lifecycle And Resource Loading

Parity sources:
- `reference/external_src/pi-mono/packages/coding-agent/docs/packages.md`
- `reference/external_src/pi-mono/packages/coding-agent/README.md`
- `reference/external_src/codex/docs/config.md`

- [x] Add user commands for package install, remove, update, list, and config.
- [x] Extend installs beyond bare git clone to support git refs, local paths,
  and npm-like package sources where practical.
- [x] Support global vs project-local package installation and settings.
- [x] Add package resource filtering so users can enable only selected tools,
  prompts, themes, hooks, or other resources from a package.
- [x] Make startup auto-install missing project-declared packages.
- [x] Add package resource types beyond the current tool/command/prompt-section
  surface, especially prompts and themes.
- [x] Add package doctor/status reporting for broken installs or stale package
  manifests.
- [x] Add tests for install/remove/update/filtering and package precedence
  between global and project scope.

## P1

## `modelaria` - Scoped Models, Roles, Usage Views

Parity sources:
- `reference/external_src/pi-mono/packages/coding-agent/README.md`
- `reference/external_src/codex/docs/config.md`
- `reference/external_src/codex/sdk/typescript/README.md`

- [ ] Add scoped model sets per session/project so model cycling is deliberate
  instead of global and flat.
- [ ] Add named model roles (`default`, `cheap`, `slow`, `plan`, `review`,
  etc.) that can be referenced by subagents and pipelines.
- [ ] Add a usage view for token totals, cache hits/misses, provider cost, and
  context pressure across the current session.
- [ ] Add transport/service-tier preferences where providers support them.
- [ ] Add commands to temporarily override a role for one turn, then fall back
  to the saved scope.
- [ ] Add tests for role resolution, cycling, session-local overrides, and
  usage aggregation.

## `guard` - Approval Policies And Sandbox Presets

Parity sources:
- `reference/external_src/codex/docs/config.md`
- `reference/external_src/codex/codex-rs/docs/codex_mcp_interface.md`
- `reference/external_src/codex/sdk/python/docs/api-reference.md`
- `reference/external_src/pi-mono/packages/coding-agent/docs/extensions.md`

- [x] Formalize approval policies as user-configurable data, not only per-tool
  ad hoc permissions.
- [ ] Add sandbox presets such as read-only, workspace-write, and full-access,
  with optional network toggles.
- [x] Add per-tool approval overrides stored in user/project config.
- [ ] Add working-directory policies and repo checks for agent runs that should
  stay inside a declared project root.
- [x] Add approval-reviewer hooks for programmatic clients and future app-server
  consumers.
- [x] Add an approval audit/history view so the user can inspect prior allow and
  deny decisions.
- [x] Add tests for policy resolution order, override precedence, and approval
  persistence.

## `artifactum` - Attachments And Artifact Buffers

Parity sources:
- `reference/external_src/pi-mono/packages/web-ui/README.md`
- `reference/external_src/codex/sdk/typescript/README.md`

- [ ] Add user attachment support for images and common document formats
  (PDF/DOCX/XLSX/PPTX at minimum).
- [ ] Add text extraction and preview metadata for non-image attachments.
- [ ] Add artifact message/buffer types for HTML, SVG, Markdown, JSON, and
  similar generated outputs.
- [ ] Let tools and packages create, update, and reference artifacts as durable
  session objects rather than only raw text.
- [ ] Include artifact references in session export/share flows.
- [ ] Add tests for attachment ingestion, artifact persistence, and artifact
  rendering in McCLIM.

## `interop` - App Server, RPC, SDK, Structured Output

Parity sources:
- `reference/external_src/codex/sdk/python/docs/getting-started.md`
- `reference/external_src/codex/sdk/python/docs/api-reference.md`
- `reference/external_src/codex/sdk/typescript/README.md`
- `reference/external_src/pi-mono/packages/coding-agent/README.md`

- [ ] Define a stable Clawmacs app-server/RPC surface instead of only shell
  wrappers like `prompt.sh`.
- [ ] Add thread/session APIs for start, resume, fork, read, and list.
- [ ] Add streamed event output for tool calls, tool results, assistant chunks,
  final usage, and interrupts.
- [ ] Add structured output / output schema support for single turns and
  pipeline stages.
- [ ] Add a non-interactive JSONL mode suitable for embedding and automation.
- [ ] Add a minimal Lisp client first; leave Python/TypeScript client shims as
  follow-on work if the protocol proves stable.
- [ ] Add tests for structured output validation, resume/fork behavior, and
  streamed event ordering.

## `mcp-bridge` - MCP Servers, Connectors, External Tool Bridges

Parity sources:
- `reference/external_src/codex/docs/config.md`
- `reference/external_src/codex/codex-rs/docs/codex_mcp_interface.md`

- [ ] Add stdio and HTTP MCP server configuration.
- [ ] Map external MCP tools into Clawmacs tool definitions with package
  ownership and approval metadata.
- [ ] Surface per-server and per-tool approval settings in the UI.
- [ ] Add commands to list, inspect, enable, disable, and doctor external MCP
  integrations.
- [ ] Add connector-style resource mentions where an external integration should
  surface a resource instead of raw tool text.
- [ ] Add tests for server discovery, tool registration, approval routing, and
  failure recovery.

## Internal Review Findings (2026-04-24)

These are not parity-package gaps. They are concrete correctness and design
problems observed while reviewing the current Clawmacs tree.

### P0

- [ ] Stabilize McCLIM input behavior against the offline e2e suite. The
  current worktree regressed basic input editing in addition to the already
  flaky mouse paths: `25-alt-f`, `30-alt-d`, `61-mouse-click-input-point`,
  `62-mouse-click-buffer-selector`, and
  `63-mouse-click-completion-candidates` are the relevant tests in
  `test-mcclim-e2e.py:1859`, `test-mcclim-e2e.py:1875`,
  `test-mcclim-e2e.py:1893`, plus the deterministic suite entries at
  `test-mcclim-e2e.py:3291-3293`, `test-mcclim-e2e.py:3323`, and
  `test-mcclim-e2e.py:3328`. The likely hot paths are the pointer/input bridge
  in `src/mcclim-app.lisp:3373-3641` and the recent input/theme edits in
  `src/mcclim-app.lisp` and `src/render-core.lisp`.
- [x] Persist each buffer's working directory in session snapshots and sidecar
  transcripts. `serialize-buffer` drops `working-directory`
  (`src/buffer.lisp:954-971`), and both loaders reconstruct buffers with
  `(truename ".")` instead of the original root
  (`src/buffer.lisp:1064-1068`, `src/buffer.lisp:1098-1102`).
- [x] Add buffer-type-specific persistence hooks. The buffer type registry only
  captures presentation metadata (`src/buffer.lisp:137-170`), while session
  serialization only stores generic buffer fields (`src/buffer.lisp:954-971`).
  Listener state is explicitly process-local (`src/listener.lisp:10-26`), so
  the new listener buffer cannot round-trip package/directory/history state and
  future package-defined buffers will hit the same wall.

### P1

- [x] Either implement `shell_exec` timeouts or remove `:timeout` from the
  contract. The tool parses `:timeout` and then explicitly ignores it
  (`src/tools.lisp:771-789`).
- [x] Make package enable/disable update prompt context symmetrically. Enabling
  appends package prompt content into an already-populated session
  (`src/package-manager.lisp:724-735`), but disabling only mutates enablement
  tables (`src/package-manager.lisp:484-520`). The live transcript and
  provider-facing conversation now retract package context cleanly even when a
  session already contained it, while tool execution is still correctly
  blocked for inactive packages (`src/tools.lisp:587-670`).
- [x] Stop swallowing session manifest decode failures. `read-session-manifest`
  now returns a structured parse error object for malformed manifests, direct
  load paths signal that error, and saved-session discovery warns instead of
  silently dropping bad sidecars. That turns corruption and partial writes into
  explicit, reviewable failures instead of unexplained fallback behavior.

## Notes From `oh-my-pi`

These are not counted as parity items for this file, but they are worth using
as implementation inspiration once the packages above exist:

- autonomous memory
- richer browser/SSH/Python tooling
- AI-assisted commit tooling and structured code review
- broader multi-tool config discovery
