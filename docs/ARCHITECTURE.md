# Clawmacs Architecture

Clawmacs is a Lisp-native agent workbench. Its core architecture is deliberately
small: buffers display state, sessions persist state, commands expose human
interactions, tools expose agent interactions, packages install optional
capabilities, and hooks/advice let users alter behavior without patching the
core.

## Core Invariants

- `buffer-last-message` is always the editable input message. Finalizing input
  makes the old input read-only and appends a new editable input message.
- `message-raw-content` is the provider round-trip source of truth. Display
  text is derived from raw content when provider blocks are present.
- Commands are human-invoked operations and tools are agent-invoked operations.
  Tool schemas, permissions, approval displays, and provider names belong to
  the tool layer, not to `defcommand`.
- Package enablement resolves from broad to narrow scopes: global, agent,
  buffer. A narrower enabled scope makes the package active for that context;
  the default state is inactive unless inherited from a broader scope.
- Sessions are durable transcript identities. Buffers display session state,
  but transcript files and transcript rotation belong to the session layer.

## Core Concepts

### Messages

Messages are the linked text objects stored in buffers. A message owns its text
lines, sender, timestamp, raw provider content, rendering face set, and optional
metadata. Provider round-tripping uses `message-raw-content`; display uses the
plain text and face set.

Message code belongs in `src/message.lisp`. Higher layers should use message
accessors instead of editing line links directly unless they are implementing
message editing primitives.

### Buffers

A buffer is the visible working state for a chat, file, document, help page,
selector, or scratch area. It owns the message list, input message, scroll
state, routing overrides, display toggles, enabled package overrides, keymap,
session handle, and transient provider state.

Buffer state lives in `src/buffer.lisp`. The McCLIM interface renders buffers; providers
and packages should update buffers through buffer operations such as
`buffer-insert-system-message`, `buffer-insert-agent-message`,
`record-buffer-message`, and `buffer-finalize-input`.

### Rings

Clawmacs uses Emacs-style rings for current working sets. `*buffer-ring*` is the
ordered list of open buffers, with the current buffer at the front. The kill
ring stores recently killed text for yank/yank-pop.

Ring operations belong beside the data they operate on: buffer ring logic in
`src/buffer.lisp`, kill ring logic with message editing.

### Sessions And Transcripts

A session is the durable identity for a conversation. Buffers display sessions,
but sessions persist transcripts to disk. Transcript entries form a tree:
messages, compaction markers, branch summaries, labels, and model/think changes
all have stable ids and optional parent ids. The session manifest records the
current leaf, so loading a session replays only the active branch while keeping
older branches available for navigation or forking.

Compaction rotates transcript segments. The new transcript records a reference
to the previous transcript so context can be audited without carrying all
historical text in memory. Buffer snapshots remain compatibility/load-speed
artifacts for buffer-facing state such as agent names, package overrides,
provider overrides, and legacy raw-content normalization; tree sidecars are the
authoritative source for branch structure.

Session storage belongs in `src/session.lisp`; buffer save/load helpers live in
`src/buffer.lisp` because they serialize buffer-facing state. Prompt runners and
UI paths should record through the session API rather than writing transcript
files directly.

### Commands

Commands are interactive user operations. `defcommand` marks a function as
callable from command dispatch and stores user-facing metadata such as
docstring, lambda list, prompt readers, package ownership, and keybindings.
Commands should describe what appears in `M-x` style command discovery.

Command registry and metadata belong in `src/commands.lisp`. Command
implementations may live near the concept they operate on, or in the main UI
module when they are purely interactive glue.

### Tools

Tools are agent-callable operations. `deftool` attaches an input schema,
description, permission policy, approval display, package ownership, and an
executor function. Tool permissions are part of the tool layer because agents
can call tools directly, while commands are human UI affordances.

Tool registry and execution belong in `src/tools.lisp`. Package-specific tools
belong in their package entrypoint files.

### Packages

A package is an installed optional capability. Installed packages can be enabled
globally, for an agent, or for a buffer. Enablement is inherited from broad to
narrow scopes unless a narrower scope overrides it. Enabled packages may add
prompt text, commands, tools, docs, agents, hooks, and keybindings.

Package discovery, enablement, package-owned metadata queries, context
injection, and package help rendering belong in `src/package-manager.lisp`.
Built-in package entrypoints should stay focused on their own domain: `sexed`
for structural Lisp file editing, `slop` for source lookup, `netcons` for
network fetching, `git` for repository operations, `subagent` for delegation,
and `pipelines` for deterministic routing.

### Hooks And Advice

Hooks are named extension points where functions run in sequence. Advice wraps
named functions with before, after, or around behavior. These are the sanctioned
user extension mechanism for changing control flow without editing core source.

Hook/advice primitives belong in `src/hooks.lisp`. Core code should call named
hooks at meaningful boundaries: buffer creation, message insertion, prompt
submission, provider result handling, package enablement, and session save/load.

### Agents And Providers

An agent is Clawmacs routing and prompt configuration: name, provider, model,
think level, core prompt, personality prompt, and default tool allowlist. A
provider is the transport/runtime implementation that talks to an LLM service.

Agent/provider definitions and API normalization belong in `src/llm.lisp`.
Code that wants a model response should go through prompt-runner functions
rather than assembling provider requests directly.

### Prompt Runs

A prompt run is one deterministic execution of a prompt through an agent,
including tool loop handling, reasoning blocks, provider usage metadata, and a
final result. Prompt runs can be invoked from the interactive UI, `prompt.sh`,
pipelines, or subagents.

Prompt run data structures live in `src/prompt-core.lisp`. Provider/tool-loop
execution, streaming response handling, prompt-mode buffer setup, and
non-interactive prompt entrypoints live in `src/prompt-runner.lisp`. UI commands,
pipelines, and subagents should call prompt-runner APIs instead of assembling
provider requests directly.

### Pipelines

A pipeline is a deterministic graph of prompt stages. Each stage can route to a
next stage by static name or by a Lisp function that inspects stage output.
Pipelines let users define flows such as plan -> implement -> test -> plan on
failure.

Pipeline data structures, registry state, normalization, template rendering,
stage routing, and execution live in `src/pipelines.lisp`. Pipeline execution
remains provider-agnostic by calling the prompt-runner interface for each stage.

### Subagents

A subagent is a nested prompt run, either synchronous or asynchronous, usually
started by an agent through the `subagent` package. Async subagents return
process-local handles with status, result, error, timestamps, and cooperative
cancellation.

Subagent handle lifecycle belongs in `src/subagents.lisp`. Package tools should
format and expose that lifecycle; the core should own the registry and result
types.

### McCLIM Interface

The McCLIM interface renders buffers and translates CLIM input events into
command dispatch. `src/mcclim-app.lisp` owns the ESA-backed application frame,
CLIM command table, presentation translators, transcript pane, Drei-backed input
pane, fixed minibuffer stream, frame-local UI state, queued display-change
events, pulse polling, and redisplay. `src/render-core.lisp` owns pure rendering
geometry and string formatting that the frame draws into CLIM panes.

Keyboard editing remains owned by Clawmacs keymaps. McCLIM key-press events are
routed through a small CLIM command and then into the core command/keymap
dispatcher so keys like `C-u` keep their editor meaning instead of becoming ESA
numeric arguments. ESA still owns the application-frame/minibuffer shape and the
active command table for CLIM presentation interactions. Semantic objects drawn
in panes should be wrapped as CLIM presentations, and dynamic transcript output
should use stable output records or `updating-output` cache keys when the object
identity is known.

Core commands still use special variables for modal editor state because tests
and prompt-only entry points call them directly. The McCLIM frame owns a
`mcclim-ui-state` object and dynamically binds those specials from the frame
while rendering, handling input, or serving self-visibility tools. This keeps
selector, minibuffer, skill-completion, prefix, and scroll-page state local to
the frame without making the core command layer depend on McCLIM classes.

McCLIM code should not own prompt execution, package enablement, or provider
transport. It should ask the core to perform operations and then render the
resulting buffer state.

### Minibuffer And Selectors

The minibuffer is short-lived typed input for commands. Selectors are transient
buffer overlays for choosing commands, packages, models, sessions, buffers, or
docs. They are UI state, not durable session state.

Selector state and key handlers live in `src/minibuffer.lisp`. The McCLIM event
loop dispatches into that module when a selector or minibuffer is active, while
rendering remains in the McCLIM app and shared render helpers.

### Startup And Init

Startup creates registries, loads package metadata, loads user init files, makes
or loads the initial session buffer, then starts the requested UI or prompt
mode. User init files should configure packages, agents, models, hooks, advice,
faces, and keybindings through public APIs.

Runtime startup currently lives in `src/main.lisp`. CLI parsing and app
bootstrap should stay thin and call concept-specific initialization functions.

## Runtime Flows

### Interactive Send

The McCLIM event loop sends a key event to command dispatch. `send-message` finalizes
the buffer input message, runs send hooks, and either starts a normal prompt run
through `send-to-agent-with-context` or dispatches the active buffer pipeline.
The interface only polls stream state and re-renders buffer state.

### `prompt.sh`

The prompt CLI parses options in `main.lisp`, initializes runtime state, builds
or loads a prompt buffer, then calls `run-single-prompt`, `run-session-prompt`,
or `run-pipeline-prompt`. Prompt execution and tool loops stay in
`src/prompt-runner.lisp`; output formatting stays in the CLI glue.

### Tool Approval

Provider tool-use blocks become pending buffer approval state. Agent-allowed
tools execute immediately; permissioned tools wait for a human approval command.
Approved, denied, and failed calls are converted into canonical tool-result
messages through the buffer insertion API.

### Pipelines

A pipeline run selects its first stage, renders the stage prompt from the
original prompt and prior stage outputs, calls the prompt runner, records a
stage result, and resolves the next stage by static name or routing function.
Invalid routes fail the pipeline without escaping into UI code.

### Session Save/Load

Message insertion records transcript events when the buffer has a session.
Snapshots are autosaved through the buffer/session APIs. Compaction rotates the
transcript segment and inserts a reference to the previous transcript at the
start of the new one.

## Module Boundaries

The core load order should keep data definitions before behavior and UI:

1. Package and data structures: packages, faces, messages, sessions, buffers.
2. Registries and extension systems: debug logging, package manager, hooks,
   commands, keymaps, projects, skills.
3. Agent/provider/tool systems: matching, LLM providers, tools, prompt core,
   subagents, compaction, prompt runner, pipelines.
4. UI boundary: render core, minibuffer state, McCLIM application frame.
5. App glue: prefix dispatch, interactive commands, startup, CLI.
6. Package entrypoints and generated docs.

When adding behavior, prefer the narrowest owning module. `main.lisp` should be
the integration layer for event loop, command implementations, and startup, not
the home for long-lived core concepts.

## Refactor Direction

The codebase should continue moving toward these boundaries:

- Move pure data types and registries out of `main.lisp`.
- Keep command definitions interactive and tool definitions agent-facing.
- Keep package entrypoints package-scoped; do not hide core behavior in package
  prompts.
- Route prompt execution through a reusable prompt-runner interface.
- Keep McCLIM UI code passive: render buffers, collect input, call core APIs.
- Add hooks at stable boundaries before adding special-purpose conditionals.
- Preserve public exports and user init compatibility during module splits.

The first concrete split establishes `debug`, `prompt-core`, `prompt-runner`,
`pipelines`, `subagents`, `prefix`, and `minibuffer` modules. Package context
rendering now belongs to `package-manager`, tool metadata belongs to `tools`,
and buffer insertion/factory helpers belong to `buffer`. Remaining cleanup
should continue moving interactive-only command glue out of broad helper code
without changing public init APIs.
