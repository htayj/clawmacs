# Clawmacs Architecture

Clawmacs should feel like a Lisp machine editor with an agent attached, not a
chat client with editor features bolted on. The architecture is strongest when
it stays buffer-centric, registry-driven, and easy to extend from Lisp.

## Design Rules

- Buffers are the primary mutable unit.
- Sessions are durable history, not UI state.
- Commands are for user interaction.
- Tools are for agent interaction.
- Packages extend registries; they should not patch core control flow.
- McCLIM is an adapter over core state, not the owner of core behavior.
- Hooks and advice are the normal extension path.

## Core Model

### Messages

`src/message.lisp` owns editable text objects and provider round-trip content.
Message primitives should stay small and mechanical.

### Buffers

`src/buffer.lisp` is the center of the application. A buffer owns:

- the message chain
- the current editable input
- buffer-local routing and display toggles
- package overrides
- keymap and editor state
- the attached session handle

If a feature is about "what this working context currently is", it probably
belongs on the buffer.

### Sessions

`src/session.lisp` owns durable transcript identity and branching history.
Buffers display a session; sessions persist it. Session code should not know
about panes, selectors, or input widgets.

### Commands

`src/commands.lisp` should stay a thin registry and metadata layer.
Command bodies should live next to the subsystem they manipulate. `defcommand`
should mean "this existing function is interactive", not "define a miniature
framework inside a macro".

### Tools

`src/tools.lisp` owns agent-callable function metadata, schemas, exposure, and
execution. Tools are not commands with extra flags. They are a separate
surface with different invocation contracts. Tool exposure, agent tool lists,
package enablement, and immutable per-run snapshots compose a workflow; none is
an authorization or containment boundary.

Clawmacs is intentionally full-trust. Init files, packages, tools, shell
commands, and live Lisp run with the authority of the Clawmacs process. Users
who need containment should place the whole process inside an external sandbox,
container, VM, or restricted account. Isolated evaluator workers and managed
background operations protect frame liveness and process stability, not data or
network access.

### Packages

`src/package-manager.lisp` owns discovery, installation, enablement, and package
resource loading. Packages should register commands, tools, prompt sections,
slash commands, agents, hooks, and buffer types. That is the main "lispy"
extension story.

### Prompt Runtime

`src/prompt-runner.lisp` should own one thing: turning buffer/session state into
a prompt run, handling tool loops, and feeding results back into the buffer.

`src/llm.lisp` should support that runtime by owning provider transport,
conversation encoding, auth, and agent/provider definitions.

### UI

`src/mcclim-interface.lisp` owns the fresh presentation-based chat frame. It
should stay as a CLIM adapter over buffer state: display functions render
messages, commands mutate buffers, and async updates request normal McCLIM pane
redisplay. `src/windows.lisp` and `src/minibuffer.lisp` are legacy logical UI
support and should not become feature-policy owners.

## Current Boundaries

The healthy layering is:

1. `message` and `session`: durable data and transcript logic
2. `buffer`: live working state
3. `commands`, `tools`, `hooks`: interaction registries
4. `llm`, `prompt-runner`, `pipelines`, `subagents`: execution runtime
5. `package-manager` and package entrypoints: extensions
6. UI modules: presentation and event routing

When code crosses those layers, it should do so through ordinary functions and
registries, not by reaching into unrelated globals.

## Simplification Priorities

### 1. Shrink `main.lisp`

`src/main.lisp` is still carrying too many responsibilities: command dispatch,
editor commands, project/package/model selectors, slash completion, skill
completion, and other interactive glue. It should move toward:

- startup/bootstrap
- top-level command wiring
- very small composition helpers

Command bodies should migrate to the modules they act on. Selector-specific
logic should live with the selector/minibuffer subsystem.

### 2. Make UI state truly local

`src/minibuffer.lisp` still defines a large amount of global selector state.
That is transitional architecture. The more emacsy shape is:

- buffer-local state for editor behavior
- frame-local state for transient UI overlays
- fewer process-global specials for modal interaction

The core should operate on explicit state objects where possible, with dynamic
binding used as compatibility glue rather than the primary design.

### 3. Split `llm.lisp` by concern

`src/llm.lisp` currently mixes:

- provider configuration
- OAuth and token storage
- agent definitions and prompt composition
- conversation encoding
- retry policy
- provider request code
- streaming normalization

That file wants at least conceptual submodules such as provider transport,
OpenAI auth, agent definitions, and conversation building. The current design
works, but it is too much gravity in one place.

### 4. Split package management into two layers

`src/package-manager.lisp` mixes:

- channel/package discovery
- install/update/remove behavior
- enablement state
- runtime resource registration
- prompt-context injection
- help/doctor/status presentation

Those are related, but not the same job. A simpler shape would separate:

- package source/install logic
- package activation/runtime registry logic

### 5. Stop treating docs as one giant runtime file

`src/docs.lisp` is useful, but it is effectively a giant registry payload. It
should stay data-like. The more lispy direction is either:

- smaller doc shards by subsystem, or
- generated/loaded doc data

so that the help system is still rich without one huge mixed source file.

## What "Emacsy And Lispy" Means Here

- Put mutable editor state on buffers and frames, not in ambient globals.
- Prefer small functions over one giant orchestrator file.
- Keep macros declarative; keep behavior in ordinary functions.
- Extend through registries, hooks, advice, and buffer types.
- Let packages add capabilities without teaching core about each package.
- Keep the UI adapter thin and the model explicit.

## How To Add Features

When adding a new feature, prefer this order:

1. define or extend the core data it needs
2. expose ordinary functions over that data
3. register commands or tools as thin entrypoints
4. add hooks if users will reasonably want to alter it
5. render it through presentations or buffer types

If a feature starts in `main.lisp`, it is often being added too high in the
stack.
