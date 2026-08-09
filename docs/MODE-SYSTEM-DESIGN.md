# RPLACA Interaction Mode System

- **Status:** accepted v1 design; implementation pending
- **Decision date:** 2026-07-14
- **Scope:** general, package-extensible interaction modes for chat workflows
- **Not in scope:** a built-in plan mode, permission prompts, or an application
  sandbox

## Decision

RPLACA will expose a general user-facing **Mode** feature. A mode is a named,
package-owned workflow composition that can:

1. contribute an additive section to the agent prompt;
2. narrow the tools offered to the model for a run; and
3. make registered commands applicable only in selected modes.

The core ships a permanent neutral mode named `normal`. A package may use this
contract to implement planning, research, review, tutoring, or another
workflow. Core RPLACA will not hard-code Codex- or Claude-style plan mode.

A mode is not an authority boundary. RPLACA deliberately follows a
full-trust, Pi-like execution posture: extensions and tools run with the
authority of the RPLACA process. Users who need containment should launch
RPLACA inside an external sandbox or container that they control. The mode
API must not use words such as `permission`, `approval`, `safe`, `sandbox`, or
`read-only guarantee` for ordinary workflow filtering.

## Why This Is a Separate Concept

The repository already uses “mode” in other senses. The new API must not
overload them:

| Concept | Owner | Meaning |
|---|---|---|
| `buffer-major-mode` | buffer/editor | File or view kind such as Lisp, scratch, help, or artifact |
| minibuffer interaction state | frame | Short-lived selector/completion state |
| agent definition | agent registry | Model role, prompt, and default tools for an agent |
| pipeline | pipeline package | A multi-stage operation |
| **interaction mode** | conversation buffer/session | The durable workflow policy composed into each run |
| `listener-input-mode` | listener context | Whether a bare interactor line defaults to Lisp evaluation or agent prose |

The Lisp name is therefore `interaction-mode`; the concise UI label is
“Mode”. The info line should show major mode and interaction mode separately.

`listener-input-mode` is an independent input-dispatch axis. Its values are
`:eval` and `:say`, and it changes only the meaning of a bare interactor line
and the prompt suffix. It does not contribute prompt text, filter tools, scope
commands, persist an agent workflow policy, or change the selected
`interaction-mode`. Likewise, selecting an interaction mode does not switch
the listener between Lisp and Say input. The two settings may vary in any
combination.

## V1 Boundaries

V1 intentionally has a small algebra:

- A conversation buffer has exactly one selected interaction mode.
- `normal` is always available and has no prompt or tool effects.
- Modes do not inherit from or compose with other modes.
- There are no minor modes, mode stacks, or one-turn overrides.
- There are no mode enter/leave hooks or mode-local mutable state.
- A mode cannot select a provider, model, reasoning level, agent, or pipeline.
- A mode cannot add tools or alter a tool schema. A package registers tools by
  the normal tool mechanism; its mode can only narrow an already eligible set.
- A mode cannot replace the base prompt. Its contribution is additive and is
  placed at one defined point.
- Mode filtering controls the agent workflow only. Trusted Lisp code, shell
  tools, and plugins are not contained by it.

This keeps the contract deterministic and makes a selected mode durable across
session navigation without inventing another extension runtime.

## Ownership Model

The mode system has four distinct layers of state:

| Layer | Owner | Lifetime | Contents |
|---|---|---|---|
| Definitions | process registry | package generation | Immutable package-owned mode definitions |
| Selection | conversation buffer | buffer lifetime | Selected mode name |
| History | session branch | session lifetime | Durable mode-change events |
| Effective run | buffer runtime or lexical headless run | one user turn and its tool continuations | Sealed prompt and tool contributions |
| Lifecycle repair | buffer runtime | until the next safe turn boundary | Optional internal fallback-to-`normal` marker after package loss |

The application frame owns only transient selector state and rendering. It is
not the source of truth for the selected or effective mode.

There must be no process-global “current mode” special variable. Such a
binding would leak between concurrent buffers and would not reliably cross the
GUI worker boundaries already used by the prompt runner. The durable selection
lives on the buffer; the effective semantics live in an immutable run context.

## Definition Contract

The conceptual data shape is:

```lisp
(defstruct interaction-mode-definition
  name
  title
  description
  prompt-contributor
  tool-filter
  package)
```

The minimum registration surface is:

```lisp
(define-interaction-mode plan
  :title "Plan"
  :description "Explore and produce an implementation plan."
  :prompt "Do not begin implementation until the user requests it."
  :tool-filter #'plan-mode-tool-filter)
```

The public v1 operations should be:

```lisp
register-interaction-mode
define-interaction-mode
find-interaction-mode
list-interaction-modes
remove-interaction-modes-for-package
interaction-mode-available-p
buffer-interaction-mode-name
buffer-interaction-mode
set-buffer-interaction-mode
```

`list-interaction-modes` should accept `:buffer` and `:include-inactive` so the
same query supports UI completion, diagnostics, and tests without duplicating
package visibility rules.

### Names and ownership

- Normalize names to non-empty, lowercase strings at registration.
- Reject blank names and malformed titles, descriptions, or callback
  designators.
- Reserve `normal`. It cannot be replaced, removed, or owned by a package.
- Infer package ownership from `*current-rplaca-package*`; package code must
  not be able to spoof a different owner in the public macro.
- A name collision between different owners signals an error.
- The same owner may replace its definition during a package reload.
- Capture or resolve callback function objects when a run is prepared. Never
  call a user callback while holding a registry, package, buffer, or runtime
  lock.

If package resource-type filtering remains useful after removal of the old
permission system, `:interaction-mode` may be a package composition resource
kind. It must not be described or implemented as a security control.

### Availability

A mode is selectable for a buffer only when:

1. its definition exists;
2. it is `normal`, or its owning package is active for that buffer; and
3. the buffer is a conversation buffer that can run an agent turn.

Selecting a package mode must not implicitly enable its package. Package
activation remains an explicit, separately visible operation.

## Contributor Contract

Mode contributors receive an immutable `interaction-mode-context`. It contains
copied values, not a live buffer or frame:

- mode name, title, and owning package;
- buffer name and buffer kind;
- current working directory and project identity/path;
- selected agent name;
- session name or identifier when present; and
- buffer-runtime generation.

The context intentionally omits mutable panes, streams, output records, and a
buffer object. Contributors are pure composition callbacks. They must not
perform UI work or rely on a dynamically current frame.

### Prompt contributor

`prompt-contributor` may be `nil`, a string, or a function of one context. A
function returns a string or `nil`.

- `nil` contributes nothing.
- An empty or whitespace-only result contributes nothing.
- A non-empty result becomes one clearly labelled interaction-mode section.
- The section is computed once when the run context is captured.
- A contributor cannot delete, reorder, or replace other prompt sections.

### Tool filter

`tool-filter` may be `nil` or a function of `(context baseline-tool-names)`.

- A `nil` filter is the identity function.
- The input is a fresh, normalized sequence.
- The returned names must be a subset of the input.
- The callback chooses membership, not ordering. Project accepted names back
  onto baseline order before constructing the run context.
- Returning `nil` intentionally exposes zero tools for that run.
- Unknown names, duplicates after normalization, malformed output, or attempts
  to restore an excluded tool signal an `interaction-mode-contribution-error`.
- Filtering happens once before provider I/O.

The callback is trusted extension code. Validation contains ordinary mistakes
and makes behavior reproducible; it is not a sandbox.

## Deterministic Run Composition

Mode behavior must be sealed before the provider sees the finalized user turn.
V1 defines this tool pipeline:

```text
registered and temporary tools
        -> tools visible from active packages
        -> explicit per-run tool selection, if supplied
           otherwise the selected agent's tool selection
        -> selected interaction mode's narrowing filter
        -> exact provider tool vector and dispatch-eligibility set
```

Each layer can only preserve or narrow the previous layer. A plugin that wants
new tools registers them through the ordinary tool registry and activates the
owning package; the mode filter does not manufacture tools.

The exact filtered vector must drive all three consumers:

1. the tool schemas sent to the provider;
2. the tools section rendered into the system prompt; and
3. tool-call dispatch eligibility for that run.

This prevents a split-brain state in which the prompt advertises one set, the
provider receives another, and execution consults a third. If a provider
fabricates a call to a filtered-out tool, dispatch reports that the tool is not
available in the current workflow. It must not call that a permission denial
or security event.

The system-prompt order is also fixed:

1. boot/project instruction files;
2. the core RPLACA prompt;
3. active-package prompt sections;
4. the selected interaction-mode section;
5. the exact filtered tools section;
6. skills;
7. personality; and
8. the runtime footer.

Existing prompt components may retain their internal ordering. The mode
section appears exactly once, after package material is available and before
the tool contract it helps explain.

## Immutable Run Context

The runtime should publish an immutable `interaction-run-context` with at
least:

```lisp
mode-name
mode-title
mode-package
mode-context
prompt-section
provider-tools
eligible-tool-names
runtime-generation
```

`eligible-tool-names` may have an internal set representation in addition to a
stable vector, but the public/readable form should preserve deterministic
order.

### Capture boundary

Capture occurs:

1. after the buffer's active packages have loaded;
2. after agent and explicit per-run tool selection is known;
3. before constructing the provider request or starting its worker; and
4. before the finalized user turn can lead to provider I/O.

Registry access takes a bounded snapshot under its lock. Contributor callbacks
then run outside locks. If all contributions validate, publish the completed
run context under the buffer-runtime lock. A partial context is never visible.

### Lifetime

One run context covers:

- the initial provider request for one user turn;
- every provider/tool continuation caused by that turn; and
- steering messages delivered into that still-running turn.

It ends on final response, cancellation, error, explicit stop, or buffer
runtime disposal. Cleanup belongs in `unwind-protect` paths.

A queued follow-up is a new user turn. Even if dispatched immediately after a
completion, it must clear and recapture the context. A registration reload or
buffer selection change cannot mutate an already published context.

Do not rely only on `*active-tool-names*`. A dynamic binding works for some
headless paths but is not a durable contract across GUI and provider worker
threads. Compatibility bindings may be derived from the run context at legacy
call sites while the context remains authoritative.

## Switching Contract

`set-buffer-interaction-mode` is the only state-changing helper. Its operation
is atomic from the caller's perspective:

1. normalize the requested name;
2. verify that the buffer is a conversation buffer;
3. verify that the buffer can run an agent turn;
4. verify that no agent run is busy;
5. return without events or redisplay if the name is already selected;
6. update the durable selection;
7. append the session event when a session is attached;
8. autosave the buffer/session state;
9. synchronize any synthetic system-prompt representation;
10. call `notify-buffer-display-change` with `:interaction-mode`; and
11. emit a short, non-durable user feedback message.

Validation occurs before mutation. An unknown, inactive, or busy switch leaves
the old selection and session tree untouched.

User switches while `buffer-agent-busy-p` is true are rejected rather than
staged. “Switch after this turn” creates hidden queue semantics and is outside
v1. Package-loss reconciliation is the one internal exception. If an owning
package becomes unavailable during a run, the runtime records a pending
fallback marker. At the normal turn-finalization boundary, before any queued
follow-up is dispatched, it durably switches to `normal` and appends the
fallback event. The in-flight run continues under its sealed definition; no
session event is appended concurrently in the middle of that turn.

Expected conditions include:

```lisp
interaction-mode-error
interaction-mode-invalid
interaction-mode-unavailable
interaction-mode-busy
interaction-mode-contribution-error
```

Callers may present these as ordinary workflow errors. None represents a
security boundary.

## Session and Snapshot Persistence

### Buffer snapshot

Add `:interaction-mode` to the serialized buffer state. Missing fields from
legacy snapshots mean `normal`.

A detached/snapshot buffer should copy the selected name for diagnostics and
export, but agent execution must always consult a newly captured run context.

### Durable event

Use an append-only session event:

```json
{
  "event": "interaction-mode-change",
  "mode": "research",
  "reason": "user"
}
```

The event advances the session leaf and becomes the parent of the next user
message. It is not retroactive. A branch before the event uses the earlier
mode; a branch after it uses the new mode.

Keep mode reduction separate from the existing provider/model/think-level
reducer. A helper such as `session-branch-interaction-mode` walks the selected
branch and returns `normal` when no event exists. This avoids changing an
existing multi-value return contract merely to add unrelated state.

The session selector should recognize and describe the event. Like
`model-change` and `think-level-change`, it can be hidden in the default
message-focused tree and shown by the “all events” view.

### Restore and fork

Mode state must be restored consistently by:

- ordinary session load/resume;
- session-tree navigation;
- GUI session fork;
- headless session fork; and
- interop thread fork/resume.

A fork inherits the effective mode at the exact selected branch, not whatever
the source buffer happens to select later.

Load active packages before validating a restored package-owned mode. If the
definition remains unavailable after package loading, select `normal`, append
one durable fallback event with reason `package-unavailable`, autosave, and
show one non-durable warning. Recording the fallback prevents the same phantom
mode from reappearing on every load.

Malformed legacy data should warn and recover to `normal`; it must not crash
the frame or loader.

## Package Lifecycle

Mode definitions follow the existing package-owned registry pattern.

- Add `remove-interaction-modes-for-package` to
  `%reset-package-runtime-state`.
- Snapshot registry contents under a dedicated bounded lock.
- Never invoke mode callbacks while holding that lock.
- On a normal safe reload, do not reconcile live buffers during the temporary
  remove/reload gap.
- After the package generation completes, keep selections whose names were
  successfully re-registered.
- If reload fails, or a package is disabled/uninstalled, reconcile affected
  idle buffers to `normal` and record reason `package-unavailable`.
- For an affected busy buffer, set only the runtime-owned pending fallback
  marker. Apply and persist it at turn finalization before dispatching queued
  follow-ups. This marker is an internal lifecycle repair, not a user-facing
  mode queue.
- A currently running turn keeps its sealed old run context. Future turns use
  the reconciled selection.

The registry owns definitions; packages do not own buffer selection objects or
frame UI objects. That division makes package reload cleanup bounded and
prevents callbacks from outliving their generation.

## Mode-Scoped Commands

Extend command metadata and `defcommand` with optional
`:interaction-modes` metadata:

```lisp
(defcommand finish-research-command
  :interaction-modes (research))
```

- Omitted or `nil` means applicable in all modes.
- A non-empty normalized list restricts the command to those modes.
- `list-available-commands` filters M-x candidates by the current buffer mode.
- `invoke-command` repeats the check so a stale selector result or key binding
  cannot invoke a command outside its declared workflow.
- Package activity and mode applicability are independent checks; both must
  pass.

V1 does not add dynamically composed mode keymaps. Existing `:keys` metadata
and the normal keymap remain in force, with invocation-time applicability as
the final check. Mode keymaps, precedence, conflict resolution, and displayed
binding inheritance need a separate design if a concrete plugin requires
them.

## CLIM-Native Interface

The UI uses standard CLIM semantics and the existing ESA/Drei integration.

### Semantic selector

Define an `interaction-mode-ref` presentation type for the current mode label.
Its object contains the mode name and enough buffer/generation identity to
reject a stale record. A presentation-to-command translator invokes a listener
command that accepts a listener-native presentation argument.

The listener command completes from `list-interaction-modes :buffer ...` in the
CLIM interactor. Each candidate is an immutable presentation object, and the
command delegates to `set-buffer-interaction-mode`. Keyboard and pointer
selection reach the same command/helper boundary without raw pointer handling.

### Command tables and menu

Add a stable `Mode` command to the `rplaca-listener` command table. Its entry is
state-independent, so the stable application command table remains appropriate
in v1.

If a future feature needs a menu whose entries genuinely vary by mode, build a
fresh frame-local command table and install it once at the mode transition.
Do not mutate the process-global `rplaca-listener` table. The existing guard
against repeatedly reinstalling an identical frame command table must remain
intact.

### Display and redisplay

Show both values in the listener wholine, for example:

```text
CL-USER>   Mode: research   idle   openrouter/model
```

Render the interaction-mode label as an
`interaction-mode-ref` presentation. Use `updating-output` with a stable unique
ID derived from the buffer and a cache value containing the selected name and
availability state. The display function reads buffer/domain state only; it
does not change the mode.

Configure the wholine for incremental redisplay if it is not already. Verify
that enabling output-record reuse there does not duplicate status text or leave
stale model/busy labels; the complete line's cache value must cover every field
inside the updated record.

Mode transitions call the existing `notify-buffer-display-change` path. The
frame queues a normal redisplay request, and CLIM incrementally updates the
wholine and any other affected records. Do not add:

- `window-clear` plus redraw;
- a private repaint loop;
- direct output-record mutation;
- raw medium or sheet manipulation;
- coordinate-based hit testing; or
- callbacks that write panes from provider/package worker threads.

Portable CLIM concepts here are frames, panes, commands, command tables,
presentation types, translators, and incremental output recording.
The CLIM interactor, listener-native presentation completers, and frame wake
handler are McCLIM-specific implementation choices and should be labelled as
such in code comments where relevant.

## CLI and Interop Contract

The headless CLI should accept:

```text
--mode NAME
```

The corresponding Lisp prompt/session entry points should accept `:mode`.

- For a one-shot prompt, select and validate the mode before finalizing the
  user turn and before provider I/O.
- For a session prompt, `:mode` is a durable switch before appending the user
  message, not a temporary dynamic binding.
- An unknown or inactive mode is a preflight error with no provider call.
- Result summaries and structured output metadata include the effective mode.

The stdio interop protocol should accept a mode parameter on the operations
equivalent to `thread.start`, `turn.start`, and `thread.run`, and expose the
current selection in `thread.read` summaries. A concurrent attempt to change
mode while the thread is busy returns a typed busy error.

This is an extension of the existing in-process/JSONL interop surface. It does
not introduce a provider SDK, HTTP service, or general public API; those remain
deferred while the application is in flux.

## Failure Semantics

The mode subsystem favors explicit preflight failure over silently changing a
run:

| Failure | Required behavior |
|---|---|
| Invalid or colliding registration | Signal before publishing the definition |
| Unknown/inactive selection | Leave old mode and history unchanged |
| User switch while busy | Reject; do not queue or partially switch |
| Prompt contributor error | Abort before provider I/O; retain selected mode |
| Invalid tool-filter output | Abort before provider I/O; retain selected mode |
| Provider fabricates filtered tool | Report tool unavailable in this workflow |
| Package removed/reload failed | Durable fallback to `normal` for future runs |
| Legacy/corrupt persisted mode | Warn and recover to `normal`, without crashing |
| Cancellation/error/disposal | Release run context through unwind cleanup |

Do not silently continue in `normal` after a contributor fails. That would
send a materially different prompt/tool contract from the one the user
selected. Package unavailability is different: the definition no longer
exists, so a durable and visible fallback is required.

## Implementation Plan

Implementation should proceed in narrow, independently testable phases.

### Phase 1: Registry and immutable definitions

- Add `src/modes.lisp` to `rplaca.asd` after `package-manager` and before
  command, prompt, and tool consumers.
- Define structures, normalization, conditions, registry lock/snapshot, the
  permanent `normal` definition, lookup/list APIs, and package cleanup.
- Export the public contract from `src/packages.lisp`.
- Add unit tests for normalization, ownership, collisions, replacement,
  snapshot isolation, and concurrent register/list/remove behavior.
- Put the focused suite in `tests/modes-test.lisp`, register it under
  `rplaca-suite` in `tests/packages.lisp`, and add it to the serial test
  components in `rplaca.asd`.

### Phase 2: Buffer selection and persistence

- Add the selected interaction-mode name to the conversation buffer state in
  `src/buffer.lisp`.
- Extend buffer snapshot encode/decode and migration defaults.
- Implement atomic `set-buffer-interaction-mode` and display notification.
- Keep `buffer-major-mode` untouched.

### Phase 3: Session branch semantics

- Add `interaction-mode-change` recording and the independent branch reducer
  in `src/session.lisp`.
- Teach `src/minibuffer.lisp` to label/filter the event.
- Restore mode on branch navigation, resume, and every fork path in
  `src/main.lisp` and `src/interop-core.lisp`.
- Prove event ordering: the switch event must parent the next user message.

### Phase 4: Package lifecycle

- Add registry cleanup to `%reset-package-runtime-state` in
  `src/package-manager.lisp`.
- Reconcile after a complete reload/disable/uninstall outcome, never during the
  transient reset gap.
- Preserve sealed in-flight contexts while forcing affected future runs to
  `normal`.

### Phase 5: Mode-scoped command metadata

- Extend `command-metadata`, `register-command-metadata`, `defcommand`,
  `command-metadata-visible-p`, `list-available-commands`, and `invoke-command`
  in `src/commands.lisp` and `src/main.lisp`.
- Test M-x filtering and invocation-time rejection independently.
- Do not add dynamic mode keymaps in this phase.

### Phase 6: Run-context integration

- Add the immutable run context to buffer runtime ownership in
  `src/prompt-runner.lisp` and a lexical equivalent in `src/prompt-core.lisp`.
- Refactor prompt assembly in `src/llm.lisp` to insert the precomputed mode
  section exactly once.
- Refactor `src/tools.lisp` and provider-request assembly to use the same exact
  filtered vector for schemas, prompt text, and dispatch.
- Capture after package activation and before provider/worker creation.
- Clear the context on all final, cancel, stop, failure, and disposal paths.
- Recapture for queued follow-ups; retain for steering and tool continuations.

### Phase 7: Headless and interop surfaces

- Add `--mode` parsing and usage text to the prompt entry points in
  `src/main.lisp` and the shell wrappers that expose them.
- Extend `src/interop-core.lisp` and `scripts/app-server.lisp` request/response
  shapes without changing the protocol's concurrency guarantees.
- Include the effective mode in summaries and structured results.

### Phase 8: McCLIM UI

- Add the mode presentation type, selector command, translator, and stable menu
  entry in `src/mcclim-interface.lisp`.
- Render the semantic mode label in the listener wholine using incremental
  output recording.
- Reuse the existing minibuffer candidate machinery and queued redisplay path.
- Add focused McCLIM unit tests and one GUI E2E selector/redisplay scenario.

### Phase 9: User and extension documentation

- Document `normal`, mode selection, `--mode`, package ownership, callback
  signatures, failure behavior, and the explicit full-trust/non-security
  posture.
- Provide a documentation-only `research` example that demonstrates prompt,
  tool-filter, and command scoping without shipping a plan-mode package.
- State that external sandboxing is the user's containment mechanism.

## Proof Matrix

Implementation is complete only when the following evidence exists:

| Area | Required proof |
|---|---|
| Registry | Name normalization, blank rejection, reserved `normal`, cross-owner collision, same-owner reload, cleanup, deterministic snapshots |
| Concurrency | Register/list/remove stress; callbacks demonstrably run outside all registry and buffer locks |
| Package visibility | Package mode appears only where its owner is active; selecting it never enables the package |
| Atomic switching | Same-mode no-op; unknown, inactive, and busy failures leave selection, events, and autosave unchanged |
| Contributor failure handling | Callback failure produces zero provider calls and no partial run context |
| Prompt composition | Mode section appears exactly once at the documented order point for GUI, headless, and interop runs |
| Tool identity | Provider schema names, rendered tool names, and dispatch-eligible names are exactly equal after filtering |
| Fabricated calls | A call to a filtered-out tool is rejected as unavailable and never executed |
| Run lifetime | Tool loops and steering keep one snapshot; queued follow-up captures a new snapshot |
| Cleanup | Final, cancel, error, explicit stop, and buffer disposal all clear the context |
| Isolation | Two concurrent buffers with different modes never leak prompt text, commands, or tools |
| Reload isolation | Re-registering a mode cannot alter an active run's captured function or result |
| Snapshot migration | Missing mode loads as `normal`; malformed mode warns and does not crash |
| Event history | Mode event parents the next user entry; branch navigation restores the effective historical mode |
| Forking | GUI, headless, and interop forks inherit the exact selected branch's mode |
| Package loss | Successful reload causes no transient fallback; failed reload/disable records exactly one fallback to `normal`; a busy buffer persists it only at the safe turn boundary before follow-up |
| Commands | M-x excludes inapplicable commands and direct/stale invocation rejects them |
| CLI | `--mode` parse, unknown-mode preflight, one-shot run, and durable session switch |
| Interop | Start/run/read propagation plus a typed concurrent-busy response |
| CLIM semantics | Mode label is a presentation; translator and command work; redisplay follows the frame event path |
| GUI E2E | Open Mode menu, choose a mode, observe info-line update, send a turn, and return to `normal` without frame exit |
| Regression | Full FiveAM suite and the relevant GUI E2E suites pass in the repository container |

Tests should use a provider spy/counter for every preflight claim. “No provider
call” is not proven by merely checking the returned condition.

## Explicitly Deferred

The following are not part of v1:

- a built-in plan mode;
- multiple, inherited, nested, or minor interaction modes;
- enter/leave callbacks and mutable per-mode state;
- serialized arbitrary plugin state;
- provider, model, reasoning-effort, agent, or pipeline overrides;
- mode-specific keymaps;
- arbitrary pane, renderer, or menu injection;
- tool creation or schema rewriting through a mode filter;
- one-turn mode stacks;
- assistant-output postprocessing;
- a public provider extension API;
- HTTP/SSE service expansion;
- permission, approval, or sandbox semantics;
- application update/release integration; and
- compatibility guarantees for a future stable plugin API.

These exclusions keep the first contract useful during pre-alpha without
freezing unrelated subsystems that are still changing.

## McCLIM Design Basis

This design follows the McCLIM application guidance used by the repository:

- the application frame owns transient UI state and pane layout;
- commands own user-visible actions;
- command tables own command/menu/keystroke context;
- visible domain objects are presentations with translators;
- display functions reflect state without mutating it; and
- dynamic output uses CLIM redisplay and stable output-record identity.

It deliberately avoids a custom renderer, repaint loop, raw hit map, or direct
sheet/medium manipulation. The mode system is domain/runtime state presented
through CLIM, not a widget layer built alongside CLIM.
