# RPLACA Feature-Gap Analysis

- **Compared products:** Codex CLI, Pi, and OpenCode
- **Research date:** 2026-07-14
- **RPLACA snapshot:** working tree on `esa-conversion`, based on
  `b602f7a949e14428dafb57634b1b374ef13b72fd`
- **Status:** comparator research plus accepted roadmap decisions. A research
  fact is not an implementation commitment unless the roadmap disposition says
  it is current.

## Scope and Method

This report compares current, documented behavior in Codex CLI, Pi, and
OpenCode with the behavior implemented in the current RPLACA working tree.
It uses primary product documentation and source repositories rather than
feature lists from secondary reviews.

The comparison uses these terms:

- **Present** means RPLACA has an end-to-end user workflow, not merely a
  similarly named helper.
- **Partial** means useful substrate exists but a user-visible workflow,
  protocol guarantee, or enforcement boundary is missing.
- **Missing** means no equivalent end-to-end capability was found.
- **Fit** measures compatibility with RPLACA as a Lisp-native,
  presentation-oriented McCLIM editor, not the popularity of the feature in a
  terminal application.
- **Effort** is relative (`S`, `M`, `L`, `XL`), not a calendar estimate.

The scope is deliberately narrow:

- Codex claims refer to the CLI. Desktop-only scheduling, managed worktrees,
  Chronicle, and Record & Replay are not counted as CLI gaps.
- Pi refers to the current canonical
  [`earendil-works/pi`](https://github.com/earendil-works/pi) repository. The
  former `badlogic/pi-mono` URL redirects there. The research snapshot is
  commit [`0e6909f`](https://github.com/earendil-works/pi/commit/0e6909f050eeb15e8f6c05185511f3788357ddb3),
  dated 2026-07-13. At that snapshot the package is
  `@earendil-works/pi-coding-agent` 0.80.6 and its author remains Mario
  Zechner.
- OpenCode claims refer to its official documentation and repository snapshot
  [`cb8be9b`](https://github.com/anomalyco/opencode/tree/cb8be9ba1217c2e7a2b93cf513eb21b41a7f5365).
  LSP and formatter support are counted but marked optional because OpenCode
  disables both by default.
- A feature present in **one or more** comparison products is eligible for the
  broader gap list. The next section separately identifies the smaller
  intersection that all three products have.

## Accepted Roadmap Decisions

The comparison evidence below is retained even where RPLACA deliberately
chooses a different product direction. These decisions supersede the earlier
recommendations in the research snapshot:

| Area | Accepted disposition |
|---|---|
| In-application sandbox and permission prompts | **Rejected.** RPLACA follows a Pi-like full-trust posture. Remove the incomplete permission/ask-for-permission system rather than presenting partial policy as security. Users who need containment should launch RPLACA in an external sandbox or container that they control. |
| Shared comparator gaps 1-3 | **Desired outcomes.** Native images, semantic project-file mentions, and configurable provider/local-model reach remain good fits. Provider extension architecture is deferred while transports are in flux; “desired” does not authorize G8 now. |
| Shared comparator gap 4 | **Deferred.** Do not add an application updater before RPLACA has releases, a release format, channels, and a distribution contract. Current users build a private pre-alpha from source. |
| Former P0, G1-G3 | **Nullified.** They remain comparison findings, not roadmap work. No replacement in-process safety system is planned. |
| Current P1 | **G4, G5, G6, G7, and modified G9.** G9 is a general package-extensible interaction-mode facility, not a built-in plan mode. G8 is deferred. |
| G8 | **Deferred.** A public provider API or adapter registry would freeze a volatile subsystem without a present use that justifies the overhead. |
| Former P2, G10-G17 | **Deferred / not on the current roadmap.** Retain the research, but do not implement these at the present stage. |
| P3 | **Notes only.** Keep especially full-trust arbitrary UI extension APIs; managed worktrees, scheduling, Chronicle, and Record & Replay; and persistent goals with ephemeral side conversations for later reconsideration. |

“Full trust” is a statement about process authority, not a claim that arbitrary
extensions are harmless. An external sandbox can contain the entire Lisp
image, including code that can modify RPLACA itself, without asking the
harness to enforce a boundary it can also rewrite.

## Bottom Line

RPLACA is not a thin chat client waiting to copy the competitors. It already
matches much of their difficult core: durable tree-shaped sessions,
branch/fork navigation, compaction without transcript loss, steering and
follow-up queues, background subagents, structured user questions, skills,
packages, hooks, basic MCP client support, headless JSON/JSONL operation, a
stdio app server, HTML export, and durable recovery records.

The comparator research found the following material gaps:

1. **OS-enforced command and network isolation exists in Codex, while the
   removed RPLACA guard settings were only authorization metadata.** This is
   a comparison fact, not an adopted direction; the in-application boundary is
   rejected and the misleading surface has been removed.
2. **Turn-coupled undo and redo.** RPLACA has change sets and recovery
   checkpoints, but not one reversible transaction per agent turn.
3. **A first-class code-review workflow.** Existing Git tools and agents are
   enough substrate; target selection, structured findings, and a semantic
   review UI are absent.
4. **Native multimodal input and semantic file mentions.** Artifactum stores
   files and images, but images are not encoded as provider-native content and
   project files cannot be referenced through a low-friction, completion-driven
   prompt syntax.
5. **Configurable custom/local provider reach.** OpenRouter provides broad
   model access, but RPLACA itself has fixed production transports. The
   outcome is desirable; a provider API or adapter registry is deferred.
6. **Comparator-specific plan and policy workflows.** Their existence is
   recorded, but RPLACA chooses a general interaction-mode facility and no
   in-process permission or sandbox model.

The accepted near-term feature work is narrower: G4-G7 plus the general mode
system specified in [`MODE-SYSTEM-DESIGN.md`](MODE-SYSTEM-DESIGN.md). The
updater, provider extension API, former P2 platform work, and P3 ideas are not
current implementation commitments.

## What RPLACA Already Has

These are controls against false-positive gaps. They should be extended, not
reimplemented.

Rows backed by `packages/channels/default/` describe capabilities shipped with
the repository, not necessarily active fresh-install UI. Subagent, Quaestor,
MCP bridge, Artifactum, and similar package surfaces must be enabled even though
their core substrates are compiled into the system.

| Capability | RPLACA evidence | Comparable products |
|---|---|---|
| Durable JSONL sessions with a branch tree, navigation, labels, fork, and compaction | `src/session.lisp`, session commands in `src/main.lisp`, `src/compaction.lisp` | Pi provides the closest full tree/navigation/label parity. Codex provides saved-session lifecycle, fork, rename, and compaction; OpenCode provides saved/child sessions and forks, but full entry-tree/label parity is not asserted here |
| Steering and follow-up queues with recall and safe-boundary delivery | queue state in `src/buffer.lisp`; delivery in `src/prompt-runner.lisp`; UI commands in `packages/channels/default/quaestor/` | Codex, Pi |
| Synchronous and background subagents with list, status, wait, and cancel | `src/subagents.lisp`, `packages/channels/default/subagent/` | Codex; OpenCode's documented foreground subagents do not establish the full background list/status/wait/cancel lifecycle; Pi offers subagents as an extension example rather than core behavior |
| Structured questions and answers | `packages/channels/default/quaestor/` | OpenCode question tool |
| Hierarchical repository instructions | root-to-working-directory discovery in `src/llm.lisp` | Codex `AGENTS.md`, Pi context files, OpenCode instructions |
| Skills with progressive disclosure and linked mentions | `src/skills.lisp` | Codex, Pi, OpenCode |
| Package/extension system for tools, commands, prompts, agents, hooks, buffer types, and other registries | `src/package-manager.lisp`, `packages/channels/default/` | Codex plugins, Pi packages/extensions, OpenCode plugins |
| Basic MCP client with one-shot stdio subprocess requests and JSON-RPC-over-HTTP POST, including RPLACA tool/resource discovery | `src/mcp-bridge-core.lisp`, `packages/channels/default/mcp-bridge/` | Codex and OpenCode are comparable MCP tool clients; explicit resource-discovery parity is not established here. RPLACA does not yet provide persistent stdio, Streamable HTTP/SSE sessions, notifications, or OAuth; Pi intentionally leaves MCP to extensions |
| Full-trust tool and package composition with immutable per-run dispatch snapshots | `src/tools.lisp`, `src/package-manager.lisp`, `src/prompt-runner.lisp` | Pi is the closest posture. Selection controls what a workflow exposes; it does not contain code. Users provide an external process boundary when desired |
| Headless prompts with JSON, JSONL events, output schemas, sessions, packages, and skills | `prompt.sh`, prompt entry points in `src/main.lisp` | Codex `exec` covers the closest full row. OpenCode `run` covers headless operation, files, and raw JSON events while its SDK/server prompt API covers structured output. Pi covers print/JSON events and session/resource options, but not core output-schema enforcement |
| Thread/turn interop with start, list, resume, fork, read, interrupt, and streamed events | `src/interop-core.lisp`, `scripts/app-server.lisp` | Codex app server (experimental), Pi RPC/SDK, OpenCode server |
| Durable attachments/artifacts and HTML export with pluggable share handlers | `src/artifactum-core.lisp`, `src/session-export.lisp`, `packages/channels/default/artifactum/`, `packages/channels/default/templata/` | Pi has image content, export/import, and private-Gist sharing; OpenCode has attachments, export/import, and share/unshare. Neither proves parity with RPLACA's general artifact store or pluggable handler registry |
| Staged change sets, diff/apply/discard/revert, and a durable recovery journal | `src/projects.lisp`, recovery tools in `src/tools.lisp` | OpenCode undo/redo is more tightly turn-coupled |

## Capabilities Shared by All Three Comparators

Only four material gaps survive the strict reading of “present in Codex CLI,
Pi, and OpenCode but missing in RPLACA.” The first three are partial. The
fourth is absent at the application level, although package updating and safe
reload provide adjacent substrate.

The product decision is to pursue the outcomes in items 1-3, subject to the
current backlog dispositions below. Item 4 is deferred until releases exist.
In particular, item 3 is an accepted direction but its G8 public provider/API
work is not current P1 work.

### 1. Native image input

- **State:** Partial
- **Fit:** Excellent
- **Gain:** Screenshot-driven debugging, UI review, diagram/document analysis,
  and fewer lossy text descriptions
- **Effort:** L

Codex accepts image input, Pi accepts images from the terminal, SDK, and RPC,
and OpenCode accepts typed file attachments and clipboard images when the model
supports them. RPLACA's Artifactum accepts image MIME types, copies the files
durably, renders Markdown references, and exports them to HTML. Provider
conversation encoding, however, has no native image content block; an attached
raster image reaches the model only as text metadata/path context.

The right RPLACA shape is to add an attachment domain object and provider
capability metadata, then encode it per provider. The compose pane should show
the attachment as a presentation. Commands should add, inspect, or remove it.
Submission should reject unsupported provider/model combinations before a
request is sent. The transcript should retain a stable artifact identifier,
not an ephemeral clipboard path.

Failing before submission is a deliberate stronger RPLACA contract, not Pi
parity: Pi can omit or ignore an image when the selected model cannot consume
it.

### 2. Prompt-time project file mentions and completion

- **State:** Partial
- **Fit:** Excellent
- **Gain:** Faster, more precise context selection without copying whole files
  into the draft
- **Effort:** S-M

All three comparators offer direct file/path references in the prompt workflow.
RPLACA can insert an entire project file, attach a local file, and insert
linked skill or MCP-resource mentions. It does not have a project-file mention
syntax with automatic path completion and delayed context resolution.

This should reuse the linked-mention and completion infrastructure rather than
inventing another overlay system. A `project-resource` presentation should
retain project identity, normalized path, and an optional line/range selector.
Prompt composition should resolve the reference under the project path policy,
record exactly what was read, and show truncation or binary-file refusal.

### 3. Configurable provider/local-model reach

- **State:** Partial
- **Fit:** Excellent
- **Gain:** Offline/private use, direct subscription use, lower latency and
  cost, provider failover, and less core editing for every new API
- **Effort:** L

Codex supports custom `model_providers`, base URLs, and authentication plus
built-in local OSS operation through LM Studio or Ollama. Pi has a broad
built-in provider set plus configurable compatible APIs. OpenCode documents a
large provider catalog, local models, and custom base URLs; its pinned
implementation also tracks per-model text, audio, image, video, and PDF
capabilities. RPLACA supports OpenAI Codex, Z.AI, and OpenRouter directly.
OpenRouter makes many models reachable, but it is not equivalent to a provider
adapter registry, local inference, or direct subscription authentication.

The strict shared external feature is configurable/local endpoint reach, not
one identical provider API across all three products. The investigation
initially proposed a stable provider protocol for authentication, discovery,
capability metadata, request encoding, streaming, cancellation, and error
classification. That architecture is **not accepted for implementation now**:
the provider subsystem is too volatile to freeze a public extension contract.
When this outcome is scheduled, re-plan it against the then-current transports
rather than treating this report as an API specification.

### 4. Application update and version-check lifecycle

- **State:** Missing for RPLACA itself; installed packages can be updated
- **Fit:** Medium-high, conditional on a defined release channel
- **Gain:** Timely security/stability fixes, visible release notes, reproducible
  support reports, and less error-prone manual checkout maintenance
- **Effort:** M plus release/distribution work
- **Roadmap:** Deferred until RPLACA has actual releases

Codex has a stable `codex update` command, Pi provides `pi update --self`, and
OpenCode provides `opencode upgrade`. RPLACA can update installed packages and
can safely reload an already updated source tree, but it has no application
version check, release discovery, or guarded application upgrade workflow.

RPLACA is currently a private, pre-alpha, source-built application. There is
no release format, channel, or compatibility promise for an updater to consume.
Do not add version checking or self-update machinery now. Revisit the entire
design only after releases and their provenance, restart, migration, and
rollback contracts have been chosen.

## Gap Catalog and Roadmap Disposition

### Former P0 — Nullified: no in-application safety boundary

G1-G3 below preserve the comparator findings and the deficiencies observed in
the old RPLACA permission surface. They are **not backlog items**. RPLACA
will remove that surface completely instead of completing it. It will not ask
for permission, advertise tool “sandbox” levels, or imply that package trust
prompts contain code running in the same Lisp process.

The containment contract is external: users who require it run the entire
RPLACA process under an OS sandbox, container, VM, restricted account, or
other boundary of their choice. This is compatible with tools and packages
being able to modify RPLACA itself while keeping the enforcement mechanism
outside the code it contains.

#### G1. OS-enforced tool sandbox and network isolation

- **Seen in:** Codex CLI
- **State:** Missing at the per-command policy boundary
- **Roadmap:** Rejected as an in-application feature
- **Gain:** A compromised or mistaken agent cannot write outside authorized
  roots, modify protected repository metadata, or open network connections
  merely because a shell command can express those operations
- **Effort:** XL

RPLACA is normally launched inside a Guix container, which can provide useful
environment separation according to how the user invokes it. It was not,
however, enforcement of the former per-tool guard contract. In the audited
snapshot, `:workspace-write` allowed execution, `shell_exec` ran an arbitrary
command with only its starting directory selected, and the network deny list
recognized a small set of named tools while shell and Lisp remained able to
open network connections.

These observations explain why the legacy labels were misleading; they did not
justify building a second sandbox inside RPLACA. The labels, policy
decisions, and approval workflow have now been removed. Current documentation
states plainly that commands inherit the authority of the RPLACA process and
points users to external containment.

#### G2. Argument-aware allow/ask/deny rules

- **Seen in:** Codex CLI rules/execpolicy (experimental) and OpenCode
  permissions
- **State:** Partial
- **Roadmap:** Rejected with the permission system
- **Gain:** Least-privilege rules for shell prefixes, paths, URLs, skills, MCP
  tools, and subagents without disabling an entire tool
- **Effort:** M-L

The snapshot implementation lacks argument matching, but RPLACA will not add
it. There is no `allow`/`ask`/`deny` rule engine in the adopted full-trust
model. Ordinary workflow selection, such as which tools an agent is offered,
must be described as composition rather than authorization.

#### G3. Trust and provenance for executable project resources

- **Seen in:** Codex hook hash review; inspired by Pi's path-scoped
  project-loading trust
- **State:** Partial: package installation/enablement is explicit, but
  change-bound provenance review is missing
- **Roadmap:** Rejected as a security/approval feature
- **Gain:** An enabled project package or hook cannot silently execute changed
  code with the RPLACA process's full authority
- **Effort:** M

Pi's trust decision controls whether project-local resources load; it does not
review each revision or hash and is not a sandbox. Its extensions still run
with full process authority. RPLACA records package source and activation
metadata, which may remain useful for composition and diagnostics, but it must
not be presented as a containment guarantee. Do not add digest-bound trust,
quarantine, or renewed-approval prompts as part of the current direction.

### Current P1 — Improve the daily coding loop

Current P1 contains G4, G5, G6, G7, and the generalized form of G9. G8 is
retained in the catalog but deferred. Nothing in P1 depends on adding an
in-application sandbox or permission system.

#### G4. Turn-coupled undo and redo

- **Seen in:** OpenCode
- **State:** Partial
- **Fit:** Excellent
- **Gain:** Safe experimentation, rapid recovery from a bad agent turn, and a
  clear relationship between transcript actions and filesystem effects
- **Effort:** L

OpenCode's `/undo` reverts the latest user turn, its responses, and associated
Git changes; `/redo` restores them. RPLACA has reversible change sets and a
durable recovery journal, but normal provider `write`, `edit`, and shell effects
are not grouped into a user-turn transaction and the transcript has no
undo/redo event.

Create a durable turn-effect ledger keyed by session entry and tool-call IDs.
Record preimages and postimages before publication, verify that current files
still match expected state, and refuse or offer a three-way workflow when live
file buffers or external edits conflict. Undo/redo should append session events
rather than erase history, so branch integrity and auditability survive.

Turn transactions, files, hunks, and conflicts should be presentations.
Commands own preview, undo, redo, open-file, and branch-from-before-turn actions.
The transcript display only reflects ledger state.

#### G5. Target-aware code review

- **Seen in:** Codex CLI
- **State:** Missing as a first-class workflow
- **Fit:** Excellent
- **Gain:** A repeatable inspect-review-fix loop with less prompt ceremony and
  structured findings that can be acted on directly
- **Effort:** M

Add review targets for uncommitted changes, a base-branch diff, a commit, and a
RPLACA change set. Run a reviewer agent with an explicitly read-only tool set.
Normalize findings into severity, summary, rationale, path, line/range, target,
and confidence rather than leaving them only as prose.

A review buffer should be an application view over review state. Findings,
files, and hunks are presentations with commands to open, inspect the diff,
dismiss, create a task, or start a constrained fix turn. Use formatted output
and `updating-output` with stable finding IDs; do not build a private diff
renderer or coordinate hit map.

#### G6. Native multimodal input

This is the first intersection gap described above. It belongs in P1 because it
substantially improves UI debugging and documentation work. Implement the
minimum internal capability handling needed by the current providers plus
durable artifact identity; do not block images on, or accidentally publish, the
deferred G8 provider extension API.

#### G7. Semantic project-file mentions

This is the second intersection gap described above. It is likely the fastest
high-impact feature in the report because linked mentions, project paths, and
automatic completion already exist in adjacent subsystems.

#### G8. Custom/local endpoint reach

This is the third desired intersection outcome, but it is **deferred and not
part of current P1**. A provider adapter/capability registry was an
investigation proposal, not an accepted API contract. RPLACA is heavily in
flux, and freezing a package-facing provider protocol would add maintenance
overhead without a present use. Reinvestigate the smallest path to custom/local
reach when the transport layer and actual use cases are stable.

#### G9. General package-extensible interaction modes

- **Seen in:** Codex CLI and OpenCode; Pi deliberately omits it from core but
  ships an authorization-only, read-only-oriented extension example
- **State:** Partial
- **Fit:** High
- **Gain:** Named, visible workflow composition for planning, research, review,
  and future package-defined interaction styles without hard-coding any one
  vendor's plan workflow
- **Effort:** M for the durable/runtime/UI contract

RPLACA will not ship Codex- or Claude-style plan mode as the core feature.
Instead, it will expose one durable interaction mode per chat buffer. A
package-owned mode may contribute additive prompt text, narrow the tools
offered to the model for a run, and scope registered commands. The built-in
`normal` mode is permanent and neutral. A plugin can implement plan mode if a
user wants it.

Mode filtering is deterministic workflow composition, not authorization or
containment. Plugins and tools retain the full authority of the RPLACA
process. The complete v1 ownership, persistence, run-snapshot, package
lifecycle, CLIM UI, failure, implementation, and proof contract is in
[`MODE-SYSTEM-DESIGN.md`](MODE-SYSTEM-DESIGN.md).

### Former P2 — Deferred / not on the current roadmap

All G10-G17 entries are research notes only. Their fit and effort columns
explain why they were identified; they do not authorize design or
implementation at the current private pre-alpha stage.

| Gap | Seen in | Current RPLACA state | Fit and gain | Effort / caution |
|---|---|---|---|---|
| **G10. Unified doctor** | Codex CLI | Partial: package and MCP doctors plus legacy guard/debug reports exist | **Excellent.** One deterministic report for Guix/runtime, provider auth, external binaries, package integrity, MCP, process authority, external-containment posture, fonts/backend, and writable paths could reduce support time | **S-M.** If revived, aggregate bounded side-effect-free reports without resurrecting guard enforcement claims |
| **G11. Session archive/delete and versioned exchange** | Codex lifecycle; Pi JSONL import/export; OpenCode delete and JSON/share-URL import/export | Partial: load, resume, fork, rename, HTML export, and local share exist | **High.** Session hygiene, migration, backup, and reproducible bug reports | **S-M.** Archive by metadata/index first; deletion must require explicit confirmation and protect open sessions. A versioned portable contract is a new RPLACA requirement; neither a product's JSON export nor RPLACA's internal sidecars should be assumed stable without one |
| **G12. Application release/update lifecycle** | Codex `update`; Pi `update --self`; OpenCode `upgrade` | Missing for RPLACA itself; installed-package updates and safe reload exist | **Deferred until releases exist.** It has no useful input contract while users build the private pre-alpha from source | **M plus release work.** Reconsider only after release format, channel, provenance, restart, migration, and rollback decisions exist |
| **G13. Versioned HTTP/SSE or socket transport and typed client schema** | OpenCode server; Codex app-server remote transports are experimental; Pi has stdio RPC | Partial: capable in-process and stdio JSONL interop already exists | **High.** Multiple clients, IDE integration, long-lived service use, reconnect, and language-neutral automation | **L.** Preserve existing turn interruption and the protocol-version field; add auth, reconnect/event cursors, schema publication, compatibility negotiation, backpressure, and conformance tests. Do not replace McCLIM with a web client |
| **G14. Language intelligence service** | OpenCode LSP | Missing as an editor protocol; SLOP supplies Lisp-specific static operations | **Medium-high.** Diagnostics, definition/reference navigation, symbols, and faster feedback across project languages | **L.** OpenCode itself disables LSP by default and warns about stale state, memory, and slowdown. Prefer a service abstraction with SLOP/Swank-friendly Common Lisp support and optional LSP adapters; do not auto-download servers |
| **G15. Opt-in cross-session memory** | Codex CLI | Missing: sessions, boot files, compaction, and branch summaries are not generated cross-session memory | **Medium-high.** Carries durable preferences and project lessons without reopening old transcripts | **L / high design risk.** Memories must be source-linked, reviewable, editable, forgettable, scoped, and off by default. Never silently convert a lossy summary into trusted fact |
| **G16. Session exchange and revocable sharing** | Pi JSONL exchange and private/unlisted-Gist sharing; OpenCode JSON/share-URL import/export plus public share/unshare. Restricted/self-hosted OpenCode sharing is enterprise-only | Partial: HTML export and pluggable local/hook handlers exist | **Medium.** Easier collaboration and issue reproduction | **M.** Self-hosting, redaction preview, expiry, and authenticated privacy are proposed RPLACA strengthening. Public/access-by-link should remain off by default; validate imports before opening them |
| **G17. MCP server exposure** | Codex CLI | Missing: RPLACA is an MCP client only | **Medium.** Other editors and agents could delegate a RPLACA thread/review/session operation | **M.** If revived, adapt the interop core and expose a deliberately small surface with explicit cancellation and full-trust process semantics; do not promise an in-app sandbox |

If any P2 feature is revived later, transport or worker threads should publish
immutable events back to the frame's normal event path. Display functions then
read frame/domain state and request ordinary CLIM redisplay. They must not write
panes, mutate output records, or invoke extension callbacks from an HTTP, LSP,
MCP, or provider worker.

### P3 — Notes only

P3 is not an implementation backlog. Three themes are worth preserving for a
future design session:

1. **Full-trust arbitrary UI extension APIs.** This could unlock unusual
   Lisp-native interfaces, but needs a deliberate contract with CLIM frame,
   event, and redisplay ownership.
2. **Managed worktrees, scheduling, Chronicle, and Record & Replay.** These are
   potentially valuable parallel-work and observability projects, not current
   CLI-parity requirements.
3. **Persistent goals and ephemeral side conversations.** These may complement
   sessions and branches once a concrete workflow shows that the existing
   models are insufficient.

The remaining rows are also retained only as research notes.

| Feature | Seen in | Fit | Recommendation |
|---|---|---|---|
| Session-scoped background terminals with list/stop | Codex CLI | Medium | Codex documents recent output and stopping terminals started by the current session, not durable jobs surviving exit/resume. Useful for dev servers and watchers, but it expands process-lifecycle risk. If reconsidered, require bounded logs, process-group ownership, job presentations, inspect/stop commands, and honest full-trust semantics. External containment remains the user's choice. Pi intentionally omits background bash. |
| Automatic formatter after every agent edit | OpenCode, disabled by default | Medium-low | Do not copy background disk mutation. Start with an explicit buffer-aware format/check command, preview a diff, and apply through the buffer/change-set model. Common Lisp has no built-in OpenCode formatter entry. |
| Persistent goals and ephemeral side conversations | Codex CLI | Potentially high; contract unknown | Preserve for later design. Organa, pipelines, agents, session branches, and scratch buffers overlap substantially, so validate a concrete workflow before adding persistence concepts or a second transcript model. |
| Hosted/shareable session links | Pi private/unlisted GitHub Gist and OpenCode public link | Low as a default | Existing share handlers are the right extension point. A private Gist is unlisted rather than authenticated end-to-end privacy. If added, make private/self-hosted and revocable the preferred shape; source code and credentials make access-by-link sharing hazardous. |
| Full-trust arbitrary UI extension APIs | Pi | Potentially high; high design risk | Preserve as a future theme. Explore broader Lisp-native UI extension while keeping CLIM frames, commands, presentations, command tables, events, and redisplay ownership coherent; do not copy terminal widget APIs mechanically. |
| JavaScript/TypeScript plugin runtime or SDK dependency | OpenCode | Low | Use the versioned wire protocol, MCP, or subprocess tools for cross-language integrations. Keep native extension logic in Common Lisp. |
| OpenCode TUI controls, terminal themes/widgets, or external-editor escape | Pi and OpenCode | Low | These solve terminal-client constraints that RPLACA does not have. Recreate useful semantics with CLIM commands, presentations, completion, Drei/editor behavior, and faces—not their rendering layers. |
| Cloud-task bridge | Codex CLI (`codex apply` stable; `codex cloud` experimental) | Low without a RPLACA cloud service | This is a real CLI feature, but it depends on a remote task product rather than a local editor primitive. Treat it as a deliberate scope exclusion until RPLACA has a remote-execution design. |
| Managed worktrees, scheduling, Chronicle, and Record & Replay | Codex desktop/app surfaces | Potentially high; currently out of scope | Preserve for future parallel-worktree, automation, and observability design. These remain desktop/app ideas rather than Codex CLI parity requirements. |

## Product-Specific Takeaways

### Codex CLI

The most relevant adopted feature is target-aware review. Codex's clear plan
workflow informs the need for a visible mode concept, but RPLACA generalizes
that concept rather than copying plan mode. Its headless execution, subagents,
instructions, skills, MCP client, session resume/fork, and queued interaction
are already represented in RPLACA. Memory, doctor, and MCP-server mode remain
deferred research items.

Do not count desktop-only features or the cloud service itself as local CLI
primitives; the real `apply`/`cloud` CLI bridge is recorded separately in P3.
Codex's OS sandbox and policy rules remain valid comparator facts, but RPLACA
does not intend to reproduce them in-process.

### Pi

Pi's most important ideas—append-only session trees, branch summaries,
compaction that changes context without deleting history, steering/follow-up
queues, package resources, and language-neutral JSONL RPC—are already unusually
well represented in RPLACA. The remaining useful gaps are native images,
prompt file references, broad/custom providers, documented JSONL session
exchange, and explicit trust for project-local executable resources.

Pi explicitly omits built-in MCP, subagents, plan mode, per-tool permission
popups, to-dos, and background bash. It does have a project-trust prompt. Those
omissions should not be incorrectly cited as Pi core features. Its full-trust
TypeScript extension model is not a security boundary. RPLACA adopts the same
honest process-authority posture: trusted extensions can do what the process
can do, and containment belongs outside the application. Pi's much broader UI
extension freedom is an interesting P3 idea, not current work.

### OpenCode

The strongest adopted features are turn-level undo/redo and typed multimodal
attachments. The documented HTTP/OpenAPI/SSE boundary, diagnostics, session
exchange, and sharing remain deferred; granular permission matching is not part
of the chosen full-trust direction.

Do not copy the web/TUI client architecture into the McCLIM frame. OpenCode's
LSP and formatter documentation explicitly marks both systems disabled by
default, and its LSP guide describes stale state, memory, version, and latency
costs. OpenCode plan mode is approval-gated rather than strictly non-mutating;
RPLACA will expose a general interaction-mode contract and leave any plan
workflow to a plugin.

## CLIM-Native Design Contract

The recommendations above use portable CLIM application patterns unless noted
otherwise. The grounding is the `mcclim-manual` skill's
`mcclim-application-guide.md`, especially **Application Frames**,
**Presentations**, **Commands And Command Tables**, **Output, Output Records,
And Redisplay**, and **Input Editing And Completion**.

For any selected feature:

1. Keep durable state and domain invariants in UI-independent runtime code.
2. Let the existing application frame own frame-local view and interaction
   state.
3. Represent sessions, reviews, findings, diffs, files, attachments, modes,
   jobs, and diagnostics as semantic objects and presentation types.
4. Put actions in commands. Menus, keystrokes, presentation translators, and
   gadget adapters should dispatch those commands or small tested helpers.
5. Use frame-local command tables for mode- or state-dependent availability.
6. Use `accept`, completion, or `accepting-values` for typed structured input;
   use gadgets only for conventional controls.
7. Use display functions, formatted output, and `updating-output` with stable
   object IDs for dynamic reports.
8. Marshal background results into the frame event path, then request normal
   pane redisplay.
9. Do not add manual repaint loops, coordinate hit testing, direct medium/sheet
   manipulation, private render trees, or `window-clear` redraw schemes.

Raster-image display may use McCLIM-specific image facilities, but attachment
identity, provider encoding, commands, and presentation semantics remain
backend-independent.

## Suggested Sequence

The current feature sequence is intentionally short. The full-trust foundation
is complete: the legacy permission, approval, and sandbox-label surface has
been removed.

1. Implement G4 turn-ledger undo/redo and G5 target-aware structured review.
2. Implement G7 semantic project-file mentions.
3. Implement G6 native images for current providers without freezing the
   deferred provider extension API.
4. Implement the generalized G9 interaction-mode contract in
   [`MODE-SYSTEM-DESIGN.md`](MODE-SYSTEM-DESIGN.md); plan mode remains a plugin
   possibility rather than a core workflow.

G8, the updater, all former P2 entries, and every P3 item stay outside this
sequence. They require a new explicit roadmap decision before implementation.

## Primary Sources

### Codex CLI

- [CLI command reference and slash commands](https://learn.chatgpt.com/docs/developer-commands.md?surface=cli)
- [Subagents](https://learn.chatgpt.com/docs/agent-configuration/subagents.md)
- [Agent approvals and security](https://learn.chatgpt.com/docs/agent-approvals-security.md)
- [Sandboxing](https://learn.chatgpt.com/docs/sandboxing.md)
- [Rules](https://learn.chatgpt.com/docs/agent-configuration/rules.md)
- [Advanced configuration and custom model providers](https://learn.chatgpt.com/docs/config-file/config-advanced.md)
- [Hooks and trust](https://learn.chatgpt.com/docs/hooks.md)
- [Memories](https://learn.chatgpt.com/docs/customization/memories.md)
- [Non-interactive mode](https://learn.chatgpt.com/docs/non-interactive-mode.md)
- [Codex as an MCP server](https://learn.chatgpt.com/docs/mcp-server.md)
- [Codex app server](https://learn.chatgpt.com/docs/app-server.md)

### Pi

- [Coding-agent README](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/README.md)
- [Package identity and version](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/package.json)
- [Sessions](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/sessions.md)
- [Session format](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/session-format.md)
- [Compaction](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/compaction.md)
- [Extensions](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/extensions.md)
- [Authorization-only plan-mode example](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/examples/extensions/plan-mode/README.md)
- [Packages and trust](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/packages.md)
- [Security](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/security.md)
- [Providers](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/providers.md)
- [Models and capability metadata](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/models.md)
- [SDK](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/sdk.md)
- [RPC](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/rpc.md)
- [Import, export, and Gist sharing](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/usage.md)
- [Image settings](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/coding-agent/docs/settings.md#terminal--images)
- [Pi AI image input and cross-provider behavior](https://github.com/earendil-works/pi/blob/0e6909f050eeb15e8f6c05185511f3788357ddb3/packages/ai/README.md)

### OpenCode

- [TUI commands, file references, and undo/redo](https://opencode.ai/docs/tui/#file-references)
- [Agents](https://opencode.ai/docs/agents/)
- [Permissions](https://opencode.ai/docs/permissions/)
- [Server and API](https://opencode.ai/docs/server/)
- [SDK](https://opencode.ai/docs/sdk/)
- [Providers](https://opencode.ai/docs/providers/)
- [Models](https://opencode.ai/docs/models/)
- [Custom commands](https://opencode.ai/docs/commands/)
- [Tools](https://opencode.ai/docs/tools/)
- [Plugins](https://opencode.ai/docs/plugins/)
- [LSP](https://opencode.ai/docs/lsp/)
- [Formatters](https://opencode.ai/docs/formatters/)
- [Sharing](https://opencode.ai/docs/share/)
- [CLI automation and session exchange](https://opencode.ai/docs/cli/)
- [Pinned provider capability detection](https://github.com/anomalyco/opencode/blob/cb8be9ba1217c2e7a2b93cf513eb21b41a7f5365/packages/opencode/src/provider/provider.ts#L1453-L1474)
- [Pinned prompt file-part schema](https://github.com/anomalyco/opencode/blob/cb8be9ba1217c2e7a2b93cf513eb21b41a7f5365/packages/schema/src/prompt.ts#L12-L44)
- [Pinned TUI local attachment handling](https://github.com/anomalyco/opencode/blob/cb8be9ba1217c2e7a2b93cf513eb21b41a7f5365/packages/tui/src/component/prompt/local-attachment.ts#L25-L47)
- [Pinned clipboard image ingestion](https://github.com/anomalyco/opencode/blob/cb8be9ba1217c2e7a2b93cf513eb21b41a7f5365/packages/tui/src/clipboard.ts#L29-L73)
- [Pinned read-tool image/PDF attachments](https://github.com/anomalyco/opencode/blob/cb8be9ba1217c2e7a2b93cf513eb21b41a7f5365/packages/opencode/src/tool/read.ts#L300-L324)

## Revalidation Note

Codex and OpenCode documentation is mutable, and Pi is under active development.
Revalidate the relevant primary-source section before turning any recommendation
into a specification. Revalidation should not erase the local comparison:
confirm the RPLACA capability against the working tree again, because several
items in this report are intentionally classified as partial rather than
missing.
