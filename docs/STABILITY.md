# Stability audit and hardening plan

Audit date: 2026-07-13 through 2026-07-15

## Scope and standard

This audit covers application crashes, hangs, lost updates, background-worker
leaks, teardown races, and failures in the GUI test harness. McCLIM code is
classified separately in [MCCLIM-ISSUES.md](MCCLIM-ISSUES.md).

Clawmacs is a full-trust application. The former in-process permission,
approval, and sandbox-label system was removed after this audit because it did
not enforce an OS boundary and could create false confidence. Users who need
containment should run the entire process inside an external sandbox,
container, VM, or restricted account. The worker, execution-ownership,
immutable-snapshot, and path-context mechanisms discussed below are stability
controls, not security controls.

The UI acceptance standard is canonical CLIM/McCLIM:

- the application frame owns UI state, panes, layouts, and frame-local command
  tables;
- commands and presentation translators own user-visible actions;
- Drei owns ordinary text editing;
- display functions and `updating-output` render state;
- workers publish data and queue a wakeup to the real top-level sheet;
- the frame process applies state and calls `redisplay-frame-pane`;
- named command tables and their menu gadgets remain stable for the frame
  lifetime, while state-dependent choices use presentations and selectors;
- no application code takes over repainting, mutates private output records, or
  performs semantic coordinate hit testing.

## Baseline

The first full FiveAM run reported 2 failures out of 2,922 checks. A cancelled
subagent from an earlier package test kept running after its test-scoped
provider override had unwound, then made an extra provider call inside a later
pipeline test. A later run happened to pass, confirming a timing-dependent
worker leak rather than a deterministic pipeline error.

The GUI baseline also exposed two harness failures that were not application
or McCLIM crashes:

- every Guix container saw shell PID 1, so the PID-derived display allocator
  always selected `:91`; concurrent suites could race on the same X socket;
- a cold compile took about 153 seconds against a hard 180-second frame-ready
  deadline, leaving too little margin. Historical "fatal" compiler output was
  consistent with the harness terminating SBCL at that deadline.

A later parallel run exposed a third environment failure: two containers ran
`ql:quickload` against the same project Quicklisp release tree, and one removed
a temporary release archive while the other was still installing it. The old
launcher regression also replaced that production cache with mock data. Both
are classified as harness faults, not Clawmacs or McCLIM crashes.

## Findings and implemented fixes

| Priority | Failure mode | Ownership | Fix | Proof |
|---|---|---|---|---|
| P0 | A redisplay requested before grafting set the coalescing latch even when no event was queued, permanently suppressing later updates. | Clawmacs | Separate dirty/reserved/handling state; queue a `window-manager-event` on the grafted top-level sheet; roll back only the reservation on enqueue failure; drain dirty state when the frame starts. | Pre-graft, enqueue-failure, coalescing, and GUI streaming/redisplay tests. |
| P0 | Replacing a live frame command table disowned submenu gadgets while queued pointer events could still refer to them; pinned ESA also reassigned even the identical table every command-loop turn. McCLIM then tried to acquire a medium for the stale sheet and signalled `Sheet ... is not grafted`. The old fallback retried the entire partially unwound frame top level forever. | Clawmacs trigger plus a pinned/current McCLIM robustness defect | Declare one stable named command-table tree on the application frame; route Skills, Packages, and Effort through frame commands and presentation-based selectors/dashboards; suppress only `EQ` table assignments on this frame; remove all live menu rebuilding and whole-top-level retry. | 59 focused menu/selector checks, exact crash backtrace classification, and a real 100-cycle pointer/menu stress with natural exit, empty process group, and no crash signature. |
| P0 | Provider HTTP connect and retry backoff ran before a stream state was returned, freezing the CLIM process and making Stop unavailable. | Clawmacs | Return a managed stream state immediately; perform connect/retry/read in its worker; make cancel-before-connect terminal and close resources once. | Barrier-controlled blocked-connect and cancellation tests plus responsive GUI streaming. |
| P0 | Provider tools ran from stream finalization or a CLIM command, so blocking tool bodies could freeze repaint and input. | Clawmacs | Dispatch ordinary interactive tools directly to a managed worker. Only the semantic `request_user_input` UI adapter remains at the command boundary. The frame process alone applies results and starts the next provider turn. | Barrier-controlled blocking-tool test demonstrates the owner returns; disposal test rejects the late result. |
| P0 | `cancel-subagent` published `:cancelled` while its provider/tool worker was still running, leaking dynamic test/provider state and allowing later side effects. | Clawmacs | Add `:cancelling`, capture and close the active stream, thread a cancellation predicate through provider/tool iterations, and publish `:cancelled` only after worker unwind. | Active-stream cancellation, early cancellation, package-tool wait, and repeated full-suite runs. |
| P1 | Stream cancellation and SSE completion could both mutate terminal state; abandoned non-200 response bodies leaked; reader resources remained attached. | Clawmacs | Use locked nonterminal mutation/terminal helpers, close response streams on all exits, clear reader ownership at worker exit, and make cancellation terminal exactly once. | Barrier-controlled cancel-versus-delta/EOF tests and close-count assertions. |
| P1 | Closing a Drakma response stream from the cancelling thread did not wake a reader blocked inside Flexi Streams/Chunga, so teardown could remain stuck until the 20-second I/O timeout. | Clawmacs | On the supported SBCL transport, discover the owned socket descriptor through the pinned Drakma wrapper stack and issue `SHUTDOWN :IO` while the stream-state lock pins ownership. The reader keeps the stream and performs the final `CLOSE :ABORT T` in its own unwind; a stream registered without a reader is detached and closed as an orphan. | A real held-open SSE server remains stalled while cancellation settles in under one second: 20 repetitions, 180 checks. Controlled reader and orphan tests add 15 checks; compatibility tests cover both Flexi/Chunga/fd and CL+SSL wrapper traversal. |
| P1 | Killing a buffer or closing its frame removed UI references but left provider, tool, or OAuth work able to report into stale state. | Clawmacs | Give each buffer a runtime generation and disposal state; frame/buffer teardown cancels owned operations, clears interaction state, and invalidates late tool results. | Deterministic blocked-worker disposal test and frame close GUI gates. |
| P1 | A terminal stream/tool updater detached its pending owner before applying durable state. Concurrent teardown could see no owner, finish, and then be followed by a stale result mutation or a new provider/tool start. | Clawmacs | Publish a unique `:applying` context in the same runtime-lock transaction as detach. Teardown is single-flight, invalidates the generation, and cannot clear `:stopping` until the exact applier, provider starter, and owned workers settle. Atomic continuation reservation checks the applying token. An unwind-safe release completes deferred teardown. | Barrier tests pause both stream and tool application after the claim, prove teardown cannot return, then prove no continuation starts and every tool ID receives one result. Blocked-worker disposal remains `:stopping` until the worker exits. |
| P1 | Stop/teardown could observe provider EOF before the frame polled it and accidentally execute terminal `tool_use`; cancelled tool queues could also leave assistant calls without matching results. | Clawmacs | Cancellation claims the stream before inspecting terminal state, strips unfinished tool calls from stopped partial responses, and synthesizes one cancellation `tool_result` for every unresolved interactive call while preserving completed results. | Terminal-before-poll tests cover allowed and semantic UI tools under both Stop and teardown; result cardinality/IDs are asserted. |
| P1 | Prompt/subagent cancellation between tool calls left a persisted assistant `tool_use` message without a matching user `tool_result`, corrupting the next provider request. | Clawmacs | The noninteractive tool loop owns an unwind cleanup: it retains completed results and records cancellation/error results for the unresolved suffix before propagating the original condition. | Cancellation between two deterministic calls plus next-request role/content-shape assertions. |
| P1 | OAuth completion only notified redisplay but was never applied; stale flows could clear successors; a flow constructor could publish after teardown had already found no pending flow; partial localhost headers and constructor failures leaked listener/client resources; a wrong method/path exited the sole callback worker without making the flow terminal. | Clawmacs | Workers only publish terminal flow state. A locked exact-flow registry and buffer runtime generation make construction/publication atomic with teardown. The ordinary CLIM redisplay handler applies exact flows. Listener/client ownership is terminal-once; cancellation uses socket shutdown to wake a partial read; every constructor failure closes the unpublished listener. Rejected callback requests keep accepting up to a bounded budget, after which the flow fails explicitly. | CLIM redisplay application test, stale-flow/status tests, a barrier-controlled publication-loses-to-teardown test, injected PKCE/thread-constructor failures, wrong-method/path followed by a valid callback, rejection-budget/port-rebind tests, and 20 partial-header cancellation repetitions (120 checks). |
| P1 | Provider retry slept unconditionally, trusted unbounded `Retry-After`, and did not make its Drakma timeout policy explicit. | Clawmacs | Cap all backoff by the configured maximum, slice the default sleeper around a cancellation predicate, wire streaming provider state into retries, and pass a configurable bounded connection/header timeout to every provider Drakma request. | Retry cap, cancellation-during-five-second-backoff, request-count, and captured-timeout tests; OpenAI/OpenRouter/Z.AI streaming lifecycle suites. |
| P1 | Interop used unprotected process-global thread/turn hash tables; active-turn check and insertion were separate. | Clawmacs | One registry lock protects both tables and snapshots; active check plus insertion is atomic; turn summaries copy under the turn lock before further work. | Two simultaneous starts admit exactly one; concurrent snapshots remain valid. |
| P1 | Concurrent interop start/resume for one persistent session could construct two mutable buffers and let both write the same transcript even if registry publication later chose one. | Clawmacs | A separate construction single-flight covers session load/create, buffer construction/autosave, and publish; publication remains compare-if-absent as a defensive boundary. | Concurrent start and concurrent resume each return the same `EQ` thread while constructing exactly one buffer. |
| P1 | Image-wide methods replaced McCLIM `basic-medium`, Edward, text-editor lifecycle, Drei command-table, and repaint behavior. These could change unrelated CLIM applications and drift from the pin. | Clawmacs | Remove broad methods and private slot/state manipulation. Retain only pane-specific standard CLIM layout and the narrow pinned-ESA modifier adapter. | Direct pinned-Edward kill/yank test, Drei editing tests, cold GUI startup, expose/resize/key suites. |
| P1 | Compose setup mutated Drei's process-global indent/deletion tables, and frame setup could begin before cleanup protection existed. | Clawmacs | Install an application command table through `drei-syntax:additional-command-tables` only for `clawmacs-chat-compose-pane`; establish top-level unwind cleanup before pane configuration or hook installation. | Plain Drei panes/global tables remain unchanged, the compose pane retains its gestures, and cleanup tests prove exact hook/buffer retirement despite injected failures. |
| P1 | Drei's `exclusive-gadget-table` won `M-x` before Clawmacs' inherited table, entered Drei's blocking extended-command workflow, and crashed through an undefined `STREAM` path in the live compose pane. An initial repair also passed list-valued key arguments to ESA unquoted, so ESA tried to call `:META` as a function. | Clawmacs | Use Drei's public pane-specific `additional-command-tables` hook to put the frame application table first. Keep ordinary editor keys in Drei's tables. Store quoted list-valued Clawmacs key arguments because ESA's partial command parser evaluates supplied argument forms. | Headless lookup/parser/real gesture regression plus two fresh live `mx` GUI passes; each opens the non-blocking Clawmacs minibuffer, exits naturally with status zero, and leaves an empty process group. |
| P1 | One rapid GUI sequence completed `C-c t` and then lost the shifted `C-c V` prefix, inserting literal `V`. The preserved logs do not include raw key fields, so a deeper cause is not claimed. | Unresolved trigger; mitigated in Clawmacs and harness | Consume every recognized standalone modifier explicitly at the compose-pane boundary so it cannot enter an accumulated command sequence. Preserve prefixes across ESA's identical command-table assignment. In the driver, wait for one toggle's semantic state to settle before starting the next chord. | A backend-spelling modifier regression followed by `C-c V`, an identical-table-assignment-between-gestures regression, the preserved failure, and repeated full live `keybinds` passes covering shifted `C-c V`, `C-c I`, `C-c A`, `C-c S`, `C-c M`, and `C-c R`. |
| P1 | The now-removed legacy approval presentation could act on a newer pending object after redisplay. | Clawmacs | The original identity repair carried the exact presented object. The later full-trust decision removed the entire approval state, presentation, commands, key mode, and response path, eliminating this interaction class. | Static removed-API sweep plus prompt, Quaestor, managed-tool, and McCLIM suites prove direct dispatch and structured questions remain intact. |
| P1 | A lone `queue-event` error or a request arriving during a failed redisplay body could leave dirty state without an event. | Clawmacs | Use bounded iterative enqueue retry, transactional reservation release, and generation-aware transfer after both normal and exceptional handler exits. | Lone transient failure, bounded burst, and concurrent request-during-handler-failure tests. |
| P1 | Thread-constructor failures could leave visible subagent/interop work permanently `:running`; a tool worker or metadata-help worker failure escaped ESA's command boundary and could unwind the main frame; registry replacement could leak losing live objects. | Clawmacs | Publish failed terminal state and release reservations on constructor errors; convert a tool-worker constructor failure into that exact call's protocol-safe result; contain help-frame worker construction and worker top-level errors; compare-if-absent publication disposes losers; cancellation occurs outside registry/turn locks. | Injected tool, help, subagent, and interop thread creation; exact tool-result ID/content; concurrent sync/async exclusion, resume/start, and reentrant interrupt tests. |
| P1 | Metadata help-frame construction and recurse child-process launch ran directly at CLIM command boundaries, so allocation, missing-executable, or fork failures could unwind `run-frame-top-level`. | Clawmacs | Put help-frame construction behind a contained application seam; catch recurse launch errors in the frame command; log and insert visible system diagnostics; request ordinary CLIM redisplay. | Injected frame-constructor and process-launch errors through `execute-frame-command`; frame command returns and its buffer contains the diagnostic. |
| P1 | Shell prefixes, listener shell forms, pipelines, and custom compaction performed blocking work or mutated the live buffer directly at a CLIM command boundary. Stop could not reliably cancel them, and a late worker could overwrite disposed or restarted state. | Clawmacs | Route each action through one managed interactive operation. The worker receives a detached buffer snapshot, publishes immutable effects/results, and owns cancellable provider/subprocess handles. The frame process claims the exact generation, replays effects, and runs continuations once; Stop retains ownership until settlement and drops late effects. | Barrier-controlled Stop and generation-loss tests, exactly-once shell/hook and compaction continuation checks, cancellable pipeline-provider test, and the focused 281-check managed-operation suite. |
| P1 | Managed shell output was bounded only when read back from temporary files, so a noisy command could fill disk first; reading stdout and stderr serially would instead risk a full-pipe deadlock. A fast `setsid` launcher exit could also hide a still-running background group. | Clawmacs | Continuously drain stdout and stderr on separate workers into fixed-size memory buffers and discard excess. Obtain the exact command session PGID through a startup control line from inside the new session (SBCL's `run-program` child is itself a group leader, so the outer `setsid` PID is not reliable). On every exit, TERM/KILL the exact group before joining drains and clearing ownership. | Concurrent two-megabyte stdout/stderr regression asserts exact 4 KiB retention, independent truncation, bounded completion, and no legacy temp artifacts. Five repeated fast-leader/background-child runs and an ignore-TERM cancellation test assert no live PID/group. Injected second drain-worker construction failure proves the first worker and session are reaped. |
| P1 | Runtime teardown completion invoked the public display-change extension hook on its reaper thread before clearing `:stopping`. Quaestor could consume a queued follow-up there, fail runtime admission, and mutate UI/application state off the CLIM process. Settlement-notifier or reaper callback errors could also escape worker roots. | Clawmacs | Split the internal frame wake from the public extension hook. A reaper settles only exact worker/resource handles, retains the teardown plus `:stopping`, marks frame delivery pending, and queues only the private CLIM wake. `handle-chat-frame-redisplay` applies cancelled stream/tool/operation state, restores input/status, then atomically clears blockers before the public completion hook. Last-frame cleanup marks buffers disposing first, so a late reaper protocol-completes silently without needing a dead frame. Reaper construction and finalization claims remain retryable after failure. | Deterministic affinity and reentrant-hook tests prove no visible state or public hook changes off-frame and no successor admits before final release. Waiter/cancellation barriers cover all four managed owner families; reaper constructor/finalizer retries and late headless frame cleanup prove exact teardown is neither overwritten nor stranded. |
| P1 | Public prompt and interop event callbacks ran inline on provider readers and async runners. One callback could hang provider settlement, strand safe reload, or grow unbounded callback threads and queues. | Clawmacs | Copy payloads with cycle, depth, node, and aggregate-element budgets and dispatch them through four fixed FIFO lanes with 256 queued deliveries per lane. Preserve order per callback, contain callback errors, refuse newest events on saturation, expose activity/drop counts, and make safe reload refuse with `:external-callback` while delivery is active. | Blocked-callback provider and interop tests prove runtime settlement; ordering, callback error, cyclic and oversized payload, sparse-hash allocation, saturation, drop-diagnostic, and safe-reload admission tests cover the bounded behavior. The runner-construction regression blocks callback delivery, proves failure and ownership release first, then drains the lane; 100 repetitions completed 1,400 checks. |
| P1 | An ordinary application command error could escape an ESA command callback and unwind the entire frame. Catching every condition would also incorrectly swallow CLIM frame-exit and abort control conditions. | Clawmacs | Add one `clim:execute-frame-command :around` application boundary that handles only `error`, reports bounded visible feedback, and leaves CLIM control conditions untouched. Gadget and presentation actions continue to dispatch frame commands. | Injected command, help-frame, and recurse-launch errors remain inside the running frame; normal frame exit still reaches unwind cleanup and emits `frame-stopped`. |
| P1 | Package lifecycle/configuration and package-published registries were mutated concurrently by frame, tool, interop, and reload paths. Some saves published memory before durable write or held a mutex across `LOAD`, I/O, or callbacks. | Clawmacs | Serialize lifecycle work through exact logical ownership without holding a mutex across `LOAD`; make configuration generations copy-on-write, write-before-publish, and atomically replace files; lock and snapshot package-owned command, hook, advice, tool, pipeline, MCP, buffer-type, provider, session-share, Slop, and related registries; remove only exact package-owned registrations. | Constructor/load/save failure, concurrent update, snapshot, exact-unload, advice replacement, and registry stress tests across package-manager, command, buffer, LLM, and package suites. |
| P1 | Project/change-set registries exposed mutable hash/list state across workers. Concurrent change sets could write the same paths, direct saves could truncate targets, a partially failed entry was not compensated, and project tools synchronized live UI buffers from worker threads. | Clawmacs | Publish immutable registry generations; serialize project/change-set transactions with exact owners; use same-directory atomic replacement; compensate the failed entry as well as earlier entries; merge status/flags under the registry lock; collect project-buffer effects until filesystem commit and replay them through the frame's existing tool-effect boundary. | Concurrent transaction and snapshot tests, atomic-write failure preservation, entry-local failure/compensation, manifest writer serialization, and worker-versus-frame buffer-effect affinity. |
| P1 | GUI runs collided on `:91`, the TCP X server was unauthenticated in Guix's shared host network namespace, and cold compilation was misreported as an app crash. | Harness | Allocate Xvfb displays with `-displayfd`; disable TCP and use only the container-local Unix socket; validate the returned number; make the frame-ready timeout configurable with a 300-second default. | Shell syntax/preflight and parallel GUI launches through distinct Unix displays. |
| P1 | Harness cleanup assumed the PID returned for `setsid --wait` was always the application PGID, gave child and group waits separate time budgets, and used a shell sleep timer that could outlive its parent. Members left in that group could escape cleanup, cleanup could take twice its advertised bound, or a stale timer could signal a recycled PID. | Harness | Publish the exact PGID from inside the new session; distinguish and retain a separately forked waitable `setsid` owner; use one monotonic deadline for child plus group; and use one parent-identity-guarded Python timer that is cancelled on normal and trap exits. TERM/KILL always target the exact application group, then the owner is reaped. Subprocesses that deliberately create their own sessions are owned by Clawmacs' separate managed-operation teardown. | Shell regression covers conditional-fork and same-owner paths, exact ordinary/signal status, natural exit, shared timeout, absent timers after natural/TERM exits, a TERM-ignoring leader and descendant, concurrent cleanup runs, repeated idempotence, and empty final group state. |
| P1 | Concurrent containers bootstrapped or installed releases into one project Quicklisp tree. Quicklisp's shared temporary archive names are not safe for concurrent writers, the launcher regression itself destructively exercised the production tree and pin file, and a host source registry could make McCLIM provenance ambiguous. | Harness | Hold one host `flock`, outside the replaceable Quicklisp directory, across bootstrap/probe and an exact `:clawmacs` dependency warmup before every payload; release it before the application starts. Give launcher tests a validated test-only repo cache root and bootstrap copy. Retain a warmup log, use the runtime library path, and derive `CL_SOURCE_REGISTRY` from the active Guix profile for warmup and payload execution. | The mocked launcher regression proves that two cold launchers bootstrap once, two warm launchers enter serially, warmup failure gates the payload, owner death releases the lock, production setup/pins remain unchanged, and a hostile host registry is replaced. Separately, the preserved real archive-deletion failure is followed by a green warmed parallel `mx`/`features` rerun; live ASDF inspection resolves the complete CLIM/ESA/Drei stack from the pinned Guix McCLIM tree. |
| P1 | A relative GUI artifact directory produced relative `HOME` and `XDG_CACHE_HOME` values. ASDF rejected the cache root before Clawmacs started, but Quicklisp collapsed the cause into `Could not load ASDF 3.0 or newer`; parallel GUI validation made the harness fault visible twice. | Harness | Convert repository-absolute host paths to `/workspace`, resolve and validate every inner artifact path before creating it or deriving runtime directories, and reject parent traversal, symlink escapes, and paths outside the shared workspace. | The original unchanged parallel relative-path `smoke`/`keybinds` invocation is the regression gate; both must start, exit naturally, pass artifact scans, and leave empty process groups. |
| P1 | Documented GUI timeout and stability-iteration overrides were not in Guix's preserved-environment pattern, so the apparent 100-cycle stress silently ran the 24-cycle default. | Harness | Preserve the two timeout and two stability-count variables explicitly at the container boundary. | Mocked launcher arguments must contain all four names, and the live stress action log must contain 100 menu interactions. |
| P1 | Legacy or externally generated Artifactum indexes without `updated_at` reached a parallel `LET` initializer that referenced `created-at` before it was bound, signalling `UNBOUND-VARIABLE`. | Clawmacs | Normalize timestamps with sequential binding and fall back to the supplied or generated creation timestamp when `updated_at` is absent. | Four regressions cover missing creation/update combinations and durable legacy-index reading; the focused Artifactum suite completes 51/51 checks. |
| P1 | A final semantic GUI state could be accepted before its matching CLIM redisplay had completed, so an immediate screenshot could record stale pixels even though the scenario itself passed. | Harness | Require a same-buffer `redisplay-handled` event with explicit `repeat: false`, or accept an already-handled snapshot only when it carries that state; then perform a separate-connection X server round trip before capture. Keep all synchronization in E2E observability rather than modifying CLIM repainting. | Thirteen dependency-free driver regressions cover stale, wrong-buffer, repeated, and legacy snapshots plus capture ordering; a focused McCLIM event test proves repeat state is emitted. |
| P1 | Every private GUI run recompiled already-warmed Common Lisp dependencies. The first cache-seed implementation also let a failed conditional `flock` proceed to `cp` because shell `errexit` was suppressed by its calling context. | Harness | Reflink or copy only the warmed `common-lisp` tree under the shared warmup lock into each private cache. Make lock/copy sequencing explicit, reject overlapping roots, clear only a validated private tree in cold mode, preserve sibling state, and fail closed on invalid policy roots. | Cache regressions prove timeout performs no copy and leaves no staging tree, cold reused artifacts are empty, siblings survive, and parallel seeds stay private. Seeded frame readiness fell from roughly 143 seconds to roughly 6 seconds. |
| P1 | The GUI driver reread and decoded the complete multi-megabyte event log on every poll, producing avoidable growth in long pointer/expose runs. | Harness | Tail by device/inode and byte offset, retain incomplete lines, cache decoded events, and reset on recognized replacement or rewrite. | Unit tests bound rereads to appended bytes plus a short anchor. A 250-menu/30-expose run completed 2,543 steps in 190 seconds, versus 228 seconds for the earlier full-log parser. |
| P1 | A Guix container that inherited a foreign read-only `/tmp/.X11-unix` made Xvfb repeatedly fail display ownership/binding and grow an approximately 11 MiB log without creating a usable display. | Harness | Unset host X variables before diagnostics, reject a pre-existing X socket namespace without removing or changing it, classify that state with status 75, and retry exactly once in a fresh Guix container while preserving unrelated failure statuses. | Mock retries prove the bound and status policy; a real disposable read-only exposure is unchanged and launches no Xvfb, followed by a clean real seeded smoke run. |
| P1 | An interop runner-construction test read terminal callback events immediately even though that delivery is intentionally asynchronous, producing one false missing-`turn.failed` failure in a two-pass full-suite run. | Test | Block and synchronize the callback lane explicitly. Assert that turn failure and execution-ownership release happen synchronously while delivery remains pending, then release, drain, and reuse the lane. | The focused interop group completed 1,400/1,400 checks across 100 repetitions, and the final complete suite passed twice in one image. |
| P2 | The broad GUI driver required a hard-coded default artifact path in session-save feedback, so a correctly isolated custom artifact directory was reported as a feature failure. | Harness | Derive the expected session directory from the driver's actual artifact root and isolated `HOME`. | The preserved failure snapshot shows the correct custom path; the rerun reaches `frame-stopped`, exits zero, passes the runtime scan, and leaves its process group empty. |

## Implementation sequence

1. Reproduce failures and preserve artifact/backtrace evidence.
2. Establish the CLIM ownership map before changing behavior.
3. Fix frame lifecycle and redisplay, and replace live menu rebuilding with a
   stable command-table tree plus semantic selectors.
4. Make subagent, provider stream, tool, buffer, and frame lifecycles
   cancellable and terminal-once.
5. Serialize interop registries and atomic operations.
6. Remove application-wide McCLIM/Drei overrides one group at a time, keeping a
   direct regression test for every removed workaround.
7. Fix harness isolation so parallel or cold runs cannot masquerade as product
   crashes, and make teardown own complete application process groups.
8. Bound public callback delivery and serialize durable package, project,
   change-set, and registry publication.
9. Run deterministic race tests repeatedly, then exercise the real GUI through
   key, pointer-menu, expose/resize, streaming, package, and reload paths.
10. Classify only stripped-down, repeatable failures as McCLIM issues.

## Stability acceptance gates

The change is acceptable only when all of the following are green:

- Guix/Quicklisp preflight;
- the mocked launcher cache regression plus a real non-preflight warmup and
  parallel GUI launch (preflight intentionally does not load `:clawmacs`);
- a fresh ASDF load of `:clawmacs/tests`;
- the full `clawmacs-suite` repeatedly, with zero failures and no worker from
  one run reaching another run's dynamic overrides;
- focused cancellation, stream terminal-race, blocked-connect, blocked-tool,
  teardown-generation, redisplay, menu, Drei, Edward, and interop race tests;
- two GUI suites launched concurrently without X display collision;
- real Xvfb `smoke`, `mx`, `features`, `keybinds`, `organa`, `quaestor`,
  `reload`, and `stability` suites;
- the dependency-free screenshot/event-log regression, private-cache policy
  regression, and Xvfb namespace/retry regression;
- terminal screenshots synchronized to an explicit non-repeating redisplay;
- a bounded process-resource profile and an extended 250-menu stress run;
- repeated pointer opening of the stable menu, presentation-based effort
  selection, window resize/expose, and a final responsive compose operation;
- no `Sheet ... is not grafted`, unhandled debugger, socket-listener debugger,
  or fatal compiler condition in application/harness artifacts;
- the minimal ESA-only control in `MCCLIM-ISSUES.md` still passes.

## Residual risks and follow-up plan

These are application risks, not confirmed current crashes and not McCLIM fork
candidates:

1. An arbitrary in-process tool already executing cannot be safely killed by
   Bordeaux Threads. Cancellation now keeps its buffer in `:stopping` and does
   not expose a successor runtime until that tool exits, but the tool may still
   have external side effects. Add a cancellation protocol to tool definitions
   and pass it to subprocess/network tools.
2. Safe reload still redefines the live image after an isolated preflight. A
   failure halfway through live ASDF loading has no image rollback. The durable
   solution is a supervised process restart with serialized session handoff;
   until then, keep the existing isolated preflight and treat live reload as an
   explicit development operation.
3. The transient minibuffer, selector, callback, and presentation-generation
   state is now owned by each application frame, but the buffer ring remains
   process-global. Current GUI use has one main frame per process (recurse opens
   another process); same-process multi-frame mode still needs frame-owned
   buffer sets before it is safe.
4. Post-header provider reads cancel immediately on the supported SBCL/Guix
   transport by shutting down the owned descriptor. This fast path is coupled
   to the pinned Drakma/Flexi/Chunga/CL+SSL wrapper structure and guarded by
   HTTP and TLS traversal regressions. Before Drakma returns a body stream,
   connect, TLS-handshake, and response-header work still has no registered
   handle; it is bounded by the explicit 20-second timeout while teardown keeps
   the buffer in `:stopping`. Other Lisp implementations fall back to that
   bounded behavior. Streaming requests use `decode-content NIL`, so no Chipz
   wrapper is currently part of the supported path.
5. Async subagents are process-global rather than children of one application
   frame. The current launcher exits the process after its sole main frame;
   long-lived multi-frame or embedded use should add an application supervisor.
6. Metadata help frames are independent CLIM frames on independent threads and
   are not yet children in a parent-frame supervisor. Track and destroy them if
   the application begins opening many help frames or supports long-lived
   multi-frame sessions.
7. If an unexpected teardown-reaper finalization error occurs, the worker root
   logs `runtime-worker-reaper-error` and deliberately retains the exact
   teardown with the buffer in `:stopping`. The finalization claim and reaper
   construction latch reset for a later frame wake, Stop, or disposal retry;
   repeated external resource failure can still leave the buffer safely
   blocked rather than falsely exposing it as idle.
8. Exact descendant containment for interactive subprocesses depends on the
   supported Linux runtime's util-linux `setsid` and procfs. The Guix and Nix
   environments now include util-linux. On an unsupported host without
   `setsid`, cancellation falls back to a bounded descendant snapshot, which
   cannot close every fork-after-snapshot race; add a platform-native session
   launcher before treating such a host as production-supported.
9. External event callbacks are bounded but still cooperative. One callback
   that never returns occupies one of four lanes; four such callbacks occupy
   every lane. Each lane retains at most 256 pending events and refuses newest
   events after saturation, while provider and interop ownership still
   settles. Safe reload deliberately refuses while any callback is active.
10. Package entrypoints, package presentation functions, and user hooks are
    trusted in-process extensions. Package enablement can synchronously `LOAD`
    an entrypoint on the CLIM command process; a hung entrypoint or presenter
    can therefore freeze the frame, and a failed `LOAD` cannot roll back
    arbitrary image mutations. Moving `LOAD` to an unsafely killable worker
    would trade a freeze for concurrent image corruption. The durable design
    is a supervised replacement process with serialized session handoff and
    package preflight.
11. MCP stdio servers, Artifactum extractors, package Git operations, listener
    background shells, and arbitrary custom in-process tools do not yet share
    the managed process-group/cancellation protocol. Some run off-frame, but a
    stalled child can retain its worker indefinitely and listener-launched
    descendants are not owned by frame teardown.
12. Package, configuration, project, manifest, and change-set locks
    are process-local. Atomic replacement prevents torn files in one process,
    but two Clawmacs processes can still produce last-writer-wins updates.
    Cross-process `project-create-file :if-exists :error` also has a narrow
    no-clobber race. A future design needs advisory locks or a single storage
    supervisor.

These follow-ups should be proven with the same rule: a deterministic barrier
or minimal GUI reproducer first, then a resource-count or responsiveness gate.

## Final validation record

Completed on 2026-07-14:

- Guix/Quicklisp preflight exited zero. The mocked launcher regression also
  passed, including cold bootstrap serialization, exact dependency warmup,
  lock-owner death, hostile source-registry replacement, the four GUI controls
  present at that checkpoint, and protection of the production cache and pin.
  Evidence:
  `.artifacts/stability-final-20260714T054821Z/final/preflight.log` and
  `guix-container-test-final.log`.
- A fresh `:clawmacs/tests` image ran `clawmacs-suite` twice without restarting
  SBCL. Each pass completed 4,430 checks: 4,430 pass, 0 skip, 0 fail. Evidence:
  `.artifacts/stability-final-20260714T054821Z/final/full-suite-twice-final.log`.
- Focused stable-menu, selector, dashboard, table-identity, and identical-table
  assignment coverage completed 59 checks. Evidence:
  `.artifacts/stability-final-20260714T054821Z/stable-menu-focused.log`.
- The real Xvfb `smoke` and `keybinds` suites ran concurrently from relative
  artifact paths; both exited naturally with status zero, passed the post-exit
  runtime scan, and left empty process groups. `mx` also passed with a host
  repository-absolute artifact path. Evidence is under
  `.artifacts/stability-final-20260714T054821Z/final/gui-parallel-final/` and
  `final/gui-batch-a/`.
- The complete real-GUI matrix passed: `smoke`, `mx`, `features`, `keybinds`,
  `organa`, `quaestor`, `reload`, and `stability`. Every summary reports
  `"ok": true`; every application exited naturally with status zero after
  `frame-stopped`, every process group was empty, and every post-exit scan was
  clean. Remaining summaries are under `final/gui-batch-a/` and
  `final/gui-batch-b/`.
- The corrected extended stress log proves exactly 100 stable-menu iterations:
  75 pointer opens/cancels and 25 CLX press-drag activations of the semantic
  effort selector. The presentation selector completed 13 `low` and 12
  `default` choices, followed by resize, expose, compose, streaming, and normal
  frame-exit checks. It exited zero with an empty group and no ungrafted-sheet,
  debugger, recovery, worker, cleanup, fatal-runtime, or heap signature.
  Evidence: `final/gui/stability-stable-menu100-final/actions.jsonl` and
  `summary.json`.
- Forced GUI cleanup covered natural exit, status propagation, `TERM`, bounded
  timeout, `KILL`, split `setsid` ownership, descendants, timer cancellation,
  and repeated cleanup. Evidence: `final/gui-cleanup-test.log`.
- Explicit provenance resolved `clim-core`, `clim`, `mcclim`, `mcclim-clx`,
  `esa-mcclim`, and `drei-mcclim` to the pinned Guix store tree. The Clawmacs-free
  ESA control produced `made frame state=:DISOWNED` and `run result=:OK`.
  Evidence: `final/mcclim-provenance.log` and
  `final/minimal-esa-control-final2.log`.
- Relative artifact paths now become absolute `/workspace/...` roots. Parent
  traversal, an outside-workspace absolute path, and an in-repository symlink
  escape were rejected before creating the outside target. Evidence:
  `final/path-validation/`.

Earlier failing reproductions and intentionally injected-condition logs remain
in sibling diagnostic artifact directories. They are evidence for the fixes,
not acceptance results; the settled green evidence is in the paths above.

### Post-removal full-trust validation

The permission, approval, guard, sandbox-label, and provider-reload-tool
surfaces were removed after the validation record above. The then-current tree
was revalidated independently on 2026-07-14:

- Guix/Quicklisp preflight exited zero, and the mocked container-launcher
  isolation regression printed `ok` and exited zero.
- One fresh Lisp image ran the complete `clawmacs-suite` twice. Each pass
  completed 4,321 checks: 4,321 pass, 0 skip, 0 fail. The lower count reflects
  deletion of obsolete guard/approval tests and migration of their remaining
  lifecycle assertions to the direct-dispatch contract. Evidence:
  `.artifacts/final-full-trust/unit/full-suite-twice.log`.
- The complete real-GUI matrix passed again: `smoke`, `features`, `mx`,
  `keybinds`, `quaestor`, `reload`, `organa`, and `stability`. Every
  `summary.json` reports `"ok": true`; every harness observed
  `frame-stopped`, a natural status-zero application exit, an empty process
  group, and a clean runtime-failure scan. Evidence:
  `.artifacts/final-full-trust/<suite>/`.
- The final stability suite exercised 24 stable-menu operations, three resize
  probes, six expose/unmap-map probes, model selection, provider completion,
  and a return to idle before normal frame exit.
- The cleanup regression passed natural exit, explicit status propagation,
  `TERM`, timeout/`KILL`, split-owner descendant, timer, and repeated-cleanup
  cases. Its expected status probes were `EXIT=37`, `TERM=143`, `NATURAL=0`,
  and `TIMEOUT=124`.
- Shell syntax, Python compilation, and `git diff --check` passed. The removed
  API sweep found only three intentional compatibility assertions: rejection
  of old tool metadata, stripping of legacy MCP permission JSON, and proof
  that `clawmacs_reload` is absent from the provider tool table.

### Post-adversarial closure — 2026-07-15

The follow-up investigation converted every application-, test-, or
harness-owned finding into an atomic change:

- `ea56ced5` makes incomplete Artifactum timestamps total and adds four
  regressions, including an on-disk legacy index;
- `d12d3130` and `da48b9d1` synchronize terminal screenshots to an explicitly
  non-repeating CLIM redisplay without adding a repaint path;
- `c09cce71` and `0dd1df5` seed private compile caches and enforce the lock,
  overlap, cold-clear, and sibling-preservation policy;
- `801fb6f6` tails event logs incrementally;
- `ff2ff3e1` synchronizes the intentionally asynchronous interop callback test;
  and
- `0f3687e4` diagnoses a contaminated X socket namespace and retries it once in
  a fresh Guix container without touching the inherited directory.

The final proof after those changes is:

- one fresh Guix Lisp image ran the full `clawmacs-suite` twice. Each pass
  completed 4,340 checks: 4,340 pass, 0 skip, 0 fail. The runner used
  `fiveam:results-status` and an explicit aggregate exit, rather than trusting
  `run!` to communicate failure through the process status. Evidence:
  `.artifacts/adversarial-mcclim/final-closure/unit/full-suite-two-pass.log`;
- the dependency-free driver completed 13/13 tests; cache, container, cleanup,
  and Xvfb namespace shell regressions and both the general and GUI Guix
  preflights exited zero. Evidence is in
  `.artifacts/adversarial-mcclim/final-closure/unit/`;
- the complete seeded real-GUI matrix passed in controlled concurrent pairs:
  `smoke`, `mx`, `features`, `keybinds`, `organa`, `quaestor`, `reload`, and
  `stability`. Every run observed `frame-stopped`, exited naturally with status
  zero, left an empty process group, passed the post-exit scan, and recorded
  terminal screenshot synchronization metadata. A separate
  `CLAWMACS_GUI_E2E_COLD_CACHE=1` smoke also passed from an empty private cache.
  Its frame-ready time was approximately 139 seconds, versus approximately 6
  seconds for the seeded smoke. Evidence:
  `.artifacts/adversarial-mcclim/final-closure/gui/`;
- the focused Artifactum suite completed 51/51 checks. The interop constructor
  regression completed 1,400/1,400 checks over 100 repetitions, including a
  callback held pending while failure and execution ownership settled;
- the extended stability run performed 188 pointer clicks and 62 drag
  activations for 250 menu operations, 62 semantic selector changes, 30
  expose/unmap-map operations, and three resizes. It completed 2,543 driver
  steps, then exited naturally with status zero, an empty process group, and a
  clean runtime scan. The incremental parser reduced driver time from 228 to
  190 seconds. Evidence:
  `.artifacts/adversarial-mcclim/parser-tail/stability-250/`;
- a separately sampled 100-menu/12-expose run distinguished cold compilation
  from the live UI. Its startup high-water mark was 772,376 KiB, including
  compilation. After excluding the one-time post-ready transition, early and
  late RSS medians were 394,128 and 394,136 KiB, the fitted slope was 0.015
  MiB/minute, file descriptors returned to 16, and threads returned to 3. This
  shows no accumulating resource trend during that bounded interval, not a
  universal no-leak guarantee. Evidence:
  `.artifacts/adversarial-mcclim/resource-profile/moderate-100-12/`;
- all six tested CLIM/ESA/Drei systems resolved to the pinned Guix McCLIM tree.
  The Clawmacs-free ESA control used a private Xvfb inside Guix and again
  produced `made frame state=:DISOWNED` and `run result=:OK`. Evidence:
  `.artifacts/adversarial-mcclim/final-closure/provenance/`.

No new failure meets the McCLIM fork threshold. The pointer-tracking robustness
locus and pinned ESA modifier-conversion limitation remain documented in
`MCCLIM-ISSUES.md`; every new crash, race, and performance cost above was owned
by Clawmacs, its tests, or its harness. Successful GUI artifacts currently
retain approximately 67–82 MiB of private cache per run; cleanup remains an
explicit evidence-retention decision rather than an automatic policy.
