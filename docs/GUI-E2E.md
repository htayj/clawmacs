# Clawmacs GUI E2E

The GUI E2E framework is opt-in and runs the real McCLIM Clawmacs frame inside
Xvfb. It drives the window with `xdotool`, observes structured `CLAWMACS_DEBUG_LOG`
E2E events, and captures screenshots at scripted milestones and on failures.

## Commands

```sh
./scripts/run-gui-e2e.sh --preflight-only
./scripts/run-gui-e2e.sh --suite smoke
./scripts/run-gui-e2e.sh --suite mx
./scripts/run-gui-e2e.sh --suite features
./scripts/run-gui-e2e.sh --suite keybinds
./scripts/run-gui-e2e.sh --suite organa
./scripts/run-gui-e2e.sh --suite quaestor
./scripts/run-gui-e2e.sh --suite reload
./scripts/run-gui-e2e.sh --suite stability
```

ASDF integration is also opt-in and runs `smoke`, `mx`, `features`,
`keybinds`, `organa`, `quaestor`, `reload`, and `stability`:

```lisp
(asdf:test-system :clawmacs/gui-e2e)
```

The default FiveAM unit suite only covers E2E primitives; it does not launch
Xvfb or the GUI smoke test.

The dependency-free Python regression checks final-screenshot event ordering,
wrong-buffer/repeated-redisplay rejection, and the X request/reply barrier
without launching Clawmacs or Xvfb:

```sh
python3 ./scripts/test_gui_e2e_driver.py
```

## Isolation

The harness:

- sets `CLAWMACS_CONTAINER_DISABLE_HOST_X=1` before entering the Guix wrapper;
- requires host `flock`, then serializes project Quicklisp bootstrap and an
  exact `:clawmacs` dependency warmup under a host advisory lock with a
  600-second acquisition bound; it releases the lock before starting the GUI,
  so concurrent suites only read installed releases while compiling into
  their artifact-local caches;
- asks `Xvfb -displayfd` to allocate a free private display, with access control
  disabled on its container-local Unix socket and TCP listening disabled, then
  connects clients through `:<display>`;
- sets `HOME` and `XDG_CACHE_HOME` under the artifact directory;
- canonicalizes relative or repository-absolute artifact paths to an absolute
  `/workspace/...` path before deriving `HOME` and `XDG_CACHE_HOME`, because
  ASDF requires absolute cache roots;
- launches SBCL in a new session and has the inner session leader publish its
  exact PGID; this remains correct whether util-linux `setsid --wait` execs in
  place or forks a distinct waitable owner;
- waits for both the waitable owner and every live application-group member
  after `frame-stopped`, and signals the exact group during emergency teardown
  so members remaining in that group cannot escape the harness; managed
  subprocesses that create their own sessions remain owned by Clawmacs'
  separate process-group teardown;
- sets `CLAWMACS_GUI_E2E=1`, `CLAWMACS_E2E_EVENTS=1`, and
  `CLAWMACS_E2E_PROVIDER=1`;
- unsets provider credential environment variables before launching SBCL;
- sets `clawmacs:*inhibit-user-init*` before `clawmacs-main`.

No real provider network call or user secret is required for the GUI E2E suites.
The frame-ready wait defaults to 300 seconds so a cold McCLIM compilation can
finish; set `CLAWMACS_GUI_E2E_FRAME_READY_TIMEOUT_SECONDS` to a positive integer
to override it.

After the semantic `frame-stopped` event, the natural SBCL exit is bounded to
30 seconds. Set `CLAWMACS_GUI_E2E_APP_EXIT_TIMEOUT_SECONDS` to a positive
integer to override that teardown deadline.

The harness runs SBCL non-interactively with the debugger disabled, preserves
ordinary command exit status, and maps `INT`/`TERM` to nonzero status while
reaping application and X server children. Emergency
cleanup uses one monotonic deadline across both owner and group waits: it sends
`TERM` to the isolated application group, escalates that group to `KILL`, and
never turns an application-exit timeout into an unbounded shell `wait`. A
single Python timer enforces the outer shell wait; it verifies the same parent
identity before signalling and is cancelled on normal and trapped exits. The
natural-exit path rejects a zero-status owner when a live group member remains.
The focused shell regression forces both util-linux ownership shapes and
includes a TERM-ignoring leader plus descendant:

```sh
sh ./scripts/test-gui-e2e-cleanup.sh
```

The Guix launcher regression uses mocks plus a validated test-only cache root
and pin-file copy; it never rewrites the production Quicklisp tree. It proves
the launcher's concurrent-cold-bootstrap and serialized-warmup control flow,
warmup-failure gating, and lock release after abrupt owner death:

```sh
sh ./scripts/test-guix-container.sh
```

Every successful driver scenario exits through the ordinary `C-x C-c` CLIM
command and waits for the semantic `frame-stopped` event. The shell harness
then waits for the SBCL child to exit naturally with status zero; the EXIT trap
is only emergency cleanup. After the child is reaped, every suite scans
`debug.log`, `app.stdout`, and `app.stderr` for debugger entry, McCLIM recovery
diagnostics, unhandled thread conditions, fatal runtime errors, and heap
exhaustion. This final scan includes output emitted during frame teardown.

## Artifacts

Each run writes under `.artifacts/gui-e2e/<timestamp-pid>/` by default:

- `debug.log` — normal debug log plus `[e2e-event]` JSON records;
- `app.stdout`, `app.stderr` — Clawmacs process output;
- `app.pgid` — exact application session/process-group ID published by the
  session leader;
- `driver.stdout`, `driver.stderr` — Python driver output;
- `harness.log` — shell harness milestones;
- `xvfb.display`, `xvfb.log` — allocated display number and X server log;
- `actions.jsonl` — driver step/action log;
- `screenshots/*.png` (or `.xwd` fallback) — visual artifacts;
- `summary.json` — pass/fail summary, screenshot records, repo-relative paths,
  per-screenshot snapshot sequence/status, and the last snapshot.

The driver correlates every screenshot with the latest `ui-snapshot` event. The
snapshot is semantic state (transcript, compose text, status/model line,
selector/toggle state, and minibuffer text), not OCR.

## Smoke behavior

The current `smoke` suite:

1. waits for `frame-ready` and a visible `Clawmacs E2E` X window;
2. captures an initial screenshot;
3. focuses the compose pane and types `hello`;
4. waits for `compose_text` to equal `hello` in a `ui-snapshot`;
5. presses Return;
6. waits for the deterministic response containing
   `CLAWMACS_E2E_HELLO_SENTINEL`, provider completion, and a final idle
   `ui-snapshot` containing the response;
7. captures a final screenshot after the rendered idle state.

## M-x behavior

The `mx` suite opens a fresh GUI and exercises the Emacs-style extended command
flow from the compose pane:

1. waits for `frame-ready` and focuses the Clawmacs window;
2. sends `Escape` then `x` as the robust keyboard equivalent of `M-x`;
3. waits for the minibuffer snapshot to show `M-x`;
4. types the fuzzy abbreviation `tdbg`;
5. waits for the semantic minibuffer snapshot to list
   `toggle-debug-mode-command` as the selected candidate;
6. presses Return;
7. waits for the transcript snapshot to contain `[Debug mode ON...` and for the
   minibuffer to deactivate;
8. captures screenshots for the initial, minibuffer-open, command-typed, and
   final result states.

## Broad feature behavior

The `features` suite is the broad deterministic no-provider-network coverage
pass for user-facing Clawmacs features that can be driven safely inside the
isolated Xvfb session. It preserves `smoke` and `mx` as focused suites and adds
coverage for:

1. compose-pane text editing: typing, `C-a`, `C-e`, `C-b`, and `C-k`;
2. minibuffer text editing during `M-x`: `C-b`, `M-b` via `Escape b`, `C-a`,
   `C-e`, and `C-u`;
3. fuzzy `M-x` execution of a command after editing the minibuffer query;
4. help/introspection via `describe-bindings-command`;
5. buffer creation and fuzzy buffer switching;
6. agent/model selectors using the deterministic `e2e/e2e-model` provider;
7. unavailable think-level feedback for models without think levels;
8. prompted command entry through `set-session-display-name-command`;
9. session save feedback;
10. offline skills help and the package dashboard.

## Keybinding behavior

The `keybinds` suite drives the real compose pane and ESA command table with
physical key chords. It asserts that Drei-owned editing keys still mutate the
compose text, that application keybindings emit the expected normalized command
event, and that modal selectors/prompts can be cancelled. The suite covers the
main default `M-x`, `C-c`, `C-h`, and `C-x` bindings, including the `C-x b`
regression path: `C-x b` and `C-x C-b` must open the minibuffer buffer selector
without crashing the frame.

## Safe reload behavior

The `reload` suite exercises the real safe in-place reload flow from the GUI. It
keeps a compose draft visible, invokes `safe-reload-clawmacs-command` through
`M-x`, waits for the `safe-reload-result` debug event from the isolated
preflight plus live reload, and asserts the semantic snapshot still shows the
same buffer and compose draft along with the success notification.

## Stability behavior

The `stability` suite is the bounded real-window regression gate for menu-sheet,
expose, and redisplay failures. It uses an effort-capable model fixture without
making a provider request, then:

1. repeatedly opens and cancels the stable, frame-declared Effort submenu
   through pointer events;
2. periodically activates its `Select Think Level...` command with CLX's
   press-drag-release gesture, then chooses alternating `low` and `default`
   values through the frame-owned presentation selector and waits for each
   semantic confirmation;
3. resizes the X window through several geometries and verifies the Drei compose
   pane accepts and clears a distinct probe after every resize;
4. repeatedly unmaps and remaps the window to force expose processing, again
   proving compose input remains responsive after every cycle;
5. switches back to the deterministic `e2e/e2e-model`, submits `hello`, and
   requires the complete sentinel response and final idle redisplay; and
6. captures the final rendered state, sends the ordinary `C-x C-c` exit command,
   and requires a `frame-stopped` event emitted after frame unwind cleanup; and
7. fails if artifacts contain a debugger entry, an ungrafted-sheet condition,
   the historical menu recovery marker, a redisplay, worker-start, help-frame,
   or cleanup diagnostic, or an SBCL fatal, unhandled-thread, or
   heap-exhaustion report.

The same post-exit artifact scan now runs for every GUI suite; `stability`
additionally performs an in-scenario scan immediately after its stress actions.

The default is 24 menu cycles and 6 unmap/map cycles. Set
`CLAWMACS_GUI_E2E_STABILITY_MENU_ITERATIONS` or
`CLAWMACS_GUI_E2E_STABILITY_EXPOSE_ITERATIONS` to a positive integer for a
longer bounded stress run; the Guix launcher preserves both overrides into the
container. The release validation also runs 100 menu cycles.
Pointer coordinates exist only in the external driver; application interaction
continues through CLIM commands, presentations, and normal redisplay.

## Organa package behavior

The `organa` suite verifies package-owned McCLIM presentation hooks without
writing to the checkout:

1. creates a deterministic `.org` fixture under the run artifact directory;
2. opens it through `M-x organa-open-todo-file-command`;
3. waits for the custom `:organa` buffer presentation to render the dashboard;
4. cycles the same buffer through kanban, dependency, and outline views with
   `M-x organa-cycle-view-command`;
5. asserts semantic snapshots for each view and captures screenshots.

## Quaestor package behavior

The `quaestor` suite verifies the package-owned active request overlay:

1. starts Clawmacs with the bundled `quaestor` package enabled;
2. installs an E2E-only `*initial-buffer-hook*` that opens a deterministic
   `quaestor-request-user-input` request in the first chat buffer;
3. waits for the generic input-presentation overlay to show the question,
   options, notes area, and submit affordance;
4. answers via keyboard navigation and text entry; and
5. verifies the answered summary appears and the overlay disappears.

Credentialed and live-provider features are intentionally excluded from GUI E2E:
OpenAI/OpenRouter/ZAI network requests, OpenAI Codex OAuth browser login, remote
model discovery against real provider APIs, external MCP/API credentials, and
project file writes into the checkout. Those paths remain covered by unit tests,
prompt harness tests, or manual credentialed checks.
