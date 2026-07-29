# RPLACA GUI E2E

The GUI E2E framework is opt-in and runs the real McCLIM RPLACA frame inside
Xvfb. It drives the window with `xdotool`, observes structured `RPLACA_DEBUG_LOG`
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
./scripts/run-gui-e2e.sh --suite appearance
./scripts/run-gui-e2e.sh --suite menu-boundaries
./scripts/run-gui-e2e.sh --suite stability
```

ASDF integration is also opt-in and runs `smoke`, `mx`, `features`,
`keybinds`, `organa`, `quaestor`, `reload`, `appearance`, and `stability`:

```lisp
(asdf:test-system :rplaca/gui-e2e)
```

The default FiveAM unit suite only covers E2E primitives; it does not launch
Xvfb or the GUI smoke test.

The dependency-free Python regression checks final-screenshot event ordering,
wrong-buffer/repeated-redisplay rejection, and the final X server round trip
without launching RPLACA or Xvfb:

```sh
python3 ./scripts/test_gui_e2e_driver.py
```

## Isolation

The harness:

- sets `RPLACA_CONTAINER_DISABLE_HOST_X=1` before entering the Guix wrapper;
- requires host `flock`, then serializes project Quicklisp bootstrap and an
  exact `:rplaca` dependency warmup under a host advisory lock with a
  600-second acquisition bound; it releases the lock before starting the GUI,
  so concurrent suites only read installed releases while compiling into
  their artifact-local caches;
- seeds only the prewarmed `common-lisp` tree into each artifact-local cache
  while holding a shared lock against later warmups; the seed uses a reflink
  when supported and otherwise copies files, never hard-links them, so every
  suite continues writing only to its private `HOME` and `XDG_CACHE_HOME`;
- asks `Xvfb -displayfd` to allocate a free private display, with access control
  disabled on its container-local Unix socket and TCP listening disabled, then
  connects clients through `:<display>`;
- requires `/tmp/.X11-unix` to be absent before Xvfb starts; a pre-existing
  directory means the container inherited or exposed another X socket
  namespace, so the inner harness exits before touching it and the host wrapper
  retries exactly once in a fresh Guix container; the harness never removes or
  changes ownership of an existing X socket directory;
- sets `HOME` and `XDG_CACHE_HOME` under the artifact directory;
- canonicalizes relative or repository-absolute artifact paths to an absolute
  `/workspace/...` path before deriving `HOME` and `XDG_CACHE_HOME`, because
  ASDF requires absolute cache roots;
- clears run-owned logs, process IDs, summaries, actions, and screenshots when
  an explicit artifact directory is reused, while retaining only its isolated
  `home/` and `cache/`; stale `frame-ready` data can therefore never satisfy a
  new launch;
- launches SBCL in a new session and has the inner session leader publish its
  exact PGID; this remains correct whether util-linux `setsid --wait` execs in
  place or forks a distinct waitable owner;
- waits for both the waitable owner and every live application-group member
  after `frame-stopped`, and signals the exact group during emergency teardown
  so members remaining in that group cannot escape the harness; managed
  subprocesses that create their own sessions remain owned by RPLACA's
  separate process-group teardown;
- sets `RPLACA_GUI_E2E=1`, `RPLACA_E2E_EVENTS=1`, and
  `RPLACA_E2E_PROVIDER=1`;
- unsets provider credential environment variables before launching SBCL;
- sets `rplaca:*inhibit-user-init*` before `rplaca-main`.

No real provider network call or user secret is required for the GUI E2E suites.
The frame-ready wait defaults to 300 seconds so a cold McCLIM compilation can
finish; set `RPLACA_GUI_E2E_FRAME_READY_TIMEOUT_SECONDS` to a positive integer
to override it. Set `RPLACA_GUI_E2E_COLD_CACHE=1` to remove any existing,
validated artifact-local `cache/common-lisp` tree before startup while
preserving sibling state. Invalid or overlapping roots and a failed cold-cache
clear abort safely. An unavailable or safely failed seed copy continues with
an empty private cache and never shares writable cache state.

After the semantic `frame-stopped` event, the natural SBCL exit is bounded to
30 seconds. Set `RPLACA_GUI_E2E_APP_EXIT_TIMEOUT_SECONDS` to a positive
integer to override that teardown deadline.

Every external driver helper has a 15-second deadline, and image capture uses
the shorter 10-second deadline. A vanished X window therefore becomes a
diagnostic `DriverError` with any partial command output instead of leaving the
harness blocked in ImageMagick or `xwd`.

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

The cache-seeding regression verifies isolation, timestamp preservation,
warmup-lock coordination, parallel private copies, overlapping-root rejection,
and that lock timeout cannot copy or leave a staging tree. It also reuses an
artifact to prove cold mode clears only its validated `common-lisp` tree and
preserves sibling cache state:

```sh
sh ./scripts/test-gui-e2e-cache.sh
```

The artifact-lifecycle regression seeds a stale `frame-ready` event, window
ID, summary, action log, and nested screenshot, then proves a reset removes
every run-owned output while preserving the artifact-private home and cache:

```sh
sh ./scripts/test-gui-e2e-artifacts.sh
```

The Xvfb namespace regression proves the fresh-container retry is bounded,
unrelated launcher failures are not retried, and a disposable read-only
`/tmp/.X11-unix` exposure is diagnosed before Xvfb starts or the directory is
changed. It does not launch SBCL or RPLACA:

```sh
sh ./scripts/test-gui-e2e-xvfb.sh
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
- `app.stdout`, `app.stderr` — RPLACA process output;
- `app.pgid` — exact application session/process-group ID published by the
  session leader;
- `driver.stdout`, `driver.stderr` — Python driver output;
- `harness.log` — shell harness milestones;
- `xvfb.display`, `xvfb.log` — allocated display number and X server log;
- `home/` — isolated runtime configuration and session state;
- `cache/common-lisp/` — private seeded and subsequently generated ASDF
  output;
- `actions.jsonl` — driver step/action log;
- `screenshots/*.png` (or `.xwd` fallback) — visual artifacts;
- `summary.json` — pass/fail summary, screenshot records, repo-relative paths,
  per-screenshot snapshot sequence/status, and the last snapshot.

The `appearance` lifecycle suite keeps the first fresh process's evidence in
`appearance-stage/` before starting its second fresh process.  Only the
artifact-private `home/` and `cache/` cross that boundary; logs, window IDs,
screenshots, summaries, Xvfb state, and the application process group are
reset.  The second launch therefore proves persisted configuration, not stale
frame state.

Successful adversarial runs retained approximately 67–82 MiB of private cache
per run. This is intentional isolation evidence, but the harness has no
automatic artifact-retention policy; remove old run directories manually when
their evidence is no longer needed.

Passing `--artifact-dir` with an existing directory starts a new evidence run:
the harness resets the files listed above and `screenshots/`, but deliberately
reuses `home/` and `cache/`. Preserve an old run under another directory before
reusing its path when its diagnostics are still needed.

The driver correlates every screenshot with the latest `ui-snapshot` event. The
snapshot is semantic state (transcript, compose text, status/model line,
selector/toggle state, and minibuffer text), not OCR.

A final semantic snapshot is accepted immediately only when its reason is
`redisplay-handled` and it explicitly carries `repeat: false`. A repeated or
legacy snapshot, or an earlier semantic state, requires a later same-buffer
`redisplay-handled` event with `repeat: false`. The driver then performs a
separate-connection X server round trip as a responsiveness and settling point;
because it is not a cross-client ordering fence, the explicit non-repeating
CLIM event remains the primary ordering gate. Final screenshot records include
`final_snapshot_sequence`, `redisplay_sequence`, and
`redisplay_already_handled` for inspection.

The driver tails `debug.log` by file identity and byte offset. It retains an
incomplete trailing line until the writer finishes it, consumes appended event
records once, and resets its cache when the log is replaced, shrinks, or has a
different same-inode rewrite. Disk reads and JSON decoding consume only newly
appended complete bytes; cached event references may still be copied or scanned
by polling helpers. The production writer is append-only. An arbitrary
truncate/regrow that recreates the same 256-byte consumed suffix before the
next poll is outside this guarantee.

## Smoke behavior

The current `smoke` suite:

1. waits for `frame-ready` and a visible `RPLACA E2E` X window;
2. captures an initial screenshot;
3. focuses the compose pane and types `hello`;
4. waits for `compose_text` to equal `hello` in a `ui-snapshot`;
5. presses Return;
6. waits for the deterministic response containing
   `RPLACA_E2E_HELLO_SENTINEL`, provider completion, and a final idle
   `ui-snapshot` containing the response;
7. captures a final screenshot after the rendered idle state.

## M-x behavior

The `mx` suite opens a fresh GUI and exercises the Emacs-style extended command
flow from the compose pane:

1. waits for `frame-ready` and focuses the RPLACA window;
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
pass for user-facing RPLACA features that can be driven safely inside the
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
main default `M-x`, `C-c`, `C-h`, and `C-x` bindings.

Its `C-x b` regression starts the frame with focus deliberately assigned to
ESA's standard-input stream, then requires selector activation to transfer
focus to the Drei compose pane that owns non-blocking modal input. The fixture
creates twelve extra buffer candidates and an exact 32 KiB multiline compose
draft with distinct point and mark offsets. Bounded physical lines isolate
selector performance from the upstream Drei long-line redisplay defect
documented in `MCCLIM-ISSUES.md`. The driver then:

1. proves every visible candidate row fits above the frame's
   pointer-documentation pane;
2. sends repeated filters and a 50-key burst without per-key waits, then
   requires a quiescent focused snapshot within five seconds;
3. cancels and proves the original buffer, draft, point, and mark are unchanged
   and the minibuffer has collapsed;
4. exercises empty-query activation, exact-name ranking among prefix
   neighbors, and ambiguous fuzzy-query confirmation while checking the
   selected buffer and retained per-buffer editor state;
   and
5. repeats the geometry check after expansion and collapse.

`C-x C-b` remains a separate ordinary activation check. The scenario uses only
the application's standard CLIM command, pane, focus, and layout contracts;
coordinates are observed by the external driver solely to verify rendered pane
separation.

## Safe reload behavior

The `reload` suite exercises the real safe in-place reload flow from the GUI. It
keeps a compose draft visible, invokes `safe-reload-rplaca-command` through
`M-x`, waits for the `safe-reload-result` debug event from the isolated
preflight plus live reload, and asserts the semantic snapshot still shows the
same buffer and compose draft along with the success notification.

## Appearance lifecycle behavior

The `appearance` suite is a two-process, private-Xvfb proof of the frame-owned
appearance contract.  Its first fresh Guix/Xvfb process starts with `:classic`,
advances the frame-local public CLX font inventory over the current live cache
without invalidating shared McCLIM font mappings, verifies the `C-h F` and
`C-c F` compatibility commands open the presentation-backed editor, stages
`:dark`, and verifies the editor's Active/Staged/preview presentations. An
Apply is required to remain `restart-required`: active theme and revision stay
classic/unchanged while the complete dark candidate remains staged. Explicit
Save then writes only the artifact-private appearance file.

The wrapper starts a second fresh Guix container, private Xvfb, SBCL image, and
application process with the same isolated HOME. It requires startup to select
`dark` and checks catalog, profile, and font-inventory generations agree with
the resolved bundle. It also records the portable transcript and compose
surface declarations; screenshots are retained as secondary visual evidence.
This does not infer correctness from pixel colors or reach into a McCLIM
medium, Drei view, output record, or private font cache.

`scripts/probe-clx-font-inventory.sh` is the smaller manual counterpart. It
always enters the Guix E2E wrapper, starts its own `Xvfb -displayfd` server, and
requires both `CLX_FONT_INVENTORY_PROBE_OK` and
`CLX_FONT_INVENTORY_PROBE_SHELL_OK` markers. A missing payload marker fails the
probe even if the outer shell happened to exit successfully.

The complete appearance stability matrix deliberately keeps each claim in its
smallest deterministic gate:

- `appearance`: startup/staging/save/restart, editor presentations and
  compatibility keys, public live font refresh, bundle-generation coherence;
- `keybinds`: `C-x F`, `C-h F`, `C-c F`, a 32 KiB multiline draft, selector
  focus, and pointer-documentation separation;
- `compose-geometry`: the bounded 100/101/112-character Drei regression;
- `stability`: pointer menu operation, resize/expose, and the post-exit
  empty-process-group/runtime-signature gate;
- `menu-boundaries`: the focused leaf-only menu crossover, off-window,
  selector-dispatch, responsiveness, and cleanup regression;
- `reload`: the public Safe Reload command with draft retention; and
- focused `appearance`, `mcclim-interface`, and `safe-reload` FiveAM suites:
  independent two-frame profiles/caches, render-boundary foreground commit and
  failed activation rollback, package-removal refusal rollback, and Safe
  Reload's no-preferences-I/O invariant.

## Stability behavior

The `stability` suite is the bounded real-window regression gate for menu-sheet,
expose, and redisplay failures. It uses an effort-capable model fixture without
making a provider request, then:

1. first relies on headless structure coverage that proves the named menu-bar
   table contains only direct `:command` leaves and no nested `:menu` entries;
   the initial real-window screenshot then confirms those exact labels are the
   attached visible bar (CLIM exports no portable frame menu-table accessor);
2. holds button 1 while crossing every direct menu entry, leaves the window,
   and releases outside, then repeats 40 no-delay Appearance/Help-equivalent
   crossings through the former submenu area. A distinct compose probe after
   each path proves the frame remains responsive;
3. repeatedly activates the direct `Effort...` leaf, cancels it or chooses
   alternating `low` and `default` values through the frame-owned presentation
   selector, and waits for each semantic confirmation;
4. resizes the X window through several geometries and verifies the Drei compose
   pane accepts and clears a distinct probe after every resize;
5. repeatedly unmaps and remaps the window to force expose processing, again
   proving compose input remains responsive after every cycle;
6. switches back to the deterministic `e2e/e2e-model`, submits `hello`, and
   requires the complete sentinel response and final idle redisplay; and
7. captures the final rendered state, sends the ordinary `C-x C-c` exit command,
   and requires a `frame-stopped` event emitted after frame unwind cleanup; and
8. fails if artifacts contain a debugger entry, an ungrafted-sheet condition,
   the historical menu recovery marker, a redisplay, worker-start, help-frame,
   or cleanup diagnostic, or an SBCL fatal, unhandled-thread, or
   heap-exhaustion report.

The same post-exit artifact scan now runs for every GUI suite; `stability`
additionally performs an in-scenario scan immediately after its stress actions.

The default is 24 menu cycles and 6 unmap/map cycles. Set
`RPLACA_GUI_E2E_STABILITY_MENU_ITERATIONS` or
`RPLACA_GUI_E2E_STABILITY_EXPOSE_ITERATIONS` to a positive integer for a
longer bounded stress run; the Guix launcher preserves both overrides into the
container. Extended adversarial validation has run both 100- and
250-menu-cycle configurations.
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

1. starts RPLACA with the bundled `quaestor` package enabled;
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
