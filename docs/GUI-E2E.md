# RPLACA Listener GUI E2E

The opt-in GUI E2E system runs the real `rplaca-listener` frame in an isolated
Xvfb display. It drives the CLIM interactor with `xdotool`, observes structured
`RPLACA_DEBUG_LOG` events, and captures screenshots at milestones and failures.
The deterministic E2E provider makes no network calls and needs no credentials.

## Commands

Run the environment check or the listener scenario from the repository root:

```sh
./scripts/run-gui-e2e.sh --preflight-only
./scripts/run-gui-e2e.sh --suite listener
```

ASDF runs the same listener suite:

```lisp
(asdf:test-system :rplaca/gui-e2e)
```

The normal FiveAM system tests the E2E helpers but does not launch Xvfb or the
GUI. The dependency-free driver regression is also available:

```sh
python3 ./scripts/test_gui_e2e_driver.py
```

## Listener Coverage

The `listener` suite checks the listener-first contract through the real frame:

1. Start `rplaca-listener` and wait for its frame-ready event and X window.
2. Confirm the initial `CL-USER>` prompt and listener-only layout.
3. Submit Lisp, command, and prose input through the same chronological
   interactor.
4. Verify a prose turn withholds the next prompt while the provider is active,
   reports progress in the wholine, and writes only the settled final answer
   inline.
5. Open a non-empty turn facet in `listener+details`, close it, and confirm the
   frame returns to `listener-only` without losing interactor history.
6. Exercise Say mode and the quote/unquote escapes, including return to Lisp
   mode.
7. Cancel an active response and verify the listener waits for cancellation to
   settle before accepting another input.
8. Resume a saved session and verify its branch is replayed once in the same
   interactor.
9. Exit through the ordinary CLIM frame command and wait for `frame-stopped`.

Provider failures are a separate negative path. The suite requires an inline
terminal error, an idle wholine, no partial assistant body, and a fresh prompt
only after the failed request has settled.

## Isolation

The harness:

- enters the Guix E2E wrapper with host X access disabled;
- uses `Xvfb -displayfd` for a private display and rejects inherited X socket
  namespaces;
- assigns a private `HOME` and `XDG_CACHE_HOME` under the artifact directory;
- serializes Quicklisp warmup, then gives each run its own writable cache;
- sets `RPLACA_GUI_E2E=1`, `RPLACA_E2E_EVENTS=1`, and
  `RPLACA_E2E_PROVIDER=1`;
- clears provider credentials and inhibits the user init file;
- starts the application in a dedicated process group; and
- bounds driver helpers, image capture, application exit, and emergency
  teardown.

Every successful run exits through the normal CLIM command. The harness then
waits for natural status-zero process exit and scans `debug.log`, `app.stdout`,
and `app.stderr` for debugger entry, McCLIM recovery diagnostics, unhandled
thread conditions, fatal runtime errors, and heap exhaustion.

## Artifacts

Each run writes to `.artifacts/gui-e2e/<timestamp-pid>/` by default:

- `debug.log` with ordinary diagnostics and `[e2e-event]` JSON records;
- `app.stdout` and `app.stderr` from RPLACA;
- `driver.stdout` and `driver.stderr` from the external driver;
- `harness.log`, `app.pgid`, `xvfb.display`, and `xvfb.log`;
- `actions.jsonl` with driver steps;
- `screenshots/` with milestone and failure images;
- `summary.json` with pass/fail state and semantic snapshot references; and
- private `home/` and `cache/common-lisp/` trees.

An explicit `--artifact-dir` starts a new evidence run in that directory. The
harness clears old run outputs but retains the private home and cache. Preserve
an earlier artifact directory under another name before reusing it if its
evidence still matters.

Semantic snapshots describe listener state, including the interactor, prompt,
input mode, details layout, wholine, and pointer documentation. Screenshots are
visual evidence, not the source of semantic assertions.
