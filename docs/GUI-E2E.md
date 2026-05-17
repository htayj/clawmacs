# Clawmacs GUI E2E

The GUI E2E framework is opt-in and runs the real McCLIM Clawmacs frame inside
Xvfb. It drives the window with `xdotool`, observes structured `CLAWMACS_DEBUG_LOG`
E2E events, and captures screenshots for every smoke step.

## Commands

```sh
./scripts/run-gui-e2e.sh --preflight-only
./scripts/run-gui-e2e.sh --suite smoke
```

ASDF integration is also opt-in:

```lisp
(asdf:test-system :clawmacs/gui-e2e)
```

The default FiveAM unit suite only covers E2E primitives; it does not launch
Xvfb or the GUI smoke test.

## Isolation

The harness:

- sets `CLAWMACS_CONTAINER_DISABLE_HOST_X=1` before entering the Guix wrapper;
- starts a private `Xvfb` display in the container with an artifact-local
  Xauthority cookie;
- sets `HOME` and `XDG_CACHE_HOME` under the artifact directory;
- sets `CLAWMACS_GUI_E2E=1`, `CLAWMACS_E2E_EVENTS=1`, and
  `CLAWMACS_E2E_PROVIDER=1`;
- unsets provider credential environment variables before launching SBCL;
- sets `clawmacs:*inhibit-user-init*` before `clawmacs-main`.

No real provider network call or user secret is required for the smoke suite.

## Artifacts

Each run writes under `.artifacts/gui-e2e/<timestamp-pid>/` by default:

- `debug.log` — normal debug log plus `[e2e-event]` JSON records;
- `app.stdout`, `app.stderr` — Clawmacs process output;
- `driver.stdout`, `driver.stderr` — Python driver output;
- `harness.log` — shell harness milestones;
- `actions.jsonl` — driver step/action log;
- `screenshots/*.png` (or `.xwd` fallback) — visual artifacts;
- `summary.json` — pass/fail summary, screenshot records, repo-relative paths,
  per-screenshot snapshot sequence/status, and the last snapshot.

The driver correlates every screenshot with the latest `ui-snapshot` event. The
snapshot is semantic state (transcript, compose text, status/model line, and
minibuffer text), not OCR.

## Smoke behavior

The current smoke suite:

1. waits for `frame-ready` and a visible `Clawmacs E2E` X window;
2. captures an initial screenshot;
3. focuses the compose pane and types `hello`;
4. waits for `compose_text` to equal `hello` in a `ui-snapshot`;
5. presses Return;
6. waits for the deterministic response containing
   `CLAWMACS_E2E_HELLO_SENTINEL`, provider completion, and a final idle
   `ui-snapshot` containing the response;
7. captures a final screenshot after the rendered idle state.
