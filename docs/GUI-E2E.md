# Clawmacs GUI E2E

The GUI E2E framework is opt-in and runs the real McCLIM Clawmacs frame inside
Xvfb. It drives the window with `xdotool`, observes structured `CLAWMACS_DEBUG_LOG`
E2E events, and captures screenshots for every scripted suite step.

## Commands

```sh
./scripts/run-gui-e2e.sh --preflight-only
./scripts/run-gui-e2e.sh --suite smoke
./scripts/run-gui-e2e.sh --suite mx
./scripts/run-gui-e2e.sh --suite features
./scripts/run-gui-e2e.sh --suite keybinds
./scripts/run-gui-e2e.sh --suite organa
./scripts/run-gui-e2e.sh --suite quaestor
```

ASDF integration is also opt-in and runs `smoke`, `mx`, `features`,
`keybinds`, `organa`, and `quaestor`:

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

No real provider network call or user secret is required for the GUI E2E suites.

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
10. offline skills help, package dashboard, and guard-policy help buffers.

## Keybinding behavior

The `keybinds` suite drives the real compose pane and ESA command table with
physical key chords. It asserts that Drei-owned editing keys still mutate the
compose text, that application keybindings emit the expected normalized command
event, and that modal selectors/prompts can be cancelled. The suite covers the
main default `M-x`, `C-c`, `C-h`, and `C-x` bindings, including the `C-x b`
regression path: `C-x b` and `C-x C-b` must open the minibuffer buffer selector
without crashing the frame.

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
