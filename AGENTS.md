# Repository Guidelines

## Project Structure & Module Organization
`src/` holds the Common Lisp application, loaded serially by [`rplaca.asd`](rplaca.asd). Keep new modules in dependency order there: core data structures first, UI-independent runtime code and `main.lisp` later. `tests/` contains FiveAM suites named `*-test.lisp`. `scripts/` contains environment wrappers, `docs/` stores design and process notes, and `docs/img/` holds screenshots and other documentation assets.

## Build, Test, and Development Commands
Use the container wrappers instead of invoking SBCL directly.

- `./run.sh` launches the McCLIM UI in the Guix container.
- `./scripts/guix-container.sh --preflight-only --mode run -- true` validates the local Guix/Quicklisp setup without starting the app.
- `./scripts/guix-container.sh --mode run -- sh -lc 'sbcl --noinform --load "$RPLACA_QUICKLISP_SETUP" --eval "(push (truename \".\") asdf:*central-registry*)" --eval "(ql:quickload :rplaca/tests)" --eval "(fiveam:run! (quote rplaca/tests::rplaca-suite))" --eval "(quit)"'` runs the unit suite.

## Coding Style & Naming Conventions
Follow the repository’s Common Lisp posture in `docs/DESIGN.md`: functional-first, immutable by default, and explicit about side effects. Use standard Lisp indentation with aligned keyword arguments, lowercase-hyphenated symbol names, and `*earmuffed*` globals for special variables. Export public API from `src/packages.lisp`, and add SBCL type declarations where they clarify intent. No formatter or linter is enforced here, so match nearby code exactly.

## McCLIM Interface Work
For any McCLIM UI work, use the `mcclim-manual` skill first and treat canonical CLIM/McCLIM patterns as the default design constraint.

- Application frames are the application object. They own UI state, command tables, pane layout, and standard CLIM streams. Prefer `define-application-frame`, `make-application-frame`, and `run-frame-top-level` over ad hoc top-level windows.
- Panes divide the interface into functional regions. Declare them in the frame. Use `:application` panes for display-function-driven output, presentations, formatted output, tables, graphs, and reports. Use layout panes for structure. Use Drei/text-editor panes for text composition and editor behavior.
- Presentation-based interfaces are the default for semantic UI. If visible text or graphics represent a domain object the user can act on, render it as a presentation with an appropriate presentation type, then use presentation translators or commands for operations on it. Do not use raw pointer coordinate hit testing for semantic object selection.
- Commands own user-visible actions. Menus, key bindings, presentation translators, and gadget event adapters should dispatch frame commands or call small UI-independent helpers. Do not hide domain mutation in display functions or gadget callbacks.
- Command tables own command availability, menus, menu bars, inherited command sets, and keystrokes. When a menu depends on frame or buffer state, build or refresh a frame-local command table rather than mutating one global table shared by every frame.
- Gadgets are for conventional controls: buttons, toggles, sliders, text fields, list panes, option panes, and similar form widgets. Gadget callbacks may translate widget activity into commands, but the actual application action should live in a command or helper that can be tested outside the gadget.
- Redisplay should flow through CLIM. Views should reflect application state through display functions. Use `updating-output` with stable unique IDs and cache values for incremental redisplay. For async updates, queue back into the frame or request `redisplay-frame-pane`/`pane-needs-redisplay`; do not take over the repaint loop.
- Drawing functions are acceptable for genuinely graphical views, but keep semantics separate from pixels and wrap interactive drawn objects in presentations.

Do not write custom render logic for McCLIM interfaces. In particular, do not build manual repaint loops, private widget/rendering layers, raw output-record mutation, direct medium/sheet manipulation, coordinate-based hit testing, or `window-clear` plus redraw schemes unless the user explicitly asks for that lower-level work and the code comment or commit message explains why normal CLIM facilities are insufficient.

## Testing Guidelines
Add unit coverage in `tests/*-test.lisp` and register new suites under `rplaca-suite` in [`tests/packages.lisp`](tests/packages.lisp). Prefer deterministic FiveAM tests for buffer, rendering, keymap, and provider helpers. Reserve live-provider checks such as `tests/openrouter-live-test.lisp` for manual runs. McCLIM E2E tests require Xvfb, xdotool, ImageMagick, and provider credentials only for online scenarios.

## Commit & Pull Request Guidelines
Recent history uses imperative subjects and often scopes; the canonical format is in `docs/COMMITS.md`: `type(scope): Subject`. Keep commits atomic, wrap bodies at 72 columns, and explain why the change exists. For pull requests, include a concise behavior summary, linked issue or rationale, test evidence, and screenshots for UI or rendering changes.

## Configuration & Secrets
Load environment with `direnv` via `.envrc` when possible. Never commit API keys, OAuth tokens, or copied config from `~/.config/rplaca/`; use environment variables and local config files instead.

The intentional legacy-name compatibility boundary is documented in
[`docs/MIGRATING-FROM-CLAWMACS.md`](docs/MIGRATING-FROM-CLAWMACS.md) and
enforced by `./scripts/check-legacy-name-allowlist.sh`. Update the exact
allowlist entry and rationale whenever migration compatibility changes.
