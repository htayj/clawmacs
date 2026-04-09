# Repository Guidelines

## Project Structure & Module Organization
`src/` holds the Common Lisp application, loaded serially by [`clawmacs.asd`](/home/tay/projects/clawmacs/clawmacs.asd). Keep new modules in dependency order there: core data structures first, UI backends and `main.lisp` later. `tests/` contains FiveAM suites named `*-test.lisp`; `test-e2e.py` and `scripts/e2e.sh` cover terminal end-to-end flows. `scripts/` contains environment wrappers, `docs/` stores design and process notes, and `docs/img/` holds screenshots and other documentation assets.

## Build, Test, and Development Commands
Use the container wrappers instead of invoking SBCL directly.

- `./run.sh` launches the default Croatoan terminal UI in the Guix container.
- `./scripts/guix-container.sh --preflight-only --mode run -- true` validates the local Guix/Quicklisp setup without starting the app.
- `./scripts/guix-container.sh --mode run -- sh -lc 'sbcl --noinform --load "$CLAWMACS_QUICKLISP_SETUP" --eval "(push (truename \".\") asdf:*central-registry*)" --eval "(ql:quickload :clawmacs/tests)" --eval "(fiveam:run! :clawmacs-suite)" --eval "(quit)"'` runs the unit suite.
- `./scripts/e2e.sh` runs Python-based TUI tests; `./scripts/e2e.sh --only readline` targets one scenario.

## Coding Style & Naming Conventions
Follow the repository’s Common Lisp posture in `docs/DESIGN.md`: functional-first, immutable by default, and explicit about side effects. Use standard Lisp indentation with aligned keyword arguments, lowercase-hyphenated symbol names, and `*earmuffed*` globals for special variables. Export public API from `src/packages.lisp`, and add SBCL type declarations where they clarify intent. No formatter or linter is enforced here, so match nearby code exactly.

## Testing Guidelines
Add unit coverage in `tests/*-test.lisp` and register new suites under `clawmacs-suite` in [`tests/packages.lisp`](/home/tay/projects/clawmacs/tests/packages.lisp). Prefer deterministic FiveAM tests for buffer, rendering, keymap, and provider helpers. Reserve live-provider checks such as `tests/openrouter-live-test.lisp` for manual runs. E2E tests require `mcp-tui-driver` plus a provider key such as `ANTHROPIC_API_KEY` or `OPENAI_API_KEY`.

## Commit & Pull Request Guidelines
Recent history uses imperative subjects and often scopes; the canonical format is in `docs/COMMITS.md`: `type(scope): Subject`. Keep commits atomic, wrap bodies at 72 columns, and explain why the change exists. For pull requests, include a concise behavior summary, linked issue or rationale, test evidence, and screenshots for UI or rendering changes.

## Configuration & Secrets
Load environment with `direnv` via `.envrc` when possible. Never commit API keys, OAuth tokens, or copied config from `~/.config/clawmacs/`; use environment variables and local config files instead.
