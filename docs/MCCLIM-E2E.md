# McCLIM E2E Harness

`scripts/mcclim-e2e.sh` runs the McCLIM application under Xvfb and
drives it with `xdotool`. It captures screenshots with ImageMagick and writes
semantic UI snapshots so coding agents can observe behavior while iterating.

Run the offline suite:

```sh
./scripts/mcclim-e2e.sh --only offline
```

Run the quick smoke checks:

```sh
./scripts/mcclim-e2e.sh --only smoke
```

Artifacts are written to `screenshots/mcclim/`. Each step produces a PNG and a
matching JSON snapshot. The JSON includes current buffer state, input text,
message tail, modeline, who-line, minibuffer state, and selector state.

The harness loads `scripts/mcclim-e2e-driver.lisp` only for tests. Production
startup paths are unchanged.

When the McCLIM main thread signals an error during a test run, the launcher
prints an `MCCLIM-E2E-ERROR` line and an SBCL backtrace into the per-run
`clawmacs.log` under `.cache/mcclim-e2e-*`. Use that log together with the
latest JSON snapshot to distinguish rendering failures from event-loop failures.
