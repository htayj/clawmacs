# mcclim-charms

`mcclim-charms` is an isolated experimental McCLIM backend over
`cl-charms`/curses. It lives under `backends/mcclim-charms/` and is not wired
into Clawmacs runtime, `run.sh`, `clawmacs.asd`, or the main Clawmacs test
suite.

The backend registers the McCLIM server path keyword `:charms` and defines the
canonical backend objects: `charms-port`, `charms-graft`,
`charms-frame-manager`, `charms-medium`, `charms-pointer`, and
`charms-mirror`.

## Run Tests

```sh
backends/mcclim-charms/scripts/test-all.sh
```

The script enters the backend-local Guix manifest when `guix` is available,
then runs:

- `mcclim-charms/tests`
- `mcclim-charms/examples-tests`
- a source audit of every file in McCLIM `Examples/clim-examples.asd` that
  extracts frames, commands, keystrokes, menus, presentation types,
  presentation translators/actions, gadgets, accepting-values forms, and
  tracking-pointer forms
- a PTY-style examples load smoke test with `TERM=xterm-256color`
- a PTY interaction probe that drives keyboard, special-key, mouse click,
  mouse drag/hold, pointer motion, resize, timeout, timer, wakeup, and cleanup
  paths
- a PTY chooser render test that launches `run-example.sh --chooser`, captures
  the curses screen, and verifies visible browser content such as
  `McCLIM Examples`, `CLIM-Fig`, and `Calculator`

`MCCLIM_SOURCE_ROOT` defaults to `/home/tay/reference/external_src/McCLIM/`.
Set it to another McCLIM checkout when needed.

## Run Examples

```sh
backends/mcclim-charms/scripts/run-example.sh --list
backends/mcclim-charms/scripts/run-example.sh --chooser
backends/mcclim-charms/scripts/run-example.sh demodemo
backends/mcclim-charms/scripts/run-example.sh calculator
```

The launcher enters the backend-local Guix manifest when `guix` is available,
sets `TERM=xterm-256color` when needed, loads `mcclim-charms`, sets the McCLIM
server path to `(:charms)`, loads `clim-examples`, and starts the selected
example. `--chooser` runs the same example browser entrypoint as:

```lisp
(ql:quickload "clim-examples")
(clim-demo:demodemo)
```

Use `--check NAME` or `--check-chooser` to load and resolve a target without
starting its interactive frame. Loader warnings are written to
`artifacts/launcher-load.log` by default so the terminal can switch cleanly to
the curses interface. Set `MCCLIM_CHARMS_LAUNCHER_VERBOSE=1` to show loader
output in the terminal.

## Scope

This backend owns curses lifecycle and terminal modes:

- `charms:initialize` / `charms:finalize`
- echo disabled
- raw input enabled with interpreted control characters
- extra keys enabled
- non-blocking input configured for the standard window
- low-level `wgetch`, mouse, resize, color, and refresh hooks where needed

Terminal drawing is cell based. CLIM coordinates remain `x/y` internally; the
backend converts to curses `y/x` only at the `cl-charms` boundary.

The examples manifest mirrors every file listed in McCLIM's
`Examples/clim-examples.asd`. There is no unsupported tier in the manifest.
Every manifest entry carries a scripted interaction scenario. Interactive
examples require keyboard, special-key, pointer click, pointer drag, pointer
motion, resize, timer, wakeup, and shutdown coverage. The runners record PTY
screen/status evidence under `backends/mcclim-charms/artifacts/`.

The example-specific audit is source-derived: it reads each example and writes
`artifacts/example-interactions/coverage.json` plus a summary. It fails if an
example source exposes an interaction surface that is not represented by a
result check category.
