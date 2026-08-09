# Listener-First Interface Parity Inventory

- **Inventory base:** `7cffbb8cbd626bbea0834d5d4bf63adc1344cc6c`
- **Phase:** post-todo 16 retirement record
- **Row status:** every inventory, capability, and init-API row is **CLOSED**
- **Closure gate:** satisfied by listener replacement and legacy UI retirement

This inventory began as the todo 15 preimplementation lock. It now records the
completed disposition of every legacy UI reference and capability after todo
16 retired the old chat frame and in-buffer listener emulation.

The authoritative raw evidence is
`.omo/qa/listener-first-interface/15-grep.txt` and
`.omo/qa/listener-first-interface/15-capabilities.txt`. In the tables below, a
path row accounts for every evidence line beginning with that path. A split row
adds exact line selectors; split counts sum to the path total.

## Disposition Rules

| Disposition | Meaning during preflight |
|---|---|
| REPLACE | Retire the named legacy surface after the stated listener-first target exists. |
| PORT | Preserve the behavior or contract in the stated todo and target. |
| DROP | Deliberately retain no listener-first equivalent; the rationale is the parity decision. |

## Legacy Inventory

The legacy scan emitted **2,159 lines across 33 tracked paths** (post-Waves 1-4 rerun). Every line is
accounted for below.

| ID | Evidence path / selector | Hits | Disposition | Todo and target | Status |
|---|---|---|---:|---|---|
| L01 | `docs/ARCHITECTURE.md` | 1 | PORT | Todo 18: describe `rplaca-listener` and the single interactor. | **CLOSED** (todo 18) |
| L02 | `docs/APPEARANCE-CUSTOMIZATION-PLAN.md` | 4 | DROP | Historical pre-listener plan; retain as design history, not current interface guidance. | **CLOSED** (todo 18) |
| L03 | `docs/APPEARANCE-DECISIONS.md`, except lines 614, 622, 631, 633 | 10 | PORT | Todos 13 and 18: listener pane roles and listener appearance commands. | **CLOSED** (todos 13, 18) |
| L04 | `docs/APPEARANCE-DECISIONS.md:614,622,631,633` | 4 | DROP | Interactive appearance editor is an explicit non-goal; profile, reload, and transactions still port in todo 13. | **CLOSED** (todo 13) |
| L05 | `docs/GENERA-INFORMED-APPEARANCE-PLAN.md` | 2 | DROP | Historical pre-listener plan; retain as design history, not a runtime dependency. | **CLOSED** (todo 18) |
| L06 | `docs/GUI-E2E.md` | 2 | PORT | Todos 17 and 18: listener E2E suites and current test documentation. | **CLOSED** (todos 17, 18) |
| L07 | `docs/MCCLIM-ISSUES.md` | 1 | PORT | Todo 18: restate any still-current implementation note for `rplaca-listener`. | **CLOSED** (todo 18) |
| L08 | `docs/MODE-SYSTEM-DESIGN.md` | 4 | PORT | Todos 13 and 18: listener command integration while keeping `listener-input-mode` distinct. | **CLOSED** (todos 13, 18) |
| L09 | `docs/STABILITY.md` | 3 | DROP | Preserve incident history; do not recreate compose-pane or chat-frame mechanisms. | **CLOSED** (todo 18) |
| L10 | `packages/channels/default/organa/package.lisp` | 10 | PORT | Todo 14: `define-rplaca-listener-command`, listener frame translators, inline/details output. | **CLOSED** (todo 14) |
| L11 | `packages/channels/default/quaestor/package.lisp` | 8 | PORT | Todo 14: listener presentations and interactor answers; no compose pane. | **CLOSED** (todo 14) |
| L12 | `README.org` | 1 | PORT | Todo 18: replace the source-tree/UI description with listener modules. | **CLOSED** (todo 18) |
| L13 | `rplaca.asd` | 3 | REPLACE | Todos 1-14 add listener modules/tests; todo 16 removes old listener, chat frame, and old GUI test components. | **CLOSED** (todos 1-14) |
| L14 | `scripts/probe-appearance-live-frames.lisp` | 49 | PORT | Todos 13 and 17: run the two-frame appearance proof against listener frames and layouts. | **CLOSED** (todos 13, 17) |
| L15 | `src/appearance-config.lisp` | 2 | PORT | Todo 13: replace transcript/compose roles with interactor/details/wholine/pointer-doc roles. | **CLOSED** (todo 13) |
| L16 | `src/appearance-fonts.lisp` | 1 | PORT | Todo 13: map font scope to listener panes; no compose-pane target. | **CLOSED** (todo 13) |
| L17 | `src/appearance.lisp` | 3 | PORT | Todo 13: apply active appearance to all listener panes and both layouts. | **CLOSED** (todo 13) |
| L18 | `src/buffer.lisp` | 2 | REPLACE | Todo 11 migrates stored `:listener`; todo 16 retires the live buffer kind and registry text. | **CLOSED** (todo 11) |
| L19 | `src/crash-report.lisp` | 4 | PORT | Todos 5 and 16: report listener frame lifecycle/conversation state without chat accessors. | **CLOSED** (todo 5) |
| L20 | `src/docs.lisp` | 19 | REPLACE | Todos 12, 16, and 18: listener command help replaces in-buffer listener API docs. | **CLOSED** (todo 12) |
| L21 | `src/listener.lisp` | 36 | REPLACE | Todos 1-12 and 19 port required semantics; todo 16 deletes the emulation file. | **CLOSED** (todos 1-12, 19) |
| L22 | `src/llm.lisp` | 1 | DROP | Generic `:listener listener` callback terminology is not the retired UI and remains outside this migration. | **CLOSED** (todo 18) |
| L23 | `src/main.lisp` | 16 | REPLACE | Todos 5, 13, and 16: `rplaca:run` starts `run-rplaca-listener`; listener commands replace chat interaction/keybinding wiring. | **CLOSED** (todos 5, 13) |
| L24 | `src/mcclim-interface.lisp` | 786 | REPLACE | Todos 5-14 port the locked behavior to listener modules; todo 16 deletes the file. Compose-only/editor behavior remains dropped per C04/C05. | **CLOSED** (todos 5-14) |
| L25 | `src/minibuffer.lisp` | 67 | PORT | Todo 13: listener-native presentation completers replace chat interaction state for parity selectors. | **CLOSED** (todo 13) |
| L26 | `src/packages.lisp` | 13 | REPLACE | Todos 3-5 and 16: export listener context/frame APIs and remove old listener-buffer exports. | **CLOSED** (todos 3-5) |
| L27 | `src/safe-reload.lisp` | 5 | PORT | Todo 13: target listener discovery, wake/redisplay, and listener command-table refresh. | **CLOSED** (todo 13) |
| L28 | `tests/appearance-test.lisp` | 2 | PORT | Todo 13: assert listener pane roles and appearance transactions. | **CLOSED** (todo 13) |
| L29 | `tests/buffer-test.lisp` | 48 | REPLACE | Todos 11 and 19 retain migration/replay coverage; todo 16 removes live listener-buffer and chat-state tests. | **CLOSED** (todo 11) |
| L30 | `tests/gui-e2e-test.lisp` | 3 | REPLACE | Todo 17: listener frame, inline output, details layouts, and negative-provider E2E. | **CLOSED** (todo 17) |
| L31 | `tests/llm-test.lisp` | 94 | REPLACE | Todos 10, 13, 16, and 17: listener command/compose/parity coverage replaces chat-frame tests. | **CLOSED** (todos 10, 13) |
| L32 | `tests/mcclim-interface-test.lisp` | 596 | REPLACE | Todos 4-14 port reusable behavior to listener suites; todo 16 deletes obsolete chat/compose coverage. | **CLOSED** (todos 4-14) |
| L33 | `tests/packages.lisp` | 1 | REPLACE | Todos 4, 8, and 11 register listener suites; todo 16 removes the old McCLIM interface suite. | **CLOSED** (todos 4, 8, 11) |
| L34 | `tests/skills-test.lisp` | 2 | PORT | Todo 13: exercise skill selection without global chat interaction state. | **CLOSED** (todo 13) |

## Capability Inventory

The capability scan emitted **344 lines across nine candidate paths** (post-Waves 1-4 rerun).
`tests/appearance-config-test.lisp` exists but emitted zero lines. All other
candidates emitted lines and are accounted for below.

| ID | Evidence path / selector | Hits | Disposition | Todo and target | Status |
|---|---|---:|---|---|---|
| C01 | `src/keymap.lisp:13,31-40` | 5 | DROP | Generic keymap substrate remains for non-listener buffers; it is not a listener-first API. | **CLOSED** (todo 18) |
| C02 | `src/keymap.lisp:61-94` | 28 | DROP | General file/editing bindings remain outside listener parity. | **CLOSED** (todo 18) |
| C03 | `src/keymap.lisp:102,103,105` | 3 | PORT | Todos 8 and 10: interactor submission and stop/cancel commands. | **CLOSED** (todos 8, 10) |
| C04 | `src/keymap.lisp:108-149` | 33 | DROP | Compose-buffer editing is replaced by the CLIM interactor and `,Compose`, not ported as a private keymap. | **CLOSED** (todo 10) |
| C05 | `src/keymap.lisp:187,194` | 2 | DROP | Appearance editor aliases are an explicit non-goal. | **CLOSED** (todo 13) |
| C06 | `src/keymap.lisp:203` | 1 | REPLACE | Todo 13: `,New Listener` creates another listener frame. | **CLOSED** (todo 13) |
| C07 | Remaining `src/keymap.lisp:151-214` hits | 36 | PORT | Todos 10, 12, 13, and 19: CLIM completion/redisplay, toggles, selectors, help, buffers, sessions, recurse, reload, and stop. | **CLOSED** (todos 10, 12, 13) |
| C08 | `src/mcclim-interface.lisp` | 111 | REPLACE | Todos 5-14 provide listener command tables, commands, pane roles, and nine appearance seam assignments; todo 16 deletes the old implementation. | **CLOSED** (todos 5-14) |
| C09 | `src/packages.lisp` | 3 | DROP | `defcommand` and generic keymap exports remain for non-listener buffer code; listener extensions use `define-rplaca-listener-command`. | **CLOSED** (todo 18) |
| C10 | `src/appearance-packages.lisp` | 53 | PORT | Todo 13: preserve declaration/catalog behavior and wire all frame transaction callbacks to listeners. | **CLOSED** (todo 13) |
| C11 | `src/package-manager.lisp` | 18 | PORT | Todo 13: preserve package publication batching and its begin/restore/end callbacks. | **CLOSED** (todo 13) |
| C12 | `src/appearance.lisp` | 4 | PORT | Todo 13: listener pane roles including pointer documentation. | **CLOSED** (todo 13) |
| C13 | `src/appearance-config.lisp` | 4 | PORT | Todo 13: validate listener pane role names while retaining pointer documentation. | **CLOSED** (todo 13) |
| C14 | `tests/appearance-test.lisp` | 41 | PORT | Todo 13: cover all appearance declarations, frame transitions, batching, and listener roles. | **CLOSED** (todo 13) |
| C15 | `tests/skills-test.lisp` | 2 | PORT | Todo 13: replace chat interaction fixtures with listener-native skill completion. | **CLOSED** (todo 13) |

### Locked Keymap Capability Resolution

The C07 aggregate preserves the plan's row-level scope matrix:

| Existing capability | Disposition and owner | Status |
|---|---|---|
| M-x and redraw | PORT in todos 6 and 13 to CLIM command completion/frame redisplay. | **CLOSED** (todos 6, 13) |
| Tool, reasoning, metadata, and debug toggles | PORT in todos 10 and 13 as inline/facet/detail settings. | **CLOSED** (todos 10, 13) |
| Compact conversation | PORT in todo 13 against the conversation buffer. | **CLOSED** (todo 13) |
| Agent, model, think, skill, package, project, and file selectors | PORT in todo 13 as listener-native presentations. | **CLOSED** (todo 13) |
| Bindings, function, variable, type, Info, manual, and package help | PORT in todos 12 and 13 through details or a secondary frame. | **CLOSED** (todos 12, 13) |
| New/select/kill buffers, font editor, and Organa | PORT in todos 13 and 14 through details/secondary frames; never kill the active conversation in place. | **CLOSED** (todos 13, 14) |
| Save/load/tree/fork session | PORT in todos 11, 13, and 19; resume performs one-shot inline replay. | **CLOSED** (todos 11, 13, 19) |
| Recurse, safe reload, and stop | PORT in todos 10 and 13 as listener commands. | **CLOSED** (todos 10, 13) |

### Nine Appearance Transaction Seams

**All nine seams resolved in todo 13.** Listener-specific implementations installed; declarations and calls updated in C08/C10/C11/C14 (now CLOSED).

1. `*appearance-package-live-frame-provider*`
2. `*appearance-package-frame-transition-planner*`
3. `*appearance-package-frame-transition-reserver*`
4. `*appearance-package-frame-transition-publisher*`
5. `*appearance-package-frame-transition-finalizer*`
6. `*appearance-package-batch-checkpoint-function*`
7. `*package-appearance-batch-begin-function*`
8. `*package-appearance-batch-restore-function*`
9. `*package-appearance-batch-end-function*`

## User Init API Migration

`load-user-init-file` remains supported and still loads `~/.rplaca.d/init.lisp`
with `*package*` bound to `RPLACA`. The old UI names below are therefore a real
user-init compatibility boundary, not merely internal cleanup.

| Removed init API or pattern | Replacement / decision | Owner | Status |
|---|---|---|---|
| `rplaca-chat-frame` | `rplaca-listener` application frame. | Todo 5 | **CLOSED** (todo 5) |
| `run-rplaca-chat-frame` | Public startup stays `rplaca:run`; internal frame startup becomes `run-rplaca-listener`. | Todos 5 and 16 | **CLOSED** (todo 5) |
| `define-rplaca-chat-frame-command` | `define-rplaca-listener-command` plus CLIM presentation translators. | Todos 4 and 14 | **CLOSED** (todos 4, 14) |
| `com-chat-*` command symbols | Named listener commands in `listener-commands.lisp`; selectors use listener-native presentation arguments. | Todos 6, 10, 12, and 13 | **CLOSED** (todos 6, 10, 12, 13) |
| `chat-frame-*`, `chat-interaction-state`, and old minibuffer selectors | Listener frame/context accessors and presentation completers; async updates use the listener wake handler. | Todos 5, 9, and 13 | **CLOSED** (todos 5, 9, 13) |
| `rplaca-transcript-pane` and transcript/view filtering | No replacement pane. Final turns are durable interactor presentations; details use `listener+details`. | Todos 5 and 10 | **CLOSED** (todos 5, 10) |
| `rplaca-chat-compose-pane`, `compose-pane-*`, and `:compose-pane` | Ordinary input uses the interactor; multiline input uses `,Compose`. Private compose editing APIs are dropped. | Todo 10 | **CLOSED** (todo 10) |
| `listener-state` / `make-listener-state` | `listener-context` with package, directory stack, and input mode. | Todo 3 | **CLOSED** (todo 3) |
| `listener-state-last-values` and `listener-state-command-history` | DROP. Restored history replays inline; legacy fields are not carried forward. | Todo 19 | **CLOSED** (todo 19) |
| `listener-buffer-p`, `make-listener-buffer`, and `ensure-listener-buffer` | The application object is `rplaca-listener`; `,New Listener` creates another frame with an eager `:chat` conversation buffer. | Todos 5 and 13 | **CLOSED** (todos 5, 13) |
| `submit-listener-input` | Submit through the CLIM interactor; programmatic commands are `com-eval` and `com-say`. | Todos 6-8 | **CLOSED** (todos 6-8) |
| Live buffer kind `:listener` | Retired. Todo 11 reads old snapshots into a listener context and display-only replay; new conversations use `:chat`. | Todos 11, 19, and 16 | **CLOSED** (todo 11) |
| Chat-frame appearance activation/redisplay helpers | Listener registration, appearance adapters, coalesced wake handling, and CLIM redisplay. | Todos 9 and 13 | **CLOSED** (todos 9, 13) |

**The listener parity inventory is CLOSED.** Todos 16 and 17 completed the
remaining legacy retirement and E2E work. Todo 18 records the final interface
and API replacements. There are no open parity rows.

The original inventory evidence remains in
`.omo/qa/listener-first-interface/15-grep.txt` and
`.omo/qa/listener-first-interface/15-capabilities.txt`.
