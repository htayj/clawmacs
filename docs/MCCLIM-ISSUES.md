# McCLIM issue classification

Audit date: 2026-07-13 through 2026-07-14

## Tested McCLIM provenance

The Guix manifest pins McCLIM tag `1.0.0-koliada`, commit
`2577ea3569bb96e2c46ccc8abed4ce6c327e8ed6`, with a fixed Guix content hash.
Fresh wrapper inspection resolves `clim`, `mcclim`, `mcclim-clx`,
`esa-mcclim`, and `drei-mcclim` from that Guix package's
`share/common-lisp/source/cl-mcclim/` tree. The launcher now sets
`CL_SOURCE_REGISTRY` from the active `GUIX_ENVIRONMENT` for both dependency
warmup and the final payload, so a host registry or stale Quicklisp McCLIM
cannot silently change the implementation under test.

## Fork recommendation

No fork is recommended yet.

All application-controlled crash triggers now have local fixes, and the full
acceptance suite can run without carrying a McCLIM patch. The two confirmed
implementation defects below are documented for a possible focused upstream
fix; neither currently justifies maintaining a fork.

## Confirmed pointer-tracking robustness defect

The exact `Sheet ... is not grafted` backtrace enters pinned McCLIM's
`invoke-tracking-pointer` with a queued pointer event whose event sheet is an
old, disowned submenu button. The implementation enters output buffering for
the tracked sheet and event sheet before event dispatch, gadget activity, or a
grafted-sheet check. Acquiring the old sheet's medium therefore signals before
the existing inactive-gadget guard can run. The same ordering remains in the
[current upstream pointer-tracking implementation](https://raw.githubusercontent.com/McCLIM/McCLIM/master/Core/extended-input/pointer-tracking.lisp)
as of this audit.

Pinned ESA also assigns the applicable frame command table on every command
loop turn, and pinned McCLIM treats an `EQ` assignment as a request to disown
and recreate menu gadgets. Those behaviors make stale queued sheets possible;
Clawmacs' former state-dependent table replacement made the timing window much
larger.

The implicated pinned-source chain is:

- `Libraries/ESA/esa.lisp`: applicable-table assignment in the ESA loop;
- `Core/clim-core/frames/frames.lisp` and
  `Core/clim-core/frames/frame-managers.lisp`: assignment notification;
- `Core/clim-core/gadgets/menu.lisp`: disown/recreate menu contents; and
- `Core/extended-input/pointer-tracking.lisp`: event-sheet buffering before
  dispatch or validation.

Clawmacs now uses one stable named command-table tree, sends state-dependent
choices to presentation-based selectors or dashboards, and ignores only an
identical table assignment for `clawmacs-chat-frame`. A 100-cycle real pointer
stress passes without an ungrafted-sheet condition. A separate bare-McCLIM
matrix completed 59 semantic menu attempts without this condition (one of 60
launches failed earlier with an unrelated pre-window broken pipe), so this is a
confirmed robustness defect at the failing McCLIM locus, not proof that
McCLIM alone creates the triggering stale event in a minimal application.

The focused upstream candidate is to reject or safely dispatch an event for an
ungrafted sheet before acquiring its medium, with a queued-stale-menu-event
regression. Until that minimal regression is accepted or a normal stable-table
application reproduces the crash, the local avoidance is lower risk than a
fork.

## Confirmed pinned implementation limitation

The pinned ESA `convert-to-gesture` method returns a gesture only when a key
event has no modifiers or Shift alone. Control, Meta, Super, and Hyper key
events therefore return `NIL` before Drei can perform command-table lookup.
That behavior is certainly present in the pinned McCLIM/ESA source. Whether a
particular end-user key symptom reaches that method still depends on the
backend event form and Clawmacs' Drei-gadget integration.

Clawmacs keeps one pane-specific adapter that passes those events to Drei's
public `handle-gesture` path. It does not replace ESA or Drei behavior
image-wide. The limitation is covered by modifier-event and real GUI keybinding
tests and is not currently a crash or a reason to carry a fork. If the adapter
becomes insufficient, this method is the first focused upstream patch to
propose.

## Investigated candidates

| Candidate | Current classification | Evidence |
|---|---|---|
| Transient menu sheet reports `Sheet ... is not grafted` during pointer tracking | Confirmed McCLIM failing locus; trigger was amplified by Clawmacs | The backtrace reaches McCLIM's unconditional output buffering for an already-disowned event sheet. Clawmacs removed live table replacement, keeps a stable menu tree, suppresses ESA's identical-table rebuild for its frame, and removed the whole-top-level retry. The bare control did not reproduce the condition. |
| ESA minibuffer/basic-medium startup failures | Not reproduced in minimal pinned ESA control | A minimal ESA frame starts and exits on the pinned McCLIM without Clawmacs. Clawmacs also installed image-wide approximate `basic-medium` metrics and a repaint override; those overrides are removed. The application-owned fixed-height minibuffer retains only a pane-specific `compose-space` method. |
| Edward word kill followed by yank fails on a list/`aref` mismatch | Fixed in the pin | The pinned Edward implementation already normalizes killed word data. The Clawmacs replacements for three private `climi::ie-*` methods are removed, and a regression test exercises the pinned implementation directly. |
| Live compose `M-x` entered an undefined `STREAM` path | Application command-table ordering | Drei's `exclusive-gadget-table` handled `M-x` before Clawmacs' inherited frame table and entered Drei's blocking extended-command workflow. A pane-specific public `additional-command-tables` method now gives the application command precedence. Headless lookup/parser coverage and live GUI passes prove the non-blocking Clawmacs minibuffer path. |
| A rapid `C-c V` became literal `V` in one GUI run | Not proven upstream | The actual pin already filters CLX's documented `:SHIFT-LEFT` and `:SHIFT-RIGHT` standalone modifier names. Clawmacs now consumes recognized standalone modifiers explicitly at its pane boundary, preserves the prefix across ESA's identical command-table assignment, and the GUI driver waits for one toggle's semantic completion before beginning the next chord. Repeated live keybinding coverage is the classification gate; there is no stripped-down McCLIM reproducer. |

## Minimal ESA control

The following control loads `:mcclim-clx` and `:esa-mcclim`, creates a real ESA
application frame with an ESA minibuffer pane, and runs its top level without
loading Clawmacs:

```sh
CLAWMACS_CONTAINER_DISABLE_HOST_X=1 \
./scripts/guix-container.sh --mode e2e -- sh -lc '
set -eu
display_file=$(mktemp)
Xvfb -displayfd 3 -screen 0 1024x768x24 -ac -nolisten tcp \
  3>"$display_file" >/dev/null 2>&1 &
xvfb_pid=$!
trap "kill $xvfb_pid >/dev/null 2>&1 || true; rm -f $display_file" EXIT INT TERM
i=0
while [ ! -s "$display_file" ] && [ "$i" -lt 100 ]; do
  kill -0 "$xvfb_pid"
  i=$((i + 1))
  sleep 0.1
done
test -s "$display_file"
export DISPLAY=":$(tr -d "\\r\\n" < "$display_file")"
i=0
until xdotool getdisplaygeometry >/dev/null 2>&1; do
  kill -0 "$xvfb_pid"
  i=$((i + 1))
  test "$i" -lt 100
  sleep 0.1
done
sbcl --noinform --non-interactive --disable-debugger \
  --load "$CLAWMACS_QUICKLISP_SETUP" \
  --eval "(ql:quickload (quote (:mcclim-clx :esa-mcclim)) :silent t)" \
  --eval "(clim:define-application-frame esa-minibuffer-repro
             (esa:esa-frame-mixin clim:standard-application-frame)
             ()
             (:panes (mini (clim:make-pane (quote esa:minibuffer-pane))))
             (:layouts (default mini))
             (:top-level ((lambda (frame)
                            (declare (ignore frame))
                            :ok))))" \
  --eval "(let ((frame (clim:make-application-frame
                        (quote esa-minibuffer-repro))))
            (format t \"~&made frame state=~S~%\"
                    (clim:frame-state frame))
            (format t \"run result=~S~%\"
                    (clim:run-frame-top-level frame)))" \
  --eval "(quit)"
'
```

Observed result:

```text
made frame state=:DISOWNED
run result=:OK
```

The final run is recorded in
`.artifacts/stability-final-20260714T054821Z/final/minimal-esa-control-final2.log`.

## Fork threshold

Promote a candidate to a McCLIM issue only when all of these are true:

1. A minimal application contains no Clawmacs methods, advice, event classes,
   or command-table refresh code.
2. It fails on the exact pinned Guix McCLIM revision under a fresh Xvfb.
3. The same semantic lifecycle is valid under the CLIM protocol: frames are
   adopted/running, event sheets are real sheets, and UI changes occur in the
   frame process.
4. The failure is repeatable with a recorded command, backtrace, and artifact.
5. Current upstream is checked separately so an already-fixed defect does not
   justify maintaining a fork.

Until an item meets that bar, it does not justify a fork; keep any necessary
mitigation local while the minimal reproducer remains absent.
