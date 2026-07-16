# McCLIM issue classification

Audit date: 2026-07-13 through 2026-07-16

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

All application-controlled crash triggers have local fixes, and the full
acceptance suite can run without carrying a McCLIM patch. The pointer-tracking
defect below is now certainly McCLIM-owned and meets the threshold for an
upstream issue and patch. It does not yet justify maintaining a fork because
Clawmacs' stable command-table design avoids the trigger and has passed the
bounded acceptance and adversarial runs.

If upstream declines the focused fix, or if Clawmacs again needs live
command-table replacement before upstream accepts it, prefer a small Guix
package patch before creating a permanent McCLIM fork.

## Confirmed McCLIM pointer-tracking robustness defect

The exact `Sheet ... is not grafted` backtrace enters pinned McCLIM's
`invoke-tracking-pointer` with a queued pointer event whose event sheet is an
old, disowned submenu button. The implementation enters output buffering for
the tracked sheet and event sheet before event dispatch, gadget activity, or a
grafted-sheet check. Acquiring the old sheet's medium therefore signals before
the existing inactive-gadget guard can run.

This failure is certainly McCLIM-owned. A plain `standard-application-frame`
following McCLIM issue
[#1312](https://codeberg.org/McCLIM/McCLIM/issues/1312), with no Clawmacs or ESA
code, reproduced the exact ungrafted `MENU-BUTTON-SUBMENU-PANE` condition in
three of three fresh Guix/private-Xvfb runs against the pinned revision. The
commands completed before the crash, and the recorded backtrace enters
`INVOKE-WITH-SHEET-MEDIUM` from `INVOKE-TRACKING-POINTER`.

Official McCLIM master was
[`537f213e6ab817002f115cc1de3763d9bce27e77`](https://codeberg.org/McCLIM/McCLIM/commit/537f213e6ab817002f115cc1de3763d9bce27e77)
on 2026-07-16, 18 commits beyond the pin. The relevant runtime behavior is
unchanged: the only pointer-tracking difference is a macro-local variable
rename, and the menu, frame-manager, output, and ESA paths have no pertinent
post-pin change. The unsafe ordering remains in the
[current official pointer-tracking source](https://codeberg.org/McCLIM/McCLIM/raw/commit/537f213e6ab817002f115cc1de3763d9bce27e77/Core/extended-input/pointer-tracking.lisp).
The former GitHub mirror is stale and is not evidence of current upstream
behavior.

Portable CLIM permits changing a frame's command table and requires command
menus to be updated as needed. The specification does not define the exact
policy for an event whose sheet is disowned after the event is queued. Dynamic
table replacement is therefore not an application protocol violation, while
the stale-event production and pre-validation medium acquisition are
McCLIM-specific robustness defects rather than a demonstrated strict CLIM
conformance violation.

Pinned ESA also assigns the applicable frame command table on every command
loop turn, and pinned McCLIM treats an `EQ` assignment as a request to disown
and recreate menu gadgets. McCLIM deliberately preserves that same-object
refresh to update dynamically enabled or disabled menu commands, as documented
by [issue #1145](https://codeberg.org/McCLIM/McCLIM/issues/1145). Clawmacs'
former state-dependent table replacement made the stale-event timing window
much larger, but neither Clawmacs nor ESA is required for the failure.

The original Clawmacs/ESA-amplified trigger chain is:

- `Libraries/ESA/esa.lisp`: applicable-table assignment in the ESA loop;
- `Core/clim-core/frames/frames.lisp` and
  `Core/clim-core/frames/frame-managers.lisp`: assignment notification;
- `Core/clim-core/gadgets/menu.lisp`: disown/recreate menu contents; and
- `Core/extended-input/pointer-tracking.lisp`: event-sheet buffering before
  dispatch or validation.

The bare McCLIM lifecycle path additionally crosses
`Core/windowing/ports.lisp`, which retains pointer and pressed-sheet state and
synthesizes or copies events; `Core/windowing/input.lisp`, which retains the
event in the shared queue; and `Core/windowing/output.lisp`, which tries to
acquire the old sheet's medium.

There are two complementary McCLIM failure mechanisms:

1. Disowning or degrafting a menu sheet does not clear a port pointer's
   `pointer-sheet` or `port-pressed-sheet`. A later boundary calculation can
   synthesize an exit for the old sheet, and motion or release distribution can
   copy an event to the stale pressed sheet. A deterministic pinned-core probe
   produced old ungrafted exit and motion events after disowning the sheet.
2. Events already in the shared frame queue retain their original event sheet.
   `invoke-tracking-pointer` reads that event and acquires the event sheet's
   medium before it checks where or whether the event should be handled.

The first mechanism should be fixed at the producer, but producer cleanup
cannot protect events that were already queued or race with sheet teardown.
Pointer tracking must classify a stale event sheet before output buffering,
coordinate transformation, medium acquisition, or an application callback. A
safe policy may route the event without touching the stale sheet, discard it,
or cancel the affected interaction; release-event semantics and pointer-grab
cleanup must be explicit whichever policy is selected.

McCLIM previously addressed the stale-gadget class in
[`78eb7653`](https://codeberg.org/McCLIM/McCLIM/commit/78eb7653f091a50d3771e56387ff33c108a15fc7)
by making ungrafted gadgets inactive. Later commits
[`25df1d89`](https://codeberg.org/McCLIM/McCLIM/commit/25df1d89df7df065dc09c73df385fba1fb8c9d51)
and
[`1e50289e`](https://codeberg.org/McCLIM/McCLIM/commit/1e50289e48a948d46776f69a15bfe0a993bb5754)
moved medium acquisition and the explicit ungrafted-sheet error ahead of that
guard. The current crash is therefore a regression of the failure class fixed
for #1312, although it now fails earlier.

Clawmacs now uses one stable named command-table tree, sends state-dependent
choices to presentation-based selectors or dashboards, and ignores only an
identical table assignment for `clawmacs-chat-frame`. The repository currently
does not use CLIM's dynamic command enable/disable APIs, so this scoped
suppression does not hide a required status refresh. If that changes, Clawmacs
must add an explicit refresh path or remove the suppression.

The focused upstream repair has two parts:

1. Validate the tracked and event sheets before output buffering. If either is
   ungrafted, perform no medium acquisition or unsafe callback on it. Evaluate
   safe routing, explicit discard, and clean cancellation as policies; blindly
   discarding a stale release risks leaving tracking blocked.
2. Clear or recompute pointer and pressed-sheet references when their subtree
   is disowned or degrafted, under the appropriate port synchronization.

Regression coverage should queue an event for a real menu gadget, disown it,
and prove that pointer tracking performs no medium acquisition or callback on
the old gadget. It should cover motion, press, release, pointer-only and
pressed-sheet state, `:multiple-window t`, transformed coordinates, pointer
grab cleanup, an `EQ` table refresh, and a genuinely different table
assignment. The official #1312 interaction should then pass repeatedly.

Do not make `with-sheet-medium` silently tolerate ungrafted sheets, globally
turn `EQ` command-table assignments into no-ops, clear the entire input queue,
or add application-level string-matched recovery. Those alternatives either
hide legitimate lifecycle errors, break command-status refresh, discard valid
input, or move McCLIM event handling into Clawmacs.

As of 2026-07-16, an official Codeberg issue and pull-request scan found no
current report or patch for the pre-buffer lifecycle check. A new upstream
report should link #1312 and its later regression history.

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
| Transient menu sheet reports `Sheet ... is not grafted` during pointer tracking | Confirmed McCLIM-owned robustness defect; trigger was amplified by Clawmacs | The official #1312 lifecycle reproduced the exact current condition in 3/3 plain-CLIM Guix/private-Xvfb runs. McCLIM buffers an already-disowned event sheet before lifecycle validation. Clawmacs removed live table replacement, keeps a stable menu tree, suppresses ESA's identical-table rebuild for its frame, and removed the whole-top-level retry. |
| ESA minibuffer/basic-medium startup failures | Not reproduced in minimal pinned ESA control | A minimal ESA frame starts and exits on the pinned McCLIM without Clawmacs. Clawmacs also installed image-wide approximate `basic-medium` metrics and a repaint override; those overrides are removed. The application-owned fixed-height minibuffer retains only a pane-specific `compose-space` method. |
| Edward word kill followed by yank fails on a list/`aref` mismatch | Fixed in the pin | The pinned Edward implementation already normalizes killed word data. The Clawmacs replacements for three private `climi::ie-*` methods are removed, and a regression test exercises the pinned implementation directly. |
| Live compose `M-x` entered an undefined `STREAM` path | Application command-table ordering | Drei's `exclusive-gadget-table` handled `M-x` before Clawmacs' inherited frame table and entered Drei's blocking extended-command workflow. A pane-specific public `additional-command-tables` method now gives the application command precedence. Headless lookup/parser coverage and live GUI passes prove the non-blocking Clawmacs minibuffer path. |
| A rapid `C-c V` became literal `V` in one GUI run | Not proven upstream | The actual pin already filters CLX's documented `:SHIFT-LEFT` and `:SHIFT-RIGHT` standalone modifier names. Clawmacs now consumes recognized standalone modifiers explicitly at its pane boundary, preserves the prefix across ESA's identical command-table assignment, and the GUI driver waits for one toggle's semantic completion before beginning the next chord. Repeated live keybinding coverage is the classification gate; there is no stripped-down McCLIM reproducer. |

## Post-adversarial classification

The extended run completed 250 menu operations, 62 semantic selector
activations, 30 expose/unmap-map operations, and three resizes without a crash.
After its one-time setup transition, the bounded resource profile showed no
accumulating RSS, descriptor, or thread trend during the sampled interval. The
Artifactum timestamp crash, screenshot-ordering race, compile-cache and
event-log costs, interop test flake, and inherited X socket namespace were
owned by Clawmacs, its tests, or its harness.

Fresh provenance again resolves the tested CLIM, McCLIM, ESA, and Drei systems
from the pinned Guix McCLIM tree. The Clawmacs-free ESA control again starts and
returns `:OK`; current evidence is under
`.artifacts/adversarial-mcclim/final-closure/provenance/`. Those bounded results
remain valid evidence that the locally mitigated Clawmacs interface is stable
under the tested workload.

The later focused investigation supersedes only the pointer issue's ownership
classification: the plain-CLIM #1312 reproduction proves that failure is
certainly McCLIM-owned. The pinned ESA modifier limitation and all other
upstream-versus-local classifications remain unchanged.

## Plain CLIM pointer-tracking reproduction

The focused reproducer loads only `:mcclim-clx`, defines a
`standard-application-frame`, and follows the public CLIM command-table and menu
protocol from issue #1312:

1. Select `File` -> `Expand menus`; this updates the frame command table.
2. Select `File` -> `Run`; this command sleeps for five seconds.
3. Click `Edit` once while `Run` is sleeping.
4. After `Run` returns, McCLIM starts menu pointer tracking and signals that the
   queued event's old `MENU-BUTTON-SUBMENU-PANE` is not grafted.

All three counted runs used
`CLAWMACS_CONTAINER_DISABLE_HOST_X=1`, the Guix E2E wrapper, and a newly started
private Xvfb selected through `-displayfd`. The harness waited until the named
application window reached 900x548 before sending real `xdotool` gestures. It
recorded every gesture, command marker, store provenance, exit status, and full
backtrace. Runs 5, 6, and 7 each completed `Expand menus` and `Run`, then exited
with status 1 at the exact pointer-tracking condition.

Evidence is retained under:

- `.artifacts/pointer-tracking-investigation/root/issue-1312-run-5/`;
- `.artifacts/pointer-tracking-investigation/root/issue-1312-run-6/`; and
- `.artifacts/pointer-tracking-investigation/root/issue-1312-run-7/`.

An older diagnostic run is not counted as reproduction evidence. It attached
to an already-running X display, targeted McCLIM's initial 100x100 off-screen
window instead of waiting for the 900x548 application frame, retained no input
trace, and captured black screenshots. The clean harness independently
observed that same transient geometry before waiting for the valid frame.

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

## Upstream attribution and fork thresholds

Attribute a candidate certainly to McCLIM only when all of these are true:

1. A minimal application contains no Clawmacs methods, advice, event classes,
   or command-table refresh code.
2. It fails on the exact pinned Guix McCLIM revision under a fresh Xvfb.
3. The same semantic lifecycle is valid under the CLIM protocol: frames are
   adopted/running, event sheets are real sheets, and UI changes occur in the
   frame process.
4. The failure is repeatable with a recorded command, backtrace, and artifact.
5. Current upstream is checked separately so an already-fixed defect does not
   justify maintaining a fork.

The pointer-tracking defect now satisfies all five conditions. This promotes it
to a confirmed upstream issue and patch candidate; it does not automatically
require a fork.

Maintain a McCLIM fork only if all of these additional conditions become true:

1. Clawmacs needs the failing lifecycle and cannot safely avoid it locally.
2. A focused McCLIM patch fixes the plain reproducer and passes Clawmacs'
   acceptance and adversarial suites.
3. Upstream declines the patch or leaves it unavailable long enough to block
   Clawmacs development.
4. Carrying the patch in the Guix package is no longer sufficient or
   maintainable.
