# McCLIM issue classification

Audit date: 2026-07-13 through 2026-07-26

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
and Drei long-line redisplay defects below are now certainly McCLIM-owned and
meet the threshold for upstream issues and patches. They do not yet justify
maintaining a fork: RPLACA avoids the pointer trigger, and the normal
multiline compose path passes the bounded acceptance and adversarial runs.

If upstream declines either focused fix, if RPLACA again needs live
command-table replacement, or if arbitrary single-line compose input must be
made robust immediately, prefer a small Guix package patch before creating a
permanent McCLIM fork.

## Switch-buffer investigation: application-owned fixes

The two reported July 19 switch-buffer failures do not add a McCLIM fork
candidate.

The apparently hung selector was an application focus-ownership error. The
visible minibuffer is a CLIM application pane that renders completion
presentations, while RPLACA's non-blocking modal key adapter lives on the Drei
compose pane. A command invoked while the transcript or standard-input stream
had focus activated the selector without returning focus to compose. RPLACA
now uses `stream-set-input-focus` at its frame-command boundary whenever an
application interaction becomes active.

The black strip that covered the final candidates is the frame's standard
pointer-documentation pane. The minibuffer had requested too little layout
space; the pointer-documentation pane itself behaved correctly. RPLACA now
derives complete-row height from live CLIM stream metrics and asks CLIM to
resize the frame through `change-space-requirements`, preserving the declared
pane layout instead of drawing around or over another pane.

The related draft, point, mark, dirty-state, default-selection, modal-key
performance, and undo defects were also in RPLACA's integration. Buffer
changes now use the public ESA current-buffer setter as one transactional
contract. `C-w` runs a native Drei command from the pane-owned command table.
Because pinned ESA consumes `C-u` as a universal argument before command-table
lookup, the narrow compose adapter invokes Drei's exported command executor
for its Emacs-style kill operation and retains normal Drei undo.

Private-Xvfb focus, layout, burst-input, cancellation, confirmation, and editor
state regressions cover these paths. No custom repaint loop, output-record
mutation, direct medium/sheet manipulation, or semantic coordinate hit testing
was introduced. The two confirmed McCLIM defects below remain real and are not
the cause of the reported selector hang or candidate overlap.

## Confirmed McCLIM Drei long-line redisplay defect

This defect is certainly McCLIM-owned. A private-Xvfb control loads only
`mcclim-clx` and `drei-mcclim`, verifies that no `RPLACA` package exists, and
creates a standard McCLIM application frame with an ordinary `:drei` pane and
its default horizontal scroller. It crashes without any RPLACA class,
package, or compose integration.

The 32 KiB single-physical-line draft used in the original adversarial run was
only a stress fixture, not a requirement for the failure. The focused control
now reproduces it with a 112-character single physical line. Drei splits that
line into strokes `0..100` and `100..112`; during initial or reentrant layout,
`displayed-lines-count` becomes 2 while the backing `displayed-lines` vector
still has length 1. `CLIM:BOUNDING-RECTANGLE*` then evaluates `ELT` at index 1,
signalling `SB-KERNEL:INDEX-TOO-LARGE-ERROR`. The backtrace crosses
`DREI::DRAW-STROKE`, `change-space-requirements`, recursive repaint, and
`DREI::CLEAR-STALE-LINES`.

In RPLACA, entering `M-x` after composing a single physical line is safe at
100 characters and crashes at both 101 and 112 characters. That is an
application-observed transition threshold, not a claim that every Drei usage
with 101 characters must fail. Bare Drei controls with the same total text,
point, and mark passed in the earlier controls, as did the tested multiline and
fixed-rack initial/post-enable cases. The standard-pane control is the
independent reproducer; the scroller is not a safe RPLACA workaround.

The pinned `1.0.0-koliada` revision
[`2577ea3569bb96e2c46ccc8abed4ce6c327e8ed6`](https://codeberg.org/McCLIM/McCLIM/commit/2577ea3569bb96e2c46ccc8abed4ce6c327e8ed6)
and then-current official master
[`92b8f12b880cd752bdf817fe1a36258a03d96aff`](https://codeberg.org/McCLIM/McCLIM/commit/92b8f12b880cd752bdf817fe1a36258a03d96aff)
have identical relevant
[`drei-redisplay.lisp` source at the pin](https://codeberg.org/McCLIM/McCLIM/raw/commit/2577ea3569bb96e2c46ccc8abed4ce6c327e8ed6/Libraries/Drei/drei-redisplay.lisp)
and [source at then-current master](https://codeberg.org/McCLIM/McCLIM/raw/commit/92b8f12b880cd752bdf817fe1a36258a03d96aff/Libraries/Drei/drei-redisplay.lisp);
the latter is not evidence of a fix or of general master stability.
The related limited
[`326a97bbc623c261aab4248598cb40ef4fe2bef1`](https://codeberg.org/McCLIM/McCLIM/commit/326a97bbc623c261aab4248598cb40ef4fe2bef1)
repair does not cover this ordinary Drei-pane path. The source still documents
the related `0, 0, 1, 2` recursive-count case for Drei output-record replay,
but that narrow repair is not a complete renderer fix.

RPLACA mitigates its specific trigger in local commit `866003f` by declaring
the compose pane's fixed preferred, minimum, and maximum height
and wrapping at construction, then removing the repaint-time compose
`change-space-requirements` request. The exact compose-geometry regression
passes 100-, 101-, and 112-character single-line inputs twice each. This
removes the known RPLACA `M-x` resize transition; it does not change Drei's
displayed-line invariant or establish safety during a genuine resize or another
McCLIM-triggered relayout.

The fork and Guix-package-patch decision remains deferred: this record
establishes a minimal upstream reproducer and an application mitigation, not a
complete upstream repair or a live upstream issue response.

## Confirmed McCLIM pointer-tracking robustness defect

The exact `Sheet ... is not grafted` backtrace enters pinned McCLIM's
`invoke-tracking-pointer` with a queued pointer event whose event sheet is an
old, disowned submenu button. The implementation enters output buffering for
the tracked sheet and event sheet before event dispatch, gadget activity, or a
grafted-sheet check. Acquiring the old sheet's medium therefore signals before
the existing inactive-gadget guard can run.

This failure is certainly McCLIM-owned. A plain `standard-application-frame`
following McCLIM issue
[#1312](https://codeberg.org/McCLIM/McCLIM/issues/1312), with no RPLACA or ESA
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
by [issue #1145](https://codeberg.org/McCLIM/McCLIM/issues/1145). RPLACA's
former state-dependent table replacement made the stale-event timing window
much larger, but neither RPLACA nor ESA is required for the failure.

The original RPLACA/ESA-amplified trigger chain is:

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

RPLACA keeps one stable named hierarchical command-table tree for M-x and
keys, sends state-dependent choices to presentation-based selectors or
dashboards, and ignores only an identical table assignment for
`rplaca-chat-frame`. The repository currently does not use CLIM's dynamic
command enable/disable APIs, so this scoped suppression does not hide a required
status refresh. If that changes, RPLACA must add an explicit refresh path or
remove the suppression.

That stable hierarchy was not sufficient for visible pointer menus. On
2026-07-29, both RPLACA and a plain two-menu `standard-application-frame`
failed under the same bounded gesture: open one nested menu, then perform 40
no-delay crossings through its first item, the adjacent heading, the adjacent
first item, and the original heading. Both fresh private-Xvfb runs terminated
with `MENU-UNMANAGED-TOP-LEVEL-SHEET-PANE ... is not grafted` from
`INVOKE-WITH-SHEET-MEDIUM` through `INVOKE-TRACKING-POINTER`; the plain frame
contains no RPLACA or ESA code. Run the local ownership probe with:

```sh
./scripts/probe-mcclim-menu-boundaries.sh
```

The wrapper uses artifact-private Guix/Xvfb process groups, a fixed 40-crossing
budget, bounded startup and exit waits, TERM/grace/KILL cleanup, and explicit
empty-group result fields. Its expected result against the pin is
`reproduced=true`.

The application workaround does not catch lifecycle errors or replace McCLIM
methods. The frame retains its full hierarchical command table, but its
`:menu-bar` names a separate public-CLIM command table containing direct command
leaves only. Stop, buffer/model/effort/skill selectors, the package dashboard,
appearance editor, and manual therefore remain one-click accessible without
creating transient submenu frames. The tradeoff is that the visible bar is a
flat high-value surface; the complete grouped hierarchy remains discoverable
through M-x and key bindings.

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
input, or move McCLIM event handling into RPLACA.

As of 2026-07-16, an official Codeberg issue and pull-request scan found no
current report or patch for the pre-buffer lifecycle check. Upstream contact and
changes are outside this repository task; this record and probe remain local.

## Confirmed pinned implementation limitation

The pinned ESA `convert-to-gesture` method returns a gesture only when a key
event has no modifiers or Shift alone. Control, Meta, Super, and Hyper key
events therefore return `NIL` before Drei can perform command-table lookup.
That behavior is certainly present in the pinned McCLIM/ESA source. Whether a
particular end-user key symptom reaches that method still depends on the
backend event form and RPLACA's Drei-gadget integration.

RPLACA keeps one pane-specific adapter that passes those events to Drei's
public `handle-gesture` path. It does not replace ESA or Drei behavior
image-wide. The limitation is covered by modifier-event and real GUI keybinding
tests and is not currently a crash or a reason to carry a fork. If the adapter
becomes insufficient, this method is the first focused upstream patch to
propose.

## Investigated candidates

| Candidate | Current classification | Evidence |
|---|---|---|
| Transient menu sheet reports `Sheet ... is not grafted` during pointer tracking | Confirmed McCLIM-owned robustness defect; trigger was amplified by RPLACA | The official #1312 lifecycle reproduced the exact current condition in 3/3 plain-CLIM Guix/private-Xvfb runs. McCLIM buffers an already-disowned event sheet before lifecycle validation. RPLACA removed live table replacement, keeps a stable menu tree, suppresses ESA's identical-table rebuild for its frame, and removed the whole-top-level retry. |
| ESA minibuffer/basic-medium startup failures | Not reproduced in minimal pinned ESA control | A minimal ESA frame starts and exits on the pinned McCLIM without RPLACA. RPLACA also installed image-wide approximate `basic-medium` metrics and a repaint override; those overrides are removed. The application-owned fixed-height minibuffer retains only a pane-specific `compose-space` method. |
| Edward word kill followed by yank fails on a list/`aref` mismatch | Fixed in the pin | The pinned Edward implementation already normalizes killed word data. The RPLACA replacements for three private `climi::ie-*` methods are removed, and a regression test exercises the pinned implementation directly. |
| Live compose `M-x` entered an undefined `STREAM` path | Application command-table ordering | Drei's `exclusive-gadget-table` handled `M-x` before RPLACA's inherited frame table and entered Drei's blocking extended-command workflow. A pane-specific public `additional-command-tables` method now gives the application command precedence. Headless lookup/parser coverage and live GUI passes prove the non-blocking RPLACA minibuffer path. |
| A rapid `C-c V` became literal `V` in one GUI run | Not proven upstream | The actual pin already filters CLX's documented `:SHIFT-LEFT` and `:SHIFT-RIGHT` standalone modifier names. RPLACA now consumes recognized standalone modifiers explicitly at its pane boundary, preserves the prefix across ESA's identical command-table assignment, and the GUI driver waits for one toggle's semantic completion before beginning the next chord. Repeated live keybinding coverage is the classification gate; there is no stripped-down McCLIM reproducer. |

## Post-adversarial classification

The extended run completed 250 menu operations, 62 semantic selector
activations, 30 expose/unmap-map operations, and three resizes without a crash.
After its one-time setup transition, the bounded resource profile showed no
accumulating RSS, descriptor, or thread trend during the sampled interval. The
Artifactum timestamp crash, screenshot-ordering race, compile-cache and
event-log costs, interop test flake, and inherited X socket namespace were
owned by RPLACA, its tests, or its harness.

Fresh provenance again resolves the tested CLIM, McCLIM, ESA, and Drei systems
from the pinned Guix McCLIM tree. The RPLACA-free ESA control again starts and
returns `:OK`; current evidence is under
`.artifacts/adversarial-mcclim/final-closure/provenance/`. Those bounded results
remain valid evidence that the locally mitigated RPLACA interface is stable
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
`RPLACA_CONTAINER_DISABLE_HOST_X=1`, the Guix E2E wrapper, and a newly started
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
loading RPLACA:

```sh
RPLACA_CONTAINER_DISABLE_HOST_X=1 \
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
  --load "$RPLACA_QUICKLISP_SETUP" \
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

## Native TrueType scalable-font protocol gap

The pinned McCLIM 1.0.0 public font protocol documents
`clim-extensions:font-face-scalable-p` for font-face objects returned by
`font-family-all-faces`. Its native TrueType face class does not implement that
generic. Calling the documented predicate on an ordinary enumerated DejaVu
face therefore signals `no-applicable-method`:

```lisp
(let* ((port (clim:find-port))
       (family (first (clim-extensions:port-all-font-families port)))
       (face (first (clim-extensions:font-family-all-faces family))))
  (clim-extensions:font-face-scalable-p face))
```

The protocol is declared in `Core/windowing/protocol.lisp` and described in
`Documentation/Manual/extensions.texi`. The native TrueType implementation in
`Extensions/fonts/mcclim-native-ttf.lisp` provides
`font-face-all-sizes` and `font-face-text-style`, but no
`font-face-scalable-p` method. The only method in the pinned source tree is for
the separate basic font-face implementation. A fresh private-Xvfb reproduction
failed on `MCCLIM-TRUETYPE:TRUETYPE-FACE` with
`SB-PCL::NO-APPLICABLE-METHOD-ERROR`, without relying on RPLACA internals.

RPLACA avoids the broken predicate. It exposes only positive sizes returned
by the public `font-face-all-sizes` method, then resolves a selected size
through public `font-face-text-style`, mapping, and metric operations. This
retains the native TrueType implementation's listed sizes while deliberately
giving up arbitrary unlisted scalable sizes. It does not inspect implementation
classes, install a global method, or hide non-stream protocol failures. An
unreadable family or face is isolated only when it signals `stream-error`, and
the frame retains its portable generation-zero appearance bundle if optional
enumeration still fails.

The same pinned backend's `port-all-font-families :invalidate-cache t` closes
TrueType streams that can still be mapped by live pane mediums. A subsequent
ordinary compose dispatch or redisplay then signals that the DejaVu font stream
is closed. RPLACA therefore treats explicit refresh as a frame-local logical
generation advance over the current public cache and never invalidates the
backend cache of an adopted live port. Newly installed system fonts become
available after a new frame/application process, not through live refresh.

No fork is warranted while listed sizes satisfy the appearance editor. Revisit
that decision only if arbitrary scalable sizes become a product requirement
and upstream has not supplied the missing public method; at that point, prefer
a focused upstream-compatible method and minimal reproducer over a
RPLACA-specific class test or monkey patch. This finding is recorded locally;
no upstream report was sent.

## Upstream attribution and fork thresholds

Attribute a candidate certainly to McCLIM only when all of these are true:

1. A minimal application contains no RPLACA methods, advice, event classes,
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

1. RPLACA needs the failing lifecycle and cannot safely avoid it locally.
2. A focused McCLIM patch fixes the plain reproducer and passes RPLACA's
   acceptance and adversarial suites.
3. Upstream declines the patch or leaves it unavailable long enough to block
   RPLACA development.
4. Carrying the patch in the Guix package is no longer sufficient or
   maintainable.
