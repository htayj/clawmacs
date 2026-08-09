# RPLACA Listener Interface

RPLACA's primary UI is the `rplaca-listener` McCLIM application frame. It uses
one chronological CLIM interactor for input, Lisp results, commands, session
replay, and settled assistant turns. The listener has no transcript pane,
`view-buffer` switch, or separate conversation view.

## Quote and Unquote Input

The listener reads three kinds of ordinary input: Lisp forms, CLIM commands,
and prose for the agent. `listener-input-mode` chooses only the default for a
bare line. It is separate from the durable agent `interaction-mode` described
in `MODE-SYSTEM-DESIGN.md`.

| Input | Eval mode, prompt `CL-USER>` | Say mode, prompt `CL-USER!>` |
|---|---|---|
| bare line | Evaluate a Lisp form. | Send prose to the agent. |
| `,Command` | Run a CLIM command. | Run a CLIM command, unquoting from prose. |
| `,(form)` | Evaluate one Lisp form. | Evaluate one Lisp form, unquoting from prose. |
| `!text` | Send one prose turn. | Force one prose turn. |
| `!` alone | Enter Say mode. | Return to Eval mode. |
| `,` alone | Report `did you mean ,Command or ! to talk?`. | Return to Eval mode. |
| `#!cmd` | Run a shell command. | Run a shell command. |
| empty or whitespace-only line | No-op and prompt again. | No-op and prompt again. |
| line beginning with whitespace | Use the mode default for the full, untrimmed line. Prefix dispatch is disabled. | Use the mode default for the full, untrimmed line. Prefix dispatch is disabled. |
| prose containing `,(form)` | Evaluate the form and splice its bounded primary value into the prose. | Evaluate the form and splice its bounded primary value into the prose. |
| prose containing `,,(` | Send a literal `,(`. | Send a literal `,(`. |
| multiline Lisp form | Continue input until the form is complete, then evaluate it. | Use `,(form)` to unquote; input continues until the form is complete. |
| multiline prose in the interactor | Reject it with `Use ,Compose for multiline prose.` | Reject it with `Use ,Compose for multiline prose.` |
| `!foo` symbol with a leading space | Evaluate the full line as Lisp, including its leading space. | Send the full line as prose, including its leading space. |

`#!` is recognized before the `!` prose rule. In prose, `,(form)` evaluates the
form and splices its bounded primary value into the text. `,,(` means a literal
`,(`. A form that returns no values contributes an empty splice. Interpolation
doesn't change the REPL history variables, and an interpolation error aborts
the send instead of sending partial prose.

Leading whitespace is the escape for a Lisp symbol whose printed name starts
with `!`. In Eval mode, `!foo` sends `foo` to the agent, while ` !foo` evaluates
the symbol `!foo` because dispatch is disabled for that line.

The input editor accepts a Lisp form across multiple lines until it is
complete. The ordinary interactor rejects multiline prose with `Use ,Compose
for multiline prose.` Use the Compose flow for long prose.

## One Chronological Interactor

The frame's standard input, standard output, and error output all meet in the
interactor. Each accepted input and result stays in chronological order. Lisp
values are CLIM presentations. Commands use the listener command table and
typed presentation arguments, so keyboard and pointer use share the same
command boundary.

An agent turn adds one final `assistant-turn` presentation after the request
settles. Streaming tokens, intermediate pipeline stages, transient system
messages, and partial assistant text aren't written into the interactor. A
redisplay therefore can't duplicate a response or turn temporary progress into
durable history.

## Waiting and Cancellation

`com-say` owns the active request and remains inside the listener command until
the provider and tool loop settle. The command loop doesn't print the next
prompt during this wait. This prompt-withheld contract means a visible prompt
always indicates that the listener can accept another submission.

Progress appears in the wholine beside the session, package, and directory.
While a request is active, the wholine also shows `Esc to cancel`. Ordinary
keys typed during the wait are discarded and beep once, so they can't become
typeahead for the next prompt.

Escape and the CLIM abort gesture, normally `Ctrl-C`, request cancellation.
Cancellation is idempotent: repeated gestures stop the response only once.
The listener keeps waiting until provider and runtime cleanup settle, then
reports cancellation and returns to a fresh prompt. Frame exit or another
nonlocal unwind follows the same cleanup path.

## Results and Facets

The settled assistant body is shown once, inline. Extra data is summarized by
presented facet labels only when that facet has content:

| Facet | Details content |
|---|---|
| `[tools]` | Tool calls, presented by tool name. |
| `[reasoning]` | Final collected reasoning blocks. |
| `[metadata]` | Stable key/value metadata for the turn. |
| `[artifacts]` | Artifact references as selectable presentations. |
| `[media]` | Media references as selectable presentations. |
| `[inspect]` | The turn's inspectable payload. |

Selecting a facet stores the selected turn and switches the frame from
`listener-only` to `listener+details`. The details display function only reads
that selection and renders the requested facet. Its `[close]` presentation
clears the selection before restoring `listener-only`. The interactor, wholine,
and pointer-documentation panes are the same pane objects in both layouts.

Long help and manual content may use the details layout. Features that need
their own application surface may open a secondary frame rather than replacing
the listener or taking over its interactor.

## Listener Commands

Type a comma followed by a command-line name. CLIM prompts for typed arguments
and presents completion candidates in the interactor.

The core listener commands include:

- `,Lisp Mode`, `,Say Mode`, `,Stop Response`, and `,Compose`;
- `,Push Directory`, `,Pop Directory`, and `,Display Directory Stack`;
- `,Apropos`, `,Describe`, `,Inspect`, `,Load File`, `,Compile File`,
  `,In Package`, and `,Room`;
- `,Model`, `,Think-Level`, `,Buffer`, `,Skill`, `,Package`, `,Project`, and
  `,File`;
- `,Help Commands`, `,Help`, `,Manual`, and `,Info`;
- `,Safe Reload` and `,Reload Appearance`; and
- `,New Session`, `,List Sessions`, and `,Resume Session`.

`,List Sessions` writes saved sessions as presentations. Selecting one invokes
the same resume command as `,Resume Session`. A resumed branch is replayed once
inline in the chronological interactor. `,New Session` creates and activates a
fresh persistent conversation. Creating a separate listener window uses the
New Listener command, with each frame owning its own listener context and
conversation buffer.

Packages extend this surface with `define-rplaca-listener-command` and normal
CLIM presentation translators. Commands own actions; display functions only
reflect state.

## Compose Flow

`,Compose` is the multiline prose path:

1. Reject the command if an agent turn is already active.
2. Open a CLIM `accepting-values` dialog with a text-editor view.
3. Return without sending if the dialog is cancelled.
4. Reject blank text.
5. Pass the complete text to `com-say` exactly once.
6. Apply the same interpolation, wait, cancellation, wholine, final-result,
   and facet contracts as one-line prose.

Compose is a transient CLIM dialog, not a permanent pane or editable transcript
buffer. There is no private compose keymap or `chat-compose` API.

## Removed Init APIs

User init files still load with `*package*` bound to `RPLACA`, but the retired
UI names aren't compatibility aliases.

| Removed API or pattern | Replacement |
|---|---|
| `rplaca-chat-frame` | `rplaca-listener` |
| `run-rplaca-chat-frame` | `rplaca:run`, or internal `run-rplaca-listener` |
| `define-rplaca-chat-frame-command` | `define-rplaca-listener-command` plus CLIM translators |
| `com-chat-*` and `chat-frame-*` | Named listener commands and listener frame/context accessors |
| transcript pane, transcript filtering, `view-buffer`, and `,Conversation` | No replacement; use the chronological interactor and details facets |
| compose pane and `compose-pane-*` | Ordinary interactor input or `,Compose` |
| `listener-state` and `make-listener-state` | `listener-context` |
| `listener-state-last-values` and `listener-state-command-history` | No replacement; session history replays inline |
| `listener-buffer-p`, `make-listener-buffer`, and `ensure-listener-buffer` | `rplaca-listener`; create another frame with New Listener |
| `submit-listener-input` | CLIM interactor submission, or `com-eval` and `com-say` programmatically |
| live buffer kind `:listener` | Retired snapshot migration input; active conversations use `:chat` |

The normal McCLIM frame, pane, command, presentation, translator, and redisplay
contracts remain in force. Only the old application-specific UI layer was
removed.
