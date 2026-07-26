# Appearance Customization Implementation Plan

- **Status:** planning only; implementation has not begun
- **Decision date:** 2026-07-26
- **Scope:** a CLIM-native, frame-local appearance system for the McCLIM UI
- **Plan provenance:** the initial design was prepared by Fable and then
  strengthened by Sol's xhigh review.

## Objective

Restore comprehensive appearance customization without restoring the old
process-global face system. The resulting design must let users select themes,
override semantic roles, configure supported typography, save those choices,
and inspect or edit them through CLIM-native commands and UI.

It preserves the repository's McCLIM posture:

- semantic roles rather than colors stored in domain data;
- application-frame ownership of active appearance state;
- CLIM inks, designs, drawing options, and text styles at rendering boundaries;
- pane appearance set at construction, with narrowly proven live adapters;
- commands, presentations, completion, and `accepting-values` for interaction;
- ordinary CLIM redisplay rather than a repaint loop; and
- a conservative boundary around the known Drei reentrant-redisplay risk.

This document is an implementation plan, not implementation authorization. It
adds no appearance behavior, test, configuration reader, or UI in this phase.

## Current State and Evidence

The current GUI has literal visual choices in
[`src/mcclim-interface.lisp`](../src/mcclim-interface.lisp):

- `display-chat-message` and `display-chat-tool-activity-summary` select
  literal RGB inks for transcript output.
- `buffer-presentation-entry-ink` maps entry plist `:face` values such as
  `:selector-title`, `:selector-entry`, `:tool-result`, `:system`, and
  `:error` directly to fixed inks.
- `display-buffer-presentation-entry` gives `:selector-title` a hard-coded
  bold face. Selected rows do not have a general filled-background treatment.
- Existing incremental paths use `clim:updating-output`, but their cache values
  do not include an appearance revision or rendering key.
- `clawmacs-chat-frame` already owns UI state and declares the transcript,
  info, compose, minibuffer, and pointer-documentation surfaces. It is the
  correct owner for active appearance state.
- The package system already has package-owned resource allowlists and reload
  machinery in [`src/package-manager.lisp`](../src/package-manager.lisp).
- User initialization is loaded from `~/.clawmacs.d/init.lisp` by
  [`src/main.lisp`](../src/main.lisp), which establishes configuration order.

Some producers use roles that are not currently rendered explicitly, including
`:default-text` and `:selector-separator`. The entry plist `:face` field is a
useful existing wire protocol and should remain so during the first migration;
one adapter maps it to appearance roles.

The bitmap-font editor is separate from UI typography and must not become its
owner as a side effect of this work.

## Historical System: Useful Evidence, Not a Template

The removed face system is evidence that inheritance and CLIM drawing styles
were desired, but it must not be restored verbatim. The new design rejects:

| Rejected pattern | Reason |
|---|---|
| Process-global mutable active faces | Leaks appearance between frames and cannot support independent windows. |
| Faces stored on buffers or messages | Mixes durable/domain data with display preference. |
| A global method on `clim:basic-medium` | Broad, implementation-sensitive interception is not an application styling boundary. |
| A parallel non-CLIM color vocabulary | Duplicates the CLIM ink/design model and obscures portability. |
| Hand-written modal raw-key customization | Bypasses commands, presentations, completion, and standard input editing. |
| Assuming a resolved background or underline is automatically rendered | CLIM text output does not make that promise. |
| Persisting arbitrary Lisp or arbitrary CLIM designs | Unsafe, non-portable, and difficult to validate. |

No compatibility shim should reintroduce these patterns merely to make an old
API compile.

## Ownership and Lifetimes

The system has two deliberately separate layers.

### Global declaration catalog

The process may hold immutable declarations that exist before a frame does:

- appearance-role definitions;
- theme definitions;
- package ownership metadata; and
- a catalog generation number.

The catalog is an extensibility registry. It must never be a mutable global
"current theme" or a cache of port-resolved styles.

### Frame-owned active appearance

Each `clawmacs-chat-frame` should own:

- its active `appearance-profile`;
- unsaved frame-local role overrides;
- a profile revision;
- a pending candidate profile while a change is applied;
- an immutable resolved appearance bundle; and
- the last applied appearance key.

Startup configuration creates an immutable initial profile and passes it into
frame construction. A frame resolves its own bundle after it has a display
port. Multiple frames can use different themes without visual or cache leakage.

## Immutable Data Model

The eventual names may vary to fit nearby package conventions, but these
conceptual objects and boundaries are required:

```lisp
appearance-role-definition
  name documentation fallback-role owner

appearance-style-spec
  foreground background text-family text-face text-size decoration

appearance-theme-definition
  name documentation parent-theme role-overrides owner

appearance-profile
  selected-theme immutable-role-overrides

resolved-appearance-bundle
  catalog-generation profile-revision port-identity role-table
  pane-defaults render-key
```

`appearance-style-spec` needs an internal *unspecified* sentinel for every
component. `nil` must not ambiguously mean both "inherit" and "explicitly
absent." Font family, face, and size inherit independently: a selector title,
for example, may request bold while retaining the base family and size.

Profiles, definitions, role tables, and resolved bundles are immutable after
publication. Candidate construction makes copies; display functions read only
the published bundle. Rendering must not traverse mutable global registries.

### Portable font descriptors and port resolution

Persist fonts only as portable descriptors: family display name, face display
name, and size. Resolve them against the active McCLIM port only after a frame
exists. A resolved text style or mapping belongs to that frame's bundle; it
must never be reused globally across ports.

## Inheritance, Resolution, and Validation

Two independent graphs participate in resolution:

1. theme-parent inheritance; and
2. semantic-role fallback inheritance.

For a requested role, resolve in this order:

1. parent themes from root to child;
2. the selected theme;
3. frame-profile overrides;
4. the role's fallback chain; then
5. the built-in base role.

The implementation must reject before publication:

- cycles or missing parents in the theme graph;
- cycles or missing fallbacks in the role graph;
- invalid inks or unsupported persisted designs;
- invalid text-family, face, or size components;
- malformed or unknown configuration clauses; and
- incomplete profiles that cannot resolve every required role.

Validation applies to the complete candidate profile. A failing partial edit
does not modify active state.

## Initial Role Vocabulary and Themes

Roles describe semantic purpose, not a specific color or rendering method.
The initial core vocabulary is:

| Surface | Roles |
|---|---|
| Transcript | `:transcript-user`, `:transcript-agent`, `:transcript-tool`, `:transcript-system`, `:transcript-empty` |
| Structured output | `:default-text`, `:system`, `:error`, `:tool-result`, `:disabled`, `:selector-title`, `:selector-header`, `:selector-entry`, `:selector-separator`, `:selector-footer`, `:selector-selection` |
| Pane/default surfaces | `:base`, `:transcript-pane`, `:info-pane`, `:compose-pane`, `:minibuffer-pane`, `:modeline`, `:help-pane`, `:pointer-documentation` |

Unknown roles render with `:default-text`, emit one bounded diagnostic, and
never make a display function fail.

Two built-ins are sufficient for the first release:

- `:classic` is the default and reproduces every current color and boldness
  choice. Its first integration must be visually inert.
- `:dark` demonstrates backgrounds, accessible transcript roles, readable
  selector/minibuffer surfaces, and inherited typography without a custom font.

A Genera-inspired theme is a later bundled extension, not a reason to expand
the initial core contract.

## Rendering Boundaries: Portable CLIM and McCLIM Adapters

### Portable CLIM application behavior

At transcript, structured-output, selector, minibuffer, info, and help display
boundaries, apply resolved foreground ink and text style with standard CLIM
facilities such as `clim:with-drawing-options`, `clim:with-text-style`, and
`clim:with-text-face`. Keep presentations intact: visual styling encloses the
existing `clim:with-output-as-presentation` output rather than replacing its
semantic records.

Every styled `clim:updating-output` cache value must include the bundle's
`render-key`. Appearance changes then invalidate output naturally, while an
unchanged key retains incremental redisplay performance.

No display function may mutate the profile, resolve a candidate, save a file,
or initiate pane reconfiguration.

### Pane construction and live updates

Pane foreground, background, and default text style should initially be passed
through standard pane-construction initargs in the existing frame declaration.
Construction-time application is the required first milestone.

Any live pane-color update is a small **McCLIM-specific adapter**, gated by a
probe against the pinned version. It may use per-pane `reinitialize-instance`
and any required synchronization of already-engrafted media, followed by
ordinary `clim:redisplay-frame-pane`. It must not reinitialize the frame,
rebuild the layout, disown panes, or mutate output records. If an adapter step
fails, the activation transaction rolls back.

Text backgrounds are not supplied merely by `with-drawing-options`. Initial
selector selection therefore keeps the present bold/marker indication. A
filled selection background is deferred until a public CLIM decoration
mechanism has been proven to preserve presentation nesting, selector geometry,
pointer interaction, and incremental redisplay. No manual painting or
output-record surgery is allowed.

Keep `(:pointer-documentation t)` and the existing layout unchanged. Do not
replace the generated pointer-documentation stream with an explicitly laid-out
pane; that layout is already sensitive (see
[`MCCLIM-ISSUES.md`](MCCLIM-ISSUES.md) and [`STABILITY.md`](STABILITY.md)).
Menu bars, scrollbars, and general gadget chrome are outside the initial scope
because McCLIM has no stable public general-purpose chrome-theming API.

## Fonts and Drei

The initial typography scope is transcript, info, minibuffer, help output, and
construction-time compose typography. Font resolution uses public port font
enumeration and face-to-text-style APIs. An unavailable font falls back
predictably and produces an actionable diagnostic.

Live compose font changes, cursor theming, and metrics-affecting editor changes
are deferred. Drei may cache measurements that do not include text style in
each cache key. Until a focused probe passes, compose typography is
restart/recreate-only rather than a live repaint-time mutation.

Appearance code must never call `clim:change-space-requirements` from compose
display or redisplay. Existing selector-space management is a separate layout
concern and must not become an appearance-update mechanism.

## Transactional Activation

All profile changes—interactive edit, file reload, package removal, or theme
selection—use one activation path on the frame process:

1. Construct an immutable candidate profile.
2. Queue the request through the existing frame event/redisplay path.
3. Resolve and validate the complete candidate against the frame's active port.
4. Snapshot affected live pane appearance.
5. Apply only properties proven safe for live McCLIM updates.
6. On failure, restore the pane snapshot and retain the old profile and bundle.
7. On success, atomically publish the profile and resolved bundle.
8. Request one necessary redisplay; unchanged later output remains incremental.

Activation must never regenerate layout, reinitialize a frame, mutate private
Drei state, alter compose geometry while repainting, or silently swallow an
error.

## Configuration, Reload, and Persistence

The proposed configuration file is:

```text
~/.clawmacs.d/appearance.sexp
```

Its versioned, data-only format is:

```lisp
(:clawmacs-appearance
 :version 1
 :theme :dark
 :styles
 ((:transcript-user :foreground "#7aa2f7")
  (:base :font-family "DejaVu Sans Mono"
         :font-face "Book"
         :font-size 14)))
```

The parser reads exactly one form with `*read-eval*` bound to `nil`, bounds
input size and nesting, rejects trailing forms and unknown clauses, and rejects
arbitrary Lisp objects or arbitrary CLIM designs. It writes only the portable
subset accepted by the parser.

Precedence is:

```text
built-ins
< appearance.sexp
< init.lisp
< environment
< command line
< unsaved frame-local edits
```

Apply environment and command-line selectors after `init.lisp`, so init code
can register themes referenced by those selectors. An invalid startup file
falls back to `:classic` with one warning. An invalid live reload retains the
last valid profile. Saving is explicit and atomic; interactive changes are not
automatically persisted.

"Safe Reload" refreshes declarations and re-resolves active frames only. It
does not reread `appearance.sexp`, rerun `init.lisp`, or save unsaved edits.
Reloading the appearance file is a separate explicit command.

## Commands and Appearance UI

Initial user-visible commands are:

- Switch Appearance Theme
- Describe Current Appearance
- Customize Appearance
- Save Appearance
- Revert Unsaved Appearance
- Reload Appearance File

Restore `C-h F` and `C-c F` compatibility bindings. Do not assign `C-c t`: it
already toggles tool results. Where inexpensive, preserve
`customize-face-command` as a deprecated alias that dispatches the new command.

The editor uses presentation types for theme names and roles, existing
minibuffer completion, rendered style samples, and `clim:accepting-values` for
conventional role fields. Static menu items dispatch frame commands. It must
not recreate the historical raw-key modal form. This follows the command and
presentation model in [`AGENTS.md`](../AGENTS.md) and
[`STABILITY.md`](STABILITY.md).

## Package and Plugin Ownership

Plugin integration follows the core system, not precedes it. Add one
package-resource category, `:appearance`, through the existing package
allowlist and ownership plumbing. Packages may register role definitions and
theme definitions; they may not modify a frame's resolved bundle directly.

On unload or failed reload:

1. remove package-owned declarations;
2. validate the remaining catalog;
3. resolve every affected active frame; and
4. transactionally fall back to `:classic` when the selected theme disappears.

No dangling role fallback or parent-theme reference may remain published.

## Accessibility

Built-in themes fail validation when normal text contrast is below 4.5:1. The
3:1 threshold is allowed only for text demonstrably large or bold enough to
qualify. User and package themes receive warnings by default, unless the user
enables strict validation.

Color is never the only selection cue. Retain the initial bold/marker treatment
until a safe, tested filled-decoration mechanism exists.

## Atomic Implementation Sequence

Each future commit must compile and pass its focused tests independently.

1. `feat(appearance): Add immutable appearance definitions and built-in profiles`
2. `feat(ui): Render frame-owned appearance roles`
3. `feat(ui): Apply pane colors and switch themes safely`
4. `feat(config): Load and save appearance profiles`
5. `feat(appearance): Resolve selectable fonts per McCLIM port`
6. `feat(ui): Add the CLIM appearance editor and migration aliases`
7. `feat(packages): Own package appearance registrations`
8. `test(gui): Prove appearance persistence and reload behavior`

Risky work remains isolated after its probe: live Drei typography, cursor
theming, filled selector decoration, and pointer-documentation theming.

## Verification Gates

### FiveAM

Add deterministic coverage for:

- parser round trips, input bounds, and rejection cases;
- immutable candidate updates and frame-local isolation;
- theme/role cycle detection and missing references;
- independent inheritance for family, face, and size;
- complete mapping of every first-party `:face` value;
- `:classic` golden values matching today's literals;
- unknown-role fallback with bounded diagnostics;
- port-specific font resolution and unavailable-font fallback;
- transactional rollback after a failed pane update;
- cache invalidation when, and only when, the render key changes;
- Safe Reload preservation and configuration precedence;
- atomic persistence behavior; and
- package unload fallback and ownership cleanup.

### Guix, Xvfb, and GUI proof

Use the repository's container wrappers and the existing GUI E2E discipline in
[`GUI-E2E.md`](GUI-E2E.md). Required proof includes:

- a hardened cold full-system build and the full headless suite;
- an isolated Xvfb appearance scenario;
- a semantic snapshot after theme switching;
- persistence through application restart;
- actual resize and expose events;
- menu/keybinding stress;
- existing compose-geometry, switch-buffer, and pointer-tracking suites; and
- empty stderr, a clean runtime-failure scan, and an empty owned process group.

Use semantic snapshots first. If pixel checks become necessary, sample tolerant
known pane-background coordinates rather than committing screenshot baselines.

## Required Probes Before Optional Work

The following questions require focused evidence on the exact pinned McCLIM and
Drei versions before implementation expands scope:

1. Does per-pane `reinitialize-instance` update an already-engrafted medium,
   and what synchronization is required?
2. Do non-Drei live text-style changes alter space requirements?
3. Can font family and face display names resolve predictably on the active
   port?
4. Does a public CLIM decoration mechanism preserve presentations, selection
   geometry, and incremental redisplay?
5. Can the generated pointer-documentation stream be styled without changing
   frame layout?
6. Does `accepting-values` nest cleanly in the current ESA/Drei command loop?
7. What declaration order is safe across built-ins, `init.lisp`, package
   loading, and Safe Reload?
8. Does live Drei typography survive 100/101/112-character single lines,
   roughly 32 KiB of multiline content, point/mark/undo preservation, repeated
   theme switches, resize/expose events, and zero compose-space mutation during
   redisplay?

If the final probe fails, compose typography remains restart-only. A failed
optional probe is a scope boundary, not a reason to compromise rendering.

## Decisions to Refine Before Implementation

The recommended starting decisions are:

| Decision | Starting point |
|---|---|
| Default theme | `:classic` |
| Additional bundled theme | `:dark` |
| Active appearance scope | per frame |
| Persistence | explicit save |
| Configuration filename | `appearance.sexp` |
| Live scope | colors first; compose typography restart-only |
| Selector highlight | current marker/bold first |
| Pointer documentation | leave unchanged initially |
| Plugin support | after core roles, persistence, and UI are stable |
| Contrast policy | strict for built-ins; warnings for user/package themes |
| Compatibility | `C-h F`, `C-c F`, and preferably the old command alias |

These are refinable decisions, not completed product behavior. Before the first
implementation commit, confirm the live-update probe, persistent schema
surface, and whether the compatibility alias still has external users.

## Non-Goals for This Phase

This planning phase changes no source, tests, generated artifacts, runtime
configuration, plugin API, or application appearance. It does not approve a
renderer rewrite, frame/layout redesign, raw event loop, output-record
mutation, direct medium/sheet manipulation, or live Drei typography change.
