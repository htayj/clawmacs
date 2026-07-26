# Genera-Informed Appearance Customization Companion Plan

- **Status:** planning only; implementation has not begun
- **Scope:** historical critique and architectural amendment to
  `docs/APPEARANCE-CUSTOMIZATION-PLAN.md`
- **Historical profile:** Symbolics Genera 8.5 evidence as documented by the
  Lisp Machine Container Museum
- **Implementation target:** Clawmacs on pinned McCLIM `1.0.0-koliada`
- **Compatibility goal:** no Genera compatibility is required

## Purpose and relationship to the existing plan

This is a companion to, not an edit of, the existing appearance-customization
plan. The original remains the baseline for:

- frame-owned active appearance;
- immutable declarations and resolved bundles;
- semantic roles;
- canonical CLIM drawing, text-style, presentation, command, and redisplay
  mechanisms;
- transactional activation;
- explicit persistence;
- conservative live updates;
- isolation from Drei renderer internals; and
- package ownership after the core is stable.

This companion amends the original plan where historical Genera evidence
exposes ambiguity or missing contracts. It supersedes the original plan's
single flat `appearance-style-spec`, underspecified inheritance order, incomplete
relative-size semantics, and overly simple font-cache model.

If the documents disagree during later consolidation, the recommendations in
this companion take precedence for:

1. the internal separation of typography, ink, surface/background, and
   decoration;
2. role-stack composition;
3. relative-size semantics;
4. port/font inventory invalidation;
5. typed validation and recovery;
6. the startup boundary between port resolution and pane construction; and
7. package unload transactions.

It does not authorize implementation.

## Evidence versus inference

The historical evidence and the design conclusions drawn from it must remain
distinct.

### Historical evidence

Genera has several adjacent appearance systems, not one face object:

- character styles are semantic family/face/size triples;
- devices resolve those triples to concrete fonts;
- sheets and formatted output carry default, current, and effective styles;
- native drawing has colors, ALUs, stipples, tiles, and opaque/transparent
  raster behavior; and
- CLIM has a separate design/ink algebra and text-style model.

This separation is the primary finding of
[Inks, faces, and character styles in Symbolics Genera — introduction](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/inks-faces-and-character-styles.md#inks-faces-and-character-styles-in-symbolics-genera).

Genera merges family, face, and size independently. Relative size is an
operation on an inherited absolute size, using a finite named ladder with
clamping; it is not a permanently scalable-font instruction.
[Exact merge behavior](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/inks-faces-and-character-styles.md#exact-merge-behavior).

Font mappings are device-owned. Successful resolution and recovery may be
cached, while global and per-device invalidation ticks prevent stale mappings.
A literal device-font style is an explicit nonportable escape hatch.
[Resolution to a real font](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/inks-faces-and-character-styles.md#resolution-to-a-real-font).

Applications often refer to semantic role variables—heading, emphasis,
deemphasis, status, mouse documentation—rather than concrete raster font names.
Those roles are distributed among subsystems; Genera 8.5 does not show one
system-wide atomic theme catalog.
[Relationship to Emacs faces and themes](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/inks-faces-and-character-styles.md#relationship-to-emacs-faces-and-themes).

Genera's style presentations are device-aware: completion offers only viable
choices and can show the resolved fonts and samples.
[Zmacs text customization](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/inks-faces-and-character-styles.md#zmacs-text).

Genera persists a document's default character style separately from the
styles attached to individual characters.
[Defaults, files, and mail](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/inks-faces-and-character-styles.md#defaults-files-and-mail).

The resident display fonts in the studied world are concrete one-bit raster
font objects, while printer paths can use different resources, including
outline mechanisms. A semantic family name therefore does not identify one
universal font artifact.
[Raster display fonts and outline hardcopy fonts](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/extracting-resident-fonts.md#raster-display-fonts-and-outline-hardcopy-fonts).

Genera stipples are native raster masks whose effect depends on drawing ALUs,
opacity behavior, phase, color state, and the device. They are not fonts, RGB
colors, or CLIM opacity objects.
[Gray patterns conclusion](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/gray-patterns-and-stipples.md#conclusion) and
[dynamic pattern protocols](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/gray-patterns-and-stipples.md#dynamic-patterns-that-are-not-fixed-bit-cells).

Indexed color, direct color, color-map allocation, and RGB color objects are
also distinct representation layers. The historical Color Editor commits or
aborts a temporary candidate instead of treating every edit as durable state.
[Two ways pixels become colors](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/color-systems-and-color-editor.md#two-ways-pixels-become-colors) and
[Genera Color Editor architecture](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/color-systems-and-color-editor.md#genera-color-editor-architecture).

Genera CLIM is a separate portable layer over the native environment. Its
sheets, ports, media, designs, text styles, output records, presentations, and
commands must not be collapsed into Dynamic Windows internals.
[Dynamic Windows relationship](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/clim-2-on-genera.md#dynamic-windows-relationship) and
[complete CLIM facility map](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/clim-2-on-genera.md#complete-facility-map).

### Design inference for Clawmacs

The following are Clawmacs design conclusions, not claims about Genera:

- An appearance theme should coordinate independent appearance axes, not turn
  them into one undifferentiated face object.
- Active state should remain centralized per Clawmacs frame even though Genera
  customization was distributed.
- McCLIM port resolution should be cached and explicitly invalidated, but
  Genera's cache implementation should not be copied.
- A temporary editor candidate with Apply/Cancel is useful inspiration; the
  Genera Color Editor itself is not a UI template.
- Genera's literal device-font escape hatch justifies considering an expert
  McCLIM escape hatch later; it does not justify making backend font names the
  ordinary configuration format.
- Genera's document-style persistence is evidence for keeping defaults and
  styled content separate. Because Clawmacs is not a rich-text editor, its
  correct interpretation is to persist appearance preferences without
  attaching resolved styles to messages or buffers.

## Current Clawmacs evidence that the amended plan must cover

Current source confirms:

- transcript sender colors are literal `clim:make-rgb-color` values;
- tool-summary color is duplicated as another literal;
- generic entry faces map directly to fixed inks;
- `:selector-title` alone receives hard-coded bold output;
- `:selector-selected` is the existing generic-entry wire value;
- minibuffer candidates independently render a visible `>` marker and bold
  selected text;
- `:default-text` and `:selector-separator` are produced but fall through to
  the default foreground;
- generic entries and transcript items use `clim:updating-output` without an
  appearance key;
- candidate-column estimation depends on the current text-style width;
- the frame owns lifecycle and redisplay state already;
- panes are generated during `make-application-frame`;
- `run-clawmacs-chat-frame` does not currently pass an appearance profile,
  resolved bundle, explicit port, or explicit frame manager; and
- the compose pane deliberately fixes its geometry to avoid repaint-time Drei
  relayout.

The existing plan's use of `:selector-selection` must therefore be understood
as a new semantic state role. It is not the existing wire keyword. The adapter
must map wire `:selector-selected` to a stack containing
`:selector-entry` and `:selector-selection`.

## Point-by-point critique of the existing plan

| Existing decision | Disposition | Companion amendment |
|---|---|---|
| Semantic roles instead of colors in domain objects | **Retain** | Add explicit content, surface, and state role kinds. |
| Frame-owned active appearance | **Retain** | Add frame-local font-inventory generation and staged-candidate state. |
| Immutable global declaration catalog | **Retain** | Catalog publication itself must be transactional. |
| One `appearance-style-spec` containing foreground, background, typography, and decoration | **Change** | Split these into orthogonal typed specifications, coordinated only by a role style/theme. |
| Theme-parent graph and role-fallback graph | **Retain with change** | Define exact per-layer, per-role, and role-stack cascade order. |
| Independent family/face/size inheritance | **Retain and expand** | Add CLIM-relative sizes, one-time relative resolution, clamping, and explicit failure over unsupported numeric/device bases. |
| Persist family display name, face display name, and size | **Retain with safeguards** | Resolve names through port enumeration objects and `font-face-text-style`; reject ambiguous matches and never assume display names equal backend text-style tokens. |
| One resolved bundle per frame/port | **Retain** | Include explicit font-inventory generation and role-specific render keys. |
| Unknown rendering roles fall back and log once | **Retain** | Configuration and catalog declarations still reject unknown roles; only runtime display adapters use fallback. |
| `:classic` and `:dark` built-ins | **Retain** | Keep `:dark` typography-compatible with `:classic` initially so it can exercise safe color-only live activation. |
| CLIM drawing options and text-style bindings | **Retain** | Apply final role stacks at output boundaries and distinguish pane default, dynamic current, and effective text style. |
| Pane-construction appearance first | **Retain with a new probe** | Determine when the target port exists relative to pane generation before promising named-font construction. |
| Per-pane live `reinitialize-instance` adapter | **Defer pending probe** | Treat each axis as a capability; do not partially activate a coordinated theme when one changed axis is unsafe. |
| Filled text-selection background | **Defer** | Keep marker/bold/foreground treatment until a public decoration mechanism passes its probe. |
| Live compose typography | **Defer** | Also gate proportional compose/minibuffer fonts because current column and geometry logic assumes representative character width. |
| Transactional activation | **Retain and strengthen** | Candidate resolution, delta classification, pane mutation, publication, catalog changes, and package unload all participate. |
| Data-only `appearance.sexp` | **Retain with schema change** | Encode typography, foreground, background, and decoration separately; omit means inherit, while `:none` is legal only where meaningful. |
| Safe Reload does not reread configuration | **Retain** | Safe Reload may reconcile declaration generations but must preserve active/staged/persisted profile distinctions. |
| Presentation-based customization UI | **Retain and expand** | Use device-aware font presentations, resolved samples, typed conditions, Apply/Cancel, and explicit font-inventory refresh. |
| Package-owned roles/themes | **Retain with stronger rollback** | A package unload that cannot transition every affected frame safely must be refused or rolled back. |
| Built-ins use 4.5:1 or 3:1 for qualifying large text | **Change** | Require 4.5:1 for all initial built-in text; port-dependent size makes the large-text exemption unnecessarily fragile. |
| One global render key in all styled records | **Change** | Keep a bundle key, but prefer role-stack-specific keys so unrelated style changes do not invalidate every output record. |
| Arbitrary CLIM designs rejected from persistence | **Retain** | The initial persisted subset remains opaque RGB and standard ink tokens; no stipple, ALU, pattern, or executable design forms. |
| Genera-inspired bundled theme later | **Retain** | It must use ordinary CLIM semantics and cannot import Genera native raster concepts. |

## Revised architecture

### 1. Appearance roles remain semantic

An appearance role names what output means. It does not contain an RGB value,
font object, pane, output record, or medium.

```lisp
appearance-role-definition
  id
  kind
  documentation
  fallback-role
  supported-axes
  owner
```

`kind` is one of:

- `:surface` — a pane or default output surface;
- `:content` — user, agent, error, title, separator, and similar meaning;
- `:state` — selected, disabled, focused, or another transient UI state.

A fallback must normally have the same kind. Cross-kind fallbacks require an
explicit future justification; they are rejected initially.

Core role IDs may remain keywords. Package-owned roles need stable
owner-qualified identifiers so unloading a Common Lisp package does not make a
persisted symbol unreadable. The exact external spelling should be decided
before plugin implementation.

### 2. Appearance axes are orthogonal

Replace the flat style structure with:

```lisp
appearance-typography-spec
  family
  face
  size

appearance-ink-spec
  foreground

appearance-surface-spec
  background

appearance-decoration-spec
  kind
  parameters

appearance-role-style
  typography
  foreground-ink
  surface
  decoration
```

This is not four independently selected themes. A theme still coordinates a
complete visual design by mapping roles to partial `appearance-role-style`
values. Orthogonality means:

- typography never contains color or background;
- foreground is a CLIM ink/design request, not a font property;
- background belongs to a surface or a specifically supported decoration;
- decoration is a small tagged policy, not an arbitrary property bag;
- unsupported axes are rejected rather than silently ignored.

Every component has an internal unspecified sentinel. In configuration,
omission means inherit. `nil` is not accepted as a user-visible synonym for
inheritance. `:none` is permitted only for components with a real absence
meaning, such as decoration.

The first decoration vocabulary should remain deliberately small. It may
express the existing selection marker and semantic emphasis. It must not
promise underline, strikeout, filled text background, border, stipple, or box
output until each has a canonical CLIM implementation.

### 3. Themes coordinate the axes

```lisp
appearance-theme-definition
  id
  documentation
  parent-theme
  role-overlays
  owner
```

A theme is a coordinated map of role overlays. It is not the semantic role
catalog and it does not own port-resolved font objects.

`:classic` is the root visual profile matching current output exactly.
`:dark` initially changes only axes proven safe for live switching. In
particular, it should inherit the same typography as `:classic` until font
activation has its own proof.

A future Genera-inspired theme may use historical proportions and semantic
contrast as visual inspiration, but:

- it uses CLIM inks and McCLIM text styles;
- it does not register Genera raster font names as semantic families;
- it does not import Genera stipple symbols;
- it does not emulate raster ALUs; and
- it does not claim historical fidelity across devices.

### 4. Profiles, candidates, and bundles have distinct lifetimes

```lisp
appearance-profile
  selected-theme
  immutable-role-overrides

appearance-candidate
  profile
  origin
  validation-diagnostics
  activation-class

resolved-role-style
  text-style
  foreground-ink
  background-ink
  decoration
  provenance
  render-key

resolved-appearance-bundle
  catalog-generation
  profile-revision
  font-inventory-generation
  port-identity
  role-table
  surface-defaults
  render-key
```

Each frame owns:

- active profile;
- active resolved bundle;
- profile revision;
- font-inventory generation;
- staged candidate, if any;
- unsaved override state;
- persisted-profile comparison state; and
- last activation result.

The staged candidate is not active. A validated candidate requiring restart
may be previewed or saved, but it must not be described as the current
appearance.

## Exact cascade and merge semantics

The existing plan's resolution order is not precise enough when theme
inheritance, role fallbacks, user base overrides, and selection overlays meet.

### Source layers

Resolve source layers from lowest to highest precedence:

1. built-in defaults;
2. root-to-leaf parent themes;
3. selected theme;
4. persisted profile overrides;
5. `init.lisp` startup overrides;
6. environment overrides;
7. command-line overrides;
8. unsaved frame-local overrides.

Not every layer must exist.

### Role fallback inside each source layer

For one role and one source layer:

1. walk its fallback chain from the root fallback toward the requested role;
2. overlay each role's partial value component by component;
3. the requested role wins over its fallback only for specified components.

Then overlay the resolved result of each source layer from low to high.

This ordering means a user override on `:base` can intentionally change all
roles, including roles that a lower-precedence theme specified individually.
A more specific user role override then wins within that same user layer.

### Role-stack composition

Rendering uses an ordered role stack rather than forcing every interaction
combination into a new role:

```text
surface role
< content role
< state role
```

Examples:

```text
(:transcript-pane :transcript-user)
(:info-pane :modeline)
(:minibuffer-pane :selector-entry :selector-selection)
(:transcript-pane :default-text :disabled)
```

Each later resolved role overlays only the components it specifies.

The existing entry-plist adapter maps:

| Existing wire face | Semantic role stack |
|---|---|
| `:default-text` | `(:default-text)` |
| `:selector-title` | `(:selector-title)` |
| `:selector-header` | `(:selector-header)` |
| `:selector-entry` | `(:selector-entry)` |
| `:selector-selected` | `(:selector-entry :selector-selection)` |
| `:selector-separator` | `(:selector-separator)` |
| `:selector-footer` | `(:selector-footer)` |
| `:tool-result` | `(:tool-result)` |
| `:system` | `(:system)` |
| `:disabled` | `(:default-text :disabled)` |
| `:error` | `(:error)` |

The minibuffer's selected candidate must use the same
`:selector-selection` state role while retaining its marker. This removes two
independent hard-coded selection styles without changing presentation
semantics.

### Typography merge

Family, face, and size merge independently.

- An unspecified family inherits.
- An unspecified face inherits.
- An unspecified size inherits.
- A specified family or face replaces that component.
- A compound bold-italic face must be explicit. The resolver must not invent a
  bold-italic merge from independently encountered bold and italic faces.
- A fully resolved role style contains no unspecified components.

Portable size vocabulary:

```text
:tiny
:very-small
:small
:normal
:large
:very-large
:huge
```

Relative overlays:

```text
:smaller
:larger
```

A relative overlay is consumed once where it occurs in the cascade, stepping
the current effective logical size and clamping at the ends. The resolved
bundle stores the resulting absolute size, while the profile retains the
relative instruction.

Do not adopt Genera-specific `:bigger`, `:same`, or `:stretched`.

The initial implementation should reject a relative overlay whose inherited
base is:

- a numeric point size;
- an unresolved size;
- an exact device font with no logical-size mapping; or
- another value whose next CLIM logical size is undefined.

A future explicit numeric scaling policy is possible, but it must not be
smuggled in as if CLIM defined one.

## Port-aware font resolution

### Portable and enumerated font descriptors

Support two ordinary descriptor classes:

1. portable CLIM typography, using portable family, face, and logical or
   numeric size values;
2. an enumerated McCLIM face selected from the active port by user-facing
   family name, face name, and size.

For enumerated fonts:

1. call the port's public font-family enumeration;
2. match the stored family display name;
3. enumerate that family's faces;
4. match the stored face display name;
5. validate scalable versus fixed-size behavior;
6. obtain the proper text style through `font-face-text-style`; and
7. validate the resulting mapping and metrics on the target port.

Do not reconstruct backend text-style family and face strings from the display
names. McCLIM explicitly does not promise those strings are identical.

Duplicate display-name matches are an ambiguity error, not a reason to take the
first result.

Do not mutate `(setf text-style-mapping)` from Clawmacs merely to install a
profile. Port text-style mappings and their internal caches are shared
implementation state; changing them could affect other frames or unrelated
CLIM applications.

### Font inventory generation and invalidation

Each frame bundle key includes:

- opaque target port identity;
- frame-local font-inventory generation;
- catalog generation; and
- effective profile content.

The app should not pretend to detect every operating-system font change.
Instead provide an explicit **Refresh Font Inventory** operation:

1. ask McCLIM to invalidate the port's font-family cache through its public
   enumeration protocol;
2. increment the frame-local font-inventory generation;
3. rebuild the staged or active candidate;
4. validate it completely; and
5. publish only if activation is safe.

Changing ports, recreating the frame on another port, reloading the declaration
catalog, or explicitly refreshing the inventory invalidates resolved font
entries.

Negative lookups may be cached only within the current bundle/generation.
Diagnostics must record the requested descriptor, port, fallback taken, and
generation.

### Literal device-font escape hatch

Genera's `:DEVICE-FONT` facility demonstrates a legitimate need for exact,
nonportable control, but it should be deferred in Clawmacs.

If later implemented, a device-font descriptor must:

- be explicitly marked nonportable;
- include a port/backend discriminator;
- contain data only, never a font object;
- be resolved with McCLIM's public device-font facility;
- include an explicit portable fallback;
- fail with a typed condition when its discriminator or literal name does not
  match;
- never appear in built-in or package themes; and
- never mutate a port's global font mappings.

It belongs behind an Advanced choice in the editor. It is not part of the
version-1 persistence contract until a pinned-McCLIM probe establishes exact
failure and round-trip behavior.

### Font scope and geometry constraints

Initial named-font scope:

- transcript;
- help;
- info/modeline; and
- noneditable structured output.

Minibuffer selectors and compose should initially require a fixed-width
resolved font, because current completion-column estimation and compose
geometry use representative character metrics.

Construction-time compose typography remains allowed only after the
port/pane-construction probe. Live compose typography remains restart-only
until the existing Drei stress gate passes.

No application font fallback engine should be added. Use McCLIM's normal
undefined-font/text behavior for missing glyphs, with a focused sample probe and
diagnostic. Do not intercept renderer internals per character.

## CLIM output-boundary contract

Genera's default/current/merged distinction has a direct portable CLIM
analogue, but Clawmacs should use CLIM's model rather than emulate TV sheets.

### Pane default

A pane's surface role supplies its fully resolved default foreground,
background, and default text style at construction where standard pane initargs
support them.

### Dynamic current style

A display function applies content and state role overlays with:

- `clim:with-drawing-options`;
- `clim:with-text-style`; and
- the narrower family/face/size binding forms where appropriate.

These are dynamic drawing/output bindings. They are not stored as the active
profile or used as a cache of user preference.

### Effective style

Metrics and actual output use the final, fully merged text style produced from:

```text
pane surface default
< content role
< state role
```

Display functions do not alter the medium's default text style, font mappings,
pane geometry, profile, catalog, or persistence.

This is particularly important for output-record replay: the semantic role
stack and its resolved render key are application state; the medium's
temporarily current state is not.

### Presentation preservation

Styling must remain inside or around existing presentation output without
replacing the presentations. The object/type linkage, nesting, pointer
highlighting, translators, and command input contexts remain CLIM-owned.

### Incremental redisplay

Expose both:

- a bundle render key; and
- role-stack render keys.

Styled update records include the key for the roles they actually use:

```text
existing object cache value
+ effective role-stack render key
```

A pane-surface change may use the bundle or surface key. A transcript-user
foreground change should not invalidate every agent record merely because the
profile revision increased.

Render keys are based on effective resolved output, not raw revision counters.
A no-op edit may increment a candidate revision without invalidating output.

## Pane construction and live activation

The existing plan contains a staging tension: extended fonts require a port,
while pane defaults are normally supplied during pane construction.

Before implementation, determine whether the pinned McCLIM frame manager and
port are reliably available before pane generation.

Recommended path if the probe passes:

1. select the target CLIM port and frame manager explicitly;
2. parse and validate the portable startup profile;
3. resolve the initial bundle for that port;
4. pass the immutable profile and bundle as frame initargs;
5. let pane forms read surface defaults from that bundle; and
6. construct the frame through ordinary `make-application-frame`.

The frame owns the state immediately after construction; no process-global
active bundle is introduced.

If the probe fails, do not mutate pane media during construction by private
means. Restrict the first milestone to portable logical pane defaults and defer
named construction-time fonts until a public staging path is proven.

### Axis capability classification

Every candidate delta is classified before activation:

- `:render-boundary-live` — safe through role keys and ordinary redisplay;
- `:pane-live` — requires a proven public/per-pane adapter;
- `:frame-recreation` — safe only by constructing a replacement frame;
- `:restart-required` — no accepted live path yet; or
- `:unsupported`.

A coordinated profile is atomic. If one changed component is restart-required,
the system must not activate only the colors and claim the theme changed. The
candidate may be previewed and explicitly saved for the next start, while the
complete old profile remains active.

## Typed error and recovery model

Genera exposes typed invalid-style conditions and interactive recovery. The
useful lesson is typed diagnosis, not its mutable “register this value now”
restarts.

Define conditions such as:

- `unknown-appearance-role`;
- `invalid-appearance-component`;
- `appearance-theme-cycle`;
- `appearance-role-cycle`;
- `missing-appearance-parent`;
- `unsupported-role-axis`;
- `ambiguous-font-family`;
- `ambiguous-font-face`;
- `font-unavailable`;
- `font-size-unavailable`;
- `relative-size-base-invalid`;
- `device-font-port-mismatch`;
- `appearance-contrast-warning`;
- `appearance-live-update-unsupported`; and
- `appearance-activation-failed`.

Conditions carry structured fields:

- candidate origin;
- role and axis;
- offending value;
- fallback/parent path;
- target port;
- available choices where bounded;
- whether the issue is fatal; and
- suggested UI repairs.

Configuration loading and package registration must not invoke restarts that
silently mutate the catalog. They catch these conditions at the transaction
boundary and leave published state unchanged.

The interactive editor may translate a condition into ordinary CLIM choices:

- Edit Value;
- Choose Available Font;
- Use Portable Fallback;
- Refresh Font Inventory;
- Discard Candidate; or
- Save for Restart, when valid but not live-safe.

The active profile is never repaired implicitly.

Unknown display-wire roles are a separate case: render with
`:default-text`, log once per role and catalog generation, and continue. An
unknown role in configuration remains an error.

## Revised role vocabulary

### Surface roles

- `:base`
- `:transcript-pane`
- `:info-pane`
- `:compose-pane`
- `:minibuffer-pane`
- `:help-pane`
- `:pointer-documentation`

### Content roles

- `:default-text`
- `:transcript-user`
- `:transcript-agent`
- `:transcript-tool`
- `:transcript-system`
- `:transcript-empty`
- `:system`
- `:error`
- `:tool-result`
- `:modeline`
- `:selector-title`
- `:selector-header`
- `:selector-entry`
- `:selector-separator`
- `:selector-footer`

### State roles

- `:selector-selection`
- `:disabled`

Possible future roles such as focus, warning, success, search match, or pending
must be added only when a first-party rendering boundary actually needs them.

Do not add roles merely to expose every visual property. Role names remain
semantic, while axes describe how they render.

## Configuration and persistence

### Data model

A revised version-1 shape should make the axes visible:

```lisp
(:clawmacs-appearance
 :version 1
 :theme :dark
 :overrides
 ((:transcript-user
   :foreground (:rgb 0.478 0.635 0.969))
  (:base
   :typography
   (:font-family "DejaVu Sans Mono"
    :font-face "Book"
    :size 14))
  (:selector-selection
   :typography (:face :bold)
   :decoration :selection-marker)))
```

Exact external syntax remains refinable, but the following semantics are
required:

- omitted property means inherit;
- omitted family, face, or size inherits independently;
- `nil` is rejected;
- `:none` is accepted only for supported absence-capable axes;
- exact enumerated family and face names are data strings;
- portable families/faces/sizes use a distinct tagged representation;
- relative sizes are only `:smaller` and `:larger`;
- literal device fonts are absent from version 1;
- inks are limited to standard ink tokens and opaque RGB;
- no CLIM object is serialized;
- no arbitrary Lisp is evaluated;
- no pattern arrays, ALUs, stipples, indexed maps, or executable palette forms
  are accepted.

The parser retains the existing requirements: one form, `*read-eval*` false,
bounded input and nesting, no trailing forms, known clauses only, and atomic
replacement on save.

### Startup sequence

Recommended startup ordering:

1. install built-in role and theme declarations;
2. parse `appearance.sexp` into an unresolved startup candidate;
3. load `init.lisp`, allowing declaration registration and startup overrides;
4. load configured packages and their declarations;
5. apply environment and command-line profile overrides;
6. validate the complete declaration catalog;
7. freeze the startup profile;
8. choose the frame's port;
9. resolve the frame bundle; and
10. construct the frame.

A mutable startup builder may exist temporarily. It is not the process-global
active appearance.

`--no-init` should skip executable Lisp initialization but should not silently
skip the data-only appearance file. If users need that, define a separate
appearance-specific flag.

One-shot prompt execution should not read or resolve GUI appearance unless it
actually constructs a frame.

### Defaults versus content

`appearance.sexp` persists display defaults. It does not alter or annotate:

- chat messages;
- session records;
- buffer contents;
- provider output;
- exported plain transcripts; or
- the existing bitmap-font editor's artifacts.

Existing sender/type/domain state remains the semantic input to role selection.

If rich styled text is ever added, its content markup requires a separate
design and persistence contract. It must not store resolved CLIM inks, port
font objects, or an active theme snapshot inside messages.

A rendered export may deliberately record its chosen profile as rendering
metadata, but that is different from modifying the underlying conversation.

## CLIM-native editor

Initial commands:

- Switch Appearance Theme
- Describe Current Appearance
- Customize Appearance
- Apply Staged Appearance
- Save Appearance
- Revert Staged Appearance
- Reload Appearance File
- Refresh Font Inventory

Retain `C-h F` and `C-c F`; do not use `C-c t`.

### Interaction model

The editor operates on a staged immutable candidate.

- **Apply** validates and transactionally activates the entire candidate.
- **Cancel/Revert** discards it.
- **Save** explicitly writes the validated candidate; if it is not active
  because it requires restart, say so prominently.
- **Describe** distinguishes active, staged, and persisted profiles.

Use presentation types for:

- theme definitions;
- appearance roles;
- role kinds;
- font-family choices;
- font-face choices;
- font-size choices;
- portable typography choices;
- colors;
- activation capability; and
- validation diagnostics.

Font presentations are tied to the active port. Selecting a family filters the
face choices; selecting a non-scalable face filters the size choices.
Completion must not offer combinations known to be unmappable on that port.

A font choice presentation contains the live McCLIM enumeration object only
during the interaction. Committing the candidate converts it to the portable
descriptor.

### Candidate preview

Borrow the useful transaction shape—not the historical implementation—from
the Genera Color Editor:

- render samples of all core roles inside an application pane;
- render the staged candidate through ordinary CLIM drawing and output records;
- leave the main frame's active panes untouched;
- provide Apply and Cancel through commands or `accepting-values`;
- show semantic descriptor, effective role stack, resolved font choice,
  fallback path, and contrast result; and
- never take over an indexed color map, graphics context, or repaint loop.

The preview is sufficient for unsafe typography candidates: users can inspect
them without partially activating the main frame.

## Package and Safe Reload contract

Packages may register:

- owner-qualified role definitions;
- package-owned theme definitions; and
- portable default overlays for their own roles.

A package may not:

- mutate a frame profile or bundle;
- install port text-style mappings;
- register executable ink resolver functions;
- reopen a built-in theme in place;
- attach appearance to durable buffers/messages;
- introduce native raster drawing state; or
- publish an invalid declaration graph.

Extension roles must have a resolvable fallback or default so every existing
theme need not enumerate every future package role.

Catalog update sequence:

1. construct a candidate catalog;
2. validate role and theme graphs;
3. identify affected frames;
4. construct replacement profiles for missing active themes or roles;
5. resolve all affected bundles;
6. classify every frame transition;
7. apply all safe frame transitions;
8. publish the candidate catalog only if all succeed; and
9. roll back every frame/catalog mutation on failure.

If unloading a package removes an active theme and the fallback to `:classic`
cannot be activated safely, the unload must be refused or deferred until frame
recreation. It must not publish a dangling catalog and hope the next redisplay
recovers.

Safe Reload:

- does not reread `appearance.sexp`;
- does not rerun `init.lisp`;
- does not save staged edits;
- does reconcile replacement declarations transactionally;
- preserves frame-local active choices when still valid; and
- retains the old catalog and bundles if reconciliation fails.

## Accessibility

For the initial built-in themes:

- require at least 4.5:1 contrast for every resolved text/surface pair;
- calculate contrast after surface, content, and state roles are composed;
- do not use the large-text 3:1 exception in version 1;
- retain a non-color selection marker;
- ensure selected and disabled states remain distinguishable without hue;
- validate dark-theme empty, error, system, selector, modeline, and
  pointer-documentation combinations explicitly.

For user/package themes:

- warn by default;
- show the exact resolved role stack and surface behind the warning;
- offer an optional strict mode; and
- never silently alter the user's color to pass validation.

Patterns and opacity are absent from version 1, so contrast is computed over
opaque RGB or standard solid inks only.

## Explicit non-goals and rejected Genera-derived patterns

The implementation must not copy:

- compact 256-entry character-style indices;
- styled fat characters;
- character-, word-, or region-level style editing;
- typein-style movement heuristics;
- the Zmacs quick-dispatch key tree;
- mutable global registries of accepted family/face/size symbols;
- Genera raster font names as a portable typography vocabulary;
- device-font backtranslation as an ordinary workflow;
- screen/printer font maps owned by Clawmacs;
- document pathname attributes or mail headers for appearance preferences;
- native ALUs;
- opaque/nonopaque raster zero-bit behavior;
- Genera stipple names or arrays;
- indexed color-map allocation or takeover;
- RGB/IHS/YIQ/CMY editor models;
- executable serialized palette forms;
- Dynamic Windows-to-CLIM conversion rules;
- direct sheet/medium manipulation;
- custom output-record mutation;
- a replacement redisplay engine; or
- live line-height/baseline mutation during output.

These mechanisms solved different historical problems. Importing their names
without their device and raster semantics would create a misleading and
nonportable API.

## Revised future atomic commit sequence

Every commit includes focused deterministic tests and passes the repository's
container build for its scope.

1. `feat(appearance): Define orthogonal style specifications`
   - immutable typography, ink, surface, decoration, role, theme, and condition
     types;
   - no UI integration.

2. `feat(appearance): Resolve semantic role cascades`
   - source layers, role fallback, role stacks, relative logical sizes, cycle
     detection, and provenance traces;
   - `:classic` golden definitions.

3. `feat(appearance): Build frame-local port bundles`
   - frame profile/bundle slots, port identity, font-inventory generation,
     structural render keys, and fake-port resolver tests.

4. `feat(ui): Route output through appearance roles`
   - transcript, tool summary, generic entries, empty output, info/modeline,
     and minibuffer selection;
   - preserve presentations;
   - role-specific incremental cache keys;
   - visually inert under `:classic`.

5. `feat(ui): Apply appearance during pane construction`
   - only after the port-before-pane probe;
   - surface defaults and safe logical typography;
   - no live switching yet.

6. `feat(ui): Activate color profiles transactionally`
   - complete candidate validation;
   - render-boundary and proven pane-color updates;
   - rollback and no partial theme activation.

7. `feat(appearance): Add accessible dark profile`
   - color-only relative to `:classic` typography;
   - resolved contrast gates.

8. `feat(config): Persist appearance profiles safely`
   - versioned parser/writer, startup builder, explicit save, precedence,
     invalid-file fallback, and prompt-mode isolation.

9. `feat(appearance): Enumerate port font choices`
   - McCLIM family/face/size presentations, explicit cache refresh, ambiguity
     handling, fixed-width checks, and predictable fallback.

10. `feat(ui): Add the CLIM appearance editor`
    - staged candidate, preview, Apply/Cancel, Describe, Save, completion,
      diagnostics, and compatibility aliases.

11. `feat(packages): Register owned appearance declarations`
    - namespaced roles, catalog transactions, Safe Reload reconciliation, and
      package-unload refusal/rollback.

12. `test(gui): Prove appearance lifecycle behavior`
    - multi-frame isolation, switching, persistence, restart-required
      typography, resize/expose, menus, and clean teardown.

Optional commits remain outside the baseline:

- guarded literal device-font descriptors;
- live non-Drei typography;
- live Drei typography;
- filled selection decoration;
- pointer-documentation styling;
- general persisted CLIM design recipes; and
- frame recreation for unsafe live profile changes.

## Verification gates

### Deterministic FiveAM coverage

- immutability of every declaration/profile/bundle type;
- unspecified versus explicit `:none`;
- axis applicability by role kind;
- theme-parent and role-fallback cycle detection;
- exact layer precedence;
- role fallback within each layer;
- surface/content/state role-stack order;
- family/face/size independent merge;
- explicit compound face behavior;
- relative-size stepping and endpoint clamping;
- rejection of relative size over numeric/device-font bases;
- complete current wire-face mapping, including `:selector-selected`;
- minibuffer selection using the same state role;
- all first-party roles resolving under `:classic`;
- golden current RGB values and title/selection emphasis;
- runtime unknown-role fallback versus config unknown-role rejection;
- structural render-key stability on no-op edits;
- role-local cache invalidation;
- port identity and font-inventory generation separation;
- duplicate font-family/face ambiguity;
- scalable and fixed-size validation;
- fixed-width requirements for compose/minibuffer scope;
- typed condition payloads;
- candidate retention after validation failure;
- complete transaction rollback;
- config parser bounds and round trips;
- active/staged/persisted state distinction;
- startup precedence;
- no GUI appearance loading in prompt-only mode;
- package catalog rollback; and
- accessibility over resolved role stacks.

### Pinned McCLIM probes

1. **Port-before-pane construction**
   - establish the earliest public point where the target port/frame manager is
     available;
   - prove initial bundle resolution can precede pane initargs.

2. **Pane initialization**
   - verify foreground, background, and default text-style initargs for each
     pane class used by Clawmacs.

3. **Live pane color**
   - verify per-pane `reinitialize-instance`, medium synchronization if public,
     redisplay, expose, and rollback.

4. **Font enumeration round trip**
   - enumerate family and face objects;
   - convert through `font-face-text-style`;
   - map and measure on the same port;
   - cover duplicate display names, bitmap sizes, scalable sizes, and explicit
     cache invalidation.

5. **No port-global mutation**
   - prove ordinary profile resolution does not install or alter shared
     text-style mappings.

6. **Relative sizes**
   - verify portable logical-size mapping, metrics, and output across the
     pinned CLX port;
   - reject undocumented numeric-relative behavior.

7. **Output-record replay**
   - change role keys, resize, expose, and confirm presentations and style
     replay remain correct.

8. **Non-Drei typography**
   - determine whether changing an application pane's effective style requires
     layout or space negotiation.

9. **`accepting-values` integration**
   - verify nested use with the current ESA/Drei command loop, completion,
     abort, and focus return.

10. **Selection decoration**
    - test only public CLIM mechanisms for filled backgrounds or borders;
    - preserve nested presentations, geometry, highlighting, and incremental
      output.

11. **Literal device font**
    - only for the deferred escape hatch;
    - establish name validation, wrong-port behavior, metrics, replay, and
      serialization fallback.

12. **Package reconciliation**
    - remove an active package theme while multiple frames use different
      profiles;
    - prove all-or-nothing catalog/frame rollback.

13. **Pointer documentation**
    - determine whether the generated stream can receive supported appearance
      defaults without modifying layout.

14. **Drei typography**
    - retain the existing 100/101/112-character single-line checks;
    - use roughly 32 KiB multiline content for the ordinary stability fixture;
    - preserve point, mark, undo, focus, and draft;
    - repeat activation, resize, expose, and teardown;
    - require zero repaint-time compose geometry mutation.

A failed optional probe narrows scope. It is not permission to add renderer
internals.

### GUI acceptance

- cold Guix build and full FiveAM suite;
- private-Xvfb appearance scenario;
- two frames with different active profiles and no cache leakage;
- semantic snapshots containing active theme, bundle key, and font-inventory
  generation;
- color-only theme switch and exact rollback failure case;
- staged restart-required typography remaining visibly nonactive;
- persistence through real application restart;
- font-inventory refresh with deterministic fixture fonts;
- selector marker plus appearance state;
- actual expose and resize events;
- menu, keybinding, completion, and `accepting-values` stress;
- existing compose, switch-buffer, pointer-tracking, and Safe Reload suites;
- no runtime-failure signatures;
- empty stderr; and
- empty owned process group.

Pixel checks remain secondary. Prefer semantic snapshots and focused tolerant
samples at known solid surface locations.

## Decisions to refine before implementation

| Decision | Recommended default |
|---|---|
| Relationship to original plan | Companion amendments are normative on conflicts; consolidate later without editing either during this planning step |
| Internal appearance model | Orthogonal typography, foreground, surface/background, and decoration axes |
| Theme model | Coordinated role overlays across those axes |
| Role composition | Surface, then content, then state |
| Existing `:selector-selected` | Adapter input mapping to `:selector-entry` plus `:selector-selection` |
| Default theme | `:classic` |
| First additional theme | Typography-neutral `:dark` |
| Frame scope | Per frame |
| Font resolution | Per target port through public enumeration |
| Font invalidation | Explicit Refresh Font Inventory plus frame-local generation |
| Relative sizes | Portable CLIM ladder only |
| Numeric relative scaling | Reject initially |
| Literal device font | Deferred advanced feature with declared fallback |
| Font mapping mutation | Never from ordinary profile activation |
| Compose/minibuffer font | Fixed-width and construction/restart-only initially |
| Live activation | Entire candidate or nothing |
| Unsafe candidate | Preview and optionally save for next start |
| Selection indication | Existing marker plus state-role typography/ink |
| Filled selection background | Deferred |
| Persisted inks | Opaque RGB and standard tokens only |
| Patterns, stipples, opacity | Out of version 1 |
| Built-in contrast | 4.5:1 for all text |
| User-theme contrast | Warning by default; optional strict mode |
| Package theme removal | Transactional fallback or refuse unload |
| Styled content | Explicit non-goal |
| Safe Reload | Reconcile declarations only; do not reread or save user config |
| Pointer documentation | Leave unchanged until its probe |
| Renderer internals | Prohibited |

## Planning conclusion

Genera's strongest lesson is not a particular theme, font table, or editor. It
is that semantic typography, concrete device fonts, native drawing state, and
CLIM designs are different layers with different lifetimes.

Clawmacs should preserve that separation while improving on Genera's
distributed customization model:

- semantic roles remain modular;
- themes coordinate them centrally;
- active state belongs to each frame;
- font resolution belongs to each target port;
- output uses ordinary CLIM dynamic state and records;
- persistence stores preferences, not resolved rendering objects or styled
  conversation content; and
- activation remains atomic and conservative around McCLIM and Drei.

No source, test, configuration, repository artifact, commit, or runtime state
is changed by this companion plan.
