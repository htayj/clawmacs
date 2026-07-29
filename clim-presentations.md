# CLIM Presentation Backlog

This is a prioritized backlog for making RPLACA more presentation-driven in
the Lisp-machine sense: semantic objects on screen, direct mouse actions,
inspection everywhere, and fewer command-only workflows.

The immediate rule for each item is:

- prefer native CLIM presentations and translators
- prefer visual operations on semantic objects over ad hoc typed commands
- keep object identity stable enough to support inspect/describe/follow

Historical direction:

- Genera / Lisp Machine inspectors and operation menus
- package and system browsers with direct object actions
- Peek / status-style dashboards for active state
- Zmacs-style semantic editing affordances instead of raw coordinate work
- graph and browser views for relationships, not just flat lists

## Highest Priority

- [x] Package dashboard buffer with package presentations
  - visual list of installed packages, scopes, source, resources, and health
  - select toggles enablement in the originating chat buffer context
  - describe shows full package docs
  - later: update/remove/resource-policy actions

- [x] Organa TODO action presentations
  - clickable TODOs with direct status changes
  - dependency presentations that jump to linked TODOs
  - later: move/reorder and dependency editing via visual operations

- [x] Transcript message command presentations
  - richer actions on chat messages, tool calls, and tool results
  - inspect metadata, raw provider payload, entry ids, and branch ancestry
  - fork session directly from a message/tree point

- [ ] Session tree browser buffer
  - dedicated tree/outline buffer instead of selector-only interaction
  - visible branch structure with direct navigate/fork/label actions

## High Value Next

- [ ] Slop symbol browser buffer
  - present definitions, callers, callees, references, and mentions as objects
  - direct jump/follow across source analysis results
  - later: rename preview and call-flow navigation

- [ ] Git object presentations
  - commit, file, and hunk presentations
  - direct show/diff/stage/unstage/open actions
  - later: branch graph and patch-browser style views

- [ ] Artifact shelf buffer
  - present artifacts as attachable/openable objects
  - image/file previews, insertion, and reattachment actions

- [ ] Pipeline graph buffer
  - stage nodes, edges, failure states, retries, and history
  - later: editable routing and deterministic pipeline graph authoring

- [ ] Subagent roster / process browser
  - active subagents, state, logs, buffers, and lineage
  - inspired by process browsers and activity monitors

## Structural Editing / Nodal Work

- [ ] Sexed structural edit presentations
  - present forms, definitions, and editable structural units
  - direct raise/slurp/barf/wrap/select actions from the buffer

- [ ] Listener result/object presentations
  - inspect returned Lisp values directly from listener output
  - follow objects into Clouseau or domain-specific inspectors

- [ ] Dependency / relationship graph views
  - Organa dependency graphs
  - package dependency graphs
  - session branch graphs
  - pipeline routing graphs

## Debugging / Introspection

- [ ] Presentation inspector buffer
  - show what presentations exist under the pointer / in a pane
  - object type, translators, pointer docs, and source object summaries

- [ ] Pane and command activity monitor
  - current panes, commands, pending streams, approvals, and polling state
  - inspired by Peek and system status tools

- [ ] Tool invocation browser
  - visual history of tool calls and results
  - rerender by tool, open artifacts, inspect raw JSON, compare repeated calls

## Later / Research

- [ ] File and directory browser package
  - Lisp-machine style pathname and directory presentations
  - direct open, insert, inspect, and project actions

- [ ] Manual cross-reference enrichment
  - present command, variable, and file references in help/info buffers
  - jump directly into source, command docs, or package docs

- [ ] Operation menus over common semantic objects
  - messages, sessions, packages, TODOs, symbols, files, commits, and panes
  - unify discoverability of available presentation actions
