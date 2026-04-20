# Clawmacs E2E Feature Inventory

This inventory maps the current Clawmacs feature surface to McCLIM e2e
coverage in `test-mcclim-e2e.py` and shared scenarios in
`scripts/e2e-scenarios.py`.

## Core Application

- Startup/rendering: initial modeline, who line, prompt, pane sizing, redraw.
- Message flow: single-turn send, multi-turn send, async agent replies,
  streaming replies, stream polling, Escape cancellation.
- Input editing: character movement, word movement, beginning/end of line,
  beginning/end of buffer, delete/backspace, kill/yank/yank-pop, kill region,
  copy region, mark, exchange point and mark, multiline input, tab insertion.
- Scrolling: PageUp/PageDown, Meta/C-v scroll, mouse wheel history scrolling.
- Buffers: chat buffers, scratch buffers, file buffers, help buffers,
  customize buffers, package-provided organa buffers.
- Buffer operations: create, switch, kill, persist, list, buffer selector.
- Projects/files: select project, open project file, edit file buffer, save,
  create file, search project.
- Sessions: save session, load session, tree selector, fork from tree entry,
  transcript persistence.
- Windows: split below, split right, other-window, delete-window,
  delete-other-windows, pointer selection.
- Commands/minibuffer: M-x, prompt readers, command completion, abort.
- Help/customize: describe bindings, function, variable, type, installed
  package; customize drawing styles/faces.
- McCLIM debugging: clim-debugger hook status, Clouseau/Listener availability,
  runtime frame/pane/window snapshots.
- Models/agents: select agent, select model, select think level, provider/model
  modeline display.
- Display toggles: tool results, reasoning output, metadata output, debug mode.
- Skills: skill list/completion, completion click, completion Escape dismiss.
- Mouse support: input point placement, selector row click, completion click,
  presentation dispatch.
- Images: Markdown image embedding in agent messages.
- Self-observation: render JSON and screenshot pixel checks in the harness.

## Built-In Tools

- `lisp_eval`: always available and reserved.

## Packages

- `lispi`: `read`, `find`, `grep`, `write`, `edit`.
- `sexed`: `sexed_text_diagnostics`, `sexed_text_outline`,
  `sexed_text_form_text`, `sexed_text_edit`, `sexed_text_edits`,
  `sexed_file_read`, `sexed_file_write`, `sexed_file_outline`,
  `sexed_file_form_text`, `sexed_file_edit`, `sexed_file_edits`,
  `sexed_project_read`, `sexed_project_write`, `sexed_project_outline`,
  `sexed_project_form_text`, `sexed_project_edit`,
  `sexed_project_edits`.
- `slop`: `slop_list_projects`, `slop_current_project`,
  `slop_project_symbols`, `slop_symbol_at`, `slop_find_definitions`,
  `slop_find_definitions_batch`, `slop_find_references`,
  `slop_find_callers`, `slop_find_callees`, `slop_trace_calls`,
  `slop_find_mentions`, `slop_definition_context`,
  `slop_find_variable_uses`, `slop_rename_variable`.
- `git`: `git_status`, `git_log`, `git_diff`, `git_show`, `git_branch`,
  `git_remote`, `git_add`, `git_commit`, `git_push`.
- `netcons`: `netcons_run`, `netcons_search`, `netcons_open`,
  `netcons_find`.
- `subagent`: `subagent_run`, `subagent_start`, `subagent_status`,
  `subagent_wait`, `subagent_cancel`.
- `pipelines`: package prompt/docs plus `set-buffer-pipeline` and
  `clear-buffer-pipeline` commands; runtime pipeline execution is covered by
  unit tests and package e2e help/command discovery.
- `speculum`: `speculum_screenshot`, `speculum_window_state`,
  `speculum_inspect`.
- `organa`: org-mode TODO buffer type and views, plus
  `organa_todo_overview`, `organa_todo_add`, `organa_todo_set_status`,
  `organa_todo_move`, `organa_todo_link_dependency`; user commands for
  opening org files, adding todos, setting status, moving, linking, unlinking,
  and cycling views.

## Offline E2E Coverage Added

- `71_tools_lispi_package_enable_and_eval`
- `71_tools_sexed_package_structural_read_write`
- `71_tools_slop_package_lookup_and_trace`
- `71_tools_git_package_status_log_and_mutations`
- `71_tools_netcons_package_open_find_offline`
- `71_tools_speculum_package_self_visibility`
- `71_tools_organa_package_todo_management`
- `69-mcclim-debug-status-and-snapshot`

## Known Gaps

- `netcons` only has a mocked local HTTP contract in offline e2e.
- `git` coverage exercises a throwaway repo, not push to a remote.
- `slop` coverage uses offline symbol lookup traces against local fixtures,
  not a real language server backend.
- `speculum` coverage validates screenshot/self-observation paths, not the
  full interactive image viewer surface.

## Coverage Map

- Runtime inventory: `70-feature-inventory-runtime-contract`.
- Core UI/rendering/windowing: `53` through `64`, plus `01` through `21`.
- Editor/project/session/toggles: `65` through `68`, `38` through `47`,
  `51`, `52`.
- McCLIM debugging: `69-mcclim-debug-status-and-snapshot`.
- Readline/input editing: `22` through `37`.
- Package tools: `71_tools_lispi_*`, `71_tools_sexed_*`,
  `71_tools_slop_*`, `71_tools_git_*`, `71_tools_netcons_*`,
  `71_tools_speculum_*`, `71_tools_organa_*`.
- Package manager/orchestration UI: `72_pkg_*`.
- Package buffer-type discovery: `72_pkg_organa_buffer_type_is_registered_and_discoverable`.
- Subagent/pipeline runtime: `72_pkg_subagent_and_pipeline_runtime_contract`.
- Live providers: `online-zai` and `online-openai-codex` groups.
