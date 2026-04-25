 (:name "packrat"
 :description "External package lifecycle and resource loading"
 :entrypoint "package.lisp"
 :system-prompt-section "## Packrat package lifecycle

- Use Packrat tools for installing, removing, updating, listing, and
  inspecting Clawmacs packages.
- Installs can target the global package root or a project-local package
  root. Project-local packages are active when a buffer belongs to that
  project.
- Package install records can restrict loaded resources to selected resource
  types such as tools, commands, docs, prompt sections, prompt templates,
  slash commands, buffer types, hooks, advice, and themes.
- For human workflows, use the `M-x packrat-*` commands. For agent workflows,
  prefer the `packrat_*` tools and supply explicit `src_type`, `repo`, `ref`,
  `scope`, `project`, and `resource_types` fields when needed.
- `packrat_doctor` reports broken installs, missing manifests, and stale or
  missing sources. `packrat_status` summarizes installed packages and their
  active scopes.")
