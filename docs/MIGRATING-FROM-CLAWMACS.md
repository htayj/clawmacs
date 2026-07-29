# Migrating from Clawmacs to RPLACA

This guide covers the pre-alpha product rename from Clawmacs to RPLACA. The
compatibility boundary is deliberately narrow: it preserves inert data and
complete session history without silently executing old configuration.

Read this guide before moving or deleting anything. Keep the old directories
until RPLACA has started, the expected sessions and settings are visible, and a
new canonical write has been verified.

## Canonical identity

RPLACA does not provide aliases for the old program identity.

| Surface | Canonical identity |
|---|---|
| Product and window title | `RPLACA` |
| Future repository URL | `https://github.com/htayj/rplaca` |
| ASDF systems | `rplaca`, `rplaca/tests`, `rplaca/gui-e2e` |
| Common Lisp packages | `RPLACA`, `RPLACA/MATCHING-CORE`, `RPLACA/TESTS` |
| User configuration package prefix | `rplaca:` |
| User configuration root | `~/.rplaca.d/` |
| Global text/data root | `~/.config/rplaca/` |
| Project registry | `~/.rplaca.projects.d/` |
| State root | `${XDG_STATE_HOME:-~/.local/state}/rplaca/` |
| Native editable bitmap font | `.rplacafont`, tag `:rplacafont` |
| Native font API | `write-rplacafont-file`, `read-rplacafont-file` |

There are no old ASDF system, package, symbol, command, environment-variable,
or native-write aliases. Update Lisp forms and scripts to use the identities
above. Compatibility code recognizes only the specific old paths and inert
data tags described below.

The canonical operational environment variables are:

- `RPLACA_QUICKLISP_SETUP`, `RPLACA_ULTRALISP_SETUP`
- `RPLACA_RUN_CLEAN_BUILD`, `RPLACA_PROMPT_CLEAN_BUILD`
- `RPLACA_SESSION_NAME`, `RPLACA_PROMPT_PROJECT_ROOT`
- `RPLACA_SSL_LIB`, `RPLACA_FONT_PATH`, `RPLACA_TRUETYPE_FONT_PATH`
- `RPLACA_SBCL_DYNAMIC_SPACE_SIZE`
- `RPLACA_APPEARANCE_THEME`
- `RPLACA_CRASH_REPORT_DIR`
- `RPLACA_DEBUG`, `RPLACA_DEBUG_LOG`

Provider environment variables retain their provider names:
`OPENAI_API_KEY`, `ZAI_CODING_MAX_API_KEY`, and `OPENROUTER_API_KEY`. OpenAI
Codex shared credentials remain at `~/.codex/auth.json`. No `CLAWMACS_*`
variable is consulted.

## The selection contract

Every built-in migration-aware resolver follows the same rule:

1. If the canonical path exists, it wins.
2. If both paths exist, RPLACA uses only the canonical path and warns; it never
   merges the two stores.
3. If only an inert legacy path exists, RPLACA may read it as a read-only
   fallback and warns once per path in the process.
4. Every write targets the canonical path, including writes made while an
   inert legacy fallback supplies the current value.
5. If the legacy path can execute code, commands, packages, tools, or
   instructions with executable resources, RPLACA detects it, warns, and
   refuses to load it automatically.
6. Filesystem probe errors propagate. An unreadable or malformed canonical
   path is not permission to fall back.

Custom paths explicitly supplied through Lisp are used as supplied. The
canonical-versus-legacy selection applies to the built-in defaults.

## Path and behavior matrix

| Subsystem | Canonical path | Legacy path | Behavior when only legacy exists |
|---|---|---|---|
| Static provider credentials | `~/.config/rplaca/{zai-api-key,openrouter-api-key,openai-codex-token}` | `~/.config/clawmacs/` with the same filenames | Inert, read-only fallback; never copied or refreshed in place |
| Personality prompt | `~/.config/rplaca/system-prompt.txt` | `~/.config/clawmacs/system-prompt.txt` | Read-only fallback |
| Agent defaults | `~/.config/rplaca/agent-defaults.json` | `~/.config/clawmacs/agent-defaults.json` | Inert, read-only fallback |
| Global boot instructions | `~/.config/rplaca/{AGENTS,SOUL,USER,IDENTITY,TOOLS}.md` | Same names under `~/.config/clawmacs/` | One whole relevant directory is selected; no merge |
| Sessions | `~/.config/rplaca/sessions/` | `~/.config/clawmacs/sessions/` | Read-only discovery, then complete atomic materialization on explicit continuation |
| User init | `~/.rplaca.d/init.lisp` | `~/.clawmacs.d/init.lisp` | Executable; detected and refused |
| Appearance | `~/.rplaca.d/appearance.sexp`, tag `:rplaca-appearance` | `~/.clawmacs.d/appearance.sexp`, tag `:clawmacs-appearance` | Inert bounded data; read-only fallback |
| Package enablement | `~/.rplaca.d/packages.json` | `~/.clawmacs.d/packages.json` | Behavioral; detected and refused |
| Installed packages | `~/.rplaca.d/packages/` | `~/.clawmacs.d/packages/` | Executable; detected and refused |
| Skill selection | `~/.rplaca.d/skills.json` | `~/.clawmacs.d/skills.json` | Behavioral; detected and refused |
| User/system skills | `~/.rplaca.d/skills/`, including `.system/` | Same roots under `~/.clawmacs.d/` | Executable resources; detected and refused |
| MCP servers | `~/.rplaca.d/mcp-servers.json` | `~/.clawmacs.d/mcp-servers.json` | Behavioral; detected and refused |
| Prompt templates | `~/.rplaca.d/prompts/` | `~/.clawmacs.d/prompts/` | Read-only fallback; one directory only |
| Modelaria global metadata | `~/.rplaca.d/modelaria.json` | `~/.clawmacs.d/modelaria.json` | Inert routing metadata; read-only fallback |
| Project registry | `~/.rplaca.projects.d/` | `~/.clawmacs.projects.d/` | Behavioral registry; detected and refused |
| Project package installs | `<project>/.rplaca.d/packages/` | `<project>/.clawmacs.d/packages/` | Executable; detected and refused |
| Project prompt templates | `<project>/.rplaca/prompts/` | `<project>/.clawmacs/prompts/` | Read-only fallback; one directory only |
| Project Modelaria metadata | `<project>/.rplaca-modelaria.json` | `<project>/.clawmacs-modelaria.json` | Inert routing metadata; read-only fallback |
| Crash reports | `${XDG_STATE_HOME:-~/.local/state}/rplaca/crash-reports/` | Equivalent `clawmacs/crash-reports/` | Archival only; never imported or written |
| Editable bitmap fonts | `*.rplacafont`, tag `:rplacafont` | `*.clawfont`, tag `:clawfont` | Import only; canonical saves never overwrite it |

### Credentials

A legacy static token produces a read-only fallback warning. RPLACA does not
copy credentials, change their permissions, refresh them in place, or delete
them. Do not bulk-copy credential files into the new directory. Re-enter each
credential through its provider environment variable, the normal login flow,
or the canonical `save-provider-token` API, then confirm the canonical file is
mode `0600`.

ChatGPT-backed OpenAI Codex authentication uses the shared
`~/.codex/auth.json` store and is not part of this rename. The optional raw
bearer override is `~/.config/rplaca/openai-codex-token`.

### Prompts, defaults, Modelaria, and appearance

Personality prompts, agent-default JSON, prompt templates, Modelaria JSON, and
appearance forms are data-only migration fallbacks. They still influence agent
behavior or routing, so review them before making a canonical copy.

Modelaria is classified as inert because its files contain normalized role,
role-set, model, and service-tier metadata; they do not load Lisp or launch
commands. The same classification applies to both
`~/.rplaca.d/modelaria.json` and `<project>/.rplaca-modelaria.json`.

Global boot files are selected as one directory based on the presence of the
five recognized filenames. If canonical instructions exist, no legacy boot
file is added. Project and global prompt-template roots similarly select one
root and never merge.

The appearance reader accepts the old bounded data tag for import
compatibility. Saving through RPLACA writes `:rplaca-appearance` to the
canonical path.

### Executable and behavioral configuration

The following old state is never loaded automatically:

- `init.lisp`
- package enablement and installed package directories
- skill selection, user skills, and system skills
- MCP server definitions
- project registry manifests
- project-local installed packages

Migrate these surfaces manually:

1. Work from a private backup, not the live old tree.
2. Inspect every Lisp form, executable, script, package entrypoint, MCP command,
   URL header, project root, system name, package name, and symbol reference.
3. Replace old ASDF/package/symbol/path identities with the canonical RPLACA
   identities. Do not add compatibility aliases to make an old init load.
4. Place reviewed init forms in `~/.rplaca.d/init.lisp` with mode `0600`.
5. Re-enable packages through RPLACA's package selector or
   `set-package-enablement-scope`; reinstall package sources into the canonical
   global or project package root.
6. Re-register reviewed skill roots or copy reviewed skills into the canonical
   roots. Treat skill scripts and assets as executable/trusted input.
7. Re-enter each MCP server in `~/.rplaca.d/mcp-servers.json` only after
   reviewing its transport command, arguments, URL, headers, and credentials.
8. Recreate project manifests under `~/.rplaca.projects.d/`, updating roots,
   ASDF systems, packages, and hooks before loading them.

Do not rename an old directory in place: that makes rollback difficult and can
turn unreviewed executable state into canonical state.

## Session migration

Listing saved sessions discovers the legacy tree read-only. Continuing a legacy
session explicitly with `C-x C-r`, `load-session-command`, or `load-session`
causes RPLACA to materialize the complete legacy session tree before it mutates
any session.

The materialization protocol is:

1. Acquire the in-process migration lock.
2. Acquire the blocking POSIX record lock at
   `~/.config/rplaca/.rplaca-session-migration.lock`. The kernel releases it
   if a publisher crashes, including across separate containers and PID
   namespaces.
3. Remove abandoned sibling `.sessions-migration-*` staging trees and recover
   an incomplete old publication while still holding the lock.
4. Copy the entire legacy tree to a sibling staging directory.
5. Verify relative names, exact file bytes, file modes, directory modes, file
   sizes, and content digests. Existing session IDs and transcript bytes are
   preserved.
6. Write a data-only `.rplaca-session-migration-complete` inventory marker in
   the staged tree.
7. Publish with one rename from the complete staging tree to canonical
   `sessions/`.

No partially copied canonical tree is used. A later process removes a crashed
publisher's staging tree and retries. Once canonical `sessions/` exists and has
a valid completion marker, it wins and the legacy tree is not merged into it.

## Native bitmap fonts

Open old `.clawfont` files with `read-font-file` or
`read-rplacafont-file`. The reader accepts the old `:clawfont` data tag. The
editor does not reuse an old filename as its save target; Save prompts for a
`.rplacafont` path and writes `:rplacafont`.

There are no `write-clawfont-file` or `read-clawfont-file` public aliases.
Use `write-rplacafont-file` and `read-rplacafont-file`. The canonical writer
rejects a `.clawfont` destination.

## Guix container visibility

The Guix launcher shares canonical state read-write. If the corresponding old
directories exist on the host, it exposes these at their same-shaped container
paths read-only:

- `~/.config/clawmacs`
- `~/.clawmacs.d`
- `~/.clawmacs.projects.d`

This lets inert readers and session discovery see old data while preserving the
executable-state refusal boundary. The exposes are not a writable migration
mechanism and do not merge with canonical shares.

## Backup, migration, and rollback

Stop RPLACA before manipulating state. Create a private backup on trusted
storage; the backup may contain credentials and transcripts:

```sh
umask 077
backup="$HOME/rplaca-migration-backup-$(date +%Y%m%dT%H%M%S)"
install -d -m 700 "$backup"
for path in \
  "$HOME/.config/clawmacs" \
  "$HOME/.clawmacs.d" \
  "$HOME/.clawmacs.projects.d"; do
  if [ -e "$path" ]; then
    cp -a -- "$path" "$backup/"
  fi
done
```

Do not use that backup command to populate canonical directories. Migrate inert
files one at a time after review, re-enter credentials, and recreate executable
configuration as described above. Start RPLACA and verify a canonical write for
each migrated subsystem before removing anything.

For rollback, stop RPLACA and move the new canonical path aside rather than
deleting it. Restore a reviewed canonical backup if one exists. The untouched
legacy inert data remains eligible for read-only fallback; an untouched legacy
session tree can be materialized again after the canonical session directory is
moved aside. Executable legacy state will continue to be refused by current
RPLACA and requires the old application version if you truly need to run it.

## Verification

Run the policy and deterministic migration checks from the repository root:

```sh
./scripts/check-legacy-name-allowlist.sh
./scripts/test-legacy-name-allowlist.sh
./scripts/guix-container.sh --preflight-only --mode run -- true
./scripts/test-guix-container.sh
./scripts/test-session-migration-containers.sh
```

Run the full Lisp suite:

```sh
./scripts/guix-container.sh --mode run -- sh -lc \
  'sbcl --noinform --load "$RPLACA_QUICKLISP_SETUP" \
   --eval "(push (truename \".\") asdf:*central-registry*)" \
   --eval "(ql:quickload :rplaca/tests)" \
   --eval "(fiveam:run! (quote rplaca/tests::rplaca-suite))" \
   --eval "(quit)"'
```

Inspect the resulting state without printing credential contents:

```sh
find "$HOME/.config/rplaca" "$HOME/.rplaca.d" \
  "$HOME/.rplaca.projects.d" -maxdepth 3 -printf '%M %p\n' 2>/dev/null
```

The residual-name gate is authoritative for repository text. Its allowlist
contains exact paths, allowed identity kinds, categories, and rationales. New
current-facing uses, stale URLs, broad wildcard exclusions, and undocumented
legacy identifiers fail the gate.
