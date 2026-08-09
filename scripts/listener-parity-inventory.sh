#!/usr/bin/env bash
set -euo pipefail

legacy_regex='rplaca-chat-frame|run-rplaca-chat-frame|define-rplaca-chat-frame-command|com-chat-|rplaca-chat|chat-frame-|chat-compose-|chat-interaction-state|rplaca-transcript-pane|mcclim-interface|compose-pane|:compose-pane|listener-buffer-p|listener-state|make-listener-buffer|submit-listener-input|buffer-kind.*:listener|[^[:alnum:]]:listener[^[:alnum:]]|\(:file "listener"\)'
capabilities_regex='keymap-bind|defcommand|define-rplaca-chat-frame-command|clim:define-command|\*appearance-package-|\*package-appearance-|:compose-pane|:pointer-documentation|chat-interaction-state'
capability_candidates=(
  src/keymap.lisp
  src/mcclim-interface.lisp
  src/packages.lisp
  src/appearance-packages.lisp
  src/package-manager.lisp
  src/appearance.lisp
  src/appearance-config.lisp
  tests/appearance-test.lisp
  tests/appearance-config-test.lisp
  tests/skills-test.lisp
)

usage() {
  printf 'usage: %s legacy|capabilities\n' "$0" >&2
  exit 64
}

run_inventory() {
  local regex=$1
  shift
  local status

  [ "$#" -gt 0 ] || return 0
  if LC_ALL=C rg --no-config --color=never --threads 1 --line-number --with-filename \
       --regexp "$regex" -- "$@"; then
    return 0
  else
    status=$?
  fi
  [ "$status" -eq 1 ] && return 0
  return "$status"
}

[ "$#" -eq 1 ] || usage
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(GIT_MASTER=1 git -C "$script_dir" rev-parse --show-toplevel)
cd "$repo_root"

case "$1" in
  legacy)
    tracked_paths_file=$(mktemp)
    trap 'rm -f "$tracked_paths_file"' EXIT HUP INT TERM
    GIT_MASTER=1 git ls-files -z >"$tracked_paths_file"
    mapfile -d '' -t tracked_paths <"$tracked_paths_file"
    run_inventory "$legacy_regex" "${tracked_paths[@]}"
    ;;
  capabilities)
    existing_candidates=()
    for path in "${capability_candidates[@]}"; do
      if [ -e "$path" ] || [ -L "$path" ]; then
        existing_candidates+=("$path")
      fi
    done
    run_inventory "$capabilities_regex" "${existing_candidates[@]}"
    ;;
  *)
    usage
    ;;
esac
