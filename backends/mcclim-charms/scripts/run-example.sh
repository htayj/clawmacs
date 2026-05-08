#!/usr/bin/env sh
set -eu

script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
script_path="${script_dir}/$(basename -- "$0")"

cd "${script_dir}/.."

export TERM="${TERM:-xterm-256color}"
export ASDF_OUTPUT_TRANSLATIONS="/:${PWD}/.cache/common-lisp"

if [ "${1-}" = "--no-guix" ]; then
  shift
elif command -v guix >/dev/null 2>&1; then
  exec guix shell -m manifest.scm -- "${script_path}" --no-guix "$@"
fi

exec sbcl --noinform --disable-debugger --load scripts/run-example.lisp -- "$@"
