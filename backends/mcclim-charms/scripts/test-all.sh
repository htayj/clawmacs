#!/usr/bin/env sh
set -eu

cd "$(dirname "$0")/.."

export ASDF_OUTPUT_TRANSLATIONS="/:${PWD}/.cache/common-lisp"

run_lisp='sbcl --noinform --disable-debugger --load'

if command -v guix >/dev/null 2>&1; then
  guix shell -m manifest.scm -- sh -lc "$run_lisp scripts/run-unit-tests.lisp && $run_lisp scripts/run-examples-tests.lisp && python3 scripts/audit-example-interactions.py && python3 scripts/pty-examples.py && python3 scripts/pty-interactions.py && python3 scripts/pty-chooser.py && python3 scripts/pty-example-launches.py && scripts/run-example.sh --no-guix --list >/dev/null && scripts/run-example.sh --no-guix --check calculator >/dev/null && scripts/run-example.sh --no-guix --check-chooser >/dev/null"
else
  sh -lc "$run_lisp scripts/run-unit-tests.lisp && $run_lisp scripts/run-examples-tests.lisp && python3 scripts/audit-example-interactions.py && python3 scripts/pty-examples.py && python3 scripts/pty-interactions.py && python3 scripts/pty-chooser.py && python3 scripts/pty-example-launches.py && scripts/run-example.sh --no-guix --list >/dev/null && scripts/run-example.sh --no-guix --check calculator >/dev/null && scripts/run-example.sh --no-guix --check-chooser >/dev/null"
fi
