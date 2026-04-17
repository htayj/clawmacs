#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
PREFLIGHT_ONLY=0

original_arg_count=$#
processed_args=0
wrapper_arg_scan=1
while [ "$processed_args" -lt "$original_arg_count" ]; do
  arg="$1"
  shift
  if [ "$wrapper_arg_scan" -eq 1 ] && [ "$arg" = "--" ]; then
    wrapper_arg_scan=0
    set -- "$@" "$arg"
  elif [ "$wrapper_arg_scan" -eq 1 ] && [ "$arg" = "--preflight-only" ]; then
    PREFLIGHT_ONLY=1
  else
    set -- "$@" "$arg"
  fi
  processed_args=$((processed_args + 1))
done

if [ "$PREFLIGHT_ONLY" -eq 1 ]; then
  CLAWMACS_CONTAINER_DISABLE_HOST_X=1 exec "$SCRIPT_DIR/guix-container.sh" --preflight-only --mode e2e -- scripts/mcclim-e2e-xvfb-run.sh "$@"
fi

CLAWMACS_CONTAINER_DISABLE_HOST_X=1 exec "$SCRIPT_DIR/guix-container.sh" --mode e2e -- scripts/mcclim-e2e-xvfb-run.sh "$@"
