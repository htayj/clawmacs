#!/bin/sh

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

: "${CLAWMACS_DEBUG_LOG:=/workspace/debug-app-server.log}"
export CLAWMACS_DEBUG_LOG

clean_build=${CLAWMACS_PROMPT_CLEAN_BUILD:-0}

exec "$SCRIPT_DIR/scripts/guix-container.sh" --mode run -- \
  sh -lc 'CLAWMACS_PROMPT_CLEAN_BUILD="$1"; export CLAWMACS_PROMPT_CLEAN_BUILD; shift; exec sbcl --script scripts/app-server.lisp "$@"' sh "$clean_build" "$@"
