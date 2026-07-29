#!/bin/sh

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

# Debug log written inside the container at /workspace/debug-prompt.log,
# which maps to $REPO_ROOT/debug-prompt.log on the host.
: "${RPLACA_DEBUG_LOG:=/workspace/debug-prompt.log}"
export RPLACA_DEBUG_LOG

clean_build=${RPLACA_PROMPT_CLEAN_BUILD:-0}

exec "$SCRIPT_DIR/scripts/guix-container.sh" --mode run -- \
  sh -lc 'RPLACA_PROMPT_CLEAN_BUILD="$1"; export RPLACA_PROMPT_CLEAN_BUILD; shift; exec sbcl --script scripts/prompt.lisp "$@"' sh "$clean_build" "$@"
