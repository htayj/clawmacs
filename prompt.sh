#!/bin/sh

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

# Debug log written inside the container at /workspace/debug-prompt.log,
# which maps to $REPO_ROOT/debug-prompt.log on the host.
: "${CLAWMACS_DEBUG_LOG:=/workspace/debug-prompt.log}"
export CLAWMACS_DEBUG_LOG

if [ -z "${CLAWMACS_PROMPT_PROJECT_NAME+x}" ] || [ -z "$CLAWMACS_PROMPT_PROJECT_NAME" ]; then
  CLAWMACS_PROMPT_PROJECT_NAME=$(basename "$SCRIPT_DIR")
fi
: "${CLAWMACS_PROMPT_PROJECT_ROOT:=/workspace/}"
export CLAWMACS_PROMPT_PROJECT_NAME
export CLAWMACS_PROMPT_PROJECT_ROOT

clean_build=${CLAWMACS_PROMPT_CLEAN_BUILD:-0}

exec "$SCRIPT_DIR/scripts/guix-container.sh" --mode run -- \
  sh -lc 'CLAWMACS_PROMPT_CLEAN_BUILD="$1"; export CLAWMACS_PROMPT_CLEAN_BUILD; shift; exec sbcl --script scripts/prompt.lisp "$@"' sh "$clean_build" "$@"
