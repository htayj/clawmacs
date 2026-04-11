#!/bin/sh

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

# Debug log written inside the container at /workspace/debug.log,
# which maps to $REPO_ROOT/debug.log on the host.
export CLAWMACS_DEBUG_LOG=/workspace/debug.log

clean_build=${CLAWMACS_RUN_CLEAN_BUILD:-0}

exec "$SCRIPT_DIR/scripts/guix-container.sh" --mode run -- \
  sh -lc 'CLAWMACS_RUN_CLEAN_BUILD="$1"; export CLAWMACS_RUN_CLEAN_BUILD; shift; exec sbcl --noinform --script scripts/run.lisp "$@"' sh "$clean_build" "$@"
