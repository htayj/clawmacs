#!/bin/sh

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

# Debug log — written inside the container at /workspace/debug.log,
# which maps to $REPO_ROOT/debug.log on the host.
export CLAWMACS_DEBUG_LOG=/workspace/debug.log

exec "$SCRIPT_DIR/scripts/guix-container.sh" --mode run -- \
  sh -lc 'exec sbcl --noinform --load "${CLAWMACS_QUICKLISP_SETUP:?missing CLAWMACS_QUICKLISP_SETUP}" --eval "(push (truename \".\") asdf:*central-registry*)" --eval "(ql:quickload :clawmacs)" --eval "(clawmacs:clawmacs-main)"'
