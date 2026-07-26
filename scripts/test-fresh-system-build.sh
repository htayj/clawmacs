#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)

exec "$SCRIPT_DIR/guix-container.sh" --mode run -- \
  sh -lc '
    set -eu
    cache_root=$(mktemp -d)
    cleanup() {
      rm -rf "$cache_root"
    }
    trap cleanup EXIT INT TERM
    export XDG_CACHE_HOME="$cache_root"
    cd "$1"
    sbcl --noinform \
      --load "$CLAWMACS_QUICKLISP_SETUP" \
      --eval "(push (truename \".\") asdf:*central-registry*)" \
      --eval "(asdf:load-system :clawmacs :force t)" \
      --eval "(multiple-value-bind (output warnings-p failure-p) (compile-file \"src/artifactum-core.lisp\") (declare (ignore output warnings-p)) (unless failure-p (format t \"~&fresh-source-compile: ok~%\")) (uiop:quit (if failure-p 1 0)))"' \
  sh "$REPO_ROOT"
