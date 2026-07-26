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
    export CLAWMACS_FRESH_BUILD_CACHE="$cache_root/asdf/"
    export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (t (\"$CLAWMACS_FRESH_BUILD_CACHE\" :implementation)) :ignore-inherited-configuration)"
    malformed_source="$cache_root/expected-reader-error.lisp"
    malformed_log="$cache_root/expected-reader-error.log"
    printf "%s\n" "(defun expected-reader-error (" > "$malformed_source"
    if sbcl --noinform --non-interactive --compile-file "$malformed_source" \
         >"$malformed_log" 2>&1
    then
      cat "$malformed_log" >&2
      echo "fresh-system-build: malformed source unexpectedly compiled" >&2
      exit 1
    fi
    cd "$1"
    sbcl --noinform --non-interactive \
      --load "$CLAWMACS_QUICKLISP_SETUP" \
      --eval "(push (truename \".\") asdf:*central-registry*)" \
      --eval "(asdf:clear-output-translations)" \
      --eval "(asdf:initialize-output-translations)" \
      --eval "(asdf:load-system :clawmacs :force :all)" \
      --eval "(let ((cache (truename (uiop:ensure-directory-pathname (uiop:getenv \"CLAWMACS_FRESH_BUILD_CACHE\")))) (count 0)) (labels ((walk (component) (when (typep component (find-class (quote asdf:cl-source-file))) (incf count) (dolist (output (asdf:output-files (quote asdf:compile-op) component)) (unless (and (probe-file output) (uiop:subpathp (truename output) cache)) (error \"Fresh build output missing or escaped isolated cache for ~A: ~A\" component output)))) (when (typep component (find-class (quote asdf:parent-component))) (map nil (function walk) (asdf:component-children component))))) (walk (asdf:find-system :clawmacs)) (unless (plusp count) (error \"Fresh build found no Common Lisp source components.\")) (format t \"~&fresh-system-build: compiled ~D clawmacs source components into ~A~%\" count cache)))"' \
  sh "$REPO_ROOT"
