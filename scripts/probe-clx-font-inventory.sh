#!/bin/sh
# Run the real public font-enumeration round trip in an isolated Guix Xvfb.
set -eu

exec ./scripts/guix-container.sh --mode e2e -- sh -lc '
  set -eu
  tmp=$(mktemp -d)
  cleanup() { test -n "${xvfb_pid:-}" && kill "$xvfb_pid" 2>/dev/null || true; rm -rf "$tmp"; }
  trap cleanup EXIT INT TERM
  Xvfb -displayfd 3 -screen 0 800x600x24 -nolisten tcp -ac \
    3>"$tmp/display" >"$tmp/xvfb.log" 2>&1 &
  xvfb_pid=$!
  for n in $(seq 1 100); do test -s "$tmp/display" && break; sleep 0.05; done
  test -s "$tmp/display"
  export DISPLAY=":$(tr -d "\r\n" < "$tmp/display")"
  sbcl --noinform --non-interactive --load "$CLAWMACS_QUICKLISP_SETUP" \
    --eval "(push (truename \".\") asdf:*central-registry*)" \
    --load scripts/probe-clx-font-inventory.lisp --eval "(quit)"
  printf "%s\\n" "CLX_FONT_INVENTORY_PROBE_SHELL_OK"
'
