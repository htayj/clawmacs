#!/bin/sh
set -eu

DISPLAY_NUMBER=${CLAWMACS_MCCLIM_E2E_DISPLAY_NUMBER:-99}
DISPLAY_VALUE="localhost:$DISPLAY_NUMBER"
LOG_PATH=".cache/mcclim-e2e-xvfb.log"

mkdir -p .cache

Xvfb ":$DISPLAY_NUMBER" -screen 0 1280x900x24 -listen tcp -ac >"$LOG_PATH" 2>&1 &
XVFB_PID=$!

cleanup() {
  if kill -0 "$XVFB_PID" 2>/dev/null; then
    kill "$XVFB_PID" 2>/dev/null || true
    wait "$XVFB_PID" 2>/dev/null || true
  fi
}

trap cleanup EXIT INT TERM

attempt=0
while [ "$attempt" -lt 100 ]; do
  if DISPLAY="$DISPLAY_VALUE" xdotool getdisplaygeometry >/dev/null 2>&1; then
    break
  fi
  if ! kill -0 "$XVFB_PID" 2>/dev/null; then
    cat "$LOG_PATH" >&2 || true
    exit 1
  fi
  attempt=$((attempt + 1))
  sleep 0.1
done

if ! DISPLAY="$DISPLAY_VALUE" xdotool getdisplaygeometry >/dev/null 2>&1; then
  cat "$LOG_PATH" >&2 || true
  echo "Xvfb did not become ready on $DISPLAY_VALUE" >&2
  exit 1
fi

export DISPLAY="$DISPLAY_VALUE"
python3 test-mcclim-e2e.py "$@"
