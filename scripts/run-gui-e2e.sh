#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
if ! REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null); then
  REPO_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
fi
SUITE=smoke
PREFLIGHT_ONLY=0
INSIDE_CONTAINER=0
ARTIFACT_DIR=''
WINDOW_TITLE='Clawmacs E2E'

usage() {
  cat <<'EOF'
Usage: scripts/run-gui-e2e.sh [--preflight-only] [--suite smoke|mx|features|organa] [--artifact-dir DIR]

Runs an opt-in Clawmacs GUI E2E suite inside an isolated Xvfb display.
EOF
}

fail() {
  printf '[gui-e2e] %s\n' "$*" >&2
  exit 1
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --preflight-only)
      PREFLIGHT_ONLY=1
      shift
      ;;
    --inside-container)
      INSIDE_CONTAINER=1
      shift
      ;;
    --suite)
      [ "$#" -ge 2 ] || fail 'missing value for --suite'
      SUITE="$2"
      shift 2
      ;;
    --artifact-dir)
      [ "$#" -ge 2 ] || fail 'missing value for --artifact-dir'
      ARTIFACT_DIR="$2"
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      fail "unknown argument: $1"
      ;;
  esac
done

need_binary() {
  if ! command -v "$1" >/dev/null 2>&1; then
    fail "missing required binary: $1"
  fi
}

have_screenshot_command() {
  command -v import >/dev/null 2>&1 || \
    command -v magick >/dev/null 2>&1 || \
    command -v xwd >/dev/null 2>&1
}

inside_preflight() {
  need_binary sbcl
  need_binary python3
  need_binary Xvfb
  need_binary xauth
  need_binary xdotool
  if ! have_screenshot_command; then
    fail 'missing screenshot command: import, magick, or xwd'
  fi
}

host_artifact_container_path() {
  run_id=$(date -u +%Y%m%dT%H%M%SZ)-$$
  rel=".artifacts/gui-e2e/$run_id"
  mkdir -p "$REPO_ROOT/$rel"
  printf '/workspace/%s\n' "$rel"
}

if [ "${CLAWMACS_IN_GUIX_CONTAINER:-0}" = "1" ]; then
  INSIDE_CONTAINER=1
fi

if [ "$INSIDE_CONTAINER" -ne 1 ]; then
  export CLAWMACS_CONTAINER_DISABLE_HOST_X=1
  if [ "$PREFLIGHT_ONLY" -eq 1 ]; then
    exec "$SCRIPT_DIR/guix-container.sh" --preflight-only --mode e2e -- true
  fi
  if [ -z "$ARTIFACT_DIR" ]; then
    ARTIFACT_DIR=$(host_artifact_container_path)
  fi
  exec "$SCRIPT_DIR/guix-container.sh" --mode e2e -- \
    sh "scripts/run-gui-e2e.sh" \
      --inside-container \
      --suite "$SUITE" \
      --artifact-dir "$ARTIFACT_DIR"
fi

inside_preflight
if [ "$PREFLIGHT_ONLY" -eq 1 ]; then
  exit 0
fi

case "$SUITE" in
  smoke|mx|features|organa) ;;
  *) fail "unsupported GUI E2E suite: $SUITE" ;;
esac

if [ -z "$ARTIFACT_DIR" ]; then
  ARTIFACT_DIR="/workspace/.artifacts/gui-e2e/$(date -u +%Y%m%dT%H%M%SZ)-$$"
fi

SCREENSHOT_DIR="$ARTIFACT_DIR/screenshots"
mkdir -p "$SCREENSHOT_DIR" "$ARTIFACT_DIR/home" "$ARTIFACT_DIR/cache"

HARNESS_LOG="$ARTIFACT_DIR/harness.log"
DEBUG_LOG="$ARTIFACT_DIR/debug.log"
APP_STDOUT="$ARTIFACT_DIR/app.stdout"
APP_STDERR="$ARTIFACT_DIR/app.stderr"
DRIVER_STDOUT="$ARTIFACT_DIR/driver.stdout"
DRIVER_STDERR="$ARTIFACT_DIR/driver.stderr"
WINDOW_ID_FILE="$ARTIFACT_DIR/window.id"

log() {
  printf '[gui-e2e] %s\n' "$*" | tee -a "$HARNESS_LOG" >&2
}

capture_root_screenshot() {
  path="$1"
  if command -v import >/dev/null 2>&1; then
    import -window root "$path"
  elif command -v magick >/dev/null 2>&1; then
    magick import -window root "$path"
  elif command -v xwd >/dev/null 2>&1; then
    xwd -silent -root -out "$path.xwd"
  else
    return 1
  fi
}

write_failure_artifacts() {
  reason="$1"
  log "failure: $reason"
  capture_root_screenshot "$SCREENSHOT_DIR/failure-harness.png" >/dev/null 2>&1 || true
  [ -f "$DEBUG_LOG" ] && tail -200 "$DEBUG_LOG" > "$ARTIFACT_DIR/debug.tail" || true
  [ -f "$APP_STDERR" ] && tail -200 "$APP_STDERR" > "$ARTIFACT_DIR/app.stderr.tail" || true
  [ -f "$DRIVER_STDERR" ] && tail -200 "$DRIVER_STDERR" > "$ARTIFACT_DIR/driver.stderr.tail" || true
  if [ ! -f "$ARTIFACT_DIR/summary.json" ]; then
    python3 - "$ARTIFACT_DIR/summary.json" "$reason" "$ARTIFACT_DIR" <<'PY' || true
import json, os, sys
summary_path, reason, artifact_dir = sys.argv[1:]
with open(summary_path, 'w', encoding='utf-8') as f:
    suite = os.environ.get('CLAWMACS_GUI_E2E_SUITE', 'unknown')
    json.dump({'suite': suite, 'ok': False, 'failure': reason,
               'artifact_dir': artifact_dir}, f, indent=2, sort_keys=True)
PY
  fi
}

XVFB_PID=''
APP_PID=''
cleanup() {
  status=$?
  if [ -n "${APP_PID:-}" ] && kill -0 "$APP_PID" >/dev/null 2>&1; then
    kill "$APP_PID" >/dev/null 2>&1 || true
    wait "$APP_PID" >/dev/null 2>&1 || true
  fi
  if [ -n "${XVFB_PID:-}" ] && kill -0 "$XVFB_PID" >/dev/null 2>&1; then
    kill "$XVFB_PID" >/dev/null 2>&1 || true
    wait "$XVFB_PID" >/dev/null 2>&1 || true
  fi
  exit "$status"
}
trap cleanup EXIT INT TERM

unset OPENAI_API_KEY ZAI_CODING_MAX_API_KEY OPENROUTER_API_KEY
unset OPENAI_TOKEN ZAI_TOKEN OPENROUTER_TOKEN
export HOME="$ARTIFACT_DIR/home"
export XDG_CACHE_HOME="$ARTIFACT_DIR/cache"
export CLAWMACS_DEBUG_LOG="$DEBUG_LOG"
export CLAWMACS_GUI_E2E=1
export CLAWMACS_E2E_EVENTS=1
export CLAWMACS_E2E_PROVIDER=1
export CLAWMACS_CONTAINER_DISABLE_HOST_X=1
export CLAWMACS_PROMPT_PROJECT_ROOT=/workspace
export CLAWMACS_GUI_E2E_SUITE="$SUITE"

DISPLAY_NUM=$((90 + ($$ % 100)))
XVFB_DISPLAY=":$DISPLAY_NUM"
export DISPLAY="localhost:$DISPLAY_NUM"
export XAUTHORITY="$ARTIFACT_DIR/xauthority"
COOKIE=$(od -An -N16 -tx1 /dev/urandom | tr -d ' \n')
xauth -f "$XAUTHORITY" add "$DISPLAY" . "$COOKIE" >/dev/null 2>&1
xauth -f "$XAUTHORITY" add "$XVFB_DISPLAY" . "$COOKIE" >/dev/null 2>&1

log "artifacts: $ARTIFACT_DIR"
log "starting Xvfb on $XVFB_DISPLAY with Xauthority (clients use $DISPLAY)"
Xvfb "$XVFB_DISPLAY" -screen 0 1280x900x24 -listen tcp -auth "$XAUTHORITY" >"$ARTIFACT_DIR/xvfb.log" 2>&1 &
XVFB_PID=$!

ready=0
i=0
while [ "$i" -lt 100 ]; do
  if xdotool getdisplaygeometry >/dev/null 2>&1; then
    ready=1
    break
  fi
  if ! kill -0 "$XVFB_PID" >/dev/null 2>&1; then
    write_failure_artifacts 'Xvfb exited before display became ready'
    exit 1
  fi
  i=$((i + 1))
  sleep 0.1
done
[ "$ready" -eq 1 ] || { write_failure_artifacts 'timed out waiting for Xvfb'; exit 1; }

log "launching Clawmacs"
sbcl --noinform \
  --load "$CLAWMACS_QUICKLISP_SETUP" \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(ql:quickload :clawmacs)' \
  --eval '(setf clawmacs:*inhibit-user-init* t)' \
  --eval '(when (string= (or (uiop:getenv "CLAWMACS_GUI_E2E_SUITE") "") "organa") (clawmacs:set-package-enablement-scope "organa" :global) (clawmacs:load-active-packages))' \
  --eval '(clawmacs:clawmacs-main :session-name "clawmacs:e2e" :agent-name "agent" :window-title "Clawmacs E2E" :working-directory (truename "."))' \
  --eval '(uiop:quit)' \
  >"$APP_STDOUT" 2>"$APP_STDERR" &
APP_PID=$!

log "waiting for frame-ready event and X window"
window_id=''
i=0
while [ "$i" -lt 900 ]; do
  if ! kill -0 "$APP_PID" >/dev/null 2>&1; then
    write_failure_artifacts 'Clawmacs exited before frame-ready'
    exit 1
  fi
  if [ -f "$DEBUG_LOG" ] && grep -q '"event":"frame-ready"' "$DEBUG_LOG"; then
    window_id=$(xdotool search --name "$WINDOW_TITLE" 2>/dev/null | head -n 1 || true)
    if [ -n "$window_id" ]; then
      printf '%s\n' "$window_id" > "$WINDOW_ID_FILE"
      break
    fi
  fi
  i=$((i + 1))
  sleep 0.2
done

if [ -z "$window_id" ]; then
  write_failure_artifacts 'timed out waiting for frame-ready/window'
  exit 1
fi

log "running driver against window $window_id"
if ! python3 scripts/gui-e2e-driver.py \
    --suite "$SUITE" \
    --artifact-dir "$ARTIFACT_DIR" \
    --debug-log "$DEBUG_LOG" \
    --window-title "$WINDOW_TITLE" \
    --window-id "$window_id" \
    >"$DRIVER_STDOUT" 2>"$DRIVER_STDERR"; then
  write_failure_artifacts 'driver failed'
  exit 1
fi

log "GUI E2E suite passed"
