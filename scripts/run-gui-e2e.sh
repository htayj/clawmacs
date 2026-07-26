#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$SCRIPT_DIR/gui-e2e-cleanup.sh"
. "$SCRIPT_DIR/gui-e2e-artifacts.sh"
. "$SCRIPT_DIR/gui-e2e-cache.sh"
. "$SCRIPT_DIR/gui-e2e-container-retry.sh"
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
Usage: scripts/run-gui-e2e.sh [--preflight-only] [--suite smoke|mx|features|keybinds|compose-geometry|organa|quaestor|reload|stability] [--artifact-dir DIR]

Runs an opt-in Clawmacs GUI E2E suite inside an isolated Xvfb display.
Set CLAWMACS_GUI_E2E_FRAME_READY_TIMEOUT_SECONDS to override the 300-second
cold-start/frame-ready timeout.
Set CLAWMACS_GUI_E2E_COLD_CACHE=1 to skip the artifact-cache seed and exercise
a deliberately cold ASDF startup.
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
  need_binary xdotool
  need_binary setsid
  need_binary flock
  need_binary cp
  need_binary realpath
  need_binary stat
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

normalize_host_artifact_path_for_container() {
  path=$1
  case "$path" in
    "$REPO_ROOT")
      printf '/workspace\n'
      ;;
    "$REPO_ROOT"/*)
      printf '/workspace/%s\n' "${path#"$REPO_ROOT"/}"
      ;;
    *)
      printf '%s\n' "$path"
      ;;
  esac
}

canonicalize_container_artifact_path() {
  path=$1
  case "/$path/" in
    */../*)
      fail 'artifact directory must not contain a parent-directory component'
      ;;
  esac
  case "$path" in
    /workspace|/workspace/*)
      ;;
    /*)
      fail 'artifact directory must be under /workspace inside the container'
      ;;
    *)
      path="/workspace/$path"
      ;;
  esac
  path=$(realpath -m -- "$path") || \
    fail 'artifact directory could not be canonicalized'
  case "$path" in
    /workspace|/workspace/*)
      ;;
    *)
      fail 'artifact directory resolves outside /workspace'
      ;;
  esac
  mkdir -p "$path"
  path=$(CDPATH= cd -- "$path" && pwd -P)
  case "$path" in
    /workspace|/workspace/*)
      printf '%s\n' "$path"
      ;;
    *)
      fail 'artifact directory resolves outside /workspace'
      ;;
  esac
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
  else
    ARTIFACT_DIR=$(normalize_host_artifact_path_for_container "$ARTIFACT_DIR")
  fi
  if gui_e2e_run_container_with_retry \
       "$SCRIPT_DIR/guix-container.sh" --mode e2e -- \
       sh "scripts/run-gui-e2e.sh" \
         --inside-container \
         --suite "$SUITE" \
         --artifact-dir "$ARTIFACT_DIR"; then
    exit 0
  else
    exit $?
  fi
fi

inside_preflight
if [ "$PREFLIGHT_ONLY" -eq 1 ]; then
  exit 0
fi

PREWARMED_XDG_CACHE_HOME=${XDG_CACHE_HOME:-}

case "$SUITE" in
  smoke|mx|features|keybinds|compose-geometry|organa|quaestor|reload|stability) ;;
  *) fail "unsupported GUI E2E suite: $SUITE" ;;
esac

if [ -z "$ARTIFACT_DIR" ]; then
  ARTIFACT_DIR="/workspace/.artifacts/gui-e2e/$(date -u +%Y%m%dT%H%M%SZ)-$$"
fi

# HOME and XDG_CACHE_HOME must be absolute.  A relative artifact path makes
# ASDF initialization fail before Clawmacs starts, while Quicklisp masks the
# original pathname error behind its generic "Could not load ASDF" condition.
ARTIFACT_DIR=$(canonicalize_container_artifact_path "$ARTIFACT_DIR")

gui_e2e_reset_run_artifacts "$ARTIFACT_DIR"
SCREENSHOT_DIR="$ARTIFACT_DIR/screenshots"
mkdir -p "$SCREENSHOT_DIR" "$ARTIFACT_DIR/home" "$ARTIFACT_DIR/cache"

HARNESS_LOG="$ARTIFACT_DIR/harness.log"
DEBUG_LOG="$ARTIFACT_DIR/debug.log"
APP_STDOUT="$ARTIFACT_DIR/app.stdout"
APP_STDERR="$ARTIFACT_DIR/app.stderr"
DRIVER_STDOUT="$ARTIFACT_DIR/driver.stdout"
DRIVER_STDERR="$ARTIFACT_DIR/driver.stderr"
WINDOW_ID_FILE="$ARTIFACT_DIR/window.id"
APP_PGID_FILE="$ARTIFACT_DIR/app.pgid"

# Never let diagnostics or probes fall back to a preserved host display.  The
# private Xvfb DISPLAY is exported only after its container-local server is up.
unset DISPLAY XAUTHORITY

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
  python3 - "$ARTIFACT_DIR/summary.json" "$reason" "$ARTIFACT_DIR" <<'PY' || true
import json, os, sys
summary_path, reason, artifact_dir = sys.argv[1:]
payload = {}
if os.path.exists(summary_path):
    try:
        with open(summary_path, encoding='utf-8') as f:
            decoded = json.load(f)
        if isinstance(decoded, dict):
            payload = decoded
    except (OSError, json.JSONDecodeError):
        pass
suite = os.environ.get('CLAWMACS_GUI_E2E_SUITE', 'unknown')
payload.update({'suite': suite, 'ok': False, 'failure': reason,
                'artifact_dir': artifact_dir})
with open(summary_path, 'w', encoding='utf-8') as f:
    json.dump(payload, f, indent=2, sort_keys=True)
    f.write('\n')
PY
}

if [ -e /tmp/.X11-unix ] || [ -L /tmp/.X11-unix ]; then
  xvfb_socket_metadata=$(stat -c 'device=%d inode=%i mode=%a uid=%u gid=%g' \
    /tmp/.X11-unix 2>/dev/null || printf 'metadata unavailable')
  write_failure_artifacts \
    "Xvfb socket directory pre-exists before startup ($xvfb_socket_metadata)"
  exit "$GUI_E2E_XVFB_NAMESPACE_RETRY_STATUS"
fi

if [ -n "$PREWARMED_XDG_CACHE_HOME" ]; then
  if ! gui_e2e_seed_private_common_lisp_cache \
       "$PREWARMED_XDG_CACHE_HOME" \
       "$ARTIFACT_DIR/cache" \
       "$PREWARMED_XDG_CACHE_HOME/quicklisp.lock"; then
    write_failure_artifacts \
      'artifact-private Common Lisp cache policy could not be applied safely'
    exit 1
  fi
else
  GUI_E2E_CACHE_SEED_STATUS=source-unavailable
fi

case "$GUI_E2E_CACHE_SEED_STATUS" in
  seeded)
    log "seeded artifact-private Common Lisp cache from $PREWARMED_XDG_CACHE_HOME"
    ;;
  cold-override)
    log 'cold-cache override enabled; starting with an empty artifact cache'
    ;;
  private-cache-present)
    log 'artifact-private Common Lisp cache already exists; preserving it'
    ;;
  source-unavailable)
    log 'prewarmed Common Lisp cache unavailable; continuing with a cold artifact cache'
    ;;
  seed-failed)
    log 'prewarmed Common Lisp cache could not be copied safely; continuing with a cold artifact cache'
    ;;
  *)
    write_failure_artifacts \
      "unexpected Common Lisp cache seed status: $GUI_E2E_CACHE_SEED_STATUS"
    exit 1
    ;;
esac

scan_runtime_failure_artifacts() {
  python3 - "$DEBUG_LOG" "$APP_STDOUT" "$APP_STDERR" <<'PY'
import sys
from pathlib import Path

paths = [Path(raw) for raw in sys.argv[1:]]
contents = {
    path: path.read_text(encoding="utf-8", errors="replace")
    for path in paths if path.exists()
}
text = "\n".join(contents.values()).lower()
signatures = {
    "debugger invoked": "SBCL entered the debugger",
    " is not grafted": "McCLIM reported an ungrafted sheet",
    "menu-bar-error-recovered": "the historical menu recovery path ran",
    "redisplay-queue-failed": "a redisplay wakeup could not be queued",
    "redisplay-handler-error": "the redisplay handler contained an error",
    "ui-action-error": "a user action raised an application error",
    "frame-cleanup-error": "frame unwind cleanup contained an error",
    "runtime-stream-cleanup-error": "stream cleanup contained an error",
    "runtime-stream-settlement-waiter-error": (
        "the provider settlement waiter failed"),
    "runtime-oauth-cleanup-error": "OAuth cleanup contained an error",
    "runtime-oauth-settlement-waiter-error": (
        "the OAuth settlement waiter failed"),
    "runtime-worker-settlement-timeout": "a runtime worker did not settle",
    "runtime-worker-reaper-error": "the runtime worker reaper failed",
    "runtime-tool-worker-start-error": "a runtime tool worker could not start",
    "runtime-tool-settlement-waiter-error": (
        "the tool settlement waiter failed"),
    "runtime-tool-effect-error": "a deferred tool effect could not be applied",
    "runtime-tool-queue-cleanup-error": "tool-queue cleanup contained an error",
    "runtime-interactive-operation-worker-start-error": (
        "a managed interactive-operation worker could not start"),
    "runtime-interactive-operation-settlement-waiter-error": (
        "a managed interactive-operation settlement waiter failed"),
    "runtime-interactive-operation-apply-error": (
        "a managed interactive-operation result failed during frame apply"),
    "runtime-interactive-operation-cancel-apply-error": (
        "a managed interactive-operation cancellation callback failed"),
    "runtime-callback-error": "an external runtime callback raised an error",
    "runtime-callback-dropped": (
        "the bounded external callback queue dropped an event"),
    "runtime-callback-dispatch-error": (
        "external callback dispatch failed before delivery"),
    "interop-event-callback-error": "an interop event callback failed",
    "interop-evicted-thread-dispose-failed": (
        "an evicted interop thread failed to dispose"),
    "interop-resume-loser-dispose-failed": (
        "a losing interop resume candidate failed to dispose"),
    "tool-execution-journal-error": (
        "a durable tool execution journal write failed"),
    "project-buffer-effect-error": (
        "a committed project write could not synchronize its UI buffer"),
    "prompt-tool-result-cleanup-error": "prompt tool-result cleanup failed",
    "message-help-thread-start-error": "a message-help worker could not start",
    "message-help-admission-error": (
        "message-help runtime admission raised an unexpected error"),
    "message-help-frame-error": "message-help frame handling failed",
    "message-help-frame-construction-error": (
        "a message-help frame could not be constructed"),
    "chat-recurse-launch-error": "a recursive chat frame could not launch",
    "safe-reload-completion-dispatch-error": (
        "safe-reload completion could not reach the frame process"),
    "safe-reload-completion-application-error": (
        "safe-reload completion failed on the frame process"),
}
found = [description for signature, description in signatures.items()
         if signature in text]
lines = [line.lower() for value in contents.values()
         for line in value.splitlines()]
if any(line.startswith("fatal error encountered") for line in lines):
    found.append("SBCL reported a fatal runtime error")
if any(line.startswith("unhandled ") and " in thread " in line
       for line in lines):
    found.append("an SBCL thread terminated with an unhandled condition")
if any(line.startswith("heap exhausted") for line in lines):
    found.append("SBCL exhausted its dynamic heap")
if found:
    print("runtime failure signatures: " + "; ".join(dict.fromkeys(found)),
          file=sys.stderr)
    raise SystemExit(1)
print("runtime failure scan passed: " + ", ".join(str(path) for path in paths))
PY
}

XVFB_PID=''
XVFB_PGID=''
APP_PID=''
APP_PGID=''
gui_e2e_install_cleanup_traps

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
if [ "$SUITE" = "keybinds" ]; then
  export CLAWMACS_GUI_E2E_INITIAL_INPUT_FOCUS=standard-input
else
  unset CLAWMACS_GUI_E2E_INITIAL_INPUT_FOCUS
fi

FRAME_READY_TIMEOUT_SECONDS=${CLAWMACS_GUI_E2E_FRAME_READY_TIMEOUT_SECONDS:-300}
case "$FRAME_READY_TIMEOUT_SECONDS" in
  ''|*[!0-9]*)
    fail 'CLAWMACS_GUI_E2E_FRAME_READY_TIMEOUT_SECONDS must be a positive integer'
    ;;
esac
[ "$FRAME_READY_TIMEOUT_SECONDS" -gt 0 ] || \
  fail 'CLAWMACS_GUI_E2E_FRAME_READY_TIMEOUT_SECONDS must be a positive integer'
FRAME_READY_ATTEMPTS=$((FRAME_READY_TIMEOUT_SECONDS * 5))

APP_EXIT_TIMEOUT_SECONDS=${CLAWMACS_GUI_E2E_APP_EXIT_TIMEOUT_SECONDS:-30}
case "$APP_EXIT_TIMEOUT_SECONDS" in
  ''|*[!0-9]*)
    fail 'CLAWMACS_GUI_E2E_APP_EXIT_TIMEOUT_SECONDS must be a positive integer'
    ;;
esac
[ "$APP_EXIT_TIMEOUT_SECONDS" -gt 0 ] || \
  fail 'CLAWMACS_GUI_E2E_APP_EXIT_TIMEOUT_SECONDS must be a positive integer'

XVFB_DISPLAY_FILE="$ARTIFACT_DIR/xvfb.display"
rm -f "$XVFB_DISPLAY_FILE"

log "artifacts: $ARTIFACT_DIR"
log 'starting Xvfb on a dynamically allocated Unix display'
Xvfb -displayfd 3 -screen 0 1280x900x24 -nolisten tcp -ac \
  3>"$XVFB_DISPLAY_FILE" >"$ARTIFACT_DIR/xvfb.log" 2>&1 &
XVFB_PID=$!

ready=0
i=0
while [ "$i" -lt 100 ]; do
  if [ -s "$XVFB_DISPLAY_FILE" ]; then
    DISPLAY_NUM=$(tr -d '\r\n' < "$XVFB_DISPLAY_FILE")
    case "$DISPLAY_NUM" in
      ''|*[!0-9]*)
        write_failure_artifacts "Xvfb returned invalid display number: $DISPLAY_NUM"
        exit 1
        ;;
    esac
    XVFB_DISPLAY=":$DISPLAY_NUM"
    export DISPLAY="$XVFB_DISPLAY"
    log "Xvfb allocated private Unix display $DISPLAY"
    break
  fi
  if ! kill -0 "$XVFB_PID" >/dev/null 2>&1; then
    write_failure_artifacts 'Xvfb exited before allocating a display'
    exit 1
  fi
  i=$((i + 1))
  sleep 0.1
done
[ -n "${DISPLAY:-}" ] || { write_failure_artifacts 'timed out waiting for Xvfb display allocation'; exit 1; }

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

log "launching Clawmacs in an isolated process group"
rm -f "$APP_PGID_FILE"
export CLAWMACS_GUI_E2E_APP_PGID_FILE="$APP_PGID_FILE"
setsid --wait sh -c '
  # setsid conditionally forks when its caller is already a process-group
  # leader.  Publish the PID from inside the new session so the harness owns
  # the exact application group instead of assuming that $! is its PGID.
  printf "%s\n" "$$" > "$CLAWMACS_GUI_E2E_APP_PGID_FILE"
  exec "$@"
' sh sbcl --noinform \
  --non-interactive \
  --disable-debugger \
  --load "$CLAWMACS_QUICKLISP_SETUP" \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(ql:quickload :clawmacs)' \
  --eval '(setf clawmacs:*inhibit-user-init* t)' \
  --eval '(let ((suite (or (uiop:getenv "CLAWMACS_GUI_E2E_SUITE") ""))) (when (member suite (list "organa" "quaestor") :test (function string=)) (clawmacs:set-package-enablement-scope suite :global) (clawmacs:load-active-packages)))' \
  --eval '(when (string= (or (uiop:getenv "CLAWMACS_GUI_E2E_SUITE") "") "keybinds") (clawmacs:add-hook (quote clawmacs:*initial-buffer-hook*) (lambda (buffer) (let* ((prefix "CLAWMACS_SWITCH_BUFFER_LARGE_DRAFT:") (draft (concatenate (quote string) prefix (make-string (- 32768 (length prefix)) :initial-element #\x))) (message (clawmacs:buffer-input-message buffer))) (loop for offset from 79 below (length draft) by 80 do (setf (char draft offset) #\Newline)) (clawmacs:set-message-text message draft) (clawmacs:set-message-point-from-absolute-offset message 8192) (clawmacs:set-message-mark-from-absolute-offset message 24576)) (dotimes (index 12) (clawmacs:make-chat-buffer (format nil "switch-e2e-~D" index) :agent-name "agent" :working-directory (truename ".") :session-persistence-mode :ephemeral :add-to-ring-p t)) (clawmacs:switch-to-buffer buffer)) :append t))' \
  --eval '(when (string= (or (uiop:getenv "CLAWMACS_GUI_E2E_SUITE") "") "quaestor") (clawmacs:add-hook (quote clawmacs:*initial-buffer-hook*) (lambda (buffer) (clawmacs::quaestor-request-user-input buffer (quote ((:header "Scope" :id "scope" :question "Pick a scope." :options ((:label "Alpha" :description "Smaller change.") (:label "Beta" :description "Broader change.")) :freeform t))))) :append t))' \
  --eval '(when (string= (or (uiop:getenv "CLAWMACS_GUI_E2E_SUITE") "") "stability") (clawmacs:add-hook (quote clawmacs:*initial-buffer-hook*) (lambda (buffer) (clawmacs:set-buffer-provider-override buffer :openai-codex) (clawmacs:set-buffer-model-override buffer "gpt-5.3-codex")) :append t))' \
  --eval '(clawmacs:clawmacs-main :session-name "clawmacs:e2e" :agent-name "agent" :window-title "Clawmacs E2E" :working-directory (truename "."))' \
  --eval '(uiop:quit)' \
  >"$APP_STDOUT" 2>"$APP_STDERR" &
APP_PID=$!
APP_PGID=''

i=0
app_group_live=0
while [ "$i" -lt 50 ]; do
  if [ -s "$APP_PGID_FILE" ]; then
    published_app_pgid=$(tr -d '\r\n' < "$APP_PGID_FILE")
    case "$published_app_pgid" in
      ''|*[!0-9]*)
        write_failure_artifacts \
          "Clawmacs launcher published invalid process group: $published_app_pgid"
        exit 1
        ;;
    esac
    APP_PGID=$published_app_pgid
    if gui_e2e_process_group_live_p "$APP_PGID"; then
      app_group_live=1
      break
    fi
  fi
  if ! kill -0 "$APP_PID" >/dev/null 2>&1; then
    write_failure_artifacts 'Clawmacs exited before establishing its process group'
    exit 1
  fi
  i=$((i + 1))
  sleep 0.1
done
[ -n "$APP_PGID" ] && [ "$app_group_live" -eq 1 ] || {
  write_failure_artifacts \
    "Clawmacs failed to establish an isolated process group (owner=${APP_PID:-none}, published=${APP_PGID:-none}, group-live=$app_group_live)"
  exit 1
}

log "waiting for frame-ready event and X window"
window_id=''
i=0
while [ "$i" -lt "$FRAME_READY_ATTEMPTS" ]; do
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
  write_failure_artifacts "timed out after ${FRAME_READY_TIMEOUT_SECONDS}s waiting for frame-ready/window"
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

log 'driver observed frame-stopped; waiting for natural Clawmacs exit'
completed_app_pid=$APP_PID
completed_app_pgid=$APP_PGID
app_status=0
if gui_e2e_wait_child_and_group_bounded \
     "$APP_PID" "$APP_EXIT_TIMEOUT_SECONDS" "$APP_PGID"; then
  app_status=0
else
  app_status=$?
fi
if [ "$GUI_E2E_WAIT_TIMED_OUT" -eq 1 ]; then
  write_failure_artifacts \
    "Clawmacs did not exit within ${APP_EXIT_TIMEOUT_SECONDS}s after frame-stopped"
  exit 1
fi
log "natural application exit observed: pid=$completed_app_pid status=$app_status; process group $completed_app_pgid is empty"
# The application child has been reaped.  Clear its trap-owned handle so EXIT
# cleanup cannot accidentally signal a recycled PID.
APP_PID=''
APP_PGID=''

scan_status=0
if scan_runtime_failure_artifacts >>"$HARNESS_LOG" 2>&1; then
  scan_status=0
else
  scan_status=$?
fi

if [ "$app_status" -ne 0 ]; then
  write_failure_artifacts "Clawmacs exited with status $app_status after frame-stopped"
  exit 1
fi
if [ "$scan_status" -ne 0 ]; then
  write_failure_artifacts 'runtime failure signature found after Clawmacs exit'
  exit 1
fi

log "GUI E2E suite passed"
