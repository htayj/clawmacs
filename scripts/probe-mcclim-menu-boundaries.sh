#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)

if [ "${1:-}" != "--inside-container" ]; then
  artifact_dir=${1:-"$REPO_ROOT/.artifacts/mcclim-menu-boundary-probe"}
  case "$artifact_dir" in
    "$REPO_ROOT"/*)
      container_artifact="/workspace/${artifact_dir#"$REPO_ROOT"/}"
      ;;
    *)
      printf '%s\n' "artifact directory must be under $REPO_ROOT" >&2
      exit 2
      ;;
  esac
  mkdir -p "$artifact_dir"
  export CLAWMACS_CONTAINER_DISABLE_HOST_X=1
  exec "$SCRIPT_DIR/guix-container.sh" --mode e2e -- \
    sh scripts/probe-mcclim-menu-boundaries.sh \
      --inside-container "$container_artifact"
fi

artifact_dir=${2:?missing container artifact directory}
case "$artifact_dir" in
  /workspace/*) ;;
  *)
    printf '%s\n' "container artifact directory must be under /workspace" >&2
    exit 2
    ;;
esac
mkdir -p "$artifact_dir"

xvfb_pid=
app_pid=
process_group_live_p() {
  kill -0 "-$1" >/dev/null 2>&1
}
stop_process_group() {
  group=$1
  if process_group_live_p "$group"; then
    kill -TERM "-$group" >/dev/null 2>&1 || true
  fi
  i=0
  while process_group_live_p "$group" && [ "$i" -lt 50 ]; do
    i=$((i + 1))
    sleep 0.05
  done
  if process_group_live_p "$group"; then
    kill -KILL "-$group" >/dev/null 2>&1 || true
  fi
  i=0
  while process_group_live_p "$group" && [ "$i" -lt 50 ]; do
    i=$((i + 1))
    sleep 0.05
  done
  ! process_group_live_p "$group"
}
cleanup() {
  if [ -n "$app_pid" ]; then
    stop_process_group "$app_pid" || true
  fi
  if [ -n "$xvfb_pid" ]; then
    stop_process_group "$xvfb_pid" || true
  fi
}
trap cleanup EXIT INT TERM

setsid --wait Xvfb -displayfd 3 -screen 0 1280x900x24 -nolisten tcp -ac \
  3>"$artifact_dir/display" \
  >"$artifact_dir/xvfb.stdout" 2>"$artifact_dir/xvfb.stderr" &
xvfb_pid=$!

i=0
while [ ! -s "$artifact_dir/display" ]; do
  kill -0 "$xvfb_pid"
  i=$((i + 1))
  [ "$i" -lt 200 ]
  sleep 0.05
done
export DISPLAY=":$(tr -d '\r\n' < "$artifact_dir/display")"

setsid --wait sbcl --noinform --disable-debugger \
  --load "$CLAWMACS_QUICKLISP_SETUP" \
  --load /workspace/scripts/probe-mcclim-menu-boundaries.lisp \
  >"$artifact_dir/app.stdout" 2>"$artifact_dir/app.stderr" &
app_pid=$!

window_id=
i=0
while [ -z "$window_id" ]; do
  window_id=$(xdotool search \
    --name '^McCLIM Menu Boundary Probe$' 2>/dev/null | head -1 || true)
  kill -0 "$app_pid"
  i=$((i + 1))
  [ "$i" -lt 2400 ]
  sleep 0.05
done
printf '%s\n' "$window_id" >"$artifact_dir/window.id"
sleep 0.4

set -- xdotool mousemove --window "$window_id" 25 15 \
  click --clearmodifiers 1 sleep 0.2
i=0
while [ "$i" -lt 40 ]; do
  set -- "$@" \
    mousemove --window "$window_id" 25 49 \
    mousemove --window "$window_id" 90 15 \
    mousemove --window "$window_id" 90 49 \
    mousemove --window "$window_id" 25 15
  i=$((i + 1))
done

xdotool_status=0
"$@" >"$artifact_dir/xdotool.stdout" 2>"$artifact_dir/xdotool.stderr" ||
  xdotool_status=$?
printf 'xdotool_status=%s repetitions=40\n' "$xdotool_status" \
  >"$artifact_dir/result"

i=0
while kill -0 "$app_pid" 2>/dev/null && [ "$i" -lt 100 ]; do
  i=$((i + 1))
  sleep 0.05
done

if kill -0 "$app_pid" 2>/dev/null; then
  xdotool mousemove --window "$window_id" 25 15 \
    mousedown 1 sleep 0.2 \
    mousemove --window "$window_id" 25 69 mouseup 1 || true
fi

i=0
while process_group_live_p "$app_pid" && [ "$i" -lt 200 ]; do
  i=$((i + 1))
  sleep 0.05
done
if process_group_live_p "$app_pid"; then
  printf 'app_exit_timeout=true\n' >>"$artifact_dir/result"
  stop_process_group "$app_pid" || {
    printf 'app_group_empty=false\n' >>"$artifact_dir/result"
    exit 1
  }
fi

app_status=0
wait "$app_pid" || app_status=$?
if process_group_live_p "$app_pid"; then
  printf 'app_group_empty=false\n' >>"$artifact_dir/result"
  exit 1
fi
printf 'app_group_empty=true\n' >>"$artifact_dir/result"
app_pid=
printf 'app_status=%s\n' "$app_status" >>"$artifact_dir/result"

if grep -q 'Sheet .* is not grafted' "$artifact_dir/app.stderr" &&
   grep -q 'INVOKE-TRACKING-POINTER' "$artifact_dir/app.stderr"; then
  printf 'reproduced=true\n' >>"$artifact_dir/result"
  stop_process_group "$xvfb_pid" || {
    printf 'xvfb_group_empty=false\n' >>"$artifact_dir/result"
    exit 1
  }
  wait "$xvfb_pid" 2>/dev/null || true
  printf 'xvfb_group_empty=true\n' >>"$artifact_dir/result"
  xvfb_pid=
  exit 0
fi

printf 'reproduced=false\n' >>"$artifact_dir/result"
exit 1
