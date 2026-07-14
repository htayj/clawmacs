#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$SCRIPT_DIR/gui-e2e-cleanup.sh"

case "${1:-}" in
  --term-worker)
    pid_file=$2
    APP_PID=''
    APP_PGID=''
    XVFB_PID=''
    XVFB_PGID=''
    setsid sleep 300 &
    APP_PID=$!
    APP_PGID=$APP_PID
    printf '%s\n' "$APP_PID" > "$pid_file"
    gui_e2e_install_cleanup_traps
    kill -TERM "$$"
    exit 99
    ;;
  --exit-worker)
    APP_PID=''
    APP_PGID=''
    XVFB_PID=''
    XVFB_PGID=''
    gui_e2e_install_cleanup_traps
    exit 37
    ;;
  --natural-worker)
    APP_PID=''
    APP_PGID=''
    XVFB_PID=''
    XVFB_PGID=''
    setsid sh -c 'exit 0' &
    APP_PID=$!
    APP_PGID=$APP_PID
    gui_e2e_install_cleanup_traps
    natural_status=0
    if gui_e2e_wait_child_and_group_bounded "$APP_PID" 2 "$APP_PGID"; then
      natural_status=0
    else
      natural_status=$?
    fi
    APP_PID=''
    APP_PGID=''
    exit "$natural_status"
    ;;
  --hung-worker)
    pid_file=$2
    APP_PID=''
    APP_PGID=''
    XVFB_PID=''
    XVFB_PGID=''
    setsid sleep 300 &
    APP_PID=$!
    APP_PGID=$APP_PID
    printf '%s\n' "$APP_PID" > "$pid_file"
    gui_e2e_install_cleanup_traps
    if gui_e2e_wait_child_and_group_bounded "$APP_PID" 1 "$APP_PGID"; then
      exit 98
    fi
    [ "$GUI_E2E_WAIT_TIMED_OUT" -eq 1 ] || exit 97
    exit 124
    ;;
  --stubborn-worker)
    pid_file=$2
    descendant_pid_file=$3
    APP_PID=''
    APP_PGID=''
    XVFB_PID=''
    XVFB_PGID=''
    setsid sh -c '
      trap "" TERM
      sleep 300 &
      descendant=$!
      printf "%s\n" "$descendant" > "$1"
      wait "$descendant"
    ' sh "$descendant_pid_file" &
    APP_PID=$!
    APP_PGID=$APP_PID
    printf '%s\n' "$APP_PID" > "$pid_file"
    gui_e2e_install_cleanup_traps
    # Do not race the EXIT trap against the descendant publishing its PID.
    # The regression must prove group cleanup, not occasionally skip its
    # descendant assertion because the worker shell had not started yet.
    publish_attempt=0
    while [ ! -s "$descendant_pid_file" ] && [ "$publish_attempt" -lt 100 ]; do
      publish_attempt=$((publish_attempt + 1))
      sleep 0.01
    done
    [ -s "$descendant_pid_file" ] || exit 96
    exit 0
    ;;
  --split-owner-worker)
    group_pid_file=$2
    descendant_pid_file=$3
    # This worker is launched as a session leader.  The inner setsid must
    # therefore fork, reproducing the util-linux path where the waitable owner
    # PID differs from the application session/process-group leader.  Both the
    # leader and its descendant ignore TERM so cleanup must preserve the owner
    # through the group grace, escalate the group, and let the owner reap it.
    exec setsid --wait sh -c '
      trap "" TERM
      sh -c "trap \"\" TERM; exec sleep 300" &
      descendant=$!
      printf "%s\n" "$$" > "$1"
      printf "%s\n" "$descendant" > "$2"
      wait "$descendant"
    ' sh "$group_pid_file" "$descendant_pid_file"
    ;;
  --timer-natural-worker)
    child_pid_file=$2
    APP_PID=''
    APP_PGID=''
    XVFB_PID=''
    XVFB_PGID=''
    setsid sleep 1 &
    APP_PID=$!
    APP_PGID=$APP_PID
    printf '%s\n' "$APP_PID" > "$child_pid_file"
    gui_e2e_install_cleanup_traps
    timer_natural_status=0
    if gui_e2e_wait_child_and_group_bounded "$APP_PID" 47 "$APP_PGID"; then
      timer_natural_status=0
    else
      timer_natural_status=$?
    fi
    APP_PID=''
    APP_PGID=''
    exit "$timer_natural_status"
    ;;
  --timer-interrupt-worker)
    child_pid_file=$2
    APP_PID=''
    APP_PGID=''
    XVFB_PID=''
    XVFB_PGID=''
    setsid sleep 300 &
    APP_PID=$!
    APP_PGID=$APP_PID
    printf '%s\n' "$APP_PID" > "$child_pid_file"
    gui_e2e_install_cleanup_traps
    gui_e2e_wait_child_and_group_bounded "$APP_PID" 47 "$APP_PGID"
    exit 95
    ;;
esac

tmp_dir=$(mktemp -d)
remove_tmp_dir() {
  rm -rf -- "$tmp_dir"
}
trap remove_tmp_dir EXIT INT TERM

exit_status=0
if sh "$0" --exit-worker; then
  exit_status=0
else
  exit_status=$?
fi
if [ "$exit_status" -ne 37 ]; then
  printf 'ordinary EXIT status: expected 37, got %s\n' "$exit_status" >&2
  exit 1
fi

term_status=0
if sh "$0" --term-worker "$tmp_dir/child.pid"; then
  term_status=0
else
  term_status=$?
fi
if [ "$term_status" -ne 143 ]; then
  printf 'TERM status: expected 143, got %s\n' "$term_status" >&2
  exit 1
fi

natural_status=0
if sh "$0" --natural-worker; then
  natural_status=0
else
  natural_status=$?
fi
if [ "$natural_status" -ne 0 ]; then
  printf 'natural child exit: expected 0, got %s\n' "$natural_status" >&2
  exit 1
fi

hung_status=0
if sh "$0" --hung-worker "$tmp_dir/hung-child.pid"; then
  hung_status=0
else
  hung_status=$?
fi
if [ "$hung_status" -ne 124 ]; then
  printf 'bounded child wait: expected 124, got %s\n' "$hung_status" >&2
  exit 1
fi

if ! sh "$0" --stubborn-worker \
     "$tmp_dir/stubborn-child.pid" "$tmp_dir/stubborn-descendant.pid"; then
  printf 'stubborn child cleanup: worker exited nonzero\n' >&2
  exit 1
fi

setsid sh "$0" --split-owner-worker \
  "$tmp_dir/split-group.pid" "$tmp_dir/split-descendant.pid" &
split_owner_pid=$!
split_publish_attempt=0
while { [ ! -s "$tmp_dir/split-group.pid" ] || \
        [ ! -s "$tmp_dir/split-descendant.pid" ]; } && \
      [ "$split_publish_attempt" -lt 100 ]; do
  split_publish_attempt=$((split_publish_attempt + 1))
  sleep 0.01
done
if [ ! -s "$tmp_dir/split-group.pid" ] || \
   [ ! -s "$tmp_dir/split-descendant.pid" ]; then
  kill -KILL "$split_owner_pid" >/dev/null 2>&1 || true
  wait "$split_owner_pid" >/dev/null 2>&1 || true
  printf 'split owner did not publish its exact process group\n' >&2
  exit 1
fi
split_group_pid=$(sed -n '1p' "$tmp_dir/split-group.pid")
split_descendant_pid=$(sed -n '1p' "$tmp_dir/split-descendant.pid")
if [ "$split_owner_pid" = "$split_group_pid" ]; then
  kill -KILL "$split_owner_pid" >/dev/null 2>&1 || true
  wait "$split_owner_pid" >/dev/null 2>&1 || true
  printf 'split owner regression did not exercise distinct owner/group PIDs\n' >&2
  exit 1
fi
if [ "$(gui_e2e_process_group_id "$split_group_pid")" != \
     "$split_group_pid" ]; then
  kill -KILL "$split_owner_pid" >/dev/null 2>&1 || true
  wait "$split_owner_pid" >/dev/null 2>&1 || true
  printf 'split owner published a non-leader PID: %s\n' \
    "$split_group_pid" >&2
  exit 1
fi

APP_PID=$split_owner_pid
APP_PGID=$split_group_pid
XVFB_PID=''
XVFB_PGID=''
GUI_E2E_CLEANUP_DONE=0
gui_e2e_cleanup_children
if ! gui_e2e_process_exited_p "$split_owner_pid"; then
  printf 'split owner cleanup left waitable owner %s running\n' \
    "$split_owner_pid" >&2
  exit 1
fi
if ! gui_e2e_process_exited_p "$split_group_pid"; then
  printf 'split owner cleanup left process-group leader %s running\n' \
    "$split_group_pid" >&2
  exit 1
fi
if ! gui_e2e_process_exited_p "$split_descendant_pid"; then
  printf 'split owner cleanup left resistant descendant %s running\n' \
    "$split_descendant_pid" >&2
  exit 1
fi

wait_for_python_timer_child() {
  timer_owner_pid=$1
  timer_child_pid=''
  timer_find_attempt=0
  while [ "$timer_find_attempt" -lt 200 ]; do
    timer_child_pid=$(ps -o pid=,comm= --ppid "$timer_owner_pid" 2>/dev/null |
      awk '$2 ~ /^python/ { print $1; exit }')
    [ -z "$timer_child_pid" ] || break
    timer_find_attempt=$((timer_find_attempt + 1))
    sleep 0.01
  done
  [ -n "$timer_child_pid" ] || return 1
  printf '%s\n' "$timer_child_pid"
}

sh "$0" --timer-natural-worker "$tmp_dir/timer-natural-child.pid" &
timer_natural_owner_pid=$!
if ! timer_natural_pid=$(wait_for_python_timer_child \
     "$timer_natural_owner_pid"); then
  kill -KILL "$timer_natural_owner_pid" >/dev/null 2>&1 || true
  wait "$timer_natural_owner_pid" >/dev/null 2>&1 || true
  printf 'natural wait did not publish a timer process\n' >&2
  exit 1
fi
if ! wait "$timer_natural_owner_pid"; then
  printf 'natural timer worker exited nonzero\n' >&2
  exit 1
fi
if kill -0 "$timer_natural_pid" >/dev/null 2>&1; then
  printf 'natural wait left timer process %s running\n' \
    "$timer_natural_pid" >&2
  exit 1
fi

sh "$0" --timer-interrupt-worker "$tmp_dir/timer-term-child.pid" &
timer_term_owner_pid=$!
if ! timer_term_pid=$(wait_for_python_timer_child "$timer_term_owner_pid"); then
  kill -KILL "$timer_term_owner_pid" >/dev/null 2>&1 || true
  wait "$timer_term_owner_pid" >/dev/null 2>&1 || true
  printf 'interruptible wait did not publish a timer process\n' >&2
  exit 1
fi
kill -TERM "$timer_term_owner_pid"
timer_term_status=0
if wait "$timer_term_owner_pid"; then
  timer_term_status=0
else
  timer_term_status=$?
fi
if [ "$timer_term_status" -ne 143 ]; then
  printf 'interrupted timer worker: expected 143, got %s\n' \
    "$timer_term_status" >&2
  exit 1
fi
if kill -0 "$timer_term_pid" >/dev/null 2>&1; then
  printf 'interrupted wait left timer process %s running\n' \
    "$timer_term_pid" >&2
  exit 1
fi
timer_term_child_pid=$(sed -n '1p' "$tmp_dir/timer-term-child.pid")
if kill -0 "$timer_term_child_pid" >/dev/null 2>&1; then
  printf 'interrupted wait left application child %s running\n' \
    "$timer_term_child_pid" >&2
  exit 1
fi

child_pid=$(sed -n '1p' "$tmp_dir/child.pid")
if kill -0 "$child_pid" >/dev/null 2>&1; then
  printf 'TERM cleanup left child process %s running\n' "$child_pid" >&2
  exit 1
fi
hung_child_pid=$(sed -n '1p' "$tmp_dir/hung-child.pid")
if kill -0 "$hung_child_pid" >/dev/null 2>&1; then
  printf 'bounded-wait cleanup left child process %s running\n' \
    "$hung_child_pid" >&2
  exit 1
fi
stubborn_child_pid=$(sed -n '1p' "$tmp_dir/stubborn-child.pid")
if ! gui_e2e_process_exited_p "$stubborn_child_pid"; then
  printf 'TERM/KILL cleanup left stubborn child process %s running\n' \
    "$stubborn_child_pid" >&2
  exit 1
fi
stubborn_descendant_pid=$(sed -n '1p' "$tmp_dir/stubborn-descendant.pid")
if ! gui_e2e_process_exited_p "$stubborn_descendant_pid"; then
  printf 'TERM/KILL cleanup left stubborn descendant process %s running\n' \
    "$stubborn_descendant_pid" >&2
  exit 1
fi

APP_PID=''
APP_PGID=''
XVFB_PID=''
XVFB_PGID=''
GUI_E2E_CLEANUP_DONE=0
gui_e2e_cleanup_children
gui_e2e_cleanup_children
if [ "$GUI_E2E_CLEANUP_DONE" -ne 1 ]; then
  printf 'repeated cleanup did not retain its completed state\n' >&2
  exit 1
fi

printf 'gui-e2e cleanup regression passed: EXIT=37 TERM=143 NATURAL=0 TIMEOUT=124 children=%s,%s,%s descendant=%s split-owner=%s group=%s split-descendant=%s timers=%s,%s reaped\n' \
  "$child_pid" "$hung_child_pid" "$stubborn_child_pid" \
  "$stubborn_descendant_pid" "$split_owner_pid" "$split_group_pid" \
  "$split_descendant_pid" "$timer_natural_pid" "$timer_term_pid"
