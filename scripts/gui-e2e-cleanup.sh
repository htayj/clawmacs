#!/bin/sh

# Process cleanup shared by the GUI E2E harness and its focused shell test.
# The including script owns APP_PID/XVFB_PID and, when a child was launched in
# an isolated session, the corresponding APP_PGID/XVFB_PGID.

GUI_E2E_CLEANUP_DONE=0
GUI_E2E_WAIT_TIMED_OUT=0
GUI_E2E_WAIT_TIMER_PID=''
GUI_E2E_WAIT_CHILD_REAPED=0

gui_e2e_monotonic_milliseconds() {
  python3 -c 'import time; print(time.monotonic_ns() // 1_000_000)'
}

gui_e2e_cancel_wait_timer() {
  [ -n "$GUI_E2E_WAIT_TIMER_PID" ] || return 0
  kill -KILL "$GUI_E2E_WAIT_TIMER_PID" >/dev/null 2>&1 || true
  wait "$GUI_E2E_WAIT_TIMER_PID" >/dev/null 2>&1 || true
  GUI_E2E_WAIT_TIMER_PID=''
}

gui_e2e_process_exited_p() {
  gui_e2e_pid=$1

  if ! kill -0 "$gui_e2e_pid" >/dev/null 2>&1; then
    return 0
  fi

  # An exited child remains visible to kill -0 as a zombie until this shell
  # reaps it.  Linux /proc is available in the Guix container and lets the
  # bounded waiter distinguish that state from a genuinely live process.
  gui_e2e_state=''
  if [ -r "/proc/$gui_e2e_pid/stat" ]; then
    gui_e2e_state=$(awk '{print $3}' "/proc/$gui_e2e_pid/stat" 2>/dev/null) || \
      gui_e2e_state=''
  fi
  [ "$gui_e2e_state" = 'Z' ]
}

gui_e2e_process_group_id() {
  gui_e2e_group_pid=$1
  [ -r "/proc/$gui_e2e_group_pid/stat" ] || return 1
  awk '
    {
      line = $0
      sub(/^[0-9]+ \(.*\) /, "", line)
      split(line, fields, " ")
      if (length(fields[3])) {
        print fields[3]
        found = 1
      }
    }
    END { if (!found) exit 1 }
  ' "/proc/$gui_e2e_group_pid/stat" 2>/dev/null
}

gui_e2e_process_group_live_p() {
  gui_e2e_group_id=$1
  [ -n "$gui_e2e_group_id" ] || return 1

  # Query through the kernel rather than /proc.  Guix may give the container a
  # PID namespace while sharing the host's procfs, in which case namespace
  # PIDs accepted by kill(2) do not name the same /proc entries.
  kill -0 -- "-$gui_e2e_group_id" >/dev/null 2>&1
}

gui_e2e_wait_child_bounded() {
  gui_e2e_pid=$1
  gui_e2e_timeout_seconds=$2
  GUI_E2E_WAIT_TIMED_OUT=0
  GUI_E2E_WAIT_CHILD_REAPED=0

  # wait(1) is the only portable way for this shell to distinguish a live
  # child from its unreaped zombie.  Interrupt it with a private timer instead
  # of inspecting a possibly foreign procfs mount.
  gui_e2e_wait_parent_pid=$$
  trap 'GUI_E2E_WAIT_TIMED_OUT=1' USR1
  # Use one timer process rather than a shell plus an external sleep child.
  # Checking getppid() before delivery also prevents a timer surviving an
  # untrappable parent death from signalling a later process that reused the
  # shell's numeric PID.
  python3 -c '
import os
import signal
import sys
import time

delay = float(sys.argv[1])
parent = int(sys.argv[2])
time.sleep(delay)
if os.getppid() == parent:
    os.kill(parent, signal.SIGUSR1)
' "$gui_e2e_timeout_seconds" "$gui_e2e_wait_parent_pid" &
  GUI_E2E_WAIT_TIMER_PID=$!

  gui_e2e_child_status=0
  if wait "$gui_e2e_pid"; then
    gui_e2e_child_status=0
  else
    gui_e2e_child_status=$?
  fi

  gui_e2e_cancel_wait_timer
  trap - USR1

  if [ "$GUI_E2E_WAIT_TIMED_OUT" -eq 1 ]; then
    return 124
  fi
  GUI_E2E_WAIT_CHILD_REAPED=1
  return "$gui_e2e_child_status"
}

gui_e2e_wait_process_group_bounded() {
  gui_e2e_group_wait_id=$1
  gui_e2e_group_wait_timeout_seconds=$2
  gui_e2e_group_wait_attempts=$((gui_e2e_group_wait_timeout_seconds * 10))
  gui_e2e_group_wait_index=0
  GUI_E2E_WAIT_TIMED_OUT=0

  while [ "$gui_e2e_group_wait_index" -lt "$gui_e2e_group_wait_attempts" ]; do
    if ! gui_e2e_process_group_live_p "$gui_e2e_group_wait_id"; then
      return 0
    fi
    gui_e2e_group_wait_index=$((gui_e2e_group_wait_index + 1))
    sleep 0.1
  done
  if ! gui_e2e_process_group_live_p "$gui_e2e_group_wait_id"; then
    return 0
  fi
  GUI_E2E_WAIT_TIMED_OUT=1
  return 124
}

gui_e2e_wait_child_and_group_bounded() {
  gui_e2e_wait_pid=$1
  gui_e2e_wait_timeout_seconds=$2
  gui_e2e_wait_group_id=${3:-}
  GUI_E2E_WAIT_TIMED_OUT=0
  gui_e2e_wait_started_ms=$(gui_e2e_monotonic_milliseconds)
  gui_e2e_wait_deadline_ms=$((gui_e2e_wait_started_ms +
                              gui_e2e_wait_timeout_seconds * 1000))

  gui_e2e_wait_child_status=0
  if gui_e2e_wait_child_bounded \
       "$gui_e2e_wait_pid" "$gui_e2e_wait_timeout_seconds"; then
    gui_e2e_wait_child_status=0
  else
    gui_e2e_wait_child_status=$?
  fi
  if [ "$GUI_E2E_WAIT_TIMED_OUT" -eq 1 ]; then
    return 124
  fi

  if ! gui_e2e_process_group_live_p "$gui_e2e_wait_group_id"; then
    return "$gui_e2e_wait_child_status"
  fi

  gui_e2e_wait_now_ms=$(gui_e2e_monotonic_milliseconds)
  gui_e2e_wait_remaining_ms=$((gui_e2e_wait_deadline_ms -
                               gui_e2e_wait_now_ms))
  if [ "$gui_e2e_wait_remaining_ms" -le 0 ]; then
    GUI_E2E_WAIT_TIMED_OUT=1
    return 124
  fi
  gui_e2e_wait_attempts=$(((gui_e2e_wait_remaining_ms + 99) / 100))
  gui_e2e_wait_index=0

  while [ "$gui_e2e_wait_index" -lt "$gui_e2e_wait_attempts" ]; do
    if ! gui_e2e_process_group_live_p "$gui_e2e_wait_group_id"; then
      return "$gui_e2e_wait_child_status"
    fi
    gui_e2e_wait_index=$((gui_e2e_wait_index + 1))
    sleep 0.1
  done

  if ! gui_e2e_process_group_live_p "$gui_e2e_wait_group_id"; then
    return "$gui_e2e_wait_child_status"
  fi

  GUI_E2E_WAIT_TIMED_OUT=1
  return 124
}

gui_e2e_signal_process_group() {
  gui_e2e_signal_group_id=$1
  gui_e2e_signal_name=$2
  [ -n "$gui_e2e_signal_group_id" ] || return 0
  kill "-$gui_e2e_signal_name" -- "-$gui_e2e_signal_group_id" \
    >/dev/null 2>&1 || true
}

gui_e2e_terminate_child_bounded() {
  gui_e2e_terminate_pid=$1
  gui_e2e_terminate_timeout_seconds=${2:-2}
  gui_e2e_terminate_group_id=${3:-}

  if gui_e2e_process_exited_p "$gui_e2e_terminate_pid" && \
     ! gui_e2e_process_group_live_p "$gui_e2e_terminate_group_id"; then
    wait "$gui_e2e_terminate_pid" >/dev/null 2>&1 || true
    return 0
  fi

  gui_e2e_signal_process_group "$gui_e2e_terminate_group_id" TERM
  if [ -z "$gui_e2e_terminate_group_id" ] || \
     [ "$gui_e2e_terminate_pid" = "$gui_e2e_terminate_group_id" ]; then
    kill -TERM "$gui_e2e_terminate_pid" >/dev/null 2>&1 || true
  fi
  if gui_e2e_wait_child_and_group_bounded \
       "$gui_e2e_terminate_pid" "$gui_e2e_terminate_timeout_seconds" \
       "$gui_e2e_terminate_group_id"; then
    return 0
  fi
  if [ "$GUI_E2E_WAIT_TIMED_OUT" -ne 1 ]; then
    return 0
  fi
  gui_e2e_terminate_child_reaped=$GUI_E2E_WAIT_CHILD_REAPED

  # Emergency cleanup must itself be bounded.  A wedged application that
  # ignores TERM must not turn a useful failure artifact into a hung harness.
  # When SETSID --wait has a distinct owner PID, leave it alive to reap the
  # application group leader; kill that owner only if it remains wedged after
  # the exact application group has received KILL and a full settlement grace.
  gui_e2e_signal_process_group "$gui_e2e_terminate_group_id" KILL
  if [ -z "$gui_e2e_terminate_group_id" ] || \
     [ "$gui_e2e_terminate_pid" = "$gui_e2e_terminate_group_id" ]; then
    kill -KILL "$gui_e2e_terminate_pid" >/dev/null 2>&1 || true
  fi
  if [ "$gui_e2e_terminate_child_reaped" -eq 1 ]; then
    if gui_e2e_wait_process_group_bounded \
         "$gui_e2e_terminate_group_id" \
         "$gui_e2e_terminate_timeout_seconds"; then
      return 0
    fi
  else
    if gui_e2e_wait_child_and_group_bounded \
         "$gui_e2e_terminate_pid" "$gui_e2e_terminate_timeout_seconds" \
         "$gui_e2e_terminate_group_id"; then
      return 0
    fi
    if [ "$GUI_E2E_WAIT_TIMED_OUT" -ne 1 ]; then
      return 0
    fi
    gui_e2e_terminate_child_reaped=$GUI_E2E_WAIT_CHILD_REAPED
  fi

  if [ "$gui_e2e_terminate_child_reaped" -ne 1 ]; then
    kill -KILL "$gui_e2e_terminate_pid" >/dev/null 2>&1 || true
    gui_e2e_wait_child_and_group_bounded \
      "$gui_e2e_terminate_pid" "$gui_e2e_terminate_timeout_seconds" \
      "$gui_e2e_terminate_group_id" >/dev/null 2>&1 || true
  fi
  # KILL can still be delayed by an uninterruptible kernel wait.  Leave the
  # failed child for the container supervisor rather than waiting forever.
  return 0
}

gui_e2e_cleanup_children() {
  if [ "$GUI_E2E_CLEANUP_DONE" -eq 1 ]; then
    return 0
  fi
  GUI_E2E_CLEANUP_DONE=1

  if [ -n "${APP_PID:-}" ]; then
    gui_e2e_terminate_child_bounded "$APP_PID" 2 "${APP_PGID:-}"
    APP_PID=''
    APP_PGID=''
  fi
  if [ -n "${XVFB_PID:-}" ]; then
    gui_e2e_terminate_child_bounded "$XVFB_PID" 2 "${XVFB_PGID:-}"
    XVFB_PID=''
    XVFB_PGID=''
  fi
}

gui_e2e_exit_cleanup() {
  gui_e2e_status=$?
  # Prevent EXIT recursion and ignore a second interactive signal while child
  # termination and reaping are already in progress.
  trap - EXIT
  trap '' INT TERM
  gui_e2e_cancel_wait_timer
  gui_e2e_cleanup_children
  exit "$gui_e2e_status"
}

gui_e2e_install_cleanup_traps() {
  trap gui_e2e_exit_cleanup EXIT
  trap 'exit 130' INT
  trap 'exit 143' TERM
}
