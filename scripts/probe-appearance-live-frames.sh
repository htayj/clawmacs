#!/bin/sh
# Execute the real two-frame CLX proof only in Guix e2e with a private Xvfb.
set -eu

probe_test_double=${CLAWMACS_PROBE_TEST_DOUBLE:-}

if [ "${CLAWMACS_IN_GUIX_CONTAINER:-0}" != "1" ]; then
  # The launcher starts this exact script once more in the container.  Do not
  # call the launcher from that inner invocation: recursive containers obscure
  # failures and can leave an owned process group behind.
  if [ -n "$probe_test_double" ]; then
    exec "$probe_test_double" launcher \
      ./scripts/guix-container.sh --mode e2e -- \
      sh scripts/probe-appearance-live-frames.sh --inner
  fi
  export CLAWMACS_CONTAINER_DISABLE_HOST_X=1
  exec ./scripts/guix-container.sh --mode e2e -- \
    sh scripts/probe-appearance-live-frames.sh --inner
fi

if [ "${1:-}" != "--inner" ]; then
  echo "probe must be entered by the Guix e2e launcher" >&2
  exit 64
fi

if [ -n "$probe_test_double" ]; then
  "$probe_test_double" inner
fi

exec sh -lc '
  set -eu
  tmp=$(mktemp -d)
  payload_timeout_tenths=${CLAWMACS_PROBE_PAYLOAD_TIMEOUT_TENTHS:-1800}
  termination_grace_tenths=${CLAWMACS_PROBE_TERMINATION_GRACE_TENTHS:-50}
  case "$payload_timeout_tenths:$termination_grace_tenths" in
    *[!0-9:]*|:*|*:) echo "invalid probe timeout" >&2; exit 64 ;;
  esac
  group_live() { kill -0 -- "-$1" 2>/dev/null; }
  terminate_group() {
    group=$1
    leader=$2
    kill -TERM -- "-$group" 2>/dev/null || true
    n=0
    while group_live "$group" && [ "$n" -lt "$termination_grace_tenths" ]; do
      n=$((n + 1))
      sleep 0.1
    done
    if group_live "$group"; then
      kill -KILL -- "-$group" 2>/dev/null || true
      n=0
      while group_live "$group" && [ "$n" -lt "$termination_grace_tenths" ]; do
        n=$((n + 1))
        sleep 0.1
      done
    fi
    group_live "$group" && return 1
    wait "$leader" 2>/dev/null || true
  }
  terminate_pid() {
    owned_pid=$1
    kill -TERM "$owned_pid" 2>/dev/null || true
    n=0
    while kill -0 "$owned_pid" 2>/dev/null &&
          [ "$n" -lt "$termination_grace_tenths" ]; do
      n=$((n + 1))
      sleep 0.1
    done
    if kill -0 "$owned_pid" 2>/dev/null; then
      kill -KILL "$owned_pid" 2>/dev/null || true
      n=0
      while kill -0 "$owned_pid" 2>/dev/null &&
            [ "$n" -lt "$termination_grace_tenths" ]; do
        n=$((n + 1))
        sleep 0.1
      done
    fi
    kill -0 "$owned_pid" 2>/dev/null && return 1
    wait "$owned_pid" 2>/dev/null || true
  }
  cleanup() {
    if [ -n "${payload_pgid:-}" ]; then
      terminate_group "$payload_pgid" "${payload_pid:-$payload_pgid}" || true
    fi
    if [ -n "${xvfb_pid:-}" ]; then
      terminate_pid "$xvfb_pid" || true
    fi
    rm -rf "$tmp"
  }
  trap cleanup EXIT INT TERM
  if [ -n "${CLAWMACS_PROBE_TEST_DOUBLE:-}" ]; then
    "$CLAWMACS_PROBE_TEST_DOUBLE" xvfb \
      3>"$tmp/display" >"$tmp/xvfb.log" 2>&1 &
  else
    Xvfb -displayfd 3 -screen 0 1280x800x24 -nolisten tcp -ac \
      3>"$tmp/display" >"$tmp/xvfb.log" 2>&1 &
  fi
  xvfb_pid=$!
  for n in $(seq 1 100); do test -s "$tmp/display" && break; sleep 0.05; done
  if [ ! -s "$tmp/display" ]; then
    cat "$tmp/xvfb.log" >&2
    exit 1
  fi
  export DISPLAY=":$(tr -d "\r\n" < "$tmp/display")"
  if [ -n "${CLAWMACS_PROBE_TEST_DOUBLE:-}" ]; then
    setsid env CLAWMACS_PROBE_KIND=appearance \
      "$CLAWMACS_PROBE_TEST_DOUBLE" sbcl >"$tmp/payload.log" 2>&1 &
  else
    setsid sbcl --noinform --non-interactive \
      --load "$CLAWMACS_QUICKLISP_SETUP" \
      --eval "(push (truename \".\") asdf:*central-registry*)" \
      --load scripts/probe-appearance-live-frames.lisp --eval "(quit)" \
      >"$tmp/payload.log" 2>&1 &
  fi
  payload_pid=$!
  payload_pgid=$payload_pid
  n=0
  while kill -0 "$payload_pid" 2>/dev/null &&
        [ "$n" -lt "$payload_timeout_tenths" ]; do
    n=$((n + 1))
    sleep 0.1
  done
  if kill -0 "$payload_pid" 2>/dev/null; then
    echo "two-frame payload exceeded its bounded deadline" >&2
    terminate_group "$payload_pgid" "$payload_pid" || {
      echo "owned SBCL process group survived TERM/KILL" >&2
    }
    cat "$tmp/payload.log"
    exit 1
  fi
  if ! wait "$payload_pid"; then
    cat "$tmp/payload.log"
    exit 1
  fi
  for marker in \
    APPEARANCE_LIVE_TWO_FRAME_OK \
    APPEARANCE_LIVE_STAGED_PROFILES_OK \
    APPEARANCE_LIVE_FONT_INVENTORIES_OK \
    APPEARANCE_LIVE_SAFE_ACTIVATION_OK \
    APPEARANCE_LIVE_ACTIVATION_ROLLBACK_OK \
    APPEARANCE_LIVE_PACKAGE_PUBLISH_OK \
    APPEARANCE_LIVE_PACKAGE_REMOVAL_ROLLBACK_OK \
    APPEARANCE_LIVE_TWO_FRAME_TEARDOWN_OK
  do
    grep -q "$marker" "$tmp/payload.log"
  done
  cat "$tmp/payload.log"
  if kill -0 -- "-$payload_pgid" 2>/dev/null; then
    echo "owned SBCL process group survived" >&2
    exit 1
  fi
  payload_pgid=
  if ! terminate_pid "$xvfb_pid"; then
    echo "owned Xvfb survived" >&2
    exit 1
  fi
  xvfb_pid=
  printf "%s\\n" \
    "APPEARANCE_LIVE_PROCESS_GROUP_OK sbcl-group-empty=true xvfb-stopped=true"
  printf "%s\\n" APPEARANCE_LIVE_TWO_FRAME_SHELL_OK
'
