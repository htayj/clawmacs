#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)
LAUNCHER="$SCRIPT_DIR/guix-container.sh"
TEST_NAME="session-migration-container-test-$$"
HOST_ROOT="${TMPDIR:-/tmp}/$TEST_NAME"
CONTAINER_ROOT="$HOST_ROOT"
HOST_LEGACY="$HOST_ROOT/clawmacs/sessions"
HOST_CANONICAL="$HOST_ROOT/rplaca/sessions"
HOST_BARRIER="$HOST_ROOT/barrier"
CONTAINER_LEGACY="$CONTAINER_ROOT/clawmacs/sessions/"
CONTAINER_CANONICAL="$CONTAINER_ROOT/rplaca/sessions/"
CONTAINER_BARRIER="$CONTAINER_ROOT/barrier/"
ENTRY=/workspace/tests/session-migration-subprocess.lisp
ONE_LOG="$HOST_ROOT/one.log"
TWO_LOG="$HOST_ROOT/two.log"
one_pid=''
two_pid=''

if [ -z "${RPLACA_SSL_LIB:-}" ]; then
  ssl_file=$(find /gnu/store -path '*/lib/libssl.so' -print -quit)
  if [ -z "$ssl_file" ]; then
    printf 'could not locate a Guix OpenSSL library for launcher test\n' >&2
    exit 1
  fi
  RPLACA_SSL_LIB=${ssl_file%/*}
  export RPLACA_SSL_LIB
fi

cleanup() {
  status=$?
  [ -n "$one_pid" ] && kill "$one_pid" >/dev/null 2>&1 || true
  [ -n "$two_pid" ] && kill "$two_pid" >/dev/null 2>&1 || true
  if [ "$status" -ne 0 ]; then
    [ -f "$ONE_LOG" ] && tail -80 "$ONE_LOG" >&2 || true
    [ -f "$TWO_LOG" ] && tail -80 "$TWO_LOG" >&2 || true
  fi
  chmod -R u+w "$HOST_ROOT" >/dev/null 2>&1 || true
  rm -rf "$HOST_ROOT"
  return "$status"
}
trap cleanup EXIT HUP INT TERM

wait_for_file() {
  path="$1"
  count=0
  while [ ! -f "$path" ]; do
    count=$((count + 1))
    if [ "$count" -ge 2400 ]; then
      printf 'timed out waiting for %s\n' "$path" >&2
      return 1
    fi
    sleep 0.05
  done
}

mkdir -p "$HOST_LEGACY/locked/nested" "$HOST_BARRIER"
printf 'cross-container\n' >"$HOST_LEGACY/locked/nested/payload.json"
chmod 400 "$HOST_LEGACY/locked/nested/payload.json"
chmod 555 "$HOST_LEGACY/locked/nested" "$HOST_LEGACY/locked" "$HOST_LEGACY"

XDG_STATE_HOME="$HOST_ROOT" "$LAUNCHER" --mode run -- \
  env \
  "RPLACA_TEST_REPO_ROOT=/workspace/" \
  "RPLACA_TEST_CANONICAL_SESSIONS=$CONTAINER_CANONICAL" \
  "RPLACA_TEST_LEGACY_SESSIONS=$CONTAINER_LEGACY" \
  "RPLACA_TEST_SESSION_BARRIER=$CONTAINER_BARRIER" \
  "RPLACA_TEST_SESSION_BARRIER_COUNT=1" \
  "RPLACA_TEST_SESSION_WORKER_ID=one" \
  "RPLACA_TEST_SESSION_HOLD_BEFORE_PUBLISH=1" \
  sbcl --noinform --disable-debugger --script "$ENTRY" \
  >"$ONE_LOG" 2>&1 &
one_pid=$!

wait_for_file "$HOST_BARRIER/holding-one"
first_stage=$(find "$HOST_ROOT/rplaca" -maxdepth 1 -type d \
  -name '.sessions-migration-*' -print -quit)
[ -n "$first_stage" ]

XDG_STATE_HOME="$HOST_ROOT" "$LAUNCHER" --mode run -- \
  env \
  "RPLACA_TEST_REPO_ROOT=/workspace/" \
  "RPLACA_TEST_CANONICAL_SESSIONS=$CONTAINER_CANONICAL" \
  "RPLACA_TEST_LEGACY_SESSIONS=$CONTAINER_LEGACY" \
  "RPLACA_TEST_SESSION_BARRIER=$CONTAINER_BARRIER" \
  "RPLACA_TEST_SESSION_BARRIER_COUNT=1" \
  "RPLACA_TEST_SESSION_WORKER_ID=two" \
  sbcl --noinform --disable-debugger --script "$ENTRY" \
  >"$TWO_LOG" 2>&1 &
two_pid=$!

wait_for_file "$HOST_BARRIER/ready-two"
sleep 1
kill -0 "$one_pid"
[ -d "$first_stage" ]

touch "$HOST_BARRIER/release-one"
wait "$one_pid"
one_pid=''
wait "$two_pid"
two_pid=''

grep -qx 'pid=1' "$HOST_BARRIER/started-one"
grep -qx 'pid=1' "$HOST_BARRIER/started-two"
if cmp -s "$HOST_BARRIER/started-one" "$HOST_BARRIER/started-two"; then
  printf 'independent containers unexpectedly reported identical proc state\n' >&2
  exit 1
fi

grep -q '^rplaca-session-migration-v1$' \
  "$HOST_CANONICAL/.rplaca-session-migration-complete"
[ "$(cat "$HOST_CANONICAL/locked/nested/payload.json")" = cross-container ]
[ "$(stat -c %a "$HOST_CANONICAL")" = 555 ]
[ "$(stat -c %a "$HOST_CANONICAL/locked")" = 555 ]
[ "$(stat -c %a "$HOST_CANONICAL/locked/nested")" = 555 ]
[ "$(stat -c %a "$HOST_CANONICAL/locked/nested/payload.json")" = 400 ]
if find "$HOST_ROOT/rplaca" -maxdepth 1 -type d \
    -name '.sessions-migration-*' | grep -q .; then
  printf 'session migration staging tree remained after publication\n' >&2
  exit 1
fi

printf 'session-migration-containers: two independent publishers passed\n'
