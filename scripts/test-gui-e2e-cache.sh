#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$SCRIPT_DIR/gui-e2e-cache.sh"

TMP_DIR=$(mktemp -d)
SOURCE_CACHE="$TMP_DIR/shared-xdg"
LOCK_FILE="$SOURCE_CACHE/quicklisp.lock"

cleanup() {
  rm -rf "$TMP_DIR"
}

trap cleanup EXIT INT TERM

mkdir -p "$SOURCE_CACHE/common-lisp/system-a"
printf 'seed-v1\n' >"$SOURCE_CACHE/common-lisp/system-a/component.fasl"
printf 'must-not-copy\n' >"$SOURCE_CACHE/unrelated-state"
touch -t 202001010000 "$SOURCE_CACHE/common-lisp/system-a/component.fasl"

PRIVATE_ONE="$TMP_DIR/private-one"
gui_e2e_seed_private_common_lisp_cache \
  "$SOURCE_CACHE" "$PRIVATE_ONE" "$LOCK_FILE"
if [ "$GUI_E2E_CACHE_SEED_STATUS" != seeded ]; then
  echo "FAIL private-cache-seed: status $GUI_E2E_CACHE_SEED_STATUS" >&2
  exit 1
fi
if [ "$(cat "$PRIVATE_ONE/common-lisp/system-a/component.fasl")" != seed-v1 ]; then
  echo 'FAIL private-cache-seed: copied FASL content differs' >&2
  exit 1
fi
if [ -e "$PRIVATE_ONE/unrelated-state" ]; then
  echo 'FAIL private-cache-seed: copied data outside common-lisp' >&2
  exit 1
fi
source_inode=$(stat -c '%d:%i' "$SOURCE_CACHE/common-lisp/system-a/component.fasl")
private_inode=$(stat -c '%d:%i' "$PRIVATE_ONE/common-lisp/system-a/component.fasl")
if [ "$source_inode" = "$private_inode" ]; then
  echo 'FAIL private-cache-isolation: source and destination are hard-linked' >&2
  exit 1
fi
printf 'private-only\n' >"$PRIVATE_ONE/common-lisp/system-a/component.fasl"
if [ "$(cat "$SOURCE_CACHE/common-lisp/system-a/component.fasl")" != seed-v1 ]; then
  echo 'FAIL private-cache-isolation: private mutation changed shared seed' >&2
  exit 1
fi

if gui_e2e_seed_private_common_lisp_cache \
     "$SOURCE_CACHE" "$SOURCE_CACHE/nested-private" "$LOCK_FILE"; then
  echo 'FAIL overlapping-cache-roots: nested destination was accepted' >&2
  exit 1
fi
if [ "$GUI_E2E_CACHE_SEED_STATUS" != seed-failed ] || \
   [ -e "$SOURCE_CACHE/nested-private/common-lisp" ]; then
  echo 'FAIL overlapping-cache-roots: unsafe destination was touched' >&2
  exit 1
fi

PRIVATE_COLD="$TMP_DIR/private-cold"
mkdir -p "$PRIVATE_COLD/common-lisp"
printf 'stale-cache\n' >"$PRIVATE_COLD/common-lisp/stale.fasl"
printf 'preserve-sibling\n' >"$PRIVATE_COLD/unrelated-state"
RPLACA_GUI_E2E_COLD_CACHE=1
export RPLACA_GUI_E2E_COLD_CACHE
gui_e2e_seed_private_common_lisp_cache \
  "$SOURCE_CACHE" "$PRIVATE_COLD" "$LOCK_FILE"
unset RPLACA_GUI_E2E_COLD_CACHE
if [ "$GUI_E2E_CACHE_SEED_STATUS" != cold-override ] || \
   [ -e "$PRIVATE_COLD/common-lisp" ]; then
  echo 'FAIL cold-cache-override: existing Common Lisp cache survived' >&2
  exit 1
fi
if [ "$(cat "$PRIVATE_COLD/unrelated-state")" != preserve-sibling ]; then
  echo 'FAIL cold-cache-override: unrelated private state was removed' >&2
  exit 1
fi

PRIVATE_EXISTING="$TMP_DIR/private-existing"
mkdir -p "$PRIVATE_EXISTING/common-lisp"
printf 'keep-me\n' >"$PRIVATE_EXISTING/common-lisp/existing.fasl"
gui_e2e_seed_private_common_lisp_cache \
  "$SOURCE_CACHE" "$PRIVATE_EXISTING" "$LOCK_FILE"
if [ "$GUI_E2E_CACHE_SEED_STATUS" != private-cache-present ] || \
   [ "$(cat "$PRIVATE_EXISTING/common-lisp/existing.fasl")" != keep-me ]; then
  echo 'FAIL existing-private-cache: existing cache was replaced' >&2
  exit 1
fi

PRIVATE_INVALIDATION="$TMP_DIR/private-invalidation"
gui_e2e_seed_private_common_lisp_cache \
  "$SOURCE_CACHE" "$PRIVATE_INVALIDATION" "$LOCK_FILE"
SOURCE_FILE="$TMP_DIR/newer-source.lisp"
printf '(in-package :cl-user)\n' >"$SOURCE_FILE"
touch -t 202101010000 "$SOURCE_FILE"
seeded_mtime=$(stat -c '%Y' \
  "$PRIVATE_INVALIDATION/common-lisp/system-a/component.fasl")
source_mtime=$(stat -c '%Y' "$SOURCE_FILE")
if [ "$seeded_mtime" -ge "$source_mtime" ]; then
  echo 'FAIL source-invalidation: copied FASL timestamp masks newer source' >&2
  exit 1
fi

LOCK_READY="$TMP_DIR/lock-ready"
LOCK_RELEASE="$TMP_DIR/lock-release"
(
  exec 8>>"$LOCK_FILE"
  flock -x 8
  printf 'ready\n' >"$LOCK_READY"
  while [ ! -e "$LOCK_RELEASE" ]; do
    sleep 0.01
  done
) &
lock_owner=$!
attempt=0
while [ ! -s "$LOCK_READY" ] && [ "$attempt" -lt 200 ]; do
  attempt=$((attempt + 1))
  sleep 0.01
done
if [ ! -s "$LOCK_READY" ]; then
  echo 'FAIL seed-lock: exclusive owner did not acquire lock' >&2
  exit 1
fi

PRIVATE_TIMED_OUT="$TMP_DIR/private-timed-out"
gui_e2e_seed_private_common_lisp_cache \
  "$SOURCE_CACHE" "$PRIVATE_TIMED_OUT" "$LOCK_FILE" 1
if [ "$GUI_E2E_CACHE_SEED_STATUS" != seed-failed ]; then
  echo "FAIL seed-lock-timeout: status $GUI_E2E_CACHE_SEED_STATUS" >&2
  exit 1
fi
if [ -e "$PRIVATE_TIMED_OUT/common-lisp" ] || \
   find "$PRIVATE_TIMED_OUT" -name '.common-lisp-seed.*' -print -quit \
     2>/dev/null | grep -q .; then
  echo 'FAIL seed-lock-timeout: unlocked copy or stage was published' >&2
  exit 1
fi

PRIVATE_BLOCKED="$TMP_DIR/private-blocked"
(
  gui_e2e_seed_private_common_lisp_cache \
    "$SOURCE_CACHE" "$PRIVATE_BLOCKED" "$LOCK_FILE"
  printf '%s\n' "$GUI_E2E_CACHE_SEED_STATUS" >"$TMP_DIR/blocked-status"
) &
blocked_seed=$!
sleep 0.1
if [ -e "$PRIVATE_BLOCKED/common-lisp" ] || [ -e "$TMP_DIR/blocked-status" ]; then
  echo 'FAIL seed-lock: seed did not wait for the warmup lock' >&2
  exit 1
fi
touch "$LOCK_RELEASE"
wait "$lock_owner"
wait "$blocked_seed"
if [ "$(cat "$TMP_DIR/blocked-status")" != seeded ]; then
  echo 'FAIL seed-lock: blocked seed did not complete' >&2
  exit 1
fi

PRIVATE_PARALLEL_ONE="$TMP_DIR/private-parallel-one"
PRIVATE_PARALLEL_TWO="$TMP_DIR/private-parallel-two"
(
  gui_e2e_seed_private_common_lisp_cache \
    "$SOURCE_CACHE" "$PRIVATE_PARALLEL_ONE" "$LOCK_FILE"
  printf '%s\n' "$GUI_E2E_CACHE_SEED_STATUS" >"$TMP_DIR/parallel-one-status"
) &
parallel_one=$!
(
  gui_e2e_seed_private_common_lisp_cache \
    "$SOURCE_CACHE" "$PRIVATE_PARALLEL_TWO" "$LOCK_FILE"
  printf '%s\n' "$GUI_E2E_CACHE_SEED_STATUS" >"$TMP_DIR/parallel-two-status"
) &
parallel_two=$!
wait "$parallel_one"
wait "$parallel_two"
if [ "$(cat "$TMP_DIR/parallel-one-status")" != seeded ] || \
   [ "$(cat "$TMP_DIR/parallel-two-status")" != seeded ]; then
  echo 'FAIL parallel-cache-seed: one seed did not complete' >&2
  exit 1
fi
if [ "$(cat "$PRIVATE_PARALLEL_ONE/common-lisp/system-a/component.fasl")" != seed-v1 ] || \
   [ "$(cat "$PRIVATE_PARALLEL_TWO/common-lisp/system-a/component.fasl")" != seed-v1 ]; then
  echo 'FAIL parallel-cache-seed: copied content differs' >&2
  exit 1
fi

echo 'gui-e2e cache tests passed'
