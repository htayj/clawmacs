#!/bin/sh
# Prove each GUI probe crosses the Guix launcher boundary exactly once.
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_dir=$(mktemp -d)
cleanup() { rm -rf "$test_dir"; }
trap cleanup EXIT INT TERM

assert_one() {
  event=$1
  count=$(awk -v event="$event" '$0 == event { count++ } END { print count + 0 }' "$state")
  [ "$count" -eq 1 ] || {
    echo "expected one $event event, got $count" >&2
    exit 1
  }
}

for probe in probe-clx-font-inventory.sh probe-appearance-live-frames.sh; do
  state="$test_dir/${probe}.events"
  : >"$state"
  RPLACA_PROBE_TEST_STATE="$state" \
  RPLACA_PROBE_TEST_DOUBLE="$repo_root/scripts/probe-wrapper-test-double.sh" \
    "$repo_root/scripts/$probe" >"$test_dir/${probe}.log" 2>&1
  assert_one launcher
  assert_one inner
  assert_one xvfb
  assert_one sbcl
done

# Both wrappers must bound a payload whose leader and descendant ignore TERM,
# escalate to the complete owned process group, and leave neither process.
for probe in probe-clx-font-inventory.sh probe-appearance-live-frames.sh; do
  state="$test_dir/${probe}.stuck.events"
  : >"$state"
  if RPLACA_PROBE_TEST_STATE="$state" \
     RPLACA_PROBE_TEST_HANG=1 \
     RPLACA_PROBE_PAYLOAD_TIMEOUT_TENTHS=2 \
     RPLACA_PROBE_TERMINATION_GRACE_TENTHS=2 \
     RPLACA_PROBE_TEST_DOUBLE="$repo_root/scripts/probe-wrapper-test-double.sh" \
       "$repo_root/scripts/$probe" >"$test_dir/${probe}.stuck.log" 2>&1; then
    echo "stuck $probe unexpectedly succeeded" >&2
    exit 1
  fi
  grep -q "payload exceeded its bounded deadline" \
    "$test_dir/${probe}.stuck.log"
  for entry in $(awk -F= '/^stuck-(leader|child)=/ { print $2 }' "$state"); do
    if kill -0 "$entry" 2>/dev/null; then
      echo "stuck $probe process survived: $entry" >&2
      exit 1
    fi
  done
done

printf '%s\n' \
  'PROBE_WRAPPER_ENTRY_OK launcher=1 inner=1 sentinel=true xvfb=1 sbcl=1 stuck-groups-empty=true'
