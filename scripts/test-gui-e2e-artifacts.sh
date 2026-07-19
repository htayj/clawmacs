#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$SCRIPT_DIR/gui-e2e-artifacts.sh"

fail() {
  printf 'GUI E2E artifact regression: %s\n' "$*" >&2
  exit 1
}

tmp_dir=$(mktemp -d)
trap 'rm -rf -- "$tmp_dir"' EXIT INT TERM
artifact_dir="$tmp_dir/artifacts"
mkdir -p "$artifact_dir/home" "$artifact_dir/cache" \
  "$artifact_dir/screenshots/nested"

printf 'preserve home\n' > "$artifact_dir/home/keep"
printf 'preserve cache\n' > "$artifact_dir/cache/keep"
printf '%s\n' \
  'prefix [clawmacs-e2e] {"event":"frame-ready","sequence":999}' \
  > "$artifact_dir/debug.log"
printf '999999\n' > "$artifact_dir/window.id"
printf 'stale screenshot\n' > "$artifact_dir/screenshots/nested/failure.png"

for path in \
  actions.jsonl app.pgid app.stderr app.stderr.tail app.stdout debug.tail \
  driver.stderr driver.stderr.tail driver.stdout harness.log summary.json \
  xvfb.display xvfb.log; do
  printf 'stale\n' > "$artifact_dir/$path"
done

gui_e2e_reset_run_artifacts "$artifact_dir"

test -f "$artifact_dir/home/keep" || fail 'artifact-private HOME was removed'
test -f "$artifact_dir/cache/keep" || fail 'artifact-private cache was removed'
test ! -e "$artifact_dir/debug.log" || fail 'stale frame-ready log survived'
test ! -e "$artifact_dir/window.id" || fail 'stale window id survived'
test ! -e "$artifact_dir/screenshots" || fail 'stale screenshots survived'

for path in \
  actions.jsonl app.pgid app.stderr app.stderr.tail app.stdout debug.tail \
  driver.stderr driver.stderr.tail driver.stdout harness.log summary.json \
  xvfb.display xvfb.log; do
  test ! -e "$artifact_dir/$path" || fail "stale run output survived: $path"
done

printf 'GUI E2E artifact regression passed\n'
