#!/bin/sh

# Artifact lifecycle helpers shared by the GUI E2E harness and focused tests.

gui_e2e_reset_run_artifacts() {
  artifact_dir=$1
  [ -n "$artifact_dir" ] || return 2

  # HOME and the Common Lisp cache intentionally survive a rerun.  Everything
  # below belongs to one harness invocation and must not satisfy readiness or
  # driver assertions in a later invocation that reuses the same directory.
  rm -f -- \
    "$artifact_dir/actions.jsonl" \
    "$artifact_dir/app.pgid" \
    "$artifact_dir/app.stderr" \
    "$artifact_dir/app.stderr.tail" \
    "$artifact_dir/app.stdout" \
    "$artifact_dir/debug.log" \
    "$artifact_dir/debug.tail" \
    "$artifact_dir/driver.stderr" \
    "$artifact_dir/driver.stderr.tail" \
    "$artifact_dir/driver.stdout" \
    "$artifact_dir/harness.log" \
    "$artifact_dir/summary.json" \
    "$artifact_dir/window.id" \
    "$artifact_dir/xvfb.display" \
    "$artifact_dir/xvfb.log"
  rm -rf -- "$artifact_dir/screenshots"
}
