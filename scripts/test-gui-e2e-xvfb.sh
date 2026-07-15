#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$SCRIPT_DIR/gui-e2e-container-retry.sh"
if ! REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null); then
  REPO_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
fi

fail() {
  printf 'GUI E2E Xvfb regression: %s\n' "$*" >&2
  exit 1
}

tmp_dir=$(mktemp -d)
artifact_rel=".artifacts/gui-e2e-xvfb-test-$$"
artifact_dir="$REPO_ROOT/$artifact_rel"
cleanup() {
  rm -rf -- "$tmp_dir" "$artifact_dir"
}
trap cleanup EXIT INT TERM

fake_launcher="$tmp_dir/fake-launcher.sh"
cat > "$fake_launcher" <<'EOF'
#!/bin/sh
set -eu
mode=$1
count_file=$2
count=0
if test -f "$count_file"; then
  count=$(sed -n '1p' "$count_file")
fi
count=$((count + 1))
printf '%s\n' "$count" > "$count_file"
case "$mode" in
  recover)
    test "$count" -gt 1 || exit 75
    exit 0
    ;;
  persistent)
    exit 75
    ;;
  unrelated)
    exit 42
    ;;
esac
exit 99
EOF
chmod +x "$fake_launcher"

recover_count="$tmp_dir/recover.count"
gui_e2e_run_container_with_retry \
  "$fake_launcher" recover "$recover_count" || \
  fail 'one transient namespace failure did not recover'
test "$(sed -n '1p' "$recover_count")" -eq 2 || \
  fail 'transient namespace failure did not run exactly twice'

persistent_count="$tmp_dir/persistent.count"
persistent_status=0
if gui_e2e_run_container_with_retry \
     "$fake_launcher" persistent "$persistent_count"; then
  persistent_status=0
else
  persistent_status=$?
fi
test "$persistent_status" -eq 75 || \
  fail "persistent namespace failure returned $persistent_status"
test "$(sed -n '1p' "$persistent_count")" -eq 2 || \
  fail 'persistent namespace failure was not bounded at two attempts'

unrelated_count="$tmp_dir/unrelated.count"
unrelated_status=0
if gui_e2e_run_container_with_retry \
     "$fake_launcher" unrelated "$unrelated_count"; then
  unrelated_status=0
else
  unrelated_status=$?
fi
test "$unrelated_status" -eq 42 || \
  fail "unrelated launcher failure returned $unrelated_status"
test "$(sed -n '1p' "$unrelated_count")" -eq 1 || \
  fail 'unrelated launcher failure was retried'

command -v guix >/dev/null 2>&1 || fail 'guix is unavailable'
socket_source="$tmp_dir/read-only-x11-socket-dir"
mkdir -m 1777 "$socket_source"

namespace_status=0
if guix shell -f "$REPO_ROOT/guix.scm" \
     --container --no-cwd --network \
     --share="$REPO_ROOT=/workspace" \
     --expose="$socket_source=/tmp/.X11-unix" \
     -- sh /workspace/scripts/run-gui-e2e.sh \
       --inside-container \
       --suite smoke \
       --artifact-dir "/workspace/$artifact_rel"; then
  namespace_status=0
else
  namespace_status=$?
fi
test "$namespace_status" -eq 75 || \
  fail "hostile socket namespace returned $namespace_status"
test -f "$artifact_dir/summary.json" || \
  fail 'hostile socket namespace did not write a summary'
grep -q 'Xvfb socket directory pre-exists before startup' \
  "$artifact_dir/summary.json" || \
  fail 'hostile socket namespace summary lacks the diagnostic'
test ! -e "$artifact_dir/xvfb.log" || \
  fail 'hostile socket namespace launched Xvfb'
test ! -s "$artifact_dir/xvfb.display" || \
  fail 'hostile socket namespace allocated a display'
if find "$socket_source" -mindepth 1 -print -quit | grep -q .; then
  fail 'hostile socket namespace probe changed the exposed directory'
fi

printf 'GUI E2E Xvfb regression passed\n'
