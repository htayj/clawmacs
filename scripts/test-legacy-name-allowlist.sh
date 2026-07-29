#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
GATE="$SCRIPT_DIR/check-legacy-name-allowlist.sh"
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT HUP INT TERM

new_repo() {
  name=$1
  repo="$TMP_DIR/$name"
  mkdir -p "$repo/scripts"
  git -C "$repo" init -q
  cp "$GATE" "$repo/scripts/check-legacy-name-allowlist.sh"
  chmod +x "$repo/scripts/check-legacy-name-allowlist.sh"
  printf '%s\n' "$repo"
}

run_gate() {
  repo=$1
  shift
  "$repo/scripts/check-legacy-name-allowlist.sh" \
    --repo "$repo" --allowlist "$repo/allowlist.tsv" "$@"
}

expect_failure() {
  label=$1
  expected=$2
  repo=$3
  if run_gate "$repo" >"$repo/output" 2>&1; then
    printf 'FAIL %s: gate unexpectedly passed\n' "$label" >&2
    exit 1
  fi
  if ! grep -Fq "$expected" "$repo/output"; then
    printf 'FAIL %s: expected %s\n' "$label" "$expected" >&2
    cat "$repo/output" >&2
    exit 1
  fi
}

repo=$(new_repo allowed)
printf '%s\n' \
  'scripts/check-legacy-name-allowlist.sh|product,font,url|product=8,font=4,url=1|policy|Policy implementation.' \
  'legacy.txt|product|product=1|fixture|Former product fixture.' \
  'legacy-font.txt|font|font=1|fixture|Former font fixture.' \
  >"$repo/allowlist.tsv"
printf 'clawmacs compatibility fixture\n' >"$repo/legacy.txt"
printf 'import demo.clawfont only\n' >"$repo/legacy-font.txt"
git -C "$repo" add .
run_gate "$repo" >"$repo/pass-output"
grep -Fq 'legacy-name gate:' "$repo/pass-output"

printf 'another clawmacs compatibility fixture\n' >>"$repo/legacy.txt"
git -C "$repo" add legacy.txt
expect_failure allowed-path-count \
  'occurrence count mismatch: legacy.txt' "$repo"
printf 'clawmacs compatibility fixture\n' >"$repo/legacy.txt"
git -C "$repo" add legacy.txt

printf 'current-facing clawmacs reference\n' >"$repo/current.md"
git -C "$repo" add current.md
expect_failure new-current-reference 'current.md' "$repo"

printf '%s\n' \
  'scripts/check-legacy-name-allowlist.sh|product,font,url|product=8,font=4,url=1|policy|Policy implementation.' \
  'legacy.txt|product|product=1|fixture|Former product fixture.' \
  'legacy-font.txt|font|font=1|fixture|Former font fixture.' \
  'current.md|product|product=1|fixture|Product-name fixture only.' \
  >"$repo/allowlist.tsv"
printf 'stale https://github.com/htayj/clawmacs URL\n' >"$repo/current.md"
git -C "$repo" add allowlist.tsv current.md
expect_failure kind-separation 'kind url' "$repo"

git -C "$repo" rm -q -f current.md
printf 'clean filename fixture\n' >"$repo/new-clawmacs-note.md"
git -C "$repo" add new-clawmacs-note.md
expect_failure filename-detection 'filename' "$repo"

repo=$(new_repo wildcard)
printf '%s\n' \
  'scripts/check-legacy-name-allowlist.sh|product,font,url|product=8,font=4,url=1|policy|Policy implementation.' \
  'docs/*|product|product=1|invalid|Wildcards must never be accepted.' \
  >"$repo/allowlist.tsv"
mkdir -p "$repo/docs"
printf 'clawmacs fixture\n' >"$repo/docs/current.md"
git -C "$repo" add .
expect_failure wildcard-rejection 'wildcard paths are forbidden' "$repo"

repo=$(new_repo historical)
printf '%s\n' \
  'scripts/check-legacy-name-allowlist.sh|product,font,url|product=8,font=4,url=1|policy|Policy implementation.' \
  'history.md|product|product=2|historical|Preserved historical body.' \
  >"$repo/allowlist.tsv"
printf 'Historical Clawmacs body.\nCurrent Clawmacs is full-trust.\n' \
  >"$repo/history.md"
git -C "$repo" add .
expect_failure historical-banner \
  'historical banner uses the former name for the current product' "$repo"

printf 'legacy-name allowlist tests: passed\n'
