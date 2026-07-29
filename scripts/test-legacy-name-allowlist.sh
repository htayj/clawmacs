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
  "$repo/scripts/check-legacy-name-allowlist.sh" \
    --repo "$repo" --allowlist "$repo/allowlist.tsv"
}

write_allowlist() {
  repo=$1
  {
    printf '%s\n' \
      '# Exact path|origin|kind|normalized fingerprint|category|rationale'
    "$repo/scripts/check-legacy-name-allowlist.sh" \
      --repo "$repo" --print-occurrences |
      while IFS='|' read -r path origin kind fingerprint; do
        encoded=$(printf '%s\n' "$path" |
          sed 's/CLAWMACS/%OLD_PRODUCT_UPPER%/g;
               s/CLAWFONT/%OLD_FONT_UPPER%/g;
               s/clawmacs/%OLD_PRODUCT%/g;
               s/clawfont/%OLD_FONT%/g')
        printf '%s|%s|%s|%s|policy|Deterministic test fixture.\n' \
          "$encoded" "$origin" "$kind" "$fingerprint"
      done
  } >"$repo/allowlist.tsv"
  git -C "$repo" add allowlist.tsv
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
printf 'clawmacs compatibility fixture\n' >"$repo/legacy.txt"
printf 'import demo.clawfont only\n' >"$repo/legacy-font.txt"
printf 'clean\n' >"$repo/clean.txt"
git -C "$repo" add .
write_allowlist "$repo"
run_gate "$repo" >"$repo/pass-output"
grep -Fq 'legacy-name gate:' "$repo/pass-output"

printf 'current-facing clawmacs substitution\n' >"$repo/legacy.txt"
expect_failure same-count-substitution 'unapproved occurrence: legacy.txt' "$repo"
printf 'clawmacs compatibility fixture\n' >"$repo/legacy.txt"

printf 'new clawmacs reference\n' >"$repo/current.md"
git -C "$repo" add current.md
expect_failure new-current-reference 'unapproved occurrence: current.md' "$repo"
git -C "$repo" rm -q -f current.md

printf 'clawmacs://legacy\n' >"$repo/uri.txt"
git -C "$repo" add uri.txt
write_allowlist "$repo"
awk -F'|' 'BEGIN {OFS="|"} !($1 == "uri.txt" && $3 == "url") {print}' \
  "$repo/allowlist.tsv" >"$repo/product-only-uri.tsv"
mv "$repo/product-only-uri.tsv" "$repo/allowlist.tsv"
expect_failure scheme-identity-uri '|url|' "$repo"
git -C "$repo" rm -q -f uri.txt
write_allowlist "$repo"

ln -s '../clawmacs-archive' "$repo/archive-link"
git -C "$repo" add archive-link
ln -sfn '../safe-archive' "$repo/archive-link"
expect_failure staged-symlink-target-divergence '|symlink-target|' "$repo"
git -C "$repo" rm -q -f archive-link

cp "$repo/allowlist.tsv" "$repo/valid-allowlist"
awk -F'|' 'BEGIN {OFS="|"} !changed && $1 !~ /^#/ {$2="bogus"; changed=1} {print}' \
  "$repo/valid-allowlist" >"$repo/allowlist.tsv"
expect_failure invalid-origin 'invalid origin: bogus' "$repo"

awk -F'|' 'BEGIN {OFS="|"} !changed && $1 !~ /^#/ {$4="bogus"; changed=1} {print}' \
  "$repo/valid-allowlist" >"$repo/allowlist.tsv"
expect_failure invalid-fingerprint 'invalid fingerprint: bogus' "$repo"

awk -F'|' 'BEGIN {OFS="|"} !changed && $1 !~ /^#/ {$5="bogus"; changed=1} {print}' \
  "$repo/valid-allowlist" >"$repo/allowlist.tsv"
expect_failure invalid-category 'invalid category: bogus' "$repo"

cp "$repo/valid-allowlist" "$repo/allowlist.tsv"
printf '%s\n' \
  'clean.txt|filename|product|0000000000000000000000000000000000000000000000000000000000000000:1|policy|Stale fixture.' \
  >>"$repo/allowlist.tsv"
expect_failure stale-entry 'stale allowlist occurrence: clean.txt' "$repo"

cp "$repo/valid-allowlist" "$repo/allowlist.tsv"
printf '%s\n' \
  'docs/*|content|product|0000000000000000000000000000000000000000000000000000000000000000:1|policy|Invalid wildcard.' \
  >>"$repo/allowlist.tsv"
expect_failure wildcard-rejection 'wildcard paths are forbidden' "$repo"

repo=$(new_repo historical)
printf 'Current Clawmacs is full-trust.\n' >"$repo/history.md"
git -C "$repo" add .
write_allowlist "$repo"
awk -F'|' 'BEGIN {OFS="|"} $1 == "history.md" {$5="historical"} {print}' \
  "$repo/allowlist.tsv" >"$repo/historical.tsv"
mv "$repo/historical.tsv" "$repo/allowlist.tsv"
expect_failure historical-context \
  'historical context uses the former name for the current product' "$repo"

printf 'legacy-name allowlist tests: passed\n'
