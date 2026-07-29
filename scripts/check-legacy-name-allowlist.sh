#!/bin/sh
set -eu

usage() {
  printf 'usage: %s [--repo PATH] [--allowlist PATH] [--print-occurrences]\n' \
    "$0" >&2
  exit 2
}

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)
ALLOWLIST="$REPO_ROOT/scripts/legacy-name-allowlist.tsv"
PRINT_OCCURRENCES=0

while [ "$#" -gt 0 ]; do
  case "$1" in
    --repo)
      [ "$#" -ge 2 ] || usage
      REPO_ROOT=$2
      shift 2
      ;;
    --allowlist)
      [ "$#" -ge 2 ] || usage
      ALLOWLIST=$2
      shift 2
      ;;
    --print-occurrences)
      PRINT_OCCURRENCES=1
      shift
      ;;
    *)
      usage
      ;;
  esac
done

REPO_ROOT=$(CDPATH= cd -- "$REPO_ROOT" && pwd)
case "$ALLOWLIST" in
  /*) ;;
  *) ALLOWLIST="$REPO_ROOT/$ALLOWLIST" ;;
esac

TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT HUP INT TERM
RAW_OCCURRENCES="$TMP_DIR/raw-occurrences"
OCCURRENCES="$TMP_DIR/occurrences"
ENTRIES="$TMP_DIR/entries"
VIOLATIONS="$TMP_DIR/violations"
SYMLINKS="$TMP_DIR/symlinks"
CONTENT_MATCHES="$TMP_DIR/content-matches"
: >"$RAW_OCCURRENCES"
: >"$ENTRIES"
: >"$VIOLATIONS"

git -C "$REPO_ROOT" ls-files -s |
  awk '$1 == "120000" {
         sub(/^[^\t]*\t/, "")
         print
       }' >"$SYMLINKS"

normalize_context() {
  printf '%s\n' "$1" |
    tr '[:upper:]' '[:lower:]' |
    awk '{$1=$1; print}'
}

record_raw_occurrence() {
  occurrence_path=$1
  occurrence_origin=$2
  occurrence_kind=$3
  occurrence_context=$4
  normalized=$(normalize_context "$occurrence_context")
  fingerprint=$(printf '%s' "$normalized" | sha256sum | awk '{print $1}')
  printf '%s|%s|%s|%s\n' \
    "$occurrence_path" "$occurrence_origin" "$occurrence_kind" "$fingerprint" \
    >>"$RAW_OCCURRENCES"
}

classify_context() {
  occurrence_path=$1
  occurrence_origin=$2
  occurrence_context=$3
  lower=$(printf '%s' "$occurrence_context" |
    tr '[:upper:]' '[:lower:]')

  product_count=$(printf '%s\n' "$lower" |
    grep -o 'clawmacs' | wc -l | tr -d ' ' || true)
  while [ "$product_count" -gt 0 ]; do
    record_raw_occurrence \
      "$occurrence_path" "$occurrence_origin" product "$occurrence_context"
    product_count=$((product_count - 1))
  done

  font_count=$(printf '%s\n' "$lower" |
    grep -o 'clawfont' | wc -l | tr -d ' ' || true)
  while [ "$font_count" -gt 0 ]; do
    record_raw_occurrence \
      "$occurrence_path" "$occurrence_origin" font "$occurrence_context"
    font_count=$((font_count - 1))
  done

  uri_count=$(printf '%s\n' "$lower" |
    grep -Eio \
      '[a-z][a-z0-9+.-]*:[^[:space:]]*(clawmacs|clawfont)[^[:space:]]*' |
    wc -l | tr -d ' ' || true)
  while [ "$uri_count" -gt 0 ]; do
    record_raw_occurrence \
      "$occurrence_path" "$occurrence_origin" url "$occurrence_context"
    uri_count=$((uri_count - 1))
  done
}

grep_status=0
git -C "$REPO_ROOT" grep -n -I -i -E 'clawmacs|clawfont' -- . \
  >"$CONTENT_MATCHES" || grep_status=$?
if [ "$grep_status" -gt 1 ]; then
  printf 'legacy-name gate: git grep failed with status %s\n' "$grep_status" >&2
  exit "$grep_status"
fi

while IFS= read -r match; do
  [ -n "$match" ] || continue
  path=${match%%:*}
  if grep -Fqx "$path" "$SYMLINKS"; then
    continue
  fi
  remainder=${match#*:}
  text=${remainder#*:}
  classify_context "$path" content "$text"
done <"$CONTENT_MATCHES"

git -C "$REPO_ROOT" ls-files | while IFS= read -r path; do
  [ -n "$path" ] || continue
  classify_context "$path" filename "$path"
done

while IFS= read -r path; do
  [ -n "$path" ] || continue
  target=$(readlink "$REPO_ROOT/$path")
  classify_context "$path" symlink-target "$target"
done <"$SYMLINKS"

awk -F'|' '
  {
    key = $1 SUBSEP $2 SUBSEP $3 SUBSEP $4
    ordinal[key]++
    printf "%s|%s|%s|%s:%d\n", $1, $2, $3, $4, ordinal[key]
  }
' "$RAW_OCCURRENCES" >"$OCCURRENCES"

if [ "$PRINT_OCCURRENCES" -eq 1 ]; then
  sort "$OCCURRENCES"
  exit 0
fi

[ -f "$ALLOWLIST" ] || {
  printf 'legacy-name gate: missing allowlist %s\n' "$ALLOWLIST" >&2
  exit 1
}

if grep -Eqi 'clawmacs|clawfont' "$ALLOWLIST"; then
  printf '%s\n' \
    'allowlist: use %OLD_PRODUCT% and %OLD_FONT% placeholders in paths' \
    >>"$VIOLATIONS"
fi

line_number=0
while IFS='|' read -r encoded_path origin kind fingerprint category rationale extra; do
  line_number=$((line_number + 1))
  case "$encoded_path" in
    ''|'#'*) continue ;;
  esac
  if [ -n "${extra:-}" ] || [ -z "$origin" ] || [ -z "$kind" ] || \
     [ -z "$fingerprint" ] || [ -z "$category" ] || [ -z "$rationale" ]; then
    printf 'allowlist:%s: expected path|origin|kind|fingerprint|category|rationale\n' \
      "$line_number" >>"$VIOLATIONS"
    continue
  fi
  path=$(printf '%s\n' "$encoded_path" |
    sed 's/%OLD_PRODUCT_UPPER%/CLAWMACS/g;
         s/%OLD_FONT_UPPER%/CLAWFONT/g;
         s/%OLD_PRODUCT%/clawmacs/g;
         s/%OLD_FONT%/clawfont/g')
  case "$path" in
    *'*'*|*'?'*|*'['*)
      printf 'allowlist:%s: wildcard paths are forbidden: %s\n' \
        "$line_number" "$encoded_path" >>"$VIOLATIONS"
      continue
      ;;
    /*|../*|*/../*)
      printf 'allowlist:%s: path must be repository-relative: %s\n' \
        "$line_number" "$encoded_path" >>"$VIOLATIONS"
      continue
      ;;
  esac
  case "$origin" in
    content|filename|symlink-target) ;;
    *)
      printf 'allowlist:%s: invalid origin: %s\n' \
        "$line_number" "$origin" >>"$VIOLATIONS"
      continue
      ;;
  esac
  case "$kind" in
    product|font|url) ;;
    *)
      printf 'allowlist:%s: invalid kind: %s\n' \
        "$line_number" "$kind" >>"$VIOLATIONS"
      continue
      ;;
  esac
  case "$category" in
    compatibility-ignore|historical|migration-doc|migration-test|policy|\
runtime-archive|runtime-data-reader|runtime-executable-refusal|runtime-mount|\
runtime-session)
      ;;
    *)
      printf 'allowlist:%s: invalid category: %s\n' \
        "$line_number" "$category" >>"$VIOLATIONS"
      continue
      ;;
  esac
  if ! printf '%s\n' "$fingerprint" |
       grep -Eq '^[0-9a-f]{64}:[1-9][0-9]*$'; then
    printf 'allowlist:%s: invalid fingerprint: %s\n' \
      "$line_number" "$fingerprint" >>"$VIOLATIONS"
    continue
  fi
  if ! git -C "$REPO_ROOT" ls-files --error-unmatch -- "$path" \
       >/dev/null 2>&1; then
    printf 'allowlist:%s: path is not tracked: %s\n' \
      "$line_number" "$encoded_path" >>"$VIOLATIONS"
    continue
  fi
  symlink_p=0
  if grep -Fqx "$path" "$SYMLINKS"; then
    symlink_p=1
  fi
  if { [ "$origin" = symlink-target ] && [ "$symlink_p" -ne 1 ]; } ||
     { [ "$origin" = content ] && [ "$symlink_p" -eq 1 ]; }; then
    printf 'allowlist:%s: origin %s does not match tracked mode for %s\n' \
      "$line_number" "$origin" "$encoded_path" >>"$VIOLATIONS"
    continue
  fi
  printf '%s|%s|%s|%s|%s|%s\n' \
    "$path" "$origin" "$kind" "$fingerprint" "$category" "$rationale" \
    >>"$ENTRIES"
  if [ "$category" = historical ] &&
     git -C "$REPO_ROOT" grep -n -I -i -E \
       'current[[:space:]]+clawmacs|description of current[[:space:]]+clawmacs' \
       -- "$path" >/dev/null 2>&1; then
    printf 'historical context uses the former name for the current product: %s\n' \
      "$encoded_path" >>"$VIOLATIONS"
  fi
done <"$ALLOWLIST"

cut -d'|' -f1-4 "$ENTRIES" | sort | uniq -d |
  while IFS= read -r duplicate; do
    [ -n "$duplicate" ] &&
      printf 'allowlist: duplicate occurrence: %s\n' "$duplicate" \
        >>"$VIOLATIONS"
  done

while IFS= read -r occurrence; do
  if ! cut -d'|' -f1-4 "$ENTRIES" | grep -Fqx "$occurrence"; then
    printf 'unapproved occurrence: %s\n' "$occurrence" >>"$VIOLATIONS"
  fi
done <"$OCCURRENCES"

cut -d'|' -f1-4 "$ENTRIES" | while IFS= read -r entry; do
  if ! grep -Fqx "$entry" "$OCCURRENCES"; then
    printf 'stale allowlist occurrence: %s\n' "$entry" >>"$VIOLATIONS"
  fi
done

if [ -s "$VIOLATIONS" ]; then
  printf 'legacy-name gate failed:\n' >&2
  sort -u "$VIOLATIONS" >&2
  exit 1
fi

occurrence_count=$(wc -l <"$OCCURRENCES" | tr -d ' ')
allowed_paths=$(cut -d'|' -f1 "$ENTRIES" | sort -u | wc -l | tr -d ' ')
printf 'legacy-name gate: %s approved occurrences across %s exact paths\n' \
  "$occurrence_count" "$allowed_paths"
