#!/bin/sh
set -eu

usage() {
  printf 'usage: %s [--repo PATH] [--allowlist PATH]\n' "$0" >&2
  exit 2
}

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)
ALLOWLIST="$REPO_ROOT/scripts/legacy-name-allowlist.tsv"

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

[ -f "$ALLOWLIST" ] || {
  printf 'legacy-name gate: missing allowlist %s\n' "$ALLOWLIST" >&2
  exit 1
}

TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT HUP INT TERM
ENTRIES="$TMP_DIR/entries"
SEEN="$TMP_DIR/seen"
VIOLATIONS="$TMP_DIR/violations"
CONTENT_MATCHES="$TMP_DIR/content-matches"
FILENAME_MATCHES="$TMP_DIR/filename-matches"
: >"$ENTRIES"
: >"$SEEN"
: >"$VIOLATIONS"

line_number=0
while IFS='|' read -r path kinds counts category rationale extra; do
  line_number=$((line_number + 1))
  case "$path" in
    ''|'#'*) continue ;;
  esac
  if [ -n "${extra:-}" ] || [ -z "$kinds" ] || [ -z "$counts" ] || \
     [ -z "$category" ] || [ -z "$rationale" ]; then
    printf 'allowlist:%s: expected path|kinds|counts|category|rationale\n' \
      "$line_number" >>"$VIOLATIONS"
    continue
  fi
  case "$path" in
    *'*'*|*'?'*|*'['*)
      printf 'allowlist:%s: wildcard paths are forbidden: %s\n' \
        "$line_number" "$path" >>"$VIOLATIONS"
      continue
      ;;
    /*|../*|*/../*)
      printf 'allowlist:%s: path must be repository-relative: %s\n' \
        "$line_number" "$path" >>"$VIOLATIONS"
      continue
      ;;
  esac
  case ",$kinds," in
    *',product,'*|*',font,'*|*',url,'*) ;;
    *)
      printf 'allowlist:%s: invalid kinds: %s\n' \
        "$line_number" "$kinds" >>"$VIOLATIONS"
      continue
      ;;
  esac
  old_ifs=$IFS
  IFS=,
  for kind in $kinds; do
    case "$kind" in
      product|font|url) ;;
      *)
        printf 'allowlist:%s: invalid kind %s\n' \
          "$line_number" "$kind" >>"$VIOLATIONS"
        ;;
    esac
    expected=$(printf '%s\n' "$counts" | tr ',' '\n' |
      awk -F= -v wanted="$kind" '$1 == wanted { print $2; exit }')
    case "$expected" in
      ''|*[!0-9]*|0)
        printf 'allowlist:%s: missing positive count for kind %s\n' \
          "$line_number" "$kind" >>"$VIOLATIONS"
        ;;
    esac
  done
  IFS=$old_ifs
  if ! git -C "$REPO_ROOT" ls-files --error-unmatch -- "$path" \
       >/dev/null 2>&1; then
    printf 'allowlist:%s: path is not tracked: %s\n' \
      "$line_number" "$path" >>"$VIOLATIONS"
    continue
  fi
  printf '%s|%s|%s|%s|%s\n' \
    "$path" "$kinds" "$counts" "$category" "$rationale" >>"$ENTRIES"
done <"$ALLOWLIST"

cut -d'|' -f1 "$ENTRIES" | sort | uniq -d | while IFS= read -r duplicate; do
  [ -n "$duplicate" ] && \
    printf 'allowlist: duplicate path: %s\n' "$duplicate" >>"$VIOLATIONS"
done

allowed_kinds() {
  awk -F'|' -v wanted="$1" '$1 == wanted { print $2; exit }' "$ENTRIES"
}

record_kind() {
  match_path=$1
  match_kind=$2
  match_origin=$3
  match_detail=$4
  kinds=$(allowed_kinds "$match_path")
  case ",$kinds," in
    *",$match_kind,"*)
      printf '%s|%s\n' "$match_path" "$match_kind" >>"$SEEN"
      ;;
    *)
      printf '%s: %s %s is not allowlisted for kind %s\n' \
        "$match_origin" "$match_path" "$match_detail" "$match_kind" \
        >>"$VIOLATIONS"
      ;;
  esac
}

classify_match() {
  match_path=$1
  match_origin=$2
  match_detail=$3
  match_text=$4
  lower=$(printf '%s' "$match_text" | tr '[:upper:]' '[:lower:]')
  case "$lower" in
    *clawmacs*)
      count=$(printf '%s\n' "$lower" | grep -o 'clawmacs' | wc -l | tr -d ' ')
      while [ "$count" -gt 0 ]; do
        record_kind "$match_path" product "$match_origin" "$match_detail"
        count=$((count - 1))
      done
      ;;
  esac
  case "$lower" in
    *clawfont*)
      count=$(printf '%s\n' "$lower" | grep -o 'clawfont' | wc -l | tr -d ' ')
      while [ "$count" -gt 0 ]; do
        record_kind "$match_path" font "$match_origin" "$match_detail"
        count=$((count - 1))
      done
      ;;
  esac
  case "$lower" in
    *github.com/*clawmacs*|*clawmacs*github.com/*)
      record_kind "$match_path" url "$match_origin" "$match_detail"
      ;;
  esac
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
  remainder=${match#*:}
  line=${remainder%%:*}
  text=${remainder#*:}
  classify_match "$path" line "$line" "$text"
done <"$CONTENT_MATCHES"

git -C "$REPO_ROOT" ls-files | grep -i -E 'clawmacs|clawfont' \
  >"$FILENAME_MATCHES" || true
while IFS= read -r path; do
  [ -n "$path" ] || continue
  classify_match "$path" filename "(path)" "$path"
done <"$FILENAME_MATCHES"

while IFS='|' read -r path kinds counts category rationale; do
  old_ifs=$IFS
  IFS=,
  for kind in $kinds; do
    expected=$(printf '%s\n' "$counts" | tr ',' '\n' |
      awk -F= -v wanted="$kind" '$1 == wanted { print $2; exit }')
    actual=$(grep -Fxc "$path|$kind" "$SEEN" || true)
    if [ "$actual" -ne "$expected" ]; then
      printf 'allowlist: occurrence count mismatch: %s (%s expected %s actual %s)\n' \
        "$path" "$kind" "$expected" "$actual" >>"$VIOLATIONS"
    fi
  done
  IFS=$old_ifs
  if [ "$category" = historical ] && \
     git -C "$REPO_ROOT" grep -n -I -i -E \
       'current[[:space:]]+clawmacs|description of current[[:space:]]+clawmacs' \
       -- "$path" >/dev/null 2>&1; then
    printf 'historical banner uses the former name for the current product: %s\n' \
      "$path" >>"$VIOLATIONS"
  fi
done <"$ENTRIES"

if [ -s "$VIOLATIONS" ]; then
  printf 'legacy-name gate failed:\n' >&2
  sort -u "$VIOLATIONS" >&2
  exit 1
fi

allowed_checks=$(sort -u "$SEEN" | wc -l | tr -d ' ')
allowed_paths=$(cut -d'|' -f1 "$ENTRIES" | sort -u | wc -l | tr -d ' ')
printf 'legacy-name gate: %s allowed path/kind checks across %s exact paths\n' \
  "$allowed_checks" "$allowed_paths"
