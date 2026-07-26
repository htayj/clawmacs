#!/bin/sh
set -eu

usage() {
  cat <<'EOF'
Usage: scripts/prompt-probes.sh [--only NAME] [--provider PROVIDER] [--model MODEL] [--fallback-provider PROVIDER] [--fallback-model MODEL] [--no-fallback] [--think LEVEL] [--keep-logs]

Runs live prompt.sh harness probes in an isolated prompt environment.
By default probes use openai-codex/gpt-5.6-sol, then retry once with
openrouter/openai/gpt-5.6-sol if the primary provider fails.

Probes:
  docs          Local CL spec and imported-library discovery.
  skills        Skill discovery, $skill mention injection, and resource reads.
  transaction   Project change set plus staged sexed edit.
  scratch       Scratch, project resource, file buffer, and save flow.
EOF
}

ONLY=""
PROVIDER="${CLAWMACS_PROMPT_PROBE_PROVIDER:-openai-codex}"
MODEL="${CLAWMACS_PROMPT_PROBE_MODEL:-gpt-5.6-sol}"
FALLBACK_PROVIDER="${CLAWMACS_PROMPT_PROBE_FALLBACK_PROVIDER:-openrouter}"
FALLBACK_MODEL="${CLAWMACS_PROMPT_PROBE_FALLBACK_MODEL:-openai/gpt-5.6-sol}"
THINK=""
KEEP_LOGS=0
EXPLICIT_PROVIDER=0
EXPLICIT_MODEL=0

while [ "$#" -gt 0 ]; do
  case "$1" in
    --only)
      shift
      [ "$#" -gt 0 ] || {
        echo "prompt-probes: --only requires a name" >&2
        exit 2
      }
      ONLY="$1"
      ;;
    --provider)
      shift
      [ "$#" -gt 0 ] || {
        echo "prompt-probes: --provider requires a name" >&2
        exit 2
      }
      PROVIDER="$1"
      EXPLICIT_PROVIDER=1
      ;;
    --fallback-provider)
      shift
      [ "$#" -gt 0 ] || {
        echo "prompt-probes: --fallback-provider requires a name" >&2
        exit 2
      }
      FALLBACK_PROVIDER="$1"
      ;;
    --no-fallback)
      FALLBACK_PROVIDER=""
      ;;
    --model)
      shift
      [ "$#" -gt 0 ] || {
        echo "prompt-probes: --model requires a model" >&2
        exit 2
      }
      MODEL="$1"
      EXPLICIT_MODEL=1
      ;;
    --fallback-model)
      shift
      [ "$#" -gt 0 ] || {
        echo "prompt-probes: --fallback-model requires a model" >&2
        exit 2
      }
      FALLBACK_MODEL="$1"
      ;;
    --think)
      shift
      [ "$#" -gt 0 ] || {
        echo "prompt-probes: --think requires a level" >&2
        exit 2
      }
      THINK="$1"
      ;;
    --keep-logs)
      KEEP_LOGS=1
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "prompt-probes: unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

if [ "$EXPLICIT_PROVIDER" -eq 1 ] && [ "$EXPLICIT_MODEL" -eq 0 ]; then
  case "$PROVIDER" in
    openai-codex)
      MODEL="gpt-5.6-sol"
      ;;
    openrouter)
      MODEL="$FALLBACK_MODEL"
      ;;
    *)
      MODEL=""
      ;;
  esac
fi

ROOT=$(mktemp -d "${TMPDIR:-/tmp}/clawmacs-prompt-probes.XXXXXX")
trap 'if [ "$KEEP_LOGS" -eq 0 ]; then rm -rf "$ROOT"; else echo "logs kept in $ROOT" >&2; fi' EXIT

run_prompt() {
  output=$1
  error=$2
  debug_log=$3
  provider=$4
  model=$5
  prompt=$6
  extra_args=${7:-}
  prompt_args=""

  [ -z "$provider" ] || prompt_args="$prompt_args --provider $provider"
  [ -z "$model" ] || prompt_args="$prompt_args --model $model"
  [ -z "$THINK" ] || prompt_args="$prompt_args --think $THINK"

  ./prompt.sh \
    $prompt_args \
    --isolated \
    --no-init \
    --show-tools \
    --show-metadata \
    --debug-log "$debug_log" \
    --max-tool-iterations 14 \
    $extra_args \
    "$prompt" >"$output" 2>"$error"
}

run_probe_with_args() {
  name=$1
  prompt=$2
  extra_args=$3
  shift 3

  if [ -n "$ONLY" ] && [ "$ONLY" != "$name" ]; then
    return 0
  fi

  stdout="$ROOT/$name.out"
  stderr="$ROOT/$name.err"
  debug="$ROOT/$name.debug.log"

  echo "==> $name" >&2
  if ! run_prompt "$stdout" "$stderr" "$debug" "$PROVIDER" "$MODEL" "$prompt" "$extra_args"; then
    if [ "$EXPLICIT_PROVIDER" -eq 0 ] && [ -n "$FALLBACK_PROVIDER" ]; then
      primary_stdout="$ROOT/$name.primary.out"
      primary_stderr="$ROOT/$name.primary.err"
      primary_debug="$ROOT/$name.primary.debug.log"
      mv "$stdout" "$primary_stdout"
      mv "$stderr" "$primary_stderr"
      mv "$debug" "$primary_debug" 2>/dev/null || true
      echo "primary prompt probe failed: $name; retrying with --provider $FALLBACK_PROVIDER --model $FALLBACK_MODEL" >&2
      if ! run_prompt "$stdout" "$stderr" "$debug" "$FALLBACK_PROVIDER" "$FALLBACK_MODEL" "$prompt" "$extra_args"; then
        echo "prompt probe failed: $name" >&2
        echo "primary stderr:" >&2
        sed -n '1,160p' "$primary_stderr" >&2
        echo "fallback stderr:" >&2
        sed -n '1,220p' "$stderr" >&2
        exit 1
      fi
    else
      echo "prompt probe failed: $name" >&2
      sed -n '1,220p' "$stderr" >&2
      exit 1
    fi
  fi

  combined="$ROOT/$name.combined"
  cat "$stderr" "$stdout" >"$combined"

  for expected in "$@"; do
    if ! grep -F "$expected" "$combined" >/dev/null; then
      echo "prompt probe missing expected text: $name: $expected" >&2
      sed -n '1,260p' "$combined" >&2
      exit 1
    fi
  done
}

run_probe() {
  name=$1
  prompt=$2
  shift 2
  run_probe_with_args "$name" "$prompt" "" "$@"
}

run_probe "docs" \
  "Use lisp_eval only. Do not modify files. In as few tool calls as practical, verify local documentation discovery by checking common-lisp-spec availability, describing HANDLER-CASE, listing imported systems, and describing one Alexandria symbol. Return a concise report including the APIs used, CL Community Spec available?, and Alexandria discovered?" \
  "CL Community Spec" \
  "Alexandria" \
  "lisp_eval"

skills_root="$ROOT/skills-root"
mkdir -p "$skills_root/demo/references"
cat >"$skills_root/demo/SKILL.md" <<'EOF'
---
name: demo-skill
description: Demo probe skill for Clawmacs.
---
When this skill is used, inspect references/guide.md and report the phrase skill-probe-marker.
EOF
cat >"$skills_root/demo/references/guide.md" <<'EOF'
The required phrase is skill-probe-marker.
EOF
run_probe_with_args "skills" \
  "Use lisp_eval only. Use \$demo-skill. Verify that the skill is listed, read its SKILL.md instructions, read references/guide.md, and return a concise report with the skill name and exact phrase skill-probe-marker." \
  "--skill-root $skills_root" \
  "demo-skill" \
  "skill-probe-marker" \
  "lisp_eval"

transaction_root="$ROOT/transaction-project/"
run_probe "transaction" \
  "Use lisp_eval only. Work only in a :persist nil temp project rooted at #P\"$transaction_root\". In one batched form if possible: create the project, create src/demo.lisp containing (defun demo () :old), begin a change set, use sexed-stage-replace-project-form to stage changing it to (defun demo () :new), show the change-set diff, apply it, run project checks, and return final file text plus balanced-p." \
  "change" \
  "(defun demo () :new)" \
  "balanced"

scratch_root="$ROOT/scratch-project/"
run_probe "scratch" \
  "Use lisp_eval only. Work only in a :persist nil temp project rooted at #P\"$scratch_root\". In one batched form if possible: put (workspace (todo alpha) (todo beta)) in scratch, use sexed to change beta into done, save scratch text into notes.lisp, open it as a project file buffer, append exactly one newline-prefixed Lisp comment containing marker done-note-marker through file-buffer-text, save with project-save-buffer, and return a plist with keys :final-scratch, :final-file, :marker-count where the value is occurrences of done-note-marker, and :dirty-p after save using file-buffer-dirty-p. Use (string #\\Newline) or FORMAT ~%; do not use \\n for newlines in Common Lisp strings." \
  "(done beta)" \
  ":MARKER-COUNT 1" \
  ":DIRTY-P NIL"

echo "prompt probes passed" >&2
