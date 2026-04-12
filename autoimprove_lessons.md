# Autoimprove Lessons

This report captures what the prompt.sh autoimprove runs showed before the loop
was stopped.

## Observed loops

- The agent over-read when the source of truth was ambiguous. It searched the
  same auth/token concepts repeatedly across TODOs, docs, tests, and source
  after broad reads returned stale or duplicate references.
- The agent used eval to shell out to `git status`, `git diff`, and the Guix
  test wrapper. That is the wrong abstraction for clawmacs: prompt-mode agents
  should use project read/write APIs and in-process Lisp checks, not host
  process execution.
- Running tests from eval exposed nested prompt activity in the outer
  `prompt.sh --show-tools` stream. Test-internal subagent/tool events appeared
  as if they were top-level model tool calls.
- Structural form editing improved reliability, but the agent still guessed
  selectors when it did not have an outline/discovery step. Failed `defdoc`
  selector reads confirmed that `read mode=outline` needs to be the default path
  before `read/write mode=form`.
- Provider-supplied reasoning blocks were not available from the OpenAI Codex
  provider in these runs, even with `--show-reasoning`. The useful diagnostic
  signal was the visible tool sequence, failed tool results, and repeated read
  patterns.

## Harness changes made

- Default prompt runs now keep an explicit default tool allowlist of `read`,
  `write`, and `eval`, instead of treating NIL as "all registered tools".
- `eval` now rejects direct external process helpers by default, including
  `run-program`, `launch-program`, `run-shell-command`, `system`, and
  `make-process`.
- `eval` suppresses nested prompt live-tool tracing, so in-process test runs do
  not pollute the outer prompt transcript.
- `read mode=outline` exposes sexed structural discovery through the normal read
  tool, reducing the need to guess form selectors or use eval for outline
  helpers.

## Remaining recommendations

- Do not add a git-backed changes tool if the goal is to avoid shelling out from
  clawmacs. If working-tree awareness is needed, implement it as a pure Lisp
  project/change-set view or keep git inspection in the supervising harness.
- Add in-process test/check helpers if agents keep guessing FiveAM forms. This
  should still be exposed through `eval` or a pure Lisp API, not shell commands.
- Prefer deterministic tool affordances over more prompt text. The prompt text
  should stay short and point to concrete read/write/eval modes.
- Continue TODO feature work only after the harness prevents broad eval abuse,
  nested trace pollution, and selector guessing loops.
