# Fatal crash reports

Clawmacs writes one private diagnostic report when an unhandled condition
reaches SBCL's debugger while `clawmacs-main` owns the application runtime.
This includes a fatal condition on the main/frame thread and an unhandled fatal
condition on a named runtime worker. Conditions consumed by `handler-case`,
restarts, or other ordinary application handlers do not produce reports.

The launcher uses SBCL's `--disable-debugger` path. Clawmacs installs its fatal
hook before runtime or frame workers start, writes the report, prints the
resulting pathname to standard error, and then delegates to SBCL's original
disabled-debugger hook. The original condition therefore still terminates the
process with a nonzero status. Embedded Lisp sessions are unaffected outside
the dynamic extent of `clawmacs-main`; normal return restores the prior hook.

## Location and permissions

The default directory follows the XDG state convention:

```text
${XDG_STATE_HOME:-$HOME/.local/state}/clawmacs/crash-reports/
```

Set `CLAWMACS_CRASH_REPORT_DIR` to an absolute or working-directory-relative
directory to override it. If that directory already exists, it must be a real
(not symbolic-link) directory owned by the current user with mode `0700`;
Clawmacs validates it without changing its permissions. Clawmacs creates only
missing owned state descendants with mode `0700`. Each report is created
through an exclusive same-directory temporary file with mode `0600`, flushed,
atomically linked into its final no-replace name, and followed by a directory
flush. A write or publication failure publishes no partial report.

Report filenames contain a UTC timestamp, PID, and process-local atomic
sequence. Existing reports are retained; Clawmacs does not prune them
automatically.

## Privacy boundary

Reports are whitelist-only diagnostics. They contain:

- report schema/version, UTC time, PID, Lisp implementation/version;
- normalized working directory and allowlisted argv metadata, never raw
  argument values;
- condition class and a small structured summary for selected standard
  condition families;
- a bounded current-thread backtrace containing allowlisted function names and
  source basenames/form numbers, never frame arguments or locals;
- a bounded thread inventory classified by role, never raw thread names;
- safe counts, booleans, provider/model identifiers, and coarse
  Clawmacs/frame/runtime phase data when they are available without locks.

The arbitrary printed text of a condition is deliberately omitted: an error
message can contain prompt, compose, conversation, tool, or provider data.
Reports also exclude environment-variable values, API keys and OAuth tokens,
session and buffer names, conversation and compose text, tool calls/results and
payloads, HTTP bodies/headers, provider stderr, package secrets, and debug-log
contents. Collected scalar metadata is still bounded, credential-shaped text is
redacted defensively, and home/current-directory prefixes are normalized.

The report pathname itself can reveal the configured state-directory path when
printed to standard error. SBCL then prints its original fatal diagnostic to
standard error; that original SBCL output is not part of the sanitized report
and should be handled as sensitive terminal/log data.

## Diagnostic workflow

1. Preserve the report before retrying, together with the application version
   or commit and the exact action that preceded the exit.
2. Check `schema_version`, `condition/type`, `clawmacs_state/phase`, and the
   first non-reporter Clawmacs frame in `current_thread_backtrace`.
3. Compare thread roles and runtime booleans to distinguish frame, provider,
   tool, subagent, and other worker failures.
4. Reproduce with the same build and configuration. Do not attach raw debug
   logs, credentials, session files, or transcripts merely because the crash
   report does not contain them.
5. Review the report before sharing it. Its collection policy is deliberately
   conservative, but provider/model identifiers and normalized paths may still
   describe private local configuration.

If report creation fails, Clawmacs prints a short reporter-failure notice and
continues into the original SBCL fatal hook. Reporter recursion is suppressed,
and a process-wide atomic claim prevents concurrent fatal threads from
publishing more than one report for the same application runtime.
