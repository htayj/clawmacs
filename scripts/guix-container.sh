#!/bin/sh
set -eu

LAUNCHER_PREFIX='[clawmacs-env]'
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
MODE=''
PREFLIGHT_ONLY=0
PAYLOAD_SHIFT_COUNT=0
QUICKLISP_ENV_LOADED=0
WORKSPACE_HOME=/workspace/.cache/home
WORKSPACE_QUICKLISP_SETUP=/workspace/.cache/home/quicklisp/setup.lisp
WORKSPACE_XDG_CACHE=/workspace/.cache
HOST_HOME=''
HOST_QUICKLISP_SETUP=''
RESOLVED_SSL_LIB_PATH=''
CONTAINER_LAUNCH_DIR=/tmp
GUIX_MANIFEST_PATH=''
HOST_USER_HOME=${HOME:-}

stderr() {
  printf '%s %s\n' "$LAUNCHER_PREFIX" "$*" >&2
}

fail() {
  code="$1"
  shift
  stderr "$*"
  exit "$code"
}

is_test_toggle_enabled() {
  key="$1"
  if [ "${CLAWMACS_ENABLE_TEST_TOGGLES:-0}" != "1" ]; then
    return 1
  fi
  eval "value=\${$key:-0}"
  [ "$value" = "1" ]
}

is_sensitive_key() {
  key="$1"
  case "$key" in
    ANTHROPIC_API_KEY|OPENAI_API_KEY|*_KEY|*_TOKEN|*_SECRET)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

redact_value() {
  key="$1"
  value="$2"
  if is_sensitive_key "$key"; then
    printf '[REDACTED]'
  else
    printf '%s' "$value"
  fi
}

diagnostic_env() {
  key="$1"
  eval "raw=\${$key-}"
  safe_value=$(redact_value "$key" "$raw")
  stderr "diag $key=$safe_value"
}

validate_launcher_cli() {
  MODE=''
  PREFLIGHT_ONLY=0
  PAYLOAD_SHIFT_COUNT=0

  if [ "$#" -eq 0 ]; then
    fail 118 "invalid launcher mode: missing --mode <run|e2e>"
  fi

  while [ "$#" -gt 0 ]; do
    PAYLOAD_SHIFT_COUNT=$((PAYLOAD_SHIFT_COUNT + 1))
    case "$1" in
      --preflight-only)
        PREFLIGHT_ONLY=1
        shift
        ;;
      --mode)
        if [ "$#" -lt 2 ]; then
          fail 118 "invalid launcher mode: missing --mode value"
        fi
        shift
        MODE="$1"
        PAYLOAD_SHIFT_COUNT=$((PAYLOAD_SHIFT_COUNT + 1))
        shift
        ;;
      --)
        shift
        break
        ;;
      *)
        fail 118 "invalid launcher mode: unexpected argument '$1'"
        ;;
    esac
  done

  case "$MODE" in
    run|e2e)
      ;;
    '')
      fail 118 "invalid launcher mode: missing --mode <run|e2e>"
      ;;
    *)
      fail 118 "invalid launcher mode: '$MODE'"
      ;;
  esac

  if [ "$#" -eq 0 ] && [ "$PREFLIGHT_ONLY" -ne 1 ]; then
    fail 119 "missing launcher command payload"
  fi

}

resolve_repo_root() {
  if is_test_toggle_enabled CLAWMACS_TEST_FAIL_REPO_ROOT; then
    fail 120 "failed to resolve repository root"
  fi

  if ! REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null); then
    fail 120 "failed to resolve repository root"
  fi

  if [ -z "$REPO_ROOT" ] || [ ! -d "$REPO_ROOT" ]; then
    fail 120 "failed to resolve repository root"
  fi

  GUIX_MANIFEST_PATH="$REPO_ROOT/guix.scm"
}

validate_guix_available() {
  if is_test_toggle_enabled CLAWMACS_TEST_MISSING_GUIX; then
    fail 110 "missing guix"
  fi

  if ! command -v guix >/dev/null 2>&1; then
    fail 110 "missing guix"
  fi
}

validate_project_mount() {
  if is_test_toggle_enabled CLAWMACS_TEST_MISSING_MOUNT; then
    fail 111 "missing project mount"
  fi

  if [ ! -d "$REPO_ROOT" ]; then
    fail 111 "missing project mount"
  fi
}

load_quicklisp_bootstrap_env() {
  if [ "$QUICKLISP_ENV_LOADED" -eq 1 ]; then
    return 0
  fi

  override_timeout=${QUICKLISP_BOOTSTRAP_TIMEOUT_SECS-__UNSET__}
  override_retries=${QUICKLISP_BOOTSTRAP_RETRIES-__UNSET__}

  QUICKLISP_BOOTSTRAP_URL='https://beta.quicklisp.org/quicklisp.lisp'
  QUICKLISP_BOOTSTRAP_SHA256='REPLACE_ME'
  QUICKLISP_BOOTSTRAP_TIMEOUT_SECS='20'
  QUICKLISP_BOOTSTRAP_RETRIES='2'

  env_path="$SCRIPT_DIR/quicklisp-bootstrap.env"
  if [ ! -f "$env_path" ]; then
    fail 115 "quicklisp bootstrap pin values missing"
  fi

  # shellcheck disable=SC1090
  . "$env_path"

  if [ "$override_timeout" != "__UNSET__" ]; then
    QUICKLISP_BOOTSTRAP_TIMEOUT_SECS="$override_timeout"
  fi
  if [ "$override_retries" != "__UNSET__" ]; then
    QUICKLISP_BOOTSTRAP_RETRIES="$override_retries"
  fi

  QUICKLISP_ENV_LOADED=1
}

validate_quicklisp_pin_values() {
  if is_test_toggle_enabled CLAWMACS_TEST_PIN_VALUES_MISSING; then
    fail 115 "quicklisp bootstrap pin values missing"
  fi

  load_quicklisp_bootstrap_env

  for value in "$QUICKLISP_BOOTSTRAP_URL" "$QUICKLISP_BOOTSTRAP_SHA256" "$QUICKLISP_BOOTSTRAP_TIMEOUT_SECS" "$QUICKLISP_BOOTSTRAP_RETRIES"; do
    case "$value" in
      ''|REPLACE_ME|*'<'*|*'>'*)
        fail 115 "quicklisp bootstrap pin values missing"
        ;;
    esac
  done
}

set_quicklisp_runtime_env() {
  HOST_HOME="$REPO_ROOT/.cache/home"
  HOST_QUICKLISP_SETUP="$HOST_HOME/quicklisp/setup.lisp"
  host_config_dir="$HOST_HOME/.config/clawmacs"
  source_config_dir=''

  if [ -n "$HOST_USER_HOME" ]; then
    source_config_dir="$HOST_USER_HOME/.config/clawmacs"
  fi

  mkdir -p "$HOST_HOME"

  if [ -n "$source_config_dir" ] && [ -d "$source_config_dir" ]; then
    mkdir -p "$host_config_dir"
    cp -R "$source_config_dir/." "$host_config_dir/"
  fi

  export HOME="$WORKSPACE_HOME"
  export CLAWMACS_QUICKLISP_SETUP="$WORKSPACE_QUICKLISP_SETUP"
  export XDG_CACHE_HOME="$WORKSPACE_XDG_CACHE"
}

run_in_container() {
  container_script="$1"
  shift

  cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --network --share="$REPO_ROOT=/workspace" -- bash -lc "$container_script" bash "$@"
}

probe_quicklisp_setup() {
  host_setup_path="$1"
  container_setup_path="$2"

  if [ ! -f "$host_setup_path" ]; then
    return 1
  fi

  run_in_container 'set -eu; setup_path="$1"; home_path="$2"; xdg_cache_path="$3"; HOME="$home_path" XDG_CACHE_HOME="$xdg_cache_path" sbcl --noinform --non-interactive --disable-debugger --load "$setup_path" --eval "(quit)" >/dev/null 2>&1' "$container_setup_path" "$WORKSPACE_HOME" "$WORKSPACE_XDG_CACHE"
}

bootstrap_quicklisp_once() {
  bootstrap_target_home="$1"
  download_path="$WORKSPACE_HOME/quicklisp.lisp"

  if is_test_toggle_enabled CLAWMACS_TEST_QUICKLISP_BOOTSTRAP_FAIL; then
    return 1
  fi

  run_in_container 'set -eu; bootstrap_url="$1"; expected_sha="$2"; timeout_secs="$3"; retry_count="$4"; download_path="$5"; bootstrap_home="$6"; runtime_home="$7"; xdg_cache_path="$8"; mkdir -p "$runtime_home"; mkdir -p "$xdg_cache_path"; curl --fail --location --silent --show-error --max-time "$timeout_secs" --retry "$retry_count" -o "$download_path" "$bootstrap_url"; actual_sha=$(sha256sum "$download_path" | cut -d" " -f1); [ "$actual_sha" = "$expected_sha" ]; mkdir -p "$bootstrap_home"; HOME="$runtime_home" XDG_CACHE_HOME="$xdg_cache_path" sbcl --noinform --non-interactive --disable-debugger --load "$download_path" --eval "(quicklisp-quickstart:install :path \"$bootstrap_home\")" --eval "(quit)" >/dev/null 2>&1' "$QUICKLISP_BOOTSTRAP_URL" "$QUICKLISP_BOOTSTRAP_SHA256" "$QUICKLISP_BOOTSTRAP_TIMEOUT_SECS" "$QUICKLISP_BOOTSTRAP_RETRIES" "$download_path" "$bootstrap_target_home" "$WORKSPACE_HOME" "$WORKSPACE_XDG_CACHE"
}

validate_quicklisp_bootstrap() {
  if is_test_toggle_enabled CLAWMACS_TEST_QUICKLISP_BOOTSTRAP_FAIL; then
    fail 112 "quicklisp bootstrap failed"
  fi

  set_quicklisp_runtime_env

  quicklisp_home="$HOST_HOME/quicklisp"
  quicklisp_backup_home="$HOST_HOME/quicklisp.backup.$$"
  quicklisp_bootstrap_home="$HOST_HOME/quicklisp.bootstrap.$$"
  container_quicklisp_setup="$WORKSPACE_QUICKLISP_SETUP"
  container_bootstrap_setup="$WORKSPACE_HOME/quicklisp.bootstrap.$$/setup.lisp"
  container_bootstrap_home="$WORKSPACE_HOME/quicklisp.bootstrap.$$"

  if probe_quicklisp_setup "$HOST_QUICKLISP_SETUP" "$container_quicklisp_setup"; then
    return 0
  fi

  rm -rf "$quicklisp_backup_home" "$quicklisp_bootstrap_home"

  if [ -e "$quicklisp_home" ]; then
    if ! mv "$quicklisp_home" "$quicklisp_backup_home"; then
      fail 112 "quicklisp bootstrap failed"
    fi
  fi

  if ! bootstrap_quicklisp_once "$container_bootstrap_home"; then
    if [ -e "$quicklisp_backup_home" ] && [ ! -e "$quicklisp_home" ]; then
      mv "$quicklisp_backup_home" "$quicklisp_home" >/dev/null 2>&1 || true
    fi
    rm -rf "$quicklisp_bootstrap_home"
    fail 112 "quicklisp bootstrap failed"
  fi

  if [ ! -f "$quicklisp_bootstrap_home/setup.lisp" ] || ! probe_quicklisp_setup "$quicklisp_bootstrap_home/setup.lisp" "$container_bootstrap_setup"; then
    if [ -e "$quicklisp_backup_home" ] && [ ! -e "$quicklisp_home" ]; then
      mv "$quicklisp_backup_home" "$quicklisp_home" >/dev/null 2>&1 || true
    fi
    rm -rf "$quicklisp_bootstrap_home"
    fail 112 "quicklisp bootstrap failed"
  fi

  rm -rf "$quicklisp_home"
  if ! mv "$quicklisp_bootstrap_home" "$quicklisp_home"; then
    if [ -e "$quicklisp_backup_home" ] && [ ! -e "$quicklisp_home" ]; then
      mv "$quicklisp_backup_home" "$quicklisp_home" >/dev/null 2>&1 || true
    fi
    rm -rf "$quicklisp_bootstrap_home"
    fail 112 "quicklisp bootstrap failed"
  fi

  rm -rf "$quicklisp_backup_home"

  if ! probe_quicklisp_setup "$HOST_QUICKLISP_SETUP" "$container_quicklisp_setup"; then
    fail 112 "quicklisp bootstrap failed"
  fi

  if [ ! -f "$HOST_QUICKLISP_SETUP" ]; then
    fail 112 "quicklisp bootstrap failed"
  fi
}

e2e_invocation_requires_credential() {
  shift_count="$PAYLOAD_SHIFT_COUNT"
  command_name=''
  command_target=''
  only_target=''

  while [ "$shift_count" -gt 0 ]; do
    if [ "$#" -eq 0 ]; then
      fail 122 "invalid e2e arguments"
    fi
    shift
    shift_count=$((shift_count - 1))
  done

  if [ "$#" -gt 0 ]; then
    command_name="$1"
  fi
  if [ "$#" -gt 1 ]; then
    command_target="$2"
  fi

  case "$command_name" in
    python3|python)
      ;;
    *)
      return 1
      ;;
  esac

  case "$command_target" in
    test-e2e.py|*/test-e2e.py)
      ;;
    *)
      return 1
      ;;
  esac

  while [ "$#" -gt 0 ]; do
    case "$1" in
      --only)
        if [ "$#" -lt 2 ]; then
          fail 122 "invalid e2e arguments"
        fi
        shift
        only_target="$1"
        ;;
    esac
    shift
  done

  if [ "$only_target" = "readline" ] || [ "$only_target" = "offline" ]; then
    return 1
  fi

  return 0
}

validate_provider_credential() {
  if is_test_toggle_enabled CLAWMACS_TEST_MISSING_PROVIDER_CREDENTIAL; then
    fail 116 "missing required provider credential"
  fi

  if [ "$MODE" != "e2e" ]; then
    return 0
  fi

  if ! e2e_invocation_requires_credential "$@"; then
    return 0
  fi

  if [ -n "${ANTHROPIC_API_KEY:-}" ] || [ -n "${OPENAI_API_KEY:-}" ]; then
    return 0
  fi

  fail 116 "missing required provider credential"
}

validate_override_path() {
  if is_test_toggle_enabled CLAWMACS_TEST_INVALID_OVERRIDE_PATH; then
    fail 117 "invalid override path"
  fi

  validate_canonical_override CLAWMACS_SSL_LIB directory
  validate_canonical_override CLAWMACS_FONT_PATH readable-file
  validate_canonical_override CLAWMACS_MCP_BIN executable-file
}

resolve_canonical_path() {
  path="$1"

  if command -v realpath >/dev/null 2>&1; then
    realpath "$path" 2>/dev/null || return 1
    return 0
  fi

  if command -v readlink >/dev/null 2>&1; then
    readlink -f "$path" 2>/dev/null || return 1
    return 0
  fi

  return 1
}

override_path_has_allowed_prefix() {
  canonical_path="$1"

  case "$canonical_path" in
    "$REPO_ROOT"|"$REPO_ROOT"/*|/tmp|/tmp/*|/gnu/store|/gnu/store/*|/run/current-system/profile|/run/current-system/profile/*)
      return 0
      ;;
  esac

  return 1
}

validate_canonical_override() {
  key="$1"
  expected_type="$2"
  eval "raw_value=\${$key:-}"

  if [ -z "$raw_value" ]; then
    return 0
  fi

  case "$expected_type" in
    directory)
      [ -d "$raw_value" ] || fail 117 "invalid override path"
      ;;
    readable-file)
      [ -f "$raw_value" ] && [ -r "$raw_value" ] || fail 117 "invalid override path"
      ;;
    executable-file)
      [ -f "$raw_value" ] && [ -x "$raw_value" ] || fail 117 "invalid override path"
      ;;
    *)
      fail 117 "invalid override path"
      ;;
  esac

  canonical_path=$(resolve_canonical_path "$raw_value") || fail 117 "invalid override path"

  if [ "$canonical_path" != "$raw_value" ]; then
    fail 117 "invalid override path"
  fi

  override_path_has_allowed_prefix "$canonical_path" || fail 117 "invalid override path"
}

prepend_ld_library_path() {
  lib_dir="$1"

  if [ -z "$lib_dir" ] || [ ! -d "$lib_dir" ]; then
    return 0
  fi

  case ":${LD_LIBRARY_PATH:-}:" in
    *:"$lib_dir":*)
      return 0
      ;;
  esac

  if [ -n "${LD_LIBRARY_PATH:-}" ]; then
    export LD_LIBRARY_PATH="$lib_dir:$LD_LIBRARY_PATH"
  else
    export LD_LIBRARY_PATH="$lib_dir"
  fi
}

validate_runtime_openssl_path() {
  if is_test_toggle_enabled CLAWMACS_TEST_MISSING_OPENSSL_PATH; then
    fail 121 "missing required runtime OpenSSL path"
  fi

  RESOLVED_SSL_LIB_PATH=''

  if [ -n "${CLAWMACS_SSL_LIB:-}" ]; then
    RESOLVED_SSL_LIB_PATH=$(resolve_canonical_path "$CLAWMACS_SSL_LIB") || fail 117 "invalid override path"
  fi

  if [ -z "$RESOLVED_SSL_LIB_PATH" ]; then
    resolved_ssl_file=$(cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --network --share="$REPO_ROOT=/workspace" -- bash -lc 'ldconfig -p 2>/dev/null | while IFS= read -r line; do case "$line" in *" => "*) lib=${line%% *}; case "$lib" in libssl.so*|libcrypto.so*) printf "%s\n" "${line##* => }"; break ;; esac ;; esac; done' 2>/dev/null || true)

    if [ -z "$resolved_ssl_file" ]; then
      resolved_ssl_file=$(cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --network --share="$REPO_ROOT=/workspace" -- bash -lc 'for lib in /run/current-system/profile/lib/libssl.so* /run/current-system/profile/lib/libcrypto.so* /run/current-system/profile/lib64/libssl.so* /run/current-system/profile/lib64/libcrypto.so* /gnu/store/*/lib/libssl.so* /gnu/store/*/lib/libcrypto.so* /gnu/store/*/lib64/libssl.so* /gnu/store/*/lib64/libcrypto.so* /lib/libssl.so* /lib/libcrypto.so* /lib64/libssl.so* /lib64/libcrypto.so* /usr/lib/libssl.so* /usr/lib/libcrypto.so* /usr/lib64/libssl.so* /usr/lib64/libcrypto.so*; do if [ -e "$lib" ]; then printf "%s\n" "$lib"; break; fi; done' 2>/dev/null || true)
    fi

    if [ -n "$resolved_ssl_file" ]; then
      RESOLVED_SSL_LIB_PATH=${resolved_ssl_file%/*}
    fi
  fi

  if [ -z "$RESOLVED_SSL_LIB_PATH" ] || [ ! -d "$RESOLVED_SSL_LIB_PATH" ]; then
    if [ "$MODE" = "run" ]; then
      fail 121 "missing required runtime OpenSSL path"
    fi
    return 0
  fi

  prepend_ld_library_path "$RESOLVED_SSL_LIB_PATH"
}

resolve_runtime_ncurses_path() {
  resolved_ncurses_file=''

  resolved_ncurses_file=$(cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --network --share="$REPO_ROOT=/workspace" -- bash -lc 'ldconfig -p 2>/dev/null | while IFS= read -r line; do case "$line" in *" => "*) lib=${line%% *}; case "$lib" in libncursesw.so*) printf "%s\n" "${line##* => }"; break ;; esac ;; esac; done' 2>/dev/null || true)

  if [ -z "$resolved_ncurses_file" ]; then
    resolved_ncurses_file=$(cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --network --share="$REPO_ROOT=/workspace" -- bash -lc 'for lib in /run/current-system/profile/lib/libncursesw.so* /run/current-system/profile/lib64/libncursesw.so* /gnu/store/*/lib/libncursesw.so* /gnu/store/*/lib64/libncursesw.so* /lib/libncursesw.so* /lib64/libncursesw.so* /usr/lib/libncursesw.so* /usr/lib64/libncursesw.so*; do if [ -e "$lib" ]; then printf "%s\n" "$lib"; break; fi; done' 2>/dev/null || true)
  fi

  if [ -n "$resolved_ncurses_file" ]; then
    prepend_ld_library_path "${resolved_ncurses_file%/*}"
  fi
}

validate_e2e_args() {
  if [ "$MODE" != "e2e" ]; then
    return 0
  fi

  if is_test_toggle_enabled CLAWMACS_TEST_INVALID_E2E_ARGS; then
    fail 122 "invalid e2e arguments"
  fi

  if e2e_invocation_requires_credential "$@"; then
    :
  fi
}

binary_visible() {
  binary="$1"
  toggle="$2"
  if is_test_toggle_enabled "$toggle"; then
    return 1
  fi
  run_in_container 'set -eu; command -v "$1" >/dev/null 2>&1' "$binary"
}

validate_mode_binaries() {
  if ! binary_visible sbcl CLAWMACS_TEST_HIDE_SBCL; then
    fail 113 "missing required binary: sbcl"
  fi

  if [ "$MODE" = "e2e" ] && ! binary_visible python3 CLAWMACS_TEST_HIDE_PYTHON3; then
    fail 114 "missing required binary: python3"
  fi
}

run_preflight() {
  validate_launcher_cli "$@"
  resolve_repo_root
  validate_guix_available
  validate_project_mount
  validate_mode_binaries
  validate_quicklisp_pin_values
  validate_e2e_args "$@"
  validate_provider_credential "$@"
  validate_override_path
  validate_runtime_openssl_path
  resolve_runtime_ncurses_path
  validate_quicklisp_bootstrap
}

clear_test_toggles() {
  names=$(env | grep '^CLAWMACS_TEST_' | cut -d= -f1 || true)
  for name in $names; do
    unset "$name"
  done
  unset CLAWMACS_ENABLE_TEST_TOGGLES
}

launch_payload() {
  shift_count="$1"
  shift

  while [ "$shift_count" -gt 0 ]; do
    shift
    shift_count=$((shift_count - 1))
  done

  if [ ! -f "$HOST_QUICKLISP_SETUP" ]; then
    fail 112 "quicklisp bootstrap failed"
  fi

  # Claude Code CLI: share credentials and expose Nix store for the binary
  # User init directory: share ~/.clawmacs.d/ so init.lisp is available
  extra_container_args=""
  if [ -n "$HOST_USER_HOME" ] && [ -d "$HOST_USER_HOME/.claude" ]; then
    extra_container_args="$extra_container_args --share=$HOST_USER_HOME/.claude=$WORKSPACE_HOME/.claude"
  fi
  if [ -n "$HOST_USER_HOME" ] && [ -d "$HOST_USER_HOME/.clawmacs.d" ]; then
    extra_container_args="$extra_container_args --share=$HOST_USER_HOME/.clawmacs.d=$WORKSPACE_HOME/.clawmacs.d"
  fi
  if [ -d "/nix" ]; then
    extra_container_args="$extra_container_args --expose=/nix"
  fi
  # X11 forwarding: expose the X socket and Xauthority so McCLIM (and any
  # other graphical toolkit) can connect to the host display server.
  if [ -d "/tmp/.X11-unix" ]; then
    extra_container_args="$extra_container_args --expose=/tmp/.X11-unix"
  fi
  if [ -n "${XAUTHORITY:-}" ] && [ -f "$XAUTHORITY" ]; then
    extra_container_args="$extra_container_args --expose=$XAUTHORITY"
  fi

  # Resolve the Claude Code CLI binary path so we can add it to PATH inside
  # the container.  The Nix wrapper lives in /nix/store/…/bin/claude and its
  # shebang + dependencies are all inside /nix/store (exposed above).
  if command -v claude >/dev/null 2>&1; then
    resolved_claude=$(readlink -f "$(command -v claude)" 2>/dev/null || true)
    if [ -n "$resolved_claude" ] && [ -x "$resolved_claude" ]; then
      export CLAWMACS_CLAUDE_CLI_DIR
      CLAWMACS_CLAUDE_CLI_DIR=$(dirname "$resolved_claude")
    fi
  fi

  # shellcheck disable=SC2086
  cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --network --preserve='TERM|DISPLAY|XAUTHORITY|ANTHROPIC_API_KEY|OPENAI_API_KEY|CLAUDE_CODE_OAUTH_TOKEN|ZAI_CODING_MAX_API_KEY|OPENROUTER_API_KEY|CLAWMACS_SSL_LIB|CLAWMACS_FONT_PATH|CLAWMACS_MCP_BIN|CLAWMACS_DEBUG_LOG|HOME|CLAWMACS_QUICKLISP_SETUP|XDG_CACHE_HOME|LD_LIBRARY_PATH|CLAWMACS_CLAUDE_CLI_DIR' --share="$REPO_ROOT=/workspace" $extra_container_args -- bash -lc 'cd /workspace && export HOME="${HOME:-/workspace/.cache/home}" CLAWMACS_QUICKLISP_SETUP="${CLAWMACS_QUICKLISP_SETUP:-/workspace/.cache/home/quicklisp/setup.lisp}" XDG_CACHE_HOME="${XDG_CACHE_HOME:-/workspace/.cache}"; if [ -n "${CLAWMACS_CLAUDE_CLI_DIR:-}" ]; then export PATH="$CLAWMACS_CLAUDE_CLI_DIR:$PATH"; fi; exec "$@"' bash "$@"
}

main() {
  run_preflight "$@"

  if [ "${CLAWMACS_DEBUG:-0}" = "1" ]; then
    diagnostic_env ANTHROPIC_API_KEY
    diagnostic_env OPENAI_API_KEY
  fi

  if [ "$PREFLIGHT_ONLY" -eq 1 ]; then
    exit 0
  fi

  clear_test_toggles

  launch_payload "$PAYLOAD_SHIFT_COUNT" "$@"
}

main "$@"
