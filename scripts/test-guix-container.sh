#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
LAUNCHER="$SCRIPT_DIR/guix-container.sh"
REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)

TMP_DIR=$(mktemp -d)
BOOTSTRAP_ENV="$SCRIPT_DIR/quicklisp-bootstrap.env"
BOOTSTRAP_ENV_BACKUP="$TMP_DIR/quicklisp-bootstrap.env.bak"
cp "$BOOTSTRAP_ENV" "$BOOTSTRAP_ENV_BACKUP"

cleanup() {
  cp "$BOOTSTRAP_ENV_BACKUP" "$BOOTSTRAP_ENV"
  rm -rf "$TMP_DIR"
}

trap cleanup EXIT INT TERM

TMP_BIN="$TMP_DIR/bin"
mkdir -p "$TMP_BIN"
TMP_CONTAINER_BIN="$TMP_DIR/container-bin"
mkdir -p "$TMP_CONTAINER_BIN"
TMP_SSL_LIB="$TMP_DIR/ssl/lib"
mkdir -p "$TMP_SSL_LIB"
touch "$TMP_SSL_LIB/libssl.so.3"
TMP_FONT_DIR="$TMP_DIR/fonts"
mkdir -p "$TMP_FONT_DIR"
TMP_FONT_FILE="$TMP_FONT_DIR/ok.ttf"
printf 'font\n' > "$TMP_FONT_FILE"
TMP_FONT_UNREADABLE="$TMP_FONT_DIR/unreadable.ttf"
cp "$TMP_FONT_FILE" "$TMP_FONT_UNREADABLE"
chmod 000 "$TMP_FONT_UNREADABLE"
TMP_MCP_DIR="$TMP_DIR/mcp"
mkdir -p "$TMP_MCP_DIR"
TMP_MCP_BIN="$TMP_MCP_DIR/mcp-tui-driver"
cat > "$TMP_MCP_BIN" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "$TMP_MCP_BIN"
TMP_MCP_NONEXEC="$TMP_MCP_DIR/nonexec-driver"
cp "$TMP_MCP_BIN" "$TMP_MCP_NONEXEC"
chmod 644 "$TMP_MCP_NONEXEC"

REAL_SHA256SUM=$(command -v sha256sum)
EXPECTED_SHA=$(printf 'quicklisp\n' | "$REAL_SHA256SUM" | cut -d' ' -f1)
export REAL_SHA256SUM
CLAWMACS_TEST_CONTAINER_PATH="$TMP_CONTAINER_BIN"
export CLAWMACS_TEST_CONTAINER_PATH

write_bootstrap_env() {
  sha="$1"
  printf '%s\n' \
    'QUICKLISP_BOOTSTRAP_URL=https://beta.quicklisp.org/quicklisp.lisp' \
    "QUICKLISP_BOOTSTRAP_SHA256=$sha" \
    'QUICKLISP_BOOTSTRAP_TIMEOUT_SECS=20' \
    'QUICKLISP_BOOTSTRAP_RETRIES=2' > "$BOOTSTRAP_ENV"
}

write_bootstrap_env "$EXPECTED_SHA"

cat > "$TMP_BIN/sbcl" <<'EOF'
#!/bin/sh
if [ "${CLAWMACS_FAIL_HOST_TOOL_USE:-0}" = "1" ]; then
  exit 98
fi
exec "$CLAWMACS_TEST_CONTAINER_PATH/sbcl" "$@"
EOF
chmod +x "$TMP_BIN/sbcl"

cat > "$TMP_CONTAINER_BIN/sbcl" <<'EOF'
#!/bin/sh
setup=''
is_probe=0
install_path=''
while [ "$#" -gt 0 ]; do
  case "$1" in
    --load)
      shift
      setup="$1"
      is_probe=1
      case "$setup" in
        */quicklisp.lisp)
          mkdir -p "$HOME/quicklisp"
          printf '(load "ok")\n' > "$HOME/quicklisp/setup.lisp"
          ;;
      esac
      ;;
    --eval)
      shift
      eval_arg="$1"
      case "$eval_arg" in
        *quicklisp-quickstart:install*':path "'*'"'*)
          install_path=$(printf '%s' "$eval_arg" | sed -n 's/.*:path "\([^"]*\)".*/\1/p')
          ;;
      esac
      ;;
    */quicklisp.lisp)
      mkdir -p "$HOME/quicklisp"
      printf '(load "ok")\n' > "$HOME/quicklisp/setup.lisp"
      ;;
  esac
  shift
done

if [ -n "$install_path" ]; then
  mkdir -p "$install_path"
  printf '(load "ok")\n' > "$install_path/setup.lisp"
fi

if [ "$is_probe" -eq 1 ]; then
  if [ "${CLAWMACS_TEST_SBCL_FAIL_PROBE:-0}" = "1" ]; then
    exit 1
  fi
  if [ ! -f "$setup" ]; then
    exit 1
  fi
fi

if [ "${CLAWMACS_TEST_SBCL_FAIL_BOOTSTRAP:-0}" = "1" ]; then
  exit 1
fi

exit 0
EOF
chmod +x "$TMP_CONTAINER_BIN/sbcl"

cat > "$TMP_BIN/python3" <<'EOF'
#!/bin/sh
if [ "${CLAWMACS_FAIL_HOST_TOOL_USE:-0}" = "1" ]; then
  exit 98
fi
exec "$CLAWMACS_TEST_CONTAINER_PATH/python3" "$@"
EOF
chmod +x "$TMP_BIN/python3"

cat > "$TMP_CONTAINER_BIN/python3" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "$TMP_CONTAINER_BIN/python3"

cat > "$TMP_BIN/guix" <<'EOF'
#!/bin/sh
share=''
while [ "$#" -gt 0 ]; do
  case "$1" in
    --share=*)
      share=${1#--share=}
      ;;
  esac
  if [ "$1" = "--" ]; then
    shift
    break
  fi
  shift
done
if [ "$#" -ge 3 ] && [ "$1" = "bash" ] && [ "$2" = "-lc" ]; then
  command="$3"
  workspace=${share%=/workspace}
  if [ -n "$workspace" ]; then
    command=$(printf '%s' "$command" | sed "s|/workspace|$workspace|g")
  fi
  if [ "$#" -ge 4 ] && [ "$4" = "bash" ]; then
    shift 4
    if [ -n "$workspace" ]; then
      mapped_args=''
      while [ "$#" -gt 0 ]; do
        arg="$1"
        shift
        case "$arg" in
          /workspace)
            arg="$workspace"
            ;;
          /workspace/*)
            arg="$workspace${arg#/workspace}"
            ;;
        esac
        mapped_args="$mapped_args
$arg"
      done
      set --
      old_ifs=$IFS
      IFS='
'
      for arg in $mapped_args; do
        set -- "$@" "$arg"
      done
      IFS=$old_ifs
    fi
    if [ -n "${CLAWMACS_TEST_CONTAINER_PATH:-}" ]; then
      PATH="$CLAWMACS_TEST_CONTAINER_PATH:$PATH"
      export PATH
    fi
    sh -c "$command" sh "$@"
    exit $?
  fi
  shift 3
  if [ -n "${CLAWMACS_TEST_CONTAINER_PATH:-}" ]; then
    PATH="$CLAWMACS_TEST_CONTAINER_PATH:$PATH"
    export PATH
  fi
  sh -c "$command"
  exit $?
fi
"$@"
EOF
chmod +x "$TMP_BIN/guix"

cat > "$TMP_BIN/curl" <<'EOF'
#!/bin/sh
if [ "${CLAWMACS_FAIL_HOST_TOOL_USE:-0}" = "1" ]; then
  exit 98
fi
exec "$CLAWMACS_TEST_CONTAINER_PATH/curl" "$@"
EOF
chmod +x "$TMP_BIN/curl"

cat > "$TMP_CONTAINER_BIN/curl" <<'EOF'
#!/bin/sh
if [ "${CLAWMACS_TEST_CURL_FAIL:-0}" = "1" ]; then
  exit 22
fi

outfile=''
seen_timeout=''
seen_retries=''
while [ "$#" -gt 0 ]; do
  case "$1" in
    --max-time)
      shift
      seen_timeout="$1"
      ;;
    --retry)
      shift
      seen_retries="$1"
      ;;
    -o)
      shift
      outfile="$1"
      ;;
  esac
  shift
done

if [ -n "${CLAWMACS_EXPECT_CURL_TIMEOUT:-}" ] && [ "$seen_timeout" != "$CLAWMACS_EXPECT_CURL_TIMEOUT" ]; then
  exit 99
fi
if [ -n "${CLAWMACS_EXPECT_CURL_RETRIES:-}" ] && [ "$seen_retries" != "$CLAWMACS_EXPECT_CURL_RETRIES" ]; then
  exit 99
fi

printf '%s\n' "${CLAWMACS_TEST_CURL_BODY:-quicklisp}" > "$outfile"
exit 0
EOF
chmod +x "$TMP_CONTAINER_BIN/curl"

cat > "$TMP_BIN/sha256sum" <<'EOF'
#!/bin/sh
if [ "${CLAWMACS_FAIL_HOST_TOOL_USE:-0}" = "1" ]; then
  exit 98
fi
exec "$CLAWMACS_TEST_CONTAINER_PATH/sha256sum" "$@"
EOF
chmod +x "$TMP_BIN/sha256sum"

cat > "$TMP_CONTAINER_BIN/sha256sum" <<'EOF'
#!/bin/sh
exec "$REAL_SHA256SUM" "$@"
EOF
chmod +x "$TMP_CONTAINER_BIN/sha256sum"

cat > "$TMP_BIN/ldconfig" <<EOF
#!/bin/sh
if [ "\${CLAWMACS_TEST_LDCONFIG_EMPTY:-0}" = "1" ]; then
  exit 0
fi
if [ "\${CLAWMACS_TEST_LDCONFIG_MODE:-ssl}" = "crypto" ]; then
  echo "libcrypto.so.3 (libc6,x86-64) => $TMP_SSL_LIB/libcrypto.so.3"
  exit 0
fi
echo "libssl.so.3 (libc6,x86-64) => $TMP_SSL_LIB/libssl.so.3"
EOF
chmod +x "$TMP_BIN/ldconfig"

run_case() {
  name="$1"
  expected_code="$2"
  expect_prefix="$3"
  toggle_var="$4"
  shift 4

  stderr_file="$TMP_DIR/$name.stderr"
  set +e
  if [ "$toggle_var" = "-" ]; then
    env PATH="$TMP_BIN:$PATH" \
      CLAWMACS_ENABLE_TEST_TOGGLES=1 \
      CLAWMACS_SSL_LIB="$TMP_SSL_LIB" \
      "$LAUNCHER" "$@" 2>"$stderr_file"
  else
    env PATH="$TMP_BIN:$PATH" \
      CLAWMACS_ENABLE_TEST_TOGGLES=1 \
      CLAWMACS_SSL_LIB="$TMP_SSL_LIB" \
      "$toggle_var=1" \
      "$LAUNCHER" "$@" 2>"$stderr_file"
  fi
  actual_code=$?
  set -e

  if [ "$actual_code" -ne "$expected_code" ]; then
    echo "FAIL $name: expected exit $expected_code got $actual_code" >&2
    cat "$stderr_file" >&2
    exit 1
  fi

  if [ "$expect_prefix" = "yes" ] && ! grep -q '^\[clawmacs-env\]' "$stderr_file"; then
    echo "FAIL $name: missing stderr prefix" >&2
    cat "$stderr_file" >&2
    exit 1
  fi
}

run_override_case() {
  name="$1"
  shift

  stderr_file="$TMP_DIR/$name.stderr"
  set +e
  env PATH="$TMP_BIN:$PATH" \
    CLAWMACS_ENABLE_TEST_TOGGLES=1 \
    CLAWMACS_SSL_LIB="$TMP_SSL_LIB" \
    "$@" \
    "$LAUNCHER" --mode run --preflight-only 2>"$stderr_file"
  actual_code=$?
  set -e

  if [ "$actual_code" -ne 117 ]; then
    echo "FAIL $name: expected exit 117 got $actual_code" >&2
    cat "$stderr_file" >&2
    exit 1
  fi

  if ! grep -q '^\[clawmacs-env\]' "$stderr_file"; then
    echo "FAIL $name: missing stderr prefix" >&2
    cat "$stderr_file" >&2
    exit 1
  fi
}

run_env_case() {
  name="$1"
  expected_code="$2"
  shift 2

  stderr_file="$TMP_DIR/$name.stderr"
  set +e
  env PATH="$TMP_BIN:$PATH" \
    CLAWMACS_ENABLE_TEST_TOGGLES=1 \
    CLAWMACS_SSL_LIB="$TMP_SSL_LIB" \
    "$@" 2>"$stderr_file"
  actual_code=$?
  set -e

  if [ "$actual_code" -ne "$expected_code" ]; then
    echo "FAIL $name: expected exit $expected_code got $actual_code" >&2
    cat "$stderr_file" >&2
    exit 1
  fi

  if [ "$expected_code" -ne 0 ] && ! grep -q '^\[clawmacs-env\]' "$stderr_file"; then
    echo "FAIL $name: missing stderr prefix" >&2
    cat "$stderr_file" >&2
    exit 1
  fi
}

run_case invalid-mode 118 yes - --mode nope -- true
run_case missing-mode 118 yes - -- true
run_case missing-payload 119 yes - --mode run --
run_case missing-guix-toggle 110 yes CLAWMACS_TEST_MISSING_GUIX --mode run --preflight-only
run_case repo-root-failure-toggle 120 yes CLAWMACS_TEST_FAIL_REPO_ROOT --mode run --preflight-only
run_case missing-mount-toggle 111 yes CLAWMACS_TEST_MISSING_MOUNT --mode run --preflight-only
run_case hidden-sbcl-run 113 yes CLAWMACS_TEST_HIDE_SBCL --mode run --preflight-only
run_case hidden-sbcl-e2e 113 yes CLAWMACS_TEST_HIDE_SBCL --mode e2e --preflight-only
run_case hidden-python3-e2e 114 yes CLAWMACS_TEST_HIDE_PYTHON3 --mode e2e --preflight-only
run_case quicklisp-bootstrap-toggle 112 yes CLAWMACS_TEST_QUICKLISP_BOOTSTRAP_FAIL --mode run --preflight-only
run_case missing-provider-credential-toggle 116 yes CLAWMACS_TEST_MISSING_PROVIDER_CREDENTIAL --mode run --preflight-only
run_case invalid-override-path-toggle 117 yes CLAWMACS_TEST_INVALID_OVERRIDE_PATH --mode run --preflight-only
run_case invalid-e2e-args-toggle 122 yes CLAWMACS_TEST_INVALID_E2E_ARGS --mode e2e --preflight-only
run_env_case preflight-precedence-credential-over-bootstrap 116 CLAWMACS_TEST_MISSING_PROVIDER_CREDENTIAL=1 CLAWMACS_TEST_QUICKLISP_BOOTSTRAP_FAIL=1 "$LAUNCHER" --mode run --preflight-only
run_env_case preflight-precedence-override-over-bootstrap 117 CLAWMACS_TEST_INVALID_OVERRIDE_PATH=1 CLAWMACS_TEST_QUICKLISP_BOOTSTRAP_FAIL=1 "$LAUNCHER" --mode run --preflight-only
run_env_case preflight-precedence-openssl-over-bootstrap 121 CLAWMACS_TEST_MISSING_OPENSSL_PATH=1 CLAWMACS_TEST_QUICKLISP_BOOTSTRAP_FAIL=1 "$LAUNCHER" --mode run --preflight-only
run_env_case e2e-credential-missing-required 116 "$LAUNCHER" --mode e2e --preflight-only -- python3 test-e2e.py --only provider
run_env_case e2e-credential-openai-present 0 OPENAI_API_KEY=dummy "$LAUNCHER" --mode e2e --preflight-only -- python3 test-e2e.py --only provider
run_env_case e2e-credential-anthropic-present 0 ANTHROPIC_API_KEY=dummy "$LAUNCHER" --mode e2e --preflight-only -- python3 test-e2e.py --only provider
run_env_case e2e-credential-optional-readline 0 "$LAUNCHER" --mode e2e --preflight-only -- python3 test-e2e.py --only readline
run_env_case e2e-credential-generic-command-optional 0 "$LAUNCHER" --mode e2e --preflight-only -- python3 -c 'print("ok")'
run_env_case e2e-credential-default-test-e2e-required 116 "$LAUNCHER" --mode e2e --preflight-only -- python3 test-e2e.py
run_env_case e2e-credential-last-only-wins-required 116 "$LAUNCHER" --mode e2e --preflight-only -- python3 test-e2e.py --only readline --only provider
run_env_case e2e-credential-last-only-wins-optional 0 "$LAUNCHER" --mode e2e --preflight-only -- python3 test-e2e.py --only provider --only readline
run_env_case e2e-args-only-missing-value 122 OPENAI_API_KEY=dummy "$LAUNCHER" --mode e2e --preflight-only -- python3 test-e2e.py --only
run_env_case host-tools-not-required-for-run-preflight 0 CLAWMACS_FAIL_HOST_TOOL_USE=1 "$LAUNCHER" --mode run --preflight-only
run_env_case host-tools-not-required-for-e2e-preflight 0 CLAWMACS_FAIL_HOST_TOOL_USE=1 OPENAI_API_KEY=dummy "$LAUNCHER" --mode e2e --preflight-only -- python3 --version
run_case payload-exit-code-passthrough 37 no - --mode run -- sh -c 'exit 37'

run_override_case ssl-override-missing-path CLAWMACS_SSL_LIB="$TMP_DIR/ssl/missing"
run_override_case ssl-override-non-directory CLAWMACS_SSL_LIB="$TMP_SSL_LIB/libssl.so.3"
run_override_case ssl-override-traversal CLAWMACS_SSL_LIB="$TMP_DIR/ssl/../ssl/lib"
run_override_case ssl-override-disallowed-prefix CLAWMACS_SSL_LIB="/usr/lib"

run_override_case font-override-missing-path CLAWMACS_FONT_PATH="$TMP_FONT_DIR/missing.ttf"
run_override_case font-override-unreadable CLAWMACS_FONT_PATH="$TMP_FONT_UNREADABLE"
run_override_case font-override-non-file CLAWMACS_FONT_PATH="$TMP_FONT_DIR"
run_override_case font-override-traversal CLAWMACS_FONT_PATH="$TMP_FONT_DIR/../fonts/ok.ttf"
run_override_case font-override-disallowed-prefix CLAWMACS_FONT_PATH="/etc/passwd"

run_override_case mcp-override-missing CLAWMACS_MCP_BIN="$TMP_MCP_DIR/missing-driver"
run_override_case mcp-override-non-executable CLAWMACS_MCP_BIN="$TMP_MCP_NONEXEC"
run_override_case mcp-override-traversal CLAWMACS_MCP_BIN="$TMP_MCP_DIR/../mcp/mcp-tui-driver"
run_override_case mcp-override-disallowed-prefix CLAWMACS_MCP_BIN="/bin/sh"

write_bootstrap_env "$EXPECTED_SHA"
printf '%s\n' \
  'QUICKLISP_BOOTSTRAP_URL=https://beta.quicklisp.org/quicklisp.lisp' \
  'QUICKLISP_BOOTSTRAP_SHA256=REPLACE_ME' \
  'QUICKLISP_BOOTSTRAP_TIMEOUT_SECS=20' \
  'QUICKLISP_BOOTSTRAP_RETRIES=2' > "$BOOTSTRAP_ENV"
set +e
env PATH="$TMP_BIN:$PATH" \
  CLAWMACS_ENABLE_TEST_TOGGLES=1 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/pin-sentinel.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 115 ]; then
  echo "FAIL quicklisp-pin-values-sentinel: expected exit 115 got $actual_code" >&2
  cat "$TMP_DIR/pin-sentinel.stderr" >&2
  exit 1
fi

write_bootstrap_env "$EXPECTED_SHA"
set +e
env PATH="$TMP_BIN:$PATH" \
  CLAWMACS_ENABLE_TEST_TOGGLES=1 \
  CLAWMACS_SSL_LIB="$TMP_SSL_LIB" \
  CLAWMACS_EXPECT_CURL_TIMEOUT=27 \
  CLAWMACS_EXPECT_CURL_RETRIES=4 \
  QUICKLISP_BOOTSTRAP_TIMEOUT_SECS=27 \
  QUICKLISP_BOOTSTRAP_RETRIES=4 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/bootstrap-timeout.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL bootstrap-timeout-retries: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/bootstrap-timeout.stderr" >&2
  exit 1
fi

set +e
rm -rf "$REPO_ROOT/.cache/home/quicklisp"
env PATH="$TMP_BIN:$PATH" \
  CLAWMACS_ENABLE_TEST_TOGGLES=1 \
  CLAWMACS_SSL_LIB="$TMP_SSL_LIB" \
  QUICKLISP_BOOTSTRAP_URL=https://invalid.example/quicklisp.lisp \
  QUICKLISP_BOOTSTRAP_SHA256=0000000000000000000000000000000000000000000000000000000000000000 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/bootstrap-pin-override.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL quicklisp-pin-override-ignored: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/bootstrap-pin-override.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  CLAWMACS_ENABLE_TEST_TOGGLES=1 \
  CLAWMACS_SSL_LIB="$TMP_SSL_LIB" \
  CLAWMACS_TEST_SBCL_FAIL_PROBE=1 \
  CLAWMACS_TEST_CURL_FAIL=1 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/bootstrap-fail.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 112 ]; then
  echo "FAIL bootstrap-failure: expected exit 112 got $actual_code" >&2
  cat "$TMP_DIR/bootstrap-fail.stderr" >&2
  exit 1
fi

mkdir -p "$REPO_ROOT/.cache/home/quicklisp"
printf '(load "existing")\n' > "$REPO_ROOT/.cache/home/quicklisp/setup.lisp"
set +e
env PATH="$TMP_BIN:$PATH" \
  CLAWMACS_ENABLE_TEST_TOGGLES=1 \
  CLAWMACS_SSL_LIB="$TMP_SSL_LIB" \
  CLAWMACS_TEST_SBCL_FAIL_PROBE=1 \
  CLAWMACS_TEST_CURL_FAIL=1 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/bootstrap-preserve.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 112 ]; then
  echo "FAIL bootstrap-failure-preserves-existing-cache: expected exit 112 got $actual_code" >&2
  cat "$TMP_DIR/bootstrap-preserve.stderr" >&2
  exit 1
fi
if [ ! -f "$REPO_ROOT/.cache/home/quicklisp/setup.lisp" ]; then
  echo "FAIL bootstrap-failure-preserves-existing-cache: expected existing setup to remain" >&2
  cat "$TMP_DIR/bootstrap-preserve.stderr" >&2
  exit 1
fi
if ! grep -q 'existing' "$REPO_ROOT/.cache/home/quicklisp/setup.lisp"; then
  echo "FAIL bootstrap-failure-preserves-existing-cache: expected existing setup content" >&2
  cat "$TMP_DIR/bootstrap-preserve.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  CLAWMACS_ENABLE_TEST_TOGGLES=1 \
  CLAWMACS_TEST_MISSING_OPENSSL_PATH=1 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/openssl-run.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 121 ]; then
  echo "FAIL missing-openssl-runtime-path: expected exit 121 got $actual_code" >&2
  cat "$TMP_DIR/openssl-run.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  CLAWMACS_ENABLE_TEST_TOGGLES=1 \
  CLAWMACS_SSL_LIB="$TMP_DIR/does-not-exist" \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/openssl-override-invalid.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 117 ]; then
  echo "FAIL openssl-override-invalid: expected exit 117 got $actual_code" >&2
  cat "$TMP_DIR/openssl-override-invalid.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  CLAWMACS_ENABLE_TEST_TOGGLES=1 \
  CLAWMACS_SSL_LIB= \
  CLAWMACS_TEST_LDCONFIG_EMPTY=1 \
  OPENAI_API_KEY=dummy \
  "$LAUNCHER" --mode e2e --preflight-only -- python3 --version 2>"$TMP_DIR/openssl-e2e.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL e2e-openssl-unresolved-allowed: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/openssl-e2e.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  CLAWMACS_ENABLE_TEST_TOGGLES=1 \
  CLAWMACS_SSL_LIB= \
  CLAWMACS_TEST_LDCONFIG_MODE=crypto \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/openssl-run-libcrypto.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL openssl-runtime-path-libcrypto-soname: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/openssl-run-libcrypto.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  CLAWMACS_ENABLE_TEST_TOGGLES=1 \
  CLAWMACS_SSL_LIB="$TMP_SSL_LIB" \
  "$LAUNCHER" --mode run -- sh -c 'test "$HOME" = "/workspace/.cache/home" && test "$CLAWMACS_QUICKLISP_SETUP" = "/workspace/.cache/home/quicklisp/setup.lisp" && test -f ".cache/home/quicklisp/setup.lisp"' 2>"$TMP_DIR/runtime-env.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL runtime-env-and-setup: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/runtime-env.stderr" >&2
  exit 1
fi

echo "ok"
