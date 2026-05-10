#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

if ! command -v sbcl >/dev/null 2>&1; then
  printf '%s\n' "run-native.sh: missing sbcl on PATH" >&2
  exit 127
fi

if [ -z "${CLAWMACS_ULTRALISP_SETUP:-}" ]; then
  if [ -n "${CLAWMACS_QUICKLISP_SETUP:-}" ]; then
    CLAWMACS_ULTRALISP_SETUP=$CLAWMACS_QUICKLISP_SETUP
  elif [ -f "${HOME:-}/quicklisp/setup.lisp" ]; then
    CLAWMACS_ULTRALISP_SETUP="${HOME}/quicklisp/setup.lisp"
  else
    printf '%s\n' "run-native.sh: set CLAWMACS_ULTRALISP_SETUP or install a Quicklisp-compatible setup at ~/quicklisp/setup.lisp" >&2
    exit 1
  fi
fi

prepend_ld_library_path() {
  case ":${LD_LIBRARY_PATH:-}:" in
    *:"$1":*) ;;
    *)
      if [ -n "${LD_LIBRARY_PATH:-}" ]; then
        LD_LIBRARY_PATH="$1:$LD_LIBRARY_PATH"
      else
        LD_LIBRARY_PATH="$1"
      fi
      ;;
  esac
}

resolve_openssl_runtime_path() {
  if [ -n "${CLAWMACS_SSL_LIB:-}" ] && [ -d "$CLAWMACS_SSL_LIB" ]; then
    printf '%s\n' "$CLAWMACS_SSL_LIB"
    return 0
  fi

  for word in ${NIX_LDFLAGS:-}; do
    case "$word" in
      -L*)
        dir=${word#-L}
        if [ -d "$dir" ] && { [ -e "$dir/libssl.so" ] || [ -e "$dir/libcrypto.so" ] || [ -e "$dir/libssl.so.3" ] || [ -e "$dir/libcrypto.so.3" ]; }; then
          printf '%s\n' "$dir"
          return 0
        fi
        ;;
    esac
  done

  if command -v ldconfig >/dev/null 2>&1; then
    path=$(ldconfig -p 2>/dev/null | awk '/lib(ssl|crypto)\.so/ && / => / { print $NF; exit }' || true)
    if [ -n "$path" ] && [ -e "$path" ]; then
      dirname -- "$path"
      return 0
    fi
  fi

  for lib in \
    /run/current-system/profile/lib/libssl.so* \
    /run/current-system/profile/lib/libcrypto.so* \
    /gnu/store/*/lib/libssl.so* \
    /gnu/store/*/lib/libcrypto.so* \
    /usr/lib/libssl.so* \
    /usr/lib/libcrypto.so* \
    /lib/libssl.so* \
    /lib/libcrypto.so* \
    /lib64/libssl.so* \
    /lib64/libcrypto.so*; do
    if [ -e "$lib" ]; then
      dirname -- "$lib"
      return 0
    fi
  done

  return 1
}

if ssl_lib_path=$(resolve_openssl_runtime_path); then
  prepend_ld_library_path "$ssl_lib_path"
  export LD_LIBRARY_PATH
fi

export CLAWMACS_ULTRALISP_SETUP
export CLAWMACS_SESSION_NAME=${CLAWMACS_SESSION_NAME:-"clawmacs:native-session-01"}
export CLAWMACS_DEBUG_LOG=${CLAWMACS_DEBUG_LOG:-"$SCRIPT_DIR/debug.log"}

clean_build=${CLAWMACS_RUN_CLEAN_BUILD:-0}
export CLAWMACS_RUN_CLEAN_BUILD="$clean_build"

cd "$SCRIPT_DIR"
exec sbcl --noinform --script scripts/run-ultralisp.lisp "$clean_build" "$@"
