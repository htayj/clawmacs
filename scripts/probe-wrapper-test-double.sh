#!/bin/sh
# Deterministic non-GUI process double for the public probe wrappers.
set -eu

state=${CLAWMACS_PROBE_TEST_STATE:?missing probe test state}
role=${1:?missing probe test-double role}
shift
printf '%s\n' "$role" >>"$state"

case "$role" in
  launcher)
    while [ "$#" -gt 0 ] && [ "$1" != "--" ]; do
      shift
    done
    [ "${1:-}" = "--" ]
    shift
    CLAWMACS_IN_GUIX_CONTAINER=1
    export CLAWMACS_IN_GUIX_CONTAINER
    exec "$@"
    ;;
  inner)
    [ "${CLAWMACS_IN_GUIX_CONTAINER:-0}" = "1" ]
    ;;
  xvfb)
    printf '%s\n' 77 >&3
    trap 'exit 0' TERM INT
    while :; do sleep 1; done
    ;;
  sbcl)
    if [ "${CLAWMACS_PROBE_TEST_HANG:-0}" = "1" ]; then
      trap "" TERM INT
      (
        trap "" TERM INT
        while :; do sleep 1; done
      ) &
      child_pid=$!
      printf 'stuck-leader=%s\n' "$$" >>"$state"
      printf 'stuck-child=%s\n' "$child_pid" >>"$state"
      while :; do sleep 1; done
    fi
    case "${CLAWMACS_PROBE_KIND:-}" in
      font)
        printf '%s\n' \
          'CLX_FONT_INVENTORY_PROBE_OK count=96 native-ttf=true listed-sizes=8'
        ;;
      appearance)
        printf '%s\n' \
          'APPEARANCE_LIVE_TWO_FRAME_OK' \
          'APPEARANCE_LIVE_STAGED_PROFILES_OK' \
          'APPEARANCE_LIVE_FONT_INVENTORIES_OK' \
          'APPEARANCE_LIVE_SAFE_ACTIVATION_OK' \
          'APPEARANCE_LIVE_ACTIVATION_ROLLBACK_OK' \
          'APPEARANCE_LIVE_PACKAGE_PUBLISH_OK' \
          'APPEARANCE_LIVE_PACKAGE_REMOVAL_ROLLBACK_OK' \
          'APPEARANCE_LIVE_TWO_FRAME_TEARDOWN_OK'
        ;;
      *)
        exit 65
        ;;
    esac
    ;;
  *)
    exit 66
    ;;
esac
