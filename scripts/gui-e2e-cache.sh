#!/bin/sh

# Helpers for seeding each GUI E2E run's private ASDF cache from the cache that
# the Guix launcher warmed before entering the final application container.
# This file is sourced by run-gui-e2e.sh and by its focused shell regression.

GUI_E2E_CACHE_SEED_STATUS=not-attempted

gui_e2e_cold_cache_requested_p() {
  case "${CLAWMACS_GUI_E2E_COLD_CACHE:-0}" in
    1|true|TRUE|yes|YES|on|ON)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

gui_e2e_seed_private_common_lisp_cache() {
  gui_e2e_seed_source_root=$1
  gui_e2e_seed_private_root=$2
  gui_e2e_seed_lock_file=$3
  gui_e2e_seed_source="$gui_e2e_seed_source_root/common-lisp"
  gui_e2e_seed_destination="$gui_e2e_seed_private_root/common-lisp"
  gui_e2e_seed_stage="$gui_e2e_seed_private_root/.common-lisp-seed.$$"
  gui_e2e_seed_timeout=${CLAWMACS_GUI_E2E_CACHE_SEED_LOCK_TIMEOUT_SECONDS:-600}

  GUI_E2E_CACHE_SEED_STATUS=not-attempted

  if gui_e2e_cold_cache_requested_p; then
    GUI_E2E_CACHE_SEED_STATUS=cold-override
    return 0
  fi

  case "$gui_e2e_seed_source_root:$gui_e2e_seed_private_root:$gui_e2e_seed_lock_file" in
    /*:/*:/*)
      ;;
    *)
      GUI_E2E_CACHE_SEED_STATUS=seed-failed
      return 0
      ;;
  esac

  case "$gui_e2e_seed_timeout" in
    ''|*[!0-9]*)
      gui_e2e_seed_timeout=600
      ;;
  esac
  if [ "$gui_e2e_seed_timeout" -eq 0 ]; then
    gui_e2e_seed_timeout=600
  fi

  if [ ! -d "$gui_e2e_seed_source" ]; then
    GUI_E2E_CACHE_SEED_STATUS=source-unavailable
    return 0
  fi

  if [ -e "$gui_e2e_seed_destination" ]; then
    GUI_E2E_CACHE_SEED_STATUS=private-cache-present
    return 0
  fi

  mkdir -p "$gui_e2e_seed_private_root"
  rm -rf -- "$gui_e2e_seed_stage"
  mkdir -p "$gui_e2e_seed_stage"

  if (
    exec 9>>"$gui_e2e_seed_lock_file"
    flock -s -w "$gui_e2e_seed_timeout" 9
    cp -a --reflink=auto -- "$gui_e2e_seed_source/." "$gui_e2e_seed_stage/"
  ); then
    if [ -e "$gui_e2e_seed_destination" ]; then
      rm -rf -- "$gui_e2e_seed_stage"
      GUI_E2E_CACHE_SEED_STATUS=private-cache-present
    elif mv -- "$gui_e2e_seed_stage" "$gui_e2e_seed_destination"; then
      GUI_E2E_CACHE_SEED_STATUS=seeded
    else
      rm -rf -- "$gui_e2e_seed_stage"
      GUI_E2E_CACHE_SEED_STATUS=seed-failed
    fi
  else
    rm -rf -- "$gui_e2e_seed_stage"
    GUI_E2E_CACHE_SEED_STATUS=seed-failed
  fi

  return 0
}
