#!/bin/sh

GUI_E2E_XVFB_NAMESPACE_RETRY_STATUS=75

gui_e2e_run_container_with_retry() {
  gui_e2e_retry_attempt=1

  while :; do
    if "$@"; then
      return 0
    else
      gui_e2e_retry_status=$?
    fi

    if [ "$gui_e2e_retry_status" -ne \
         "$GUI_E2E_XVFB_NAMESPACE_RETRY_STATUS" ] || \
       [ "$gui_e2e_retry_attempt" -ge 2 ]; then
      return "$gui_e2e_retry_status"
    fi

    printf '[gui-e2e] retrying once with a fresh Guix X socket namespace\n' >&2
    gui_e2e_retry_attempt=$((gui_e2e_retry_attempt + 1))
  done
}
