#!/usr/bin/env python3
"""Run backend interaction coverage through a PTY and save terminal evidence."""

from __future__ import annotations

import argparse
import errno
import fcntl
import os
import pathlib
import pty
import select
import signal
import subprocess
import sys
import time


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--timeout", type=int, default=30)
    parser.add_argument("--artifacts", default="artifacts/interactions")
    args = parser.parse_args()

    root = pathlib.Path(__file__).resolve().parents[1]
    artifacts = root / args.artifacts
    artifacts.mkdir(parents=True, exist_ok=True)

    env = os.environ.copy()
    env.setdefault("TERM", "xterm-256color")
    env.setdefault("MCCLIM_SOURCE_ROOT", "/home/tay/reference/external_src/McCLIM/")

    command = [
        "sbcl",
        "--noinform",
        "--disable-debugger",
        "--load",
        str(root / "scripts" / "interaction-e2e.lisp"),
    ]

    started = time.monotonic()
    master_fd, slave_fd = pty.openpty()
    output = bytearray()
    process = None
    timed_out = False
    sent_external_key = False
    sent_external_mouse = True

    try:
        flags = fcntl.fcntl(master_fd, fcntl.F_GETFL)
        fcntl.fcntl(master_fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)
        process = subprocess.Popen(
            command,
            cwd=str(root),
            env=env,
            stdin=slave_fd,
            stdout=slave_fd,
            stderr=slave_fd,
            close_fds=True,
        )
        os.close(slave_fd)
        slave_fd = -1

        deadline = started + args.timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                timed_out = True
                process.send_signal(signal.SIGTERM)
                break

            ready, _, _ = select.select([master_fd], [], [], min(0.25, remaining))
            if ready:
                try:
                    chunk = os.read(master_fd, 4096)
                except OSError as exc:
                    if exc.errno == errno.EIO:
                        break
                    raise
                if not chunk:
                    break
                output.extend(chunk)

            screen = output.decode("utf-8", errors="replace")
            if not sent_external_key and "MCCLIM-CHARMS-INTERACTION-READY" in screen:
                os.write(master_fd, b"z")
                sent_external_key = True
            if process.poll() is not None:
                while True:
                    try:
                        chunk = os.read(master_fd, 4096)
                    except BlockingIOError:
                        break
                    except OSError as exc:
                        if exc.errno == errno.EIO:
                            break
                        raise
                    if not chunk:
                        break
                    output.extend(chunk)
                break

        if timed_out:
            try:
                process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=2)
        elif process.poll() is None:
            process.wait(timeout=2)
    finally:
        if slave_fd != -1:
            os.close(slave_fd)
        os.close(master_fd)

    elapsed = time.monotonic() - started
    screen = output.decode("utf-8", errors="replace")
    returncode = process.returncode if process is not None and process.returncode is not None else 124
    (artifacts / "screen.txt").write_text(screen, encoding="utf-8")
    (artifacts / "status.txt").write_text(
        (
            "timeout\n" if timed_out else f"exit={returncode}\n"
        )
        + f"elapsed={elapsed:.3f}\n"
        + "pty=true\n"
        + f"external-key-sent={str(sent_external_key).lower()}\n"
        + f"external-mouse-sent={str(sent_external_mouse).lower()}\n",
        encoding="utf-8",
    )

    bad_markers = ("debugger invoked", "Unhandled", "interaction e2e failed")
    if timed_out:
        return 124
    if returncode != 0 or not sent_external_key or not sent_external_mouse:
        return returncode or 1
    if "MCCLIM-CHARMS-INTERACTION-PASS" not in screen:
        return 1
    if any(marker in screen for marker in bad_markers):
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
