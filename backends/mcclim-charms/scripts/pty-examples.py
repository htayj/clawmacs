#!/usr/bin/env python3
"""Run the McCLIM examples loader in a PTY and save terminal evidence."""

from __future__ import annotations

import argparse
import errno
import fcntl
import os
import pty
import select
import signal
import pathlib
import subprocess
import sys
import time


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--timeout", type=int, default=60)
    parser.add_argument("--artifacts", default="artifacts/examples")
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
        str(root / "scripts" / "example-smoke.lisp"),
    ]
    started = time.monotonic()
    master_fd, slave_fd = pty.openpty()
    output = bytearray()
    process = None
    timed_out = False

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
    (artifacts / "stdout.txt").write_text(screen, encoding="utf-8")
    (artifacts / "stderr.txt").write_text("", encoding="utf-8")
    if timed_out:
        (artifacts / "status.txt").write_text(
            f"timeout\nelapsed={elapsed:.3f}\npty=true\n",
            encoding="utf-8",
        )
        return 124
    (artifacts / "status.txt").write_text(
        f"exit={returncode}\nelapsed={elapsed:.3f}\npty=true\n",
        encoding="utf-8",
    )

    bad_markers = ("debugger invoked", "Unhandled", "example smoke failed")
    if returncode != 0 or any(marker in screen for marker in bad_markers):
        return returncode or 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
