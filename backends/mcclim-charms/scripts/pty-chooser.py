#!/usr/bin/env python3
"""Launch the McCLIM example chooser through the charms backend in a PTY."""

from __future__ import annotations

import argparse
import errno
import fcntl
import os
import pathlib
import pty
import select
import signal
import struct
import subprocess
import sys
import termios
import time


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--timeout", type=int, default=20)
    parser.add_argument("--rows", type=int, default=48)
    parser.add_argument("--cols", type=int, default=160)
    parser.add_argument("--artifacts", default="artifacts/chooser")
    args = parser.parse_args()

    root = pathlib.Path(__file__).resolve().parents[1]
    artifacts = root / args.artifacts
    artifacts.mkdir(parents=True, exist_ok=True)

    env = os.environ.copy()
    env.setdefault("TERM", "xterm-256color")
    env.setdefault("MCCLIM_SOURCE_ROOT", "/home/tay/reference/external_src/McCLIM/")
    env["MCCLIM_CHARMS_TRACE_EVENTS"] = "1"

    command = [
        str(root / "scripts" / "run-example.sh"),
        "--no-guix",
        "--chooser",
    ]

    started = time.monotonic()
    master_fd, slave_fd = pty.openpty()
    output = bytearray()
    process = None
    rendered = False
    clicked_calculator = False
    launched_calculator = False
    timed_out = False

    required_markers = [
        "Launching example chooser function DEMODEMO",
        "McCLIM Examples",
        "Sheets and input",
        "Formatting and Drawing",
    ]

    try:
        fcntl.ioctl(
            slave_fd,
            termios.TIOCSWINSZ,
            struct.pack("HHHH", args.rows, args.cols, 0, 0),
        )
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
            if all(marker in screen for marker in required_markers):
                rendered = True

            if rendered and not clicked_calculator:
                # xterm-256color advertises SGR mouse mode to ncurses. These
                # one-based coordinates target the Calculator chooser button in
                # the fixed PTY size above.
                def mouse_event(button: int, x: int, y: int, final: str = "M") -> bytes:
                    return f"\x1b[<{button};{x};{y}{final}".encode("ascii")

                os.write(master_fd, mouse_event(0, 16, 32))
                time.sleep(0.05)
                os.write(master_fd, mouse_event(0, 16, 32, "m"))
                time.sleep(0.05)
                clicked_calculator = True

            if clicked_calculator and (
                all(marker in screen for marker in ("AC", "CE"))
                or 'dispatch push-button release label="Calculator"' in screen
            ):
                launched_calculator = True
                break

            if process.poll() is not None:
                break

        if process.poll() is None:
            process.send_signal(signal.SIGTERM)
            try:
                process.wait(timeout=3)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=3)

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
    finally:
        if slave_fd != -1:
            os.close(slave_fd)
        os.close(master_fd)

    elapsed = time.monotonic() - started
    screen = output.decode("utf-8", errors="replace")
    returncode = process.returncode if process is not None and process.returncode is not None else 124
    missing = [marker for marker in required_markers if marker not in screen]

    (artifacts / "screen.txt").write_text(screen, encoding="utf-8")
    (artifacts / "status.txt").write_text(
        f"exit={returncode}\n"
        f"elapsed={elapsed:.3f}\n"
        "pty=true\n"
        f"rows={args.rows}\n"
        f"cols={args.cols}\n"
        f"rendered={str(rendered).lower()}\n"
        f"clicked-calculator={str(clicked_calculator).lower()}\n"
        f"launched-calculator={str(launched_calculator).lower()}\n"
        f"timed-out-before-render={str(timed_out and not rendered).lower()}\n"
        + "".join(f"missing={marker}\n" for marker in missing),
        encoding="utf-8",
    )

    bad_markers = ("debugger invoked", "Unhandled", "example launch failed")
    if any(marker in screen for marker in bad_markers):
        return 1
    if not rendered:
        return 1
    if not launched_calculator:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
