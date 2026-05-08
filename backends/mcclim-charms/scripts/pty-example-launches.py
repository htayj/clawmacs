#!/usr/bin/env python3
"""Launch every registered McCLIM example in a PTY and exercise basic input."""

from __future__ import annotations

import argparse
import errno
import json
import os
import pathlib
import pty
import fcntl
import select
import signal
import struct
import subprocess
import sys
import termios
import time


BAD_MARKERS = (
    "debugger invoked",
    "Unhandled",
    "example launch failed",
    "There is no applicable method",
    "invalid number of arguments",
)


def mouse_event(button: int, x: int, y: int) -> bytes:
    return f"\x1b[<{button};{x};{y}M".encode("ascii")


def mouse_release(x: int, y: int) -> bytes:
    return f"\x1b[<0;{x};{y}m".encode("ascii")


def launchable_examples(root: pathlib.Path) -> list[str]:
    output = subprocess.check_output(
        [str(root / "scripts" / "run-example.sh"), "--no-guix", "--list"],
        cwd=str(root),
        text=True,
    )
    names: list[str] = []
    for line in output.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("Launchable"):
            continue
        names.append(stripped.split()[0])
    return names


def drain(master_fd: int, output: bytearray) -> None:
    while True:
        try:
            chunk = os.read(master_fd, 8192)
        except BlockingIOError:
            break
        except OSError as exc:
            if exc.errno == errno.EIO:
                break
            raise
        if not chunk:
            break
        output.extend(chunk)


def run_one(
    root: pathlib.Path,
    name: str,
    artifacts: pathlib.Path,
    timeout: float,
    rows: int,
    cols: int,
) -> dict[str, object]:
    example_dir = artifacts / name
    example_dir.mkdir(parents=True, exist_ok=True)

    env = os.environ.copy()
    env.setdefault("TERM", "xterm-256color")
    env.setdefault("MCCLIM_SOURCE_ROOT", "/home/tay/reference/external_src/McCLIM/")

    command = [str(root / "scripts" / "run-example.sh"), "--no-guix", name]
    started = time.monotonic()
    master_fd, slave_fd = pty.openpty()
    output = bytearray()
    process: subprocess.Popen[bytes] | None = None
    rendered = False
    sent_keyboard = False
    sent_mouse = False
    sent_wheel = False
    planned_stop = False
    timed_out = False

    try:
        fcntl.ioctl(
            slave_fd,
            termios.TIOCSWINSZ,
            struct.pack("HHHH", rows, cols, 0, 0),
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

        deadline = started + timeout
        post_interaction_deadline: float | None = None
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                timed_out = True
                break

            ready, _, _ = select.select([master_fd], [], [], min(0.20, remaining))
            if ready:
                drain(master_fd, output)

            screen = output.decode("utf-8", errors="replace")
            if "Launching frame" in screen or "Launching function" in screen:
                rendered = True
            if "\x1b[?1049h" in screen:
                rendered = True

            if rendered and not sent_keyboard:
                os.write(master_fd, b"z")
                sent_keyboard = True
                time.sleep(0.05)

            if rendered and not sent_mouse:
                os.write(master_fd, mouse_event(0, 12, 8))
                time.sleep(0.05)
                os.write(master_fd, mouse_release(12, 8))
                sent_mouse = True
                time.sleep(0.05)

            if rendered and not sent_wheel:
                os.write(master_fd, mouse_event(64, 12, 8))
                time.sleep(0.05)
                os.write(master_fd, mouse_event(65, 12, 8))
                sent_wheel = True
                post_interaction_deadline = time.monotonic() + 0.75

            if post_interaction_deadline is not None and time.monotonic() >= post_interaction_deadline:
                planned_stop = True
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
        drain(master_fd, output)
    finally:
        if slave_fd != -1:
            os.close(slave_fd)
        os.close(master_fd)

    elapsed = time.monotonic() - started
    screen = output.decode("utf-8", errors="replace")
    returncode = process.returncode if process is not None and process.returncode is not None else 124
    bad = [marker for marker in BAD_MARKERS if marker in screen]
    ok = bool(rendered and sent_keyboard and sent_mouse and sent_wheel and not bad)
    if not planned_stop and returncode not in (0, -signal.SIGTERM):
        ok = False
    if timed_out and not rendered:
        ok = False

    (example_dir / "screen.txt").write_text(screen, encoding="utf-8")
    status = {
        "example": name,
        "ok": ok,
        "exit": returncode,
        "elapsed": round(elapsed, 3),
        "pty": True,
        "rendered": rendered,
        "sent_keyboard": sent_keyboard,
        "sent_mouse": sent_mouse,
        "sent_wheel": sent_wheel,
        "planned_stop": planned_stop,
        "timed_out": timed_out,
        "bad_markers": bad,
    }
    (example_dir / "status.json").write_text(json.dumps(status, indent=2), encoding="utf-8")
    return status


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--timeout-per-example", type=float, default=12)
    parser.add_argument("--rows", type=int, default=48)
    parser.add_argument("--cols", type=int, default=160)
    parser.add_argument("--artifacts", default="artifacts/example-launches")
    parser.add_argument("--examples", nargs="*", help="Optional subset of example names")
    args = parser.parse_args()

    root = pathlib.Path(__file__).resolve().parents[1]
    artifacts = root / args.artifacts
    artifacts.mkdir(parents=True, exist_ok=True)
    examples = args.examples or launchable_examples(root)

    results = [
        run_one(root, name, artifacts, args.timeout_per_example, args.rows, args.cols)
        for name in examples
    ]
    failures = [item for item in results if not item["ok"]]
    summary = {
        "examples": len(results),
        "passed": len(results) - len(failures),
        "failed": len(failures),
        "failures": failures,
    }
    (artifacts / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    (artifacts / "status.txt").write_text(
        ("pass\n" if not failures else "fail\n") + json.dumps(summary, indent=2) + "\n",
        encoding="utf-8",
    )
    print(json.dumps(summary, indent=2))
    return 0 if not failures else 1


if __name__ == "__main__":
    sys.exit(main())
