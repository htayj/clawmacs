#!/usr/bin/env python3
"""Launch examples through the McCLIM chooser and exercise the launched frame."""

from __future__ import annotations

import argparse
import errno
import json
import os
import pathlib
import pty
import fcntl
import re
import select
import signal
import struct
import subprocess
import sys
import termios
import time

from example_features import example_keystroke_events


BAD_MARKERS = (
    "debugger invoked",
    "Unhandled",
    "example launch failed",
    "There is no applicable method",
    "invalid number of arguments",
)

BUTTON_LABELS = {
    "CLIM-Fig": "clim-fig",
    "Calculator": "calculator",
    "Method Browser": "method-browser",
    "Address Book": "address-book",
    "Dingus": "dingus",
    "Modifier": "modifier",
    "Puzzle": "puzzle",
    "Colorslider": "colorslider",
    "Logic Cube": "logic-cube",
    "Checkers": "checkers",
    "Draggable Graph": "draggable-graph",
    "Tab Layout": "tabdemo",
    "German Towns": "town-example",
    "Data Graph Toy": "graph-toy",
    "Traffic lights": "traffic-lights",
    "Image Transform": "image-transform-demo",
    "File manager": "file-manager",
    "Stopwatch": "stopwatch",
    "Sheet geometry": "sheet-geometry",
    "Stream test": "stream-test",
    "Summation": "summation",
    "Menu Test": "menu-test",
    "Slider Test": "slider-test",
    "Gadget Test": "gadget-test",
    "Text Gadgets": "text-gadgets",
    "Accepting Values": "accepting-values-test",
    "Pane hierarchy viewer": "hierarchy-tool",
    "Tracking Pointer test": "tracking-pointer",
    "Selections (clipboard)": "selection",
    "DND various": "dnd-commented",
    "Border Styles Test": "bordered-output-examples",
    "Borders and Outlines": "borders-and-outlines",
    "Tables with borders": "tabledemo",
    "Draw Text test": "text-transformation-test",
    "SEOS baseline and wrapping": "seos-baseline",
    "SEOS with-room-for-graphics": "seos-wrfg",
    "Indentation": "indentation",
    "Graph formatting": "graph-formatting-test",
    "Flipping ink": "flipping-ink",
    "Patterns and designs": "patterns",
    "Overlapping patterns": "patterns-overlap",
    "Patterns on the Text Line": "wrfg-test",
    "Nested clipping": "nested-clipping",
    "Misc. Tests": "misc-tests",
    "Drawing Tests": "drawing-tests",
    "Frame Icon and Name Test": "frame-sheet-name-test",
    "Frame reinitialize": "reinitialize-frame",
    "Asynchronous commands": "asynchronous-commands",
    "Indirect gestures": "indirect-gestures",
    "Timer gestures": "timer-gestures",
    "Presentation translators": "presentation-translators-test",
    "Drag and drop translator": "dragndrop-translator",
    "Drag and drop": "dragndrop",
    "Text Size Test": "text-size-test",
    "Render Image Tests": "render-image-tests",
    "Pixmaps": "pixmaps",
    "Drawing Benchmark": "drawing-benchmark",
    "Coordinate swizzling": "coordinate-swizzling",
    "Incremental Redisplay": "unique-id-test",
    "Animations with pulse": "animation-pulse",
    "Concurrent Drawing": "concurrent-draw",
    "Concurrent Writing": "concurrent-text",
    "Concurrent Griding": "concurrent-grid",
}

BUTTON_COORDS = {
    "CLIM-Fig": [16, 20],
    "Calculator": [16, 30],
    "Method Browser": [16, 40],
    "Address Book": [16, 50],
    "Dingus": [16, 70],
    "Modifier": [16, 80],
    "Puzzle": [16, 90],
    "Colorslider": [16, 110],
    "Logic Cube": [16, 120],
    "Checkers": [16, 130],
    "Draggable Graph": [16, 140],
    "Tab Layout": [16, 170],
    "German Towns": [16, 180],
    "Data Graph Toy": [16, 200],
    "Traffic lights": [16, 210],
    "Image Transform": [16, 220],
    "File manager": [16, 230],
    "Stopwatch": [16, 240],
    "Sheet geometry": [16, 250],
    "Stream test": [56, 20],
    "Summation": [56, 30],
    "Menu Test": [56, 130],
    "Slider Test": [56, 140],
    "Gadget Test": [56, 160],
    "Text Gadgets": [56, 170],
    "Accepting Values": [56, 180],
    "Pane hierarchy viewer": [56, 200],
    "Tracking Pointer test": [56, 210],
    "Selections (clipboard)": [56, 220],
    "DND various": [56, 235],
    "Border Styles Test": [100, 20],
    "Borders and Outlines": [100, 30],
    "Tables with borders": [100, 40],
    "Draw Text test": [100, 50],
    "SEOS baseline and wrapping": [100, 70],
    "SEOS with-room-for-graphics": [100, 80],
    "Indentation": [100, 90],
    "Graph formatting": [100, 110],
    "Flipping ink": [100, 120],
    "Patterns and designs": [100, 130],
    "Overlapping patterns": [100, 140],
    "Patterns on the Text Line": [100, 160],
    "Nested clipping": [100, 170],
    "Misc. Tests": [100, 180],
    "Drawing Tests": [100, 200],
    "Frame Icon and Name Test": [140, 20],
    "Frame reinitialize": [140, 30],
    "Asynchronous commands": [140, 40],
    "Indirect gestures": [140, 50],
    "Timer gestures": [140, 70],
    "Presentation translators": [140, 80],
    "Drag and drop translator": [140, 90],
    "Drag and drop": [140, 110],
    "Text Size Test": [140, 120],
    "Render Image Tests": [140, 130],
    "Pixmaps": [140, 140],
    "Drawing Benchmark": [140, 160],
    "Coordinate swizzling": [140, 170],
    "Incremental Redisplay": [140, 180],
    "Animations with pulse": [140, 200],
    "Concurrent Drawing": [140, 210],
    "Concurrent Writing": [140, 220],
    "Concurrent Griding": [140, 235],
}

LABEL_PATTERN = re.compile(r'dispatch push-button release label="((?:[^"\\]|\\.)*)"')


def mouse_event(button: int, x: int, y: int, final: str = "M") -> bytes:
    return f"\x1b[<{button};{x};{y}{final}".encode("ascii")


def mouse_click(x: int, y: int) -> bytes:
    return mouse_event(0, x, y) + mouse_event(0, x, y, "m")


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


def chooser_env() -> dict[str, str]:
    env = os.environ.copy()
    env.setdefault("TERM", "xterm-256color")
    env.setdefault("MCCLIM_SOURCE_ROOT", "/home/tay/reference/external_src/McCLIM/")
    env["MCCLIM_CHARMS_TRACE_EVENTS"] = "1"
    return env


def chooser_ready(screen: str) -> bool:
    return all(
        marker in screen
        for marker in (
            "Launching example chooser function DEMODEMO",
            "McCLIM Examples",
            "Sheets and input",
            "Formatting and Drawing",
        )
    )


def start_chooser(root: pathlib.Path, rows: int, cols: int):
    master_fd, slave_fd = pty.openpty()
    fcntl.ioctl(slave_fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
    flags = fcntl.fcntl(master_fd, fcntl.F_GETFL)
    fcntl.fcntl(master_fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)
    process = subprocess.Popen(
        [str(root / "scripts" / "run-example.sh"), "--no-guix", "--chooser"],
        cwd=str(root),
        env=chooser_env(),
        stdin=slave_fd,
        stdout=slave_fd,
        stderr=slave_fd,
        close_fds=True,
    )
    os.close(slave_fd)
    return process, master_fd


def stop_process(process: subprocess.Popen[bytes], master_fd: int) -> None:
    if process.poll() is None:
        process.send_signal(signal.SIGTERM)
        try:
            process.wait(timeout=3)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait(timeout=3)
    os.close(master_fd)


def run_click(
    root: pathlib.Path,
    x: int,
    y: int,
    timeout: float,
    rows: int,
    cols: int,
    exercise_launched_frame: bool,
    exercise_chooser_wheel: bool = True,
) -> tuple[str, int, bool, bool, bool, list[str], bool]:
    process, master_fd = start_chooser(root, rows, cols)
    output = bytearray()
    rendered = False
    clicked = False
    sent_keyboard = False
    sent_declared_keystrokes = False
    sent_app_click = False
    sent_wheel = False
    sent_chooser_wheel = False
    label = ""
    deadline = time.monotonic() + timeout
    post_interaction_deadline: float | None = None
    try:
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                break
            ready, _, _ = select.select([master_fd], [], [], min(0.20, remaining))
            if ready:
                drain(master_fd, output)
            screen = output.decode("utf-8", errors="replace")
            if chooser_ready(screen):
                rendered = True
            if rendered and exercise_chooser_wheel and not sent_chooser_wheel:
                os.write(master_fd, mouse_event(64, 12, 8))
                time.sleep(0.05)
                os.write(master_fd, mouse_event(65, 12, 8))
                sent_chooser_wheel = True
                time.sleep(0.05)
            if rendered and not clicked:
                os.write(master_fd, mouse_click(x, y))
                clicked = True
                time.sleep(0.10)
            match = LABEL_PATTERN.search(screen)
            if match and not label:
                label = bytes(match.group(1), "utf-8").decode("unicode_escape")
                if not exercise_launched_frame:
                    break
                time.sleep(0.20)
            if label and exercise_launched_frame and not sent_keyboard:
                os.write(master_fd, b"z")
                sent_keyboard = True
                time.sleep(0.05)
            if label and exercise_launched_frame and not sent_declared_keystrokes:
                example = BUTTON_LABELS.get(label, "")
                for _, encoded in example_keystroke_events(example):
                    os.write(master_fd, encoded)
                    time.sleep(0.03)
                sent_declared_keystrokes = True
            if label and exercise_launched_frame and not sent_app_click:
                os.write(master_fd, mouse_click(12, 8))
                sent_app_click = True
                time.sleep(0.05)
            if label and exercise_launched_frame and not sent_wheel:
                os.write(master_fd, mouse_event(64, 12, 8))
                time.sleep(0.05)
                os.write(master_fd, mouse_event(65, 12, 8))
                sent_wheel = True
                post_interaction_deadline = time.monotonic() + 0.75
            if post_interaction_deadline is not None and time.monotonic() >= post_interaction_deadline:
                break
            if process.poll() is not None:
                break
        drain(master_fd, output)
    finally:
        stop_process(process, master_fd)
    screen = output.decode("utf-8", errors="replace")
    bad = [marker for marker in BAD_MARKERS if marker in screen]
    return (
        screen,
        process.returncode if process.returncode is not None else 124,
        rendered,
        clicked,
        bool(label),
        bad,
        bool(sent_keyboard and sent_app_click and sent_wheel),
    )


def discover_buttons(
    root: pathlib.Path,
    timeout: float,
    rows: int,
    cols: int,
    labels: set[str],
) -> dict[str, tuple[int, int]]:
    found: dict[str, tuple[int, int]] = {}
    xs = (16, 56, 100, 140)
    for y in range(4, rows):
        for x in xs:
            if len(found) == len(labels):
                return found
            screen, _, rendered, clicked, _, bad, _ = run_click(
                root, x, y, timeout, rows, cols, False, False
            )
            if not rendered or not clicked or bad:
                continue
            match = LABEL_PATTERN.search(screen)
            if match:
                label = bytes(match.group(1), "utf-8").decode("unicode_escape")
                if label in labels and label not in found:
                    found[label] = (x, y)
                    print(f"discovered {label}: {x},{y}")
    return found


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--timeout-per-click", type=float, default=8)
    parser.add_argument("--rows", type=int, default=280)
    parser.add_argument("--cols", type=int, default=220)
    parser.add_argument("--artifacts", default="artifacts/chooser-examples")
    parser.add_argument("--examples", nargs="*", help="Optional example names")
    parser.add_argument("--discover", action="store_true", help="Probe chooser coordinates instead of using the checked-in map")
    args = parser.parse_args()

    root = pathlib.Path(__file__).resolve().parents[1]
    artifacts = root / args.artifacts
    artifacts.mkdir(parents=True, exist_ok=True)
    wanted_examples = set(args.examples or BUTTON_LABELS.values())
    wanted_labels = {
        label: example
        for label, example in BUTTON_LABELS.items()
        if example in wanted_examples
    }

    coordinates = (
        discover_buttons(root, args.timeout_per_click, args.rows, args.cols, set(wanted_labels))
        if args.discover
        else {label: tuple(coords) for label, coords in BUTTON_COORDS.items()}
    )
    missing = sorted(set(wanted_labels) - set(coordinates))
    results = []
    for label, example in wanted_labels.items():
        example_dir = artifacts / example
        example_dir.mkdir(parents=True, exist_ok=True)
        if label not in coordinates:
            status = {
                "example": example,
                "label": label,
                "ok": False,
                "missing_coordinate": True,
            }
            (example_dir / "status.json").write_text(json.dumps(status, indent=2), encoding="utf-8")
            results.append(status)
            continue
        x, y = coordinates[label]
        screen, returncode, rendered, clicked, launched, bad, exercised = run_click(
            root, x, y, args.timeout_per_click, args.rows, args.cols, True
        )
        ok = bool(rendered and clicked and launched and exercised and not bad)
        status = {
            "example": example,
            "label": label,
            "ok": ok,
            "exit": returncode,
            "coordinate": [x, y],
            "rendered_chooser": rendered,
            "sent_chooser_wheel": True,
            "clicked_chooser": clicked,
            "launched_from_chooser": launched,
            "declared_keystrokes": [spec for spec, _ in example_keystroke_events(example)],
            "exercised_keyboard_click_and_wheel_after_launch": exercised,
            "bad_markers": bad,
        }
        (example_dir / "screen.txt").write_text(screen, encoding="utf-8")
        (example_dir / "status.json").write_text(json.dumps(status, indent=2), encoding="utf-8")
        results.append(status)

    failures = [item for item in results if not item.get("ok")]
    summary = {
        "examples": len(results),
        "passed": len(results) - len(failures),
        "failed": len(failures),
        "missing_labels": missing,
        "failures": failures,
    }
    (artifacts / "coordinates.json").write_text(
        json.dumps({k: list(v) for k, v in sorted(coordinates.items())}, indent=2),
        encoding="utf-8",
    )
    (artifacts / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    (artifacts / "status.txt").write_text(
        ("pass\n" if not failures and not missing else "fail\n") + json.dumps(summary, indent=2) + "\n",
        encoding="utf-8",
    )
    print(json.dumps(summary, indent=2))
    return 0 if not failures and not missing else 1


if __name__ == "__main__":
    sys.exit(main())
