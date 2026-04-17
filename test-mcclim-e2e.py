#!/usr/bin/env python3
"""Offline McCLIM E2E tests with screenshots and semantic snapshots.

The harness runs Clawmacs inside an existing X display, drives it with
xdotool, captures screenshots with ImageMagick, and reads structured state
from scripts/mcclim-e2e-driver.lisp.
"""
import argparse
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time


CLAWMACS_DIR = os.path.dirname(os.path.abspath(__file__))
SCREENSHOT_DIR = os.path.join(CLAWMACS_DIR, "screenshots", "mcclim")
SESSION_NAME = "mcclim-e2e-session"

PASSED = []
FAILED = []


def fail(message):
    raise AssertionError(message)


def ensure_tool(name):
    if shutil.which(name) is None:
        fail(f"missing required command: {name}")


def wait_until(predicate, timeout=20.0, interval=0.1, description="condition"):
    deadline = time.time() + timeout
    last_error = None
    while time.time() < deadline:
        try:
            value = predicate()
            if value:
                return value
        except Exception as exc:
            last_error = exc
        time.sleep(interval)
    if last_error is not None:
        fail(f"timed out waiting for {description}: {last_error}")
    fail(f"timed out waiting for {description}")


def run_checked(args, timeout=10, **kwargs):
    result = subprocess.run(
        args,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        timeout=timeout,
        **kwargs,
    )
    if result.returncode != 0:
        fail(
            f"command failed: {' '.join(args)}\n"
            f"stdout:\n{result.stdout}\n"
            f"stderr:\n{result.stderr}"
        )
    return result.stdout.strip()


class McclimSession:
    def __init__(self):
        self.proc = None
        self.window_id = None
        self.artifact_root = tempfile.mkdtemp(
            prefix="mcclim-e2e-", dir=os.path.join(CLAWMACS_DIR, ".cache")
        )
        self.control_dir = os.path.join(self.artifact_root, "control")
        self.log_path = os.path.join(self.artifact_root, "clawmacs.log")
        os.makedirs(self.control_dir, exist_ok=True)
        os.makedirs(SCREENSHOT_DIR, exist_ok=True)

    def launch(self):
        if "DISPLAY" not in os.environ:
            fail("DISPLAY is not set; run via scripts/mcclim-e2e.sh")
        if "CLAWMACS_QUICKLISP_SETUP" not in os.environ:
            fail("CLAWMACS_QUICKLISP_SETUP is not set; run via guix-container.sh")

        env = os.environ.copy()
        env["CLAWMACS_MCCLIM_E2E_CONTROL_DIR"] = self.control_dir
        env["CLAWMACS_DEBUG_LOG"] = os.path.join(self.artifact_root, "debug.log")
        args = [
            "sbcl",
            "--noinform",
            "--load",
            env["CLAWMACS_QUICKLISP_SETUP"],
            "--eval",
            '(push (truename ".") asdf:*central-registry*)',
            "--eval",
            "(ql:quickload :clawmacs/mcclim :silent t)",
            "--load",
            "scripts/mcclim-e2e-driver.lisp",
            "--eval",
            "(setf clawmacs:*inhibit-user-init* t)",
            "--eval",
            "(setf clawmacs:*ui-backend* (make-instance 'clawmacs:mcclim-backend))",
            "--eval",
            "(clawmacs/mcclim-e2e:start-control-thread)",
            "--eval",
            f'(clawmacs:clawmacs-main :session-name "{SESSION_NAME}")',
        ]
        log = open(self.log_path, "w", encoding="utf-8")
        self.proc = subprocess.Popen(
            args,
            cwd=CLAWMACS_DIR,
            env=env,
            stdout=log,
            stderr=subprocess.STDOUT,
            text=True,
        )
        self.window_id = self.wait_for_window()
        wait_until(
            lambda: self.snapshot().get("ready"),
            timeout=30,
            description="semantic snapshot",
        )
        return self

    def wait_for_window(self):
        def find_window():
            if self.proc.poll() is not None:
                fail(f"clawmacs exited early; see {self.log_path}")
            result = subprocess.run(
                ["xdotool", "search", "--name", "^Clawmacs$"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip().splitlines()[-1]
            return None

        window_id = wait_until(find_window, timeout=30, description="Clawmacs window")
        self.focus()
        return window_id

    def focus(self):
        if self.window_id:
            for args in (
                ["xdotool", "windowfocus", "--sync", self.window_id],
                ["xdotool", "mousemove", "--window", self.window_id, "20", "20"],
                ["xdotool", "click", "1"],
            ):
                try:
                    subprocess.run(
                        args,
                        stdout=subprocess.DEVNULL,
                        stderr=subprocess.DEVNULL,
                        text=True,
                        timeout=3,
                    )
                except subprocess.TimeoutExpired:
                    pass

    def key(self, *keys):
        self.focus()
        run_checked(["xdotool", "key", *keys], timeout=5)
        time.sleep(0.1)

    def type_text(self, text):
        self.focus()
        run_checked(
            ["xdotool", "type", "--delay", "1", text],
            timeout=10,
        )
        time.sleep(0.1)

    def snapshot(self):
        path = os.path.join(self.control_dir, "latest.json")
        deadline = time.time() + 5
        last_error = None
        while time.time() < deadline:
            try:
                with open(path, "r", encoding="utf-8") as stream:
                    return json.load(stream)
            except (FileNotFoundError, json.JSONDecodeError) as exc:
                last_error = exc
                time.sleep(0.05)
        if last_error is not None:
            raise last_error
        fail(f"snapshot not available: {path}")

    def wait_snapshot(self, predicate, timeout=10, description="snapshot predicate"):
        return wait_until(
            lambda: self._snapshot_if(predicate),
            timeout=timeout,
            description=description,
        )

    def _snapshot_if(self, predicate):
        snapshot = self.snapshot()
        if predicate(snapshot):
            return snapshot
        return None

    def save_artifacts(self, name):
        png_path = os.path.join(SCREENSHOT_DIR, f"{name}.png")
        json_path = os.path.join(SCREENSHOT_DIR, f"{name}.json")
        result = subprocess.run(
            ["import", "-window", "root", png_path],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        if result.returncode != 0:
            result = subprocess.run(
                ["import", "-window", self.window_id, png_path],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )
            if result.returncode != 0:
                fail(f"screenshot failed: {result.stderr}")
        with open(json_path, "w", encoding="utf-8") as stream:
            json.dump(self.snapshot(), stream, indent=2, sort_keys=True)
            stream.write("\n")
        return png_path, json_path

    def close(self):
        if self.proc and self.proc.poll() is None:
            try:
                self.key("ctrl+x", "ctrl+c")
                self.proc.wait(timeout=5)
            except Exception:
                self.proc.terminate()
                try:
                    self.proc.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    self.proc.kill()
                    self.proc.wait(timeout=5)


def test_initial_state(session):
    snapshot = session.snapshot()
    buffer = snapshot.get("buffer") or {}
    if buffer.get("name") != SESSION_NAME:
        fail(f"unexpected buffer name: {buffer.get('name')!r}")
    if buffer.get("status") != "idle":
        fail(f"unexpected buffer status: {buffer.get('status')!r}")
    session.save_artifacts("initial")


def test_text_input(session):
    session.type_text("Hello McCLIM E2E")
    snapshot = session.wait_snapshot(
        lambda s: (s.get("buffer") or {}).get("input") == "Hello McCLIM E2E",
        description="typed text in input",
    )
    if snapshot["buffer"]["messageCount"] != 0:
        fail("typing should not finalize a message")
    session.save_artifacts("text-input")


def test_line_editing(session):
    session.key("ctrl+a")
    session.type_text(">>> ")
    session.wait_snapshot(
        lambda s: (s.get("buffer") or {}).get("input", "").startswith(">>> "),
        description="beginning-of-line edit",
    )
    session.save_artifacts("line-editing")


def test_shell_prefix(session):
    session.key("ctrl+a")
    session.key("ctrl+k")
    session.type_text("!echo mcclim-e2e-shell")
    session.key("Return")

    def has_shell_result(snapshot):
        messages = (snapshot.get("buffer") or {}).get("messages") or []
        return any("mcclim-e2e-shell" in message.get("text", "") for message in messages)

    session.wait_snapshot(has_shell_result, timeout=15, description="shell result")
    session.save_artifacts("shell-prefix")


def test_buffer_selector(session):
    session.key("ctrl+x", "b")
    snapshot = session.wait_snapshot(
        lambda s: (s.get("selectors") or {}).get("bufferSelectorActive") is True,
        timeout=10,
        description="buffer selector overlay",
    )
    if snapshot.get("buffer", {}).get("name") != SESSION_NAME:
        fail("buffer selector opened on an unexpected session")
    session.save_artifacts("buffer-selector")
    session.key("ctrl+g")


def test_execute_extended_command_minibuffer(session):
    session.key("alt+x")
    snapshot = session.wait_snapshot(
        lambda s: (s.get("minibuffer") or {}).get("active") is True,
        timeout=10,
        description="M-x command minibuffer",
    )
    prompt = snapshot.get("minibuffer", {}).get("prompt", "")
    if prompt.lower() not in ("m-x", "command"):
        fail(f"unexpected M-x prompt: {prompt!r}")
    session.save_artifacts("execute-command-minibuffer")
    session.key("ctrl+g")


TESTS = {
    "smoke": [test_initial_state, test_text_input],
    "offline": [
        test_initial_state,
        test_text_input,
        test_line_editing,
        test_shell_prefix,
        test_buffer_selector,
        test_execute_extended_command_minibuffer,
    ],
}


def run_test(name, func, session):
    try:
        func(session)
        PASSED.append(name)
        print(f"[PASS] {name}")
    except Exception as exc:
        FAILED.append((name, str(exc)))
        print(f"[FAIL] {name}: {exc}", file=sys.stderr)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--only",
        choices=["smoke", "offline", "all"],
        default="offline",
        help="test group to run",
    )
    return parser.parse_args()


def main():
    args = parse_args()
    ensure_tool("xdotool")
    ensure_tool("import")
    selected = TESTS["offline"] if args.only == "all" else TESTS[args.only]

    session = McclimSession()
    try:
        session.launch()
        for func in selected:
            run_test(func.__name__, func, session)
    finally:
        print(f"Artifacts: {SCREENSHOT_DIR}")
        print(f"Control/logs: {session.artifact_root}")
        session.close()

    if FAILED:
        print("\nFailures:", file=sys.stderr)
        for name, message in FAILED:
            print(f"- {name}: {message}", file=sys.stderr)
        return 1

    print(f"\nPassed {len(PASSED)} McCLIM E2E test(s).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
