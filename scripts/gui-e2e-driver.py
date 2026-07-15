#!/usr/bin/env python3
"""External xdotool/ImageMagick driver for the Clawmacs GUI E2E suites."""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path
from typing import Any, Callable

EVENT_MARKER = "[e2e-event]"
HELLO_SENTINEL = "CLAWMACS_E2E_HELLO_SENTINEL"
DEBUG_LOG_ANCHOR_BYTES = 256


class DriverError(RuntimeError):
    pass


class McCLIMGuiSession:
    def __init__(self, *, window_id: str, window_title: str, debug_log: Path,
                 artifact_dir: Path) -> None:
        self.window_id = window_id
        self.window_title = window_title
        self.debug_log = debug_log
        self.artifact_dir = artifact_dir
        self.screenshot_dir = artifact_dir / "screenshots"
        self.screenshot_dir.mkdir(parents=True, exist_ok=True)
        self.action_log = artifact_dir / "actions.jsonl"
        self.steps: list[dict[str, Any]] = []
        self._event_cache: list[dict[str, Any]] = []
        self._debug_log_identity: tuple[int, int] | None = None
        self._debug_log_offset = 0
        self._debug_log_partial = b""
        self._debug_log_anchor = b""
        self._debug_log_mtime_ns: int | None = None

    def log_action(self, action: str, **fields: Any) -> None:
        entry = {
            "time": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            "action": action,
            **fields,
        }
        self.steps.append(entry)
        with self.action_log.open("a", encoding="utf-8") as f:
            f.write(json.dumps(entry, sort_keys=True) + "\n")

    def run(self, argv: list[str], *, check: bool = True) -> subprocess.CompletedProcess[str]:
        self.log_action("run", argv=argv)
        result = subprocess.run(argv, text=True, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, check=False)
        if check and result.returncode != 0:
            raise DriverError(
                f"command failed ({result.returncode}): {' '.join(argv)}\n"
                f"stdout: {result.stdout[-1000:]}\n"
                f"stderr: {result.stderr[-1000:]}"
            )
        return result

    def focus(self) -> None:
        # Xvfb runs without a window manager in the harness, so
        # _NET_ACTIVE_WINDOW activation can legitimately fail. Direct X focus is
        # sufficient for xdotool key/type delivery.
        self.run(["xdotool", "windowactivate", "--sync", self.window_id], check=False)
        self.run(["xdotool", "windowfocus", self.window_id])
        self.log_action("focus", window_id=self.window_id)

    def press(self, key: str) -> None:
        self.run(["xdotool", "key", "--clearmodifiers", key])
        self.log_action("press", key=key)

    def type_text(self, text: str) -> None:
        self.run(["xdotool", "type", "--clearmodifiers", "--delay", "25", text])
        self.log_action("type_text", text=text)

    def artifact_relative_path(self, path: Path) -> str:
        raw = str(path)
        workspace_prefix = "/workspace/"
        if raw.startswith(workspace_prefix):
            return raw[len(workspace_prefix):]
        try:
            return str(path.relative_to(Path.cwd()))
        except ValueError:
            return raw

    def screenshot(self, name: str, *, root: bool = False) -> dict[str, Any]:
        png_path = self.screenshot_dir / f"{name}.png"
        screenshot_format = "png"
        target = "root" if root else self.window_id
        if shutil.which("import"):
            argv = ["import", "-window", target, str(png_path)]
            output_path = png_path
        elif shutil.which("magick"):
            argv = ["magick", "import", "-window", target, str(png_path)]
            output_path = png_path
        elif shutil.which("xwd"):
            xwd_path = self.screenshot_dir / f"{name}.xwd"
            if root:
                argv = ["xwd", "-silent", "-root", "-out", str(xwd_path)]
            else:
                argv = ["xwd", "-silent", "-id", self.window_id,
                        "-out", str(xwd_path)]
            output_path = xwd_path
            screenshot_format = "xwd"
        else:
            raise DriverError("no screenshot command available")
        self.run(argv)
        snapshot = self.latest_event("ui-snapshot") or {}
        record = {
            "name": name,
            "path": str(output_path),
            "relative_path": self.artifact_relative_path(output_path),
            "format": screenshot_format,
            "snapshot_sequence": snapshot.get("sequence"),
            "status": snapshot.get("status"),
            "reason": snapshot.get("reason"),
            "target": target,
        }
        self.log_action("screenshot", **record)
        return record

    def x_request_reply_barrier(self) -> None:
        """Wait for the X server to answer a request before image capture.

        The application emits ``redisplay-handled`` after its CLIM redisplay
        work, but X drawing requests may still be pending when the driver sees
        that log event.  ``getwindowgeometry`` requires a server reply, so it
        provides a deterministic synchronization point without reaching into
        McCLIM's repaint or output-record machinery.
        """
        self.run(["xdotool", "getwindowgeometry", "--shell",
                  self.window_id])
        self.log_action("x_request_reply_barrier", window_id=self.window_id)

    def final_state_screenshot(self, name: str, *,
                               final_snapshot: dict[str, Any],
                               root: bool = False,
                               timeout: float = 10.0) -> dict[str, Any]:
        """Capture final state only after a newer redisplay and X barrier."""
        snapshot_sequence = event_sequence(final_snapshot,
                                           "final screenshot snapshot")
        redisplay_already_handled = (
            final_snapshot.get("reason") == "redisplay-handled")
        redisplay_sequence: int | None = None
        if redisplay_already_handled:
            self.log_action(
                "final_snapshot_after_redisplay",
                snapshot_sequence=snapshot_sequence)
        else:
            expected_buffer = final_snapshot.get("buffer_name")
            redisplay = self.wait_event_after(
                "redisplay-handled", snapshot_sequence,
                lambda event: (
                    (expected_buffer is None
                     or event.get("buffer_name") == expected_buffer)
                    and not event.get("repeat")
                ),
                timeout=timeout)
            redisplay_sequence = event_sequence(
                redisplay, "final screenshot redisplay")
        self.x_request_reply_barrier()
        record = self.screenshot(name, root=root)
        record["final_snapshot_sequence"] = snapshot_sequence
        record["redisplay_already_handled"] = redisplay_already_handled
        record["redisplay_sequence"] = redisplay_sequence
        return record

    def window_geometry(self) -> dict[str, int]:
        """Return xdotool's current geometry for the main application window."""
        result = self.run(["xdotool", "getwindowgeometry", "--shell",
                           self.window_id])
        geometry: dict[str, int] = {}
        for line in result.stdout.splitlines():
            key, separator, raw_value = line.partition("=")
            if separator and key in {"X", "Y", "WIDTH", "HEIGHT", "SCREEN"}:
                try:
                    geometry[key.lower()] = int(raw_value)
                except ValueError as exc:
                    raise DriverError(
                        f"invalid window geometry value {line!r}"
                    ) from exc
        if "width" not in geometry or "height" not in geometry:
            raise DriverError(f"incomplete window geometry: {result.stdout!r}")
        self.log_action("window_geometry", **geometry)
        return geometry

    def pointer_click(self, x: int, y: int) -> None:
        """Click a main-window-relative coordinate from the external driver."""
        # Do not use xdotool's --sync here: repeated menu stress intentionally
        # returns to the same coordinate, and --sync waits for a motion event
        # that cannot occur when the pointer is already there.
        self.run(["xdotool", "mousemove", "--window",
                  self.window_id, str(x), str(y)])
        self.run(["xdotool", "click", "--clearmodifiers", "1"])
        self.log_action("pointer_click", window_id=self.window_id, x=x, y=y)

    def pointer_drag(self, start_x: int, start_y: int,
                     end_x: int, end_y: int) -> None:
        """Send one ordered primary-button press, drag, and release gesture."""
        # Keep the whole gesture on one X connection.  Separate xdotool
        # clients, or a screenshot while the button is held, can interleave
        # with McCLIM's tracking-pointer loop and are not a user-like gesture.
        self.run([
            "xdotool",
            "mousemove", "--window", self.window_id,
            str(start_x), str(start_y),
            "mousedown", "1",
            "sleep", "0.35",
            "mousemove", "--window", self.window_id,
            str(end_x), str(end_y),
            "sleep", "0.15",
            "mouseup", "1",
        ])
        self.log_action("pointer_drag", window_id=self.window_id,
                        start_x=start_x, start_y=start_y,
                        end_x=end_x, end_y=end_y)

    def resize(self, width: int, height: int) -> None:
        """Resize the application window and verify the X server applied it."""
        self.run(["xdotool", "windowsize", "--sync", self.window_id,
                  str(width), str(height)])
        deadline = time.monotonic() + 5.0
        last_geometry: dict[str, int] = {}
        while time.monotonic() < deadline:
            last_geometry = self.window_geometry()
            if (abs(last_geometry["width"] - width) <= 2
                    and abs(last_geometry["height"] - height) <= 2):
                self.log_action("resize", requested_width=width,
                                requested_height=height,
                                actual_width=last_geometry["width"],
                                actual_height=last_geometry["height"])
                return
            time.sleep(0.1)
        raise DriverError(
            f"window did not resize to {width}x{height}; "
            f"last geometry: {last_geometry}"
        )

    def unmap_map(self) -> None:
        """Force a real unmap/map cycle, then restore focus to the frame."""
        self.run(["xdotool", "windowunmap", "--sync", self.window_id])
        time.sleep(0.05)
        self.run(["xdotool", "windowmap", "--sync", self.window_id])
        self.run(["xdotool", "windowraise", self.window_id])
        self.focus()
        self.log_action("unmap_map", window_id=self.window_id)

    def _reset_event_reader(self,
                            identity: tuple[int, int] | None = None) -> None:
        """Forget events and byte state after a debug-log lifecycle change."""
        self._event_cache.clear()
        self._debug_log_identity = identity
        self._debug_log_offset = 0
        self._debug_log_partial = b""
        self._debug_log_anchor = b""
        self._debug_log_mtime_ns = None

    def _consume_debug_log_lines(self, data: bytes) -> None:
        """Decode newly completed event lines from DATA exactly once."""
        buffered = self._debug_log_partial + data
        lines = buffered.split(b"\n")
        self._debug_log_partial = lines.pop()
        for raw_line in lines:
            line = raw_line.decode("utf-8", errors="replace")
            marker = line.find(EVENT_MARKER)
            if marker < 0:
                continue
            payload = line[marker + len(EVENT_MARKER):].strip()
            try:
                decoded = json.loads(payload)
            except json.JSONDecodeError:
                continue
            if isinstance(decoded, dict):
                self._event_cache.append(decoded)

    def _events(self) -> list[dict[str, Any]]:
        """Return cached events after consuming only newly appended log bytes."""
        try:
            stream = self.debug_log.open("rb")
        except FileNotFoundError:
            if self._debug_log_identity is not None:
                self._reset_event_reader()
            return list(self._event_cache)

        with stream:
            stat = os.fstat(stream.fileno())
            identity = (stat.st_dev, stat.st_ino)
            mtime_ns = getattr(stat, "st_mtime_ns",
                               int(stat.st_mtime * 1_000_000_000))
            if self._debug_log_identity is None:
                self._debug_log_identity = identity
            elif identity != self._debug_log_identity:
                self._reset_event_reader(identity)
            elif stat.st_size < self._debug_log_offset:
                self._reset_event_reader(identity)
            elif (self._debug_log_anchor
                  and (stat.st_size != self._debug_log_offset
                       or mtime_ns != self._debug_log_mtime_ns)):
                anchor_offset = (self._debug_log_offset
                                 - len(self._debug_log_anchor))
                stream.seek(anchor_offset)
                observed_anchor = stream.read(len(self._debug_log_anchor))
                if observed_anchor != self._debug_log_anchor:
                    # A same-inode truncate-and-rewrite can grow beyond the old
                    # offset before the next poll.  Validate a short consumed
                    # suffix so that case resets instead of reading mid-line.
                    self._reset_event_reader(identity)

            stream.seek(self._debug_log_offset)
            data = stream.read()
            self._debug_log_offset += len(data)
            if data:
                combined_anchor = self._debug_log_anchor + data
                self._debug_log_anchor = combined_anchor[-DEBUG_LOG_ANCHOR_BYTES:]
                self._consume_debug_log_lines(data)
            final_stat = os.fstat(stream.fileno())
            self._debug_log_mtime_ns = getattr(
                final_stat, "st_mtime_ns",
                int(final_stat.st_mtime * 1_000_000_000))

        return list(self._event_cache)

    def latest_event(self, event_name: str) -> dict[str, Any] | None:
        for event in reversed(self._events()):
            if event.get("event") == event_name:
                return event
        return None

    def wait_event(self, event_name: str,
                   predicate: Callable[[dict[str, Any]], bool] | None = None,
                   *, timeout: float = 10.0) -> dict[str, Any]:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            event = self.latest_event(event_name)
            if event is not None and (predicate is None or predicate(event)):
                self.log_action("wait_event", event=event_name, sequence=event.get("sequence"))
                return event
            time.sleep(0.1)
        raise DriverError(f"timed out waiting for debug event {event_name}")

    def latest_sequence(self) -> int:
        sequence = 0
        for event in self._events():
            raw_sequence = event.get("sequence")
            if isinstance(raw_sequence, int):
                sequence = max(sequence, raw_sequence)
        return sequence

    def wait_event_after(self, event_name: str, after_sequence: int,
                         predicate: Callable[[dict[str, Any]], bool] | None = None,
                         *, timeout: float = 10.0) -> dict[str, Any]:
        deadline = time.monotonic() + timeout
        last_event: dict[str, Any] | None = None
        while time.monotonic() < deadline:
            for event in reversed(self._events()):
                if event.get("event") != event_name:
                    continue
                raw_sequence = event.get("sequence")
                if not isinstance(raw_sequence, int) or raw_sequence <= after_sequence:
                    continue
                last_event = event
                if predicate is None or predicate(event):
                    self.log_action("wait_event_after", event=event_name,
                                    sequence=event.get("sequence"),
                                    after_sequence=after_sequence)
                    return event
            time.sleep(0.1)
        raise DriverError(
            f"timed out waiting for debug event {event_name} after {after_sequence}; "
            f"last event: {json.dumps(last_event, sort_keys=True) if last_event else 'none'}"
        )

    def latest_snapshot(self) -> dict[str, Any]:
        snapshot = self.latest_event("ui-snapshot")
        if snapshot is None:
            raise DriverError("no ui-snapshot event found")
        return snapshot

    def text(self) -> str:
        snapshot = self.latest_snapshot()
        value = (snapshot.get("screen_text") or snapshot.get("screenText") or
                 snapshot.get("screen-text") or "")
        return str(value)

    def wait_text_contains(self, needle: str, *, timeout: float = 15.0) -> str:
        deadline = time.monotonic() + timeout
        last_text = ""
        while time.monotonic() < deadline:
            try:
                last_text = self.text()
            except DriverError:
                last_text = ""
            if needle in last_text:
                self.log_action("wait_text_contains", needle=needle)
                return last_text
            time.sleep(0.1)
        raise DriverError(f"timed out waiting for text {needle!r}; last text:\n{last_text}")

    def wait_snapshot(self, description: str,
                      predicate: Callable[[dict[str, Any]], bool],
                      *, timeout: float = 15.0) -> dict[str, Any]:
        deadline = time.monotonic() + timeout
        last_snapshot: dict[str, Any] | None = None
        while time.monotonic() < deadline:
            last_snapshot = self.latest_event("ui-snapshot")
            if last_snapshot is not None and predicate(last_snapshot):
                self.log_action("wait_snapshot", description=description,
                                sequence=last_snapshot.get("sequence"),
                                status=last_snapshot.get("status"))
                return last_snapshot
            time.sleep(0.1)
        raise DriverError(
            f"timed out waiting for snapshot {description}; "
            f"last snapshot: {json.dumps(last_snapshot, sort_keys=True) if last_snapshot else 'none'}"
        )

    def debug_tail(self, lines: int = 120) -> str:
        if not self.debug_log.exists():
            return ""
        data = self.debug_log.read_text(encoding="utf-8", errors="replace").splitlines()
        return "\n".join(data[-lines:])


def prepare_session(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Wait for the frame, focus it, and capture the initial state."""
    screenshots: list[dict[str, Any]] = []
    session.wait_event("frame-ready", timeout=20.0)
    session.wait_event("ui-snapshot", timeout=10.0)
    session.focus()
    screenshots.append(session.screenshot("01-initial"))
    return screenshots


def event_sequence(event: dict[str, Any], description: str) -> int:
    """Return EVENT's sequence or reject an unusable observability event."""
    sequence = event.get("sequence")
    if not isinstance(sequence, int):
        raise DriverError(f"{description} has no integer sequence: {event!r}")
    return sequence


def run_smoke(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    screenshots = prepare_session(session)

    session.type_text("hello")
    session.wait_snapshot("compose contains hello",
                          lambda snapshot: snapshot.get("compose_text") == "hello",
                          timeout=10.0)
    screenshots.append(session.screenshot("02-typed-hello"))

    session.press("Return")
    session.wait_text_contains(HELLO_SENTINEL, timeout=20.0)
    session.wait_event("e2e-provider-complete",
                       lambda event: event.get("sentinel") == HELLO_SENTINEL,
                       timeout=20.0)
    final_snapshot = session.wait_snapshot(
        "final idle response rendered",
        lambda snapshot: snapshot.get("status") == "idle"
        and HELLO_SENTINEL in str(snapshot.get("screen_text", "")),
        timeout=20.0)
    screenshots.append(session.final_state_screenshot(
        "03-agent-response",
        final_snapshot=final_snapshot,
        timeout=20.0))
    return screenshots


def positive_environment_integer(name: str, default: int) -> int:
    """Return positive integer environment variable NAME or DEFAULT."""
    raw_value = os.environ.get(name)
    if raw_value is None or not raw_value.strip():
        return default
    try:
        value = int(raw_value)
    except ValueError as exc:
        raise DriverError(f"{name} must be a positive integer") from exc
    if value <= 0:
        raise DriverError(f"{name} must be a positive integer")
    return value


def stability_effort_menu_coordinates(
        session: McCLIMGuiSession) -> tuple[int, int, int]:
    """Return robust Effort-label and selector-item coordinates.

    McCLIM lays the menu bar from the upper-left using the fixed font supplied by
    the Guix E2E environment. Derive the vertical positions from the actual
    window and keep the pointer safely inside the Effort label and first row.
    Coordinates deliberately live here, outside application code.
    """
    geometry = session.window_geometry()
    width = geometry["width"]
    height = geometry["height"]
    menu_x = min(width - 80, max(300, round(width * 0.35)))
    menu_y = min(18, max(12, round(height * 0.027)))
    selector_item_y = menu_y + 33
    session.log_action("stability_menu_coordinates", menu_x=menu_x,
                       menu_y=menu_y, selector_item_y=selector_item_y,
                       width=width, height=height)
    return menu_x, menu_y, selector_item_y


def open_stability_effort_menu(session: McCLIMGuiSession,
                               menu_x: int, menu_y: int) -> None:
    """Open the real frame-local Effort menu with a pointer click."""
    session.pointer_click(menu_x, menu_y)
    # The click is delivered asynchronously to McCLIM.  Leave enough time for
    # the menu sheet to map before a second click selects an item; under CPU
    # contention a short delay can otherwise land on the application pane
    # before the standard CLIM menu is visible.
    time.sleep(0.35)


def assert_compose_probe(session: McCLIMGuiSession, probe: str) -> None:
    """Prove the Drei compose pane accepts and clears PROBE."""
    session.focus()
    existing = str(session.latest_snapshot().get("compose_text", ""))
    if existing:
        session.press("ctrl+a")
        session.press("ctrl+k")
        wait_compose_text(session, "")
    session.type_text(probe)
    wait_compose_text(session, probe)
    session.press("ctrl+a")
    session.press("ctrl+k")
    wait_compose_text(session, "")


def assert_no_stability_failure_signatures(session: McCLIMGuiSession) -> None:
    """Fail on crash/recovery signatures that a responsive snapshot can miss."""
    paths = [session.debug_log, session.artifact_dir / "app.stderr"]
    text = "\n".join(
        path.read_text(encoding="utf-8", errors="replace")
        for path in paths if path.exists()
    ).lower()
    signatures = {
        "debugger invoked": "SBCL entered the debugger",
        " is not grafted": "McCLIM reported an ungrafted sheet",
        "menu-bar-error-recovered": "the historical menu recovery path ran",
        "redisplay-queue-failed": "a redisplay wakeup could not be queued",
        "redisplay-handler-error": "the redisplay handler contained an error",
        "frame-cleanup-error": "frame unwind cleanup contained an error",
        "runtime-stream-cleanup-error": "stream cleanup contained an error",
        "runtime-oauth-cleanup-error": "OAuth cleanup contained an error",
        "runtime-worker-settlement-timeout": "a runtime worker did not settle",
        "runtime-worker-reaper-error": "the runtime worker reaper failed",
        "runtime-tool-worker-start-error": "a runtime tool worker could not start",
        "runtime-tool-queue-cleanup-error": "tool-queue cleanup contained an error",
        "prompt-tool-result-cleanup-error": "prompt tool-result cleanup failed",
        "message-help-thread-start-error": "a message-help worker could not start",
        "message-help-frame-error": "message-help frame handling failed",
        "message-help-frame-construction-error": (
            "a message-help frame could not be constructed"),
        "chat-recurse-launch-error": "a recursive chat frame could not launch",
    }
    found = [description for signature, description in signatures.items()
             if signature in text]
    # SBCL emits these process diagnostics at column zero. Restrict the
    # unhandled-condition check to the runtime's "Unhandled ... in thread"
    # line so compiler notes or source snippets containing the word do not
    # become false positives.
    lowered_lines = [line.lower() for line in text.splitlines()]
    if any(line.startswith("fatal error encountered")
           for line in lowered_lines):
        found.append("SBCL reported a fatal runtime error")
    if any(line.startswith("unhandled ") and " in thread " in line
           for line in lowered_lines):
        found.append("an SBCL thread terminated with an unhandled condition")
    if any(line.startswith("heap exhausted") for line in lowered_lines):
        found.append("SBCL exhausted its dynamic heap")
    if found:
        raise DriverError("stability failure signatures: " + "; ".join(found))
    session.log_action("assert_no_stability_failure_signatures",
                       checked=[str(path) for path in paths])


def request_frame_exit(session: McCLIMGuiSession) -> None:
    """Exit through the ordinary frame command and prove unwind completed."""
    after_sequence = session.latest_sequence()
    press_chord(session, "ctrl+x", "ctrl+c")
    session.wait_event_after("frame-stopped", after_sequence, timeout=20.0)
    session.log_action("request_frame_exit", after_sequence=after_sequence)


def run_stability(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Stress stable menus, semantic selectors, X lifecycle, and redisplay."""
    screenshots = prepare_session(session)
    menu_iterations = positive_environment_integer(
        "CLAWMACS_GUI_E2E_STABILITY_MENU_ITERATIONS", 24)
    expose_iterations = positive_environment_integer(
        "CLAWMACS_GUI_E2E_STABILITY_EXPOSE_ITERATIONS", 6)

    session.wait_snapshot(
        "effort-capable stability fixture selected",
        lambda snapshot: snapshot.get("provider") == "openai-codex"
        and snapshot.get("model") == "gpt-5.3-codex",
        timeout=10.0,
    )
    menu_x, menu_y, selector_item_y = stability_effort_menu_coordinates(session)
    selection_count = 0
    for iteration in range(menu_iterations):
        after_sequence = session.latest_sequence()
        if iteration % 4 == 3:
            # The stable menu dispatches into the frame-owned, presentation-
            # based minibuffer selector. Alternate values so every command has
            # a distinct semantic result for the driver to observe.
            select_low = selection_count % 2 == 0
            selected_level = "low" if select_low else "default"
            expected_message = (
                "[Think level set to low for openai-codex/gpt-5.3-codex]"
                if select_low else
                "[Think level reset to default for openai-codex/gpt-5.3-codex]"
            )
            # The CLX menu protocol tracks a press-drag-release gesture: keep
            # button 1 held while the submenu maps and while motion arms the
            # leaf, just as a user does.  Two independent synthetic clicks can
            # race the tracking loop and fall through to the application pane.
            session.pointer_drag(
                menu_x, menu_y,
                menu_x, selector_item_y)
            wait_minibuffer_text(
                session, "effort selector opened from the menu",
                lambda text: "Select Think Level" in text,
                timeout=10.0)
            session.type_text(selected_level)
            session.wait_snapshot(
                f"effort candidate {selected_level!r} selected",
                lambda snapshot, selected_level=selected_level:
                    selected_candidate_contains(snapshot, selected_level),
                timeout=10.0)
            session.press("Return")
            selection_count += 1
            session.wait_event_after(
                "ui-snapshot", after_sequence,
                lambda snapshot: (
                    expected_message in str(snapshot.get("screen_text", ""))
                    and snapshot.get("status") == "idle"
                ),
                timeout=10.0,
            )
            session.log_action("stability_effort_selected",
                               level=selected_level,
                               selection_count=selection_count)
        else:
            open_stability_effort_menu(session, menu_x, menu_y)
            if iteration == 0:
                screenshots.append(
                    session.screenshot("02-effort-menu-open", root=True))
            session.press("Escape")
            time.sleep(0.15)
        if (iteration + 1) % 8 == 0:
            assert_compose_probe(session, f"menu-probe-{iteration + 1}")
    if selection_count == 0:
        raise DriverError("stability suite did not use the effort menu selector")
    screenshots.append(session.screenshot("03-after-menu-stress"))

    original_geometry = session.window_geometry()
    original_width = original_geometry["width"]
    original_height = original_geometry["height"]
    resize_targets = [
        (min(1120, original_width + 120), min(760, original_height + 100)),
        (max(760, original_width - 120), max(500, original_height - 48)),
        (original_width, original_height),
    ]
    for index, (width, height) in enumerate(resize_targets, start=1):
        session.resize(width, height)
        assert_compose_probe(session, f"resize-probe-{index}")
    screenshots.append(session.screenshot("04-after-resize-stress"))

    for iteration in range(1, expose_iterations + 1):
        session.unmap_map()
        assert_compose_probe(session, f"expose-probe-{iteration}")
    screenshots.append(session.screenshot("05-after-expose-stress"))

    # Restore deterministic no-network routing and finish with a streamed
    # response. This proves menu/expose stress did not strand the redisplay
    # coalescer or the compose command path.
    run_mx_selection(session, "minibuffer-select-model-command")
    wait_minibuffer_text(session, "stability model selector opened",
                         lambda text: "Select Model" in text)
    session.type_text("e2e")
    session.wait_snapshot(
        "stability e2e model candidate selected",
        lambda snapshot: selected_candidate_contains(snapshot, "e2e/e2e-model"),
        timeout=10.0,
    )
    session.press("Return")
    session.wait_snapshot(
        "stability e2e model selected",
        lambda snapshot: snapshot.get("provider") == "e2e"
        and snapshot.get("model") == "e2e-model",
        timeout=10.0,
    )
    session.type_text("hello")
    wait_compose_text(session, "hello")
    session.press("Return")
    session.wait_event("e2e-provider-complete",
                       lambda event: event.get("sentinel") == HELLO_SENTINEL,
                       timeout=20.0)
    final_snapshot = session.wait_snapshot(
        "stability final idle response rendered",
        lambda snapshot: snapshot.get("status") == "idle"
        and HELLO_SENTINEL in str(snapshot.get("screen_text", "")),
        timeout=20.0,
    )
    screenshots.append(session.final_state_screenshot(
        "06-stability-complete",
        final_snapshot=final_snapshot,
        timeout=20.0))
    assert_no_stability_failure_signatures(session)
    return screenshots


def open_mx(session: McCLIMGuiSession) -> None:
    """Open the M-x fuzzy command selector."""
    # Use the Emacs-compatible ESC prefix because it is more robust across
    # Xvfb/window-manager modifier handling than synthesizing Alt+x.
    session.press("Escape")
    session.press("x")
    session.wait_snapshot("M-x minibuffer opened",
                          lambda snapshot: "M-x" in str(snapshot.get("minibuffer_text", "")),
                          timeout=10.0)


def selected_candidate_contains(snapshot: dict[str, Any], text: str) -> bool:
    minibuffer = str(snapshot.get("minibuffer_text", ""))
    return any(line.lstrip().startswith(">") and text in line
               for line in minibuffer.splitlines())


def run_mx_selection(session: McCLIMGuiSession, query: str,
                     *, selected: str | None = None,
                     timeout: float = 10.0) -> None:
    """Open M-x, type QUERY, wait for SELECTED, and press Return."""
    open_mx(session)
    session.type_text(query)
    expected = selected or query
    session.wait_snapshot(f"M-x candidate {expected!r} selected",
                          lambda snapshot: selected_candidate_contains(snapshot, expected),
                          timeout=timeout)
    session.press("Return")


def wait_minibuffer_text(session: McCLIMGuiSession, description: str,
                         predicate: Callable[[str], bool],
                         *, timeout: float = 10.0) -> dict[str, Any]:
    return session.wait_snapshot(description,
                                 lambda snapshot: predicate(
                                     str(snapshot.get("minibuffer_text", ""))),
                                 timeout=timeout)


def wait_compose_text(session: McCLIMGuiSession, expected: str,
                      *, timeout: float = 10.0) -> dict[str, Any]:
    return session.wait_snapshot(f"compose text is {expected!r}",
                                 lambda snapshot: snapshot.get("compose_text") == expected,
                                 timeout=timeout)


def press_chord(session: McCLIMGuiSession, *keys: str) -> None:
    """Press KEYS in order, allowing McCLIM time to retain prefix state."""
    for key in keys:
        session.press(key)
        time.sleep(0.08)


def expect_key_command(session: McCLIMGuiSession, keys: tuple[str, ...],
                       command: str, *, timeout: float = 10.0) -> dict[str, Any]:
    """Press KEYS and assert the normalized Clawmacs keymap command fired."""
    after_sequence = session.latest_sequence()
    press_chord(session, *keys)
    return session.wait_event_after(
        "key-command", after_sequence,
        lambda event: event.get("command") == command,
        timeout=timeout,
    )


def cancel_modal_input(session: McCLIMGuiSession) -> None:
    """Cancel minibuffer/selector state if one is active."""
    if (session.latest_snapshot().get("minibuffer_active")
            or session.latest_snapshot().get("model_selector_active")
            or session.latest_snapshot().get("think_selector_active")
            or session.latest_snapshot().get("session_tree_selector_active")
            or session.latest_snapshot().get("buffer_selector_active")):
        session.press("ctrl+g")
        session.wait_snapshot("modal input cancelled",
                              lambda snapshot: not snapshot.get("minibuffer_active")
                              and not snapshot.get("model_selector_active")
                              and not snapshot.get("think_selector_active")
                              and not snapshot.get("session_tree_selector_active")
                              and not snapshot.get("buffer_selector_active"),
                              timeout=10.0)


def close_current_buffer_with_key(session: McCLIMGuiSession,
                                  expected_name: str | None = None) -> None:
    """Close the current auxiliary buffer with C-x k during scenario cleanup."""
    initial_name = session.latest_snapshot().get("buffer_name")
    press_chord(session, "ctrl+x", "k")
    if expected_name is not None:
        session.wait_snapshot(f"buffer {expected_name!r} selected after kill",
                              lambda snapshot: snapshot.get("buffer_name") == expected_name,
                              timeout=10.0)
    else:
        session.wait_snapshot("current buffer changed after kill",
                              lambda snapshot, initial_name=initial_name:
                              snapshot.get("buffer_name") != initial_name,
                              timeout=10.0)


def kill_current_buffer_with_key(session: McCLIMGuiSession,
                                 expected_name: str | None = None) -> None:
    """Assert C-x k dispatches through the Clawmacs keymap and kills a buffer."""
    expect_key_command(session, ("ctrl+x", "k"), "kill-buffer-command")
    if expected_name is not None:
        session.wait_snapshot(f"buffer {expected_name!r} selected after kill",
                              lambda snapshot: snapshot.get("buffer_name") == expected_name,
                              timeout=10.0)


def run_mx(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    screenshots = prepare_session(session)

    open_mx(session)
    screenshots.append(session.screenshot("02-mx-open"))

    abbreviation = "tdbg"
    command = "toggle-debug-mode-command"
    session.type_text(abbreviation)
    session.wait_snapshot("M-x fuzzy command candidate listed",
                          lambda snapshot: (
                              f"M-x: {abbreviation}" in str(snapshot.get("minibuffer_text", ""))
                              and selected_candidate_contains(snapshot, command)
                          ),
                          timeout=10.0)
    screenshots.append(session.screenshot("03-mx-command"))

    session.press("Return")
    final_snapshot = session.wait_snapshot(
        "M-x command executed",
        lambda snapshot: "[Debug mode ON" in str(snapshot.get("screen_text", ""))
        and not snapshot.get("minibuffer_text"),
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "04-mx-result",
        final_snapshot=final_snapshot))
    return screenshots


def write_organa_fixture(session: McCLIMGuiSession) -> Path:
    """Create a deterministic organa TODO file under the artifact directory."""
    path = session.artifact_dir / "organa-tasks.org"
    path.write_text(
        """#+TITLE: E2E Project

* NEXT Implement package UI
:PROPERTIES:
:ID: implement-package-ui
:END:
* TODO Test package UI
:PROPERTIES:
:ID: test-package-ui
:ORGANA_DEPENDS: implement-package-ui
:END:
* DONE Document package UI
""",
        encoding="utf-8",
    )
    session.log_action("write_organa_fixture", path=str(path))
    return path


def run_organa(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Exercise Organa's package-owned McCLIM presentation views."""
    screenshots = prepare_session(session)
    org_path = write_organa_fixture(session)

    run_mx_selection(session, "organa-open-todo-file-command")
    wait_minibuffer_text(session, "Organa path prompt opened",
                         lambda text: "Org TODO file path" in text)
    session.type_text(str(org_path))
    session.press("Return")
    session.wait_snapshot("Organa dashboard shown",
                          lambda snapshot: snapshot.get("buffer_name") == "organa:organa-tasks.org"
                          and snapshot.get("major_mode") == "organa"
                          and "Organa dashboard" in str(snapshot.get("screen_text", ""))
                          and "Ready next" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("02-organa-dashboard"))

    run_mx_selection(session, "organa-cycle-view-command")
    session.wait_snapshot("Organa kanban shown",
                          lambda snapshot: "Organa kanban" in str(snapshot.get("screen_text", ""))
                          and "TODO" in str(snapshot.get("screen_text", ""))
                          and "NEXT" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("03-organa-kanban"))

    run_mx_selection(session, "organa-cycle-view-command")
    session.wait_snapshot("Organa dependency view shown",
                          lambda snapshot: "Organa dependency" in str(snapshot.get("screen_text", ""))
                          and "Test package UI" in str(snapshot.get("screen_text", ""))
                          and "Implement package UI" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("04-organa-dependency"))

    run_mx_selection(session, "organa-cycle-view-command")
    final_snapshot = session.wait_snapshot(
        "Organa outline view shown",
        lambda snapshot: "Organa outline" in str(snapshot.get("screen_text", ""))
        and "Implement package UI" in str(snapshot.get("screen_text", ""))
        and "Test package UI" in str(snapshot.get("screen_text", "")),
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "05-organa-outline",
        final_snapshot=final_snapshot))
    return screenshots


def run_quaestor(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Exercise Quaestor's package-owned active request panel."""
    screenshots = prepare_session(session)

    session.wait_snapshot("Quaestor request panel shown",
                          lambda snapshot: "Quaestor request 1/1: Scope" in str(snapshot.get("screen_text", ""))
                          and "[x] Alpha" in str(snapshot.get("screen_text", ""))
                          and "[ ] Beta" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("02-quaestor-panel"))

    session.press("Down")
    session.wait_snapshot("Quaestor option changed",
                          lambda snapshot: "[x] Beta" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    session.press("Tab")
    session.type_text("ship it")
    session.wait_snapshot("Quaestor notes updated",
                          lambda snapshot: "Notes: ship it" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("03-quaestor-answering"))

    session.press("Return")
    final_snapshot = session.wait_snapshot(
        "Quaestor request answered",
        lambda snapshot: "[request_user_input answered]" in str(snapshot.get("screen_text", ""))
        and "Scope: Beta; ship it" in str(snapshot.get("screen_text", ""))
        and "Quaestor request 1/1" not in str(snapshot.get("screen_text", "")),
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "04-quaestor-answered",
        final_snapshot=final_snapshot))
    return screenshots


def run_keybinds(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Exercise default keybindings through the real McCLIM/ESA GUI path."""
    screenshots = prepare_session(session)

    # Drei-owned compose editing keys should edit text instead of being stolen
    # by the frame command table.
    session.type_text("one two")
    wait_compose_text(session, "one two")
    press_chord(session, "ctrl+a", "Escape", "f")
    session.type_text("X")
    wait_compose_text(session, "oneX two")
    press_chord(session, "Escape", "b")
    session.type_text("Y")
    wait_compose_text(session, "YoneX two")
    press_chord(session, "ctrl+e", "ctrl+u")
    wait_compose_text(session, "")
    session.type_text("YoneX two")
    wait_compose_text(session, "YoneX two")
    press_chord(session, "ctrl+a", "ctrl+k")
    wait_compose_text(session, "")
    session.press("ctrl+y")
    wait_compose_text(session, "YoneX two")
    press_chord(session, "ctrl+a", "Escape", "d")
    wait_compose_text(session, " two")
    session.press("ctrl+y")
    wait_compose_text(session, "YoneX two")
    press_chord(session, "ctrl+e", "ctrl+w")
    wait_compose_text(session, "YoneX ")
    session.type_text("two")
    wait_compose_text(session, "YoneX two")
    press_chord(session, "ctrl+a", "ctrl+d")
    wait_compose_text(session, "oneX two")
    session.press("BackSpace")
    wait_compose_text(session, "oneX two")
    press_chord(session, "ctrl+a", "ctrl+k")
    wait_compose_text(session, "")
    screenshots.append(session.screenshot("02-compose-keybinds"))

    # M-x and redraw are global keybindings that should dispatch from compose.
    expect_key_command(session, ("Escape", "x"), "execute-extended-command")
    wait_minibuffer_text(session, "M-x opened by keybinding",
                         lambda text: "M-x" in text)
    cancel_modal_input(session)
    expect_key_command(session, ("ctrl+l",), "redraw-screen-command")
    screenshots.append(session.screenshot("03-global-keybinds"))

    # C-c mode-specific commands.
    initial_snapshot = session.latest_snapshot()
    expect_key_command(session, ("ctrl+c", "t"), "toggle-tool-results-command")
    session.wait_snapshot("tool result visibility toggled",
                          lambda snapshot: snapshot.get("show_tool_results")
                          != initial_snapshot.get("show_tool_results"),
                          timeout=10.0)
    expect_key_command(session, ("ctrl+c", "t"), "toggle-tool-results-command")
    session.wait_snapshot("tool result visibility restored",
                          lambda snapshot: snapshot.get("show_tool_results")
                          == initial_snapshot.get("show_tool_results"),
                          timeout=10.0)

    initial_snapshot = session.latest_snapshot()
    expect_key_command(session, ("ctrl+c", "shift+v"), "toggle-reasoning-output-command")
    session.wait_snapshot("reasoning visibility toggled",
                          lambda snapshot: snapshot.get("show_reasoning")
                          != initial_snapshot.get("show_reasoning"),
                          timeout=10.0)
    expect_key_command(session, ("ctrl+c", "shift+v"), "toggle-reasoning-output-command")
    session.wait_snapshot("reasoning visibility restored",
                          lambda snapshot: snapshot.get("show_reasoning")
                          == initial_snapshot.get("show_reasoning"),
                          timeout=10.0)

    initial_snapshot = session.latest_snapshot()
    expect_key_command(session, ("ctrl+c", "shift+i"), "toggle-metadata-output-command")
    session.wait_snapshot("metadata visibility toggled",
                          lambda snapshot: snapshot.get("show_metadata")
                          != initial_snapshot.get("show_metadata"),
                          timeout=10.0)
    expect_key_command(session, ("ctrl+c", "shift+i"), "toggle-metadata-output-command")
    session.wait_snapshot("metadata visibility restored",
                          lambda snapshot: snapshot.get("show_metadata")
                          == initial_snapshot.get("show_metadata"),
                          timeout=10.0)

    expect_key_command(session, ("ctrl+c", "c"), "compact-buffer-command")
    session.wait_snapshot("compact command feedback shown",
                          lambda snapshot: "Nothing compacted" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)

    expect_key_command(session, ("ctrl+c", "shift+a"), "minibuffer-select-agent-command")
    wait_minibuffer_text(session, "agent selector opened by keybinding",
                         lambda text: "Select Agent" in text)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "s"), "minibuffer-insert-skill-command")
    session.wait_snapshot("insert-skill command handled",
                          lambda snapshot: "Insert Skill" in str(snapshot.get("minibuffer_text", ""))
                          or "No enabled skills" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "shift+s"), "minibuffer-toggle-skill-command")
    session.wait_snapshot("toggle-skill command handled",
                          lambda snapshot: "Toggle Skill" in str(snapshot.get("minibuffer_text", ""))
                          or "No skills" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "ctrl+Return"), "minibuffer-select-model-command")
    wait_minibuffer_text(session, "model selector opened by keybinding",
                         lambda text: "Select Model" in text)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "shift+m"), "select-model-command")
    wait_minibuffer_text(session, "compatibility model selector opened by keybinding",
                         lambda text: "Select Model" in text)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "ctrl+r"), "minibuffer-select-think-level-command")
    session.wait_snapshot("minibuffer think command handled",
                          lambda snapshot: "Think levels" in str(snapshot.get("screen_text", ""))
                          or "Select Think Level" in str(snapshot.get("minibuffer_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "shift+r"), "select-think-level-command")
    session.wait_snapshot("compatibility think command handled",
                          lambda snapshot: "Think levels" in str(snapshot.get("screen_text", ""))
                          or "Select Think Level" in str(snapshot.get("minibuffer_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)
    screenshots.append(session.screenshot("04-ctrl-c-keybinds"))

    # C-h help prefix and C-c compatibility aliases.
    for keys, command, prompt in [
        (("ctrl+h", "f"), "describe-function-command", "Describe Function"),
        (("ctrl+h", "v"), "describe-variable-command", "Describe Variable"),
        (("ctrl+h", "shift+t"), "describe-type-command", "Describe Type"),
        (("ctrl+c", "f"), "describe-function-command", "Describe Function"),
        (("ctrl+c", "v"), "describe-variable-command", "Describe Variable"),
        (("ctrl+c", "shift+t"), "describe-type-command", "Describe Type"),
    ]:
        expect_key_command(session, keys, command)
        wait_minibuffer_text(session, f"{command} prompt opened", lambda text, prompt=prompt: prompt in text)
        cancel_modal_input(session)

    for keys, command in [
        (("ctrl+h", "b"), "describe-bindings-command"),
        (("ctrl+c", "b"), "describe-bindings-command"),
    ]:
        expect_key_command(session, keys, command)
        session.wait_snapshot("keybindings help shown",
                              lambda snapshot: snapshot.get("buffer_name") == "*help:keybindings*"
                              and "Key Bindings" in str(snapshot.get("screen_text", "")),
                              timeout=10.0)
        close_current_buffer_with_key(session, "clawmacs:e2e")

    for keys, command in [
        (("ctrl+h", "i"), "info-directory-command"),
        (("ctrl+h", "shift+i"), "clawmacs-manual-command"),
    ]:
        expect_key_command(session, keys, command)
        session.wait_snapshot("info buffer shown",
                              lambda snapshot: snapshot.get("major_mode") == "info",
                              timeout=10.0)
        close_current_buffer_with_key(session, "clawmacs:e2e")
    screenshots.append(session.screenshot("05-help-keybinds"))

    # C-x global buffer/session/file commands.  C-x b is the regression path:
    # it must open the ESA minibuffer selector rather than crash the frame.
    expect_key_command(session, ("ctrl+x", "n"), "new-buffer-command")
    session.wait_snapshot("new buffer created by C-x n",
                          lambda snapshot: snapshot.get("buffer_name") == "session-1",
                          timeout=10.0)
    expect_key_command(session, ("ctrl+x", "b"), "minibuffer-select-buffer-command")
    wait_minibuffer_text(session, "C-x b buffer selector opened",
                         lambda text: "Switch Buffer" in text)
    session.type_text("clawmacs")
    session.wait_snapshot("original buffer selected in C-x b selector",
                          lambda snapshot: selected_candidate_contains(snapshot, "clawmacs:e2e"),
                          timeout=10.0)
    session.press("Return")
    session.wait_snapshot("C-x b switched back to original buffer",
                          lambda snapshot: snapshot.get("buffer_name") == "clawmacs:e2e",
                          timeout=10.0)

    expect_key_command(session, ("ctrl+x", "ctrl+b"), "minibuffer-select-buffer-command")
    wait_minibuffer_text(session, "C-x C-b buffer selector opened",
                         lambda text: "Switch Buffer" in text)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "p"), "minibuffer-select-project-command")
    session.wait_snapshot("project selector keybinding handled",
                          lambda snapshot: "Select Project" in str(snapshot.get("minibuffer_text", ""))
                          or "No projects" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "ctrl+f"), "open-project-file-command")
    session.wait_snapshot("open-project-file keybinding handled",
                          lambda snapshot: snapshot.get("minibuffer_active")
                          or "No projects" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "l"), "new-listener-buffer-command")
    session.wait_snapshot("listener buffer opened",
                          lambda snapshot: snapshot.get("major_mode") == "listener",
                          timeout=10.0)
    close_current_buffer_with_key(session)

    expect_key_command(session, ("ctrl+x", "shift+f"), "font-editor-command")
    session.wait_snapshot("font editor opened",
                          lambda snapshot: snapshot.get("major_mode") == "font-editor",
                          timeout=10.0)
    close_current_buffer_with_key(session)

    expect_key_command(session, ("ctrl+x", "ctrl+s"), "save-session-command")
    session.wait_snapshot("save-session feedback shown",
                          lambda snapshot: "Session saved to" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)

    expect_key_command(session, ("ctrl+x", "ctrl+r"), "load-session-command")
    session.wait_snapshot("load-session keybinding handled",
                          lambda snapshot: "Load Session" in str(snapshot.get("minibuffer_text", ""))
                          or "No saved sessions" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "t"), "session-tree-command")
    session.wait_snapshot("session-tree keybinding handled",
                          lambda snapshot: snapshot.get("session_tree_selector_active")
                          or "Current session has no tree entries" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "shift+t"), "fork-session-command")
    session.wait_snapshot("fork-session keybinding handled",
                          lambda snapshot: snapshot.get("session_tree_selector_active")
                          or "Current session has no tree entries" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "n"), "new-buffer-command")
    temp_name = str(session.latest_snapshot().get("buffer_name"))
    kill_current_buffer_with_key(session)
    final_snapshot = session.wait_snapshot(
        "temporary buffer killed by C-x k",
        lambda snapshot, temp_name=temp_name:
        snapshot.get("buffer_name") != temp_name,
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "06-ctrl-x-keybinds",
        final_snapshot=final_snapshot))

    return screenshots


def run_features(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Exercise broad no-network GUI feature coverage in one Clawmacs session."""
    screenshots = prepare_session(session)

    # Compose-pane editing: ordinary typing, C-a/C-e movement, C-b movement,
    # and C-k killing all text from the beginning of the line.
    session.type_text("abc")
    wait_compose_text(session, "abc")
    session.press("ctrl+a")
    session.type_text("X")
    wait_compose_text(session, "Xabc")
    session.press("ctrl+e")
    session.type_text("Y")
    wait_compose_text(session, "XabcY")
    session.press("ctrl+b")
    session.type_text("Z")
    wait_compose_text(session, "XabcZY")
    session.press("ctrl+a")
    session.press("ctrl+k")
    wait_compose_text(session, "")
    screenshots.append(session.screenshot("02-compose-editing"))

    # Minibuffer editing while M-x is active, including C-b and ESC-b (M-b).
    open_mx(session)
    session.type_text("foo bar")
    wait_minibuffer_text(session, "M-x text typed",
                         lambda text: "M-x: foo bar" in text)
    session.press("ctrl+b")
    session.press("ctrl+b")
    session.type_text("Z")
    wait_minibuffer_text(session, "C-b moved minibuffer point",
                         lambda text: "M-x: foo bZar" in text)
    session.press("Escape")
    session.press("b")
    session.type_text("X")
    wait_minibuffer_text(session, "M-b moved minibuffer point",
                         lambda text: "M-x: foo XbZar" in text)
    session.press("ctrl+a")
    session.type_text("S")
    session.press("ctrl+e")
    session.type_text("E")
    wait_minibuffer_text(session, "C-a/C-e moved minibuffer point",
                         lambda text: "M-x: Sfoo XbZarE" in text)
    session.press("ctrl+u")
    wait_minibuffer_text(session, "C-u cleared M-x input",
                         lambda text: "M-x: " in text and "No matches" not in text)
    session.type_text("tdbg")
    session.wait_snapshot("M-x fuzzy debug command selected",
                          lambda snapshot: selected_candidate_contains(
                              snapshot, "toggle-debug-mode-command"),
                          timeout=10.0)
    session.press("Return")
    session.wait_snapshot("debug mode toggled by edited M-x",
                          lambda snapshot: "[Debug mode ON" in str(snapshot.get("screen_text", ""))
                          and not snapshot.get("minibuffer_text"),
                          timeout=10.0)
    screenshots.append(session.screenshot("03-minibuffer-editing"))

    # Help/introspection through the fuzzy command selector.
    run_mx_selection(session, "describe-bindings-command")
    session.wait_snapshot("keybindings help buffer shown",
                          lambda snapshot: snapshot.get("buffer_name") == "*help:keybindings*"
                          and "execute-extended-command" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("04-help-bindings"))

    # Buffer management and fuzzy buffer selector.
    run_mx_selection(session, "new-buffer-command")
    session.wait_snapshot("new buffer selected",
                          lambda snapshot: snapshot.get("buffer_name") == "session-1"
                          and snapshot.get("major_mode") == "chat",
                          timeout=10.0)
    run_mx_selection(session, "minibuffer-select-buffer-command")
    wait_minibuffer_text(session, "buffer selector opened",
                         lambda text: "Switch Buffer" in text)
    session.type_text("clawmacs")
    session.wait_snapshot("original e2e buffer candidate selected",
                          lambda snapshot: selected_candidate_contains(snapshot, "clawmacs:e2e"),
                          timeout=10.0)
    session.press("Return")
    session.wait_snapshot("original e2e buffer restored",
                          lambda snapshot: snapshot.get("buffer_name") == "clawmacs:e2e",
                          timeout=10.0)
    screenshots.append(session.screenshot("05-buffer-selector"))

    # Agent/model/think selectors without real provider credentials.
    run_mx_selection(session, "minibuffer-select-agent-command")
    wait_minibuffer_text(session, "agent selector opened",
                         lambda text: "Select Agent" in text)
    session.type_text("agent")
    session.wait_snapshot("agent candidate selected",
                          lambda snapshot: selected_candidate_contains(snapshot, "agent"),
                          timeout=10.0)
    session.press("Return")
    session.wait_snapshot("agent selector closed",
                          lambda snapshot: not snapshot.get("minibuffer_text")
                          and "agent agent" in str(snapshot.get("info_text", "")),
                          timeout=10.0)

    run_mx_selection(session, "minibuffer-select-model-command")
    wait_minibuffer_text(session, "model selector opened",
                         lambda text: "Select Model" in text)
    session.type_text("e2e")
    session.wait_snapshot("e2e model candidate selected",
                          lambda snapshot: selected_candidate_contains(snapshot, "e2e/e2e-model"),
                          timeout=10.0)
    session.press("Return")
    session.wait_snapshot("e2e model selected",
                          lambda snapshot: "[Model changed to e2e/e2e-model" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)

    run_mx_selection(session, "minibuffer-select-think-level-command")
    session.wait_snapshot("think-level unavailable message shown",
                          lambda snapshot: "Think levels not available" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("06-agent-model-think"))

    # Prompted commands and session persistence feedback.
    run_mx_selection(session, "set-session-display-name-command")
    wait_minibuffer_text(session, "display-name prompt opened",
                         lambda text: "Session display name" in text)
    session.type_text("E2E Label")
    session.press("Return")
    session.wait_snapshot("display name set",
                          lambda snapshot: "[Session display name: E2E Label]" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    run_mx_selection(session, "save-session-command")
    expected_session_dir = session.artifact_dir / "home" / ".config" / "clawmacs" / "sessions"
    session.wait_snapshot("session saved under isolated artifact home",
                          lambda snapshot: (
                              "[Session saved to" in str(snapshot.get("screen_text", ""))
                              and str(expected_session_dir)
                              in str(snapshot.get("screen_text", ""))
                          ),
                          timeout=10.0)
    screenshots.append(session.screenshot("07-session-commands"))

    # Offline help/dashboard features that do not require credentials.
    run_mx_selection(session, "list-skills-command")
    session.wait_snapshot("skills help buffer shown",
                          lambda snapshot: snapshot.get("buffer_name") == "*help:skills*"
                          and "Skills" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    run_mx_selection(session, "package-dashboard-command")
    final_snapshot = session.wait_snapshot(
        "package dashboard shown",
        lambda snapshot: snapshot.get("buffer_name") == "*Packages*"
        and snapshot.get("major_mode") == "package-dashboard"
        and "Packages for" in str(snapshot.get("screen_text", "")),
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "08-offline-help-dashboards",
        final_snapshot=final_snapshot))

    return screenshots


def run_reload(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Exercise safe in-place reload through the semantic GUI harness."""
    screenshots = prepare_session(session)

    session.type_text("reload draft remains visible")
    wait_compose_text(session, "reload draft remains visible")
    screenshots.append(session.screenshot("02-before-safe-reload"))

    after_sequence = session.latest_sequence()
    run_mx_selection(session, "safe-reload-clawmacs-command", timeout=20.0)
    session.wait_event_after(
        "ui-snapshot", after_sequence,
        lambda snapshot: (
            snapshot.get("buffer_name") == "clawmacs:e2e"
            and snapshot.get("status") == "reloading"
            and snapshot.get("compose_text") == "reload draft remains visible"
            and " reloading " in str(snapshot.get("info_text", ""))
            and "Clawmacs safe reload started" in str(snapshot.get("screen_text", ""))
        ),
        timeout=15.0,
    )
    screenshots.append(session.screenshot("03-safe-reload-started"))
    session.wait_event_after(
        "safe-reload-result", after_sequence,
        lambda event: event.get("status") == "ok",
        timeout=90.0,
    )
    final_snapshot = session.wait_snapshot(
        "safe reload success notification visible",
        lambda snapshot: (
            snapshot.get("buffer_name") == "clawmacs:e2e"
            and snapshot.get("status") == "idle"
            and snapshot.get("compose_text") == "reload draft remains visible"
            and "Clawmacs safe reload succeeded" in str(snapshot.get("screen_text", ""))
        ),
        timeout=30.0,
    )
    screenshots.append(session.final_state_screenshot(
        "04-after-safe-reload",
        final_snapshot=final_snapshot,
        timeout=30.0))
    return screenshots


def write_summary(path: Path, *, ok: bool, suite: str, artifact_dir: Path,
                  steps: list[dict[str, Any]], screenshots: list[dict[str, Any]],
                  failure: str | None = None,
                  last_snapshot: dict[str, Any] | None = None) -> None:
    payload: dict[str, Any] = {
        "ok": ok,
        "suite": suite,
        "artifact_dir": str(artifact_dir),
        "artifact_dir_relative": str(artifact_dir).replace("/workspace/", "", 1),
        "steps": steps,
        "screenshots": screenshots,
    }
    if failure is not None:
        payload["failure"] = failure
    if last_snapshot is not None:
        payload["last_snapshot"] = last_snapshot
    with path.open("w", encoding="utf-8") as f:
        json.dump(payload, f, indent=2, sort_keys=True)
        f.write("\n")


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--suite", default="smoke")
    parser.add_argument("--artifact-dir", required=True)
    parser.add_argument("--debug-log", required=True)
    parser.add_argument("--window-title", default="Clawmacs E2E")
    parser.add_argument("--window-id", default=None)
    args = parser.parse_args(argv)

    artifact_dir = Path(args.artifact_dir)
    artifact_dir.mkdir(parents=True, exist_ok=True)
    summary_path = artifact_dir / "summary.json"
    screenshots: list[dict[str, Any]] = []

    window_id = args.window_id
    if not window_id:
        result = subprocess.run(["xdotool", "search", "--name", args.window_title],
                                text=True, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, check=False)
        if result.returncode != 0 or not result.stdout.strip():
            raise DriverError(f"could not find window titled {args.window_title!r}")
        window_id = result.stdout.split()[0]

    session = McCLIMGuiSession(window_id=window_id,
                               window_title=args.window_title,
                               debug_log=Path(args.debug_log),
                               artifact_dir=artifact_dir)
    try:
        if args.suite == "smoke":
            screenshots = run_smoke(session)
        elif args.suite == "mx":
            screenshots = run_mx(session)
        elif args.suite == "features":
            screenshots = run_features(session)
        elif args.suite == "keybinds":
            screenshots = run_keybinds(session)
        elif args.suite == "organa":
            screenshots = run_organa(session)
        elif args.suite == "quaestor":
            screenshots = run_quaestor(session)
        elif args.suite == "reload":
            screenshots = run_reload(session)
        elif args.suite == "stability":
            screenshots = run_stability(session)
        else:
            raise DriverError(f"unsupported suite: {args.suite}")
        # Every successful scenario finishes through the same public CLIM
        # command.  FRAME-STOPPED is emitted after RUN-FRAME-TOP-LEVEL unwinds,
        # so the shell harness can then require a natural zero-status process
        # exit instead of relying on its emergency EXIT trap.
        request_frame_exit(session)
        write_summary(summary_path, ok=True, suite=args.suite,
                      artifact_dir=artifact_dir, steps=session.steps,
                      screenshots=screenshots,
                      last_snapshot=session.latest_snapshot())
        return 0
    except Exception as exc:  # noqa: BLE001 - script must preserve artifacts on any failure.
        try:
            screenshots.append(session.screenshot("failure"))
        except Exception:
            pass
        (artifact_dir / "debug.tail").write_text(session.debug_tail(), encoding="utf-8")
        last_snapshot = session.latest_event("ui-snapshot")
        write_summary(summary_path, ok=False, suite=args.suite,
                      artifact_dir=artifact_dir, steps=session.steps,
                      screenshots=screenshots, failure=str(exc),
                      last_snapshot=last_snapshot)
        print(f"GUI E2E failure: {exc}", file=sys.stderr)
        if last_snapshot:
            print(json.dumps(last_snapshot, indent=2, sort_keys=True), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
