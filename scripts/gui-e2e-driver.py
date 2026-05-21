#!/usr/bin/env python3
"""External xdotool/ImageMagick driver for the Clawmacs GUI E2E smoke suite."""

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

    def screenshot(self, name: str) -> dict[str, Any]:
        png_path = self.screenshot_dir / f"{name}.png"
        screenshot_format = "png"
        if shutil.which("import"):
            argv = ["import", "-window", self.window_id, str(png_path)]
            output_path = png_path
        elif shutil.which("magick"):
            argv = ["magick", "import", "-window", self.window_id, str(png_path)]
            output_path = png_path
        elif shutil.which("xwd"):
            xwd_path = self.screenshot_dir / f"{name}.xwd"
            argv = ["xwd", "-silent", "-id", self.window_id, "-out", str(xwd_path)]
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
        }
        self.log_action("screenshot", **record)
        return record

    def _events(self) -> list[dict[str, Any]]:
        if not self.debug_log.exists():
            return []
        events: list[dict[str, Any]] = []
        with self.debug_log.open("r", encoding="utf-8", errors="replace") as f:
            for line in f:
                marker = line.find(EVENT_MARKER)
                if marker < 0:
                    continue
                payload = line[marker + len(EVENT_MARKER):].strip()
                try:
                    decoded = json.loads(payload)
                except json.JSONDecodeError:
                    continue
                if isinstance(decoded, dict):
                    events.append(decoded)
        return events

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
    session.wait_snapshot("final idle response rendered",
                          lambda snapshot: snapshot.get("status") == "idle"
                          and HELLO_SENTINEL in str(snapshot.get("screen_text", "")),
                          timeout=20.0)
    screenshots.append(session.screenshot("03-agent-response"))
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
    session.wait_snapshot("M-x command executed",
                          lambda snapshot: "[Debug mode ON" in str(snapshot.get("screen_text", ""))
                          and not snapshot.get("minibuffer_text"),
                          timeout=10.0)
    screenshots.append(session.screenshot("04-mx-result"))
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
    session.wait_snapshot("Organa outline view shown",
                          lambda snapshot: "Organa outline" in str(snapshot.get("screen_text", ""))
                          and "Implement package UI" in str(snapshot.get("screen_text", ""))
                          and "Test package UI" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("05-organa-outline"))
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
    session.wait_snapshot("Quaestor request answered",
                          lambda snapshot: "[request_user_input answered]" in str(snapshot.get("screen_text", ""))
                          and "Scope: Beta; ship it" in str(snapshot.get("screen_text", ""))
                          and "Quaestor request 1/1" not in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("04-quaestor-answered"))
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

    initial_snapshot = session.latest_snapshot()
    expect_key_command(session, ("ctrl+c", "shift+v"), "toggle-reasoning-output-command")
    session.wait_snapshot("reasoning visibility toggled",
                          lambda snapshot: snapshot.get("show_reasoning")
                          != initial_snapshot.get("show_reasoning"),
                          timeout=10.0)
    expect_key_command(session, ("ctrl+c", "shift+v"), "toggle-reasoning-output-command")

    initial_snapshot = session.latest_snapshot()
    expect_key_command(session, ("ctrl+c", "shift+i"), "toggle-metadata-output-command")
    session.wait_snapshot("metadata visibility toggled",
                          lambda snapshot: snapshot.get("show_metadata")
                          != initial_snapshot.get("show_metadata"),
                          timeout=10.0)
    expect_key_command(session, ("ctrl+c", "shift+i"), "toggle-metadata-output-command")

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
    session.wait_snapshot("overlay model selector opened by keybinding",
                          lambda snapshot: snapshot.get("model_selector_active"),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "ctrl+r"), "minibuffer-select-think-level-command")
    session.wait_snapshot("minibuffer think command handled",
                          lambda snapshot: "Think levels" in str(snapshot.get("screen_text", ""))
                          or "Select Think Level" in str(snapshot.get("minibuffer_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "shift+r"), "select-think-level-command")
    session.wait_snapshot("overlay think command handled",
                          lambda snapshot: "Think levels" in str(snapshot.get("screen_text", ""))
                          or snapshot.get("think_selector_active"),
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
    session.wait_snapshot("temporary buffer killed by C-x k",
                          lambda snapshot, temp_name=temp_name: snapshot.get("buffer_name") != temp_name,
                          timeout=10.0)
    screenshots.append(session.screenshot("06-ctrl-x-keybinds"))

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
    session.wait_snapshot("session saved under isolated artifact home",
                          lambda snapshot: (
                              "[Session saved to" in str(snapshot.get("screen_text", ""))
                              and ".artifacts/gui-e2e" in str(snapshot.get("screen_text", ""))
                              and "/home/" in str(snapshot.get("screen_text", ""))
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
    session.wait_snapshot("package dashboard shown",
                          lambda snapshot: snapshot.get("buffer_name") == "*Packages*"
                          and snapshot.get("major_mode") == "package-dashboard"
                          and "Packages for" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    run_mx_selection(session, "describe-guard-policy-command")
    session.wait_snapshot("guard policy help buffer shown",
                          lambda snapshot: snapshot.get("buffer_name") == "*help:guard-policy*"
                          and "Guard Policy" in str(snapshot.get("screen_text", ""))
                          and "Working directory" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("08-offline-help-dashboards"))

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
    session.wait_snapshot(
        "safe reload success notification visible",
        lambda snapshot: (
            snapshot.get("buffer_name") == "clawmacs:e2e"
            and snapshot.get("status") == "idle"
            and snapshot.get("compose_text") == "reload draft remains visible"
            and "Clawmacs safe reload succeeded" in str(snapshot.get("screen_text", ""))
        ),
        timeout=30.0,
    )
    screenshots.append(session.screenshot("04-after-safe-reload"))
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
        else:
            raise DriverError(f"unsupported suite: {args.suite}")
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
