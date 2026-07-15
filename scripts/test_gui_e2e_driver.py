#!/usr/bin/env python3
"""Dependency-free unit tests for the external GUI E2E driver."""

from __future__ import annotations

import importlib.util
import json
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


DRIVER_PATH = Path(__file__).with_name("gui-e2e-driver.py")
SPEC = importlib.util.spec_from_file_location("clawmacs_gui_e2e_driver",
                                              DRIVER_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"cannot load GUI E2E driver from {DRIVER_PATH}")
DRIVER = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = DRIVER
SPEC.loader.exec_module(DRIVER)


def event_line(event: dict[str, object], *, newline: bool = True) -> bytes:
    """Return one debug-log event line."""
    suffix = "\n" if newline else ""
    return (f"debug-prefix {DRIVER.EVENT_MARKER}"
            f"{json.dumps(event, sort_keys=True)}{suffix}").encode("utf-8")


class EventLogReaderTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary_directory.cleanup)
        root = Path(self.temporary_directory.name)
        self.debug_log = root / "debug.log"
        self.session = DRIVER.McCLIMGuiSession(
            window_id="1",
            window_title="test",
            debug_log=self.debug_log,
            artifact_dir=root / "artifacts",
        )

    def append(self, data: bytes) -> None:
        with self.debug_log.open("ab") as stream:
            stream.write(data)

    def test_reads_new_append_bytes_without_duplicating_cached_events(self) -> None:
        self.assertEqual([], self.session._events())

        self.append(event_line({"event": "ready", "sequence": 1}))
        self.assertEqual([1], [event["sequence"]
                              for event in self.session._events()])
        self.assertEqual([1], [event["sequence"]
                              for event in self.session._events()])

        self.append(event_line({"event": "ready", "sequence": 2}))
        self.assertEqual([1, 2], [event["sequence"]
                                 for event in self.session._events()])
        self.assertEqual(self.debug_log.stat().st_size,
                         self.session._debug_log_offset)

    def test_buffers_partial_lines_until_their_terminating_newline(self) -> None:
        self.append(
            f"{DRIVER.EVENT_MARKER}{{\"event\":\"partial\",\"sequence\":".encode(
                "utf-8"))
        self.assertEqual([], self.session._events())

        self.append(b"7}")
        self.assertEqual([], self.session._events())

        self.append(b"\n")
        self.assertEqual([{"event": "partial", "sequence": 7}],
                         self.session._events())
        self.assertEqual([{"event": "partial", "sequence": 7}],
                         self.session._events())

    def test_skips_malformed_lines_and_non_object_json(self) -> None:
        self.append(
            b"ordinary debug output\n"
            + f"{DRIVER.EVENT_MARKER}{{not-json}}\n".encode("utf-8")
            + f"{DRIVER.EVENT_MARKER}[1, 2, 3]\n".encode("utf-8")
            + event_line({"event": "valid", "sequence": 8})
        )
        self.assertEqual([{"event": "valid", "sequence": 8}],
                         self.session._events())

    def test_same_inode_truncate_and_regrow_resets_cached_events(self) -> None:
        old_line = event_line({"event": "old", "sequence": 10,
                               "padding": "x" * 400})
        self.debug_log.write_bytes(old_line)
        self.assertEqual(["old"], [event["event"]
                                   for event in self.session._events()])

        new_line = event_line({"event": "new", "sequence": 1,
                               "padding": "y" * 800})
        self.debug_log.write_bytes(new_line)
        self.assertGreater(len(new_line), len(old_line))
        self.assertEqual(["new"], [event["event"]
                                   for event in self.session._events()])

    def test_inode_replacement_resets_cached_events(self) -> None:
        self.debug_log.write_bytes(
            event_line({"event": "old", "sequence": 10}))
        self.assertEqual(["old"], [event["event"]
                                   for event in self.session._events()])

        replacement = self.debug_log.with_suffix(".replacement")
        replacement.write_bytes(
            event_line({"event": "replacement", "sequence": 1}))
        os.replace(replacement, self.debug_log)
        self.assertEqual(["replacement"], [event["event"]
                                           for event in self.session._events()])

    def test_latest_helpers_and_wait_after_use_incremental_cache(self) -> None:
        self.append(event_line({"event": "target", "sequence": 2}))
        self.append(event_line({"event": "other", "sequence": 5}))
        self.assertEqual(2,
                         self.session.latest_event("target")["sequence"])
        self.assertEqual(5, self.session.latest_sequence())

        appended = False

        def append_after_first_poll(_seconds: float) -> None:
            nonlocal appended
            if not appended:
                appended = True
                self.append(event_line({"event": "target", "sequence": 6,
                                        "accepted": True}))

        with mock.patch.object(DRIVER.time, "sleep",
                               side_effect=append_after_first_poll):
            event = self.session.wait_event_after(
                "target", 2,
                lambda candidate: candidate.get("accepted") is True,
                timeout=1.0)
        self.assertEqual(6, event["sequence"])
        self.assertEqual(6, self.session.latest_sequence())

    def test_polls_read_log_bytes_once_plus_small_append_anchor(self) -> None:
        initial = b"".join(
            event_line({"event": "sample", "sequence": sequence,
                        "text": "x" * 80})
            for sequence in range(100)
        )
        self.debug_log.write_bytes(initial)
        bytes_read = 0
        real_open = Path.open

        class CountingReader:
            def __init__(self, stream: object) -> None:
                self.stream = stream

            def __enter__(self) -> "CountingReader":
                self.stream.__enter__()
                return self

            def __exit__(self, *args: object) -> object:
                return self.stream.__exit__(*args)

            def __getattr__(self, name: str) -> object:
                return getattr(self.stream, name)

            def read(self, *args: object, **kwargs: object) -> bytes:
                nonlocal bytes_read
                data = self.stream.read(*args, **kwargs)
                bytes_read += len(data)
                return data

        def counting_open(path: Path, *args: object,
                          **kwargs: object) -> object:
            stream = real_open(path, *args, **kwargs)
            mode = str(args[0] if args else kwargs.get("mode", "r"))
            if path == self.debug_log and mode == "rb":
                return CountingReader(stream)
            return stream

        with mock.patch.object(Path, "open", new=counting_open):
            for _ in range(25):
                self.session._events()
            appended = event_line({"event": "sample", "sequence": 100})
            with real_open(self.debug_log, "ab") as stream:
                stream.write(appended)
            for _ in range(25):
                self.session._events()

        expected = (self.debug_log.stat().st_size
                    + min(len(initial), DRIVER.DEBUG_LOG_ANCHOR_BYTES))
        self.assertEqual(expected, bytes_read)
        self.assertEqual(101, len(self.session._events()))


class FinalStateScreenshotTests(unittest.TestCase):
    def test_waits_for_newer_redisplay_then_barrier_then_capture(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            def wait_event_after(self, event_name: str, after_sequence: int,
                                 predicate: object,
                                 *, timeout: float) -> dict[str, object]:
                accepts = predicate  # Keep the assertions readable below.
                wrong_buffer = {"buffer_name": "other", "repeat": None}
                repeated = {"buffer_name": "chat", "repeat": True}
                completed = {"buffer_name": "chat", "repeat": None}
                calls.append(("wait", event_name, after_sequence, timeout,
                              accepts(wrong_buffer), accepts(repeated),
                              accepts(completed)))
                return {"event": event_name, "sequence": 43,
                        **completed}

            def x_request_reply_barrier(self) -> None:
                calls.append(("barrier",))

            def screenshot(self, name: str, *, root: bool) -> dict[str, object]:
                calls.append(("screenshot", name, root))
                return {"name": name}

        record = DRIVER.McCLIMGuiSession.final_state_screenshot(
            FakeSession(), "final",
            final_snapshot={"sequence": 17, "reason": "pane-rendered",
                            "buffer_name": "chat"},
            root=True,
            timeout=3.5)

        self.assertEqual(
            [("wait", "redisplay-handled", 17, 3.5,
              False, False, True),
             ("barrier",),
             ("screenshot", "final", True)],
            calls,
        )
        self.assertEqual(17, record["final_snapshot_sequence"])
        self.assertFalse(record["redisplay_already_handled"])
        self.assertEqual(43, record["redisplay_sequence"])

    def test_already_redisplayed_snapshot_skips_later_event_wait(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            def wait_event_after(self, *args: object,
                                 **kwargs: object) -> dict[str, object]:
                raise AssertionError("must not wait for another redisplay")

            def log_action(self, action: str, **fields: object) -> None:
                calls.append(("log", action, fields))

            def x_request_reply_barrier(self) -> None:
                calls.append(("barrier",))

            def screenshot(self, name: str, *, root: bool) -> dict[str, object]:
                calls.append(("screenshot", name, root))
                return {"name": name}

        record = DRIVER.McCLIMGuiSession.final_state_screenshot(
            FakeSession(), "final",
            final_snapshot={"sequence": 29,
                            "reason": "redisplay-handled",
                            "repeat": None})

        self.assertEqual(
            [("log", "final_snapshot_after_redisplay",
              {"snapshot_sequence": 29}),
             ("barrier",),
             ("screenshot", "final", False)],
            calls,
        )
        self.assertTrue(record["redisplay_already_handled"])
        self.assertIsNone(record["redisplay_sequence"])

    def test_repeating_redisplay_snapshot_waits_for_quiescent_cycle(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            def wait_event_after(self, event_name: str, after_sequence: int,
                                 predicate: object,
                                 *, timeout: float) -> dict[str, object]:
                repeated = {"buffer_name": "chat", "repeat": True}
                completed = {"buffer_name": "chat", "repeat": None}
                calls.append(("wait", event_name, after_sequence, timeout,
                              predicate(repeated), predicate(completed)))
                return {"event": event_name, "sequence": 37, **completed}

            def x_request_reply_barrier(self) -> None:
                calls.append(("barrier",))

            def screenshot(self, name: str, *, root: bool) -> dict[str, object]:
                calls.append(("screenshot", name, root))
                return {"name": name}

        record = DRIVER.McCLIMGuiSession.final_state_screenshot(
            FakeSession(), "final",
            final_snapshot={"sequence": 29,
                            "reason": "redisplay-handled",
                            "repeat": True,
                            "buffer_name": "chat"})

        self.assertEqual(
            [("wait", "redisplay-handled", 29, 10.0, False, True),
             ("barrier",),
             ("screenshot", "final", False)],
            calls,
        )
        self.assertFalse(record["redisplay_already_handled"])
        self.assertEqual(37, record["redisplay_sequence"])

    def test_legacy_redisplay_snapshot_without_repeat_waits(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            def wait_event_after(self, event_name: str, after_sequence: int,
                                 predicate: object,
                                 *, timeout: float) -> dict[str, object]:
                completed = {"buffer_name": "chat", "repeat": None}
                calls.append(("wait", event_name, after_sequence, timeout,
                              predicate(completed)))
                return {"event": event_name, "sequence": 43, **completed}

            def x_request_reply_barrier(self) -> None:
                calls.append(("barrier",))

            def screenshot(self, name: str, *, root: bool) -> dict[str, object]:
                calls.append(("screenshot", name, root))
                return {"name": name}

        record = DRIVER.McCLIMGuiSession.final_state_screenshot(
            FakeSession(), "final",
            final_snapshot={"sequence": 31,
                            "reason": "redisplay-handled",
                            "buffer_name": "chat"})

        self.assertEqual(
            [("wait", "redisplay-handled", 31, 10.0, True),
             ("barrier",),
             ("screenshot", "final", False)],
            calls,
        )
        self.assertFalse(record["redisplay_already_handled"])
        self.assertEqual(43, record["redisplay_sequence"])

    def test_x_barrier_requires_an_x_server_reply(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            window_id = "4242"

            def run(self, argv: list[str]) -> None:
                calls.append(("run", tuple(argv)))

            def log_action(self, action: str, **fields: object) -> None:
                calls.append(("log", action, fields))

        DRIVER.McCLIMGuiSession.x_request_reply_barrier(FakeSession())

        self.assertEqual(
            ("run", ("xdotool", "getwindowgeometry", "--shell", "4242")),
            calls[0],
        )
        self.assertEqual(
            ("log", "x_request_reply_barrier", {"window_id": "4242"}),
            calls[1],
        )

    def test_mx_final_capture_uses_accepted_final_snapshot_sequence(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            def screenshot(self, name: str) -> dict[str, object]:
                calls.append(("screenshot", name))
                return {"name": name}

            def type_text(self, text: str) -> None:
                calls.append(("type", text))

            def wait_snapshot(self, description: str, predicate: object,
                              *, timeout: float) -> dict[str, object]:
                calls.append(("wait_snapshot", description, timeout))
                if description == "M-x command executed":
                    return {"event": "ui-snapshot", "sequence": 97,
                            "reason": "pane-rendered"}
                return {"event": "ui-snapshot", "sequence": 41,
                        "reason": "pane-rendered"}

            def press(self, key: str) -> None:
                calls.append(("press", key))

            def final_state_screenshot(self, name: str, *,
                                       final_snapshot: dict[str, object]
                                       ) -> dict[str, object]:
                calls.append(("final", name,
                              final_snapshot.get("sequence")))
                return {"name": name}

        session = FakeSession()
        with mock.patch.object(DRIVER, "prepare_session", return_value=[]), \
             mock.patch.object(DRIVER, "open_mx", return_value=None):
            screenshots = DRIVER.run_mx(session)

        final_wait = ("wait_snapshot", "M-x command executed", 10.0)
        final_capture = ("final", "04-mx-result", 97)
        self.assertLess(calls.index(("press", "Return")),
                        calls.index(final_wait))
        self.assertLess(calls.index(final_wait), calls.index(final_capture))
        self.assertIn(final_capture, calls)
        self.assertEqual("04-mx-result", screenshots[-1]["name"])


if __name__ == "__main__":
    unittest.main()
