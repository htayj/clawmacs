#!/usr/bin/env python3
"""Deterministic unit tests for GUI E2E screenshot synchronization."""

from __future__ import annotations

import importlib.util
import sys
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
                            "reason": "redisplay-handled"})

        self.assertEqual(
            [("log", "final_snapshot_after_redisplay",
              {"snapshot_sequence": 29}),
             ("barrier",),
             ("screenshot", "final", False)],
            calls,
        )
        self.assertTrue(record["redisplay_already_handled"])
        self.assertIsNone(record["redisplay_sequence"])

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
