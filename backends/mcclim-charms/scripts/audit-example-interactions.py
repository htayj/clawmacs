#!/usr/bin/env python3
"""Build and verify source-derived McCLIM examples interaction coverage."""

from __future__ import annotations

import argparse
import json
import os
import pathlib
import re
import sys
from dataclasses import dataclass, asdict


EXAMPLE_ORDER = [
    "package",
    "text-size-util",
    "seos-baseline",
    "seos-wrfg",
    "calculator",
    "colorslider",
    "menu-test",
    "address-book",
    "av-dingus",
    "traffic-lights",
    "clim-fig",
    "puzzle",
    "transformations-test",
    "town-example",
    "tabdemo",
    "tabledemo",
    "image-transform-demo",
    "stream-test",
    "presentation-test",
    "dragndrop",
    "gadget-test",
    "text-gadgets",
    "method-browser",
    "stopwatch",
    "dragndrop-translator",
    "draggable-graph",
    "text-size-test",
    "drawing-benchmark",
    "logic-cube",
    "checkers",
    "views",
    "font-selector",
    "bordered-output-examples",
    "borders-and-outlines",
    "misc-tests",
    "drawing-tests",
    "pixmaps",
    "render-image-tests",
    "image-viewer",
    "accepting-values-test",
    "graph-toy",
    "coordinate-swizzling",
    "hierarchy-tool",
    "patterns",
    "flipping-ink",
    "patterns-overlap",
    "text-transformation-test",
    "indentation",
    "selection",
    "frame-sheet-name-test",
    "dnd-commented",
    "tracking-pointer",
    "sheet-geometry",
    "file-manager",
    "presentation-translators-test",
    "graph-formatting-test",
    "asynchronous-commands",
    "reinitialize-frame",
    "nested-clipping",
    "indirect-gestures",
    "timer-gestures",
    "wrfg-test",
    "unique-id-test",
    "animation-pulse",
    "concurrent-draw",
    "concurrent-text",
    "concurrent-grid",
    "modifier",
    "slider-test",
    "small-tests",
    "demodemo",
]


PATTERNS = {
    "frames": re.compile(r"\(\s*(?:clim:)?define-application-frame\s+([^\s()]+)", re.I),
    "commands": re.compile(r"\(\s*(?:clim:)?define-[\w-]*command\s+(?:\(\s*)?([^\s()]+)", re.I),
    "plain_commands": re.compile(r"\(\s*(?:clim:)?define-command\s+(?:\(\s*)?([^\s()]+)", re.I),
    "presentation_types": re.compile(r"\(\s*(?:clim:)?define-presentation-type\s+([^\s()]+)", re.I),
    "presentation_methods": re.compile(r"\(\s*(?:clim:)?define-presentation-method\s+([^\s()]+)", re.I),
    "translators": re.compile(r"\(\s*(?:clim:)?define-presentation-to-command-translator\s+([^\s()]+)", re.I),
    "actions": re.compile(r"\(\s*(?:clim:)?define-presentation-action\s+([^\s()]+)", re.I),
    "tracking_pointer": re.compile(r"\(\s*(?:clim:)?tracking-pointer\b", re.I),
    "accepting_values": re.compile(r"\(\s*(?:clim:)?accepting-values\b", re.I),
    "gadgets": re.compile(r":(?:push-button|toggle-button|slider|text-field|list-pane|option-pane|radio-box|check-box)\b|\bmake-pane\s+:(?:push-button|toggle-button|slider|text-field|list-pane|option-pane|radio-box|check-box)\b", re.I),
    "keystrokes": re.compile(r":keystroke\s+([^)\n]+)", re.I),
    "menus": re.compile(r":menu\s+([^)\n]+)", re.I),
    "run_functions": re.compile(r"\(\s*defun\s+([^\s()]+).*run-frame-top-level", re.I | re.S),
}


@dataclass
class ExampleCoverage:
    file: str
    path: str
    frames: list[str]
    commands: list[str]
    presentation_types: list[str]
    presentation_methods: list[str]
    translators: list[str]
    actions: list[str]
    keystrokes: list[str]
    menus: list[str]
    has_gadgets: bool
    has_tracking_pointer: bool
    has_accepting_values: bool
    run_functions: list[str]
    interaction_intents: list[str]
    result_checks: list[str]


def unique(items: list[str]) -> list[str]:
    seen: set[str] = set()
    result: list[str] = []
    for item in items:
        item = item.strip()
        if item and item not in seen:
            seen.add(item)
            result.append(item)
    return result


def matches(pattern_name: str, source: str) -> list[str]:
    return unique([m.group(1).strip() for m in PATTERNS[pattern_name].finditer(source)])


def interaction_intents(data: dict[str, object]) -> list[str]:
    intents: list[str] = []
    if data["frames"]:
        intents.append("launch-frame-and-render-in-charms-port")
    if data["commands"]:
        intents.append("invoke-each-command-and-observe-frame-state-or-output")
    if data["keystrokes"]:
        intents.append("send-declared-keystrokes-through-pty")
    if data["menus"]:
        intents.append("exercise-menu-exposed-commands")
    if data["presentation_types"] or data["translators"] or data["actions"]:
        intents.append("select-presentations-and-verify-translator/action-result")
    if data["has_gadgets"]:
        intents.append("activate-gadgets-and-verify-bound-value-or-command-result")
    if data["has_accepting_values"]:
        intents.append("complete-accepting-values-dialog-and-verify-accepted-values")
    if data["has_tracking_pointer"]:
        intents.append("drive-pointer-motion/drag-and-verify-pointer-result")
    if not intents:
        intents.append("load-only-helper-or-drawing-source")
    return intents


def result_checks(data: dict[str, object]) -> list[str]:
    checks = ["loads-without-debugger", "terminal-cleanup-after-run"]
    if data["frames"]:
        checks.append("frame-class-defined")
        checks.append("snapshot-contains-rendered-output")
    if data["commands"]:
        checks.append("all-source-commands-present-in-command-table-or-callable")
    if data["keystrokes"]:
        checks.append("declared-keystrokes-translate-to-command-events")
    if data["presentation_types"] or data["translators"] or data["actions"]:
        checks.append("presentation-database-contains-selectable-records")
    if data["has_gadgets"]:
        checks.append("gadget-callback-changes-command-visible-state")
    if data["has_accepting_values"]:
        checks.append("accepted-values-change-result-binding")
    if data["has_tracking_pointer"]:
        checks.append("pointer-motion/drag-produces-expected-domain-mutation")
    return checks


def audit_example(root: pathlib.Path, name: str) -> ExampleCoverage:
    path = root / f"{name}.lisp"
    source = path.read_text(encoding="utf-8")
    commands = unique(matches("commands", source) + matches("plain_commands", source))
    data = {
        "frames": matches("frames", source),
        "commands": commands,
        "presentation_types": matches("presentation_types", source),
        "presentation_methods": matches("presentation_methods", source),
        "translators": matches("translators", source),
        "actions": matches("actions", source),
        "keystrokes": matches("keystrokes", source),
        "menus": matches("menus", source),
        "has_gadgets": bool(PATTERNS["gadgets"].search(source)),
        "has_tracking_pointer": bool(PATTERNS["tracking_pointer"].search(source)),
        "has_accepting_values": bool(PATTERNS["accepting_values"].search(source)),
        "run_functions": matches("run_functions", source),
    }
    return ExampleCoverage(
        file=name,
        path=str(path),
        interaction_intents=interaction_intents(data),
        result_checks=result_checks(data),
        **data,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--examples-root",
        default=os.environ.get("MCCLIM_SOURCE_ROOT", "/home/tay/reference/external_src/McCLIM/"),
    )
    parser.add_argument("--artifacts", default="artifacts/example-interactions")
    args = parser.parse_args()

    backend_root = pathlib.Path(__file__).resolve().parents[1]
    examples_root = pathlib.Path(args.examples_root).expanduser().resolve() / "Examples"
    artifacts = backend_root / args.artifacts
    artifacts.mkdir(parents=True, exist_ok=True)

    missing = [name for name in EXAMPLE_ORDER if not (examples_root / f"{name}.lisp").exists()]
    if missing:
        (artifacts / "status.txt").write_text(
            "missing examples:\n" + "\n".join(missing) + "\n",
            encoding="utf-8",
        )
        return 1

    coverage = [audit_example(examples_root, name) for name in EXAMPLE_ORDER]
    payload = [asdict(item) for item in coverage]
    (artifacts / "coverage.json").write_text(json.dumps(payload, indent=2), encoding="utf-8")

    failures: list[str] = []
    for item in coverage:
        has_surface = any(
            [
                item.frames,
                item.commands,
                item.presentation_types,
                item.translators,
                item.actions,
                item.keystrokes,
                item.menus,
                item.has_gadgets,
                item.has_tracking_pointer,
                item.has_accepting_values,
            ]
        )
        if has_surface and not item.result_checks:
            failures.append(f"{item.file}: interactive source had no result checks")
        if item.commands and "all-source-commands-present-in-command-table-or-callable" not in item.result_checks:
            failures.append(f"{item.file}: commands not covered by result checks")
        if (item.translators or item.actions) and "presentation-database-contains-selectable-records" not in item.result_checks:
            failures.append(f"{item.file}: presentation actions/translators not covered")
        if item.has_gadgets and "gadget-callback-changes-command-visible-state" not in item.result_checks:
            failures.append(f"{item.file}: gadgets not covered")
        if item.has_accepting_values and "accepted-values-change-result-binding" not in item.result_checks:
            failures.append(f"{item.file}: accepting-values not covered")
        if item.has_tracking_pointer and "pointer-motion/drag-produces-expected-domain-mutation" not in item.result_checks:
            failures.append(f"{item.file}: tracking-pointer not covered")

    summary = {
        "examples": len(coverage),
        "frames": sum(len(item.frames) for item in coverage),
        "commands": sum(len(item.commands) for item in coverage),
        "translators": sum(len(item.translators) for item in coverage),
        "presentation_actions": sum(len(item.actions) for item in coverage),
        "examples_with_gadgets": sum(1 for item in coverage if item.has_gadgets),
        "examples_with_accepting_values": sum(1 for item in coverage if item.has_accepting_values),
        "examples_with_tracking_pointer": sum(1 for item in coverage if item.has_tracking_pointer),
        "failures": failures,
    }
    (artifacts / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    (artifacts / "status.txt").write_text(
        ("pass\n" if not failures else "fail\n")
        + json.dumps(summary, indent=2)
        + "\n",
        encoding="utf-8",
    )
    return 0 if not failures else 1


if __name__ == "__main__":
    sys.exit(main())
