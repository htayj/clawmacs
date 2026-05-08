#!/usr/bin/env python3
"""Report source feature coverage against the current PTY e2e artifacts."""

from __future__ import annotations

import argparse
import json
import pathlib
import sys

from example_features import encodable_source_keystrokes


def load_json(path: pathlib.Path):
    if not path.exists():
        return None
    return json.loads(path.read_text(encoding="utf-8"))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--artifacts", default="artifacts/e2e-feature-coverage")
    parser.add_argument("--strict", action="store_true")
    args = parser.parse_args()

    root = pathlib.Path(__file__).resolve().parents[1]
    out = root / args.artifacts
    out.mkdir(parents=True, exist_ok=True)
    coverage = load_json(root / "artifacts/example-interactions/coverage.json") or []
    direct_summary = load_json(root / "artifacts/example-launches/summary.json") or {}
    chooser_summary = load_json(root / "artifacts/chooser-examples/summary.json") or {}
    command_summary = load_json(root / "artifacts/command-table-features/summary.json") or {}
    surface_summary = load_json(root / "artifacts/surface-features/summary.json") or {}
    command_results = {
        result.get("example"): result
        for result in (load_json(root / "artifacts/command-table-features/results.json") or [])
    }
    surface_results = {
        result.get("example"): result
        for result in (load_json(root / "artifacts/surface-features/results.json") or [])
    }

    direct_status = {
        p.parent.name: load_json(p)
        for p in (root / "artifacts/example-launches").glob("*/status.json")
    }
    chooser_status = {
        p.parent.name: load_json(p)
        for p in (root / "artifacts/chooser-examples").glob("*/status.json")
    }

    rows = []
    gaps = []
    for item in coverage:
        name = item["file"]
        direct = direct_status.get(name)
        chooser = chooser_status.get(name)
        declared_keystrokes = item.get("keystrokes", [])
        encodable_keystrokes = encodable_source_keystrokes(name)
        driven_keystrokes = []
        if direct:
            driven_keystrokes.extend(direct.get("declared_keystrokes", []))
        if chooser:
            driven_keystrokes.extend(chooser.get("declared_keystrokes", []))
        source_features = {
            "commands": len(item.get("commands", [])),
            "keystrokes": len(declared_keystrokes),
            "menus": len(item.get("menus", [])),
            "presentation_types": len(item.get("presentation_types", [])),
            "translators": len(item.get("translators", [])),
            "actions": len(item.get("actions", [])),
            "gadgets": bool(item.get("has_gadgets")),
            "accepting_values": bool(item.get("has_accepting_values")),
            "tracking_pointer": bool(item.get("has_tracking_pointer")),
        }
        e2e_driven = {
            "direct_launch_keyboard_click_wheel": bool(direct and direct.get("ok")),
            "chooser_launch_keyboard_click_wheel": bool(chooser and chooser.get("ok")),
            "declared_keystrokes": sorted(set(driven_keystrokes)),
            "encodable_declared_keystrokes": encodable_keystrokes,
        }
        uncovered = []
        command_feature = command_results.get(name)
        surface_feature = surface_results.get(name)
        command_surface_checked = bool(
            command_feature
            and command_feature.get("commands_present", 0) == command_feature.get("commands", 0)
        )
        if source_features["commands"] and not command_surface_checked:
            uncovered.append("command-specific result assertions")
        if source_features["menus"] and not command_feature:
            uncovered.append("menu command traversal")
        translator_surface_checked = bool(
            command_feature
            and command_feature.get("translators_present", 0) > 0
        )
        if (
            source_features["presentation_types"]
            or source_features["translators"]
            or source_features["actions"]
        ) and not translator_surface_checked:
            uncovered.append("presentation selection/translator assertions")
        gadget_surface_checked = bool(
            surface_feature
            and surface_feature.get("ok")
            and surface_feature.get("gadgets_expected")
        )
        if source_features["gadgets"] and not gadget_surface_checked:
            uncovered.append("per-gadget value/callback assertions")
        accepting_values_checked = bool(
            surface_feature and surface_feature.get("accepting_values_runtime_checked")
        )
        if source_features["accepting_values"] and not accepting_values_checked:
            uncovered.append("accepting-values form completion assertions")
        tracking_pointer_checked = bool(
            surface_feature and surface_feature.get("tracking_pointer_runtime_checked")
        )
        if source_features["tracking_pointer"] and not tracking_pointer_checked:
            uncovered.append("tracking-pointer drag/motion assertions")
        missing_keystrokes = sorted(set(encodable_keystrokes) - set(driven_keystrokes))
        if missing_keystrokes:
            uncovered.append("declared keystrokes not driven")
        row = {
            "example": name,
            "source_features": source_features,
            "e2e_driven": e2e_driven,
            "missing_declared_keystrokes": missing_keystrokes,
            "remaining_feature_specific_gaps": uncovered,
        }
        rows.append(row)
        if uncovered:
            gaps.append(row)

    summary = {
        "examples": len(rows),
        "direct_examples": direct_summary.get("examples", 0),
        "direct_passed": direct_summary.get("passed", 0),
        "chooser_examples": chooser_summary.get("examples", 0),
        "chooser_passed": chooser_summary.get("passed", 0),
        "command_feature_examples": command_summary.get("examples", 0),
        "command_feature_passed": command_summary.get("passed", 0),
        "commands_checked": command_summary.get("commands", 0),
        "commands_present": command_summary.get("commands_present", 0),
        "surface_feature_examples": surface_summary.get("examples", 0),
        "surface_feature_passed": surface_summary.get("passed", 0),
        "gadget_count": surface_summary.get("gadget_count", 0),
        "gadget_events_exercised": surface_summary.get("gadget_events_exercised", 0),
        "examples_with_remaining_feature_specific_gaps": len(gaps),
        "strict": args.strict,
    }
    (out / "coverage.json").write_text(json.dumps(rows, indent=2), encoding="utf-8")
    (out / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    (out / "gaps.json").write_text(json.dumps(gaps, indent=2), encoding="utf-8")
    (out / "status.txt").write_text(
        ("fail\n" if args.strict and gaps else "pass\n")
        + json.dumps(summary, indent=2)
        + "\n",
        encoding="utf-8",
    )
    print(json.dumps(summary, indent=2))
    return 1 if args.strict and gaps else 0


if __name__ == "__main__":
    sys.exit(main())
