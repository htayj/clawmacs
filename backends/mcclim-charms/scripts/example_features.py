"""Source-derived feature probes for McCLIM example PTY tests."""

from __future__ import annotations

import os
import pathlib
import re


KEYSTROKE_PATTERN = re.compile(r":keystroke\s+([^)\n]+(?:\))?)", re.I)
IN_PACKAGE_PATTERN = re.compile(r"\(\s*(?:[\w-]+:)?in-package\s+(?:#?:)?([^\s()]+)\s*\)", re.I)
GESTURE_PATTERN = re.compile(
    r"\(\s*(?:clim:)?define-gesture-name\s+([^\s()]+)\s+(:[^\s()]+)\s+([^)\n]+(?:\))?)",
    re.I,
)


def examples_root() -> pathlib.Path:
    return pathlib.Path(
        os.environ.get("MCCLIM_SOURCE_ROOT", "/home/tay/reference/external_src/McCLIM/")
    ).expanduser().resolve() / "Examples"


def example_file(name: str) -> pathlib.Path:
    aliases = {
        "presentation-test": "presentation-test",
        "summation": "presentation-test",
    }
    return examples_root() / f"{aliases.get(name, name)}.lisp"


def source_keystrokes(name: str) -> list[str]:
    path = example_file(name)
    if not path.exists():
        return []
    source = path.read_text(encoding="utf-8")
    return [normalize_keystroke_spec(match.group(1)) for match in KEYSTROKE_PATTERN.finditer(source)]


def normalize_keystroke_spec(spec: str) -> str:
    spec = spec.strip()
    opens = spec.count("(")
    closes = spec.count(")")
    if opens > closes:
        spec += ")" * (opens - closes)
    return spec


def source_gestures(name: str) -> dict[str, tuple[str, str]]:
    path = example_file(name)
    if not path.exists():
        return {}
    source = path.read_text(encoding="utf-8")
    gestures: dict[str, tuple[str, str]] = {}
    for match in GESTURE_PATTERN.finditer(source):
        gesture_name = match.group(1).strip().lower()
        gesture_type = match.group(2).strip().lower()
        gesture_spec = normalize_keystroke_spec(match.group(3))
        gestures[gesture_name] = (gesture_type, gesture_spec)
    return gestures


def source_package(name: str) -> str:
    path = example_file(name)
    if not path.exists():
        return "CLIM-DEMO"
    source = path.read_text(encoding="utf-8")
    match = IN_PACKAGE_PATTERN.search(source)
    if not match:
        return "CLIM-DEMO"
    package = match.group(1).strip()
    return package.strip('"').upper()


def symbol_designator(name: str, package: str) -> tuple[str, str]:
    name = name.strip("'")
    if ":" in name:
        package_part, symbol_part = name.rsplit(":", 1)
        return package_part.strip(":").strip('"').upper(), symbol_part.strip(":").upper()
    return package, name.upper()


def _char_bytes(token: str) -> bytes | None:
    token = token.strip()
    if token.startswith("#\\"):
        name = token[2:]
        names = {
            "space": b" ",
            "Space": b" ",
            "tab": b"\t",
            "Tab": b"\t",
            "newline": b"\n",
            "Newline": b"\n",
            "return": b"\r",
            "Return": b"\r",
        }
        if name in names:
            return names[name]
        if len(name) == 1:
            return name.encode("ascii", errors="ignore")
    return None


def _ctrl_byte(char_byte: bytes) -> bytes | None:
    if len(char_byte) != 1:
        return None
    code = char_byte[0]
    if 97 <= code <= 122:
        return bytes([code - 96])
    if 65 <= code <= 90:
        return bytes([code - 64])
    if code == 32:
        return b"\x00"
    return None


def keystroke_bytes(spec: str, gestures: dict[str, tuple[str, str]] | None = None) -> bytes | None:
    gestures = gestures or {}
    spec = normalize_keystroke_spec(spec)
    cleaned = spec.replace("(", " ").replace(")", " ")
    tokens = cleaned.split()
    if not tokens:
        return None
    first = tokens[0]
    gesture = gestures.get(first.lower())
    if gesture:
        gesture_type, gesture_spec = gesture
        if gesture_type == ":keyboard":
            return keystroke_bytes(gesture_spec, gestures)
        if gesture_type == ":indirect":
            return keystroke_bytes(gesture_spec, gestures)
        return None
    lower_tokens = {token.lower() for token in tokens[1:]}
    special = {
        ":up": b"\x1b[A",
        ":down": b"\x1b[B",
        ":right": b"\x1b[C",
        ":left": b"\x1b[D",
    }
    base = special.get(first)
    if base is None:
        base = _char_bytes(first)
    if base is None:
        return None
    if ":control" in lower_tokens:
        base = _ctrl_byte(base) or base
    if ":meta" in lower_tokens:
        base = b"\x1b" + base
    return base


def example_keystroke_events(name: str) -> list[tuple[str, bytes]]:
    events: list[tuple[str, bytes]] = []
    gestures = source_gestures(name)
    for spec in source_keystrokes(name):
        encoded = keystroke_bytes(spec, gestures)
        if encoded is not None:
            events.append((spec, encoded))
    return events


def encodable_source_keystrokes(name: str) -> list[str]:
    gestures = source_gestures(name)
    return [
        spec
        for spec in source_keystrokes(name)
        if keystroke_bytes(spec, gestures) is not None
    ]
