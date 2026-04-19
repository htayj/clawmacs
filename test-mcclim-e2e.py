#!/usr/bin/env python3
"""McCLIM E2E tests for Clawmacs.

The harness runs Clawmacs under an existing X display, drives the McCLIM UI with
xdotool, captures ImageMagick screenshots, and reads structured state from
scripts/mcclim-e2e-driver.lisp.
"""
import argparse
import importlib.util
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time


CLAWMACS_DIR = os.path.dirname(os.path.abspath(__file__))
SCREENSHOT_DIR = os.path.join(CLAWMACS_DIR, "screenshots", "mcclim")
SESSION_NAME = "session-01"

PASSED = []
FAILED = []


def load_e2e_scenarios_module():
    path = os.path.join(CLAWMACS_DIR, "scripts", "e2e-scenarios.py")
    spec = importlib.util.spec_from_file_location("clawmacs_e2e_scenarios", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


E2E = load_e2e_scenarios_module()
DEFAULT_AGENT_NAME = E2E.DEFAULT_AGENT_NAME

OFFLINE_AGENT_EVAL = r"""
(setf (symbol-function 'clawmacs::send-to-agent-with-context)
      (lambda (buf)
        (let ((text (or (clawmacs::buffer-previous-user-command-text buf) "")))
          (cond
            ((search "async-render-probe" text :test #'char-equal)
             (setf (clawmacs::buffer-status buf) :thinking)
             (clawmacs:notify-buffer-display-change buf :async-render-test)
             (bt:make-thread
              (lambda ()
                (sleep 0.6)
                (clawmacs::buffer-insert-agent-message
                 buf
                 "MCCLIM-ASYNC-RENDER-VISIBLE")
                (setf (clawmacs::buffer-status buf) :idle)
                (clawmacs:notify-buffer-display-change buf :async-render-test))
              :name "mcclim-e2e-async-render"))
            ((search "stream-poll-probe" text :test #'char-equal)
             (let ((state (clawmacs::make-stream-state))
                   (agent-msg (clawmacs::buffer-insert-agent-message
                               buf "" :record-p nil :run-hook-p nil)))
               (setf (clawmacs::buffer-pending-stream buf) state
                     (clawmacs::buffer-streaming-message buf) agent-msg
                     (clawmacs::buffer-status buf) :thinking)
               (clawmacs:notify-buffer-display-change buf :stream-started)
               (bt:make-thread
                (lambda ()
                  (sleep 0.6)
                  (bt:with-lock-held ((clawmacs::stream-state-lock state))
                    (setf (clawmacs::stream-state-content-blocks state)
                          (list (clawmacs::canonical-text-block
                                 "MCCLIM-PULSE-STREAM-VISIBLE"))
                          (clawmacs::stream-state-stop-reason state) "end_turn"
                          (clawmacs::stream-state-done-p state) t)))
                :name "mcclim-e2e-pulse-stream")))
            (t
             (clawmacs::buffer-insert-agent-message
              buf
              (cond
                ((search "(+ 40 2)" text :test #'char-equal)
                 "42")
                ((search "describe-common-lisp-symbol-to-string" text
                         :test #'char-equal)
                 "format~%Reference: CL Community Spec")
                ((search "file_write" text :test #'char-equal)
                 "first second")
                ((search "alpha" text :test #'char-equal)
                 "alpha")
                ((search "beta" text :test #'char-equal)
                 "beta")
                ((search "tiling-resize-probe" text :test #'char-equal)
                 "MCCLIM-TILING-RESIZE-VISIBLE")
                (t
                 (format nil "offline echo: ~A" text))))
             (setf (clawmacs::buffer-status buf) :idle)
             (clawmacs:notify-buffer-display-change buf :offline-agent)))
          buf)))
"""

ONLINE_GROUPS = ("online", "online-zai", "online-openai-codex")
ONLINE_PROVIDER_SPECS = [
    {
        "slug": "zai",
        "name": "ZAI",
        "provider": ":zai",
        "provider_text": "zai",
        "model": "glm-5",
        "think_level": None,
        "expected": "MCCLIM-ZAI-ONLINE-PROBE",
    },
    {
        "slug": "openai-codex",
        "name": "OpenAI Codex",
        "provider": ":openai-codex",
        "provider_text": "openai-codex",
        "model": "gpt-5.4",
        "think_level": "low",
        "expected": "MCCLIM-OPENAI-CODEX-ONLINE-PROBE",
    },
]


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


def lisp_string(value):
    return E2E.lisp_string(value)


def key_name(name):
    aliases = {
        "Enter": "Return",
        "Backspace": "BackSpace",
        "PageUp": "Page_Up",
        "PageDown": "Page_Down",
        "Escape": "Escape",
        "Ctrl+a": "ctrl+a",
        "Ctrl+b": "ctrl+b",
        "Ctrl+c": "ctrl+c",
        "Ctrl+d": "ctrl+d",
        "Ctrl+e": "ctrl+e",
        "Ctrl+f": "ctrl+f",
        "Ctrl+g": "ctrl+g",
        "Ctrl+h": "ctrl+h",
        "Ctrl+k": "ctrl+k",
        "Ctrl+l": "ctrl+l",
        "Ctrl+n": "ctrl+n",
        "Ctrl+o": "ctrl+o",
        "Ctrl+p": "ctrl+p",
        "Ctrl+r": "ctrl+r",
        "Ctrl+s": "ctrl+s",
        "Ctrl+t": "ctrl+t",
        "Ctrl+u": "ctrl+u",
        "Ctrl+v": "ctrl+v",
        "Ctrl+w": "ctrl+w",
        "Ctrl+x": "ctrl+x",
        "Ctrl+y": "ctrl+y",
        "Alt+x": "alt+x",
        ".": "period",
        "_": "underscore",
    }
    return aliases.get(name, name)


def text_chunks(text, size=40):
    for start in range(0, len(text), size):
        yield text[start : start + size]


def type_delay_ms(length):
    return 8 if length > 120 else 1


def compact_deterministic_prompt(text):
    if "e2e-diff-test.txt" in text and "with-open-file" in text:
        return "offline diff setup"
    if 'clawmacs::execute-tool "file_write"' in text:
        return "file_write append probe"
    return text


def base_extra_evals(skill_root_path):
    return [f'(clawmacs:register-skill-root #P"{lisp_string(skill_root_path)}")']


def online_provider_specs(group):
    if group == "online":
        return list(ONLINE_PROVIDER_SPECS)
    slug = group.removeprefix("online-")
    return [spec for spec in ONLINE_PROVIDER_SPECS if spec["slug"] == slug]


def online_agent_eval(spec):
    args = [
        f'"{lisp_string(DEFAULT_AGENT_NAME)}"',
        ":provider",
        spec["provider"],
        ":model",
        f'"{lisp_string(spec["model"])}"',
    ]
    if spec.get("think_level"):
        args.extend([":think-level", f'"{lisp_string(spec["think_level"])}"'])
    return (
        "(progn "
        "(setf clawmacs:*default-max-tokens* 128) "
        f"(clawmacs:register-agent-definition {' '.join(args)}))"
    )


def wait_for_non_user_message_text(session, text, timeout=120):
    def observe():
        snapshot = session.snapshot()
        messages = (snapshot.get("buffer") or {}).get("messages") or []
        for message in messages:
            sender = str(message.get("sender", "")).lower()
            if sender != "user" and text in str(message.get("text", "")):
                return session.text()
        return None

    return wait_until(
        observe,
        timeout=timeout,
        interval=0.25,
        description=f"non-user message containing {text}",
    )


def render_visible_messages(snapshot):
    render = snapshot.get("render") or {}
    return render.get("visibleMessages") or render.get("visible-messages") or []


def render_contains_text(snapshot, text):
    for message in render_visible_messages(snapshot):
        if text in str(message.get("text", "")):
            return True
    return False


def render_get(render, camel_key, kebab_key, default=None):
    if camel_key in render:
        return render[camel_key]
    if kebab_key in render:
        return render[kebab_key]
    return default


def wait_for_rendered_message_text(session, text, timeout=15):
    return wait_until(
        lambda: session._snapshot_if(lambda snap: render_contains_text(snap, text)),
        timeout=timeout,
        interval=0.1,
        description=f"rendered message containing {text}",
    )


def wait_for_render_width(session, predicate, description, timeout=10):
    def observe():
        snapshot = session.snapshot()
        render = snapshot.get("render") or {}
        width = render.get("pixelWidth") or render.get("pixel-width") or 0
        if predicate(width):
            return snapshot
        return None

    return wait_until(observe, timeout=timeout, interval=0.1, description=description)


class McclimSession:
    def __init__(self, extra_evals=None):
        self.proc = None
        self.window_id = None
        self.focused = False
        self.extra_evals = extra_evals or []
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
            "(ql:quickload :clawmacs :silent t)",
            "--load",
            "scripts/mcclim-e2e-driver.lisp",
            "--eval",
            "(setf clawmacs:*inhibit-user-init* t)",
            "--eval",
            "(clawmacs/mcclim-e2e:start-control-thread)",
        ]
        for form in self.extra_evals:
            args.extend(["--eval", form])
        args.extend([
            "--eval",
            (
                "(handler-bind "
                "((error "
                "(lambda (condition) "
                "(format *error-output* \"~&MCCLIM-E2E-ERROR: ~A~%\" condition) "
                "(sb-debug:print-backtrace :stream *error-output* :count 120) "
                "(sb-ext:exit :code 70)))) "
                f'(clawmacs:clawmacs-main :session-name "{SESSION_NAME}"))'
            ),
        ])

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
        self.focus(force=True)
        return window_id

    def focus(self, force=False):
        if self.window_id and (force or not self.focused):
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
            self.focused = True

    def key(self, *keys):
        self.focus()
        run_checked(
            ["xdotool", "key", *[key_name(key) for key in keys]],
            timeout=5,
        )
        time.sleep(0.3)

    def press(self, key):
        self.key(key)

    def press_keys(self, keys):
        self.key(*keys)

    def resize(self, width, height):
        run_checked(
            [
                "xdotool",
                "windowsize",
                "--sync",
                self.window_id,
                str(width),
                str(height),
            ],
            timeout=5,
        )
        time.sleep(0.5)

    def type_text(self, text):
        text = compact_deterministic_prompt(text)
        self.focus()
        parts = text.split("\n")
        for index, part in enumerate(parts):
            self.type_text_part(part)
            if index != len(parts) - 1:
                self.key("Ctrl+o")
        if text and "\n" not in text:
            self.wait_for_typed_suffix(
                text,
                timeout=max(3.0, len(text) / 80.0),
                strict=len(text) > 120,
            )
        time.sleep(0.1)

    def type_text_part(self, part):
        delay = type_delay_ms(len(part))
        for chunk in text_chunks(part):
            self.type_chunk(chunk, delay)
            time.sleep(0.08 if delay > 1 else 0.02)

    def type_chunk(self, chunk, delay):
        path = os.path.join(self.artifact_root, "type-chunk.txt")
        with open(path, "w", encoding="utf-8") as stream:
            stream.write(chunk)
        run_checked(
            [
                "xdotool",
                "type",
                "--clearmodifiers",
                "--delay",
                str(delay),
                "--file",
                path,
            ],
            timeout=max(10, 1 + (len(chunk) * delay) // 500),
        )

    def wait_for_typed_suffix(self, text, timeout=3.0, strict=False):
        def observed(snapshot):
            minibuffer = snapshot.get("minibuffer") or {}
            if minibuffer.get("active") and str(minibuffer.get("input", "")).endswith(text):
                return True
            buffer = snapshot.get("buffer") or {}
            return str(buffer.get("input", "")).endswith(text)

        deadline = time.time() + timeout
        last_input = ""
        while time.time() < deadline:
            snapshot = self.snapshot()
            minibuffer = snapshot.get("minibuffer") or {}
            buffer = snapshot.get("buffer") or {}
            last_input = str(
                minibuffer.get("input", "")
                if minibuffer.get("active")
                else buffer.get("input", "")
            )
            if observed(snapshot):
                return
            time.sleep(0.05)
        if strict:
            fail(
                "typed text did not reach input; "
                f"expected suffix length {len(text)}, last input length {len(last_input)}"
            )
        return False

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

    def text(self):
        snapshot = self.snapshot()
        buffer = snapshot.get("buffer") or {}
        minibuffer = snapshot.get("minibuffer") or {}
        selectors = snapshot.get("selectors") or {}
        skill = snapshot.get("skillCompletion") or {}
        lines = []

        if selectors.get("bufferSelectorActive"):
            lines.append("Switch Buffer")
            for entry in snapshot.get("buffers") or []:
                marker = "*" if entry.get("current") else " "
                lines.append(
                    f"{marker} {entry.get('name', '')}  "
                    f"[{entry.get('agent', '')}] {entry.get('status', '')}  "
                    f"msgs:{entry.get('messageCount', 0)}"
                )
        if selectors.get("modelSelectorActive"):
            lines.append("Select Model")
        if selectors.get("thinkSelectorActive"):
            lines.append("Select Think Level")

        if minibuffer.get("active"):
            prompt = minibuffer.get("prompt", "")
            input_text = minibuffer.get("input", "")
            lines.append(f"{prompt}: {input_text}".rstrip())
            for candidate in minibuffer.get("candidates") or []:
                if candidate:
                    lines.append(candidate)

        if skill.get("active"):
            lines.append(f"Skill: {skill.get('token', '')}")
            for candidate in skill.get("candidates") or []:
                if candidate:
                    lines.append(candidate)

        approval = buffer.get("approval")
        if approval:
            lines.append("PERMISSION REQUIRED")
            lines.append("[a]pprove  [d]eny  deny with [m]essage")

        for message in buffer.get("messages") or []:
            sender = message.get("sender", "message")
            text = message.get("text", "")
            for i, line in enumerate(str(text).splitlines() or [""]):
                prefix = f"{sender}> " if i == 0 else ""
                lines.append(prefix + line)

        input_text = buffer.get("input", "")
        input_lines = str(input_text).splitlines() or [""]
        for i, line in enumerate(input_lines):
            lines.append(("user> " if i == 0 else "") + line)

        for row in buffer.get("whoLine") or []:
            lines.append(row)
        if buffer.get("modeline"):
            lines.append(buffer["modeline"])
        return "\n".join(lines)

    def screenshot(self, name):
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
        return png_path

    def close(self):
        if self.proc and self.proc.poll() is None:
            try:
                self.key("Ctrl+x", "Ctrl+c")
                self.proc.wait(timeout=5)
            except Exception:
                self.proc.terminate()
                try:
                    self.proc.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    self.proc.kill()
                    self.proc.wait(timeout=5)


def run_test(name, fn, session):
    print(f"  [{name}] ", end="", flush=True)
    try:
        fn(session)
        screen_after = session.text()
        E2E.assert_not_contains(
            screen_after,
            "[Error:",
            f"{name}: unexpected runtime error detected",
        )
        PASSED.append(name)
        print("PASS")
    except Exception as exc:
        FAILED.append((name, str(exc)))
        print(f"FAIL: {exc}")
        try:
            session.screenshot(f"FAIL-{name}")
        except Exception:
            pass
    finally:
        snapshot = session.snapshot()
        if (snapshot.get("minibuffer") or {}).get("active") or any(
            (snapshot.get("selectors") or {}).get(key)
            for key in (
                "bufferSelectorActive",
                "sessionTreeSelectorActive",
                "modelSelectorActive",
                "thinkSelectorActive",
            )
        ):
            session.press("Ctrl+g")


def make_online_modeline_test(spec):
    def test(session):
        screen = session.text()
        provider_model = f"{spec['provider_text']}/{spec['model']}"
        E2E.assert_contains(
            screen,
            provider_model,
            f"{spec['name']} provider/model in modeline",
        )
        if spec.get("think_level"):
            E2E.assert_contains(
                screen,
                f"think:{spec['think_level']}",
                f"{spec['name']} think level in modeline",
            )
        session.screenshot(f"online-{spec['slug']}-modeline")

    return test


def make_online_response_test(spec):
    def test(session):
        expected = spec["expected"]
        E2E.set_input(session, f"Reply with exactly: {expected}")
        session.press("Enter")
        screen = wait_for_non_user_message_text(session, expected, timeout=120)
        E2E.assert_contains(
            screen,
            expected,
            f"{spec['name']} live provider response",
        )
        session.screenshot(f"online-{spec['slug']}-response")

    return test


def online_test_registry(spec):
    return [
        (f"online-{spec['slug']}-modeline", make_online_modeline_test(spec)),
        (f"online-{spec['slug']}-response", make_online_response_test(spec)),
    ]


def test_53_async_agent_reply_renders_without_next_input(session):
    """Async agent messages must render without waiting for another user key."""
    expected = "MCCLIM-ASYNC-RENDER-VISIBLE"
    E2E.set_input(session, "async-render-probe")
    session.press("Enter")
    snapshot = wait_for_rendered_message_text(session, expected, timeout=10)
    E2E.assert_contains(
        session.text(),
        expected,
        "async agent response in semantic snapshot",
    )
    if not render_contains_text(snapshot, expected):
        fail("async agent response reached buffer but not McCLIM render snapshot")
    session.screenshot("53-async-agent-reply-renders")


def test_54_tiling_resize_keeps_latest_message_visible(session):
    """Externally imposed resizes should recompute the visible McCLIM pane."""
    expected = "MCCLIM-TILING-RESIZE-VISIBLE"
    E2E.set_input(session, "tiling-resize-probe")
    session.press("Enter")
    first = wait_for_rendered_message_text(session, expected, timeout=10)
    initial_render = first.get("render") or {}
    initial_width = initial_render.get("pixelWidth") or initial_render.get("pixel-width") or 0

    session.resize(640, 420)
    narrow = wait_for_render_width(
        session,
        lambda width: width and width < initial_width,
        "narrow McCLIM render width",
        timeout=10,
    )
    if not render_contains_text(narrow, expected):
        fail("latest agent message disappeared after narrow resize")
    narrow_render = narrow.get("render") or {}
    if render_get(narrow_render, "inputStartRow", "input-start-row", -1) < 0:
        fail("input row was not present after narrow resize")

    session.resize(1000, 680)
    wide = wait_for_render_width(
        session,
        lambda width: width and width > (narrow_render.get("pixelWidth")
                                         or narrow_render.get("pixel-width")
                                         or 0),
        "wide McCLIM render width",
        timeout=10,
    )
    if not render_contains_text(wide, expected):
        fail("latest agent message disappeared after wide resize")
    session.screenshot("54-tiling-resize-latest-visible")


def test_55_stream_poll_renders_without_next_input(session):
    """Provider-style streaming must repaint from pulse polling alone."""
    expected = "MCCLIM-PULSE-STREAM-VISIBLE"
    E2E.set_input(session, "stream-poll-probe")
    session.press("Enter")
    snapshot = wait_for_rendered_message_text(session, expected, timeout=10)
    E2E.assert_contains(
        session.text(),
        expected,
        "streamed agent response in semantic snapshot",
    )
    if not render_contains_text(snapshot, expected):
        fail("streamed response reached buffer but not McCLIM render snapshot")
    session.screenshot("55-stream-poll-renders")


def test_registry(group):
    offline_tests = [
        ("53-async-agent-reply-renders", test_53_async_agent_reply_renders_without_next_input),
        ("54-tiling-resize-latest-visible", test_54_tiling_resize_keeps_latest_message_visible),
        ("55-stream-poll-renders", test_55_stream_poll_renders_without_next_input),
        ("38-shell-prefix", E2E.test_38_shell_prefix),
        ("39-debug-mode", E2E.test_39_debug_mode_toggle),
        ("40-save-session", E2E.test_40_save_session),
        ("41-buffer-persistence", E2E.test_41_buffer_state_persistence),
        ("42-minibuffer-selector", E2E.test_42_minibuffer_buffer_selector),
        ("43-describe-bindings", E2E.test_43_describe_bindings),
        ("44-describe-function", E2E.test_44_describe_function),
        ("45-describe-variable", E2E.test_45_describe_variable),
        ("46-describe-type", E2E.test_46_describe_type),
        ("47-customize-face", E2E.test_47_customize_face),
        ("52-skill-completion", E2E.test_52_skill_completion),
    ]
    llm_new_tests = [
        ("48-tool-lisp-eval", E2E.test_48_tool_lisp_eval),
        ("49-tool-spec-lookup", E2E.test_49_tool_spec_lookup),
        ("50-multi-turn", E2E.test_50_multi_turn),
        ("51-toggle-tool-results", E2E.test_51_toggle_tool_results),
    ]
    readline_tests = [
        ("22-ctrl-b", E2E.test_22_ctrl_b),
        ("23-ctrl-f", E2E.test_23_ctrl_f),
        ("24-alt-b", E2E.test_24_alt_b),
        ("25-alt-f", E2E.test_25_alt_f),
        ("26-ctrl-a", E2E.test_26_ctrl_a),
        ("27-ctrl-e", E2E.test_27_ctrl_e),
        ("28-ctrl-u", E2E.test_28_ctrl_u),
        ("29-ctrl-k", E2E.test_29_ctrl_k),
        ("30-alt-d", E2E.test_30_alt_d),
        ("31-ctrl-w", E2E.test_31_ctrl_w),
        ("32-ctrl-y", E2E.test_32_ctrl_y),
        ("33-alt-y", E2E.test_33_alt_y),
        ("34-alt-ctrl-y", E2E.test_34_alt_ctrl_y),
        ("35-alt-dot", E2E.test_35_alt_dot),
        ("36-alt-underscore", E2E.test_36_alt_underscore),
        ("37-ctrl-d", E2E.test_37_ctrl_d),
    ]
    full_initial_tests = [
        ("01-initial-render", E2E.test_01_initial_render),
        ("02-text-input", E2E.test_02_text_input),
        ("03-line-editing", E2E.test_03_line_editing_c_a_c_e),
        ("04-kill-yank", E2E.test_04_kill_yank),
        ("05-multiline-input", E2E.test_05_multiline_input),
        ("06-send-message", E2E.test_06_send_message),
        ("07-line-wrapping", E2E.test_07_line_wrapping),
        ("08-scroll", E2E.test_08_scroll),
        ("09-meta-scroll", E2E.test_09_meta_scroll),
        ("10-new-buffer", E2E.test_10_new_buffer),
        ("11-switch-buffer", E2E.test_11_switch_buffer),
        ("12-kill-buffer", E2E.test_12_kill_buffer),
        ("13-backspace", E2E.test_13_backspace),
        ("14-point-face", E2E.test_14_point_face),
        ("15-permission-approve", E2E.test_15_permission_approve),
        ("16-permission-deny", E2E.test_16_permission_deny),
        ("17-permission-deny-message", E2E.test_17_permission_deny_with_message),
        ("18-file-write-diff", E2E.test_18_file_write_diff),
        ("19-file-write-append", E2E.test_19_file_write_append),
        ("20-file-edit", E2E.test_20_file_edit_search_replace),
        ("21-modeline", E2E.test_21_modeline_content),
    ]

    if group == "smoke":
        return full_initial_tests[:2]
    if group == "offline":
        return offline_tests + readline_tests
    if group == "readline":
        return readline_tests
    return full_initial_tests + offline_tests + llm_new_tests + readline_tests


def parse_args():
    parser = argparse.ArgumentParser(description="Run clawmacs McCLIM e2e tests")
    parser.add_argument(
        "--only",
        choices=["all", "readline", "offline", "smoke", *ONLINE_GROUPS],
        default="offline",
        help="run only a subset of tests",
    )
    return parser.parse_args()


def print_summary():
    print(f"\n=== Results: {len(PASSED)} passed, {len(FAILED)} failed ===")
    if FAILED:
        print("\nFailed tests:")
        for name, err in FAILED:
            print(f"  {name}: {err}")
        return 1
    print("All tests passed!")
    print(f"Screenshots saved to {SCREENSHOT_DIR}/")
    return 0


def run_deterministic_suite(group, skill_root_path):
    extra_evals = base_extra_evals(skill_root_path)
    extra_evals.append(f"(progn {OFFLINE_AGENT_EVAL})")
    session = McclimSession(
        extra_evals=extra_evals
    )

    try:
        print("=== Clawmacs McCLIM E2E Tests ===")
        print(f"Screenshots: {SCREENSHOT_DIR}/")
        print("Launching clawmacs McCLIM...")
        session.launch()
        print("clawmacs ready.\n")

        for name, fn in test_registry(group):
            run_test(name, fn, session)

        return print_summary()
    finally:
        print(f"Artifacts: {SCREENSHOT_DIR}")
        print(f"Control/logs: {session.artifact_root}")
        session.close()


def run_online_suite(group, skill_root_path):
    print("=== Clawmacs McCLIM Online E2E Tests ===")
    print(f"Screenshots: {SCREENSHOT_DIR}/")
    for spec in online_provider_specs(group):
        extra_evals = base_extra_evals(skill_root_path)
        extra_evals.append(online_agent_eval(spec))
        session = McclimSession(extra_evals=extra_evals)
        try:
            print(f"Launching clawmacs McCLIM for {spec['name']}...")
            session.launch()
            print("clawmacs ready.\n")

            for name, fn in online_test_registry(spec):
                run_test(name, fn, session)
        finally:
            print(f"Control/logs: {session.artifact_root}")
            session.close()
    print(f"Artifacts: {SCREENSHOT_DIR}")
    return print_summary()


def main():
    args = parse_args()
    ensure_tool("xdotool")
    ensure_tool("import")
    os.makedirs(SCREENSHOT_DIR, exist_ok=True)

    skill_root = E2E.create_e2e_skill_root()
    skill_root_path = skill_root if skill_root.endswith(os.sep) else skill_root + os.sep
    try:
        if args.only in ONLINE_GROUPS:
            return run_online_suite(args.only, skill_root_path)
        return run_deterministic_suite(args.only, skill_root_path)
    finally:
        shutil.rmtree(skill_root, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
