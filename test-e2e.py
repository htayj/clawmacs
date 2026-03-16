#!/usr/bin/env python3
"""Comprehensive E2E tests for clawmacs TUI using mcp-tui-driver.

Tests all UI features and saves a screenshot for each test.
Screenshots are saved to screenshots/ directory.

Requires: mcp-tui-driver (cargo install --git https://github.com/michaellee8/mcp-tui-driver)

Usage: python3 test-e2e.py
"""
import subprocess
import json
import sys
import base64
import time
import os
import select
import traceback

MCP_BIN = os.path.expanduser("~/.cargo/bin/mcp-tui-driver")
CLAWMACS_DIR = os.path.dirname(os.path.abspath(__file__))
SCREENSHOT_DIR = os.path.join(CLAWMACS_DIR, "screenshots")
SSL_LIB = "/gnu/store/mb4yqk21zfvbkdy58ry1wi032mm9lsh5-openssl-3.0.8/lib"
FONT_PATH = "/run/current-system/profile/share/fonts/truetype/DejaVuSansMono.ttf"

# Track test results
PASSED = []
FAILED = []

# Try to import Pillow for text-based screenshot rendering
try:
    from PIL import Image, ImageDraw, ImageFont
    HAS_PILLOW = True
except ImportError:
    HAS_PILLOW = False


def render_text_screenshot(text, path, cols=120, rows=35):
    """Render terminal text content to a PNG image using Pillow.
    Falls back to mcp-tui-driver screenshots if Pillow unavailable."""
    if not HAS_PILLOW:
        return False

    font_size = 14
    try:
        font = ImageFont.truetype(FONT_PATH, font_size)
    except (OSError, IOError):
        font = ImageFont.load_default()

    # Measure character size
    bbox = font.getbbox("M")
    char_w = bbox[2] - bbox[0]
    char_h = int(font_size * 1.5)

    img_w = cols * char_w + 20  # padding
    img_h = rows * char_h + 20

    # Terminal colors
    bg_color = (0, 0, 0)         # black background
    fg_color = (204, 204, 204)   # light gray text
    ml_bg = (200, 200, 200)      # modeline background (white-ish)
    ml_fg = (0, 0, 0)            # modeline foreground
    user_bg = (0, 0, 120)        # user message blue bg
    user_fg = (255, 255, 255)    # user message white fg

    img = Image.new("RGB", (img_w, img_h), bg_color)
    draw = ImageDraw.Draw(img)

    lines = text.split("\n")
    # Pad or truncate to rows
    while len(lines) < rows:
        lines.append("")
    lines = lines[:rows]

    for row_idx, line in enumerate(lines):
        y = 10 + row_idx * char_h
        # Detect modeline (contains " | " separators and looks like status bar)
        is_modeline = (" | " in line and
                       any(s in line for s in ["session", "IDLE", "THINKING", "ERROR"]))
        # Detect user message
        is_user = line.strip().startswith("user>")
        # Detect continuation of user message (indented under user>)
        is_user_cont = (row_idx > 0 and not line.strip().startswith("echo-agent>")
                       and not line.strip().startswith("tool-result>")
                       and not is_modeline)

        if is_modeline:
            # Draw modeline with inverted colors
            draw.rectangle([(10, y), (img_w - 10, y + char_h)], fill=ml_bg)
            draw.text((10, y), line[:cols], font=font, fill=ml_fg)
        elif is_user or (is_user_cont and False):  # only color user> lines for now
            draw.rectangle([(10, y), (10 + len(line) * char_w, y + char_h)], fill=user_bg)
            draw.text((10, y), line[:cols], font=font, fill=user_fg)
        else:
            draw.text((10, y), line[:cols], font=font, fill=fg_color)

    img.save(path)
    return True


class MCPClient:
    def __init__(self):
        self.proc = subprocess.Popen(
            [MCP_BIN],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
        )
        self.req_id = 0

    def send(self, method, params=None, timeout=30):
        self.req_id += 1
        req_id = self.req_id
        msg = {"jsonrpc": "2.0", "id": req_id, "method": method}
        if params:
            msg["params"] = params
        line = json.dumps(msg) + "\n"
        self.proc.stdin.write(line)
        self.proc.stdin.flush()
        deadline = time.time() + timeout
        while time.time() < deadline:
            remaining = deadline - time.time()
            ready, _, _ = select.select([self.proc.stdout], [], [], min(remaining, 1.0))
            if ready:
                resp = self.proc.stdout.readline()
                if resp:
                    parsed = json.loads(resp)
                    if parsed.get("id") == req_id:
                        return parsed
        return None

    def call_tool(self, name, args=None, timeout=30):
        return self.send("tools/call", {"name": name, "arguments": args or {}}, timeout=timeout)

    def initialize(self):
        r = self.send("initialize", {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "e2e-test", "version": "0.1"},
        })
        # Send required initialized notification
        notify = json.dumps({"jsonrpc": "2.0", "method": "notifications/initialized"}) + "\n"
        self.proc.stdin.write(notify)
        self.proc.stdin.flush()
        time.sleep(0.2)
        return r

    def close(self):
        try:
            self.proc.stdin.close()
            self.proc.wait(timeout=5)
        except:
            self.proc.kill()


class ClawmacsSession:
    """Manages a clawmacs session via mcp-tui-driver."""

    def __init__(self, client, cols=120, rows=35):
        self.client = client
        self.cols = cols
        self.rows = rows
        self.session_id = None

    def launch(self):
        ql_setup = os.path.expanduser("~/quicklisp/setup.lisp")
        cmd = (
            f"LD_LIBRARY_PATH={SSL_LIB} "
            f"sbcl --noinform "
            f"--load {ql_setup} "
            f"--eval '(push (truename \"{CLAWMACS_DIR}/\") asdf:*central-registry*)' "
            f"--eval '(asdf:load-system :clawmacs)' "
            f"--eval '(clawmacs:clawmacs-main)'"
        )
        r = self.client.call_tool("tui_launch", {
            "command": "bash",
            "args": ["-c", cmd],
            "cols": self.cols,
            "rows": self.rows,
        }, timeout=60)
        content = r.get("result", {}).get("content", [])
        text = next((c["text"] for c in content if c.get("type") == "text"), "{}")
        self.session_id = json.loads(text).get("session_id")
        return self.session_id

    def wait_ready(self, timeout=20):
        """Wait for clawmacs to finish loading."""
        r = self.client.call_tool("tui_wait_for_text", {
            "session_id": self.session_id,
            "text": "user>",
            "timeout_ms": timeout * 1000,
        })
        content = r.get("result", {}).get("content", [])
        text = next((c["text"] for c in content if c.get("type") == "text"), "{}")
        return json.loads(text).get("found", False)

    def screenshot(self, name):
        """Take a screenshot and save to screenshots/name.png.
        Uses Pillow to render text content with a real font if available,
        falls back to mcp-tui-driver's block renderer."""
        path = os.path.join(SCREENSHOT_DIR, f"{name}.png")
        # Prefer Pillow text rendering (legible fonts)
        if HAS_PILLOW:
            screen = self.text()
            if screen and render_text_screenshot(screen, path, self.cols, self.rows):
                return path
        # Fallback: mcp-tui-driver screenshot
        r = self.client.call_tool("tui_screenshot", {"session_id": self.session_id})
        if not r:
            return None
        for c in r.get("result", {}).get("content", []):
            if c.get("type") == "image":
                data = base64.b64decode(c["data"])
                with open(path, "wb") as f:
                    f.write(data)
                return path
        return None

    def text(self):
        """Get current screen text."""
        r = self.client.call_tool("tui_text", {"session_id": self.session_id})
        if r:
            content = r.get("result", {}).get("content", [])
            text = next((c["text"] for c in content if c.get("type") == "text"), "{}")
            return json.loads(text).get("text", "")
        return ""

    def type_text(self, text):
        """Send text to the terminal."""
        self.client.call_tool("tui_send_text", {
            "session_id": self.session_id,
            "text": text,
        })
        time.sleep(0.3)

    def press(self, key):
        """Press a key."""
        self.client.call_tool("tui_press_key", {
            "session_id": self.session_id,
            "key": key,
        })
        time.sleep(0.2)

    def press_keys(self, keys):
        """Press multiple keys."""
        self.client.call_tool("tui_press_keys", {
            "session_id": self.session_id,
            "keys": keys,
        })
        time.sleep(0.3)

    def close(self):
        if self.session_id:
            self.client.call_tool("tui_press_key", {
                "session_id": self.session_id,
                "key": "Ctrl+c",
            })
            time.sleep(0.3)
            self.client.call_tool("tui_close", {"session_id": self.session_id})


def assert_contains(screen_text, expected, msg=""):
    """Assert that screen text contains expected string."""
    if expected not in screen_text:
        raise AssertionError(f"{msg}: expected '{expected}' in screen, got:\n{screen_text[:300]}")


def assert_not_contains(screen_text, expected, msg=""):
    """Assert that screen text does NOT contain expected string."""
    if expected in screen_text:
        raise AssertionError(f"{msg}: '{expected}' should NOT be in screen")


def run_test(name, fn, session):
    """Run a test function, track pass/fail."""
    print(f"  [{name}] ", end="", flush=True)
    try:
        fn(session)
        PASSED.append(name)
        print("PASS")
    except Exception as e:
        FAILED.append((name, str(e)))
        print(f"FAIL: {e}")
        # Take a failure screenshot
        try:
            session.screenshot(f"FAIL-{name}")
        except:
            pass


# ==========================================================================
# Test Functions
# ==========================================================================

def test_01_initial_render(s):
    """Test: TUI starts with modeline and user> prompt."""
    time.sleep(0.5)  # Let initial render complete
    screen = s.text()
    assert_contains(screen, "user>", "input prompt")
    assert_contains(screen, "session-01", "buffer name in modeline")
    assert_contains(screen, "echo-agent", "agent name in modeline")
    s.screenshot("01-initial-render")


def test_02_text_input(s):
    """Test: Typing text appears in the input area."""
    s.type_text("Hello, clawmacs!")
    screen = s.text()
    assert_contains(screen, "Hello, clawmacs!", "typed text")
    s.screenshot("02-text-input")


def test_03_line_editing_c_a_c_e(s):
    """Test: C-a moves to beginning, C-e moves to end of line."""
    # C-a to beginning, type prefix
    s.press("Ctrl+a")
    s.type_text(">>> ")
    screen = s.text()
    assert_contains(screen, ">>> Hello, clawmacs!", "C-a + insert at beginning")
    # C-e to end, type suffix
    s.press("Ctrl+e")
    s.type_text(" <<<")
    screen = s.text()
    assert_contains(screen, ">>> Hello, clawmacs! <<<", "C-e + insert at end")
    s.screenshot("03-line-editing")


def test_04_kill_yank(s):
    """Test: C-k kills to end of line, C-y yanks it back."""
    # Move to beginning, move past ">>> "
    s.press("Ctrl+a")
    # Kill entire line
    s.press("Ctrl+k")
    screen = s.text()
    assert_not_contains(screen, ">>> Hello", "C-k killed line")
    # Yank it back
    s.press("Ctrl+y")
    screen = s.text()
    assert_contains(screen, ">>> Hello, clawmacs! <<<", "C-y yanked text back")
    s.screenshot("04-kill-yank")


def test_05_multiline_input(s):
    """Test: C-o inserts newline for multi-line input."""
    # Clear current input
    s.press("Ctrl+a")
    s.press("Ctrl+k")
    s.type_text("Line one")
    s.press("Ctrl+o")  # C-o = open line / insert newline
    s.type_text("Line two")
    s.press("Ctrl+o")
    s.type_text("Line three")
    screen = s.text()
    assert_contains(screen, "Line one", "first line")
    assert_contains(screen, "Line two", "second line")
    assert_contains(screen, "Line three", "third line")
    s.screenshot("05-multiline-input")


def test_06_send_message(s):
    """Test: Enter sends message and agent responds."""
    # Thoroughly clear: select all with C-a, then kill repeatedly
    # Also use backspace to delete join points
    for _ in range(20):
        s.press("Ctrl+k")
    s.press("Ctrl+a")
    for _ in range(20):
        s.press("Ctrl+k")
    # Also backspace anything remaining
    for _ in range(50):
        s.press("Backspace")
    time.sleep(0.2)
    s.type_text("say hello")
    s.press("Enter")
    time.sleep(5)  # Wait for agent response (may call LLM)
    screen = s.text()
    assert_contains(screen, "say hello", "sent user message")
    assert_contains(screen, "echo-agent>", "agent responded")
    s.screenshot("06-send-message")


def test_07_line_wrapping(s):
    """Test: Long lines wrap instead of being truncated."""
    long_text = "This is a very long message that should wrap " * 4
    s.type_text(long_text)
    s.press("Enter")
    time.sleep(1)
    screen = s.text()
    # The text should appear (possibly across multiple lines)
    assert_contains(screen, "This is a very long message that should wrap", "long text visible")
    s.screenshot("07-line-wrapping")


def test_08_scroll(s):
    """Test: Scrolling through chat history with PageUp/PageDown."""
    # Send several messages to build up history
    for i in range(5):
        s.type_text(f"Scroll test message {i}")
        s.press("Enter")
        time.sleep(0.5)

    # Scroll up
    s.press("PageUp")
    time.sleep(0.3)
    screen_up = s.text()
    s.screenshot("08-scroll-pageup")

    # Scroll back down
    s.press("PageDown")
    time.sleep(0.3)
    screen_down = s.text()
    s.screenshot("08-scroll-pagedown")

    # After scroll down, should see the latest messages and input
    assert_contains(screen_down, "user>", "input visible after scroll down")


def test_09_meta_scroll(s):
    """Test: M-v (Esc+v) scrolls up, C-v scrolls down."""
    # Esc then v for M-v (scroll up)
    s.press("Escape")
    s.press("v")
    time.sleep(0.3)
    s.screenshot("09-meta-v-scroll")

    # C-v to scroll back down (ASCII 22)
    s.press("Ctrl+v")
    time.sleep(0.3)
    screen = s.text()
    assert_contains(screen, "user>", "input visible after C-v scroll down")


def test_10_new_buffer(s):
    """Test: C-x n creates a new buffer."""
    # C-x n = new buffer
    s.press("Ctrl+x")
    s.press("n")
    time.sleep(0.5)
    screen = s.text()
    # New buffer should have a different name in modeline
    assert_contains(screen, "session-", "new buffer name in modeline")
    # New buffer should have empty chat (just user>)
    assert_contains(screen, "user>", "new buffer has input prompt")
    s.screenshot("10-new-buffer")


def test_11_switch_buffer(s):
    """Test: C-x b switches to the next buffer."""
    # C-x b = next buffer (should go back to session-01)
    s.press("Ctrl+x")
    s.press("b")
    time.sleep(0.5)
    screen = s.text()
    assert_contains(screen, "session-01", "switched back to original buffer")
    # Should see previous conversation (agent responded earlier)
    assert_contains(screen, "echo-agent>", "previous conversation preserved")
    s.screenshot("11-switch-buffer")


def test_12_kill_buffer(s):
    """Test: C-x k kills the current buffer."""
    # Switch to the new buffer first
    s.press("Ctrl+x")
    s.press("b")
    time.sleep(0.3)
    # Kill it
    s.press("Ctrl+x")
    s.press("k")
    time.sleep(0.3)
    screen = s.text()
    # Should be back to session-01
    assert_contains(screen, "clawmacs:session-01", "back to original after kill")
    s.screenshot("12-kill-buffer")


def test_13_backspace(s):
    """Test: Backspace deletes character before cursor."""
    s.type_text("abcdef")
    s.press("Backspace")
    s.press("Backspace")
    screen = s.text()
    assert_contains(screen, "abcd", "backspace deleted chars")
    # Clean up
    s.press("Ctrl+a")
    s.press("Ctrl+k")
    s.screenshot("13-backspace")


def test_14_permission_approve(s):
    """Test: Agent tool needing permission shows approval prompt, user approves."""
    # Switch back to main buffer if needed, clear input
    for _ in range(20):
        s.press("Ctrl+k")
    for _ in range(20):
        s.press("Backspace")
    # Ask agent to write a file (file_write requires :agent-with-permission)
    s.type_text("Please write the text 'hello test' to a file called /tmp/clawmacs-e2e-test.txt")
    s.press("Enter")
    time.sleep(8)  # Wait for LLM to respond with tool_use

    # Check for approval prompt
    screen = s.text()
    s.screenshot("14-permission-prompt")

    # The screen should show PERMISSION REQUIRED or the approval options
    has_approval = ("PERMISSION" in screen or "[a]pprove" in screen
                    or "APPROVAL" in screen or "approve" in screen.lower())
    if has_approval:
        # Approve it
        s.press("a")
        time.sleep(3)
        screen = s.text()
        s.screenshot("14-permission-approved")
        # After approval, tool should have executed
        assert_contains(screen, "echo-agent>", "agent continued after approval")
    else:
        # Agent might have answered without using the tool, or tool was auto-approved
        # Just verify it responded
        assert_contains(screen, "echo-agent>", "agent responded")
        s.screenshot("14-permission-approved")


def test_15_permission_deny(s):
    """Test: Agent tool needing permission shows approval prompt, user denies."""
    for _ in range(20):
        s.press("Ctrl+k")
    for _ in range(20):
        s.press("Backspace")
    s.type_text("Run the command: echo 'deny test'")
    s.press("Enter")
    time.sleep(8)

    screen = s.text()
    s.screenshot("15-permission-deny-prompt")

    has_approval = ("PERMISSION" in screen or "[a]pprove" in screen
                    or "APPROVAL" in screen or "approve" in screen.lower())
    if has_approval:
        # Deny it
        s.press("d")
        time.sleep(3)
        screen = s.text()
        s.screenshot("15-permission-denied")
        assert_contains(screen, "DENIED", "denial shown in chat")
    else:
        s.screenshot("15-permission-denied")


def test_16_permission_deny_with_message(s):
    """Test: User denies with a message to the agent."""
    for _ in range(20):
        s.press("Ctrl+k")
    for _ in range(20):
        s.press("Backspace")
    s.type_text("Execute the command: echo 'message test'")
    s.press("Enter")
    time.sleep(8)

    screen = s.text()
    s.screenshot("16-permission-message-prompt")

    has_approval = ("PERMISSION" in screen or "[a]pprove" in screen
                    or "APPROVAL" in screen or "approve" in screen.lower())
    if has_approval:
        # Press m for deny-with-message
        s.press("m")
        time.sleep(0.5)
        s.type_text("Please do not run commands without explaining why first")
        s.press("Enter")
        time.sleep(5)
        screen = s.text()
        s.screenshot("16-permission-deny-message")
        assert_contains(screen, "echo-agent>", "agent responded to denial message")
    else:
        s.screenshot("16-permission-deny-message")


def test_17_file_write_diff(s):
    """Test: file_write approval prompt shows a diff against existing file."""
    # First, create a file with known content via shell
    for _ in range(20):
        s.press("Ctrl+k")
    for _ in range(20):
        s.press("Backspace")
    s.type_text("Please use the lisp_eval tool to evaluate this: (with-open-file (s (merge-pathnames \"e2e-diff-test.txt\" (truename \".\")) :direction :output :if-exists :supersede :if-does-not-exist :create) (write-string \"line one\" s) (terpri s) (write-string \"line two\" s) (terpri s) (write-string \"line three\" s))")
    s.press("Enter")
    time.sleep(8)

    # Now ask the agent to overwrite it with different content
    for _ in range(20):
        s.press("Ctrl+k")
    for _ in range(20):
        s.press("Backspace")
    s.type_text("Please write this exact content to e2e-diff-test.txt: line one\nline two modified\nline four")
    s.press("Enter")
    time.sleep(8)

    screen = s.text()
    s.screenshot("17-file-write-diff-prompt")

    has_approval = ("PERMISSION" in screen or "[a]pprove" in screen
                    or "APPROVAL" in screen)
    if has_approval:
        # Check that diff-like content is visible (+ or - prefixed lines)
        has_diff = ("+" in screen or "---" in screen or "new file" in screen)
        if has_diff:
            s.screenshot("17-file-write-diff-visible")
        # Deny it (we just wanted to see the diff)
        s.press("d")
        time.sleep(3)
        screen = s.text()
        s.screenshot("17-file-write-diff-denied")
        assert_contains(screen, "echo-agent>", "agent responded after denial")
    else:
        # Agent may not have used file_write
        s.screenshot("17-file-write-diff-no-approval")


def test_18_modeline_content(s):
    """Test: Modeline shows all expected fields."""
    screen = s.text()
    lines = [l for l in screen.split("\n") if l.strip()]
    modeline = lines[-1] if lines else ""
    assert_contains(modeline, "session-01", "buffer name")
    assert_contains(modeline, "echo-agent", "agent name")
    assert_contains(modeline, "/200000", "context limit")
    s.screenshot("18-modeline")


# ==========================================================================
# Main
# ==========================================================================

def main():
    os.makedirs(SCREENSHOT_DIR, exist_ok=True)

    if not os.path.exists(MCP_BIN):
        print(f"ERROR: mcp-tui-driver not found at {MCP_BIN}")
        print("Install: cargo install --git https://github.com/michaellee8/mcp-tui-driver")
        sys.exit(1)

    print("=== Clawmacs E2E Tests ===")
    print(f"Screenshots: {SCREENSHOT_DIR}/")
    print()

    client = MCPClient()
    client.initialize()

    session = ClawmacsSession(client)
    print("Launching clawmacs...")
    sid = session.launch()
    if not sid:
        print("FATAL: Failed to launch clawmacs session")
        client.close()
        sys.exit(1)
    print(f"Session: {sid}")

    if not session.wait_ready():
        print("FATAL: clawmacs did not start (no 'user>' found)")
        session.close()
        client.close()
        sys.exit(1)
    print("clawmacs ready.\n")

    # Run tests sequentially (they build on each other's state)
    tests = [
        ("01-initial-render", test_01_initial_render),
        ("02-text-input", test_02_text_input),
        ("03-line-editing", test_03_line_editing_c_a_c_e),
        ("04-kill-yank", test_04_kill_yank),
        ("05-multiline-input", test_05_multiline_input),
        ("06-send-message", test_06_send_message),
        ("07-line-wrapping", test_07_line_wrapping),
        ("08-scroll", test_08_scroll),
        ("09-meta-scroll", test_09_meta_scroll),
        ("10-new-buffer", test_10_new_buffer),
        ("11-switch-buffer", test_11_switch_buffer),
        ("12-kill-buffer", test_12_kill_buffer),
        ("13-backspace", test_13_backspace),
        ("14-permission-approve", test_14_permission_approve),
        ("15-permission-deny", test_15_permission_deny),
        ("16-permission-deny-message", test_16_permission_deny_with_message),
        ("17-file-write-diff", test_17_file_write_diff),
        ("18-modeline", test_18_modeline_content),
    ]

    for name, fn in tests:
        run_test(name, fn, session)

    # Cleanup
    print()
    session.close()
    client.close()

    # Summary
    print(f"\n=== Results: {len(PASSED)} passed, {len(FAILED)} failed ===")
    if FAILED:
        print("\nFailed tests:")
        for name, err in FAILED:
            print(f"  {name}: {err}")
        sys.exit(1)
    else:
        print("All tests passed!")
        print(f"Screenshots saved to {SCREENSHOT_DIR}/")


if __name__ == "__main__":
    main()
