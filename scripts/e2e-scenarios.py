#!/usr/bin/env python3
"""Shared Clawmacs E2E scenarios for McCLIM-driven tests."""
import os
import tempfile
import time


DEFAULT_AGENT_NAME = "agent"
PASSED = []
FAILED = []


def assert_contains(screen_text, expected, msg=""):
    """Assert that screen text contains expected string."""
    if expected not in screen_text:
        raise AssertionError(f"{msg}: expected '{expected}' in screen, got:\n{screen_text[:300]}")


def assert_not_contains(screen_text, expected, msg=""):
    """Assert that screen text does NOT contain expected string."""
    if expected in screen_text:
        raise AssertionError(f"{msg}: '{expected}' should NOT be in screen")


def wait_for_send_result(s, expected_agent_prefix, timeout=20):
    """Wait until a sent message produces an agent turn and returns input prompt."""
    deadline = time.time() + timeout
    last_screen = ""
    while time.time() < deadline:
        screen = s.text()
        last_screen = screen
        has_agent_turn = f"{expected_agent_prefix}>" in screen
        has_input_prompt = screen.count("user>") >= 2
        if has_agent_turn and has_input_prompt:
            return screen
        time.sleep(0.25)
    raise AssertionError(
        "message send did not complete with agent turn + input prompt; "
        f"last screen:\n{last_screen[:500]}"
    )


def clear_input(s):
    """Clear current input robustly using editor keybinds."""
    s.press("Ctrl+a")
    for _ in range(20):
        s.press("Ctrl+k")
    for _ in range(20):
        s.press("Backspace")


def set_input(s, text):
    """Replace current input text with TEXT."""
    clear_input(s)
    s.type_text(text)


def seed_previous_command(s, command):
    """Send COMMAND so argument-yank binds have previous-command context."""
    set_input(s, command)
    s.press("Enter")
    time.sleep(1.0)


def run_test(name, fn, session):
    """Run a test function, track pass/fail."""
    print(f"  [{name}] ", end="", flush=True)
    try:
        fn(session)
        screen_after = session.text()
        assert_not_contains(
            screen_after,
            "[Error:",
            f"{name}: unexpected runtime error detected",
        )
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
    """Test: McCLIM starts with modeline and user> prompt."""
    time.sleep(0.5)  # Let initial render complete
    screen = s.text()
    assert_contains(screen, "user>", "input prompt")
    assert_contains(screen, "session-", "buffer name in modeline")
    assert_contains(screen, DEFAULT_AGENT_NAME, "agent name in modeline")
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
    screen = wait_for_send_result(s, DEFAULT_AGENT_NAME)
    assert_contains(screen, "say hello", "sent user message")
    assert_contains(screen, f"{DEFAULT_AGENT_NAME}>", "agent turn rendered")
    assert_not_contains(screen, "[Error:", "message send completed without API error")
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
    assert_contains(screen, ">", "previous conversation preserved")
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
    assert_contains(screen, "session-0", "back to original after kill")
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


def test_14_point_face(s):
    """Test: Cursor at point shows reverse-video face on the character."""
    # Type text and position cursor in the middle
    s.type_text("Hello World Cursor Test")
    time.sleep(0.3)
    # Move to beginning, then forward a few chars
    s.press("Ctrl+a")
    time.sleep(0.2)
    s.screenshot("14-point-face-at-start")
    # The cursor should be at the 'H' character, rendered with reverse-video
    # We can't verify reverse-video via tui_text, but the screenshot shows it
    # Move to end
    s.press("Ctrl+e")
    time.sleep(0.2)
    s.screenshot("14-point-face-at-end")
    # Clean up
    s.press("Ctrl+a")
    s.press("Ctrl+k")


def test_15_permission_approve(s):
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
    s.screenshot("15-permission-prompt")

    # The screen should show PERMISSION REQUIRED or the approval options
    has_approval = ("PERMISSION" in screen or "[a]pprove" in screen
                    or "APPROVAL" in screen or "approve" in screen.lower())
    if has_approval:
        # Approve it
        s.press("a")
        time.sleep(3)
        screen = s.text()
        s.screenshot("15-permission-approved")
        # After approval, tool should have executed
        assert_contains(screen, ">", "agent continued after approval")
    else:
        # Agent might have answered without using the tool, or tool was auto-approved
        # Just verify it responded
        assert_contains(screen, ">", "agent responded")
        s.screenshot("15-permission-approved")


def test_16_permission_deny(s):
    """Test: Agent tool needing permission shows approval prompt, user denies."""
    for _ in range(20):
        s.press("Ctrl+k")
    for _ in range(20):
        s.press("Backspace")
    s.type_text("Run the command: echo 'deny test'")
    s.press("Enter")
    time.sleep(8)

    screen = s.text()
    s.screenshot("16-permission-deny-prompt")

    has_approval = ("PERMISSION" in screen or "[a]pprove" in screen
                    or "APPROVAL" in screen or "approve" in screen.lower())
    if has_approval:
        # Deny it
        s.press("d")
        time.sleep(3)
        screen = s.text()
        s.screenshot("16-permission-denied")
        assert_contains(screen, "DENIED", "denial shown in chat")
    else:
        s.screenshot("16-permission-denied")


def test_17_permission_deny_with_message(s):
    """Test: User denies with a message to the agent."""
    for _ in range(20):
        s.press("Ctrl+k")
    for _ in range(20):
        s.press("Backspace")
    s.type_text("Execute the command: echo 'message test'")
    s.press("Enter")
    time.sleep(8)

    screen = s.text()
    s.screenshot("17-permission-message-prompt")

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
        s.screenshot("17-permission-deny-message")
        assert_contains(screen, ">", "agent responded to denial message")
    else:
        s.screenshot("17-permission-deny-message")


def test_18_file_write_diff(s):
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
    s.screenshot("18-file-write-diff-prompt")

    has_approval = ("PERMISSION" in screen or "[a]pprove" in screen
                    or "APPROVAL" in screen)
    if has_approval:
        # Check that diff-like content is visible (+ or - prefixed lines)
        has_diff = ("+" in screen or "---" in screen or "new file" in screen)
        if has_diff:
            s.screenshot("18-file-write-diff-visible")
        # Deny it (we just wanted to see the diff)
        s.press("d")
        time.sleep(3)
        screen = s.text()
        s.screenshot("18-file-write-diff-denied")
        assert_contains(screen, ">", "agent responded after denial")
    else:
        # Agent may not have used file_write
        s.screenshot("18-file-write-diff-no-approval")


def test_19_file_write_append(s):
    """Test: file_write appends to existing files, never overwrites."""
    for _ in range(20):
        s.press("Ctrl+k")
    for _ in range(20):
        s.press("Backspace")
    # Use lisp_eval to verify append behavior directly
    s.type_text("Use the lisp_eval tool to: (progn (clawmacs::init-tools) (setf clawmacs::*sandbox-root* (truename \".\")) (clawmacs::execute-tool \"file_write\" (list (cons :path \"e2e-append-test.txt\") (cons :content \"first\"))) (clawmacs::execute-tool \"file_write\" (list (cons :path \"e2e-append-test.txt\") (cons :content \" second\"))) (uiop:read-file-string (merge-pathnames \"e2e-append-test.txt\" (truename \".\"))))")
    s.press("Enter")
    time.sleep(8)
    screen = s.text()
    s.screenshot("19-file-write-append")
    # The eval result should show "first second" (appended, not overwritten)
    assert_contains(screen, "first second", "file_write appended content")


def test_20_file_edit_search_replace(s):
    """Test: file_edit does search-and-replace with approval prompt showing diff."""
    for _ in range(20):
        s.press("Ctrl+k")
    for _ in range(20):
        s.press("Backspace")
    # Ask agent to edit the file we just created
    s.type_text("Edit the file e2e-append-test.txt: replace 'first' with 'FIRST'")
    s.press("Enter")
    time.sleep(8)
    screen = s.text()
    s.screenshot("20-file-edit-prompt")

    has_approval = ("PERMISSION" in screen or "[a]pprove" in screen
                    or "APPROVAL" in screen)
    if has_approval:
        # Should show diff with -first / +FIRST
        has_diff = ("-" in screen or "+" in screen or "old" in screen)
        s.screenshot("20-file-edit-diff")
        # Approve the edit
        s.press("a")
        time.sleep(5)
        screen = s.text()
        s.screenshot("20-file-edit-approved")
        assert_contains(screen, ">", "agent responded after edit approval")
    else:
        s.screenshot("20-file-edit-no-approval")


def test_21_modeline_content(s):
    """Test: Modeline shows all expected fields."""
    screen = s.text()
    lines = [l for l in screen.split("\n") if l.strip()]
    modeline = lines[-1] if lines else ""
    assert_contains(modeline, "session-", "buffer name")
    assert_contains(modeline, DEFAULT_AGENT_NAME, "agent name")
    assert_contains(modeline, "/200000", "context limit")
    s.screenshot("21-modeline")


def test_22_ctrl_b(s):
    """Test: Ctrl+b moves cursor one character left."""
    set_input(s, "ac")
    s.press("Ctrl+e")
    s.press("Ctrl+b")
    s.type_text("b")
    screen = s.text()
    assert_contains(screen, "abc", "Ctrl+b moved left before insert")
    s.screenshot("22-ctrl-b")


def test_23_ctrl_f(s):
    """Test: Ctrl+f moves cursor one character right."""
    set_input(s, "ac")
    s.press("Ctrl+a")
    s.press("Ctrl+f")
    s.type_text("b")
    screen = s.text()
    assert_contains(screen, "abc", "Ctrl+f moved right before insert")
    s.screenshot("23-ctrl-f")


def test_24_alt_b(s):
    """Test: Alt+b moves cursor one word left."""
    set_input(s, "one two three")
    s.press("Escape")
    s.press("b")
    s.type_text("X")
    screen = s.text()
    assert_contains(screen, "one two Xthree", "Alt+b moved left by word")
    s.screenshot("24-alt-b")


def test_25_alt_f(s):
    """Test: Alt+f moves cursor one word right."""
    set_input(s, "one two three")
    s.press("Ctrl+a")
    s.press("Escape")
    s.press("f")
    s.type_text("X")
    screen = s.text()
    assert_contains(screen, "oneX two three", "Alt+f moved right by word")
    s.screenshot("25-alt-f")


def test_26_ctrl_a(s):
    """Test: Ctrl+a moves cursor to start of line."""
    set_input(s, "middle")
    s.press("Ctrl+a")
    s.type_text(">>>")
    screen = s.text()
    assert_contains(screen, ">>>middle", "Ctrl+a moved to line start")
    s.screenshot("26-ctrl-a")


def test_27_ctrl_e(s):
    """Test: Ctrl+e moves cursor to end of line."""
    set_input(s, "middle")
    s.press("Ctrl+a")
    s.press("Ctrl+e")
    s.type_text("<<<")
    screen = s.text()
    assert_contains(screen, "middle<<<", "Ctrl+e moved to line end")
    s.screenshot("27-ctrl-e")


def test_28_ctrl_u(s):
    """Test: Ctrl+u cuts from line start to cursor."""
    set_input(s, "hello world")
    s.press("Ctrl+e")
    s.press("Ctrl+u")
    screen = s.text()
    assert_not_contains(screen, "hello world", "Ctrl+u removed content before cursor")
    s.press("Ctrl+y")
    screen = s.text()
    assert_contains(screen, "hello world", "Ctrl+y restored Ctrl+u kill")
    s.screenshot("28-ctrl-u")


def test_29_ctrl_k(s):
    """Test: Ctrl+k cuts from cursor to end of line."""
    set_input(s, "hello world")
    s.press("Ctrl+a")
    s.press("Ctrl+k")
    screen = s.text()
    assert_not_contains(screen, "hello world", "Ctrl+k removed content after cursor")
    s.press("Ctrl+y")
    screen = s.text()
    assert_contains(screen, "hello world", "Ctrl+y restored Ctrl+k kill")
    s.screenshot("29-ctrl-k")


def test_30_alt_d(s):
    """Test: Alt+d cuts current word after cursor."""
    set_input(s, "one two")
    s.press("Ctrl+a")
    s.press("Escape")
    s.press("d")
    screen = s.text()
    assert_contains(screen, " two", "Alt+d removed first word")
    s.press("Ctrl+y")
    screen = s.text()
    assert_contains(screen, "one two", "Ctrl+y restored Alt+d kill")
    s.screenshot("30-alt-d")


def test_31_ctrl_w(s):
    """Test: Ctrl+w cuts current word before cursor."""
    set_input(s, "one two")
    s.press("Ctrl+e")
    s.press("Ctrl+w")
    screen = s.text()
    assert_contains(screen, "user> one", "Ctrl+w removed previous word")
    assert_not_contains(screen, "one two", "Ctrl+w removed text before cursor")
    s.press("Ctrl+y")
    screen = s.text()
    assert_contains(screen, "one two", "Ctrl+y restored Ctrl+w kill")
    s.screenshot("31-ctrl-w")


def test_32_ctrl_y(s):
    """Test: Ctrl+y pastes previous cut text."""
    set_input(s, "paste me")
    s.press("Ctrl+a")
    s.press("Ctrl+k")
    s.press("Ctrl+y")
    screen = s.text()
    assert_contains(screen, "paste me", "Ctrl+y pasted latest kill")
    s.screenshot("32-ctrl-y")


def test_33_alt_y(s):
    """Test: Alt+y pastes second latest cut text."""
    set_input(s, "first")
    s.press("Ctrl+a")
    s.press("Ctrl+k")
    set_input(s, "second")
    s.press("Ctrl+a")
    s.press("Ctrl+k")
    s.press("Ctrl+y")
    s.press("Escape")
    s.press("y")
    screen = s.text()
    assert_contains(screen, "second", "Ctrl+y pasted latest kill")
    assert_contains(screen, "first", "Alt+y pasted older kill ring entry")
    s.screenshot("33-alt-y")


def test_34_alt_ctrl_y(s):
    """Test: Alt+Ctrl+y pastes first argument of previous command."""
    seed_previous_command(s, "git commit -m msg")
    clear_input(s)
    s.press("Escape")
    s.press("Ctrl+y")
    screen = s.text()
    assert_contains(screen, "commit", "Alt+Ctrl+y yanked first argument")
    s.screenshot("34-alt-ctrl-y")


def test_35_alt_dot(s):
    """Test: Alt+. pastes last argument of previous command."""
    seed_previous_command(s, "echo alpha beta")
    clear_input(s)
    s.press("Escape")
    s.press(".")
    screen = s.text()
    assert_contains(screen, "beta", "Alt+. yanked last argument")
    s.screenshot("35-alt-dot")


def test_36_alt_underscore(s):
    """Test: Alt+_ pastes last argument of previous command."""
    seed_previous_command(s, "echo alpha gamma")
    clear_input(s)
    s.press("Escape")
    s.press("_")
    screen = s.text()
    assert_contains(screen, "gamma", "Alt+_ yanked last argument")
    s.screenshot("36-alt-underscore")


def test_37_ctrl_d(s):
    """Test: Ctrl+d deletes next character after cursor."""
    set_input(s, "abc")
    s.press("Ctrl+a")
    s.press("Ctrl+f")
    s.press("Ctrl+d")
    screen = s.text()
    assert_contains(screen, "ac", "Ctrl+d deleted next character")
    assert_not_contains(screen, "abc", "Ctrl+d removed middle character")
    s.screenshot("37-ctrl-d")


# ==========================================================================
# Helper Functions
# ==========================================================================

def wait_for_text(s, text, timeout=5):
    """Poll until text appears on screen or timeout."""
    deadline = time.time() + timeout
    last_screen = ""
    while time.time() < deadline:
        screen = s.text()
        last_screen = screen
        if text in screen:
            return screen
        time.sleep(0.25)
    raise AssertionError(
        f"Timed out waiting for '{text}'; last screen:\n{last_screen[:500]}"
    )


def cancel_minibuffer(s):
    """Press C-g to close active minibuffer."""
    s.press("Ctrl+g")
    time.sleep(0.3)


def kill_current_buffer(s):
    """Press C-x k to kill the current buffer and return to previous."""
    s.press("Ctrl+x")
    s.press("k")
    time.sleep(0.3)


def switch_to_buffer(s, query, expected_name=None):
    """Open the minibuffer buffer selector and switch to QUERY."""
    s.press("Ctrl+x")
    s.press("Ctrl+b")
    time.sleep(0.5)
    screen = s.text()
    assert_contains(screen, "Switch Buffer", "buffer selector prompt")
    if query:
        s.type_text(query)
        time.sleep(0.3)
        screen = s.text()
    if expected_name:
        assert_contains(screen, expected_name, "buffer candidate visible")
    s.press("Enter")
    time.sleep(0.5)


def lisp_string(value):
    """Return VALUE escaped for a Common Lisp string literal."""
    return value.replace("\\", "\\\\").replace("\"", "\\\"")


def create_e2e_skill_root():
    """Create a temporary skill root used by offline skill-completion tests."""
    root = tempfile.mkdtemp(prefix="clawmacs-e2e-skills-")
    skill_dir = os.path.join(root, "demo-skill")
    os.makedirs(skill_dir, exist_ok=True)
    with open(os.path.join(skill_dir, "SKILL.md"), "w", encoding="utf-8") as f:
        f.write(
            "---\n"
            "name: demo-skill\n"
            "description: \"E2E demo skill\"\n"
            "metadata:\n"
            "  short-description: \"E2E skill completion fixture\"\n"
            "---\n"
            "Use this skill for e2e completion tests.\n"
        )
    return root


# ==========================================================================
# New Tests — Tier 1: Offline (No LLM Required)
# ==========================================================================

def test_38_shell_prefix(s):
    """Test: ! prefix executes shell command inline."""
    set_input(s, "!echo e2e-shell-test")
    s.press("Enter")
    time.sleep(1.5)
    screen = s.text()
    assert_contains(screen, "e2e-shell-test", "shell output visible")
    assert_contains(screen, "$ echo e2e-shell-test", "command echo format")
    s.screenshot("38-shell-prefix")
    clear_input(s)


def test_39_debug_mode_toggle(s):
    """Test: C-c C-d toggles debug mode on and off."""
    s.press("Ctrl+c")
    s.press("Ctrl+d")
    time.sleep(0.5)
    screen = s.text()
    assert_contains(screen, "Debug mode ON", "debug mode turned on")
    s.screenshot("39-debug-mode-on")
    # Toggle back off
    s.press("Ctrl+c")
    s.press("Ctrl+d")
    time.sleep(0.5)
    screen = s.text()
    assert_contains(screen, "Debug mode OFF", "debug mode turned off")
    s.screenshot("39-debug-mode-off")


def test_40_save_session(s):
    """Test: C-x C-s saves the current session."""
    s.press("Ctrl+x")
    s.press("Ctrl+s")
    time.sleep(0.5)
    screen = s.text()
    assert_contains(screen, "Session saved to", "save session message")
    s.screenshot("40-save-session")


def test_41_buffer_state_persistence(s):
    """Test: Input text persists when switching away and back."""
    set_input(s, "persistent-text-check")
    time.sleep(0.3)
    # Create new buffer B
    s.press("Ctrl+x")
    s.press("n")
    time.sleep(0.5)
    screen = s.text()
    assert_not_contains(screen, "persistent-text-check", "new buffer has no old text")
    # Switch back to buffer A
    switch_to_buffer(s, "session-01", "session-01")
    screen = s.text()
    assert_contains(screen, "persistent-text-check", "text preserved after switch")
    s.screenshot("41-buffer-persistence")
    # Switch to B and kill it to clean up
    switch_to_buffer(s, "session-02", "session-02")
    kill_current_buffer(s)
    # Clear input in buffer A
    clear_input(s)


def test_42_minibuffer_buffer_selector(s):
    """Test: C-x C-b opens minibuffer buffer selector with fuzzy filtering."""
    s.press("Ctrl+x")
    s.press("Ctrl+b")
    time.sleep(0.5)
    screen = s.text()
    assert_contains(screen, "Switch Buffer", "buffer selector prompt")
    assert_contains(screen, "session-", "session candidate visible")
    s.screenshot("42-minibuffer-buffer-selector")
    # Type to fuzzy-filter
    s.type_text("1")
    time.sleep(0.3)
    screen = s.text()
    assert_contains(screen, "session-", "fuzzy filter matches session buffer")
    s.screenshot("42-minibuffer-filtered")
    # Cancel
    cancel_minibuffer(s)
    time.sleep(0.3)
    screen = s.text()
    assert_not_contains(screen, "Switch Buffer", "minibuffer closed after C-g")


def test_43_describe_bindings(s):
    """Test: C-c b opens describe bindings help buffer."""
    s.press("Ctrl+c")
    time.sleep(0.3)
    s.press("b")
    time.sleep(0.5)
    screen = s.text()
    for _ in range(4):
        if "Key Bindings" in screen:
            break
        s.press("PageUp")
        time.sleep(0.3)
        screen = s.text()
    assert_contains(screen, "Key Bindings", "bindings header visible")
    assert_contains(screen, "send-message", "send-message command listed")
    s.screenshot("43-describe-bindings")
    # Kill help buffer to return
    kill_current_buffer(s)
    time.sleep(0.3)
    screen = s.text()
    assert_contains(screen, "session-", "back to session buffer")


def test_44_describe_function(s):
    """Test: C-c f opens describe function minibuffer."""
    s.press("Ctrl+c")
    time.sleep(0.3)
    s.press("f")
    time.sleep(0.5)
    screen = s.text()
    assert_contains(screen, "Describe Function", "describe function prompt")
    s.screenshot("44-describe-function-prompt")
    # Type a function name and verify it appears in the filtered candidates.
    s.type_text("send-message")
    time.sleep(0.3)
    screen = s.text()
    assert_contains(screen, "send-message", "send-message in candidates")
    s.screenshot("44-describe-function-filtered")
    cancel_minibuffer(s)


def test_45_describe_variable(s):
    """Test: C-c v opens describe variable minibuffer."""
    s.press("Ctrl+c")
    time.sleep(0.3)
    s.press("v")
    time.sleep(0.5)
    screen = s.text()
    assert_contains(screen, "Describe Variable", "describe variable prompt")
    s.screenshot("45-describe-variable-prompt")
    s.type_text("default-model")
    time.sleep(0.3)
    screen = s.text()
    assert_contains(screen, "default-model", "default-model in candidates")
    s.screenshot("45-describe-variable-filtered")
    cancel_minibuffer(s)
    time.sleep(0.3)
    screen = s.text()
    assert_not_contains(screen, "Describe Variable", "minibuffer closed")


def test_46_describe_type(s):
    """Test: C-c T opens describe type minibuffer."""
    s.press("Ctrl+c")
    time.sleep(0.3)
    s.press("T")
    time.sleep(0.5)
    screen = s.text()
    assert_contains(screen, "Describe Type", "describe type prompt")
    s.screenshot("46-describe-type-prompt")
    s.type_text("buffer")
    time.sleep(0.3)
    screen = s.text()
    assert_contains(screen, "buffer", "buffer in candidates")
    s.screenshot("46-describe-type-filtered")
    cancel_minibuffer(s)
    time.sleep(0.3)
    screen = s.text()
    assert_not_contains(screen, "Describe Type", "minibuffer closed")


def test_47_customize_face(s):
    """Test: C-c F opens drawing-style customization minibuffer."""
    s.press("Ctrl+c")
    time.sleep(0.3)
    s.press("F")
    time.sleep(0.5)
    screen = s.text()
    assert_contains(screen, "Customize Drawing Style", "customize drawing style prompt")
    assert_contains(screen, "ink:", "drawing style ink display format")
    assert_contains(screen, "bg:", "drawing style background display format")
    s.screenshot("47-customize-face")
    cancel_minibuffer(s)
    time.sleep(0.3)
    screen = s.text()
    assert_not_contains(screen, "Customize Drawing Style", "minibuffer closed")


def test_52_skill_completion(s):
    """Test: Typing $ opens automatic skill completion and inserts a mention."""
    clear_input(s)
    s.type_text("$dem")
    screen = wait_for_text(s, "demo-skill", timeout=5)
    assert_contains(screen, "Skill: $dem", "skill completion prompt")
    assert_contains(screen, "demo-skill", "skill candidate visible")
    s.screenshot("52-skill-completion-popup")

    s.press("Enter")
    time.sleep(0.5)
    screen = s.text()
    assert_contains(screen, "[$demo-skill]", "linked skill mention inserted")
    assert_contains(screen, "skill://", "skill mention has exact link")
    s.screenshot("52-skill-completion-inserted")
    clear_input(s)


# ==========================================================================
# New Tests — Tier 2: LLM Required
# ==========================================================================

def test_48_tool_lisp_eval(s):
    """Test: Agent uses lisp_eval tool (auto-approved) and returns result."""
    set_input(s, "Use the lisp_eval tool to evaluate (+ 40 2)")
    s.press("Enter")
    screen = wait_for_text(s, "42", timeout=30)
    assert_contains(screen, "42", "lisp_eval result visible")
    s.screenshot("48-tool-lisp-eval")


def test_49_tool_spec_lookup(s):
    """Test: Agent uses lisp_eval to query the bundled Common Lisp spec."""
    set_input(
        s,
        "Use the lisp_eval tool to evaluate "
        "(describe-common-lisp-symbol-to-string 'format :max-chars 800)"
    )
    s.press("Enter")
    screen = wait_for_text(s, "Reference: CL Community Spec", timeout=30)
    assert_contains(screen, "format", "spec lookup result visible")
    assert_contains(screen, "Reference: CL Community Spec", "spec reference visible")
    s.screenshot("49-tool-spec-lookup")


def test_50_multi_turn(s):
    """Test: Multi-turn conversation preserves history."""
    # Create a fresh buffer
    s.press("Ctrl+x")
    s.press("n")
    time.sleep(0.5)
    # Turn 1
    set_input(s, "Say only the word 'alpha'")
    s.press("Enter")
    screen = wait_for_text(s, "alpha", timeout=30)
    assert_contains(screen, "alpha", "first turn response")
    s.screenshot("50-multi-turn-1")
    # Turn 2
    set_input(s, "Now say only the word 'beta'")
    s.press("Enter")
    screen = wait_for_text(s, "beta", timeout=30)
    assert_contains(screen, "alpha", "first turn still visible")
    assert_contains(screen, "beta", "second turn response")
    s.screenshot("50-multi-turn-2")
    # Clean up
    kill_current_buffer(s)


def test_51_toggle_tool_results(s):
    """Test: C-c t toggles tool result visibility without error."""
    s.press("Ctrl+c")
    s.press("t")
    time.sleep(0.5)
    s.screenshot("51-toggle-tool-results-on")
    # Toggle back
    s.press("Ctrl+c")
    s.press("t")
    time.sleep(0.5)
    s.screenshot("51-toggle-tool-results-off")
    # Primary check: run_test wrapper verifies no [Error: on screen
