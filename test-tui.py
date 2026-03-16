#!/usr/bin/env python3
"""Drive mcp-tui-driver to test clawmacs TUI."""
import subprocess
import json
import sys
import base64
import time
import os

MCP_BIN = os.path.expanduser("~/.local/bin/mcp-tui-driver")
CLAWMACS_DIR = os.path.dirname(os.path.abspath(__file__))
SSL_LIB = "/gnu/store/mb4yqk21zfvbkdy58ry1wi032mm9lsh5-openssl-3.0.8/lib"

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
        import select
        self.req_id += 1
        req_id = self.req_id
        msg = {"jsonrpc": "2.0", "id": req_id, "method": method}
        if params:
            msg["params"] = params
        line = json.dumps(msg) + "\n"
        self.proc.stdin.write(line)
        self.proc.stdin.flush()
        # Read responses, skipping notifications, until we get our response
        deadline = time.time() + timeout
        while time.time() < deadline:
            remaining = deadline - time.time()
            ready, _, _ = select.select([self.proc.stdout], [], [], min(remaining, 1.0))
            if ready:
                resp = self.proc.stdout.readline()
                if resp:
                    parsed = json.loads(resp)
                    # Skip notifications (no id) or responses for other requests
                    if parsed.get("id") == req_id:
                        return parsed
                    # else: skip notification or old response
        print(f"  [timeout waiting for response to {method} id={req_id}]", file=sys.stderr)
        return None

    def call_tool(self, name, args=None, timeout=30):
        return self.send("tools/call", {"name": name, "arguments": args or {}}, timeout=timeout)

    def close(self):
        self.proc.stdin.close()
        self.proc.wait(timeout=5)


def main():
    client = MCPClient()

    # Initialize
    r = client.send("initialize", {
        "protocolVersion": "2024-11-05",
        "capabilities": {},
        "clientInfo": {"name": "test", "version": "0.1"},
    })
    print(f"Initialized: {r['result']['serverInfo']['name']}")

    # Send initialized notification (required by MCP protocol)
    notify = json.dumps({"jsonrpc": "2.0", "method": "notifications/initialized"}) + "\n"
    client.proc.stdin.write(notify)
    client.proc.stdin.flush()
    time.sleep(0.2)

    # Launch clawmacs
    ql_setup = os.path.expanduser("~/quicklisp/setup.lisp")
    cmd = (
        f"LD_LIBRARY_PATH={SSL_LIB} "
        f"sbcl --noinform "
        f"--load {ql_setup} "
        f"--eval '(push (truename \"{CLAWMACS_DIR}/\") asdf:*central-registry*)' "
        f"--eval '(asdf:load-system :clawmacs)' "
        f"--eval '(clawmacs:clawmacs-main)'"
    )
    r = client.call_tool("tui_launch", {
        "command": "bash",
        "args": ["-c", cmd],
        "cols": 120,
        "rows": 35,
    }, timeout=60)
    # Extract session_id from response
    if not r:
        print("Failed to launch - no response")
        client.close()
        return
    content = r.get("result", {}).get("content", [])
    text_content = next((c["text"] for c in content if c.get("type") == "text"), "{}")
    session_id = json.loads(text_content).get("session_id")
    print(f"Session: {session_id}")

    if not session_id:
        print(f"Failed to get session ID. Response: {json.dumps(r, indent=2)[:500]}")
        client.close()
        return

    # Wait for startup
    print("Waiting for clawmacs to load...")
    r = client.call_tool("tui_wait_for_text", {
        "session_id": session_id,
        "text": "user>",
        "timeout_ms": 15000,
    })
    found = json.loads(next(c["text"] for c in r["result"]["content"] if c.get("type") == "text"))
    print(f"Found 'user>': {found.get('found', False)}")

    # Take screenshot
    r = client.call_tool("tui_screenshot", {"session_id": session_id})
    if r:
        for c in r.get("result", {}).get("content", []):
            if c.get("type") == "image":
                data = base64.b64decode(c["data"])
                outpath = os.path.join(CLAWMACS_DIR, "screenshot-initial.png")
                with open(outpath, "wb") as f:
                    f.write(data)
                print(f"Screenshot saved: {outpath} ({len(data)} bytes)")
                break
            elif c.get("type") == "text":
                # Maybe screenshot data is in a text response
                try:
                    d = json.loads(c["text"])
                    if "data" in d:
                        data = base64.b64decode(d["data"])
                        outpath = os.path.join(CLAWMACS_DIR, "screenshot-initial.png")
                        with open(outpath, "wb") as f:
                            f.write(data)
                        print(f"Screenshot saved: {outpath} ({len(data)} bytes)")
                        break
                except: pass
        else:
            print(f"Screenshot response: {json.dumps(r)[:300]}")

    # Type a message
    client.call_tool("tui_send_text", {
        "session_id": session_id,
        "text": "Hello from the TUI test!",
    })
    time.sleep(0.5)

    # Take screenshot with text
    r = client.call_tool("tui_screenshot", {"session_id": session_id})
    if r:
        for c in r.get("result", {}).get("content", []):
            if c.get("type") == "image":
                data = base64.b64decode(c["data"])
                outpath = os.path.join(CLAWMACS_DIR, "screenshot-typed.png")
                with open(outpath, "wb") as f:
                    f.write(data)
                print(f"Screenshot saved: {outpath} ({len(data)} bytes)")
                break

    # Get text content
    r = client.call_tool("tui_text", {"session_id": session_id})
    text = json.loads(next(c["text"] for c in r["result"]["content"] if c.get("type") == "text"))
    print(f"Screen text (first 200 chars):\n{text.get('text', '')[:200]}")

    # Close
    client.call_tool("tui_press_key", {
        "session_id": session_id,
        "key": "Ctrl+c",
    })
    time.sleep(0.5)
    client.call_tool("tui_close", {"session_id": session_id})
    client.close()
    print("Done!")


if __name__ == "__main__":
    main()
