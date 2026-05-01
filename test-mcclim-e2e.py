#!/usr/bin/env python3
"""McCLIM E2E tests for Clawmacs.

The harness runs Clawmacs under an existing X display, drives the McCLIM UI with
xdotool, captures ImageMagick screenshots, and reads structured state from
scripts/mcclim-e2e-driver.lisp.
"""
import argparse
import importlib.util
import http.server
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
import threading
from functools import partial


CLAWMACS_DIR = os.path.dirname(os.path.abspath(__file__))
SCREENSHOT_DIR = os.path.join(CLAWMACS_DIR, "screenshots", "mcclim")
SESSION_NAME = "session-01"
E2E_PROJECT_NAME = "mcclim-e2e-project"
E2E_PROJECT_ROOT = None
E2E_PROJECT_FILE = None
PROMPT_PROJECT_ROOT = "/tmp/clawmacs-prompt-project-test"

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
          (labels ((finish (message)
                     (clawmacs::buffer-insert-agent-message buf message)
                     (setf (clawmacs::buffer-status buf) :idle)
                     (clawmacs:notify-buffer-display-change buf :offline-agent))
                   (ensure-speculum ()
                     (pushnew "speculum"
                              (clawmacs:buffer-enabled-packages buf)
                              :test #'string=)
                     (clawmacs:load-active-packages :buffer buf))
                   (speculum-tool-data (tool-name args)
                     (handler-case
                         (progn
                           (ensure-speculum)
                           (let ((clawmacs::*current-tool-buffer* buf)
                                 (clawmacs::*current-caller* :agent))
                             (nth-value 0
                               (clawmacs::lisp-data-read
                                (clawmacs:execute-tool tool-name args)))))
                       (error (condition)
                         (list :ok nil :error (format nil "~A" condition)))))
                   (package-enabled-p (name)
                     (member name
                             (clawmacs:buffer-enabled-packages buf)
                             :test #'string=))
                   (prompt-arg (key)
                     (let* ((marker (concatenate 'string key "="))
                            (start (search marker text :test #'char-equal)))
                       (when start
                         (let* ((begin (+ start (length marker)))
                                (quoted-p (and (< begin (length text))
                                               (char= (char text begin) #\"))))
                           (if quoted-p
                               (let* ((quoted-begin (1+ begin))
                                      (end (or (position #\" text
                                                         :start quoted-begin)
                                               (length text))))
                                 (subseq text quoted-begin end))
                               (let ((end (or (position-if
                                               (lambda (char)
                                                 (member char
                                                         '(#\Space #\Tab
                                                           #\Return #\Newline)
                                                         :test #'char=))
                                               text
                                               :start begin)
                                              (length text))))
                                 (subseq text begin end)))))))
                   (tool-result (tool-name args)
                     (handler-case
                         (let ((clawmacs::*current-tool-buffer* buf)
                               (clawmacs::*current-caller* :agent))
                           (clawmacs:execute-tool tool-name args))
                       (error (condition)
                         (format nil "(:ok nil :error ~S)"
                                 (format nil "~A" condition)))))
                   (ensure-e2e-project (project-name root-path)
                     (let ((root (uiop:ensure-directory-pathname root-path)))
                       (or (clawmacs:find-project project-name)
                           (clawmacs:define-project project-name
                                                    :root root
                                                    :description "McCLIM e2e fixture")))))
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
              ((search "stream-stop-probe" text :test #'char-equal)
               (let ((state (clawmacs::make-stream-state))
                     (agent-msg (clawmacs::buffer-insert-agent-message
                                 buf "" :record-p nil :run-hook-p nil)))
                 (bt:with-lock-held ((clawmacs::stream-state-lock state))
                   (setf (clawmacs::stream-state-text state)
                         "MCCLIM-STOP-PARTIAL"
                         (clawmacs::stream-state-content-blocks state)
                         (list (clawmacs::canonical-text-block
                                "MCCLIM-STOP-PARTIAL"))))
                 (setf (clawmacs::buffer-pending-stream buf) state
                       (clawmacs::buffer-streaming-message buf) agent-msg
                       (clawmacs::buffer-status buf) :thinking)
                 (clawmacs:notify-buffer-display-change buf :stream-started)))
              ((search "speculum-window-state-probe" text :test #'char-equal)
               (let ((state (speculum-tool-data
                             "speculum_window_state"
                             '(:scope "all" :message-limit 3))))
                 (finish
                  (if (and (getf state :ok)
                           (getf state :available)
                           (getf state :frame)
                           (getf state :panes)
                           (getf state :render))
                      "SPECULUM-WINDOW-STATE-OK"
                      (format nil "SPECULUM-WINDOW-STATE-FAIL ~S" state)))))
              ((search "speculum-screenshot-probe" text :test #'char-equal)
               (let* ((result (speculum-tool-data
                               "speculum_screenshot"
                               '(:refresh t)))
                      (status (if (getf result :ok) "OK" "FAIL")))
                 (finish
                  (format nil "SPECULUM-SCREENSHOT-~A path=~A bytes=~A"
                          status
                          (or (getf result :path) "")
                          (or (getf result :file-bytes) 0)))))
              ((search "speculum-package-probe" text :test #'char-equal)
               (if (package-enabled-p "speculum")
                   (let* ((window-state
                           (clawmacs::lisp-data-read
                            (tool-result "speculum_window_state"
                                         '(:scope "all" :message-limit 3))))
                          (screenshot
                           (clawmacs::lisp-data-read
                            (tool-result "speculum_screenshot"
                                         '(:refresh t)))))
                     (finish
                      (format nil "SPECULUM-PACKAGE-OK window=~A path=~A bytes=~A"
                              (if (and (getf window-state :ok)
                                       (getf window-state :available)
                                       (getf window-state :frame)
                                       (getf window-state :panes)
                                       (getf window-state :render))
                                  "window-state"
                                  "window-state-fail")
                              (or (getf screenshot :path) "")
                              (or (getf screenshot :file-bytes) 0))))
                   (finish "SPECULUM-PACKAGE-MISSING")))
              ((search "inline-image-probe" text :test #'char-equal)
               (finish
                "INLINE-IMAGE-RENDER-PROBE
![Inline red probe](screenshots/mcclim/inline-image-probe-source.png)"))
              ((search "lispi-package-probe" text :test #'char-equal)
               (if (package-enabled-p "lispi")
                   (let* ((path (or (prompt-arg "path")
                                    "/tmp/clawmacs-e2e-lispi.txt"))
                          (content (or (prompt-arg "content")
                                       "(defun lispi-probe () :ok)"))
                          (code (or (prompt-arg "code") "(+ 40 2)"))
                          (eval-result (tool-result "lisp_eval"
                                                    `(:code ,code)))
                          (write-result (tool-result "write"
                                                     `(:path ,path
                                                       :content ,content)))
                          (read-result (tool-result "read"
                                                    `(:path ,path))))
                     (finish
                      (format nil "LISPI-TOOLS-OK eval=~A write=~A read=~A"
                              eval-result write-result read-result)))
                   (finish "LISPI-PACKAGE-MISSING")))
              ((search "sexed-package-probe" text :test #'char-equal)
               (if (package-enabled-p "sexed")
                   (let* ((path (or (prompt-arg "path")
                                    "/tmp/clawmacs-e2e-sexed.lisp"))
                          (content (or (prompt-arg "content")
                                       "(defun sexed-probe (n) (+ n 1))"))
                          (replacement (or (prompt-arg "replacement")
                                           "(defun sexed-probe (n) (1+ n))"))
                          (outline (tool-result "sexed_text_outline"
                                                `(:text ,content
                                                  :head "defun"
                                                  :max-depth 1)))
                          (write-result (tool-result "sexed_file_write"
                                                     `(:path ,path
                                                       :content ,content)))
                          (read-result (tool-result "sexed_file_read"
                                                    `(:path ,path)))
                          (edit-result (tool-result "sexed_text_edits"
                                                    `(:text ,content
                                                      :edits (((:operation
                                                                . "replace")
                                                               (:selector
                                                                . ((:head
                                                                    . "+")))
                                                               (:newtext
                                                                . ,replacement)))))))
                     (tool-result "sexed_file_write"
                                  `(:path ,path :content ,replacement))
                     (finish
                      (format nil "SEXED-TOOLS-OK outline=~A write=~A read=~A edit=~A"
                              outline write-result read-result edit-result)))
                   (finish "SEXED-PACKAGE-MISSING")))
              ((search "slop-package-probe" text :test #'char-equal)
               (if (package-enabled-p "slop")
                   (let* ((project-name (or (prompt-arg "project") "e2e-tools"))
                         (root (or (prompt-arg "root") (truename ".")))
                         (symbol-a (or (prompt-arg "symbol-a")
                                       "buffer-insert-agent-message"))
                         (symbol-b (or (prompt-arg "symbol-b")
                                       "minibuffer-toggle-package-command"))
                          (query (or (prompt-arg "query") "package"))
                          (binding-path (or (prompt-arg "binding-path")
                                            "src/main.lisp"))
                          (binding-offset (and (prompt-arg "offset")
                                               (parse-integer
                                                (prompt-arg "offset"))))
                          (project (ensure-e2e-project project-name root))
                          (list-projects (tool-result "slop_list_projects" '()))
                          (current-project (tool-result "slop_current_project" '()))
                          (definitions (tool-result
                                        "slop_find_definitions_batch"
                                        `(:project ,project-name
                                          :symbols ,(vector symbol-a symbol-b)
                                          :namespace "function"
                                          :per-symbol-limit 1)))
                          (context (tool-result "slop_definition_context"
                                                `(:project ,project-name
                                                  :symbol ,symbol-a
                                                  :before-forms 1
                                                  :after-forms 1)))
                          (trace (tool-result "slop_trace_calls"
                                              `(:project ,project-name
                                                :symbol ,symbol-b
                                                :direction "callees"
                                                :max-depth 2)))
                          (mentions (tool-result "slop_find_mentions"
                                                 `(:project ,project-name
                                                   :query ,query
                                                   :path "docs"
                                                   :limit 3)))
                          (uses (clawmacs::lisp-data-read
                                 (tool-result "slop_find_variable_uses"
                                              `(:project ,project-name
                                                :path ,binding-path
                                                :offset ,binding-offset
                                                :limit 5))))
                          (binding (getf uses :binding))
                          (binding-id (or (getf binding :id) ""))
                          (rename (tool-result "slop_rename_variable"
                                               `(:project ,project-name
                                                 :binding-id ,binding-id
                                                 :new-name "renamed-total"))))
                     (finish
                      (format nil
                              "SLOP-TOOLS-OK projects=~A current=~A defs=~A context=~A trace=~A mentions=~A uses=~A rename=~A project-object=~A"
                              list-projects current-project definitions context trace mentions uses rename project)))
                   (finish "SLOP-PACKAGE-MISSING")))
              ((search "git-package-probe" text :test #'char-equal)
               (if (package-enabled-p "git")
                   (let* ((repository (prompt-arg "repository"))
                         (remote (or (prompt-arg "remote") "origin"))
                         (branch (or (prompt-arg "branch") "main"))
                          (add-path (or (prompt-arg "path") "CHANGELOG.md"))
                          (message (or (prompt-arg "message")
                                       "E2E git package commit"))
                          (status (tool-result "git_status"
                                               `(:repository ,repository
                                                 :branch t)))
                          (log (tool-result "git_log"
                                            `(:repository ,repository
                                              :limit 2)))
                          (add (tool-result "git_add"
                                            `(:repository ,repository
                                              :paths ,(vector add-path))))
                          (commit (tool-result "git_commit"
                                               `(:repository ,repository
                                                 :message ,message)))
                          (push (tool-result "git_push"
                                             `(:repository ,repository
                                               :remote ,remote
                                               :branch ,branch))))
                     (finish
                      (format nil "GIT-TOOLS-OK status=~A log=~A add=~A commit=~A push=~A"
                              status log add commit push)))
                   (finish "GIT-PACKAGE-MISSING")))
              ((search "netcons-package-probe" text :test #'char-equal)
               (if (package-enabled-p "netcons")
                   (let* ((url (prompt-arg "url"))
                          (open (tool-result "netcons_open"
                                             `(:url ,url :response-length "short")))
                          (find (tool-result "netcons_find"
                                             `(:url ,url
                                               :pattern "needle"
                                               :limit 3
                                               :response-length "short"))))
                     (finish
                      (format nil "NETCONS-TOOLS-OK open=~A find=~A"
                              open find)))
                   (finish "NETCONS-PACKAGE-MISSING")))
              (t
               (finish
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
                   (format nil "offline echo: ~A" text))))))
          buf))))
"""

CORE_OFFLINE_AGENT_EVAL = r"""
(setf (symbol-function 'clawmacs::send-to-agent-with-context)
      (lambda (buf)
        (let ((text (or (clawmacs::buffer-previous-user-command-text buf) "")))
          (labels ((finish (message)
                     (clawmacs::buffer-insert-agent-message buf message)
                     (setf (clawmacs::buffer-status buf) :idle)
                     (clawmacs:notify-buffer-display-change buf :offline-agent))
                   (ensure-speculum ()
                     (pushnew "speculum"
                              (clawmacs:buffer-enabled-packages buf)
                              :test #'string=)
                     (clawmacs:load-active-packages :buffer buf))
                   (speculum-tool-data (tool-name args)
                     (handler-case
                         (progn
                           (ensure-speculum)
                           (let ((clawmacs::*current-tool-buffer* buf)
                                 (clawmacs::*current-caller* :agent))
                             (nth-value 0
                               (clawmacs::lisp-data-read
                                (clawmacs:execute-tool tool-name args)))))
                       (error (condition)
                         (list :ok nil :error (format nil "~A" condition))))))
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
              ((search "stream-stop-probe" text :test #'char-equal)
               (let ((state (clawmacs::make-stream-state))
                     (agent-msg (clawmacs::buffer-insert-agent-message
                                 buf "" :record-p nil :run-hook-p nil)))
                 (bt:with-lock-held ((clawmacs::stream-state-lock state))
                   (setf (clawmacs::stream-state-text state)
                         "MCCLIM-STOP-PARTIAL"
                         (clawmacs::stream-state-content-blocks state)
                         (list (clawmacs::canonical-text-block
                                "MCCLIM-STOP-PARTIAL"))))
                 (setf (clawmacs::buffer-pending-stream buf) state
                       (clawmacs::buffer-streaming-message buf) agent-msg
                       (clawmacs::buffer-status buf) :thinking)
                 (clawmacs:notify-buffer-display-change buf :stream-started)))
              ((search "speculum-window-state-probe" text :test #'char-equal)
               (let ((state (speculum-tool-data
                             "speculum_window_state"
                             '(:scope "all" :message-limit 3))))
                 (finish
                  (if (and (getf state :ok)
                           (getf state :available)
                           (getf state :frame)
                           (getf state :panes)
                           (getf state :render))
                      "SPECULUM-WINDOW-STATE-OK"
                      (format nil "SPECULUM-WINDOW-STATE-FAIL ~S" state)))))
              ((search "speculum-screenshot-probe" text :test #'char-equal)
               (let* ((result (speculum-tool-data
                               "speculum_screenshot"
                               '(:refresh t)))
                      (status (if (getf result :ok) "OK" "FAIL")))
                 (finish
                  (format nil "SPECULUM-SCREENSHOT-~A path=~A bytes=~A"
                          status
                          (or (getf result :path) "")
                          (or (getf result :file-bytes) 0)))))
              ((search "inline-image-probe" text :test #'char-equal)
               (finish
                "INLINE-IMAGE-RENDER-PROBE
![Inline red probe](screenshots/mcclim/inline-image-probe-source.png)"))
              ((search "tiling-resize-probe" text :test #'char-equal)
               (finish "MCCLIM-TILING-RESIZE-VISIBLE"))
              (t
               (finish
                (format nil "offline echo: ~A" text)))))
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


def package_orchestration_extra_evals():
    return [
        '(clawmacs:register-agent-definition "writer" '
        ':provider :zai '
        ':model "glm-5" '
        ':think-level "low")',
        '(clawmacs:register-agent-definition "pair" '
        ':provider :openai-codex '
        ':model "gpt-5.4" '
        ':think-level "high")',
        '(setf (symbol-function \'clawmacs::available-models-for-selector)\n'
        '      (lambda (buffer)\n'
        '        (declare (ignore buffer))\n'
        '        (list (list :provider :zai\n'
        '                    :model "glm-5"\n'
        '                    :active-p t)\n'
        '              (list :provider :openai-codex\n'
        '                    :model "gpt-5.4"\n'
        '                    :active-p nil))))',
        '(setf (symbol-function \'clawmacs::provider-model-supported-think-levels)\n'
        '      (lambda (provider model)\n'
        '        (declare (ignore provider model))\n'
        '        \'("low" "high")))',
    ]


def create_e2e_project_root():
    root = tempfile.mkdtemp(
        prefix="clawmacs-e2e-project-",
        dir=os.path.join(CLAWMACS_DIR, ".cache"),
    )
    source_dir = os.path.join(root, "src")
    os.makedirs(source_dir, exist_ok=True)
    path = os.path.join(source_dir, "probe.lisp")
    with open(path, "w", encoding="utf-8") as stream:
        stream.write(
            "(defpackage :mcclim-e2e-project-probe (:use :cl))\n"
            "(in-package :mcclim-e2e-project-probe)\n\n"
            "(defun probe-target ()\n"
            "  :initial-state)\n\n"
            ";; E2E-PROJECT-BASELINE\n"
        )
    return root, path


def ensure_prompt_project_root():
    os.makedirs(PROMPT_PROJECT_ROOT, exist_ok=True)
    marker = os.path.join(PROMPT_PROJECT_ROOT, ".clawmacs-e2e-marker")
    if not os.path.exists(marker):
        with open(marker, "w", encoding="utf-8") as stream:
            stream.write("prompt project root fixture\n")


def project_fixture_eval(project_root):
    return (
        "(progn "
        f'(clawmacs::define-project "{lisp_string(E2E_PROJECT_NAME)}" '
        f':root #P"{lisp_string(project_root)}" '
        ':description "Offline McCLIM e2e fixture" '
        ':replace t)'
        ")"
    )


def package_config_fixture_eval():
    root = tempfile.mkdtemp(
        prefix="clawmacs-e2e-package-config-",
        dir=os.path.join(CLAWMACS_DIR, ".cache"),
    )
    path = os.path.join(root, "packages.json")
    return (
        "(progn "
        f'(setf clawmacs:*package-configuration-path* #P"{lisp_string(path)}") '
        "(setf clawmacs::*package-configuration* nil))"
    )


def core_offline_agent_eval():
    """Return the deterministic offline agent fixture used by e2e tests."""
    return OFFLINE_AGENT_EVAL


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


def non_user_message_text_containing(session, marker):
    snapshot = session.snapshot()
    messages = (snapshot.get("buffer") or {}).get("messages") or []
    for message in reversed(messages):
        sender = str(message.get("sender", "")).lower()
        text = str(message.get("text", ""))
        if sender != "user" and marker in text:
            return text
    return ""


def message_field(text, name):
    prefix = f"{name}="
    for token in text.split():
        if token.startswith(prefix):
            return token[len(prefix) :]
    return ""


def package_selector_scope_label(screen_text, package_name):
    screen_text = str(screen_text)
    marker = f"] {package_name} - "
    marker_index = screen_text.find(marker)
    if marker_index < 0:
        return ""
    start = screen_text.rfind("[", 0, marker_index)
    if start < 0:
        return ""
    return screen_text[start + 1 : marker_index]


def minibuffer_candidate_strings(snapshot):
    minibuffer = snapshot.get("minibuffer") or {}
    return [str(candidate) for candidate in minibuffer.get("candidates") or []]


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


def window_get(windows, camel_key, kebab_key, default=None):
    if camel_key in windows:
        return windows[camel_key]
    if kebab_key in windows:
        return windows[kebab_key]
    return default


def snapshot_get(data, camel_key, kebab_key, default=None):
    if camel_key in data:
        return data[camel_key]
    if kebab_key in data:
        return data[kebab_key]
    return default


def control_result(snapshot):
    return snapshot_get(snapshot, "controlResult", "control-result", {}) or {}


def control_sequence(snapshot):
    try:
        return int(control_result(snapshot).get("sequence") or 0)
    except (TypeError, ValueError):
        return 0


def assert_rendered_message_row_has_dark_pixels(png_path, snapshot, text):
    """Assert the physical screenshot has ink on TEXT's rendered row.

    The McCLIM control JSON is semantic state; this catches bugs where a
    message is listed as visible but the pane draw loop skipped its row.
    """
    try:
        from PIL import Image
    except Exception as exc:
        fail(f"Pillow is required for McCLIM visual assertions: {exc}")

    render = snapshot.get("render") or {}
    messages = render_visible_messages(snapshot)
    message_index = None
    for index, message in enumerate(messages):
        if text in str(message.get("text", "")):
            message_index = index
            break
    if message_index is None:
        fail(f"{text} was not present in render snapshot")

    rows = int(render_get(render, "rows", "rows", 0) or 0)
    history_height = int(render_get(render, "historyHeight", "history-height", 0) or 0)
    pixel_height = int(render_get(render, "pixelHeight", "pixel-height", 0) or 0)
    pixel_width = int(render_get(render, "pixelWidth", "pixel-width", 0) or 0)
    if rows <= 0 or history_height <= 0 or pixel_height <= 0 or pixel_width <= 0:
        fail("render snapshot did not include usable pane geometry")

    row = 1 + max(0, history_height - len(messages)) + message_index
    char_height = max(1, pixel_height // rows)
    y0 = row * char_height
    y1 = y0 + char_height

    image = Image.open(png_path).convert("RGB")
    if y0 >= image.height:
        fail(f"render row {row} was outside screenshot bounds")
    y1 = min(y1, image.height)
    x1 = min(max(1, pixel_width), image.width)
    dark_pixels = 0
    for y in range(y0, y1):
        for x in range(0, x1):
            red, green, blue = image.getpixel((x, y))
            if red < 80 and green < 80 and blue < 80:
                dark_pixels += 1

    if dark_pixels < 200:
        fail(
            f"{text} was in render JSON but its screenshot row had only "
            f"{dark_pixels} dark pixels"
        )


def make_inline_image_probe_source():
    try:
        from PIL import Image, ImageDraw
    except Exception as exc:
        fail(f"Pillow is required for McCLIM image e2e tests: {exc}")

    os.makedirs(SCREENSHOT_DIR, exist_ok=True)
    path = os.path.join(SCREENSHOT_DIR, "inline-image-probe-source.png")
    image = Image.new("RGB", (120, 64), (230, 20, 20))
    draw = ImageDraw.Draw(image)
    draw.rectangle((12, 12, 108, 52), fill=(20, 180, 50))
    draw.rectangle((30, 22, 90, 42), fill=(230, 20, 20))
    image.save(path)
    return path


def assert_screenshot_has_inline_probe_pixels(png_path):
    try:
        from PIL import Image
    except Exception as exc:
        fail(f"Pillow is required for McCLIM visual assertions: {exc}")

    image = Image.open(png_path).convert("RGB")
    red_pixels = 0
    green_pixels = 0
    for red, green, blue in image.getdata():
        if red > 180 and green < 80 and blue < 80:
            red_pixels += 1
        if green > 130 and red < 80 and blue < 100:
            green_pixels += 1
    if red_pixels < 500 or green_pixels < 500:
        fail(
            "inline image did not render visible probe colors: "
            f"red={red_pixels} green={green_pixels}"
        )


def wait_for_rendered_message_text(session, text, timeout=15):
    return wait_until(
        lambda: session._snapshot_if(lambda snap: render_contains_text(snap, text)),
        timeout=timeout,
        interval=0.1,
        description=f"rendered message containing {text}",
    )


def wait_for_minibuffer_prompt(session, prompt, timeout=10):
    return wait_until(
        lambda: session.text()
        if (
            (session.snapshot().get("minibuffer") or {}).get("active")
            and prompt in str((session.snapshot().get("minibuffer") or {}).get("prompt", ""))
        )
        else None,
        timeout=timeout,
        interval=0.1,
        description=f"minibuffer prompt containing {prompt}",
    )


def wait_for_text(session, text, timeout=5):
    return E2E.wait_for_text(session, text, timeout=timeout)


def current_buffer_message_text(session):
    snapshot = session.snapshot()
    messages = (snapshot.get("buffer") or {}).get("messages") or []
    return "\n".join(str(message.get("text", "")) for message in messages)


def wait_for_current_buffer_message_text(session, text, timeout=10):
    return wait_until(
        lambda: current_buffer_message_text(session)
        if text in current_buffer_message_text(session)
        else None,
        timeout=timeout,
        interval=0.1,
        description=f"current buffer message text containing {text}",
    )


def package_help_text(session, package_name):
    return session.eval_lisp(
        f'''(let ((definition
                    (clawmacs:find-installed-package
                     "{lisp_string(package_name)}")))
              (unless definition
                (error "No installed package named {lisp_string(package_name)}"))
              (clawmacs::describe-installed-package-to-string
               definition
               (clawmacs:current-buffer)))''',
        timeout=20,
    )


def switch_to_session_buffer(session):
    session.eval_lisp(
        f'''(progn
             (let ((buffer (clawmacs::find-buffer-by-name "{SESSION_NAME}")))
               (when buffer
                 (let ((frame (and (boundp 'clawmacs::*clawmacs-frame*)
                                   clawmacs::*clawmacs-frame*)))
                   (if frame
                       (progn
                         (clawmacs::mcclim-set-selected-window-buffer frame buffer)
                         (clawmacs::mcclim-sync-drei-from-buffer frame :force-p t))
                       (clawmacs::switch-to-buffer buffer))
                   (clawmacs:notify-buffer-display-change
                    buffer :e2e-session-buffer))))
             "SESSION-BUFFER-OK")''',
        timeout=10,
    )
    session.wait_snapshot(
        lambda snap: (
            (snap.get("buffer") or {}).get("name") == SESSION_NAME
            and (snap.get("render") or {}).get("bufferName") == SESSION_NAME
        ),
        timeout=10,
        description="session buffer selected in logical window",
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


def make_temp_project_root(label, files):
    """Create a temporary project tree populated with FILES."""
    root = tempfile.mkdtemp(
        prefix=f"clawmacs-{label}-",
        dir=os.path.join(CLAWMACS_DIR, ".cache"),
    )
    for relative_path, content in files.items():
        path = os.path.join(root, relative_path)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w", encoding="utf-8") as stream:
            stream.write(content)
    return root


def make_temp_git_repo(label, initial_content, modified_content):
    """Create a local git repo with one commit and one staged change target."""
    repo = tempfile.mkdtemp(
        prefix=f"clawmacs-git-{label}-",
        dir=os.path.join(CLAWMACS_DIR, ".cache"),
    )
    remote = tempfile.mkdtemp(
        prefix=f"clawmacs-git-remote-{label}-",
        dir=os.path.join(CLAWMACS_DIR, ".cache"),
    )
    run_checked(["git", "init", "-b", "main", repo], timeout=20)
    run_checked(["git", "-C", repo, "config", "user.name", "Clawmacs E2E"], timeout=20)
    run_checked(["git", "-C", repo, "config", "user.email", "e2e@clawmacs.local"], timeout=20)
    with open(os.path.join(repo, "README.md"), "w", encoding="utf-8") as stream:
        stream.write(initial_content)
    run_checked(["git", "-C", repo, "add", "README.md"], timeout=20)
    run_checked(["git", "-C", repo, "commit", "-m", "Initial commit"], timeout=20)
    run_checked(["git", "init", "--bare", remote], timeout=20)
    run_checked(["git", "-C", repo, "remote", "add", "origin", remote], timeout=20)
    with open(os.path.join(repo, "CHANGELOG.md"), "w", encoding="utf-8") as stream:
        stream.write(modified_content)
    return repo, remote


class LocalHTTPFixture:
    """Serve a static HTML directory on localhost for netcons tests."""

    def __init__(self, html_text):
        self.root = tempfile.mkdtemp(prefix="clawmacs-http-")
        self.port = 0
        self._server = None
        self._thread = None
        with open(os.path.join(self.root, "index.html"), "w", encoding="utf-8") as stream:
            stream.write(html_text)

    def __enter__(self):
        handler = partial(http.server.SimpleHTTPRequestHandler, directory=self.root)
        self._server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler)
        self.port = self._server.server_address[1]
        self._thread = threading.Thread(target=self._server.serve_forever, daemon=True)
        self._thread.start()
        return self

    def __exit__(self, exc_type, exc, tb):
        if self._server is not None:
            self._server.shutdown()
            self._server.server_close()
        if self._thread is not None:
            self._thread.join(timeout=2)


#
# Core McCLIM feature inventory covered by the offline e2e suite
#
# Buffer management:
#   10-new-buffer, 11-switch-buffer, 12-kill-buffer, 41-buffer-persistence,
#   62-mouse-click-buffer-selector
# Scratch/editor flows:
#   65-scratch-buffer-editor-flow
# M-x / help / customize:
#   43-describe-bindings, 44-describe-function, 45-describe-variable,
#   46-describe-type, 47-customize-face, 56-meta-x-command-picker
# Project open / save / search:
#   66-project-open-edit-save-search
# Sessions load / tree / fork:
#   40-save-session, 67-session-load-tree-fork
# Logical windows:
#   64-logical-window-commands
# Rendering toggles:
#   51-toggle-tool-results, 68-toggle-reasoning-metadata-tool-results
# McCLIM debugging:
#   69-mcclim-debug-status-and-snapshot
# Streaming control:
#   55-stream-poll-renders, 56-escape-stops-stream
# Mouse / selection helpers:
#   61-mouse-click-input-point, 63-mouse-click-completion-candidates


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
        self.window_title = f"Clawmacs E2E {os.path.basename(self.artifact_root)}"
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
            "(clawmacs::apply-prompt-isolation)",
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
                f'(clawmacs:clawmacs-main :session-name "{SESSION_NAME}" '
                f':window-title "{self.window_title}"))'
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
            lambda: (
                self.snapshot().get("ready")
                and ((self.snapshot().get("render") or {}).get("ready"))
                and (((self.snapshot().get("panes") or {}).get("main") or {})
                     .get("pixelWidth", 0) > 200)
            ),
            timeout=30,
            description="render-ready semantic snapshot",
        )
        self.focused = False
        self.focus(force=True)
        return self

    def wait_for_window(self):
        def largest_window_id(result):
            ids = [line.strip() for line in result.stdout.splitlines() if line.strip()]
            best_id = None
            best_area = -1
            log_lines = []
            for window_id in ids:
                geometry = subprocess.run(
                    ["xdotool", "getwindowgeometry", "--shell", window_id],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.DEVNULL,
                    text=True,
                )
                if geometry.returncode != 0:
                    continue
                width = 0
                height = 0
                for line in geometry.stdout.splitlines():
                    if line.startswith("WIDTH="):
                        width = int(line.split("=", 1)[1] or 0)
                    elif line.startswith("HEIGHT="):
                        height = int(line.split("=", 1)[1] or 0)
                area = width * height
                log_lines.append(f"{window_id} {width}x{height} area={area}")
                if area > best_area:
                    best_area = area
                    best_id = window_id
            with open(os.path.join(self.artifact_root, "window-search.log"),
                      "a", encoding="utf-8") as stream:
                stream.write("candidates for Clawmacs\n")
                for line in log_lines:
                    stream.write(line + "\n")
                stream.write(f"selected {best_id}\n")
            return best_id

        def find_window():
            if self.proc.poll() is not None:
                fail(f"clawmacs exited early; see {self.log_path}")
            result = subprocess.run(
                ["xdotool", "search", "--name", "Clawmacs"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )
            if result.returncode == 0 and result.stdout.strip():
                return largest_window_id(result)
            return None

        window_id = wait_until(find_window, timeout=30, description="Clawmacs window")
        self.focus(force=True)
        return window_id

    def focus(self, force=False):
        if self.window_id and (force or not self.focused):
            actions = [["xdotool", "windowfocus", "--sync", self.window_id]]
            try:
                pane = self.pane_geometry("input")
            except Exception:
                pane = None
            if pane and pane["pixelWidth"] > 1 and pane["pixelHeight"] > 1:
                target_x = pane["x"] + max(4, pane["pixelWidth"] // 4)
                target_y = pane["y"] + max(4, pane["pixelHeight"] // 2)
            else:
                target_x = 20
                target_y = 20
            actions.extend(
                [
                    ["xdotool", "mousemove", "--window", self.window_id,
                     str(int(target_x)), str(int(target_y))],
                    ["xdotool", "click", "1"],
                ]
            )
            for args in actions:
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

    def click_button(self, button):
        self.focus()
        run_checked(["xdotool", "click", str(button)], timeout=5)
        time.sleep(0.3)

    def click_relative(self, x, y, button=1):
        self.focus()
        run_checked(
            [
                "xdotool",
                "mousemove",
                "--window",
                self.window_id,
                str(int(x)),
                str(int(y)),
            ],
            timeout=5,
        )
        run_checked(["xdotool", "click", str(button)], timeout=5)
        time.sleep(0.3)

    def move_relative(self, x, y):
        self.focus()
        run_checked(
            [
                "xdotool",
                "mousemove",
                "--window",
                self.window_id,
                str(int(x)),
                str(int(y)),
            ],
            timeout=5,
        )
        time.sleep(0.3)

    def render_cell_size(self):
        render = self.snapshot().get("render") or {}
        cols = max(1, render.get("cols") or 1)
        rows = max(1, render.get("rows") or 1)
        return (
            (render.get("pixelWidth") or 1) / cols,
            (render.get("pixelHeight") or 1) / rows,
        )

    def pane_geometry(self, name):
        panes = self.snapshot().get("panes") or {}
        pane = panes.get(name) or {}
        return {
            "x": pane.get("x") or 0,
            "y": pane.get("y") or 0,
            "pixelWidth": max(1, pane.get("pixelWidth") or 1),
            "pixelHeight": max(1, pane.get("pixelHeight") or 1),
            "cols": max(1, pane.get("cols") or 1),
            "rows": max(1, pane.get("rows") or 1),
        }

    def click_pane_cell(self, pane_name, row, col, button=1):
        pane = self.pane_geometry(pane_name)
        char_w = pane["pixelWidth"] / pane["cols"]
        char_h = pane["pixelHeight"] / pane["rows"]
        self.click_relative(
            pane["x"] + ((col + 0.5) * char_w),
            pane["y"] + ((row + 0.5) * char_h),
            button,
        )

    def move_pane_cell(self, pane_name, row, col):
        pane = self.pane_geometry(pane_name)
        char_w = pane["pixelWidth"] / pane["cols"]
        char_h = pane["pixelHeight"] / pane["rows"]
        self.move_relative(
            pane["x"] + ((col + 0.5) * char_w),
            pane["y"] + ((row + 0.5) * char_h),
        )

    def click_main_cell(self, row, col, button=1):
        self.click_pane_cell("main", row, col, button)

    def move_main_cell(self, row, col):
        self.move_pane_cell("main", row, col)

    def click_input_cell(self, row, col, button=1):
        self.click_pane_cell("input", row, col, button)

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
        self.focused = False
        time.sleep(0.5)
        self.focus(force=True)

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
                timeout=max(5.0, len(text) / 18.0, 20.0 if len(text) > 120 else 0.0),
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

    def control_command(self, command):
        path = os.path.join(self.control_dir, "command.sexp")
        temp = path + ".tmp"
        with open(temp, "w", encoding="utf-8") as stream:
            stream.write(command)
            stream.write("\n")
        os.replace(temp, path)
        time.sleep(0.2)

    def eval_lisp(self, form, timeout=20):
        before = control_sequence(self.snapshot())
        self.control_command(f"(:eval {form})")
        snapshot = self.wait_snapshot(
            lambda snap: control_sequence(snap) > before,
            timeout=timeout,
            description="control eval result",
        )
        result = control_result(snapshot)
        if not result.get("ok"):
            fail(f"control eval failed:\n{result.get('error', '')}")
        return str(result.get("value", ""))

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
    snapshot = wait_for_rendered_message_text(session, expected, timeout=20)
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
    first = wait_for_rendered_message_text(session, expected, timeout=20)
    initial_render = first.get("render") or {}
    initial_width = initial_render.get("pixelWidth") or initial_render.get("pixel-width") or 0

    session.resize(640, 420)
    narrow = wait_for_render_width(
        session,
        lambda width: width and width < initial_width,
        "narrow McCLIM render width",
        timeout=20,
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
        timeout=20,
    )
    if not render_contains_text(wide, expected):
        fail("latest agent message disappeared after wide resize")
    session.screenshot("54-tiling-resize-latest-visible")


def test_54_input_wrap_expands_input_pane(session):
    """Long unsent input should grow the input pane so wrapped text stays visible."""
    long_text = "wrap-input-pane-probe " * 20

    def input_pane_grid():
        result = session.eval_lisp(
            r'''(let* ((frame (and (boundp 'clawmacs::*clawmacs-frame*)
                                  clawmacs::*clawmacs-frame*))
                       (pane (and frame (clawmacs::frame-drei-input-pane frame)))
                       (char-w (and frame (clawmacs::frame-char-width frame)))
                       (char-h (and frame (clawmacs::frame-char-height frame))))
                  (unless (and frame pane char-w char-h
                               (plusp char-w) (plusp char-h))
                    (error "input pane metrics unavailable"))
                  (multiple-value-bind (cols rows)
                      (clawmacs::pane-grid-dimensions pane char-w char-h)
                    (format nil "~D ~D" cols rows)))''',
            timeout=10,
        )
        cols_text, rows_text = result.replace('"', "").split()
        return int(cols_text), int(rows_text)

    try:
        E2E.set_input(session, long_text)
        session.wait_snapshot(
            lambda snap: (snap.get("buffer") or {}).get("input") == long_text,
            timeout=10,
            description="long input present in buffer snapshot",
        )
        initial_cols, initial_rows = input_pane_grid()

        session.resize(640, 420)
        wait_for_render_width(
            session,
            lambda width: width and width < 900,
            "narrow render width for wrapped input",
            timeout=20,
        )
        narrow_cols, narrow_rows = input_pane_grid()

        if narrow_rows <= 3:
            fail(
                f"wrapped input did not grow the pane: cols={narrow_cols} "
                f"rows={narrow_rows}"
            )
        if narrow_rows < initial_rows:
            fail(
                f"input pane shrank after narrow resize: initial={initial_rows} "
                f"narrow={narrow_rows}"
            )
        panes = session.snapshot().get("panes") or {}
        compose = panes.get("compose") or {}
        input_pane = panes.get("input") or {}
        if (compose.get("rows") or 0) <= 0:
            fail(f"compose pane missing or collapsed: {compose}")
        if (compose.get("y") or 0) >= (input_pane.get("y") or 0):
            fail(
                f"compose pane did not stay above input pane: "
                f"compose={compose} input={input_pane}"
            )
        E2E.assert_contains(session.text(), "wrap-input-pane-probe",
                            "long input remains present")
        session.screenshot("54-input-wrap-expands-input-pane")
    finally:
        session.resize(1000, 680)
        wait_for_render_width(
            session,
            lambda width: width and width > 900,
            "restored render width after wrapped input test",
            timeout=20,
        )


def test_55_stream_poll_renders_without_next_input(session):
    """Provider-style streaming must repaint from pulse polling alone."""
    expected = "MCCLIM-PULSE-STREAM-VISIBLE"
    E2E.set_input(session, "stream-poll-probe")
    session.press("Enter")
    snapshot = wait_for_rendered_message_text(session, expected, timeout=20)
    E2E.assert_contains(
        session.text(),
        expected,
        "streamed agent response in semantic snapshot",
    )
    if not render_contains_text(snapshot, expected):
        fail("streamed response reached buffer but not McCLIM render snapshot")
    png_path = session.screenshot("55-stream-poll-renders")
    assert_rendered_message_row_has_dark_pixels(png_path, snapshot, expected)


def test_56_escape_stops_active_stream(session):
    """Escape stops an active provider stream without turning into Meta."""
    partial = "MCCLIM-STOP-PARTIAL"
    E2E.set_input(session, "stream-stop-probe")
    session.press("Enter")
    wait_for_rendered_message_text(session, partial, timeout=10)
    session.press("Escape")

    def stopped_snapshot():
        snapshot = session.snapshot()
        buffer = snapshot.get("buffer") or {}
        if buffer.get("status") != "idle":
            return None
        text = non_user_message_text_containing(session, partial)
        if "[Stopped by user]" in text:
            return snapshot
        return None

    snapshot = wait_until(stopped_snapshot, timeout=10, interval=0.1,
                          description="stream stopped by Escape")
    screen = session.text()
    E2E.assert_contains(screen, partial, "partial stream text remains visible")
    E2E.assert_contains(screen, "[Stopped by user]", "stop marker visible")
    E2E.assert_not_contains(screen, "Streaming error", "stop is not an error")
    if not render_contains_text(snapshot, "[Stopped by user]"):
        fail("stop marker reached buffer but not McCLIM render snapshot")
    session.screenshot("56-escape-stops-stream")


def test_56_meta_x_opens_extended_command(session):
    """Direct Alt+x should open M-x while Alt emulates Meta by default."""
    session.press("Alt+x")

    def observe():
        screen = session.text()
        if "M-x" in screen and "execute-extended-command" in screen:
            return screen
        return None

    wait_until(observe, timeout=10, interval=0.1, description="M-x minibuffer")
    session.screenshot("56-meta-x-command-picker")
    session.press("Ctrl+g")


def test_57_skill_completion_escape_dismisses(session):
    """Escape should dismiss skill completion instead of becoming Meta prefix."""
    E2E.set_input(session, "$dem")

    def completion_open():
        snapshot = session.snapshot()
        skill = snapshot.get("skillCompletion") or {}
        candidates = " ".join(str(item) for item in skill.get("candidates") or [])
        if skill.get("active") and "demo-skill" in candidates:
            return snapshot
        return None

    wait_until(completion_open, timeout=10, interval=0.1,
               description="skill completion popup")
    session.press("Escape")
    wait_until(
        lambda: not ((session.snapshot().get("skillCompletion") or {}).get("active")),
        timeout=10,
        interval=0.1,
        description="dismissed skill completion popup",
    )
    screen = session.text()
    E2E.assert_not_contains(
        screen,
        "Skill:",
        "skill completion should be absent after Escape",
    )
    E2E.assert_contains(screen, "$dem", "skill mention input preserved")
    session.screenshot("57-skill-completion-escape")


def test_58_page_and_wheel_scroll_history(session):
    """Page keys and wheel events should scroll chat history."""
    E2E.clear_input(session)
    for index in range(8):
        text = f"scroll parity turn {index:02d}"
        session.type_text(text)
        session.press("Enter")
        wait_for_non_user_message_text(session, text, timeout=20)

    session.press("PageUp")
    page_up = session.wait_snapshot(
        lambda snap: ((snap.get("buffer") or {}).get("scrollOffset") or 0) > 0,
        timeout=10,
        description="PageUp scroll offset",
    )
    page_offset = (page_up.get("buffer") or {}).get("scrollOffset") or 0

    session.press("PageDown")
    session.wait_snapshot(
        lambda snap: ((snap.get("buffer") or {}).get("scrollOffset") or 0) == 0,
        timeout=10,
        description="PageDown returned to bottom",
    )

    session.click_button(4)
    wheel_up = session.wait_snapshot(
        lambda snap: ((snap.get("buffer") or {}).get("scrollOffset") or 0) > 0,
        timeout=10,
        description="wheel-up scroll offset",
    )
    wheel_offset = (wheel_up.get("buffer") or {}).get("scrollOffset") or 0
    if wheel_offset <= 0 or page_offset <= 0:
        fail("scroll offset did not increase for page or wheel input")

    session.click_button(5)
    session.wait_snapshot(
        lambda snap: ((snap.get("buffer") or {}).get("scrollOffset") or 0) == 0,
        timeout=10,
        description="wheel-down returned to bottom",
    )
    session.screenshot("58-page-and-wheel-scroll")


def test_59_speculum_self_visibility_tools(session):
    E2E.set_input(session, "speculum-window-state-probe")
    session.press("Enter")
    wait_for_non_user_message_text(session, "SPECULUM-WINDOW-STATE-OK", timeout=15)

    E2E.set_input(session, "speculum-screenshot-probe")
    session.press("Enter")
    wait_for_non_user_message_text(session, "SPECULUM-SCREENSHOT-OK", timeout=20)
    text = non_user_message_text_containing(session, "SPECULUM-SCREENSHOT-OK")
    path = message_field(text, "path")
    if not path:
        fail(f"speculum screenshot response did not include a path: {text}")
    if not os.path.exists(path):
        fail(f"speculum screenshot path does not exist: {path}")
    if os.path.getsize(path) <= 0:
        fail(f"speculum screenshot path is empty: {path}")
    session.screenshot("59-speculum-self-visibility")


def test_60_inline_image_markdown_renders(session):
    make_inline_image_probe_source()
    E2E.set_input(session, "inline-image-probe")
    session.press("Enter")
    wait_for_non_user_message_text(session, "INLINE-IMAGE-RENDER-PROBE", timeout=15)
    snapshot = wait_for_rendered_message_text(
        session,
        "![Inline red probe](screenshots/mcclim/inline-image-probe-source.png)",
        timeout=15,
    )
    png_path = session.screenshot("60-inline-image-render")
    assert_screenshot_has_inline_probe_pixels(png_path)
    if not render_contains_text(snapshot, "INLINE-IMAGE-RENDER-PROBE"):
        fail("inline image message was not tracked in render snapshot")


def completion_candidate_cell(_session):
    return "completion", 1, 3


def test_61_mouse_click_input_moves_point(session):
    """Left-click in the input pane should place point before typed text."""
    E2E.set_input(session, "ac")
    session.wait_snapshot(
        lambda snap: (snap.get("buffer") or {}).get("input") == "ac",
        timeout=10,
        description="input seeded for mouse click",
    )
    session.click_input_cell(0, 1)
    session.wait_snapshot(
        lambda snap: (snap.get("buffer") or {}).get("inputPoint") == 1,
        timeout=10,
        description="mouse click moved input point",
    )
    session.type_text("b")
    session.wait_snapshot(
        lambda snap: (snap.get("buffer") or {}).get("input") == "abc",
        timeout=10,
        description="typed text inserted at clicked point",
    )
    session.screenshot("61-mouse-click-input-point")
    E2E.clear_input(session)


def test_62_mouse_click_buffer_selector_row(session):
    """Clicking a buffer selector row should choose it and close the selector."""
    session.press("Ctrl+x")
    session.press("b")
    session.wait_snapshot(
        lambda snap: (snap.get("selectors") or {}).get("bufferSelectorActive"),
        timeout=10,
        description="buffer selector active for mouse click",
    )
    session.move_pane_cell("selector", 5, 3)
    wait_until(
        lambda: (
            (session.snapshot().get("pointerDocumentation") or {}).get("active")
            and (
                ((session.snapshot().get("pointerDocumentation") or {}).get("count") or 0)
                > 0
            )
            and "Switch to this buffer"
            in str((session.snapshot().get("pointerDocumentation") or {}).get("text") or "")
        ),
        timeout=10,
        interval=0.1,
        description="hover highlight and pointer documentation active",
    )
    hyper_doc = session.eval_lisp(
        r'''(let* ((frame (and (boundp 'clawmacs::*clawmacs-frame*)
                              clawmacs::*clawmacs-frame*))
                   (pane (and frame (clawmacs::frame-hover-pane frame)))
                   (presentation
                    (and frame
                         (clawmacs::frame-hover-presentation frame))))
              (unless (and frame pane presentation)
                (error "hover state unavailable"))
              (clawmacs::mcclim-hover-documentation-text
               frame
               pane
               presentation
               clim:+hyper-key+))''',
        timeout=10,
    ).strip().strip('"')
    if "Pane: selector-pane" not in hyper_doc or "Object: buffer" not in hyper_doc:
        fail(f"unexpected Hyper hover documentation: {hyper_doc!r}")
    session.click_pane_cell("selector", 5, 3)
    session.wait_snapshot(
        lambda snap: not (snap.get("selectors") or {}).get("bufferSelectorActive"),
        timeout=10,
        description="buffer selector closed after mouse click",
    )
    session.screenshot("62-mouse-click-buffer-selector")


def test_63_mouse_click_completion_candidates(session):
    """Clicking minibuffer and skill completion candidates should select them."""
    session.press("Ctrl+x")
    session.press("Ctrl+b")
    session.wait_snapshot(
        lambda snap: (snap.get("minibuffer") or {}).get("active"),
        timeout=10,
        description="minibuffer active for candidate click",
    )
    pane_name, row, col = completion_candidate_cell(session)
    session.click_pane_cell(pane_name, row, col)
    session.wait_snapshot(
        lambda snap: not (snap.get("minibuffer") or {}).get("active"),
        timeout=10,
        description="minibuffer closed after candidate click",
    )

    E2E.set_input(session, "$dem")
    session.wait_snapshot(
        lambda snap: (snap.get("skillCompletion") or {}).get("active"),
        timeout=10,
        description="skill popup active for candidate click",
    )
    pane_name, row, col = completion_candidate_cell(session)
    session.click_pane_cell(pane_name, row, col)
    session.wait_snapshot(
        lambda snap: "[$demo-skill]" in ((snap.get("buffer") or {}).get("input") or ""),
        timeout=10,
        description="skill candidate inserted after mouse click",
    )
    session.screenshot("63-mouse-click-completion-candidates")
    E2E.clear_input(session)


def test_64_logical_window_commands(session):
    """Frame window operations should split, cycle, and delete windows."""
    session.control_command(":split-below")
    split = session.wait_snapshot(
        lambda snap: window_get(snap.get("windows") or {}, "count", "count", 0) == 2,
        timeout=10,
        description="split-below created two windows",
    )
    first_selected = window_get(split.get("windows") or {},
                                "selectedId", "selected-id", -1)

    session.control_command(":other-window")
    other = session.wait_snapshot(
        lambda snap: (
            window_get(snap.get("windows") or {}, "count", "count", 0) == 2
            and window_get(snap.get("windows") or {},
                           "selectedId", "selected-id", -1) != first_selected
        ),
        timeout=10,
        description="other-window selected another window",
    )
    second_selected = window_get(other.get("windows") or {},
                                 "selectedId", "selected-id", -1)

    session.control_command(":split-right")
    session.wait_snapshot(
        lambda snap: window_get(snap.get("windows") or {}, "count", "count", 0) == 3,
        timeout=10,
        description="split-right created third window",
    )

    session.control_command(":delete-other-windows")
    narrowed = session.wait_snapshot(
        lambda snap: window_get(snap.get("windows") or {}, "count", "count", 0) == 1,
        timeout=10,
        description="delete-other-windows deleted other windows",
    )
    narrowed_selected = window_get(narrowed.get("windows") or {},
                                   "selectedId", "selected-id", -1)
    if narrowed_selected != second_selected:
        fail("delete-other-windows did not preserve the selected logical window")

    session.control_command(":split-below")
    session.wait_snapshot(
        lambda snap: window_get(snap.get("windows") or {}, "count", "count", 0) == 2,
        timeout=10,
        description="second C-x 2 split",
    )
    session.control_command(":delete-window")
    session.wait_snapshot(
        lambda snap: window_get(snap.get("windows") or {}, "count", "count", 0) == 1,
        timeout=10,
        description="delete-window deleted selected window",
    )
    session.screenshot("64-logical-window-commands")


def test_70_feature_inventory_runtime_contract(session):
    """Runtime inventory covers core commands, buffer types, and package tools."""
    result = session.eval_lisp(
        r'''(progn
             (clawmacs::init-tools)
             (let ((packages '("lispi" "sexed" "slop" "git" "netcons"
                               "subagent" "pipelines" "prove" "quaestor"
                               "modelaria" "artifactum" "speculum" "organa"
                               "templata" "packrat")))
               (dolist (package packages)
                 (clawmacs:load-clawmacs-package package))
               (let* ((installed
                        (mapcar #'clawmacs:package-definition-name
                                (clawmacs:list-installed-packages)))
                       (commands
                        (mapcar (lambda (symbol)
                                  (string-downcase (symbol-name symbol)))
                                (clawmacs:list-available-commands
                                 :include-inactive t)))
                       (buffer-types
                        (mapcar (lambda (type)
                                  (string-downcase
                                   (symbol-name
                                    (clawmacs:buffer-type-name type))))
                                (clawmacs:list-buffer-types)))
                       (tools
                        (mapcar #'clawmacs:agent-tool-metadata-name
                                (clawmacs:list-agent-tool-metadata))))
                 (labels ((need (name items label)
                            (unless (member name items :test #'string=)
                              (error "Missing ~A feature ~A in ~S"
                                     label name items))))
                   (dolist (name packages)
                     (need name installed "installed package"))
                   (dolist (name '("chat" "scratch" "file" "help"
                                   "customize" "listener" "artifact"
                                   "organa"))
                     (need name buffer-types "buffer type"))
                   (dolist (name
                            '("send-message"
                              "stop-llm-command"
                              "execute-extended-command"
                              "list-buffers-command"
                              "new-buffer-command"
                              "new-listener-buffer-command"
                              "next-buffer-command"
                              "kill-buffer-command"
                              "split-window-below-command"
                              "split-window-right-command"
                              "delete-window-command"
                              "delete-other-windows-command"
                              "other-window-command"
                              "save-session-command"
                              "load-session-command"
                              "session-tree-command"
                              "fork-session-command"
                              "toggle-tool-results-command"
                              "toggle-reasoning-output-command"
                              "toggle-metadata-output-command"
                              "toggle-debug-mode-command"
                              "mcclim-debug-status-command"
                              "mcclim-debug-snapshot-command"
                              "mcclim-install-debugger-command"
                              "mcclim-disable-debugger-command"
                              "mcclim-launch-listener-command"
                              "mcclim-toggle-listener-debugger-command"
                              "mcclim-inspect-current-frame-command"
                              "mcclim-inspect-visible-buffer-command"
                              "mcclim-inspect-current-buffer-command"
                              "mcclim-inspect-window-tree-command"
                              "mcclim-inspect-selected-window-command"
                              "mcclim-inspect-main-pane-command"
                              "mcclim-inspect-input-pane-command"
                              "mcclim-inspect-render-snapshot-command"
                              "mcclim-inspect-debug-status-command"
                              "mcclim-inspect-lisp-form-command"
                              "mcclim-refresh-inspectors-command"
                              "clouseau-status-command"
                              "clouseau-install-extensions-command"
                              "clouseau-list-inspectors-command"
                              "clouseau-refresh-inspectors-command"
                              "clouseau-inspect-application-state-command"
                              "clouseau-inspect-buffer-ring-command"
                              "clouseau-inspect-current-session-command"
                              "clouseau-inspect-input-message-command"
                              "clouseau-inspect-package-registry-command"
                              "clouseau-inspect-tool-registry-command"
                              "clouseau-inspect-pipeline-registry-command"
                              "clouseau-set-inspector-root-command"
                              "customize-drawing-style-command"
                              "customize-face-command"
                              "describe-function-command"
                              "describe-variable-command"
                              "describe-type-command"
                              "describe-bindings-command"
                              "compact-buffer-command"
                              "set-buffer-pipeline"
                              "clear-buffer-pipeline"
                              "artifactum-list-artifacts-command"
                              "artifactum-open-artifact-command"
                              "artifactum-attach-file-command"
                              "organa-open-todo-file-command"
                              "organa-add-todo-command"
                              "organa-set-todo-status-command"
                              "organa-move-todo-command"
                              "organa-link-todo-command"
                              "organa-unlink-todo-command"
                              "organa-cycle-view-command"))
                     (need name commands "command"))
                   (dolist (name
                            '("lisp_eval"
                              "read" "find" "grep" "write" "edit"
                              "sexed_text_diagnostics"
                              "sexed_text_outline"
                              "sexed_text_form_text"
                              "sexed_text_edit"
                              "sexed_text_edits"
                              "sexed_file_read"
                              "sexed_file_write"
                              "sexed_file_outline"
                              "sexed_file_form_text"
                              "sexed_file_edit"
                              "sexed_file_edits"
                              "sexed_project_read"
                              "sexed_project_write"
                              "sexed_project_outline"
                              "sexed_project_form_text"
                              "sexed_project_edit"
                              "sexed_project_edits"
                              "slop_list_projects"
                              "slop_current_project"
                              "slop_project_symbols"
                              "slop_symbol_at"
                              "slop_find_definitions"
                              "slop_find_definitions_batch"
                              "slop_find_references"
                              "slop_find_callers"
                              "slop_find_callees"
                              "slop_trace_calls"
                              "slop_find_mentions"
                              "slop_definition_context"
                              "slop_find_variable_uses"
                              "slop_rename_variable"
                              "git_status" "git_log" "git_diff"
                              "git_show" "git_branch" "git_remote"
                              "git_add" "git_commit" "git_push"
                              "netcons_run" "netcons_search"
                              "netcons_open" "netcons_find"
                              "subagent_run" "subagent_start"
                              "subagent_status" "subagent_wait"
                              "subagent_cancel"
                              "speculum_screenshot"
                              "speculum_window_state"
                              "speculum_inspect"
                              "artifactum_list"
                              "artifactum_read"
                              "artifactum_create"
                              "artifactum_update"
                              "organa_todo_overview"
                              "organa_todo_add"
                              "organa_todo_set_status"
                              "organa_todo_move"
                              "organa_todo_link_dependency"))
                     (need name tools "tool")))
                 (format nil "FEATURE-INVENTORY-OK packages=~D commands=~D buffers=~D tools=~D"
                         (length installed)
                         (length commands)
                         (length buffer-types)
                         (length tools)))))''',
        timeout=60,
    )
    if "FEATURE-INVENTORY-OK" not in result:
        fail(f"feature inventory eval returned unexpected result: {result}")


def test_70_listener_buffer_eval_and_commands(session):
    """Listener buffers evaluate Lisp and support McCLIM-style commands."""
    result = session.eval_lisp(
        r'''(progn
             (let* ((buf (clawmacs:ensure-listener-buffer
                          :working-directory #P"/tmp/"))
                    (frame (and (boundp 'clawmacs::*clawmacs-frame*)
                                clawmacs::*clawmacs-frame*)))
               (if frame
                   (progn
                     (clawmacs::mcclim-set-selected-window-buffer frame buf)
                     (clawmacs::mcclim-sync-drei-from-buffer frame :force-p t))
                   (clawmacs:switch-to-buffer buf))
               (labels ((latest-text ()
                          (let ((msg (clawmacs::message-prev
                                      (clawmacs:buffer-input-message buf))))
                            (if msg (clawmacs:message-text msg) "")))
                        (submit (text expected)
                          (clawmacs:set-message-text
                           (clawmacs:buffer-input-message buf) text)
                          (clawmacs:submit-listener-input buf)
                          (unless (search expected (latest-text)
                                          :test #'char-equal)
                            (error "Expected ~S after ~S, got ~S"
                                   expected text (latest-text)))))
                 (submit "(+ 2 5)" "=> 7")
                 (submit ",Help Commands" "McCLIM Listener commands")
                 (submit "#! printf LISTENER-SHELL" "LISTENER-SHELL")
                 (submit ",Package cl-user" "Package set to"))
               (clawmacs:notify-buffer-display-change buf :e2e-listener)
               "LISTENER-E2E-OK"))''',
        timeout=20,
    )
    if "LISTENER-E2E-OK" not in result:
        fail(f"listener buffer smoke returned unexpected result: {result}")
    wait_for_current_buffer_message_text(session, "=> 7", timeout=10)
    wait_for_current_buffer_message_text(session, "McCLIM Listener commands", timeout=10)
    wait_for_current_buffer_message_text(session, "LISTENER-SHELL", timeout=10)
    wait_for_current_buffer_message_text(session, "Package set to", timeout=10)
    session.screenshot("70-listener-buffer-eval-commands")
    switch_to_session_buffer(session)


def test_70_font_editor_import_smoke(session):
    """Font editor imports AST into its dedicated buffer and renders it."""
    try:
        result = session.eval_lisp(
            r'''(let* ((root (merge-pathnames
                              (format nil ".cache/mcclim-e2e-font-editor-~D-~D/"
                                      (get-universal-time)
                                      (get-internal-real-time))
                              (truename ".")))
                       (path (merge-pathnames "demo.ast" root))
                       (buf (clawmacs:ensure-font-editor-buffer)))
                  (ensure-directories-exist (merge-pathnames ".keep" root))
                  (with-open-file (stream path
                                          :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
                    (write-string
                     (format nil
                             "0 KSTID TEST;DEMO KST~%6 HEIGHT~%5 BASE LINE~%0 COLUMN POSITION ADJUSTMENT~C65 CHARACTER CODE TEST;DEMO KST~%3 RASTER WIDTH~%4 CHARACTER WIDTH~%0 LEFT KERN~%***~%* *~%***~C66 CHARACTER CODE TEST;DEMO KST~%3 RASTER WIDTH~%4 CHARACTER WIDTH~%0 LEFT KERN~%** ~%* *~%** ~%"
                             #\Page
                             #\Page)
                     stream))
                  (clawmacs::font-editor-load-font-into-buffer
                   buf
                   (clawmacs:import-ast-font path)
                   path)
                  (clawmacs:switch-to-buffer buf)
                  (clawmacs:notify-buffer-display-change buf :e2e-font-editor)
                  (let* ((font (clawmacs:font-editor-current-font buf))
                         (glyph (clawmacs:font-editor-current-glyph buf)))
                    (format nil "~A ~D ~D"
                            (clawmacs:bitmap-font-name font)
                            (clawmacs::bitmap-glyph-width glyph)
                            (clawmacs::bitmap-glyph-height glyph))))''',
            timeout=20,
        )
        session.wait_snapshot(
            lambda snap: (snap.get("buffer") or {}).get("kind") == "font-editor",
            timeout=10,
            description="font editor buffer active",
        )
        E2E.assert_contains(result, "DEMO", "font editor imported AST font name")
        E2E.assert_contains(result, "3", "font editor imported glyph width")
        E2E.assert_contains(result, "6", "font editor imported glyph height")
        session.screenshot("70-font-editor-import-smoke")
    finally:
        switch_to_session_buffer(session)



def test_71_tools_lispi_package_enable_and_eval(session):
    """Enable lispi, inspect its help, and exercise lisp_eval plus file tools."""
    probe_root = tempfile.mkdtemp(
        prefix="clawmacs-lispi-probe-",
        dir=os.path.join(CLAWMACS_DIR, ".cache"),
    )
    probe_path = os.path.join(probe_root, "lispi-probe.txt")

    E2E.describe_installed_package(session, "lispi")
    help_screen = wait_for_current_buffer_message_text(session, "Package: lispi")
    E2E.assert_contains(help_screen, "Package: lispi", "lispi help package name")
    E2E.assert_contains(help_screen, "Enabled:", "lispi help scope")
    E2E.assert_not_contains(help_screen, "lisp_eval", "lispi help does not own lisp_eval")
    E2E.assert_contains(help_screen, "read", "lispi help mentions file read")
    E2E.assert_contains(help_screen, "write", "lispi help mentions file write")
    session.screenshot("71_tools_lispi_help")
    E2E.kill_current_buffer(session)
    switch_to_session_buffer(session)

    E2E.set_input(session, "lispi package context probe")
    session.press("Enter")
    wait_for_non_user_message_text(session, "offline echo: lispi package context probe", timeout=15)

    E2E.enable_installed_package(session, "lispi")
    enabled_screen = current_buffer_message_text(session)
    E2E.assert_contains(enabled_screen, 'package_context package="lispi"',
                         "lispi context appended after enablement")
    E2E.assert_contains(enabled_screen, "[Package lispi enabled for this buffer]",
                         "lispi enablement status message")

    E2E.set_input(
        session,
        f'lispi-package-probe path="{probe_path}" code="(+ 40 2)"'
    )
    session.press("Enter")
    screen = wait_for_non_user_message_text(session, "LISPI-TOOLS-OK", timeout=15)
    E2E.assert_contains(screen, "42", "lispi lisp_eval result")
    E2E.assert_contains(screen, "LISPI-TOOLS-OK", "lispi tool probe response")
    if not os.path.exists(probe_path):
        fail(f"lispi file tool did not write expected file: {probe_path}")
    with open(probe_path, "r", encoding="utf-8") as stream:
        file_text = stream.read()
    E2E.assert_contains(file_text, "(defun lispi-probe () :ok)", "lispi file content")
    session.screenshot("71_tools_lispi_package")


def test_71_tools_sexed_package_structural_read_write(session):
    """Enable sexed and exercise structural read/write on a local fixture file."""
    project_root = make_temp_project_root(
        "sexed-probe",
        {
            "src/main.lisp": "(defun sexed-probe (n) (+ n 1))\n",
        },
    )
    file_path = os.path.join(project_root, "src", "main.lisp")

    E2E.describe_installed_package(session, "sexed")
    help_screen = wait_for_current_buffer_message_text(session, "Package: sexed")
    E2E.assert_contains(help_screen, "Structural editing with sexed", "sexed help prompt section")
    E2E.assert_contains(help_screen, "sexed_file_edit", "sexed help mentions editing tool")
    session.screenshot("71_tools_sexed_help")
    E2E.kill_current_buffer(session)
    switch_to_session_buffer(session)

    E2E.enable_installed_package(session, "sexed")
    enabled_screen = current_buffer_message_text(session)
    E2E.assert_contains(enabled_screen, 'package_context package="sexed"',
                         "sexed context appended after enablement")

    prompt = f'sexed-package-probe path="{file_path}"'
    E2E.set_input(session, prompt)
    session.press("Enter")
    screen = wait_for_non_user_message_text(session, "SEXED-TOOLS-OK", timeout=15)
    E2E.assert_contains(screen, "SEXED-TOOLS-OK", "sexed probe response")
    E2E.assert_contains(screen, "sexed-probe", "sexed structural output visible")
    with open(file_path, "r", encoding="utf-8") as stream:
        file_text = stream.read()
    E2E.assert_contains(file_text, "(defun sexed-probe (n) (1+ n))",
                         "sexed file write + rewrite persisted")
    session.screenshot("71_tools_sexed_package")


def test_71_tools_slop_package_lookup_and_trace(session):
    """Enable slop and exercise lookup, context, trace, mentions, and rename."""
    source = """(defpackage :e2e-slop
  (:use :cl))

(in-package :e2e-slop)

(defun alpha (n)
  (let ((total n))
    (incf total)
    (beta total)))

(defun beta (n)
  (+ n n))

(defun gamma (value)
  (alpha value))
"""
    docs = """alpha is mentioned in docs.
Quoted alpha is also here: \"alpha\".
"""
    project_root = os.path.join(CLAWMACS_DIR, ".cache", "slop-e2e")
    shutil.rmtree(project_root, ignore_errors=True)
    for relative_path, content in {
        "src/main.lisp": source,
        "docs/notes.md": docs,
    }.items():
        path = os.path.join(project_root, relative_path)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w", encoding="utf-8") as stream:
            stream.write(content)
    offset = source.index("total n")

    E2E.describe_installed_package(session, "slop")
    wait_for_current_buffer_message_text(session, "Package: slop")
    help_screen = package_help_text(session, "slop")
    E2E.assert_contains(help_screen, "Symbol lookup with slop", "slop help prompt section")
    E2E.assert_contains(help_screen, "slop_current_project", "slop help mentions current project")
    E2E.assert_contains(help_screen, "slop_project_symbols", "slop help mentions project symbols")
    session.screenshot("71_tools_slop_help")
    E2E.kill_current_buffer(session)
    switch_to_session_buffer(session)

    E2E.enable_installed_package(session, "slop")
    enabled_screen = current_buffer_message_text(session)
    E2E.assert_contains(enabled_screen, 'package_context package="slop"',
                         "slop context appended after enablement")

    prompt = (
        "slop-package-probe root=.cache/slop-e2e "
        f"symbol-a=alpha symbol-b=gamma query=alpha "
        f"binding-path=\"src/main.lisp\" offset={offset}"
    )
    E2E.set_input(session, prompt)
    session.press("Enter")
    wait_for_non_user_message_text(session, "SLOP-TOOLS-OK", timeout=20)
    with open(os.path.join(project_root, "src", "main.lisp"), "r", encoding="utf-8") as stream:
        file_text = stream.read()
    E2E.assert_contains(file_text, "renamed-total", "slop rename persisted to file")
    E2E.assert_not_contains(file_text, "(let ((total n))", "slop rename removed original binding")
    session.screenshot("71_tools_slop_package")


def test_71_tools_git_package_status_log_and_mutations(session):
    """Enable git and exercise status, log, add, commit, and push on local repos."""
    repo, remote = make_temp_git_repo(
        "probe",
        "initial git package content\n",
        "updated git package content\n",
    )

    E2E.describe_installed_package(session, "git")
    help_screen = wait_for_current_buffer_message_text(session, "Package: git")
    E2E.assert_contains(help_screen, "git_status", "git help mentions status")
    E2E.assert_contains(help_screen, "git_commit", "git help mentions commit")
    E2E.assert_contains(help_screen, "git_push", "git help mentions push")
    session.screenshot("71_tools_git_help")
    E2E.kill_current_buffer(session)
    switch_to_session_buffer(session)

    E2E.enable_installed_package(session, "git")
    enabled_screen = current_buffer_message_text(session)
    E2E.assert_contains(enabled_screen, 'package_context package="git"',
                         "git context appended after enablement")

    prompt = f"git-package-probe repository={repo}"
    E2E.set_input(session, prompt)
    session.press("Enter")
    screen = wait_for_non_user_message_text(session, "GIT-TOOLS-OK", timeout=20)
    E2E.assert_contains(screen, "git_status", "git status result visible")
    E2E.assert_contains(screen, "git_log", "git log result visible")
    E2E.assert_contains(screen, "git_commit", "git commit result visible")
    E2E.assert_contains(screen, "git_push", "git push result visible")
    remote_log = run_checked(["git", "--git-dir", remote, "log", "--oneline", "--all"], timeout=20)
    E2E.assert_contains(remote_log, "E2E git package commit", "git push reached local remote")
    session.screenshot("71_tools_git_package")


def test_71_tools_netcons_package_open_find_offline(session):
    """Enable netcons and exercise open/find against a local offline HTTP fixture."""
    html = """<html>
  <head><title>Clawmacs Netcons Offline Probe</title></head>
  <body>
    <h1>Clawmacs Netcons Offline Probe</h1>
    <p>needle alpha</p>
    <p>needle beta</p>
  </body>
</html>
"""
    with LocalHTTPFixture(html) as fixture:
        E2E.describe_installed_package(session, "netcons")
        help_screen = wait_for_current_buffer_message_text(session, "Package: netcons")
        E2E.assert_contains(help_screen, "netcons_open", "netcons help mentions open")
        E2E.assert_contains(help_screen, "netcons_find", "netcons help mentions find")
        session.screenshot("71_tools_netcons_help")
        E2E.kill_current_buffer(session)
        switch_to_session_buffer(session)

        E2E.enable_installed_package(session, "netcons")
        enabled_screen = current_buffer_message_text(session)
        E2E.assert_contains(enabled_screen, 'package_context package="netcons"',
                             "netcons context appended after enablement")

        prompt = f'netcons-package-probe url="http://127.0.0.1:{fixture.port}/index.html"'
        E2E.set_input(session, prompt)
        session.press("Enter")
        screen = wait_for_non_user_message_text(session, "NETCONS-TOOLS-OK", timeout=20)
        E2E.assert_contains(screen, "Clawmacs Netcons Offline Probe", "netcons page title visible")
        E2E.assert_contains(screen, "needle alpha", "netcons page content visible")
        E2E.assert_contains(screen, "needle beta", "netcons find content visible")
        session.screenshot("71_tools_netcons_package")


def test_71_tools_speculum_package_self_visibility(session):
    """Enable speculum and exercise current window state plus screenshot capture."""
    E2E.describe_installed_package(session, "speculum")
    help_screen = wait_for_current_buffer_message_text(session, "Package: speculum")
    E2E.assert_contains(help_screen, "speculum_screenshot", "speculum help mentions screenshot")
    E2E.assert_contains(help_screen, "speculum_window_state", "speculum help mentions window state")
    session.screenshot("71_tools_speculum_help")
    E2E.kill_current_buffer(session)
    switch_to_session_buffer(session)

    E2E.enable_installed_package(session, "speculum")
    enabled_screen = current_buffer_message_text(session)
    if 'package_context package="speculum"' not in enabled_screen:
        active = session.eval_lisp(
            '''(if (member "speculum"
                           (clawmacs:active-package-names
                            :buffer (clawmacs:current-buffer))
                           :test #'string=)
                   "SPECULUM-ACTIVE"
                   "SPECULUM-INACTIVE")''',
            timeout=10,
        )
        if "SPECULUM-ACTIVE" not in active:
            fail("speculum package was not active after enablement")

    E2E.set_input(session, "speculum-package-probe")
    session.press("Enter")
    screen = wait_for_non_user_message_text(session, "SPECULUM-PACKAGE-OK", timeout=20)
    E2E.assert_contains(screen, "window-state", "speculum window-state result visible")
    E2E.assert_contains(screen, "path=", "speculum screenshot path visible")
    response = non_user_message_text_containing(session, "SPECULUM-PACKAGE-OK")
    path = ""
    for token in response.split():
        if token.startswith("path="):
            path = token[len("path="):]
            break
    if not path or not os.path.exists(path):
        fail(f"speculum screenshot path missing or invalid: {path}")
    if os.path.getsize(path) <= 0:
        fail(f"speculum screenshot output was empty: {path}")
    session.screenshot("71_tools_speculum_package")


def test_71_tools_templata_package_slash_completion(session):
    """Enable templata and exercise slash completion plus /reload dispatch."""
    E2E.describe_installed_package(session, "templata")
    help_screen = wait_for_current_buffer_message_text(session, "Package: templata")
    E2E.assert_contains(help_screen, "Slash commands and prompt templates",
                        "templata help prompt section")
    E2E.assert_contains(help_screen, "Slash Commands:", "templata help slash command section")
    E2E.assert_contains(help_screen, "/reload", "templata help lists /reload")
    session.screenshot("71_tools_templata_help")
    E2E.kill_current_buffer(session)
    switch_to_session_buffer(session)

    E2E.enable_installed_package(session, "templata")
    enabled_screen = current_buffer_message_text(session)
    E2E.assert_contains(enabled_screen, 'package_context package="templata"',
                         "templata context appended after enablement")

    E2E.set_input(session, "/re")

    def slash_popup_open():
        snapshot = session.snapshot()
        slash = snapshot.get("slashCompletion") or {}
        selected = str(slash.get("selected") or "")
        if slash.get("active") and "/reload" in selected:
            return snapshot
        return None

    wait_until(slash_popup_open, timeout=10, interval=0.1,
               description="templata slash completion popup")
    session.press("Tab")
    session.wait_snapshot(
        lambda snap: (snap.get("buffer") or {}).get("input") == "/reload ",
        timeout=10,
        description="slash completion inserted /reload",
    )
    session.press("Enter")
    screen = wait_for_current_buffer_message_text(
        session,
        "Reloaded skills, package manifests, and on-disk prompt templates.",
        timeout=15,
    )
    E2E.assert_contains(screen, "Reloaded skills, package manifests, and on-disk prompt templates.",
                        "templata /reload message visible")
    session.screenshot("71_tools_templata_package")


def test_71_tools_artifactum_package_artifact_buffers(session):
    """Enable artifactum and exercise durable attachments, tools, and artifact buffers."""
    result = session.eval_lisp(
        r'''(progn
             (clawmacs::init-tools)
             (let* ((root (merge-pathnames
                           (format nil ".cache/mcclim-e2e-artifactum-~D-~D/"
                                   (get-universal-time)
                                   (get-internal-real-time))
                           (truename ".")))
                    (buf (clawmacs:current-buffer))
                    (attachment-path (merge-pathnames "notes.md" root)))
               (ensure-directories-exist (merge-pathnames ".keep" root))
               (with-open-file (stream attachment-path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                 (write-string "# Notes

Artifactum e2e preview text." stream))
               (let ((clawmacs::*sandbox-root* (truename root)))
                 (setf (clawmacs:buffer-enabled-packages buf)
                       (remove-duplicates
                        (cons "artifactum" (clawmacs:buffer-enabled-packages buf))
                        :test #'string=))
                 (clawmacs:load-active-packages :buffer buf)
                 (labels ((tool (name args)
                            (let ((clawmacs::*current-tool-buffer* buf)
                                  (clawmacs::*current-caller* :agent))
                              (clawmacs:execute-tool name args)))
                          (data (name args)
                            (nth-value 0
                              (clawmacs::lisp-data-read
                               (tool name args))))
                          (need (condition label)
                            (unless condition
                              (error "Artifactum e2e failed: ~A" label))))
                   (let* ((attachment (clawmacs::artifactum-attach-file-command
                                       buf
                                       (namestring attachment-path)))
                          (created (data "artifactum_create"
                                         '((:name . "report.json")
                                           (:content . "{\"ok\":true}")
                                           (:mime_type . "application/json"))))
                          (artifact-id (getf created :id))
                          (record (clawmacs::artifactum-find-record buf artifact-id)))
                     (need (string= "attachment" (getf attachment :kind))
                           "attachment kind")
                     (need (search "Artifactum e2e preview text"
                                   (or (getf attachment :preview) ""))
                           "attachment preview")
                     (need (string= "report.json" (getf created :name))
                           "created artifact name")
                     (need (string= "{\"ok\":true}" (getf created :content))
                           "created artifact content")
                     (need record "artifact record lookup")
                     (clawmacs::artifactum-open-record-buffer record)
                     (need (eq :artifact
                               (clawmacs:buffer-kind (clawmacs:current-buffer)))
                           "artifact buffer kind")
                     (let ((message (clawmacs::message-prev
                                     (clawmacs:buffer-input-message
                                      (clawmacs:current-buffer)))))
                       (need message "artifact message")
                       (need (search "Artifact: report.json"
                                     (clawmacs:message-text message))
                             "artifact header")
                       (need (search "Content:"
                                     (clawmacs:message-text message))
                             "artifact content block"))))
                 "ARTIFACTUM-PACKAGE-SMOKE-OK")))''',
        timeout=60,
    )
    if "ARTIFACTUM-PACKAGE-SMOKE-OK" not in result:
        fail(f"artifactum package smoke returned unexpected result: {result}")
    screen = wait_for_current_buffer_message_text(session, "Artifact: report.json")
    E2E.assert_contains(screen, "artifact-id:", "artifactum buffer shows artifact id")
    E2E.assert_contains(screen, "Content:", "artifactum buffer shows textual content")
    session.screenshot("71_tools_artifactum_package")
    E2E.kill_current_buffer(session)
    switch_to_session_buffer(session)


def test_71_tools_organa_package_todo_management(session):
    """Enable organa and exercise TODO overview, mutation, and buffer views."""
    result = session.eval_lisp(
        r'''(progn
             (clawmacs::init-tools)
             (let* ((root (merge-pathnames
                           (format nil ".cache/mcclim-e2e-organa-~D-~D/"
                                   (get-universal-time)
                                   (get-internal-real-time))
                           (truename ".")))
                    (old-buffer (clawmacs:current-buffer))
                    (buf old-buffer))
               (ensure-directories-exist (merge-pathnames ".keep" root))
               (with-open-file (stream (merge-pathnames "tasks.org" root)
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                 (write-string "* TODO Design feature
* NEXT Implement feature
:PROPERTIES:
:ID: implement-feature
:END:
* TODO Test feature
" stream))
               (let ((clawmacs::*sandbox-root* (truename root)))
                 (setf (clawmacs:buffer-enabled-packages buf)
                       (remove-duplicates
                        (cons "organa" (clawmacs:buffer-enabled-packages buf))
                        :test #'string=))
                 (clawmacs:load-active-packages :buffer buf)
                 (labels ((tool (name args)
                            (let ((clawmacs::*current-tool-buffer* buf)
                                  (clawmacs::*current-caller* :user))
                              (clawmacs:execute-tool name args)))
                          (data (name args)
                            (nth-value 0
                              (clawmacs::lisp-data-read
                               (tool name args))))
                          (need (condition label)
                            (unless condition
                              (error "Organa e2e failed: ~A" label))))
                   (let ((overview (data "organa_todo_overview"
                                         '(:path "tasks.org")))
                         (added (data "organa_todo_add"
                                      '(:path "tasks.org"
                                        :title "Ship feature"
                                        :status "NEXT")))
                         (linked (data "organa_todo_link_dependency"
                                       '(:path "tasks.org"
                                         :todo "Test feature"
                                         :depends-on "Implement feature")))
                         (updated (data "organa_todo_set_status"
                                        '(:path "tasks.org"
                                          :todo "Implement feature"
                                          :status "DONE")))
                         (moved (data "organa_todo_move"
                                      '(:path "tasks.org"
                                        :todo "Ship feature"
                                        :after "Design feature"))))
                     (need (= 3 (length (getf overview :todos)))
                           "overview")
                     (need (getf added :ok) "add")
                     (need (getf linked :ok) "link dependency")
                     (need (getf updated :ok) "status")
                     (need (getf moved :ok) "move"))
                   (let ((org-buffer
                           (clawmacs::organa-open-todo-file "tasks.org")))
                     (need (eq :organa (clawmacs:buffer-kind org-buffer))
                           "buffer kind")
                     (need (eq :dashboard
                               (clawmacs::organa-view-for-buffer org-buffer))
                           "dashboard view")
                     (clawmacs::organa-cycle-view-command org-buffer)
                     (need (eq :kanban
                               (clawmacs::organa-view-for-buffer org-buffer))
                           "kanban view")
                     (clawmacs::organa-cycle-view-command org-buffer)
                     (need (eq :dependency
                               (clawmacs::organa-view-for-buffer org-buffer))
                           "dependency view"))
                   (clawmacs:switch-to-buffer old-buffer)
                   "ORGANA-PACKAGE-SMOKE-OK"))))''',
        timeout=60,
    )
    if "ORGANA-PACKAGE-SMOKE-OK" not in result:
        fail(f"organa package smoke returned unexpected result: {result}")


def test_65_scratch_buffer_editor_flow(session):
    """Scratch should edit in place and keep Enter as text insertion."""
    E2E.switch_to_buffer(session, "scratch", "*scratch*")
    snapshot = session.snapshot()
    buffer = snapshot.get("buffer") or {}
    if buffer.get("kind") != "scratch":
        fail(f"expected scratch buffer, got {buffer.get('kind')}")

    E2E.clear_input(session)
    session.type_text("scratch-alpha")
    session.press("Enter")
    session.type_text("beta")
    snapshot = session.snapshot()
    buffer = snapshot.get("buffer") or {}
    if buffer.get("kind") != "scratch":
        fail(f"scratch editor flow switched away from scratch buffer: {buffer}")
    if "scratch-alpha\nbeta" not in str(buffer.get("input") or ""):
        fail(f"scratch buffer did not keep newline editing: {buffer.get('input')}")
    if len(buffer.get("messages") or []) != 0:
        fail("scratch editing should not create chat messages")
    session.screenshot("65-scratch-buffer-editor-flow")
    E2E.switch_to_buffer(session, "session-01", "session-01")


def test_66_project_open_edit_save_search_flow(session):
    """Project file buffers should open, save, and search deterministically."""
    global E2E_PROJECT_FILE
    E2E.switch_to_buffer(session, "session-01", "session-01")
    E2E.clear_input(session)

    session.press("Ctrl+x")
    session.press("Ctrl+f")
    wait_for_minibuffer_prompt(session, "Select Project")
    session.type_text(E2E_PROJECT_NAME)
    E2E.wait_for_minibuffer_input(session, E2E_PROJECT_NAME, timeout=5)
    session.press("Enter")
    wait_for_minibuffer_prompt(session, f"Open {E2E_PROJECT_NAME}")
    session.type_text("src/probe.lisp")
    E2E.wait_for_minibuffer_input(session, "src/probe.lisp", timeout=5)
    session.press("Enter")

    snapshot = session.wait_snapshot(
        lambda snap: (
            (snap.get("buffer") or {}).get("kind") == "file"
            and E2E_PROJECT_NAME in str((snap.get("buffer") or {}).get("name") or "")
        ),
        timeout=10,
        description="project file buffer opened",
    )
    buffer = snapshot.get("buffer") or {}
    E2E.assert_contains(
        session.text(),
        "E2E-PROJECT-BASELINE",
        "project file contents visible after opening",
    )
    if "probe-target" not in session.text():
        fail("project file editor did not show expected Lisp source")

    session.press("Ctrl+e")
    session.type_text("\n;; E2E-PROJECT-MARKER")
    session.press("Ctrl+x")
    session.press("Ctrl+s")
    wait_for_non_user_message_text(
        session,
        f"Saved {E2E_PROJECT_NAME}:src/probe.lisp",
        timeout=15,
    )

    with open(E2E_PROJECT_FILE, "r", encoding="utf-8") as stream:
        saved_text = stream.read()
    if "E2E-PROJECT-MARKER" not in saved_text:
        fail("project file was not saved back to disk")

    E2E.run_extended_command(session, "search-project-command")
    wait_for_minibuffer_prompt(session, "Select Project", timeout=15)
    session.type_text(E2E_PROJECT_NAME)
    E2E.wait_for_minibuffer_input(session, E2E_PROJECT_NAME, timeout=5)
    session.press("Enter")
    wait_for_minibuffer_prompt(session, "Search", timeout=15)
    session.type_text("E2E-PROJECT-MARKER")
    E2E.wait_for_minibuffer_input(session, "E2E-PROJECT-MARKER", timeout=5)
    session.press("Enter")
    search_text = wait_for_non_user_message_text(
        session,
        "E2E-PROJECT-MARKER",
        timeout=15,
    )
    E2E.assert_contains(
        search_text,
        "src/probe.lisp:",
        "project search reported the expected file path",
    )
    session.screenshot("66-project-open-edit-save-search")


def test_67_session_load_tree_and_fork_flow(session):
    """Session save, load, tree browsing, and fork should all remain usable."""
    E2E.switch_to_buffer(session, "session-01", "session-01")
    E2E.clear_input(session)
    E2E.set_input(session, "session-tree-probe-1")
    session.press("Enter")
    wait_for_non_user_message_text(session, "session-tree-probe-1", timeout=20)

    before_count = len(session.snapshot().get("buffers") or [])
    session.press("Ctrl+x")
    session.press("Ctrl+s")
    wait_for_non_user_message_text(session, "Session saved to", timeout=15)

    session.press("Ctrl+x")
    session.press("Ctrl+r")
    wait_for_minibuffer_prompt(session, "Load Session")
    session.type_text("session-01")
    E2E.wait_for_minibuffer_input(session, "session-01", timeout=5)
    session.press("Enter")
    loaded = session.wait_snapshot(
        lambda snap: len(snap.get("buffers") or []) > before_count,
        timeout=15,
        description="loaded session opened in a new buffer",
    )
    current = next(
        (entry for entry in loaded.get("buffers") or [] if entry.get("current")),
        None,
    )
    if not current:
        fail("load session did not leave a current buffer selected")
    if current.get("name") == "session-01":
        fail("load session should open a distinct buffer, not overwrite session-01")
    if not str(current.get("name") or "").startswith("session-01"):
        fail(f"unexpected loaded session name: {current.get('name')}")
    E2E.assert_contains(
        session.text(),
        "session-tree-probe-1",
        "loaded session preserved the original conversation",
    )

    session.press("Ctrl+x")
    session.press("t")
    tree_snapshot = session.wait_snapshot(
        lambda snap: (
            (snap.get("selectors") or {}).get("sessionTreeSelectorActive")
            and len((snap.get("selectors") or {}).get("sessionTreeSelectorRows") or [])
        ),
        timeout=15,
        description="session tree selector active",
    )
    rows = (tree_snapshot.get("selectors") or {}).get("sessionTreeSelectorRows") or []
    if not rows:
        fail("session tree selector did not render any entries")
    session.press("Ctrl+g")
    session.wait_snapshot(
        lambda snap: not (snap.get("selectors") or {}).get("sessionTreeSelectorActive"),
        timeout=10,
        description="session tree selector closed before fork",
    )
    session.press("Ctrl+x")
    session.press("T")
    session.wait_snapshot(
        lambda snap: (snap.get("selectors") or {}).get("sessionTreeSelectorActive"),
        timeout=15,
        description="fork session tree selector active",
    )
    buffers_before_fork = len(session.snapshot().get("buffers") or [])
    session.press("Enter")
    forked = session.wait_snapshot(
        lambda snap: len(snap.get("buffers") or []) > buffers_before_fork,
        timeout=15,
        description="fork session created a new buffer",
    )
    current = next(
        (entry for entry in forked.get("buffers") or [] if entry.get("current")),
        None,
    )
    if not current:
        fail("forking the session tree did not leave a current buffer")
    E2E.assert_contains(
        session.text(),
        "[Forked from",
        "fork session reported the selected tree item",
    )
    session.screenshot("67-session-load-tree-fork")


def test_68_toggle_reasoning_metadata_and_tool_results(session):
    """The display toggles should flip their semantic flags without error."""
    buffer = session.snapshot().get("buffer") or {}
    before_tool = bool(buffer.get("showToolResults"))
    before_reasoning = bool(buffer.get("showReasoning"))
    before_metadata = bool(buffer.get("showMetadata"))

    session.press("Ctrl+c")
    session.press("t")
    session.press("Ctrl+c")
    session.press("V")
    session.press("Ctrl+c")
    session.press("I")

    snapshot = session.snapshot()
    buffer = snapshot.get("buffer") or {}
    if bool(buffer.get("showToolResults")) == before_tool:
        fail("tool result visibility did not toggle")
    if bool(buffer.get("showReasoning")) == before_reasoning:
        fail("reasoning visibility did not toggle")
    if bool(buffer.get("showMetadata")) == before_metadata:
        fail("metadata visibility did not toggle")

    session.press("Ctrl+c")
    session.press("t")
    session.press("Ctrl+c")
    session.press("V")
    session.press("Ctrl+c")
    session.press("I")
    session.screenshot("68-toggle-reasoning-metadata-tool-results")


def test_69_mcclim_debug_status_and_snapshot(session):
    """McCLIM debug status and runtime snapshots render as help buffers."""
    session.eval_lisp(
        r'''(progn
             (clawmacs:mcclim-debug-status-command
              (clawmacs:current-buffer))
             "MCCLIM-DEBUG-STATUS-OK")''',
        timeout=20,
    )
    status = wait_for_current_buffer_message_text(session, "McCLIM Debugging")
    E2E.assert_contains(status, "clim-debugger", "debugger status visible")
    E2E.assert_contains(status, "Clouseau", "Clouseau status visible")
    E2E.assert_contains(status, "CLIM Listener", "Listener status visible")
    session.screenshot("69-mcclim-debug-status")

    session.eval_lisp(
        r'''(progn
             (clawmacs:mcclim-debug-snapshot-command
              (clawmacs:current-buffer))
             "MCCLIM-DEBUG-SNAPSHOT-OK")''',
        timeout=20,
    )
    snapshot = wait_for_current_buffer_message_text(
        session,
        "McCLIM Runtime Snapshot",
    )
    E2E.assert_contains(snapshot, "Panes:", "snapshot pane section visible")
    E2E.assert_contains(snapshot, "Logical windows:",
                         "snapshot logical window section visible")
    session.screenshot("69-mcclim-debug-snapshot")

    session.eval_lisp(
        r'''(progn
             (clawmacs:clouseau-status-command
              (clawmacs:current-buffer))
             "CLOUSEAU-STATUS-OK")''',
        timeout=20,
    )
    clouseau_status = wait_for_current_buffer_message_text(
        session,
        "Clouseau Support",
    )
    E2E.assert_contains(clouseau_status, "inspect-object-using-state",
                         "Clouseau protocol documentation visible")
    E2E.assert_contains(clouseau_status, "format-place-row",
                         "Clouseau place-row support visible")
    E2E.assert_contains(clouseau_status, "clouseau-inspect-application-state-command",
                         "Clouseau command list visible")
    session.screenshot("69-clouseau-status")

    session.eval_lisp(
        r'''(progn
             (clawmacs:clouseau-list-inspectors-command
              (clawmacs:current-buffer))
             "CLOUSEAU-INSPECTORS-OK")''',
        timeout=20,
    )
    inspectors = wait_for_current_buffer_message_text(
        session,
        "Clouseau Inspectors",
    )
    E2E.assert_contains(inspectors, "No inspectors",
                         "empty inspector list visible")
    session.screenshot("69-clouseau-inspectors")

    E2E.kill_current_buffer(session)
    E2E.kill_current_buffer(session)
    E2E.kill_current_buffer(session)
    E2E.kill_current_buffer(session)
    switch_to_session_buffer(session)


def test_41_buffer_state_persistence_core(session):
    """Input text persists when switching between distinct chat buffers."""
    suffix = str(int(time.time() * 1000))
    buffer_a = f"e2e-persist-a-{suffix}"
    buffer_b = f"e2e-persist-b-{suffix}"
    marker = f"persistent-text-check-{suffix}"
    setup = session.eval_lisp(
        f'''(progn
             (let ((a (clawmacs:make-buffer "{lisp_string(buffer_a)}"))
                   (b (clawmacs:make-buffer "{lisp_string(buffer_b)}")))
               (clawmacs::initialize-buffer-display-defaults a)
               (clawmacs::initialize-buffer-display-defaults b)
               (clawmacs::add-buffer-to-ring b)
               (clawmacs::add-buffer-to-ring a)
               (let ((frame (and (boundp 'clawmacs::*clawmacs-frame*)
                                 clawmacs::*clawmacs-frame*)))
                 (if frame
                     (progn
                       (clawmacs::mcclim-set-selected-window-buffer frame a)
                       (clawmacs::mcclim-sync-drei-from-buffer frame :force-p t))
                     (clawmacs::switch-to-buffer a)))
               (clawmacs:notify-buffer-display-change
                a :e2e-persistence-setup))
             "PERSISTENCE-BUFFERS-OK")''',
        timeout=10,
    )
    if "PERSISTENCE-BUFFERS-OK" not in setup:
        fail(f"persistence buffer setup failed: {setup}")
    session.wait_snapshot(
        lambda snap: (
            (snap.get("buffer") or {}).get("name") == buffer_a
            and (snap.get("render") or {}).get("bufferName") == buffer_a
        ),
        timeout=10,
        description="persistence buffer A selected",
    )
    try:
        session.focus(force=True)
        E2E.set_input(session, marker)
        session.wait_for_typed_suffix(marker, timeout=5, strict=True)

        E2E.switch_to_buffer(session, buffer_b, buffer_b)
        snapshot = session.wait_snapshot(
            lambda snap: (
                (snap.get("buffer") or {}).get("name") == buffer_b
                and (snap.get("render") or {}).get("bufferName") == buffer_b
            ),
            timeout=10,
            description="persistence buffer B selected",
        )
        if marker in str((snapshot.get("buffer") or {}).get("input") or ""):
            fail("new buffer inherited the previous buffer input text")

        E2E.switch_to_buffer(session, buffer_a, buffer_a)
        snapshot = session.wait_snapshot(
            lambda snap: (
                (snap.get("buffer") or {}).get("name") == buffer_a
                and (snap.get("render") or {}).get("bufferName") == buffer_a
                and marker in str((snap.get("buffer") or {}).get("input") or "")
            ),
            timeout=10,
            description="input text preserved after switching back",
        )
        if marker not in str((snapshot.get("buffer") or {}).get("input") or ""):
            fail("input text was not preserved after switching back")
        session.screenshot("41-buffer-persistence")
    finally:
        session.eval_lisp(
            f'''(progn
                 (let ((session (clawmacs::find-buffer-by-name "{SESSION_NAME}")))
                   (when session
                     (let ((frame (and (boundp 'clawmacs::*clawmacs-frame*)
                                       clawmacs::*clawmacs-frame*)))
                       (if frame
                           (progn
                             (clawmacs::mcclim-set-selected-window-buffer frame session)
                             (clawmacs::mcclim-sync-drei-from-buffer frame :force-p t))
                           (clawmacs::switch-to-buffer session))
                       (clawmacs:notify-buffer-display-change
                        session :e2e-persistence-cleanup))))
                 (dolist (name (list "{lisp_string(buffer_a)}"
                                     "{lisp_string(buffer_b)}"))
                   (let ((buf (clawmacs::find-buffer-by-name name)))
                     (when buf
                       (clawmacs::kill-buffer-from-ring buf))))
                 "PERSISTENCE-CLEANUP-OK")''',
            timeout=10,
        )


def test_72_pkg_installed_package_selector_lists_all_bundled_packages(session):
    """The package selector lists installed packages with scope and description."""
    package_names = [
        "artifactum",
        "git",
        "lispi",
        "modelaria",
        "netcons",
        "organa",
        "packrat",
        "pipelines",
        "prove",
        "quaestor",
        "sexed",
        "slop",
        "speculum",
        "subagent",
        "templata",
    ]
    E2E.open_extended_command(session, "minibuffer-toggle-package-command")
    snapshot = session.wait_snapshot(
        lambda snap: (
            (snap.get("minibuffer") or {}).get("active")
            and (snap.get("minibuffer") or {}).get("prompt") == "Enable Package"
        ),
        timeout=10,
        description="installed package selector active",
    )
    candidates = minibuffer_candidate_strings(snapshot)
    candidate_text = "\n".join(candidates)
    for name in package_names:
        scope = package_selector_scope_label(candidate_text, name)
        if not scope:
            fail(f"package selector did not list {name} with a scope label")
        if scope not in {"default", "buffer", "agent", "global"}:
            fail(f"package selector reported an unknown scope for {name}: {scope}")
        if not any(f"] {name} - " in candidate for candidate in candidates):
            fail(f"package selector did not include {name} in the minibuffer list")
    session.screenshot("72-pkg-installed-package-selector")
    E2E.cancel_minibuffer(session)


def test_72_pkg_package_toggle_cycles_scope_and_appends_context(session):
    """Enabling a package in context cycles scope and appends package context."""
    package_name = "sexed"
    E2E.set_input(session, "package context seed")
    session.press("Enter")
    wait_for_non_user_message_text(session, "offline echo: package context seed", timeout=15)

    E2E.enable_installed_package(session, package_name)
    session.wait_snapshot(
        lambda snap: any(
            "<package_context package=\"sexed\">" in str(message.get("text", ""))
            for message in (snap.get("buffer") or {}).get("messages") or []
        ),
        timeout=10,
        description="sexed package context insertion",
    )
    screen = session.text()
    E2E.assert_contains(screen, "[Package sexed enabled for this buffer]",
                         "sexed package enable confirmation")
    E2E.assert_contains(screen, "<package_context package=\"sexed\">",
                         "sexed package context appended to conversation")
    E2E.assert_contains(screen, "Structural editing with sexed",
                         "sexed package prompt text visible after enabling")
    session.screenshot("72-pkg-package-toggle-context")


def test_72_pkg_describe_installed_package_opens_help_buffer(session):
    """The package describe command opens a dedicated help buffer."""
    E2E.open_extended_command(session, "describe-installed-package-command")
    wait_for_minibuffer_prompt(session, "Describe Package", timeout=10)
    E2E.confirm_minibuffer_candidate(session, "organa")
    wait_for_current_buffer_message_text(session, "Package: organa")
    screen = package_help_text(session, "organa")
    E2E.assert_contains(screen, "Prompt Sections:", "organa help prompt sections")
    E2E.assert_contains(screen, "Org-mode TODO project management buffers and agent tools.",
                         "organa help description text")
    E2E.assert_contains(screen, "Organa org TODO project management",
                         "organa prompt section title")
    E2E.assert_contains(screen, "Buffer Types:", "organa help buffer types")
    session.screenshot("72-pkg-describe-installed-package")
    E2E.kill_current_buffer(session)
    switch_to_session_buffer(session)


def test_72_pkg_pipelines_help_buffer_lists_commands_and_prompt_sections(session):
    """The pipelines package help buffer exposes its command and prompt surface."""
    E2E.open_extended_command(session, "describe-installed-package-command")
    wait_for_minibuffer_prompt(session, "Describe Package", timeout=10)
    E2E.confirm_minibuffer_candidate(session, "pipelines")
    wait_for_current_buffer_message_text(session, "Package: pipelines")
    screen = package_help_text(session, "pipelines")
    E2E.assert_contains(screen, "Deterministic pipelines",
                         "pipelines prompt text")
    E2E.assert_contains(screen, "define-pipeline", "pipelines prompt docs")
    E2E.assert_contains(screen, "defpipeline", "pipelines prompt macro docs")
    E2E.assert_contains(screen, "Commands:", "pipelines command section")
    E2E.assert_contains(screen, "set-buffer-pipeline", "pipelines set command")
    E2E.assert_contains(screen, "clear-buffer-pipeline", "pipelines clear command")
    session.screenshot("72-pkg-pipelines-help")
    E2E.kill_current_buffer(session)
    switch_to_session_buffer(session)


def test_72_pkg_subagent_help_buffer_lists_tools_and_prompt_sections(session):
    """The subagent package help buffer exposes delegation tools and guidance."""
    E2E.open_extended_command(session, "describe-installed-package-command")
    wait_for_minibuffer_prompt(session, "Describe Package", timeout=10)
    E2E.confirm_minibuffer_candidate(session, "subagent")
    wait_for_current_buffer_message_text(session, "Package: subagent")
    screen = package_help_text(session, "subagent")
    E2E.assert_contains(screen, "Delegation with subagent",
                         "subagent prompt text")
    E2E.assert_contains(screen, "subagent_run", "subagent prompt docs")
    E2E.assert_contains(screen, "subagent_start", "subagent prompt docs")
    E2E.assert_contains(screen, "subagent_wait", "subagent prompt docs")
    E2E.assert_contains(screen, "subagent_cancel", "subagent prompt docs")
    E2E.assert_contains(screen, "subagent_status", "subagent prompt docs")
    E2E.assert_contains(screen, "custom transient agent",
                         "subagent custom agent docs")
    session.screenshot("72-pkg-subagent-help")
    E2E.kill_current_buffer(session)
    switch_to_session_buffer(session)


def test_72_pkg_organa_buffer_type_is_registered_and_discoverable(session):
    """The organa package registers a buffer type that help can describe."""
    result = session.eval_lisp(
        r'''(progn
             (clawmacs::init-tools)
             (let* ((definition (clawmacs:find-installed-package "organa"))
                    (types (clawmacs::package-owned-buffer-types "organa"))
                    (help (clawmacs::describe-installed-package-to-string
                           definition
                           (clawmacs:current-buffer))))
               (unless definition
                 (error "organa package was not installed"))
               (unless (and types
                            (find :organa types
                                  :key #'clawmacs::buffer-type-name
                                  :test #'eq))
                 (error "organa buffer type was not registered: ~S" types))
               (unless (search "Buffer Types:" help)
                 (error "organa help did not list buffer types: ~A" help))
               (unless (search "organa" help :test #'char-equal)
                 (error "organa help did not mention the buffer type name: ~A" help))
               "ORGANA-BUFFER-TYPE-OK"))''',
        timeout=30,
    )
    if "ORGANA-BUFFER-TYPE-OK" not in result:
        fail(f"organa buffer type test returned unexpected result: {result}")
    session.screenshot("72-pkg-organa-buffer-type")


def test_72_pkg_subagent_and_pipeline_runtime_contract(session):
    """Subagent tools and deterministic pipelines run inside the live app."""
    result = session.eval_lisp(
        r'''(progn
             (clawmacs::init-tools)
             (let* ((buf (clawmacs:current-buffer))
                    (packages '("subagent" "pipelines")))
               (setf (clawmacs:buffer-enabled-packages buf)
                     (remove-duplicates
                      (append packages
                              (clawmacs:buffer-enabled-packages buf))
                      :test #'string=))
               (clawmacs:load-active-packages :buffer buf)
               (labels ((tool (name args)
                          (let ((clawmacs::*current-tool-buffer* buf)
                                (clawmacs::*current-caller* :user))
                            (clawmacs:execute-tool name args)))
                        (data (name args)
                          (nth-value 0
                            (clawmacs::lisp-data-read
                             (tool name args))))
                        (need (condition label)
                          (unless condition
                            (error "Subagent/pipeline e2e failed: ~A" label)))
                        (completed (text)
                          (let ((state (clawmacs::make-stream-state)))
                            (bt:with-lock-held
                                ((clawmacs::stream-state-lock state))
                              (setf (clawmacs::stream-state-stop-reason state)
                                    "end_turn"
                                    (clawmacs::stream-state-content-blocks state)
                                    (list
                                     (clawmacs::canonical-text-block text))
                                    (clawmacs::stream-state-done-p state)
                                    t))
                            state)))
                 (let ((original
                         (symbol-function
                          'clawmacs::provider-request-streaming))
                       (count 0))
                   (unwind-protect
                        (progn
                          (setf (symbol-function
                                 'clawmacs::provider-request-streaming)
                                (lambda (provider messages callback
                                         &key model max-tokens tools
                                           reasoning-effort system-prompt
                                         &allow-other-keys)
                                  (declare (ignore provider messages callback
                                                   model max-tokens tools
                                                   reasoning-effort
                                                   system-prompt))
                                  (incf count)
                                  (when (= count 3)
                                    (sleep 0.5))
                                  (completed
                                   (format nil "runtime-response-~D"
                                           count))))
                          (let* ((run (data "subagent_run"
                                            '(:prompt "sync delegate"
                                              :agent_spec
                                              ((:name . "e2e-runtime")
                                               (:provider . "zai")
                                               (:model . "glm-5")
                                               (:core_prompt . "CORE")
                                               (:personality_prompt
                                                . "PERSONALITY")))))
                                 (started
                                  (data "subagent_start"
                                        '(:prompt "async delegate"
                                          :provider "zai"
                                          :model "glm-5")))
                                 (id (getf (getf started :subagent) :id))
                                 (status (data "subagent_status" nil))
                                 (waited (data "subagent_wait"
                                               `(:id ,id :timeout 3)))
                                 (cancel-start
                                  (data "subagent_start"
                                        '(:prompt "cancel delegate"
                                          :provider "zai"
                                          :model "glm-5")))
                                 (cancel-id
                                  (getf (getf cancel-start :subagent) :id))
                                 (cancelled
                                  (data "subagent_cancel"
                                        `(:id ,cancel-id))))
                            (need (getf run :ok) "subagent_run")
                            (need (getf started :ok) "subagent_start")
                            (need (getf status :ok) "subagent_status")
                            (need (eq :succeeded (getf waited :status))
                                  "subagent_wait")
                            (need (getf cancelled :ok)
                                  "subagent_cancel"))
                          (clawmacs:define-pipeline
                           "e2e-runtime-pipeline"
                           :stages '((:name "plan"
                                      :prompt "Plan {{input}}"
                                      :next "build")
                                     (:name "build"
                                      :prompt "Build {{stage:plan}}")))
                          (clawmacs:set-buffer-pipeline
                           buf "e2e-runtime-pipeline")
                          (need (string= "e2e-runtime-pipeline"
                                         (clawmacs:buffer-pipeline-name buf))
                                "set-buffer-pipeline")
                          (let ((pipeline
                                  (clawmacs:run-pipeline-on-buffer
                                   "e2e-runtime-pipeline"
                                   "ship"
                                   :buffer buf)))
                            (need (eq :succeeded
                                      (clawmacs:pipeline-run-result-status
                                       pipeline))
                                  "run-pipeline-on-buffer")
                            (need (search "runtime-response"
                                          (or (clawmacs:pipeline-run-result-final-text
                                               pipeline)
                                              ""))
                                  "pipeline final text"))
                          (clawmacs:clear-buffer-pipeline buf)
                          (need (null (clawmacs:buffer-pipeline-name buf))
                                "clear-buffer-pipeline"))
                     (setf (symbol-function
                            'clawmacs::provider-request-streaming)
                           original)))))
               "SUBAGENT-PIPELINE-RUNTIME-OK"))''',
        timeout=90,
    )
    if "SUBAGENT-PIPELINE-RUNTIME-OK" not in result:
        fail(f"subagent/pipeline runtime returned unexpected result: {result}")


def test_72_pkg_agent_selector_switches_registered_agent(session):
    """The agent selector lists registered agents and can switch buffers."""
    E2E.open_extended_command(session, "minibuffer-select-agent-command")
    snapshot = session.wait_snapshot(
        lambda snap: (
            (snap.get("minibuffer") or {}).get("active")
            and (snap.get("minibuffer") or {}).get("prompt") == "Select Agent"
            and len((snap.get("minibuffer") or {}).get("candidates") or []) >= 3
        ),
        timeout=10,
        description="agent selector candidates",
    )
    joined = "\n".join(minibuffer_candidate_strings(snapshot))
    E2E.assert_contains(joined, "writer", "writer agent visible in selector")
    E2E.assert_contains(joined, "pair", "pair agent visible in selector")
    E2E.assert_contains(joined, "openai-codex/gpt-5.4",
                         "pair agent model visible in selector")
    E2E.assert_contains(joined, "zai/glm-5", "writer agent model visible in selector")
    E2E.confirm_minibuffer_candidate(session, "pair")
    screen = wait_for_non_user_message_text(session, "Agent changed to pair", timeout=10)
    E2E.assert_contains(screen, "openai-codex/gpt-5.4",
                         "agent selection message shows provider/model")
    E2E.assert_contains(screen, "think high", "agent selection message shows think level")
    session.screenshot("72-pkg-agent-selector")


def test_72_pkg_model_selector_switches_registered_model(session):
    """The model selector lists deterministic models and can switch them."""
    E2E.open_extended_command(session, "minibuffer-select-model-command")
    snapshot = session.wait_snapshot(
        lambda snap: (
            (snap.get("minibuffer") or {}).get("active")
            and (snap.get("minibuffer") or {}).get("prompt") == "Select Model"
            and len((snap.get("minibuffer") or {}).get("candidates") or []) >= 2
        ),
        timeout=10,
        description="model selector candidates",
    )
    joined = "\n".join(minibuffer_candidate_strings(snapshot))
    E2E.assert_contains(joined, "zai/glm-5", "zai model visible in selector")
    E2E.assert_contains(joined, "openai-codex/gpt-5.4",
                         "openai-codex model visible in selector")
    E2E.confirm_minibuffer_candidate(session, "glm-5")
    screen = wait_for_non_user_message_text(session, "Model changed to zai/glm-5", timeout=10)
    E2E.assert_contains(screen, "Model changed to zai/glm-5",
                         "model selection confirmation visible")
    E2E.assert_contains(screen, "zai/glm-5", "selected model visible in message")
    session.screenshot("72-pkg-model-selector")


def test_72_pkg_think_selector_switches_think_level(session):
    """The think selector lists supported levels and updates the buffer."""
    session.eval_lisp(
        r'''(progn
             (clawmacs::init-tools)
             (let ((buf (clawmacs:current-buffer)))
               (clawmacs:set-buffer-provider-override buf :zai)
               (clawmacs:set-buffer-model-override buf "glm-5")
               (clawmacs:clear-buffer-think-level-override buf)
               "THINK-SELECTOR-READY"))''',
        timeout=20,
    )
    E2E.open_extended_command(session, "minibuffer-select-think-level-command")
    snapshot = session.wait_snapshot(
        lambda snap: (
            (snap.get("minibuffer") or {}).get("active")
            and (snap.get("minibuffer") or {}).get("prompt") == "Select Think Level"
            and len((snap.get("minibuffer") or {}).get("candidates") or []) >= 3
        ),
        timeout=10,
        description="think selector candidates",
    )
    joined = "\n".join(minibuffer_candidate_strings(snapshot))
    E2E.assert_contains(joined, "default", "think selector includes default")
    E2E.assert_contains(joined, "low", "think selector includes low")
    E2E.assert_contains(joined, "high", "think selector includes high")
    E2E.confirm_minibuffer_candidate(session, "low")
    screen = wait_for_non_user_message_text(session, "Think level set to low", timeout=10)
    E2E.assert_contains(screen, "Think level set to low",
                         "think selection confirmation visible")
    E2E.assert_contains(screen, "zai/glm-5", "think selection message names the model")
    session.screenshot("72-pkg-think-selector")


def test_registry(group):
    offline_tests = [
        ("53-async-agent-reply-renders", test_53_async_agent_reply_renders_without_next_input),
        ("54-tiling-resize-latest-visible", test_54_tiling_resize_keeps_latest_message_visible),
        ("54-input-wrap-expands-input-pane", test_54_input_wrap_expands_input_pane),
        ("55-stream-poll-renders", test_55_stream_poll_renders_without_next_input),
        ("56-escape-stops-stream", test_56_escape_stops_active_stream),
        ("56-meta-x-command-picker", test_56_meta_x_opens_extended_command),
        ("57-skill-completion-escape", test_57_skill_completion_escape_dismisses),
        ("58-page-and-wheel-scroll", test_58_page_and_wheel_scroll_history),
        ("59-speculum-self-visibility", test_59_speculum_self_visibility_tools),
        ("60-inline-image-render", test_60_inline_image_markdown_renders),
        ("61-mouse-click-input-point", test_61_mouse_click_input_moves_point),
        ("62-mouse-click-buffer-selector", test_62_mouse_click_buffer_selector_row),
        ("63-mouse-click-completion-candidates", test_63_mouse_click_completion_candidates),
        ("64-logical-window-commands", test_64_logical_window_commands),
        ("65-scratch-buffer-editor-flow", test_65_scratch_buffer_editor_flow),
        ("66-project-open-edit-save-search", test_66_project_open_edit_save_search_flow),
        ("67-session-load-tree-fork", test_67_session_load_tree_and_fork_flow),
        ("68-toggle-reasoning-metadata-tool-results", test_68_toggle_reasoning_metadata_and_tool_results),
        ("69-mcclim-debug-status-and-snapshot", test_69_mcclim_debug_status_and_snapshot),
        ("70-listener-buffer-eval-commands", test_70_listener_buffer_eval_and_commands),
        ("70-font-editor-import-smoke", test_70_font_editor_import_smoke),
        ("38-shell-prefix", E2E.test_38_shell_prefix),
        ("39-debug-mode", E2E.test_39_debug_mode_toggle),
        ("40-save-session", E2E.test_40_save_session),
        ("41-buffer-persistence", test_41_buffer_state_persistence_core),
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
    package_tests = [
        ("70_feature_inventory_runtime_contract",
         test_70_feature_inventory_runtime_contract),
        ("71_tools_lispi_package_enable_and_eval",
         test_71_tools_lispi_package_enable_and_eval),
        ("71_tools_sexed_package_structural_read_write",
         test_71_tools_sexed_package_structural_read_write),
        ("71_tools_slop_package_lookup_and_trace",
         test_71_tools_slop_package_lookup_and_trace),
        ("71_tools_git_package_status_log_and_mutations",
         test_71_tools_git_package_status_log_and_mutations),
        ("71_tools_netcons_package_open_find_offline",
         test_71_tools_netcons_package_open_find_offline),
        ("71_tools_speculum_package_self_visibility",
         test_71_tools_speculum_package_self_visibility),
        ("71_tools_templata_package_slash_completion",
         test_71_tools_templata_package_slash_completion),
        ("71_tools_artifactum_package_artifact_buffers",
         test_71_tools_artifactum_package_artifact_buffers),
        ("71_tools_organa_package_todo_management",
         test_71_tools_organa_package_todo_management),
        ("72_pkg_installed_package_selector_lists_all_bundled_packages",
         test_72_pkg_installed_package_selector_lists_all_bundled_packages),
        ("72_pkg_package_toggle_cycles_scope_and_appends_context",
         test_72_pkg_package_toggle_cycles_scope_and_appends_context),
        ("72_pkg_describe_installed_package_opens_help_buffer",
         test_72_pkg_describe_installed_package_opens_help_buffer),
        ("72_pkg_pipelines_help_buffer_lists_commands_and_prompt_sections",
         test_72_pkg_pipelines_help_buffer_lists_commands_and_prompt_sections),
        ("72_pkg_subagent_help_buffer_lists_tools_and_prompt_sections",
         test_72_pkg_subagent_help_buffer_lists_tools_and_prompt_sections),
        ("72_pkg_organa_buffer_type_is_registered_and_discoverable",
         test_72_pkg_organa_buffer_type_is_registered_and_discoverable),
        ("72_pkg_subagent_and_pipeline_runtime_contract",
         test_72_pkg_subagent_and_pipeline_runtime_contract),
        ("72_pkg_agent_selector_switches_registered_agent",
         test_72_pkg_agent_selector_switches_registered_agent),
        ("72_pkg_model_selector_switches_registered_model",
         test_72_pkg_model_selector_switches_registered_model),
        ("72_pkg_think_selector_switches_think_level",
         test_72_pkg_think_selector_switches_think_level),
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
    if group == "windows":
        return [("64-logical-window-commands", test_64_logical_window_commands)]
    if group == "packages":
        return package_tests
    if group == "offline":
        return offline_tests + readline_tests + package_tests
    if group == "readline":
        return readline_tests
    return full_initial_tests + offline_tests + llm_new_tests + readline_tests + package_tests


def parse_args():
    parser = argparse.ArgumentParser(description="Run clawmacs McCLIM e2e tests")
    parser.add_argument(
        "--only",
        choices=["all", "readline", "offline", "packages", "smoke", "windows", *ONLINE_GROUPS],
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
    global E2E_PROJECT_ROOT, E2E_PROJECT_FILE
    ensure_prompt_project_root()
    E2E_PROJECT_ROOT, E2E_PROJECT_FILE = create_e2e_project_root()
    extra_evals = base_extra_evals(skill_root_path)
    extra_evals.append(package_config_fixture_eval())
    extra_evals.append(project_fixture_eval(E2E_PROJECT_ROOT))
    extra_evals.extend(package_orchestration_extra_evals())
    extra_evals.append(f"(progn {core_offline_agent_eval()})")
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
        shutil.rmtree(E2E_PROJECT_ROOT, ignore_errors=True)


def run_online_suite(group, skill_root_path):
    ensure_prompt_project_root()
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
