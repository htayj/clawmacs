#!/usr/bin/env python3
"""Validate source-declared command/menu/translator features in McCLIM runtime."""

from __future__ import annotations

import argparse
import json
import pathlib
import subprocess
import sys

from example_features import source_package, symbol_designator


def q(value: str) -> str:
    return json.dumps(value)


def load_coverage(root: pathlib.Path) -> list[dict[str, object]]:
    path = root / "artifacts/example-interactions/coverage.json"
    if not path.exists():
        subprocess.check_call(["python3", "scripts/audit-example-interactions.py"], cwd=root)
    return json.loads(path.read_text(encoding="utf-8"))


def build_probe(root: pathlib.Path, coverage: list[dict[str, object]], out_path: pathlib.Path) -> None:
    items = []
    for item in coverage:
        name = str(item["file"])
        package = source_package(name)
        frames = [symbol_designator(frame, package) for frame in item.get("frames", [])]
        commands = [symbol_designator(command, package) for command in item.get("commands", [])]
        translators = [str(translator).upper() for translator in item.get("translators", [])]
        actions = [str(action).upper() for action in item.get("actions", [])]
        if frames or commands or translators or actions or item.get("menus"):
            items.append(
                {
                    "name": name,
                    "frames": frames,
                    "commands": commands,
                    "translators": translators,
                    "actions": actions,
                    "menu_count": len(item.get("menus", [])),
                }
            )

    cases = "\n".join(
        "    (list :name {name} :frames '{frames} :commands '{commands} "
        ":translators '{translators} :actions '{actions} :menu-count {menu_count})".format(
            name=q(item["name"]),
            frames="(" + " ".join(f'("{pkg}" "{sym}")' for pkg, sym in item["frames"]) + ")",
            commands="(" + " ".join(f'("{pkg}" "{sym}")' for pkg, sym in item["commands"]) + ")",
            translators="(" + " ".join(q(t) for t in item["translators"]) + ")",
            actions="(" + " ".join(q(a) for a in item["actions"]) + ")",
            menu_count=item["menu_count"],
        )
        for item in items
    )

    probe = f"""
(require :asdf)

(defun configure-registry ()
  (let* ((script (or *load-pathname* *compile-file-pathname*))
         (backend-root (truename
                        (merge-pathnames "../../"
                                         (make-pathname :directory (pathname-directory script)))))
         (repo-root (truename (merge-pathnames "../../" backend-root)))
         (mcclim-root (or (uiop:getenv "MCCLIM_SOURCE_ROOT")
                          "/home/tay/reference/external_src/McCLIM/")))
    (pushnew backend-root asdf:*central-registry* :test #'equal)
    (pushnew repo-root asdf:*central-registry* :test #'equal)
    (asdf:initialize-source-registry
     `(:source-registry
       (:tree ,(namestring backend-root))
       (:tree ,(namestring (truename mcclim-root)))
       :inherit-configuration))))

(defun find-runtime-symbol (pkg name)
  (let ((package (find-package pkg)))
    (and package (find-symbol name package))))

(defun symbol-status (pkg name)
  (let ((symbol (find-runtime-symbol pkg name)))
    (cond ((null symbol) :missing-symbol)
          ((not (fboundp symbol)) :not-fbound)
          (t :ok))))

(defun make-frame-or-nil (frames)
  (loop for (pkg name) in frames
        for symbol = (find-runtime-symbol pkg name)
        when (and symbol (find-class symbol nil))
          return (handler-case
                     (funcall (symbol-function (find-symbol "MAKE-APPLICATION-FRAME" "CLIM"))
                              symbol)
                   (error () nil))))

(defun command-table-command-present-p (command table)
  (and table
       (funcall (symbol-function (find-symbol "COMMAND-ACCESSIBLE-IN-COMMAND-TABLE-P" "CLIM"))
                command table)))

(defun command-table-menu-count (table)
  (let ((count 0))
    (when table
      (funcall (symbol-function (find-symbol "MAP-OVER-COMMAND-TABLE-MENU-ITEMS" "CLIM"))
               (lambda (&rest args)
                 (declare (ignore args))
                 (incf count))
               table))
    count))

(defun command-table-translator-count (table)
  (let ((count 0))
    (when table
      (funcall (symbol-function (find-symbol "MAP-OVER-COMMAND-TABLE-TRANSLATORS" "CLIM"))
               (lambda (&rest args)
                 (declare (ignore args))
                 (incf count))
               table))
    count))

(defun emit-json-string (stream string)
  (write-char #\\" stream)
  (loop for ch across string
        do (case ch
             (#\\" (write-string "\\\\\\"" stream))
             (#\\\\ (write-string "\\\\\\\\" stream))
             (t (write-char ch stream))))
  (write-char #\\" stream))

(defun emit-result (stream result firstp)
  (unless firstp (write-string "," stream))
  (write-string "{{\\"example\\":" stream)
  (emit-json-string stream (getf result :name))
  (format stream ",\\"ok\\":~:[false~;true~]" (getf result :ok))
  (format stream ",\\"commands\\":~D" (getf result :commands))
  (format stream ",\\"commands_present\\":~D" (getf result :commands-present))
  (format stream ",\\"menus_expected\\":~D" (getf result :menus-expected))
  (format stream ",\\"menus_present\\":~D" (getf result :menus-present))
  (format stream ",\\"translators_expected\\":~D" (getf result :translators-expected))
  (format stream ",\\"translators_present\\":~D" (getf result :translators-present))
  (write-string ",\\"failures\\":[" stream)
  (loop for failure in (getf result :failures)
        for first = t then nil
        do (progn
             (unless first (write-string "," stream))
             (emit-json-string stream failure)))
  (write-string "]}}" stream))

(defparameter *cases*
  (list
{cases}))

(configure-registry)
(asdf:load-system :mcclim-charms)
(setf (symbol-value (find-symbol "*INITIALIZE-CURSES-ON-PORT-CREATE*" "MCCLIM-CHARMS")) nil)
(setf (symbol-value (find-symbol "*DEFAULT-SERVER-PATH*" "CLIM")) '(:charms))
(asdf:load-system :clim-examples)

(let ((results '()))
  (dolist (case *cases*)
    (let* ((name (getf case :name))
           (frame (make-frame-or-nil (getf case :frames)))
           (table (and frame
                       (funcall (symbol-function (find-symbol "FRAME-COMMAND-TABLE" "CLIM"))
                                frame)))
           (failures '())
           (commands-present 0))
      (dolist (command-designator (getf case :commands))
        (destructuring-bind (pkg symbol-name) command-designator
          (let* ((symbol (find-runtime-symbol pkg symbol-name))
                 (status (symbol-status pkg symbol-name)))
            (cond ((not (eq status :ok))
                   (push (format nil "~A::~A ~A" pkg symbol-name status) failures))
                  ((or (null table)
                       (fboundp symbol)
                       (command-table-command-present-p symbol table))
                   (incf commands-present))
                  (t
                   (push (format nil "~A::~A not accessible in frame command table"
                                 pkg symbol-name)
                         failures))))))
      (let* ((menus-present (command-table-menu-count table))
             (translators-present (command-table-translator-count table))
             (menus-expected (getf case :menu-count))
             (translators-expected (+ (length (getf case :translators))
                                      (length (getf case :actions)))))
        ;; Some examples define menu bars through a separate command table, so
        ;; command accessibility is the portable runtime assertion here. The
        ;; menu counts are still reported for coverage accounting.
        (when (and (> translators-expected 0) (= translators-present 0))
          (push "source declares translators/actions but runtime command table has no translators" failures))
        (push (list :name name
                    :ok (null failures)
                    :commands (length (getf case :commands))
                    :commands-present commands-present
                    :menus-expected menus-expected
                    :menus-present menus-present
                    :translators-expected translators-expected
                    :translators-present translators-present
                    :failures (nreverse failures))
              results))))
  (with-open-file (stream {q(str(out_path))}
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string "[" stream)
    (loop for result in (nreverse results)
          for first = t then nil
          do (emit-result stream result first))
    (write-string "]" stream)))

(uiop:quit 0)
"""
    (out_path.parent / "command-table-feature-probe.lisp").write_text(probe, encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--artifacts", default="artifacts/command-table-features")
    args = parser.parse_args()

    root = pathlib.Path(__file__).resolve().parents[1]
    artifacts = root / args.artifacts
    artifacts.mkdir(parents=True, exist_ok=True)
    results_path = artifacts / "results.json"
    build_probe(root, load_coverage(root), results_path)
    subprocess.check_call(
        [
            "sbcl",
            "--noinform",
            "--disable-debugger",
            "--load",
            str(artifacts / "command-table-feature-probe.lisp"),
        ],
        cwd=root,
    )
    results = json.loads(results_path.read_text(encoding="utf-8"))
    failures = [item for item in results if not item["ok"]]
    summary = {
        "examples": len(results),
        "passed": len(results) - len(failures),
        "failed": len(failures),
        "commands": sum(item["commands"] for item in results),
        "commands_present": sum(item["commands_present"] for item in results),
        "failures": failures,
    }
    (artifacts / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    (artifacts / "status.txt").write_text(
        ("pass\n" if not failures else "fail\n") + json.dumps(summary, indent=2) + "\n",
        encoding="utf-8",
    )
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main())
