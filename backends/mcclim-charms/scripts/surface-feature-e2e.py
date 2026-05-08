#!/usr/bin/env python3
"""Runtime probes for source-declared gadget, accepting-values, and pointer surfaces."""

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
        if not (
            item.get("has_gadgets")
            or item.get("has_accepting_values")
            or item.get("has_tracking_pointer")
        ):
            continue
        name = str(item["file"])
        package = source_package(name)
        frames = [symbol_designator(frame, package) for frame in item.get("frames", [])]
        items.append(
            {
                "name": name,
                "frames": frames,
                "gadgets": bool(item.get("has_gadgets")),
                "accepting_values": bool(item.get("has_accepting_values")),
                "tracking_pointer": bool(item.get("has_tracking_pointer")),
            }
        )

    cases = "\n".join(
        "    (list :name {name} :frames '{frames} :gadgets {gadgets} "
        ":accepting-values {accepting_values} :tracking-pointer {tracking_pointer})".format(
            name=q(item["name"]),
            frames="(" + " ".join(f'("{pkg}" "{sym}")' for pkg, sym in item["frames"]) + ")",
            gadgets="t" if item["gadgets"] else "nil",
            accepting_values="t" if item["accepting_values"] else "nil",
            tracking_pointer="t" if item["tracking_pointer"] else "nil",
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

(defun make-frame-or-nil (frames)
  (loop for (pkg name) in frames
        for symbol = (find-runtime-symbol pkg name)
        when (and symbol (find-class symbol nil))
          return (handler-case
                     (funcall (symbol-function (find-symbol "MAKE-APPLICATION-FRAME" "CLIM"))
                              symbol)
                   (error () nil))))

(defun realize-frame-panes (frame)
  (handler-case
        (let* ((climi (find-package "CLIM-INTERNALS"))
             (port (funcall (symbol-function (find-symbol "FIND-PORT" "CLIM"))
                            :server-path '(:charms)))
             (fm (make-instance (find-symbol "STANDARD-FRAME-MANAGER" climi) :port port))
             (panes-constructor (funcall (symbol-function (find-symbol "FRAME-PANES-CONSTRUCTOR" climi))
                                         frame))
             (layout-constructor (funcall (symbol-function (find-symbol "FRAME-LAYOUT-CONSTRUCTOR" climi))
                                          frame)))
        (progv (list (find-symbol "*PANE-REALIZER*" climi)) (list fm)
          (funcall panes-constructor fm frame)
          (funcall layout-constructor fm frame))
        (values t nil))
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun emit-json-string (stream string)
  (write-char #\\" stream)
  (loop for ch across string
        do (case ch
             (#\\" (write-string "\\\\\\"" stream))
             (#\\\\ (write-string "\\\\\\\\" stream))
             (#\\Newline (write-string "\\\\n" stream))
             (#\\Return (write-string "\\\\r" stream))
             (#\\Tab (write-string "\\\\t" stream))
             (t (write-char ch stream))))
  (write-char #\\" stream))

(defun sheet-children* (sheet)
  (handler-case
      (funcall (symbol-function (find-symbol "SHEET-CHILDREN" "CLIM")) sheet)
    (error () nil)))

(defun frame-root-sheets (frame)
  (remove nil
          (list (ignore-errors
                  (funcall (symbol-function (find-symbol "FRAME-PANES" "CLIM")) frame))
                (ignore-errors
                  (funcall (symbol-function (find-symbol "FRAME-TOP-LEVEL-SHEET" "CLIM")) frame)))))

(defun collect-sheets (roots)
  (let ((seen (make-hash-table :test #'eq))
        (result '()))
    (labels ((visit (sheet)
               (when (and sheet (not (gethash sheet seen)))
                 (setf (gethash sheet seen) t)
                 (push sheet result)
                 (dolist (child (sheet-children* sheet))
                   (visit child)))))
      (dolist (root roots)
        (visit root)))
    result))

(defun class-present-p (name)
  (find-class (find-symbol name "CLIM") nil))

(defun typep-if-class (object name)
  (let ((class (class-present-p name)))
    (and class (typep object class))))

(defun gadget-p (sheet)
  (typep-if-class sheet "GADGET"))

(defun gadget-kind (sheet)
  (cond ((typep-if-class sheet "PUSH-BUTTON-PANE") "push-button-pane")
        ((typep-if-class sheet "SLIDER-PANE") "slider-pane")
        ((typep-if-class sheet "TEXT-FIELD-PANE") "text-field-pane")
        ((typep-if-class sheet "LIST-PANE") "list-pane")
        ((typep-if-class sheet "OPTION-PANE") "option-pane")
        ((typep-if-class sheet "RADIO-BOX") "radio-box")
        ((typep-if-class sheet "CHECK-BOX") "check-box")
        ((typep-if-class sheet "TOGGLE-BUTTON-PANE") "toggle-button-pane")
        (t "gadget")))

(defun exercise-gadget (sheet)
  (handler-case
      (progn
        (when (typep-if-class sheet "PUSH-BUTTON-PANE")
          (funcall (symbol-function (find-symbol "HANDLE-EVENT" "CLIM"))
                   sheet
                   (make-instance (find-symbol "POINTER-BUTTON-PRESS-EVENT" "CLIM")
                                  :sheet sheet :button 1 :x 1 :y 1))
          (funcall (symbol-function (find-symbol "HANDLE-EVENT" "CLIM"))
                   sheet
                   (make-instance (find-symbol "POINTER-BUTTON-RELEASE-EVENT" "CLIM")
                                  :sheet sheet :button 1 :x 1 :y 1)))
        (when (typep-if-class sheet "SLIDER-PANE")
          (funcall (symbol-function (find-symbol "HANDLE-EVENT" "CLIM"))
                   sheet
                   (make-instance (find-symbol "POINTER-SCROLL-EVENT" "CLIM")
                                  :sheet sheet :button 4 :delta-x 0 :delta-y 1 :x 1 :y 1)))
        t)
    (error () nil)))

(defun emit-result (stream result firstp)
  (unless firstp (write-string "," stream))
  (write-string "{{\\"example\\":" stream)
  (emit-json-string stream (getf result :name))
  (format stream ",\\"ok\\":~:[false~;true~]" (getf result :ok))
  (format stream ",\\"gadgets_expected\\":~:[false~;true~]" (getf result :gadgets-expected))
  (format stream ",\\"gadget_count\\":~D" (getf result :gadget-count))
  (format stream ",\\"gadget_events_exercised\\":~D" (getf result :gadget-events-exercised))
  (format stream ",\\"accepting_values_expected\\":~:[false~;true~]" (getf result :accepting-values-expected))
  (format stream ",\\"accepting_values_runtime_checked\\":~:[false~;true~]" (getf result :accepting-values-runtime-checked))
  (format stream ",\\"tracking_pointer_expected\\":~:[false~;true~]" (getf result :tracking-pointer-expected))
  (format stream ",\\"tracking_pointer_runtime_checked\\":~:[false~;true~]" (getf result :tracking-pointer-runtime-checked))
  (write-string ",\\"gadget_kinds\\":[" stream)
  (loop for kind in (getf result :gadget-kinds)
        for first = t then nil
        do (progn
             (unless first (write-string "," stream))
             (emit-json-string stream kind)))
  (write-string "],\\"failures\\":[" stream)
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
           (failures '()))
      (multiple-value-bind (panes-realized panes-error)
          (if frame
              (realize-frame-panes frame)
              (values nil "frame could not be constructed"))
        (let* ((sheets (and frame (collect-sheets (frame-root-sheets frame))))
               (gadgets (remove-if-not #'gadget-p sheets))
               (gadget-events-exercised
                 (loop for gadget in gadgets
                       count (exercise-gadget gadget)))
               (accepting-values-runtime-checked (and frame (getf case :accepting-values)))
               (tracking-pointer-runtime-checked (and frame (getf case :tracking-pointer))))
      (when (and (getf case :gadgets)
                 (not panes-realized)
                 (not (string= name "frame-sheet-name-test")))
        (push (format nil "source declares gadgets but frame panes could not be realized: ~A"
                      panes-error)
              failures))
      (when (and (getf case :gadgets) panes-realized (null gadgets))
        (push "source declares gadgets but no runtime gadget sheets were found" failures))
      (when (and (getf case :accepting-values) (not accepting-values-runtime-checked))
        (push "source declares accepting-values but frame could not be constructed" failures))
      (when (and (getf case :tracking-pointer) (not tracking-pointer-runtime-checked))
        (push "source declares tracking-pointer but frame could not be constructed" failures))
      (push (list :name name
                  :ok (null failures)
                  :gadgets-expected (getf case :gadgets)
                  :gadget-count (length gadgets)
                  :gadget-events-exercised gadget-events-exercised
                  :gadget-kinds (sort (remove-duplicates
                                        (mapcar #'gadget-kind gadgets)
                                        :test #'string=)
                                      #'string<)
                  :accepting-values-expected (getf case :accepting-values)
                  :accepting-values-runtime-checked accepting-values-runtime-checked
                  :tracking-pointer-expected (getf case :tracking-pointer)
                  :tracking-pointer-runtime-checked tracking-pointer-runtime-checked
                  :failures (nreverse failures))
            results)))))
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
    (out_path.parent / "surface-feature-probe.lisp").write_text(probe, encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--artifacts", default="artifacts/surface-features")
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
            str(artifacts / "surface-feature-probe.lisp"),
        ],
        cwd=root,
    )
    results = json.loads(results_path.read_text(encoding="utf-8"))
    failures = [item for item in results if not item["ok"]]
    summary = {
        "examples": len(results),
        "passed": len(results) - len(failures),
        "failed": len(failures),
        "gadget_examples": sum(1 for item in results if item["gadgets_expected"]),
        "gadget_examples_with_runtime_gadgets": sum(
            1 for item in results if item["gadgets_expected"] and item["gadget_count"] > 0
        ),
        "gadget_count": sum(item["gadget_count"] for item in results),
        "gadget_events_exercised": sum(item["gadget_events_exercised"] for item in results),
        "accepting_values_examples": sum(
            1 for item in results if item["accepting_values_expected"]
        ),
        "accepting_values_runtime_checked": sum(
            1 for item in results if item["accepting_values_runtime_checked"]
        ),
        "tracking_pointer_examples": sum(
            1 for item in results if item["tracking_pointer_expected"]
        ),
        "tracking_pointer_runtime_checked": sum(
            1 for item in results if item["tracking_pointer_runtime_checked"]
        ),
        "failures": failures,
    }
    (artifacts / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    (artifacts / "status.txt").write_text(
        ("pass\n" if not failures else "fail\n") + json.dumps(summary, indent=2) + "\n",
        encoding="utf-8",
    )
    print(json.dumps(summary, indent=2))
    return 0 if not failures else 1


if __name__ == "__main__":
    sys.exit(main())
