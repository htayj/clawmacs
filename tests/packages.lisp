(defpackage :clawmacs/tests
  (:use :cl :fiveam :clawmacs))

(in-package :clawmacs/tests)

(def-suite clawmacs-suite
  :description "All clawmacs tests")

(def-suite faces-suite
  :description "Face system tests"
  :in clawmacs-suite)

(def-suite message-suite
  :description "Message and line tests"
  :in clawmacs-suite)

(def-suite buffer-suite
  :description "Buffer tests"
  :in clawmacs-suite)

(def-suite windows-suite
  :description "Logical window tree tests"
  :in clawmacs-suite)

(def-suite commands-suite
  :description "Command system tests"
  :in clawmacs-suite)

(def-suite matching-suite
  :description "Minibuffer matching tests"
  :in clawmacs-suite)

(def-suite projects-suite
  :description "Project resource abstraction tests"
  :in clawmacs-suite)

(def-suite skills-suite
  :description "Skill discovery and prompt injection tests"
  :in clawmacs-suite)

(def-suite sexed-suite
  :description "Agent-oriented s-expression editing tests"
  :in clawmacs-suite)

(def-suite slop-suite
  :description "Agent-oriented Common Lisp symbol lookup tests"
  :in clawmacs-suite)

(def-suite git-package-suite
  :description "Agent-oriented git package tests"
  :in clawmacs-suite)

(def-suite organa-package-suite
  :description "Org-mode TODO project management package tests"
  :in clawmacs-suite)

(def-suite netcons-suite
  :description "Agent-oriented web lookup package tests"
  :in clawmacs-suite)

(def-suite prove-package-suite
  :description "Agent-oriented self-testing package tests"
  :in clawmacs-suite)

(def-suite speculum-package-suite
  :description "Agent-oriented McCLIM self-visibility package tests"
  :in clawmacs-suite)

(def-suite subagent-package-suite
  :description "Agent-oriented subagent package tests"
  :in clawmacs-suite)

(def-suite templata-package-suite
  :description "Slash command and prompt template package tests"
  :in clawmacs-suite)

(def-suite quaestor-package-suite
  :description "Structured user-question and queued delivery package tests"
  :in clawmacs-suite)

(def-suite modelaria-package-suite
  :description "Scoped model-role and usage package tests"
  :in clawmacs-suite)

(def-suite artifactum-package-suite
  :description "Attachment and durable artifact package tests"
  :in clawmacs-suite)

(def-suite interop-suite
  :description "App-server, JSONL, and structured-output interop tests"
  :in clawmacs-suite)

(def-suite package-manager-suite
  :description "Package loader tests"
  :in clawmacs-suite)

(def-suite guard-suite
  :description "Approval policy and sandbox preset tests"
  :in clawmacs-suite)

(def-suite reference-suite
  :description "Common Lisp spec and local library discovery tests"
  :in clawmacs-suite)

(def-suite llm-suite
  :description "LLM helper tests"
  :in clawmacs-suite)
