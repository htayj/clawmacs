# KVC Design Docs

## Common Lisp Standards & Corpus Program (Living Section)

This section defines the Common Lisp engineering posture for KVC's Codex Vector.

### Core Standards (v0.1)

1. **Functional-first architecture**
   - Prefer pure functions and referentially transparent APIs.
   - Isolate side effects at the edges (I/O, network, filesystem, process control).

2. **Immutability by default**
   - Prefer immutable data flow and persistent structures where practical.
   - Mutability must be explicit, local, and justified in comments/docstrings.

3. **Static typing discipline**
   - Use strict SBCL declarations in all Common Lisp code (`declaim`, `ftype`, local `declare`).
   - Use Coalton for core modules that benefit from stronger compile-time guarantees.
   - Build with safety- and warning-strict settings in CI.

4. **Metaphor-rich design communication**
   - Design docs should pair formal specs with metaphors that improve transfer of intuition.
   - Metaphors are explanatory scaffolding, not substitutes for precise contracts.

5. **Skills and MCP integration**
   - Build a reusable library of Common Lisp skills (automation patterns, tool wrappers, quality gates).
   - Build MCP servers/tooling in Common Lisp where feasible to expand ecosystem capability.

6. **Canonical corpus of excellent Common Lisp code**
   - Curate a living corpus of exemplary projects, snippets, and patterns.
   - For each corpus entry: include source URL, license, why it is exemplary, and what standard it demonstrates.

### Standards Backlog (to evolve)

- Style guide: naming, package boundaries, condition system conventions.
- Type contract style sheet (function signatures + declaration templates).
- Immutability checklist and mutation exception policy.
- Macro hygiene and code-walker policies.
- Testing profile (unit/property/integration/perf).
- Performance profile (allocation budgets, compiler optimization declarations).
- Security profile for networked tools and MCP servers.

### Research Inputs (current)

- C2 Common Lisp corpus snapshot: `research/common-lisp/c2-wiki/`
- Lead harvesting is performed continuously by hourly automation.

### Exit Criteria for v1.0 Standards

- Written standards accepted for functional style, immutability, static typing, and metaphor usage.
- At least 50 high-quality corpus entries with annotations.
- At least 3 Common Lisp skills and 1 Common Lisp MCP implementation documented.
- CI checks enforce a baseline declaration/type policy.
