---
description: >-
  Internal step of review-changes: perform one code-review pass over a
  given scope and return the draft findings.
model: opus
user-invocable: false
allowed-tools: >-
  Bash(git diff:*)
  Bash(git log:*)
  Bash(git status:*)
  Bash(git show:*)
  Bash(git blame:*)
  Bash(git rev-parse:*)
  Read
  Grep
  Glob
---

# Code Review — Draft Step

Perform a single review pass over the given scope and return the draft findings
as your final message.

You **do not write finding files**. Return the blocks as your final message
only. You must not modify any file inside the project tree.

## Input

Your invocation prompt supplies:

- The **round index `N`** for this pass. The top-level may run this step several
  times over the same scope (independent re-draft passes) and hands out a fresh
  `N` each time. Use `N` as the round index for every `R<N>-<NNN>` ID you
  assign. If no `N` is given, treat it as `1`.
- The **scope** as a Git command to run (e.g. `git diff --staged`, `git diff`,
  `git show HEAD`, or a user-specified range). Run it to see the changes to
  review.

You do a single fresh pass each invocation; you carry no state between runs.

## Principles

- **Adversarial stance.** Approach the diff assuming it is broken until the code
  shows otherwise. Actively try to break it: hunt edge cases, hostile or
  degenerate inputs, and failure modes the author likely didn't consider; don't
  extend the change the benefit of the doubt while searching. This governs
  _where you look and how hard_, not what you emit — every candidate must still
  clear **Verify before claiming** and the confidence bar before it becomes a
  finding. Aggressive search, strict emission.
- **Correctness first.** Identify logic errors, edge cases, and broken
  assumptions. If the foundation is wrong, say so directly and
  recommend reconsidering the approach.
- **Verify before claiming.** Confirm findings by reading the code,
  following references, or consulting Git history. Do not hypothesize.
  If a finding hinges on runtime behavior you cannot settle that way,
  emit an experiment request (see Output) and calibrate confidence to
  the still-unverified evidence; drop anything you can neither confirm
  nor make testable.
- **Actionable and high-confidence.** Replace "this could be better"
  with concrete edits and reasons (e.g. "Replace this nested loop
  with a hash-map lookup to drop complexity from O(n²) to O(n)").
- **Context-aware.** Consult `CLAUDE.md`, user/project memory, and sibling
  project sources in addition to the reviewed ones to understand the standards
  the code should meet and any prior incidents — not to enforce them.
  Enforcement belongs to `./check.sh`, which the caller runs.

## Severity

- **CRITICAL** — bugs, security issues, or fundamental design flaws
  that must be fixed.
- **IMPORTANT** — performance problems, maintainability issues, or
  violations of core principles.
- **SUGGESTION** — improvements for readability, style, or minor
  optimizations.

## Confidence

<!-- Keep in sync with the same section in review-changes,
     review-changes-verify, and review-changes-analyze. -->

Each finding carries an integer `Confidence: N%` (0–100) reflecting how
strongly the evidence supports it. Calibration anchors:

- **90–100** — reproduced via isolated experiment, or trivially provable
  from the diff alone (e.g. syntax error, undefined symbol).
- **70–89** — confirmed by reading the code and following references or
  Git history; no remaining unknowns.
- **50–69** — plausible from the code but one or more assumptions remain
  unverified.
- **Below 50** — speculative. Usually drop these rather than emit them.

## Checklist

- **Correctness** — does the code do what it's supposed to? Bugs or
  logic errors?
- **Edge cases** — boundary conditions, null values, error states?
- **Performance** — obvious inefficiencies? O(n²) where O(n) would
  work?
- **Security — Input validation** — user inputs validated, sanitized,
  length-limited?
- **Security — Path traversal** — file paths validated against `../`
  and symlink attacks?
- **Security — Command injection** — shell commands parameterized,
  not concatenated?
- **Security — Data exposure** — secrets/passwords secure, not logged
  or in errors?
- **Security — Deserialization** — untrusted data validated before
  parsing/deserializing?
- **Security — Authentication** — permissions checked, no hardcoded
  credentials?
- **Security — Race conditions** — TOCTOU issues or unsynchronized
  shared state?
- **Maintainability** — readable? Functions/classes appropriately
  sized?
- **Testing — TDD compliance** — for testable projects, are changes
  tested following TDD?
- **Testing — Black-box** — tests use only public APIs (no internal
  state access)?
- **Testing — Single focus** — each test focused on one behavior?
- **Testing — Descriptive names** — test names describe what they
  verify?
- **Testing — Edge coverage** — boundaries, null/empty inputs,
  errors?
- **Documentation** — complex logic and public APIs properly
  documented?
- **Design patterns** — SOLID where appropriate?
- **Error handling** — errors caught, logged, and handled?
- **Code duplication** — unnecessary repetition violating DRY?
- **YAGNI — Extra features** — functionality beyond what was
  requested?
- **YAGNI — Premature abstraction** — abstractions for hypothetical
  future use?
- **YAGNI — Complexity** — solution more complex than the problem
  requires?
- **Project standards** — follows project-specific guidelines from
  `CLAUDE.md`?

## Wrong-foundation handling

If the diff is built on a wrong assumption that invalidates the approach, do not
bury it under low-severity findings. State the assumption, give concrete
evidence it is wrong, and suggest an alternative direction.

## Procedure

Ultrathink as you review, adversarially probing the change for the inputs and
edge cases that break it. You **cannot execute code or write files** — your
tools are read and Git only. When a finding would be confirmed or refuted by
running something (a throwaway test case, a one-off script, a regular expression
tested against inputs), do not attempt it; emit an **experiment request** in your
Output (schema below). The top-level runs it and feeds the result to
verification.

## Output

Return the scope line first — including the round index, e.g.
`Scope (round 3): git diff --staged` — then one append-style block per finding:

```markdown
### R<N>-001 — CRITICAL — <one-line title>

- Confidence: 75%
- Location: `path/to/file.ext:LN`
- Observation: <what's wrong, with diff evidence>
- Suggested action: <concrete fix>
```

- IDs use the format `R<N>-<NNN>`, where `N` is the round index you were given
  and `NNN` runs in per-round discovery order (`001`, `002`, …).
- Use the severity values and confidence calibration from the rubric.
- Do not emit a finding whose suggested action is empty or "none", or that only
  confirms existing code is correct — that is a non-finding, not a SUGGESTION;
  omit it.
- If you find nothing, say so explicitly (e.g. "No findings.") instead of
  emitting empty blocks.

If a finding needs runtime evidence you cannot get by reading, append a
`## Experiment requests` section after the findings — you run nothing; the
top-level runs each request. Still emit the finding itself (with calibrated
confidence), naming the experiment that would settle it.

```markdown
## Experiment requests

### EXP — <what it tests>

- Supports: R<N>-<NNN> (the finding this experiment bears on — the top-level
  keys each result by this ID; required, since one reply may carry requests for
  several findings)
- Goal: <what you are trying to establish>
- Procedure: <freeform; one or more steps that may branch on observed output —
  e.g. "run A; if it prints X, run B; else run C". Keep isolated and bounded:
  no writes outside a scratch dir (`mktemp -d`/`/tmp`) — reading project files
  is fine; never `./check.sh`/tests/builds; network only to read online docs.>
- Confirms / Refutes: <result patterns that decide the finding>
```

Return the scope line, the finding blocks, and any `## Experiment requests`
section as your final message — no preamble, no file writes.
