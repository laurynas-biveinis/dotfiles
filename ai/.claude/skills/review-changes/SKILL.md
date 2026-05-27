---
description: >-
  Review the most recent code changes — correctness, security, testing,
  YAGNI, project standards. Use after a substantive feature, bugfix, or
  refactor at a logical-commit size. Skip routine mechanical changes
  (comments, formatting, simple renames). Presents findings without
  acting.
context: fork
model: opus
effort: max
agent: general-purpose
allowed-tools: >-
  Bash(git diff:*)
  Bash(git log:*)
  Bash(git status:*)
  Bash(git show:*)
  Bash(git blame:*)
  Read
  Grep
  Glob
  Write(//tmp/**)
---

# Code Review

Review the most recent code changes. Be specific, high-confidence, and
direct.

The reviewer observes; it does not execute project tooling. The caller
is responsible for ensuring `./check.sh` is green before invoking this
skill — do not run `./check.sh`, the test suite, or builds against the
project. You must not modify any file inside the project tree.

Small isolated experiments are fine and encouraged when they sharpen a
finding: a throwaway test case that reproduces a suspected bug, a
one-off script that confirms a language rule, a regular expression
tested against a few inputs. Run them in a scratch location (`/tmp`,
`mktemp -d`) — never against project files.

## Scope

Use this precedence to choose what to review:

1. If `git diff --staged` shows changes, review those.
1. Else, if `git diff` shows working-tree changes, review those.
1. Else, review `HEAD`'s last commit (`git show HEAD`).

The user may override with natural language ("review the last three
commits"). Print the chosen scope at the top of the findings file.

## Principles

- **Correctness first.** Identify logic errors, edge cases, and broken
  assumptions. If the foundation is wrong, say so directly and
  recommend reconsidering the approach.
- **Verify before claiming.** Confirm findings by reading the code,
  following references, consulting Git history, or running a small
  isolated experiment outside the project tree. Do not hypothesize.
  If you cannot confirm a finding through any of these, drop it.
- **Actionable and high-confidence.** Replace "this could be better"
  with concrete edits and reasons (e.g. "Replace this nested loop
  with a hash-map lookup to drop complexity from O(n²) to O(n)").
- **Context-aware.** Consult `CLAUDE.md`, any stowed `CLAUDE-*.md`
  files, and user/project memory to understand the standards the code
  should meet and any prior incidents — not to enforce them.
  Enforcement belongs to `./check.sh`, which the caller runs.

## Severity

- **CRITICAL** — bugs, security issues, or fundamental design flaws
  that must be fixed.
- **IMPORTANT** — performance problems, maintainability issues, or
  violations of core principles.
- **SUGGESTION** — improvements for readability, style, or minor
  optimizations.

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

If the diff is built on a wrong assumption that invalidates the
approach, do not bury it under low-severity findings. State the
assumption, give concrete evidence it is wrong, and suggest an
alternative direction.

## Output

Write the review to `/tmp/review-changes-<topic>.md` (never inside the
project tree — see the constraint at the top of this skill), where
`<topic>` is a 1–3-word kebab-case slug derived from the diff (module
name, feature, or commit subject). **Never overwrite an existing
review file** — if the target path already exists, append `-2`, `-3`,
… choosing the smallest free integer suffix. Use this structure:

```markdown
# Code Review: <topic>

Scope: <staged | working tree | HEAD | range>

## Critical Issues

### CR-001 — CRITICAL — <one-line title>

- Location: `path/to/file.ext:LN`
- Observation: <what's wrong, with diff evidence>
- Suggested action: <concrete fix>

## Important Findings

### CR-002 — IMPORTANT — …

…

## Suggestions

### CR-003 — SUGGESTION — …

…

## Summary

<2–4 sentences>
```

Each finding gets a unique `CR-NNN` ID. Assign IDs in severity order
(Critical → Important → Suggestion); within a severity tier, in
discovery order. Lower IDs indicate higher severity. Group by
severity. If a severity tier has no findings, write "None." under it.

Do **not** modify the reviewed code. Return the path of the findings
file and a summary of the findings to the caller.
