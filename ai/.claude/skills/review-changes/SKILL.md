---
description: >-
  Review the most recent code changes — correctness, security, testing,
  YAGNI, project standards. Use after a substantive feature, bugfix, or
  refactor at a logical-commit size. Skip routine mechanical changes
  (comments, formatting, simple renames). Presents findings without
  acting.
model: opus
effort: max
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
  Agent
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

## Workflow: draft → verify → final

The review is produced in three phases. The top-level skill is the
**sole writer** of every file under `/tmp/review-changes-<topic>-*`.
Files are append-only.

`<topic>` is a 1–3-word kebab-case slug derived from the diff (module
name, feature, or commit subject). If any
`/tmp/review-changes-<topic>*` file already exists, suffix `<topic>`
with the smallest free integer (`-2`, `-3`, …) so a fresh review never
collides with an existing one.

### Phase 1 — Initial draft (round 1)

Perform the review per the principles, severity definitions, and
checklist above. Write findings to
`/tmp/review-changes-<topic>-draft-1.md` as append-only blocks:

```markdown
### R1-001 — CRITICAL — <one-line title>

- Location: `path/to/file.ext:LN`
- Observation: <what's wrong, with diff evidence>
- Suggested action: <concrete fix>
```

IDs use the format `R<round>-<NNN>`, with `NNN` in per-round
discovery order. Include the scope line at the top of the file.
If no findings, skip Phase 2 and proceed to Phase 3.

### Phase 2 — Verification rounds

For each round, spawn one verification subagent **per finding** in
the current draft file, in parallel (single message, multiple Agent
tool calls). The subagent contract is in **Verification subagents**
below.

Once the batch returns, validate each reply (rules below). Process
the valid replies:

1. Write all verdict blocks to
   `/tmp/review-changes-<topic>-verdicts-<round>.md`.
1. Dedup `Proposed new findings` against entries already in
   `draft-<round+1>.md` for this round, then within this batch —
   first occurrence wins; later duplicates dropped. Do **not**
   re-dedup against prior rounds; trust the subagent's own check
   against the prior draft files it was given.
1. Write the surviving proposals to
   `/tmp/review-changes-<topic>-draft-<round+1>.md` (created only
   if any survive), assigning `R<round+1>-<NNN>` IDs.

For each invalid reply, write nothing to disk; add the finding to
the retry set. After the batch is fully processed, if the retry set
is non-empty, dispatch a new parallel batch — one Agent call per
retry-set finding, same prompt as the original attempt.
Repeat until the retry set is empty. **Budget: 2 retries (3 attempts
total) per finding.** If any finding's third-attempt batch contains
an invalid reply, abort the review (see **Abort on retry exhaustion**
below).

This batch-then-retry pattern reflects foreground Agent dispatch:
parallel calls return as one batch, so retries naturally synchronize
at attempt boundaries. Under background dispatch (asynchronous
result delivery), retries can fire per finding as each result
arrives, and the batch boundaries here are relaxed.

Once every finding in `draft-<round>.md` has a valid verdict in the
verdicts file, if `draft-<round+1>.md` exists and is non-empty, run
another round on that new draft. Otherwise iteration stops.

Safety stop: **50 rounds.** This is a runaway-loop guard, not a
quality knob; convergence is expected far sooner. If round 50 still
produces new findings, stop and record the truncation in the final
summary.

#### Subagent reply validation

A reply is _unusable_ if any of the following holds:

1. Agent invocation returned an error or timeout.
1. Reply lacks a `## Verdict: <assigned-ID>` header for the
   finding's assigned ID.
1. Required field missing or empty. Always required: `Outcome:`,
   `Verification trace:`. Additionally required when `Outcome:` is
   `keep`: the five `Final …:` fields (severity, title, location,
   observation, suggested action).
1. `Outcome:` value is not `keep` or `drop`.
1. `Final severity:` (when present) is not `CRITICAL`, `IMPORTANT`,
   or `SUGGESTION`.
1. Reply truncates mid-bullet or before the `Verification trace:`
   line.

Detection is purely structural. The top-level does not judge verdict
quality, only schema conformance.

#### Abort on retry exhaustion

If a finding's attempt 3 reply also fails validation, the skill
aborts on that observation:

- No new retries are dispatched for any other finding.
- Other replies in the same returned batch are processed normally:
  valid replies contribute their verdicts and proposed new findings
  per Phase 2's regular flow; invalid replies contribute nothing.
- **Phase 3 does not run.** No `/tmp/review-changes-<topic>.md` is
  written.
- Return an abort message to the caller naming the exhausted
  finding(s) and pointing at the existing draft and verdicts files:

  ```text
  Review aborted in round <R>: finding <ID> exhausted retry budget
  (3 attempts, all failed). Inspect: /tmp/review-changes-<topic>-*
  ```

Other findings whose verdicts had already been written to the
verdicts file remain there — append-only is preserved, no rollback.
If a single returned batch contains multiple attempt-3 failures, all
are named in the caller message; the abort is still a single event.

### Phase 3 — Final assembly

Read every `verdicts-<round>.md` file. Write the final review to
`/tmp/review-changes-<topic>.md` containing every kept finding,
using this structure:

```markdown
# Code Review: <topic>

Scope: <staged | working tree | HEAD | range>

## Critical Issues

### R<round>-<NNN> — CRITICAL — <one-line title>

- Location: `path/to/file.ext:LN`
- Observation: <what's wrong, with diff evidence>
- Suggested action: <concrete fix>

## Important Findings

### R<round>-<NNN> — IMPORTANT — …

…

## Suggestions

### R<round>-<NNN> — SUGGESTION — …

…

## Summary

<2–4 sentences, plus: rounds run; total drafted; total kept; total
dropped; truncation note if the 50-round stop fired>
```

Assembly rules:

- **Keep the original IDs.** A finding written as `R2-004` stays
  `R2-004` in the final file. No renumbering.
- **Use the verdict's refined content, not the draft's.** Each
  block's severity, title, location, observation, and suggested
  action come verbatim from the verifier's verdict; these supersede
  the original draft text.
- Group by final severity (Critical → Important → Suggestion).
  Within a severity tier, order by `(round, NNN)` ascending.
- If a severity tier has no surviving findings, write "None." under it.
- Findings with `Outcome: drop` do **not** appear in the final file;
  they remain in their verdict file with a reason.

Do **not** modify the reviewed code. Return the path of the final
file and a brief summary to the caller. Leave draft and verdict files
in place for audit.

## Verification subagents

Each verification subagent verifies exactly one finding. They are
spawned in parallel via the Agent tool, with `subagent_type:
general-purpose`.

Subagents **do not write files**. They return their output as their
final message — both the verdict block and any proposed new findings.
Only the top-level skill writes to `/tmp/review-changes-<topic>-*`.

Each subagent must receive in its prompt:

- The verdict-block schema and the structural validation rules the
  reply must satisfy, copied verbatim from this skill.
- The finding ID and the full finding block from the draft.
- The scope as a Git command for the subagent to run (e.g.
  `git diff --staged`, `git show HEAD`).
- Paths of **all prior draft files** (`draft-1.md` …
  `draft-<round>.md`) for deduplication of any new findings it
  proposes.
- An explicit instruction that it must not write files and must not
  modify the project tree.

Small isolated experiments outside the project tree (e.g. in
`mktemp -d`) are fine if they sharpen the verdict; summarize the
experiment in the verification trace rather than writing a file the
top-level will not read.

The subagent must:

1. Independently confirm the finding by reading the code, following
   references, consulting Git history, or running an isolated
   experiment. Do not hypothesize. If it cannot be confirmed, the
   verdict is `drop`.
1. Return one verdict block in exactly this schema (omit
   `Final …:` lines on `Outcome: drop`):

   ```markdown
   ## Verdict: R<round>-<NNN>

   - Outcome: keep | drop
   - Final severity: CRITICAL | IMPORTANT | SUGGESTION
   - Final title: <one-line title>
   - Final location: `path/to/file.ext:LN`
   - Final observation: <refined, with evidence>
   - Final suggested action: <concrete fix>
   - Verification trace: <what was checked to confirm/reject>
   - Reason: <required on drop, explains why; optional on keep>
   ```

1. Optionally append a `## Proposed new findings` section after the
   verdict, listing additional issues spotted while verifying. Each
   entry must be a complete finding block (severity, title, location,
   observation, suggested action) **without an ID** — the top-level
   assigns IDs when it appends to the next draft. Before listing a
   proposal, the subagent must confirm it is not a duplicate of any
   finding in the prior draft files it was given; duplicates are
   dropped silently.

If a subagent's reply fails the **Subagent reply validation** rules
in Phase 2, the top-level re-spawns the subagent with the same
prompt, up to 2 retries (3 attempts total). If attempt 3 also fails,
the review aborts; the subagent itself never returns `Outcome:
error`.
