---
name: inspect-changes-step
description: >-
  Internal step of inspect-changes: perform one code-review pass over a
  given scope and return the draft findings.
user-invocable: false
allowed-tools: >-
  Bash(git diff:*)
  Bash(git log:*)
  Bash(git status:*)
  Bash(git show:*)
  Bash(git blame:*)
  Bash(git merge-base:*)
  Bash(git rev-parse:*)
  Bash(git symbolic-ref --quiet --no-recurse HEAD)
  Bash(git grep:*)
  Bash(git ls-files:*)
  Bash(git ls-tree:*)
  Read
  Grep
  Glob
  Write(//tmp/**)
---

# Code Review — Draft Step

Perform a single review pass over the given scope and return the draft findings
as your reply.

Write your reply, in one Write, to the reply path your prompt supplies, then end
with `Wrote <reply path>`. Write nothing else, and never modify the project
tree.

## Input

Your invocation prompt supplies:

- The **round index `N`** for this pass. The top-level may run this step several
  times over the same scope (independent re-draft passes) and hands out a fresh
  `N` each time. Use `N` as the round index for every `R<N>-<NNN>` ID you
  assign. If no `N` is given, treat it as `1`.
- The **scope** as a Git command to run (e.g. `git diff --staged`, `git diff`,
  `git show HEAD`, or a user-specified range as the endpoint diff
  `git diff A..B`). Run it to see the changes to review.
- The **pre-image baseline** to tag provenance against, if supplied.
- Any **caller requirements**, if present. Apply the
  [shared caller-requirements guidance](../inspect-changes/references/caller-requirements.md).
- The **reply path** to write this pass's draft to.

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

As you review, adversarially probe the change for the inputs and edge cases
that break it. You **cannot execute code or write files** beyond your reply
file — your tools are read and Git only. When a finding would be confirmed or
refuted by running something (a throwaway test case, a one-off script, a regular
expression tested against inputs), do not attempt it; emit an **experiment
request** in your Output (schema below). The top-level runs it and feeds the
result to verification.

## Output

Your reply is one JSON object, with nothing around it:

```json
{
  "findings": [
    {
      "id": "R<N>-001",
      "severity": "CRITICAL",
      "title": "<one-line title>",
      "confidence": 75,
      "provenance": "introduced",
      "location": "`path/to/file.ext:LN`",
      "observation": "<what's wrong, with diff evidence>",
      "suggested_action": "<concrete fix>"
    }
  ]
}
```

- `findings` is required: an empty list says the pass found nothing.
- IDs use the format `R<N>-<NNN>`, where `N` is the round index you were given
  and `NNN` runs in per-round discovery order (`001`, `002`, …).
- `severity` is `CRITICAL`, `IMPORTANT` or `SUGGESTION`; `confidence` is an
  integer percentage from 0 to 100; `provenance` is `introduced`,
  `pre-existing-on-path` or `pre-existing-off-path`. The text fields may hold
  Markdown such as code spans, lists and fenced blocks; JSON string escaping is
  the only escaping they need.
- Grade every finding's severity per the
  [shared severity guidance](../inspect-changes/references/severity.md).
- Calibrate every finding's confidence per the
  [shared confidence guidance](../inspect-changes/references/confidence.md).
- Classify every finding's provenance per the
  [shared provenance guidance](../inspect-changes/references/provenance.md),
  which says when the scope's diff settles it, when Git history is needed, and
  how cheap a first-pass tag may be. Tag it from the diff and a cheap baseline
  read; never run history attribution at this tier.
- Your severity, confidence, and provenance are all first-pass claims.
  Verification re-derives each independently and its answers are the ones that
  reach the review, so yours are not inputs to them — each is the claim a
  verifier records disagreeing with, which is what makes an ambiguous case
  visible in the audit trail rather than silently resolved. One exception for
  provenance: see the shared provenance guidance's **Tie-breaks**.
- Do not emit a finding whose suggested action is empty or "none", or that only
  confirms existing code is correct — that is a non-finding, not a SUGGESTION;
  omit it.
- If you could not run the scope command or read the change, do not report no
  findings: add `"review_failed": "<reason>"`, and the pass is re-run.

If a finding needs runtime evidence you cannot get by reading, add an
`experiment_requests` list — you run nothing; the top-level runs each request.
Still emit the finding itself (with calibrated confidence), naming the
experiment that would settle it. Each request is an object:

```json
{
  "title": "<what it tests>",
  "supports": "R<N>-<NNN>",
  "goal": "<what you are trying to establish>",
  "procedure": "<freeform steps that may branch on observed output>",
  "confirms_refutes": "<result patterns that decide the finding>"
}
```

- `supports` names the finding the experiment bears on. It is required, since
  one reply may carry requests for several findings, and the top-level keys
  each result by it.
- Keep the procedure isolated and bounded: no writes outside a scratch dir
  (`mktemp -d`/`/tmp`) — reading project files is fine; never
  `./check.sh`/tests/builds; network only to read online docs.
