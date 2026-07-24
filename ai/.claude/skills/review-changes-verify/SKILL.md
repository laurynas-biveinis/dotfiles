---
description: >-
  Internal step of review-changes: verify one draft finding against the
  code and return its verdict block.
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

# Code Review — Verification Step

Verify exactly one drafted finding and return its verdict block as your final
message.

You **do not write finding files**. Return the verdict block — and any proposed
new findings — as your final message only. You must not modify any file inside
the project tree.

## Input

Your invocation prompt supplies, for the single finding you must verify:

- The finding **ID** and its full finding block from the current draft.
- The **scope** as a Git command to run (e.g. `git diff --staged`, `git diff`,
  `git show HEAD`, or a user-specified range). Run it to see the reviewed change.
- Paths of **the prior draft files that exist**, so you can self-suppress
  proposals that duplicate an already-raised finding. This is best-effort; the
  top-level dedups authoritatively against all raw findings (kept, dropped, or
  rejected alike).
- Any **experiment results** for this finding (the matching `EXP` blocks), if
  present.

## Confidence

<!-- Keep in sync with review-changes, review-changes-step, and
     review-changes-analyze. -->

Each finding carries an integer `Confidence: N%` (0–100) reflecting how strongly
the evidence supports it. Calibration anchors:

- **90–100** — reproduced via isolated experiment, or trivially provable from
  the diff alone (e.g. syntax error, undefined symbol).
- **70–89** — confirmed by reading the code and following references or Git
  history; no remaining unknowns.
- **50–69** — plausible from the code but one or more assumptions remain
  unverified.
- **Below 50** — speculative. Usually drop these rather than emit them.

Raise the confidence of `keep` findings and lower it on `drop`; a `keep`
candidate that falls below 50 should usually become a `drop`.

## Procedure

Ultrathink while verifying this finding. You **cannot execute code or write
files** — your tools are read and Git only.

1. Independently confirm the finding by reading the code, following references,
   or consulting Git history. Do not hypothesize. A finding fails verification —
   verdict `drop` — in either of two ways: (a) it **cannot be confirmed** and no
   experiment would help (if an experiment would settle it, defer instead — see
   **Experiment requests**); or (b) it is confirmed but **records no defect** —
   its refined observation identifies nothing wrong and its suggested action is
   empty or "none". Confirmation establishes that a finding is _true_; a true
   statement that prescribes no fix is a verification note, not a review finding,
   so `drop` it (give that as the reason) rather than keeping it as a zero-action
   SUGGESTION.
1. Return one verdict block in exactly the schema below. `Final confidence:` is
   required on every verdict; the severity, title, location, observation, and
   suggested-action lines may be omitted on `Outcome: drop`.

## Output

Return one verdict block:

```markdown
## Verdict: R<round>-<NNN>

- Outcome: keep | drop
- Final severity: CRITICAL | IMPORTANT | SUGGESTION
- Final confidence: <0–100>%
- Final title: <one-line title>
- Final location: `path/to/file.ext:LN`
- Final observation: <refined, with evidence>
- Final suggested action: <concrete fix>
- Verification trace: <what was checked to confirm/reject>
- Reason: <required on drop, explains why; optional on keep>
```

Optionally append a `## Proposed new findings` section after the verdict,
listing additional issues spotted while verifying. Each entry must be a complete
finding block (severity, confidence, title, location, observation, suggested
action) **without an ID** — the top-level assigns IDs when it appends to the
next draft. Before listing a proposal, confirm it is not a duplicate of any
finding in the prior draft files you were given; duplicates are dropped
silently. The top-level re-dedups authoritatively, so this check is best-effort.

```markdown
## Proposed new findings

### CRITICAL — <one-line title>

- Confidence: 70%
- Location: `path/to/file.ext:LN`
- Observation: <what's wrong, with evidence>
- Suggested action: <concrete fix>
```

## Experiment requests

You **must not** run experiments. If runtime evidence would help, append a
`## Experiment requests` section whose every entry is a `### EXP — <what it
tests>` header block (the header is required) giving a goal, a freeform
procedure whose commands may branch on output, and what confirms/refutes; the
top-level runs it. The entry need not name the finding — the top-level
attributes the requests to the finding you were invoked to verify. Two cases:

- If the experiment is needed to **decide**, return the requests with **no**
  verdict (a _deferral_) — the top-level runs them and re-invokes verification
  with the results.
- If the verdict is already settled but an experiment would aid the deeper
  analysis, return the verdict **and** the requests — the results flow to the
  analysis step.

Your experiments test a **finding's validity only**. You never author or ground
a remedy — attached requests exist solely to feed the analysis step, which never
re-invokes you — so remedy-feasibility experiments are reserved for the analysis
tier.

Keep each procedure isolated and bounded: no writes outside a scratch dir
(`mktemp -d`/`/tmp`) — reading project files is fine; never
`./check.sh`/tests/builds; network only to read online docs.

```markdown
## Experiment requests

### EXP — <what it tests>

- Goal: <what you are trying to establish>
- Procedure: <freeform; one or more steps that may branch on observed output>
- Confirms / Refutes: <result patterns that decide the finding>
```
