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
- Paths of **existing prior drafts**. Use the
  [shared prior-draft guidance](../review-changes/references/prior-drafts.md)
  to screen issues discovered during verification.
- Any **experiment results** for this finding (the matching `EXP` blocks), if
  present.

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
1. Recalibrate confidence from the evidence: raise it on `keep` and lower it on
   `drop`; usually drop a candidate whose final confidence falls below 50.
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
listing additional issues spotted while verifying. Follow the
[shared output-section contract](../review-changes/references/shared-output-sections.md).

## Experiment requests

Follow the [shared experiment-request format and safety
constraints](../review-changes/references/shared-output-sections.md). Request
runtime evidence in two cases:

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
