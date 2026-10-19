---
name: inspect-changes-verify
description: >-
  Internal step of inspect-changes: verify one draft finding against the
  code and return its verdict.
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

# Code Review — Verification Step

Verify exactly one drafted finding and return its verdict as your reply.

Your reply — the verdict and any proposed new findings — goes to the reply
path your prompt supplies, in one Write, after which you end with
`Wrote <reply path>`. Write no other file; you must not modify any file inside
the project tree.

## Input

Your invocation prompt supplies, for the single finding you must verify:

- The finding **ID** and the path of the current draft file (JSON Lines, one
  finding per line) holding its finding.
- The **scope** as a Git command to run (e.g. `git diff --staged`, `git diff`,
  `git show HEAD`, or a user-specified range as the endpoint diff
  `git diff A..B`). Run it to see the reviewed change.
- The **pre-image baseline** to re-derive provenance against, if supplied.
- Paths of **existing prior drafts**. Use the
  [shared prior-draft guidance](../inspect-changes/references/prior-drafts.md)
  to screen issues discovered during verification.
- Any **experiment results** for this finding (the matching `EXP` blocks), if
  present.
- Any **caller requirements**, if present. Apply the
  [shared caller-requirements guidance](../inspect-changes/references/caller-requirements.md).
- The **reply path** your verdict reply goes to.

## Procedure

You **cannot execute code or write files** beyond your reply file — your tools
are read and Git only.

1. Independently confirm the finding by reading the code, following references,
   or consulting Git history. Do not hypothesize. Before deciding, run rule 3's
   in-image check from the [shared provenance
   guidance](../inspect-changes/references/provenance.md): a site in neither the
   pre-image nor the post-image is outside the reviewed content, and the rule's
   re-location half can move `final_location` onto in-image content that
   depends on it, which is why the check belongs here rather than after the
   outcome is fixed. A finding fails verification — verdict `drop` — in any of
   three ways: (a) it **cannot be confirmed** and no experiment would help (if
   an experiment would settle it, defer instead — see **Experiment requests**);
   (b) it is confirmed but **records no defect** — its refined observation
   identifies nothing wrong and its suggested action is empty or "none"; or (c)
   its site is in neither image and cannot be re-located onto in-image content,
   so it is outside the reviewed content — `drop` is your form of that rule's
   "omit the finding", since no provenance value describes such a site.
   Confirmation establishes that a finding is _true_; a true statement that
   prescribes no fix is a verification note, not a review finding, so `drop` it
   (give that as the reason) rather than keeping it as a zero-action
   SUGGESTION.
1. Re-derive the finding's confidence from the evidence per the
   [shared confidence guidance](../inspect-changes/references/confidence.md),
   rather than adjusting the draft's number. On either outcome the number
   grades the evidence for the finding's claim, not the verdict you reach
   about it. Give the evidence that settled it in `verification_trace`, and
   when your confidence falls in a different anchor band than the draft's, say
   so there too. A `keep` candidate whose final confidence lands below 50
   should usually become a `drop` — a gate on the number, not a fourth way of
   failing verification.
1. On `keep`, re-derive the finding's severity per the
   [shared severity guidance](../inspect-changes/references/severity.md), rather
   than inheriting the draft's grade. When your severity differs from the
   draft's, say so in `verification_trace` and give the consequence that
   settled it.
1. On `keep`, re-derive the finding's provenance per the
   [shared provenance guidance](../inspect-changes/references/provenance.md).
   Where that guidance sends you to attribution, read the [attribution
   procedure](../inspect-changes/references/provenance-attribution.md) before
   running any of its commands — several fail by printing a plausible wrong
   answer rather than an error. Yours is the only tier that runs them.
   Establish it yourself rather than inheriting the draft's tag; the draft's
   value is a claim to check, not a premise. When your value differs from the
   draft's, say so in `verification_trace` and give the evidence that settled
   it — that record is what the draft's tag is for.
1. Unless deferring (see **Experiment requests**), return one verdict in
   exactly the schema below. `final_confidence` is required on every verdict
   and `reason` on every `drop`; the severity, provenance, title, location,
   observation, and suggested-action fields may be omitted when `outcome` is
   `drop`.

## Output

Your reply is one JSON object, with nothing around it:

```json
{
  "verdict": {
    "id": "R<round>-<NNN>",
    "outcome": "keep",
    "final_severity": "CRITICAL",
    "final_confidence": 80,
    "final_provenance": "introduced",
    "final_title": "<one-line title>",
    "final_location": "`path/to/file.ext:LN`",
    "final_observation": "<refined, with evidence>",
    "final_suggested_action": "<concrete fix>",
    "verification_trace": "<what was checked to confirm/reject>",
    "reason": "<required on drop, explains why; optional on keep>"
  }
}
```

`outcome` is `keep` or `drop`; `final_severity` is `CRITICAL`, `IMPORTANT` or
`SUGGESTION`; `final_confidence` is an integer percentage from 0 to 100;
`final_provenance` is `introduced`, `pre-existing-on-path` or
`pre-existing-off-path`. Text fields may hold Markdown; JSON string escaping is
the only escaping they need.

Optionally add a `proposed_findings` list beside the verdict, listing
additional issues spotted while verifying. Follow the
[shared output-section contract](../inspect-changes/references/shared-output-sections.md).

## Experiment requests

Follow the [shared experiment-request format and safety
constraints](../inspect-changes/references/shared-output-sections.md). Request
runtime evidence in two cases:

- If the experiment is needed to **decide**, return the requests and omit
  `verdict` (a _deferral_) — the top-level runs them and re-invokes
  verification with the results.
- If the verdict is already settled but an experiment would aid the deeper
  analysis, return the verdict **and** the requests — the results flow to the
  analysis step.

Your experiments test a **finding's validity only**. You never author or ground
a remedy — attached requests exist solely to feed the analysis step, which never
re-invokes you — so remedy-feasibility experiments are reserved for the analysis
tier.
