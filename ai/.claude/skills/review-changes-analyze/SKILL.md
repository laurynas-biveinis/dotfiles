---
description: >-
  Internal step of review-changes: deeply analyze one kept finding and
  return its analysis block (or a rejection).
user-invocable: false
allowed-tools: >-
  Bash(git diff:*)
  Bash(git log:*)
  Bash(git status:*)
  Bash(git show:*)
  Bash(git blame:*)
  Bash(git merge-base:*)
  Bash(git rev-parse:*)
  Bash(git grep:*)
  Bash(git ls-files:*)
  Bash(git ls-tree:*)
  Read
  Grep
  Glob
---

# Code Review — Analysis Step

Analyze exactly one kept finding and return its analysis block as your final
message.

You **do not write finding files**. Return the analysis block — and any
rejection or proposed new findings — as your final message only. You must not
modify any file inside the project tree.

## Input

Your invocation prompt supplies, for the single finding you must analyze:

- The finding **ID** and its **verdict block** — the refined content from
  verification (`verdicts-<round>.md`), not the original draft.
- The **scope** as a Git command to run (e.g. `git diff --staged`, `git diff`,
  `git show HEAD`, or a user-specified range as the endpoint diff
  `git diff A..B`). Run it to see the reviewed change.
- The **pre-image baseline** to classify any `## Proposed new findings` entry
  against, if supplied — distinct from the placement `REV` below. The finding
  under analysis arrives with the verdict's `Final severity:`,
  `Final confidence:`, and `Final provenance:`; you re-assess all three in the
  Procedure below and change one only through `## Correction`, never by
  restating it in the analysis body.
- Any **caller requirements**, if present. Apply the
  [shared caller-requirements guidance](../review-changes/references/caller-requirements.md).
- **Only when a placement decision applies** (the caller supplied a stack and
  a `REV`): the **stack** as a list of SHA + subject, and the blame-target
  revision `REV`. The stack may be **empty** — nothing is amendable from this
  checkout — which is an answer rather than an absence: every owned defect then
  routes to a new commit, case (b). Only when the inputs are absent altogether
  do you omit any placement discussion. Under an
  uncommitted scope — `git diff --staged`, or a bare `git diff` over a clean
  index — `REV` is `HEAD` and only a `pre-existing-*` finding has a placement
  answer; an `introduced` one is WIP in the uncommitted change. There the
  finding's `Location:` line is a post-image number, so locate the defect in
  `REV` by content, and note that the index holds the reviewed change, so
  amending there is not a bare `git commit --amend`.
- Existing **prior draft paths**. Follow the
  [shared prior-draft guidance](../review-changes/references/prior-drafts.md)
  when filtering issues discovered during analysis.
- Any **experiment results** for this finding (the matching `EXP` blocks), if
  present.
- **Only when this is an alongside-analysis re-spawn:** the invocation mode
  `alongside` and the complete latest provisional analysis block (header and
  body, excluding routed level-2 sections). Revise that analysis using the new
  experiment results instead of reconstructing the analysis body from the
  verdict; re-assessing the verdict's values under Procedure step 3 is still
  required.

## Procedure

Ultrathink about this finding. You **cannot execute code or write files** — your
tools are read and Git only.

1. Analyze, research, and ultrathink about this finding. Restate the critique in
   your own words, examine what the code actually does, identify whether the
   finding addresses the root cause or a symptom, present alternative
   resolutions when more than one is reasonable, recommend one, and note
   anything the analysis does not change. Drop sections that do not apply — do
   not pad.
1. This finding was already confirmed by verification — treat it as valid and
   deepen it. But if your deeper analysis instead proves it a **false
   positive** — the code is actually correct, or the finding misreads the
   diff — append a `## Rejection` section (schema below) stating why; do not
   bury that conclusion in the analysis body. Rejection is all-or-nothing and
   removes the finding from the review, so reserve it for genuine false
   positives, not disagreements of emphasis or severity. You can only reject;
   you cannot revive a finding verification dropped.
1. Re-assess the verdict's **provenance** (per the
   [shared provenance guidance](../review-changes/references/provenance.md)),
   **severity** (per the
   [shared severity guidance](../review-changes/references/severity.md)), and
   **confidence** against what your deeper study found. When one of the three is
   wrong, append a `## Correction` section (schema below) carrying only the
   fields you are correcting, and state the reasoning in your analysis body as
   well: the body is rendered in the review, the correction's `Rationale:` is
   not. Correction is for a value the verdict got wrong, not a difference of
   taste; the finding itself stands. It cannot remove a finding — a confidence
   you correct below 50 keeps the finding in the review, so reject instead when
   it is a false positive. The other verdict fields (title, location,
   observation, suggested action) are not correctable: fold any refinement of
   those into your analysis body.
1. **Only when your prompt supplies the unpublished stack and blame-target
   `REV`:** if your analysis recommends a concrete code change, also recommend
   **where** to apply it within the unpublished stack. Identify the commit that
   **owns the region the fix touches**. First, if the finding's provenance is
   `introduced` and the scope is uncommitted, the fix lives in that uncommitted
   change: recommend (d) and skip the blame. Otherwise settle the removal
   question first: where the defect exists because something was **deleted** —
   the fix restores it — the deleting commit owns it and no surviving
   neighbour does, so blaming the neighbours places the fix on whoever happens
   to sit beside the hole. Find the deleting commit with
   `git log -m -p -S'<removed text>' <REV> -- <path>`, taking the newest hit
   whose diff shows the removal from the affected region. `-m` is not optional:
   `git log` computes no merge diffs by default, so a deletion authored by a
   merge resolution is otherwise invisible and ownership reads as unresolved.
   Two escalations follow, in order, before ownership counts as unresolved.
   `-S` counts a text's occurrences across the file, so a removal that
   preserves that count — a guard moved from one function to another in the
   same file — never hits it: re-run as
   `git log -m -p -G'<escaped pattern>' <REV> -- <path>`, which matches changed
   lines instead, and read each hit's deletion in the affected region, since
   `-G` also returns additions and unrelated matches. Then drop the pathspec —
   `git log -m -p -S'<removed text>' <REV>` — because `<path>` names the file
   at `REV` and so cannot reach a deletion made before a later rename; tie each
   candidate back to the affected region through the intervening history, never
   on matching text alone. A merge hit is not yet authorship either: enumerate
   its parents with `git rev-parse <merge>^@` and read
   `git diff <parent> <merge> -- <path>` for each, attributing the removal to
   the merge only where it deletes the content against **every** parent. Where
   it deletes against some parents and not others the merge inherited the
   deletion, and the branch that supplied it is the parent whose image
   **already lacks** the content in the affected region — not the parent the
   diff shows the removal against, which is the one that still had it. Where
   several parents already lack it, inspect each of their histories rather than
   naming an owner prematurely.
   Only where no removal is in play blame the affected lines at the supplied
   blame-target revision `REV` (not the working tree) with
   `git blame -L <start>,<end> <REV> -- <path>`, or use
   `git log -L <start>,<end>:<path> <REV>` — but `REV` need not hold the
   reviewed lines in final form, since under an uncommitted scope it is the
   pre-image, so the `Location:` numbers need not index it (see the **Input**
   bullet). Find the region's counterpart in `REV` by content and pass _those_
   numbers to `-L`. Where the target has no counterpart there — a mid-file
   insertion, an append past end-of-file, or a path absent from `REV`
   altogether — blame the nearest surrounding anchor line located by content
   the same way, which is the right owner for content that was always missing.
   Where nothing resolves, do **not** read that as the content being
   uncommitted: a committed removal leaves neither target nor anchor at `REV`
   while still owning the defect and still being amendable, so work back
   through the removal ladder above — `-S` on the path, `-G` on the path, then
   `-S` with no pathspec — and if ownership is still unresolved, say so rather
   than defaulting to (d). (a) If that commit is
   **one of the stack commits** and the fix corrects its own change, recommend
   amending it — name the specific SHA + subject, and say the recommendation
   holds only if that commit was never published, since the stack is a
   candidate list built from this checkout's remote-tracking refs rather than
   proof. (b) If that commit is
   **not in the stack** — already in trunk, or published on some other branch
   and so filtered out of it — recommend a new commit (never amend published
   history) and name its position (e.g. after `<sha> <subject>`, or at the
   stack tip). An empty stack lands here too: with nothing amendable, a new
   commit is the answer. (c) If the fix is a logically separate concern,
   recommend a new commit. (d) If the fix is best left uncommitted, recommend
   WIP. Emit this as a `**Suggested placement:**` bold-paragraph label in your
   analysis body —
   never as an ATX heading.
1. If, while analyzing, you discover a **new** issue not covered by the finding
   you were given, you **must** report it — do not silently drop it. Append it
   as a `## Proposed new findings` section after your analysis block (schema
   below). Confine that section to genuinely new issues; do not restate or
   re-scope the finding under analysis — unless you are rejecting it, in which
   case a materially different claim about that defect and location is a
   genuinely new issue and the gate admits it.

## Output

Return the analysis block in this schema, beginning your reply with the
`#### Analysis: <ID>` header line (no preamble before it):

```markdown
#### Analysis: R<round>-<NNN>

<freeform body — recommended subsections, all optional:
Restated critique / What the code actually does /
Root cause vs symptom / Options / Recommendation /
Suggested placement (only when given the unpublished-commit stack) /
What this analysis does not change.
Emit the heading above as the
first line of the reply, at exactly level 4 (four `#`) — the level
it occupies in the final review, where the analysis body is copied
verbatim under the finding. Do not emit any further ATX
(`#`-prefixed) heading in the body; use bold-paragraph labels
(e.g. **Recommendation:**)
for subsections instead, so nothing outranks the `#### Analysis`
heading or breaks the document outline. Quoting code that contains
`#` lines inside a fenced code block is fine — those are not
headings; the prohibition is only on real ATX headings outside
fences.>
```

If — and only if — analysis proves the finding a false positive, append a
`## Rejection` section after the analysis block, giving the reason. The analysis
body is optional in this case (the reason carries the rationale):

```markdown
## Rejection

<why the kept finding is invalid — the code is actually correct,
or the finding misreads the diff>
```

If — and only if — the verdict's severity, confidence, or provenance is wrong,
append a `## Correction` section after the analysis block, listing only the
fields you are correcting. `Rationale:` is required; the corrected values
supersede the verdict's in the final review:

```markdown
## Correction

- Corrected severity: IMPORTANT
- Corrected confidence: 60%
- Corrected provenance: pre-existing-off-path
- Rationale: <why the verdict's value is wrong>
```

A `## Correction` qualifies an analysis, so it is valid only on a reply that
carries an analysis block: never on a deferral, and never with a `## Rejection`
(a finding you remove has no values left to correct).

If — and only if — analysis surfaced a genuinely new issue, append a
`## Proposed new findings` section after the analysis block, following the
[shared output-section contract](../review-changes/references/shared-output-sections.md).

`## Rejection`, `## Correction`, `## Proposed new findings`, and
`## Experiment requests` are the only higher-level (`##`) headings allowed in a
reply that carries an analysis block. A reply may carry more than one — a
rejecting analyst that also spotted a genuinely different issue still reports it
— except that `## Rejection` may never co-occur with `## Experiment requests`
(rejecting is terminal; see **Experiment requests** below) or with
`## Correction`.

## Experiment requests

Follow the [shared experiment-request format and safety
constraints](../review-changes/references/shared-output-sections.md). Request
runtime evidence when it would sharpen the analysis — **including whether a
suggested action or one of your options is actually feasible given the tooling,
environment, or APIs, not only whether the finding is valid**. Return the
section in one of two shapes:

- **Deferral** — requests and **no** analysis block: you cannot decide the
  finding yet and need the evidence first. The top-level runs the experiments
  and re-invokes you with the results.
- **Alongside an analysis** — requests _plus_ your completed analysis block:
  your analysis stands, but a suggested action (or option) asserts a
  tooling/environment capability you have not verified. The top-level runs the
  experiments and re-invokes you with the results, invocation mode `alongside`,
  and your complete latest provisional analysis block so you can finalize that
  suggested action without losing its context. When re-invoked in this mode, you
  have three valid replies: return your **finalized analysis** with no further
  requests (if the results settle it); attach **another `## Experiment requests`
  section** alongside your updated analysis block (if they surface a further
  unverified capability); or, if the results instead disprove the finding,
  **reject it** with a `## Rejection` section and no experiment requests. Do
  **not** return a pure deferral — once you have produced an alongside analysis,
  always carry an analysis block (or a rejection) forward; requests with no
  analysis block in this context are invalid. You are re-spawned fresh, and no
  earlier `## Correction` is handed back to you — the provisional block excludes
  routed level-2 sections. So re-run Procedure step 3 on every reply of this
  loop **except a rejection** (a finding you remove has no values left to
  correct) and re-emit a `## Correction` if one is still warranted; only the
  correction on the reply the caller ends up appending is recorded. Note that
  the caller does not retry an alongside re-spawn: any validation failure
  terminates the loop immediately and falls back to your latest provisional
  analysis (the 3-attempt budget governs only the initial dispatch and pure
  deferrals), so when uncertain prefer returning a finalized analysis over
  attaching yet another experiment batch.

**Mandate:** if your recommendation hinges on an unverified capability of the
tooling, environment, or APIs — e.g. whether a parameter, keyword, or config
actually has the claimed effect — do **not** present it as an unvalidated option
or punt its feasibility to the reader. Either defer, or attach the experiment
alongside your analysis, so the remedy you recommend is grounded.

A `## Rejection` is incompatible with **either** shape: rejecting is a terminal
decision, so you cannot also request experiments — never combine a `## Rejection`
with a `## Experiment requests` section.
