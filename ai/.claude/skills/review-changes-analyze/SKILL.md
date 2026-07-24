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
  Bash(git rev-parse:*)
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
  `git show HEAD`, or a user-specified range). Run it to see the reviewed change.
- **Only when a placement decision applies** (committed scope with a non-empty
  unpublished stack): the **stack** as a list of SHA + subject, and the
  blame-target revision `REV`. When these are absent, omit any placement
  discussion.
- Paths of **the prior draft files that exist**, so you can self-suppress
  proposals that duplicate an already-raised finding. This is best-effort; the
  top-level dedups authoritatively against all raw findings (kept, dropped, or
  rejected alike).
- Any **experiment results** for this finding (the matching `EXP` blocks), if
  present.

## Confidence

<!-- Keep in sync with review-changes, review-changes-step, and
     review-changes-verify. -->

Each finding carries an integer `Confidence: N%` (0–100) reflecting how strongly
the evidence supports it. Calibration anchors:

- **90–100** — reproduced via isolated experiment, or trivially provable from
  the diff alone (e.g. syntax error, undefined symbol).
- **70–89** — confirmed by reading the code and following references or Git
  history; no remaining unknowns.
- **50–69** — plausible from the code but one or more assumptions remain
  unverified.
- **Below 50** — speculative. Usually drop these rather than emit them.

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
1. **Only when your prompt supplies the unpublished stack and blame-target
   `REV`:** if your analysis recommends a concrete code change, also recommend
   **where** to apply it within the unpublished stack. Identify the commit that
   **owns the region the fix touches**: blame the affected lines at the supplied
   blame-target revision `REV` (not the working tree) with
   `git blame -L <start>,<end> <REV> -- <path>`, or use
   `git log -L <start>,<end>:<path> <REV>`. Where the fix's exact target does
   not exist at `REV` (e.g. an append past end-of-file), blame the nearest
   surrounding anchor line within that region instead. (a) If that commit is
   **one of the stack commits** and the fix corrects its own change, recommend
   amending it — name the specific SHA + subject. (b) If that commit is
   **already in trunk** (not in the stack), recommend a new commit (never amend
   published history) and name its position (e.g. after `<sha> <subject>`, or at
   the stack tip). (c) If the fix is a logically separate concern, recommend a
   new commit. (d) If the fix is best left uncommitted, recommend WIP. Emit this
   as a `**Suggested placement:**` bold-paragraph label in your analysis body —
   never as an ATX heading.
1. If, while analyzing, you discover a **new** issue not covered by the finding
   you were given, you **must** report it — do not silently drop it. Append it
   as a `## Proposed new findings` section after your analysis block (schema
   below). Confine that section to genuinely new issues; do not restate or
   re-scope the finding under analysis.

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

If — and only if — analysis surfaced a genuinely new issue, append a
`## Proposed new findings` section after the analysis block. Each entry must be
a complete finding block (severity, confidence, title, location, observation,
suggested action) **without an ID** — the top-level assigns IDs when it appends
to the next draft. Before listing a proposal, confirm it is not a duplicate of
any finding in the prior draft files you were given; duplicates are dropped
silently. The top-level re-dedups authoritatively, so this check is best-effort.

```markdown
## Proposed new findings

### CRITICAL — <one-line title>

- Confidence: 70%
- Location: `path/to/file.ext:LN`
- Observation: <what's wrong, with evidence>
- Suggested action: <concrete fix>
```

`## Rejection`, `## Proposed new findings`, and `## Experiment requests` are the
only higher-level (`##`) headings allowed in a reply that carries an analysis
block. A reply may carry more than one — a rejecting analyst that also spotted a
genuinely different issue still reports it — except that `## Rejection` and
`## Experiment requests` may never co-occur (rejecting is terminal; see
**Experiment requests** below).

## Experiment requests

You **must not** run experiments. If runtime evidence would sharpen the
analysis — **including whether a suggested action or one of your options is
actually feasible given the tooling, environment, or APIs, not only whether the
finding is valid** — return a `## Experiment requests` section whose every entry
is a `### EXP — <what it tests>` header block (the header is required) giving a
goal, a freeform procedure whose commands may branch on output, and what
confirms/refutes (a finding's validity **or a remedy's feasibility**). The entry
need not name the finding — the top-level attributes the requests to the finding
you were invoked to analyze. Return the section in one of two shapes:

- **Deferral** — requests and **no** analysis block: you cannot decide the
  finding yet and need the evidence first. The top-level runs the experiments
  and re-invokes you with the results.
- **Alongside an analysis** — requests _plus_ your completed analysis block:
  your analysis stands, but a suggested action (or option) asserts a
  tooling/environment capability you have not verified. The top-level runs the
  experiments and re-invokes you with the results so you can finalize that
  suggested action. When re-invoked with results, you have three valid replies:
  return your **finalized analysis** with no further requests (if the results
  settle it); attach **another `## Experiment requests` section** alongside your
  updated analysis block (if they surface a further unverified capability); or,
  if the results instead disprove the finding, **reject it** with a
  `## Rejection` section and no experiment requests. Do **not** return a pure
  deferral — once you have produced an alongside analysis, always carry an
  analysis block (or a rejection) forward; requests with no analysis block in
  this context are invalid. Note that the caller does not retry an alongside
  re-spawn: any validation failure terminates the loop immediately and falls
  back to your latest provisional analysis (the 3-attempt budget governs only
  the initial dispatch and pure deferrals), so when uncertain prefer returning a
  finalized analysis over attaching yet another experiment batch.

**Mandate:** if your recommendation hinges on an unverified capability of the
tooling, environment, or APIs — e.g. whether a parameter, keyword, or config
actually has the claimed effect — do **not** present it as an unvalidated option
or punt its feasibility to the reader. Either defer, or attach the experiment
alongside your analysis, so the remedy you recommend is grounded.

A `## Rejection` is incompatible with **either** shape: rejecting is a terminal
decision, so you cannot also request experiments — never combine a `## Rejection`
with a `## Experiment requests` section.

Keep each procedure isolated and bounded: no writes outside a scratch dir
(`mktemp -d`/`/tmp`) — reading project files is fine; never
`./check.sh`/tests/builds; network only to read online docs.

```markdown
## Experiment requests

### EXP — <what it tests>

- Goal: <what you are trying to establish>
- Procedure: <freeform; one or more steps that may branch on observed output>
- Confirms / Refutes: <result patterns that decide the finding, or the
  feasibility of a proposed remedy>
```
