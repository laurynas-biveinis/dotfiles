---
description: >-
  Review the most recent code changes — correctness, security, testing,
  YAGNI, project standards. Use after a substantive feature, bugfix, or
  refactor at a logical-commit size. Skip routine mechanical changes
  (comments, formatting, simple renames). Presents findings without
  acting.
model: sonnet
effort: max
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
  Write(//tmp/**)
  Agent
  Skill(review-changes-step)
  Skill(review-changes-verify)
---

# Code Review

Review the most recent code changes. Be specific, high-confidence, and
direct.

The reviewer observes; it does not execute project tooling. The caller
is responsible for ensuring `./check.sh` is green before invoking this
skill — do not run `./check.sh`, the test suite, or builds against the
project. You must not modify any file inside the project tree.

The draft/verify/analyze subagents never execute code; when one needs runtime
evidence it emits an **experiment request**. The top-level (you) is the sole
executor: you run each request in a scratch location (`/tmp`, `mktemp -d`),
bounded by the isolation rules in **Experiment requests** below — which are the
mandatory safety control.

## Scope

Use this precedence to choose what to review:

1. If `git diff --staged` shows changes, review those.
1. Else, if `git diff` shows working-tree changes, review those.
1. Else, review `HEAD`'s last commit (`git show HEAD`).

The user may override with natural language ("review the last three
commits"). Print the chosen scope at the top of the findings file.

## Confidence

<!-- Keep in sync with the same section in review-changes-step and
     review-changes-verify. -->

Each finding carries an integer `Confidence: N%` (0–100) reflecting how
strongly the evidence supports it. Calibration anchors:

- **90–100** — reproduced via isolated experiment, or trivially provable
  from the diff alone (e.g. syntax error, undefined symbol).
- **70–89** — confirmed by reading the code and following references or
  Git history; no remaining unknowns.
- **50–69** — plausible from the code but one or more assumptions remain
  unverified.
- **Below 50** — speculative. Usually drop these rather than emit them.

## Experiment requests

No subagent runs code. When the draft, a verifier, or an analyst needs runtime
evidence, it appends a `## Experiment requests` section to its reply — one entry
per experiment, each giving a goal, a freeform procedure (commands that may
branch on observed output), and what confirms or refutes the finding. The
top-level reads each request as freeform and runs it; it does not parse a rigid
schema, so the request format is defined only where requests are produced (the
`review-changes-step` and `review-changes-verify` skills, and the analyze
contract below).

The draft always returns its findings in one reply, with any experiment
requests attached alongside; the top-level runs those and feeds the results to
Phase 2's verifiers. A verifier or analyst may instead reply with experiment
requests and **no** verdict/analysis — a _deferral_ — after which the top-level
runs them and re-spawns that subagent with the results (Phase 2/3). (A verifier
may also attach requests to a finished verdict; see Phase 2.)

**Isolation rules** — the requester must comply, and the top-level enforces them
as a safety net on **every** concrete command before running it:

- No writes outside a scratch dir (`mktemp -d`/`/tmp`) — reading project files
  is fine; never `./check.sh`/tests/builds. Network only to read online docs.
- Bounded and interpretable against Confirms/Refutes.

**Run routine** (invoked whenever any tier returns requests):

1. For each request, execute its Procedure adaptively — run a step, observe the
   output, follow the request's branch logic to choose the next step. Before
   running each concrete command, enforce the isolation rules: if a command
   violates them, record `unsafe` and skip it. Otherwise run it; if a command is
   refused or fails, that branch ends (`denied`). Capture the full step/output
   trace.
1. Append each outcome to `/tmp/review-changes-<topic>-experiments.md` (the
   top-level is its sole writer) as an
   `### EXP-<n> — supports <finding-ID> — <ok | denied | unsafe>` block with the
   executed step/output trace and the Confirms/Refutes conclusion.
1. Experiments are **never load-bearing**: `denied`/`unsafe`/failed simply means
   the consuming tier proceeds on read/Git evidence and calibrates confidence
   lower. They never abort the review.

Splitting a reply on its `## Experiment requests` header uses the same
CommonMark-aware rule as `## Proposed new findings`: split on the first such
header that occurs as a true top-level line (outside any fenced code block or
block quote); a reply may carry both sections.

A request section is _parseable_ when it contains at least one `### EXP` entry
(header present, body non-empty). A `## Experiment requests` header with no such
entry is not parseable. This is the minimal recognition floor — not a rigid
schema; the entry's own fields stay freeform per the producer contracts.

## Workflow: draft → (verify ⇄ analyze) → final

The review is produced in four phases. The top-level skill is the
**sole writer** of every file under `/tmp/review-changes-<topic>-*`.
Files are append-only.

The workflow is not linear. Verification can surface new findings that
re-trigger verification (Phase 2's own loop), and analysis can surface
new findings too: emitting a new draft in Phase 3 re-enters Phase 2 for
those findings, the same way verification's new drafts already do. The
two loops share one flat round counter and converge on the same
terminal condition — a draft that yields no new findings.

Verification decides the fate of every drafted finding (`keep`/`drop`);
analysis holds one **asymmetric** power on top of that: it may **reject**
a finding verification kept, when deeper study proves it a false positive,
which removes it from the final review. Analysis can only reject — it never
resurrects a dropped finding. So a finding reaches the final review only if
verification kept it **and** analysis did not reject it.

`<topic>` is a 1–3-word kebab-case slug derived from the diff (module
name, feature, or commit subject). If any
`/tmp/review-changes-<topic>*` file already exists, suffix `<topic>`
with the smallest free integer (`-2`, `-3`, …) so a fresh review never
collides with an existing one.

### Phase 1 — Initial draft (round 1)

Determine the scope (per **Scope** above) as a Git command and invoke the
`review-changes-step` skill, passing the scope command as its argument. It
returns the round-1 draft — the scope line and `R1-<NNN>` finding blocks.

Validate the reply structurally: it must contain parseable `R1-<NNN>`
blocks (or an explicit no-findings statement) and must not truncate
mid-block. If the reply is unusable, re-invoke `review-changes-step` with
the same arguments. **Budget: 2 retries (3 attempts total).** If attempt
3 also fails, abort the review — write no `draft-1.md` — and return an
abort message to the caller in the style of **Abort on retry
exhaustion** below:

```text
Review aborted in round 1: initial draft step exhausted retry budget
(3 attempts, all failed).
```

On a valid reply, split off any `## Experiment requests` section (per the
splitting rule in **Experiment requests**), then write the finding blocks
verbatim to `/tmp/review-changes-<topic>-draft-1.md` as append-only blocks, with
the scope line at the top — the top-level remains the **sole writer** of all
`/tmp/review-changes-*` files. IDs use the format `R<round>-<NNN>`, with `NNN` in
per-round discovery order; the step assigns the round-1 IDs.

If the draft carried experiment requests, run them now via the **Experiment
requests** run routine, recording results in
`/tmp/review-changes-<topic>-experiments.md` keyed to the `R1-<NNN>` each
supports; Phase 2 feeds each finding's results into its verifier.

If the step reports no findings, skip Phases 2 and 3 and proceed to
Phase 4.

### Phase 2 — Verification rounds

For each round, invoke `review-changes-verify` once **per finding** in
the current draft file, in parallel (single message, multiple
`Skill(review-changes-verify)` calls). That skill holds the per-finding
verification contract; build each invocation's prompt to supply the one
finding's ID and full block, the scope as a Git command, the paths of
all prior draft files (`draft-1.md` … `draft-<round>.md`) for dedup, and
any experiment results (the matching `EXP` blocks) for that finding.

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
is non-empty, dispatch a new parallel batch — one
`Skill(review-changes-verify)` invocation per retry-set finding, same
prompt as the original attempt.
Repeat until the retry set is empty. **Budget: 2 retries (3 attempts
total) per finding.** If any finding's third-attempt batch contains
an invalid reply, abort the review (see **Abort on retry exhaustion**
below).

This batch-then-retry pattern reflects foreground fork-skill dispatch:
parallel invocations return as one batch, so retries naturally
synchronize at attempt boundaries. Under background dispatch
(asynchronous result delivery), retries can fire per finding as each
result arrives, and the batch boundaries here are relaxed.

Once every finding in `draft-<round>.md` has a valid verdict in the
verdicts file, if `draft-<round+1>.md` exists and is non-empty, run
another round on that new draft. Otherwise iteration stops.

Safety stop: **50 iterations.** This bounds, under a single cap, every
iteration the review performs — each draft opened (verification rounds
_and_ those Phase 3 analysis emits) _and_ each experiment-request re-spawn
(Phase 2 or 3). It is a runaway-loop guard, not a quality knob;
convergence is expected far sooner. If the cap is hit while iterations
are still producing new findings or experiment requests, stop and record
the truncation in the final summary.

#### Subagent reply validation

A verifier reply contains a verdict, or a `## Experiment requests` section, or
both (see **Experiment deferrals**). The rules below judge the verdict part; a
reply with a well-formed request section and no verdict (a deferral) is not
unusable. A verdict (when present) is _unusable_ if any of the following holds:

1. Agent invocation returned an error or timeout.
1. Reply lacks a `## Verdict: <assigned-ID>` header for the
   finding's assigned ID.
1. Required field missing or empty. Always required: `Outcome:`,
   `Final confidence:`, `Verification trace:`. Additionally required
   when `Outcome:` is `keep`: `Final severity:`, `Final title:`,
   `Final location:`, `Final observation:`, `Final suggested action:`.
1. `Outcome:` value is not `keep` or `drop`.
1. `Final severity:` (when present) is not `CRITICAL`, `IMPORTANT`,
   or `SUGGESTION`.
1. `Final confidence:` is not an integer in `[0, 100]` (e.g. missing
   `%`, non-numeric, out of range).
1. Reply truncates mid-bullet or before the `Verification trace:`
   line.

Detection is purely structural. The top-level does not judge verdict
quality, only schema conformance.

#### Experiment deferrals

Run any `## Experiment requests` a verifier returns via the **Experiment
requests** run routine, appending results to the experiments file. Then:

- **Requests with no verdict (a deferral):** the verifier needs the results to
  decide — re-spawn it; the rebuilt prompt's "Any experiment results for this
  finding" bullet now carries the new `EXP` blocks, so nothing is separately
  appended.
- **Requests alongside a verdict:** process the verdict normally (no re-spawn);
  the experiment results stay in the experiments file and flow to the analyst
  for that finding in Phase 3 (via the analyst prompt's same bullet). This lets
  a verifier that has already decided still queue evidence the deeper analysis
  will want.

Each re-spawn is a normal main-loop iteration counted under the single
50-iteration safety cap — there is **no** separate experiment budget. A request
section that is not parseable (see **Experiment requests**) counts as an invalid
reply under the retry budget.

#### Abort on retry exhaustion

If a finding's attempt 3 reply also fails validation, the skill
aborts on that observation:

- No new retries are dispatched for any other finding.
- Other replies in the same returned batch are processed normally:
  valid replies contribute their verdicts and proposed new findings
  per Phase 2's regular flow; invalid replies contribute nothing.
- **Phases 3 and 4 do not run.** No `/tmp/review-changes-<topic>.md`
  is written.
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

### Phase 3 — Analysis of kept findings

After all verification rounds have converged, collect every finding
with `Outcome: keep` across all `verdicts-<round>.md` files **that has
not already been analyzed in a prior Phase 3 pass**. The set of
analyzed finding IDs is derived from disk, not held in memory: an ID
counts as analyzed once it has an analysis block, an exhaustion marker,
or a rejection marker (`<!-- analysis-rejected: <ID> -->`) in
`/tmp/review-changes-<topic>-analyses.md` (see below). A finding is
analyzed exactly once even though Phase 3 may run several times. Spawn
one analysis subagent **per not-yet-analyzed kept
finding**, in parallel (single message, multiple Agent tool calls). The
subagent contract is in **Analysis subagents** below.

On first entry to Phase 3, compute the **unpublished-commit stack context**
once and reuse it for every analysis subagent across all later verify⇄analyze
passes — no commits are made during a review, so it is stable.

Placement only makes sense when the reviewed lines are themselves committed —
i.e. the scope is `git show HEAD` or a user-specified commit range. Under
staged or working-tree scope the reviewed change is uncommitted, so any fix to
it is WIP and there is **no placement decision**: skip the computation below,
omit placement context from the subagent prompts, and run the rest of Phase 3
unchanged.

For committed scope, compute the stack with allowed commands only:

- Trunk branch = `main` if `git rev-parse --verify --quiet main` succeeds, else
  `master` if `git rev-parse --verify --quiet master` succeeds.
- Stack = `git log --oneline <trunk>..HEAD` — the local commits not yet
  contained in trunk (each entry is a SHA + subject). If that range is empty
  _and_ HEAD is the trunk branch itself — i.e.
  `git rev-parse --abbrev-ref HEAD` equals `<trunk>` (not the literal `HEAD`
  of a detached checkout) — (you committed directly on trunk), recompute the
  stack as `git log --oneline <trunk>@{upstream}..HEAD` when an upstream exists
  (`git rev-parse --verify --quiet <trunk>@{upstream}` succeeds): those
  un-pushed commits are still amendable. With no upstream configured, leave the
  stack empty.
- Blame-target revision `REV` — the newest reviewed revision whose tree holds
  the reviewed lines in final form: `HEAD` for `git show HEAD` scope, or the
  right-hand endpoint `B` of an `A..B`/`A...B` range (`HEAD` for the common
  "last N commits" case). `REV` is a pure function of the already-chosen scope
  and is identical for every analysis subagent, so compute it once here rather
  than having each subagent re-derive it.

If no trunk branch exists, or the stack is still empty after the upstream
fallback above — e.g. HEAD is already merged into trunk, or HEAD is a trunk
branch with nothing un-pushed — there is again **no placement decision**: omit
placement context from the subagent prompts and run the rest of Phase 3
unchanged. Otherwise, pass the stack and `REV` into each analysis subagent
prompt (see **Analysis subagents**).

Each analysis reply has up to three parts: the `#### Analysis: <ID>`
block, optionally followed by a `## Rejection` section and/or a
`## Proposed new findings` section (both level-2 headers). **Split the
reply on the first occurrence of each such header that occurs as a true
top-level line — outside any fenced code block or block quote**, using
the same CommonMark-aware rule. A subagent that merely quotes a delimiter
inside a fence (e.g. when reviewing this skill's own schema) does not
trigger the split. Everything before the first such boundary is the
analysis body; each routed section is handled below and is **never
inlined** into the final review (so no level-2 header ever outranks the
`#### Analysis` heading in the assembled document).

A `## Rejection` section means the subagent has proven this kept finding a
false positive: it is **removed** from the final review (the same end state
as a verifier `drop`). Verification keeps drafted findings; analysis can
additionally reject one but never resurrects a drop.

Validate each reply with the rules in **Analysis subagent reply
validation**. For each valid reply:

- **If it carries a `## Rejection` section,** append a rejection marker for
  its ID to `/tmp/review-changes-<topic>-analyses.md` — an inert HTML
  comment `<!-- analysis-rejected: <ID> -->` followed by the rejection
  reason verbatim — instead of an analysis body. The marker mirrors the
  `<!-- analysis-skipped:` exhaustion marker: the ID counts as analyzed
  (terminal — never re-dispatched), and Phase 4 reads it to exclude the
  finding. Do **not** append the analysis body (the finding is gone, so
  there is nothing to inline). A `## Proposed new findings` section on the
  same reply is still processed by the loop below.
- **Otherwise (no rejection),** append its analysis body verbatim — the
  `#### Analysis: <ID>` header line (already at the level it occupies in
  the final review) and the body below it, up to but excluding any
  `## Rejection` or `## Proposed new findings` header.

The append-only file `/tmp/review-changes-<topic>-analyses.md` is written
solely by the top-level (analysis subagents still never write files).
Phase 4 reads the bodies and markers back from this file rather than from
memory, so a body produced in an early pass survives intact across any
number of later verify⇄analyze rounds. Invalid replies enter a retry set;
dispatch a new parallel batch, same prompts. **Budget: 2 retries (3
attempts total) per finding.**

**Exhaustion is non-fatal here.** Analysis augments findings and may
reject one (above), but its _absence_ is never load-bearing: a finding
with no usable analysis reply simply **falls back to its verifier
`keep`** — exhaustion never infers a rejection. If a finding's attempt 3
reply also fails validation, append an exhaustion marker for its ID to
`/tmp/review-changes-<topic>-analyses.md` — an inert HTML comment
`<!-- analysis-skipped: <ID> (retry exhaustion) -->`, so the ID counts
as analyzed and is never re-dispatched on a later pass — then continue.
Phase 4 will include the finding (kept) without an Analysis subsection and
will note the omission in the Summary. Do not abort the review.

Once every not-yet-analyzed finding in the batch has either a valid
reply or has exhausted its retry budget, process the proposed new
findings from the valid replies:

1. Collect the `## Proposed new findings` entries from all valid
   replies in the batch.
1. Dedup them against **all previous raw findings** — every
   `draft-<round>.md` file, whether the finding was ultimately kept or
   dropped — then within this batch (first occurrence wins; later
   duplicates dropped). Unlike Phase 2, which trusts each verification
   subagent's own dedup against the prior drafts it was handed, Phase 3
   re-dedups authoritatively at the top level against all raw findings:
   analysis proposals can recur across passes and must never re-litigate
   any previously raised finding. The two loops differ deliberately, not
   by oversight — Phase 2's incidental verification proposals are
   low-recurrence (a verifier surfaces a new issue only as a side effect
   of adjudicating its assigned finding), so subagent self-check
   suffices, whereas analysis subagents actively hunt for new issues
   across repeated passes, making recurrence likely enough to warrant
   the authoritative re-dedup.
1. Write the survivors to a new `/tmp/review-changes-<topic>-draft-<N>.md`
   (next free flat round index `N`, created only if any survive),
   assigning `R<N>-<NNN>` IDs.

**Loop-back trigger.** If a new draft was created, re-enter Phase 2 to
verify it — verification runs to convergence as usual, possibly opening
further drafts — then return to Phase 3 to analyze the findings it kept
(skipping any already analyzed). Repeat until an analysis pass produces
no surviving new draft. Then proceed to Phase 4. The 50-iteration safety
cap bounds the combined loop.

If there are no kept findings at all, skip Phase 3 entirely and
proceed to Phase 4.

#### Analysis subagent reply validation

A reply is _unusable_ if any of the following holds:

1. Agent invocation returned an error or timeout.
1. Reply lacks a `#### Analysis: <assigned-ID>` header (exactly four
   `#`) for the finding's assigned ID.
1. Body under the header (before any `## Rejection` or
   `## Proposed new findings` section) is empty or whitespace-only —
   **unless** the reply carries a `## Rejection` section, in which case the
   analysis body is optional (the rejection reason is the content).
1. A `## Rejection` section is present but empty or whitespace-only (it
   must carry a reason).
1. A `## Rejection` section and a `## Experiment requests` section are
   both present. The two are mutually exclusive: a rejection is a
   terminal decision, whereas a deferral means the analyst has not yet
   decided and needs evidence to do so — a reply that both rejects and
   requests experiments is contradictory and must never happen.
1. The analysis body (everything before any `## Rejection` or
   `## Proposed new findings` delimiter) contains an ATX heading — a
   `#`-prefixed line per CommonMark (a `#` run with ≤3 leading spaces,
   outside any fenced code block or code span) — other than the leading
   `#### Analysis:`. A `#` line a body legitimately quotes inside a fenced
   code block (e.g. a shell comment or `#!` shebang) is not a heading and
   does not trip this rule.
1. Reply truncates mid-sentence or mid-bullet.

Carrying a `## Rejection` or `## Proposed new findings` section does
**not** by itself make a reply unusable — `## Proposed new findings` is
expected when the subagent spots a new issue, and `## Rejection` when it
proves the finding a false positive. Detection is structural but
CommonMark-aware: it must honor fenced-code-block and code-span
boundaries, so `#`/`##` lines a body quotes inside a fence are treated as
neither headings nor section delimiters. The top-level does not judge
analysis quality, only schema conformance.

An analyst that needs runtime evidence may instead return a `## Experiment
requests` section and **no** analysis block. Handle the happy path as in
Phase 2's **Experiment deferrals**: run the requests via the run routine, append
results, and re-spawn the analyst — the rebuilt prompt's "Any experiment results
for this finding" bullet then carries the new `EXP` blocks, so nothing is
separately appended. Each re-spawn is a main-loop iteration under the single
50-iteration safety cap — no separate experiment budget. The edge cases follow
Phase 3's own **non-fatal** handling, not Phase 2's abort: a reply carrying both
an analysis block and requests is treated as an analysis (requests ignored); a
reply carrying both a `## Rejection` and requests is malformed and counts as an
invalid analysis reply under Phase 3's retry budget (per the validation rule
above); and a deferral whose request section is not parseable (see **Experiment
requests**) likewise counts as an invalid analysis reply under that budget
(→ exhaustion marker, never abort).
If the cap is hit while the analyst is still deferring, proceed without the
experiment and treat it as the analysis-exhaustion case (exhaustion marker, noted
in the Summary).

### Phase 4 — Final assembly

Read every `verdicts-<round>.md` file and the analysis bodies and markers
from `/tmp/review-changes-<topic>-analyses.md`. First compute the
**rejected-ID set**: every ID with a `<!-- analysis-rejected: <ID> -->`
marker in the analyses file. Using the kept verdicts and the analysis
bodies, write the final review to `/tmp/review-changes-<topic>.md`
containing every kept finding **except those in the rejected-ID set**,
using this structure:

```markdown
# Code Review: <topic>

Scope: <staged | working tree | HEAD | range>

## Critical Issues

### R<round>-<NNN> — CRITICAL — <one-line title>

- Confidence: 85%
- Location: `path/to/file.ext:LN`
- Observation: <what's wrong, with diff evidence>
- Suggested action: <concrete fix>

#### Analysis: R<round>-<NNN>

<verbatim analysis body — restated critique, root cause, options,
recommendation, caveats; whatever the subagent produced>

## Important Findings

### R<round>-<NNN> — IMPORTANT — …

…

## Suggestions

### R<round>-<NNN> — SUGGESTION — …

…

## Summary

<2–4 sentences, plus: drafts opened; verify⇄analyze passes run (how
many times analysis fed findings back to verification); total drafted;
total kept; total dropped; findings rejected on analysis (list IDs, if
any); total analyzed; findings proposed by analysis (and how many
survived dedup); analyses skipped due to retry exhaustion (list IDs, if
any); truncation note if the 50-iteration stop fired>
```

Assembly rules:

- **Keep the original IDs.** A finding written as `R2-004` stays
  `R2-004` in the final file. No renumbering.
- **Use the verdict's refined content, not the draft's.** Each
  block's severity, confidence, title, location, observation, and
  suggested action come verbatim from the verifier's verdict; these
  supersede the original draft text.
- **Inline analysis.** For each kept finding, copy its analysis body
  from `/tmp/review-changes-<topic>-analyses.md` verbatim immediately
  after the `Suggested action:` bullet. A finding's body in that file
  runs from its `#### Analysis: <ID>` line up to (but excluding) the
  next `#### Analysis:` line, the next line beginning with the
  `<!-- analysis-skipped:` or `<!-- analysis-rejected:` marker prefix, or
  EOF — whichever comes first. Treating those marker prefixes as
  delimiters keeps an interleaved exhaustion or rejection marker from
  being swept into a preceding body. Body extraction also never _begins_
  inside a rejection span: the text from a `<!-- analysis-rejected: <ID> -->`
  marker up to the next `#### Analysis:` line, marker prefix, or EOF is
  owned by that rejection (its verbatim reason) and is scanned neither as
  its own body nor as any other finding's — so a `#### Analysis:` line a
  reason happens to quote is never mistaken for a body anchor. The body
  already opens with its
  own `#### Analysis: <ID>` heading at the right level, so the top-level
  adds no heading and strips nothing further (Phase 3 already excluded
  any `## Rejection` or `## Proposed new findings` section when it appended
  the body) — do not edit, summarize, or re-level it. Analysis subagents
  emit that one level-4 heading and otherwise use bold-paragraph labels
  instead of `#`-prefixed headings (see **Analysis subagents**), so nothing
  in the body outranks the `#### Analysis` heading or breaks the document
  outline. If a kept finding has no analysis body in the file (only an
  exhaustion or rejection marker, or nothing), omit the analysis for that
  finding.
- Group by final severity (Critical → Important → Suggestion).
  Within a severity tier, order by `(round, NNN)` ascending.
- If a severity tier has no surviving findings, write "None." under it.
- Findings with `Outcome: drop` do **not** appear in the final file;
  they remain in their verdict file with a reason.
- Findings in the rejected-ID set (rejected on analysis) do **not** appear
  either; their `<!-- analysis-rejected: <ID> -->` marker and reason remain
  in the analyses file. A finding rejected on analysis is excluded even
  though its verdict is `keep`.

Do **not** modify the reviewed code. Return the path of the final
file and a brief summary to the caller. Leave draft and verdict files
in place for audit.

## Analysis subagents

Each analysis subagent analyzes exactly one kept finding. They are
spawned in parallel via the Agent tool, with `subagent_type:
general-purpose` and `model: opus`.

Analysis subagents **do not write files**. They return the analysis
as their final message. Only the top-level skill writes to
`/tmp/review-changes-<topic>-*`.

Each subagent must receive in its prompt:

- The finding's assigned ID and its **verdict block** (the refined
  content from `verdicts-<round>.md`, not the draft).
- The scope as a Git command for the subagent to run (e.g.
  `git diff --staged`, `git show HEAD`).
- **Only when a placement decision applies** (committed scope with a non-empty
  stack — see Phase 3): the stack as a list of SHA + subject, and the
  blame-target revision `REV` (computed once in Phase 3).
- The analysis schema and the structural validation rules the reply
  must satisfy.
- Paths of **all prior draft files** (`draft-1.md` …
  `draft-<round>.md`), so it can self-suppress proposals that
  duplicate an already-raised finding. This is best-effort only; the
  top-level dedups authoritatively against all raw findings.
- Any experiment results for this finding (the matching `EXP` blocks from
  the experiments file), if present.
- An explicit instruction that it must not write files and must not
  modify the project tree.
- The instruction: "Analyze, research, and ultrathink about this
  finding. Restate the critique in your own words, examine what the
  code actually does, identify whether the finding addresses the
  root cause or a symptom, present alternative resolutions when more
  than one is reasonable, recommend one, and note anything the
  analysis does not change. Drop sections that do not apply — do not
  pad."
- The instruction: "This finding was already confirmed by verification —
  treat it as valid and deepen it. But if your deeper analysis instead
  proves it a **false positive** — the code is actually correct, or the
  finding misreads the diff — append a `## Rejection` section (schema
  below) stating why; do not bury that conclusion in the analysis body.
  Rejection is all-or-nothing and removes the finding from the review, so
  reserve it for genuine false positives, not disagreements of emphasis or
  severity. You can only reject; you cannot revive a finding verification
  dropped."
- **Only when a placement decision applies** (see Phase 3), the instruction:
  "If your analysis recommends a concrete code change, also recommend
  **where** to apply it within the unpublished stack. Identify the commit that
  **owns the region the fix touches**: blame the affected lines at the supplied
  blame-target revision `<REV>` (not the working tree) with
  `git blame -L <start>,<end> <REV> -- <path>`, or use
  `git log -L <start>,<end>:<path> <REV>`. Where the fix's exact target does
  not exist at `<REV>` (e.g. an append past end-of-file), blame the nearest
  surrounding anchor line within that region instead. (a) If that commit is
  **one of the stack commits** and the fix corrects its own change,
  recommend amending it — name the specific SHA + subject. (b) If that
  commit is **already in trunk** (not in the stack), recommend a new commit
  (never amend published history) and name its position (e.g. after
  `<sha> <subject>`, or at the stack tip). (c) If the fix is a logically
  separate concern, recommend a new commit. (d) If the fix is best left
  uncommitted, recommend WIP. Emit this as a `**Suggested placement:**`
  bold-paragraph label in your analysis body — never as an ATX heading."
- The instruction: "If, while analyzing, you discover a **new** issue
  not covered by the finding you were given, you **must** report it —
  do not silently drop it. Append it as a `## Proposed new findings`
  section after your analysis block (schema below). Confine that
  section to genuinely new issues; do not restate or re-scope the
  finding under analysis."

The analyst must **not** run experiments. If runtime evidence would
sharpen the analysis, it returns an experiment-request deferral instead:
a `## Experiment requests` section whose every entry is a
`### EXP — <what it tests>` header block (the header is required) giving a
goal, a freeform procedure whose commands may branch on output, and what
confirms or refutes the finding — and **no** analysis block and **no**
`## Rejection` section. A deferral and a rejection are mutually exclusive:
deferring means the analyst has not yet decided and needs the evidence first,
so it cannot also reject — return one or the other, never both. The entry need
not name the finding; the top-level attributes the requests to the finding
this subagent was spawned to analyze. The top-level runs it and re-spawns the
analyst with the result.

The subagent must return the analysis block in this schema, beginning
its reply with the `#### Analysis: <ID>` header line (no preamble
before it):

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
`## Rejection` section after the analysis block, giving the reason. The
analysis body is optional in this case (the reason carries the rationale):

```markdown
## Rejection

<why the kept finding is invalid — the code is actually correct,
or the finding misreads the diff>
```

If — and only if — analysis surfaced a genuinely new issue, append a
`## Proposed new findings` section after the analysis block. Each entry
must be a complete finding block (severity, confidence, title, location,
observation, suggested action) **without an ID** — the top-level assigns
IDs when it appends to the next draft:

```markdown
## Proposed new findings

### CRITICAL — <one-line title>

- Confidence: 70%
- Location: `path/to/file.ext:LN`
- Observation: <what's wrong, with evidence>
- Suggested action: <concrete fix>
```

`## Rejection` and `## Proposed new findings` are the only higher-level
(`##`) headings allowed in a reply that carries an analysis block; the
top-level splits the reply on each, inlines the analysis block (unless
rejected), records the rejection, and routes any proposals into the loop.
A reply may carry both — a rejecting analyst that also spotted a genuinely
different issue still reports it. Before listing a proposal, the subagent
should confirm it is not a duplicate of any finding in the prior draft
files it was given; duplicates are dropped silently. The top-level
re-dedups authoritatively, so this check is best-effort.

If the reply fails the **Analysis subagent reply validation** rules
in Phase 3, the top-level re-spawns the subagent with the same
prompt, up to 2 retries (3 attempts total). If attempt 3 also
fails, the top-level drops the analysis for that finding (Phase 3
non-fatal exhaustion); the subagent itself never returns an error
outcome.
