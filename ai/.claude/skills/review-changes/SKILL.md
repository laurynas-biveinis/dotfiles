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

<!-- Keep in sync with the same section in review-changes-step,
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

## Experiment requests

No subagent runs code. When the draft, a verifier, or an analyst needs runtime
evidence, it appends a `## Experiment requests` section to its reply — one entry
per experiment, each giving a goal, a freeform procedure (commands that may
branch on observed output), and what confirms or refutes the finding (or the
feasibility of a proposed remedy). The top-level reads each request as freeform
and runs it; it does not parse a rigid schema, so the request format is defined
only where requests are produced (the `review-changes-step`,
`review-changes-verify`, and `review-changes-analyze` skills).

The draft always returns its findings in one reply, with any experiment
requests attached alongside; the top-level runs those and feeds the results to
Phase 2's verifiers. A verifier or analyst may instead reply with experiment
requests and **no** verdict/analysis — a _deferral_ — after which the top-level
runs them and re-spawns that subagent with the results (Phase 2/3). A verifier
may also attach requests to a finished verdict, and an analyst to a finished
analysis — grounding a suggested action whose feasibility it has not verified;
see Phases 2 and 3.

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

## Workflow: draft ⇄ (verify ⇄ analyze) → final

The review is produced in four phases. The top-level skill is the
**sole writer** of every file under `/tmp/review-changes-<topic>-*`.
Files are append-only.

**Dispatching a sub-step.** The draft, verify, and analyze sub-steps each run
as a subagent spawned via the **Agent tool** (`subagent_type: general-purpose`,
`model: opus`) — never via `Skill(...)`, which serializes forked invocations and
would run a per-finding "batch" one at a time. The Agent call's own parameters
and the prompt below are authoritative; each child SKILL.md is read as plain
instructions, so its frontmatter (`model`, `allowed-tools`) is documentation of
intent only, not enforced. The Agent call cannot carry an effort level, so the
prompt ends with the `ultrathink` keyword to request the deepest reasoning for
the sub-step. To dispatch one, issue an Agent call whose prompt is:

> Read `ai/.claude/skills/review-changes-<step>/SKILL.md` and follow it exactly
> as your instructions. You are a read-only reviewer: do not modify, stage,
> execute, or build anything in the project; use only read-only git, Read,
> Grep, and Glob. Your inputs: `<the structured inputs for this step>`. Return
> only the output that skill's Output section specifies (its primary block plus
> any auxiliary sections it defines) as your final message. ultrathink

The `<the structured inputs for this step>` placeholder is the bulleted Input
list the named child skill defines, so later references to a specific input
(e.g. the "Any experiment results" bullet) resolve unambiguously.

Spawning several subagents in **one message** (multiple Agent calls) runs them
concurrently — this is what makes a per-finding batch actually parallel.

The workflow is not linear. New findings arise from **three** sources:
verification can surface them as a side effect (Phase 2's own loop);
analysis actively hunts for them (emitting a new draft in Phase 3
re-enters Phase 2, the same way verification's new drafts do); and the
draft step itself is **re-run** over the scope (Phase 1, after each
convergence), each fresh independent pass possibly opening another
draft. All of it shares one flat round counter: `<round>` is its current value
— the index of the draft in hand. Every new draft (a re-draft pass, or a
verifier's or analyst's proposal batch) is created at the next index
`<round+1>` with `R<round+1>-<NNN>` IDs; the draft step is dispatched with that
index as its `N` input. (For recovery or verification, `<round>` equals the
highest `N` for which `/tmp/review-changes-<topic>-draft-<N>.md` exists, or 0
if none — the files are append-only and single-writer, so the latest draft on
disk is the round in hand. `<round>` = 0 means no draft is on disk, which is
indistinguishable between a never-started review and one that converged on an
empty initial pass; either way recovery simply runs the initial pass at `N = 1`
— never a Phase 4 shortcut — since re-running an empty-converged review just
re-converges and falls through to Phase 4.) The loop is **expected** to
converge — a fresh draft-step re-run yields no new finding once verify⇄analyze
has converged — and is hard-bounded in any case by the 50-iteration cap (see
Phase 2).

Verification decides the fate of every drafted finding (`keep`/`drop`);
analysis holds one **asymmetric** power on top of that: it may **reject**
a finding verification kept, when deeper study proves it a false positive,
which removes it from the final review. Analysis can only reject — it never
resurrects a dropped finding. So a finding reaches the final review only if
verification kept it **and** analysis did not reject it.

**Raw-findings corpus.** The corpus is every finding block written to any
`draft-<round>.md` file — **kept, dropped, or rejected alike**. Draft files
are append-only and never pruned, so once written, a block stays in the corpus
regardless of its later fate. (A rejected finding is a `keep` verdict that
analysis marked with `<!-- analysis-rejected: <ID> -->`; its original draft
block is untouched and remains in the corpus.)

**Unified dedup.** Whenever any source yields candidate new findings — a
verifier's or analyst's `## Proposed new findings`, or a fresh
`review-changes-step` re-run — the top-level deduplicates them
authoritatively: first against the entire raw-findings corpus, then within the
current batch (first occurrence wins; later duplicates dropped). Two findings
are **duplicates** when they identify the same defect at the same location,
irrespective of severity, title, or wording; the top-level decides this by
semantic judgment, not a literal field-equality test. A dropped duplicate
needs no corpus entry of its own: the surviving occurrence it duplicates —
whether a prior corpus finding or this batch's first occurrence once written —
is its anchor for every future pass. Every source is gated identically.
Verify and analyze subagents are still handed prior draft files and asked to
self-suppress obvious duplicates, but that self-check is only a best-effort
noise reducer — the top-level dedup is the sole authoritative gate.

`<topic>` is a 1–3-word kebab-case slug derived from the diff (module
name, feature, or commit subject). If any
`/tmp/review-changes-<topic>*` file already exists, suffix `<topic>`
with the smallest free integer (`-2`, `-3`, …) so a fresh review never
collides with an existing one.

### Phase 1 — Draft passes

The draft step seeds the review (round 1) and then runs **again** after each
time verify⇄analyze converges — a fresh, independent **re-draft pass** over the
same scope — until a pass yields no new finding. Every pass works the same way;
the two differences (initial vs re-draft) are called out below.

Determine the scope (per **Scope** above) as a Git command. For each pass,
choose the **round index** `N`: `1` for the initial pass; `<round+1>` for a
re-draft pass. Spawn one `review-changes-step` subagent **blind** — i.e. given
only `N` and the scope, not the prior draft files (unlike verify/analyze), so
each re-draft pass is a fresh, independent look and relies solely on the
top-level corpus dedup to suppress rediscoveries — via the dispatch convention
above, passing as its inputs the round index `N` and the scope command —
e.g. `N = 3`, `scope = git show HEAD`. It must use `N` as the round index for
every `R<N>-<NNN>` ID it assigns. It returns that pass's draft — the scope line
and `R<N>-<NNN>` finding blocks.

Validate the reply structurally: it must contain parseable `R<N>-<NNN>` blocks
(or an explicit no-findings statement) and must not truncate mid-block. If the
reply is unusable, re-spawn the `review-changes-step` subagent with the same
inputs (round index `N` and scope).
**Budget: 2 retries (3 attempts total).** Retry exhaustion (attempt 3 also
fails) is handled differently by pass:

- **Initial pass (round 1):** abort the review — write no `draft-1.md` — and
  return an abort message to the caller in the style of **Abort on retry
  exhaustion** below:

  ```text
  Review aborted in round 1: initial draft step exhausted retry budget
  (3 attempts, all failed).
  ```

- **Re-draft pass:** **non-fatal.** The review already holds a complete finding
  set, so do not abort: stop opening further re-draft passes, proceed to Phase 4
  (final assembly), and note the failed re-draft pass in the Summary.

On a valid reply, split off any `## Experiment requests` section (per the
splitting rule in **Experiment requests**), then dedup the pass's findings per
**Unified dedup** (above) — against the full raw-findings corpus, then within
the batch. (On round 1 the corpus is empty, so every finding survives.) Write
the survivors verbatim to `/tmp/review-changes-<topic>-draft-<N>.md` as
append-only blocks, with the scope line at the top, keeping the IDs the step
assigned — the top-level remains the **sole writer** of all
`/tmp/review-changes-*` files. The file is created **only if at least one
finding survives** (matching the same guard in Phases 2 and 3); an empty pass
writes no draft file at all. Dedup may drop some, leaving gaps in `NNN` among
survivors; that is fine — IDs stay unique.

If the draft carried experiment requests, run those whose supported
`R<N>-<NNN>` survived dedup (skip any keyed to a deduped-out finding) via the
**Experiment requests** run routine, recording results in
`/tmp/review-changes-<topic>-experiments.md` keyed to the `R<N>-<NNN>` each
supports; Phase 2 feeds each finding's results into its verifier.

**Terminal condition.** If a pass yields no surviving finding — the initial
draft reports none, or a re-draft pass's findings are all dropped by dedup —
the review has converged: skip to Phase 4 (final assembly); no further re-draft
is attempted. So any review that found anything always incurs one final
re-draft pass whose findings are all deduped away — that empty pass **is** the
convergence detector and must always be run, because "no surviving finding" is a
post-run, post-dedup observation, never a pre-run prediction, so it cannot be
optimized away on the assumption the corpus is already exhaustive. Otherwise
proceed to Phase 2 to verify the new draft; once
verify⇄analyze converges, return here for another re-draft pass. Each re-draft
pass, and every draft it transitively opens, counts under the single
50-iteration safety cap (see Phase 2).

### Phase 2 — Verification rounds

For each round, spawn one `review-changes-verify` subagent **per finding** in
the current draft file, in parallel — one message, multiple Agent calls (per
the dispatch convention above). That skill holds the per-finding verification
contract; each subagent's inputs are the one finding's ID and full block, the
scope as a Git command, the paths of the prior draft files that exist, for
dedup, and any experiment results (the matching `EXP` blocks) for that finding.

Once the batch returns, validate each reply (rules below). Process
the valid replies:

1. Write all verdict blocks to
   `/tmp/review-changes-<topic>-verdicts-<round>.md`.
1. Dedup the batch's `## Proposed new findings` per **Unified dedup**
   (above) — against the full raw-findings corpus, then within this batch.
1. Write the surviving proposals to
   `/tmp/review-changes-<topic>-draft-<round+1>.md` (created only
   if any survive), assigning `R<round+1>-<NNN>` IDs.

For each invalid reply, write nothing to disk; add the finding to
the retry set. After the batch is fully processed, if the retry set
is non-empty, dispatch a new parallel batch — one
`review-changes-verify` Agent call per retry-set finding, same
prompt as the original attempt.
Repeat until the retry set is empty. **Budget: 2 retries (3 attempts
total) per finding.** If any finding's third-attempt batch contains
an invalid reply, abort the review (see **Abort on retry exhaustion**
below).

This batch-then-retry pattern reflects foreground Agent dispatch: the
parallel calls return as one batch, so retries naturally
synchronize at attempt boundaries. Under background dispatch
(asynchronous result delivery), retries can fire per finding as each
result arrives, and the batch boundaries here are relaxed.

Once every finding in `draft-<round>.md` has a valid verdict in the
verdicts file, if `draft-<round+1>.md` exists and is non-empty, run
another round on that new draft. Otherwise iteration stops.

Safety stop: **50 iterations.** This bounds, under a single cap, every
iteration the review performs — each draft opened (verification rounds and
Phase 3 analysis emits), each experiment-request re-spawn (Phase 2 or 3), and
each Phase 1 re-draft step invocation. It is a runaway-loop guard, not a quality
knob;
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
one `review-changes-analyze` subagent **per not-yet-analyzed kept finding**, in
parallel — one message, multiple Agent calls (per the dispatch convention
above). That skill holds the per-finding analysis contract; each subagent's
inputs are the finding's ID and full verdict block (from
`verdicts-<round>.md`), the scope as a Git command, the paths of the prior
draft files that exist, for dedup, any experiment results (the matching `EXP`
blocks) for that finding, and — only when a
placement decision applies (committed scope with a non-empty stack, computed
below) — the stack as a list of SHA + subject and the blame-target revision
`REV`.

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
prompt (see the `review-changes-analyze` skill).

Each analysis reply has up to four parts: the `#### Analysis: <ID>` block,
optionally followed by a `## Rejection` section, a `## Proposed new findings`
section, and/or a `## Experiment requests` section (all level-2 headers).
**Split the reply on the first occurrence of each such header that occurs as a
true top-level line — outside any fenced code block or block quote**, using the
same CommonMark-aware rule. A subagent that merely quotes a delimiter inside a
fence (e.g. when reviewing this skill's own schema) does not trigger the split.
Everything before the first such boundary is the analysis body; each routed
section is handled below and is **never inlined** into the final review (so no
level-2 header ever outranks the `#### Analysis` heading in the assembled
document).

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
  `#### Analysis: <ID>` header line (already at the level it occupies in the
  final review) and the body below it, up to but excluding any `## Rejection`,
  `## Proposed new findings`, or `## Experiment requests` header. The one
  exception is the **alongside-analysis shape** — the reply also carries a
  `## Experiment requests` section — in which case do not append now. The body
  is held pending experiment results; the experiment-request handling below runs
  inline for this finding, re-spawns the analyst, and appends the finalized (or
  latest provisional) body instead — so a re-spawn intervenes before any append,
  and the body finally appended may differ from this provisional one. That
  inline alongside loop runs to completion before this finding counts as having
  a valid reply, so the "once every … has a valid reply" condition below is
  reached only after every finding's alongside loop has terminated.

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
Phase 4 (final assembly) will include the finding (kept) without an Analysis
subsection and will note the omission in the Summary. Do not abort the review.

Once every not-yet-analyzed finding in the batch has either a valid
reply or has exhausted its retry budget, process the proposed new
findings from the valid replies:

1. Collect the `## Proposed new findings` entries from all valid replies in the
   batch, including any re-spawn replies produced by deferral or alongside loops
   (per the experiment-request handling below).
1. Dedup them per **Unified dedup** (above) — against the full
   raw-findings corpus, then within this batch.
1. Write the survivors to a new
   `/tmp/review-changes-<topic>-draft-<round+1>.md` (created only if any
   survive), assigning `R<round+1>-<NNN>` IDs.

**Loop-back trigger.** If a new draft was created, re-enter Phase 2 to
verify it — verification runs to convergence as usual, possibly opening
further drafts — then return to Phase 3 to analyze the findings it kept
(skipping any already analyzed). Repeat until an analysis pass produces
no surviving new draft — verify⇄analyze has then converged, so **return to
Phase 1 for a re-draft pass**. The 50-iteration safety cap bounds the
combined loop.

If there are no kept findings at all, skip Phase 3 entirely and return to
Phase 1 for a re-draft pass.

#### Analysis subagent reply validation

An analyst reply contains an analysis block, or a `## Experiment requests`
section (a deferral), or both (an analysis with requests attached alongside).
The rules below judge the analysis part; a reply with a well-formed request
section and **no** analysis block (a deferral) is not unusable on that account.
A reply is _unusable_ if any of the following holds:

1. Agent invocation returned an error or timeout.
1. Reply lacks a `#### Analysis: <assigned-ID>` header (exactly four
   `#`) for the finding's assigned ID — **unless** it is a deferral (a
   well-formed `## Experiment requests` section and no analysis block).
1. Body under the header (before any `## Rejection`,
   `## Proposed new findings`, or `## Experiment requests` section) is empty or
   whitespace-only — **unless** the reply carries a `## Rejection` section, in
   which case the analysis body is optional (the rejection reason is the
   content).
1. A `## Rejection` section is present but empty or whitespace-only (it
   must carry a reason).
1. A `## Rejection` section and a `## Experiment requests` section are
   both present. The two are mutually exclusive: a rejection is a
   terminal decision, whereas a deferral means the analyst has not yet
   decided and needs evidence to do so — a reply that both rejects and
   requests experiments is contradictory and must never happen.
1. The analysis body (everything before any `## Rejection`,
   `## Proposed new findings`, or `## Experiment requests` delimiter) contains
   an ATX heading — a
   `#`-prefixed line per CommonMark (a `#` run with ≤3 leading spaces,
   outside any fenced code block or code span) — other than the leading
   `#### Analysis:`. A `#` line a body legitimately quotes inside a fenced
   code block (e.g. a shell comment or `#!` shebang) is not a heading and
   does not trip this rule.
1. Reply truncates mid-sentence or mid-bullet.

Carrying a `## Rejection`, `## Proposed new findings`, or
`## Experiment requests` section does
**not** by itself make a reply unusable — `## Proposed new findings` is
expected when the subagent spots a new issue, `## Rejection` when it
proves the finding a false positive, and `## Experiment requests` when it
defers or attaches a remedy-feasibility experiment alongside its analysis.
Detection is structural but
CommonMark-aware: it must honor fenced-code-block and code-span
boundaries, so `#`/`##` lines a body quotes inside a fence are treated as
neither headings nor section delimiters. The top-level does not judge
analysis quality, only schema conformance.

An analyst that needs runtime evidence may return a `## Experiment requests`
section in one of two shapes. Run any such requests via the **Experiment
requests** run routine, appending results to the experiments file, then handle
by shape:

- **Requests with no analysis block (a deferral):** the analyst cannot decide
  the finding yet — re-spawn it; the rebuilt prompt's "Any experiment results
  for this finding" bullet carries the new `EXP` blocks, so nothing is
  separately appended. Append no analysis body for this ID until the re-spawn
  produces one, so the ID stays unanalyzed and is re-dispatched. Any
  `## Proposed new findings` on a re-spawn reply are collected and fed to the
  proposed-new-findings step alongside the initial batch's.
- **Requests alongside an analysis block (grounding a remedy):** the analyst has
  a usable analysis but flagged a suggested action whose feasibility it has not
  verified. Re-spawn it with the results so it can finalize that suggested
  action. The re-spawn's outcome is one of:
  - **Finalized analysis** (a plain block with no further requests): append it.
  - **Another alongside batch:** run those requests and re-spawn again — the
    alongside path loops like a deferral, except every reply already carries a
    usable provisional body, so it is never blocked.
  - **A pure deferral** (well-formed `## Experiment requests` and no analysis
    block): loop-terminating — append the latest provisional body and exit the
    alongside loop. Do **not** enter a deferral sub-loop; the alongside loop
    started because the analyst already held a usable answer, and reverting to
    pure deferral from that state is a regression.
  - **A `## Rejection`** (and no `## Experiment requests`): process it as a
    rejection exactly as the non-alongside rejection path above does — write the
    rejection marker `<!-- analysis-rejected: <ID> -->` followed by the reason
    verbatim, do **not** append the provisional body, and terminate the loop.

  Otherwise the loop ends when the analyst finalizes, or — if a re-spawn fails
  validation or the 50-cap is hit — by appending the latest **provisional**
  analysis body instead (never discard it); the experiments stay in the file for
  audit. A re-spawn that fails validation in this alongside loop is **not**
  subject to the 3-attempt retry budget (which governs only the initial analysis
  dispatch and pure deferrals): the loop terminates immediately and appends the
  latest provisional body. Exactly one analysis body is appended per finding
  either way — or, on rejection, a rejection marker instead — preserving
  analyze-once. Any `## Proposed new findings` on **any** re-spawn reply
  (finalized, intermediate alongside, or rejecting) are collected and fed to the
  proposed-new-findings step alongside the initial batch's. The analyst is the
  last tier, so its experiment results feed its own next pass rather than
  downstream — unlike Phase 2's "requests alongside a verdict," which never
  re-spawns.

Each re-spawn is a main-loop iteration under the single 50-iteration safety
cap — no separate experiment budget. Remaining edge cases follow Phase 3's own
**non-fatal** handling, not Phase 2's abort. When no provisional body exists to
fall back on (a pure deferral — whether the initial dispatch or a re-spawn,
which in either case carries no analysis block): a reply carrying both a
`## Rejection` and requests is malformed and counts as an invalid analysis reply
under Phase 3's retry budget (per the validation rule above); and a request
section that is not parseable (see **Experiment requests**) likewise counts as
an invalid analysis reply under that budget (→ exhaustion marker, never abort).
For any **alongside** reply — the first one or a re-spawn, since it carries an
analysis block and so a provisional body — these same malformations instead
terminate the alongside path with the latest provisional body (above), never the
retry budget. If the cap is hit while the analyst is still deferring (no
provisional body to fall back on), proceed without the experiment and treat it
as the analysis-exhaustion case (exhaustion marker, noted in the Summary).

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
many times analysis fed findings back to verification); re-draft passes
run (how many produced surviving new findings, and findings they
contributed after dedup; note any re-draft pass that failed on retry
exhaustion); total drafted; total kept; total dropped; findings rejected
on analysis (list IDs, if any); total analyzed; findings proposed by
analysis (and how many survived dedup); analyses skipped due to retry
exhaustion (list IDs, if any); truncation note if the 50-iteration stop
fired>
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
  any `## Rejection`, `## Proposed new findings`, or `## Experiment requests`
  section when it appended the body) — do not edit, summarize, or re-level it.
  The analysis step emits that one level-4 heading and otherwise uses
  bold-paragraph labels instead of `#`-prefixed headings (see the
  `review-changes-analyze` skill), so nothing in the body outranks the
  `#### Analysis` heading or breaks the document outline. If a kept finding
  has no analysis body in the file (only an exhaustion or rejection marker, or
  nothing), omit the analysis for that finding.
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
