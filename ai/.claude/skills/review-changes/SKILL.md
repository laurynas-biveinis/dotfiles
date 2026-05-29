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

## Confidence

Each finding carries an integer `Confidence: N%` (0–100) reflecting how
strongly the evidence supports it. Calibration anchors:

- **90–100** — reproduced via isolated experiment, or trivially provable
  from the diff alone (e.g. syntax error, undefined symbol).
- **70–89** — confirmed by reading the code and following references or
  Git history; no remaining unknowns.
- **50–69** — plausible from the code but one or more assumptions remain
  unverified.
- **Below 50** — speculative. Phase 1 should usually drop these rather
  than emit them; Phase 2 should usually `drop` on `keep` candidates
  that fall here.

Verifiers are expected to _raise_ the confidence of `keep` findings and
_lower_ the confidence of `drop` findings, but no ordering is enforced
structurally — the validator only checks the value is an integer in
`[0, 100]`.

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

`<topic>` is a 1–3-word kebab-case slug derived from the diff (module
name, feature, or commit subject). If any
`/tmp/review-changes-<topic>*` file already exists, suffix `<topic>`
with the smallest free integer (`-2`, `-3`, …) so a fresh review never
collides with an existing one.

### Phase 1 — Initial draft (round 1)

Perform the review per the principles, severity definitions, and
checklist above. Ultrathink as you review. Write findings to
`/tmp/review-changes-<topic>-draft-1.md` as append-only blocks:

```markdown
### R1-001 — CRITICAL — <one-line title>

- Confidence: 75%
- Location: `path/to/file.ext:LN`
- Observation: <what's wrong, with diff evidence>
- Suggested action: <concrete fix>
```

IDs use the format `R<round>-<NNN>`, with `NNN` in per-round
discovery order. Include the scope line at the top of the file.
If no findings, skip Phases 2 and 3 and proceed to Phase 4.

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

Safety stop: **50 drafts.** This bounds every draft the review ever
opens — those from verification rounds _and_ those Phase 3 analysis
emits — under a single cap. It is a runaway-loop guard, not a quality
knob; convergence is expected far sooner. If the 50th draft still
produces new findings, stop and record the truncation in the final
summary.

#### Subagent reply validation

A reply is _unusable_ if any of the following holds:

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
counts as analyzed once it has either an analysis block or an
exhaustion marker in `/tmp/review-changes-<topic>-analyses.md` (see
below). A finding is analyzed exactly once even though Phase 3 may run
several times. Spawn one analysis subagent **per not-yet-analyzed kept
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

Each analysis reply has up to two parts: the `#### Analysis: <ID>`
block, optionally followed by a `## Proposed new findings` section
(level-2 header). **Split the reply on the first
`## Proposed new findings` header that occurs as a true top-level line
— outside any fenced code block or block quote.** A subagent that
merely quotes the delimiter inside a fence (e.g. when reviewing this
skill's own schema) does not trigger the split. Everything before that
boundary is the analysis body; the section after it is routed into the
loop below and is **never inlined** into the final review (so no
level-2 header ever outranks the `#### Analysis` heading in the
assembled document).

Validate each reply with the rules in **Analysis subagent reply
validation**. For each valid reply, append its analysis body verbatim —
the `#### Analysis: <ID>` header line (already at the level it occupies
in the final review) and the body below it, up to but excluding any
`## Proposed new findings` header — to the append-only file
`/tmp/review-changes-<topic>-analyses.md`, of which the top-level is the
sole writer (analysis subagents still never write files). Phase 4 reads
the bodies back from this file rather than from memory, so a body
produced in an early pass survives intact across any number of later
verify⇄analyze rounds. Invalid replies enter a retry set; dispatch a new
parallel batch, same prompts. **Budget: 2 retries (3 attempts total)
per finding.**

**Exhaustion is non-fatal here.** Analysis augments findings; it is
not load-bearing for correctness. If a finding's attempt 3 reply
also fails validation, append an exhaustion marker for its ID to
`/tmp/review-changes-<topic>-analyses.md` — an inert HTML comment
`<!-- analysis-skipped: <ID> (retry exhaustion) -->`, so the ID counts
as analyzed and is never re-dispatched on a later pass — then continue.
Phase 4 will include the finding without an Analysis subsection and
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
no surviving new draft. Then proceed to Phase 4. The 50-draft safety
cap bounds the combined loop.

If there are no kept findings at all, skip Phase 3 entirely and
proceed to Phase 4.

#### Analysis subagent reply validation

A reply is _unusable_ if any of the following holds:

1. Agent invocation returned an error or timeout.
1. Reply lacks a `#### Analysis: <assigned-ID>` header (exactly four
   `#`) for the finding's assigned ID.
1. Body under the header (before any `## Proposed new findings`
   section) is empty or whitespace-only.
1. The analysis body (everything before any `## Proposed new findings`
   delimiter) contains an ATX heading — a `#`-prefixed line per
   CommonMark (a `#` run with ≤3 leading spaces, outside any fenced
   code block or code span) — other than the leading `#### Analysis:`.
   A `#` line a body legitimately quotes inside a fenced code block
   (e.g. a shell comment or `#!` shebang) is not a heading and does
   not trip this rule.
1. Reply truncates mid-sentence or mid-bullet.

Carrying a `## Proposed new findings` section does **not** by itself
make a reply unusable — that section is expected when the subagent
spots a new issue. Detection is structural but CommonMark-aware: it
must honor fenced-code-block and code-span boundaries, so `#`/`##`
lines a body quotes inside a fence are treated as neither headings nor
section delimiters. The top-level does not judge analysis quality, only
schema conformance.

### Phase 4 — Final assembly

Read every `verdicts-<round>.md` file and the analysis bodies from
`/tmp/review-changes-<topic>-analyses.md`. Using the kept verdicts and
those analysis bodies, write the final review to
`/tmp/review-changes-<topic>.md` containing every kept finding, using
this structure:

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
total kept; total dropped; total analyzed; findings proposed by
analysis (and how many survived dedup); analyses skipped due to retry
exhaustion (list IDs, if any); truncation note if the 50-draft stop
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
  `<!-- analysis-skipped:` marker prefix, or EOF — whichever comes
  first. Treating that marker prefix as a delimiter keeps an
  interleaved exhaustion marker from being swept into a preceding
  body. The body already opens with its
  own `#### Analysis: <ID>` heading at the right level, so the top-level
  adds no heading and strips nothing further (Phase 3 already excluded
  any `## Proposed new findings` section when it appended the body) — do
  not edit, summarize, or re-level it. Analysis subagents emit that
  one level-4 heading and otherwise use bold-paragraph labels instead
  of `#`-prefixed headings (see **Analysis subagents**), so nothing in
  the body outranks the `#### Analysis` heading or breaks the document
  outline. If a kept finding has no analysis body in the file (only an
  exhaustion marker, or nothing), omit the analysis for that finding.
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
- The "Confidence" section (calibration anchors and the
  raise-on-keep / lower-on-drop expectation), copied verbatim from
  this skill.
- The finding ID and the full finding block from the draft.
- The scope as a Git command for the subagent to run (e.g.
  `git diff --staged`, `git show HEAD`).
- Paths of **all prior draft files** (`draft-1.md` …
  `draft-<round>.md`) for deduplication of any new findings it
  proposes.
- An explicit instruction that it must not write files and must not
  modify the project tree.
- The instruction: "Ultrathink while verifying this finding."

Small isolated experiments outside the project tree (e.g. in
`mktemp -d`) are fine if they sharpen the verdict; summarize the
experiment in the verification trace rather than writing a file the
top-level will not read.

The subagent must:

1. Independently confirm the finding by reading the code, following
   references, consulting Git history, or running an isolated
   experiment. Do not hypothesize. If it cannot be confirmed, the
   verdict is `drop`.
1. Return one verdict block in exactly this schema. `Final confidence:`
   is required on every verdict; the severity, title, location,
   observation, and suggested-action lines may be omitted on
   `Outcome: drop`:

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

1. Optionally append a `## Proposed new findings` section after the
   verdict, listing additional issues spotted while verifying. Each
   entry must be a complete finding block (severity, confidence,
   title, location, observation, suggested action) **without an ID**
   — the top-level assigns IDs when it appends to the next draft.
   Before listing a proposal, the subagent must confirm it is not a
   duplicate of any finding in the prior draft files it was given;
   duplicates are dropped silently.

If a subagent's reply fails the **Subagent reply validation** rules
in Phase 2, the top-level re-spawns the subagent with the same
prompt, up to 2 retries (3 attempts total). If attempt 3 also fails,
the review aborts; the subagent itself never returns `Outcome:
error`.

## Analysis subagents

Each analysis subagent analyzes exactly one kept finding. They are
spawned in parallel via the Agent tool, with `subagent_type:
general-purpose`.

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
  must satisfy, copied verbatim from this skill.
- Paths of **all prior draft files** (`draft-1.md` …
  `draft-<round>.md`), so it can self-suppress proposals that
  duplicate an already-raised finding. This is best-effort only; the
  top-level dedups authoritatively against all raw findings.
- An explicit instruction that it must not write files and must not
  modify the project tree.
- The instruction: "Analyze, research, and ultrathink about this
  finding. Restate the critique in your own words, examine what the
  code actually does, identify whether the finding addresses the
  root cause or a symptom, present alternative resolutions when more
  than one is reasonable, recommend one, and note anything the
  analysis does not change. Drop sections that do not apply — do not
  pad."
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

Small isolated experiments outside the project tree (e.g. in
`mktemp -d`) are fine if they sharpen the analysis; summarize the
experiment inline rather than writing a file the top-level will not
read.

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

If — and only if — analysis surfaced a genuinely new issue, append a
`## Proposed new findings` section after the analysis block. This is
the **only** higher-level (`##`) heading allowed anywhere in the
reply; the top-level splits the reply on it, inlining the analysis
block and routing the proposals into the loop. Each entry must be a
complete finding block (severity, confidence, title, location,
observation, suggested action) **without an ID** — the top-level
assigns IDs when it appends to the next draft:

```markdown
## Proposed new findings

### CRITICAL — <one-line title>

- Confidence: 70%
- Location: `path/to/file.ext:LN`
- Observation: <what's wrong, with evidence>
- Suggested action: <concrete fix>
```

Before listing a proposal, the subagent should confirm it is not a
duplicate of any finding in the prior draft files it was given;
duplicates are dropped silently. The top-level re-dedups
authoritatively, so this check is best-effort.

If the reply fails the **Analysis subagent reply validation** rules
in Phase 3, the top-level re-spawns the subagent with the same
prompt, up to 2 retries (3 attempts total). If attempt 3 also
fails, the top-level drops the analysis for that finding (Phase 3
non-fatal exhaustion); the subagent itself never returns an error
outcome.
