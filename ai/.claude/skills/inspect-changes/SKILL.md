---
name: inspect-changes
description: >-
  Inspect code changes and report verified, analyzed findings without acting.
  Use when review-changes needs an independent inspection, or when the caller
  requests a review report without fixes or tracking operations.
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
  Bash(python3 ${CLAUDE_SKILL_DIR}/scripts/replies.py *)
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

## Result contract

Every return, including a stop before dispatch, uses this envelope:

```text
Status: converged | incomplete | aborted | not-run
Scope: <normalized command actually reviewed, or attempted scope on a stop>
Baseline: <derived pre-image, or unavailable before derivation>
Report: <absolute final report path, or none>
Reason: <brief summary or the existing stop/abort diagnostic verbatim>
```

`converged` means the terminal draft pass was observed to produce no surviving
new findings and no completion limitation occurred. It can still carry kept
findings: convergence describes the inspection, not whether the code is fixed.
`not-run` covers Scope's pre-dispatch stops and a failed `claim`; `aborted`
covers initial-draft or verification retry exhaustion. A report assembled after
any of the following is `incomplete`, even if a later draft pass yields no new
findings:

- A re-draft pass exhausts its retries.
- An analysis is skipped after retry exhaustion or a pure-deferral cap stop.
- An alongside-analysis loop falls back to a provisional body.
- The 50-iteration safety cap prevents further work.
- A repository-state check detects drift or cannot establish an unchanged
  basis after dispatch began (see [repository state](references/repository-state.md)).

At each such event append its reason to
`/tmp/inspect-changes-<topic>-limitations.md`, preserving the existing fallback
and assembly behavior. Read this append-only file when assembling or resuming
a review; any entry precludes `converged`. A denied or unsafe experiment alone
is not a completion limitation when the consuming tier finishes on read/Git
evidence. An empty scope is never convergence.

Print the status in the report as well as the return envelope, and include all
completion limitations in its Summary. The envelope also applies to the fixed
stop messages below: retain their diagnostic wording in `Reason`, and do not
create a report file for a stop that previously wrote none.

## Scope

**Observe the repository in this review.** Never answer a probe, check or
other repository read this skill calls for from what you ran or saw before
this review started: the user stages, unstages, edits and commits out-of-band,
so such an observation is not evidence of the current state. Before Scope's
first repository read, capture the guard observations in
[repository state](references/repository-state.md). That procedure checks the
assumption that the repository holds still; reuse captured values only while
its checks pass. It also defines how to resume from persisted review files.

**Normalize before running a scope or a precedence probe.** Reject
`--no-index` as described below before executing it. Otherwise derive a
patch-producing command, preserving revisions, pathspecs, diff filters and
merge mode:

- Remove `--exit-code`, `--quiet`, `--name-only`, `--name-status`, `--check`,
  and `--graph` when they occur as options before any `--` or
  `--end-of-options` delimiter. These modes suppress patches, change a
  successful status, or decorate patch headers.
- Remove `--raw`, `--numstat`, `--patch-with-raw`, and `-z` as options
  under the same delimiter rules. Their NUL-delimited records can hide a patch
  header from the line-start check below.
- Remove every `--output=<file>` and `--output <file>` option before any
  option-ending delimiter, consuming the separate filename when present. This
  keeps the patch on stdout and prevents a file write.
- Add `--patch --no-color --line-prefix= --submodule=short --no-ext-diff`
  and `--no-textconv --word-diff=none` to every shape. Also add
  `--output-indicator-new=+ --output-indicator-old=-` and
  `--output-indicator-context=' '` to restore the standard hunk markers. These
  options force Git's own patch format with undecorated headers and submodule
  updates as gitlink patches, without external diff or textconv helpers.
  For `git show`, also add `--format=`.
- Pin every revision operand before running the scope. Resolve each individual
  revision with `git rev-parse --verify '<revision>^{commit}'`, require a zero
  exit and exactly one full commit SHA, and substitute that SHA in the command.
  A resolution failure takes the failed-scope stop below; never take the first
  line of partial output as a successful resolution. Peeling also suppresses
  annotated-tag metadata for `git show`.
  - Split `A..B` and `A...B` into endpoints first, resolve each separately,
    then reconstruct with the same two-dot or three-dot operator. An omitted
    endpoint means `HEAD`; pin it too. Never resolve an entire range as one
    revision: its output can contain endpoints and exclusions, not one commit.
  - Pin separate endpoints in order, including `<merge>^1 <merge>`. For
    revision-set shorthands such as `<commit>^!` or `<commit>^@`, pin the
    underlying commit and retain the set suffix; do not peel the whole set or
    collapse it to a single-commit operand. Ordinary single-revision suffixes
    such as `~3` and `^1` resolve to their own commit SHAs.
  - Materialize implicit revisions: `git show` uses pinned `HEAD`; “last N
    commits” becomes a two-dot range from pinned `HEAD~N` to pinned `HEAD`.
    A staged diff without an explicit revision uses the captured `HEAD` SHA,
    except under an unborn `HEAD`, where it stays revision-free. A bare
    unstaged `git diff` stays revision-free and keeps its index baseline.
  - For `--merge-base` comparisons, preserve invalid forms for the failed-scope
    stop: Git rejects range operands with this option, and the unstaged form
    needs an explicit revision. For valid forms, resolve the base from the
    pinned endpoints, using captured `HEAD` as the second endpoint where
    implicit. Run
    `git merge-base --all A B` and require success with exactly one full SHA.
    For no or multiple bases, obtain Git's diagnostic from
    `git diff --merge-base A B` with the output safeguards above and take the
    failed-scope stop. Otherwise remove `--merge-base` and use that base SHA
    as the left operand and declared baseline, preserving the working-tree,
    staged, or committed right-hand side. Do not add `HEAD` as a second diff
    operand to a working-tree or staged comparison. Keep the three-dot form's
    separate multiple-base behavior below when `--merge-base` is absent.
  - Keep the original scope shape and merge intent for the checks below.
    Any later merge-scope selection must derive its revisions from the pinned
    commit and normalize the resulting command too. Derive the baseline from
    these same pinned operands; dispatch and print only the normalized command,
    never re-resolve the original symbols in a sub-step.
- Reconstruct the command with existing options and their arguments first,
  added options next, then revisions and finally pathspecs behind an explicit
  `--`. Preserve each operand's role; do not turn revisions into pathspecs.
  Keep any `--end-of-options` between the added options and revisions. Never
  append added options after either delimiter or an implicit pathspec.

Use this precedence to choose what to review:

1. If `git diff --staged` shows changes, review those.
1. Else, if `git diff` shows working-tree changes, review those.
1. Else, review `HEAD`'s last commit (`git show HEAD`).

Normalize both diff probes with the rules above before running them. Read each
exit status before stdout: a failure stops with the failed-scope message below;
a successful probe without a patch advances to the next candidate. Use the
patch-header test below to decide that, not whether stdout is nonempty. Retain
the selected probe's status and stdout for validation instead of running it
again. An explicit user scope skips precedence and runs once through the same
normalization and validation.

**Before applying the default precedence**, run both checks below in order. For
an explicit scope, run the unmerged-entry check whenever the current index is
its pre-image or post-image, counting only paths selected by the normalized
scope, including its diff filters and pathspecs. Run the merge-in-progress check
whenever the scope uses the `HEAD`-to-index staged form, including its normalized
form with an explicit pinned `HEAD`. Revision-only commit and range scopes skip
both. Each applicable check stops the review before any
`<topic>` is derived, in the shape the stops below use:

1. **Unmerged index entries.** Locate the repository root with
   `git rev-parse --show-toplevel`; run
   `git ls-files --unmerged --full-name -z` from there and deduplicate the paths
   across index stages. Under default precedence, count all of them.
   For an explicit scope, intersect them with paths selected by a probe derived
   from the normalized diff: replace its output-format options with
   `--name-only -z`, retaining its endpoints, selection options (including every
   `--diff-filter` option in order), pathspecs, and external-helper safeguards.
   Follow normalization's option-placement rules, but do not normalize this
   probe again: that would remove its filename output.

   Keep the probe in the scope's original working directory and convert its
   NUL-delimited filenames to repository-relative paths before intersecting.
   Restore only the prefix stripped by the effective relative-output setting,
   accounting for `--relative=<prefix>`, `--relative`, `--no-relative`, their
   option order, and `diff.relative`. For an implicit relative prefix, use
   `git rev-parse --show-prefix` from that working directory. Running from a
   subdirectory alone does not strip the prefix from diff output. Preserve
   `--relative`: removing it can broaden selection and include excluded
   conflicts. Count distinct paths in the normalized intersection. A command
   failure uses the failed-scope stop below; an empty intersection proceeds to
   the remaining checks and scope validation.

   Let Git apply the filters, including lowercase exclusions and all-or-none
   `*`; adding `--diff-filter=U` would change the selection. Intersect paths
   rather than counting only `U` diff records: a working-tree diff can also
   report an unmerged path as `M`, so `--diff-filter=M` can still select it.
   Each selected unmerged path has no stage-0 blob for `git show :<path>` to
   serve, and `git diff --staged` renders it as `* Unmerged path <file>` with
   no patch. Conflicts excluded by the scope do not block its review. Any
   conflicted operation produces these entries, not merges alone.

   ```text
   Review not run: unresolved conflicts remain (<n> unmerged paths); resolve
   and stage them, then re-run.
   ```

1. **A merge in progress** (the path from
   `git rev-parse --git-path MERGE_HEAD` exists). Resolve relative output
   against the repository working directory and check that exact path with
   `Read` or `Glob`, not by resolving the bare name as a revision. The path is
   worktree-specific, so it detects linked-worktree merges while a branch or tag
   named `MERGE_HEAD` cannot satisfy it. `git merge` stages the whole merge
   result before stopping, so rule 1 selects `git diff --staged`, whose
   pre-image is `HEAD` — the pre-merge tip. Every line the merged-in branch
   contributed then reads as authored content and tags `introduced`, and
   neither the diff nor `git status --porcelain` signals that a merge is
   underway. Committing first supplies a stable merge object and parent
   topology so the follow-up can choose the intended merge scope.

   ```text
   Review not run: a merge is in progress; commit it, then review the merge's
   own resolution with `git show <merge>`, or review everything integrated from
   the other side with `git diff <merge>^1 <merge>`.
   ```

   Cherry-pick, revert, and a rebase stopped at `edit` need no such check:
   their staged content's pre-image genuinely is `HEAD`, since the author is
   adopting it as their own commit. `git merge --squash` is the same case, and
   records no `MERGE_HEAD`.

The user may override with natural language ("review the last three
commits"). The chosen scope becomes the report's `Scope:` line, recorded as
**The reply script** describes. A range scope is a single diff, not a
per-commit walk — `git diff A..B`, or `git diff A...B` for the three-dot form —
whose pre-image the [shared
provenance guidance](references/provenance.md) already names for each form, and
which carries merge-authored content that survives to `B`.

**Untracked paths are in no diff**, so no scope command reaches them and none
of the precedence rules above can select them. After choosing the scope, run
`git status --porcelain -uall` and, when it reports `??` entries, name them as
excluded on the printed scope line and on a trailing line appended to any stop
message below — the stop templates are fixed strings with no slot of their own.
Use `-uall` because the default collapses a whole new subtree into a single
entry, and summarize by count and top-level directory rather than listing every
path when the list runs long. They enter a review only by being staged or named
in an override, and saying so is what keeps a partially-untracked change from
reading as fully reviewed.

That covers both shapes. When rule 1 or 2 selected the scope, the review is
merely partial — the untracked files are excluded, and the named exclusions say
so. When rule 3 was reached **only** because both probes were empty while `??`
entries exist, the review's entire subject is wrong: it would spend a full
multi-subagent run on the previous commit while the user's only new work goes
unread. Confirm the scope with the user before dispatching there rather than
stopping outright, since untracked scratch files alongside a genuine
just-committed change are ordinary.

**A merge in the chosen scope.** Apply this selection only when the scope
reviews a single commit's changes, including the default `git show HEAD` and
equivalent commit comparisons such as `git diff <commit>^!`. Preserve
`git diff <merge>` and `git diff --staged/--cached <merge>` as working-tree and
index comparisons against that merge, subject to the existing conflict checks
and normalization. Preserve explicitly selected endpoint comparisons as well.
For a qualifying commit review, check whether that commit is a merge
(`git log -1 --format=%P <commit>` lists more than one parent) before dispatching
anything. If it is, say which of these you are reviewing rather than defaulting
silently — they are different intents, and an empty diff means something
different in each. A range scope reaches the merges inside it through the range
command above, and a merge that precedence rules 1 and 2 would otherwise have
caught uncommitted is stopped before the precedence runs.

1. **Everything the merge brought in** — `git diff <commit>^1 <commit>`, which
   diffs against the first parent and so carries every other side's work
   whatever the parent count. Name the endpoints rather than writing
   `git show --first-parent <commit>`: that form relies on `--first-parent`
   overriding `git show`'s combined-diff default, which arrived with the
   `--diff-merges` family in Git 2.31, and on older Git it yields the combined
   diff instead, which can hide merged-in changes and trigger the empty-scope
   stop below. This is the default, except for a back-merge:
   when some other parent `<commit>^<n>` is already contained in trunk's
   upstream and `<commit>^1` is not
   (`git log --oneline <trunk>@{upstream}..<commit>^<n>` empty while the same
   over `<commit>^1` is not — `<trunk>` as Phase 3 defines it below, `n`
   ranging over the parents `%P` listed above, and `2` on the ordinary
   two-parent merge), the merge integrated upstream history, so the
   first-parent diff would review published work while the author's own commits
   went unread. Default there to `git diff <commit>^<n> <commit>` — the
   author's side, with that parent as its pre-image — and say which side is
   already published, naming any other published parent the diff still carries.
   Containment in trunk's upstream is narrower than published: a merge of a
   branch that is pushed but outside trunk — a colleague's branch being
   integrated — tests as not a back-merge and falls to the first-parent
   default, as does a merge where no trunk or no upstream exists and the test
   cannot run at all.
1. **The merge's own resolution** — `git show <commit>`, a dense combined
   comparison against all parents. It shows only files modified from every
   parent and suppresses hunks choosing one of two parent variants unchanged.
   Parent-selected conflict resolutions can be hidden, while automatic
   combinations of nearby edits can appear. State these limits on the printed
   scope line and preserve an explicit resolution-review request: do not
   substitute an integration review when the comparison is empty. Use the
   resolution-specific stop below instead.
1. **One authored commit, on a named side** — a resolution step, then an
   ordinary scope. Resolve the commit with
   `git log --no-merges -1 --format=%H <commit>^<n> --not <every other parent>`,
   where `^1` is the branch that received the merge and `^2` and up are each
   branch it brought in, `n` ranging over the parents `%P` already listed; then
   review it as `git show <sha>`. Excluding the other parents is what makes the
   answer a commit that side alone contributed. `--first-parent` is compatible
   with `--not`, but restricts traversal to the receiving lineage at each nested
   merge and can omit exclusive commits reached through other parents. If that
   lineage reaches excluded trunk without encountering an exclusive non-merge
   commit, the combination returns nothing even when another parent leads to an
   eligible commit. Without either, plain `git log --no-merges -1` takes the
   newest non-merge commit by committer date, which is whichever side happens to
   hold it. Run the
   resolution and read it before substituting: a non-zero exit is a failed
   scope, and empty output — a side that contributed nothing exclusive — is an
   empty scope. Report either in the shapes below, naming the resolution
   command, because a bare `git show` with an empty argument silently reviews
   `HEAD`. Resolving first is also what keeps this intent ordinary downstream:
   the printed scope names the actual commit, and its `REV` and pre-image
   follow the `git show <commit>` rules with no exception anywhere.

Intent 1, back-merge exception included, is the one the orchestrator picks
unaided; intents 2 and 3 are entered only when the user's scope request names
one, arriving through the natural-language override above, for which the
intents' titles are the vocabulary. Each is a chosen scope in its own right —
that is why the stop below has a clause about intent 2's empty diff — so
announcing which one is in play names a decision, not a side note.

**An empty or failed scope is a stop, not a converged review.** Before
dispatching anything, reject a chosen scope that uses `--no-index`: it compares
arbitrary filesystem paths, for which this workflow defines neither a repository
baseline nor a post-image, so its mandatory provenance analysis cannot run.

```text
Review not run: the chosen scope (<command>) uses --no-index, which has no
repository baseline for provenance.
```

Use the derived command as the scope command passed to every sub-step. Reuse a
selected probe's captured status and stdout; otherwise run the normalized chosen
scope once. Read its exit status before interpreting stdout. Any non-zero is
a failed scope — the revision never resolved, a mistyped name, or a deleted
branch — so quote the failing Git invocation's message verbatim. In the stop
message, keep the originally chosen command so the user sees the selection they
requested; print the derived command as the reviewed scope because it is what
every reviewer runs. Every stop below happens before any `<topic>` is derived
and writes no `/tmp/inspect-changes-*` file:

```text
Review not run: the chosen scope (<command>) failed: <Git's message verbatim>.
```

After a successful run, judge emptiness only by patch headers in stdout:
`diff --git`, `diff --cc`, or `diff --combined` at the start of a line. Do not
use total output or a diffstat: `git show` can print object metadata without a
patch, and a merge's combined diff can report files and insertions under
`--stat`, `--shortstat` or `--numstat` while emitting no patch. If no patch
header appears, stop: an empty scope reaching Phase 4 as "no findings" is a
clean bill of health for code nobody read. For intent 2, use:

```text
Review not run: the combined comparison exposes no changes; this does not
establish that no conflict resolution occurred.
```

For all other scopes, use:

```text
Review not run: the chosen scope (<command>) selected no changes.
```

That guards an `--allow-empty` commit, a range whose endpoints do not differ, a
first-parent merge diff on a merge whose tree equals its first parent's, an
unmatched pathspec (which exits 0, so it arrives here rather than as a failed
scope), an intent-3 resolution that named no commit, and a user-requested
working-tree or staged scope whose own probe — `git diff` or
`git diff --staged` — is empty. Under the default precedence a tree with no
tracked modifications instead selects `git show HEAD`, so it reaches this stop
only through the commit-shaped cases already listed. Untracked files leave both
probes empty, so an untracked-only tree takes whichever of those two routes
applies, its new files read by nothing either way.

**The pre-image baseline.** Derive it once here, alongside the scope, and pass
it to every draft, verify, and analyze dispatch as a declared input — it is a
pure function of the already-chosen scope, so having each per-finding subagent
re-derive it risks two of them resolving an unusual scope string differently.
Read the [shared provenance guidance](references/provenance.md) for what each
scope shape yields — it is the single source for that, and the bullets below
add only how to resolve and pin what it names, never a second copy of the
values themselves, except the one shape whose mapped value is the command to
run. Revision placeholders below refer to normalization's pinned SHAs, never
the original symbolic operands.

- **A bare `git diff`.** The mapped baseline names no revision: neither probe
  nor pin it.
- **A diff against an explicit revision, a staged diff, or a two-dot range.**
  Reuse the mapped revision's already-pinned SHA. An unborn `HEAD` under
  revision-free `git diff --staged` maps to no revision: neither pin nor pass
  one.
- **A three-dot range `git diff A...B`.** The mapping names a command, so run
  it: `git merge-base A B` — singular — and take its output, already a full SHA
  needing no pin; never pin one of `--all`'s extra bases, which are not the one
  the diff used. Never pass the range itself to `git rev-parse --verify`: on
  `A...B` that exits non-zero _while printing_ the endpoints followed by one
  `^`-prefixed line per merge base, and its first line is `B`, the post-image,
  so pinning any line of it inverts the baseline. Then count with
  `git merge-base --all A B`: more than one line means the pre-image was chosen
  rather than unique, so say so alongside the pinned SHA — still the one the
  diff used. Only the scope command itself warns
  `multiple merge bases, using <sha>`, and nothing here runs it.
- **A single commit — `git show <commit>`, and both merge shapes.** Enumerate
  the parents with `git rev-parse <commit>^@`, _without_ `--verify`, which
  rejects the `^@` form outright at every parent count, zero and one included,
  while still printing every parent it found. Do not reach for the
  `--verify --quiet` shape above either: it fails the same way but silently
  (exit 1, parents still on stdout), and under it a root commit is
  indistinguishable from a bad revision. Read the count only off a **zero**
  exit — a failure prints either nothing at all or the argument echoed back
  with its `^@` suffix, so both low counts below are reachable with no parent
  behind them. No output then means a root commit, whose mapped baseline
  likewise names none; one line is that commit's parent, already a full SHA;
  two or more is a merge, where the mapping's own per-shape rule decides which
  parents are the baseline — see the [shared provenance
  guidance](references/provenance.md) for how a supplied baseline interacts
  with it.

## Experiment requests

No subagent runs code. When the draft, a verifier, or an analyst needs runtime
evidence, it adds an `experiment_requests` list to its JSON reply — one entry
per experiment, each giving a goal, a freeform procedure (commands that may
branch on observed output), and what confirms or refutes the finding (or the
feasibility of a proposed remedy). The top-level prints them with
`requests <reply>`, reads each one as freeform and runs it; it does not parse a
rigid schema, so the request format is defined only where requests are produced
(the `inspect-changes-step`, `inspect-changes-verify`, and
`inspect-changes-analyze` skills).

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
1. Append each outcome to `/tmp/inspect-changes-<topic>-experiments.md` (the
   top-level is its sole writer) as an
   `### EXP-<n> — supports <finding-ID> — <ok | denied | unsafe>` block with the
   executed step/output trace and the Confirms/Refutes conclusion.
1. Experiments are **never load-bearing**: `denied`/`unsafe`/failed simply means
   the consuming tier proceeds on read/Git evidence. They never abort the
   review.

A request is _usable_ when it is an object whose `procedure` holds non-blank
text (see **Reply files** below) and, in a draft reply, has a `supports`
finding ID. A request list is _parseable_ when it is a list holding
at least one usable request, a lone request object counting as a one-entry
list; its other entries are dropped, the status line ends with
`; unusable requests dropped: <n>`, and the usable requests count as the whole
list, so the drop is no completion limitation. An empty list or object, `null`,
`""`, `false`, `0`, or an absent list carries no requests. This is the minimal
recognition floor — not a rigid schema; the entry's other fields stay freeform
per the producer contracts.

## Workflow: draft ⇄ (verify ⇄ analyze) → final

The review is produced in four phases. The top-level skill is the
**sole writer** of every file under `/tmp/inspect-changes-<topic>-*` except
the reply files below. The review records the script keeps are JSON Lines, one
record per line: the draft files `/tmp/inspect-changes-<topic>-draft-<N>.jsonl`
(findings), the verdicts files `…-verdicts-<round>.jsonl`, the analyses file
`…-analyses.jsonl` (records of kind `analysis`, `rejected`, `corrected` or
`skipped`) and the proposal batches `…-batch-<n>.jsonl`. Files are append-only,
except three. The state file's `claim` placeholder is replaced once, by the
review basis; later writes append.
`/tmp/inspect-changes-<topic>-summary.md` holds one assembly's prose rather
than review evidence, and is rewritten when Phase 4 runs again (drift at
its final check, or resume). `/tmp/inspect-changes-<topic>-verdict-<ID>.json`,
the analyst's copy of a kept verdict, is written whole by `verify-record` before
that verdict is recorded, so a retry after an interrupted call replaces it; once
the verdict is in its round's verdicts file, nothing writes the copy again.

**Reply files.** Each dispatch names a fresh reply path,
`/tmp/inspect-changes-<topic>-reply-<step>-<key>-<k>.json`: `<step>` is `draft`,
`verify`, or `analyze`; `<key>` is the round index `N` for a draft pass and the
finding ID otherwise; `<k>` is one more than the highest `<k>` that is either
recorded for that `<step>-<key>` in the state file's dispatch checkpoints or
present among the existing
`/tmp/inspect-changes-<topic>-reply-<step>-<key>-*.json` files (1 if neither),
so no retry, re-spawn, or resumed dispatch reuses a path, even one whose reply
was never written. Record each reply path with its dispatch checkpoint. The
sub-step writes its whole reply there once, as one JSON object, and nothing
modifies it afterwards. Only the file is the reply: a missing or empty reply
file, or one that is not a JSON object, is an unusable reply, whatever the final
message carries. A reply's text field holds a JSON string, or a list of strings
read as one line each; any other value counts as absent.

**The reply script.** Handle replies with
`python3 <skills-dir>/inspect-changes/scripts/replies.py <command>`, where
`<prefix>` below is `/tmp/inspect-changes-<topic>`. Its commands validate a
reply against this skill's rules, copy accepted records into the review files,
and print one status line, so a reply's content never passes through your
context to be recorded. Read reply content only where a step below says so:
draft findings and proposals for dedup, and experiment requests to run them.
`invalid: <reason>` marks an unusable reply. `refused: <reason>` means the call
would break append-only or analyze-once, record a second verdict for a finding,
record a verdict for a finding no draft holds, record an analysis or skip for a
finding with no kept verdict, append to a draft whose round already has a
verdict, write outside `/tmp/inspect-changes-*`, name a malformed finding ID,
select an entry twice, one already written or one the reply or batch does not
hold, claim a prefix whose last component does not start with
`inspect-changes-` or whose directory cannot be used, assemble before the
scope, baseline or summary file exists, or assemble a `converged` report while
the limitations file has an entry, the analyses file has a `skipped` or
provisional record, or a drafted finding has no verdict or a kept one no
analysis record. Each is an orchestration error to correct, never a reply
outcome. An empty draft pass records nothing, so `assemble` cannot tell whether
the terminal one ran; running it stays your job. A verdict refused for a
finding no draft holds was dispatched in
error: discard that reply, its proposals included, and take Phase 2's IDs from
the draft file. For a missing assembly input, write it (scope and baseline from
the values the state file records, the summary per Phase 4) and re-run
`assemble`. For a refused `claim`, derive a one-component kebab-case slug and
claim once more. If that claim is refused too, or any `claim` prints neither a
`claimed` nor a `refused:` line, stop before dispatch with `Status: not-run`,
`Report: none`, and that output verbatim in `Reason`. For the findings any other
refusal names, finish their Phase 2 or Phase 3 work, taking the IDs to analyze
from `pending-analysis`, after which the usual loop decides whether another
re-draft pass is due. When you write the state file, also write, with Write,
the printed scope line (the text following `Scope:`) to `<prefix>-scope.txt` and
the rendered baseline to `<prefix>-baseline.txt`, once; `assemble` reads them
for the report header, so no shell quoting touches either value.

**Dispatching a sub-step.** Before every dispatch batch, and after it returns
before consuming any reply, run the
[repository-state check](references/repository-state.md#check-boundaries).
This includes the first draft, re-drafts, retries, experiment deferrals, and
alongside-analysis re-spawns; a single asynchronous dispatch or returned reply
is its own batch. A failed check takes that procedure's stop path, before
reply validation, dedup, rejection, or provisional-analysis fallback.

The draft, verify, and analyze sub-steps each run
as a subagent spawned via the **Agent tool** (`subagent_type: general-purpose`)
— never via `Skill(...)`, which serializes forked invocations and
would run a per-finding "batch" one at a time. The Agent call's own parameters
and the prompt below are authoritative; each child SKILL.md is read as plain
instructions, so its frontmatter (`allowed-tools`) is documentation of
intent only, not enforced; the child Bash lists mirror the top-level's Git
entries and move with them, and what actually gates a sub-step's Git commands
is the session's own permissions, so a command may prompt regardless of any
list. To dispatch one, issue an Agent call whose prompt is:

> Read `<skills-dir>/inspect-changes-<step>/SKILL.md`,
> `<skills-dir>/inspect-changes/references/confidence.md`,
> `<skills-dir>/inspect-changes/references/severity.md`, and
> `<skills-dir>/inspect-changes/references/provenance.md`; follow all of them
> exactly as your instructions. You are a read-only reviewer: do not modify,
> stage, execute, or build anything in the project; use only read-only git,
> Read, Grep, Glob, and one Write of your reply path. Your inputs:
> `<the structured inputs for this step>`. Write exactly the JSON reply that
> skill's Output section specifies (its primary part plus any optional parts it
> defines) to your reply path, then return only `Wrote <reply path>` as your
> final message.

`<skills-dir>` is the absolute path of the directory holding this skill: the
skill body is loaded prefixed with
`Base directory for this skill: <absolute path>`, and `<skills-dir>` is that
path's parent. Substitute it rather than naming an install location, so every
sub-step reads the same copy of the skill set the orchestrator is running — this
tree is registered both user-level and directory-scoped, and a checkout under
review may be a worktree whose copy is neither. If no base directory is stated,
fall back to `~/.claude/skills`.

Every graded field's rubric is wired twice, and both halves are required: it is
named in the prompt above, which is the delivery floor that puts it in every
sub-step's context, and it is linked inline at each site that assigns the value,
which is what keeps the wiring visible to whoever edits that site. Do not add a
standalone pointer section for a rubric — that shape was tried and withdrawn,
and the inline link is what survives a rework of this prompt.

The `<the structured inputs for this step>` placeholder is the bulleted Input
list the named child skill defines, so later references to a specific input
(e.g. the "Any experiment results" bullet) resolve unambiguously.

**Caller requirements.** At the start of the review, capture any requirements
defined by the [shared caller-requirements
guidance](references/caller-requirements.md). Pass the same verbatim block as a
declared input to every draft, verify, and analyze dispatch, including retries,
re-drafts, experiment deferrals, and alongside-analysis re-spawns.

**Known findings.** A caller may supply `Known findings: <path>`, a file of
findings it already tracks, one per line as `- <title> (<location>) — <claim>`.
An entry's _known side_ is its `<title> (<location>)`, spelled as the line gives
it. Once the state file is written, copy that file verbatim to
`/tmp/inspect-changes-<topic>-known.md` and read only the snapshot afterwards.
It serves **Unified dedup** alone: never pass it, or anything drawn from it, to
a sub-step.

**No orchestration steering.** The prompt above is fixed, and its only variable
content is the named child skill, its skills directory, and that skill's
declared inputs. Apart from the verbatim caller-requirements input, never add
orchestration-generated direction — no "focus on X", no statement of what
earlier rounds covered, missed, or already found, no ranking of files or areas,
no instruction to skip part of the scope. Every subagent examines the whole
scope on its own terms; overlap between rounds is removed afterwards by
**Unified dedup**, never by narrowing a subagent's attention. The prior draft
paths handed to verify and analyze are a duplicate-suppression filter on what
they emit (see [prior drafts](references/prior-drafts.md)), not a redirection of
where they look.

Spawning several subagents in **one message** (multiple Agent calls) runs them
concurrently — this is what makes a per-finding batch actually parallel.

The workflow is not linear. New findings arise from **three** sources:
verification can surface them as a side effect (Phase 2's own loop);
analysis actively hunts for them (emitting a new draft in Phase 3
re-enters Phase 2, the same way verification's new drafts do); and the
draft step itself is **re-run** over the scope (Phase 1, after each
convergence), each fresh independent pass possibly opening another
draft. All of it shares one flat round counter: `<round>` is its current value
— the index of the draft in hand. Every new draft (a re-draft pass's, or one
written from a verifier's or analyst's proposal batch) is created at the next
index
`<round+1>` with `R<round+1>-<NNN>` IDs; the draft step is dispatched with that
index as its `N` input. (For recovery or verification, `<round>` equals the
highest `N` for which `/tmp/inspect-changes-<topic>-draft-<N>.jsonl` exists, or
0 if none — the files are append-only and single-writer, so the latest draft on
disk is the round in hand. `<round>` = 0 means no draft is on disk, which is
indistinguishable between a never-started review and one that converged on an
empty initial pass; either way recovery simply runs the initial pass at `N = 1`
— never a Phase 4 shortcut — since re-running an empty-converged review just
re-converges and falls through to Phase 4.) The loop is **expected** to
converge — a fresh draft-step re-run yields no new finding once verify⇄analyze
has converged — and is hard-bounded in any case by the 50-iteration cap (see
Phase 2).

Verification decides the fate of every drafted finding (`keep`/`drop`); analysis
holds two powers on top of that. It may **reject** a finding verification kept,
when deeper study proves it a false positive, which removes it from the final
review — an **asymmetric** power: analysis can only reject, never resurrect a
dropped finding. It may also **correct** a surviving finding's severity,
confidence, or provenance, which never removes it. So a finding reaches the
final review only if verification kept it **and** analysis did not reject it.

**Raw-findings corpus.** Read and apply the
[shared prior-draft guidance](references/prior-drafts.md) throughout the
workflow. Draft files are append-only and never pruned, so every written
finding stays in the corpus regardless of its later fate. (A rejected finding is
a `keep` verdict that analysis marked with a `rejected` record; its original
draft record is untouched and remains in the corpus.)

**Unified dedup.** Whenever any source yields candidate new findings — a
verifier's or analyst's `proposed_findings`, or a fresh
`inspect-changes-step` re-run — apply the authoritative gate in the shared
guidance. A dropped duplicate needs no corpus entry of its own: the surviving
occurrence it duplicates — whether a prior corpus finding or this batch's first
occurrence once written — is its anchor for every future pass. Every source is
gated identically.

With a known-findings snapshot, the gate also drops a candidate drafted
`pre-existing-off-path` that identifies the same defect at the same location as
a known entry, and appends one line for it to
`/tmp/inspect-changes-<topic>-known-matches.md`:
`- <candidate title> (<location>) — matches known: <known side>`, copying the
known side verbatim. A known entry's location
may come from an earlier image (another stack entry, or this one before a fix),
so judge it by the code it names, following moves and rewrites, rather than by
line number. A candidate
drafted with any other provenance passes this test, since the caller must
reassess a known finding the change may have put on its path. The test reads
the unverified draft tag by design, trading a residual risk for the match's
saved verification and analysis: a known finding that the change did put on its
path, but that its drafter tagged off-path, is suppressed instead of reassessed.
Only a later pass that tags it otherwise recovers it, and none may run: a pass
whose candidates are all known matches ends the review, which then hands its
caller no new finding. A known match is not a surviving finding,
so it anchors nothing: run this test before the within-batch step, which then
compares only the candidates this test passed.

`<slug>` is a 1–3-word kebab-case slug derived from the diff (module name,
feature, or commit subject). Run `claim /tmp/inspect-changes-<slug>`: it picks
the topic by the [review topic](references/topic.md) rule and creates its state
file in one step, under the lock every review's script calls share, so reviews
that start together cannot take colliding topics. It prints `claimed <prefix>`;
`<topic>` is `<prefix>` without `/tmp/inspect-changes-`: `<slug>` in lowercase,
plus `-<n>` when another review's filenames could reach it. Never pick or
suffix a topic yourself. After Scope succeeds, persist the review basis per
[repository state](references/repository-state.md) before any dispatch, even if
no draft file is ever written.

### Phase 1 — Draft passes

The draft step seeds the review (round 1) and then runs **again** after each
time verify⇄analyze converges — a fresh, independent **re-draft pass** over the
same scope — until a pass yields no new finding. Every pass works the same way;
the two differences (initial vs re-draft) are called out below.

Determine the scope (per **Scope** above) as a Git command. For each pass,
choose the **round index** `N`: `1` for the initial pass; `<round+1>` for a
re-draft pass. Spawn one `inspect-changes-step` subagent **blind** — unlike
verify/analyze it is handed no prior draft paths or cross-round state, only
`N`, the scope, the pre-image baseline, any verbatim caller requirements
(see **No orchestration steering** above), and its reply path — via the dispatch
convention above — e.g. `N = 3`, `scope = git show <sha>` with the options
derived above, `baseline = HEAD~ (<parent-sha>)`. It must use
`N` as the round index for every `R<N>-<NNN>` ID it assigns. Its reply is that
pass's draft — a JSON object whose `findings` list holds the `R<N>-<NNN>`
findings.

Validate the reply with `draft-check <N> <reply>`: it must be a JSON object
with a `findings` list — empty for a pass that found nothing, a lone finding
object counting as a one-entry list — whose every entry is an object
carrying a round-`N` ID and non-empty `title`, `location`,
`observation` and `suggested_action` text. Its `severity`, `confidence` and
`provenance` are first-pass claims that verification re-derives, so none makes
the reply unusable: `draft-write` records each in canonical form where it is in
range and as given otherwise, a non-text severity or provenance as its JSON text
and a null one as empty. A non-empty `review_failed`, or a repeated ID,
makes the reply unusable. An
`experiment_requests` list that is not parseable (see **Experiment requests**)
does not: `draft-check` counts no requests and ends its status with
`; malformed requests ignored`. Verification can still request the evidence, so
an ignored draft or verifier list is no completion limitation. If the reply
is unusable, re-spawn the `inspect-changes-step` subagent with the same inputs
(round index `N`, scope, baseline, and caller requirements) and a fresh reply
path.
**Budget: 2 retries (3 attempts total).** Retry exhaustion (attempt 3 also
fails) is handled differently by pass:

- **Initial pass (round 1):** abort the review — write no `draft-1.jsonl` — and
  return an abort message to the caller in the style of **Abort on retry
  exhaustion** below:

  ```text
  Review aborted in round 1: initial draft step exhausted retry budget
  (3 attempts, all failed).
  ```

- **Re-draft pass:** **non-fatal.** The review already holds a complete finding
  set, so do not abort: stop opening further re-draft passes, proceed to Phase 4
  (final assembly), and note the failed re-draft pass in the Summary.

On a valid reply, read the reply file and dedup the pass's findings per
**Unified dedup** (above) — against the full raw-findings corpus, then within
the batch. (On round 1 the corpus is empty, so only a within-batch duplicate or
a known-findings match can drop one.) Write the survivors with
`draft-write <prefix> <N> --reply <reply> <ID>...`, which appends them,
keeping the IDs the step assigned, to
`/tmp/inspect-changes-<topic>-draft-<N>.jsonl`. Run it **only if at least one
finding survives** (matching the same guard in Phases 2 and 3); an empty pass
writes no draft file at all. Dedup may drop some, leaving gaps in `NNN` among
survivors; that is fine — IDs stay unique.

If `draft-check` counted experiment requests, read them with
`requests --draft <reply>` and run those whose supported
`R<N>-<NNN>` survived dedup (skip any keyed to a deduped-out finding) via the
**Experiment requests** run routine, recording results in
`/tmp/inspect-changes-<topic>-experiments.md` keyed to the `R<N>-<NNN>` each
supports; Phase 2 feeds each finding's results into its verifier.

**Terminal condition.** If a pass yields no surviving finding — the pass reports
none, or dedup drops all its findings (on round 1, only known-findings matches
can) — the review has converged: skip to Phase 4 (final assembly); no further
re-draft is attempted. So any review whose initial pass wrote a draft always
incurs one final
re-draft pass whose findings are all deduped away — that empty pass **is** the
convergence detector and must always be run, because "no surviving finding" is a
post-run, post-dedup observation, never a pre-run prediction, so it cannot be
optimized away on the assumption the corpus is already exhaustive. Otherwise
proceed to Phase 2 to verify the new draft; once
verify⇄analyze converges, return here for another re-draft pass. Each re-draft
pass, and every draft it transitively opens, counts under the single
50-iteration safety cap (see Phase 2).

### Phase 2 — Verification rounds

For each round, spawn one `inspect-changes-verify` subagent **per finding** in
the current draft file, in parallel — one message, multiple Agent calls (per
the dispatch convention above). That skill holds the per-finding verification
contract; each subagent's inputs are the one finding's ID and the path of the
draft file holding it, the scope as a Git command, the pre-image
baseline, the paths of the prior draft files that exist, for dedup, any
experiment results (the matching `EXP` blocks) for that finding, the same
verbatim caller requirements, if present, and its reply path.

Once the batch returns, run `verify-record <prefix> <ID> <reply>` on each
reply. It validates the reply (rules below), appends an accepted verdict to
`/tmp/inspect-changes-<topic>-verdicts-<round>.jsonl`, copies a kept one to
`/tmp/inspect-changes-<topic>-verdict-<ID>.json` as its analyst's input, and
prints the outcome, severity, provenance, confidence, location, and title, or
the drop reason. The line identifies the verdict, not its claim: where a
candidate names the same defect and location as a finding that dropped, or that
analysis rejected, read that finding's verdict and compare against the
claim as settled per the shared prior-draft guidance. Then process the valid
replies:

1. Run `proposals <prefix> <round+1> <reply>...` over the valid replies that
   report proposals; it appends them to
   `/tmp/inspect-changes-<topic>-batch-<round+1>.jsonl`, numbering on from its
   last `P<n>` label, and prints the new ones as Markdown blocks.
1. Dedup them per **Unified dedup** (above) — against the full raw-findings
   corpus, then within this batch.
1. Write the survivors with
   `draft-write <prefix> <round+1> --batch <P>...` (run only
   if any survive), which assigns `R<round+1>-<NNN>` IDs.

For each reply the script reports `invalid`, nothing is written; add the finding
to the retry set. After the batch is fully processed, if the retry set
is non-empty, dispatch a new parallel batch — one
`inspect-changes-verify` Agent call per retry-set finding, same
prompt as the original attempt except for a fresh reply path.
Repeat until the retry set is empty. **Budget: 2 retries (3 attempts
total) per finding.** If any finding's third-attempt batch contains
an invalid reply, abort the review (see **Abort on retry exhaustion**
below).

This batch-then-retry pattern reflects foreground Agent dispatch: the
parallel calls return as one batch, so retries naturally
synchronize at attempt boundaries. Under background dispatch
(asynchronous result delivery), retries can fire per finding as each
result arrives, and the batch boundaries here are relaxed.

Once every finding in `draft-<round>.jsonl` has a valid verdict in the
verdicts file, if `draft-<round+1>.jsonl` exists and is non-empty, run
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

A verifier reply contains a `verdict`, or `experiment_requests`, or both (see
**Experiment deferrals**). The rules below judge the verdict part; a reply with
parseable requests and no verdict (a deferral) is not unusable. Beside parseable
requests, a `verdict` that is not an object or has no non-blank `outcome`
counts as absent.
`verify-record` applies these rules. A verdict (when present) is _unusable_ if
any of the following holds:

1. Agent invocation returned an error or timeout, or the reply file is missing,
   empty, or not a JSON object.
1. Reply lacks a `verdict` whose `id` is the finding's assigned ID.
1. Required field missing or empty. Always required: `outcome`,
   `final_confidence`, `verification_trace`. Additionally required when
   `outcome` is `keep`: `final_severity`, `final_provenance`, `final_title`,
   `final_location`, `final_observation`, `final_suggested_action`.
   Additionally required when `outcome` is `drop`: `reason`.
1. `outcome` is not `keep` or `drop`.
1. On a keep, `final_severity` is not `CRITICAL`, `IMPORTANT`, or
   `SUGGESTION`.
1. On a keep, `final_provenance` is not `introduced`, `pre-existing-on-path`,
   or `pre-existing-off-path`.
1. `final_confidence` is not an integer in `[0, 100]`.

These values are compared without regard to case or surrounding whitespace and
recorded in the case shown; `final_confidence` may also be a number with a zero
fractional part, such as `80.0`, or a string of digits, optionally followed by
`%`. The remaining fields are optional and recorded only as non-empty text: a
keep's `reason`, and a drop's `final_location` and `final_observation`.
Detection is purely structural. The top-level does not judge verdict quality,
only schema conformance.

#### Experiment deferrals

Run the experiment requests `verify-record` counts (read them with
`requests <reply>`) via the **Experiment requests** run
routine, appending results to the experiments file. Then:

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
list that is not parseable (see **Experiment requests**) is ignored, as in a
draft: `verify-record` records a verdict beside it and ends its status with
`; malformed requests ignored`, and a reply with no verdict is invalid under
the retry budget.

#### Abort on retry exhaustion

If a finding's attempt 3 reply also fails validation, the skill
aborts on that observation:

- No new retries are dispatched for any other finding.
- Other replies in the same returned batch are processed normally:
  valid replies contribute their verdicts and proposed new findings
  per Phase 2's regular flow; invalid replies contribute nothing.
- **Phases 3 and 4 do not run.** No `/tmp/inspect-changes-<topic>.md`
  is written.
- Return an abort message to the caller naming the exhausted
  finding(s) and pointing at the existing draft and verdicts files:

  ```text
  Review aborted in round <R>: finding <ID> exhausted retry budget
  (3 attempts, all failed). Inspect: /tmp/inspect-changes-<topic>-*
  ```

Other findings whose verdicts had already been written to the
verdicts file remain there — append-only is preserved, no rollback.
If a single returned batch contains multiple attempt-3 failures, all
are named in the caller message; the abort is still a single event.

### Phase 3 — Analysis of kept findings

**Reading persisted analyses.** `/tmp/inspect-changes-<topic>-analyses.jsonl`
holds one record per line, of kind `analysis` (with its `body`, and
`"provisional": true` when it is a provisional-body fallback), `rejected` (with
its `reason`), `corrected` (with the corrected fields and `rationale`), or
`skipped`. The reply script's `pending-analysis`, `analysis-record`, and
`assemble` read it.

After all verification rounds have converged, collect every finding
with `outcome` `keep` across all `verdicts-<round>.jsonl` files **that has
not already been analyzed in a prior Phase 3 pass**. The set of
analyzed finding IDs is derived from disk, not held in memory: an ID
counts as analyzed once it has an `analysis`, `skipped`, or `rejected` record
in `/tmp/inspect-changes-<topic>-analyses.jsonl` (see below);
`pending-analysis <prefix>` lists the rest with their verdict files. A finding
is analyzed exactly once even though Phase 3 may run several times. Spawn
one `inspect-changes-analyze` subagent **per not-yet-analyzed kept finding**, in
parallel — one message, multiple Agent calls (per the dispatch convention
above). That skill holds the per-finding analysis contract; each subagent's
inputs are the finding's ID and its verdict file,
`/tmp/inspect-changes-<topic>-verdict-<ID>.json`, the scope as a Git command,
the pre-image baseline, the paths of the prior draft files that exist, for
dedup, any experiment results (the matching `EXP` blocks) for that finding, the
same verbatim caller requirements, if present, its reply path, and — only for an
alongside-analysis re-spawn — the invocation mode `alongside` and the path of
the reply holding the complete latest provisional analysis. Also pass the explicit
WIP placement mode for an unborn HEAD, or — when committed placement applies
(computed below) — the stack as a list of SHA +
subject, which may be empty, the blame-target revision `REV`, and the rebase
boundary `BASE` when the stack is non-empty.

Before any committed placement computation, handle an unborn HEAD under
`git diff --staged`, or a bare `git diff` whose index is clean by the check
below. Confirm that HEAD is symbolic and its branch ref is missing; other
resolution failures remain errors. Pass **placement mode: WIP (unborn HEAD)**
to every analyst, with no stack, `REV`, or `BASE`. Skip SHA resolution, stack,
blame, and reachability queries in this mode. The analyst must explicitly
place fixes in the uncommitted change; absence of a commit is not an empty
stack. Continue the rest of Phase 3 normally. The rules below govern scopes
with an existing commit for placement.

On first entry to Phase 3, compute the **unpublished-commit stack context**
between repository-state checks and append it to the state file. Reuse it
across later verify⇄analyze passes only while those checks pass; resumption
recomputes it per [repository state](references/repository-state.md#resume).
Resolve `REV` to its full commit SHA before persisting or passing it.

Placement asks where a fix belongs, so it turns on where the **defect** is, not
on where the reviewed lines are. Under any committed scope — any single-commit
or range scope, `git show HEAD`, `git show <commit>` and a user-specified range
among them — every finding qualifies, and `REV` is as defined below.

Under `git diff --staged` the reviewed change is uncommitted, but a
`pre-existing-*` finding names a defect in the pre-image, which is `HEAD` —
committed, and possibly still amendable. Compute the stack there too, with
`REV = HEAD`, and pass placement context to every analysis subagent; the analyst
returns WIP for an `introduced` finding, whose fix really is in the uncommitted
change. Passing it to every subagent rather than gating at dispatch keeps that
provenance rule in one place — the analyst already chooses among (a)–(d) under
every scope — makes an `introduced` finding answer WIP explicitly instead of
omitting placement, and keeps the dispatch payload a pure function of the scope.
Both caveats this scope needs — that a finding's `final_location` line number is
a post-image number, and that the index holds the reviewed change, so "amend
`<sha>`" is not a bare `git commit --amend` — are stated in the analyze skill's
placement input bullet, which every analyst reads.

Under a bare `git diff` the pre-image is the index — which the precedence above
reaches only when `git diff --staged` is empty, i.e. when the index tree is
`HEAD`'s, so its content is committed after all. Ask the index rather than the
scope name: run `git diff --cached --quiet`, and on success treat this scope
exactly as `git diff --staged` above, `REV = HEAD` included. Only when it fails
— a user override selecting a bare `git diff` over a dirty index — is there
**no placement decision**, the pre-image then holding staged content that
belongs to no commit; skip the computation below, omit placement context from
the subagent prompts, and run the rest of Phase 3 unchanged.

Where placement applies, compute the stack with allowed commands only:

- Trunk branch = `main` if `git rev-parse --verify --quiet main` succeeds, else
  `master` if `git rev-parse --verify --quiet master` succeeds.
- Stack = `git log --oneline <trunk>..HEAD --not --remotes` — the local
  commits not yet contained in trunk and not reachable from any
  remote-tracking ref (each entry is a SHA + subject). `<trunk>..HEAD` alone
  says only "not in trunk", which a pushed feature branch also satisfies, so
  without `--not --remotes` the stack offers published commits for amendment.
  Remote-tracking refs record only the publication this checkout knows about,
  so the filter removes what is known published and leaves the rest _presumed_
  unpublished: a commit pushed from another clone, or whose remote ref has
  since been pruned, survives it. The stack is therefore a candidate list, not
  proof — pass it as one, so an amendment recommendation reads as contingent
  on the commit never having been published. If that range is empty
  _and_ HEAD is the trunk branch itself — i.e.
  `git rev-parse --abbrev-ref HEAD` equals `<trunk>` (not the literal `HEAD`
  of a detached checkout) — (you committed directly on trunk), recompute the
  stack as `git log --oneline HEAD --not --remotes` — the same publication
  filter, which subsumes the upstream range — when an upstream exists
  (`git rev-parse --verify --quiet <trunk>@{upstream}` succeeds): those
  un-pushed commits are still amendable. With no upstream configured, leave the
  stack empty. Retain the revision arguments of the query that selected the
  stack as `<stack-revisions>` for the topology walks below, including its
  publication exclusions and the fallback's omission of `<trunk>`.
- Topology marks, only for a non-empty stack: run
  `git log --oneline --first-parent <stack-revisions>` and mark every stack
  entry missing from it `[behind a merge]`; run
  `git log --format='%h %P' <stack-revisions>` and mark every stack entry
  listing more than one parent `[merge commit]`. These are independent
  properties: an entry may carry neither, either, or both marks. An inner
  merge brought in through an outer merge's second parent carries both.
- Rebase boundary `BASE`, only for a non-empty stack: take the oldest stack
  entry on the first-parent walk above and read its parents with
  `git log -1 --format=%P <oldest>`. Its first parent is `BASE`; no parents
  means `BASE` is the option `--root`. This boundary precedes the candidates
  even in the direct-on-trunk fallback, where the local trunk name is `HEAD`
  and would select an empty replay. Pass `BASE` with the stack; the analyst
  checks the full replay range before recommending a rebase, since marks on
  the owning commit alone say nothing about merges replayed after it.
- Blame-target revision `REV` — the newest reviewed revision whose tree holds
  the reviewed lines in final form. That criterion governs; the usual shapes are
  `HEAD` for a `git show HEAD` scope, `<commit>` for a `git show <commit>`
  scope, and the right-hand endpoint `B` of an `A..B`/`A...B` range (`HEAD` for
  the common "last N commits" case). Under `git diff --staged` — and under a
  bare `git diff` over a clean index, per the paragraph above — the reviewed
  lines are not in any revision, so the criterion has no instance and `REV` is
  `HEAD` — the pre-image, which is where a `pre-existing-*` defect lives and the
  only thing placement is asked about there. The merge intents need no exception
  of their own: each is one of those shapes by the time it is dispatched — the
  first-parent and back-merge diffs as endpoint diffs whose right endpoint is
  `<commit>`, and intent 3 as a `git show` of the commit its resolution step
  already named. `REV` is a pure function of the already-chosen scope and is
  identical for every analysis subagent, so compute it once here rather than
  having each subagent re-derive it.

An empty stack is a placement answer, not the absence of one: nothing is
amendable from this checkout, so every owned defect routes to a new commit.
That is the ordinary outcome when no trunk branch exists, when HEAD is already
merged into trunk or is a trunk branch with nothing un-pushed, and when the
publication filter above removes the whole feature branch. Pass the stack —
empty or not — and `REV`, plus `BASE` for a non-empty stack, into each analysis
subagent prompt (see the `inspect-changes-analyze` skill).

Placement context is omitted only where no answer exists at all: the dirty-index
bare `git diff` above, and a `REV` not reachable from `HEAD`
(`git log --oneline HEAD..<REV>` prints anything), where the reviewed revision
sits outside this checkout and the stack can say nothing about it. Omit it
there and run the rest of Phase 3 unchanged.

Each analysis reply is a JSON object with up to five parts: the `analysis` (its
`id` and Markdown `body`), a `rejection`, a `correction`, `proposed_findings`,
and `experiment_requests`. Only the body is inlined into the final review, under
an `#### Analysis: <ID>` heading; each other part is handled below.

A `rejection` means the subagent has proven this kept finding a false positive:
it is **removed** from the final review (the same end state as a verifier
`drop`). Verification keeps drafted findings; analysis can additionally reject
one but never resurrects a drop.

A `correction` revises the verdict's `final_severity`, `final_confidence`,
and/or `final_provenance` for a finding that stays in the review; Phase 4
overlays the corrected values on the verdict's. It corrects no other field, and
it never removes a finding — a corrected confidence below 50 still leaves the
finding in the review.

Run `analysis-record <prefix> <ID> <reply>` on each reply. It validates the
reply with the rules in **Analysis subagent reply validation** (rule 3 as
qualified there) and records each valid reply as follows, printing `rejected`,
`analysis recorded` (naming any corrected fields, and ending with
`; malformed requests ignored` when it kept a body beside unparseable or
contradictory requests, after `; rejection discarded` when the reply also
rejected), `deferral`, or `provisional analysis held`:

- **If it carries a `rejection`,** it appends a `rejected` record holding the
  reason to `/tmp/inspect-changes-<topic>-analyses.jsonl` instead of an
  analysis body. Like the `skipped` record, it makes the ID count as analyzed
  (terminal — never re-dispatched), and Phase 4 reads it to exclude the
  finding. The analysis body is **not** recorded (the finding is gone, so there
  is nothing to inline). `proposed_findings` on the same reply are still
  processed by the loop below.
- **Otherwise (no rejection),** it appends an `analysis` record holding the
  body. Then, if the same reply carried a `correction`, it appends a
  `corrected` record directly after it, holding each in-range corrected field
  and the `rationale`. Order matters: the body first, its correction after it.
  The one exception is the **alongside-analysis shape** — the reply also carries
  `experiment_requests` — which it holds as `provisional analysis held`,
  appending nothing. The body is held pending experiment results; the
  experiment-request handling below runs inline for this finding, re-spawns the
  analyst, and appends the finalized (or latest provisional) body instead — so a
  re-spawn intervenes before any append, and the body finally appended may
  differ from this provisional one. That inline alongside loop runs to
  completion before this finding counts as having a valid reply, so the "once
  every … has a valid reply" condition below is reached only after every
  finding's alongside loop has terminated. Throughout that loop the correction
  travels with the body: the one appended is the correction carried by the same
  reply whose body is appended, and a correction on a superseded provisional
  reply is discarded. At most one `corrected` record is written per finding,
  mirroring the analyze-once rule for bodies. To append the latest provisional
  body, run `analysis-record --provisional` on the reply that carries it; that
  mode records its body and correction and ignores its rejection and requests.

A defective `correction` is **ignored**, never fatal. A correction that corrects
nothing, gives no `rationale`, appears on a reply carrying no analysis, or
accompanies a `rejection` that removes the finding is dropped; one whose
`severity`, `provenance`, or `confidence` value is out of range (the
enumerations at **Phase 2**'s verdict rules, and an integer in `[0, 100]`) has
that field dropped and its remaining fields applied. In every such case no
record is written for the dropped part and the rest of the reply — body,
rejection, experiment requests, proposed new findings — is processed normally.
No defect in a `correction` makes a reply unusable, and none can void a
co-carried `rejection`: the correction is an optional qualifier, so discarding a
whole reply over it would trade a rendered field for an analysis or a proven
false positive. The analyze skill's own prohibitions stay as authoring guidance.

The append-only file `/tmp/inspect-changes-<topic>-analyses.jsonl` is written
solely by the top-level, through `analysis-record` and `analysis-skip`.
Phase 4 reads the bodies and records back from this file rather than from
memory, so a body produced in an early pass survives intact across any
number of later verify⇄analyze rounds. Invalid replies enter a retry set;
dispatch a new parallel batch, same prompts except for fresh reply paths.
**Budget: 2 retries (3 attempts total) per finding.**

**Exhaustion is non-fatal here.** Analysis augments findings and may
reject one (above), but its _absence_ is never load-bearing: a finding
with no usable analysis reply simply **falls back to its verifier
`keep`** — exhaustion never infers a rejection. If a finding's attempt 3
reply also fails validation, run `analysis-skip <prefix> <ID>`, which appends a
`skipped` record for its ID to `/tmp/inspect-changes-<topic>-analyses.jsonl`, so
the ID counts as analyzed and is never re-dispatched on a later pass — then
continue.
Phase 4 (final assembly) will include the finding (kept) without an Analysis
subsection and will note the omission in the Summary. Do not abort the review.

Once every not-yet-analyzed finding in the batch has either a valid
reply or has exhausted its retry budget, process the proposed new
findings from the valid replies:

1. Collect the `proposed_findings` with
   `proposals <prefix> <round+1> <reply>...` over all valid replies
   in the batch that report proposals, including any re-spawn replies produced by
   deferral or alongside loops (per the experiment-request handling below).
1. Dedup them per **Unified dedup** (above) — against the full
   raw-findings corpus, then within this batch.
1. Write the survivors with
   `draft-write <prefix> <round+1> --batch <P>...` to a new
   `/tmp/inspect-changes-<topic>-draft-<round+1>.jsonl` (run only if any
   survive), which assigns `R<round+1>-<NNN>` IDs.

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

An analyst reply contains an `analysis`, or `experiment_requests` (a deferral),
or both (an analysis with requests attached alongside). The rules below judge
the analysis part; a reply with parseable requests and **no** analysis (a
deferral) is not unusable on that account. `analysis-record` applies these
rules. A `rejection` counts only as non-blank text, or as an object whose
`reason` or `rationale` is non-blank text; any other value is absent. A reply
is _unusable_ if any of the following holds:

1. Agent invocation returned an error or timeout, or the reply file is missing,
   empty, or not a JSON object.
1. Reply lacks an `analysis` whose `id` is the finding's assigned ID —
   **unless** it is a deferral (parseable `experiment_requests` and no
   analysis). An analysis whose `body` is missing, blank, or not text counts as
   absent unless the reply carries a `rejection`, whose reason is then the
   content.
1. A `rejection` and `experiment_requests` are both present. The two are
   mutually exclusive: a rejection is a terminal decision, whereas a deferral
   means the analyst has not yet decided and needs evidence to do so — a reply
   that both rejects and requests experiments is contradictory and must never
   happen. A reply that also carries a usable analysis is the exception: its
   body and any `correction` are recorded, its rejection and requests are
   discarded, and you log the provisional-body completion limitation (see the
   alongside fallback below), naming the discarded rejection. The analyst's
   rejection is then not honoured: the finding stays in the review with that
   body as its analysis.

Carrying a `rejection`, `correction`, `proposed_findings`, or
`experiment_requests` does **not** by itself make a reply unusable —
`proposed_findings` are expected when the subagent spots a new issue, a
`rejection` when it proves the finding a false positive, a `correction` when
deeper study revises the verdict's severity, confidence, or provenance, and
`experiment_requests` when it defers or attaches a remedy-feasibility experiment
alongside its analysis. The top-level does not judge analysis quality, only
schema conformance.

An analyst that needs runtime evidence may return `experiment_requests` in one of
two shapes. Run any such requests (read them with `requests <reply>`) via the
**Experiment requests** run
routine, appending results to the experiments file, then handle by shape:

- **Requests with no analysis (a deferral):** the analyst cannot decide
  the finding yet — re-spawn it; the rebuilt prompt's "Any experiment results
  for this finding" bullet carries the new `EXP` blocks, so nothing is
  separately appended. Append no analysis body for this ID until the re-spawn
  produces one, so the ID stays unanalyzed and is re-dispatched. Any
  `proposed_findings` on a re-spawn reply are collected and fed to the
  proposed-new-findings step alongside the initial batch's.
- **Requests alongside an analysis (grounding a remedy):** the analyst has
  a usable analysis but flagged a suggested action whose feasibility it has not
  verified. Re-spawn it with the results, the invocation mode `alongside`, and
  the path of the reply holding the complete latest provisional analysis so it
  can finalize that suggested action. The re-spawn's outcome is one of:
  - **Finalized analysis** (no further requests): append it.
  - **Another alongside batch:** make its analysis the latest provisional one,
    run those requests, and re-spawn again with the same alongside state
    inputs. The alongside path loops like a deferral, except every reply already
    carries a usable provisional body, so it is never blocked.
  - **A pure deferral** (parseable `experiment_requests` and no analysis):
    loop-terminating — append the latest provisional body and exit the
    alongside loop. Do **not** enter a deferral sub-loop; the alongside loop
    started because the analyst already held a usable answer, and reverting to
    pure deferral from that state is a regression.
  - **A `rejection`** (and no `experiment_requests`): process it as a rejection
    exactly as the non-alongside rejection path above does — record the
    `rejected` record, do **not** append the provisional body, and terminate the
    loop.

  Otherwise the loop ends when the analyst finalizes, or — if a re-spawn fails
  validation or the 50-cap is hit — by appending the latest **provisional**
  analysis body instead (never discard it); the experiments stay in the file for
  audit. A re-spawn that fails validation in this alongside loop is **not**
  subject to the 3-attempt retry budget (which governs only the initial analysis
  dispatch and pure deferrals): the loop terminates immediately and appends the
  latest provisional body. Exactly one analysis body is appended per finding
  either way — or, on rejection, a `rejected` record instead — preserving
  analyze-once. Any `proposed_findings` on **any** re-spawn reply
  (finalized, intermediate alongside, or rejecting) are collected and fed to the
  proposed-new-findings step alongside the initial batch's. The analyst is the
  last tier, so its experiment results feed its own next pass rather than
  downstream — unlike Phase 2's "requests alongside a verdict," which never
  re-spawns.

Each re-spawn is a main-loop iteration under the single 50-iteration safety
cap — no separate experiment budget. Remaining edge cases follow Phase 3's own
**non-fatal** handling, not Phase 2's abort. When no provisional body exists to
fall back on (a pure deferral — whether the initial dispatch or a re-spawn,
which in either case carries no analysis): a reply carrying both a `rejection`
and requests is malformed and counts as an invalid analysis reply under Phase
3's retry budget (per the validation rule above); and a request list that is not
parseable (see **Experiment requests**) likewise counts as an invalid analysis
reply under that budget (→ `skipped` record, never abort). A reply that carries
a usable analysis beside either malformation — an
alongside reply, or an initial one — is recorded by `analysis-record` itself,
whose `analysis recorded` status ends with `; malformed requests ignored`; that
ends any alongside loop, never spends the retry budget, and is the
provisional-body fallback the Result contract counts as a completion
limitation. Only when an
alongside reply's own analysis is unusable does the loop fall back to the latest
provisional body (above). If the cap is hit while the analyst is still
deferring (no provisional body to fall back on), proceed without the experiment
and treat it as the analysis-exhaustion case (`skipped` record, noted in the
Summary).

### Phase 4 — Final assembly

Run the [repository-state check](references/repository-state.md#check-boundaries)
before assembly, and again before writing the report straight to its final
path; return it once. Its drift-stop path assembles only previously accepted
evidence, under the original basis.

Write the Summary's prose to `/tmp/inspect-changes-<topic>-summary.md`: the
2–4 sentences and every Summary item below that only the orchestration knows,
including the repository-state observation limit.
After the second check, run `assemble <prefix> --status <status>`, with
`converged` or `incomplete` per the Result contract, which writes
`/tmp/inspect-changes-<topic>.md`. Add `--merge combined` for a combined merge
scope (`git show <merge>`) or `--merge single-parent` for a single-parent one
(`git diff <merge>^<n> <merge>`, whether first-parent or back-merge).
It reads the scope, baseline and limitations files, the known-matches file
when a known-findings snapshot exists, every
`draft-<round>.jsonl` and `verdicts-<round>.jsonl` file, and the records in
`/tmp/inspect-changes-<topic>-analyses.jsonl` (see **Reading persisted
analyses** above), and computes the **rejected-ID set**: every ID with
a `rejected` record in the analyses file. From the kept verdicts and the analysis
bodies it renders every kept finding **except those in the rejected-ID set**,
appending to your prose the limitations file's entries and the Summary counts
the files settle. The report has this structure:

```markdown
# Code Review: <topic>

Status: <converged | incomplete, per Result contract>

Scope: <the derived reviewed scope command, with every revision pinned to its
SHA>

Review basis: /tmp/inspect-changes-<topic>-state.md

Baseline: <the value derived in **Scope**, rendered as it was derived: every
revision pinned to its SHA and named as the reader would recognise it — a
symbolic name where it has one, `merge-base(A, B)` for a three-dot range,
marked `(chosen; <n> merge bases)` where that base was not unique, each parent
listed for a combined-diff merge with the first marked — and a
non-revision value (`index`, `empty tree`) printed literally>

<the fixed confidence and provenance notes `assemble` prints>

<only for a merge scope: the fixed note `assemble` prints for its `--merge`
shape>

## Critical Issues

### R<round>-<NNN> — CRITICAL — <one-line title>

- Confidence: 85%
- Provenance: introduced | pre-existing-on-path | pre-existing-off-path
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

## Known findings matched

<only with a known-findings snapshot: the first known-matches line for each
known entry, or "None.">

## Summary

<your prose: 2–4 sentences, plus: drafts opened; verify⇄analyze passes run
(how many times analysis fed findings back to verification); re-draft passes
run (how many produced surviving new findings, and findings they
contributed after dedup; note any re-draft pass that failed on retry
exhaustion); findings proposed by analysis (and how many survived dedup);
findings dropped on a confidence below 50 rather than on the merits, judged
from each drop's `reason` (list IDs, if any); truncation note if the
50-iteration stop fired>

Completion limitations:

<appended by `assemble` only when the limitations file has an entry: that
file's entries verbatim>

Counts: <appended by `assemble`: total drafted; total kept; total dropped;
findings rejected on analysis;
total analyzed; analyses skipped, after retry exhaustion or a pure-deferral cap
stop; analyses recorded from a provisional body; the count of findings listed
in this review per final provenance value,
relative to the reviewed scope's pre-image (introduced / pre-existing-on-path /
pre-existing-off-path);
findings whose severity, confidence, or provenance analysis corrected — one
entry per correction as `<ID>: <field> <verdict value> → <corrected value>`;
with a snapshot, known findings matched>
```

`assemble` applies these rules:

- **Keep the original IDs.** A finding written as `R2-004` stays
  `R2-004` in the final file. No renumbering.
- **Use the verdict's refined content, not the draft's.** Each
  block's severity, confidence, provenance, title, location, observation, and
  suggested action come verbatim from the verifier's verdict; these
  supersede the original draft text.
- **Overlay any analysis correction.** If the finding has a `corrected` record
  in the analyses file, each corrected `severity`, `confidence`, or
  `provenance` it carries replaces the verdict's value for that field; fields
  the correction omits keep the verdict's. Severity grouping and ordering use
  the corrected severity. Retain the pre-overlay value for each field you
  replace — the Summary reports it. The correction's `rationale` is bookkeeping:
  it stays in the analyses file and is not rendered, so an analyst that wants
  its reasoning in the review states it in the analysis body, which is. A
  correction never removes a finding: one that drops confidence below 50 still
  appears, since only a verifier `drop` or an analysis rejection removes
  anything.
- **Inline analysis.** For each kept finding, render its `analysis` record's
  body verbatim under an `#### Analysis: <ID>` heading immediately after the
  `Suggested action:` bullet, closing any fenced block the body leaves open so
  it cannot swallow the findings after it. The analysis step asks for
  bold-paragraph labels instead of `#`-prefixed headings (see the
  `inspect-changes-analyze` skill); nothing enforces that, so a body that uses
  headings anyway is rendered as written, and only the report's outline suffers.
  If a kept finding has no
  analysis body in the file (only a `skipped` or `rejected` record, or nothing),
  omit the analysis for that finding.
- Group by final severity (Critical → Important → Suggestion).
  Within a severity tier, sort findings whose final provenance is
  `pre-existing-off-path` after all others — they are the only tier members the
  report itself calls independent of this change — then by `(round, NNN)`
  ascending.
- If a severity tier has no surviving findings, write "None." under it.
- Findings whose `outcome` is `drop` do **not** appear in the final file;
  they remain in their round's verdicts file (`verdicts-<round>.jsonl`) with a
  reason.
- Findings in the rejected-ID set (rejected on analysis) do **not** appear
  either; their `rejected` record and reason remain in the analyses file. A
  finding rejected on analysis is excluded even though its verdict is `keep`.

Do **not** modify the reviewed code. Return the **Result contract** envelope
with the final file path and summary. Leave all intermediate files in place
for audit.
