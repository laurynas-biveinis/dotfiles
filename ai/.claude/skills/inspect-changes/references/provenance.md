# Provenance

Each finding carries a `Provenance:` tag with exactly one of three values,
saying whether the reviewed change created the defect and, when it did not,
whether this change must address it anyway:

- **introduced** — the reviewed change created the defect.
- **pre-existing-on-path** — the defect predates the change, but addressing it
  is on the critical path for the change.
- **pre-existing-off-path** — the defect predates the change, and the change is
  correct and complete without addressing it.

## Determining it

The baseline is the pre-image of the reviewed scope — the side the diff's `-`
lines come from: `HEAD` for `git diff --staged`, the index for a bare
`git diff` (unstaged changes), `<commit>~` for `git show <commit>` where
`<commit>` is not a merge (so `HEAD~` for `git show HEAD`), `A` for an `A..B`
range (`git merge-base A B` for `A...B`), every parent for a combined diff
(`git show <merge>`), and the first parent alone for a first-parent merge scope
(`git diff <merge>^1 <merge>`). For explicit endpoint comparisons
(`git diff A B`), the baseline is `A`; for a working-tree or staged comparison
against an explicit revision (`git diff A` or `git diff --staged A`), it is
that revision. Scope normalization pins these revisions, including implicit
`HEAD` when it exists; use those SHAs throughout. A supplied baseline wins over
this mapping, but
never over the rule that content
present on any parent predates a merge reviewed as a combined diff. Under a
combined diff the commit has no single pre-image — a shown hunk differs from
every parent, yet the lines inside it can be one side's text verbatim; run the
queries below against each of the reviewed commit's parents
(`git rev-parse <merge>^@` lists them) and treat content present on any parent
as predating the merge. A first-parent merge scope
(`git diff <merge>^1 <merge>`) is the opposite case: its diff has exactly one
pre-image, so it follows the ordinary rules and only content already on that
parent is `pre-existing` — expanding to every parent there would tag the whole
merged-in branch, which is the change under review.
A root commit has no baseline, so everything in it is `introduced` — likewise
a `git diff --staged` scope under an unborn `HEAD`
(`git rev-parse --verify --quiet HEAD` fails): the pre-image is the empty tree,
there is no baseline revision to pass, and every defect is `introduced`. A bare
`git diff` keeps its index baseline under an unborn `HEAD`; only the
`--contents` attribution in the [attribution
procedure](provenance-attribution.md) is unavailable there.

The post-image — the content actually under review — is the other half, and
your file tools do not track it: `Read`, `Grep`, and `Glob` serve the working
tree. It is the right-hand side of the scope's diff: the indexed blob under
`git diff --staged [<rev>]`, read with `git show :<path>` for any path whose
`git status --porcelain` second column is non-blank; the working tree's tracked
paths under `git diff [<rev>]`, where `Read` is correct; and the scope's
right-hand revision otherwise — `HEAD` for `git show HEAD`, `<commit>` for
`git show <commit>`, `B` for an `A B`/`A..B`/`A...B` comparison, the merge itself
for either merge shape — read with `git show <rev>:<path>`. Unlike the pre-image,
the post-image is one tree even for a merge, so no per-parent expansion arises
here. Prefer the `git show` forms because they are self-diagnosing where `Read`
is silent, failing with `fatal: path '…' exists on disk, but not in the index`
or `… but not in '<rev>'` and naming the hazard, where `Read` returns content
that is in neither image.

`Grep` and `Glob` cannot search either image, and both — like `Read` — return
untracked paths, which are in no image under any scope and come back silently:
the sharpest case. So confirm a hit against the diff or the post-image blob
before asserting it of the reviewed content, and note that a hit in a region no
hunk covers is not thereby context. Git's own search reads the right image
directly and answers absence as well as presence, exiting 1 on no match:
`git grep --cached <pattern> -- <paths>` for the index, and
`git grep <pattern> <rev> -- <paths>` for a committed post-image — pattern
first, per `git grep [<options>] [-e] <pattern> [<rev>…] [[--] <path>…]`;
reversed, it either dies or silently greps the working tree for the revision
name. A bare `git grep <pattern>` searches tracked working-tree files, which is
`git diff [<rev>]`'s post-image exactly and the wrong image under staged or
committed comparisons. `Glob`'s counterparts are
`git ls-files --cached -- <pathspec>` and
`git ls-tree -r --name-only <rev> -- <pathspec>`.

Reading the post-image correctly governs every
claim you make about the reviewed change, not only this tag.

`pre-existing` is relative to that pre-image, not to the author's in-flight
work: a defect an earlier **unpublished** stack commit introduced is still
`pre-existing`. Whether to fix it by amending that commit is the analysis step's
placement question, not this tag's.

A verifier's `Final provenance:` is authoritative unless superseded by a valid
analysis correction for that field.

These rules locate the defect's origin, not the diff's `+` lines: throughout,
the tag follows the defect, not the line.

1. Flawed lines among the scope's added or modified lines are `introduced`, as
   is a defect the scope's **removals** create — a deleted guard, check,
   cleanup, or test — even though no added line carries it. Two exceptions
   share one shape, the line changed but the flaw did not:

   - relocated pre-image content — a move, rename, extraction, or file split
     presenting unchanged lines as added ones; the relocation itself is not
     authorship;
   - a flaw that survived an in-place edit — the modified line's pre-image
     counterpart in the same hunk already carries the same defect, so the edit
     inherited it rather than authoring it.

   In both, keep reading rather than tagging `introduced` here. If the
   relocation is what makes the content wrong — moved out from under a
   caller-held invariant, or lifted onto a live path — that is the next rule;
   otherwise rule 3 decides.

   To tell whether a relocation is in play, re-run the scope command with
   `--color-moved=zebra`, adding `--color-moved-ws=allow-indentation-change`
   (an extraction usually re-indents) and `--color=always` (Git drops color
   into a captured pipe, and without it the marking is silently absent);
   `--color-moved` is a diff option, so a range and `git show` take it too —
   with one exception. A combined diff (`git show <merge>`, the default for a
   merge) runs no move detection at all: it accepts the flag and marks nothing.
   Probe each parent instead, with a two-tree
   `git diff <parent> <merge> --color-moved=zebra`, which the per-parent rule
   above already licenses — a mark there means the content was on that parent,
   hence pre-existing — or let rule 3's per-parent baseline read decide. It
   marks lines that moved rather than appeared. `git diff -M` catches
   whole-file renames only, so it sees neither an extraction nor a file split.
   A mark proves relocation; its absence does not disprove one — `zebra`
   detects greedily and only blocks of at least 20 alphanumeric characters,
   line matching is exact unless `--color-moved-ws` relaxes it, and the
   matching removal must also fall inside the scope.

2. Unchanged lines the change _made_ wrong are `introduced` too — a new caller,
   an altered invariant or contract, a latent defect the change makes
   reachable.

3. Otherwise ask whether the defect is already present in the baseline image.
   First check that the site is in an image at all. A path in neither the
   pre-image nor the post-image is outside the reviewed content, and none of
   the three values describes it — `introduced` would claim the change authored
   it, and both `pre-existing` values would claim it predates the change.
   Re-locate the finding onto the in-image content that depends on that path: a
   reference the change adds to a file it does not contain is `introduced` at
   the referencing line. Omit the finding when nothing in either image depends
   on it. For a line the scope's diff shows as context, the diff settles it.
   For a path or line the diff does not cover, read the baseline blob —
   `git show <baseline>:<path>`, or `git show :<path>` when the baseline is the
   index. Where rule 1 identified the content as relocated, ask this at the
   source path, not the finding's. An in-scope relocation needs no read: a mark
   requires the matching removal inside the scope, so the removed lines are
   already in the diff and settle it. Otherwise read the baseline at the source
   path the mark or the removed hunk names. Neither outcome at the post-image
   path is evidence about the baseline — a `fatal:` there says only that the
   path is new, and a successful read only that the defect is absent from that
   file. If the defect is present — including when its origin is the baseline
   commit itself, or the index under a bare `git diff` — decide the
   critical-path question below. Tag it `introduced` only when the defective
   content is absent from the baseline entirely, its origin lying strictly
   after it: an intermediate commit of an `A..B` range, typically arriving here
   through rule 1's relocation exception.

   Attribution — naming the commit that owns the content — is needed only to
   confirm a relocation and to resolve a path the scope's diff does not cover.
   The analysis step's placement question is answered at the blame-target
   `REV` instead, per the `inspect-changes-analyze` skill, so the procedure does
   not apply to it, nor to a first-pass tag — a draft finding's, or a
   `## Proposed new findings` entry's from any tier — which a cheap baseline
   read settles. Where it does apply, the commands and their traps are in the
   [attribution procedure](provenance-attribution.md) — read that before
   running any of them, because several fail in ways that print a plausible
   wrong answer rather than an error.

## Critical path

A pre-existing defect is `on-path` when the reviewed change is wrong,
incomplete, or unsafe until that defect is addressed: the new code depends on
the broken behavior, or builds on it so directly that fixing it later forces
this change to be reworked, or the change propagates the same flaw. It is
`off-path` when the fix stands on its own — an independent issue in a touched
file, an opportunistic cleanup.

## Tie-breaks

Without evidence that the change caused the defect, tag it `pre-existing-*`
rather than assert authorship. When on-path versus off-path is genuinely
uncertain, tag `pre-existing-on-path`.
