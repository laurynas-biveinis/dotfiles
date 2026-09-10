# History and scope

## Initial queue

Use only the **Scope** section of the sibling
[inspection skill](../../inspect-changes/SKILL.md#scope) to select and normalize
the initial scope, including conflict checks, merge intent, exclusions, and
baseline derivation. The outer selects; a separately spawned inner reviewer
performs each inspection. Preserve the user's requirements independently of
the Git scope.

Capture the selected logical endpoint once. It is the ceiling of the run, not
whatever `HEAD` later happens to name. Initialize the queue with only that
requested change, or the commits of an explicitly requested range, ordered
ancestors before descendants. For a range, use its resolved pre-image and
right-hand endpoint. Before enumerating commits, run
`git merge-base --is-ancestor <pre-image> <endpoint>`. Exit 0 permits traversal;
exit 1 means this remediation workflow does not support the requested
non-ancestor comparison. Finish incomplete before mutation, explaining that
commit traversal would omit part of the scope and standalone `inspect-changes`
can report on the original endpoint comparison. Any other status is a command
failure: preserve Git's diagnostic and stop. Never substitute a merge-base
comparison; a three-dot scope already has its selected merge base as pre-image.

Enumerate the selected commits only after that gate passes, preserving
pathspecs and filters when reviewing each commit. Do not seed the queue with
the whole unpublished stack. Treat a selected staged or unstaged change as the
final uncommitted entry; do not commit it merely to give it an identity.

For a nonempty requested range, preflight all initial members before mutation
under the entrypoint's **Empty range members** rules. Keep members without
selected changes in the logical queue, including an empty endpoint, with their
comparison evidence; do not substitute a different endpoint or dispatch an
empty member to obtain convergence.

For every iteration derive the entry's scope from its current SHA and parents,
or its current index/working-tree layer. A rewritten SHA changes the address,
not which logical entry is under review. Re-derive the pre-image after a
prerequisite changes; an old pinned parent is not the new baseline. Preserve
the selected merge intent. The inner skill's range inspection remains an
endpoint diff for standalone callers; the outer intentionally walks commits.

If a requested committed endpoint is outside the current checkout's ancestry,
prepare an isolated checkout at that endpoint before remediation. Do not
interpret this checkout's stack as permission to amend a different history.

## Preserve and place

Before mutation, record branch/HEAD, index and working-tree differences, and
untracked files. Save recoverable copies before stashing or rebasing. Keep
unrelated work out of fixes and commits, including unrelated hunks in the same
file. Restore the exact staged/unstaged split; do not assume a plain stash pop
preserves it. Retain recovery data until restoration is verified. Stop on
unexpected concurrent edits and reconcile them before continuing.

For uncommitted introduced fixes, update only the reviewed layer and necessary
new files, staging explicitly when the entry is staged. Preserve any separate
unstaged edits to the same paths. For a new file belonging to an unstaged fix,
use `git add --intent-to-add -- <path>` so the next unstaged diff includes it
without staging its contents. Leave unrelated untracked files alone. For a
historical fix, isolate its target state before applying and testing it; an
amendment must not absorb the current index's unrelated or upper-stack changes.

Use the inner analysis's ownership and placement evidence. Its unpublished
stack is a candidate list, not proof of never having been published. Before
rewriting, establish that the full replay range is unpublished using session
evidence, commits created by this run, or user confirmation; inspect current
remote refs as corroboration. Do not infer permission from stale or absent
remote-tracking refs. Existing authorization remains valid; do not ask again
for the same operation or boundary. When publication or placement is unresolved,
prepare the patch and concrete placement before requesting that missing input.

- An introduced finding belongs in the currently reviewed commit or
  uncommitted layer, even if its remedy edits previously existing code.
- A pre-existing on-path fix belongs in an earlier unpublished commit when it
  corrects that commit's own change. Otherwise insert a distinct prerequisite
  immediately before the currently reviewed entry, with any prerequisites it
  needs already below it. Never append it above the reviewed change as a
  substitute for the requested placement.
- An uncommitted baseline can itself contain the owning change, such as staged
  work below an explicitly selected unstaged diff. Do not guess a committed
  owner or silently commit that work: prepare the fix and resolve placement
  with the user if it cannot satisfy the rule while preserving their layers.
- Never amend published history. If the required placement would rewrite it,
  prepare the fix and report the placement blocker.

Inspect the entire replay range for merges, not only the amended commit.
Preserve topology with `--rebase-merges` when necessary and reapply both merge
resolutions and other merge-authored changes. An earlier merge needs a stop
after its merge instruction for amendment, not a nonexistent `pick` line.
The inspection analysis carries the detailed ownership/rebase guidance; do
not replace it with a blanket autosquash recipe.

## Validation state

Before every inspection, materialize the entry's full post-image, including
its checker, configuration, file modes, and submodule revisions. A staged
entry uses indexed content; an unstaged entry uses its working-tree post-image;
a committed entry uses that commit's content, without later commits. Review
filters do not trim the validation snapshot. Use an isolated checkout when
the primary checkout differs or checks could disturb unrelated work. Direct
checks are valid only when both content identity and preservation are assured.

Record the checked content's identity and check results in the ledger. Compare
source content before and after checks, including any formatter edits. Apply
applicable changes to the proper reviewed layer, preserving unrelated edits
and following the placement and queue-update rules for any history rewrite.
Changes outside authorized scope remain unresolved; never silently absorb or
discard them. Rerun required checks after reconciliation, and confirm that the
entry sent to inspection matches the validated content. Invalidate this
evidence whenever relevant content changes.

Run `./check.sh` after each change when present and appropriate project checks
otherwise. Validate each fix at its owning state, then validate the restored
integrated endpoint under the same contract. Fix introduced check failures;
never suppress them or silently fix unrelated baseline failures. If another
decision is required, retain the evidence and finish incomplete rather than
claiming checks passed.

## Update the queue

After every insertion, amendment, or replay, map the old logical entries to
their current commits and update the endpoint, scopes, and resolution
dependencies. Validate this map against topology and content, not commit
subjects alone. A dropped or squashed target needs an explicit mapping to its
replacement change when one exists; it is not automatically converged or the
new `HEAD`. If validated remediation removed it entirely, retain its logical
entry under the entrypoint's **Eliminated changes** rules instead of inventing
a replacement mapping.

Mark the changed entry and every descendant on the path through the original
endpoint unhandled, adding intervening commits not previously in the queue.
Review newly inserted prerequisites as their own changes. Even an unchanged
patch above a changed foundation loses its completed review. Then choose the
lowest unhandled entry; with merges, ancestors precede dependent merges and
descendants. Never enqueue unrelated branches, unchanged lower history, or
commits above the original endpoint.

Recheck handled-above dependencies as the entrypoint specifies: editing a
higher remedy can reopen a lower entry as well. Retain previously recorded
off-path dispositions, but reassess their applicability against current
provenance and evidence. Commit remapping never authorizes suppression of a
recurring unresolved defect.

Revalidate previously empty members under **Empty range members**, retaining
their initial comparisons alongside the current evidence. A rewrite that makes
a previously nonempty member disappear cannot reclassify it as initially empty.
