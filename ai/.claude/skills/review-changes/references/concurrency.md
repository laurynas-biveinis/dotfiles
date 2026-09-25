# Concurrent repository changes

The outer workflow can reconcile changes and continue. An inner inspection
has an immutable basis: its drift stop ends that inspection, not necessarily
the outer run. Never accept stale inspection or validation evidence as current.

## Repository record

At run start, before selecting scope, capture each checkout the run will read
or mutate. Use the observation inventory in the inspection skill's
[Capture the basis](../../inspect-changes/references/repository-state.md#capture-the-basis),
including refs, index entries and flags, ancestry metadata, operation state,
and recursive submodule observations. Use this reference's storage and
reconciliation rules instead of the inner skill's immutable-basis and stop
rules. A failed observation is not an empty or unchanged state.

Supplement that inventory with the per-worktree paths `CHERRY_PICK_HEAD`,
`REVERT_HEAD`, `REBASE_HEAD`, `sequencer`, `rebase-merge`, and `rebase-apply`.
Resolve each with `git rev-parse --git-path` using the inventory's Git-path
rules. Record explicit absence or complete file contents; for directories,
record presence, recursive membership, file types and contents, and symlink
targets. Retain enumeration and read failures. Include these observations at
initialization, resume, and every checkpoint under the attribution and
reconciliation rules below. `MERGE_HEAD` alone misses an empty cherry-pick
and edits to a paused rebase's remaining instructions.

Keep the following in the outer ledger, with complete observations in linked
scratch files when needed:

- **Observed state:** checkout/Git directory identity, branch and HEAD, and
  the inventory above. Key path records by checkout and path. Include index
  modes, stages, blob IDs and flags; working-tree file type, executable mode
  and content identity (raw bytes or symlink target); and explicit absence for
  removed paths. Enumerate untracked files individually, and record the same
  identity for each one an operation or in-place check may read or write:
  names alone cannot detect its edits. Do not rely on timestamps, dirty
  markers, or diff summaries. Include ignored paths if an operation will
  touch them; never clean unrelated ignored files.
- **In-flight mutation:** before each write, its starting observations,
  intended paths/refs and layer, expected changes, and recoverable inputs.
  For a rewrite whose new SHAs are not yet known, verify its produced content
  and topology against those inputs before attributing the new refs to it.
- **Recovery obligations:** saved content and staged/unstaged split, the
  workflow operation that disturbed them, paths still needing restoration,
  and the agreed disposition of each restored or deliberately retained path.
- **Pending concurrent changes:** before/after identities, affected entries
  and their selected paths before and after the change, plus any user decision.
  A change remains pending until the user has addressed it, even if it was
  safe to continue without asking at its first observation.

Supersede settled observations in place; do not create stash commits merely
to identify state. Preserve the old values needed by pending events and
recovery obligations. Updating an observation never acknowledges an event,
finishes a restoration, or validates review evidence.

Capture again after initial scope selection and reconcile any difference
before using that selection. Establish the same record before first using
an additional checkout. On resume, load the saved record and in-flight
operation before observing afresh; classify any interrupted operation's
effects before continuing. If the record or recovery evidence is missing,
preserve what remains and ask how to recover; do not silently re-baseline.

## Checkpoints and attribution

Re-observe and compare against the ledger:

- Before resolving any entry's current state and scope, and before dispatch.
- After every inner result, before using its findings or completion status.
- Immediately before and after every repository mutation: fixes, staging,
  checks that can write, history operations, stashing, restoration and cleanup.
- Before any `converged`, `empty` or `eliminated` mark and before final success.

Observe all recorded checkouts, without review path filters. Compare complete
identities and command statuses, retaining failures. A failed check blocks
dependent work until it can be completed; it never permits a refresh that
would discard the last known state.

Before a mutation, settle the comparison and record the operation's expected
effects. After it, compare against that expectation _before_ advancing the
record. Only demonstrated effects belong to the workflow; unexplained or
ambiguous differences use the concurrent-change procedure below. Changes the
run's own checks produced, including any formatter edits, follow history's
**Validation state** instead. Running a check does not make every change
observed during it check-produced.

For each difference, preserve the before/after evidence, classify its origin,
record unresolved recovery obligations and pending events, and invalidate
affected evidence. Then advance the observed state. It may describe a partial
operation; the separate obligations remain open until resolved. Never use an
old observation as a blanket restoration target.

## Reconcile and continue

For concurrent file or index changes, keep the user's current content and
staging. Re-derive affected scopes using the existing logical entries, filters
and merge intent; do not rerun default precedence and select a different task.
Test overlap against the union of selected paths **before and after** the
change, including both sides of renames. A path wholly unstaged or removed
from the new diff still overlaps its old selection.

Ask before continuing affected work when a concurrent change overlaps a
queued entry's old or new selection, or the next mutation's paths/refs. Also
ask unconditionally for a skipped restoration path or an out-of-band branch
or HEAD change. Before any history rewrite, present all still-pending events,
including unrelated changes recorded at earlier checkpoints, and resolve them
with the user. State the concrete difference and proposed continuation or
recovery; an earlier generic rewrite authorization does not settle newly
observed changes. A decision covers that event and proposal, so do not ask
again while both remain unchanged. Recheck after the answer before acting.

Unrelated changes can be recorded and preserved without interrupting work;
they stay pending for the pre-rewrite gate. Re-evaluate pending events when
scope or mutation footprints change. A completed run may leave unrelated
events unasked if it needs no rewrite; report their preservation. An unanswered
required question suspends dependent work, while independent authorized work
may continue. If no safe continuation can be established, finish incomplete
with the ledger and recovery data intact.

Invalidate checks and inspections whose scope, baseline, post-image or
supporting evidence changed, even when the edited path is outside the diff.
Recheck handled-above dependencies and empty/eliminated evidence too. If
independence cannot be established, invalidate conservatively. Mark affected
entries unhandled and return to the lowest one after reconciliation.

If the inner stops on its repository-state check — drift or a failed state
observation — preserve its incomplete/not-run result and reconcile even if the
outer's next observation matches its prior record:
the inner's detected event must not be lost. Revalidate the entry and dispatch
a fresh inner inspection with no old findings as input. Other inner failures
keep the entrypoint's incomplete-result handling. A converged inner result
whose basis no longer matches the entry is stale and likewise needs a fresh
inspection; it cannot authorize fixes or convergence on the changed state.

## Restoration

Before each restoration write, compare current paths, index layers and refs
with the expected state left by the workflow operation. Restore only what the
workflow disturbed, preserving the user's later changes, including deliberate
staging or unstaging. Do not force a saved index, tree or stash over them.
Use recovery copies to prepare a concrete reconciliation when both versions
need to survive. A conflict is unresolved recovery, not permission to reset.

If a path cannot be restored safely, leave it untouched, retain its copies,
record the skipped restoration, and ask even when it lies outside review
scope. Safe independent paths can be restored while that decision is pending.
After each write, verify the expected result under the mutation protocol.
Restoration is complete only when every obligation is either verified restored
or explicitly resolved by the user's decision to retain a different state.
Record that decision and keep recovery data for any deliberately unrestored
content. Equality with the original snapshot is not the completion criterion.

## Ref and layer changes

An out-of-band branch or HEAD move always needs a user decision before
continuation. Preserve the new refs. Compare old and new topology and content
to identify which queued logical changes, if any, acquired a new address;
neither matching subjects nor the new HEAD alone establishes a mapping.

- A new commit above a captured committed endpoint does not move the endpoint
  or enter the queue. A branch switch does not retarget the run. Prepare an
  isolated checkout at the recorded endpoint if needed to continue there.
- A user rewrite can re-address an entry only with a verified counterpart.
  Keep the captured endpoint's identity; update its current address only when
  that same logical change has a proven replacement. Leave unaffected entries
  pinned. Show the proposed mapping when asking to continue.
- Moving selected work between index, working tree and a commit needs a
  corresponding layer/commit mapping agreed with the user. Never stage it back
  merely to recreate the old scope. A missing counterpart follows the
  entrypoint's **Eliminated changes** rules: user removal, a now-empty diff or
  unexplained disappearance alone cannot establish remediation or convergence.
  If those rules do not apply, finish incomplete unless the user supplies a
  new review request.

After an agreed mapping, apply history's **Update the queue** invalidation and
dependency rules only to the affected entries and descendants through the
logical endpoint. Entries above user-rewritten history lose completed review
even if their patches are unchanged. Re-establish publication and placement
evidence before proposing any further rewrite; the move itself grants no
rewrite permission. Never expand the run to unrelated branches or new work
above its endpoint.

A user rewrite below the lowest queued entry changes that entry's baseline
but does not itself enqueue the rewritten lower history. Lower entries enter
the queue only when required by remediation under the existing placement rules.

## Observation limits

Checkpoints do not lock the repository: edits made and undone between them
can escape detection, as can races between a check and a write. State this
limit in the result. Prefer isolated checkouts for broad operations and
expected-old-value checks when publishing refs or applying prepared changes.
If safe application requires exclusive access to a shared path, prepare the
result in isolation and ask for that access before writing it. Repeated drift
without a safe continuation ends incomplete; never force restoration to make
the record match.
