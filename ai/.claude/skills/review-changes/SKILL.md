---
name: review-changes
description: >-
  Review code changes, fix introduced and prerequisite defects, track
  independent pre-existing findings, and repeat from the lowest unhandled
  commit through the original changes. Use after a substantive feature,
  bugfix, or refactor at a logical-commit size. Skip routine mechanical changes.
---

# Review Changes

Own remediation and convergence. Use `inspect-changes` for independent
reporting; this outer skill owns project edits, checks, history changes, and
tracking operations. Apply caller requirements verbatim throughout.

## Start

Create a unique `/tmp/review-changes-<topic>-state.md` scratch ledger before
the first repository observation, deriving `<topic>` from the request and
working directory because no diff is known yet. Read
[concurrent changes](references/concurrency.md) and capture its initial
repository record. Then read
[history and scope](references/history.md) to establish the original target,
initialize the unreviewed stack, and preserve the user's work. Use the
scope-selection rules from `inspect-changes` once, without running a review
during selection. Initially only the requested scope is unreviewed.

Record the original endpoint, current logical entries and their SHAs or
uncommitted layers, scope filters, review states, inspection paths, finding
dispositions, resolution dependencies, and tracking results. Keep the repository
record, in-flight mutations, recovery copies and restoration obligations, and pending
concurrent changes in this ledger as the concurrency reference specifies.
Keep a cumulative list of records created for off-path findings, with their
returned URLs/Org URIs; later completion or reclassification never removes an
item from that list.

An entry is `unhandled` until a complete inspection produces no new findings
after the filtering below and all required actions are handled. Its state then
becomes `converged`. Initially empty range members use `empty`; validated
removal uses `eliminated`, under the rules below. Record state changes as work
happens so a resumed run can reconstruct the queue. On resume, reconcile the
repository against the saved record before using the queue or taking a new
baseline; follow the concurrency reference if the record is missing.

## Inspect and handle

1. Select the lowest unhandled entry, with ancestors before descendants.
   Apply the concurrency checkpoints throughout this loop, including before
   resolving scope, after an inner result, and before accepting completion.
   Resolve its current state and scope per the history reference. Validate its
   exact post-image under that reference's **Validation state** contract,
   including isolation and reconciliation of formatter edits. Run required
   project checks before inspection: `./check.sh` when present in that state,
   otherwise the project's relevant checks. Record the checked content and
   results, and confirm inspection will receive that same content. Never
   suppress failures. Pass any known blocker to the caller and finish
   incomplete if it cannot be resolved within scope.
   Before dispatch, retire an eligible entry per **Empty range members** or
   **Eliminated changes**, then restart this step for the next unhandled entry.
1. Spawn a fresh reviewer subagent to read the sibling
   `inspect-changes/SKILL.md` and follow it, supplying the current explicit
   scope and the original caller requirements. Resolve the sibling from this
   skill's installed directory, as the inner dispatch convention does. Include
   `Base directory for this skill: <absolute inspection directory>` in the
   reviewer prompt, naming that sibling `inspect-changes` directory alongside
   its absolute entrypoint path. The reviewer owns its inner subagents and
   scratch reports. It cannot edit the project or file records. Give it no
   prior outer findings or focus steering. Wait for its result before mutating
   the reviewed state.
1. Read its result envelope and final report. Only `Status: converged` supplies
   a completed inspection. For an inner repository-state stop — drift or a
   failed state observation, named in its `Reason` or limitations file —
   reconcile under the concurrency reference and dispatch a fresh inspection
   when cleared; never promote that stopped inspection to completed. Any other
   incomplete, aborted, missing, or malformed result never counts as no
   findings; preserve its evidence and report the blocker.
   A `not-run` result also cannot establish convergence.
1. For each surviving finding, apply **Handled above** first. For the rest,
   match against the outer ledger semantically by defect and affected behavior,
   following moves and rewrites rather than comparing titles, line numbers,
   SHAs, or inner review IDs. Use the report's final provenance, including
   analysis corrections, and the analysis's recommendation. Reassess the
   disposition on new evidence: a known off-path finding that becomes on-path
   now needs a fix. A previously attempted fix that still fails is unresolved.
1. Handle every remaining finding per **Routing**, prioritizing severity and
   dependencies. All severity levels receive action. Read current content
   before applying each recommendation; a fix earlier in the batch may have
   resolved another finding or changed its placement. Validate each logical
   change and update the queue after every rewrite.
1. Select the lowest unhandled entry again. If this iteration produced any
   newly handled findings, inspect again even when handling only filed records,
   except for entries retired under **Empty range members** or
   **Eliminated changes**.
   Otherwise mark the inspected entry converged only if its reviewed state is
   still current and no required action remains. An entry invalidated by a
   lower change must receive a fresh inspection before convergence.

### Handled above

Before any fix or tracking operation, check whether the same finding is already
fully addressed in higher entries through the original endpoint, including the
original uncommitted layer if present. Read their current diffs and content;
the remedy must still hold at the endpoint. An intention, partial fix, or
existing issue/task alone is insufficient. A fix subsequently reverted or a
similar change for a different defect is insufficient too.

Drop a finding proven handled above, regardless of severity or provenance. Do
not move its remedy down, fix it again, or create/update tracking records for
it. It contributes no new finding to the outer convergence test. Keep the
inner report unchanged and record the evidence, originating review entry, and
resolving higher entry in the ledger.

These drops depend on current code, not merely on the presence of a commit.
After changes to a resolving entry or any descendant through the endpoint,
revalidate its dependent drops. If the remedy no longer holds, mark their
originating entries unhandled again. Remap dependencies after history rewrites;
never carry forward evidence tied only to an obsolete SHA. Verify remaining
dependencies before declaring the whole run complete.

### Empty range members

After a requested range passes initial Scope validation, preflight each
member of its initial queue before any mutation. Record its normalized
comparison, baseline, endpoint, filters, and result in the ledger. An ordinary
comparison that succeeds without patch headers, as defined by the inspector,
establishes that this member initially contains no selected changes. Once
required checks pass and no caller requirement or finding handling remains,
mark it `empty`. Retain the logical member and original endpoint boundary,
including when the endpoint itself is empty. Skip these entries during
inspection without calling them inspected or converged.

This disposition applies only to derived members of a nonempty requested
range. An entirely empty original request still stops. A failed comparison
also stops; an empty combined merge comparison retains the inspector's
resolution-specific stop because it cannot prove absence of resolution work.

Revalidate the evidence after relevant rewrites using the current entry and
baseline, preserving filters and merge intent. Reopen an entry if its comparison
now contains changes. Never use `empty` for a member previously shown nonempty:
its disappearance requires **Eliminated changes** evidence or stops the run.

### Eliminated changes

A previously inspected change may be intentionally removed in full by its
remediation. Mark that logical entry `eliminated` only after recording the
inspection and before/after evidence against its correctly derived current
baseline. Confirm that the change was removed, not hidden by changed filters
or moved into another staged/unstaged layer, and that no caller requirement or
finding handling remains outstanding. Required checks must pass at the affected
state and integrated endpoint.

Keep the original logical entry and endpoint boundary in the ledger without
inventing a replacement change. Skip eliminated entries when selecting work,
but retain surviving prerequisites and invalidated descendants in the queue.
Revalidate handled-above dependencies and elimination evidence after relevant
rewrites; reopen an entry if that evidence no longer holds. An initially empty
requested scope or unexplained disappearance still stops and cannot establish
elimination.

### Routing

- `introduced`: fix within the currently reviewed change. Amend its unpublished
  commit, or preserve its staged/unstaged layer when uncommitted.
- `pre-existing-on-path`: deduplicate in GitHub, when applicable, and Org first.
  Fix in an earlier unpublished commit that owns the change, or insert a
  separate prerequisite below the reviewed entry.
- `pre-existing-off-path`: deduplicate in both systems, then create only missing
  tracking records. Do not edit the code for this finding.

Read [tracking](references/tracking.md) before handling either pre-existing
class. Existing tracking records never change this routing. Read
[history and scope](references/history.md) before implementing a fix or
changing its placement. If a recommendation requires an unresolved design
decision, prepare the concrete alternatives before asking; do not silently
substitute a different remedy.

During testable fixes, follow TDD; its logical-commit review step returns to
this active loop rather than starting another `review-changes` invocation.
Do not run several remediation writers concurrently.

## Finish

Finish successfully only when the original endpoint and all lower queued
entries are `converged`, `empty`, or `eliminated`, all handling is complete, and
handled-above dependencies and empty/eliminated evidence still hold. Resolve
all restoration obligations and required concurrent-change decisions first.
Never expand beyond that endpoint. There is no arbitrary outer iteration cap. An
unresolved blocker or a repeated fix attempt with no new evidence or viable
remedy ends incomplete, with recoverable state. Repetition alone never makes
an unresolved finding handled. Continue independent authorized work before
returning a blocker, including preserving any outstanding tracking content
per the tracking reference.

Write `/tmp/review-changes-<topic>.md` and return its path with a concise
outcome. Include fixes and placement, validation, findings dropped as handled
above, empty and eliminated entries, remaining blockers, and why traversal
stopped.
Refer to findings by their text, never by ephemeral inner review IDs.

At every termination, state the concurrency reference's observation limit and
list concurrent changes preserved without a user decision.

At every termination, including an incomplete run, present **all GitHub issues
and Org items created for pre-existing off-path findings across every
iteration**. Group them by finding with descriptive titles and clickable
GitHub URLs and Org links, including records subsequently resolved. Label each
created record; show reused or reopened records as such. If none were created,
say so explicitly. Report partial filing accurately: a successful GitHub issue
creation still belongs here when its Org pointer could not be created.
