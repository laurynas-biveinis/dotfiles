---
description: >-
  Process one GTD inbox item through David Allen's clarify/process flowchart,
  asking clarifying questions at each branch. Calls org-mcp tools directly
  while following gtd's conventions for recording, completing, and archiving.
  The argument is the item to process. Use when the user asks to process,
  clarify, or triage a single inbox/captured item.
argument-hint: the inbox item to clarify and process
allowed-tools: >-
  Skill(gtd)
  AskUserQuestion
  Bash(gcalcli add:*)
  Bash(gcalcli list:*)
  Write
  mcp__org-mcp__org-get-allowed-files
  mcp__org-mcp__org-read-outline
  mcp__org-mcp__org-read-headline
  mcp__org-mcp__org-read-by-id
  mcp__org-mcp__org-grep
  mcp__org-mcp__org-find-tagged-ancestor
  mcp__org-mcp__org-get-tag-config
  mcp__org-mcp__org-get-agenda-config
  mcp__org-mcp__org-get-agenda
  mcp__org-mcp__org-add-todo
  mcp__org-mcp__org-update-todo-state
  mcp__org-mcp__org-set-planning
  mcp__org-mcp__org-refile-headline
  mcp__org-mcp__org-edit-body
  mcp__org-mcp__org-edit-headline
  mcp__org-mcp__org-archive-subtree
---

# Process one inbox item

Run the single item in `$ARGUMENTS` through David Allen's GTD clarify
flowchart, asking the user at any branch that isn't obvious. At every branch,
if the answer is unambiguous from context already gathered, state your reading
and ask for confirmation rather than posing an open question. If `$ARGUMENTS`
is empty, ask for the item. If a single capture bundles more than one outcome
— a rule _and_ an action, or several unrelated actions — split it and run each
through this flow independently. **Inbox-clear rule:** deliver the inbox-clear
reminder exactly once per original capture, in the first Close executed for
this capture (for a split, the first run's Close). The reminder is: remind the
user to clear this capture from their inbox. Once delivered for a capture, skip
it in every subsequent Close for that same capture.

**Resolution-summary rule:** resolving or updating an earlier in-pass
`@waitingfor` item, resolving a `WAIT` or somedaymaybe item, promoting a
somedaymaybe item, updating a still-parked `WAIT`'s dependency note,
parking (`TODO`→`WAIT`, with its `Blocked by:` note) or closing
(`DONE`/`KILL`) a pre-existing `TODO`, or retiring a spent one-time
`@checklist` trigger (`DONE` + archive) is a once-per-pass event.
Surface each such event's summary — its Org state or note change (including
any archive path) and any returned `org-id://` URI — exactly once, in the
first Close that executes at or after it, before that Close's own outcome
summary; skip it in every later Close of the same pass. Exception: a trigger
retirement the `@checklist` case already surfaced directly (its sub-step 4, in
place of a suppressed outer Close) counts as that once — skip it in every
subsequent Close of the pass likewise.

First invoke `Skill(gtd)` and follow its conventions. This skill is **only
the decision flow** — follow `gtd`'s conventions for **recording** (choose
the track per `gtd`'s _Where to track new work_; for public source-code work,
surface "create a GitHub issue" and record only an Org pointer; for Org-direct
work, write to `org-mcp` where the destination file is exposed, else surface
the exact item), **completing & archiving items**, and **resolving
`@waitingfor`**; ask one question at a time and stop once the item is filed.

<!-- Keep in sync with weekly-review's Always-confirm rule: the always-confirm
core is shared. weekly-review's copy additionally covers changing an existing
context ("or changes"); this copy only covers choosing one. -->

**Always-confirm rule:** whenever this flow — or a `gtd` procedure it
invokes — chooses a self-context (per `gtd`'s **Choosing the context**) for an
action, always state your reading and confirm it with the user before
recording, not only when unsure (a context the user already specified is
recorded as given, per `gtd`).

Because this flow executes `gtd`'s procedures itself, `allowed-tools` lists
every `org-mcp` operation this flow calls (e.g. `org-add-todo`), not only those
this flow names directly — `gtd` carries no `allowed-tools` of its own. `Write`
is also listed for saving reference content to the local filesystem (step 2,
Not actionable → Reference path).

## 1. What is it?

Restate the item and its desired outcome; if unclear, ask the user.

**Reconcile against existing items** (per `gtd`). Begin by running `org-grep`
with terms drawn from the capture (sender, subject nouns, identifiers); drive
the item-matching case checks below — each seeking a specific known item the
capture reports on — from its results. The `@checklist` case does **not** use
those results: a trigger is matched by enumeration, per `gtd`'s "Work triggers".
Do not ask the user whether the item relates to existing work as an open
question — ask only to confirm specific matches (`org-grep` covers only
`org-mcp`-exposed files, so also ask / surface for any private Org files outside
that set). Confirm the matched item with the user before any reconcile action
that changes existing Org state — resolving or updating a `@waitingfor` item or
a `WAIT`, completing, `KILL`ing, or parking a pre-existing `TODO`, or firing a
`@checklist`'s "then Y". Don't auto-apply a second-level trigger either: surface
chained triggers to the user rather than acting on them automatically. Check
these cases in order; each is terminal (routes to Close) unless its bullet says
otherwise, and the final **Otherwise** routes to step 2 or Close per its bullet.
Exception: a `@waitingfor` resolution or status update, a `WAIT` unblock
(`WAIT`→`TODO`), and any reconcile action that names new work per the
**Captured-new-work rule** below, continue through the remaining checks rather
than ending at Close — so one capture can resolve or park a delegation, clear a
dependency, or route named new work to **Otherwise** → step 2, and still match
any later terminal case whose Close then applies — a firing terminal case first
discharges any still-unrecorded named new work (per the **Captured-new-work
rule**'s **Discharge**) before it fires, so preemption never strands it.

**Archived-project addressing rule:** `org-archive-subtree` returns the
archived headline's `org-id://` `uri` and the absolute `archive_file` path.
When a reconcile case's close archives a project and processing continues,
any later reconcile case in this pass targeting that project addresses it by
that `uri` (the mutation tools accept it directly; `org-read-by-id` takes the
uuid extracted from it) — do not re-locate it via `org-grep`. If
`archive_file` is among the files returned by `org-get-allowed-files`
(compare expanded absolute paths — the allowed list may use `~/`), the `uri`
resolves — proceed normally. Otherwise the archive is not exposed to
`org-mcp`: tell the user the project was just archived (give its title and
`archive_file`), explain that the later reconcile case cannot be applied
automatically, and ask the user to handle any cascading reconcile effect
manually in the archive file.

**Unfound-item rule:** when a reconcile case's reported item has no matching
open candidate — `org-grep` found none, or none of its candidates matches
the report — ask the user to identify the item. If they place it in an
exposed file (e.g. captured under a different headline), re-locate it by the
user-supplied title and apply the case's resolution to it; if it exists only
in a file not exposed to `org-mcp`, or it still cannot be found, surface the
capture and ask the user to apply that resolution manually; if they confirm
none exists, the case does not apply.

**Somedaymaybe-membership check:** the membership variant of `gtd`'s
**Incubation check** — a match is incubated when it or any of its
`headline_path` ancestors carries the `somedaymaybe` tag (the tag inherits, and
`org-grep` reports local tags only, so the confirming tag may sit on an ancestor
node, not the item itself).

**Captured-new-work rule:** several reconcile cases, while resolving, parking,
or closing a pre-existing item, meet new work the capture itself names — a
follow-up or a blocker — that belongs on its own track:

- the **`@waitingfor` case** — a follow-up supplied alongside a delegation's
  resolution (the user's own next action or a replacement delegation, when the
  outcome is still wanted), or a new blocker that parks a still-open delegation;
- the **`WAIT` case** — a _Partial progress_ blocker that changed into new
  work, or a _Completed or abandoned_ action whose still-wanted outcome names
  its successor (an _Achieved_ incubated match excepted — that successor rides
  the re-entered run);
- the **pre-existing-`TODO` case** — a _Newly blocked_ dependency, or a
  _Completed or abandoned_ action whose still-wanted outcome names its
  successor (an _Achieved_ incubated match excepted — that successor rides the
  re-entered run).

When that named work is itself new, untracked work — the user's own next action
or a new delegation, not an external event, a date, or an already-tracked
action — it is recorded as its own new item (never the existing item retagged
or relabeled), and the case does not end at Close: it continues through the
remaining reconcile cases so the new work reaches **Otherwise** → step 2 and is
recorded via steps 2–4. A report naming no such new work — a delegation whose
outcome is dropped, a still-blocked item with nothing new — stays terminal
(Trash → Close): a reconcile case that continued such a capture past its own
Close leaves it to reach **Otherwise**, which routes a spent continuation
naming no new work to Close, not step 2.

Placement and discharge of that recorded new work follow step 3's project, if
any.

**Placement.** For a **project child** whose reconcile action left the project
open, treat that project as the one step 3 identifies — applying step 3's
**Existing-project incubation check** to it — so step 4 places the new
work under it; where that flavor runs `gtd`'s project health check, suppress its
filler-action / deliberately-parked ask — the continued processing records the
new work instead. When the reconcile close instead completed or killed and
archived the project, or the item is **standalone**, continued processing places
the new work via steps 2–4 normally (standalone, or under another project),
never under an archived project.

**Discharge.** Normally the continuation records the new work when it reaches
**Otherwise** → step 2. Two guards keep a preempted or spent pass from stranding
it, and are mutually exclusive so the work is recorded exactly once. (1) **At
preemption:** a later terminal reconcile case — a firing `@checklist` or a
somedaymaybe flavor (_Achieved_, _Activate now_, or _Abandoned_) — that would
preempt **Otherwise** first
records any still-unrecorded new work via steps 2–4
(placed per **Placement** above) before it fires. (2) **Final backstop:** if the
pass still reaches its final Close (no split or re-entered run remains) with the
work unrecorded, discharge it before that Close — for a **project child** left
with no open action recorded under that project, run `gtd`'s project health
check now (the filler/parked outcome suppressed above), letting it create the
project's next action or confirm the deliberate park; for a **standalone** item
neither recorded on any `gtd`-sanctioned track nor done during the pass, record
it via steps 2–4, which for the `@waitingfor` _Delegation dead_ flavor is that
flavor's own ask-and-capture (skipped up front only because the capture already
shows the outcome is still wanted).

**Outer-Close suppression rule:** in the `WAIT` and pre-existing-`TODO`
reconcile cases, if any confirmed report was an _Achieved_ incubated match, its
re-entered run already provides that outcome's Close (per the somedaymaybe
_Achieved_ flavor, mirroring the `@checklist` case), so emit **no** outer Close
for that capture — where the case's routing would "go to Close", end there with
no Close instead.

- **Does it resolve or update a `@waitingfor` item?** Resolve the delegation
  per `gtd`'s "Resolving a delegated item (`@waitingfor`)" — it identifies
  and confirms the item with the user, handles an incubated delegation (its
  disclosure, archive-unconditionally close, and leave-incubating in-place
  flavors), and maps the capture's report to
  `DONE`, `KILL`, or leaving the item open, possibly parked `TODO`→`WAIT`
  (`gtd`'s close still runs in full within this case, including its
  project-health check — but when the capture names a follow-up, that
  check's filler-action / deliberately-parked-ask provisioning is deferred to
  the **Captured-new-work rule** continuation, not fabricated here). When the
  capture itself supplies a follow-up — the
  user's own next action or a replacement delegation — or a new blocker that
  parks the delegation, that new work is routed and recorded by the
  **Captured-new-work rule** (never this item retagged), not created inside
  this case.

  A close that completes the item's project archives that subtree.

  If the capture reports a delegation's resolution or update but no open
  `@waitingfor` candidate matches it, `gtd`'s procedure supplies the
  identification and unfound ask — do not layer the unfound-item rule on
  top. Add only the pass-level routing that procedure cannot know: if the
  user confirms no delegation exists, this case does not apply — continue
  with the remaining reconcile cases; if the item exists only in a file not
  exposed to `org-mcp`, surface the capture and ask the user to apply the
  resolution manually.

  Then check the remaining reconcile cases below.

- **Does it resolve or update a `WAIT`?** A `WAIT` match is a parked action
  per `gtd`'s state definitions; a `@waitingfor`-tagged match belongs to the
  previous case, whatever its state. Run the **Somedaymaybe-membership check**
  on each match and disclose incubated provenance at confirmation: a _Completed
  or abandoned_ report on an incubated match is closed in place by the
  _Completed or abandoned_ flavor below (applying the somedaymaybe case's
  found-flavor), while the in-place flavors (_Dependency cleared_, _Partial
  progress_) apply here as usual and leave it incubating — disclosing that an
  unblocked or still-parked incubated item stays off the active views until
  promoted. Confirm
  the matched `WAIT` with the user, even for a single candidate; if more than
  one open `WAIT` qualifies,
  show the candidates and confirm which (one, some, or all) the capture
  resolves — several parked actions may legitimately wait on the same
  dependency. Then apply the matching flavor below to each confirmed `WAIT`,
  per what the capture reports for it:
  - _Dependency cleared, action still to do_: unblock it, `WAIT`→`TODO` via
    `org-update-todo-state`, handling the dependency note (via
    `org-edit-body`) per `gtd`'s States.
  - _Partial progress, still blocked_ — part of the dependency arrived, or it
    changed: leave the state `WAIT`, offer to update the dependency note (via
    `org-edit-body`) to what it now waits on; the note update surfaces per the
    resolution-summary rule. It counts
    as a confirmed non-cleared report in the routing below — and if the changed
    blocker is itself new work, the **Captured-new-work rule** routes it.
  - _Completed or abandoned_: for an active match, follow `gtd`'s completing
    and archiving steps in full (close state `DONE` or `KILL`, including the
    state-marking and any project-health check). For an incubated match (per
    the **Somedaymaybe-membership check**), apply the somedaymaybe case's
    matching found-flavor to this already-confirmed match, skipping that case's
    locate-and-confirm preamble — _Achieved_ for completed (surface its body
    notes, close `DONE` per the superseded-outcome rule, archive
    unconditionally, then run its outcome through the flowchart) or _Abandoned_
    for abandoned (`KILL`, archive unconditionally). If the capture names a
    still-wanted successor, the **Captured-new-work rule** routes it (for an
    _Achieved_ incubated match, the successor rides the re-entered run).

  For a reported `WAIT` with no matching open candidate, apply the
  unfound-item rule; the case's resolution is the matching flavor above. A
  re-located or manually handled report still counts, by flavor, in the
  routing below; one the user confirms nonexistent contributes no confirmed
  report.

  The inbox capture itself is `Trash`, so record nothing for it. Then route
  once for the whole capture:
  - if no report was confirmed at all (every candidate a false positive, no
    unfound `WAIT` acknowledged), this case does not apply — continue with the
    remaining reconcile cases below;
  - else if any confirmed report is a continue-trigger for this case — it took
    the _Dependency cleared_ flavor, or it named new work per the
    **Captured-new-work rule** (a _Partial progress_ blocker that changed into
    new work, or a _Completed or abandoned_ action whose still-wanted
    outcome names its successor; an _Achieved_ incubated match's successor is
    not a continue-trigger — it rides that match's re-entered run) — continue
    with the remaining reconcile cases below;
  - otherwise the capture is `Trash` — go to Close.

  Apply the **Outer-Close suppression rule**.

- **Does it complete, abandon, or block a pre-existing `TODO`?** A
  `@waitingfor`-tagged match belongs to the `@waitingfor` resolution case
  above, whatever its state. Run the **Somedaymaybe-membership check** on each
  match and disclose incubated provenance at confirmation: a _Completed or
  abandoned_ report on an incubated match is closed in place by the _Completed
  or abandoned_ flavor below (applying the somedaymaybe case's found-flavor),
  while a _Newly blocked_ report applies here as usual and leaves it incubating
  (skip the standalone `SCHEDULED` offer — an incubated item is never dated,
  per `gtd`'s Tags and structure — and disclose that the re-parked item stays
  off the active views until promoted). Confirm the matched `TODO`
  with the user, even for a single candidate; if more than one open `TODO`
  qualifies, show the
  candidates and confirm which (one, some, or all) the capture reports on —
  one report may complete several actions, and one new dependency may block
  several at once. Then apply the matching flavor below to each confirmed
  `TODO`, per what the capture reports for it:
  - _Completed or abandoned_: for an active match, follow `gtd`'s completing
    and archiving steps in full (including the state-marking and any
    project-health check). For an incubated match (per the
    **Somedaymaybe-membership check**), apply the somedaymaybe case's matching
    found-flavor to this already-confirmed match, skipping that case's
    locate-and-confirm preamble — _Achieved_ for completed (surface its body
    notes, close `DONE` per the superseded-outcome rule, archive
    unconditionally, then run its outcome through the flowchart) or _Abandoned_
    for abandoned (`KILL`, archive unconditionally). If the capture names a
    still-wanted successor, the **Captured-new-work rule** routes it (for an
    _Achieved_ incubated match, the successor rides the re-entered run).
  - _Newly blocked by a dependency_: park it, `TODO`→`WAIT` via
    `org-update-todo-state`, adding the dependency note (via `org-edit-body`)
    per `gtd`'s States; for a standalone, non-incubated item, offer a
    `SCHEDULED` via `org-set-planning` (per `gtd`'s States, the execution date
    by/at which it must be unblocked and completed); for a project child, run
    `gtd`'s project health check (when the capture's blocker is itself new
    work, that check's filler/parked-ask provisioning is likewise deferred to
    the **Captured-new-work rule** continuation).

  For a reported `TODO` with no matching open candidate, apply the
  unfound-item rule; the case's resolution is the matching flavor above. A
  re-located or manually handled report still counts, by flavor, in the
  routing below; one the user confirms nonexistent contributes no confirmed
  report.

  The inbox capture itself is `Trash`, so record nothing for it. Then route
  once for the whole capture:
  - if no report was confirmed at all (every candidate a false positive, no
    unfound `TODO` acknowledged), this case does not apply — continue with the
    remaining reconcile cases below;
  - else if any confirmed report is a continue-trigger for this case — it named
    new work per the **Captured-new-work rule** (a _Newly blocked_ report whose
    blocker is new work, or a _Completed or abandoned_ report whose
    still-wanted outcome names its successor; an _Achieved_ incubated match's
    successor is not a continue-trigger — it rides that match's re-entered
    run) — continue with the remaining reconcile cases below;
  - otherwise the capture is `Trash` — go to Close.

  Apply the **Outer-Close suppression rule**.

- **Does it match a `@checklist` trigger?** Match the capture against the
  trigger headlines by enumeration, per `gtd`'s "Work triggers" — not from step
  1's `org-grep` results; that section drops retired triggers. If more than one
  trigger matches, show all candidates to the user and confirm which (one, some,
  or all) to apply before proceeding — multiple rules may legitimately all match
  one capture. If the user applies none of the matched triggers (all matches
  were false positives), this case does not apply: fall through to the remaining
  reconcile cases in order (standing-rule → somedaymaybe → **Otherwise**),
  stopping at the first that applies — do not enter sub-steps 1–4 or emit a
  Close. Otherwise confirm with the user; this firing preempts **Otherwise**, so
  first discharge any still-unrecorded named new work this capture carries (per
  the **Captured-new-work rule**'s **Discharge**), then repeat sub-steps 1–4 for
  each selected trigger:
  1. The matched trigger input is Trash — record nothing for this matched piece.
  2. Read the confirmed trigger item's full body via its URI (per `gtd`'s
     reading guidance) to obtain its complete "then Y" clause. Apply it; run
     each resulting outcome through this full flowchart independently. Each gets
     its own Close. On re-entry, always run the re-entered item through the full
     flowchart from step 1; if the `@checklist` case matches at re-entry,
     surface the chained trigger to the user (do not auto-apply or recurse) and
     fall through to the remaining cases.
  3. In all re-entered Closes, apply the inbox-clear rule and the
     resolution-summary rule (the latter surfaces each earlier in-pass
     event exactly once).
  4. Retire the fired trigger per the **Spent** case of `gtd`'s "Work triggers"
     retirement rule — sub-step 2 applied its Y and any outcomes are recorded
     (or surfaced/done) by the re-entered runs above, so that case's precondition
     holds; it retires a spent **one-time** trigger and keeps a **recurring** one
     (the abandonment `KILL` case is not applied here — the weekly review is
     its venue). Then the outer Close: if at least one re-entered run
     produced a Close, emit no outer Close for this trigger match — but if a
     retirement happened, surface it directly here (its close + archive and
     returned `org-id://` URI), which is its once-only surfacing (later Closes of
     the pass skip it per the resolution-summary rule). If no re-entered run ran
     (zero outcomes from sub-step 2), emit a Close now, applying the
     resolution-summary rule (which surfaces any yet-unsurfaced retirement) and
     the inbox-clear rule.
- **Is it itself a standing "if X then do Y" rule?** Capture it as a
  `@checklist`-tagged item per `gtd`'s Tags and structure — the condition X in
  the headline, the action Y in the body — then go to Close.
- **Does it realize, activate, or abandon a somedaymaybe item?** Confirm with
  the user which flavor applies — _Achieved_ (the outcome already happened),
  _Activate now_ (start pursuing it), or _Abandoned_ — if not already clear
  from the capture text; then locate it by grepping its title or content terms
  via `org-grep`, confirming candidates per the **Somedaymaybe-membership
  check**. If more than one candidate passes the check, show the list and ask
  the user which one this inbox item refers to. All three flavors are terminal
  and preempt **Otherwise**, so before the action below, first discharge any
  still-unrecorded named new work this capture carries (per the
  **Captured-new-work rule**'s **Discharge**).
  - If found in an exposed file — _Abandoned_: the capture is Trash; follow
    `gtd`'s completing and archiving steps with close state `KILL` (archive
    unconditionally — do not offer); go to Close.
    _Achieved_: read the somedaymaybe item's full body per `gtd`'s reading
    guidance and surface any notes, links, or hints to the user to inform the
    re-entered processing; follow `gtd`'s completing and archiving steps with
    close state `DONE` per the superseded-outcome rule (archive unconditionally
    — do not offer); the `DONE`+archive step precedes re-entry; the re-entered
    run's Close surfaces this somedaymaybe resolution per the resolution-summary
    rule (which anchors each resolution to the first Close at or after it); run
    the somedaymaybe's outcome through this full flowchart independently (do not
    Close here for this somedaymaybe match; the re-entered run provides its own
    Close).
    _Activate now_: the item is already a fully-formed action or project —
    promote it in place per `gtd`'s **Promoting a someday/maybe item** (the
    user decides the target shape); do not close, archive, or re-create it.
    The capture is Trash; go to Close, surfacing the promotion and its
    returned `org-id://` URI.
  - If not found / unexposed: the item may be in an unexposed file or captured
    under a different headline. Surface the capture and ask the user to locate
    the somedaymaybe item. For _Abandoned_: treat the capture as Trash, inform
    the user the somedaymaybe entry may need manual archiving, and go to Close.
    For _Achieved_: ask what the somedaymaybe's outcome was, surface the
    archiving step for manual execution; run the outcome through this full
    flowchart independently (do not Close here for this somedaymaybe match; the
    re-entered run provides its own Close). For _Activate now_: surface `gtd`'s
    **Promoting a someday/maybe item** steps for manual execution, then go to
    Close.
- **Otherwise** — no case above sent the item to Close. Proceed to step 2 to
  process the item itself only if the capture is a fresh item (no reconcile
  case above applied to it) or it carries named new work not yet recorded (a
  reconcile case continued it here per the **Captured-new-work rule**). If it
  reached here solely via a spent reconcile continuation that named no new
  work — e.g. a `@waitingfor` resolution/update, or a `WAIT` _Dependency
  cleared_ unblock, that resolved the capture and left nothing to record — the
  capture is Trash: go to Close.

## 2. Is it actionable?

Ask `Actionable` vs `Not actionable`. The user's answer governs.

### Not actionable → `Trash` / `Someday/Maybe` / `Reference`

- **Trash** — confirm, then record nothing; go to Close.
- **Someday/Maybe** — record a someday/maybe item per `gtd`'s convention (Tags
  and structure), then go to Close.
- **Reference** — surface the item with a suggested destination for manual
  filing: Org, **or the local filesystem** (a reference directory or saved
  file, optionally with an Org link back). For content to capture, offer to
  save it to a file (`Write`) on confirmation; for a file already on disk,
  suggest a destination path for the user to move it. Then go to Close.

## 3. Actionable: what is the next action?

Ask for the very next visible, physical action.

### One action or many?

**Parked-entry check:** when a duplicate search below — the single-action search
or the **Existing project** path's — confirms an open match (in place or just
refiled) in `WAIT` state, its "go to Close" first passes through this check:
surface the parked status and any `Blocked by:` note (a legacy or stale park may
carry none); if the user says the dependency has cleared, offer `WAIT`→`TODO`,
handling the dependency note per `gtd`'s States; otherwise leave it parked — or,
if the user instead names a different next action, treat that action as new and
pass it to step 4. (The single-action path has no project to refile into, so
only the in-place case arises there.)

A **single action** is not a project, but first confirm it is not already
tracked — the same duplicate search the **Existing project** path runs, without
a project to refile matches into. Search for the described action via
`org-grep`; consider only open-state (`TODO`/`WAIT`) matches, applying the
**Somedaymaybe-membership check** to each and disclosing incubated provenance on
any surfaced match. Confirm any surfaced match with the user (standalone or
under any project; for a single candidate too — for several, show the list and
ask which, if any, is the same action), then act by outcome:

- **Confirmed active match** — the action is already tracked, so the capture is
  a duplicate: record nothing and go to Close (for a `WAIT` match, that go to
  Close first passes through the **Parked-entry check**).
- **Confirmed incubated match** — offer promotion per `gtd`'s **Promoting a
  someday/maybe item** (the user decides the target shape and home; the
  **Parked-entry check** covers its state step), or keep it incubating —
  disclosing that it then stays off every active view until promoted, and
  offering to instead treat the action as new via step 4, recording a body note
  (via `org-edit-body`) on the incubated match that an active counterpart now
  exists, citing the new item's returned `org-id://` URI — then go to Close.
- **Otherwise** (no open match, or the user accepts none) — carry the action to
  step 4.

If reaching the outcome needs **more than one** action, it is a **project**.
Capture the desired **outcome**, then:

- **Existing project** — find it (`org-grep` / `org-read-outline`, or ask
  which) and review its plan (`org-read-headline`) for the right next action.
  **Existing-project incubation check:** apply the **Somedaymaybe-membership
  check** to the project itself (its just-read `headline_path`); when
  incubated, disclose it and decide with the user before proceeding — activate
  the project now per `gtd`'s **Promoting a someday/maybe item**, deferring its
  project health check (Promoting step 4) filler-action / deliberately-parked
  provisioning to the continued processing that records the described action
  (per **Placement**; if the pass ends leaving the just-activated project no
  live next action, the **Discharge** final backstop runs the deferred check
  before Close), then continue, addressing it by the refile-returned
  `org-id://` URI — or knowingly record the new work incubated under it,
  disclosing it will surface only via the someday/maybe review, skipping step
  4's `SCHEDULED` offers for it (an incubated item is never dated, per
  `gtd`'s Tags and structure), and omitting step 4's projects-list
  reassurance (an incubated project is off the projects list).
  Search for the described action via `org-grep`; consider only open-state
  (`TODO`/`WAIT`) matches, applying the **Somedaymaybe-membership check** to
  each and disclosing incubated provenance on any surfaced match. The
  **Parked-entry check** (defined at the top of this section) wraps the "go to
  Close" of any bullet below that confirms a `WAIT` match, in place or just
  refiled.
  Evaluate the bullets in order; as soon as any returned result satisfies a
  bullet's condition, take its action and stop:
  - If one or more open matching entries are found inside the project subtree:
    if exactly one, confirm with the user; if more than one, show the list and
    ask which one is the already-tracked action. If the user indicates none of
    the listed entries matches the described action, continue to the next
    bullet. Once a match is confirmed, its provenance decides — for an active
    match the action is already tracked, so go to Close; for a confirmed
    incubated match (its confirming tag strictly below the project heading, or
    local to the item — at or above the heading means the target project itself
    is incubated, the **Existing-project incubation check**'s territory), first
    offer promotion per `gtd`'s **Promoting a someday/maybe item** with the
    project as its active home (out of the pocket to a direct child of the
    project; for a local-tag-only match the container refile is inapplicable and
    Promoting's step-2 tag fixup is the activation; the Parked-entry check
    covers its state step), or keep it incubating — disclosing that the project
    is then left without the described action on any active view, and offering
    to instead treat the action as new via step 4, recording a body note (via
    `org-edit-body`) on the incubated match that an active counterpart now
    exists, citing the new item's returned `org-id://` URI — then go to Close.
  - If one or more open matching entries are found outside the project subtree
    with no `project`-tagged ancestor (truly standalone): if more than one, show
    the list and ask which one is the already-tracked action before proceeding.
    Confirm with the user. If the user indicates none matches the described
    action, continue to the next bullet. Once a match is confirmed, an active
    match moves under the project with `org-refile-headline` — go to Close. For
    a confirmed incubated match, first offer promotion under the project per
    `gtd`'s **Promoting a someday/maybe item** with the project as its active
    home (the Parked-entry check covers its state step), or keep it incubating
    in place — disclosing that the project is then left without the described
    action on any active view, and offering to instead treat the action as new
    via step 4, recording a body note (via `org-edit-body`) on the incubated
    match that an active counterpart now exists, citing the new item's returned
    `org-id://` URI — then go to Close.
  - If open matching entries are found under a different project's subtree: if
    matches span more than one owning project, show all (owning project, entry)
    pairs and ask the user which to handle first (or that none is the described
    action), then process each in turn. Otherwise, if more than one entry shares
    the same owning project, show the list and ask which entry to handle (or that
    none is the described action). Once a single entry is identified, read its
    owning project from `headline_path`, surface the target project and the
    owning project of the selected match to the user — disclosing when the
    owning project is incubated, and that refiling the action out changes its
    parked plan — and ask whether to refile the action here (for an incubated
    match per `gtd`'s **Promoting a someday/maybe item**, with the target
    project as its active home; creating a new entry — or, for an incubated
    owner, a body note — for the project it was moved from if needed) or leave
    it in place (go to Close; for an active match the action is already
    tracked, while for an incubated match this means keeping it incubating — off
    every active view until promoted or the someday/maybe review — leaving the
    target project without the described action; offer to instead treat the
    action as new and pass it to step 4, recording a body note (via
    `org-edit-body`) on the incubated match that an active counterpart now
    exists, citing the new item's returned `org-id://` URI). If the user
    indicates none of the entries is the described action, treat the action as
    new and pass it to step 4.
  - **Otherwise** (no remaining open match the user accepts as the described
    action) — treat the action as new and pass it to step 4.

- **New project** — create a `project`-tagged item carrying the outcome per
  `gtd`'s recording guidance (choose the destination — Org file, GitHub issue +
  Org pointer, or surface for manual filing — as `gtd` directs), then treat its
  first next action as the input to step 4.

## 4. Will the next action take less than 2 minutes?

Ask. The human's estimate governs and may be overridden **either way** —
"actually longer" routes to delegate/defer; "actually shorter (< 2 min)"
routes to doing it now. Assess the 2-minute question against the **underlying
action as the user would perform it themselves**, not the act of asking someone
else to do it: an action only a delegate can perform, or that hands work off and
leaves the user awaiting a result, is a delegation — it always takes
**Longer → Delegate** (where the `@waitingfor` tracker is recorded), never the
"Under 2 minutes" branch, however quick the ask itself would be. The hand-off's
own 2-minute assessment happens only inside Delegate.

- **Under 2 minutes** — first confirm this is the user's own action to finish
  now, not a delegation: if completing it hands work to someone else and leaves
  the user awaiting their result (or only a delegate can perform it), treat it
  as **Longer → Delegate** so the `@waitingfor` tracker is recorded — do not
  do-it-now-and-Close. Otherwise confirm, then do it now (within your tools, or
  prompt the user to do it). The action was never recorded, so there is nothing
  to mark `DONE` or archive — **unless** step 4 was entered via a treat-as-new
  offer on a confirmed incubated match (step 3): that incubated copy is its
  recording, so decide with the user per `gtd`'s superseded-outcome rule to
  close it `DONE` (via its close case) or knowingly keep it incubating when its
  scope outlives the just-done action, rather than treating it as unrecorded. In
  this sub-path no new item or `org-id://` URI is created, so step 3's promised
  counterpart body note is not written — this incubated copy's disposition here
  is the entire record.
  Then: if standalone, go to Close; if it belongs to a project (just created,
  or the existing one from step 3), apply `gtd`'s "caller that performed an
  action without recording it" rule to that project, then go to Close —
  **except** when the treat-as-new incubated copy above was just closed `DONE`
  via a **Project child** close on that same project (an inside-the-project
  match, per step 3's first bullet): that close already ran the project's
  outcome decision / health check, so skip the rule and go straight to Close.
  (A standalone-match or different-project `DONE` close did not resolve this
  project, so the rule still applies.) If it
  cannot be done now — outside your tools, the user
  won't do it this moment, or you prompted the user but they did not confirm
  completing it — treat it as **Longer** below so the action
  is captured, not dropped (the full three-way choice applies;
  `Defer (as soon as I can)` is the typical pick, but still offer `Delegate`
  and `Defer to a specific time`).
- **Longer** — then ask `Delegate` / `Defer to a specific time` /
  `Defer (as soon as I can)`. Place any Org item recorded below under its
  project if step 3 established or identified one. Tag Org actions you record
  for yourself with a self-context per `gtd`'s _Tags and structure_, which
  covers the choice (context confirmed per this skill's **Always-confirm rule**
  above). **Blocking-status rule:** for all three branches, the recorded Org
  action's state follows its blocking status
  per `gtd`'s state definitions: `WAIT` if the action is blocked by a
  dependency (noting the dependency per `gtd`'s `WAIT` convention), `TODO` if
  it is doable as soon as its actor gets to it — infer from context already
  gathered and confirm. When an undated recorded `WAIT` leaves its project
  with no live `TODO` — always the case for a new project's first action —
  tell the user nothing actionable for this project surfaces on any
  tag-based list, though the project itself stays on the projects list for
  review: per `gtd`'s States, the parked action returns to its tag-based
  list once unblocked (while undated), or surfaces on the date-based agenda
  once dated (the offers below apply). The state does not change
  the tag: a deferred action of the user's own keeps its self-context tag
  whether `TODO` or `WAIT`; `@waitingfor` marks only delegations (the
  Delegate branch):
  - **Delegate** — delegating is two parts, both following this step's project
    placement and blocking-status rule: the **hand-off** (your own act of
    asking the delegate) and the **`@waitingfor` tracker** (what you now await
    back).
    - **Hand-off** — the act of asking is itself an action subject to the
      2-minute rule: if under 2 min, do it now (within your tools, or prompt
      the user), then continue to the **`@waitingfor` tracker** below — do not
      go to Close at the hand-off (Close comes only after the tracker is
      recorded), and do not run a project health check here: doing the ask
      inline would otherwise invoke `gtd`'s "caller that performed an action
      without recording it" rule (which ends in a health check), but this branch
      **overrides** that routing — recording the tracker, not a health check,
      settles the project's next-action state (see the tracker below). If
      longer, or it can't be done now, record it as a deferred own-action — a
      self-context `TODO` (or `WAIT` if the hand-off is itself blocked) using
      the recording mechanics of **Defer (as soon as I can)** — its recording
      only, not that bullet's own `SCHEDULED` offer, which does not fire on the
      hand-off; the delegation's resurfacing date is decided once, on the
      tracker below, per its suppression rule — or, for a date-bound hand-off,
      via **Defer to a specific time**'s **Org `SCHEDULED`/`DEADLINE`**
      sub-branch (where the date is the point). Then continue to the
      **`@waitingfor` tracker** below; do not go to Close at the hand-off. It is
      the user's own action, never re-delegated.
    - **`@waitingfor` tracker** — record the delegation per `gtd`'s Tags
      convention (`@waitingfor`), noting who/what it waits on. Its state per the
      blocking-status rule: `TODO` once the hand-off is done and the delegate is
      free to proceed (the under-2-min, unblocked case), else `WAIT`. Recording
      the tracker settles the project's next-action state for this branch — no
      separate project health check runs: a `TODO` tracker is itself the
      project's live next action, and a `WAIT` tracker is kept on a path
      back to a list — by the `SCHEDULED` offer below, or, for a project
      child, by the visible pending hand-off or other live `TODO` that keeps
      the project surfacing, per the suppression rule. Name the
      live blocker in the `Blocked by:` dependency note beside the
      `Waiting on <who> for <what>` delegation note — the pending hand-off while
      it is still a recorded, undone action, or the delegate's own dependency
      once the hand-off is done but the delegate is itself blocked — so that
      clearing the last remaining blocker flips it `WAIT`→`TODO` onto the
      waiting-for list (while undated; once dated it resurfaces on the
      date-based agenda instead, per `gtd`'s States). For a `WAIT` delegation,
      offer a `SCHEDULED` (as in `Defer (as soon as I can)` below) unless
      something already keeps it on a path back to a list. For a **standalone**
      delegation (no project), always offer it — a standalone `WAIT` tracker has
      no project health check or projects-list review to resurface it, so the
      date is its only system-level guarantee; it then rides the date-based
      agenda rather than the waiting-for list (as `Defer (as soon as I can)`
      discloses). For a **project-child** delegation, suppress the offer when a
      visible pending hand-off (any `TODO`, or a dated `WAIT`) or another live
      `TODO` under the same project keeps the project — and thus the parked
      delegation — surfacing for review; an undated `WAIT` hand-off, itself off
      every list, does not suppress.
  - **Defer to a specific time** — choose Org vs Calendar per `gtd`'s _Where
    to track new work_; the branch is usually decidable from what earlier
    steps established:
    - **Org `SCHEDULED`/`DEADLINE`** — when the action belongs to a project or
      relates to existing Org work, needs a repeater, or the date is a
      start/review date ("from this day onward") rather than a fixed-day
      commitment. Create the action with an appropriate context tag and state
      per the blocking-status rule above (a dated item can still be blocked),
      then set the timestamp with `org-set-planning` (`DEADLINE` for a hard
      due date, `SCHEDULED` for a start date; repeaters like `+1w` are fine).
      On a `WAIT`-state item any timestamp carries `gtd`'s States meaning —
      `SCHEDULED` is the execution date, never a start/review date, which is
      expressible only on a `TODO` item; when start/review intent meets a
      blocked action, confirm with the user — upgrade the date to that
      execution-date commitment, or leave the item undated and fold the
      review date into the dependency note.
    - **Google Calendar** — when the action is a day/time-specific commitment
      (appointment, delivery, event — it happens on that date regardless of
      any list) and steps 1/3 tied it to no Org work or project, including a
      project created in step 3. For a timed deferral, run `gcalcli add` with
      `--calendar`, `--title`, `--when`, `--duration`, and `--noprompt`; for a
      date-only deferral, additionally pass `--allday`, and `--duration` is then
      counted in days (e.g. `--allday --duration 1` for a single day). Ask for
      the calendar, date/time, and duration; if the user
      doesn't know the calendar name, run `gcalcli list` to enumerate the options
      and let them pick. Confirm, and run it. If `gcalcli` is unavailable or not
      authenticated, surface the full command for the user to run. Tell the user
      the event will appear in the Org agenda after the next `org-gcal` sync, if
      that calendar is among those `org-gcal` syncs.
  - **Defer (as soon as I can)** — record the next action with an appropriate
    context tag, state per the blocking-status rule above. For a `WAIT` that
    is standalone or leaves its project with no live `TODO`, offer a
    `SCHEDULED` via `org-set-planning` — per `gtd`'s States, the execution
    date by/at which it must be unblocked and completed; the parked item
    sits on no tag-based list, and the date resurfaces it on the date-based
    agenda — disclosing that the date keeps the item off every tag-based
    list, even after `WAIT`→`TODO`, for as long as it stands.
  - **Revised to < 2 min after recording** — if after recording above the user
    says "< 2 min":
    - _Recorded Org item (`TODO` or `WAIT`)_: if "do it now" succeeds (within
      your tools and the user confirms completion), first decide each recorded
      item's disposition — a **non-hand-off** own action closes `DONE`; a
      recorded **hand-off** own-action closes per the same superseded-outcome
      rule: `DONE` if it was carried out, else `KILL` as mooted; a
      Delegate-branch **`@waitingfor` tracker** is **not** blanket-closed but
      disposed of via `gtd`'s "Resolving a delegated item" mapping against what
      the on-the-spot action achieved — `DONE` if the underlying task got done
      (the user did it themselves, or the delegate delivered on the spot), else
      leave it **open** and flip `WAIT`→`TODO` (the hand-off
      cleared, the delegate still awaited). Then apply those dispositions via
      `gtd`'s "Completing and archiving items": when more than one item was
      recorded under the **same project**, mark all of them to their decided
      states first and run `gtd`'s project-outcome/health-check/archive
      resolution **once** for the shared project (via its "caller that performed
      an action without recording it" entry, which resolves the project
      independently of per-item close) — do not loop the full per-item project
      close over each, and do not treat "close the tracker last" as equivalent
      (closing any first item still resolves the project while a sibling is
      open). For a **treat-as-new** entry (step 3), also settle the incubated
      match once the counterpart(s) are disposed: a counterpart that ended
      closed (`DONE`/`KILL`) means the action was completed or mooted, so
      surface the incubated match's superseded-outcome decision as under
      **Under 2 minutes** above — keep it incubating when its scope outlives the
      done action, or close it `DONE`; when a closed incubated match shares a
      project with a recorded counterpart, mark both to their decided states and
      resolve that shared project **once** (per the multi-item rule above), not
      with a second per-item close — and write **no** counterpart body note for
      that now-closed counterpart. A counterpart left open (a `@waitingfor`
      tracker flipped `WAIT`→`TODO`) keeps the incubated match incubating and
      takes step 3's counterpart body note (via `org-edit-body`) citing that
      still-open tracker. Then go to Close. If it cannot be done now, leave the
      recorded item(s) unchanged — a treat-as-new entry's still-open counterpart
      then takes step 3's counterpart body note (via `org-edit-body`) citing
      it — and go to Close.
    - _Recorded Google Calendar event_: first do the action now. If it cannot
      be done now, leave the event in place and go to Close. Only on confirmed
      completion, surface the full command for the user to run (gcalcli always
      prompts for confirmation before deleting), using the exact calendar name,
      title, and times from creation:
      `gcalcli delete --calendar <calendar> "<title>" "<start>" "<end>"`

  After any of the above, go to Close. When step 4 was entered via a
  treat-as-new offer on a confirmed incubated match (step 3) and a new item was
  recorded above **and left open** (the Delegate and Defer cases — the
  **Revised to < 2 min** sub-branch settles its own note and the incubated
  match's disposition above), first write step 3's promised counterpart body
  note (via `org-edit-body`) on the incubated match — that an active
  counterpart now exists — citing the recorded action's returned `org-id://`
  URI (for the Delegate branch, the `@waitingfor` tracker's URI), then go to
  Close.

## Close

Apply the resolution-summary rule (its output precedes the outcome summary),
then summarize what was written, surfaced, or run (report any returned
`org-id://` URI), then apply the inbox-clear rule.
