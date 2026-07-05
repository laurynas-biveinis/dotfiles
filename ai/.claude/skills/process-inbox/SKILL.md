---
model: opus
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
  mcp__org-mcp__org-get-tag-config
  mcp__org-mcp__org-add-todo
  mcp__org-mcp__org-update-todo-state
  mcp__org-mcp__org-set-planning
  mcp__org-mcp__org-refile-headline
  mcp__org-mcp__org-edit-body
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
`@waitingfor` item, resolving a `WAIT` or somedaymaybe item, updating a
still-parked `WAIT`'s dependency note, or parking (`TODO`→`WAIT`, with its
`Blocked by:` note) or closing (`DONE`/`KILL`) a pre-existing `TODO` is a
one-time, pass-level event.
Surface each such event's summary — its Org state or note change (including
any archive path) and any returned `org-id://` URI — exactly once, in the
first Close that executes at or after it, before that Close's own outcome
summary; skip it in every later Close of the same pass.

First invoke `Skill(gtd)` and follow its conventions. This skill is **only
the decision flow** — follow `gtd`'s conventions for **recording** (choose
the track per `gtd`'s _Where to track new work_; for public source-code work,
surface "create a GitHub issue" and record only an Org pointer; for Org-direct
work, write to `org-mcp` where the destination file is exposed, else surface
the exact item), **completing & archiving items**, and **resolving
`@waitingfor`**; ask one question at a time and stop once the item is filed.
`allowed-tools` therefore lists every
`org-mcp` operation this flow calls directly (e.g. `org-add-todo`),
not only those this flow names directly — `gtd` carries no `allowed-tools` of
its own. `Write` is also listed for saving reference content to the local
filesystem (step 2, Not actionable → Reference path).

## 1. What is it?

Restate the item and its desired outcome; if unclear, ask the user.

**Reconcile against existing items** (per `gtd`). Begin by running `org-grep`
with terms drawn from the capture (sender, subject nouns, identifiers); drive
the case checks below from its results. Do not ask the user whether the item
relates to existing work as an open question — ask only to confirm specific
matches (`org-grep` covers only `org-mcp`-exposed files, so also ask / surface
for any private Org files outside that set). Confirm the matched item with the
user before any reconcile action that changes existing Org state — resolving
or updating a `@waitingfor` item or a `WAIT`, completing, `KILL`ing, or
parking a pre-existing `TODO`, or firing a `@checklist`'s "then Y". Don't
auto-apply a second-level trigger either: surface chained triggers to the
user rather than acting on them automatically. Check these cases in order;
each is terminal (routes to Close) unless its bullet says otherwise, and the
final **Otherwise** routes to step 2 or Close per its bullet. Exception: a
`@waitingfor` resolution or status update, a `WAIT` unblock (`WAIT`→`TODO`),
and any reconcile action that names new work per the
**Captured-new-work rule** below, continue through the
remaining checks rather than ending at Close — so one capture can resolve or
park a delegation, clear a dependency, or route named new work to
**Otherwise** → step 2, and still match any later terminal case whose Close
then applies — a firing terminal case first discharges any still-unrecorded
named new work (per the **Captured-new-work rule**'s **Discharge**) before it
fires, so preemption never strands it.

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

**Captured-new-work rule:** several reconcile cases, while resolving, parking,
or closing a pre-existing item, meet new work the capture itself names — a
follow-up or a blocker — that belongs on its own track:

- the **`@waitingfor` case** — a follow-up supplied alongside a delegation's
  resolution (the user's own next action or a replacement delegation, when the
  outcome is still wanted), or a new blocker that parks a still-open delegation;
- the **`WAIT` case** — a _Partial progress_ blocker that changed into new
  work, or a _Completed or abandoned_ action whose still-wanted outcome names
  its successor;
- the **pre-existing-`TODO` case** — a _Newly blocked_ dependency, or a
  _Completed or abandoned_ action whose still-wanted outcome names its
  successor.

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
open, treat that project as the one step 3 identifies, so step 4 places the new
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
somedaymaybe _Realized_/_Abandoned_ — that would preempt **Otherwise** first
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

- **Does it resolve or update a `@waitingfor` item?** Resolve the delegation
  per `gtd`'s "Resolving a delegated item (`@waitingfor`)" — it identifies
  and confirms the item with the user and maps the capture's report to
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
  previous case, whatever its state. Confirm the matched `WAIT` with the
  user, even for a single candidate; if more than one open `WAIT` qualifies,
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
  - _Completed or abandoned_: follow `gtd`'s completing and archiving steps in
    full (close state `DONE` or `KILL`, including the state-marking and any
    project-health check). If the capture names a still-wanted successor, the
    **Captured-new-work rule** routes it.

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
    outcome names its successor) — continue with the remaining reconcile cases
    below;
  - otherwise the capture is `Trash` — go to Close.

- **Does it complete, abandon, or block a pre-existing `TODO`?** A
  `@waitingfor`-tagged match belongs to the `@waitingfor` resolution case
  above, whatever its state. Confirm the matched `TODO` with the user, even
  for a single candidate; if more than one open `TODO` qualifies, show the
  candidates and confirm which (one, some, or all) the capture reports on —
  one report may complete several actions, and one new dependency may block
  several at once. Then apply the matching flavor below to each confirmed
  `TODO`, per what the capture reports for it:
  - _Completed or abandoned_: follow `gtd`'s completing and archiving steps
    in full (including the state-marking and any project-health check). If the
    capture names a still-wanted successor, the **Captured-new-work rule**
    routes it.
  - _Newly blocked by a dependency_: park it, `TODO`→`WAIT` via
    `org-update-todo-state`, adding the dependency note (via `org-edit-body`)
    per `gtd`'s States; for a standalone item, offer a `SCHEDULED` via
    `org-set-planning` (per `gtd`'s States, the execution date by/at which it
    must be unblocked and completed); for a project child, run `gtd`'s
    project health check (when the capture's blocker is itself new work, that
    check's filler/parked-ask provisioning is likewise deferred to the
    **Captured-new-work rule** continuation).

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
    still-wanted outcome names its successor) — continue with the remaining
    reconcile cases below;
  - otherwise the capture is `Trash` — go to Close.

- **Does it match a `@checklist` trigger?** Disregard any `DONE`/`KILL` results
  from `org-grep`. If more than one open match remains, show all
  candidates to the user and confirm which (one, some, or all) to apply before
  proceeding — multiple rules may legitimately all match one capture. If the
  user applies none of the matched triggers (all matches were false positives),
  this case does not apply: fall through to the remaining reconcile cases in
  order (standing-rule → somedaymaybe → **Otherwise**), stopping at the first
  that applies — do not enter sub-steps 1–4 or emit a Close. Otherwise confirm
  with the user; this firing preempts **Otherwise**, so first discharge any
  still-unrecorded named new work this capture carries (per the
  **Captured-new-work rule**'s **Discharge**), then repeat sub-steps 1–4 for
  each selected trigger:
  1. The matched trigger input is Trash — record nothing for this matched piece.
  2. Read the confirmed trigger item's full body via its URI (per `gtd`'s
     post-`org-grep` reading guidance) to obtain its complete "then Y" clause.
     Apply it; run each resulting outcome through this full flowchart
     independently. Each gets its own Close. On re-entry, always run the
     re-entered item through the full flowchart from step 1; if the `@checklist`
     case matches at re-entry, surface the chained trigger to the user (do not
     auto-apply or recurse) and fall through to the remaining cases.
  3. In all re-entered Closes, apply the inbox-clear rule and the
     resolution-summary rule (the latter surfaces each earlier in-pass
     event exactly once, in the first Close at or after it).
  4. If at least one re-entered run produced a Close, emit no outer Close for
     this trigger match. If no re-entered run ran (zero outcomes from
     sub-step 2), emit a Close now, applying the resolution-summary rule and the
     inbox-clear rule.
- **Is it itself a standing "if X then do Y" rule?** Capture it as a
  `@checklist`-tagged item, then go to Close.
- **Does it realize or abandon a somedaymaybe item?** Confirm with the user —
  including whether this is an Abandoned or Realized case if not already clear
  from the capture text — then locate it by grepping its title or content terms
  via `org-grep`; confirm `somedaymaybe` membership by checking `tags` on each
  `headline_path` node (item and all ancestors — the tag inherits, and
  `org-grep` reports local tags only, so the confirming tag may appear on an
  ancestor, not the item itself). If more than one candidate passes the tag
  check, show the list and ask the user which one this inbox item refers to.
  Realize/abandon is terminal and preempts **Otherwise**, so before the action
  below, first discharge any still-unrecorded named new work this capture
  carries (per the **Captured-new-work rule**'s **Discharge**).
  - If found in an exposed file — _Abandoned_: the capture is Trash; follow
    `gtd`'s completing and archiving steps with close state `KILL` (archive
    unconditionally — do not offer); go to Close.
    _Realized_: read the somedaymaybe item's full body per `gtd`'s post-`org-grep`
    reading guidance and surface any notes, links, or hints to the user to
    inform the re-entered processing; follow `gtd`'s completing and archiving
    steps (archive unconditionally — do not offer); the `DONE`+archive step
    precedes re-entry; the re-entered run's Close surfaces this somedaymaybe
    resolution per the resolution-summary rule (which anchors each resolution to
    the first Close at or after it); run the somedaymaybe's outcome through this
    full flowchart independently (do not Close here for this somedaymaybe match;
    the re-entered run provides its own Close).
  - If not found / unexposed: the item may be in an unexposed file or captured
    under a different headline. Surface the capture and ask the user to locate
    the somedaymaybe item. For _Abandoned_: treat the capture as Trash, inform
    the user the somedaymaybe entry may need manual archiving, and go to Close.
    For _Realized_: ask what the somedaymaybe's outcome was, surface the
    archiving step for manual execution; run the outcome through this full
    flowchart independently (do not Close here for this somedaymaybe match; the
    re-entered run provides its own Close).
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

A **single action** is not a project — carry it directly to step 4.

If reaching the outcome needs **more than one** action, it is a **project**.
Capture the desired **outcome**, then:

- **Existing project** — find it (`org-grep` / `org-read-outline`, or ask
  which) and review its plan (`org-read-headline`) for the right next action.
  Search for the described action via `org-grep`; consider only open-state
  (`TODO`/`WAIT`) matches. **Parked-entry check:** when a bullet below
  identifies the entry — confirmed in place or just refiled — and it is in
  `WAIT` state, its "go to Close" first passes through this check: surface
  the parked status and any `Blocked by:` note (a legacy or stale park may
  carry none); if the user says the dependency
  has cleared, offer `WAIT`→`TODO`, handling the dependency note per `gtd`'s
  States; otherwise leave it parked — or, if the user instead names a
  different next action, treat that action as new and pass it to step 4.
  Evaluate the bullets in order; as soon as any returned result satisfies a
  bullet's condition, take its action and stop:
  - If one or more open matching entries are found inside the project subtree:
    if exactly one, confirm with the user; if confirmed, the action is already
    tracked — go to Close; otherwise, continue to the next bullet. If more than
    one, show the list and ask which one is the already-tracked action; once
    identified, confirm and go to Close. If the user indicates none of the listed
    entries matches the described action, continue to the next bullet.
  - If one or more open matching entries are found outside the project subtree
    with no `project`-tagged ancestor (truly standalone): if more than one, show
    the list and ask which one to refile before proceeding. Confirm with the
    user; if confirmed, move it under the project with `org-refile-headline` and
    go to Close; otherwise, continue to the next bullet.
  - If open matching entries are found under a different project's subtree: if
    matches span more than one owning project, show all (owning project, entry)
    pairs and ask the user which to handle first (or that none is the described
    action), then process each in turn. Otherwise, if more than one entry shares
    the same owning project, show the list and ask which entry to handle (or that
    none is the described action). Once a single entry is identified, read its
    owning project from `headline_path`, surface the target project and the
    owning project of the selected match to the user, and ask whether to refile
    the action here (creating a new entry for the project it was moved from if
    needed) or leave it in place (go to Close; the action is already tracked). If
    the user indicates none of the entries is the described action, treat the
    action as new and pass it to step 4.
  - **Otherwise** (no remaining open match the user accepts as the described
    action) — treat the action as new and pass it to step 4.

- **New project** — create a `project`-tagged item carrying the outcome per
  `gtd`'s recording guidance (choose the destination — Org file, GitHub issue +
  Org pointer, or surface for manual filing — as `gtd` directs), then treat its
  first next action as the input to step 4.

## 4. Will the next action take less than 2 minutes?

Ask. The human's estimate governs and may be overridden **either way** —
"actually longer" routes to delegate/defer; "actually shorter (< 2 min)"
routes to doing it now.

- **Under 2 minutes** — confirm, then do it now (within your tools, or prompt
  the user to do it). The action was never recorded, so there is nothing to
  mark `DONE` or archive: if standalone, go to Close; if it belongs to a
  project (just created, or the existing one from step 3), apply `gtd`'s
  "caller that performed an action without recording it" rule to that project,
  then go to Close. If it cannot be done now — outside your tools, the user
  won't do it this moment, or you prompted the user but they did not confirm
  completing it — treat it as **Longer** below so the action
  is captured, not dropped (the full three-way choice applies;
  `Defer (as soon as I can)` is the typical pick, but still offer `Delegate`
  and `Defer to a specific time`).
- **Longer** — then ask `Delegate` / `Defer to a specific time` /
  `Defer (as soon as I can)`. Place any Org item recorded below under its
  project if step 3 established or identified one. Tag Org actions you record
  for yourself with a self-context per `gtd`'s _Tags and structure_ (discovered
  at runtime, not `@internet` by default); confirm the chosen context with the
  user unless they already specified one. **Blocking-status rule:** for all
  three branches, the recorded Org action's state follows its blocking status
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
  - **Delegate** — record the delegation per `gtd`'s Tags convention
    (`@waitingfor`, state per the blocking-status rule above), noting
    who/what it waits on. For a `WAIT` delegation that is standalone or
    leaves its project with no live `TODO`, offer a `SCHEDULED` as in
    `Defer (as soon as I can)` below.
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
      your tools and the user confirms completion), close the recorded item via
      `gtd`'s "Completing and archiving items", choosing the close state per
      its superseded-outcome rule (the user's own action closes `DONE`; a
      Delegate-branch delegation closes `KILL` when the user did the action
      themselves, `DONE` if the delegate delivered on the spot), then go to
      Close. If it cannot be done now, leave the recorded item unchanged and
      go to Close.
    - _Recorded Google Calendar event_: first do the action now. If it cannot
      be done now, leave the event in place and go to Close. Only on confirmed
      completion, surface the full command for the user to run (gcalcli always
      prompts for confirmation before deleting), using the exact calendar name,
      title, and times from creation:
      `gcalcli delete --calendar <calendar> "<title>" "<start>" "<end>"`

  After any of the above, go to Close.

## Close

Apply the resolution-summary rule (its output precedes the outcome summary),
then summarize what was written, surfaced, or run (report any returned
`org-id://` URI), then apply the inbox-clear rule.
