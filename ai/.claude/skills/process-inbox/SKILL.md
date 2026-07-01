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
flowchart, asking the user at any branch that isn't obvious. If `$ARGUMENTS`
is empty, ask for the item. If a single capture bundles more than one outcome
— a rule _and_ an action, or several unrelated actions — split it and run each
through this flow independently. **Inbox-clear rule:** deliver the inbox-clear
reminder exactly once per original capture, in the first Close executed for
this capture (for a split, the first run's Close). The reminder is: remind the
user to clear this capture from their inbox. Once delivered for a capture, skip
it in every subsequent Close for that same capture.

**Resolution-summary rule:** resolving an earlier in-pass `@waitingfor` or
somedaymaybe item is a one-time, pass-level event. Surface each such
resolution's summary — its Org state change (including any archive path) and any
returned `org-id://` URI — exactly once, in the first Close that executes at or
after that resolution, before that Close's own outcome summary; skip it in every
later Close of the same pass.

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

**Reconcile against existing items** (per `gtd`; `org-grep` covers only
`org-mcp`-exposed files, so also ask / surface for any private Org files
outside that set). Confirm the matched item with the user before any
reconcile action that changes existing Org state — resolving a `@waitingfor`,
completing or `KILL`ing a pre-existing `TODO`, or firing a `@checklist`'s
"then Y".
Don't auto-apply a second-level trigger either: surface chained triggers to the
user rather than acting on them automatically. Check these cases in order; each
is terminal (routes to Close) unless its bullet says otherwise, and the final
**Otherwise** routes to step 2. Exception: `@waitingfor` continues through the
remaining checks, so the same item can close a `@waitingfor` and then match a
later terminal case whose Close then applies.

- **Does it close a `@waitingfor`?** Confirm with the user, then, if the item
  is the awaited input/response to a delegated `WAIT` item, resolve that `WAIT`
  item per `gtd`: `WAIT`→`TODO` if the input unblocks further action still
  needed, `WAIT`→`DONE` if the delegated action is now fully resolved. A
  `WAIT`→`DONE` that completes the item's project archives that subtree, so any
  later reconcile case targeting the same project must re-locate it via
  `org-grep`:
  - If the archive file is among those returned by `org-get-allowed-files`,
    `org-grep` finds it — proceed normally.
  - Otherwise the archive is not exposed to `org-mcp`: tell the user the project
    was just archived (give its title, and the archive file path if
    `org-archive-subtree` returned one), explain that the later reconcile case
    cannot be applied automatically, and ask the user to handle any cascading
    reconcile effect manually in the archive file.

  Then check the remaining reconcile cases below.

- **Does it complete or abandon a pre-existing `TODO`?** Confirm with the
  user, then find it (`org-grep`) and follow `gtd`'s completing and archiving
  steps in full (including the state-marking and any project-health check); the
  inbox capture itself is `Trash`, so record nothing and go to Close. If
  `org-grep` returns no match, the item may be in an unexposed file or captured
  under a different headline — surface the capture and ask the user to locate
  and close the `TODO` manually, then go to Close.
- **Does it match a `@checklist` trigger?** Disregard any `DONE`/`KILL` results
  from `org-grep`. If more than one open match remains, show all
  candidates to the user and confirm which (one, some, or all) to apply before
  proceeding — multiple rules may legitimately all match one capture. If the
  user applies none of the matched triggers (all matches were false positives),
  this case does not apply: fall through to the remaining reconcile cases in
  order (standing-rule → somedaymaybe → **Otherwise**), stopping at the first
  that applies — do not enter sub-steps 1–4 or emit a Close. Otherwise confirm
  with the user, then repeat sub-steps 1–4 for each selected trigger:
  1. The matched trigger input is Trash — record nothing for this matched piece.
  2. Read the confirmed trigger item's full body via its URI (per `gtd`'s
     post-`org-grep` reading guidance) to obtain its complete "then Y" clause.
     Apply it; run each resulting outcome through this full flowchart
     independently. Each gets its own Close. On re-entry, always run the
     re-entered item through the full flowchart from step 1; if the `@checklist`
     case matches at re-entry, surface the chained trigger to the user (do not
     auto-apply or recurse) and fall through to the remaining cases.
  3. In all re-entered Closes, apply the inbox-clear rule and the
     resolution-summary rule (the latter surfaces any earlier in-pass
     `@waitingfor` resolution exactly once, in the first Close at or after that
     resolution).
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
- **Otherwise** — no case above sent the item to Close, so proceed to step 2
  to process the item itself.

## 2. Is it actionable?

Ask `Actionable` vs `Not actionable`. The user's answer governs; if it's
unambiguous from context, state your reading and ask for confirmation rather
than posing an open question.

### Not actionable → `Trash` / `Someday/Maybe` / `Reference`

- **Trash** — confirm, then record nothing; go to Close.
- **Someday/Maybe** — record a someday/maybe item per `gtd`'s convention (Tags
  and structure), offer to set a review date
  via `org-set-planning` (`SCHEDULED` — keeps the item off
  the agenda until the review date and surfaces it as overdue from that date
  onward, serving as a deliberate review nag), then go to Close.
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
  (`TODO`/`WAIT`) matches. Evaluate the bullets in order; as soon as any
  returned result satisfies a bullet's condition, take its action and stop:
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
  user unless they already specified one:
  - **Delegate** — record a `WAIT` action using the appropriate delegated
    context tag per `gtd`, noting who/what it waits on.
  - **Defer to a specific time** — ask Org vs Calendar:
    - **Org `SCHEDULED`/`DEADLINE`** — create the action with an appropriate
      context tag, then set the timestamp with `org-set-planning` (`DEADLINE`
      for a hard due date, `SCHEDULED` for a start date; repeaters like `+1w`
      are fine).
    - **Google Calendar** — for a timed deferral, run `gcalcli add` with
      `--calendar`, `--title`, `--when`, `--duration`, and `--noprompt`; for a
      date-only deferral, additionally pass `--allday`, and `--duration` is then
      counted in days (e.g. `--allday --duration 1` for a single day). Ask for
      the calendar, date/time, and duration; if the user
      doesn't know the calendar name, run `gcalcli list` to enumerate the options
      and let them pick. Confirm, and run it. If `gcalcli` is unavailable or not
      authenticated, surface the full command for the user to run. Tell the user
      the event will appear in the Org agenda after the next `org-gcal` sync, if
      that calendar is among those `org-gcal` syncs. If step 3 established or
      identified a project, also record a `TODO` next action under that project
      (with an appropriate context tag per `gtd`, noting the calendar event in
      its body) so the project retains a visible next action until the event fires.
  - **Defer (as soon as I can)** — record a `TODO` next action with an
    appropriate context tag.
  - **Revised to < 2 min after recording** — if after recording above the user
    says "< 2 min":
    - _Recorded Org item (`WAIT` or `TODO`)_: if "do it now" succeeds (within
      your tools and the user confirms completion), close the recorded item via
      `gtd`'s "Completing and archiving items" (for a `WAIT`, use `KILL` — the
      delegation was superseded, not fulfilled; for a `TODO`, use `DONE`), then
      go to Close. If it cannot be done now, leave the existing `WAIT`/`TODO`
      unchanged and go to Close.
    - _Recorded Google Calendar event_: first do the action now. If it cannot
      be done now, leave the event in place and go to Close. Only on confirmed
      completion, surface the full command for the user to run (gcalcli always
      prompts for confirmation before deleting), using the exact calendar name,
      title, and times from creation:
      `gcalcli delete --calendar <calendar> "<title>" "<start>" "<end>"`
      If step 3 established or identified a project, close the recorded TODO
      via `gtd`'s "Completing and archiving items" (state `DONE`), then go to
      Close.

  After any of the above, go to Close.

## Close

Apply the resolution-summary rule (its output precedes the outcome summary),
then summarize what was written, surfaced, or run (report any returned
`org-id://` URI), then apply the inbox-clear rule.
