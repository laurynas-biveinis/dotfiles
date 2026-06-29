---
description: >-
  Process one GTD inbox item through David Allen's clarify/process flowchart,
  asking clarifying questions at each branch. Delegates recording, completion,
  and reconciliation mechanics to the gtd skill. The argument is the item to
  process. Use when the user asks to process, clarify, or triage a single
  inbox/captured item.
argument-hint: the inbox item to clarify and process
allowed-tools: >-
  Skill(gtd)
  AskUserQuestion
  Bash(gcalcli:*)
  Write
  mcp__org-mcp__org-get-allowed-files
  mcp__org-mcp__org-read-outline
  mcp__org-mcp__org-read-headline
  mcp__org-mcp__org-grep
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
through this flow; deliver the inbox-clear reminder (in Close) only on the
first split run's Close, and omit it from the remaining runs' Closes.

First invoke `Skill(gtd)` and follow its conventions. This skill is **only
the decision flow** — delegate every mechanic to `gtd`: **recording** (choose
the track per `gtd`'s _Where to track new work_; for public source-code work,
surface "create a GitHub issue" and record only an Org pointer; for Org-direct
work, write to `org-mcp` where the destination file is exposed, else surface
the exact item), **completing & archiving items**, **resolving `@waitingfor`**,
**work triggers**; ask one question at a time and stop once the item is filed.
`allowed-tools` therefore lists every
`org-mcp` operation this flow drives `gtd` to perform (e.g. `org-edit-body`),
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
user rather than acting on them automatically. Check these cases in order.
Every case is terminal (ends at Close) except three: `@waitingfor` (resolves
the wait, then continues through the remaining checks, so the same item can
close a `@waitingfor` and then match a later terminal case whose Close then
applies), somedaymaybe-Realized (leaves the reconcile scan and proceeds to
step 2), and `@checklist` (outcomes each get their own Close via independent
re-entry; the trigger match itself emits no outer Close). The final
**Otherwise** bullet routes the item to step 2 when no case has yet done so.

- **Does it close a `@waitingfor`?** Confirm with the user, then, if the item
  is the awaited input/response to a delegated `WAIT` item, resolve that
  `WAIT` item per `gtd` (`WAIT`→`TODO` if the input unblocks further action
  still needed, `WAIT`→`DONE` if the delegated action is now fully resolved).
  A `WAIT`→`DONE` that completes the item's project archives that subtree, so
  any later reconcile case targeting the same project must re-locate it via
  `org-grep` (which finds it only if the archive file is among
  `org-mcp-allowed-files`; otherwise surface/ask). Then check the remaining
  reconcile cases below.
- **Does it complete or abandon a pre-existing `TODO`?** Confirm with the
  user, then find it (`org-grep`) and follow `gtd`'s completing and archiving
  steps in full (including the state-marking and any project-health check); the
  inbox capture itself is `Trash`, so record nothing and go to Close.
- **Does it match a `@checklist` trigger?** Confirm with the user, then:
  1. The original capture is Trash — record nothing for it; before the first
     re-entered run, note this once: inform the user the capture is Trash and
     remind them to clear it from their inbox.
  2. Apply the matching "then Y"; run each resulting outcome through this full
     flowchart independently. Each gets its own Close. On re-entry, any further
     `@checklist` match must be surfaced to the user, not applied automatically.
  3. In all re-entered Closes, omit the original-capture-as-Trash note and the
     inbox-clear reminder (already delivered in sub-step 1 above). If a
     `@waitingfor` was resolved earlier in the same pass, include a summary of
     that resolution (Org state change, any returned `org-id://` URI) in the
     first re-entered Close, before that run's triggered-outcome summary.
  4. Emit no outer Close for this trigger match.
- **Is it itself a standing "if X then do Y" rule?** Capture it as a
  `@checklist`-tagged item, then go to Close.
- **Does it realize or abandon a somedaymaybe item?** Confirm with the user,
  then locate it by grepping its title or content terms via `org-grep`; confirm
  `somedaymaybe` membership by checking `tags` on each `headline_path` node
  (item and all ancestors — the tag inherits, and `org-grep` reports local tags
  only, so the confirming tag may appear on an ancestor, not the item itself).
  If more than one candidate passes the tag check, show the list and ask the
  user which one this inbox item refers to before archiving. Then archive it via
  `org-archive-subtree`.
  - _Abandoned_: the capture is Trash; go to Close.
  - _Realized_: treat the somedaymaybe's outcome as the item to process;
    continue to step 2 (do not Close here).
- **Otherwise** — no case above sent the item to Close, so proceed to step 2
  to process the item itself.

## 2. Is it actionable?

Ask `Actionable` vs `Not actionable`. The user's answer governs; if it's
unambiguous from context, state your reading and ask for confirmation rather
than posing an open question.

### Not actionable → `Trash` / `Someday/Maybe` / `Reference`

- **Trash** — confirm, then record nothing; go to Close.
- **Someday/Maybe** — record a `somedaymaybe`-tagged item, offer
  to set a review date via `org-set-planning` (`SCHEDULED`), then go to Close.
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
  Search for the described action via `org-grep`; if a matching standalone
  entry is found (outside the project subtree), confirm with the user, then
  move it under the project with `org-refile-headline` and go to Close.
  Otherwise treat the action as new and pass it to step 4.
- **New project** — create a `project`-tagged headline carrying the outcome,
  then treat its first next action as the input to step 4.

## 4. Will the next action take less than 2 minutes?

Ask. The human's estimate governs and may be overridden **either way** —
"actually longer" routes to delegate/defer; "actually shorter (< 2 min)"
routes to doing it now.

- **Revised to < 2 min after recording** — if the user says "< 2 min" after
  a `WAIT`/`TODO` or Google Calendar event has already been written:
  - _Recorded Org item (`WAIT` or `TODO`)_: if "do it now" succeeds (within
    your tools and the user confirms completion), close the recorded item via
    `gtd`'s "Completing and archiving items" (for a `WAIT`, use `KILL` — the
    delegation was superseded, not fulfilled; for a `TODO`, use `DONE`), then
    go to Close. If it cannot be done now, leave the existing `WAIT`/`TODO`
    unchanged and go to Close.
  - _Recorded Google Calendar event_: first do the action now. If it cannot be
    done now, leave the event in place and go to Close. Only on confirmed
    completion, surface `gcalcli delete --calendar <calendar used at creation>
"<title>" "<start>" "<end>"` (exact title and time from creation) for the
    user to confirm interactively (`gcalcli delete` is always interactive); if
    step 3 established or identified a project, apply `gtd`'s "caller that
    performed an action without recording it" rule to it; then go to Close.

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
  project if step 3 established or identified one. Org actions you record for
  yourself get the appropriate context tag per `gtd` (unless told otherwise):
  - **Delegate** — record a `WAIT` action using the appropriate delegated
    context tag per `gtd`, noting who/what it waits on.
  - **Defer to a specific time** — ask Org vs Calendar:
    - **Org `SCHEDULED`/`DEADLINE`** — create the action with an appropriate
      context tag, then set the timestamp with `org-set-planning` (`DEADLINE`
      for a hard due date, `SCHEDULED` for a start date; repeaters like `+1w`
      are fine).
    - **Google Calendar** — run `gcalcli add` with `--calendar`, `--title`,
      `--when`, `--duration`, and `--noprompt` (ask for the calendar,
      date/time, and duration; if the user doesn't know the calendar name,
      run `gcalcli list` to enumerate the options and let them
      pick), confirm, and run it. If `gcalcli` is unavailable or not
      authenticated, surface the full command for the user to run. Tell the
      user the event will appear in the Org agenda after the next `org-gcal`
      sync. If step 3 established or identified a project, also record a
      `TODO` next action under that project (with an appropriate context tag
      per `gtd`, noting the calendar event in its body) so the project retains
      a visible next action until the event fires.
  - **Defer (as soon as I can)** — record a `TODO` next action with an
    appropriate context tag.

  After any of the above, go to Close.

## Close

Summarize what was written, surfaced, or run (report any returned
`org-id://` URI), and remind the user to clear the original item from their
inbox capture.
