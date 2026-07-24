---
description: >-
  Facilitate a full GTD weekly review over the org-mcp Org files and Google
  Calendar, one section at a time: catch calendar items that slipped, look ahead
  a month, and review every project, standalone next action, waiting-for,
  checklist trigger, and someday/maybe item, then close with a creative,
  whole-system synthesis. Assumes the inbox is already processed and "empty your
  head" is done. Calls gtd for all Org mechanics. Use when the user asks to run
  or do their weekly review.
argument-hint: optional look-back start date (e.g. 2026-06-01); default derives from the last review
allowed-tools: >-
  Skill(gtd)
  Skill(process-inbox)
  AskUserQuestion
  Bash(gcalcli agenda:*)
  Bash(gcalcli add:*)
  Bash(gcalcli list:*)
  mcp__org-mcp__org-get-allowed-files
  mcp__org-mcp__org-get-tag-config
  mcp__org-mcp__org-get-todo-config
  mcp__org-mcp__org-get-agenda-config
  mcp__org-mcp__org-get-agenda
  mcp__org-mcp__org-read-outline
  mcp__org-mcp__org-read-headline
  mcp__org-mcp__org-read-by-id
  mcp__org-mcp__org-grep
  mcp__org-mcp__org-find-tagged-ancestor
  mcp__org-mcp__org-add-todo
  mcp__org-mcp__org-update-todo-state
  mcp__org-mcp__org-set-planning
  mcp__org-mcp__org-edit-body
  mcp__org-mcp__org-edit-headline
  mcp__org-mcp__org-refile-headline
  mcp__org-mcp__org-archive-subtree
---

# Weekly review

Facilitate the user's GTD weekly review over the Org files and Google Calendar.
This skill is **only the review flow** — defer every Org mechanic (states, tags,
project health, completing/archiving, resolving `@waitingfor`, recording) to
`gtd`. Invoke `Skill(gtd)` first and follow its conventions; `allowed-tools`
lists every `org-mcp` operation and `gcalcli` command this flow calls directly
because `gtd` carries no `allowed-tools` of its own.

The user asserts two preconditions: the inbox is already processed and "empty
your head" is done. Where stray items surface at any point, run
`Skill(process-inbox)` on each.

**Discover at runtime, never hardcode.** At the start, read the file set
(`org-get-allowed-files`), contexts (`org-get-tag-config`), states
(`org-get-todo-config`), and the review's agenda views (`org-get-agenda-config`).
Resolve each review list below to a dispatch key by the command's **description**,
not a fixed letter — the keys are the user's and may change.

**Interaction.** Work one section at a time: enumerate, present findings, state
your reading and ask the user to confirm (prefer `AskUserQuestion` over open
questions), apply the agreed edits via `gtd`, then move on. Keep a running list
of every change for the close. A long list may be presented in batches for
pacing, but review **every** item — batching never means dropping or summarizing
items away.

<!-- Keep in sync with process-inbox's Always-confirm rule: the always-confirm
core (state and confirm every chosen context before recording, not only when
unsure) is shared. This copy deliberately broadens the trigger to "or changes"
(step-6 context edits) and names the resource/precedent gloss. -->

**Always-confirm rule.** Whenever this flow or a `gtd` procedure it invokes
chooses or changes an action's self-context — e.g. the **Project health
check**'s new next action, **Promoting a someday/maybe item**'s tag fixup, or a
step-6 context fix — apply `gtd`'s **Choosing the context** procedure (the
resource-required rule and the precedent check). Per `process-inbox`'s rule of
the same name, state your reading and confirm the context with the user before
recording, not only when unsure — a context the user already specified is
recorded as given (per `gtd`).

**Setup — anchor the look-back window.** Find the weekly-review tracking habit
(`org-grep "weekly review"`, or the tracking-habit entry the Agenda view
surfaces) and read its last-completion date from `:LAST_REPEAT:` or the
newest DONE logbook entry (via its `uri`). The calendar look-back start is
**that date minus 14 days**, so the window always overlaps the previous review.
`$ARGUMENTS`, if given, overrides the start date; with no last-completion found,
default to 14 days ago. However overdue the review, cover the **entire** window
in full — a long gap is never a reason to summarize or skip.

Then run the ten steps in order.

## 1. Inbox

The user asserts the inbox is already processed. Verify quickly with
`org-read-outline`, drilling in with `org-read-headline` or `org-grep` if a
heading needs a closer look; if anything remains, run `Skill(process-inbox)` on
each item.

## 2. Recent calendar

`gcalcli agenda <look-back-start> <today>`. Review every past event in the window
for a commitment that slipped — an unfinished follow-up, an implied action,
something to carry forward. For each, decide with the user: reschedule it
(`gcalcli add`, per `process-inbox`'s Google Calendar invocation, including
`--noprompt`), turn it into a next action or project (via `gtd`), re-capture it
and run `process-inbox`, or dismiss it. Before proposing that an event become a
next action or project, run the **Related-item cross-check** so you neither
duplicate an existing item nor ask the user cold.

**Related-item cross-check.** Look for an existing item related to the event — a
next action, a delegation, or a **project**, in any open state (`TODO`/`WAIT`) —
before proposing a new one. Fetch the Agenda view and Projects view once per
step and reuse them across every event this step loops over — only `org-grep`,
re-run per event, needs same-pass freshness. The Agenda view is one composite
call whose single response already carries both its week block (open
`SCHEDULED`/`DEADLINE` items)
and its **Waiting-for items** and per-context blocks; those tag-based blocks list
only `TODO`, undated items (per `gtd`'s States), so they — not `org-grep` — are
the primary source for a `TODO`, undated `@waitingfor`/context action: scan them
first. Neither a `WAIT`-parked delegation/context action nor a pre-existing
**project** ever appears on any of the Agenda view's tag-based blocks, though —
the former because those blocks are `TODO`-only, the latter because each block
is scoped to a context or `@waitingfor` tag while a project heading is not
expected to also carry a context or `@waitingfor` tag. A dated `WAIT` action
or a dated project surfaces on the week block instead (per `gtd`'s States) —
scan it too, for a `WAIT`-state match or a matching project title. So, once the
tag-based blocks and week block have turned up no match, for the project gap
also consult the Projects view's headings by title (a project-only
duplicate check — not step 5's review); for either gap, `org-grep` a
distinctive term from the event (a person, vendor, or thing) — one
literal-substring search that, filtering on neither tag nor state, surfaces
whichever kind is actually there; the title check also misses a `WAIT`-parked
or incubated project outright (step 5's state-based reason or the
`somedaymaybe` exclusion, not wording), otherwise both
checks can still miss on wording, so a miss is inconclusive — lean on user
confirmation rather than assume absence (an `org-grep` miss especially proves
nothing, for either gap: it is a literal substring search, per `gtd`'s **Work
triggers**). Before treating
any candidate as coverage, validate it as `process-inbox` does: consider only
open-state (`TODO`/`WAIT`) matches, apply its **Somedaymaybe-membership check**
(a match incubated under a someday/maybe container is not live coverage), and
route a `WAIT` match through its **Parked-entry check** (surfacing the match's
blocked status and offering to unblock it — coverage still holds, per
`process-inbox`'s **Confirmed active match** treatment). State your reading
against what survives.

## 3. Upcoming month

`gcalcli agenda <today> <today+1month>`. For each upcoming commitment, first run
the **Related-item cross-check**, extending its week-block search across the
month: the Agenda view's week block always anchors on today and reaches only
~7 days out (a custom-command view ignores any date argument), so also run the
built-in `week` span (`org-get-agenda`) at `+7d`, `+14d`, `+21d`, `+28d` — day
offsets, since a whole-month offset can overflow a short month — to cover open
`SCHEDULED`/`DEADLINE` items through the full window, fetched once per step
like the base cross-check's own fetches; the undated-item and project checks
are not date-scoped and already cover the month unchanged. Then decide with
the user whether it needs preparation recorded now (a next action or project,
via `gtd`) or is already covered.

## 4. Empty your head

The user asserts this is done. **Stop here and wait.** The user may issue
`/process-inbox` for any incompletion triggers that came to mind, or tell you to
proceed. Do not advance to step 5 on your own.

## 5. Projects

Run the Projects view (`org-get-agenda` with the resolved key); it lists the
project headings. For each project:

- Read its subtree via the node's `uri` (per `gtd`'s reading guidance) to see
  its actions and any nested sub-projects.
- **Outcome defined?** Confirm the project states a clear desired outcome /
  done-state. If it is vague, work it out with the user and record it
  (`org-edit-body`).
- **Next action present?** Apply `gtd`'s **Project health check** to ensure the
  project has a good next action.
- Review **nested** sub-projects too — each is itself a `project`-tagged `TODO`
  that surfaces on its own in this view, independent of any ancestor's health
  check — plus **outer** ancestors and **deliberately-parked** (all-`WAIT`)
  projects, the two cases `gtd`'s health check explicitly defers to the weekly
  review.
- Sweep the subtree for a `somedaymaybe`-tagged heading below the project (an
  in-project incubation pocket) or an open child carrying a local `somedaymaybe`
  tag — shapes no agenda view enumerates (per `gtd`'s **Incubation check**).
  Normalize each with the user per `gtd`'s Tags and structure: refile its open
  contents under the file's Someday/maybe container to keep them incubating,
  promote them (per `gtd`'s **Promoting a someday/maybe item**), or
  `KILL` + archive.

**Parked/dated cross-check** (steps 5–7, each on its step's tag — one tag for
steps 5 and 7, each of the user's execution contexts for step 6): these
views match only open `TODO` items, so an item parked in `WAIT`, or carrying a
`SCHEDULED`/`DEADLINE`, is off the view (per `gtd`'s States); cross-check with
`org-grep` on the step's tag in its colon-wrapped raw form (`:project:`,
`:@waitingfor:` — `org-grep` is a literal substring search, so the bare word
floods with prose matches), reviewing any `WAIT` or dated open item it
surfaces — and any open item whose own `tags` carry `somedaymaybe` with no
someday/maybe-container ancestor (a stray local tag parks it off every active
view; normalize with the user per `gtd`'s Tags and structure: refile it under
the container to keep it incubating, or shed the tag / promote it if active) —
skipping undated incubated ones under a file's someday/maybe container (step
9's territory). For this step the tag is `project` — it is the project
heading's own state or date that takes it off the view.

## 6. Non-project next actions

Run the non-project next-actions view. For each standalone action, confirm with
the user: still relevant? right context? still the true next physical action?
Judge "right context?" per `gtd`'s **Choosing the context** (by the resource the
action requires — an online step in otherwise-local work, such as a push,
review, or CI, makes it an online action — and its precedent check), then
confirm per the **Always-confirm rule** above. Reschedule, change state,
complete, or kill it via `gtd`, or edit its title or context tag with
`org-edit-headline` (to fix a wrong context, remove the old context tag and add
the new one in one call — they are a mutually-exclusive group).

Apply the **Parked/dated cross-check** on **each** of the user's execution
contexts (per `gtd`'s _Tags and structure_ — the mutually-exclusive context
group from `org-get-tag-config`, minus `@waitingfor` and `@checklist`), so no
context's parked/dated items are skipped.

## 7. Waiting-for

Run the Agenda view and take its **Waiting-for items** block. For each delegated
item, check how long it has been outstanding (read its logbook via the `uri`) and
decide with the user whether it needs a ping. Handle a ping like any other action
— it may be a quick do-it-now or a recorded follow-up; leave the mechanics to
`gtd` / `process-inbox`. Resolve any item that has completed or died via `gtd`'s
**Resolving a delegated item (`@waitingfor`)**.

Apply the **Parked/dated cross-check** on the `@waitingfor` tag.

## 8. Checklist triggers

Enumerate the triggers per `gtd`'s **Work triggers** (which resolves the
Checklists view and drops retired triggers). For each, ask the user whether its
condition currently applies (upcoming travel, a purchase, a release, …), and
whether they still want the rule at all. For each firing trigger, act on its
"then Y" — capture the resulting items and run `process-inbox`, or record them
via `gtd` — then apply `gtd`'s **Work triggers** retirement rule (it retires a
spent fired **one-time** trigger and keeps a **recurring** one for future
firings). Also apply that rule to any trigger whose condition can no longer
occur, or that the user no longer wants (it retires, whether or not it ever
fired). Retire per that rule rather than leaving a spent, dead, or unwanted
trigger to resurface every review.

## 9. Someday/maybe

Run the Someday/maybe view. Review **every** item — the list is large, but
nothing is skipped or summarized away; present in batches only for pacing. For
each item, decide with the user: rehydrate it (promote it to an active next
action or project, in place, per `gtd`'s **Promoting a someday/maybe item** —
the caller decides which shape), keep it incubating, close it as already
achieved (`DONE` + archive per `gtd`'s superseded-outcome rule — surface its
body notes first and run any follow-up work through `Skill(process-inbox)` /
`gtd`), or delete it if interest is fully gone (`KILL` + archive via `gtd`).

The Someday/maybe view lists each incubated outcome in any open state —
`TODO` and `WAIT` alike, since per `gtd`'s States this list is open-state
rather than `TODO`-only, unlike the active views — the someday/maybe
containers' direct children (per `gtd`'s Tags and structure). An item nested
deeper (an incubated project's own actions) surfaces through its parent, not
separately, and a dated item is off this view like any tag-based list (per
`gtd`'s States) — a dated _keyworded_ incubated item is an anomaly the
**Parked/dated cross-check** (steps 5–7) normalizes (per `gtd`'s Tags and
structure); a dated _keyword-less_ one is neither open nor closed, so that
open-scoped check cannot see it — the container sweep below clears its date
instead. A keyword-less legacy item appears on no view at all: sweep each
file's Someday/maybe container (`org-read-headline`) for keyword-less children
the view cannot show, and normalize each with the user per `gtd`'s **Promoting
a someday/maybe item** step 3 (assign `TODO`/`WAIT`, and clear any date to keep
it incubating) or close it.

## 10. Be creative and courageous

This step is GTD's canonical closing move, right after the someday/maybe review:
_"Any new, wonderful, hare-brained, creative, thought-provoking, risk-taking
ideas to add into your system???"_ It is **synthesis, not re-review**: do not
re-litigate items already handled in steps 1–9; step back to the whole and name
what the siloed passes structurally cannot.

Two voices:

1. **Invite the user's own ideas.** Ask, in the spirit of the canonical prompt,
   for any bold, half-formed, or long-deferred idea the review stirred up. Capture
   each via `Skill(process-inbox)` or record it directly through `gtd`.
2. **Offer an outsider read.** Having just seen the entire system this session,
   give a candid, fresh-eyes synthesis as fuel for the same creative step. This
   is advisory — you propose, the user disposes. Surface:
   - **Blind spots** — commitments or areas conspicuously absent or untouched.
   - **Synergies** — one next action that would unblock several projects; two
     projects that overlap or should merge; a waiting-for that gates other work.
   - **Stale / zombie projects** — nominally alive but drifting, candidates to
     `KILL` rather than perpetually carry.
   - **Overcommitment & imbalance** — too many active projects to be real; skew
     across the user's areas of focus (e.g. all work, no personal).
   - **Low-hanging fruit** — quick wins visible only now that all is in view.
   - **Bigger picture** — are the active projects the _right_ things, and do they
     ladder up to anything.

Present the outsider read as a concise list of observations (batch for pacing per
the one-section-at-a-time convention). For each, the user decides whether to act;
route any resulting capture or edit through `gtd` / `Skill(process-inbox)` exactly
as the other steps do, and add it to the running change list for the close. A
purely-advisory observation the user declines needs no Org change.

## Close

- Summarize every change made this session (per `gtd`'s surfacing conventions;
  report any returned `org-id://` URIs).
- Offer to mark the tracking habit `DONE` (`org-update-todo-state`), which
  advances its weekly repeat. Confirm first, then report the new scheduled
  date. If the repeater does not advance cleanly through the tool, tell the
  user to complete it in Emacs instead.
- Per `gtd`'s **Availability**: if `org-mcp` is unavailable, proceed where you
  can; if it disappears mid-review, stop and ask.
- If `gcalcli` is unavailable, proceed too, skipping the calendar steps (2-3)
  (this is the skill's own policy, not `gtd`'s).
