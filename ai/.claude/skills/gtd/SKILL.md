---
description: >-
  Getting Things Done (GTD) task management via the org-mcp Org files. Apply
  whenever the user mentions GTD, tasks, projects, or the agenda; when recording
  or looking up work that persists across sessions; or when choosing between Org
  and TodoWrite.
user-invocable: false
---

# GTD with org-mcp

The `org-mcp` server exposes selected Org files from the user's running Emacs.
They serve both the user and you, implementing Getting Things Done (GTD).

This mechanics skill does not itself run the clarify flowchart. Processing a
single captured/inbox item through it (actionable? next action? 2-minute rule?
delegate/defer?) is the job of the `process-inbox` skill, which calls `gtd` for
the mechanics — `gtd` never invokes `process-inbox`.

## States

- `TODO` — live action, shown on its list.
- `WAIT` — action blocked by a dependency: parked, shown on no active
  tag-based list until reactivated (`WAIT`→`TODO`; the someday/maybe review
  list alone still shows an incubated one — see below). Note what it waits on
  in the item body as a `Blocked by: <dependency>` line (the dependency note);
  on unblocking,
  remove the note or replace it with what cleared it, leaving the rest of the
  body intact. A `SCHEDULED` on a `WAIT` item is the execution date: the item
  must be unblocked and completed by/at that date — a genuine commitment,
  kept through `WAIT`→`TODO`; a `DEADLINE` on a `WAIT` item is the same
  commitment as a hard due date. A start/review date is never expressed as a
  timestamp on a `WAIT` item.
- `DONE` — completed action.
- `KILL` — abandoned action.

State is orthogonal to tags: it records whether the action is live or parked,
never whose move it is — that is the tag's job (see "Tags and structure").
The tag-based agenda lists show only `TODO` items, and only undated ones: an
item with a `SCHEDULED`/`DEADLINE` (or any plain active timestamp) is off every
tag-based list regardless of state. Two lists depart from this. The
someday/maybe review list is the state exception: undated-only like the rest,
but open-state rather than `TODO`-only — it shows `WAIT` incubated outcomes too,
since a parked incubated outcome is reviewable there or nowhere; an incubated
keyword-less item, being neither open nor closed, does not appear on it and must
be normalized per "Promoting a someday/maybe item" step 3. The Checklists list
is the wider exception: it filters on neither state nor date, so it shows every
trigger — keyword-less ones included, which is what makes "Work triggers"'
enumeration complete. A `SCHEDULED`/`DEADLINE` item surfaces on the date-based
agenda instead — from its date onward (`DEADLINE`: prewarning window) — while
open (`TODO`/`WAIT`); closed, it appears on neither. A keyword-less item with a
`SCHEDULED`/`DEADLINE` is neither open nor closed and keeps surfacing as
overdue. A plain active timestamp surfaces the item only on its exact date,
open or closed — once that date passes, an item dated only that way is on no
current view. Hence a date inside an item body — dependency and delegation
notes included — is written as plain text or an inactive timestamp
(`[2026-08-01 Sat]`), never an active `<…>` one, which would count as dating
the item.

## Tags and structure

- Every action has an execution context saying whose move it is:
  `@waitingfor` marks a delegated action — an external party's move, reviewed
  on the waiting-for list and resolved per "Resolving a delegated item
  (`@waitingfor`)" — while every other context marks the user's own action and
  is chosen per **Choosing the context** below. Record a delegation as
  `@waitingfor`, noting who/what it waits on in the item body as a
  `Waiting on <who> for <what>` line (the delegation note); its state follows
  its blocking status per States — `TODO` normally, `WAIT` if the delegation is
  itself blocked by a dependency (add the dependency note as well) — and like
  any action, it appears on its list only while `TODO` and undated (per States).
- **Choosing the context** — a context the user already specified is recorded
  as given; the rest of this procedure governs only a context you choose. The
  user's own contexts are user-configured and discoverable at runtime via
  `org-get-tag-config` (the mutually-exclusive context group, minus
  `@waitingfor` and `@checklist`, which this section defines separately). Never
  assume the set, and never default to a member of it:
  choose by the **resource the action requires** — what must be at hand to
  finish it, not the equipment it incidentally uses. An online service (a
  repository host, CI, a review, the web, mail, a cloud account) requires the
  network; a local-only edit, build, or read requires just the machine; an
  errand requires its place; a synchronous conversation (a call or live chat)
  requires the person or the device, while asynchronous correspondence (mail,
  review comments) requires only its carrying service — the network. Match
  that resource to the discovered context whose name denotes it. Nearly all of
  the user's work happens at a computer, so "done at a computer" never picks a
  context on its own.
  Where an action needs more than one resource, the **most restrictive** — the
  one at hand in the fewest situations — wins: a place, a person, or the
  conversation's device (its carrying service is mere transport) outranks the
  network, and the network outranks the machine, so an action belongs to the
  local machine only if it can be finished start to finish offline — an errand's
  incidental online step leaves it an errand, while any online step in
  otherwise-local work (a push, a review, CI) makes it an online action.
  Development work ending in a push is one such action, not two; never split an
  action merely to separate its local half from its online half.
  Before tagging, check precedent — the nearest analogous existing action's
  context. When a project in hand has sibling actions, those are the nearest
  analogues — read them (`org-read-headline` on the project) if not already in
  hand. With no such siblings — no project in hand, or a project without
  sibling actions — run one `org-grep` on a distinctive term from the headline
  (a repository, tool, or party name) and read its open matches' `tags`, unless
  equivalent open matches are already in hand. Stop there: `org-grep` is a
  literal substring search, so a miss proves nothing (per "Work triggers") —
  fall through to the resource rule.
  Precedent informs the choice but does not override it; an existing tag may
  itself be wrong. **Confirm when unsure:** when the required resource, or the
  context it maps to, is not plain from the material in hand, or precedent
  contradicts it, state your reading and ask the user to confirm the context
  before recording.
- `@checklist` marks GTD trigger/checklist items of the form "if X, then do Y":
  the condition X **must** be stated in the headline, the action Y in the body.
  A standing rule is not an active next action, so it is created with no `TODO`
  keyword (`todo_state` `""`); `@checklist` shares the contexts'
  mutually-exclusive group, so a trigger is never also a context, but it is not
  itself an execution context and must never tag an action. The headline
  invariant serves matching (per "Work triggers"): a candidate condition is
  matched against trigger headlines alone, so an X reachable only by reading a
  body would never fire. Y is read from the body once its trigger matches.
- `somedaymaybe` (bare, no `@`) marks each file's **top-level**, keyword-less
  "Someday/maybe" container heading. Incubated outcomes live under it as
  ordinary open items — `TODO` (or `WAIT` if blocked), tagged `project` or an
  execution context — holding the tag by inheritance. Parking is positional:
  the active agenda views exclude the tag, so an item incubates by living
  under the container, not by a state or a local tag. Record a new
  someday/maybe outcome as a `TODO` **directly under** the container — a direct
  child, so it surfaces on the someday/maybe review list, which shows the
  containers' direct children — via `org-add-todo`, with its normal state and
  tags, and never dated: a `SCHEDULED`/`DEADLINE` is a commitment (per States)
  that would resurface the item on the date-based agenda despite the parking.
  If the destination file has no Someday/maybe container, create one first — a
  top-level keyword-less heading tagged `somedaymaybe` (`org-add-todo`,
  `todo_state` `""`) — or choose a file that already has one (per Recording).
  On finding a dated incubated item, normalize with the user — clear the date
  (`org-set-planning`) to keep it incubating, or promote it (per "Promoting a
  someday/maybe item") if the commitment is real. Likewise, on finding an open
  item carrying a local `somedaymaybe` tag with no container ancestor — the
  local tag does keep it off every active view (a resolved tag like any
  inherited `somedaymaybe`), but it is not a sanctioned parking method: with no
  container ancestor it also never reaches the someday/maybe review list, so it
  is stranded off every view, a legacy artifact — normalize with the user:
  refile it under the container to keep it incubating, or shed
  the local tag (`org-edit-headline`) / promote it (per "Promoting a
  someday/maybe item") if active.
- **Incubation check** — whether an item is incubated, and whether its
  `somedaymaybe` tag is local or inherited. It is incubated when it carries
  `somedaymaybe` on itself or on any ancestor (the container, an enclosing
  pocket, or a rare local tag); the tag is _local_ when on the item's own
  `tags`, _inherited_ when only a proper ancestor carries it. Decide from an
  in-hand `headline_path` by scanning node `tags` (the chain includes the item
  itself), or from a bare `uri` via `org-find-tagged-ancestor` — `include_self`
  true tests membership, its default false tests ancestors only. Every reported
  node's `tags` are local (`org-grep` matches too), so read inheritance from
  the chain, never from one node.
- A project is any outcome requiring multiple actions. Tag it `project`; its
  actions are its children.
- Actions, projects, and someday/maybe items may carry supporting text
  (reference links, implementation details, etc.) in their bodies.

## Where to track new work

Decide the record path:

1. Ephemeral within-session step → TodoWrite.
2. Persists across sessions, in a public source-code project → a GitHub issue,
   used idiomatically (labels, cross-references in commits/PRs). Org then holds
   only a pointer (link) to the issue, not a copy of its content.
3. Date/time-specific commitment that happens on that date regardless of any
   list (GTD's hard landscape) and is tied to no project or context list →
   Google Calendar. Other dated items live in Org via `SCHEDULED`/`DEADLINE`.
4. Persists across sessions otherwise → Org directly.

Heuristic for step 1 vs. persisting: "If the session ends now, does the user
need to remember this?" No → TodoWrite (e.g. "run formatter", "check syntax").
Yes → steps 2–4 (e.g. "fix bug in module X", "implement feature Y").

## Work triggers

The user keeps GTD trigger lists of the form "if X, then do Y", tagged
`@checklist`, shaped per "Tags and structure".

When a candidate condition surfaces during work, enumerate every trigger and
match the condition against their headlines — do not rely on recall. Run the
user's Checklists view via `org-get-agenda`, resolving its dispatch key from
`org-get-agenda-config` by the command's **description**, never hardcoding a
key. Enumeration must be state-agnostic. A `type` other than `tags` disqualifies
the command outright — a `tags-todo` view, the shape of the user's other
tag-based lists (per States), returns only keyword-ed items and would omit every
trigger — but `tags` alone does not qualify it: that command's match string and
settings can re-impose the same filter, and `org-get-agenda-config` exposes
neither. The result settles it — every trigger is keyword-less, so a live
trigger list never enumerates empty. If no description names the checklists
list, its type is not `tags`, or the enumeration returns nothing, surface that
to the user and ask how to proceed: never silently skip trigger matching, and
never fall back to the grep below. One call then returns every trigger across
all allowed files, at any depth, each node carrying its `title` — the condition
X to match, rather than the response's rendered `agenda` text — plus its `todo`
and `uri`; the view filters on no state, so disregard `DONE`/`KILL` results —
retired triggers — before matching. Then read the matched trigger's body for its
Y (per "Reading and editing") and act on it.

Do not instead match by grepping terms drawn from the condition. Matching is a
judgement about meaning; `org-grep` tests whether one literal term, chosen up
front, occurs in the text. Which term would surface a trigger is a property of
that trigger, not of the condition — so a miss proves nothing.

## Reading and editing

- Read outline-first: start from outlines (`org-read-outline`), and request full
  content only for items of interest (`org-read-headline` for a single
  headline's body and its subtree). Avoid reading whole files unless instructed.
- Read a matched headline using the result's `uri` — from `org-grep`, an agenda
  view, or any other node result: for an `org-id://` URI use `org-read-by-id`
  with the uuid; for an `org-headline://` URI, drop the `org-headline://`
  prefix, then split the rest at its single literal `#` — the part before it is
  `file`, the part after is `headline_path`. Pass both undecoded: a `#` in a
  filename arrives as `%23`, which `org-read-headline` decodes itself. So
  `org-headline:///Users/u/gtd.org#Kai%20skrendu` gives `file`
  `/Users/u/gtd.org` and `headline_path` `Kai%20skrendu`. For any other URI
  scheme, surface the URI to the user and ask how to proceed.
- Keep new items brief but understandable to the user.
- Transition action states as work progresses: `WAIT`→`TODO` on unblocking,
  handling the dependency note per States; `TODO`→`WAIT` when a new
  dependency blocks an action, adding the dependency note per States (for a
  standalone, non-incubated item, optionally offer a `SCHEDULED` via
  `org-set-planning` — per States, the execution-date commitment, which
  resurfaces it on the date-based agenda; never a date on an incubated item,
  per Tags and structure) and, for a project child, running the project health
  check — but when the blocking dependency is itself new work that you will
  record as the project's next action, record it first, as an open `TODO`
  under the project, so the check finds a live child and provisions nothing.
  When the blocking dependency is itself new, untracked work — the
  user's own next action or a new delegation, not an external event, a date, or
  an already-tracked action — that blocker must also end up recorded as its own
  action (per "Where to track new work" and "Tags and structure"); a caller
  whose flow already routes captured new work to recording (e.g. `process-inbox`
  via its Captured-new-work rule) satisfies this, so record it here only when
  nothing else will. For closing an item (`DONE`/`KILL`) and the health check
  itself, see "Completing and archiving items" below.
- `org-read-outline` nodes and the `headline_path` entries returned by
  `org-grep`, `org-read-headline`, and `org-read-by-id` carry `todo`/`tags`/`uri`
  per node — read an item's state from these rather than parsing raw heading
  text. `org-read-outline` is top-down (each node has its
  children) but returns **only the top two levels** (level-1 headings with their
  level-2 children); level-3+ items do not appear. Never treat an outline miss
  as proof of absence — use `org-read-headline` (full subtree) or `org-grep` to
  reach deeper items. `org-read-outline` does not surface an item's ancestor
  chain. For an item's **ancestors'** state use the `headline_path` returned by
  `org-grep`, `org-read-headline`, or `org-read-by-id` (full chain with per-node
  `todo`/`tags`/`uri`).
- Before acting on an `org-grep` match as an open item — to close, mutate, or
  treat as already tracked — verify the matched entry's `todo` is not already in
  a closed state (`DONE`/`KILL`). Disregard closed matches; if all matches are
  closed, treat the lookup as not-found.

## Recording: write or surface

Prefer recording via `org-mcp`:

1. **Decide the outcome and its destination.** When several exposed files could
   hold a new top-level item, prefer the one already holding items of its
   kind — discover at runtime via `org-grep`/`org-read-outline`, never hardcode
   a filename. Ask the user when the choice stays ambiguous; a single writable
   exposed file needs no question.
2. **Check the destination is reachable.** The file must be exposed to org-mcp
   (`org-get-allowed-files`) and the operation supported (`org-add-todo`,
   `org-update-todo-state`, `org-set-planning`, `org-refile-headline`,
   `org-edit-body`, `org-edit-headline`, `org-archive-subtree`). Never hardcode
   which files or operations are available; discover at runtime.
3. **If not reachable, surface instead of writing.** If the file is not exposed
   or the operation is unavailable, **surface** the exact item — proposed
   headline, target file/heading, tags, state, any timestamp — for the user to
   file manually.

Never echo secrets. `org-grep`/`org-get-agenda` see only files returned by
`org-get-allowed-files`; items elsewhere must be surfaced/asked, not assumed
absent.

## Promoting a someday/maybe item

Activating an incubated `somedaymaybe` outcome into live work. Done in place, so
its ID, history, and body notes are preserved (not closed and recreated). The
caller decides the target shape (a single next action, or a `project`); this is
the mechanics.

An incubated item is already an ordinary open item (per "Tags and structure"),
so promotion is chiefly the move:

1. If the item sits under a `somedaymaybe`-tagged heading (the normal case, per
   the **Incubation check**), refile it out of that container to its active
   home (per "Where to track new work" / "Recording") with
   `org-refile-headline`: this sheds the inherited `somedaymaybe` tag, which is
   what activates the item — the active views exclude that tag, and
   `org-edit-headline` could not shed it (it edits local tags only). An item
   incubated only by a local `somedaymaybe` tag has no container to leave:
   refile it only if a move to its active home is otherwise needed (its current
   location is not that home), and step 2's removal of the local tag is then the
   activation. After any refile, address the item in the steps below by the
   `org-id://` URI the refile returns — the move invalidates a pre-refile
   `org-headline://` URI.
2. Fix up tags only if needed (`org-edit-headline`, one call): remove a rare
   local `somedaymaybe` tag, and ensure the shape's tag is present — an
   execution context (per "Tags and structure") for a single next action, or
   `project` for a project. Incubated items normally already carry it.
3. Confirm the state: normally already `TODO`; handle a `WAIT` per States —
   keep it with its dependency note while blocked, or unblock `WAIT`→`TODO`
   (`org-update-todo-state`) if its dependency has cleared. A legacy
   keyword-less item gets `TODO` — or `WAIT` plus a dependency note per States
   if genuinely blocked — via `org-update-todo-state` (`current_state` `""`);
   when a caller reuses this step to normalize a keyword-less item **in place,
   kept incubating** (per States, Harvest residuals, or the weekly review —
   none of which refiled it out via step 1), also clear any
   `SCHEDULED`/`DEADLINE` (`org-set-planning`) so it is not left dated, since an
   incubated item is never dated (per "Tags and structure"). A genuinely
   promoted item, refiled to its active home, may keep a legitimate date.
4. For a project, run the **Project health check** to ensure it has a good
   next action.

Determine local vs inherited `somedaymaybe` per the **Incubation check**, read
before any refile — step 1 branches on it. If a file or operation isn't
reachable, surface the item instead (per "Recording").

## Completing and archiving items

When you close an item — `DONE` (completed) or `KILL` (abandoned) — decide
standalone vs project child by calling `org-find-tagged-ancestor` with the
item's `uri` and tag `project` (leave `include_self` at its default false, so
only the ancestors are tested): a null `found` means standalone; a non-null
`found` means project child, and that node is the innermost `project`-tagged
ancestor — reuse it (its `uri`) for the project operations below. Every read
tool and `org-grep` result already carries the item's `uri` — an
`org-read-outline` node included — so pass it straight in. Only a bare title
first needs `org-grep` to obtain a URI: run it with the title, and if
`org-grep` returns no results, or
only closed matches, ask the user to identify the item before proceeding; if
more than one open match remains, show the candidates to the user and ask which
item to use.

**Superseded-outcome rule:** when closing an action whose outcome has been
reached or mooted — possibly via another route — the close state reflects
whether the action's task was, in the end, actually carried out, not merely
which actor did it: `DONE` if it was — the delegate delivered, the user did
their own action (even bypassing its dependency), or the user did a
delegated task themselves rather than await the delegate — `KILL` only if
the task went undone (a delegation dropped with nothing delivered and its
outcome no longer pursued, an own action mooted). A `project`-tagged item's
close state instead records the fate of its outcome: `DONE` if reached —
however reached — `KILL` only if unreachable (per the Project child bullets
below).

A caller that performed an action **that belongs to a project** without
recording it in Org — e.g. a sub-2-minute action a caller did inline — has no
item to close: skip the classification and go directly to the _Decide whether
the project outcome is reached_ decision in the **Project child** case below,
treating the project the caller already holds as the innermost ancestor.
(If the action was standalone, the caller handles Close directly and this
rule does not apply.)

Both close cases below — **Standalone** and **Project child** — invoke the
shared **Harvest residuals** procedure (defined after them) only on the
sub-paths each specifies, not on every close.

**To-done marking:** every close marks its target — and Harvest residuals its
residual kills — to `DONE`/`KILL` via `org-update-todo-state`. Org's dependency
and checkbox enforcement veto a to-done change while the entry has an open
(`TODO`/`WAIT`) descendant at any depth, or an unchecked checkbox
(`- [ ]`/`- [-]`) in its **own body**, and the tool then **errors and does not
save** — it does not report success on an unchanged item. So before any to-done
mark, clear both blockers: relocate or close open descendants first (Harvest
residuals does this; on a multi-item `KILL`, deepest-first — children before
parents), and resolve the target's own unchecked checkboxes with the user via
`org-edit-body` (tick the done ones; make the rest plain `-` list lines). Should
a to-done mark error anyway, stop the cascade and surface which mark was vetoed,
the blocking descendant or checkbox, and what the cascade already changed; then
resolve that blocker (relocate/close the descendant, or fix the checkbox) and
retry the mark, or abort — never power through a partially-applied close.

**Pre-archive check:** before any `org-archive-subtree` in a close, re-read the
target subtree (`org-read-headline`); if any `TODO`/`WAIT` item remains inside,
stop and surface it to the user rather than archiving. With enforcement on and
the fixed tool, a successful to-done mark already implies a clean subtree
(dependency enforcement is all-descendant), so this re-read is a thin final
guard against a harvest logic-gap before the irreversible archive, not a
detector of silent failure.

### Standalone

No ancestor carries a `project` tag. If the item itself carries a `project` tag
(a top-level project being closed directly), harvest residuals (`DONE` or
`KILL`, matching the intended close state), then mark it `DONE` (or `KILL`) via
`org-update-todo-state` and archive the subtree via `org-archive-subtree`.
Otherwise, mark it `DONE` (or `KILL`) via `org-update-todo-state`, then offer to
archive via `org-archive-subtree` (offer, rather than archive outright, because
a standalone item may still serve as a reference node the user wants to keep in
place; callers may direct archive-unconditionally when the item's purpose is
exhausted and no such reference value remains — an incubated item whose outcome
is settled, or a retired `@checklist` trigger whose condition can no longer
occur).

### Project child

Possibly completing the project. If the child item itself carries a `project`
tag (a nested sub-project), harvest residuals (`DONE` or `KILL`, matching the
intended close state), then mark the child item `DONE` (or `KILL`) via
`org-update-todo-state`. Otherwise (child is not project-tagged), mark it `DONE`
(or `KILL`) directly. The innermost `project`-tagged ancestor is the `found`
node returned by the classification `org-find-tagged-ancestor` call above.
Decide whether the project outcome is reached:

- if achieved: harvest residuals (`DONE`), then mark the project headline `DONE`
  via `org-update-todo-state` and archive the subtree via `org-archive-subtree`.
- if the outcome became unreachable (not merely superseded by completion via
  another route): harvest residuals (`KILL`), then mark the project `KILL` via
  `org-update-todo-state` and archive likewise.
- if uncertain which branch applies — e.g. the last open action was just closed
  but the user has not explicitly confirmed the project outcome — ask the user
  before archiving or KILLing remaining open actions.
- Otherwise: run the **Project health check** below.

### Project health check

Ensure the innermost `project`-tagged ancestor still has a good next action.
The **Project child** close's Otherwise branch above runs it, as does parking
a project child (`TODO`→`WAIT`, per the transitions bullet in "Reading and
editing"). When the project under check is itself incubated (per the
**Incubation check**), skip the check: incubation is the park — no
provisioning, no deliberately-parked confirmation, and no `SCHEDULED`/re-date
offers (never a date on an incubated item, per Tags and structure); normalize
any dated open child per Tags and structure instead of noting its date, and
leave the rest to the weekly review's someday/maybe step. Otherwise proceed.
An open descendant that is itself incubated relative to the project — under a
`somedaymaybe`-tagged heading below the project, or carrying a local
`somedaymaybe` tag (per the **Incubation check**, testing the headings between
it and the project) — is not a live next action: treat it as absent throughout
this check (it neither satisfies the criterion below nor counts as a `WAIT` to
offer or park), optionally normalizing the stray with the user per Tags and
structure.
If an open `TODO` child remains, nothing more is needed.
Otherwise: create one, or offer `WAIT`→`TODO` on an existing `WAIT` —
confirming with the user that its dependency has cleared before flipping,
and handling its dependency note per States. For a `WAIT` with no dependency
note (its park predates the current convention or went stale), first ask
what it was waiting on; per the answer, offer the unblock, backfill the
dependency note (the item then counts as still blocked below), or close it
if the park is moot. If every remaining open action is a `WAIT` still
blocked by its dependency: when one already carries a `SCHEDULED`/`DEADLINE`
that has not passed, the project is already deliberately parked and dated
per States — note the date to the user rather than re-confirming; when that
date has passed, surface the breached commitment and offer to re-date it.
Otherwise confirm with the user that the project is deliberately parked —
optionally offering a `SCHEDULED` on a blocking `WAIT` per States — rather
than creating a filler action; the weekly review re-examines it in every
case. This check covers only the innermost project ancestor; any outer
`project`-tagged ancestors are reviewed during the weekly review.

### Harvest residuals

Shared sub-procedure the **Standalone** and **Project child** cases invoke — not
a standalone step; run it only when a case above directs. Collect anything still
live in the closing subtree and relocate it clear of the close cascade.

Place relocated items outside the closing project subtree and outside any
ancestor being closed in the same cascade — at the top level of the appropriate
Org file, or under another open project not part of the current close cascade;
when unsure whether an ancestor will be closed, prefer the top level of the file
— so they are not archived with the project.

Relocate **subtree-once**: move only the shallowest live residual in each
ancestry chain — one not nested under another live residual being relocated — via
`org-refile-headline`; its descendants travel with it in that single move, since
refile carries the whole subtree out intact (state, tags, body, and ID
preserved). Never separately refile a descendant of a residual you already
relocated. What matters is _live_: a live residual under a _dead_ (`DONE`/`KILL`)
intermediate that will be archived with the project is still rescued.
Subtree-once governs the two-destination ordering below: a someday/maybe
residual nested under another live residual being relocated travels with it
(staying incubated at the destination), so the "first … then" ordering applies
only to residuals that are themselves relocation roots.

A **someday/maybe residual** is a live (`todo_state` not `DONE`/`KILL`) item
under a `somedaymaybe`-tagged heading that itself lies strictly below the close
target (an in-subtree someday/maybe pocket), or a live item carrying a local
`somedaymaybe` tag. A tagged heading at or above the close target never
qualifies: when the close target is itself incubated (per the **Incubation
check**), its live descendants are not thereby someday/maybe residuals; on
`KILL` they die with the blanket kill, as deleting an incubated item intends,
and on `DONE` the incubated-close-target rule below applies. A keyword-less
`somedaymaybe`-tagged heading with live descendants is a container, not an
outcome — once its live contents are relocated, it archives with the close; a
keyword-less `somedaymaybe`-tagged heading with no live descendants is a legacy
outcome (the old convention's shape) and is itself a residual — relocate it
under the file's container and normalize per "Promoting a someday/maybe item"
step 3.

**Container target:** every "under the file's Someday/maybe container"
relocation in this procedure follows Tags and structure — if the file has no
container, create one first (a top-level keyword-less `somedaymaybe` heading,
`org-add-todo`) or route to a file that already has one.

**Kill ordering:** any `KILL` this procedure applies to an item with open
(`TODO`/`WAIT`) descendants proceeds deepest-first — children before their
parents — each mark following the **To-done marking** rule above: resolve the
item's own unchecked checkboxes (`- [ ]`/`- [-]`) first, and, since
deepest-first has already closed its descendants, its dependency blocker is
clear — so no mark trips Org's enforcement veto.

- On `DONE`: first scan the closing subtree's bodies (notes, references) for
  anything implying new next actions, projects, or someday/maybe outcomes not
  already present as live headlines, and capture those as new `TODO` /
  `project` items placed clear of the close cascade (per the placement rule
  above) — a someday/maybe outcome as a `TODO` directly under the file's
  Someday/maybe container (per Tags and structure). Then relocate residuals:
  **first** any someday/maybe residuals — move them directly under the file's
  Someday/maybe container; **then**, by the placement rule, the remaining
  residual open actions (`todo_state` `TODO`/`WAIT`) and sub-projects
  (`todo_state` `TODO`, tagged `project`) — except, when the close target is
  itself incubated (per the residual definition above), its own such
  descendants, which the **Incubated-close-target rule** below handles, not the
  placement rule — plus standing `@checklist` rules (`todo_state` `""`, tagged
  `@checklist`), which relocate clear regardless of incubation. No separate
  `KILL` is needed — the move removes each from the closing subtree.
  **Incubated-close-target rule:** when the close target is itself incubated
  (per the residual definition above — a realized incubated outcome), its
  still-live descendants **other than someday/maybe residuals and standing
  `@checklist` rules (both relocated by the passes above)** are incubated
  outcomes, not active residuals: for
  each shallowest live one, decide with the user — `KILL` it as mooted by the
  realized outcome (per the superseded-outcome rule and **Kill ordering**; it
  then archives with the close), keep it incubating (refile it directly under
  the container), or promote it (per "Promoting a someday/maybe item") —
  defaulting to the container.
- On `KILL`: a parent may be `KILL`ed only once all its child projects are
  themselves closed (`DONE`/`KILL`) — an abandoned parent must not force-kill a
  still-viable sub-project. If any live (`todo_state` not `DONE`/`KILL`) child
  `project` — other than one qualifying as a someday/maybe residual (relocated
  below, not killed) or, when the close target is itself incubated (per the
  residual definition above), any other descendant project (which dies with
  the blanket kill below) — remains in the subtree, **block the parent
  `KILL`**: surface each and ask the user to close it (`DONE`/`KILL`, via its
  own **Project child** close) or refile it clear of the cascade
  (`org-refile-headline`) first, then resume. Once no such child `project`
  remains, first relocate any someday/maybe residuals (directly under the
  file's Someday/maybe container, **before** the blanket `KILL` below — or
  their open states die with the cascade) and, by the placement rule, standing
  `@checklist` rules (`todo_state` `""`, tagged `@checklist`), then `KILL` all
  remaining `TODO`/`WAIT` **descendants** via `org-update-todo-state`, per
  **Kill ordering** (deepest-first, checkboxes resolved).

### Resolving a delegated item (`@waitingfor`)

When new input resolves a delegation or updates its status, `org-grep` to
find it. Candidates are open (`TODO`/`WAIT`) results carrying `@waitingfor`
on the item itself; an open item without the tag is the user's own action
and out of scope for this procedure. If `org-grep` yields no qualifying
candidate — no results, only closed matches, or no open `@waitingfor`-tagged
result — ask the user to identify the delegated item before proceeding,
never treating the delegation as absent (per Recording's surfaced/asked
rule); if the user confirms none exists, there is nothing to resolve and the
procedure ends. Confirm the identified item(s) with the user before acting,
even for a single candidate; if more than one candidate matches, show them
and ask which delegated item(s) (one, some, or all) the new input resolves
or updates — one input from a party may bear on several items delegated to
them. Check each confirmed item for incubation (per the **Incubation check**) and
disclose incubated provenance at confirmation, surfacing the item's body
notes. For an incubated item, the
closing flavors below direct the Standalone close to archive unconditionally
(its incubation purpose is exhausted); the leave-open flavors leave it
incubating — say so, never implying a return to the review list — and
_Delegation dead_'s still-wanted ask offers recording the follow-up as a new
someday/maybe outcome (staying parked) as well as active work. Apply the
outcome mapping below to each confirmed item, per what the
input reports for it:

- _Awaited result delivered_: close the item `DONE` per the **Standalone** or
  **Project child** case above (which handles the state-marking, archiving,
  and project-health check).
- _Outcome achieved without the delegate delivering_ — the user or another
  route did the task: close it `DONE` likewise, per the superseded-outcome
  rule.
- _Delegation dead — neither delivery nor outcome_ — the delegate refuses or
  cannot deliver, or either side withdrew the delegation: for a standalone
  item, first ask whether the outcome is still wanted, and if so ensure the
  follow-up (a new delegation, or a new self-context action) is recorded
  before closing — though a caller whose flow already establishes the outcome
  is still wanted and routes captured new work to recording (e.g.
  `process-inbox` via its Captured-new-work rule) satisfies both the ask and
  the capture, so ask and capture here only when nothing else will; a project
  child's continuity is covered by the close's project-health check. Then
  close it `KILL` likewise (abandoned action, per States).
- _Partial delivery, delegation still awaited_: leave the item open,
  optionally noting the partial result in its body.
- _Own dependency cleared, delivery still awaited_ — nothing delivered, the
  delegation itself intact: leave the item open; the `WAIT`-state handling
  below offers the unblock (the delegation note stays).
- _Newly blocked by a dependency, delivery still awaited_ — nothing
  delivered, the delegation itself intact: leave the item open, parking it
  (`TODO`→`WAIT`) per the transitions bullet — add the dependency note per
  States; the delegation note and tag stay, and that bullet's standalone
  `SCHEDULED` offer and project-child health check apply.

When a confirmed candidate in `WAIT` state remains open, key the handling on
its dependency status: if the item's own blocking dependency has cleared — or
it has none (its `WAIT` predates the current convention or went stale) —
offer `WAIT`→`TODO` (tag kept) per the Tags convention, handling the
dependency note per States (the delegation note stays), so the still-awaited
delegation returns to the review list — if undated and not incubated; a
dated one resurfaces on the date-based agenda at its date instead, per
States, and an incubated one stays off the active views until promoted —
disclose this when offering the unblock. If it is still
blocked by a dependency of its own, keep `WAIT`, updating the dependency
note when the input reports the blocker changed. A follow-up action of the
user's own is always a new self-context item, never the delegated item
retagged or relabeled.

## Availability

`org-mcp` may be absent at session start; the user is aware — proceed without
it. If it disappears mid-session, stop and ask the user for guidance.
