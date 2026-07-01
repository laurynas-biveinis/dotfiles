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

- `TODO` — next action ready to execute.
- `WAIT` — action blocked by a dependency.
- `DONE` — completed action.
- `KILL` — abandoned action.

## Tags and structure

- Every action has an execution context. Tag delegated actions `@waitingfor`;
  tag your own online actions `@internet`, unless told otherwise. The user's
  full context set (e.g. `@home`, `@office`, `@calls`, `@errands`) is
  user-configured at runtime and discoverable via `org-get-tag-config` (the
  mutually-exclusive context group, minus `@waitingfor`) — `@internet` is only
  your default for online actions, not a catch-all for the user's physical
  actions.
- `@checklist` marks GTD trigger/checklist items of the form "if X, then do Y";
  like someday/maybe, a standing rule is not an active next action, so it is
  created with no `TODO` keyword (`todo_state` `""`).
- `somedaymaybe` (bare, no `@`) marks incubated someday/maybe outcomes — not
  active next actions; created with no `TODO` keyword (`todo_state` `""`). It
  inherits, so tagging a project `somedaymaybe` propagates to its children.
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
3. Persists across sessions otherwise → Org directly.

Heuristic for step 1 vs. persisting: "If the session ends now, does the user
need to remember this?" No → TodoWrite (e.g. "run formatter", "check syntax").
Yes → step 2 or 3 (e.g. "fix bug in module X", "implement feature Y").

## Work triggers

The user keeps GTD trigger lists of the form "if X, then do Y", tagged
`@checklist`. When a candidate condition X surfaces during work, call `org-grep`
with terms drawn from X to find any matching trigger, then act on its Y — do not
rely on recall.

## Reading and editing

- Read outline-first: start from outlines (`org-read-outline`), and request full
  content only for items of interest (`org-read-headline` for a single
  headline's body and its subtree). Avoid reading whole files unless instructed.
- After `org-grep`, read a matched headline using the result's `uri`: for an
  `org-id://` URI use `org-read-by-id` with the uuid; for an `org-headline://`
  URI use `org-read-headline`, passing the result's `file` field as `file` and
  the URI's already-encoded fragment (the path component after `#`) as
  `headline_path` verbatim — do not re-encode. For any other URI scheme,
  surface the URI to the user and ask how to proceed.
- Keep new items brief but understandable to the user.
- Transition action states as work progresses: `WAIT`→`TODO` on unblocking;
  for closing an item (`DONE`/`KILL`), see "Completing and archiving items"
  below.
- `org-read-outline` nodes and `org-grep` `headline_path` entries carry
  `todo`/`tags`/`uri` per node — read an item's state from these rather than
  parsing raw heading text. `org-read-outline` is top-down (each node has its
  children) but returns **only the top two levels** (level-1 headings with their
  level-2 children); level-3+ items do not appear. Never treat an outline miss
  as proof of absence — use `org-read-headline` (full subtree) or `org-grep` to
  reach deeper items. `org-read-outline` does not surface an item's ancestor
  chain. For an item's **ancestors'** state use `org-grep`'s `headline_path`
  (full chain with per-node metadata).
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
   `org-edit-body`, `org-archive-subtree`). Never hardcode which files or
   operations are available; discover at runtime.
3. **If not reachable, surface instead of writing.** If the file is not exposed
   or the operation is unavailable, **surface** the exact item — proposed
   headline, target file/heading, tags, state, any timestamp — for the user to
   file manually.

Never echo secrets. `org-grep`/`org-get-agenda` see only files returned by
`org-get-allowed-files`; items elsewhere must be surfaced/asked, not assumed
absent.

## Completing and archiving items

When you close an item — `DONE` (completed) or `KILL` (abandoned) — decide
standalone vs project child by scanning the item's ancestors — every entry in
`org-grep`'s `headline_path` except the last, which is the item itself: if any
ancestor carries a `project` tag, the item is a project child; otherwise it is
standalone. For exposed files, each ancestor node in `headline_path` carries
`todo` and `tags`; otherwise ask the user. If you do not already hold an
`org-grep` result for this item (e.g. you reached it via `org-read-by-id` or
`org-read-outline`), run `org-grep` with the item's title to obtain its
`headline_path` before classifying. If `org-grep` returns no results, or only
closed matches, ask the user to identify the item
before proceeding. Cross-reference by URI first: match the item's known URI
against the `uri` field of each result; if exactly one result matches, use that
chain. If the match is ambiguous or no URI is available, show the candidates to
the user and ask which item to use.

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

### Standalone

No ancestor carries a `project` tag. If the item itself carries a `project` tag
(a top-level project being closed directly), harvest residuals (`DONE` or
`KILL`, matching the intended close state), then mark it `DONE` (or `KILL`) via
`org-update-todo-state` and archive the subtree via `org-archive-subtree`.
Otherwise, mark it `DONE` (or `KILL`) via `org-update-todo-state`, then offer to
archive via `org-archive-subtree` (offer, rather than archive outright, because
a standalone item may still serve as a reference node the user wants to keep in
place; callers may direct archive-unconditionally when the item's incubation
purpose is exhausted).

### Project child

Possibly completing the project. If the child item itself carries a `project`
tag (a nested sub-project), harvest residuals (`DONE` or `KILL`, matching the
intended close state), then mark the child item `DONE` (or `KILL`) via
`org-update-todo-state`. Otherwise (child is not project-tagged), mark it `DONE`
(or `KILL`) directly. Then locate the innermost `project`-tagged ancestor in
`org-grep`'s `headline_path` (excluding the last entry, the item itself;
innermost = the last `project`-tagged entry in the remaining ancestors). Decide
whether the project outcome is reached:

- if achieved: harvest residuals (`DONE`), then mark the project headline `DONE`
  via `org-update-todo-state` and archive the subtree via `org-archive-subtree`.
- if the outcome became unreachable (not merely superseded by completion via
  another route): harvest residuals (`KILL`), then mark the project `KILL` via
  `org-update-todo-state` and archive likewise.
- if uncertain which branch applies — e.g. the last open action was just closed
  but the user has not explicitly confirmed the project outcome — ask the user
  before archiving or KILLing remaining open actions.
- Otherwise: ensure the project has a good next action — create one, or unblock
  an existing `WAIT`→`TODO`. This health check covers only the innermost project
  ancestor; any outer `project`-tagged ancestors are reviewed during the weekly
  review.

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

- On `DONE`: first scan the closing subtree's bodies (notes, references) for
  anything implying new next actions, projects, or someday/maybe outcomes not
  already present as live headlines, and capture those as new
  `TODO` / `project` / `somedaymaybe` items placed clear of the close cascade
  (per the placement rule above). Then relocate, by that rule, residual next
  actions and delegated items (`todo_state` `TODO`/`WAIT`), sub-projects
  (`todo_state` `TODO`, tagged `project`), someday/maybe items (`todo_state`
  `""` — no keyword, tagged `somedaymaybe`), and standing `@checklist` rules
  (`todo_state` `""`, tagged `@checklist`). No separate `KILL` is needed — the
  move removes each from the closing subtree.
- On `KILL`: a parent may be `KILL`ed only once all its child projects are
  themselves closed (`DONE`/`KILL`) — an abandoned parent must not force-kill a
  still-viable sub-project. If any live (`todo_state` not `DONE`/`KILL`) child
  `project` remains in the subtree, **block the parent `KILL`**: surface each
  and ask the user to close it (`DONE`/`KILL`, via its own **Project child**
  close) or refile it clear of the cascade (`org-refile-headline`) first, then
  resume. Once no live child `project` remains, `KILL` all remaining
  `TODO`/`WAIT` items in the subtree via `org-update-todo-state`, then relocate
  any live someday/maybe items (`todo_state` `""` — no keyword, tagged
  `somedaymaybe`) and standing `@checklist` rules (`todo_state` `""`, tagged
  `@checklist`) by that rule.

### Resolving a delegated `WAIT` (`@waitingfor`)

When new input resolves a delegated `WAIT` (`@waitingfor`) item, `org-grep` to
find it; if more than one `WAIT`/`@waitingfor` result matches, show the
candidates and ask the user to identify which delegated item the new input
resolves before acting. If `WAIT`→`TODO`, transition it to `TODO` (unblock); if
`WAIT`→`DONE`, close it per the **Standalone** or **Project child** case above
(which handles the state-marking, archiving, and project-health check).

## Availability

`org-mcp` may be absent at session start; the user is aware — proceed without
it. If it disappears mid-session, stop and ask the user for guidance.
