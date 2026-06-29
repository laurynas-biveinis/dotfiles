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

- Every action has an execution context. Tag delegated actions `@waitingfor` and
  your own `@internet`, unless told otherwise.
- `@checklist` marks GTD trigger/checklist items of the form "if X, then do Y".
- `somedaymaybe` (bare, no `@`) marks incubated someday/maybe outcomes — not
  active next actions. It inherits, so tagging a project `somedaymaybe`
  propagates to its children.
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
  `headline_path` verbatim — do not re-encode.
- Keep new items brief but understandable to the user.
- Transition action states as work progresses: `WAIT`→`TODO` on unblocking;
  for closing an item (`DONE`/`KILL`), see "Completing and archiving items"
  below.
- `org-read-outline` nodes and `org-grep` `headline_path` entries carry
  `todo`/`tags`/`uri` per node — read an item's state from these rather than
  parsing raw heading text. For an item's **ancestors'** state use `org-grep`'s
  `headline_path` (full chain); `org-read-outline` reaches only 2 levels deep,
  so it exposes ancestor state only for a level-2 item's level-1 parent;
  level-3+ headings are absent.

## Recording: write or surface

Prefer recording via `org-mcp`. Decide the outcome and its destination, then
check the destination file is writable (`org-get-allowed-files`) and the
operation is supported (`org-add-todo`, `org-update-todo-state`,
`org-set-planning`, `org-refile-headline`, `org-edit-body`,
`org-archive-subtree`). If the file is not exposed or the operation is
unavailable, **surface** the exact item — proposed headline, target
file/heading, tags, state, any timestamp — for the user to file manually. Never
hardcode which files or operations are available; discover at runtime. Never
echo secrets. `org-grep`/`org-get-agenda` see only `org-mcp-allowed-files`;
items elsewhere must be surfaced/asked, not assumed absent.

## Completing and archiving items

When you close an item — `DONE` (completed) or `KILL` (abandoned) — decide
standalone vs project child by scanning the item's ancestors — every entry in
`org-grep`'s `headline_path` except the last, which is the item itself: if any
ancestor carries a `project` tag, the item is a project child; otherwise it is
standalone. For exposed files, each ancestor node in `headline_path` carries
`todo` and `tags`; otherwise ask the user.

A caller that performed an action **that belongs to a project** without
recording it in Org — e.g. a sub-2-minute action a caller did inline — has no
item to close: skip the classification and go directly to the project review
(sub-step 1) and completion check (sub-step 2) of the **Project child** case
below, using the project the caller already holds. (If the action was
standalone, the caller handles Close directly and this rule does not apply.)

- **Standalone** (no ancestor carries a `project` tag): if the item itself
  carries a `project` tag (a top-level project being closed directly), first
  review its subtree body for residual next actions, projects, or someday/maybe
  items and capture any found, then mark it `DONE` (or `KILL`) via
  `org-update-todo-state` and archive the subtree via `org-archive-subtree`.
  Otherwise, mark it `DONE` (or `KILL`) via
  `org-update-todo-state`, then offer to archive via `org-archive-subtree`
  (offer, rather than archive outright, because a standalone item may still
  serve as a reference node the user wants to keep in place).
- **Project child (possibly completing the project):** mark the child item
  `DONE` (or `KILL`) via `org-update-todo-state`, then locate the innermost
  `project`-tagged ancestor in `org-grep`'s `headline_path` (again
  excluding the last entry, the item itself; innermost = the last
  `project`-tagged entry in the remaining ancestors), then:
  1. Review the project subtree body (notes, references, residual sub-items)
     for anything implying new next actions, projects, or someday/maybe
     outcomes, and capture those as new `TODO` / `project` / `somedaymaybe`
     items.
  2. Decide whether the project outcome is reached: if achieved, mark the
     project headline `DONE` via `org-update-todo-state` and
     archive the subtree via `org-archive-subtree`; if its last action was
     `KILL`ed because the outcome became unreachable (not merely superseded by
     completion via another route) and no viable next action remains, mark the
     project `KILL` via `org-update-todo-state` and archive likewise. Otherwise
     ensure the project has a good next action —
     create one, or unblock an existing `WAIT`→`TODO`.

When new input resolves a delegated `WAIT` (`@waitingfor`) item, `org-grep` to
find it; if `WAIT`→`TODO`, transition it to `TODO` (unblock); if `WAIT`→`DONE`,
apply the Completing and archiving items section above (which handles the
state-marking, archiving, and project-health check).

## Availability

`org-mcp` may be absent at session start; the user is aware — proceed without
it. If it disappears mid-session, stop and ask the user for guidance.
