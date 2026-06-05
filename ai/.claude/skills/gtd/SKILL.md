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
- Actions and projects may carry supporting text (reference links,
  implementation details, etc.) in their bodies.

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
  content only for items of interest. Avoid reading whole files unless
  instructed.
- Keep new items brief but understandable to the user.
- Transition action states as work progresses: `TODO`→`DONE` on completion,
  `WAIT`→`TODO` on unblocking.

## Project completion

When a project's outcome is achieved:

1. Review the entire project subtree body (notes, references, residual
   sub-items) for anything implying new next actions, projects, or someday/maybe
   outcomes, and capture those as new `TODO` / `project` / `somedaymaybe` items.
2. Mark the project's top-level headline `DONE` via `org-update-todo-state`.
3. Archive the subtree via `org-archive-subtree` (sends it to the headline's
   configured archive location).

## Availability

`org-mcp` may be absent at session start; the user is aware — proceed without it.
If it disappears mid-session, stop and ask the user for guidance.
