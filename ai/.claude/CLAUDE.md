# CLAUDE.md

## Communication Style

Be precise and brief. Do not restate the obvious. This applies to all your
output — replies to the user, written artifacts (plans, design documents, commit
messages, etc.), and source code comments. Source code comments have additional
guidance in [Code Comments](#code-comments) below.

When presenting multiple options/alternatives to choose from, NEVER use Greek
letters (α, β, γ, δ, ...). The user does not know the full Greek alphabet and
it's awkward to type them in conversation.

Never reference ephemeral IDs (review IDs from `review-changes` or similar
tools) in any artifact — duplicate the relevant finding text instead.

## Task Management

You have access to an `org-mcp` MCP server that exposes selected Org files from
the user's running Emacs. They serve both the user and you, and implement
Getting Things Done (GTD) as follows:

- Next actions ready to execute are in `TODO`.
- Actions blocked by dependencies are in `WAIT`.
- Completed actions are in `DONE` and abandoned ones in `KILL`.
- All actions have an execution context. Tag delegated actions `@waitingfor`
  and your own `@internet` unless told otherwise.
- A project is any outcome requiring multiple actions. It is tagged `project`
  and contains actions as children.
- Actions and projects may have supporting text (reference links,
  implementation details, etc.) in their bodies.

Use these Org files as your working memory and todo system. To decide between
TodoWrite and Org, ask: "If the session ends now, does the user need to remember
this?" If no, use TodoWrite (e.g., "run formatter", "check syntax"). If yes, use
Org (e.g., "fix bug in module X", "implement feature Y").

When exploring Org files, avoid reading them in full unless instructed. Start
with outlines, then request full content for items of interest.

Keep new items brief but understandable to the user. Update task states as
work progresses (`TODO` to `DONE` on completion, `WAIT` to `TODO` on
unblocking).

If `org-mcp` is unavailable at session start, proceed without it; the user is
aware. If it disappears mid-session, stop and ask the user for guidance.

## Domain Specific Memory Extensions

For code in these languages, include these files in memory:

- Elisp (Emacs Lisp): @CLAUDE-elisp.md
- Shell scripts: @CLAUDE-shell.md

## Guardrails

If the repository has a `./check.sh` script, it runs tests and formatting
checks. Run it after every change. NEVER suppress test failures, linting warnings,
or errors unless the user explicitly allows it.

If `./check.sh` is absent, use linters and test commands proactively at your
discretion.

## Focus

NEVER switch tasks mid-work. If you notice something off the critical path,
note it in a scratch file and ask the user about it when the current task is
done.

## Analysis

When analyzing a bug or code inner workings:

1. Do not guess. Collect hard evidence for each hypothesis. Adding temporary
   tracing for this is OK.
1. Explore multiple approaches in turn, systematically.
1. NEVER include any time-based effort estimates in your plans.

## Design

Always consider trade-offs and alternatives. If they exist, present them to
the user.

## Development Methodology

Follow DRY and YAGNI strictly in all projects. For testable projects, also
follow TDD strictly; for mostly configuration-storing projects (e.g. dotfiles),
full TDD may not apply.

YAGNI exception: a single-caller abstraction is acceptable when it meaningfully
breaks up a long, deeply nested function. Readability of the original site
outweighs the "no helper for a one-shot" default in this case.

## Test Code

Test code must be fail-fast and fail-hard — no defensive programming:

- Assert directly on values without null/optional guards. Let the test crash if
  the value is missing; never write
  `if x = get-value(); x != nil: assert x.value == 42` —
  write `assert get-value().value == 42`.
- Never catch exceptions in test bodies unless the test explicitly targets error
  behavior. Never use `try/catch` to silently discard errors.
- Do not use fallback operators (`x or default`, `x ?? default`, null-guard
  forms) on values under test — they mask failures.
- Do not use conditional assertions (`if condition: assert ...`); the test
  silently passes when the condition is false.
- Assert specific expected values, not just existence:
  `assert x == 42` rather than `assert x != nil`.
- Test helpers and setup code must fail loudly on unexpected state, not degrade
  gracefully.

## Code Comments

- Do not add comments that restate the code.
- Descriptive variable and function names often eliminate the need for comments.
- Prefer comments that explain the "why", not the "what", when the code isn't
  self-explanatory.
- Use comments to explain complex algorithms, non-obvious side effects, or
  historical context.

## Commit Guidelines

A logical commit represents a single, cohesive change that:

- Addresses one concern (one feature, one bugfix, or one refactoring)
- Passes all tests and quality checks (`./check.sh`)
- Can be understood and reverted independently
- Doesn't mix unrelated concerns

When in doubt: "Does this commit tell one coherent story?"
