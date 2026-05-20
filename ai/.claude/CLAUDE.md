# CLAUDE.md

This file provides guidance to you, Claude Code, when working with any
repositories owned by the user. There will be project-specific CLAUDE.md files
that will provide project-specific guidance.

## Task management

You have access to an `org-mcp` MCP server that provides access to the selected
Org files in user's running Emacs. They serve both the user and you, the agent,
and implement Getting Things Done (GTD) system as follows:

- Immediate next actions available for execution are in the `TODO` state.
- Actions blocked by the dependencies are in the `WAIT` state.
- Completed actions are in `DONE` state and abandoned ones are in `KILL` state.
- All actions have an execution context. An action delegated to someone else is
  tagged with `@waitingfor`. For your own actions, always use `@internet` unless
  instructed otherwise.
- A project is any outcome that needs more than one action to be achieved. A
  project is tagged with `project` tag and contains actions as children.
- All actions and projects may have any support text material (i.e. reference
  links, implementation details) in their bodies.

Use these Org files as your working memory and todo system. To decide between
TodoWrite and Org, ask: "If the session ends now, does the user need to remember
this?" If no, use TodoWrite (e.g., "run formatter", "check syntax"). If yes, use
Org (e.g., "fix bug in module X", "implement feature Y").

When familiarizing with Org file content, avoid reading complete files, unless
explicitly instructed to do so. Start by reading their outlines, then requesting
full content for the outline items of interest.

When you write new items in Org, make them brief but understandable for the
user. Update Org task states as work progresses (`TODO` to `DONE` on completion,
`WAIT` to `TODO` on unblocking).

If `org-mcp` MCP server is unavailable at the start of the session, work as you
would without it. Assume the user is aware of its unavailability. However, if
the server disappears in the middle of a session, stop and ask the user for
guidance.

## Domain Specific Memory Extensions

For code written in each of the following languages, include these files in
memory:

- Elisp (Emacs Lisp): @CLAUDE-elisp.md
- Shell scripts: @CLAUDE-shell.md

## Communication Style

When presenting multiple options/alternatives to choose from, NEVER use Greek
letters (α, β, γ, δ, ...). The user does not know the full Greek alphabet and
it's awkward to type them in conversation.

## Guardrails

The `./check.sh` script in the repository, if present, will help you stay on
course by making sure that all the tests pass and the code is properly
formatted. Always run it after every change to validate it. NEVER suppress
testcase failures, linting warnings or errors unless explicitly allowed by the
user.

If `./check.sh` is not present, then use the linters and test-running commands
proactively at your discretion.

## Focus

NEVER switch tasks mid-work. If you notice something that needs or could be done
but is not on the critical path for the current work, write it down to a scratch
file and ask the user for guidance once you are finished with the current task.

## Analysis

When analyzing a bug or code inner workings:

1. Do not guess. Collect hard evidence for any hypotheses you come up with. It's
   OK to add temporary tracing to the code for this purpose.
1. Explore multiple approaches in turn systematically and methodically.
1. NEVER include any time-based effort estimates in your plans.

## Design

Always consider trade-offs and alternative options. If they exist, present them
to the user.

## Development methodology

Follow DRY and YAGNI strictly in all projects. For testable projects,
additionally follow TDD strictly; for mostly configuration-storing projects
(e.g. dotfiles), full TDD may not apply.

YAGNI exception: extracting a new abstraction with only a single caller is
acceptable when it meaningfully breaks up a very long function, especially
one with deeply nested logic. Readability of the original site outweighs the
"no helper for a one-shot" default in this case.

## Code Comments

- Do not add obvious comments that restate the code in slightly different words
- Descriptive variable and function names often eliminate the need for comments.
- Prefer comments that explain that "why" not "what" when the code isn't
  self-explanatory
- Use comments to explain complex algorithms, non-obvious side effects,
  historical context

## Commit Guidelines

A logical commit represents a single, cohesive change that:

- Addresses one concern (one feature, one bugfix, or one refactoring)
- Passes all tests and quality checks (`./check.sh`)
- Can be understood and reverted independently
- Doesn't mix unrelated concerns

When in doubt: "Does this commit tell one coherent story?"
