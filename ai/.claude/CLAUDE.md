# CLAUDE.md

This file provides guidance to you, Claude Code, when working with any
repositories owned by the user. There will be project-specific CLAUDE.md files
that will provide project-specific guidance.

## Domain Specific Memory Extensions

For code written in each of the following languages, include these files in
memory:

- Elisp (Emacs Lisp): @CLAUDE-elisp.md
- Shell scripts: @CLAUDE-shell.md

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

## Design

Always consider trade-offs and alternative options. If they exist, present them
to the user.

## Development methodology

For the testable projects, follow strictly the following principles: TDD, DRY,
YAGNI. For the mostly configuration-storing projects - such as dotfiles - full
TDD might not be possible.

### Test-Driven Development

With the smallest possible iteration steps:

1. Analyze, ultrathink, ask the user any clarifying questions to fully
   understand what needs to be done. Follow the YAGNI (You Ain't Gonna Need It)
   principle and do NOT design functionality not asked for, but feel free to
   note it and ask the user for feedback. Only then design. Strive for
   simplicity. For any non-trivially sized plan, write it to a scratch plan
   file.
1. Run `./check.sh` to ensure you are starting from a clean baseline.
1. Write user-facing documentation for the feature or for any behavior changes
   by a bugfix.
1. Run `./check.sh` to format/lint the documentation and to confirm the
   baseline.
1. Write ONE failing test for the smallest bit of functionality or a minimal bug
   testcase. Tests are NOT allowed to access internal APIs or internal state of
   the module being tested. If that precludes testing, ultrathink about making
   the design more testable. NEVER write multiple tests at once. Feel free to
   note any useful missing tests in the plan.
1. Run `./check.sh` to observe the expected test failure
1. Write the minimal code to make the test pass. Make sure to follow DRY (Don't
   Repeat Yourself) principle: extract common functionality into reusable
   functions or modules. If you find yourself writing similar code twice,
   refactor it. Do not add obvious comments.
1. Run `./check.sh` to confirm the test passes now.
1. Refactor the code if needed, one smallest possible refactoring at a time.
1. Run `./check.sh` to confirm the refactoring did not break the code.
1. Go back to the refactoring step until you no longer see any good
   refactorings.
1. If the code change is at a logical commit size, commit.
1. Repeat for the next iteration.

## Code Comments

- Do not add obvious comments that restate the code in slightly different words
- Descriptive variable and function names often eliminate the need for comments.
- Prefer comments that explain that "why" not "what" when the code isn't
  self-explanatory
- Use comments to explain complex algorithms, non-obvious side effects,
  historical context
