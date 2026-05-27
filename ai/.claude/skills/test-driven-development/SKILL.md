---
description: >-
  Red-green TDD iteration process for testable codebases. You MUST use this when
  starting a new feature, bugfix, or behavior change that can be exercised by
  tests. Drives one failing test, minimal pass, refactor cycle, validated by
  ./check.sh at every step.
user-invocable: false
---

# Test-Driven Development

With the smallest possible iteration steps, use red-green TDD as follows:

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
1. Run `./check.sh` to observe the expected test failure.
1. Write the minimal code to make the test pass. Make sure to follow DRY (Don't
   Repeat Yourself) principle: extract common functionality into reusable
   functions or modules. If you find yourself writing similar code twice,
   refactor it. Do not add obvious comments.
1. Run `./check.sh` to confirm the test passes now.
1. Refactor the code if needed, one smallest possible refactoring at a time.
1. Run `./check.sh` to confirm the refactoring did not break the code.
1. Go back to the refactoring step until you no longer see any good
   refactorings.
1. If the code change is at a logical commit size (see Commit Guidelines in
   CLAUDE.md), invoke the `review-changes` skill to do the code review.
   Present the findings file path and summary it returns to the user, and
   wait for the next steps. Do not invoke the review-changes skill for
   routine mechanical changes (comments, formatting, simple renames).
1. Repeat for the next iteration.
