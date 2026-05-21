---
description: >-
  Identify and list any logically complete and self-contained concerns in the
  current working tree changes that would be perfect candidates for separate
  commits.
model: haiku
context: fork
agent: general-purpose
allowed-tools: Bash(git diff:*) Read
---

# Identify Diff Splits

Identify the independent concerns in the current working tree. Name
and list them.

## Method

1. Read the working tree changes.
2. Identify the distinct concerns. Most working trees have one; some
   have a few genuinely independent ones. Concerns may be mixed
   within a single hunk — chunk boundaries do not delimit concerns.
3. Return one named entry per concern. Output only the final list.
   If you can write "absorbs into concern X", "part of concern X",
   "incidental", or "without independent value" next to an entry,
   that entry is absorbed — remove it. The skill names concerns; it
   does not classify every line.

## What is one concern

A concern is a single motivation — one feature, fix, or refactor.
It absorbs everything that has no standalone value of its own:

- The change itself, its inverse, cleanup, and state handlers it
  introduces.
- Helpers, validation, frameworks, and test infrastructure that
  exist solely to enable it.
- Wiring needed only to expose the change.
- Deprecation of an API replaced by another change in this working
  tree, and migration of in-repo callers — never a separate entry,
  the deprecation references the replacement.
- Tests for behavior introduced by this concern — never a separate
  entry. Tests backfilling coverage of pre-existing code stand
  alone.
- Documentation (NEWS, README, docstrings) of the concern's changes
  — never a separate entry.
- Pure formatting side effects (autoformatter output, blank-line
  shuffling) in files touched for the concern — never a separate
  entry, even with an "absorbs into X" note.

## Standalone-value test

When unsure whether some change is part of an existing concern or
stands on its own, ask: **"Does this change have standalone value —
would it be worth doing on its own, even if the surrounding work
weren't here?"**

- If no, it is part of the concern that motivated the surrounding
  work.
- If yes, it is its own concern.

This is a test of intrinsic value, not development context.
"Developed during the same session" or "triggered by the same
feature work" does not make a change dependent on the feature.
Drive-by bugfixes, validation tightening, and protocol additions
that have value on their own are their own concerns.
