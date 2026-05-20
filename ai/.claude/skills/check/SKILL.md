---
description: >-
  Run ./check.sh (lint/test/autoformat) and fix one category of reported
  errors.
disable-model-invocation: true
context: fork
allowed-tools: Bash(./check.sh) Bash(git status:*)
---

# Check

Check script output: !`./check.sh 2>&1; echo "check.sh exit code: $?"`

Read the output above:

- If errors are reported, pick exactly one category (e.g. all
  Markdown lint "line too long" hits, or all shellcheck SC2086 hits - never a
  mix). Drive that category to clean: iterate fix -> `./check.sh` -> fix as
  many rounds as needed. If a pass doesn't reduce the count, try a different
  approach next pass - not the same approach again. Stop when the category
  is clean, or when you've run out of substantively different approaches to
  try (report the remaining hits and stop). Never start on a different
  category in this invocation, even if other categories still report errors.
- If no errors are reported, say so.

Then run `git status` to surface any working-tree changes - from your
fixes or from autoformat the script ran. Draft a commit message to describe the
changes and return it.
