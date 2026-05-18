---
description: Run ./check.sh (lint/test/autoformat) and fix one logical set of reported errors.
disable-model-invocation: true
allowed-tools:
  - Bash(./check.sh)
  - Bash(git status:*)
---

# Check

Check script output: !`./check.sh`

Read the output above:

- If errors are reported, fix one logical set of them and re-run
  `./check.sh` to confirm.
- If none are reported, say so.

Then run `git status` to surface any working-tree changes - from your
fixes or from autoformat the script ran.
