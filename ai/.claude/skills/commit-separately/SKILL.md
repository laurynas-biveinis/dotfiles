---
description: >-
  Isolate one change into its own commit. Use only for changes the
  subagent can execute without judgement or dialogue: extracting an
  unambiguous slice of current working-tree changes, or a
  fully-determined operation like a symbol rename. Do NOT use for changes that
  need design choices or back-and-forth. Mechanism: stash the working tree,
  apply the change, commit, restore the stash.
context: fork
argument-hint: slice of current changes, or mechanical operation (no design)
allowed-tools: >-
  Bash(./check.sh)
  Bash(git stash:*)
  Bash(git status:*)
  Bash(git diff:*)
  Bash(git add:*)
  Bash(git rm:*)
  Bash(git apply:*)
  Bash(git show:*)
  Read
  Edit
  Write
  Skill(commit)
---

# Commit Separately

1. Abort if $ARGUMENTS requires design choices or back-and-forth discussion.
1. Abort if the current working tree has both staged and unstaged changes,
   because stashing and unstashing would lose this state.
1. Stash the current working tree changes.
1. Do the $ARGUMENTS change.
1. Run `./check.sh` if present, fix any errors, repeat as needed. Abort if
   fixing them requires design choices or back-and-forth discussion.
1. Stage all the changes.
1. Draft the commit message.
1. Call `/commit` skill with the drafted commit message to commit. Note in the
   skill invocation that the commit message draft is only a suggestion and that
   the commit skill is responsible for verifying and crafting the final message.
1. Pop the stash.
1. Resolve any merge conflicts.
1. Return the commit message as returned by the commit skill.
