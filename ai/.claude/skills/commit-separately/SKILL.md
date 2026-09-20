---
description: >-
  Isolate one change into its own commit. Use only for changes the
  subagent can execute without judgement or dialogue: extracting an
  unambiguous slice of current working-tree changes, or a
  fully-determined operation like a symbol rename. Name any untracked path
  the slice needs: the stash does not carry untracked files, so nothing else
  tells this skill they belong to it. Do NOT use for changes that
  need design choices or back-and-forth. Mechanism: stash the working tree,
  apply the change, commit, restore the stash.
context: fork
argument-hint: slice of changes (name untracked paths), or mechanical operation
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
1. Stash the current working tree changes with a plain `git stash` — not
   `-u`, which would take the untracked files the baseline relies on. Then
   write the output of `git status --porcelain -uall` to
   `/tmp/commit-separately-<topic>-baseline.txt`, outside the repository,
   where `<topic>` is a 1-3 word kebab-case slug derived from $ARGUMENTS:
   whatever that listing still reports survived the stash, so those paths
   predate this change.
1. Do the $ARGUMENTS change.
1. Run `./check.sh` if present, fix any errors, repeat as needed. Abort if
   fixing them requires design choices or back-and-forth discussion.
1. Stage exactly these, and nothing else: (a) every path
   `git status --porcelain -uall` now reports that the baseline file does not,
   including one a `./check.sh` fix landed on; and (b) every path $ARGUMENTS
   names as part of the slice, including one whose status record is unchanged
   because it was already untracked, or is a submodule the stash left dirty.
   Leave any other edit this invocation made in place and unstaged, and report
   it on return. If a path to stage cannot be passed to the staging hook as an
   argument, abort and report both that path and the stash entry still holding
   the user's work.
1. Draft the commit message.
1. Call `/commit` skill with the drafted commit message to commit. Note in the
   skill invocation that the commit message draft is only a suggestion and that
   the commit skill is responsible for verifying and crafting the final message.
1. Pop the stash.
1. Resolve any merge conflicts.
1. Return the commit message as returned by the commit skill, plus any path
   this invocation left deliberately unstaged, and why.
