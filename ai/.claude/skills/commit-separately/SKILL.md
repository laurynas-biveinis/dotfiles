---
description: >-
  Isolate one change into its own commit. Use only for changes the
  subagent can execute without judgement or dialogue: extracting an
  unambiguous slice of current working-tree changes, or a
  fully-determined operation like a symbol rename. Name any untracked path
  the slice needs: the stash does not carry untracked files, so nothing else
  tells this skill they belong to it. Do NOT use for changes that
  need design choices or back-and-forth. Mechanism: stash the working tree,
  apply the change, commit, restore the stash. Requires exclusive use of the
  checkout and the repository stash list, with tracked-file buffers saved.
context: fork
argument-hint: slice of changes (name untracked paths), or mechanical operation
allowed-tools: >-
  Bash(./check.sh)
  Bash(git stash push:*)
  Bash(git stash list:*)
  Bash(git stash show:*)
  Bash(git stash pop:*)
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

The caller must arrange exclusive use of this checkout and the repository's
stash list until the fork returns, including stash operations in other
worktrees. Save tracked-file buffers before invocation; the caller must keep
unrelated checkout readers and writers idle. Abort before stashing unless the
caller has established these preconditions.

Every abort uses the failure return below. If a step cannot be completed,
abort; keep partial edits and recovery data in place.

1. Abort if $ARGUMENTS requires design choices or back-and-forth discussion.
1. Abort if the index already contains staged changes. Restoring that index
   after extracting a staged slice can fail on the newly committed content.
1. Choose a unique invocation marker and record
   `git stash list --format='%H %gd %gs'`. Stash the current working tree
   changes with `git stash push -m "commit-separately: <marker>"` — not
   `-u`, which would take the untracked files the baseline relies on. Read
   the same stash listing again: record the new marked entry's full object
   ID, or record "nothing stashed" if the push succeeded and the list is
   unchanged. A clean tree is a valid input. If the push fails or the new
   entry cannot be identified unambiguously, stop using the failure return
   below. Then write the output of `git status --porcelain -uall` to
   `/tmp/commit-separately-<topic>-baseline.txt`, outside the repository,
   where `<topic>` is a 1-3 word kebab-case slug derived from $ARGUMENTS:
   whatever that listing still reports survived the stash, so those paths
   predate this change.
1. For an extraction, read tracked changes from
   `git stash show -p <recorded-object-id>` when an entry was created; read
   caller-named untracked or dirty-submodule content from the checkout. Apply
   only the requested slice with `Edit`/`Write` or working-tree-only `git apply`,
   never `--cached` or `--index`. For a mechanical operation, make the requested
   change directly.
1. Run `./check.sh` if present, fix any errors, repeat as needed. Abort if
   fixing them requires design choices or back-and-forth discussion.
1. Stage exactly these, and nothing else: (a) every path
   `git status --porcelain -uall` now reports that the baseline file does not,
   including one a `./check.sh` fix landed on; and (b) every path $ARGUMENTS
   names as part of the slice, including one whose status record is unchanged
   because it was already untracked, or is a submodule the stash left dirty.
   Leave any other edit this invocation made in place and unstaged, and report
   it on return. If a path to stage cannot be passed to the staging hook as an
   argument, abort through the failure return and report that path. Enumerate
   the files:
   - Pass each path as its own `git add`/`git rm` argument, never `-A`, `.`, a
     glob, or a directory, except a caller-named submodule path already tracked
     as a gitlink. An untracked nested Git repository reports as a single
     directory entry even under `-uall`, and staging it records a gitlink rather
     than the files.
   - Run each staging command as its own Bash call, with no `cd` prefix and no
     `&&`, `||`, `;`, `|` or redirects: the staging hook denies compound
     commands before it validates anything else.
1. Draft the commit message.
1. Call `/commit` skill with the drafted commit message to commit. Note in the
   skill invocation that the commit message draft is only a suggestion and that
   the commit skill is responsible for verifying and crafting the final message.
   Require a resulting commit hash and final message; on a non-commit return,
   abort through the failure return before attempting restoration.
1. If nothing was stashed, skip restoration and continue to the success
   return. Otherwise, run `git stash pop 'stash@{0}'`; the exclusive-use
   precondition keeps this invocation's stash on top.
1. On any pop failure, including conflicts, use the failure return; do not
   resolve or stage the conflicts. A failed pop can leave its entry in the
   stash list; retain it as recovery data and report it.
1. On success, return the commit hash and final message from the commit skill,
   whether restoration succeeded or was skipped because nothing was stashed,
   and any path this invocation left deliberately unstaged, with its reason.

**Failure return:** stop without further mutations. Report the reason, the
commit hash and message if one was made (otherwise say no commit was made),
and the current working-tree/index state from `git status --porcelain -uall`,
including staged and unmerged paths. Report the invocation's stash marker,
object ID and current selector when known. Distinguish stashing not attempted,
nothing stashed, and an entry missing or uncertain; never identify another
entry as this invocation's. Retain any surviving invocation stash as recovery
data. A commit alone does not mean the procedure succeeded.
