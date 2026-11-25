# Aerospike Tools

Retired and unmaintained, kept for reference only.

## Known `usr/bin/as_branch` defects

- `BRANCH_ROOT` is set only for `rsync`, so `create`, `checkout` and `rm` abort
  with "parameter not set". `create` and `checkout` first leave an empty
  `$ASD_BRANCHES/<branch>` that blocks a retry. The defects below sit behind
  this one.
- `create` runs `git branch -u origin/<branch>`, which fails for a branch not
  yet on origin, after both worktrees exist.
- `checkout` creates a new EE branch at EE master's `HEAD` when the EE branch
  exists only on origin. It also adds the EE half before finding that CE lacks
  the branch, leaving a half-built pair.
- `rm` checks nothing before `rm -rf` and a repository-wide
  `git worktree prune`. Uncommitted changes and private submodule commits and
  stashes are lost, including those of other pairs whose directories are
  missing. If EE's `git branch -d` refuses, the CE branch is left behind.
- `rm` passes the branch name unquoted through `eval`.
- The branch argument is not validated. A missing one dies with "parameter not
  set", `worktree` with anything but `prune` runs the checkout flow, and
  `rm ""` would delete every pair once `BRANCH_ROOT` is set for it.
- Branch names containing `/` fail: `mkdir` lacks `-p`, and `rsync` keeps only
  the last path component on the VM.
- A branch named `+N` makes `pushd` rotate the directory stack, so `init +1`
  overwrites project files in the caller's directory.
- `push` and `pull` stop at the upstream-less EE half of a CE-only pair before
  reaching CE.
- On Linux, `init` does not link `ee/compile_commands.json`.
