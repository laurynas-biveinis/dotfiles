# Repository state

These checks detect changes at review boundaries; they do not lock the
repository or prove that it stayed unchanged between observations. A change
made and undone entirely between checks can escape detection. State this
limit in the report. Guard the whole checkout because supporting reads and
placement can depend on paths and refs outside the selected diff.

## Capture the basis

Before Scope's first read, record the repository root, scope working
directory, and resolved Git directory. Capture each observation below with
its command, exit status, and complete stdout; retain stderr on failures.
Run root-wide observations from the repository root, without scope filters:

- `git rev-parse --verify --quiet HEAD`, plus
  `git symbolic-ref --quiet --no-recurse HEAD`. Capture the symbolic target
  on exit 0; exit 1 means detached only if HEAD resolution succeeds. Other
  statuses stop the review. This distinguishes branch switches under both
  files and reftable storage, whose physical HEAD file can be a fixed marker.
  Record an unborn HEAD explicitly only when a symbolic HEAD names a missing
  branch ref; other revision-resolution failures stop the review.
- `git rev-parse --symbolic-full-name --all` and `git rev-parse --all`:
  ref names and their object IDs. These guard local and remote-tracking
  refs used by scope selection and the unpublished stack.
- Presence and complete content of the files at
  `git rev-parse --git-path shallow` and `git rev-parse --git-path info/grafts`.
  Resolve these paths as for `MERGE_HEAD` below; record absence explicitly
  and stop on read errors. Both files can change ancestry and blame without
  changing HEAD or refs.
- For each existing `main`/`master` branch, its upstream's symbolic name
  (`git rev-parse --symbolic-full-name <trunk>@{upstream}`) and full SHA
  (`git rev-parse --verify --quiet <trunk>@{upstream}`). Record no upstream
  explicitly when none is configured; do not turn other errors into absence.
- `git ls-files --stage`: the full index, including paths, modes, stages,
  and blob IDs. A scope patch alone does not identify an index baseline.
- `git ls-files --cached -v -z`: index flags for every tracked path. A
  lowercase status denotes `assume-unchanged`; `S` or `s` denotes
  `skip-worktree`. These flags can hide persistent edits from both diff and
  status. If either is present before dispatch, take the pre-dispatch stop
  below, naming the paths and flags that prevent reliable capture. Do not
  clear the flags during review. Later flag changes use the normal drift stop.
- `git diff --patch --binary --full-index`, with
  `--no-color --no-ext-diff --no-textconv --no-relative`,
  `--submodule=short --ignore-submodules=none`, and a final `--`:
  all tracked working-tree changes relative to the index. Keep the binary
  patches: a `--raw` working-tree post-image can have an all-zero object ID.
- `git status --porcelain -uall`, plus the presence and content of the file
  at `git rev-parse --git-path MERGE_HEAD`. Resolve Git paths against the
  working directory; use Read/Glob as Scope does. This guards exclusions
  and a merge starting without changing the index.

Enumerate stage-0 gitlinks (index mode `160000`) in each captured repository.
Record each path, gitlink object ID, and whether it has its own checked-out
Git repository. For every populated submodule, record its root and Git
directory and recursively capture the same observations from that root.
Record an unpopulated path explicitly; do not initialize it. A Git command
run in an empty submodule directory can find the parent repository, so that
alone does not establish a populated submodule. The parent's dirty marker
cannot distinguish successive edits inside an already-dirty submodule.

After Scope succeeds, derive a fresh topic and write
`/tmp/inspect-changes-<topic>-state.md`. Save the observations in separate
files under that prefix and link them from the state file. Record:

- The original scope request (or default precedence), merge intent, and
  verbatim caller requirements, so selection can be repeated on resume.
- The normalized selected command, its retained exit status and complete
  stdout, resolved revision operands, and the derived pre-image baseline.
- The observation commands, working directories, statuses, and saved-output
  paths. Preserve empty output and distinguish it from a missing capture.
- Dispatch and acceptance checkpoints as they occur; draft-file existence
  alone cannot show whether the first dispatch happened.
- Phase 3's placement mode when computed: committed, WIP (unborn HEAD), or
  omitted. For committed placement, also record trunk, stack query and
  marked stack, pinned `REV`, and `BASE` if present. WIP has no such commit
  context.

The initial observations and selected probe are immutable. Append later
checks and their separate output paths; never replace the basis with current
content. Write complete command output directly to scratch files rather than
reconstructing it from truncated tool output. No project writes or object
creation (`git write-tree`, `git hash-object -w`) are needed.

Compare saved outputs using the already-allowed
`git diff --no-index --quiet --no-ext-diff --no-textconv -- <saved> <fresh>`:
exit 0 means equal, 1 means different, and any other status is a failed check.
This compares scratch evidence only; `--no-index` remains invalid as a review
scope. Never infer equality from a summary or a truncated read.

## Check boundaries

At each boundary required by the entrypoint, re-run every guard observation
and the selected normalized scope command in their recorded directories.
Re-enumerate submodules recursively and compare their membership, populated
state, roots, Git directories, and observations too; a change to any of these
is drift even when the parent's dirty marker is unchanged.
Compare statuses and complete output with the basis, including HEAD and the
retained scope stdout. Re-run the guards after the scope command as well, so
a change during the probe cannot silently pair two different states.
Expected absence sentinels must still hold; unexpected failure is not equality.
Before the first dispatch this also validates that Scope and its baseline
were derived against the initial observations.

On drift or a failed check, stop dispatching and accepting replies. Do not
consume pending results as verdicts, proposals, rejections, or fallback
analyses; preserve them separately as unaccepted evidence. This stop takes
precedence over retry exhaustion and every normal continuation path.

- Before any dispatch: return `Status: not-run`, `Report: none`, naming
  the changed components or failed command in `Reason`. Retain any basis
  files already written; the earlier Scope stops still write none.
- After dispatch began: append the changed components or failed command and
  the last accepted checkpoint to `limitations.md`. Assemble an `incomplete`
  report from evidence accepted before detection, retaining the original
  scope and baseline. List pending/unverified work in Summary; absent
  verdicts or drafts mean unfinished work, never no findings. Do not run
  more checks or sub-steps as part of this stop's assembly. If detected at
  the final return check, issue the incomplete report instead of returning
  the assembled converged result.

Once stopped, this topic cannot become converged even if the repository
returns to its former state. A new review uses a fresh topic and no prior
findings as input. Remote-tracking refs describe only local knowledge of
publication; these checks do not fetch or detect a push from another clone
until a local ref changes, and preserve Phase 3's publication caveat.

## Resume

A resume is any continuation that reconstructs a review from its scratch
files after an interruption or loss of in-memory orchestration state. Before
using its drafts, verdicts, analyses, or cached placement, read its state and
limitations files. Re-run Scope from the saved original request, including
precedence, conflict/merge guards, exclusions, and baseline derivation, with
fresh guard observations. Do not merely re-run the old selected command.

Continue under the existing topic only if the newly selected normalized
command, resolved operands, baseline, requirements, and all observations
match the saved basis and no drift stop was recorded. If placement was
previously computed, recompute Phase 3's context and compare it too before
reusing any analysis. Append the successful resume checkpoint, then recover
the round and pending work by the entrypoint's normal rules. Existing
completion limitations still preclude convergence.

If the basis differs, is missing, or records a drift stop, preserve the old
files for audit and start a fresh review from that original request under a
new topic, with no old finding corpus or cached context. Explain what changed.
Do not end the resumed request as incomplete merely because the old basis
changed. If fresh Scope instead stops (including an empty explicit staged
scope after a commit), return its `not-run` result; never fall through to
re-draft convergence or silently substitute a different explicit scope.
