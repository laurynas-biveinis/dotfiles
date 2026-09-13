---
description: >-
  Shell script development guidelines (bash, zsh, sh). Apply whenever writing,
  editing, reviewing, or discussing shell scripts (.sh, .zsh, .bash files).
user-invocable: false
---

# Shell Script Guidelines

The shebang and `set`-line conventions below govern new scripts and
files you are already changing for another reason; existing files are not
made non-conforming by them. The rules about how tools behave apply to
every file.

- Standalone scripts you execute default to `#!/bin/bash` with
  `set -euo pipefail`, for the feature set (arrays, `[[ ]]`, `BASH_SOURCE`,
  `shopt`) at no cost in lint coverage. A non-interactive Bash script reads no
  startup file aside from a caller-set `$BASH_ENV`, which it sources before the
  first line, so its `PATH` is the caller's, not the login shell's. On macOS
  the login shell is typically what adds Homebrew's `bin` (`/opt/homebrew/bin`
  on Apple silicon; on Intel `/usr/local/bin` is already in `/etc/paths`) and
  may also prepend GNU shims such as `…/coreutils/libexec/gnubin` (shadowing
  `date`, `readlink`) and `…/gnu-sed/libexec/gnubin` (shadowing `sed`) — so a
  command can be missing, or silently a different implementation, when the
  script runs from cron, a hook, or an editor. Depend only on what the default
  `PATH` provides, or set `PATH` explicitly at the top of the script. When
  every caller of a repo-internal script is known and supplies the environment,
  stating the requirement in the script's header beats duplicating the login
  shell's `PATH` construction.
- Establish the target interpreter's version before using Bash 4+ features.
  macOS ships Bash 3.2.57 as `/bin/bash` and nothing newer unless Homebrew
  installed one, so a `#!/bin/bash` script that must run on a stock Mac is
  3.2-only: no `mapfile`/`readarray`, `declare -A`, `${v^^}`, or
  `shopt -s globstar`, and before 4.4 `"${arr[@]}"` under `set -u` aborts as
  an unbound variable, so test `[ ${#arr[@]} -gt 0 ]` before expanding.
  `#!/usr/bin/env bash` picks up a newer Bash when one is on `PATH` — which
  one it finds depends on the caller's `PATH`, per the previous bullet — while
  `#!/bin/bash` pins the system one. Read the `man bash` that matches the
  interpreter you targeted; on macOS the local manual is 3.2's. `bash -n`
  splits the 4.x forms three ways. New operators (`|&`, `;&`, `;;&`, `&>>`) it
  rejects outright. A new reserved word used with its compound syntax it also
  rejects, but misleadingly: `coproc mytask { sleep 1; }` fails under 3.2 as
  `syntax error near unexpected token '}'`, because `coproc` is read as an
  ordinary command and its `{` as a plain argument, so the `}` closes a group
  that was never opened — a diagnostic naming neither `coproc` nor the version.
  New built-ins, options and expansions (`mapfile`, `declare -A`, `${v^^}`,
  `shopt -s globstar`) it accepts silently. Neither `shellcheck` nor `shfmt`
  has any bash-version check either, so that third case surfaces only when the
  script runs — and never on a Linux CI runner, which is Bash 5.
- `-n` checks one file per invocation, in every shell: `bash -n f1 f2`,
  `sh -n f1 f2`, and `zsh -n f1 f2` all parse `f1` only and hand `f2`… to it as
  positional parameters. The ignored files are arguments, not inputs, so
  nothing is reported about them and the exit status is `f1`'s parse alone — a
  broken second file yields no output and status 0. Loop over the files. Only
  `-n` has this shape: `shellcheck` and `shfmt` both take a file list, so
  writing the `-n` call in the same shape as its neighbours is how this bug
  gets written. The exit status is not a complete signal either: on macOS's
  Bash 3.2.57 a nested `(` inside an array assignment — the shape a Zsh glob
  qualifier such as `files=(*.el(N))` keeps when a Zsh body is re-shebanged to
  Bash — is printed as a syntax error on stderr while `-n` still exits 0,
  because Bash keeps parsing and the status reports only the final outcome.
  Gate a checker on non-empty `-n` output as well as on non-zero status;
  accepted files print nothing. That still does not make `-n` sufficient — an
  unterminated here-document is accepted silently, rc 0 and no output — so
  treat `-n` as the cheap first gate and `shellcheck`/`shfmt` as the backstops
  that reject both shapes.
- Zsh gets the weakest tooling of the three: shellcheck refuses it outright
  (SC1071), so a Zsh script gets no lint check, and `shfmt` covers it only as
  far as the Zsh support in its version reaches. The only syntax check
  available for Zsh is `zsh -n`, per the invocation rule above — available, not
  automatic: wire it into the local checker or nothing runs it. Use
  `#!/bin/zsh` (also with `set -euo pipefail`) only for scripts that need Zsh
  features or the `zshenv` environment — a script is neither interactive nor a
  login shell, so Zsh reads `/etc/zshenv` and `~/.zshenv` and nothing else,
  never `.zprofile`, `.zshrc`, or `.zlogin`. Prefer Bash otherwise.
- Scripts meant as copy-and-paste references, rather than to run, use
  `#!/bin/sh` with `set -eu`, unless they are Zsh-specific.
- Files that are sourced rather than executed never run their shebang; it only
  tells shellcheck and `shfmt` which dialect to parse. Pick the narrowest
  dialect the body conforms to, not the shell that sources the file:
  `#!/bin/sh` for a POSIX body, which is the strictest check and the only
  correct choice when both Bash and Zsh source the same file; `#!/bin/bash`
  when the body needs Bash features; `#!/bin/zsh` only when the body needs Zsh
  syntax — that last one turns shellcheck off entirely, so sitting among Zsh
  configuration is not by itself a reason to reach for it. Each tool resolves
  the dialect the same way — shebang first, then the filename — and they differ
  only when neither supplies one: ShellCheck fails the file with SC2148 at
  error severity, while `shfmt` quietly falls back to Bash, losing the POSIX
  check. ShellCheck infers a dialect from `.bash` but never from `.sh`;
  `shfmt` 3.13.0+ infers Zsh from `.zsh`, and ShellCheck infers nothing from
  it. `# shellcheck shell=sh` supplies the dialect to the first tool only, so
  keep the shebang: it outranks the filename in both. These files get no `set`
  line of their own, because shell options belong to the sourcing shell — and
  for interactive-shell configuration that is the user's own shell, where `-e`
  in particular can break the prompt and other interactive machinery. That
  applies to `shopt` as much as to `set`: scope an option you must enable, or
  pick a formulation that needs none. A file that is both sourced and executed
  is the exception: it keeps its shebang, which selects the real interpreter on
  the execute path, and keeps its `set` line, which it would otherwise lose
  there. The constraint moves to the sourcing shell — its options must be a
  superset of the script's, and the body must be correct under its dialect as
  well as the shebang's — and the script's top-level names, `readonly` marks,
  function definitions and working directory persist into it.
- Files in a Zsh `fpath` autoload directory are function bodies, not scripts:
  no shebang, and no executable bit. Leaving the shebang off is also what keeps
  linters from picking them up as scripts and applying script rules to them; an
  Emacs `# -*- mode: sh; sh-shell: zsh; -*-` line gives editors the dialect
  without doing that. Do not add a shebang.
- If CI lints shell files, it may require the executable bit on every file it
  recognizes as a shell script, including ones that are never run —
  sourced-only helpers and copy-and-paste references alike. Honour that, and
  check whether the local checker asserts it too; when it does not, the failure
  shows up only in CI.
- A checker's file set is a real trade-off: derived from a glob or
  `git ls-files` it grows on its own, but it can pull files in silently, and
  where it must agree with a CI denylist it ends up mirroring one; named
  explicitly it is legible in one place, but it goes stale silently unless the
  checker asserts every entry still resolves — `git ls-files --error-unmatch`
  does that in one flag. Naming does not by itself decouple the list from a CI
  denylist either: a list whose job is to predict what CI sees after its
  exclusions is a hand-kept mirror, and the worse kind, because no tool
  compares the copies. Either way, record what the checker does not reach and
  what backstops it.
- Function names should use snake_case (e.g., `check_prerequisites`,
  `install_packages`)
- Shellcheck: The project's `./check.sh` script typically invokes shellcheck.
  If no `./check.sh` exists, use shellcheck liberally and proactively.
  If `./check.sh` exists but doesn't invoke shellcheck, ask the user to add it.
  Run it with `-x` so it follows sourced files; when the sourced path is
  computed at runtime, put `# shellcheck source=lib.sh source-path=SCRIPTDIR`
  immediately above the `source` line — the bare filename resolves against the
  script's own directory because of `source-path=SCRIPTDIR`. Without it the
  path resolves against the current directory, so the check silently depends on
  where it is run from, and reports SC1091 plus spurious SC2154 for the
  lowercase and mixed-case names the sourced file defines — ShellCheck presumes
  an all-caps name is external and never flags it. When the sourced path cannot
  be pointed at any file `-x` can read — a `~`-prefixed path, or one that only
  exists inside a virtualenv or toolchain install absent from the checker's
  environment — use `# shellcheck disable=SC1091` for a constant absolute path
  or `disable=SC1090` for a non-constant one instead.
