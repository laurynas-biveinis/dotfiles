---
description: >-
  Shell script development guidelines (bash, zsh, sh). Apply whenever writing,
  editing, reviewing, or discussing shell scripts (.sh, .zsh, .bash files).
user-invocable: false
---

# Shell Script Guidelines

- The default shell for any new executable scripts is Zsh. Use `#!/bin/zsh`
  shebang. However, there are some scripts that are not directly executable but
  meant as a source for copying and pasting some commands. These should use
  `#!/bin/sh`, unless they are Zsh-specific.
- Add `set -euo pipefail` to the top of any new Zsh scripts, and `set -eu` to
  the top of any new `/bin/sh` scripts.
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
- Function names should use snake_case (e.g., `check_prerequisites`,
  `install_packages`)
- Shellcheck: The project's `./check.sh` script typically invokes shellcheck.
  If no `./check.sh` exists, use shellcheck liberally and proactively.
  If `./check.sh` exists but doesn't invoke shellcheck, ask the user to add it.
