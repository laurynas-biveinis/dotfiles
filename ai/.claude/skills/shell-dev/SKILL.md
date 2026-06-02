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
- Function names should use snake_case (e.g., `check_prerequisites`,
  `install_packages`)
- Shellcheck: The project's `./check.sh` script typically invokes shellcheck.
  If no `./check.sh` exists, use shellcheck liberally and proactively.
  If `./check.sh` exists but doesn't invoke shellcheck, ask the user to add it.
