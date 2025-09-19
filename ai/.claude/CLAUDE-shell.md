# CLAUDE-shell.md

This file provides guidance to you, Claude Code, when working with any shell
scripts owned by the user. This file will be imported by the global user memory
file.

## Shell Script Guidelines

- The default shell for any new scripts is Zsh. Use `#!/bin/zsh` shebang.
- Add `set -euo pipefail` to the top of any new scripts.
- Function names should use snake_case (e.g., `check_prerequisites`, `install_packages`)
- Shellcheck: The project's `./check.sh` script typically invokes shellcheck.
  If no `./check.sh` exists, use shellcheck liberally and proactively.
  If `./check.sh` exists but doesn't invoke shellcheck, ask the user to add it.
