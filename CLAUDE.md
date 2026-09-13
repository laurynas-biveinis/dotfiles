# Repository Instructions

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

Use `./check.sh` script to lint and check for common errors.

## Repository Structure

- Modules are organized by tool/application (Git, Emacs, Zsh, etc.)
- System-specific setup scripts follow pattern:
  `setup-[system]-[component].sh`
- Configuration is managed via GNU Stow from base and extra modules
- Custom scripts are stored in usr/bin directories
- Tool-specific configurations live in their respective directories

## Shell Scripts

See the `shell-dev` skill for the portable rules; these are how they land here.

- Sourced rather than executed: `elisp-env.sh`, `zsh/.zshenv`,
  `bash/.noninteractive_init.bash`, `*/.bash.d/{rc,noninteractive_init}/*`,
  `*/.zsh.d/{env,rc}/*`, `zsh/.zsh.d/paths`, and `zsh/.p10k.zsh`. Two are
  symlinked across `.bash.d` and `.zsh.d`, so those must stay POSIX. The
  `#!/bin/zsh` ones are `zsh/.zshenv`, `zsh/.zsh.d/paths`,
  `wakatime/.zsh.d/rc/wakatime.zsh`, and `mysql-work/.zsh.d/rc/mysql-work.sh`;
  of those only `mysql-work.sh` needs a `FILTER_REGEX_EXCLUDE` entry, and the
  reason is that `shfmt` would reformat it (4-space indentation to tabs), not
  its name — `shfmt` takes the dialect from the shebang whatever the file is
  called, and parses its Zsh-only expansions fine. `zsh/.p10k.zsh` is wizard
  output, stays 644, and is excluded for the same formatting reason.
  `wakatime/.bash.d/rc/wakatime.bash` carries no shebang because its `.bash`
  extension already fixes the dialect. See the `# Workaround P10K going crazy`
  lines in `mysql-work/.zsh.d/rc/mysql-work.sh` for what setting `-e` in
  interactive configuration costs.

## Code Style Guidelines

- Add `TODO` comments with attribution: `TODO(laurynas): description`
