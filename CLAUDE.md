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

- `check.sh`'s list definitions carry their own comments saying what each one
  covers and why it is named or derived; read them before adding a script.
- Backstopped by CI only: `mysql-work/README.md` is the one tracked Markdown
  file CI's markdownlint lints and `./check.sh` does not. It runs the other
  way for `zsh -n`, which CI has no counterpart for.
- Format-checked nowhere: the three `FILTER_REGEX_EXCLUDE`'d shell paths and
  the `*/.zsh.d/functions/*` autoload bodies.
  All except `zsh/.p10k.zsh` get a syntax check from
  `./check.sh`; no checker sees `zsh/.p10k.zsh`.
- Super-linter (`.github/workflows/linter.yml`) lints by denylist, so it picks
  up new files on its own. Zsh never reaches shellcheck there: current
  super-linter routes Zsh files to `bash-exec` and `shfmt` only, so the
  `--exclude=SC1071` in `SHELLCHECK_OPTS` fires on nothing. It stays because
  `slim@latest` floats and pre-routing super-linter did fail this repository
  with SC1071 (`c7e353089`). A file `shfmt` would reformat, or cannot parse,
  needs a `FILTER_REGEX_EXCLUDE` entry. Both `check.sh` and super-linter run
  shellcheck with `-x`, and both format-check Zsh scripts with `shfmt`.
- Any shell file super-linter sees — detected by `.sh`/`.bash`/`.zsh` extension
  or shell shebang, and not matched by `FILTER_REGEX_EXCLUDE` — must be mode
  755, including files that are never executed. Its `bash-exec` check fails
  them otherwise. That is also why `zsh/.p10k.zsh` stays 644 despite its `.zsh`
  extension: an excluded path is dropped from super-linter's file list before
  any validator runs.
- The default-to-Bash rule freezes the `#!/bin/zsh` set, but migrating out of
  Zsh is a per-file decision, not one target for all of `ZSH_ROOT_FILES`: only
  `setup-macos-git.sh` and `setup-macos-ai.sh` are meant to be run, the rest
  being copy-and-paste sources. That split is recorded intent rather than
  something the files show — `setup-macos.sh` self-declares;
  `setup-macos-emacs.sh`, `setup-macos-mysql-work.sh` and
  `setup-macos-python.sh` carry a manual step; and `setup-macos-cpp.sh`,
  `setup-macos-gh.sh` and `setup-macos-rust.sh` are on the copy-and-paste side
  by intent alone. `shell-dev`'s per-category shebang
  rules then decide each, with `setup-ubuntu.sh` as the in-repo template for
  the copy-and-paste shape: it carries the `#!/bin/sh`, the `set -eu`, and the
  same "consult and copy and paste" header comment `setup-macos.sh` has. Four
  local caveats:
  - the shebang alone buys nothing here, since the file must also move from
    `ZSH_ROOT_FILES` into `SHELL_FILES` for `bash -n` and `shellcheck -x`
    (super-linter re-routes on its own);
  - anything actually executed loses the `~/.zshenv` `PATH` that supplies
    `brew`, the `asdf` shims and `~/usr/bin`;
  - `setup-macos-mysql-work.sh` carries dialect-independent shellcheck findings
    that the Zsh shebang hides today;
  - the scripts that `source` a virtualenv activate script —
    `setup-macos-python.sh`, `setup-macos-cpp.sh`, `setup-macos-mysql-work.sh` —
    hit two independent shellcheck walls the Zsh shebang also hides. `#!/bin/sh`
    makes `source` itself an error (`SC3046`, `warning`; neither `check.sh` nor
    super-linter sets an `-S` floor), so the body needs `.` first — `a3c81b160`
    split this line out of `#!/bin/zsh` `setup-macos.sh` into a new `#!/bin/sh`
    `setup-macos-cmake.sh` (today `setup-macos-cpp.sh`), and `6d2b8e63e`
    reverted the shebang the same day rather than converting `source`.
    `#!/bin/bash` keeps `source` verbatim and is the cheaper per-file answer.
    Neither shebang clears the second wall: `-x` follows the sourced path, so
    where it is unreadable — every CI runner for the `/opt/virtualenvs` two, and
    everywhere for the `~`-prefixed one — the file needs a `# shellcheck`
    directive: `source=` to point `-x` at a file, as `nightly/usr/bin/nightly`
    does with `source=/dev/null`, or a `disable=` — `SC1091` for a constant
    absolute path, as `rust/.zsh.d/env/rust.sh` carries, `SC1090` for a
    non-constant one.
- Sourced rather than executed: `elisp-env.sh`, `zsh/.zshenv`,
  `*/.zsh.d/{env,rc}/*`, `zsh/.zsh.d/paths`, and `zsh/.p10k.zsh`.
  The Git and Emacs hooks have POSIX bodies and `#!/bin/sh` shebangs. The
  `#!/bin/zsh` ones are `zsh/.zshenv`, `zsh/.zsh.d/paths`,
  `wakatime/.zsh.d/rc/wakatime.zsh`, and `mysql-work/.zsh.d/rc/mysql-work.sh`;
  of those only `mysql-work.sh` needs a `FILTER_REGEX_EXCLUDE` entry, and the
  reason is that `shfmt` would reformat it (4-space indentation to tabs), not
  its name — `shfmt` takes the dialect from the shebang whatever the file is
  called, and parses its Zsh-only expansions fine. `zsh/.p10k.zsh` is wizard
  output, stays 644, and is excluded for the same formatting reason.
  See the `# Workaround P10K going crazy`
  lines in `mysql-work/.zsh.d/rc/mysql-work.sh` for what setting `-e` in
  interactive configuration costs.
- Sourced **and** executed: `nightly/usr/bin/nightly` sources, rather than
  runs, the seven `*/usr/bin/` scripts symlinked from `*/.nightly/`, while
  `~/usr/bin` on `PATH` means they are also invoked directly. On the sourced
  path their shebangs are inert, so the three `#!/bin/sh` bodies execute under
  Zsh; the runner's `-euo pipefail` is a floor their own `set` lines can add to
  but cannot clear — those three are `set -eu`, so each gains `pipefail` it
  does not set, while the four `#!/bin/zsh` bodies already set `-euo pipefail`
  themselves; and their top-level names, `readonly` marks, function definitions
  and working directory persist into the runner for the rest of the loop.
  Command prefixes such as `IFS=" " read -r -A x <file` in
  `scripts/usr/bin/dotfilesupdate` do not leak.
- `zsh/.zshrc` is sourced too. Its Zsh shebang makes it visible to
  super-linter, so `FILTER_REGEX_EXCLUDE` excludes it to preserve its
  formatting. `./check.sh`
  syntax-checks it with `zsh -n`, without imposing mode or formatting
  requirements.
- `*/.zsh.d/functions/*` are Zsh autoload bodies, not scripts: `zsh/.zshenv`
  does `fpath+=~/.zsh.d/functions` and consumers `autoload` them by name. No
  shebang, mode 644, an Emacs `# -*- mode: sh; sh-shell: zsh; -*-` line
  instead — which is what keeps them out of super-linter's view, and so out of
  the mode-755 rule above. `./check.sh` includes them by directory glob in
  `ZSH_FILES` for `zsh -n`; `ZSH_SCRIPT_FILES` keeps the mode check separate.

## Code Style Guidelines

- Add `TODO` comments with attribution: `TODO(laurynas): description`
