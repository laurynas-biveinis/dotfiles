# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Build/Lint/Test Commands

- GNU Stow: Used for symlinking dotfiles (`stow <module-name>`)
- Shell scripts: Use shellcheck for linting (`shellcheck <script.sh>`)
- For Emacs packages:
  `emacs -batch -l package-lint.el -f package-lint-batch-and-exit <file.el>`
- C++: Use cpplint with project-specific configuration
  (`cpplint --filter=<filters> <file.cpp>`)
- Markdown: Lint with mdl (`mdl <file.md>`)
- YAML: Lint with yamllint (`yamllint <file.yml>`)

## Code Style Guidelines

- Shell scripts: Use zsh with shebang `#!/bin/zsh`, add `set -euo pipefail`
  for error handling
- Use consistent indentation (2 spaces for shell scripts, 4 for C++)
- Add TODO comments with attribution: `TODO(laurynas): description`
- Prefer descriptive variable and function names
- For Emacs Lisp: Use lexical binding, add proper package headers
- Include meaningful comments explaining purpose of code
- Use appropriate prefixing for private functions
  (`dotfiles--function-name`)
- Follow system-specific conventions in corresponding setup scripts
- Organize code with clear section markers

## Design Principles

- YAGNI (You Aren't Gonna Need It): Avoid adding functionality until it is
  necessary, preventing over-engineering and unnecessary complexity

## Repository Structure

- Modules are organized by tool/application (git, emacs, zsh, etc.)
- System-specific setup scripts follow pattern:
  `setup-[system]-[component].sh`
- Configuration is managed via GNU Stow from base and extra modules
- Custom scripts are stored in usr/bin directories
- Tool-specific configurations live in their respective directories
