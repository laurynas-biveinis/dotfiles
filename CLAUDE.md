# CLAUDE.md

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

## Code Style Guidelines

- Add `TODO` comments with attribution: `TODO(laurynas): description`
