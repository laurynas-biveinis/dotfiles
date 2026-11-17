"""Shared fixtures for the tests that source the mysql-work shell configuration."""

from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[2] / "mysql-work/.zsh.d/rc/mysql-work.sh"

# Sourcing SCRIPT runs `brew --prefix` on macOS and `arch` on every platform.
SOURCE_STUBS = "brew() { print -r -- /stub-brew; }\narch() { print arm64; }\n"
