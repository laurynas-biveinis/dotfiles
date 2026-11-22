"""Shared fixtures and zsh harnesses for the mysql-work tests."""

import subprocess
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[2] / "mysql-work/.zsh.d/rc/mysql-work.sh"

# Keep platform-dependent exports independent of the host's CPU and Homebrew.
SOURCE_STUBS = "CPUTYPE=arm64\ncommands[brew]=/stub-brew/bin/brew\n"


def run_zsh(commands, name, *arguments, cwd=None):
    """Run COMMANDS in `zsh -f` as NAME, with ARGUMENTS as $1 onward."""
    return subprocess.run(
        ["/bin/zsh", "-f", "-c", commands, name, *arguments],
        cwd=cwd,
        check=False,
        capture_output=True,
        text=True,
    )


def run_sourced_zsh(commands, name, *arguments, cwd=None):
    """Run COMMANDS in `zsh -f` after SOURCE_STUBS, as NAME.

    $1 is SCRIPT, for COMMANDS to source; ARGUMENTS follow it.
    """
    return run_zsh(SOURCE_STUBS + commands, name, str(SCRIPT), *arguments, cwd=cwd)
