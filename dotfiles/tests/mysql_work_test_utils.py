"""Shared fixtures and the zsh harness for the tests that source mysql-work."""

import subprocess
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[2] / "mysql-work/.zsh.d/rc/mysql-work.sh"

# Sourcing SCRIPT runs `brew --prefix` on macOS and `arch` on every platform.
SOURCE_STUBS = "brew() { print -r -- /stub-brew; }\narch() { print arm64; }\n"


def run_sourced_zsh(commands, name, *arguments):
    """Run COMMANDS in `zsh -f` after SOURCE_STUBS, as NAME.

    $1 is SCRIPT, for COMMANDS to source; ARGUMENTS follow it.
    """
    return subprocess.run(
        [
            "/bin/zsh",
            "-f",
            "-c",
            SOURCE_STUBS + commands,
            name,
            str(SCRIPT),
            *arguments,
        ],
        check=False,
        capture_output=True,
        text=True,
    )
