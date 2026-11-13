"""Regression test for re-sourcing the mysql-work shell configuration."""

import subprocess
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[2] / "mysql-work/.zsh.d/rc/mysql-work.sh"


class MysqlWorkStartupTest(unittest.TestCase):
    """Source the rc file the way a repeated `source ~/.zshrc` does."""

    def test_second_source_is_silent(self):
        """Top-level code must tolerate re-running in the same shell."""
        # The second source must stay last: its status is the process status.
        result = subprocess.run(
            [
                "/bin/zsh",
                "-f",
                "-c",
                "brew() { print -r -- /stub-brew; }; arch() { print arm64; }; "
                'source "$1"; source "$1"',
                "mysql-work-startup-test",
                str(SCRIPT),
            ],
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "", ""))


if __name__ == "__main__":
    unittest.main()
