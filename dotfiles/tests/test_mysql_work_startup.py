"""Regression test for re-sourcing the mysql-work shell configuration."""

import unittest

from mysql_work_test_utils import run_sourced_zsh


class MysqlWorkStartupTest(unittest.TestCase):
    """Source the rc file the way a repeated `source ~/.zshrc` does."""

    def test_second_source_is_silent(self):
        """Top-level code must tolerate re-running in the same shell."""
        # The second source must stay last: its status is the process status.
        result = run_sourced_zsh('source "$1"; source "$1"', "mysql-work-startup-test")
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "", ""))


if __name__ == "__main__":
    unittest.main()
