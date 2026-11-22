"""Regression tests for loading the mysql-work shell configuration."""

import unittest

from mysql_work_test_utils import SCRIPT, run_sourced_zsh


class MysqlWorkStartupTest(unittest.TestCase):
    """Check what loading the rc file costs and does, re-sourcing included."""

    def test_flag_annotations_are_plain_comments(self):
        """A comment-only command substitution still forks at every start."""
        self.assertNotIn("`#", SCRIPT.read_text(encoding="utf-8"))

    def test_second_source_is_silent(self):
        """Top-level code must tolerate re-running in the same shell."""
        # The second source must stay last: its status is the process status.
        result = run_sourced_zsh('source "$1"; source "$1"', "mysql-work-startup-test")
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "", ""))


if __name__ == "__main__":
    unittest.main()
