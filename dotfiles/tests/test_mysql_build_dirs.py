"""Regression test for iterating over the mysql-work build directories."""

import shutil
import tempfile
import unittest
from pathlib import Path

from mysql_work_test_utils import run_sourced_zsh


class ForAllBuildDirsTest(unittest.TestCase):
    """Run for_all_build_dirs over two build directories in a scratch tree."""

    def test_loop_variable_stays_local(self):
        """The calling shell must not keep the loop's build_dir afterwards."""
        root = Path(tempfile.mkdtemp(prefix="mysql build dirs "))
        self.addCleanup(shutil.rmtree, root)
        (root / "_build-a").mkdir()
        (root / "_build-b").mkdir()
        result = run_sourced_zsh(
            r"""
source "$1"
setopt pushd_silent
visit() { print -r -- "${PWD:t}"; }
for_all_build_dirs visit
print -r -- "${build_dir-unset}"
""",
            "mysql-build-dirs-test",
            cwd=root,
        )
        self.assertEqual(
            (result.returncode, result.stdout, result.stderr),
            (0, "_build-a\n_build-b\nunset\n", ""),
        )


if __name__ == "__main__":
    unittest.main()
