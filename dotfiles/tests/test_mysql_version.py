"""Exercise MySQL version lookup and OpenSSL decisions in fresh Zsh shells."""

import tempfile
import unittest
from pathlib import Path

from mysql_work_test_utils import SCRIPT, run_zsh

FUNCTIONS = SCRIPT.parents[1] / "functions"


class MysqlVersionTest(unittest.TestCase):
    """Use real version files without sourcing the interactive configuration."""

    def test_pre_8_mysql_needs_openssl_workaround(self):
        """The major-version branch must not require minor 0 and patch <= 32."""
        with tempfile.TemporaryDirectory(prefix="mysql version ") as folder:
            Path(folder, "VERSION").write_text(
                "MYSQL_VERSION_MAJOR=5\nMYSQL_VERSION_MINOR=7\n"
                "MYSQL_VERSION_PATCH=44\n",
                encoding="utf-8",
            )
            result = run_zsh(
                'fpath=("$1" $fpath); OSTYPE=darwin; '
                "autoload mysql_need_openssl_unlink_workaround; "
                "mysql_need_openssl_unlink_workaround",
                "mysql-version-test",
                str(FUNCTIONS),
                cwd=folder,
            )
        self.assertEqual(
            (result.returncode, result.stdout, result.stderr), (0, "1\n", "")
        )


if __name__ == "__main__":
    unittest.main()
