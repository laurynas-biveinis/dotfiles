"""Exercise MySQL version lookup and OpenSSL decisions in fresh Zsh shells."""

import tempfile
import unittest
from pathlib import Path

from mysql_work_test_utils import SCRIPT, run_zsh

FUNCTIONS = SCRIPT.parents[1] / "functions"
VERSION_5_7_44_CONTENTS = (
    "MYSQL_VERSION_MAJOR=5\nMYSQL_VERSION_MINOR=7\nMYSQL_VERSION_PATCH=44\n"
)


class MysqlVersionTest(unittest.TestCase):
    """Use real version files without sourcing the interactive configuration."""

    def run_function(self, function, contents, *arguments, ostype="darwin"):
        """Invoke an autoloaded function in a temporary source tree."""
        with tempfile.TemporaryDirectory(prefix="mysql version ") as folder:
            Path(folder, "VERSION").write_text(contents, encoding="utf-8")
            return run_zsh(
                r"""
fpath=("$1" $fpath)
OSTYPE=$2
autoload "$3"
shift 2
"$@"
""",
                "mysql-version-test",
                str(FUNCTIONS),
                ostype,
                function,
                *arguments,
                cwd=folder,
            )

    def test_get_version(self):
        """The shared reader prints one complete dotted version."""
        result = self.run_function(
            "mysql_get_version", VERSION_5_7_44_CONTENTS, "VERSION"
        )
        self.assertEqual(
            (result.returncode, result.stdout, result.stderr), (0, "5.7.44\n", "")
        )

    def test_get_version_normalizes_numbers(self):
        """Keep the integer conversion previously performed by both callers."""
        result = self.run_function(
            "mysql_get_version",
            "MYSQL_VERSION_MAJOR=08\nMYSQL_VERSION_MINOR=00\nMYSQL_VERSION_PATCH=032\n",
            "VERSION",
        )
        self.assertEqual(
            (result.returncode, result.stdout, result.stderr), (0, "8.0.32\n", "")
        )

    def test_openssl_version_cutoff(self):
        """Only versions before 8.0.33 need the macOS workaround."""
        for version, expected in (
            ("5.7.44", "1\n"),
            ("8.0.18", "1\n"),
            ("8.0.32", "1\n"),
            ("8.0.33", "0\n"),
            ("8.1.0", "0\n"),
            ("8.4.0", "0\n"),
            ("9.0.0", "0\n"),
            ("26.10.0", "0\n"),
        ):
            with self.subTest(version=version):
                major, minor, patch = version.split(".")
                result = self.run_function(
                    "mysql_need_openssl_unlink_workaround",
                    f"MYSQL_VERSION_MAJOR={major}\nMYSQL_VERSION_MINOR={minor}\n"
                    f"MYSQL_VERSION_PATCH={patch}\n",
                )
                self.assertEqual(
                    (result.returncode, result.stdout, result.stderr), (0, expected, "")
                )

    def test_missing_version_components_fail_without_output(self):
        """Neither caller may turn a missing field into a successful result."""
        for key in ("MAJOR", "MINOR", "PATCH"):
            contents = "".join(
                line
                for line in VERSION_5_7_44_CONTENTS.splitlines(keepends=True)
                if not line.startswith(f"MYSQL_VERSION_{key}=")
            )
            for function, arguments, filename in (
                ("mysql_get_version", ("VERSION",), "VERSION"),
                ("mysql_need_openssl_unlink_workaround", (), "./VERSION"),
            ):
                with self.subTest(key=key, function=function):
                    result = self.run_function(function, contents, *arguments)
                    self.assertEqual(
                        (result.returncode, result.stdout, result.stderr),
                        (
                            1,
                            "",
                            f"No MYSQL_VERSION_{key} version number in {filename}\n",
                        ),
                    )

    def test_linux_skips_version_lookup(self):
        """Linux needs no workaround, even without readable version fields."""
        result = self.run_function(
            "mysql_need_openssl_unlink_workaround", "", ostype="linux-gnu"
        )
        self.assertEqual(
            (result.returncode, result.stdout, result.stderr), (0, "0\n", "")
        )


if __name__ == "__main__":
    unittest.main()
