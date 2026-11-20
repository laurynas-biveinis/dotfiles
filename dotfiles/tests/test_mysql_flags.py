"""Regression tests for direct mysql-work flag lookups."""

import unittest

from mysql_work_test_utils import run_sourced_zsh


class MysqlFlagsTest(unittest.TestCase):
    """Flag lookups must leave the calling shell's variables alone."""

    def test_getters_do_not_create_globals(self):
        """Direct calls return registered flags without leaking temporaries."""
        for kind, flag_type in (("comp", "cxx"), ("cmake", "any")):
            with self.subTest(kind=kind):
                result = run_sourced_zsh(
                    r"""
set -eu
source "$1"
"mysql_add_${2}_flags" 999.0.0 999.0.0 "$3" -DGETTER_SCOPE_TEST=1
typeset -a before after
before=("${(@k)parameters}")
"mysql_get_${2}_flags" 999.0.0 "$3"
after=("${(@k)parameters}")
print -rl -- ${(o)after:|before}
""",
                    "mysql-flags-test",
                    kind,
                    flag_type,
                )
                self.assertEqual(
                    (result.returncode, result.stdout.split(), result.stderr),
                    (0, ["-DGETTER_SCOPE_TEST=1"], ""),
                )


if __name__ == "__main__":
    unittest.main()
