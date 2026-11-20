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


class MysqlFlagTypesTest(unittest.TestCase):
    """A lookup gets only its own type's flags and those of the type it extends."""

    def test_types_cover_their_underscore_suffixed_variants(self):
        """A lookup of cxx_debug, say, gets the flags of both cxx_debug and cxx."""
        expected = {
            ("comp", "c"): ["-DC"],
            ("comp", "cxx"): ["-DCXX"],
            ("comp", "cxx_debug"): ["-DCXX", "-DCXX_DEBUG"],
            ("comp", "cxx_release"): ["-DCXX", "-DCXX_RELEASE"],
            ("cmake", "any"): ["-DANY"],
            ("cmake", "any_debug"): ["-DANY", "-DANY_DEBUG"],
            ("cmake", "any_release"): ["-DANY", "-DANY_RELEASE"],
        }
        result = run_sourced_zsh(
            r"""
set -eu
source "$1"
shift
for kind flag_type in "$@"; do
    "mysql_add_${kind}_flags" 999.0.0 999.0.0 "$flag_type" "-D${(U)flag_type}"
done
for kind flag_type in "$@"; do
    flags=$("mysql_get_${kind}_flags" 999.0.0 "$flag_type")
    print -r -- "$kind" "$flag_type" "$flags"
done
""",
            "mysql-flag-types-test",
            *(word for lookup in expected for word in lookup),
        )
        selected = {}
        for line in result.stdout.splitlines():
            kind, flag_type, *flags = line.split()
            selected[kind, flag_type] = sorted(flags)
        self.assertEqual(
            (result.returncode, selected, result.stderr), (0, expected, "")
        )


if __name__ == "__main__":
    unittest.main()
