"""Regression tests for direct mysql-work flag lookups."""

import unittest

from mysql_work_test_utils import run_sourced_zsh

# Each flag table's kind, as in mysql_add_<kind>_flags, with one of its types.
FLAG_TABLES = (("comp", "cxx"), ("cmake", "any"))


class MysqlFlagsTest(unittest.TestCase):
    """Flag lookups must leave the calling shell's variables alone."""

    def test_adders_reject_invalid_registrations(self):
        """An invalid registration fails to its caller and registers nothing."""
        invalid_version = (
            "Invalid version 999.0. Expected format X.Y.Z, each part below 1000"
        )
        for kind, valid_type, invalid_type, type_choices in (
            ("comp", "cxx", "any", "c, cxx, cxx_debug, cxx_release"),
            ("cmake", "any", "cxx", "any, any_debug, any_release"),
        ):
            for start, end, flag_type, diagnostic in (
                (
                    "999.0.1",
                    "999.0.0",
                    valid_type,
                    "Start 999.0.1 cannot be higher than end 999.0.0",
                ),
                ("999.0", "999.0.0", valid_type, invalid_version),
                ("999.0.0", "999.0", valid_type, invalid_version),
                (
                    "999.0.0",
                    "999.0.0",
                    invalid_type,
                    f"Invalid flag type {invalid_type}. Can be one of {type_choices}",
                ),
            ):
                with self.subTest(kind=kind, start=start, end=end, type=flag_type):
                    result = run_sourced_zsh(
                        r"""
set -eu
source "$1"
before="${(kv)comp_flags} ${(kv)cmake_flags}"
rc=0
"mysql_add_${2}_flags" "$3" "$4" "$5" -DREJECTED || rc=$?
[[ "${(kv)comp_flags} ${(kv)cmake_flags}" == "$before" ]] || exit 2
exit $rc
""",
                        "mysql-invalid-registration-test",
                        kind,
                        start,
                        end,
                        flag_type,
                    )
                    self.assertEqual(
                        (result.returncode, result.stdout, result.stderr),
                        (1, "", diagnostic + "\n"),
                    )

    def test_getters_do_not_create_globals(self):
        """Direct calls return registered flags without leaking temporaries."""
        for kind, flag_type in FLAG_TABLES:
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

    def test_adders_keep_the_callers_reply(self):
        """Registration builds its key in REPLY without touching the caller's."""
        for kind, flag_type in FLAG_TABLES:
            with self.subTest(kind=kind):
                # Sourcing already runs the adders, so a leaked REPLY would
                # predate any parameter snapshot; a sentinel catches it.
                result = run_sourced_zsh(
                    r"""
set -eu
source "$1"
REPLY=caller
"mysql_add_${2}_flags" 999.0.0 999.0.0 "$3" -DADDER_SCOPE_TEST=1
print -r -- "$REPLY"
""",
                    "mysql-adder-scope-test",
                    kind,
                    flag_type,
                )
                self.assertEqual(
                    (result.returncode, result.stdout, result.stderr),
                    (0, "caller\n", ""),
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
