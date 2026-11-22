"""Regression tests for loading the mysql-work shell configuration."""

import unittest

from mysql_work_test_utils import SCRIPT, run_sourced_zsh, run_zsh

# Sources SCRIPT for macOS and opens a loop over every MySQL configuration:
# $name names its array and $args holds its elements. The body must add `done`.
EACH_CONFIGURATION = r"""
set -eu
OSTYPE=darwin
source "$1"
for name in ${(ok)parameters[(I)(MY|FB|PS)[0-9][0-9][0-9]*]}; do
    [[ $name == MY8030_MAX_CORE_DUMP_FLAGS ]] && continue
    args=("${(@P)name}")
"""


class MysqlWorkStartupTest(unittest.TestCase):
    """Check what loading the rc file costs and does, re-sourcing included."""

    def test_missing_homebrew_fails_with_a_diagnostic(self):
        """An absent command must not become a nounset error during startup."""
        result = run_zsh(
            'OSTYPE=darwin; path=(); source "$1"',
            "mysql-missing-brew-test",
            str(SCRIPT),
        )
        self.assertEqual(
            (result.returncode, result.stdout, result.stderr),
            (1, "", "brew not found on PATH\n"),
        )

    def test_platform_startup_forks_nothing(self):
        """Every platform branch starts up without a process: forks fail here."""
        for ostype, cputype, prefix, expected in (
            (
                "darwin26.0",
                "arm64",
                "/opt/homebrew",
                [
                    "-DROCKSDB_BUILD_ARCH=armv8.1-a+crypto",
                    "-DCMAKE_C_COMPILER=/opt/homebrew/opt/llvm/bin/clang",
                ],
            ),
            (
                "darwin26.0",
                "x86_64",
                "/usr/local",
                [
                    "-DCMAKE_C_COMPILER=/usr/local/opt/llvm/bin/clang",
                ],
            ),
            (
                "linux-gnu",
                "aarch64",
                "/unused",
                [
                    "-DROCKSDB_BUILD_ARCH=armv8.1-a+crypto",
                    "-DCMAKE_C_COMPILER=clang",
                ],
            ),
            (
                "linux-gnu",
                "x86_64",
                "/unused",
                [
                    "-DCMAKE_C_COMPILER=clang",
                ],
            ),
        ):
            with self.subTest(ostype=ostype, cputype=cputype):
                result = run_sourced_zsh(
                    r"""
set -eu
OSTYPE=$2 CPUTYPE=$3
commands[brew]=$4/bin/brew
brew() { print -u2 'unexpected brew invocation'; return 1; }
arch() { print -u2 'unexpected arch invocation'; return 1; }
ulimit -u 1
source "$1"
print -rl -- "${(@M)FB8036:#-DROCKSDB_BUILD_ARCH=*}" "$MYCLANG[1]"
""",
                    "mysql-platform-test",
                    ostype,
                    cputype,
                    prefix,
                )
                self.assertEqual(
                    (result.returncode, result.stdout.splitlines(), result.stderr),
                    (0, expected, ""),
                )

    def test_every_mysql_configuration_sets_c_flags(self):
        """Empty C flags clear cached flags when a workaround range shrinks."""
        result = run_sourced_zsh(
            EACH_CONFIGURATION + r"""
    c_flags=("${(@M)args:#-DCMAKE_C_FLAGS=*}")
    [[ ${#c_flags} == 1 ]] || { print -u2 -- "$name: ${#c_flags} C flags"; exit 1; }
done
print -r -- ${MY8046[(r)-DCMAKE_C_FLAGS=*]}
print -r -- ${MY26100[(r)-DCMAKE_C_FLAGS=*]}
""",
            "mysql-c-flags-test",
        )
        self.assertEqual(
            (result.returncode, result.stdout, result.stderr),
            (0, "-DCMAKE_C_FLAGS=-Wno-unknown-warning-option\n-DCMAKE_C_FLAGS=\n", ""),
        )

    def test_only_8026_debug_builds_disable_debug_extname(self):
        """The 8.0.26 debug option reaches its debug builds and no other."""
        result = run_sourced_zsh(
            EACH_CONFIGURATION + r"""
    matches=("${(@M)args:#-DDEBUG_EXTNAME=OFF}")
    if (( ${#matches} )); then
        print -r -- "$name ${#matches}"
    fi
done
""",
            "mysql-debug-extname-test",
        )
        self.assertEqual(
            (result.returncode, result.stdout.splitlines(), result.stderr),
            (0, ["FB8026D 1", "MY8026D 1"], ""),
        )

    def test_unknown_build_type_is_reported_and_skipped(self):
        """A misspelled build type exports nothing in place of a sibling."""
        for build_types, exported in (
            (("debug", "relase"), ["MY99900D"]),
            (("relase",), []),
        ):
            with self.subTest(build_types=build_types):
                result = run_sourced_zsh(
                    r"""
set -eu
source "$1"
shift
typeset -a myd=(-DDEBUG_MARK)
mysql_export_version 999.0.0 "$@"
for name in ${(ok)parameters[(I)MY99900*]}; do
    print -r -- "$name"
done
""",
                    "mysql-build-type-test",
                    *build_types,
                )
                self.assertEqual(
                    (result.returncode, result.stdout.splitlines(), result.stderr),
                    (
                        0,
                        exported,
                        "Invalid build type relase."
                        " Can be one of debug, release, benchmark\n",
                    ),
                )

    def test_second_source_is_silent(self):
        """Top-level code must tolerate re-running in the same shell."""
        # The second source must stay last: its status is the process status.
        result = run_sourced_zsh('source "$1"; source "$1"', "mysql-work-startup-test")
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "", ""))


if __name__ == "__main__":
    unittest.main()
