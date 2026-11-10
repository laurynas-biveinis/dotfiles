"""Regression tests for nightly build and script failure handling."""

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]

COMMAND_STUB = r"""#!/bin/sh
call="${0##*/} $*"
echo "$call" >>"$NIGHTLY_TEST_CALLS"
if [ "$1" = list ]; then
	printf 'Package Version Latest Type\n------- ------- ------ -----\n'
	echo "foo 1.0 2.0 wheel"
fi
case "$call" in
"$NIGHTLY_TEST_FAILED_CALL"*) exit 1 ;;
esac
"""


def write_executable(path, text):
    """Write an executable file."""
    path.write_text(text, encoding="utf-8")
    path.chmod(0o755)


class ScriptTestCase(unittest.TestCase):
    """Run a repository Zsh script in an isolated home that records calls."""

    def setUp(self):
        self.root = Path(tempfile.mkdtemp(prefix="nightly-test-")).resolve()
        self.addCleanup(shutil.rmtree, self.root)
        self.calls = self.root / "calls"

    def run_script(self, script, env):
        """Run the script with the test home and env added to the environment."""
        self.calls.write_text("", encoding="utf-8")
        return subprocess.run(
            ["/bin/zsh", str(REPO / script)],
            env={
                **os.environ,
                "HOME": str(self.root),
                "ZDOTDIR": str(self.root),
                "NIGHTLY_TEST_CALLS": str(self.calls),
                **env,
            },
            cwd=self.root,
            check=False,
            capture_output=True,
            text=True,
        )

    def recorded_calls(self):
        """Return the calls the stubs recorded during the last run."""
        return self.calls.read_text(encoding="utf-8").splitlines()


class NightlyTest(ScriptTestCase):
    """Run the real scripts with isolated worktrees and external dependencies."""

    def setUp(self):
        super().setUp()
        self.nightly = self.root / ".nightly"
        self.nightly.mkdir()
        self.functions = self.root / "functions"
        self.functions.mkdir()
        for worktree in ("first", "second"):
            for build in ("_build-a", "_build-b"):
                (self.root / worktree / build).mkdir(parents=True)
        (self.root / ".zshenv").write_text(
            r"""
fpath=("$HOME/functions")
WORK_SRC_ROOT=$HOME
WORK_REPOS_TO_PULL=()
WORK_TREES_TO_UPDATE=(first second)
git() { return 0; }
cf_off() { return 0; }
cf_on() { return 0; }
record() {
    print -r -- "$1" >> "$NIGHTLY_TEST_CALLS"
    [[ $1 != "$NIGHTLY_TEST_FAILED_CALL"* ]]
}
ninja() { record "build:${PWD:h:t}/${PWD:t}"; }
""",
            encoding="utf-8",
        )
        for name, body in {
            "is_on_ac": "return 0",
            "mysql_unlink_googletest": "return 0",
            "mysql_need_openssl_unlink_workaround": (
                '[[ "version:${PWD:t}" != "$NIGHTLY_TEST_FAILED_CALL"* ]] && print 1'
            ),
            "mysql_maybe_unlink_openssl": 'record "unlink:$1:${PWD:t}"',
            "mysql_relink_openssl": 'record "relink:$1:${PWD:t}"',
        }.items():
            (self.functions / name).write_text(body + "\n", encoding="utf-8")
        write_executable(
            self.nightly / "01fetchworksrc",
            r"""#!/bin/zsh
"$NIGHTLY_TEST_FETCHWORKSRC"
fetch_status=$?
print -r -- "fetch-exit:$fetch_status" >> "$NIGHTLY_TEST_CALLS"
exit $fetch_status
""",
        )
        write_executable(
            self.nightly / "02after",
            '#!/bin/zsh\nprint after >> "$NIGHTLY_TEST_CALLS"\n',
        )

    def run_nightly(self, failed_call="build:first/_build-a"):
        """Run nightly using only the test home's scripts and configuration."""
        return self.run_script(
            "nightly/usr/bin/nightly",
            {
                "NIGHTLY_TEST_FAILED_CALL": failed_call,
                "NIGHTLY_TEST_FETCHWORKSRC": str(
                    REPO / "mysql-work/usr/bin/fetchworksrc"
                ),
            },
        )

    def test_build_outcomes(self):
        """Every build and script runs, with cleanup and an aggregate status."""
        for failed_call, expected_status in (
            ("build:first/_build-a", 1),
            ("build:first/_build-b", 1),
            ("build:second/_build-a", 1),
            ("build:second/_build-b", 1),
            ("relink:1:first", 1),
            ("none", 0),
        ):
            with self.subTest(failed_call=failed_call):
                result = self.run_nightly(failed_call)
                self.assertEqual(
                    result.returncode, expected_status, result.stdout + result.stderr
                )
                self.assertEqual(
                    self.recorded_calls(),
                    [
                        "unlink:1:first",
                        "build:first/_build-a",
                        "build:first/_build-b",
                        "relink:1:first",
                        "unlink:1:second",
                        "build:second/_build-a",
                        "build:second/_build-b",
                        "relink:1:second",
                        f"fetch-exit:{expected_status}",
                        "after",
                    ],
                )
                self.assertEqual(
                    result.stdout.count("Nightly completed!"), 1 - expected_status
                )
                self.assertEqual(
                    result.stderr.count("Nightly completed with failures!"),
                    expected_status,
                )

    def test_worktree_without_build_dirs(self):
        """A worktree with no build directory does not stop the others."""
        for build in ("_build-a", "_build-b"):
            (self.root / "first" / build).rmdir()
        result = self.run_nightly("none")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(
            self.recorded_calls(),
            [
                "unlink:1:first",
                "relink:1:first",
                "unlink:1:second",
                "build:second/_build-a",
                "build:second/_build-b",
                "relink:1:second",
                "fetch-exit:0",
                "after",
            ],
        )

    def test_unlink_failure_skips_worktree_builds(self):
        """A failed OpenSSL unlink skips that worktree's builds but relinks."""
        result = self.run_nightly("unlink:1:first")
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertEqual(
            self.recorded_calls(),
            [
                "unlink:1:first",
                "relink:1:first",
                "unlink:1:second",
                "build:second/_build-a",
                "build:second/_build-b",
                "relink:1:second",
                "fetch-exit:1",
                "after",
            ],
        )

    def test_version_check_failure_stops_fetchworksrc(self):
        """A failed MySQL version check stops fetchworksrc but not the runner."""
        result = self.run_nightly("version:first")
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertEqual(self.recorded_calls(), ["fetch-exit:1", "after"])

    def test_script_failure_stops_only_that_script(self):
        """Checking a script's status must not suppress errexit inside it."""
        write_executable(
            self.nightly / "01fetchworksrc",
            "#!/bin/zsh\n"
            "set -e\n"
            'print before >> "$NIGHTLY_TEST_CALLS"\n'
            "false\n"
            'print unreachable >> "$NIGHTLY_TEST_CALLS"\n',
        )
        result = self.run_nightly()
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertEqual(self.recorded_calls(), ["before", "after"])
        self.assertIn(
            f"Nightly script failed: {self.nightly / '01fetchworksrc'}", result.stderr
        )
        self.assertIn("Nightly completed with failures!", result.stderr)

    def test_script_runs_through_its_shebang(self):
        """A script runs under its own interpreter, as when invoked directly."""
        write_executable(
            self.nightly / "01fetchworksrc",
            '#!/bin/bash\necho "${BASH_VERSION:+bash}" >>"$NIGHTLY_TEST_CALLS"\n',
        )
        result = self.run_nightly()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(self.recorded_calls(), ["bash", "after"])


class PythonUpdateTest(ScriptTestCase):
    """Run pythonupdate with stub asdf and pip commands."""

    def setUp(self):
        super().setUp()
        self.bin = self.root / "bin"
        self.bin.mkdir()
        for command in ("asdf", "pip"):
            write_executable(self.bin / command, COMMAND_STUB)

    def run_pythonupdate(self, failing_call):
        """Run pythonupdate with the stub commands first on PATH."""
        return self.run_script(
            "python/usr/bin/pythonupdate",
            {
                "PATH": f"{self.bin}{os.pathsep}{os.environ['PATH']}",
                "NIGHTLY_TEST_FAILED_CALL": failing_call,
            },
        )

    def test_step_failures(self):
        """A failed step fails the script after the remaining steps run."""
        all_steps = [
            "asdf reshim python",
            "pip --version",
            "pip list --outdated --not-required",
            "pip install -U foo",
            "pip install -U python-lsp-server[all]",
            "asdf reshim python",
            "pip check",
        ]
        for failing_call, expected_status, expected_steps in (
            ("none", 0, all_steps),
            ("asdf reshim python", 1, all_steps),
            ("pip --version", 1, all_steps[:2]),
            ("pip list", 1, all_steps),
            ("pip install -U foo", 1, all_steps),
            ("pip install -U python-lsp-server", 1, all_steps),
            ("pip check", 1, all_steps),
        ):
            with self.subTest(failing_call=failing_call):
                result = self.run_pythonupdate(failing_call)
                self.assertEqual(
                    result.returncode, expected_status, result.stdout + result.stderr
                )
                self.assertEqual(
                    [" ".join(call.split()[:4]) for call in self.recorded_calls()],
                    expected_steps,
                )


if __name__ == "__main__":
    unittest.main()
