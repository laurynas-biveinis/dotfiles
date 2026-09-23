"""Regression tests for automatic Git configuration selection at shell startup."""

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[2] / "git/.zsh.d/env/git.sh"
SHELLS = ("/bin/sh", "/bin/zsh")


class GitStartupTest(unittest.TestCase):
    """Source the hook with a controlled Git version and isolated configuration."""

    def setUp(self):
        self.test_dir = Path(tempfile.mkdtemp(prefix="git-startup-"))
        self.addCleanup(shutil.rmtree, self.test_dir)
        self.config = self.test_dir / ".gitconfig"
        self.calls = self.test_dir / "calls"
        for version in ("1.0", "2.35", "2.38"):
            (self.test_dir / f".gitconfig.{version}").write_text(
                version, encoding="utf-8"
            )

    def run_startup(self, shell, version):
        """Return external command calls made while sourcing the hook."""
        self.calls.write_text("", encoding="utf-8")
        result = subprocess.run(
            [
                shell,
                "-f",
                "-c",
                r"""
git() {
    printf 'git %s\n' "$*" >> "$GIT_TEST_CALLS"
    [ "$#" -eq 1 ] && [ "$1" = --version ] || return 99
    printf 'git version %s\n' "$GIT_TEST_VERSION"
}
ln() {
    printf 'ln %s\n' "$*" >> "$GIT_TEST_CALLS"
    command ln "$@"
}
. "$1"
""",
                "git-startup-test",
                str(SCRIPT),
            ],
            env={
                **os.environ,
                "HOME": str(self.test_dir),
                "GIT_TEST_CALLS": str(self.calls),
                "GIT_TEST_VERSION": version,
            },
            check=True,
            capture_output=True,
            text=True,
        )
        self.assertEqual((result.stdout, result.stderr), ("", ""))
        return self.calls.read_text(encoding="utf-8").splitlines()

    def test_config_follows_git_version(self):
        """One probe selects the compatible config, including after version changes."""
        for shell in SHELLS:
            for version, expected in (
                ("2.25.1", "1.0"),
                ("2.34.9", "1.0"),
                ("2.35.0", "2.35"),
                ("2.37.9", "2.35"),
                ("2.38.0", "2.38"),
                ("2.54.0 (Apple Git-157)", "2.38"),
                ("3.0.0", "2.38"),
                ("2.25.1", "1.0"),
            ):
                with self.subTest(shell=shell, version=version):
                    calls = self.run_startup(shell, version)
                    self.assertEqual(
                        os.readlink(self.config),
                        str(self.test_dir / f".gitconfig.{expected}"),
                    )
                    self.assertEqual(calls.count("git --version"), 1)

    def test_matching_config_is_not_rewritten(self):
        """An equivalent relative symlink stays untouched across shell starts."""
        self.config.symlink_to(".gitconfig.2.38")
        for shell in SHELLS:
            with self.subTest(shell=shell):
                self.assertEqual(self.run_startup(shell, "2.38.0"), ["git --version"])
                self.assertEqual(os.readlink(self.config), ".gitconfig.2.38")


if __name__ == "__main__":
    unittest.main()
