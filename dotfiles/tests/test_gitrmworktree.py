"""Exercise worktree removal against disposable repositories and local remotes."""

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[2] / "git/usr/bin/gitrmworktree"


class GitRmWorktreeTest(unittest.TestCase):
    """Refusals must preserve both the worktree and its branches."""

    def setUp(self):
        self.root = Path(tempfile.mkdtemp(prefix="gitrmworktree-")).resolve()
        self.addCleanup(shutil.rmtree, self.root)
        self.env = {
            key: value
            for key, value in os.environ.items()
            if not key.startswith("GIT_")
        }
        self.env.update(
            GIT_CONFIG_NOSYSTEM="1",
            GIT_CONFIG_GLOBAL="/dev/null",
            GIT_AUTHOR_NAME="Worktree Test",
            GIT_AUTHOR_EMAIL="worktree@example.invalid",
            GIT_COMMITTER_NAME="Worktree Test",
            GIT_COMMITTER_EMAIL="worktree@example.invalid",
            GIT_TERMINAL_PROMPT="0",
            ZDOTDIR=str(self.root),
        )
        self.main = self.root / "main"
        self.remote = self.root / "remote.git"
        self.worktree = self.root / "worktree"
        self.git(
            self.root, "init", "--bare", "--initial-branch=master", str(self.remote)
        )
        self.git(self.root, "clone", str(self.remote), str(self.main))
        (self.main / "tracked").write_text("original\n", encoding="utf-8")
        self.git(self.main, "add", "tracked")
        self.git(self.main, "commit", "-m", "Initial")
        self.git(self.main, "push", "-u", "origin", "master")

    def git(self, directory, *arguments):
        """Run fixture commands fail-fast, returning stdout."""
        return subprocess.run(
            ["git", "-C", str(directory), *arguments],
            env=self.env,
            check=True,
            capture_output=True,
            text=True,
        ).stdout.strip()

    def create_worktree(self, branch="topic", unmerged=False):
        """Create a pushed branch with optional work beyond master."""
        self.git(self.main, "worktree", "add", "-b", branch, str(self.worktree))
        if unmerged:
            self.git(self.worktree, "commit", "--allow-empty", "-m", "Topic work")
        self.git(self.worktree, "push", "-u", "origin", branch)
        return self.git(self.worktree, "rev-parse", "HEAD")

    def remove_worktree(self, *options, target=None):
        """Invoke the public command without loading personal Zsh configuration."""
        return subprocess.run(
            [
                "/bin/zsh",
                "-df",
                str(SCRIPT),
                *options,
                str(self.worktree if target is None else target),
            ],
            cwd=self.root,
            env=self.env,
            check=False,
            capture_output=True,
            text=True,
        )

    def test_branch_name_containing_master_does_not_prove_a_merge(self):
        """The old substring guard deletes this unmerged branch's worktree."""
        commit = self.create_worktree("fix-master-build", unmerged=True)
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 2, result.stderr)
        self.assertIn("is not fully merged", result.stderr)
        self.assertEqual(self.git(self.worktree, "rev-parse", "HEAD"), commit)
        self.assertEqual(
            self.git(self.remote, "rev-parse", "refs/heads/fix-master-build"), commit
        )


if __name__ == "__main__":
    unittest.main()
