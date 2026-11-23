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

    def test_refuses_hidden_untracked_files(self):
        """The default guard includes untracked files hidden by Git configuration."""
        self.create_worktree()
        self.git(self.worktree, "config", "status.showUntrackedFiles", "no")
        (self.worktree / "new").write_text("keep me\n", encoding="utf-8")
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 4, result.stderr)
        self.assertIn("has uncommitted changes", result.stderr)
        self.assertIn("?? new", result.stderr)
        self.assertEqual(
            (self.worktree / "new").read_text(encoding="utf-8"), "keep me\n"
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

    def test_refuses_tracked_changes(self):
        """Both unstaged and staged edits survive default removal."""
        commit = self.create_worktree()
        (self.worktree / "tracked").write_text("changed\n", encoding="utf-8")
        for staged in (False, True):
            with self.subTest(staged=staged):
                result = self.remove_worktree()
                self.assertEqual(result.returncode, 4, result.stderr)
                self.assertIn("has uncommitted changes", result.stderr)
                self.assertEqual(
                    (self.worktree / "tracked").read_text(encoding="utf-8"), "changed\n"
                )
                self.assertEqual(
                    self.git(self.remote, "rev-parse", "refs/heads/topic"), commit
                )
                self.git(self.worktree, "add", "tracked")

    def test_force_discards_dirty_files_and_unmerged_local_commit(self):
        """An explicit --force still permits discarding local work."""
        self.create_worktree()
        self.git(self.worktree, "commit", "--allow-empty", "-m", "Local only")
        (self.worktree / "new").write_text("discard\n", encoding="utf-8")
        result = self.remove_worktree("--force")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(self.worktree.exists())
        self.assertEqual(
            self.git(
                self.main, "for-each-ref", "--format=%(refname)", "refs/heads/topic"
            ),
            "",
        )
        self.assertEqual(
            self.git(
                self.remote, "for-each-ref", "--format=%(refname)", "refs/heads/topic"
            ),
            "",
        )

    def test_ignored_build_output_does_not_block_removal(self):
        """Ignored files do not turn a finished worktree into dirty work."""
        self.create_worktree()
        (self.main / ".git/info/exclude").write_text("build/\n", encoding="utf-8")
        (self.worktree / "build").mkdir()
        (self.worktree / "build/output").write_text("generated\n", encoding="utf-8")
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(self.worktree.exists())

    def test_refuses_main_worktree(self):
        """Reject the main checkout without changing its branch."""
        before = self.git(self.main, "rev-parse", "HEAD")
        result = self.remove_worktree("--force", target=self.main)
        self.assertEqual(result.returncode, 3, result.stderr)
        self.assertIn("is not a linked worktree", result.stderr)
        self.assertEqual(self.git(self.main, "rev-parse", "HEAD"), before)
        self.assertEqual(self.git(self.remote, "rev-parse", "master"), before)

    def test_refuses_worktree_subdirectory(self):
        """Reject a subdirectory before any removal or remote change."""
        commit = self.create_worktree()
        nested = self.worktree / "nested"
        nested.mkdir()
        result = self.remove_worktree("--force", target=nested)
        self.assertEqual(result.returncode, 3, result.stderr)
        self.assertIn("is not the top of a worktree", result.stderr)
        self.assertTrue(nested.is_dir())
        self.assertEqual(self.git(self.remote, "rev-parse", "topic"), commit)

    def test_refuses_linked_master_even_with_force(self):
        """Protect the trunk even when it is in a linked worktree."""
        commit = self.git(self.main, "rev-parse", "master")
        self.git(self.main, "switch", "-c", "holding")
        self.git(self.main, "worktree", "add", str(self.worktree), "master")
        result = self.remove_worktree("--force")
        self.assertEqual(result.returncode, 2, result.stderr)
        self.assertIn("Refusing to delete master", result.stderr)
        self.assertEqual(self.git(self.worktree, "rev-parse", "HEAD"), commit)
        self.assertEqual(self.git(self.remote, "rev-parse", "master"), commit)

    def test_refuses_separate_git_directory(self):
        """Fail before side effects when the main directory cannot be inferred."""
        self.git(self.main, "init", "--separate-git-dir=" + str(self.root / "separate"))
        commit = self.create_worktree()
        result = self.remove_worktree("--force")
        self.assertEqual(result.returncode, 3, result.stderr)
        self.assertIn("Unsupported repository layout", result.stderr)
        self.assertEqual(self.git(self.worktree, "rev-parse", "HEAD"), commit)
        self.assertEqual(self.git(self.remote, "rev-parse", "topic"), commit)

    def test_accepts_a_symlink_to_the_worktree_root(self):
        """Resolve a root alias to the registered worktree."""
        self.create_worktree()
        alias = self.root / "alias"
        alias.symlink_to(self.worktree, target_is_directory=True)
        result = self.remove_worktree(target=alias)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(self.worktree.exists())

    def test_refuses_detached_head_even_with_force(self):
        """Explain detached HEAD refusal independently of remote configuration."""
        commit = self.create_worktree()
        self.git(self.worktree, "switch", "--detach")
        result = self.remove_worktree("--force")
        self.assertEqual(result.returncode, 3, result.stderr)
        self.assertIn("is not on a branch (detached HEAD", result.stderr)
        self.assertEqual(self.git(self.worktree, "rev-parse", "HEAD"), commit)
        self.assertEqual(self.git(self.remote, "rev-parse", "topic"), commit)


if __name__ == "__main__":
    unittest.main()
