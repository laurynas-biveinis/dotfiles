"""Exercise worktree removal against disposable repositories and local remotes."""

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[2] / "git/usr/bin/gitrmworktree"


class GitRmWorktreeTestCase(unittest.TestCase):
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
            GIT_CONFIG_COUNT="2",
            GIT_CONFIG_KEY_0="gc.auto",
            GIT_CONFIG_VALUE_0="0",
            GIT_CONFIG_KEY_1="maintenance.auto",
            GIT_CONFIG_VALUE_1="false",
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

    def assert_local_removal(self, result, branch="topic", returncode=0):
        """Assert that both the checkout and its local branch were removed."""
        self.assertEqual(result.returncode, returncode, result.stderr)
        self.assertFalse(self.worktree.exists())
        self.assertEqual(
            self.git(
                self.main, "for-each-ref", "--format=%(refname)", f"refs/heads/{branch}"
            ),
            "",
        )

    def create_merged_worktree_with_older_local_upstream(self):
        """Create a topic merged into master while its local upstream stays behind."""
        base = self.git(self.main, "rev-parse", "master")
        self.git(self.main, "branch", "base")
        commit = self.create_worktree(unmerged=True)
        self.git(self.main, "merge", "--ff-only", "topic")
        self.git(self.main, "push", "origin", "master")
        self.git(self.main, "config", "branch.topic.remote", ".")
        self.git(self.main, "config", "branch.topic.merge", "refs/heads/base")
        return base, commit


class GitRmWorktreeTest(GitRmWorktreeTestCase):
    """Validate the target and local-work policy before removing anything."""

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
        self.assert_local_removal(result)
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


class GitRmWorktreeRemovalTest(GitRmWorktreeTestCase):
    """Keep other worktrees and private submodule work intact."""

    def test_locked_worktree_is_preserved(self):
        """Default removal honors a worktree lock."""
        commit = self.create_worktree()
        self.git(self.main, "worktree", "lock", str(self.worktree))
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 128, result.stderr)
        self.assertIn("locked", result.stderr)
        self.assertEqual(self.git(self.worktree, "rev-parse", "HEAD"), commit)
        self.assertEqual(self.git(self.remote, "rev-parse", "topic"), commit)

    def create_populated_submodule(self):
        """Return a populated worktree module whose initial commit is published."""
        source = self.root / "module-source"
        self.git(self.root, "init", "--initial-branch=master", str(source))
        (source / "content").write_text("original\n", encoding="utf-8")
        self.git(source, "add", "content")
        self.git(source, "commit", "-m", "Module initial")
        self.git(
            self.main,
            "-c",
            "protocol.file.allow=always",
            "submodule",
            "add",
            str(source),
            "module",
        )
        self.git(self.main, "commit", "-am", "Add module")
        self.git(self.main, "push", "origin", "master")
        self.create_worktree()
        self.git(
            self.worktree,
            "-c",
            "protocol.file.allow=always",
            "submodule",
            "update",
            "--init",
        )
        return self.worktree / "module"

    def test_preserves_submodule_only_branch_commits(self):
        """Protect a private module branch even when its checkout is clean."""
        module = self.create_populated_submodule()
        base = self.git(module, "rev-parse", "HEAD")
        self.git(module, "switch", "-c", "private-work")
        self.git(module, "commit", "--allow-empty", "-m", "Unpushed work")
        commit = self.git(module, "rev-parse", "HEAD")
        self.git(module, "switch", "--detach", base)
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 2, result.stderr)
        self.assertIn(
            "module has unpushed commits or a stash, or cannot be read", result.stderr
        )
        self.assertEqual(self.git(module, "rev-parse", "private-work"), commit)

    def test_preserves_submodule_stash(self):
        """Protect a module stash even when its checkout is clean."""
        module = self.create_populated_submodule()
        (module / "content").write_text("stashed work\n", encoding="utf-8")
        self.git(module, "stash", "push", "-m", "Keep this")
        stash = self.git(module, "rev-parse", "refs/stash")
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 2, result.stderr)
        self.assertEqual(self.git(module, "rev-parse", "refs/stash"), stash)

    def test_removes_populated_submodule_with_only_published_work(self):
        """Allow completed worktrees containing populated submodules."""
        self.create_populated_submodule()
        result = self.remove_worktree()
        self.assert_local_removal(result)

    def test_keeps_other_missing_worktree_registrations(self):
        """Removing one checkout must not prune other registrations."""
        self.create_worktree()
        other = self.root / "other"
        self.git(self.main, "worktree", "add", "-b", "other", str(other))
        other.rename(self.root / "moved-other")
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(
            "worktree " + str(other) + "\n",
            self.git(self.main, "worktree", "list", "--porcelain"),
        )

    def test_refuses_unreadable_populated_submodule(self):
        """Unreadable module metadata must fail closed by default."""
        module = self.create_populated_submodule()
        (module / ".git").write_text(
            "gitdir: /nonexistent/gitrmworktree-test\n", encoding="utf-8"
        )
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 128, result.stderr)
        self.assertEqual((module / "content").read_text(encoding="utf-8"), "original\n")

    def test_preserves_unpushed_detached_submodule_commit(self):
        """Protect a module commit referenced only by its detached HEAD."""
        module = self.create_populated_submodule()
        self.git(module, "commit", "--allow-empty", "-m", "Detached module work")
        commit = self.git(module, "rev-parse", "HEAD")
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 4, result.stderr)
        self.assertEqual(self.git(module, "rev-parse", "HEAD"), commit)

    def test_submodule_edits_require_force_despite_ignore_configuration(self):
        """Honor the dirty policy even when Git normally hides module edits."""
        module = self.create_populated_submodule()
        self.git(self.main, "config", "submodule.module.ignore", "all")
        (module / "content").write_text("edited\n", encoding="utf-8")
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 4, result.stderr)
        self.assertEqual((module / "content").read_text(encoding="utf-8"), "edited\n")
        result = self.remove_worktree("--force")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(self.worktree.exists())

    def test_refuses_unreadable_stash_reference(self):
        """Distinguish a missing module stash from an unreadable stash ref."""
        module = self.create_populated_submodule()
        git_dir = Path(self.git(module, "rev-parse", "--absolute-git-dir"))
        for contents in ("not-an-object-id\n", "ref: refs/heads/missing\n"):
            with self.subTest(contents=contents):
                (git_dir / "refs/stash").write_text(contents, encoding="utf-8")
                result = self.remove_worktree()
                self.assertEqual(result.returncode, 2, result.stderr)
                self.assertEqual(
                    (git_dir / "refs/stash").read_text(encoding="utf-8"), contents
                )

    def test_force_removes_locked_worktree_with_private_submodule_work(self):
        """Force overrides locks as well as private module commits and stashes."""
        module = self.create_populated_submodule()
        self.git(module, "switch", "-c", "private-work")
        self.git(module, "commit", "--allow-empty", "-m", "Private work")
        (module / "content").write_text("stashed work\n", encoding="utf-8")
        self.git(module, "stash", "push", "-m", "Private stash")
        self.git(self.main, "worktree", "lock", str(self.worktree))
        result = self.remove_worktree("--force")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(self.worktree.exists())
        self.assertNotIn(
            f"worktree {self.worktree}\n",
            self.git(self.main, "worktree", "list", "--porcelain"),
        )

    def test_preserves_private_submodule_commits_referenced_only_by_tags(self):
        """Both tag types retain private work until a remote ref reaches it."""
        module = self.create_populated_submodule()
        published = self.git(module, "rev-parse", "HEAD")
        self.git(module, "commit", "--allow-empty", "-m", "Private tagged work")
        private = self.git(module, "rev-parse", "HEAD")
        self.git(module, "switch", "--detach", published)
        for options in ((), ("-a", "-m", "Keep private work")):
            with self.subTest(options=options):
                self.git(module, "tag", *options, "private", private)
                result = self.remove_worktree()
                self.assertEqual(result.returncode, 2, result.stderr)
                self.assertEqual(
                    self.git(module, "rev-parse", "private^{commit}"), private
                )
                self.git(module, "tag", "-d", "private")
        self.git(module, "tag", "private", private)
        self.git(module, "push", "origin", "private:refs/heads/published")
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(self.worktree.exists())

    def test_preserves_private_submodule_notes_unless_forced(self):
        """Protect private commits outside branch and tag refs unless forced."""
        module = self.create_populated_submodule()
        self.git(module, "notes", "add", "-m", "Private note")
        note = self.git(module, "rev-parse", "refs/notes/commits")
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 2, result.stderr)
        self.assertEqual(self.git(module, "rev-parse", "refs/notes/commits"), note)
        result = self.remove_worktree("--force")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(self.worktree.exists())


class GitRmWorktreeRemoteTest(GitRmWorktreeTestCase):
    """Choose and verify remote branches before deletion."""

    def make_origin_unreachable(self):
        """Point fetches at a missing repository without deleting the fixture remote."""
        self.git(
            self.main, "remote", "set-url", "origin", str(self.root / "unavailable.git")
        )

    def test_push_default_takes_precedence_over_upstream_remote(self):
        """Use the configured push destination before the pull destination."""
        commit = self.create_worktree()
        publish = self.root / "publish.git"
        self.git(self.root, "init", "--bare", str(publish))
        self.git(self.main, "remote", "add", "publish", str(publish))
        self.git(self.worktree, "push", "publish", "topic")
        self.git(self.main, "config", "remote.pushDefault", "publish")
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.git(self.remote, "rev-parse", "topic"), commit)
        self.assertEqual(
            self.git(
                publish, "for-each-ref", "--format=%(refname)", "refs/heads/topic"
            ),
            "",
        )

    def test_branch_push_remote_overrides_push_default(self):
        """A branch-specific push remote remains the highest-priority choice."""
        self.create_worktree()
        self.git(self.main, "config", "remote.pushDefault", "not-a-remote")
        self.git(self.main, "config", "branch.topic.pushRemote", "origin")
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            self.git(
                self.remote, "for-each-ref", "--format=%(refname)", "refs/heads/topic"
            ),
            "",
        )
        self.assertFalse(self.worktree.exists())

    def test_no_configured_remote_skips_remote_deletion(self):
        """A branch without a configured destination can be removed locally."""
        commit = self.create_worktree()
        self.git(self.main, "config", "--remove-section", "branch.topic")
        self.make_origin_unreachable()
        result = self.remove_worktree()
        self.assert_local_removal(result)
        self.assertEqual(self.git(self.remote, "rev-parse", "topic"), commit)

    def test_local_upstream_skips_remote_deletion(self):
        """A local upstream must not cause a push that deletes local refs."""
        base, commit = self.create_merged_worktree_with_older_local_upstream()
        self.make_origin_unreachable()
        result = self.remove_worktree()
        self.assert_local_removal(result)
        self.assertEqual(self.git(self.remote, "rev-parse", "topic"), commit)
        self.assertEqual(self.git(self.main, "rev-parse", "base"), base)

    def test_deletion_uses_master_instead_of_an_older_local_upstream(self):
        """A topic merged into master can be deleted despite an older upstream."""
        base, _ = self.create_merged_worktree_with_older_local_upstream()
        self.git(self.main, "config", "branch.topic.pushRemote", "origin")
        result = self.remove_worktree()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            self.git(self.main, "for-each-ref", "--format=%(refname)", "refs/heads"),
            "refs/heads/base\nrefs/heads/master",
        )
        self.assertEqual(self.git(self.main, "rev-parse", "base"), base)
        self.assertFalse(self.worktree.exists())

    def test_removes_worktree_without_any_remote_or_upstream(self):
        """A local-only repository does not need a pull destination for removal."""
        self.create_worktree()
        self.git(self.main, "remote", "remove", "origin")
        result = self.remove_worktree()
        self.assert_local_removal(result)

    def test_differently_named_upstream_does_not_select_an_unrelated_remote_branch(
        self,
    ):
        """Avoid deleting an unrelated same-named branch on the pull remote."""
        self.create_worktree()
        self.git(self.main, "config", "branch.topic.merge", "refs/heads/master")
        self.git(self.worktree, "commit", "--allow-empty", "-m", "Remote only")
        commit = self.git(self.worktree, "rev-parse", "HEAD")
        self.git(self.worktree, "push", "origin", "topic")
        self.git(self.worktree, "reset", "--hard", "master")
        result = self.remove_worktree()
        self.assert_local_removal(result)
        self.assertEqual(self.git(self.remote, "rev-parse", "topic"), commit)

    def test_force_uses_pull_remote_despite_differently_named_upstream(self):
        """Force overrides the conservative pull-remote fallback."""
        self.create_worktree()
        self.git(self.main, "config", "branch.topic.merge", "refs/heads/master")
        self.git(self.worktree, "commit", "--allow-empty", "-m", "Remote only")
        self.git(self.worktree, "push", "origin", "topic")
        self.git(self.worktree, "reset", "--hard", "master")
        result = self.remove_worktree("--force")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            self.git(
                self.remote, "for-each-ref", "--format=%(refname)", "refs/heads/topic"
            ),
            "",
        )


if __name__ == "__main__":
    unittest.main()
