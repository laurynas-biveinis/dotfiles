"""Tests for the inspect-changes script's claim command."""

import contextlib
import fcntl
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from inspect_changes_replies_test_utils import SCRIPT, RepliesTestCase

CLAIMED = "Claimed; the review basis is not written yet.\n"
LOCK = "/tmp/inspect-changes-.lock"


class ClaimTest(RepliesTestCase):
    """`claim` picks a review topic and creates its state file in one step."""

    def claim(self, slug):
        """Claim a slug among the scratch directory's review files."""
        return self.run_script("claim", str(self.dir / f"inspect-changes-{slug}"))

    def test_free_topic_is_claimed_as_is(self):
        """A slug no review file starts with is the topic; its state file appears."""
        result = self.claim("topic")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, f"claimed {self.prefix}\n")
        self.assertEqual(self.path("state.md").read_text(encoding="utf-8"), CLAIMED)

    def test_taken_topic_gets_the_smallest_free_suffix(self):
        """Any file whose name the slug begins takes it; a free `-3` beats `-5`."""
        self.put("2-summary.md", "Prose.\n")
        self.put("4-state.md", "Claimed.\n")
        result = self.claim("topic")
        self.assertEqual(result.stdout, f"claimed {self.prefix}-3\n")
        self.assertEqual(self.path("3-state.md").read_text(encoding="utf-8"), CLAIMED)
        self.assertFalse(self.path("state.md").exists())

    def test_topic_extending_a_live_review_is_suffixed_once(self):
        """A state file at a hyphen cut of the slug takes it, but not its suffixes."""
        self.write("inspect-changes-foo-state.md", "Claimed.\n")
        result = self.claim("foo-bar-summary")
        claimed = self.dir / "inspect-changes-foo-bar-summary-2"
        self.assertEqual(result.stdout, f"claimed {claimed}\n")
        self.assertEqual(
            Path(f"{claimed}-state.md").read_text(encoding="utf-8"), CLAIMED
        )

    def test_topic_extending_a_live_review_at_a_later_hyphen_is_suffixed(self):
        """Every hyphen cut of the slug is tested, not only the first."""
        self.write("inspect-changes-foo-bar-state.md", "Claimed.\n")
        result = self.claim("foo-bar-summary")
        claimed = self.dir / "inspect-changes-foo-bar-summary-2"
        self.assertEqual(result.stdout, f"claimed {claimed}\n")

    def test_only_a_state_file_at_a_hyphen_cut_suffixes(self):
        """Neither a sibling topic's files nor a cut inside a word take the slug."""
        self.write("inspect-changes-foo-baz-state.md", "Claimed.\n")
        self.write("inspect-changes-fo-state.md", "Claimed.\n")
        result = self.claim("foo-bar")
        self.assertEqual(result.stdout, f"claimed {self.dir}/inspect-changes-foo-bar\n")

    def test_live_review_differing_only_in_case_takes_the_slug(self):
        """`/tmp` may be case-insensitive, so `Foo`'s files take the slug `foo`."""
        self.write("inspect-changes-Foo-state.md", "Claimed.\n")
        result = self.claim("foo")
        self.assertEqual(result.stdout, f"claimed {self.dir}/inspect-changes-foo-2\n")

    def test_mixed_case_slug_is_claimed_in_lowercase(self):
        """The topic is lowercase whatever the slug's case."""
        result = self.claim("Topic")
        self.assertEqual(result.stdout, f"claimed {self.prefix}\n")
        self.assertEqual(self.path("state.md").read_text(encoding="utf-8"), CLAIMED)

    def test_cut_differing_only_in_case_suffixes(self):
        """A live `Foo` takes `foo-summary`, whose report is `Foo`'s summary."""
        self.write("inspect-changes-Foo-state.md", "Claimed.\n")
        result = self.claim("foo-summary")
        claimed = self.dir / "inspect-changes-foo-summary-2"
        self.assertEqual(result.stdout, f"claimed {claimed}\n")

    def test_slug_of_more_than_one_path_component_is_refused(self):
        """A `/` in the slug would claim inside another directory."""
        directory = self.dir / "inspect-changes-a"
        directory.mkdir()
        result = self.claim("a/b")
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertEqual(
            result.stdout,
            f"refused: {directory}/b: the slug must be one path component\n",
        )
        self.assertEqual(list(directory.iterdir()), [])

    def test_slug_under_a_non_directory_is_refused(self):
        """A prefix whose directory cannot be listed is refused, not a crash."""
        self.write("inspect-changes-file", "x")
        causes = {"absent": "No such file or directory", "file": "Not a directory"}
        for slug, cause in causes.items():
            with self.subTest(slug=slug):
                parent = self.dir / f"inspect-changes-{slug}"
                result = self.claim(f"{slug}/inspect-changes-x")
                self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
                self.assertEqual(
                    result.stdout,
                    f"refused: {parent}/inspect-changes-x: cannot list {parent}: "
                    f"{cause}\n",
                )

    @unittest.skipIf(os.geteuid() == 0, "root lists and writes any directory")
    def test_slug_under_an_unusable_directory_is_refused(self):
        """An unreadable directory cannot be listed, a read-only one written to."""
        failures = {
            "locked": (0, "cannot list {parent}: Permission denied"),
            "readonly": (
                0o500,
                "cannot create {parent}/inspect-changes-x-state.md: Permission denied",
            ),
        }
        for slug, (mode, failure) in failures.items():
            with self.subTest(slug=slug):
                parent = self.dir / f"inspect-changes-{slug}"
                parent.mkdir(mode=mode)
                self.addCleanup(parent.chmod, 0o700)
                result = self.claim(f"{slug}/inspect-changes-x")
                self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
                reason = failure.format(parent=parent)
                self.assertEqual(
                    result.stdout, f"refused: {parent}/inspect-changes-x: {reason}\n"
                )

    def test_link_at_the_unsuffixed_name_is_suffixed_past(self):
        """A stray link there, leading out or looping, is just a taken name."""
        outside = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, outside)
        targets = {"out": outside, "loop": self.dir / "inspect-changes-loop"}
        for slug, target in targets.items():
            with self.subTest(slug=slug):
                (self.dir / f"inspect-changes-{slug}").symlink_to(target)
                result = self.claim(slug)
                claimed = self.dir / f"inspect-changes-{slug}-2"
                self.assertEqual(result.stdout, f"claimed {claimed}\n", result.stderr)
        self.assertEqual(list(outside.iterdir()), [])

    def test_claim_under_the_lock_sees_a_review_claimed_meanwhile(self):
        """Listing and creating under the shared lock keeps reviews off one topic."""
        with contextlib.ExitStack() as stack:
            flags = os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW
            lock = stack.enter_context(os.fdopen(os.open(LOCK, flags, 0o600)))
            fcntl.flock(lock, fcntl.LOCK_EX)
            call = stack.enter_context(
                subprocess.Popen(
                    [sys.executable, str(SCRIPT), "claim", self.prefix],
                    stdout=subprocess.PIPE,
                    text=True,
                )
            )
            # Unwinds before the Popen exit, which would wait on a blocked claim.
            stack.callback(fcntl.flock, lock, fcntl.LOCK_UN)
            with self.assertRaises(subprocess.TimeoutExpired):
                call.wait(timeout=1)
            self.put("state.md", "Claimed.\n")
            fcntl.flock(lock, fcntl.LOCK_UN)
            stdout, _ = call.communicate()
        self.assertEqual(call.returncode, 0)
        self.assertEqual(stdout, f"claimed {self.prefix}-2\n")
        self.assertEqual(self.path("2-state.md").read_text(encoding="utf-8"), CLAIMED)
        self.assertEqual(
            self.path("state.md").read_text(encoding="utf-8"), "Claimed.\n"
        )


if __name__ == "__main__":
    unittest.main()
