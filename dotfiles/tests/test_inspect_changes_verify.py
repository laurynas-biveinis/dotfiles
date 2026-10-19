"""Tests for the inspect-changes script's verify-record command."""

import contextlib
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from inspect_changes_replies_test_utils import (
    DROP,
    FINDING,
    PROPOSAL,
    REQUEST_ONLY,
    SCRIPT,
    VERDICT,
    RepliesTestCase,
    with_id,
    without,
)


class VerifyRecordCase(RepliesTestCase):
    """Run verify-record on verifier replies to findings the draft holds."""

    def setUp(self):
        super().setUp()
        drafted = [with_id(FINDING, f"R1-{number:03d}") for number in range(1, 41)]
        self.put_records("draft-1.jsonl", drafted)

    def verify(self, reply, finding="R1-001"):
        """Run verify-record on a JSON reply."""
        return self.run_script(
            "verify-record", self.prefix, finding, self.reply(f"{finding}.json", reply)
        )

    def assert_every_reply_refused(self, finding, reason):
        """Assert a verdict, a deferral and an absent reply are all refused."""
        replies = (
            {"verdict": with_id(VERDICT, finding)},
            {"experiment_requests": [REQUEST_ONLY]},
        )
        paths = [self.reply(f"r{n}.json", reply) for n, reply in enumerate(replies)]
        for reply in [*paths, str(self.dir / "absent.json")]:
            with self.subTest(reply=reply):
                result = self.run_script("verify-record", self.prefix, finding, reply)
                self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
                self.assertEqual(result.stdout, f"refused: {reason}\n")


class VerifyRecordTest(VerifyRecordCase):
    """`verify-record` validates a verifier reply and records its verdict."""

    def test_keep_verdict_is_appended_copied_and_summarized(self):
        """A valid keep lands in its round's file and in its analyst's copy."""
        result = self.verify({"verdict": VERDICT})
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(
            result.stdout,
            "R1-001: keep IMPORTANT introduced 80% — `src/a.py:10` — "
            "Loop skips the last element\n",
        )
        self.assertEqual(self.records("verdicts-1.jsonl"), [VERDICT])
        self.assertEqual(self.verdict_copy("R1-001"), VERDICT)

    def test_drop_verdict_reports_its_reason(self):
        """A drop needs no keep fields and reports why it dropped."""
        result = self.verify({"verdict": DROP})
        self.assertEqual(
            result.stdout, "R1-001: drop 30% — `n` already counts one past the end.\n"
        )
        self.assertFalse(self.path("verdict-R1-001.json").exists())

    def test_drop_keeps_its_refined_claim(self):
        """A drop's own location and observation are what dedup compares."""
        drop = {**DROP, "final_location": "`src/a.py:12`", "final_observation": "No."}
        self.verify({"verdict": drop})
        self.assertEqual(self.records("verdicts-1.jsonl"), [drop])

    def test_tolerated_spellings_are_recorded_canonically(self):
        """Report grouping reads the recorded case, not the verifier's."""
        loose = {
            **VERDICT,
            "outcome": " Keep",
            "final_severity": " important",
            "final_provenance": "Introduced",
            "final_confidence": "80%",
        }
        result = self.verify({"verdict": loose})
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        loud_drop = with_id(
            {**DROP, "outcome": "DROP", "final_confidence": 30.0}, "R1-002"
        )
        result = self.verify({"verdict": loud_drop}, "R1-002")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        recorded = self.records("verdicts-1.jsonl")
        self.assertEqual(recorded, [VERDICT, with_id(DROP, "R1-002")])
        # 30.0 == 30, so only the type shows the float was recorded as an integer.
        self.assertIs(type(recorded[1]["final_confidence"]), int)
        self.assertEqual(self.verdict_copy("R1-001"), VERDICT)
        self.assertFalse(self.path("verdict-R1-002.json").exists())

    def test_lone_or_text_proposal_is_one_proposal(self):
        """A proposal written without its list brackets, or as text, still counts."""
        cases = {"R1-001": PROPOSAL, "R1-002": "x", "R1-003": ["x"]}
        for finding, proposal in cases.items():
            with self.subTest(proposal=proposal):
                verdict = with_id(VERDICT, finding)
                reply = {"verdict": verdict, "proposed_findings": proposal}
                result = self.verify(reply, finding)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual(
                    result.stdout,
                    f"{finding}: keep IMPORTANT introduced 80% — `src/a.py:10` — "
                    "Loop skips the last element; proposals: 1\n",
                )
        self.assertEqual(
            self.records("verdicts-1.jsonl"),
            [with_id(VERDICT, finding) for finding in cases],
        )

    def test_multi_line_keep_fields_keep_their_first_line_indentation(self):
        """Only surrounding line ends and trailing blanks are trimmed."""
        fenced = "  ```sh\n  make\n  ```"
        padded = {
            **VERDICT,
            "final_observation": "\n" + fenced,
            "final_suggested_action": "Use `range(n)`.\n  ",
        }
        self.verify({"verdict": padded})
        kept = {**VERDICT, "final_observation": fenced}
        self.assertEqual(self.records("verdicts-1.jsonl"), [kept])
        self.assertEqual(self.verdict_copy("R1-001"), kept)

    def test_drop_ignores_keep_only_fields(self):
        """Severity and provenance are checked only on a keep."""
        self.verify(
            {"verdict": {**DROP, "final_severity": "HIGH", "final_provenance": "new"}}
        )
        self.assertEqual(self.records("verdicts-1.jsonl"), [DROP])

    def test_keep_records_a_reason_only_when_it_is_text(self):
        """A keep's optional reason is kept as text and otherwise ignored."""
        cases = {"R1-001": " ", "R1-002": ["x", "y"], "R1-003": "x", "R1-004": 5}
        for finding, reason in cases.items():
            with self.subTest(reason=reason):
                verdict = {**with_id(VERDICT, finding), "reason": reason}
                result = self.verify({"verdict": verdict}, finding)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        listed = {**with_id(VERDICT, "R1-002"), "reason": "x\ny"}
        reasoned = {**with_id(VERDICT, "R1-003"), "reason": "x"}
        self.assertEqual(
            self.records("verdicts-1.jsonl"),
            [VERDICT, listed, reasoned, with_id(VERDICT, "R1-004")],
        )
        self.assertEqual(self.verdict_copy("R1-003"), reasoned)

    def test_listed_text_fields_are_read_as_lines(self):
        """Required and optional text written as a list of strings is its lines."""
        keep = {
            **VERDICT,
            "verification_trace": ["Read a.", "Read b."],
            "final_observation": ["  indented", "next"],
        }
        result = self.verify({"verdict": keep})
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        drop = {
            **with_id(DROP, "R1-002"),
            "reason": ["a", "b"],
            "final_location": ["x"],
        }
        self.assertEqual(self.verify({"verdict": drop}, "R1-002").returncode, 0)
        self.assertEqual(
            self.records("verdicts-1.jsonl"),
            [
                {
                    **VERDICT,
                    "verification_trace": "Read a.\nRead b.",
                    "final_observation": "  indented\nnext",
                },
                {**with_id(DROP, "R1-002"), "reason": "a\nb", "final_location": "x"},
            ],
        )
        empty = {**with_id(VERDICT, "R1-003"), "verification_trace": []}
        self.assertEqual(
            self.verify({"verdict": empty}, "R1-003").stdout,
            "invalid: R1-003 verdict lacks verification_trace\n",
        )

    def test_verdict_without_a_required_field_is_invalid(self):
        """Each keep field is required; an invalid verdict writes nothing."""
        for key in ("verification_trace", "final_title", "final_suggested_action"):
            with self.subTest(key=key):
                verdict = without(VERDICT, key)
                result = self.verify({"verdict": verdict})
                self.assertEqual(result.returncode, 1)
                self.assertEqual(
                    result.stdout, f"invalid: R1-001 verdict lacks {key}\n"
                )
                self.assertFalse(self.path("verdicts-1.jsonl").exists())
        drop = without(DROP, "reason")
        self.assertEqual(
            self.verify({"verdict": drop}).stdout,
            "invalid: R1-001 verdict lacks reason\n",
        )

    def test_out_of_range_values_are_invalid(self):
        """Enumerated fields and the confidence percentage are checked."""
        cases = {
            "outcome": "maybe",
            "final_severity": "HIGH",
            "final_provenance": "new",
            "final_confidence": "high",
        }
        for key, value in cases.items():
            with self.subTest(key=key):
                result = self.verify({"verdict": {**VERDICT, key: value}})
                self.assertEqual(
                    result.stdout, f"invalid: R1-001 verdict has {key}: {value!r}\n"
                )

    def test_wrong_or_repeated_verdicts_are_rejected(self):
        """A verdict for another ID is invalid; any reply after a verdict is refused."""
        other = self.verify({"verdict": with_id(VERDICT, "R1-002")})
        self.assertEqual(other.stdout, "invalid: lacks verdict for R1-001\n")
        self.assertEqual(self.verify({}).stdout, "invalid: lacks verdict for R1-001\n")
        setup = self.verify({"verdict": VERDICT})
        self.assertEqual(setup.returncode, 0, setup.stdout + setup.stderr)
        self.assert_every_reply_refused("R1-001", "R1-001 already has a verdict")
        self.assertEqual(self.records("verdicts-1.jsonl"), [VERDICT])

    def test_finding_no_draft_holds_is_refused(self):
        """Neither a verdict, a deferral nor a missing reply counts for one."""
        self.assert_every_reply_refused("R2-001", "not in draft: R2-001")
        self.assertFalse(self.path("verdicts-2.jsonl").exists())
        self.assertFalse(self.path("verdict-R2-001.json").exists())

    def test_analyst_copy_is_written_first_and_whole(self):
        """A stale copy is replaced, and a refused copy records no verdict."""
        self.put("verdict-R1-001.json", "stale\n")
        self.verify({"verdict": VERDICT})
        self.assertEqual(self.verdict_copy("R1-001"), VERDICT)
        outside = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, outside)
        self.path("verdict-R1-002.json").symlink_to(outside / "copy.json")
        result = self.verify({"verdict": with_id(VERDICT, "R1-002")}, "R1-002")
        self.assertEqual(result.returncode, 2, result.stdout)
        self.assertEqual(self.records("verdicts-1.jsonl"), [VERDICT])
        self.assertEqual(list(outside.iterdir()), [])

    def test_concurrent_calls_lose_no_verdict(self):
        """Calls that overlap on one review file each keep their record."""
        findings = [f"R1-{number:03d}" for number in range(1, 41)]
        replies = {
            finding: self.reply(
                f"{finding}.json", {"verdict": with_id(VERDICT, finding)}
            )
            for finding in findings
        }
        with contextlib.ExitStack() as stack:
            calls = [
                stack.enter_context(
                    subprocess.Popen(
                        [sys.executable, str(SCRIPT), "verify-record", self.prefix]
                        + [finding, reply],
                        stdout=subprocess.DEVNULL,
                    )
                )
                for finding, reply in replies.items()
            ]
            self.assertEqual([call.wait() for call in calls], [0] * len(findings))
        recorded = sorted(r["id"] for r in self.records("verdicts-1.jsonl"))
        self.assertEqual(recorded, findings)


class VerifyDeferralTest(VerifyRecordCase):
    """`verify-record` tells a deferral from a verdict and routes requests."""

    def test_placeholder_verdict_beside_requests_is_a_deferral(self):
        """Beside usable requests, a verdict stating no outcome holds a place."""
        placeholders = (
            None,
            {},
            "",
            [],
            "pending",
            {"id": "R1-001"},
            {"id": "R1-001", "outcome": " "},
            without(VERDICT, "outcome"),
        )
        replies = [{"experiment_requests": [REQUEST_ONLY]}] + [
            {"verdict": verdict, "experiment_requests": [REQUEST_ONLY]}
            for verdict in placeholders
        ]
        for reply in replies:
            with self.subTest(reply=reply):
                result = self.verify(reply)
                self.assertEqual(
                    result.stdout, "R1-001: deferral; experiment requests: 1\n"
                )
                self.assertFalse(self.path("verdicts-1.jsonl").exists())

    def test_verdict_stating_an_outcome_is_judged_as_one(self):
        """Without requests a stub is invalid, and beside them so is a bad outcome."""
        stub = self.verify({"verdict": {"id": "R1-001"}})
        self.assertEqual(stub.stdout, "invalid: R1-001 verdict has outcome: None\n")
        maybe = {"verdict": {**VERDICT, "outcome": "maybe"}}
        result = self.verify({**maybe, "experiment_requests": [REQUEST_ONLY]})
        self.assertEqual(
            result.stdout, "invalid: R1-001 verdict has outcome: 'maybe'\n"
        )

    def test_listed_procedure_steps_are_a_deferral(self):
        """A procedure written as a list of steps is a usable request."""
        steps = {**REQUEST_ONLY, "procedure": ["a", "b"]}
        result = self.verify({"experiment_requests": [steps]})
        self.assertEqual(result.stdout, "R1-001: deferral; experiment requests: 1\n")

    def test_lone_request_object_is_a_deferral(self):
        """A request written without its list brackets still defers."""
        result = self.verify({"experiment_requests": REQUEST_ONLY})
        self.assertEqual(result.stdout, "R1-001: deferral; experiment requests: 1\n")

    def test_usable_requests_beside_unusable_ones_are_a_deferral(self):
        """A bad request beside a usable one leaves a valid deferral."""
        result = self.verify({"experiment_requests": [REQUEST_ONLY, "no procedure"]})
        self.assertEqual(
            result.stdout,
            "R1-001: deferral; experiment requests: 1; unusable requests dropped: 1\n",
        )

    def test_malformed_requests_beside_a_verdict_are_ignored(self):
        """An unusable request does not void the verdict it travels with."""
        bad = [{"goal": "no procedure"}]
        result = self.verify({"verdict": VERDICT, "experiment_requests": bad})
        self.assertTrue(
            result.stdout.endswith("; malformed requests ignored\n"), result.stdout
        )
        self.assertEqual(self.records("verdicts-1.jsonl"), [VERDICT])
        alone = self.verify({"experiment_requests": bad}, "R1-002")
        self.assertEqual(alone.stdout, "invalid: lacks verdict for R1-002\n")

    def test_routed_parts_are_counted_not_recorded(self):
        """Proposals and attached requests are reported; only the verdict is kept."""
        reply = {
            "verdict": VERDICT,
            "proposed_findings": [PROPOSAL],
            "experiment_requests": [REQUEST_ONLY],
        }
        result = self.verify(reply)
        self.assertTrue(
            result.stdout.endswith("; proposals: 1; experiment requests: 1\n"),
            result.stdout,
        )
        self.assertEqual(self.records("verdicts-1.jsonl"), [VERDICT])


if __name__ == "__main__":
    unittest.main()
