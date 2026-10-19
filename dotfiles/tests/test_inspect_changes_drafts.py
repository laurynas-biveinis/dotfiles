"""Tests for the inspect-changes script's draft commands, run as a subprocess."""

import json
import unittest

from inspect_changes_replies_test_utils import (
    FINDING,
    PROPOSAL,
    REQUEST,
    REQUEST_ONLY,
    VERDICT,
    RepliesTestCase,
    with_id,
    without,
)


class DraftCase(RepliesTestCase):
    """Write draft findings and read the draft file back."""

    def written(self, reply):
        """Write R1-001 from a reply file and return the draft's records."""
        result = self.run_script(
            "draft-write", self.prefix, "1", "--reply", reply, "R1-001"
        )
        self.assertEqual(result.stdout, "wrote R1-001\n", result.stderr)
        return self.records("draft-1.jsonl")

    def assert_refused(self, args, cases, draft):
        """Run each selection; its refusal must leave the draft file as it was."""
        before = self.records(draft)
        for selected, status in cases.items():
            with self.subTest(selected=selected):
                result = self.run_script(*args, selected)
                self.assertEqual(result.returncode, 2)
                self.assertEqual(result.stdout, status)
                self.assertEqual(self.records(draft), before)


class DraftCheckTest(DraftCase):
    """`draft-check` validates a draft-step reply."""

    def check(self, reply, round_index="1"):
        """Run draft-check on a JSON reply and return the completed process."""
        return self.run_script("draft-check", round_index, self.reply("r.json", reply))

    def test_valid_reply_lists_its_finding_ids(self):
        """A reply with well-formed findings reports their IDs."""
        reply = {"findings": [FINDING, with_id(FINDING, "R1-002")]}
        result = self.check(reply)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "valid: R1-001, R1-002\n")

    def test_empty_findings_list_is_a_no_findings_pass(self):
        """An explicit empty list states that the pass found nothing."""
        result = self.check({"findings": []}, "2")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "valid: no findings\n")

    def test_reply_without_findings_is_invalid(self):
        """Silence is not a no-findings statement."""
        result = self.check({})
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stdout, "invalid: lacks findings\n")

    def test_lone_finding_object_is_a_one_entry_list(self):
        """A finding written without its list brackets is still read."""
        self.assertEqual(self.check({"findings": FINDING}).stdout, "valid: R1-001\n")
        lone = self.reply("l.json", {"findings": FINDING})
        self.assertEqual(self.written(lone), [FINDING])
        result = self.check({"findings": "none"})
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stdout, "invalid: findings is not a list\n")

    def test_missing_empty_and_non_json_replies_are_invalid(self):
        """Only a JSON object is a reply; errors point into the file as written."""
        not_json = "invalid: reply is not JSON: "
        cases = {
            "": "invalid: reply file missing or empty\n",
            "No findings.\n": not_json + "Expecting value: line 1 column 1 (char 0)\n",
            '{"findings": ["x': not_json
            + "Unterminated string starting at: line 1 column 15 (char 14)\n",
            "```json\nNo findings.\n```\n": not_json
            + "Expecting value: line 2 column 1 (char 8)\n",
            "\n```json\nNo findings.\n```\n": not_json
            + "Expecting value: line 3 column 1 (char 9)\n",
            "[]": "invalid: reply is not a JSON object\n",
        }
        for text, status in cases.items():
            with self.subTest(text=text):
                result = self.run_script("draft-check", "1", self.write("r.json", text))
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, status)
        (self.dir / "bytes.json").write_bytes(b'{"findings": [\xff]}')
        unreadable = "invalid: reply file unreadable: "
        paths = {
            self.dir / "absent.json": "invalid: reply file missing or empty\n",
            self.dir / "bytes.json": unreadable
            + "'utf-8' codec can't decode byte 0xff in position 14: invalid start byte\n",
            self.dir: unreadable + f"[Errno 21] Is a directory: '{self.dir}'\n",
        }
        for path, status in paths.items():
            with self.subTest(path=path):
                result = self.run_script("draft-check", "1", str(path))
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, status)

    def test_raw_newlines_and_stray_backslashes_are_read_as_written(self):
        """A model's unescaped newline or regex backslash does not void a reply."""
        observation = "line one\nline two \\d+ \\(x\\) \\usepackage \\bword\\b \\frac"
        placeholder = json.dumps({"findings": [{**FINDING, "observation": "@"}]})
        text = placeholder.replace('"@"', '"' + observation + '"')
        self.assertEqual(
            self.written(self.write("r.json", text)),
            [{**FINDING, "observation": observation}],
        )

    def test_lone_surrogate_escape_is_recorded_as_its_text(self):
        """An unpaired surrogate escape decodes to a code point UTF-8 cannot write."""
        odd = {**FINDING, "observation": "a\ud800b"}
        self.assertEqual(
            self.written(self.reply("r.json", {"findings": [odd]})),
            [{**odd, "observation": "a\\ud800b"}],
        )

    def test_backspace_and_form_feed_are_recorded_as_their_escapes(self):
        """A reply that parses first time still keeps a regex `\\b` visible."""
        odd = {**FINDING, "observation": "a\bword\b \frac"}
        self.assertEqual(
            self.written(self.reply("r.json", {"findings": [odd]})),
            [{**odd, "observation": "a\\bword\\b \\frac"}],
        )

    def test_reply_wrapped_in_a_code_fence_is_read(self):
        """A model may fence the JSON it writes; the fence is not the reply."""
        odd = json.dumps(
            {"findings": [{**FINDING, "title": "a\u2028b"}]}, ensure_ascii=False
        )
        cases = {
            '```json\n{"findings": []}\n```\n': "valid: no findings\n",
            '```json\n{"findings": []}\n    ```\n': "valid: no findings\n",
            f"```json\n{odd}\n```\n": "valid: R1-001\n",
        }
        for text, status in cases.items():
            with self.subTest(text=text):
                result = self.run_script("draft-check", "1", self.write("r.json", text))
                self.assertEqual(result.stdout, status)

    def test_finding_missing_a_field_is_invalid(self):
        """Every draft finding carries every finding field."""
        for key in ("title", "location", "observation", "suggested_action"):
            with self.subTest(key=key):
                finding = without(FINDING, key)
                result = self.check({"findings": [finding]})
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, f"invalid: R1-001 lacks {key}\n")

    def test_listed_text_is_read_as_lines(self):
        """A text field written as a list of strings is its lines; [] is empty."""
        listed = self.reply(
            "l.json", {"findings": [{**FINDING, "observation": ["a", "b"]}]}
        )
        self.assertEqual(self.written(listed), [{**FINDING, "observation": "a\nb"}])
        result = self.check({"findings": [{**FINDING, "observation": []}]})
        self.assertEqual(result.stdout, "invalid: R1-001 lacks observation\n")

    def test_tolerated_spellings_are_accepted(self):
        """A percentage string and another case still mean the same values."""
        finding = {**FINDING, "confidence": "80%", "severity": "important"}
        result = self.check({"findings": [finding]})
        self.assertEqual(result.stdout, "valid: R1-001\n")

    def test_bad_ids_are_invalid(self):
        """IDs must use the pass's round and appear once."""
        cases = {
            "invalid: R2-001 is not a round-1 ID\n": [with_id(FINDING, "R2-001")],
            "invalid: repeated ID: R1-001\n": [FINDING, FINDING],
        }
        for status, findings in cases.items():
            with self.subTest(status=status):
                self.assertEqual(self.check({"findings": findings}).stdout, status)

    def test_review_failed_voids_the_pass(self):
        """A drafter that could not review says so and why, whatever else it wrote."""
        failed = "invalid: drafter reports a failed review"
        cases = {
            "git diff was denied": f"{failed}: git diff was denied\n",
            "\ngit diff was denied\nby the user": f"{failed}: git diff was denied\n",
            True: f"{failed}\n",
        }
        for reason, status in cases.items():
            with self.subTest(reason=reason):
                result = self.check({"findings": [], "review_failed": reason})
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, status)

    def test_experiment_requests_are_counted_and_checked(self):
        """An unusable draft request is ignored; the findings still count."""
        ignored = "valid: R1-001; malformed requests ignored\n"
        cases = (
            (REQUEST, "valid: R1-001; experiment requests: 1\n"),
            (
                {**REQUEST, "procedure": ["a", "b"]},
                "valid: R1-001; experiment requests: 1\n",
            ),
            (REQUEST_ONLY, ignored),
            (without(REQUEST, "procedure"), ignored),
            ({**REQUEST, "procedure": []}, ignored),
            ({**REQUEST, "procedure": ["", " "]}, ignored),
            ({**REQUEST, "procedure": [1]}, ignored),
        )
        for request, status in cases:
            with self.subTest(request=request):
                reply = {"findings": [FINDING], "experiment_requests": [request]}
                self.assertEqual(self.check(reply).stdout, status)

    def test_unusable_requests_beside_usable_ones_are_dropped(self):
        """A bad request does not void the usable ones beside it."""
        mixed = [REQUEST, REQUEST_ONLY]
        result = self.check({"findings": [FINDING], "experiment_requests": mixed})
        self.assertEqual(
            result.stdout,
            "valid: R1-001; experiment requests: 1; unusable requests dropped: 1\n",
        )

    def test_draft_requests_print_only_what_draft_check_counted(self):
        """A draft request without `supports` is dropped by both commands."""
        mixed = [REQUEST, REQUEST_ONLY]
        reply = self.reply(
            "r.json", {"findings": [FINDING], "experiment_requests": mixed}
        )
        result = self.run_script("requests", "--draft", reply)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout.count("### EXP"), 1, result.stdout)
        self.assertIn("- Supports: R1-001\n", result.stdout)


class DraftWriteTest(DraftCase):
    """`draft-write` appends surviving findings to a round's draft file."""

    def batch_of(self, *entries):
        """Run proposals over a verifier reply into round 2's batch."""
        reply = self.reply("v.json", {"proposed_findings": list(entries)})
        result = self.run_script("proposals", self.prefix, "2", reply)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_writes_selected_findings_normalized(self):
        """Only the named findings are written, as validated records."""
        second = {
            **with_id(FINDING, "R1-002"),
            "confidence": "70%",
            "severity": "important",
            "provenance": "Introduced",
        }
        reply = self.reply("r.json", {"findings": [FINDING, second]})
        result = self.run_script(
            "draft-write", self.prefix, "1", "--reply", reply, "R1-002"
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, "wrote R1-002\n")
        self.assertEqual(
            self.records("draft-1.jsonl"),
            [{**with_id(FINDING, "R1-002"), "confidence": 70}],
        )

    def test_out_of_range_claims_are_kept_as_given(self):
        """Verification re-derives a draft's claims, so none voids the pass.

        Each is recorded as given, as text where it is not.
        """
        cases = (
            ({**FINDING, "severity": "HIGH"}, {}),
            ({**with_id(FINDING, "R1-002"), "confidence": 101}, {}),
            ({**with_id(FINDING, "R1-003"), "provenance": "new"}, {}),
            ({**with_id(FINDING, "R1-004"), "confidence": 0.8}, {}),
            ({**with_id(FINDING, "R1-005"), "severity": None}, {"severity": ""}),
            ({**with_id(FINDING, "R1-006"), "provenance": 3}, {"provenance": "3"}),
            (
                {**with_id(FINDING, "R1-007"), "severity": ["HIGH"]},
                {"severity": '["HIGH"]'},
            ),
        )
        odd = [given for given, _ in cases]
        reply = self.reply("r.json", {"findings": odd})
        ids = [entry["id"] for entry in odd]
        check = self.run_script("draft-check", "1", reply)
        self.assertEqual(check.stdout, "valid: " + ", ".join(ids) + "\n")
        args = ("draft-write", self.prefix, "1", "--reply", reply, *ids)
        result = self.run_script(*args)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        recorded = [{**given, **text} for given, text in cases]
        self.assertEqual(self.records("draft-1.jsonl"), recorded)

    def test_unicode_line_separators_stay_inside_a_record(self):
        """JSON Lines split at LF only, so U+2028 and the like stay content."""
        for separator in ("\u0085", "\u2028", "\u2029"):
            with self.subTest(separator=repr(separator)):
                odd = {**FINDING, "observation": f"a{separator}b"}
                reply = self.reply(
                    "r.json", {"findings": [odd, with_id(FINDING, "R1-002")]}
                )
                self.written(reply)
                second = self.run_script(
                    "draft-write", self.prefix, "1", "--reply", reply, "R1-002"
                )
                self.assertEqual(second.stdout, "wrote R1-002\n", second.stderr)
                self.assertEqual(
                    self.records("draft-1.jsonl"), [odd, with_id(FINDING, "R1-002")]
                )
                self.path("draft-1.jsonl").unlink()

    def test_batch_selection_gets_fresh_round_ids(self):
        """Selected proposals become the next round's findings, labels dropped."""
        other = {**PROPOSAL, "title": "Other helper"}
        self.batch_of(PROPOSAL, other)
        result = self.run_script("draft-write", self.prefix, "2", "--batch", "P2")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, "wrote P2 → R2-001\n")
        self.assertEqual(self.records("draft-2.jsonl"), [{"id": "R2-001", **other}])

    def test_refusals_leave_the_draft_alone(self):
        """A selection repeating, rewriting or naming an absent entry is refused."""
        reply = self.reply("r.json", {"findings": [FINDING]})
        args = ("draft-write", self.prefix, "1", "--reply", reply)
        self.assertEqual(
            self.run_script(*args, "R1-001", "R1-001").stdout,
            "refused: repeated: R1-001\n",
        )
        self.assertFalse(self.path("draft-1.jsonl").exists())
        self.written(reply)
        cases = {
            "R1-001": "refused: already in draft: R1-001\n",
            "R1-009": "refused: not in reply: R1-009\n",
        }
        self.assert_refused(args, cases, "draft-1.jsonl")

    def test_batch_refusals_leave_the_draft_alone(self):
        """A batch proposal becomes one draft finding, once; an absent one none.

        P3 repeats P1, as a reply listing a proposal twice gives.
        """
        other = {**PROPOSAL, "title": "Other helper"}
        self.batch_of(PROPOSAL, other, PROPOSAL)
        args = ("draft-write", self.prefix, "2", "--batch")
        self.assertEqual(
            self.run_script(*args, "P1", "P1").stdout, "refused: repeated: P1\n"
        )
        self.assertEqual(
            self.run_script(*args, "P1", "P3").stdout, "refused: repeated: P3 as P1\n"
        )
        self.assertFalse(self.path("draft-2.jsonl").exists())
        self.assertEqual(self.run_script(*args, "P1").stdout, "wrote P1 → R2-001\n")
        self.assertEqual(self.run_script(*args, "P2").stdout, "wrote P2 → R2-002\n")
        cases = {
            "P1": "refused: already in draft: P1 as R2-001\n",
            "P3": "refused: already in draft: P3 as R2-001\n",
            "P9": "refused: not in batch: P9\n",
        }
        self.assert_refused(args, cases, "draft-2.jsonl")

    def test_draft_whose_round_has_a_verdict_is_refused(self):
        """Once a round's verification began, its draft takes no more findings."""
        reply = self.reply(
            "r.json", {"findings": [FINDING, with_id(FINDING, "R1-002")]}
        )
        self.written(reply)
        self.put_records("verdicts-1.jsonl", [VERDICT])
        proposal = self.reply("v.json", {"proposed_findings": [PROPOSAL]})
        batch = self.run_script("proposals", self.prefix, "1", proposal)
        self.assertEqual(batch.returncode, 0, batch.stdout + batch.stderr)
        refused = "refused: round 1 already has a verdict\n"
        selections = (("--reply", reply, "R1-002"), ("--batch", "P1"))
        for *source, selected in selections:
            args = ("draft-write", self.prefix, "1", *source)
            self.assert_refused(args, {selected: refused}, "draft-1.jsonl")


if __name__ == "__main__":
    unittest.main()
