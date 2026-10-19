"""Tests for the inspect-changes reply-handling script, run as a subprocess."""

import os
import shutil
import tempfile
import unittest
from pathlib import Path

from inspect_changes_replies_test_utils import (
    ANALYSIS_RECORD,
    BODY,
    DROP,
    FINDING,
    PROPOSAL,
    REQUEST,
    REQUEST_ONLY,
    VERDICT,
    RepliesTestCase,
    read_records,
    with_id,
    without,
)

ANALYSIS = {"id": "R1-001", "body": BODY}


class ConfinementTest(RepliesTestCase):
    """The script writes only review files under `/tmp/inspect-changes-*`."""

    def test_every_writing_command_refuses_an_escaping_prefix(self):
        """Neither a plain path, a `..` escape nor a symlink leads a write out."""
        outside = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, outside)
        link = self.dir / "inspect-changes-link"
        link.symlink_to(outside)
        draft = self.reply("draft.json", {"findings": [FINDING]})
        verdict = self.reply("verdict.json", {"verdict": VERDICT})
        analysis = self.reply("analysis.json", {"analysis": ANALYSIS})
        proposal = self.reply("proposal.json", {"proposed_findings": [PROPOSAL]})
        prefixes = {
            "plain": str(outside / "inspect-changes-x"),
            "dotdot": f"{self.dir}/../../{outside.relative_to('/')}/x",
            "symlink": str(link / "inspect-changes-x"),
        }
        for kind, prefix in prefixes.items():
            commands = {
                "draft-write": ["draft-write", prefix, "1", "--reply", draft, "R1-001"],
                "verify-record": ["verify-record", prefix, "R1-001", verdict],
                "analysis-record": ["analysis-record", prefix, "R1-001", analysis],
                "analysis-skip": ["analysis-skip", prefix, "R1-001"],
                "proposals": ["proposals", prefix, "2", proposal],
                "assemble": ["assemble", prefix, "--status", "converged"],
            }
            for name, args in commands.items():
                with self.subTest(kind=kind, command=name):
                    result = self.run_script(*args)
                    self.assertEqual(
                        result.returncode, 2, result.stdout + result.stderr
                    )
                    self.assertTrue(
                        result.stdout.startswith("refused: "), result.stdout
                    )
        self.assertEqual(list(outside.iterdir()), [])

    def test_link_between_review_files_is_written_through(self):
        """A review file linking to another is followed, never replaced."""
        target = self.dir / "inspect-changes-other-batch-2.jsonl"
        self.path("batch-2.jsonl").symlink_to(target)
        reply = self.reply("r.json", {"proposed_findings": [PROPOSAL]})
        result = self.run_script("proposals", self.prefix, "2", reply)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertTrue(self.path("batch-2.jsonl").is_symlink())
        self.assertEqual(
            read_records(target), [{"label": "P1", "source": "r.json", **PROPOSAL}]
        )

    def test_no_bytecode_is_written(self):
        """The script runs from the dotfiles tree, which a review must not write."""
        cache = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, cache)
        env = {k: v for k, v in os.environ.items() if k != "PYTHONDONTWRITEBYTECODE"}
        env["PYTHONPYCACHEPREFIX"] = str(cache)
        absent = str(self.dir / "absent.json")
        result = self.run_script("draft-check", "1", absent, env=env)
        self.assertEqual(result.stdout, "invalid: reply file missing or empty\n")
        for module in ("rendering", "reply_file"):
            self.assertEqual(list(cache.rglob(f"{module}.*")), [])

    def test_finding_id_must_be_one(self):
        """An ID becomes part of a file name, so it is checked."""
        result = self.run_script("analysis-skip", self.prefix, "R1-001\nrm -rf x #")
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertEqual(
            result.stdout, "refused: not a finding ID: 'R1-001\\nrm -rf x #'\n"
        )
        self.assertFalse(self.path("analyses.jsonl").exists())


class ProposalsTest(RepliesTestCase):
    """`proposals` labels the proposed findings of several replies."""

    def test_batch_labels_proposals_and_names_sources(self):
        """Each proposal gets a label and the reply it came from."""
        other = {**PROPOSAL, "title": "Other helper", "confidence": "70%"}
        first = self.reply(
            "reply-verify-R1-001-1.json", {"proposed_findings": [PROPOSAL]}
        )
        second = self.reply(
            "reply-analyze-R1-002-1.json", {"proposed_findings": [other]}
        )
        result = self.run_script("proposals", self.prefix, "2", first, second)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(
            self.records("batch-2.jsonl"),
            [
                {"label": "P1", "source": "reply-verify-R1-001-1.json", **PROPOSAL},
                {
                    "label": "P2",
                    "source": "reply-analyze-R1-002-1.json",
                    **other,
                    "confidence": 70,
                },
            ],
        )
        self.assertTrue(
            result.stdout.startswith(
                "### P1 — SUGGESTION — Helper name hides its side effect\n\n"
                "- Source: reply-verify-R1-001-1.json\n- Confidence: 60%\n"
            ),
            result.stdout,
        )
        self.assertIn("### P2 — SUGGESTION — Other helper\n", result.stdout)

    def test_later_call_appends_with_the_next_label(self):
        """A round's batch file keeps every label it ever printed."""
        first = self.reply("a.json", {"proposed_findings": [PROPOSAL]})
        second = self.reply(
            "b.json", {"proposed_findings": [{**PROPOSAL, "title": "B"}]}
        )
        setup = self.run_script("proposals", self.prefix, "2", first)
        self.assertEqual(setup.returncode, 0, setup.stdout + setup.stderr)
        result = self.run_script("proposals", self.prefix, "2", second)
        self.assertTrue(
            result.stdout.startswith("### P2 — SUGGESTION — B\n"), result.stdout
        )
        self.assertEqual(
            [record["label"] for record in self.records("batch-2.jsonl")],
            ["P1", "P2"],
        )

    def test_multi_line_parts_keep_the_report_layout(self):
        """A proposal renders its title and fields as a kept finding does."""
        wrapped = {**PROPOSAL, "title": "Helper\nname", "observation": "a\n- b"}
        reply = self.reply("w.json", {"proposed_findings": [wrapped]})
        result = self.run_script("proposals", self.prefix, "2", reply)
        self.assertTrue(
            result.stdout.startswith("### P1 — SUGGESTION — Helper name\n"),
            result.stdout,
        )
        self.assertIn("- Observation:\n  a\n  - b\n- Suggested action:", result.stdout)

    def test_out_of_range_proposal_values_are_kept_as_given(self):
        """Verification re-derives a proposal's claims, so none voids it."""
        odd = {
            **without(PROPOSAL, "title"),
            "severity": "HIGH",
            "confidence": "high",
            "provenance": "new",
        }
        reply = self.reply("odd.json", {"proposed_findings": [odd]})
        result = self.run_script("proposals", self.prefix, "2", reply)
        self.assertEqual(
            self.records("batch-2.jsonl"),
            [{"label": "P1", "source": "odd.json", **odd, "title": ""}],
        )
        self.assertTrue(
            result.stdout.startswith(
                "### P1 — HIGH —\n\n- Source: odd.json\n"
                "- Confidence: high\n- Provenance: new\n"
            ),
            result.stdout,
        )

    def test_proposal_confidence_prints_as_given(self):
        """A missing confidence prints empty, and one not a number as its JSON."""
        cases = (
            ({**PROPOSAL, "confidence": True}, "true"),
            (without(PROPOSAL, "confidence"), ""),
        )
        for proposal, shown in cases:
            with self.subTest(shown=shown):
                reply = self.reply("c.json", {"proposed_findings": [proposal]})
                result = self.run_script("proposals", self.prefix, "2", reply)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertIn(f"- Confidence: {shown}\n", result.stdout)

    def test_proposal_text_is_text_or_empty(self):
        """A list of strings is its lines; a null or non-text value is empty."""
        odd = {**PROPOSAL, "observation": ["a", "b"], "location": None, "title": 5}
        reply = self.reply("odd.json", {"proposed_findings": [odd]})
        result = self.run_script("proposals", self.prefix, "2", reply)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        expected = {**PROPOSAL, "observation": "a\nb", "location": "", "title": ""}
        self.assertEqual(
            self.records("batch-2.jsonl"),
            [{"label": "P1", "source": "odd.json", **expected}],
        )

    def test_lone_proposal_joins_the_batch(self):
        """A proposal written without its list brackets is batched as one."""
        lone = self.reply("l.json", {"proposed_findings": PROPOSAL})
        result = self.run_script("proposals", self.prefix, "2", lone)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(
            self.records("batch-2.jsonl"),
            [{"label": "P1", "source": "l.json", **PROPOSAL}],
        )

    def test_proposal_text_becomes_its_observation(self):
        """A proposal written as text instead of an object still reaches dedup."""
        blank = dict.fromkeys(("title", "location", "suggested_action"), "")
        claims = {"severity": "", "confidence": None, "provenance": ""}
        for name, proposals in (("s.json", "Loop skips n."), ("l.json", ["x"])):
            reply = self.reply(name, {"proposed_findings": proposals})
            result = self.run_script("proposals", self.prefix, "2", reply)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(
            self.records("batch-2.jsonl"),
            [
                {
                    "label": "P1",
                    "source": "s.json",
                    **blank,
                    **claims,
                    "observation": "Loop skips n.",
                },
                {
                    "label": "P2",
                    "source": "l.json",
                    **blank,
                    **claims,
                    "observation": "x",
                },
            ],
        )

    def test_replies_without_proposals_add_none(self):
        """A reply with no usable proposal contributes nothing."""
        for proposals in ([5, ""], {}, " ", 5):
            with self.subTest(proposals=proposals):
                empty = self.reply("e.json", {"proposed_findings": proposals})
                result = self.run_script("proposals", self.prefix, "2", empty)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual(result.stdout, "")
        self.assertEqual(self.path("batch-2.jsonl").read_text(encoding="utf-8"), "")


class RequestsTest(RepliesTestCase):
    """`requests` prints a reply's experiment requests to run."""

    def test_prints_each_request_as_a_block(self):
        """The orchestrator reads requests without reading the rest."""
        reply = self.reply(
            "r.json", {"verdict": VERDICT, "experiment_requests": [REQUEST]}
        )
        result = self.run_script("requests", reply)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(
            result.stdout,
            "### EXP — does the loop reach n - 1\n\n- Supports: R1-001\n"
            "- Goal: confirm the skipped index\n"
            "- Procedure: run `python3 -c 'print(list(range(2)))'`\n"
            "- Confirms / Refutes: `[0, 1]` refutes\n",
        )

    def test_only_usable_requests_are_printed(self):
        """An unusable entry beside a usable one is left out, not fatal."""
        reply = self.reply("r.json", {"experiment_requests": ["junk", REQUEST]})
        result = self.run_script("requests", reply)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout.count("### EXP"), 1, result.stdout)

    def test_lone_request_object_prints_as_a_one_entry_list(self):
        """A request written without its list brackets is still read."""
        lone = self.reply("l.json", {"experiment_requests": REQUEST_ONLY})
        listed = self.reply("r.json", {"experiment_requests": [REQUEST_ONLY]})
        result = self.run_script("requests", lone)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, self.run_script("requests", listed).stdout)

    def test_unknown_request_fields_follow_the_known_ones(self):
        """A request's fields stay freeform, so none is hidden."""
        extra = {**REQUEST, "refutes": "`[0]` confirms"}
        reply = self.reply("r.json", {"experiment_requests": [extra]})
        result = self.run_script("requests", reply)
        self.assertIn(
            "- Confirms / Refutes: `[0, 1]` refutes\n- refutes: `[0]` confirms\n",
            result.stdout,
        )

    def test_listed_procedure_steps_print_one_per_line(self):
        """A procedure written as a list of steps reads as a multi-line one."""
        listed = {**REQUEST, "procedure": ["a", "b"]}
        reply = self.reply("r.json", {"experiment_requests": [listed]})
        result = self.run_script("requests", reply)
        self.assertIn("- Procedure:\n  a\n  b\n- Confirms / Refutes:", result.stdout)

    def test_multi_line_procedure_keeps_the_report_layout(self):
        """A request renders its fields as a kept finding does."""
        listed = {**REQUEST, "procedure": "1. a\n2. b"}
        reply = self.reply("r.json", {"experiment_requests": [listed]})
        result = self.run_script("requests", reply)
        self.assertIn(
            "- Procedure:\n  1. a\n  2. b\n- Confirms / Refutes:", result.stdout
        )


class AnalysisRecordCase(RepliesTestCase):
    """Record analyst replies for R1-001 and read the analyses file back.

    R1-001 to R1-003 are kept findings; R1-004 is dropped.
    """

    def setUp(self):
        super().setUp()
        kept = [with_id(VERDICT, finding) for finding in ("R1-001", "R1-002", "R1-003")]
        self.put_records("verdicts-1.jsonl", [*kept, with_id(DROP, "R1-004")])

    def analyses(self):
        """Return the analyses file's records."""
        return self.records("analyses.jsonl")

    def record(self, reply, *extra):
        """Record a JSON reply for R1-001."""
        path = self.reply("analysis.json", reply)
        return self.run_script("analysis-record", self.prefix, "R1-001", path, *extra)

    def reset(self):
        """Forget the analyses recorded so far."""
        self.path("analyses.jsonl").unlink()


class AnalysisRecordTest(AnalysisRecordCase):
    """`analysis-record` validates an analyst reply and records its outcome."""

    def test_analysis_body_is_appended(self):
        """A plain analysis is recorded with its body."""
        result = self.record({"analysis": ANALYSIS})
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, "R1-001: analysis recorded\n")
        self.assertEqual(self.analyses(), [ANALYSIS_RECORD])

    def test_correction_follows_its_body(self):
        """A correction is recorded right after the body it qualifies."""
        correction = {"severity": "CRITICAL", "rationale": "It corrupts data."}
        result = self.record({"analysis": ANALYSIS, "correction": correction})
        self.assertEqual(
            result.stdout, "R1-001: analysis recorded; corrected: severity\n"
        )
        self.assertEqual(
            self.analyses(),
            [ANALYSIS_RECORD, {"kind": "corrected", "id": "R1-001", **correction}],
        )

    def test_correction_spellings_are_recorded_canonically(self):
        """A corrected severity, confidence or provenance is stored canonically."""
        correction = {
            "severity": "critical",
            "confidence": 60.0,
            "provenance": "Pre-existing-On-Path",
            "rationale": "It corrupts data.",
        }
        result = self.record({"analysis": ANALYSIS, "correction": correction})
        self.assertEqual(
            result.stdout,
            "R1-001: analysis recorded; corrected: severity, confidence, provenance\n",
        )
        recorded = self.analyses()[1]
        self.assertEqual(
            recorded,
            {
                "kind": "corrected",
                "id": "R1-001",
                "severity": "CRITICAL",
                "confidence": 60,
                "provenance": "pre-existing-on-path",
                "rationale": "It corrupts data.",
            },
        )
        self.assertIs(type(recorded["confidence"]), int)

    def test_listed_text_is_read_as_lines(self):
        """A body, rejection or rationale written as a list of strings is its lines."""
        listed = {**ANALYSIS, "body": ["a", "b"]}
        correction = {"severity": "CRITICAL", "rationale": ["Data", "loss."]}
        self.record({"analysis": listed, "correction": correction})
        self.assertEqual(
            self.analyses(),
            [
                {**ANALYSIS_RECORD, "body": "a\nb"},
                {
                    **correction,
                    "kind": "corrected",
                    "id": "R1-001",
                    "rationale": "Data\nloss.",
                },
            ],
        )
        self.reset()
        result = self.record({"analysis": ANALYSIS, "rejection": ["x", "y"]})
        self.assertEqual(result.stdout, "R1-001: rejected\n")
        self.assertEqual(
            self.analyses(), [{"kind": "rejected", "id": "R1-001", "reason": "x\ny"}]
        )
        self.reset()
        empty = self.record({"analysis": {**ANALYSIS, "body": []}})
        self.assertEqual(empty.stdout, "invalid: lacks analysis for R1-001\n")

    def test_defective_correction_parts_are_dropped(self):
        """A correction without rationale is ignored; a bad field alone is dropped."""
        self.record({"analysis": ANALYSIS, "correction": {"severity": "CRITICAL"}})
        self.assertEqual(self.analyses(), [ANALYSIS_RECORD])
        self.reset()
        mixed = {"severity": "HIGH", "confidence": "60%", "rationale": "Rare input."}
        result = self.record({"analysis": ANALYSIS, "correction": mixed})
        self.assertEqual(
            result.stdout, "R1-001: analysis recorded; corrected: confidence\n"
        )
        self.assertEqual(
            self.analyses()[1],
            {
                "kind": "corrected",
                "id": "R1-001",
                "confidence": 60,
                "rationale": "Rare input.",
            },
        )

    def test_rejection_records_its_reason_only(self):
        """A rejection removes the finding: its reason replaces the body."""
        reason = "    The caller already pads `n` by one."
        correction = {"severity": "CRITICAL", "rationale": "It corrupts data."}
        reply = {"analysis": ANALYSIS, "rejection": reason, "correction": correction}
        result = self.record(reply)
        self.assertEqual(result.stdout, "R1-001: rejected\n")
        self.assertEqual(
            self.analyses(), [{"kind": "rejected", "id": "R1-001", "reason": reason}]
        )

    def test_recorded_outcomes_report_their_proposals(self):
        """An analyst's proposals are announced whatever the analysis decided."""
        cases = {
            "R1-001: analysis recorded; proposals: 1\n": {"analysis": ANALYSIS},
            "R1-001: rejected; proposals: 1\n": {
                "analysis": ANALYSIS,
                "rejection": "No.",
            },
        }
        for status, reply in cases.items():
            with self.subTest(status=status):
                result = self.record({**reply, "proposed_findings": [PROPOSAL]})
                self.assertEqual(result.stdout, status)
                self.reset()

    def test_deferral_and_alongside_shapes(self):
        """Requests alone defer; beside an analysis they hold it provisionally."""
        deferral = self.record({"experiment_requests": [REQUEST_ONLY]})
        self.assertEqual(deferral.stdout, "R1-001: deferral; experiment requests: 1\n")
        held = self.record(
            {"analysis": ANALYSIS, "experiment_requests": [REQUEST_ONLY]}
        )
        self.assertEqual(
            held.stdout, "R1-001: provisional analysis held; experiment requests: 1\n"
        )
        stub = self.record(
            {
                "analysis": {"id": "R1-001", "body": ""},
                "experiment_requests": [REQUEST_ONLY],
            }
        )
        self.assertEqual(stub.stdout, deferral.stdout)
        blank = self.record(
            {
                "analysis": ANALYSIS,
                "rejection": "",
                "experiment_requests": [REQUEST_ONLY],
            }
        )
        self.assertEqual(blank.stdout, held.stdout)
        self.assertFalse(self.path("analyses.jsonl").exists())

    def test_listed_procedure_steps_are_a_deferral(self):
        """A procedure written as a list of steps is a usable request."""
        steps = {**REQUEST_ONLY, "procedure": ["a", "b"]}
        result = self.record({"experiment_requests": [steps]})
        self.assertEqual(result.stdout, "R1-001: deferral; experiment requests: 1\n")

    def test_lone_request_object_holds_the_analysis(self):
        """A request written without its list brackets still asks for evidence."""
        held = self.record({"analysis": ANALYSIS, "experiment_requests": REQUEST_ONLY})
        self.assertEqual(
            held.stdout, "R1-001: provisional analysis held; experiment requests: 1\n"
        )
        self.assertFalse(self.path("analyses.jsonl").exists())

    def test_usable_requests_beside_unusable_ones_keep_their_shape(self):
        """A bad request beside a usable one changes neither shape."""
        mixed = {"experiment_requests": [REQUEST_ONLY, {"goal": "no procedure"}]}
        dropped = "; experiment requests: 1; unusable requests dropped: 1\n"
        self.assertEqual(self.record(mixed).stdout, "R1-001: deferral" + dropped)
        held = self.record({"analysis": ANALYSIS, **mixed})
        self.assertEqual(held.stdout, "R1-001: provisional analysis held" + dropped)
        self.assertFalse(self.path("analyses.jsonl").exists())

    def test_discarded_rejection_leaves_body_and_correction(self):
        """Ending an alongside loop, or the rule-3 fallback, keeps both."""
        correction = {"severity": "CRITICAL", "rationale": "It corrupts data."}
        reply = {
            "analysis": ANALYSIS,
            "correction": correction,
            "rejection": "Wrong.",
            "experiment_requests": [REQUEST_ONLY],
        }
        notes = {
            ("--provisional",): "",
            (): "; rejection discarded; malformed requests ignored",
        }
        for extra, note in notes.items():
            with self.subTest(extra=extra):
                result = self.record(reply, *extra)
                self.assertEqual(
                    result.stdout,
                    f"R1-001: analysis recorded; corrected: severity{note}\n",
                )
                corrected = {"kind": "corrected", "id": "R1-001", **correction}
                provisional = {**ANALYSIS_RECORD, "provisional": True}
                self.assertEqual(self.analyses(), [provisional, corrected])
                self.reset()

    def test_already_analyzed_finding_is_refused(self):
        """Analysis happens once per finding, a rejection included, whatever the reply."""
        rejected = {"kind": "rejected", "id": "R1-001", "reason": "Wrong."}
        cases = (
            ({"analysis": ANALYSIS}, "R1-001: analysis recorded\n", ANALYSIS_RECORD),
            (
                {"analysis": ANALYSIS, "rejection": "Wrong."},
                "R1-001: rejected\n",
                rejected,
            ),
        )
        for first, status, record in cases:
            self.assertEqual(self.record(first).stdout, status)
            again = self.reply("again.json", {"analysis": ANALYSIS})
            for reply in (again, str(self.dir / "absent.json")):
                with self.subTest(status=status, reply=reply):
                    result = self.run_script(
                        "analysis-record", self.prefix, "R1-001", reply
                    )
                    self.assertEqual(result.returncode, 2, result.stdout)
                    self.assertEqual(
                        result.stdout, "refused: R1-001 is already analyzed\n"
                    )
            self.assertEqual(self.analyses(), [record])
            self.reset()

    def test_skip_marks_retry_exhaustion(self):
        """An exhausted finding counts as analyzed, with no body."""
        result = self.run_script("analysis-skip", self.prefix, "R1-001")
        self.assertEqual(result.stdout, "R1-001: analysis skipped\n")
        self.assertEqual(self.analyses(), [{"kind": "skipped", "id": "R1-001"}])
        self.assertEqual(self.record({"analysis": ANALYSIS}).returncode, 2)

    def test_finding_without_a_kept_verdict_is_refused(self):
        """Only a kept finding is analyzed or skipped, a dropped or unknown one not."""
        for finding in ("R1-004", "R1-009"):
            analysis = {"analysis": with_id(ANALYSIS, finding)}
            reply = self.reply("analysis.json", analysis)
            absent = str(self.dir / "absent.json")
            commands = {
                "analysis-record": ["analysis-record", self.prefix, finding, reply],
                "absent reply": ["analysis-record", self.prefix, finding, absent],
                "analysis-skip": ["analysis-skip", self.prefix, finding],
            }
            for name, args in commands.items():
                with self.subTest(finding=finding, command=name):
                    result = self.run_script(*args)
                    self.assertEqual(
                        result.returncode, 2, result.stdout + result.stderr
                    )
                    self.assertEqual(
                        result.stdout, f"refused: {finding} has no kept verdict\n"
                    )
                    self.assertFalse(self.path("analyses.jsonl").exists())


class AnalysisReplyShapeTest(AnalysisRecordCase):
    """`analysis-record` handles malformed replies by its validity rules."""

    def test_malformed_replies_are_invalid(self):
        """Each analysis validity rule rejects its malformation, writing nothing."""
        lacks = "lacks analysis for R1-001"
        cases = (
            (lacks, {"analysis": with_id(ANALYSIS, "R1-002")}),
            (lacks, {"analysis": {"id": "R1-001", "body": " "}}),
            (lacks, {"analysis": {"id": "R1-001", "body": ""}, "rejection": " "}),
            (
                "R1-001 rejects and requests experiments",
                {
                    "analysis": {"id": "R1-001", "body": ""},
                    "rejection": "Wrong.",
                    "experiment_requests": [REQUEST_ONLY],
                },
            ),
        )
        for reason, reply in cases:
            with self.subTest(reason=reason):
                result = self.record(reply)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, f"invalid: {reason}\n")
                self.assertFalse(self.path("analyses.jsonl").exists())

    def test_usable_body_survives_malformed_requests(self):
        """A body beside contradictory or unusable requests is its own fallback."""
        ignored = "; malformed requests ignored"
        discarded = "; rejection discarded" + ignored
        unusable = [{"goal": "no procedure"}]
        malformed = (
            ({"rejection": "Wrong.", "experiment_requests": [REQUEST_ONLY]}, discarded),
            ({"rejection": "Wrong.", "experiment_requests": unusable}, discarded),
            ({"experiment_requests": unusable}, ignored),
        )
        for extra, note in malformed:
            with self.subTest(extra=extra):
                result = self.record({"analysis": ANALYSIS, **extra})
                self.assertEqual(result.stdout, f"R1-001: analysis recorded{note}\n")
                provisional = {**ANALYSIS_RECORD, "provisional": True}
                self.assertEqual(self.analyses(), [provisional])
                self.reset()

    def test_rejection_without_a_reason_is_no_rejection(self):
        """Only a reason rejects; `false`, `[]` or a blank cannot remove a finding."""
        for value in (False, 0, True, [], {}, "", "  ", {"reason": " "}):
            with self.subTest(value=value):
                result = self.record({"analysis": ANALYSIS, "rejection": value})
                self.assertEqual(result.stdout, "R1-001: analysis recorded\n")
                self.assertEqual(self.analyses(), [ANALYSIS_RECORD])
                self.reset()
        correction = {"severity": "CRITICAL", "rationale": "It corrupts data."}
        result = self.record(
            {"analysis": ANALYSIS, "rejection": False, "correction": correction}
        )
        self.assertEqual(
            result.stdout, "R1-001: analysis recorded; corrected: severity\n"
        )

    def test_rejection_object_rejects_by_its_reason(self):
        """A rejection written as an object carries its `reason` or `rationale`."""
        rejected = [{"kind": "rejected", "id": "R1-001", "reason": "Wrong."}]
        for key in ("reason", "rationale"):
            for analysis in (ANALYSIS, {"id": "R1-001", "body": ""}):
                with self.subTest(key=key, analysis=analysis):
                    reply = {"analysis": analysis, "rejection": {key: "Wrong."}}
                    self.assertEqual(self.record(reply).stdout, "R1-001: rejected\n")
                    self.assertEqual(self.analyses(), rejected)
                    self.reset()

    def test_rejection_body_may_be_empty_missing_or_null(self):
        """A rejection's reason carries it, whatever the body says."""
        for analysis in (
            {"id": "R1-001", "body": ""},
            {"id": "R1-001"},
            {"id": "R1-001", "body": None},
        ):
            with self.subTest(analysis=analysis):
                result = self.record({"analysis": analysis, "rejection": "Wrong."})
                self.assertEqual(result.stdout, "R1-001: rejected\n")
                self.assertEqual(
                    self.analyses(),
                    [{"kind": "rejected", "id": "R1-001", "reason": "Wrong."}],
                )
                self.reset()

    def test_markdown_in_the_body_is_kept_verbatim(self):
        """A body is a string, so nothing in it can split or forge a record."""
        body = BODY + "\n\n## Rejection\n\n<!-- analysis-rejected: R1-002 -->\n```sh\n"
        result = self.record({"analysis": {"id": "R1-001", "body": body}})
        self.assertEqual(result.stdout, "R1-001: analysis recorded\n")
        self.assertEqual(self.analyses(), [{**ANALYSIS_RECORD, "body": body}])


class EmptyListsTest(AnalysisRecordCase):
    """An empty optional list, in any spelling, means the same as leaving it out."""

    def test_empty_requests_are_no_requests(self):
        """No command reads empty requests as malformed."""
        empty_body = {"id": "R1-001", "body": ""}
        rejection = {"rejection": "Wrong."}
        cases = (
            (["draft-check", "1"], {"findings": [FINDING]}, "valid: R1-001\n"),
            (
                ["analysis-record", self.prefix, "R1-001"],
                {"analysis": ANALYSIS},
                "R1-001: analysis recorded\n",
            ),
            (
                ["analysis-record", self.prefix, "R1-002"],
                {"analysis": with_id(empty_body, "R1-002"), **rejection},
                "R1-002: rejected\n",
            ),
            (
                ["analysis-record", self.prefix, "R1-003"],
                {"analysis": with_id(ANALYSIS, "R1-003"), **rejection},
                "R1-003: rejected\n",
            ),
            (["requests"], {}, ""),
        )
        for empty in (None, {}, [], "", False):
            for args, reply, status in cases:
                with self.subTest(empty=empty, command=args[0], status=status):
                    path = self.reply("r.json", {**reply, "experiment_requests": empty})
                    result = self.run_script(*args, path)
                    self.assertEqual(
                        result.returncode, 0, result.stdout + result.stderr
                    )
                    self.assertEqual(result.stdout, status, result.stderr)
            self.reset()
        malformed = self.reply("m.json", {"experiment_requests": "x"})
        self.assertEqual(
            self.run_script("requests", malformed).stdout,
            "invalid: experiment_requests is not a list\n",
        )


class PendingAnalysisTest(RepliesTestCase):
    """`pending-analysis` lists kept findings that still need an analyst."""

    def test_topic_with_glob_characters_reads_its_own_verdicts(self):
        """The prefix names files literally, so `[1]` is no character class."""
        self.prefix = str(self.dir / "inspect-changes-foo[1]")
        self.put_records("verdicts-1.jsonl", [VERDICT])
        result = self.run_script("pending-analysis", self.prefix)
        self.assertEqual(result.stdout, f"R1-001 {self.prefix}-verdict-R1-001.json\n")

    def test_lists_unanalyzed_keeps_with_their_verdict_file(self):
        """Drops and analyzed, rejected or skipped findings are not pending."""
        self.put_records("verdicts-1.jsonl", [VERDICT, with_id(DROP, "R1-002")])
        self.put_records(
            "verdicts-2.jsonl",
            [with_id(VERDICT, finding) for finding in ("R2-001", "R2-002", "R2-003")],
        )
        self.put_records(
            "analyses.jsonl",
            [
                ANALYSIS_RECORD,
                {"kind": "rejected", "id": "R2-002", "reason": "Wrong."},
                {"kind": "skipped", "id": "R2-003"},
            ],
        )
        result = self.run_script("pending-analysis", self.prefix)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, f"R2-001 {self.prefix}-verdict-R2-001.json\n")

    def test_nothing_pending_says_none(self):
        """A review with every keep analyzed has nothing pending."""
        result = self.run_script("pending-analysis", self.prefix)
        self.assertEqual(result.stdout, "none\n")


if __name__ == "__main__":
    unittest.main()
