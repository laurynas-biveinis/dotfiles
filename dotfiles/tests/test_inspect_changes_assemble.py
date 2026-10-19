"""Tests for the inspect-changes script's report assembly, run as a subprocess."""

import unittest
from pathlib import Path

from inspect_changes_replies_test_utils import (
    ANALYSIS_RECORD,
    BODY,
    DROP,
    FINDING,
    VERDICT,
    RepliesTestCase,
    with_id,
)

RENDERED = (
    "### R1-001 — IMPORTANT — Loop skips the last element\n\n"
    "- Confidence: 80%\n- Provenance: introduced\n- Location: `src/a.py:10`\n"
    "- Observation: `range(n - 1)` never visits index `n - 1`.\n"
    "- Suggested action: Use `range(n)`.\n\n#### Analysis: R1-001\n\n" + BODY + "\n"
)


def verdict(finding, severity, provenance):
    """Return a keep verdict under another ID, severity and provenance."""
    return {
        **with_id(VERDICT, finding),
        "final_severity": severity,
        "final_provenance": provenance,
    }


class AssembleTest(RepliesTestCase):
    """`assemble` renders the final report from the recorded review files."""

    def setUp(self):
        super().setUp()
        self.put("summary.md", "Prose.\n")
        self.put("scope.txt", "git show abc\n")
        self.put("baseline.txt", "HEAD~ (def)\n")

    def assemble(self, *extra, status="converged"):
        """Assemble the report and return its text."""
        result = self.run_script("assemble", self.prefix, "--status", status, *extra)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, f"wrote {self.prefix}.md\n")
        return Path(self.prefix + ".md").read_text(encoding="utf-8")

    def test_report_layout_with_one_analyzed_finding(self):
        """Header, severity tiers with None. placeholders, the finding, Summary."""
        self.put_records("verdicts-1.jsonl", [VERDICT])
        self.put_records("analyses.jsonl", [ANALYSIS_RECORD])
        report = self.assemble()
        self.assertTrue(
            report.startswith(
                "# Code Review: topic\n\nStatus: converged\n\nScope: git show abc\n\n"
                f"Review basis: {self.prefix}-state.md\n\nBaseline: HEAD~ (def)\n\n"
            ),
            report,
        )
        self.assertEqual(
            [line for line in report.splitlines() if line.startswith("#")],
            [
                "# Code Review: topic",
                "## Critical Issues",
                "## Important Findings",
                "### R1-001 — IMPORTANT — Loop skips the last element",
                "#### Analysis: R1-001",
                "## Suggestions",
                "## Summary",
            ],
        )
        self.assertIn(
            "## Critical Issues\n\nNone.\n\n## Important Findings\n\n", report
        )
        self.assertIn(
            RENDERED + "\n## Suggestions\n\nNone.\n\n## Summary\n\nProse.\n", report
        )

    def test_overlay_exclusion_and_ordering(self):
        """Corrections regroup, rejections vanish, off-path sorts last per tier."""
        self.put_records(
            "verdicts-1.jsonl",
            [
                verdict("R1-001", "IMPORTANT", "introduced"),
                verdict("R1-002", "SUGGESTION", "pre-existing-off-path"),
                verdict("R1-003", "IMPORTANT", "introduced"),
            ],
        )
        self.put_records(
            "verdicts-2.jsonl", [verdict("R2-001", "SUGGESTION", "introduced")]
        )
        self.put_records(
            "analyses.jsonl",
            [
                ANALYSIS_RECORD,
                {
                    "kind": "corrected",
                    "id": "R1-001",
                    "severity": "CRITICAL",
                    "confidence": 95,
                    "rationale": "Data loss.",
                },
                {"kind": "rejected", "id": "R1-003", "reason": "The caller pads `n`."},
            ],
        )
        report = self.assemble(status="incomplete")
        self.assertEqual(
            [line[:10] for line in report.splitlines() if line.startswith("#")],
            [
                "# Code Rev",
                "## Critica",
                "### R1-001",
                "#### Analy",
                "## Importa",
                "## Suggest",
                "### R2-001",
                "### R1-002",
                "## Summary",
            ],
        )
        self.assertIn(
            "### R1-001 — CRITICAL — Loop skips the last element\n\n"
            "- Confidence: 95%\n- Provenance: introduced\n",
            report,
        )

    def test_summary_appends_counts_from_the_files(self):
        """The orchestrator's prose is followed by counts the files settle."""
        self.put_records("draft-1.jsonl", [FINDING, with_id(FINDING, "R1-002")])
        self.put_records(
            "draft-2.jsonl", [with_id(FINDING, "R2-001"), with_id(FINDING, "R2-002")]
        )
        self.put_records("verdicts-1.jsonl", [VERDICT, with_id(DROP, "R1-002")])
        self.put_records(
            "verdicts-2.jsonl",
            [
                verdict("R2-001", "IMPORTANT", "pre-existing-off-path"),
                verdict("R2-002", "SUGGESTION", "introduced"),
            ],
        )
        self.put_records(
            "analyses.jsonl",
            [
                {**ANALYSIS_RECORD, "provisional": True},
                {
                    "kind": "corrected",
                    "id": "R1-001",
                    "severity": "CRITICAL",
                    "rationale": "Data loss.",
                },
                {"kind": "skipped", "id": "R2-001"},
                {"kind": "rejected", "id": "R2-002", "reason": "Wrong."},
            ],
        )
        report = self.assemble(status="incomplete")
        self.assertTrue(
            report.endswith(
                "## Summary\n\nProse.\n\nCounts: drafted 4; kept 3; dropped 1; "
                "rejected on analysis: R2-002; "
                "analyzed 2; analyses skipped: R2-001; provisional analyses: R1-001; "
                "listed per provenance: introduced 1 / pre-existing-on-path 0 / "
                "pre-existing-off-path 1; analysis corrections: "
                "R1-001: severity IMPORTANT → CRITICAL.\n"
            ),
            report,
        )

    def test_summary_lists_the_logged_limitations(self):
        """The limitations file's entries follow the prose; a blank file adds none."""
        cases = (
            ("- Drift.\n- Cap.\n", "Completion limitations:\n\n- Drift.\n- Cap.\n\n"),
            (" \n", ""),
        )
        for logged, block in cases:
            with self.subTest(logged=logged):
                self.put("limitations.md", logged)
                self.assertIn(
                    f"## Summary\n\nProse.\n\n{block}Counts: ",
                    self.assemble(status="incomplete"),
                )

    def test_summary_omits_correction_fields_restating_the_verdict(self):
        """A correction field equal to the verdict's changes nothing."""
        self.put_records("draft-1.jsonl", [FINDING])
        self.put_records("verdicts-1.jsonl", [VERDICT])
        cases = (
            ({"confidence": 60}, "R1-001: confidence 80% → 60%"),
            ({"confidence": 80}, "none"),
        )
        restated = {"kind": "corrected", "id": "R1-001", "severity": "IMPORTANT"}
        for fields, changes in cases:
            with self.subTest(fields=fields):
                correction = {**restated, **fields, "rationale": "Rare input."}
                self.put_records("analyses.jsonl", [ANALYSIS_RECORD, correction])
                self.assertIn(f"; analysis corrections: {changes}.\n", self.assemble())

    def test_converged_is_refused_while_the_files_record_a_limitation(self):
        """A logged limitation, or a skipped or provisional analysis, rules it out."""
        self.put_records("verdicts-1.jsonl", [VERDICT])
        skipped = [{"kind": "skipped", "id": "R1-001"}]
        provisional = [{**ANALYSIS_RECORD, "provisional": True}]
        report = Path(self.prefix + ".md")
        cases = (("- Drift.\n", [ANALYSIS_RECORD]), ("", skipped), ("", provisional))
        for limitations, analyses in cases:
            with self.subTest(limitations=limitations, analyses=analyses):
                self.put("limitations.md", limitations)
                self.put_records("analyses.jsonl", analyses)
                result = self.run_script(
                    "assemble", self.prefix, "--status", "converged"
                )
                self.assertEqual(result.returncode, 2, result.stderr)
                self.assertEqual(
                    result.stdout,
                    "refused: converged, but the review files record a "
                    "completion limitation\n",
                )
                self.assertFalse(report.exists())
                self.assertTrue(
                    self.assemble(status="incomplete").startswith(
                        "# Code Review: topic\n\nStatus: incomplete\n"
                    )
                )
                report.unlink()

    def test_converged_is_refused_while_a_finding_awaits_its_verdict_or_analysis(
        self,
    ):
        """A drafted finding without a verdict, or a kept one unanalyzed, is pending."""
        report = Path(self.prefix + ".md")
        cases = (
            (
                [FINDING, with_id(FINDING, "R1-002")],
                [ANALYSIS_RECORD],
                "no verdict: R1-002",
            ),
            ([FINDING], [], "no analysis record: R1-001"),
            (
                [FINDING, with_id(FINDING, "R1-002")],
                [],
                "no verdict: R1-002; no analysis record: R1-001",
            ),
        )
        self.put_records("verdicts-1.jsonl", [VERDICT])
        for drafted, analyses, pending in cases:
            with self.subTest(pending=pending):
                self.put_records("draft-1.jsonl", drafted)
                self.put_records("analyses.jsonl", analyses)
                result = self.run_script(
                    "assemble", self.prefix, "--status", "converged"
                )
                self.assertEqual(result.returncode, 2, result.stderr)
                self.assertEqual(result.stdout, f"refused: converged, but {pending}\n")
                self.assertFalse(report.exists())
                self.assemble(status="incomplete")
                report.unlink()

    def test_status_is_converged_or_incomplete(self):
        """No other status belongs in an assembled report."""
        result = self.run_script("assemble", self.prefix, "--status", "aborted")
        self.assertEqual(result.returncode, 2, result.stdout)
        self.assertFalse(Path(self.prefix + ".md").exists())

    def test_scope_and_baseline_files_are_printed_verbatim(self):
        """Header values with shell quotes reach the report unchanged."""
        scope = "git diff --staged --output-indicator-context=' ' abc"
        baseline = "o'neil's-fix (abc)"
        self.put("scope.txt", scope + "\n")
        self.put("baseline.txt", baseline + "\n")
        self.assertIn(
            f"Scope: {scope}\n\nReview basis: {self.prefix}-state.md\n\n"
            f"Baseline: {baseline}\n\n",
            self.assemble(),
        )

    def test_missing_input_file_is_refused(self):
        """The scope, baseline and summary files must exist before assembly."""
        report = Path(self.prefix + ".md")
        for suffix in ("scope.txt", "baseline.txt", "summary.md"):
            with self.subTest(suffix=suffix):
                text = self.path(suffix).read_text(encoding="utf-8")
                self.path(suffix).unlink()
                result = self.run_script(
                    "assemble", self.prefix, "--status", "incomplete"
                )
                self.assertEqual(result.returncode, 2, result.stderr)
                self.assertEqual(
                    result.stdout, f"refused: {self.path(suffix)} is missing\n"
                )
                self.assertFalse(report.exists())
                self.put(suffix, text)

    def test_merge_shape_prints_its_fixed_note(self):
        """A merge scope's baseline caveat is a fixed sentence per shape."""
        notes = {
            "combined": "Reviewed as a combined diff, every parent listed above is a"
            " pre-image, and pre-existing means present on any of them.",
            "single-parent": "Reviewed against one parent, the Baseline above is the"
            " sole pre-image, and pre-existing means present on it.",
        }
        for shape, note in notes.items():
            with self.subTest(shape=shape):
                self.assertIn(
                    "for where a fix belongs.\n\n" + note + "\n\n## Critical Issues\n",
                    self.assemble("--merge", shape),
                )

    def test_rendered_text_keeps_the_outline(self):
        """A wrapped title stays one heading; an open fence ends with its finding."""
        wrapped = {**VERDICT, "final_title": "Loop skips\n  the last element"}
        self.put_records("verdicts-1.jsonl", [wrapped, with_id(VERDICT, "R1-002")])
        open_fence = {**ANALYSIS_RECORD, "body": BODY + "\n\n```sh\nrm -rf build"}
        self.put_records("analyses.jsonl", [open_fence])
        report = self.assemble(status="incomplete")
        self.assertIn("### R1-001 — IMPORTANT — Loop skips the last element\n", report)
        self.assertIn("```sh\nrm -rf build\n```\n\n### R1-002 — IMPORTANT", report)

    def test_body_gets_the_closer_its_open_fence_needs(self):
        """Each body tail maps to the closer appended after it, if any."""
        self.put_records("verdicts-1.jsonl", [VERDICT, with_id(VERDICT, "R1-002")])
        closers = {
            "```sh\nrm -rf build": "```",
            "```sh``` names the shell.": "",
            "```sh\nrm -rf build\n```": "",
            "````markdown\n```sh\nrm -rf build\n```\n````": "",
            "~~~sh\nrm -rf build\n```": "~~~",
            "```sh\nrm -rf build\n``` done": "```",
            "- Item:\n\n  ```sh\n  rm -rf build": "  ```",
            "- Item:\n\n  ```sh\n  rm -rf build\n\nTail.": "",
            "- ```sh\n  make\n  ```\n\nMore text.": "",
            "see\f```": "",
            "```sh\nrm -rf build\nx\f```": "```",
            "```sh\nrm -rf build\n\f```\nmore": "```",
            "```sh\nrm -rf build\n```\f\nmore": "```",
            "```sh\nrm -rf build\n\u00a0```\nmore": "```",
            "```sh\nrm -rf build\n\t```\nmore": "```",
            "```\n     ```\n```": "",
            "```markdown\n     ```bash\n     git commit\n     ```\n```": "",
            "- Item:\n\n  ```sh\n  make\n\u00a0\u00a0done": "",
            "- Item:\n\n  ```sh\n  make\n\u00a0\n  ```": "  ```",
            "- ```sh\n  make\n  ```": "",
            "1. ```sh\n   make\n   ```": "",
            "  - ```sh\n    make\n    ```": "",
            "10. ```sh\n    make\n    ```": "",
        }
        for tail, closer in closers.items():
            with self.subTest(tail=tail):
                body = BODY + "\n\n" + tail
                self.put_records("analyses.jsonl", [{**ANALYSIS_RECORD, "body": body}])
                appended = "\n" + closer if closer else ""
                report = self.assemble(status="incomplete")
                self.assertIn(tail + appended + "\n\n### R1-002", report)

    def test_body_keeps_its_first_line_indentation(self):
        """A body opening with indented code or an indented fence stays so."""
        for body in ("    $ make\n    ok", "  ```sh\n  make\n  ```"):
            with self.subTest(body=body):
                record = {**ANALYSIS_RECORD, "body": "\n" + body + "\n"}
                self.put_records("verdicts-1.jsonl", [VERDICT])
                self.put_records("analyses.jsonl", [record])
                expected = f"#### Analysis: R1-001\n\n{body}\n\n## Suggestions"
                self.assertIn(expected, self.assemble())

    def test_multi_line_fields_stay_inside_their_bullet(self):
        """A multi-line field starts below its label, indented into the item."""
        cases = {
            "a\n\n- b\n  - c\n\nd": (
                "- Observation:\n  a\n\n  - b\n    - c\n\n  d\n- Suggested action:"
            ),
            "a\n\n```sh\nrm -rf build": (
                "- Observation:\n  a\n\n  ```sh\n  rm -rf build\n"
                "- Suggested action: Use `range(n)`.\n\n## Suggestions"
            ),
            "a\r```sh": "- Observation:\n  a\n  ```sh\n- Suggested action:",
            "```diff\n- old\n+ new\n```": (
                "- Observation:\n  ```diff\n  - old\n  + new\n  ```\n- Suggested action:"
            ),
            "1. a\n2. b": "- Observation:\n  1. a\n  2. b\n- Suggested action:",
        }
        for observation, expected in cases.items():
            with self.subTest(observation=observation):
                self.put_records(
                    "verdicts-1.jsonl", [{**VERDICT, "final_observation": observation}]
                )
                self.assertIn(expected, self.assemble(status="incomplete"))


if __name__ == "__main__":
    unittest.main()
