"""Shared fixtures and the subprocess harness for the inspect-changes script tests."""

import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT = (
    Path(__file__).resolve().parents[2]
    / "ai/.claude/skills/inspect-changes/scripts/replies.py"
)

FINDING = {
    "id": "R1-001",
    "severity": "IMPORTANT",
    "title": "Loop skips the last element",
    "confidence": 80,
    "provenance": "introduced",
    "location": "`src/a.py:10`",
    "observation": "`range(n - 1)` never visits index `n - 1`.",
    "suggested_action": "Use `range(n)`.",
}

PROPOSAL = {
    "severity": "SUGGESTION",
    "title": "Helper name hides its side effect",
    "confidence": 60,
    "provenance": "pre-existing-off-path",
    "location": "`src/b.py:3`",
    "observation": "`get_config` writes a cache file.",
    "suggested_action": "Rename it to `load_and_cache_config`.",
}

REQUEST = {
    "title": "does the loop reach n - 1",
    "supports": "R1-001",
    "goal": "confirm the skipped index",
    "procedure": "run `python3 -c 'print(list(range(2)))'`",
    "confirms_refutes": "`[0, 1]` refutes",
}

VERDICT = {
    "id": "R1-001",
    "outcome": "keep",
    "final_severity": "IMPORTANT",
    "final_confidence": 80,
    "final_provenance": "introduced",
    "final_title": "Loop skips the last element",
    "final_location": "`src/a.py:10`",
    "final_observation": "`range(n - 1)` never visits index `n - 1`.",
    "final_suggested_action": "Use `range(n)`.",
    "verification_trace": "Read the loop bounds.",
}

DROP = {
    "id": "R1-001",
    "outcome": "drop",
    "final_confidence": 30,
    "verification_trace": "Read the caller.",
    "reason": "`n` already counts one past the end.",
}

BODY = (
    "**Restated critique:** The loop stops one element short.\n\n"
    "**Recommendation:** Use `range(n)`."
)


ANALYSIS_RECORD = {"kind": "analysis", "id": "R1-001", "body": BODY}


def without(record, *keys):
    """Return a copy of a fixture without some of its fields."""
    return {key: value for key, value in record.items() if key not in keys}


REQUEST_ONLY = without(REQUEST, "supports")


def read_records(path):
    """Return a JSON Lines file's records, split at LF as the script splits them."""
    text = Path(path).read_text(encoding="utf-8")
    return [json.loads(line) for line in text.split("\n") if line]


def with_id(record, finding):
    """Return a copy of a fixture under another finding ID."""
    return {**record, "id": finding}


class RepliesTestCase(unittest.TestCase):
    """Run the script against reply files in a scratch directory."""

    def setUp(self):
        self.dir = Path(tempfile.mkdtemp(prefix="inspect-changes-", dir="/tmp"))
        self.addCleanup(shutil.rmtree, self.dir)
        self.prefix = str(self.dir / "inspect-changes-topic")

    def path(self, suffix):
        """Return the path of one of the review's files under the topic prefix."""
        return Path(f"{self.prefix}-{suffix}")

    def put(self, suffix, text):
        """Write one of the review's files under the topic prefix."""
        self.path(suffix).write_text(text, encoding="utf-8")

    def put_records(self, suffix, records):
        """Write a JSON Lines review file under the topic prefix."""
        self.put(suffix, "".join(json.dumps(record) + "\n" for record in records))

    def records(self, suffix):
        """Return a JSON Lines review file's records."""
        return read_records(self.path(suffix))

    def verdict_copy(self, finding):
        """Return the verdict copy written for a finding's analyst."""
        return json.loads(
            self.path(f"verdict-{finding}.json").read_text(encoding="utf-8")
        )

    def write(self, name, text):
        """Write a scratch file and return its path."""
        path = self.dir / name
        path.write_text(text, encoding="utf-8")
        return str(path)

    def reply(self, name, reply):
        """Write a JSON reply file and return its path."""
        return self.write(name, json.dumps(reply))

    def run_script(self, *args, env=None):
        """Run the script and return the completed process."""
        return subprocess.run(
            [sys.executable, str(SCRIPT), *args],
            capture_output=True,
            text=True,
            check=False,
            env=env,
        )
