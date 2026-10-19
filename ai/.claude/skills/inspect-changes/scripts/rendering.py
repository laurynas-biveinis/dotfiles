"""Markdown rendering for the inspect-changes report and the orchestrator's views."""

import json
import re

# CommonMark: a fence is indented at most three spaces (a tab reaches column
# 4), and a backtick fence's info string holds no backtick, so a line led by a
# code span such as ```` ```sh ```` is not an opener.
FENCE = re.compile(r"^ {0,3}(`{3,}(?!.*`)|~{3,})")
LIST_MARKER = re.compile(r"^ {0,3}(?:[-+*]|\d{1,9}[.)])(?= )")
LINE_END = re.compile(r"\r\n|\r|\n")
CONFIDENCE_NOTE = (
    "Confidence is how strongly the evidence establishes that what the observation\n"
    "describes is wrong, as described, at that location — not how much it would cost\n"
    "if it is (the severity section it sits in says that), nor whether this change\n"
    "caused it (Provenance says that).\n"
)
PROVENANCE_NOTE = (
    "Provenance is relative to the pre-image of the reviewed scope, which may itself\n"
    "be your own unpushed commit: introduced = this change caused it;\n"
    "pre-existing-on-path = present in the pre-image and on this change's critical\n"
    "path; pre-existing-off-path = present in the pre-image and independent of this\n"
    "change — see any Suggested placement for where a fix belongs.\n"
)
MERGE_NOTES = {
    "combined": "Reviewed as a combined diff, every parent listed above is a"
    " pre-image, and pre-existing means present on any of them.\n",
    "single-parent": "Reviewed against one parent, the Baseline above is the sole"
    " pre-image, and pre-existing means present on it.\n",
}
TIERS = (
    ("CRITICAL", "Critical Issues"),
    ("IMPORTANT", "Important Findings"),
    ("SUGGESTION", "Suggestions"),
)


def claim_text(value):
    """Return a claim as given, as text: null as empty, a non-string as its JSON."""
    if value is None:
        return ""
    return value if isinstance(value, str) else json.dumps(value, ensure_ascii=False)


def confidence_text(value):
    """Render a confidence for Markdown."""
    number = isinstance(value, int) and not isinstance(value, bool)
    return f"{value}%" if number else claim_text(value)


def field(label, value):
    """Render a field as one bullet, a multi-line value below its label.

    CommonMark ends a list item at a line indented less than its content,
    closing any fence left open in it, so the indentation keeps a value's
    paragraphs, lists and fences inside its bullet. Starting the value on its
    own line lets a first line that can interrupt a paragraph, such as a fence
    or a list starting at 1, open its block. Any other first line continues
    the label's paragraph, or underlines it as a heading if it is a run of `-`
    or `=`; values seldom start so.
    """
    lines = LINE_END.split(value)
    if len(lines) == 1:
        return f"- {label}: {value}"
    indented = [f"  {line}" if line.strip() else "" for line in lines]
    return "\n".join([f"- {label}:", *indented])


def block(heading, bullets):
    """Render a `###` heading kept to one line, then a `field()` bullet per pair."""
    lines = ["### " + " ".join(heading.split()), ""]
    lines += [field(label, value) for label, value in bullets]
    return "\n".join(lines) + "\n"


def render_proposal(record):
    """Render a batch proposal as a Markdown block for dedup."""
    return block(
        f"{record['label']} — {record['severity']} — {record['title']}",
        [
            ("Source", record["source"]),
            ("Confidence", confidence_text(record["confidence"])),
            ("Provenance", record["provenance"]),
            ("Location", record["location"]),
            ("Observation", record["observation"]),
            ("Suggested action", record["suggested_action"]),
        ],
    )


def joined(value):
    """Return a list of strings as one line each, any other value unchanged."""
    if isinstance(value, list) and all(isinstance(item, str) for item in value):
        return "\n".join(value)
    return value


def render_request(entry):
    """Render one experiment request as a Markdown block for the orchestrator."""
    labels = {
        "supports": "Supports",
        "goal": "Goal",
        "procedure": "Procedure",
        "confirms_refutes": "Confirms / Refutes",
    }
    known = [
        (name, str(joined(entry[key]))) for key, name in labels.items() if key in entry
    ]
    extra = [
        (key, str(joined(value)))
        for key, value in entry.items()
        if key not in labels and key != "title"
    ]
    return block(f"EXP — {entry.get('title', '')}", known + extra)


def indent(text):
    """Return text's leading space-and-tab width, as CommonMark counts it.

    A tab advances to the next four-column stop.
    """
    expanded = text.expandtabs(4)
    return len(expanded) - len(expanded.lstrip(" "))


def fence_after(fence, line):
    """Return the fence open after line, given the one open before it.

    A fence is its opener's indentation and backtick or tilde run.
    """
    # A list item, and any fence in it, ends at a line indented less than the
    # item's content. The opener's indentation stands in for that column, so a
    # top-level fence indented one to three spaces is taken to end at its
    # first less-indented line: a closed one gains an appended opener, and an
    # open one gets no closer. Fence lines are matched from the margin, not the
    # item's column, so one four or more columns in is missed. Bodies seldom
    # hold either.
    if fence and line.strip(" \t") and indent(line) < indent(fence):
        fence = None
    if not fence:
        # Blanking the marker tracks a fence opened on an item's first line. A
        # nested item's opener lands past FENCE's three spaces, where its closer
        # is out of reach too, so that fence stays untracked rather than open.
        unmarked = LIST_MARKER.sub(lambda m: " " * len(m[0]), line, count=1)
        match = FENCE.match(unmarked)
        return match.group(0) if match else None
    match = FENCE.match(line)
    run = fence.lstrip()
    closer = match.group(1) if match else ""
    closes = (
        closer[:1] == run[0]
        and len(closer) >= len(run)
        and not line[match.end() :].strip(" \t")
    )
    return None if closes else fence


def close_fence(text):
    """Close a fence left open at the end of rendered text.

    The report concatenates findings, so an open fence would turn every later
    one into code. Fence lines inside a raw HTML block count as fences, though
    CommonMark parses none there: renderers with raw HTML disabled agree, and
    bodies seldom hold raw HTML.
    """
    fence = None
    for line in LINE_END.split(text):
        fence = fence_after(fence, line)
    return text if fence is None else text.rstrip("\n") + "\n" + fence + "\n"


def render_finding(record, body):
    """Render one kept finding with its analysis body, if any."""
    finding = record["id"]
    text = block(
        f"{finding} — {record['final_severity']} — {record['final_title']}",
        [
            ("Confidence", confidence_text(record["final_confidence"])),
            ("Provenance", record["final_provenance"]),
            ("Location", record["final_location"]),
            ("Observation", record["final_observation"]),
            ("Suggested action", record["final_suggested_action"]),
        ],
    )
    if body is not None:
        trimmed = body.strip("\n")
        text += "\n" + close_fence(f"#### Analysis: {finding}\n\n{trimmed}\n")
    return text
