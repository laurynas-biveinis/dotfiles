"""Mechanical reply handling for the inspect-changes review orchestrator.

Sub-steps reply in JSON; the review files are append-only JSON Lines; only the
final report is Markdown.
"""

import argparse
import fcntl
import glob
import json
import os
import re
import sys
import tempfile
from pathlib import Path

# The script runs from the dotfiles tree, which a review must not write into.
sys.dont_write_bytecode = True

from rendering import (  # noqa: E402, RUF100  # pylint: disable=wrong-import-position
    CONFIDENCE_NOTE,
    MERGE_NOTES,
    PROVENANCE_NOTE,
    TIERS,
    claim_text,
    confidence_text,
    joined,
    render_finding,
    render_proposal,
    render_request,
)
from reply_file import (  # noqa: E402, RUF100  # pylint: disable=wrong-import-position
    InvalidReply,
    read_reply,
)

FINDING_ID = re.compile(r"R\d+-\d+")
SEVERITIES = tuple(severity for severity, _ in TIERS)
PROVENANCES = ("introduced", "pre-existing-on-path", "pre-existing-off-path")
OUTCOMES = ("keep", "drop")
PERCENT = re.compile(r"\s*(\d{1,3})\s*%?\s*")
FINDING_TEXT = ("title", "location", "observation", "suggested_action")
KEEP_TEXT = (
    "final_title",
    "final_location",
    "final_observation",
    "final_suggested_action",
)
# Rendered as Markdown blocks, where the first line's indentation is structure.
KEEP_BLOCKS = ("final_observation", "final_suggested_action")
# A drop's refined claim, which the dedup gate compares against when given.
SETTLED_CLAIM = ("final_location", "final_observation")
# Resolved, so a symlink or `..` cannot lead a write out of the review files.
REVIEW_FILES = str(Path("/tmp").resolve() / "inspect-changes-")


class MalformedRequests(InvalidReply):
    """A reply whose experiment requests alone make it unusable."""


class Refusal(Exception):
    """A request that would break the review files' invariants."""


def text_field(record, key):
    """Return a text field as a string, a list of strings as its lines, or None."""
    value = joined(record.get(key))
    return value if isinstance(value, str) else None


def text_of(record, key, label):
    """Return a required non-empty text field."""
    value = text_field(record, key)
    if value is None or not value.strip():
        raise InvalidReply(f"{label} lacks {key}")
    return value.strip()


def percent(value):
    """Return a confidence as an integer percentage, or None outside [0, 100]."""
    if isinstance(value, bool):
        return None
    if isinstance(value, float) and value.is_integer():
        value = int(value)
    if isinstance(value, int):
        return value if 0 <= value <= 100 else None
    match = PERCENT.fullmatch(value) if isinstance(value, str) else None
    return int(match.group(1)) if match and int(match.group(1)) <= 100 else None


def member(value, allowed, fold):
    """Return value stripped and case-folded if that is allowed, or None."""
    folded = fold(value.strip()) if isinstance(value, str) else None
    return folded if folded in allowed else None


def severity_of(value):
    """Return a severity in its canonical case, or None."""
    return member(value, SEVERITIES, str.upper)


def provenance_of(value):
    """Return a provenance in its canonical case, or None."""
    return member(value, PROVENANCES, str.lower)


def outcome_of(value):
    """Return a verdict outcome in its canonical case, or None."""
    return member(value, OUTCOMES, str.lower)


def checked(label, key, value, normal):
    """Return a field normalized by normal, failing when it is out of range."""
    result = normal(value)
    if result is None:
        raise InvalidReply(f"{label} has {key}: {value!r}")
    return result


def claims(entry):
    """Return a finding's first-pass claims, canonical where in range, else as given.

    Verification re-derives all three, so no value voids the finding. Severity
    and provenance stay text, which the proposal view prints.
    """
    severity = severity_of(entry.get("severity"))
    confidence = percent(entry.get("confidence"))
    provenance = provenance_of(entry.get("provenance"))
    return {
        "severity": severity or claim_text(entry.get("severity")),
        "confidence": (
            confidence if confidence is not None else entry.get("confidence")
        ),
        "provenance": provenance or claim_text(entry.get("provenance")),
    }


def finding_record(label, entry):
    """Validate a draft finding's text fields and return it normalized."""
    if not isinstance(entry, dict):
        raise InvalidReply(f"{label} is not an object")
    record = {key: text_of(entry, key, label) for key in FINDING_TEXT}
    return {**record, **claims(entry)}


def entries(reply, key, required=False):
    """Return a reply's list field; any empty value means an optional one is absent.

    A required list is absent only when missing or `null`, which is invalid.
    A lone object is a one-entry list, the likeliest slip in writing one entry.
    """
    value = reply.get(key)
    if value is None and required:
        raise InvalidReply(f"lacks {key}")
    if not value and not required:
        return []
    if isinstance(value, dict):
        return [value]
    if not isinstance(value, list):
        raise InvalidReply(f"{key} is not a list")
    return value


def request_defect(entry, supports):
    """Return what makes an experiment request unusable, or None.

    Draft requests must name the finding they support; the others belong to the
    finding the sub-step handled.
    """
    procedure = text_field(entry, "procedure") if isinstance(entry, dict) else None
    if not procedure or not procedure.strip():
        return "lacks procedure"
    if supports and not FINDING_ID.fullmatch(str(entry.get("supports", ""))):
        return "lacks supports"
    return None


def usable_requests(reply, supports=False):
    """Return a reply's usable experiment requests and how many were dropped.

    A list fails only when it is a non-empty value that is neither a list nor an
    object, or none of its entries is usable.
    """
    try:
        found = entries(reply, "experiment_requests")
    except InvalidReply as error:
        raise MalformedRequests(str(error)) from None
    defects = [request_defect(entry, supports) for entry in found]
    usable = [entry for entry, defect in zip(found, defects) if defect is None]
    if found and not usable:
        raise MalformedRequests(f"an experiment request {defects[0]}")
    return usable, len(found) - len(usable)


def experiment_count(reply, supports=False):
    """Count a reply's usable experiment requests, with a note naming any dropped."""
    usable, dropped = usable_requests(reply, supports)
    return len(usable), (f"; unusable requests dropped: {dropped}" if dropped else "")


def tolerated_count(reply, supports=False):
    """Count a reply's experiment requests; an unusable list counts as none.

    Return the count and a status note naming any requests ignored.
    """
    try:
        return experiment_count(reply, supports)
    except MalformedRequests:
        return 0, "; malformed requests ignored"


def proposals_in(reply):
    """Return a reply's proposed new findings as objects.

    Text written where an object belongs becomes that proposal's observation, so
    a misformatted proposal reaches dedup instead of vanishing.
    """
    try:
        found = entries(reply, "proposed_findings")
    except InvalidReply:
        value = reply["proposed_findings"]
        found = [value] if isinstance(value, str) else []
    return [
        entry if isinstance(entry, dict) else {"observation": entry}
        for entry in found
        if isinstance(entry, dict) or (isinstance(entry, str) and entry.strip())
    ]


def describe_routed(reply, count):
    """Render a reply's proposal and experiment-request counts as a status suffix."""
    proposed = len(proposals_in(reply))
    return (f"; proposals: {proposed}" if proposed else "") + (
        f"; experiment requests: {count}" if count else ""
    )


def draft_findings(reply, round_index):
    """Validate a draft reply's findings and return them by ID, in order."""
    records = {}
    for entry in entries(reply, "findings", required=True):
        finding = entry.get("id") if isinstance(entry, dict) else None
        if not isinstance(finding, str) or not FINDING_ID.fullmatch(finding):
            raise InvalidReply(f"a finding has no ID: {entry!r}"[:200])
        if not finding.startswith(f"R{round_index}-"):
            raise InvalidReply(f"{finding} is not a round-{round_index} ID")
        if finding in records:
            raise InvalidReply(f"repeated ID: {finding}")
        records[finding] = {"id": finding, **finding_record(finding, entry)}
    return records


def draft_check(args):
    """Validate a draft-step reply and print its finding IDs."""
    reply = read_reply(args.reply)
    if reply.get("review_failed"):
        reason = first_line((text_field(reply, "review_failed") or "").strip())
        raise InvalidReply(
            "drafter reports a failed review" + (f": {reason}" if reason else "")
        )
    requests_count, note = tolerated_count(reply, supports=True)
    ids = list(draft_findings(reply, args.round))
    suffix = describe_routed(reply, requests_count) + note
    print("valid: " + (", ".join(ids) or "no findings") + suffix)
    return 0


def confined(path):
    """Return the resolved write target, refusing one outside the review files."""
    resolved = Path(path).resolve()
    if not str(resolved).startswith(REVIEW_FILES):
        raise Refusal(f"{path} is outside /tmp/inspect-changes-*")
    return resolved


def write_whole(target, text):
    """Replace a file's content at once, so no reader sees it half written."""
    descriptor, temporary = tempfile.mkstemp(dir=target.parent, prefix=target.name)
    with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
        stream.write(text)
    os.replace(temporary, target)


def read_records(path):
    """Return a JSON Lines review file's records, or none for a new file."""
    target = Path(path)
    if not target.exists():
        return []
    # LF only: json.dumps escapes LF and CR, but not U+2028 and the like, which
    # splitlines() would also split at.
    lines = target.read_text(encoding="utf-8").split("\n")
    return [json.loads(line) for line in lines if line.strip()]


def append_records(path, records):
    """Append records to a JSON Lines review file."""
    target = confined(path)
    existing = target.read_text(encoding="utf-8") if target.exists() else ""
    added = "".join(json.dumps(record, ensure_ascii=False) + "\n" for record in records)
    write_whole(target, existing + added)


def require_present(found, wanted, where):
    """Refuse a selection naming entries the source does not hold."""
    missing = [name for name in wanted if name not in found]
    if missing:
        raise Refusal(f"not in {where}: " + ", ".join(missing))


def require_distinct(names):
    """Refuse a selection naming an entry twice."""
    repeated = sorted({name for name in names if names.count(name) > 1})
    if repeated:
        raise Refusal("repeated: " + ", ".join(repeated))


def unnumbered(record):
    """Return a draft record without its finding ID."""
    return {key: value for key, value in record.items() if key != "id"}


def batch_selection(args, draft):
    """Return (label → ID, record) for selected batch proposals, with fresh IDs."""
    batch = read_records(round_file(args.prefix, "batch", args.round))
    found = {entry["label"]: entry for entry in batch}
    require_present(found, args.ids, "batch")
    require_distinct(args.ids)
    drafted = read_records(draft)
    written = {json.dumps(unnumbered(r), sort_keys=True): r["id"] for r in drafted}
    taken = [int(record["id"].split("-")[1]) for record in drafted]
    selection = []
    chosen = {}
    for number, label in enumerate(args.ids, start=max(taken, default=0) + 1):
        fields = {k: v for k, v in found[label].items() if k not in ("label", "source")}
        key = json.dumps(fields, sort_keys=True)
        if key in written:
            raise Refusal(f"already in draft: {label} as {written[key]}")
        if key in chosen:
            raise Refusal(f"repeated: {label} as {chosen[key]}")
        chosen[key] = label
        new_id = f"R{args.round}-{number:03d}"
        selection.append((f"{label} → {new_id}", {"id": new_id, **fields}))
    return selection


def reply_selection(args, draft):
    """Return (ID, record) for selected draft-step findings."""
    found = draft_findings(read_reply(args.reply), args.round)
    require_present(found, args.ids, "reply")
    require_distinct(args.ids)
    present = {record["id"] for record in read_records(draft)}
    duplicates = [name for name in args.ids if name in present]
    if duplicates:
        raise Refusal("already in draft: " + ", ".join(duplicates))
    return [(name, found[name]) for name in args.ids]


def recorded(prefix, name):
    """Return a header value the orchestrator recorded with the state file.

    Scope and baseline travel by file because they can hold shell quotes.
    """
    return required_text(Path(f"{prefix}-{name}.txt")).rstrip("\n")


def draft_write(args):
    """Append surviving findings to the round's draft file."""
    draft = round_file(args.prefix, "draft", args.round)
    # Verification settles a round's draft, and every prescribed write precedes
    # it, so this also catches `<round>` given where `<round+1>` was meant.
    if read_records(round_file(args.prefix, "verdicts", args.round)):
        raise Refusal(f"round {args.round} already has a verdict")
    selection = (
        batch_selection(args, draft) if args.batch else reply_selection(args, draft)
    )
    append_records(draft, [record for _, record in selection])
    print("wrote " + ", ".join(label for label, _ in selection))
    return 0


def proposal_record(entry):
    """Return a proposal's fields, normalized where they are in range."""
    record = {key: (text_field(entry, key) or "").strip() for key in FINDING_TEXT}
    return {**record, **claims(entry)}


def proposals(args):
    """Append the proposed new findings of replies to the round's batch file.

    Labels continue from the file's last one, so a printed label keeps naming the
    same proposal for the rest of the round.
    """
    batch = round_file(args.prefix, "batch", args.round)
    labels = [int(record["label"][1:]) for record in read_records(batch)]
    start = max(labels, default=0) + 1
    added = []
    for reply in args.replies:
        for entry in proposals_in(read_reply(reply)):
            label = f"P{start + len(added)}"
            added.append(
                {"label": label, "source": Path(reply).name, **proposal_record(entry)}
            )
    append_records(batch, added)
    print("\n".join(render_proposal(record) for record in added), end="")
    return 0


def requests(args):
    """Print a reply's experiment requests for the orchestrator to run."""
    usable, _ = usable_requests(read_reply(args.reply), supports=args.draft)
    print("\n".join(render_request(entry) for entry in usable), end="")
    return 0


def present_text(verdict, keys):
    """Return those keys whose values are non-empty text, stripped."""
    found = {key: text_field(verdict, key) for key in keys}
    return {key: text.strip() for key, text in found.items() if text and text.strip()}


def validate_verdict(finding, verdict):
    """Validate a verdict against the schema and return it normalized."""
    label = f"{finding} verdict"
    outcome = checked(label, "outcome", verdict.get("outcome"), outcome_of)
    record = {
        "id": finding,
        "outcome": outcome,
        "final_confidence": checked(
            label, "final_confidence", verdict.get("final_confidence"), percent
        ),
        "verification_trace": text_of(verdict, "verification_trace", label),
    }
    if outcome == "drop":
        record["reason"] = text_of(verdict, "reason", label)
        record.update(present_text(verdict, SETTLED_CLAIM))
    else:
        record.update(present_text(verdict, ("reason",)))
        record.update({key: text_of(verdict, key, label) for key in KEEP_TEXT})
        record.update(
            {
                key: text_field(verdict, key).lstrip("\r\n").rstrip()
                for key in KEEP_BLOCKS
            }
        )
        record["final_severity"] = checked(
            label, "final_severity", verdict.get("final_severity"), severity_of
        )
        record["final_provenance"] = checked(
            label, "final_provenance", verdict.get("final_provenance"), provenance_of
        )
    return record


def first_line(value):
    """Return the first line of a field value."""
    return value.splitlines()[0] if value else ""


def verdict_summary(record):
    """Identify a verdict's outcome and location in one line."""
    confidence = confidence_text(record["final_confidence"])
    if record["outcome"] == "drop":
        return f"drop {confidence} — {first_line(record['reason'])}"
    return (
        f"keep {record['final_severity']} {record['final_provenance']} "
        f"{confidence} — {first_line(record['final_location'])} — "
        f"{first_line(record['final_title'])}"
    )


def round_file(prefix, kind, round_index):
    """Return a round's file of a kind (draft, verdicts, batch)."""
    return f"{prefix}-{kind}-{round_index}.jsonl"


def round_of(finding):
    """Return the round index a finding ID belongs to."""
    return int(finding.split("-")[0][1:])


def verdicts_path(prefix, finding):
    """Return the verdicts file of the round a finding ID belongs to."""
    return round_file(prefix, "verdicts", round_of(finding))


def verdict_copy_path(prefix, finding):
    """Return the file holding one kept finding's verdict, its analyst's input."""
    return f"{prefix}-verdict-{finding}.json"


def verify_record(args):
    """Validate a verifier reply and append its verdict to the round's file."""
    drafted = read_records(round_file(args.prefix, "draft", round_of(args.id)))
    require_present({record["id"] for record in drafted}, [args.id], "draft")
    path = verdicts_path(args.prefix, args.id)
    if any(r["id"] == args.id for r in read_records(path)):
        raise Refusal(f"{args.id} already has a verdict")
    reply = read_reply(args.reply)
    requests_count, note = tolerated_count(reply)
    verdict = reply.get("verdict")
    # Beside requests, a verdict stating no outcome holds the decision's place.
    if requests_count and not (
        isinstance(verdict, dict) and present_text(verdict, ("outcome",))
    ):
        print(f"{args.id}: deferral" + describe_routed(reply, requests_count) + note)
        return 0
    if not isinstance(verdict, dict) or verdict.get("id") != args.id:
        raise InvalidReply(f"lacks verdict for {args.id}")
    record = validate_verdict(args.id, verdict)
    # The copy goes first and whole: an interrupted call then leaves no recorded
    # verdict without its analyst input, and a retry passes the guard above.
    if record["outcome"] == "keep":
        copy = confined(verdict_copy_path(args.prefix, args.id))
        write_whole(copy, json.dumps(record, ensure_ascii=False, indent=2) + "\n")
    append_records(path, [record])
    routed = describe_routed(reply, requests_count) + note
    print(f"{args.id}: {verdict_summary(record)}" + routed)
    return 0


CORRECTABLE = {
    "severity": severity_of,
    "confidence": percent,
    "provenance": provenance_of,
}


def rejection_of(reply):
    """Return a reply's rejection reason, or None.

    Only non-blank text is one, or such text in an object's `reason` or
    `rationale`, the keys a verdict and a correction give theirs under.
    """
    rejection = reply.get("rejection")
    if isinstance(rejection, dict):
        holder, keys = rejection, ("reason", "rationale")
    else:
        holder, keys = reply, ("rejection",)
    for key in keys:
        reason = text_field(holder, key)
        if reason and reason.strip():
            return reason
    return None


def correction_record(finding, reply):
    """Return a correction record and its corrected fields, dropping defects."""
    correction = reply.get("correction")
    if not isinstance(correction, dict):
        return None, []
    valid = {
        name: normal(correction[name])
        for name, normal in CORRECTABLE.items()
        if name in correction and normal(correction[name]) is not None
    }
    rationale = text_field(correction, "rationale")
    if not valid or not rationale or not rationale.strip():
        return None, []
    record = {"kind": "corrected", "id": finding, **valid, "rationale": rationale}
    return record, list(valid)


def validate_analysis(finding, reply, body, requests_count):
    """Apply the analysis reply rules that do not depend on its shape."""
    rejecting = rejection_of(reply) is not None
    if rejecting and requests_count:
        raise MalformedRequests(f"{finding} rejects and requests experiments")
    if body is None:
        if not requests_count:
            raise InvalidReply(f"lacks analysis for {finding}")
        return
    # A blank body reaches here only through checked_analysis's fallback, which
    # validates a rejecting reply's body with the rejection stripped.
    if not rejecting and not body.strip():
        raise InvalidReply(f"{finding} analysis body is empty")


def analysis_body(finding, reply):
    """Return the reply's analysis body for the finding, or None.

    A blank body is none unless the reply rejects: the reason is then the content.
    """
    analysis = reply.get("analysis")
    if not isinstance(analysis, dict) or analysis.get("id") != finding:
        return None
    body = text_field(analysis, "body") or ""
    return body if body.strip() or rejection_of(reply) is not None else None


def analysis_records(prefix):
    """Return the analyses file's records."""
    return read_records(f"{prefix}-analyses.jsonl")


def analyzed_ids(prefix):
    """Return the IDs that count as analyzed: a body, rejection, or skip record."""
    return {r["id"] for r in analysis_records(prefix) if r["kind"] != "corrected"}


def require_pending(prefix, finding):
    """Refuse an analysis record for a finding not pending analysis.

    The analyses file is append-only, and assemble expects each record to name a
    kept finding.
    """
    if finding in analyzed_ids(prefix):
        raise Refusal(f"{finding} is already analyzed")
    if finding not in unanalyzed(prefix):
        raise Refusal(f"{finding} has no kept verdict")


def provisional_view(reply):
    """Return a reply without its rejection and experiment requests."""
    return {
        k: v for k, v in reply.items() if k not in ("rejection", "experiment_requests")
    }


def checked_analysis(finding, reply, body):
    """Validate an analyst reply.

    Return it, its request count, a status note, and whether it fell back to a
    provisional body: a usable body beside malformed requests is recorded as the
    alongside fallback would record it, rather than spending retries on the reply.
    """
    try:
        requests_count, note = experiment_count(reply)
        validate_analysis(finding, reply, body, requests_count)
        return reply, requests_count, note, False
    except MalformedRequests as error:
        fallback = provisional_view(reply)
        try:
            validate_analysis(finding, fallback, body, 0)
        except InvalidReply:
            raise error from None
        dropped = "; rejection discarded" if rejection_of(reply) is not None else ""
        return fallback, 0, dropped + "; malformed requests ignored", True


def analysis_record(args):
    """Validate an analyst reply and record its analysis, rejection, or nothing."""
    require_pending(args.prefix, args.id)
    reply = read_reply(args.reply)
    if args.provisional:
        reply = provisional_view(reply)
    analyses = f"{args.prefix}-analyses.jsonl"
    body = analysis_body(args.id, reply)
    reply, requests_count, note, fell_back = checked_analysis(args.id, reply, body)
    routed = describe_routed(reply, requests_count) + note
    if rejection_of(reply) is not None:
        reason = rejection_of(reply).strip("\n")
        append_records(
            analyses, [{"kind": "rejected", "id": args.id, "reason": reason}]
        )
        print(f"{args.id}: rejected" + routed)
        return 0
    if body is None:
        print(f"{args.id}: deferral" + routed)
        return 0
    if requests_count:
        print(f"{args.id}: provisional analysis held" + routed)
        return 0
    correction, corrected = correction_record(args.id, reply)
    record = {"kind": "analysis", "id": args.id, "body": body}
    # A completion limitation, which assemble must see to refuse convergence.
    if args.provisional or fell_back:
        record["provisional"] = True
    append_records(analyses, [record] + ([correction] if correction else []))
    print(
        f"{args.id}: analysis recorded"
        + (f"; corrected: {', '.join(corrected)}" if corrected else "")
        + routed
    )
    return 0


def analysis_skip(args):
    """Mark a finding's analysis as skipped (retry exhaustion or a cap stop)."""
    require_pending(args.prefix, args.id)
    record = {"kind": "skipped", "id": args.id}
    append_records(f"{args.prefix}-analyses.jsonl", [record])
    print(f"{args.id}: analysis skipped")
    return 0


def round_files(prefix, kind):
    """Return the existing per-round files of a kind (draft, verdicts) in order."""
    base = Path(prefix)
    pattern = re.compile(re.escape(f"{base.name}-{kind}-") + r"(\d+)\.jsonl")
    numbered = []
    for path in base.parent.glob(f"{glob.escape(base.name)}-{kind}-*.jsonl"):
        match = pattern.fullmatch(path.name)
        if match:
            numbered.append((int(match.group(1)), path))
    return [path for _, path in sorted(numbered)]


def recorded_verdicts(prefix):
    """Return every recorded verdict, round by round."""
    return [
        record
        for path in round_files(prefix, "verdicts")
        for record in read_records(path)
    ]


def unanalyzed(prefix):
    """Return the IDs of kept findings with no analysis record yet."""
    done = analyzed_ids(prefix)
    return [
        record["id"]
        for record in recorded_verdicts(prefix)
        if record["outcome"] == "keep" and record["id"] not in done
    ]


def pending_analysis(args):
    """List kept findings with no analysis record yet, with their verdict file."""
    pending = [
        f"{finding} {verdict_copy_path(args.prefix, finding)}"
        for finding in unanalyzed(args.prefix)
    ]
    print("\n".join(pending) if pending else "none")
    return 0


def overlay(record, correction):
    """Return a verdict with an analysis correction's fields applied."""
    corrected = dict(record)
    for name in CORRECTABLE:
        if name in correction:
            corrected[f"final_{name}"] = correction[name]
    return corrected


def finding_order(record):
    """Sort off-path findings last, then by round and number."""
    round_index, number = record["id"][1:].split("-")
    off_path = record["final_provenance"] == "pre-existing-off-path"
    return (off_path, int(round_index), int(number))


def listing(items):
    """Join IDs or entries for the Summary, or say none."""
    return ", ".join(items) or "none"


def corrections_of(records):
    """Map each corrected finding to its correction record."""
    return {r["id"]: r for r in records if r["kind"] == "corrected"}


def ids_of(records, *kinds):
    """Return the IDs of the analysis records of the given kinds, in file order."""
    return [record["id"] for record in records if record["kind"] in kinds]


def provisional_ids(records):
    """Return the IDs of analyses recorded from a provisional body, in file order."""
    return [record["id"] for record in records if record.get("provisional")]


def summary_counts(prefix, records, listed):
    """Render the Summary counts that the review files settle by themselves."""
    verdicts = recorded_verdicts(prefix)
    drafted = sum(len(read_records(path)) for path in round_files(prefix, "draft"))
    corrections = corrections_of(records)
    changes = [
        f"{v['id']}: {name} {confidence_text(v[f'final_{name}'])} → "
        f"{confidence_text(corrections[v['id']][name])}"
        for v in verdicts
        for name in CORRECTABLE
        if name in corrections.get(v["id"], {})
        and corrections[v["id"]][name] != v[f"final_{name}"]
    ]
    by_provenance = " / ".join(
        f"{p} {sum(r['final_provenance'] == p for r in listed)}" for p in PROVENANCES
    )
    parts = [
        f"drafted {drafted}",
        f"kept {sum(v['outcome'] == 'keep' for v in verdicts)}",
        f"dropped {sum(v['outcome'] == 'drop' for v in verdicts)}",
        f"rejected on analysis: {listing(ids_of(records, 'rejected'))}",
        f"analyzed {len(ids_of(records, 'analysis', 'rejected'))}",
        f"analyses skipped: {listing(ids_of(records, 'skipped'))}",
        f"provisional analyses: {listing(provisional_ids(records))}",
        f"listed per provenance: {by_provenance}",
        f"analysis corrections: {listing(changes)}",
    ]
    return "Counts: " + "; ".join(parts) + ".\n"


def listed_findings(prefix, records):
    """Return kept, unrejected verdicts, corrections applied."""
    rejected = set(ids_of(records, "rejected"))
    corrections = corrections_of(records)
    return [
        overlay(record, corrections.get(record["id"], {}))
        for record in recorded_verdicts(prefix)
        if record["outcome"] == "keep" and record["id"] not in rejected
    ]


def report_header(args):
    """Return the report's opening paragraphs."""
    topic = Path(args.prefix).name.removeprefix("inspect-changes-")
    pieces = [
        f"# Code Review: {topic}\n",
        f"Status: {args.status}\n",
        f"Scope: {recorded(args.prefix, 'scope')}\n",
        f"Review basis: {args.prefix}-state.md\n",
        f"Baseline: {recorded(args.prefix, 'baseline')}\n",
        CONFIDENCE_NOTE,
        PROVENANCE_NOTE,
    ]
    return pieces + ([MERGE_NOTES[args.merge]] if args.merge else [])


def severity_tiers(listed, records):
    """Return the three severity sections, findings ordered within each."""
    bodies = {r["id"]: r["body"] for r in records if r["kind"] == "analysis"}
    pieces = []
    for severity, title in TIERS:
        tier = sorted(
            (r for r in listed if r["final_severity"] == severity), key=finding_order
        )
        pieces.append(f"## {title}\n")
        pieces.extend(
            [render_finding(r, bodies.get(r["id"])) for r in tier] or ["None.\n"]
        )
    return pieces


def optional_text(path):
    """Return a file's text, or "" when it does not exist."""
    return path.read_text(encoding="utf-8") if path.exists() else ""


def required_text(path):
    """Return the text of a file the orchestrator must have written."""
    try:
        return path.read_text(encoding="utf-8")
    except FileNotFoundError:
        raise Refusal(f"{path} is missing") from None


def logged_limitations(prefix):
    """Return the limitations file's entries, or "" when it has none."""
    return optional_text(Path(f"{prefix}-limitations.md")).strip()


def convergence_gap(prefix, records):
    """Return why the review files cannot back a converged report, or ""."""
    logged = logged_limitations(prefix)
    if logged or provisional_ids(records) or ids_of(records, "skipped"):
        return "the review files record a completion limitation"
    verified = {record["id"] for record in recorded_verdicts(prefix)}
    unverified = [
        record["id"]
        for path in round_files(prefix, "draft")
        for record in read_records(path)
        if record["id"] not in verified
    ]
    pending = (("no verdict", unverified), ("no analysis record", unanalyzed(prefix)))
    return "; ".join(f"{gap}: {', '.join(ids)}" for gap, ids in pending if ids)


def assemble(args):
    """Render the final report from the verdicts, analyses, and summary files."""
    records = analysis_records(args.prefix)
    if args.status == "converged":
        gap = convergence_gap(args.prefix, records)
        if gap:
            raise Refusal(f"converged, but {gap}")
    listed = listed_findings(args.prefix, records)
    pieces = report_header(args) + severity_tiers(listed, records)
    counts = summary_counts(args.prefix, records, listed)
    summary = required_text(Path(f"{args.prefix}-summary.md")).strip()
    logged = logged_limitations(args.prefix)
    block = ["Completion limitations:\n", logged + "\n"] if logged else []
    pieces.extend(["## Summary\n", summary + "\n", *block, counts])
    out = f"{args.prefix}.md"
    write_whole(confined(out), "\n".join(pieces))
    print(f"wrote {out}")
    return 0


def arg(*names, **options):
    """Describe one command-line argument for COMMANDS."""
    return names, options


PREFIX = arg("prefix")
FINDING = arg("id")
REPLY = arg("reply")
ROUND = arg("round", type=int)
COMMANDS = (
    (draft_check, [ROUND, REPLY]),
    (
        draft_write,
        [
            PREFIX,
            ROUND,
            [arg("--reply"), arg("--batch", action="store_true")],
            arg("ids", nargs="+"),
        ],
    ),
    (verify_record, [PREFIX, FINDING, REPLY]),
    (
        analysis_record,
        [PREFIX, FINDING, REPLY, arg("--provisional", action="store_true")],
    ),
    (analysis_skip, [PREFIX, FINDING]),
    (pending_analysis, [PREFIX]),
    (proposals, [PREFIX, ROUND, arg("replies", nargs="+")]),
    (requests, [REPLY, arg("--draft", action="store_true")]),
    (
        assemble,
        [
            PREFIX,
            arg("--status", required=True, choices=("converged", "incomplete")),
            arg("--merge", choices=tuple(MERGE_NOTES)),
        ],
    ),
)


def build_parser():
    """Build the command-line parser from COMMANDS.

    A list among a command's arguments is a required mutually exclusive group.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(required=True)
    for func, arguments in COMMANDS:
        command = commands.add_parser(
            func.__name__.replace("_", "-"), help=func.__doc__
        )
        for argument in arguments:
            if isinstance(argument, list):
                group = command.add_mutually_exclusive_group(required=True)
                for names, options in argument:
                    group.add_argument(*names, **options)
            else:
                command.add_argument(*argument[0], **argument[1])
        command.set_defaults(func=func)
    return parser


def check_arguments(args):
    """Refuse a prefix outside the review files or a malformed finding ID."""
    if getattr(args, "prefix", None) is not None:
        confined(args.prefix)
    if getattr(args, "id", None) is not None and not FINDING_ID.fullmatch(args.id):
        raise Refusal(f"not a finding ID: {args.id!r}")


def main():
    """Dispatch a subcommand."""
    args = build_parser().parse_args()
    try:
        check_arguments(args)
        # One command at a time across reviews: each one's checks and writes
        # then see the files as the previous command left them.
        flags = os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW
        with os.fdopen(os.open(REVIEW_FILES + ".lock", flags, 0o600)) as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            return args.func(args)
    except InvalidReply as error:
        print(f"invalid: {error}")
        return 1
    except Refusal as error:
        print(f"refused: {error}")
        return 2


if __name__ == "__main__":
    sys.exit(main())
