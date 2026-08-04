# Prior drafts

Treat every finding block written to a draft file as part of the raw-findings
corpus, including findings later kept, dropped, or rejected. When prior draft
paths are supplied, read every file before emitting any
`## Proposed new findings`.

Suppress a proposal when it identifies the same defect at the same location as
a corpus finding, regardless of severity, title, or wording. Judge duplicates
semantically rather than by literal field equality.

This subagent check is a best-effort noise reducer. The top-level review is the
sole authoritative deduplication gate: it checks every candidate against the
corpus and then within the current batch, preserving the first occurrence and
dropping later duplicates.
