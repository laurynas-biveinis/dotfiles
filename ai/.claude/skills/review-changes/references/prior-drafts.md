# Prior drafts

Treat every finding block written to a draft file as part of the raw-findings
corpus, including findings later kept, dropped, or rejected. When prior draft
paths are supplied, read every file before emitting any
`## Proposed new findings`.

The two tiers that dedup hold different evidence, so they apply different tests.
A subagent holds no corpus finding's fate, except that an analyst holds the
verdict of the one finding it is analyzing, and its check is a best-effort noise
reducer. The top-level review is the sole authoritative deduplication gate: it
checks every candidate against the corpus and then within the current batch,
preserving the first occurrence and dropping later duplicates, and it alone
holds each corpus finding's outcome — `Outcome:` in the round's verdict file,
and the `<!-- analysis-rejected: <ID> -->` markers in the analyses file, since a
rejected finding's own verdict reads `keep`.

**As a subagent**, suppress a proposal that makes the same claim as a corpus
finding, judged semantically rather than by literal field equality. Where it
names the same defect and location but makes a materially different claim, emit
it and let the gate decide: a duplicate the gate then drops costs one candidate,
whereas suppressing here is terminal. The finding you are analyzing is the
exception — `review-changes-analyze` governs what you may propose about it.

**At the gate**, suppress a proposal identifying the same defect at the same
location as a corpus finding, regardless of severity, confidence, provenance,
title, or wording — except where that finding both left the review and settled
its claim alone. A verification drop as unconfirmed or as recording no defect,
and an analysis rejection, meet both, so one materially different claim about
that defect and location earns its own verification. A finding still in the
review fails the first test: a refinement of its claim belongs in that finding's
own analysis. A location neither image contains — which you can check directly,
rather than reading it off a drop reason — fails the second: nothing there is
reviewable, so every claim at that location stays suppressed. Neither carve-out
reaches a genuinely distinct defect at an occupied location, which the rule
above never suppressed.

Read settlement as of the moment you run and do not reopen it: at Phase 2's gate
a kept finding has not reached analysis yet, so it counts as still in the
review, and a rejection arriving later does not revive a proposal already
suppressed — the re-draft pass the convergence detector guarantees is where that
claim can return. Admit at most one materially different claim per defect and
location, and require it to differ from every claim already settled there; once
two findings stand at that pair, suppress whatever the wording. Compare against
the claim as settled: the verdict's `Final observation:` and `Final location:`
where it printed them, otherwise the corpus block's `Observation:` and
`Location:` as narrowed by its `Reason:` and `Verification trace:`.
