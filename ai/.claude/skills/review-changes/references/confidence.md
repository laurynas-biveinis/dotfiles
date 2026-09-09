# Confidence

Each finding carries an integer `Confidence: N%` (0–100) reflecting how
strongly the evidence supports its claim — that what the observation
describes is wrong, as described, at that location — not how much the defect
would cost or whether the change caused it. Read "the observation" and "that
location" as the finding's most recent statement of them: the block you are
writing, which for a verifier means the observation and location as you have
refined them — printed as `Final observation:` and `Final location:`, or held
unprinted where an `Outcome: drop` omits those lines — rather than the draft's.

Calibration anchors:

- **90–100** — reproduced via isolated experiment, or trivially provable
  from the diff alone (e.g. syntax error, undefined symbol).
- **70–89** — confirmed by reading the code and following references or
  Git history; no remaining unknowns.
- **50–69** — plausible from the code but one or more assumptions remain
  unverified.
- **Below 50** — speculative: usually drop these rather than emit them.
  Also the band for an affirmatively refuted claim, which is never emitted
  as a finding; the number is what a verifier's `drop` verdict still reports.

How far the evidence you hold establishes the claim picks the band; choose
the number within it — evidence you have not gathered does not license a
higher one. Where verification confirms what the observation describes but
finds nothing wrong, the anchors grade the finding's claim and not the
surrounding facts, so confirming the facts does not by itself buy the 70–89
band.

How much the defect would cost belongs to [severity](severity.md), and
whether the reviewed change caused it belongs to [provenance](provenance.md);
neither should be folded into this value.

A verifier's `Final confidence:` is the authoritative value that reaches the
review.
