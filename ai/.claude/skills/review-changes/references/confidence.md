# Confidence

Each finding carries an integer `Confidence: N%` (0–100) reflecting how
strongly the evidence supports it. Calibration anchors:

- **90–100** — reproduced via isolated experiment, or trivially provable
  from the diff alone (e.g. syntax error, undefined symbol).
- **70–89** — confirmed by reading the code and following references or
  Git history; no remaining unknowns.
- **50–69** — plausible from the code but one or more assumptions remain
  unverified.
- **Below 50** — speculative. Usually drop these rather than emit them.
