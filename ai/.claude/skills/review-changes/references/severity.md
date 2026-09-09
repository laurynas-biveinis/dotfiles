# Severity

Each finding carries exactly one severity, which decides the section it lands
in; the sections are read most severe first. Severity measures consequence —
how much the defect costs if it is real — not evidential support. Anchors:

- **CRITICAL** — bugs, security issues, or fundamental design flaws
  that must be fixed.
- **IMPORTANT** — performance problems, maintainability issues, or
  violations of core principles.
- **SUGGESTION** — improvements for readability, style, or minor
  optimizations.

How well established a finding is belongs to [confidence](confidence.md), and
whether the reviewed change caused it belongs to [provenance](provenance.md);
neither should be folded into this value.

A verifier's `Final severity:` is the authoritative value that reaches the
review.
