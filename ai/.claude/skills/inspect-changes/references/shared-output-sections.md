# Shared output sections

Use these schemas when the invoking review stage calls for either optional
list in its JSON reply. The invoking skill defines when each list is allowed
and what its entries may address. Text fields may hold Markdown; JSON string
escaping is the only escaping they need.

## Proposed new findings

Add a `proposed_findings` list, each entry a complete finding with no ID. The
top-level assigns IDs when it appends the proposals to the next draft.

```json
{
  "proposed_findings": [
    {
      "severity": "CRITICAL",
      "title": "<one-line title>",
      "confidence": 70,
      "provenance": "introduced",
      "location": "`path/to/file.ext:LN`",
      "observation": "<what's wrong, with evidence>",
      "suggested_action": "<concrete fix>"
    }
  ]
}
```

`severity` is `CRITICAL`, `IMPORTANT` or `SUGGESTION`; `confidence` is an
integer percentage from 0 to 100; `provenance` is `introduced`,
`pre-existing-on-path` or `pre-existing-off-path`. Grade each proposal's
severity per the [shared severity guidance](severity.md), calibrate its
confidence per the [shared confidence guidance](confidence.md), and classify its
provenance per the [shared provenance guidance](provenance.md); all three are
first-pass claims that verification re-derives, and for provenance a cheap read
suffices.

## Experiment requests

Do not run experiments. Add an `experiment_requests` list whose every entry has
a non-empty `procedure` and states the goal, a freeform procedure whose commands
may branch on output, and the result patterns that answer the stated goal. The
entry need not name the finding because the top-level attributes it to the
finding handled by this invocation.

Keep every procedure isolated and bounded: write only inside a scratch directory
created with `mktemp -d` or under `/tmp`; reading project files is allowed; never
run `./check.sh`, tests, or builds; use the network only to read online docs.

```json
{
  "experiment_requests": [
    {
      "title": "<what it tests>",
      "goal": "<what you are trying to establish>",
      "procedure": "<freeform; one or more steps that may branch on output>",
      "confirms_refutes": "<result patterns that answer the stated goal>"
    }
  ]
}
```
