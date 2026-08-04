# Shared output sections

Use these schemas when the invoking review stage calls for either optional
section. The invoking skill defines when each section is allowed and what its
content may address.

## Proposed new findings

Emit each proposal as a complete finding block with no ID. The top-level assigns
IDs when it appends the proposals to the next draft.

```markdown
## Proposed new findings

### CRITICAL — <one-line title>

- Confidence: 70%
- Location: `path/to/file.ext:LN`
- Observation: <what's wrong, with evidence>
- Suggested action: <concrete fix>
```

## Experiment requests

Do not run experiments. Return a `## Experiment requests` section whose every
entry begins with the required `### EXP — <what it tests>` header and specifies
the goal, a freeform procedure whose commands may branch on output, and the
result patterns that answer the stated goal. The entry need not name the finding
because the top-level attributes it to the finding handled by this invocation.

Keep every procedure isolated and bounded: write only inside a scratch directory
created with `mktemp -d` or under `/tmp`; reading project files is allowed; never
run `./check.sh`, tests, or builds; use the network only to read online docs.

```markdown
## Experiment requests

### EXP — <what it tests>

- Goal: <what you are trying to establish>
- Procedure: <freeform; one or more steps that may branch on observed output>
- Confirms / Refutes: <result patterns that answer the stated goal>
```
