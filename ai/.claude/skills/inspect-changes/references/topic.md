# Review topic

A skill uses this rule when it names every review file
`/tmp/<skill>-<topic>-<suffix>`, except its report `/tmp/<skill>-<topic>.md`,
and writes `/tmp/<skill>-<topic>-state.md` before any other. `<skill>` is that
skill's name.

Suffix the derived `<topic>` with the smallest free integer (`-2`, `-3`, …) if
any `/tmp/<skill>-<topic>*` file already exists, or if
`/tmp/<skill>-<p>-state.md` exists where `<p>` is `<topic>` cut at one of its
hyphens: that review may later write a file named like this review's report.
Apply that second test to the derived slug only; `<topic>-<n>` is free when no
`/tmp/<skill>-<topic>-<n>*` file exists. Compare names without regard to case,
since `/tmp` may be case-insensitive.

inspect-changes applies this rule through its reply script's `claim` command,
which creates the state file in the same step. A skill that applies it by hand
checks before it creates, so two of its reviews that start together can still
take colliding topics.
