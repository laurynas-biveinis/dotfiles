---
paths:
  - ".github/workflows/**"
---

# GitHub Actions Workflow Rules

Extend `paths:` above to cover composite actions (`.github/actions/**`) or a
root `action.yml` if/when added. Update this rule as needed if/when you start
using Docker refs (`docker://...`), local refs (`uses: ./...`), or
reusable-workflow refs (`owner/repo/.github/workflows/*.yml@ref`).

- For SemVer refs:
  - Existing SemVer refs are deliberate choices. Do not modify them when
    editing the file for unrelated reasons, and do not suggest bumping
    solely because a newer major/minor/patch exists upstream.
  - Bumps are user-driven: write exactly the ref the user specifies, at
    the precision the user wrote (e.g., user says `@v4` → write `@v4`,
    not `@v4.0` or `@v4.0.1`). If the user requests an update without
    naming a target ref, ask which one rather than guessing.
  - When introducing a new `uses:` line, prefer this order (the `v` is
    only an example; the actual format may differ):
    1. `@v<major>` (floating major)
    1. `@v<major>.<minor>` (floating major-minor)
    1. `@v<major>.<minor>.<patch>` (explicit patch)
    Drop down a level when the higher form does not exist or when a
    branch shares its name (would trigger zizmor `ref-confusion`).
    Verify with `gh api repos/<owner>/<repo>/branches/<ref>`: a 404
    means no collision.
- For non-SemVer refs (`@latest`, `@beta`, `@main`, `@master`): existing
  ones are deliberate choices. Do not modify them when editing the file for
  unrelated reasons, and do not suggest swapping them solely because a
  SemVer alternative exists (e.g., `@latest` is used for super-linter
  despite available SemVer tags). When introducing a new `uses:` line whose
  action documents a non-SemVer ref, use the documented ref as-is.
