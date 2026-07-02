---
description: Commits the staged changes with well-written messages
context: fork
model: haiku
agent: general-purpose
allowed-tools: >-
  Bash(git diff:*)
  Bash(git log:*)
  Bash(git status:*)
  Bash(git commit:*)
  Bash(cat:*)
  Read
---

# Commit

You are running as the forked `commit` skill. This document is your
complete task — there is no separate user request. Execute the
numbered steps now, starting with step 1.

1. **Analyze the Staged Diff**: Execute `git diff --staged` to examine all
   changes that have been staged for commit. Pay careful attention to:
   - The nature and scope of changes (feature, fix, refactor, docs, test, etc.)
   - Which files and modules are affected
   - The semantic meaning of the changes, not just the mechanical edits
   - Whether changes represent a single logical commit per the project's
     commit guidelines

   If no changes are staged, stop and return the empty state.

1. **Learn the Repository Style**: Before crafting a commit message:
   - Run `git log --oneline -20` to examine recent commit messages
   - Identify the pattern: module prefixes (e.g., "Emacs:", "AI:"),
     conventional commits (e.g., "feat:", "fix:"), or other formats
   - Your commit message must match this style

   If the convention is unclear from recent commits, stop and return a
   clarification question.

1. **Assess Commit Readiness**: Before writing a commit message, verify the
   following.
   - Staged changes represent a single, cohesive concern
   - The staged diff tells one coherent story
   - No unrelated changes are staged together
   - If the staged changes mix multiple concerns, stop, report the mixed
     concerns, and suggest staging them separately

1. **Craft the Commit Message**: Write a commit message following these
   principles:
   - Use conventional commit format when appropriate: `type(scope): subject`
   - Common types: feat, fix, refactor, docs, test, style, chore, perf
   - Subject line: imperative mood, no period, under 72 characters
   - Subject should complete the sentence: "If applied, this commit will..."
   - Add a body when the change needs explanation beyond the subject
   - Body: explain WHY the change was made, not WHAT changed (the diff shows
     what)
   - Include breaking change notes if applicable
   - Reference issue/ticket numbers when relevant
   - Wrap body lines at 72 characters
   - Do NOT use unstaged changes as input to the commit message.

1. **Execute the Commit**: After crafting the commit message:
   - Execute `git commit -m "<message>"`. For multi-line messages use a
     heredoc:

     ```bash
     git commit -m "$(cat <<'EOF'
     <subject>

     <body>
     EOF
     )"
     ```

   - Confirm the commit was successful. If a pre-commit hook rejects the
     commit, fix the underlying issue and create a new commit. Do not
     use `--amend` or `--no-verify`.

1. **Report Back**: Return the resulting commit hash and the final commit
   message. If no commit was made (e.g. nothing staged, mixed concerns, hook
   failure), return the reason instead.

**Quality Standards**:

- Commit messages must be informative enough for someone to understand the
  change without viewing the diff
- Avoid vague messages like "fix bug" or "update code"
- Be precise about what component or functionality is affected
- Maintain consistency with existing commit message style in the repository
