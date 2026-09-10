# Tracking pre-existing findings

Read the sibling [GTD skill](../../gtd/SKILL.md) for Org mechanics, destinations,
contexts, completion, and availability. This workflow supplies the clarify
decision: on-path work is fixed now; independent off-path work is recorded.
Do not invoke a fresh inbox-processing dialogue for each finding.

## Search before acting

Apply this procedure to both pre-existing provenance classes after excluding
findings handled above. Determine the relevant repository and whether GitHub
issues apply from project instructions, repository configuration, and existing
practice. Public source-code projects use GitHub plus an Org pointer; private
or non-GitHub work defaults to full Org records unless established practice
specifies otherwise. Do not guess a repository from a directory name.

Search applicable GitHub issues and exposed Org files before creating records
or fixing an on-path finding. Search open and closed records, varying terms
for the affected component, behavior, and root cause; `org-grep` is a literal
substring search, so one missed phrase does not establish absence. Read
candidate bodies and relevant comments/subtrees to establish that the defect
and remaining outcome match. An existing record can cover more than one
finding; create no duplicate merely because its wording or location differs.

Record the search result and any matching GitHub URL/Org URI in the ledger.
A matching record never changes provenance routing. A failed, truncated, or
unavailable search is incomplete evidence, not a no-match. Resume the missing
search before creating records; continue independent code fixes meanwhile.
If Org is absent at session start, GTD permits proceeding without it, but
mandatory deduplication and filing remain unfinished: surface the exact
proposed record and report incomplete tracking. Follow GTD's guidance if it
disappears mid-session. Do not work around unavailable Org access by editing
its underlying files.

Unlike GTD's ordinary open-item lookup, retain closed matches here. When the
same off-path defect still exists, reopen the existing issue or Org action
with the supporting evidence, rather than creating a duplicate. Respect GTD's
placement/context rules when restoring an archived or incubated action; a
closed record in an inaccessible archive remains an unresolved tracking
operation, not permission to duplicate it.

## Record off-path work

Handle GitHub and Org matches independently:

- Neither exists: create the GitHub issue when applicable, then an Org action
  pointing to it; otherwise create the full Org action.
- Only GitHub exists: reuse or reopen it and create the missing Org pointer.
- Only Org exists: reuse or reopen it. When GitHub applies, create the missing
  issue from the full finding and turn the Org action into its pointer,
  preserving personal planning/context information.
- Both exist: reuse or reopen them and repair a missing pointer if necessary.

For a full finding record, include the problem, affected code/behavior,
evidence, recommended action, and enough repository context to resume the
work without a scratch report. Use permanent repository/commit references
where available; never make `/tmp` files or ephemeral review IDs load-bearing.
Once GitHub holds the finding, Org holds the actionable title and issue link
with personal GTD metadata, rather than a second copy of the technical body.
Do not expose private local content in a public issue.

Use available labels and repository conventions. Begin GitHub comments,
reviews, and replies with `(LLM agent)`; use the same attribution for new issue
bodies. With `gh`, supply multiline bodies through a file and `--body-file`,
preserving exact text. Record every successful write's returned URL or URI
immediately. An uncertain write result requires a fresh lookup before retrying;
do not create a duplicate after a timeout. Avoid repeating identical comments
on rediscovery.

Append each newly created issue and Org item to the cumulative off-path
creation list, once per actual record. Keep created, reused, reopened, and
pending operations distinct. A failure to create the second record neither
undoes nor hides the first; retry only the missing operation after deduplication.

## Resolve on-path work

After the searches, fix the finding regardless of existing tracking. Create no
new issue or Org action if it is resolved within this run. If work remains
unresolved at termination, record or surface it through the same deduplication
and GTD mechanics, marked as outstanding on-path work rather than adding it to
the list of off-path creations.

Update existing records when the fix is validated. Complete an Org action only
when its stated outcome is met; an action requiring a push or integration is
not done after a local amendment. On GitHub, record the local fix and close
only when the repository's resolution convention is satisfied. Do not imply
that unpublished changes have reached other users. After later rewrites,
ensure any recorded commit reference still identifies the fix or provide its
replacement. Apply GTD's completion/archive mechanics and retain stable Org
links returned by those operations.

At termination, supply the entrypoint's cumulative list of all off-path
creations, with descriptive titles and clickable GitHub and Org links. Include
records subsequently completed, reopened, or reclassified. Explicitly report
when none were created, and separately surface any unfinished filing.
