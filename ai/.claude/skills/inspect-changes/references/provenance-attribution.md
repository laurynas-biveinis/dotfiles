# Provenance attribution

Naming the commit that owns a piece of content, for the cases rule 3 of the
[shared provenance guidance](provenance.md) routes here. **This file is for the
verify tier.** The draft tier stops at the cheap baseline read and never runs
history attribution; the analysis step answers its placement question at the
blame-target `REV` instead, per the `inspect-changes-analyze` skill.

Ask Git about the **baseline**, never the default: `git blame` with no
revision annotates the working tree, and `git log -L` starts at `HEAD`. Under
a combined-diff merge scope `<baseline>` is not one revision but every parent,
so run each command below once per parent: a hit on any parent settles
`pre-existing`, and only an empty result on all of them supports `introduced`.
Use `git log -L <start>,<end>:<path> <baseline>` over a range that brackets
the site — it resolves the range in `<baseline>`, not in the file you are
reading, so bracket generously or locate the site by content with
`git log -S'<text>' <baseline> -- <path>` — or
`git blame -w -C -C20 <baseline> -- <path>` when content was relocated. Under
a bare `git diff` the baseline is the index, which has no revision to pass:
feed the indexed blob in as the final image with
`git show :<path> | git blame -w -C -C20 --contents - HEAD -- <path>` (needs
Git ≥ 2.41 — older Git rejects `--contents` with a revision:
`fatal: cannot use --contents with final commit object name`). Every blame
here carries the same two flags. The `20` is `-C`'s cross-file detection bound
in alphanumeric characters, set explicitly because its default is 40 — twice
`zebra`'s — so a shorter relocated block would go silently unattributed. Write
the number on the last `-C` that carries one; an argument-less `-C` after it
does not reset the score. `-w` is not optional either: without it `-C`'s
cross-file search matches exactly, so an extraction that re-indents on the way
out is attributed to the commit that moved it rather than to its origin.

`External file (--contents)` marks the lines of the piped image that blame
cannot trace into the revision in the final-commit slot, so what it means
for the tag depends on which revision sits there. It means `introduced` only
when that revision is the baseline — which is the blame below, not the
command just given. There the slot holds `HEAD` while the baseline is the
index, and the index is what is piped, so the marked lines are index-only,
hence baseline content: a defect on one is `pre-existing`, and only what the
bare `git diff` itself adds is `introduced`.

Every command above pins the path you are reading, and rule 1's relocation
exception in the [shared provenance guidance](provenance.md) routes you here
exactly when that path may be absent from the baseline. `git log -L` and plain
`git blame` fail loudly there (`fatal: There is no path …` /
`fatal: no such path … in <baseline>`). The `--contents` form checks a weaker
condition: its pathspec must resolve in the final-commit slot **or** in your
current index, and the working tree does not count. A path absent from the
baseline alone therefore passes, at exit 0; the form dies only when the path is
in neither, and then prints `fatal: no such path '…' in HEAD` with `HEAD`
literal whatever revision you passed — never read that message as naming the
baseline. Meanwhile `git log -S'<text>' <baseline> -- <path>` exits 0 with no
output: the pathspec filters, it does not check. A result with no hit whose diff
shows the change you are chasing — empty, or carrying only the opposite side —
is not evidence of `introduced`: re-run it with no pathspec, adding `-m` when
the change may have arrived in a merge, since `git log` computes no merge diffs
by default and pickaxe therefore never examines them. Under `-m` a merge that
merely _integrated_ the change hits too, from the parent that already lacked it;
that is propagation, not authorship, so read the per-parent diffs and keep to
the newest non-merge hit, or to a merge showing the change against every parent
— the conflict resolution that authored it. A hit is not yet evidence of
`pre-existing` either: `-S` matches every commit that changed the text's
occurrence count — the adding and the removing side alike — so content deleted
before the baseline and re-added by the scope still hits. Add `--name-only` to
list each hit commit's paths as candidates. Two tests bear on `pre-existing`,
and they answer different questions, so run both rather than treating either as
the other's equivalent. The containment read, `git show <baseline>:<candidate>`,
settles whether the baseline still holds the content; it tolerates
re-indentation and re-wrapping, but sees only the candidate paths `--name-only`
listed. The blame,

```text
<post-image read> |
  git blame -w -C -C20 --contents - <baseline> -- <path>
```

settles whether the post-image's exact lines are attributable above the copy
threshold. Its reach is a property of the pathspec, not of the flag: name
`<path>` — the reviewed file under its post-image name — so the piped image
_creates_ that path relative to `<baseline>`, which is the boundary at which
`-C -C` searches the whole baseline tree and can surface a path the pickaxe
missed. A pathspec already resident in `<baseline>` still resolves and still
blames, but collapses to `-C` reach and forfeits that sweep; recovering it
needs a third `-C`, which is markedly slower. For `<post-image read>` use
the read named earlier — `git show :<path>` under `git diff --staged`,
`git show <rev>:<path>` under a committed scope. Under a bare `git diff` it
has no instantiation at all, since the post-image is the working tree and
the index baseline has no revision for the final-commit slot; there the
containment read is the whole test.

Read that output rather than predicting it from the path. Blame traces
content, so lines that reached `<path>` by relocation are attributed to the
path they came from — a whole-file rename Git detects at any `-C` level,
because blame follows detected renames on its own with no `-C` at all, and a
copy, extraction or split only at the `-C -C` prescribed here. Only lines
with no counterpart in `<baseline>` carry `External file (--contents)`; a
path absent from `<baseline>` does not by itself produce them. The pathspec
is required either way, and for a rename either name resolves — the
baseline-resident one through the ordinary same-path lookup, the index-only
one through rename following — but prefer the post-image name, for the reach
above.

Two empty results do not by themselves prove absence, because the tests'
blind spots can overlap: pickaxe offers only the paths `--name-only` listed,
which a rename the scope did not touch can leave stale, while blame's `-C20`
bound and its line matching can miss the same content at its real baseline
path. Before concluding `introduced`, search the baseline tree itself —
`git grep <pattern> <baseline>` for the content, and
`git ls-tree -r --name-only <baseline>` to see what paths exist there — and
read any match in context: a
common fragment recurring on its own is not the defect predating the change.
Only an informed absence supports `introduced`; an unresolved one takes the
tie-break.

Two bounds limit what an empty blame proves. `-w` recovers content that was
only re-indented; re-wrapping still defeats line matching, which is why the
containment read governs wherever the two disagree. The `-C20` detection bound
set above is the other, and a relocated block shorter than it still blames
empty. An attribution proves relocation; an empty blame does not disprove
one.

Run the blame only where its pathspec can resolve. Per the `--contents`
prerequisite above, a path in neither `<baseline>` nor your current index
makes it exit 128 — the historical case where the scope reviews a commit that
copied a path later deleted before `HEAD`. That is a test that could not run,
never an empty attribution: when it happens, say so and go on with the
containment reads alone, where a hit still settles `pre-existing` under the
**either** rule below, and an inconclusive read falls to the tie-break in the
[shared provenance guidance](provenance.md) rather than to `introduced`.

Neither test is authoritative alone, so compose them in the safe direction:
a hit from **either** settles `pre-existing`, and `introduced` needs **both**
to come up empty — every candidate lacking the content and the blame
attributing none of it, the blame having actually run. An empty blame is a
third outcome, not the second one: a failed piped read writes nothing to
stdout, blame on empty input exits 0 with no output, and the `fatal:` reaches
stderr only, so confirm the read produced the file before reading the blame.
That holds of the bare-`git diff` attribution form above too.
`git log --follow` confirms a whole-file rename but stops at the commit that
created a file produced by a split; `git blame -C -C` crosses that split.

Plain `git blame` misleads on both shapes, in opposite directions. On a
removal it cannot see the deleted lines, reports the surrounding survivors,
and falsely reads as pre-existing. On a relocation it depends on what it
follows unaided: a whole-file rename it detects, and nothing else — an
intra-file move needs `-M`, and a copy, extraction or split needs `-C -C`.
For those three it attributes the moved lines to whichever commit moved
them — the scope itself, or `Not Committed Yet` when the scope is
uncommitted — and so falsely reads as `introduced`. A detected rename is the
exception, attributed correctly with only post-move edits left unattributed;
the tell is blame printing the old name in its filename column, or
`git diff --name-status` recording an `R`. Do not read that exception as
unconditional: detection is similarity-based, so a rename carrying a large
enough rewrite is not detected and falls back to the misattribution above,
which is what the prescribed `-C -C` is there to catch.
