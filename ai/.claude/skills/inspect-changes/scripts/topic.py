"""Claiming an inspect-changes review's topic: its file prefix and state file."""

import itertools
import os
from pathlib import Path

from review_files import NAME_PREFIX, Refusal, state_file

# A claimed state file is never empty, so the Write tool can Read it first.
CLAIMED = "Claimed; the review basis is not written yet.\n"


def claim(args):
    """Pick this review's topic and create its state file in one step."""
    derived = Path(args.prefix)
    if not derived.name.startswith(NAME_PREFIX):
        raise Refusal(f"{args.prefix}: the slug must be one path component")
    try:
        listing = os.listdir(derived.parent)
    except OSError as error:
        raise Refusal(
            f"{args.prefix}: cannot list {derived.parent}: {error.strerror}"
        ) from None
    # `/tmp` may be case-insensitive, so names that differ only in case collide.
    names = [other.lower() for other in listing]

    def taken(name):
        return any(other.startswith(name) for other in names)

    name = derived.name.lower()
    words = name.removeprefix(NAME_PREFIX).split("-")
    cuts = ("-".join(words[:end]) for end in range(1, len(words)))
    # A live review at a shorter topic may yet write a fixed file named like this
    # review's report, which the suffix rules out. A file it names freely stays
    # an unlikely clash, since every candidate still extends its topic.
    if taken(name) or any(state_file(NAME_PREFIX + cut) in names for cut in cuts):
        # Every candidate has the same hyphen cuts, so the cut test refuses all
        # of them or none: it cannot pick one, and refusing all would never end.
        suffixed = (f"{name}-{n}" for n in itertools.count(2))
        name = next(candidate for candidate in suffixed if not taken(candidate))
    prefix = derived.parent / name
    # The prefix check confined this directory, and nothing holds the name.
    flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
    state = state_file(prefix)
    try:
        descriptor = os.open(state, flags, 0o600)
    except OSError as error:
        raise Refusal(
            f"{args.prefix}: cannot create {state}: {error.strerror}"
        ) from None
    with os.fdopen(descriptor, "w", encoding="utf-8") as file:
        file.write(CLAIMED)
    print(f"claimed {prefix}")
    return 0
