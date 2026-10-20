"""The review files' namespace, the only place the script writes."""

import os
from pathlib import Path

NAME_PREFIX = "inspect-changes-"
# Resolved, so a symlink or `..` cannot lead a write out of the review files.
REVIEW_FILES = str(Path("/tmp").resolve() / NAME_PREFIX)


def state_file(prefix):
    """Return the name of the state file of the review at a prefix."""
    return f"{prefix}-state.md"


class Refusal(Exception):
    """A request that would break the review files' invariants."""


def confined(path):
    """Return the resolved write target, refusing one outside the review files."""
    return contained(path, Path(path).resolve())


def confined_prefix(prefix):
    """Refuse a prefix whose files would land outside the review files.

    Only its directory is resolved: every file is a sibling named after the
    prefix, so whatever already sits at the prefix itself decides nothing.
    """
    directory, name = os.path.split(prefix)
    contained(prefix, Path(directory).resolve() / name)


def contained(path, resolved):
    """Return a path's resolved form, refusing one outside the review files."""
    if not str(resolved).startswith(REVIEW_FILES):
        raise Refusal(f"{path} is outside /tmp/{NAME_PREFIX}*")
    return resolved
