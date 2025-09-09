#!/usr/bin/env python3
"""Block git add/rm commands that use glob patterns or directories.

This hook prevents the use of glob patterns, directories, or bulk staging flags
in git add and git rm commands, enforcing the requirement to stage individual
files explicitly.
"""

import json
import os
import sys
import shlex


def has_glob_patterns(arg):
    """Check if an argument contains glob pattern characters."""
    glob_chars = ["*", "?", "[", "]", "{", "}"]
    return any(char in arg for char in glob_chars)


def is_directory(arg):
    """Check if argument refers to a directory on the filesystem."""
    # Special cases that are always directories
    if arg in [".", ".."]:
        return True

    # Remove trailing slash for filesystem check
    path = arg.rstrip("/")

    # Check if it's a git pathspec (starts with : or contains :)
    if arg.startswith(":") or ":(glob)" in arg or ":(literal)" in arg:
        return False  # Let git handle pathspecs, but they're suspicious

    # Check if path exists and is a directory
    if os.path.exists(path) and os.path.isdir(path):
        return True

    # If it explicitly ends with /, treat as directory even if doesn't exist yet
    if arg.endswith("/"):
        return True

    return False


def _split_command(command):
    """Split command into parts, handling quotes properly."""
    try:
        # Use shlex to properly split the command respecting quotes
        return shlex.split(command)
    except ValueError:
        # If shlex fails, fall back to simple split
        return command.split()


def _is_git_staging_command(parts):
    """Check if parts represent a git add/rm command."""
    if len(parts) < 2:
        return False
    if parts[0] != "git":
        return False
    if parts[1] not in ["add", "rm"]:
        return False
    return True


def parse_git_staging_command(command):
    """Parse git add/rm command and analyze arguments."""
    parts = _split_command(command)

    if not _is_git_staging_command(parts):
        return None, [], []

    # Extract the git subcommand
    subcommand = parts[1]

    # Check for dangerous flags
    dangerous_flags = [
        "-A",
        "--all",
        "-u",
        "--update",
        "--no-ignore-removal",
        "-i",
        "--interactive",
        "-p",
        "--patch",
        "--intent-to-add",
    ]

    blocked_flags = []
    file_args = []
    skip_next = False
    after_double_dash = False

    for arg in parts[2:]:
        if skip_next:
            skip_next = False
            continue

        # Check for double dash separator
        if arg == "--":
            after_double_dash = True
            continue

        # After --, everything is treated as a file
        if after_double_dash:
            file_args.append(arg)
            continue

        # Check if it's a flag
        if arg.startswith("-"):
            # Check for dangerous flags
            if arg in dangerous_flags:
                blocked_flags.append(arg)
            # Check for flags that take arguments
            elif arg in ["-m", "--chmod"]:
                skip_next = True
            # Check for combined short flags (e.g., -Am)
            elif not arg.startswith("--") and ("A" in arg or "u" in arg):
                blocked_flags.append(arg)
        else:
            # It's a file argument
            file_args.append(arg)

    return subcommand, blocked_flags, file_args


def main():
    """Process the tool input and block inappropriate git staging commands."""
    try:
        # Read the tool input from stdin
        tool_input = json.load(sys.stdin)

        # Extract the command from the Bash tool input
        if tool_input.get("tool") != "Bash":
            # Not a Bash command, allow it
            sys.exit(0)

        command = tool_input.get("params", {}).get("command", "")

        # Parse the git command
        result = parse_git_staging_command(command)
        if result is None:
            sys.exit(0)
        subcommand, blocked_flags, file_args = result

        # If not a git add/rm command, allow it
        if subcommand is None:
            sys.exit(0)

        # Check for blocked flags
        if blocked_flags:
            output = {
                "hookSpecificOutput": {
                    "hookEventName": "PreToolUse",
                    "permissionDecision": "deny",
                    "permissionDecisionReason": (
                        f"Blocked: git {subcommand} with flags "
                        f"{', '.join(blocked_flags)} is not allowed. "
                        f"Per CLAUDE.md guidelines, you must stage individual files explicitly. "
                        f"Do not use bulk staging flags like -A, --all, -u, or --update."
                    ),
                }
            }
            print(json.dumps(output))
            sys.exit(0)

        # Check each file argument for glob patterns or directories
        blocked_patterns = []
        for arg in file_args:
            if has_glob_patterns(arg):
                blocked_patterns.append(f"'{arg}' (contains glob patterns)")
            elif is_directory(arg):
                blocked_patterns.append(f"'{arg}' (is a directory)")

        if blocked_patterns:
            output = {
                "hookSpecificOutput": {
                    "hookEventName": "PreToolUse",
                    "permissionDecision": "deny",
                    "permissionDecisionReason": (
                        f"Blocked: git {subcommand} cannot use glob patterns or directories. "
                        f"Invalid arguments: {', '.join(blocked_patterns)}. "
                        f"Per CLAUDE.md guidelines, you must stage individual files explicitly. "
                        f"Use specific file paths without glob patterns or directory references."
                    ),
                }
            }
            print(json.dumps(output))
            sys.exit(0)

        # If no file arguments at all, that's fine (e.g., git add with no args shows usage)
        # Allow the command
        sys.exit(0)

    except Exception as e:
        # On error, allow the command but log the error
        print(f"Hook error: {e}", file=sys.stderr)
        sys.exit(0)


if __name__ == "__main__":
    main()
