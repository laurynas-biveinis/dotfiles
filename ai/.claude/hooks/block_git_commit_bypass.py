#!/usr/bin/env python3
# pylint: disable=broad-exception-caught,duplicate-code
"""Block git commit commands that bypass explicit staging.

This hook prevents the use of git commit flags that would commit files
without explicitly staging them first with git add.
"""

import json
import re
import sys


def _block_command():
    """Return the JSON structure for blocking a command."""
    return {
        "hookSpecificOutput": {
            "hookEventName": "PreToolUse",
            "permissionDecision": "deny",
            "permissionDecisionReason": (
                "Blocked: This git commit command bypasses explicit staging. "
                "Per CLAUDE.md guidelines, you must use 'git add' to stage "
                "individual files before committing. Do not use flags like "
                "-a, --all, -i, --include, -o, --only, or specify files "
                "directly with git commit."
            ),
        }
    }


def _has_direct_files(parts):
    """Check if git commit command has direct file arguments."""
    if len(parts) <= 2 or parts[0] != "git" or parts[1] != "commit":
        return False

    skip_next = False
    for part in parts[2:]:
        if skip_next:
            skip_next = False
            continue
        if part.startswith("-"):
            # Check if this flag takes an argument
            if part in ["-m", "--message", "-i", "--include", "-o", "--only"]:
                skip_next = True
            elif part.startswith("--message="):
                continue
        elif part != "&&" and part != ";" and not part.startswith("|"):
            # Found a non-flag argument, likely a file
            return True
    return False


def _check_bypass_patterns(command):
    """Check if command matches any bypass pattern."""
    bypass_patterns = [
        # -a or --all flag
        r"\bgit\s+commit\s+.*(-a\b|--all\b)",
        # Combined flags with 'a' (e.g., -am, -ai, -ao)
        r"\bgit\s+commit\s+-[a-z]*a[a-z]*\b",
        # -i or --include with files
        r"\bgit\s+commit\s+.*(-i\s+|--include\s+)\S+",
        # -o or --only with files
        r"\bgit\s+commit\s+.*(-o\s+|--only\s+)\S+",
        # git commit with paths after -- separator
        r"\bgit\s+commit\s+.*--\s+\S+",
    ]

    for pattern in bypass_patterns:
        if re.search(pattern, command):
            return True
    return False


def main():
    """Process the tool input and block inappropriate git commit commands."""
    try:
        # Read the tool input from stdin
        tool_input = json.load(sys.stdin)

        # Extract the command from the Bash tool input
        if tool_input.get("tool") != "Bash":
            # Not a Bash command, allow it
            sys.exit(0)

        command = tool_input.get("params", {}).get("command", "")

        # Check if this is a git commit command
        if not re.search(r"\bgit\s+commit\b", command):
            # Not a git commit, allow it
            sys.exit(0)

        # Check for direct file specification
        parts = command.split()
        if _has_direct_files(parts):
            # Block direct file specification
            print(json.dumps(_block_command()))
            sys.exit(0)

        # Check if any bypass pattern matches
        if _check_bypass_patterns(command):
            # Block the command
            print(json.dumps(_block_command()))
            sys.exit(0)

        # Allow the command
        sys.exit(0)

    except Exception as e:
        # On error, allow the command but log the error
        print(f"Hook error: {e}", file=sys.stderr)
        sys.exit(0)


if __name__ == "__main__":
    main()
