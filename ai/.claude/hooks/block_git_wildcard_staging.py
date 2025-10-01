#!/usr/bin/env python3
# pylint: disable=broad-exception-caught,duplicate-code
"""Block git add/rm commands that use glob patterns or directories.

This hook uses pure allowlist validation: only commands matching the exact
pattern 'git add|rm file1 file2 ...' with simple filenames are allowed.
All other patterns including flags, glob patterns, shell operators, and
directories are rejected.
"""

import json
import re
import sys


def is_valid_filename(arg):
    """Check if argument is a valid simple filename.

    Valid filenames contain only: alphanumeric, /, -, _, .
    Rejects: . and .. (directory shortcuts that stage entire directories)
    """
    # Reject directory shortcuts
    if arg in [".", ".."]:
        return False
    # Only allow safe characters
    return bool(re.match(r"^[a-zA-Z0-9/_.\-]+$", arg))


def is_valid_git_staging_command(command):  # pylint: disable=too-many-return-statements
    """Check if command matches the allowed pattern: git add|rm file1 file2 ...

    Returns: (is_valid, error_message)
    """
    parts = command.split()

    # Validate command structure
    if len(parts) < 2:
        return False, "Too few arguments"
    if parts[0] != "git":
        return False, "Not a git command"
    if parts[1] not in ["add", "rm"]:
        return False, "Not a staging command"

    # Allow 'git add' or 'git rm' with no arguments (shows usage)
    if len(parts) == 2:
        return True, None

    # Validate all file arguments
    for part in parts[2:]:
        if part.startswith("-"):
            return False, f"Flags not allowed: {part}"
        if not is_valid_filename(part):
            return False, f"Invalid filename pattern: {part}"

    return True, None


def has_shell_operators(command):
    """Check if command contains shell operators."""
    # Shell operators that indicate compound commands
    operators = ["&&", "||", ";", "|", ">", "<", "$(", "`", "&"]
    return any(op in command for op in operators)


def main():
    """Process the tool input and block inappropriate git staging commands."""
    # Parse JSON input
    try:
        input_data = json.load(sys.stdin)
    except Exception as e:
        print(f"Error: Invalid JSON input: {e}", file=sys.stderr)
        # Exit code 1 shows stderr to the user but not to Claude
        sys.exit(1)

    # Extract the command from the Bash tool input
    if input_data.get("tool_name") != "Bash":
        # Not a Bash command, allow it
        sys.exit(0)

    tool_input = input_data.get("tool_input", {})
    command = tool_input.get("command", "")

    # Check if this is a git staging command
    if "git add" not in command and "git rm" not in command:
        # Not a git staging command, pass through
        sys.exit(0)

    # Check for shell operators in git staging commands
    if has_shell_operators(command):
        output = {
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": "deny",
                "permissionDecisionReason": (
                    "Blocked: git staging commands cannot be used in compound commands. "
                    "Per CLAUDE.md guidelines, you must use separate Bash tool calls for "
                    "git add/rm commands. Do not use shell operators like &&, ||, ;, or | "
                    "with git staging commands."
                ),
            }
        }
        print(json.dumps(output))
        sys.exit(0)

    # Validate against allowlist pattern
    is_valid, error_message = is_valid_git_staging_command(command)

    if not is_valid:
        output = {
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": "deny",
                "permissionDecisionReason": (
                    f"Blocked: {error_message}. "
                    f"Per CLAUDE.md guidelines, only 'git add file1 file2 ...' or "
                    f"'git rm file1 file2 ...' with explicit file paths are allowed. "
                    f"No flags, glob patterns, directories, or shell operators permitted."
                ),
            }
        }
        print(json.dumps(output))
        sys.exit(0)

    # Command matches allowlist, allow it
    sys.exit(0)


if __name__ == "__main__":
    main()
