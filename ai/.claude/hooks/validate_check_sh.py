#!/usr/bin/env python3
"""
Claude Code Hook: Check.sh Validator
=====================================
This hook ensures ./check.sh is run without any additional arguments or pipes.
It runs as a PreToolUse hook for the Bash tool.

Configuration in settings.local.json:
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "python3 /Users/laurynas/dotfiles/.claude/hooks/validate-check-sh.py"
          }
        ]
      }
    ]
  }
}
"""

import json
import sys


def main():
    """Entry point for the check.sh validation hook."""
    try:
        input_data = json.load(sys.stdin)
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON input: {e}", file=sys.stderr)
        # Exit code 1 shows stderr to the user but not to Claude
        sys.exit(1)

    tool_name = input_data.get("tool_name", "")
    if tool_name != "Bash":
        sys.exit(0)

    tool_input = input_data.get("tool_input", {})
    command = tool_input.get("command", "")

    if not command:
        sys.exit(0)

    # Check if command starts with ./check.sh but isn't exactly ./check.sh
    if command.startswith("./check.sh") and command != "./check.sh":
        print("./check.sh must be run as-is", file=sys.stderr)
        # Exit code 2 blocks tool call and shows stderr to Claude
        sys.exit(2)

    sys.exit(0)


if __name__ == "__main__":
    main()
