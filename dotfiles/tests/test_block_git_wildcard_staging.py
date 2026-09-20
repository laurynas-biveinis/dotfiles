"""Unit tests for the block_git_wildcard_staging Claude Code hook."""

import contextlib
import importlib
import io
import json
import sys
import unittest
import unittest.mock
from pathlib import Path

HOOKS_DIR = Path(__file__).resolve().parent.parent.parent / "ai" / ".claude" / "hooks"
if str(HOOKS_DIR) not in sys.path:
    sys.path.insert(0, str(HOOKS_DIR))

HOOK = importlib.import_module("block_git_wildcard_staging")

COMPOUND_REASON = (
    "Blocked: git staging commands cannot be used in compound commands. Run "
    "git add/rm in its own Bash tool call, without shell operators like &&, "
    "||, ;, or |."
)
ALLOWLIST_REASON_SUFFIX = (
    ". Stage individual files: 'git add [--intent-to-add] [--] file1 file2 ...' "
    "or 'git rm [--cached] [--] file1 file2 ...'. The working tree may hold "
    "unrelated changes, files not meant to be tracked, or the user's own "
    "parallel work, so no other flags, glob patterns, directories, or shell "
    "operators are allowed."
)


def run_main(tool_name, command):
    """Run the hook's main() on one Bash tool input; return (exit code, stdout)."""
    stdin = io.StringIO(
        json.dumps({"tool_name": tool_name, "tool_input": {"command": command}})
    )
    stdout = io.StringIO()
    with unittest.mock.patch.object(sys, "stdin", stdin):
        with contextlib.redirect_stdout(stdout):
            with unittest.mock.patch.object(sys, "stderr", io.StringIO()):
                try:
                    HOOK.main()
                except SystemExit as exit_request:
                    return exit_request.code, stdout.getvalue()
    raise AssertionError("main() returned without exiting")


def denial(reason):
    """The JSON line the hook prints to deny a command for the given reason."""
    return (
        json.dumps(
            {
                "hookSpecificOutput": {
                    "hookEventName": "PreToolUse",
                    "permissionDecision": "deny",
                    "permissionDecisionReason": reason,
                }
            }
        )
        + "\n"
    )


class IsValidGitStagingCommandTest(unittest.TestCase):
    """is_valid_git_staging_command accepts only enumerated-file staging."""

    def test_allowed_commands(self):
        """Enumerated files, the one allowed option, and a bare '--' pass."""
        for command in (
            "git add a b",
            "git add --intent-to-add -- a",
            "git add --intent-to-add a",
            "git add -- a/b.txt",
            "git rm --cached a",
            "git rm --cached -- a",
            "git rm -- a",
            "git add",
            "git rm",
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    HOOK.is_valid_git_staging_command(command), (True, None)
                )

    def test_rejected_commands(self):
        """Other flags, globs, directories, and option-only commands fail."""
        for command, message in (
            ("git add -A", "Flags not allowed: -A"),
            ("git add .", "Invalid filename pattern: ."),
            ("git add ..", "Invalid filename pattern: .."),
            ("git add *.py", "Invalid filename pattern: *.py"),
            ("git add --intent-to-add", "No files specified after --intent-to-add"),
            ("git add --intent-to-add --", "No files specified after --"),
            ("git add --", "No files specified after --"),
            ("git add -- -x", "Flags not allowed: -x"),
            ("git add -- ..", "Invalid filename pattern: .."),
            ("git rm --cached -- .", "Invalid filename pattern: ."),
            ("git add a --intent-to-add", "Flags not allowed: --intent-to-add"),
            (
                "git add --intent-to-add --intent-to-add a",
                "Flags not allowed: --intent-to-add",
            ),
            ("git add -N a", "Flags not allowed: -N"),
            ("git rm --cached", "No files specified after --cached"),
            ("git add --cached a", "Flags not allowed: --cached"),
            ("git rm --intent-to-add a", "Flags not allowed: --intent-to-add"),
            ("git status", "Not a staging command"),
            ("git", "Too few arguments"),
            ("ls a", "Not a git command"),
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    HOOK.is_valid_git_staging_command(command), (False, message)
                )


class HasShellOperatorsTest(unittest.TestCase):
    """has_shell_operators flags compound commands."""

    def test_operators(self):
        """Compound commands are detected."""
        for command in (
            "git add a && git commit",
            "git add a; git status",
            "git add a | cat",
            "git add $(ls)",
        ):
            with self.subTest(command=command):
                self.assertTrue(HOOK.has_shell_operators(command))

    def test_plain_command(self):
        """A single staging command is not compound."""
        self.assertFalse(HOOK.has_shell_operators("git add --intent-to-add -- a"))


class MainTest(unittest.TestCase):
    """main() allows or denies the Bash tool call from its stdin JSON."""

    def test_allows_intent_to_add(self):
        """The intent-to-add form passes silently."""
        self.assertEqual(run_main("Bash", "git add --intent-to-add -- a"), (0, ""))

    def test_allows_non_bash_tool(self):
        """Only Bash tool calls are inspected."""
        self.assertEqual(run_main("Read", "git add -A"), (0, ""))

    def test_allows_non_staging_git_command(self):
        """Non-staging git commands pass, operators included."""
        self.assertEqual(run_main("Bash", "git status && git log"), (0, ""))

    def test_denies_wildcard(self):
        """A wildcard stage is denied with the self-contained rule."""
        self.assertEqual(
            run_main("Bash", "git add -A"),
            (0, denial("Blocked: Flags not allowed: -A" + ALLOWLIST_REASON_SUFFIX)),
        )

    def test_denies_compound(self):
        """A compound staging command is denied."""
        self.assertEqual(
            run_main("Bash", "git add a && git commit -m x"),
            (0, denial(COMPOUND_REASON)),
        )


if __name__ == "__main__":
    unittest.main()
