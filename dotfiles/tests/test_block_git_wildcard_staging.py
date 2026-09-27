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
    "git add/rm in its own Bash tool call, on one line, without shell "
    "operators like &&, ||, ;, or |."
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
            'git add "a b"',
            "git add 'a b'",
            "git add a\\ b",
            "git add emacs/.emacs.d/elpa/lsp-treemacs-0.5/icons/eclipse/boolean@2x.png",
            "git rm --cached a",
            "git rm --cached -- a",
            'git rm --cached -- "a b"',
            'git add "--" a',
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
            ("git add .", "Invalid filename pattern: . (directory shortcut)"),
            ("git add ..", "Invalid filename pattern: .. (directory shortcut)"),
            (
                "git add *.py",
                "Invalid filename pattern: *.py (disallowed characters: '*')",
            ),
            (
                'git add "*.py"',
                "Invalid filename pattern: *.py (disallowed characters: '*')",
            ),
            ('git add "a b', "Unparsable quoting: No closing quotation"),
            ("git add a\\", "Unparsable quoting: No escaped character"),
            ('git add " "', "Invalid filename pattern:   (blank name)"),
            ('git add ""', "Invalid filename pattern:  (blank name)"),
            # main() denies these two as compound commands before validating.
            (
                "git add \\\nfile1.py",
                "Invalid filename pattern: \nfile1.py (disallowed characters: '\\n')",
            ),
            (
                'git add "a\n"',
                "Invalid filename pattern: a\n (disallowed characters: '\\n')",
            ),
            (
                "git add a+b",
                "Invalid filename pattern: a+b (disallowed characters: '+')",
            ),
            (
                "git add a*+b",
                "Invalid filename pattern: a*+b (disallowed characters: '*', '+')",
            ),
            (
                "git add a**b",
                "Invalid filename pattern: a**b (disallowed characters: '*')",
            ),
            ("git add --intent-to-add", "No files specified after --intent-to-add"),
            ("git add --intent-to-add --", "No files specified after --"),
            ("git add --", "No files specified after --"),
            ("git add -- -x", "Flags not allowed: -x"),
            ('git add "-A"', "Flags not allowed: -A"),
            ('git add -- "-x"', "Flags not allowed: -x"),
            ("git add -- ..", "Invalid filename pattern: .. (directory shortcut)"),
            (
                "git rm --cached -- .",
                "Invalid filename pattern: . (directory shortcut)",
            ),
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
            "git add a\ngit push",
            "git add a\rgit push",
        ):
            with self.subTest(command=command):
                self.assertTrue(HOOK.has_shell_operators(command))

    def test_plain_command(self):
        """A single staging command is not compound, even with edge line feeds."""
        for command in ("git add --intent-to-add -- a", "git add a\n", "\ngit add a"):
            with self.subTest(command=command):
                self.assertFalse(HOOK.has_shell_operators(command))


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

    def test_allows_edge_line_feed(self):
        """A line feed at either end joins no commands, so it is no operator."""
        for command in ("git add a\n", "\ngit add a", "git add a\n \n"):
            with self.subTest(command=command):
                self.assertEqual(run_main("Bash", command), (0, ""))

    def test_denies_irregular_whitespace(self):
        """Any whitespace between git and the subcommand still reaches validation."""
        for command in ("git  add -A", "git\tadd -A", "git\trm -A"):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command),
                    (
                        0,
                        denial(
                            "Blocked: Flags not allowed: -A" + ALLOWLIST_REASON_SUFFIX
                        ),
                    ),
                )

    def test_denies_wildcard(self):
        """A wildcard stage is denied with the self-contained rule."""
        self.assertEqual(
            run_main("Bash", "git add -A"),
            (0, denial("Blocked: Flags not allowed: -A" + ALLOWLIST_REASON_SUFFIX)),
        )

    def test_denies_unparsable_quoting(self):
        """Unbalanced quoting is denied instead of escaping as an exception."""
        self.assertEqual(
            run_main("Bash", 'git add "a b'),
            (
                0,
                denial(
                    "Blocked: Unparsable quoting: No closing quotation"
                    + ALLOWLIST_REASON_SUFFIX
                ),
            ),
        )

    def test_allows_quoted_path(self):
        """A quoted path with spaces reaches git as one filename."""
        self.assertEqual(run_main("Bash", 'git add "a b"'), (0, ""))

    def test_denies_quoted_directory_shortcut(self):
        """Quoting does not hide a directory shortcut."""
        self.assertEqual(
            run_main("Bash", 'git add "."'),
            (
                0,
                denial(
                    "Blocked: Invalid filename pattern: . (directory shortcut)"
                    + ALLOWLIST_REASON_SUFFIX
                ),
            ),
        )

    def test_denies_multiline(self):
        """A line break in the command is denied like the other operators."""
        for command in (
            "git add a\ngit push",
            "git add a\rgit push",
            "git\nadd -A",
            "git\radd -A",
            "git\nrm -A",
            "git add a\r",
            "\rgit add a",
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command), (0, denial(COMPOUND_REASON))
                )

    def test_denies_compound(self):
        """A compound staging command is denied."""
        self.assertEqual(
            run_main("Bash", "git add a && git commit -m x"),
            (0, denial(COMPOUND_REASON)),
        )


if __name__ == "__main__":
    unittest.main()
