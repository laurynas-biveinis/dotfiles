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
    ". Stage individual files: "
    "'git [-C <path>] add [--intent-to-add] [--] file1 file2 ...' or "
    "'git [-C <path>] rm [--cached] [--] file1 file2 ...'. The working tree may hold "
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


def allowlist_denial(problem):
    """The JSON line the hook prints to deny a staging command for a problem."""
    return denial(f"Blocked: {problem}{ALLOWLIST_REASON_SUFFIX}")


class StagingValidationTest(unittest.TestCase):
    """main() allows only enumerated-file staging."""

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
            "git -C /tmp/x add a",
            "git -C a -C b rm --cached -- c",
            'git -C "$repo" add a',
            'git -C "a (copy)" add b',
            'git -C "D&D; a|b" add c',
            'git -C "<a>" add b',
            "git add a # see <b>",
        ):
            with self.subTest(command=command):
                self.assertEqual(run_main("Bash", command), (0, ""))

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
            (
                'git add ";"',
                "Invalid filename pattern: ; (disallowed characters: ';')",
            ),
            (
                "git add '&&'",
                "Invalid filename pattern: && (disallowed characters: '&')",
            ),
            (
                "git add \\|",
                "Invalid filename pattern: | (disallowed characters: '|')",
            ),
            (
                'git add "a>b"',
                "Invalid filename pattern: a>b (disallowed characters: '>')",
            ),
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command), (0, allowlist_denial(message))
                )

    def test_non_staging_commands(self):
        """Commands that stage nothing pass."""
        for command in (
            "git status",
            "git",
            "ls a",
            "git remote add origin x",
            "git --no-pager log",
            "git -c core.x=add log",
            "git --git-dir add log",
            "GIT_PAGER=cat git log",
            "grep -l git *.md",
            "cp *.py *.txt d",
            "grep -rn git ~/x",
        ):
            with self.subTest(command=command):
                self.assertEqual(run_main("Bash", command), (0, ""))


class StagingDetectionTest(unittest.TestCase):
    """main() finds staging commands where the shell would run them."""

    def test_allows_staging_phrase_in_argument(self):
        """Quoted text that mentions staging is an argument, not a command."""
        self.assertEqual(
            run_main("Bash", 'git commit -m "Fix git add handling"'), (0, "")
        )

    def test_ignores_heredoc_body(self):
        """A heredoc body is data: neither its quotes nor its git words count."""
        for command in (
            "git commit -F- <<'EOF'\nIt's git add -A\nEOF",
            'git commit -F- <<-"EOF"\n\tgit add -A\n\tEOF\n',
            "cat <<A <<B | git commit -F-\ngit add -A\nA\nit's\nB",
        ):
            with self.subTest(command=command):
                self.assertEqual(run_main("Bash", command), (0, ""))

    def test_ignores_heredoc_body_in_substitution(self):
        """A heredoc inside a quoted command substitution is data too."""
        self.assertEqual(
            run_main(
                "Bash",
                "git commit -m \"$(cat <<'EOF'\nIt's an odd \" (git add -A\nEOF\n)\"",
            ),
            (0, ""),
        )

    def test_denies_staging_in_substitution(self):
        """A command substitution is a compound command, quoted or not."""
        for command in (
            'echo "$(git add -A)"',
            'echo "`git add -A`"',
            "git -C $(pwd) add -A",
            "git -C `pwd` add -A",
            "git `echo add` -A",
            'echo "$(echo "$(git add a)")"',
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command), (0, denial(COMPOUND_REASON))
                )

    def test_reads_unterminated_heredoc_as_commands(self):
        """Without its delimiter line a heredoc hides nothing from the check."""
        self.assertEqual(
            run_main("Bash", "echo $((1<<2))\ngit add -A"), (0, denial(COMPOUND_REASON))
        )

    def test_reads_lines_after_shift_as_commands(self):
        """In arithmetic, << is a shift, with no heredoc body to hide lines in."""
        for command in (
            "((x = 1<<EOF))\ngit add -A\nEOF",
            "echo $((1<<EOF\n))\ngit add -A\nEOF",
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command), (0, denial(COMPOUND_REASON))
                )

    def test_reads_lines_after_here_string_as_commands(self):
        """A <<< here-string has no body to hide lines in."""
        self.assertEqual(
            run_main("Bash", "cat <<<X\ngit add -A\nX"), (0, denial(COMPOUND_REASON))
        )

    def test_ignores_comments(self):
        """A comment is no command, and its quotes are no quoting."""
        for command in (
            "# it's a note\ngit status",
            "git log # it's",
            "git status # git add -A",
            "git status\n  # git add -A",
        ):
            with self.subTest(command=command):
                self.assertEqual(run_main("Bash", command), (0, ""))

    def test_reads_hash_inside_word_as_text(self):
        """Only a # that starts an unquoted word starts a comment."""
        for command in ("git add a\\ #b -A", "git add file#2 -A", 'git add "#b" -A'):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command),
                    (0, allowlist_denial("Flags not allowed: -A")),
                )

    def test_joins_continued_lines(self):
        """A backslash-newline continues the command, as in the shell."""
        for command in ("git \\\nadd -A", "gi\\\nt add -A", 'git "ad\\\nd" -A'):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command), (0, denial(COMPOUND_REASON))
                )

    def test_keeps_command_across_redirection(self):
        """A redirection and its target leave the rest of the command intact."""
        for command in (
            "git >x add -A",
            "git 2>/dev/null add -A",
            "git 2>&1 add -A",
            "git >&2 add -A",
            "git <<<x add -A",
            "git <<EOF add -A\nEOF",
            "git -C 2 > log add -A",
            "git -C 2 2>log add -A",
            "git >#x add -A",
            "cat <(git add -A)",
            "cat >(git add -A)",
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command), (0, denial(COMPOUND_REASON))
                )

    def test_allows_line_feed_after_git(self):
        """A line feed ends the git command, leaving a subcommand-less git."""
        for command in ("git\nadd -A", "git\nrm -A"):
            with self.subTest(command=command):
                self.assertEqual(run_main("Bash", command), (0, ""))

    def test_denies_irregular_whitespace(self):
        """Any whitespace between git and the subcommand still reaches validation."""
        for command in ("git  add -A", "git\tadd -A", "git\trm -A"):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command),
                    (0, allowlist_denial("Flags not allowed: -A")),
                )

    def test_denies_quoted_or_prefixed_git(self):
        """Quoting the words or prefixing the command does not hide staging."""
        for command in (
            "git 'add' -A",
            '"git" add -A',
            "\\git add -A",
            "env git add -A",
            "git -C /tmp/x add -A",
            "git -C add add -A",
            "g?t add -A",
            "[g]it add -A",
            "/usr/bin/git add -A",
            "./git add -A",
            "(/opt/homebrew/bin/git add -A",
            "/usr/bin/$GIT add -A",
            "=git add -A",
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command),
                    (0, allowlist_denial("Flags not allowed: -A")),
                )

    def test_checks_expansion_as_git(self):
        """A word the shell may expand to git starts a git command."""
        for command, output in (
            ("`echo git` add -A", denial(COMPOUND_REASON)),
            ("$(echo git) add -A", denial(COMPOUND_REASON)),
            ("$GIT add -A", allowlist_denial("Flags not allowed: -A")),
            ("(g)it add -A", allowlist_denial("Flags not allowed: -A")),
        ):
            with self.subTest(command=command):
                self.assertEqual(run_main("Bash", command), (0, output))

    def test_allows_expansion_before_non_staging_word(self):
        """A word that may expand to git counts only before a literal staging
        subcommand, or every command passing variables around would."""
        for command in ('cp "$a" "$b"', 'cp * "$dst"', "g?t a* -A"):
            with self.subTest(command=command):
                self.assertEqual(run_main("Bash", command), (0, ""))

    def test_denies_expansion_in_subcommand(self):
        """A subcommand the shell may expand to a staging one cannot be checked."""
        for command, subcommand in (
            ("git ${x:-add} -A", "${x:-add}"),
            ("git ${x:-a}dd -A", "${x:-a}dd"),
            ("git {add,} -A", "{add,}"),
            ("git -C /tmp/x $sub -A", "$sub"),
            ("git${=IFS}add -A", "${=IFS}add"),
            ("git a* -A", "a*"),
            ("git ?m -r .", "?m"),
            ("git [s]tage -A", "[s]tage"),
            ("git [^x]dd -A", "[^x]dd"),
            ("git (add) -A", "(add)"),
            ("git a(d)d -A", "a(d)d"),
            ("git (#i)add -A", "(#i)add"),
            ("git ad#d -A", "ad#d"),
            ("git add~x -A", "add~x"),
            ("git ^x -A", "^x"),
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command),
                    (0, allowlist_denial(f"Expansion in git subcommand: {subcommand}")),
                )


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

    def test_denies_environment_assignments(self):
        """A variable assignment ahead of git can retarget it like an option."""
        for command, assignment in (
            ("GIT_DIR=/tmp/x git add a", "GIT_DIR=/tmp/x"),
            ("env GIT_WORK_TREE=/x git add a", "GIT_WORK_TREE=/x"),
            ("(GIT_INDEX_FILE=i git add a)", "GIT_INDEX_FILE=i"),
            ("GIT_TRACE=1 git add -A", "GIT_TRACE=1"),
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command),
                    (
                        0,
                        allowlist_denial(
                            f"Environment assignments not allowed: {assignment}"
                        ),
                    ),
                )

    def test_denies_other_global_options(self):
        """Only -C may come between git and a staging subcommand."""
        for command, option in (
            ("git --no-pager add a", "--no-pager"),
            ("git -c core.x=y add a", "-c"),
            ("git -C /tmp/x --git-dir=/x add a", "--git-dir=/x"),
            ("git --work-tree /x stage a", "--work-tree"),
            ("git -c alias.w=add w -A", "-c"),
            ("${GIT:=git} -c alias.w=add w -A", "-c"),
            ("git -c Include.path=/tmp/c w", "-c"),
            ("git --config-env=alias.w=W w -A", "--config-env=alias.w=W"),
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command),
                    (
                        0,
                        allowlist_denial(
                            f"Global options other than -C not allowed: {option}"
                        ),
                    ),
                )

    def test_checks_stage_as_add(self):
        """git stage, a synonym for git add, follows the same rules."""
        self.assertEqual(run_main("Bash", "git stage --intent-to-add a"), (0, ""))
        self.assertEqual(
            run_main("Bash", "git stage -A"),
            (0, allowlist_denial("Flags not allowed: -A")),
        )

    def test_denies_wildcard(self):
        """A wildcard stage is denied with the self-contained rule."""
        self.assertEqual(
            run_main("Bash", "git add -A"),
            (0, allowlist_denial("Flags not allowed: -A")),
        )

    def test_denies_too_deep_nesting(self):
        """Nesting past the parser's recursion limit is denied, not a crash."""
        command = "echo " + "$(" * 1000 + "git add -A" + ")" * 1000
        self.assertEqual(
            run_main("Bash", command),
            (0, allowlist_denial("Unparsable command: nested too deeply")),
        )

    def test_denies_unparsable_quoting(self):
        """Unbalanced quoting is denied instead of escaping as an exception."""
        self.assertEqual(
            run_main("Bash", 'git add "a b'),
            (0, allowlist_denial("Unparsable quoting: No closing quotation")),
        )

    def test_allows_quoted_path(self):
        """A quoted path with spaces reaches git as one filename."""
        self.assertEqual(run_main("Bash", 'git add "a b"'), (0, ""))

    def test_denies_quoted_directory_shortcut(self):
        """Quoting does not hide a directory shortcut."""
        self.assertEqual(
            run_main("Bash", 'git add "."'),
            (0, allowlist_denial("Invalid filename pattern: . (directory shortcut)")),
        )

    def test_denies_multiline(self):
        """A line break in the command is denied like the other operators."""
        for command in (
            "git add a\ngit push",
            "git add a\rgit push",
            "git\radd -A",
            "git add a\r",
            "\rgit add a",
            "git add \\\nfile1.py",
            'git add "a\n"',
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command), (0, denial(COMPOUND_REASON))
                )

    def test_denies_parentheses(self):
        """Parentheses stay in their words, where zsh may glob with them: a(:h)
        is '.'. A subshell's closing one fails its last argument."""
        for command, problem in (
            (
                "git add a(:h)",
                "Invalid filename pattern: a(:h) "
                "(disallowed characters: '(', ':', ')')",
            ),
            (
                "(git add a)",
                "Invalid filename pattern: a) (disallowed characters: ')')",
            ),
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command), (0, allowlist_denial(problem))
                )

    def test_denies_compound(self):
        """A staging command among others, or with a redirection, is denied."""
        for command in (
            "git add a && git commit -m x",
            "git add a; git status",
            "git add a | cat",
            "git add a & rm x",
            "git add $(ls)",
            "git add a > log",
            "git add a &",
            "git add a;",
        ):
            with self.subTest(command=command):
                self.assertEqual(
                    run_main("Bash", command), (0, denial(COMPOUND_REASON))
                )


if __name__ == "__main__":
    unittest.main()
