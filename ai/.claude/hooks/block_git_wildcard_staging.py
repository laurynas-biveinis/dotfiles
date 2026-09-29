#!/usr/bin/env python3
# pylint: disable=broad-exception-caught,duplicate-code
"""Block git add/rm commands that use glob patterns or directories.

This hook uses pure allowlist validation: only commands matching the exact
pattern 'git [-C <path>] add|rm [<option>] [--] file1 file2 ...' with simple
filenames are allowed, where the one option is --intent-to-add for add and
--cached for rm; 'git stage' is checked as 'git add'.
The command is parsed rather than searched as text: heredoc bodies, comments
and line continuations are dropped, and the rest is split into words by
shlex's POSIX quoting rules and into simple commands at separators, not at
redirections. Every 'git' word, path-qualified or not, in any simple command,
subshell, or command or process substitution, starts a git command, so quoted
text such as a commit message is never mistaken for one, while a quoted or
backslash-escaped path with spaces is one filename. Unquoted text counts too,
as in 'echo git add -A': taking git at command position only would need every
wrapper that runs one (env, xargs, sudo, ...). A word the shell may expand to
'git' also starts one, before a literal staging subcommand. Unbalanced
quoting is rejected, as is a staging subcommand behind a variable assignment
or any global option other than -C, and a subcommand that the shell would
still expand ($, backticks, braces, zsh glob groups and operators, or a glob
that a staging subcommand's name matches). So is any subcommand after a -c or
--config-env that defines an alias or includes config. A staging command must
be the only simple command, without separators, redirections or line breaks.
All other patterns including other flags, glob patterns, and directories are
rejected. Commands handed to another interpreter as a string (bash -c, eval,
ssh), aliases from git config files, a git word and subcommand that both come
from expansions ($GIT $ADD), and an unquoted expansion in a global option's
value that vanishes or splits into several words (git -C $() . add) are not
inspected.
"""

import fnmatch
import json
import re
import shlex
import sys

# The one option each subcommand may carry, directly after it. git stage is a
# synonym for git add.
ALLOWED_OPTIONS = {
    "add": "--intent-to-add",
    "stage": "--intent-to-add",
    "rm": "--cached",
}

# Git's global options that take their value as the next word, rather than
# after '=' (git 2.55). Git rejects unknown or abbreviated global options.
GIT_OPTIONS_WITH_VALUE = {
    "-C",
    "-c",
    "--attr-source",
    "--config-env",
    "--git-dir",
    "--namespace",
    "--shallow-file",
    "--work-tree",
}

# The characters by which the shell may turn a word into a different one, e.g.
# ${x:-add}, {add,} or $(echo add), and zsh's glob groups such as (add) or
# a(d)d and extended-glob operators # ^ ~, whose matches fnmatch cannot tell.
# A leading ~ is tilde expansion, which yields a path.
EXPANSION_CHARS = re.compile(r"[$`{()#^]|(?<=.)~")

# A git config setting that defines a command or pulls in config that may:
# alias.*, include.path or includeIf.*.path. Git config sections ignore case.
COMMAND_CONFIG = re.compile(r"(?i)(alias|include|includeif)\.")

# A shell variable assignment word, NAME=value.
ASSIGNMENT = re.compile(r"[A-Za-z_][A-Za-z0-9_]*=")

# The characters that make a word a glob pattern, expanded to matching files.
GLOB_CHARS = re.compile(r"[*?\[]")

# A heredoc operator, not a <<< here-string, with its '-' flag and delimiter
# word.
HEREDOC_OPERATOR = re.compile(
    r"(?<!<)<<(?!<)(-?)[ \t]*((?:'[^']*'|\"[^\"]*\"|\\.|[^\s;&|<>()'\"\\])+)"
)

# Everything up to the next line feed, which '.' does not match.
REST_OF_LINE = re.compile(".*")

# Stands in for a command or process substitution in the text split into
# words: a word part that the shell expands.
SUBSTITUTION = "$_"

# The characters a filename may contain, as a regex character-class body.
FILENAME_CHARS = r"a-zA-Z0-9 /_.\-#~@"

# The characters of the shell's control and redirection operators, which end a
# word. Parentheses do not: zsh globs with them inside and next to words, and a
# subshell's ( is stripped where it matters, from a git word.
OPERATOR_CHARS = ";&|<>\n"

# Private-use stand-ins for operator characters that quoting or a backslash
# made literal, which the word splitter would otherwise read as operators.
QUOTED_OPERATORS = {char: chr(0xE000 + n) for n, char in enumerate(OPERATOR_CHARS)}
UNQUOTE_OPERATORS = str.maketrans({v: k for k, v in QUOTED_OPERATORS.items()})

# The characters after which a # starts a comment. After ( ) < > it belongs to
# a glob or a redirection target instead.
COMMENT_START_AFTER = " \t\n;&|"

# One operator in a run of OPERATOR_CHARS. A redirection, the ones with < or >,
# takes the next word as its target and leaves the simple command going on.
OPERATOR = re.compile(r"<<<|<<|<>|<&|>&|>>\|?|>\||&>>?|&&|\|\||\|&|(?s:.)")


def filename_problem(arg):
    """Return why arg is not a simple filename, or None if it is one.

    Simple filenames contain only: alphanumeric, space, /, -, _, ., #, ~, @
    Rejects: . and .. (directory shortcuts that stage entire directories), and
    names made only of spaces
    """
    if arg in [".", ".."]:
        return "directory shortcut"
    if not arg.strip(" "):
        return "blank name"
    disallowed = dict.fromkeys(re.findall(f"[^{FILENAME_CHARS}]", arg))
    if disallowed:
        return f"disallowed characters: {', '.join(map(repr, disallowed))}"
    return None


def staging_problem(assignments, global_options, subcommand, arguments):
    """Return why '<assignments> git <global_options> <subcommand> <arguments>'
    breaks the allowed pattern: git [-C <path>] add|rm [<allowed option>] [--]
    file1 ...

    Returns None if it follows the pattern.
    """
    if assignments:
        # Git reads GIT_DIR, GIT_WORK_TREE and more from its environment
        return f"Environment assignments not allowed: {assignments[0]}"
    for option in global_options:
        if option != "-C":
            return f"Global options other than -C not allowed: {option}"
    if subcommand not in ALLOWED_OPTIONS:
        return f"Expansion in git subcommand: {subcommand}"
    return arguments_problem(ALLOWED_OPTIONS[subcommand], arguments)


def arguments_problem(allowed_option, arguments):
    """Return why a staging subcommand's arguments break the allowed pattern
    [<allowed option>] [--] file1 file2 ..., or None if they follow it."""
    # Allow 'git add' or 'git rm' with no arguments (shows usage)
    if not arguments:
        return None

    file_args_start = 0
    if arguments[file_args_start] == allowed_option:
        file_args_start += 1
    if file_args_start < len(arguments) and arguments[file_args_start] == "--":
        file_args_start += 1
    if file_args_start == len(arguments):
        return f"No files specified after {arguments[-1]}"

    # Validate all file arguments
    for argument in arguments[file_args_start:]:
        if argument.startswith("-"):
            return f"Flags not allowed: {argument}"
        problem = filename_problem(argument)
        if problem:
            return f"Invalid filename pattern: {argument} ({problem})"

    return None


def scan(command, start, substitutions, in_substitution, in_arithmetic=False):
    """Return the text of command from start as the shell splits it into
    commands, and the index past the ')' ending the command substitution that
    start is in, if in_substitution and it ends; in_arithmetic if that is a
    $(( … )) one.

    Heredoc bodies and comments are dropped, as they are data. Each command
    substitution becomes SUBSTITUTION, and its body is added to substitutions.
    """
    text, quote, delimiters, i = [], None, [], start
    word_start = True
    # For each open unquoted (, whether it is in arithmetic, where << shifts
    groups = [in_arithmetic]
    while i < len(command):
        char = command[i]
        substitution = quote != "'" and substitution_at(
            command, i, quote is None, word_start
        )
        heredoc = (
            quote is None and not groups[-1] and HEREDOC_OPERATOR.match(command, i)
        )
        if quote == "'":
            if char == "'":
                quote = None
        elif char == "\\":
            escaped = command[i + 1 : i + 2]
            # A backslash-newline is a line continuation, which the shell drops
            text.append(
                "" if escaped == "\n" else "\\" + QUOTED_OPERATORS.get(escaped, escaped)
            )
            i += 2
            word_start = False
            continue
        elif substitution:
            substitutions.append(substitution[0])
            i = substitution[1]
            text.append(SUBSTITUTION)
            word_start = False
            continue
        elif quote is None and char == "#" and word_start:
            i = REST_OF_LINE.match(command, i).end()
            continue
        elif heredoc:
            delimiters.append((heredoc[1] == "-", re.sub(r"[\"'\\]", "", heredoc[2])))
            drop_file_descriptor(text, command, i, quote)
            text.append(heredoc[0])
            i = heredoc.end()
            word_start = False
            continue
        elif quote is None and char == "\n" and delimiters:
            text.append(char)
            i = heredoc_end(command, i + 1, delimiters) or i + 1
            delimiters = []
            word_start = True
            continue
        elif quote is None and char == "'":
            quote = char
        elif char == '"':
            quote = None if quote else char
        elif quote is None and char in "()":
            if char == ")" and len(groups) == 1 and in_substitution:
                return "".join(text), i + 1
            track_group(groups, char, word_start and command.startswith("((", i))
        drop_file_descriptor(text, command, i, quote)
        text.append(QUOTED_OPERATORS.get(char, char) if quote else char)
        i += 1
        word_start = quote is None and char in COMMENT_START_AFTER
    return "".join(text), None


def drop_file_descriptor(text, command, i, quote):
    """Before an unquoted redirection at command[i], drop from text the file
    descriptor number it applies to, as in 2>: a word of digits right before it.

    Left in, it would pass for the subcommand in git 2>x add. Spaced off, as in
    git -C 2 >x add, the digits are an ordinary word.
    """
    if quote is not None or command[i] not in "<>":
        return
    start = len(text)
    while start > 0 and text[start - 1] in "0123456789":
        start -= 1
    if start < len(text) and (start == 0 or text[start - 1] in " \t" + OPERATOR_CHARS):
        del text[start:]


def track_group(groups, paren, opens_arithmetic):
    """Open or close a ( group in groups, the arithmetic flags of scan()."""
    if paren == "(":
        groups.append(groups[-1] or opens_arithmetic)
    else:
        del groups[max(1, len(groups) - 1) :]  # the base entry stays


def substitution_at(command, i, unquoted, word_start):
    """Return the body of the command or process substitution at index i and
    the index past it, or None if none starts there.

    Unquoted, <( and >( start a process substitution, and so does zsh's =( at
    the start of a word.
    """
    process_starts = ("<(", ">(", "=(") if word_start else ("<(", ">(")
    if command.startswith("$(", i) or (
        unquoted and command.startswith(process_starts, i)
    ):
        _, end = scan(command, i + 2, [], True, command.startswith("$((", i))
        if end is not None:
            return command[i + 2 : end - 1], end
    elif command.startswith("`", i):
        end = i + 1
        while end < len(command) and command[end] != "`":
            end += 2 if command[end] == "\\" else 1
        if end < len(command):
            # Inside backticks, a backslash escapes only $, ` and itself
            return re.sub(r"\\([$`\\])", r"\1", command[i + 1 : end]), end + 1
    return None


def heredoc_end(command, start, delimiters):
    """Return the index past the delimiter lines of the heredoc bodies that
    begin at start, or None if one is missing: the shell would then read the
    rest as the body, but reading it as commands hides nothing from the check.
    """
    for strip_tabs, delimiter in delimiters:
        tabs = "\t*" if strip_tabs else ""
        line = re.compile(f"^{tabs}{re.escape(delimiter)}$", re.M).search(
            command, start
        )
        if not line:
            return None
        start = line.end() + 1
    return min(start, len(command))


def simple_commands(command):
    """Split command into the word lists of its simple commands, those in its
    command substitutions included, and list the unquoted operators met
    outside the substitutions.

    Raises ValueError on unbalanced quoting.
    """
    substitutions = []
    text, _ = scan(command, 0, substitutions, False)
    lexer = shlex.shlex(text, posix=True, punctuation_chars=OPERATOR_CHARS)
    # The shell keeps a carriage return inside a word. Splitting on it can only
    # reveal more git words, and is_compound denies staging with one.
    lexer.whitespace = " \t\r"
    lexer.whitespace_split = True
    lexer.commenters = ""
    commands, operators = [[]], []
    redirection_target = False
    for token in lexer:
        if token and set(token) <= set(OPERATOR_CHARS):
            for operator in OPERATOR.findall(token):
                operators.append(operator)
                redirection_target = "<" in operator or ">" in operator
                if not redirection_target:
                    commands.append([])
        elif redirection_target:
            redirection_target = False
        else:
            commands[-1].append(token.translate(UNQUOTE_OPERATORS))
    for body in substitutions:
        commands.extend(simple_commands(body)[0])
    return commands, operators


def staging_commands(commands):
    """Yield (assignments, global options, subcommand, arguments) for each git
    staging command in the word lists of simple commands: the variable
    assignments ahead of it in its simple command, and the global options
    without their values.

    A 'git' word anywhere in a simple command counts, so that prefixes such as
    'env' or 'xargs' cannot hide one. So does a word the shell may expand to
    git, but only before a literal staging subcommand: before any word that
    may expand, it would take in most commands passing variables around.
    """
    for words in commands:
        for i, word in enumerate(words):
            # The command name, without a subshell's (, zsh's = that expands a
            # command name to its path, or a path such as /usr/bin/
            name = word.lstrip("(").removeprefix("=").rsplit("/", 1)[-1]
            if name.startswith("git$"):
                # zsh expands git${=IFS}add to the two words git and add
                staging = parse_git_staging([name[len("git") :]] + words[i + 1 :])
            elif name == "git":
                staging = parse_git_staging(words[i + 1 :])
            elif EXPANSION_CHARS.search(word) or glob_matches(name, ["git"]):
                staging = parse_git_staging(words[i + 1 :])
                if staging and may_expand_to_staging(staging[1]):
                    staging = None
            else:
                continue
            if staging:
                prefix = [w.lstrip("(") for w in words[:i]]
                yield ([w for w in prefix if ASSIGNMENT.match(w)], *staging)


def parse_git_staging(words):
    """Return (global options, subcommand, arguments) if the words after 'git'
    name a subcommand that stages or may stage, else None. Any subcommand may
    stage after a -c or --config-env that defines an alias or includes config.

    The global options are listed without their values.
    """
    global_options, configs = [], []
    subcommand_at = 0
    while subcommand_at < len(words) and words[subcommand_at].startswith("-"):
        option = words[subcommand_at]
        global_options.append(option)
        if option in ("-c", "--config-env"):
            configs += words[subcommand_at + 1 : subcommand_at + 2]
        elif option.startswith("--config-env="):
            configs.append(option.partition("=")[2])
        subcommand_at += 2 if option in GIT_OPTIONS_WITH_VALUE else 1
    if subcommand_at >= len(words):
        return None
    subcommand = words[subcommand_at]
    if not (
        subcommand in ALLOWED_OPTIONS
        or may_expand_to_staging(subcommand)
        or any(COMMAND_CONFIG.match(config) for config in configs)
    ):
        return None
    return global_options, subcommand, words[subcommand_at + 1 :]


def may_expand_to_staging(word):
    """Whether the shell may expand word to a staging subcommand's name."""
    return bool(EXPANSION_CHARS.search(word)) or glob_matches(word, ALLOWED_OPTIONS)


def glob_matches(word, names):
    """Whether word is a glob pattern that a file with one of names matches, so
    the shell would expand it to that name in a directory holding the file."""
    return bool(GLOB_CHARS.search(word)) and any(
        fnmatch.fnmatchcase(name, word) for name in names
    )


def is_compound(command, commands, operators):
    """Check whether command, split by simple_commands() into commands and
    operators, is more than one simple command, or has an operator or a line
    break, ignoring edge line feeds."""
    # Line breaks are searched for in the raw text, quoted or not. A carriage
    # return is no shell separator, but the split treats it as whitespace, so
    # allowing it would let one garbled argument pass as several filenames.
    # The shell drops line feeds and blanks at either end but keeps a carriage
    # return there as part of the adjacent word, so only the former are
    # stripped, and edge line feeds among the operators do not count.
    if any(char in command.strip(" \t\n") for char in "\n\r"):
        return True
    return any(operator != "\n" for operator in operators) or (
        len([words for words in commands if words]) > 1
    )


def deny(reason):
    """Deny the tool call for reason and exit."""
    output = {
        "hookSpecificOutput": {
            "hookEventName": "PreToolUse",
            "permissionDecision": "deny",
            "permissionDecisionReason": reason,
        }
    }
    print(json.dumps(output))
    sys.exit(0)


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

    try:
        commands, operators = simple_commands(command)
    # Passing an unparsable command through would let it hide a staging command
    except ValueError as e:
        commands, operators, problems = [], [], [f"Unparsable quoting: {e}"]
    except RecursionError:
        commands, operators = [], []
        problems = ["Unparsable command: nested too deeply"]
    else:
        problems = [staging_problem(*staging) for staging in staging_commands(commands)]
    if not problems:
        # Not a git staging command, pass through
        sys.exit(0)

    # Check for shell operators in git staging commands
    if is_compound(command, commands, operators):
        deny(
            "Blocked: git staging commands cannot be used in compound commands. "
            "Run git add/rm in its own Bash tool call, on one line, without "
            "shell operators like &&, ||, ;, or |."
        )

    # Validate against allowlist pattern
    for problem in problems:
        if not problem:
            continue
        deny(
            f"Blocked: {problem}. "
            "Stage individual files: "
            "'git [-C <path>] add [--intent-to-add] [--] file1 file2 ...' or "
            "'git [-C <path>] rm [--cached] [--] file1 file2 ...'. The working "
            "tree may hold unrelated changes, files not meant to be tracked, "
            "or the user's own parallel work, so no other flags, glob "
            "patterns, directories, or shell operators are allowed."
        )

    # Command matches allowlist, allow it
    sys.exit(0)


if __name__ == "__main__":
    main()
