"""Codex rule suggestion for xml2qif: prompting, validation, and config file
insertion."""

import configparser
import contextlib
import copy
import json
import os
import re
import subprocess
import tempfile
import xml.etree.ElementTree as ET

from xml2qif_rules import (
    MissingRuleError,
    RuleConfigError,
    RuleSuggestionError,
    collect_rules,
    find_transaction_rule,
    is_control_character,
    quote_untrusted_lines,
    read_config_text,
    replace_control_characters,
)

CODEX_TIMEOUT_SECONDS = 300
# Exclude newline from the name so a match cannot span lines: configparser's
# grammar is per-line, and a line-spanning match would feed a headerless line
# to configparser_header_name
SECTION_HEADER_RE = re.compile(r"^\[([^\]\n]+)\]", re.MULTILINE)


def transaction_xml_string(xml_obj):
    """Serialize a transaction XML node for prompting."""
    xml_copy = copy.deepcopy(xml_obj)
    ET.indent(xml_copy, space="  ")
    return ET.tostring(xml_copy, encoding="unicode")


def build_rule_suggestion_prompt(xml_transaction, rule_type, config_text):
    """Build the prompt for Codex to suggest a new rule."""
    return f"""Suggest one new xml2qif configuration rule.

The existing configuration is an INI file. It has deliberate grouping and order.
Choose the best existing section after which to insert the new rule.

Return exactly one JSON object and no prose. Use one of these schemas.

If you have a defensible suggestion:
{{
  "action": "suggest",
  "confidence": 75,
  "insert_after": "Existing section name",
  "config_entry": "[New Section]\\nmatch = ^...$\\npayee = ...\\ncategory = ..."
}}

If you cannot make a defensible suggestion:
{{
  "action": "punt",
  "confidence": 30,
  "reason": "Short explanation"
}}

Rules:
- Prefer action "punt" over a low-quality or guessed rule.
- confidence is an integer from 0 to 100.
- The config_entry must contain exactly one INI section.
- Required keys are match, payee, and category.
- Optional keys are type and amount.
- The regex in match must match one of the transaction text fields used by the
  script: debitor first, then creditor, then unstructured remittance info.
- If the derived rule type is useful, include a type key with that value.
- Do not duplicate an existing section name.
- Preserve the naming, category, regex, and ordering style of the existing file.

Derived rule type: {rule_type}

Unmatched transaction XML:
```xml
{transaction_xml_string(xml_transaction.xml_obj)}
```

Existing ~/.xml2qifrc:
```ini
{config_text}
```
"""


def run_codex_rule_suggestion(prompt):
    """Run Codex and return the final response text."""
    output_fd, output_path = tempfile.mkstemp()
    os.close(output_fd)
    try:
        try:
            # An empty cwd keeps the agent's habitual directory exploration
            # from finding anything; the read-only sandbox itself still lets
            # it read any local file it names
            with tempfile.TemporaryDirectory() as empty_dir:
                result = subprocess.run(
                    [
                        "codex",
                        "exec",
                        "--sandbox",
                        "read-only",
                        "--skip-git-repo-check",
                        "--color",
                        "never",
                        "--output-last-message",
                        output_path,
                        "-",
                    ],
                    input=prompt,
                    capture_output=True,
                    # Codex output is untrusted; tolerant decoding keeps a
                    # stray invalid byte from aborting with a raw traceback
                    encoding="utf-8",
                    errors="replace",
                    timeout=CODEX_TIMEOUT_SECONDS,
                    check=False,
                    cwd=empty_dir,
                )
        except subprocess.TimeoutExpired:
            raise RuleSuggestionError(
                f"Codex timed out after {CODEX_TIMEOUT_SECONDS} s"
            ) from None
        except FileNotFoundError:
            raise RuleSuggestionError("codex executable not found") from None
        with open(output_path, encoding="utf-8", errors="replace") as output_file:
            last_message = output_file.read()
    finally:
        os.unlink(output_path)

    if result.returncode != 0:
        raise RuleSuggestionError(
            "Codex failed while suggesting a rule.\n"
            f"stdout:\n{quote_untrusted_lines(result.stdout)}\n"
            f"stderr:\n{quote_untrusted_lines(result.stderr)}"
        )

    return last_message.strip()


def strip_json_fence(text):
    """Strip a Markdown JSON fence if Codex ignored the no-prose instruction."""
    stripped = text.strip()
    if not stripped.startswith("```"):
        return stripped

    lines = stripped.splitlines()
    if lines[0].startswith("```") and lines[-1].strip() == "```":
        return "\n".join(lines[1:-1]).strip()
    return stripped


def reject_control_characters(field_name, value):
    """Reject Codex text whose control characters could alter what the
    terminal shows next to the confirmation prompt or persist into the
    config file."""
    if any(is_control_character(c) for c in value):
        raise RuleSuggestionError(f"Codex {field_name} contains control characters")


def normalize_line_endings(value):
    """Normalize CRLF to LF before the control-character scan: a line-ending
    misformat must not discard the reply, while a lone \\r — a
    terminal-spoofing primitive — stays rejected."""
    return value.replace("\r\n", "\n")


def normalize_confidence(value):
    """Normalize the display-only confidence field to an int in 0-100 or None.

    confidence never gates control flow, so a Codex misformat here must not
    discard an otherwise valid suggestion."""
    # bool is an int subclass, so it must be screened before the int check
    if isinstance(value, bool):
        return None
    # isdecimal(), not isdigit(): the latter also accepts Unicode digits like
    # "²" that int() cannot parse, which would raise ValueError here
    if isinstance(value, str) and value.isdecimal():
        value = int(value)
    if isinstance(value, int) and 0 <= value <= 100:
        return value
    return None


def format_confidence(confidence):
    """Format a normalized confidence for terminal display."""
    return "unknown" if confidence is None else f"{confidence}%"


def parse_rule_suggestion(suggestion_text):
    """Parse Codex's JSON rule suggestion."""
    try:
        suggestion = json.loads(strip_json_fence(suggestion_text))
    except json.JSONDecodeError as exc:
        raise RuleSuggestionError(
            f"Codex returned invalid JSON:\n{quote_untrusted_lines(suggestion_text)}"
        ) from exc

    if not isinstance(suggestion, dict):
        raise RuleSuggestionError("Codex suggestion must be a JSON object")

    action = suggestion.get("action")
    confidence = normalize_confidence(suggestion.get("confidence"))
    if action not in ("suggest", "punt"):
        raise RuleSuggestionError("Codex suggestion lacks valid action")

    if action == "punt":
        reason = suggestion.get("reason", "")
        if not isinstance(reason, str):
            raise RuleSuggestionError("Codex punt reason must be a string")
        reason = normalize_line_endings(reason)
        reject_control_characters("reason", reason)
        return {
            "action": action,
            "confidence": confidence,
            # Collapse line breaks: printed raw, a multi-line reason could
            # spoof a line of program output on the terminal
            "reason": " ".join(reason.split()),
        }

    insert_after = suggestion.get("insert_after")
    config_entry = suggestion.get("config_entry")
    if not isinstance(insert_after, str) or not insert_after.strip():
        raise RuleSuggestionError("Codex suggestion lacks insert_after")
    if not isinstance(config_entry, str) or not config_entry.strip():
        raise RuleSuggestionError("Codex suggestion lacks config_entry")
    insert_after = normalize_line_endings(insert_after)
    config_entry = normalize_line_endings(config_entry)
    reject_control_characters("insert_after", insert_after)
    reject_control_characters("config_entry", config_entry)

    return {
        "action": action,
        "confidence": confidence,
        "insert_after": insert_after.strip(),
        "config_entry": config_entry.strip(),
    }


def parse_single_rule(config_entry):
    """Parse a suggested rule entry and return its section and rule."""
    # interpolation=None must match load_rule_config so the validated rule
    # equals what a reload of the written file produces
    config = configparser.ConfigParser(interpolation=None)
    try:
        config.read_string(config_entry)
    except configparser.Error as exc:
        # Collapse line breaks and sanitize control characters: printed raw,
        # a multi-line or escape-laden entry could spoof or rewrite lines of
        # program output on the terminal
        single_line_entry = replace_control_characters(config_entry, single_line=True)
        raise RuleSuggestionError(
            f"Codex returned invalid INI config: {single_line_entry}"
        ) from exc

    # sections() hides DEFAULT, whose keys would leak into every existing rule
    if config.defaults():
        raise RuleSuggestionError("Suggested entry must not contain a DEFAULT section")

    sections = config.sections()
    if len(sections) != 1:
        raise RuleSuggestionError("Codex must suggest exactly one config section")

    section = sections[0]
    # The insertion grammar must read the entry exactly as configparser did,
    # or the written section could never anchor or duplicate-check correctly
    if SECTION_HEADER_RE.findall(config_entry.strip()) != [section]:
        raise RuleSuggestionError(
            f"Suggested section [{section}] is invisible to the insertion grammar"
        )

    # configparser folds indented continuation lines into the value with \n,
    # which QIF serialization would emit as a stray non-field line
    for key, value in config.items(section):
        if "\n" in value:
            raise RuleSuggestionError(f"Suggested {key} must be a single line")

    # A misspelled constraint key would be silently ignored by collect_rules,
    # widening the rule, and would persist into the config file
    unknown_keys = set(config.options(section)) - {
        "match",
        "payee",
        "category",
        "type",
        "amount",
    }
    if unknown_keys:
        raise RuleSuggestionError(
            f"Suggested section has unknown keys: {', '.join(sorted(unknown_keys))}"
        )

    for key in ("match", "payee", "category"):
        if not config.has_option(section, key):
            raise RuleSuggestionError(f"Suggested section lacks {key}")

    # An empty match regex would silently match every transaction
    if not config.get(section, "match").strip():
        raise RuleSuggestionError("Suggested match regex is empty")

    try:
        return section, collect_rules(config)[0]
    except RuleConfigError as exc:
        raise RuleSuggestionError("Suggested match regex is invalid") from exc


def validate_rule_suggestion(config_text, xml_transaction, rule_type, suggestion):
    """Validate a suggested rule and return normalized parts."""
    if suggestion["action"] != "suggest":
        raise RuleSuggestionError(
            f"Expected suggest action, got {suggestion['action']!r}"
        )

    insert_after = suggestion["insert_after"]
    config_entry = suggestion["config_entry"]
    # Same scan as the insertion, so the two cannot disagree about the anchor
    find_section_insert_index(config_text, insert_after)

    section, rule = parse_single_rule(config_entry)
    if section in existing_sections(config_text):
        raise RuleSuggestionError(f"Suggested section [{section}] already exists")

    if rule["type"] and rule["type"] != rule_type:
        raise RuleSuggestionError(
            f"Suggested type {rule['type']} does not match transaction type {rule_type}"
        )
    if rule["amount"] and rule["amount"] != xml_transaction.amount:
        raise RuleSuggestionError(
            f"Suggested amount {rule['amount']} does not match {xml_transaction.amount}"
        )

    if find_transaction_rule([rule], xml_transaction, rule_type) is None:
        raise RuleSuggestionError("Suggested rule does not match the transaction")

    # Return section so insert_rule_after_section need not re-parse config_entry
    return insert_after, config_entry, section, suggestion["confidence"]


def existing_sections(config_text):
    """Return the section names a strict reload of config_text will see.

    Duplicate checks must use configparser's grammar, not the column-0
    insertion regex: configparser strips lines before matching, so an
    indented header it accepts would be invisible to the regex and the write
    would leave the config unloadable on the next parse."""
    config = configparser.ConfigParser(interpolation=None)
    try:
        config.read_string(config_text)
    except configparser.Error as exc:
        raise RuleSuggestionError(f"Config file became unparseable: {exc}") from exc
    return config.sections()


def back_up_over_comment_lines(config_text, header_start):
    """Move an insert offset before comment lines directly above a section
    header: they document that section and must stay attached to it."""
    index = header_start
    while index > 0:
        # Stop at index - 1, not index: index starts the current line, so the
        # \n at index - 1 ends the line above. Excluding it makes rfind find
        # the \n before that — the previous line's start. Stopping at index
        # would return that same \n, leaving line_start == index and an empty
        # slice that ends the loop without backing up.
        line_start = config_text.rfind("\n", 0, index - 1) + 1
        # configparser strips lines before comment matching, so an indented
        # comment is still a full-line comment
        if not config_text[line_start:index].lstrip().startswith(("#", ";")):
            break
        index = line_start
    return index


def configparser_header_name(config_text, header_start):
    """Read the section name configparser sees on the header line at the
    given offset: its greedy grammar reads [A]B] as section A]B where the
    column-0 insertion regex reads only A."""
    line_end = config_text.find("\n", header_start)
    if line_end == -1:
        line_end = len(config_text)
    header_line = config_text[header_start:line_end].strip()
    return configparser.ConfigParser.SECTCRE.match(header_line).group("header")


def find_section_insert_index(config_text, insert_after):
    """Find the text offset after a config section."""
    matches = list(SECTION_HEADER_RE.finditer(config_text))
    prefix_read_only = False
    for index, match in enumerate(matches):
        if match.group(1) != insert_after:
            continue
        # A prefix read of a header configparser parses differently (e.g.
        # [A] out of [A]B]) is not the anchor section; skip it so the anchor
        # resolves to a header both grammars read as insert_after
        if configparser_header_name(config_text, match.start()) != insert_after:
            prefix_read_only = True
            continue
        if index + 1 >= len(matches):
            return len(config_text)
        return back_up_over_comment_lines(config_text, matches[index + 1].start())

    # insert_after is untrusted Codex text that may carry an interior newline
    # (parse_rule_suggestion strips only edges); collapse it so a crafted anchor
    # cannot spoof a line of program output when the error is printed
    safe_anchor = replace_control_characters(insert_after, single_line=True)
    if prefix_read_only:
        raise RuleSuggestionError(
            f"Insert anchor [{safe_anchor}] is not a config section"
        )
    raise RuleSuggestionError(f"Insert anchor [{safe_anchor}] does not exist")


def insert_rule_after_section(config_path, insert_after, config_entry, *, section=None):
    """Insert a rule into the config file after an existing section.

    A caller that already parsed config_entry passes its section to skip the
    re-parse; a direct caller leaves it None and the section is parsed here."""
    config_text = read_config_text(config_path)
    # The file may have changed between validation and the user's confirmation;
    # a duplicate section would make the config unloadable on the next parse
    if section is None:
        section = parse_single_rule(config_entry)[0]
    if section in existing_sections(config_text):
        raise RuleSuggestionError(f"Suggested section [{section}] already exists")
    insert_index = find_section_insert_index(config_text, insert_after)

    before = config_text[:insert_index].rstrip()
    after = config_text[insert_index:]
    new_text = f"{before}\n\n{config_entry.strip()}\n"
    if after:
        new_text = f"{new_text}\n{after}"

    # Write-then-rename so an interrupted write cannot corrupt the config; the
    # temp file must be in the same directory for os.replace to be atomic
    config_dir = os.path.dirname(config_path)
    tmp_file = tempfile.NamedTemporaryFile(
        "w", dir=config_dir, delete=False, encoding="utf-8"
    )
    try:
        with tmp_file:
            tmp_file.write(new_text)
        # NamedTemporaryFile creates the file as 0600; keep the config's own mode
        os.chmod(tmp_file.name, os.stat(config_path).st_mode & 0o7777)
        os.replace(tmp_file.name, config_path)
    except BaseException:
        with contextlib.suppress(FileNotFoundError):
            os.unlink(tmp_file.name)
        raise


def suggest_and_confirm_rule(config_path, xml_transaction, rule_type):
    """Ask Codex for a rule, confirm it with the user, and update the config file."""
    config_text = read_config_text(config_path)
    prompt = build_rule_suggestion_prompt(xml_transaction, rule_type, config_text)

    print(
        f"No matching rule found. Sending transaction XML and {config_path} "
        "to Codex (OpenAI API); Codex may also read other local files..."
    )
    suggestion_text = run_codex_rule_suggestion(prompt)
    suggestion = parse_rule_suggestion(suggestion_text)
    if suggestion["action"] == "punt":
        print(
            "Codex could not suggest a rule "
            f"({format_confidence(suggestion['confidence'])} confidence): "
            f"{suggestion['reason']}"
        )
        raise MissingRuleError(xml_transaction, rule_type)

    insert_after, config_entry, section, confidence = validate_rule_suggestion(
        config_text, xml_transaction, rule_type, suggestion
    )

    print(
        f"\nCodex suggests inserting after [{insert_after}] "
        f"({format_confidence(confidence)} confidence):\n"
    )
    print(config_entry)
    try:
        answer = input(
            f"\nInsert this rule after [{insert_after}] and continue? [y/N] "
        )
    except EOFError:
        print("\nstdin closed; treating as decline")
        raise MissingRuleError(xml_transaction, rule_type) from None
    if answer.strip().lower() not in ("y", "yes"):
        raise MissingRuleError(xml_transaction, rule_type)

    insert_rule_after_section(config_path, insert_after, config_entry, section=section)
    print(f"Inserted rule into {config_path}")
