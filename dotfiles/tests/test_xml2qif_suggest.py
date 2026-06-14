"""Unit tests for the xml2qif_suggest module.

Covers the Codex suggestion parsing/validation helpers and the config-file
rewrite path, which replaces ~/.xml2qifrc and whose edge cases would otherwise
only surface after the original file is gone.
"""

import configparser
import contextlib
import importlib
import io
import json
import os
import shutil
import tempfile
import types
import unicodedata
import unittest
import unittest.mock
import xml.etree.ElementTree as ET

from xml2qif_test_utils import (
    COMMENTED_CONFIG_TEXT,
    CONFIG_TEXT,
    NEW_ENTRY,
    make_transaction,
    read_text,
    suggest_json,
    write_text,
)

RULES = importlib.import_module("xml2qif_rules")
SUGGEST = importlib.import_module("xml2qif_suggest")


class FindSectionInsertIndexTest(unittest.TestCase):
    """Tests for find_section_insert_index."""

    def test_middle_anchor_returns_next_header_start(self):
        """The anchor [First] must not match the [First Extra] header."""
        index = SUGGEST.find_section_insert_index(CONFIG_TEXT, "First")
        self.assertEqual(index, CONFIG_TEXT.index("[First Extra]"))

    def test_prefix_named_anchor_returns_following_header_start(self):
        """The anchor [First Extra] resolves to the start of [Last]."""
        index = SUGGEST.find_section_insert_index(CONFIG_TEXT, "First Extra")
        self.assertEqual(index, CONFIG_TEXT.index("[Last]"))

    def test_last_anchor_returns_text_length(self):
        """Inserting after the final section appends at end of text."""
        index = SUGGEST.find_section_insert_index(CONFIG_TEXT, "Last")
        self.assertEqual(index, len(CONFIG_TEXT))

    def test_missing_anchor_raises(self):
        """An anchor absent from the text is an error."""
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.find_section_insert_index(CONFIG_TEXT, "No Such Section")

    def test_prefix_collision_header_skipped_for_real_anchor(self):
        """A [A]B] header that the insertion regex prefix-reads as [A] must
        not capture the anchor when a real [A] section exists elsewhere: the
        insertion must land after the real [A] section the user confirmed."""
        config_text = (
            "[A]B]\nmatch = ^X$\npayee = P\ncategory = C\n\n"
            "[A]\nmatch = ^Y$\npayee = Q\ncategory = D\n\n"
            "[Last]\nmatch = ^Z$\npayee = R\ncategory = E\n"
        )
        index = SUGGEST.find_section_insert_index(config_text, "A")
        self.assertEqual(index, config_text.index("[Last]"))

    def test_line_spanning_anchor_rejected_with_curated_error(self):
        """A multi-line anchor matching a header the insertion regex reads
        across a line break must raise the curated error, not crash with an
        uncurated AttributeError from the header re-parse."""
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.find_section_insert_index("[A\nB]\nmatch = ^X$\n", "A\nB")

    def test_multiline_anchor_collapsed_in_error_message(self):
        """A multi-line anchor that survived parsing must not inject its own
        line into the printed error: printed raw, it could spoof a line of
        program output on the terminal."""
        with self.assertRaises(RULES.RuleSuggestionError) as ctx:
            SUGGEST.find_section_insert_index(CONFIG_TEXT, "X\nInserted rule into x")
        self.assertNotIn("\n", str(ctx.exception))

    def test_index_precedes_comments_attached_to_next_header(self):
        """Comment lines directly above the next header belong to it and the
        insert offset must precede them."""
        index = SUGGEST.find_section_insert_index(COMMENTED_CONFIG_TEXT, "First Extra")
        self.assertEqual(index, COMMENTED_CONFIG_TEXT.index("# Last rules"))

    def test_blank_line_detaches_comment_from_next_header(self):
        """A comment separated from the next header by a blank line trails the
        anchor section and stays before the insert offset."""
        config_text = CONFIG_TEXT.replace("[Last]", "# trailing comment\n\n[Last]")
        index = SUGGEST.find_section_insert_index(config_text, "First Extra")
        self.assertEqual(index, config_text.index("[Last]"))

    def test_index_precedes_indented_comment_attached_to_next_header(self):
        """An indented comment is still a full-line comment to configparser,
        so it belongs to the next header and the insert offset must precede
        it."""
        config_text = CONFIG_TEXT.replace("[Last]", "  # documents Last\n[Last]")
        index = SUGGEST.find_section_insert_index(config_text, "First Extra")
        self.assertEqual(index, config_text.index("  # documents Last"))


class InsertRuleAfterSectionTest(unittest.TestCase):
    """Round-trip tests for insert_rule_after_section on temp copies."""

    def setUp(self):
        """Create a scratch directory holding a config copy."""
        tmp_dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, tmp_dir)
        self.config_path = os.path.join(tmp_dir, "xml2qifrc")

    def _write_config(self, text):
        """Write the given config text to the scratch config path."""
        write_text(self.config_path, text)

    def _read_config(self):
        """Read the scratch config back as text."""
        return read_text(self.config_path)

    def test_insert_after_middle_section(self):
        """The entry lands between the anchor and the next section."""
        self._write_config(CONFIG_TEXT)
        SUGGEST.insert_rule_after_section(self.config_path, "First", NEW_ENTRY)
        expected = (
            "[First]\n"
            "match = ^FIRST$\n"
            "payee = First Payee\n"
            "category = First:Category\n"
            "\n"
            f"{NEW_ENTRY}\n"
            "\n"
            "[First Extra]\n"
            "match = ^FIRST EXTRA$\n"
            "payee = Extra Payee\n"
            "category = Extra:Category\n"
            "\n"
            "[Last]\n"
            "match = ^LAST$\n"
            "payee = Last Payee\n"
            "category = Last:Category\n"
        )
        self.assertEqual(self._read_config(), expected)

    def test_insert_after_last_section(self):
        """The entry is appended after the final section."""
        self._write_config(CONFIG_TEXT)
        SUGGEST.insert_rule_after_section(self.config_path, "Last", NEW_ENTRY)
        expected = f"{CONFIG_TEXT.rstrip()}\n\n{NEW_ENTRY}\n"
        self.assertEqual(self._read_config(), expected)

    def test_insert_into_config_without_trailing_newline(self):
        """A config not ending in a newline still round-trips correctly."""
        self._write_config(CONFIG_TEXT.rstrip())
        SUGGEST.insert_rule_after_section(self.config_path, "Last", NEW_ENTRY)
        expected = f"{CONFIG_TEXT.rstrip()}\n\n{NEW_ENTRY}\n"
        self.assertEqual(self._read_config(), expected)

    def test_result_reparses_with_expected_section_order(self):
        """The rewritten file parses with the new section at its anchor."""
        self._write_config(CONFIG_TEXT)
        SUGGEST.insert_rule_after_section(self.config_path, "First", NEW_ENTRY)
        config = configparser.ConfigParser(interpolation=None)
        with open(self.config_path, encoding="utf-8") as config_file:
            config.read_file(config_file)
        self.assertEqual(
            config.sections(), ["First", "New Rule", "First Extra", "Last"]
        )

    def test_preserves_file_mode(self):
        """The rewrite keeps the config's permission bits."""
        self._write_config(CONFIG_TEXT)
        os.chmod(self.config_path, 0o644)
        SUGGEST.insert_rule_after_section(self.config_path, "First", NEW_ENTRY)
        self.assertEqual(os.stat(self.config_path).st_mode & 0o7777, 0o644)

    def test_duplicate_section_in_fresh_text_rejected(self):
        """A section gained by the file after validation must be re-detected
        from the fresh text, or the write would make the config unloadable."""
        config_text = f"{CONFIG_TEXT}\n{NEW_ENTRY}\n"
        self._write_config(config_text)
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "already exists"):
            SUGGEST.insert_rule_after_section(self.config_path, "First", NEW_ENTRY)
        self.assertEqual(self._read_config(), config_text)

    def test_indented_duplicate_section_in_fresh_text_rejected(self):
        """An indented duplicate header is a section to configparser even
        though the column-0 insertion regex cannot see it; the write must be
        refused or the next strict reload would fail with
        DuplicateSectionError."""
        config_text = CONFIG_TEXT.replace("[Last]", "[Last]\n  [New Rule]")
        self._write_config(config_text)
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "already exists"):
            SUGGEST.insert_rule_after_section(self.config_path, "First", NEW_ENTRY)
        self.assertEqual(self._read_config(), config_text)

    def test_unparsable_fresh_text_rejected(self):
        """A config that became unparsable after validation is refused with
        a curated error instead of a configparser traceback."""
        config_text = "[First]\nbroken line without delimiter\n"
        self._write_config(config_text)
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "unparsable"):
            SUGGEST.insert_rule_after_section(self.config_path, "First", NEW_ENTRY)
        self.assertEqual(self._read_config(), config_text)

    def test_insert_before_comments_of_next_section(self):
        """The entry lands before comment lines documenting the next section."""
        self._write_config(COMMENTED_CONFIG_TEXT)
        SUGGEST.insert_rule_after_section(self.config_path, "First Extra", NEW_ENTRY)
        expected = COMMENTED_CONFIG_TEXT.replace(
            "# Last rules", f"{NEW_ENTRY}\n\n# Last rules"
        )
        self.assertEqual(self._read_config(), expected)


class StripJsonFenceTest(unittest.TestCase):
    """Tests for strip_json_fence."""

    def test_plain_text_passes_through(self):
        """Unfenced text is returned stripped."""
        self.assertEqual(SUGGEST.strip_json_fence(' {"a": 1} '), '{"a": 1}')

    def test_fence_is_stripped(self):
        """A plain Markdown fence is removed."""
        self.assertEqual(SUGGEST.strip_json_fence('```\n{"a": 1}\n```'), '{"a": 1}')

    def test_fence_with_language_tag_is_stripped(self):
        """A ```json fence is removed."""
        self.assertEqual(SUGGEST.strip_json_fence('```json\n{"a": 1}\n```'), '{"a": 1}')

    def test_unterminated_fence_is_left_alone(self):
        """Text starting with a fence but not ending in one is unchanged."""
        self.assertEqual(SUGGEST.strip_json_fence('```\n{"a": 1}'), '```\n{"a": 1}')

    def test_indented_closing_fence_is_stripped(self):
        """A closing fence with leading whitespace is still recognised."""
        self.assertEqual(
            SUGGEST.strip_json_fence('```json\n{"a": 1}\n   ```'), '{"a": 1}'
        )


class ParseRuleSuggestionTest(unittest.TestCase):
    """Tests for parse_rule_suggestion validation."""

    def test_valid_suggest(self):
        """A well-formed suggest response parses to its fields."""
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json())
        self.assertEqual(
            suggestion,
            {
                "action": "suggest",
                "confidence": 75,
                "insert_after": "First",
                "config_entry": NEW_ENTRY,
            },
        )

    def test_valid_punt(self):
        """A well-formed punt response parses to its fields."""
        text = json.dumps(
            {"action": "punt", "confidence": 30, "reason": "No good anchor"}
        )
        suggestion = SUGGEST.parse_rule_suggestion(text)
        self.assertEqual(
            suggestion,
            {"action": "punt", "confidence": 30, "reason": "No good anchor"},
        )

    def test_fenced_json_accepted(self):
        """A response wrapped in a ```json fence still parses."""
        suggestion = SUGGEST.parse_rule_suggestion(f"```json\n{suggest_json()}\n```")
        self.assertEqual(suggestion["action"], "suggest")

    def test_invalid_action_rejected(self):
        """Unknown action values are rejected."""
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion(suggest_json(action="guess"))

    def test_empty_insert_after_rejected(self):
        """An empty insert_after is rejected."""
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion(suggest_json(insert_after=""))

    def test_absent_insert_after_rejected(self):
        """A missing insert_after key is rejected."""
        suggestion = json.loads(suggest_json())
        del suggestion["insert_after"]
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion(json.dumps(suggestion))

    def test_whitespace_insert_after_rejected(self):
        """A whitespace-only insert_after is rejected."""
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion(suggest_json(insert_after="   "))

    def test_padded_insert_after_stored_stripped(self):
        """Surrounding whitespace on insert_after is stripped, matching
        config_entry's normalization."""
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json(insert_after=" First "))
        self.assertEqual(suggestion["insert_after"], "First")

    def test_whitespace_config_entry_rejected(self):
        """A whitespace-only config_entry is rejected."""
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion(suggest_json(config_entry="   "))

    def test_invalid_json_rejected(self):
        """Non-JSON text is rejected."""
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion("not json at all")


class ParseRuleSuggestionSanitizationTest(unittest.TestCase):
    """Tests for parse_rule_suggestion's handling of control characters and
    line endings in untrusted Codex text."""

    def test_invalid_json_message_has_control_characters_replaced(self):
        """Raw Codex text quoted in the invalid-JSON diagnostic is sanitized."""
        with self.assertRaises(RULES.RuleSuggestionError) as ctx:
            SUGGEST.parse_rule_suggestion("\x1b[2Jnot json at all")
        self.assertNotIn("\x1b", str(ctx.exception))
        self.assertIn("?[2Jnot json at all", str(ctx.exception))

    def test_invalid_json_message_indents_quoted_lines(self):
        """Every quoted line of the invalid-JSON reply is indented: a flush-left
        line in the dump could spoof a line of program output on the
        terminal."""
        with self.assertRaises(RULES.RuleSuggestionError) as ctx:
            SUGGEST.parse_rule_suggestion("not json\nInserted rule into x")
        message = str(ctx.exception)
        self.assertIn("\n  not json\n  Inserted rule into x", message)
        self.assertNotIn("\nInserted rule into x", message)

    def test_long_quoted_line_wrapped_below_terminal_width(self):
        """A long quoted line is soft-wrapped: an over-width line would
        hard-wrap on the terminal and render its continuation flush-left,
        defeating the indentation defense."""
        reply = "x" * 200 + "Inserted rule into x"
        with self.assertRaises(RULES.RuleSuggestionError) as ctx:
            SUGGEST.parse_rule_suggestion(reply)
        widest = max(len(line) for line in str(ctx.exception).split("\n"))
        self.assertLessEqual(widest, 78)

    def test_wide_characters_wrapped_by_display_width(self):
        """Wrapping counts terminal columns, not characters: East Asian wide
        glyphs span two columns, so a character-count cap would still over-run
        the terminal width and wrap flush-left."""
        with self.assertRaises(RULES.RuleSuggestionError) as ctx:
            SUGGEST.parse_rule_suggestion("Ａ" * 100)
        widest = max(
            sum(2 if unicodedata.east_asian_width(c) in "WFA" else 1 for c in line)
            for line in str(ctx.exception).split("\n")
        )
        self.assertLessEqual(widest, 78)

    def test_tabs_wrapped_by_expanded_width(self):
        """A tab run must not escape the wrap: a terminal expands each tab to
        the next tab stop, so a tab line counted as one column per tab would
        hard-wrap flush-left."""
        with self.assertRaises(RULES.RuleSuggestionError) as ctx:
            SUGGEST.parse_rule_suggestion("\t" * 40 + "Inserted rule into x")
        widest = max(len(line.expandtabs(8)) for line in str(ctx.exception).split("\n"))
        self.assertLessEqual(widest, 78)

    def test_nbsp_in_config_entry_accepted(self):
        """A space-rendering separator from a bank-provided name is not a
        control character and must not veto the suggestion."""
        entry = NEW_ENTRY.replace("New Payee", "New\u00a0Payee")
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json(config_entry=entry))
        self.assertEqual(suggestion["config_entry"], entry)

    def test_crlf_in_config_entry_normalized(self):
        """CRLF line breaks are a Codex line-ending misformat, not a control
        threat; they are normalized to LF instead of discarding the reply."""
        entry = NEW_ENTRY.replace("\n", "\r\n")
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json(config_entry=entry))
        self.assertEqual(suggestion["config_entry"], NEW_ENTRY)

    def test_lone_carriage_return_in_config_entry_rejected(self):
        """A bare \\r is not a line-ending misformat but a terminal-spoofing
        primitive (cursor-return overwrite) and stays rejected."""
        entry = NEW_ENTRY.replace("New Payee", "New\rPayee")
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion(suggest_json(config_entry=entry))

    def test_lone_surrogate_in_config_entry_rejected(self):
        """A lone surrogate decoded from a JSON escape must be rejected here,
        or the UTF-8 config write would crash with UnicodeEncodeError."""
        text = suggest_json().replace("New Payee", "New\\ud800Payee")
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion(text)

    def test_line_separator_in_config_entry_rejected(self):
        """U+2028 LINE SEPARATOR would break the config's line structure and
        stays rejected even though space separators are allowed."""
        entry = NEW_ENTRY.replace("New Payee", "New\u2028Payee")
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion(suggest_json(config_entry=entry))

    def test_control_characters_in_config_entry_rejected(self):
        """Escape sequences in config_entry must not reach the terminal."""
        entry = f"\x1b[2J{NEW_ENTRY}"
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion(suggest_json(config_entry=entry))

    def test_crlf_in_insert_after_normalized(self):
        """CRLF in insert_after is normalized to LF like the other fields. The
        resulting multi-line anchor is accepted here and fails only later at
        lookup; that lookup failure is covered by
        test_line_spanning_anchor_rejected_with_curated_error."""
        suggestion = SUGGEST.parse_rule_suggestion(
            suggest_json(insert_after="First\r\nExtra")
        )
        self.assertEqual(suggestion["insert_after"], "First\nExtra")

    def test_control_characters_in_insert_after_rejected(self):
        """Escape sequences in insert_after must not reach the terminal."""
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion(suggest_json(insert_after="Anchor\x1b[1m"))

    def test_control_characters_in_reason_rejected(self):
        """Escape sequences in a punt reason must not reach the terminal."""
        text = json.dumps(
            {"action": "punt", "confidence": 30, "reason": "bad\x1b[1mreason"}
        )
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_rule_suggestion(text)

    def test_multiline_reason_collapsed_to_single_line(self):
        """Line breaks in a punt reason are collapsed: printed raw, a crafted
        reason could spoof a line of program output on the terminal."""
        text = json.dumps(
            {"action": "punt", "confidence": 30, "reason": "line1\nline2"}
        )
        suggestion = SUGGEST.parse_rule_suggestion(text)
        self.assertEqual(suggestion["reason"], "line1 line2")

    def test_crlf_reason_collapsed_to_single_line(self):
        """CRLF line breaks in a punt reason are a line-ending misformat and
        are collapsed like LF ones instead of discarding the reply."""
        text = json.dumps(
            {"action": "punt", "confidence": 30, "reason": "line1\r\nline2"}
        )
        suggestion = SUGGEST.parse_rule_suggestion(text)
        self.assertEqual(suggestion["reason"], "line1 line2")


class ParseRuleSuggestionConfidenceTest(unittest.TestCase):
    """Tests for the lenient normalization of the display-only confidence
    field: a Codex misformat here must not discard the reply."""

    def test_bool_confidence_normalized_to_none(self):
        """JSON true is not a confidence despite bool subclassing int."""
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json(confidence=True))
        self.assertIsNone(suggestion["confidence"])

    def test_digit_string_confidence_coerced(self):
        """A digit-string confidence is accepted as its integer value."""
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json(confidence="75"))
        self.assertEqual(suggestion["confidence"], 75)

    def test_unicode_digit_confidence_normalized_to_none(self):
        """A Unicode-digit string like "²" passes str.isdigit() but int()
        cannot parse it; it must normalize to None, not crash with an
        uncaught ValueError that would discard the whole suggestion."""
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json(confidence="²"))
        self.assertIsNone(suggestion["confidence"])

    def test_out_of_range_confidence_normalized_to_none(self):
        """Confidence above 100 maps to None rather than fabricating a value
        Codex did not give."""
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json(confidence=101))
        self.assertIsNone(suggestion["confidence"])

    def test_absent_confidence_normalized_to_none(self):
        """A missing confidence key must not discard the reply."""
        suggestion = json.loads(suggest_json())
        del suggestion["confidence"]
        parsed = SUGGEST.parse_rule_suggestion(json.dumps(suggestion))
        self.assertIsNone(parsed["confidence"])


class RunCodexRuleSuggestionTest(unittest.TestCase):
    """Tests for run_codex_rule_suggestion."""

    def test_success(self):
        """The stripped final message is read from the --output-last-message
        file on a zero exit."""

        def fake_run(command, **_kwargs):
            """Write the canned response to the requested output file."""
            output_path = command[command.index("--output-last-message") + 1]
            with open(output_path, "w", encoding="utf-8") as output_file:
                output_file.write("response text\n")
            return types.SimpleNamespace(returncode=0, stdout="", stderr="")

        with unittest.mock.patch.object(SUGGEST.subprocess, "run", fake_run):
            self.assertEqual(
                SUGGEST.run_codex_rule_suggestion("prompt"), "response text"
            )

    def test_invalid_utf8_in_output_file_replaced(self):
        """Invalid bytes in the last-message file are decoded with
        replacement characters; the mangled text then fails downstream JSON
        validation with a curated error instead of a UnicodeDecodeError
        traceback."""

        def fake_run(command, **_kwargs):
            """Write invalid UTF-8 to the requested output file."""
            output_path = command[command.index("--output-last-message") + 1]
            with open(output_path, "wb") as output_file:
                output_file.write(b"bad \xff bytes")
            return types.SimpleNamespace(returncode=0, stdout="", stderr="")

        with unittest.mock.patch.object(SUGGEST.subprocess, "run", fake_run):
            self.assertEqual(
                SUGGEST.run_codex_rule_suggestion("prompt"), "bad \ufffd bytes"
            )

    def test_failure_message_has_control_characters_replaced(self):
        """Raw codex stdout/stderr quoted on failure is sanitized."""
        result = types.SimpleNamespace(
            returncode=1, stdout="\x1b[2Jout", stderr="\x1b[31merr"
        )
        with unittest.mock.patch.object(SUGGEST.subprocess, "run", return_value=result):
            with self.assertRaises(RULES.RuleSuggestionError) as ctx:
                SUGGEST.run_codex_rule_suggestion("prompt")
        message = str(ctx.exception)
        self.assertNotIn("\x1b", message)
        self.assertIn("?[2Jout", message)
        self.assertIn("?[31merr", message)

    def test_failure_message_indents_quoted_lines(self):
        """Every quoted line of the codex stdout/stderr dump is indented: a
        flush-left line in the dump could spoof a line of program output on the
        terminal."""
        result = types.SimpleNamespace(
            returncode=1, stdout="out\nInserted rule into x", stderr="err"
        )
        with unittest.mock.patch.object(SUGGEST.subprocess, "run", return_value=result):
            with self.assertRaises(RULES.RuleSuggestionError) as ctx:
                SUGGEST.run_codex_rule_suggestion("prompt")
        message = str(ctx.exception)
        self.assertIn("\n  out\n  Inserted rule into x", message)
        self.assertNotIn("\nInserted rule into x", message)

    def test_runs_codex_in_empty_temporary_directory(self):
        """Codex must not inherit the caller's cwd, so its habitual directory
        exploration finds nothing; the scratch cwd is removed afterwards."""
        seen = {}

        def fake_run(command, **kwargs):
            """Record the cwd and its contents, then fake a codex success."""
            seen["cwd"] = kwargs["cwd"]
            seen["entries"] = os.listdir(kwargs["cwd"])
            output_path = command[command.index("--output-last-message") + 1]
            with open(output_path, "w", encoding="utf-8") as output_file:
                output_file.write("response text\n")
            return types.SimpleNamespace(returncode=0, stdout="", stderr="")

        with unittest.mock.patch.object(SUGGEST.subprocess, "run", fake_run):
            SUGGEST.run_codex_rule_suggestion("prompt")

        self.assertEqual(seen["entries"], [])
        self.assertNotEqual(
            os.path.realpath(seen["cwd"]), os.path.realpath(os.getcwd())
        )
        self.assertFalse(os.path.exists(seen["cwd"]))


class ParseSingleRuleTest(unittest.TestCase):
    """Tests for parse_single_rule validation."""

    def test_valid_entry(self):
        """A valid single-section entry returns its section and rule."""
        section, rule = SUGGEST.parse_single_rule(NEW_ENTRY)
        self.assertEqual(section, "New Rule")
        self.assertEqual(rule["payee"], "New Payee")
        self.assertEqual(rule["category"], "New:Category")
        self.assertEqual(rule["pattern"].pattern, "^NEW$")

    def test_invalid_ini_error_is_single_line(self):
        """The invalid-INI diagnostic quotes the entry with line breaks
        collapsed to "?": printed raw, a multi-line entry could spoof lines of
        program output on the terminal."""
        entry = "key = no section yet\n[New Rule]\nmatch = ^NEW$"
        with self.assertRaises(RULES.RuleSuggestionError) as ctx:
            SUGGEST.parse_single_rule(entry)
        message = str(ctx.exception)
        self.assertNotIn("\n", message)
        self.assertIn("key = no section yet?[New Rule]?match = ^NEW$", message)

    def test_invalid_ini_error_replaces_control_characters(self):
        """Control characters in the quoted entry are sanitized, not just
        newline-collapsed: printed raw, an escape sequence could rewrite the
        terminal next to the diagnostic."""
        entry = "\x1b[2Jkey = no section\n[New Rule]\nmatch = ^NEW$"
        with self.assertRaises(RULES.RuleSuggestionError) as ctx:
            SUGGEST.parse_single_rule(entry)
        message = str(ctx.exception)
        self.assertNotIn("\x1b", message)
        self.assertIn("?[2Jkey = no section", message)

    def test_default_section_rejected(self):
        """A DEFAULT block would leak keys into every existing rule."""
        entry = f"[DEFAULT]\ntype = deposit\n\n{NEW_ENTRY}"
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_single_rule(entry)

    def test_missing_required_key_rejected(self):
        """An entry lacking a payee is rejected."""
        entry = "[New Rule]\nmatch = ^NEW$\ncategory = New:Category"
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_single_rule(entry)

    def test_empty_match_rejected(self):
        """A blank match value would silently match every transaction."""
        entry = "[New Rule]\nmatch =\npayee = New Payee\ncategory = New:Category"
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_single_rule(entry)

    def test_invalid_regex_rejected(self):
        """An unparsable match regex is rejected."""
        entry = "[New Rule]\nmatch = ^(NEW$\npayee = New Payee\ncategory = New:Category"
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_single_rule(entry)

    def test_unknown_key_rejected(self):
        """A misspelled optional key must be rejected: collect_rules would
        silently ignore it and the rule would lose its intended constraint."""
        entry = f"{NEW_ENTRY}\namout = -5.00"
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "amout"):
            SUGGEST.parse_single_rule(entry)

    def test_multiple_sections_rejected(self):
        """An entry with two sections is rejected."""
        entry = f"{NEW_ENTRY}\n\n[Second]\nmatch = ^X$\npayee = P\ncategory = C"
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_single_rule(entry)

    def test_continuation_line_value_rejected(self):
        """An indented continuation line folds a newline into the value, which
        QIF serialization would emit as a stray non-field line."""
        entry = "[New Rule]\nmatch = ^NEW$\npayee = Foo\n Bar\ncategory = C:C"
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "single line"):
            SUGGEST.parse_single_rule(entry)

    def test_section_name_invisible_to_insertion_grammar_rejected(self):
        """A section name configparser parses but the insertion regex cannot
        read back would poison future anchor lookups and duplicate checks."""
        entry = "[A]B]\nmatch = ^NEW$\npayee = P\ncategory = C"
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_single_rule(entry)

    def test_indented_section_header_rejected(self):
        """configparser strips a header line before matching, but the insertion
        regex is anchored at column 0 and would never see it."""
        entry = "# comment\n  [New Rule]\nmatch = ^NEW$\npayee = P\ncategory = C"
        with self.assertRaises(RULES.RuleSuggestionError):
            SUGGEST.parse_single_rule(entry)


class ValidateRuleSuggestionTest(unittest.TestCase):
    """Tests for validate_rule_suggestion."""

    def _validate(self, transaction, rule_type="", **suggest_overrides):
        """Run validate_rule_suggestion on a parsed suggest JSON."""
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json(**suggest_overrides))
        return SUGGEST.validate_rule_suggestion(
            CONFIG_TEXT, transaction, rule_type, suggestion
        )

    def test_matching_suggestion_accepted(self):
        """A valid suggestion matching the transaction returns its parts,
        including the parsed section name so the caller need not re-parse."""
        result = self._validate(make_transaction(debitor="NEW"))
        self.assertEqual(result, ("First", NEW_ENTRY, "New Rule", 75))

    def test_nonexistent_anchor_rejected(self):
        """An insert anchor absent from the config is rejected."""
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "does not exist"):
            self._validate(make_transaction(debitor="NEW"), insert_after="No Such")

    def test_anchor_invisible_to_insertion_grammar_rejected(self):
        """An anchor configparser parses but the insertion regex cannot locate
        must be rejected at validation, before the user confirms."""
        config_text = f"[A]B]\nmatch = ^X$\npayee = P\ncategory = C\n\n{CONFIG_TEXT}"
        config = configparser.ConfigParser(interpolation=None)
        config.read_string(config_text)
        self.assertIn("A]B", config.sections())
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json(insert_after="A]B"))
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "does not exist"):
            SUGGEST.validate_rule_suggestion(
                config_text, make_transaction(debitor="NEW"), "", suggestion
            )

    def test_anchor_invisible_to_configparser_rejected(self):
        """An anchor the insertion regex locates but configparser does not
        parse as a section (the regex reads a prefix of a [A]B] header) must
        be rejected at validation, before the user confirms."""
        config_text = f"[A]B]\nmatch = ^X$\npayee = P\ncategory = C\n\n{CONFIG_TEXT}"
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json(insert_after="A"))
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "not a config section"):
            SUGGEST.validate_rule_suggestion(
                config_text, make_transaction(debitor="NEW"), "", suggestion
            )

    def test_duplicate_section_rejected(self):
        """A suggested section name already in the config is rejected."""
        entry = "[Last]\nmatch = ^NEW$\npayee = New Payee\ncategory = New:Category"
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "already exists"):
            self._validate(make_transaction(debitor="NEW"), config_entry=entry)

    def test_duplicate_section_visible_only_to_configparser_rejected(self):
        """A duplicate only configparser's grammar can see (an indented
        header) is rejected at validation, before the user confirms."""
        config_text = CONFIG_TEXT.replace("[Last]", "[Last]\n  [New Rule]")
        suggestion = SUGGEST.parse_rule_suggestion(suggest_json())
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "already exists"):
            SUGGEST.validate_rule_suggestion(
                config_text, make_transaction(debitor="NEW"), "", suggestion
            )

    def test_type_mismatch_rejected(self):
        """A suggested type differing from the derived rule type is rejected."""
        entry = f"{NEW_ENTRY}\ntype = deposit"
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "Suggested type"):
            self._validate(
                make_transaction(debitor="NEW"),
                rule_type="bank_service",
                config_entry=entry,
            )

    def test_amount_mismatch_rejected(self):
        """A suggested amount differing from the transaction is rejected."""
        entry = f"{NEW_ENTRY}\namount = -5.00"
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "Suggested amount"):
            self._validate(make_transaction(debitor="NEW"), config_entry=entry)

    def test_punt_action_rejected(self):
        """A non-suggest action is rejected even under python -O."""
        suggestion = {"action": "punt", "confidence": 30, "reason": "No good anchor"}
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "Expected suggest"):
            SUGGEST.validate_rule_suggestion(
                CONFIG_TEXT, make_transaction(debitor="NEW"), "", suggestion
            )

    def test_rule_not_matching_transaction_rejected(self):
        """A suggested rule matching no transaction field is rejected."""
        with self.assertRaisesRegex(
            RULES.RuleSuggestionError, "does not match the transaction"
        ):
            self._validate(
                make_transaction(debitor="OTHER", creditor="X", unstructured_info="Y")
            )


class SuggestAndConfirmRuleTest(unittest.TestCase):
    """Integration tests for suggest_and_confirm_rule with Codex mocked out."""

    def setUp(self):
        """Create a scratch config file and build a transaction the suggested
        rule matches."""
        tmp_dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, tmp_dir)
        self.config_path = os.path.join(tmp_dir, "xml2qifrc")
        write_text(self.config_path, CONFIG_TEXT)
        self.transaction = make_transaction(
            debitor="NEW", xml_obj=ET.fromstring("<Ntry />")
        )

    def _suggest_and_confirm(self, suggestion_text, input_mock):
        """Run suggest_and_confirm_rule with Codex and input() mocked."""
        with unittest.mock.patch.object(
            SUGGEST, "run_codex_rule_suggestion", return_value=suggestion_text
        ):
            with unittest.mock.patch("builtins.input", input_mock):
                with contextlib.redirect_stdout(io.StringIO()):
                    SUGGEST.suggest_and_confirm_rule(
                        self.config_path, self.transaction, ""
                    )

    def _read_config(self):
        """Read the scratch config back as text."""
        return read_text(self.config_path)

    def test_user_accepts(self):
        """An accepted suggestion is inserted into the config file."""
        self._suggest_and_confirm(suggest_json(), lambda _: "y")
        config = configparser.ConfigParser(interpolation=None)
        config.read_string(self._read_config())
        self.assertEqual(
            config.sections(), ["First", "New Rule", "First Extra", "Last"]
        )

    def test_user_declines(self):
        """A declined suggestion raises and leaves the config unchanged."""
        with self.assertRaises(RULES.MissingRuleError):
            self._suggest_and_confirm(suggest_json(), lambda _: "n")
        self.assertEqual(self._read_config(), CONFIG_TEXT)

    def test_eof_on_input(self):
        """Closed stdin counts as a decline and leaves the config unchanged."""

        def raise_eof(_prompt):
            """Simulate stdin closing at the confirmation prompt."""
            raise EOFError

        with self.assertRaises(RULES.MissingRuleError):
            self._suggest_and_confirm(suggest_json(), raise_eof)
        self.assertEqual(self._read_config(), CONFIG_TEXT)

    def test_punt_response(self):
        """A punt raises MissingRuleError without prompting the user."""
        text = json.dumps({"action": "punt", "confidence": 30, "reason": "No anchor"})
        input_mock = unittest.mock.Mock()
        with self.assertRaises(RULES.MissingRuleError):
            self._suggest_and_confirm(text, input_mock)
        input_mock.assert_not_called()
        self.assertEqual(self._read_config(), CONFIG_TEXT)

    def test_invalid_anchor_raises(self):
        """A suggestion whose anchor is absent raises RuleSuggestionError
        before prompting."""
        input_mock = unittest.mock.Mock()
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "does not exist"):
            self._suggest_and_confirm(
                suggest_json(insert_after="No Such Section"), input_mock
            )
        input_mock.assert_not_called()
        self.assertEqual(self._read_config(), CONFIG_TEXT)


if __name__ == "__main__":
    unittest.main()
