"""Unit tests for the xml2qif_suggest config-file locating and rewrite layer.

Covers find_section_insert_index / find_section_span (locating a section in the
config text under both the column-0 regex and configparser grammars) and
insert_rule_after_section / replace_rule_section (the atomic rewrites of
~/.xml2qifrc, whose edge cases would otherwise only surface after the original
file is gone). The suggestion parsing, validation, and integration tests live in
test_xml2qif_suggest.
"""

import configparser
import importlib
import os
import shutil
import tempfile
import unittest

from xml2qif_test_utils import (
    COMMENTED_CONFIG_TEXT,
    CONFIG_TEXT,
    EDIT_ENTRY,
    NEW_ENTRY,
    read_text,
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


class FindSectionSpanTest(unittest.TestCase):
    """Tests for find_section_span."""

    def test_middle_section_span(self):
        """A middle section spans from its header to the next header start."""
        start, end = SUGGEST.find_section_span(CONFIG_TEXT, "First Extra")
        self.assertEqual(start, CONFIG_TEXT.index("[First Extra]"))
        self.assertEqual(end, CONFIG_TEXT.index("[Last]"))

    def test_first_section_span_starts_at_zero(self):
        """The first section's span starts at the top of the file."""
        start, end = SUGGEST.find_section_span(CONFIG_TEXT, "First")
        self.assertEqual(start, 0)
        self.assertEqual(end, CONFIG_TEXT.index("[First Extra]"))

    def test_last_section_span_ends_at_text_length(self):
        """The final section's span runs to the end of the text."""
        start, end = SUGGEST.find_section_span(CONFIG_TEXT, "Last")
        self.assertEqual(start, CONFIG_TEXT.index("[Last]"))
        self.assertEqual(end, len(CONFIG_TEXT))

    def test_comments_above_header_excluded_from_span(self):
        """Comments documenting the edited section stay outside the span, so an
        edit keeps them."""
        start, _ = SUGGEST.find_section_span(COMMENTED_CONFIG_TEXT, "Last")
        self.assertEqual(start, COMMENTED_CONFIG_TEXT.index("[Last]"))

    def test_comments_on_next_section_excluded_from_span(self):
        """Comments attached to the following section belong to it and the span
        must end before them."""
        _, end = SUGGEST.find_section_span(COMMENTED_CONFIG_TEXT, "First Extra")
        self.assertEqual(end, COMMENTED_CONFIG_TEXT.index("# Last rules"))

    def test_indented_comment_on_next_section_excluded_from_span(self):
        """An indented comment is still a full-line comment to configparser, so
        it belongs to the next section and the span must end before it."""
        config_text = CONFIG_TEXT.replace("[Last]", "  # documents Last\n[Last]")
        _, end = SUGGEST.find_section_span(config_text, "First Extra")
        self.assertEqual(end, config_text.index("  # documents Last"))

    def test_blank_line_pulls_trailing_comment_into_span(self):
        """A comment separated from the next header by a blank line trails the
        edited section and falls inside the replaced span."""
        config_text = CONFIG_TEXT.replace("[Last]", "# trailing comment\n\n[Last]")
        _, end = SUGGEST.find_section_span(config_text, "First Extra")
        self.assertEqual(end, config_text.index("[Last]"))

    def test_missing_section_raises(self):
        """A section absent from the text is an error."""
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "does not exist"):
            SUGGEST.find_section_span(CONFIG_TEXT, "No Such Section")

    def test_prefix_collision_header_skipped_for_real_section(self):
        """A [A]B] header the span regex prefix-reads as [A] must not capture
        the section when a real [A] exists elsewhere."""
        config_text = (
            "[A]B]\nmatch = ^X$\npayee = P\ncategory = C\n\n"
            "[A]\nmatch = ^Y$\npayee = Q\ncategory = D\n\n"
            "[Last]\nmatch = ^Z$\npayee = R\ncategory = E\n"
        )
        start, end = SUGGEST.find_section_span(config_text, "A")
        self.assertEqual(start, config_text.index("[A]\nmatch = ^Y$"))
        self.assertEqual(end, config_text.index("[Last]"))

    def test_configparser_only_name_does_not_exist(self):
        """A name only configparser can see ([A]B] read as A]B) is invisible to
        the column-0 span regex, so it is reported as not existing — the same
        limitation the insert anchor has."""
        config_text = "[A]B]\nmatch = ^X$\npayee = P\ncategory = C\n"
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "does not exist"):
            SUGGEST.find_section_span(config_text, "A]B")

    def test_prefix_only_name_not_a_section(self):
        """An [A] the span regex reads as a prefix of [A]B], with no real [A]
        section, is reported as not a config section."""
        config_text = "[A]B]\nmatch = ^X$\npayee = P\ncategory = C\n"
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "not a config section"):
            SUGGEST.find_section_span(config_text, "A")

    def test_multiline_name_collapsed_in_error_message(self):
        """A crafted multi-line section name must not inject its own line into
        the printed error."""
        with self.assertRaises(RULES.RuleSuggestionError) as ctx:
            SUGGEST.find_section_span(CONFIG_TEXT, "X\nEdited rule in x")
        self.assertNotIn("\n", str(ctx.exception))


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


class ReplaceRuleSectionTest(unittest.TestCase):
    """Round-trip tests for replace_rule_section on temp copies."""

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

    def test_replace_middle_section(self):
        """Editing a middle section replaces only its body, keeping order."""
        self._write_config(CONFIG_TEXT)
        entry = (
            "[First Extra]\nmatch = ^(FIRST EXTRA|MORE)$\n"
            "payee = Extra Payee\ncategory = Extra:Category"
        )
        SUGGEST.replace_rule_section(self.config_path, "First Extra", entry)
        expected = CONFIG_TEXT.replace(
            "match = ^FIRST EXTRA$", "match = ^(FIRST EXTRA|MORE)$"
        )
        self.assertEqual(self._read_config(), expected)

    def test_replace_first_section(self):
        """Editing the first section round-trips with correct spacing."""
        self._write_config(CONFIG_TEXT)
        SUGGEST.replace_rule_section(self.config_path, "First", EDIT_ENTRY)
        expected = CONFIG_TEXT.replace("match = ^FIRST$", "match = ^(FIRST|NEW)$")
        self.assertEqual(self._read_config(), expected)

    def test_replace_last_section(self):
        """Editing the final section round-trips with correct spacing."""
        self._write_config(CONFIG_TEXT)
        entry = (
            "[Last]\nmatch = ^(LAST|NEW)$\npayee = Last Payee\ncategory = Last:Category"
        )
        SUGGEST.replace_rule_section(self.config_path, "Last", entry)
        expected = CONFIG_TEXT.replace("match = ^LAST$", "match = ^(LAST|NEW)$")
        self.assertEqual(self._read_config(), expected)

    def test_replace_into_config_without_trailing_newline(self):
        """A config not ending in a newline still round-trips."""
        self._write_config(CONFIG_TEXT.rstrip())
        entry = (
            "[Last]\nmatch = ^(LAST|NEW)$\npayee = Last Payee\ncategory = Last:Category"
        )
        SUGGEST.replace_rule_section(self.config_path, "Last", entry)
        expected = (
            CONFIG_TEXT.rstrip().replace("match = ^LAST$", "match = ^(LAST|NEW)$")
            + "\n"
        )
        self.assertEqual(self._read_config(), expected)

    def test_replace_only_section(self):
        """A single-section config produces just the replacement entry."""
        single = (
            "[First]\nmatch = ^FIRST$\npayee = First Payee\ncategory = First:Category\n"
        )
        self._write_config(single)
        SUGGEST.replace_rule_section(self.config_path, "First", EDIT_ENTRY)
        self.assertEqual(self._read_config(), EDIT_ENTRY.strip() + "\n")

    def test_rename_changes_header_and_reparses(self):
        """An edit may rename the section; the file re-parses with the new name
        in the old position."""
        self._write_config(CONFIG_TEXT)
        entry = "[Renamed]\nmatch = ^(FIRST|NEW)$\npayee = First Payee\ncategory = First:Category"
        SUGGEST.replace_rule_section(self.config_path, "First", entry)
        config = configparser.ConfigParser(interpolation=None)
        config.read_string(self._read_config())
        self.assertEqual(config.sections(), ["Renamed", "First Extra", "Last"])

    def test_result_reparses_with_edited_body(self):
        """The rewritten file parses with the broadened match."""
        self._write_config(CONFIG_TEXT)
        SUGGEST.replace_rule_section(self.config_path, "First", EDIT_ENTRY)
        config = configparser.ConfigParser(interpolation=None)
        config.read_string(self._read_config())
        self.assertEqual(config["First"]["match"], "^(FIRST|NEW)$")

    def test_preserves_file_mode(self):
        """The rewrite keeps the config's permission bits."""
        self._write_config(CONFIG_TEXT)
        os.chmod(self.config_path, 0o644)
        SUGGEST.replace_rule_section(self.config_path, "First", EDIT_ENTRY)
        self.assertEqual(os.stat(self.config_path).st_mode & 0o7777, 0o644)

    def test_section_deleted_on_disk_rejected(self):
        """A section that vanished after validation aborts with the file
        untouched."""
        config_text = CONFIG_TEXT.replace(
            "[First]\nmatch = ^FIRST$\npayee = First Payee\ncategory = First:Category\n\n",
            "",
        )
        self._write_config(config_text)
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "does not exist"):
            SUGGEST.replace_rule_section(self.config_path, "First", EDIT_ENTRY)
        self.assertEqual(self._read_config(), config_text)

    def test_rename_colliding_with_fresh_duplicate_rejected(self):
        """A rename whose new name now collides with an existing section is
        refused before the write by the pre-write rename-collision check."""
        self._write_config(CONFIG_TEXT)
        entry = "[Last]\nmatch = ^(FIRST|NEW)$\npayee = First Payee\ncategory = First:Category"
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "already exists"):
            SUGGEST.replace_rule_section(self.config_path, "First", entry)
        self.assertEqual(self._read_config(), CONFIG_TEXT)

    def test_unparsable_other_section_rejected(self):
        """Corruption in a section the edit does not touch survives into the
        rewritten text, so the post-reload guard refuses the write."""
        config_text = CONFIG_TEXT.replace(
            "match = ^LAST$", "broken line without delimiter"
        )
        self._write_config(config_text)
        with self.assertRaisesRegex(RULES.RuleSuggestionError, "unparsable"):
            SUGGEST.replace_rule_section(self.config_path, "First", EDIT_ENTRY)
        self.assertEqual(self._read_config(), config_text)

    def test_comments_above_and_on_next_section_survive(self):
        """An edit keeps comments documenting the edited section and the next."""
        config_text = COMMENTED_CONFIG_TEXT.replace(
            "[First Extra]", "# documents Extra\n[First Extra]"
        )
        self._write_config(config_text)
        entry = (
            "[First Extra]\nmatch = ^(FIRST EXTRA|MORE)$\n"
            "payee = Extra Payee\ncategory = Extra:Category"
        )
        SUGGEST.replace_rule_section(self.config_path, "First Extra", entry)
        result = self._read_config()
        self.assertIn("# documents Extra", result)
        self.assertIn("# Last rules", result)


if __name__ == "__main__":
    unittest.main()
