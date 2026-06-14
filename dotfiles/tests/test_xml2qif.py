"""Unit tests for the xml2qif script and the xml2qif_rules module.

Covers XML transaction parsing, rule loading and matching, the
process_transactions flow, and main()'s curated error handling. The Codex
suggestion helpers are tested in test_xml2qif_suggest.py.
"""

import configparser
import contextlib
import importlib.machinery
import importlib.util
import io
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
import unittest.mock
import xml.etree.ElementTree as ET

from xml2qif_test_utils import (
    BIN_DIR,
    CONFIG_TEXT,
    make_transaction,
    read_text,
    suggest_json,
    write_text,
)


def _load_xml2qif():
    """Load the extensionless xml2qif script as a module."""
    loader = importlib.machinery.SourceFileLoader("xml2qif", str(BIN_DIR / "xml2qif"))
    spec = importlib.util.spec_from_loader("xml2qif", loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


XML2QIF = _load_xml2qif()
RULES = importlib.import_module("xml2qif_rules")
SUGGEST = importlib.import_module("xml2qif_suggest")

_NAMESPACE = "urn:iso:std:iso:20022:tech:xsd:camt.053.001.02"


def _entry_xml(
    tx_details, amount="-10.00", family="OTHR", subfamily="OTHR", namespaced=False
):
    """Build a camt.053 <Ntry> element string around the given <TxDtls>
    content. A standalone entry must carry the namespace itself; one inside a
    <Document> envelope inherits it."""
    xmlns = f' xmlns="{_NAMESPACE}"' if namespaced else ""
    return (
        f"<Ntry{xmlns}>"
        f"<Amt>{amount}</Amt>"
        "<CdtDbtInd>DBIT</CdtDbtInd>"
        "<BookgDt><Dt>2026-06-11</Dt></BookgDt>"
        "<BkTxCd><Domn><Cd>PMNT</Cd>"
        f"<Fmly><Cd>{family}</Cd><SubFmlyCd>{subfamily}</SubFmlyCd></Fmly>"
        "</Domn></BkTxCd>"
        f"<NtryDtls><TxDtls>{tx_details}</TxDtls></NtryDtls></Ntry>"
    )


def _transaction_from_xml(tx_details, **entry_overrides):
    """Build an XMLTransaction whose <TxDtls> holds the given content."""
    return XML2QIF.XMLTransaction(
        ET.fromstring(_entry_xml(tx_details, namespaced=True, **entry_overrides))
    )


def _document_envelope(entries):
    """Wrap pre-built <Ntry> elements in the camt.053
    Document/BkToCstmrStmt/Stmt envelope."""
    return (
        f'<Document xmlns="{_NAMESPACE}"><BkToCstmrStmt><Stmt>'
        f"{entries}</Stmt></BkToCstmrStmt></Document>"
    )


def _document_xml(tx_details, amount, family="RCDT", subfamily="NTAV"):
    """Wrap an entry around the given <TxDtls> content in the camt.053
    envelope. The RCDT/NTAV default derives an empty rule type, letting a
    type-less suggested rule pass validation."""
    return _document_envelope(_entry_xml(tx_details, amount, family, subfamily))


class LoadRuleConfigTest(unittest.TestCase):
    """Tests for load_rule_config error curation."""

    def setUp(self):
        """Create a scratch directory holding the config path."""
        tmp_dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, tmp_dir)
        self.config_path = os.path.join(tmp_dir, "xml2qifrc")

    def _assert_rule_config_error(self, config_text, expected_detail=""):
        """Load the given config text and expect RuleConfigError naming the
        config path, followed by the expected detail, if any."""
        write_text(self.config_path, config_text)
        pattern = re.escape(self.config_path) + ".*" + re.escape(expected_detail)
        with self.assertRaisesRegex(RULES.RuleConfigError, pattern):
            RULES.load_rule_config(self.config_path)

    def test_broken_ini_raises_rule_config_error(self):
        """A config with no section header raises the curated error naming
        the config path exactly once: configparser's own message already
        carries the path, so the curated prefix must not repeat it."""
        write_text(self.config_path, "match = ^X$\n")
        with self.assertRaises(RULES.RuleConfigError) as ctx:
            RULES.load_rule_config(self.config_path)
        self.assertEqual(str(ctx.exception).count(self.config_path), 1)

    def test_invalid_regex_raises_rule_config_error(self):
        """A rule with an unparsable match regex raises the curated error
        naming the section."""
        self._assert_rule_config_error(
            "[Rule]\nmatch = ^(X$\npayee = P\ncategory = C\n", "section [Rule]"
        )

    def test_missing_required_key_raises_rule_config_error(self):
        """A rule lacking a required key raises the curated error naming the
        section and the missing key."""
        self._assert_rule_config_error(
            "[Rule]\nmatch = ^X$\npayee = P\n",
            "section [Rule]: missing required key 'category'",
        )

    def test_default_section_with_keys_raises_rule_config_error(self):
        """A config with a keyed DEFAULT section raises the curated error,
        because configparser would inject the keys into every rule."""
        self._assert_rule_config_error(
            f"[DEFAULT]\namount = -5.00\n\n{CONFIG_TEXT}",
            "DEFAULT section is not supported",
        )

    def test_non_utf8_config_raises_rule_config_error(self):
        """A config file that is not valid UTF-8 raises the curated error
        naming the config path, not a UnicodeDecodeError traceback."""
        with open(self.config_path, "wb") as config_file:
            config_file.write(b"[Rule]\nmatch = ^\xff$\n")
        with self.assertRaisesRegex(RULES.RuleConfigError, re.escape(self.config_path)):
            RULES.load_rule_config(self.config_path)


class XMLTransactionUnstructuredInfoTest(unittest.TestCase):
    """Tests for XMLTransaction unstructured info extraction."""

    def test_text_is_returned(self):
        """A populated <Ustrd> yields its text."""
        transaction = _transaction_from_xml("<RmtInf><Ustrd>INFO</Ustrd></RmtInf>")
        self.assertEqual(transaction.unstructured_info, "INFO")

    def test_absent_node_yields_empty_string(self):
        """A missing <Ustrd> yields the empty string."""
        transaction = _transaction_from_xml("<RmtInf></RmtInf>")
        self.assertEqual(transaction.unstructured_info, "")

    def test_empty_element_yields_empty_string(self):
        """An empty <Ustrd/> must yield "" like an absent node, never None,
        which would crash regex matching in find_transaction_rule."""
        transaction = _transaction_from_xml("<RmtInf><Ustrd/></RmtInf>")
        self.assertEqual(transaction.unstructured_info, "")

    def test_whitespace_element_yields_empty_string(self):
        """A whitespace-only <Ustrd> </Ustrd> is as missing as an empty one:
        a blank string left unstripped is truthy and would be fed to
        start-anchored rule matching, silently failing every rule."""
        transaction = _transaction_from_xml("<RmtInf><Ustrd> </Ustrd></RmtInf>")
        self.assertEqual(transaction.unstructured_info, "")


class XMLTransactionPartyNameTest(unittest.TestCase):
    """Tests for XMLTransaction creditor and debitor name extraction."""

    def test_empty_creditor_name_yields_empty_string(self):
        """An empty <Nm/> must yield "" like an absent node, never None,
        which would render as literal "None" in MissingRuleError messages."""
        transaction = _transaction_from_xml("<RltdPties><Cdtr><Nm/></Cdtr></RltdPties>")
        self.assertEqual(transaction.creditor, "")

    def test_whitespace_creditor_name_yields_empty_string(self):
        """A whitespace-only <Nm> </Nm> is as missing as an empty one: an
        unstripped blank is truthy, so find_transaction_rule would try it
        against rules instead of skipping to the next field."""
        transaction = _transaction_from_xml(
            "<RltdPties><Cdtr><Nm> </Nm></Cdtr></RltdPties>"
        )
        self.assertEqual(transaction.creditor, "")

    def test_empty_debitor_name_yields_empty_string(self):
        """An empty <Nm/> must yield "" like an absent node, never None,
        which would render as literal "None" in MissingRuleError messages."""
        transaction = _transaction_from_xml("<RltdPties><Dbtr><Nm/></Dbtr></RltdPties>")
        self.assertEqual(transaction.debitor, "")

    def test_whitespace_debitor_name_yields_empty_string(self):
        """A whitespace-only <Nm> </Nm> is as missing as an empty one: an
        unstripped blank is truthy, so find_transaction_rule would try it
        against rules instead of skipping to the next field."""
        transaction = _transaction_from_xml(
            "<RltdPties><Dbtr><Nm> </Nm></Dbtr></RltdPties>"
        )
        self.assertEqual(transaction.debitor, "")


class XMLTransactionAmountTest(unittest.TestCase):
    """Tests for XMLTransaction amount extraction."""

    def test_empty_amount_element_raises(self):
        """An empty <Amt/> must fail loud like an absent node, never yield
        None, which would silently corrupt the QIF output as a T-None
        record."""
        with self.assertRaisesRegex(ValueError, "Unable to find amount"):
            _transaction_from_xml("<RmtInf></RmtInf>", amount="")

    def test_whitespace_amount_element_raises(self):
        """A whitespace-only <Amt> </Amt> is as missing as an empty one and
        must fail loud, never yield a blank amount in the QIF output."""
        with self.assertRaisesRegex(ValueError, "Unable to find amount"):
            _transaction_from_xml("<RmtInf></RmtInf>", amount=" ")


class XMLTransactionDateTest(unittest.TestCase):
    """Tests for XMLTransaction date extraction."""

    def test_empty_value_date_element_falls_back_to_booking_date(self):
        """An empty <ValDt><Dt/></ValDt> must behave like an absent node and
        fall back to the booking date, never yield None, which would silently
        corrupt the QIF output as a DNone record."""
        transaction = _transaction_from_xml("<ValDt><Dt/></ValDt>")
        self.assertEqual(transaction.date, "2026-06-11")

    def test_whitespace_value_date_element_falls_back_to_booking_date(self):
        """A whitespace-only <ValDt><Dt> </Dt></ValDt> is as missing as an
        empty one and must fall back to the booking date, never yield a blank
        date in the QIF output."""
        transaction = _transaction_from_xml("<ValDt><Dt> </Dt></ValDt>")
        self.assertEqual(transaction.date, "2026-06-11")

    def test_all_date_sources_empty_raises(self):
        """An empty <BookgDt><Dt/></BookgDt> with no other date source must
        fail loud like an absent node, never yield None, which would silently
        corrupt the QIF output as a DNone record."""
        entry = _entry_xml("<RmtInf></RmtInf>", namespaced=True).replace(
            "<Dt>2026-06-11</Dt>", "<Dt/>"
        )
        with self.assertRaisesRegex(ValueError, "transaction date"):
            XML2QIF.XMLTransaction(ET.fromstring(entry))

    def test_all_date_sources_whitespace_raises(self):
        """A whitespace-only <BookgDt><Dt> </Dt></BookgDt> with no other date
        source is as missing as an empty one and must fail loud, never yield
        a blank date in the QIF output."""
        entry = _entry_xml("<RmtInf></RmtInf>", namespaced=True).replace(
            "<Dt>2026-06-11</Dt>", "<Dt> </Dt>"
        )
        with self.assertRaisesRegex(ValueError, "transaction date"):
            XML2QIF.XMLTransaction(ET.fromstring(entry))


class MissingRuleErrorMessageTest(unittest.TestCase):
    """Tests for MissingRuleError's message composition."""

    def test_control_characters_in_fields_replaced(self):
        """XML-derived fields quoted in the message are sanitized."""
        transaction = make_transaction(
            debitor="EVIL\x1b[2mDEB", creditor="CRED", unstructured_info="INFO"
        )
        message = str(RULES.MissingRuleError(transaction, ""))
        self.assertNotIn("\x1b", message)
        self.assertIn("EVIL?[2mDEB", message)

    def test_newlines_in_fields_replaced(self):
        """A literal newline in an XML field must not break the single-line
        message: it could spoof a line of program output."""
        transaction = make_transaction(
            debitor="EVIL\nDEB", creditor="CRED", unstructured_info="INFO"
        )
        message = str(RULES.MissingRuleError(transaction, ""))
        self.assertNotIn("\n", message)
        self.assertIn("EVIL?DEB", message)


class FindTransactionRuleTest(unittest.TestCase):
    """Tests for find_transaction_rule field-matching precedence."""

    @staticmethod
    def _rule(match, payee):
        """Build a rule dict through production parsing."""
        entry = f"[{payee}]\nmatch = {match}\npayee = {payee}\ncategory = C:C"
        return SUGGEST.parse_single_rule(entry)[1]

    def test_debitor_match_beats_earlier_creditor_match(self):
        """Field order outranks rule order: all rules are tried against the
        debitor before any rule is tried against the creditor."""
        rules = [self._rule("^CRED$", "By Creditor"), self._rule("^DEB$", "By Debitor")]
        transaction = make_transaction(debitor="DEB", creditor="CRED")
        found = RULES.find_transaction_rule(rules, transaction, "")
        self.assertEqual(found["payee"], "By Debitor")

    def test_creditor_matched_when_debitor_is_empty(self):
        """An empty debitor is skipped and the creditor is matched."""
        rules = [self._rule("^CRED$", "By Creditor")]
        transaction = make_transaction(creditor="CRED")
        found = RULES.find_transaction_rule(rules, transaction, "")
        self.assertEqual(found["payee"], "By Creditor")

    def test_creditor_matched_when_debitor_is_unmatched(self):
        """A non-empty debitor matching no rule falls back to the creditor."""
        rules = [self._rule("^CRED$", "By Creditor")]
        transaction = make_transaction(debitor="DEB", creditor="CRED")
        found = RULES.find_transaction_rule(rules, transaction, "")
        self.assertEqual(found["payee"], "By Creditor")

    def test_unstructured_info_matched_when_no_party_matches(self):
        """Unstructured info is the last field tried."""
        rules = [self._rule("^INFO$", "By Info")]
        transaction = make_transaction(
            debitor="DEB", creditor="CRED", unstructured_info="INFO"
        )
        found = RULES.find_transaction_rule(rules, transaction, "")
        self.assertEqual(found["payee"], "By Info")

    def test_no_field_matching_returns_none(self):
        """A transaction matching no rule on any field yields None."""
        rules = [self._rule("^NOPE$", "Unreachable")]
        transaction = make_transaction(
            debitor="DEB", creditor="CRED", unstructured_info="INFO"
        )
        self.assertIsNone(RULES.find_transaction_rule(rules, transaction, ""))


class ProcessTransactionsTest(unittest.TestCase):
    """End-to-end test for process_transactions' suggestion retry path."""

    _DOCUMENT_XML = _document_xml(
        "<RltdPties><Dbtr><Nm>NEW</Nm></Dbtr></RltdPties>", "10.00"
    )

    def setUp(self):
        """Create a scratch config file whose rules do not match the
        transaction, forcing the suggestion path."""
        tmp_dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, tmp_dir)
        self.config_path = os.path.join(tmp_dir, "xml2qifrc")
        write_text(self.config_path, CONFIG_TEXT)

    def test_accepted_suggestion_reloads_rules_and_writes_qif(self):
        """An unmatched transaction triggers the suggestion flow, and after
        the accepted rule is inserted and reloaded, the transaction is
        converted with it."""
        rules = RULES.load_rule_config(self.config_path)
        root = ET.fromstring(self._DOCUMENT_XML)
        output = io.StringIO()
        with unittest.mock.patch.object(
            SUGGEST, "run_codex_rule_suggestion", return_value=suggest_json()
        ):
            with unittest.mock.patch("builtins.input", return_value="y"):
                with contextlib.redirect_stdout(io.StringIO()):
                    XML2QIF.process_transactions(root, rules, self.config_path, output)
        self.assertEqual(
            output.getvalue(),
            "D2026-06-11\nT-10.00\nPNew Payee\nLNew:Category\n^\n",
        )
        reloaded_config = configparser.ConfigParser(interpolation=None)
        with open(self.config_path, encoding="utf-8") as config_file:
            reloaded_config.read_file(config_file)
        self.assertIn("New Rule", reloaded_config.sections())

    def test_reloaded_rules_apply_to_subsequent_transactions(self):
        """After an accepted suggestion, a later transaction matching the
        inserted rule is converted directly from the reloaded in-memory
        rules. The single-element mock would raise StopIteration if a stale
        rule set sent the second transaction into the suggestion flow."""
        rules = RULES.load_rule_config(self.config_path)
        root = ET.fromstring(
            _document_envelope(
                _entry_xml(
                    "<RltdPties><Dbtr><Nm>NEW</Nm></Dbtr></RltdPties>",
                    "10.00",
                    "RCDT",
                    "NTAV",
                )
                + _entry_xml(
                    "<RltdPties><Dbtr><Nm>NEW</Nm></Dbtr></RltdPties>",
                    "20.00",
                    "RCDT",
                    "NTAV",
                )
            )
        )
        output = io.StringIO()
        with unittest.mock.patch.object(
            SUGGEST, "run_codex_rule_suggestion", side_effect=[suggest_json()]
        ):
            with unittest.mock.patch("builtins.input", return_value="y"):
                with contextlib.redirect_stdout(io.StringIO()):
                    XML2QIF.process_transactions(root, rules, self.config_path, output)
        self.assertEqual(
            output.getvalue(),
            "D2026-06-11\nT-10.00\nPNew Payee\nLNew:Category\n^\n"
            "D2026-06-11\nT-20.00\nPNew Payee\nLNew:Category\n^\n",
        )

    def test_unmatched_retry_after_insertion_raises_distinct_error(self):
        """If the transaction is still unmatched after the suggestion flow
        inserts a rule and the config is reloaded (e.g. an amount filter
        rejects it), the retry failure is re-raised with a distinct prefix —
        not the bare MissingRuleError, which reads like the initial prompt and
        would invite re-entering the suggestion flow into an endless loop."""
        rules = RULES.load_rule_config(self.config_path)
        root = ET.fromstring(self._DOCUMENT_XML)

        def insert_amount_mismatched_rule(config_path, _xml_transaction, _rule_type):
            write_text(
                config_path,
                read_text(config_path)
                + "\n[New Rule]\nmatch = ^NEW$\npayee = P\ncategory = C\n"
                + "amount = 999.00\n",
            )

        with unittest.mock.patch.object(
            XML2QIF,
            "suggest_and_confirm_rule",
            side_effect=insert_amount_mismatched_rule,
        ):
            with contextlib.redirect_stdout(io.StringIO()):
                with self.assertRaises(ValueError) as ctx:
                    XML2QIF.process_transactions(
                        root, rules, self.config_path, io.StringIO()
                    )
        self.assertNotIsInstance(ctx.exception, RULES.MissingRuleError)
        self.assertIn("Added rule still did not match", str(ctx.exception))

    def test_declined_suggestion_propagates_missing_rule_error(self):
        """When the user declines (or EOFs/punts), suggest_and_confirm_rule
        raises MissingRuleError. The inner except catches only suggestion-flow
        failures, so that decline error must propagate by identity, neither
        displaced by the original miss nor swallowed by a widened except."""
        rules = RULES.load_rule_config(self.config_path)
        root = ET.fromstring(self._DOCUMENT_XML)
        decline_error = RULES.MissingRuleError(make_transaction(debitor="NEW"), "")

        def decline(_config_path, _xml_transaction, _rule_type):
            raise decline_error

        with unittest.mock.patch.object(
            XML2QIF, "suggest_and_confirm_rule", side_effect=decline
        ):
            with contextlib.redirect_stdout(io.StringIO()):
                with self.assertRaises(RULES.MissingRuleError) as ctx:
                    XML2QIF.process_transactions(
                        root, rules, self.config_path, io.StringIO()
                    )
        self.assertIs(ctx.exception, decline_error)

    def test_suggestion_failure_preserves_missing_rule_message(self):
        """When the Codex flow itself fails, the actionable missing-rule
        message must still control the exit; the suggestion failure is only
        a diagnostic and is printed instead."""
        rules = RULES.load_rule_config(self.config_path)
        root = ET.fromstring(self._DOCUMENT_XML)
        captured = io.StringIO()
        with unittest.mock.patch.object(
            SUGGEST,
            "run_codex_rule_suggestion",
            side_effect=RULES.RuleSuggestionError("codex executable not found"),
        ):
            with contextlib.redirect_stdout(captured):
                with self.assertRaisesRegex(
                    RULES.MissingRuleError, "Please add a rule for"
                ):
                    XML2QIF.process_transactions(
                        root, rules, self.config_path, io.StringIO()
                    )
        self.assertIn("codex executable not found", captured.getvalue())

    def test_config_error_in_suggestion_flow_preserves_missing_rule_message(self):
        """A config read failure inside the suggestion flow is a diagnostic
        like any other suggestion failure and must not displace the
        actionable missing-rule message."""
        rules = RULES.load_rule_config(self.config_path)
        root = ET.fromstring(self._DOCUMENT_XML)
        captured = io.StringIO()
        with unittest.mock.patch.object(
            SUGGEST,
            "read_config_text",
            side_effect=RULES.RuleConfigError("config became non-UTF-8"),
        ):
            with contextlib.redirect_stdout(captured):
                with self.assertRaisesRegex(
                    RULES.MissingRuleError, "Please add a rule for"
                ):
                    XML2QIF.process_transactions(
                        root, rules, self.config_path, io.StringIO()
                    )
        self.assertIn("config became non-UTF-8", captured.getvalue())

    def test_os_error_in_suggestion_flow_preserves_missing_rule_message(self):
        """An I/O failure inside the suggestion flow (config unreadable, temp
        file write failure) is likewise only a diagnostic and must not
        displace the actionable missing-rule message."""
        rules = RULES.load_rule_config(self.config_path)
        root = ET.fromstring(self._DOCUMENT_XML)
        captured = io.StringIO()
        with unittest.mock.patch.object(
            SUGGEST,
            "read_config_text",
            side_effect=OSError("config disappeared"),
        ):
            with contextlib.redirect_stdout(captured):
                with self.assertRaisesRegex(
                    RULES.MissingRuleError, "Please add a rule for"
                ):
                    XML2QIF.process_transactions(
                        root, rules, self.config_path, io.StringIO()
                    )
        self.assertIn("config disappeared", captured.getvalue())


class ProcessTransactionsOutputSanitizationTest(unittest.TestCase):
    """Tests for control-character sanitization of the progress output.

    Uses U+009B (single-byte CSI): unlike ESC, it is a valid XML 1.0
    character, so it survives parsing and would reach the terminal."""

    _CONFIG_TEXT = "[Evil]\nmatch = ^EVIL\npayee = Evil Payee\ncategory = E:C\n"

    def _captured_stdout(self, amount, debitor):
        """Convert a single-transaction document and return what was printed."""
        tmp_dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, tmp_dir)
        config_path = os.path.join(tmp_dir, "xml2qifrc")
        write_text(config_path, self._CONFIG_TEXT)
        rules = RULES.load_rule_config(config_path)
        root = ET.fromstring(
            _document_xml(
                f"<RltdPties><Dbtr><Nm>{debitor}</Nm></Dbtr></RltdPties>", amount
            )
        )
        captured = io.StringIO()
        with contextlib.redirect_stdout(captured):
            XML2QIF.process_transactions(root, rules, config_path, io.StringIO())
        return captured.getvalue()

    def test_before_line_replaces_control_characters_in_debitor(self):
        """A control character in the debitor name is sanitized in the
        Before: line."""
        captured = self._captured_stdout("10.00", "EVIL\x9bDEB")
        self.assertNotIn("\x9b", captured)
        self.assertIn("EVIL?DEB", captured)

    def test_before_line_replaces_newline_in_debitor(self):
        """A literal newline in the debitor name must not break the Before:
        line: it could spoof a line of program output."""
        captured = self._captured_stdout("10.00", "EVIL\nDEB")
        self.assertIn("EVIL?DEB", captured)

    def test_after_line_replaces_control_characters_in_amount(self):
        """A control character in the amount is sanitized in the After: line,
        which quotes it via the QIF transaction."""
        captured = self._captured_stdout("10.00\x9b", "EVILDEB")
        self.assertNotIn("\x9b", captured)
        self.assertIn("amount: 10.00?), rule_type:", captured)

    def test_after_line_replaces_newline_in_amount(self):
        """A literal newline in the amount must not break the After: line:
        it could spoof a line of program output. The newline is interior —
        edge whitespace is already stripped at extraction."""
        captured = self._captured_stdout("10\n.00", "EVILDEB")
        self.assertIn("amount: 10?.00), rule_type:", captured)


class MainErrorCurationTest(unittest.TestCase):
    """End-to-end tests for main()'s curated error messages, running the
    script as a subprocess with HOME pointing at a scratch directory."""

    def setUp(self):
        """Create a scratch HOME holding a valid ~/.xml2qifrc."""
        self.home_dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.home_dir)
        write_text(os.path.join(self.home_dir, ".xml2qifrc"), CONFIG_TEXT)

    def _run_xml2qif(self, input_path, output_path):
        """Run the script on the given paths and return the completed
        process."""
        # -B: keep the child from writing __pycache__ into the stowed bin dir
        return subprocess.run(
            [sys.executable, "-B", str(BIN_DIR / "xml2qif"), input_path, output_path],
            env={**os.environ, "HOME": self.home_dir},
            capture_output=True,
            text=True,
            check=False,
        )

    def _assert_curated_error(self, result, expected_message_part):
        """Expect a clean exit-1 error naming the failure, not a traceback."""
        self.assertEqual(result.returncode, 1)
        self.assertIn(expected_message_part, result.stderr)
        self.assertNotIn("Traceback", result.stderr)

    def test_missing_config_file_exits_cleanly(self):
        """A missing ~/.xml2qifrc produces an error message naming the
        resolved config path, not a traceback."""
        config_path = os.path.join(self.home_dir, ".xml2qifrc")
        os.remove(config_path)
        input_path = os.path.join(self.home_dir, "input.xml")
        write_text(input_path, "<Document></Document>")
        result = self._run_xml2qif(input_path, os.path.join(self.home_dir, "out.qif"))
        self._assert_curated_error(result, f"Config file not found: {config_path}")

    def test_missing_input_file_exits_cleanly(self):
        """A nonexistent input file produces an error message, not a
        traceback."""
        result = self._run_xml2qif(
            os.path.join(self.home_dir, "missing.xml"),
            os.path.join(self.home_dir, "out.qif"),
        )
        self._assert_curated_error(result, "missing.xml")

    def test_malformed_input_xml_exits_cleanly(self):
        """An unparsable input file produces an error message naming the
        file, not a traceback."""
        input_path = os.path.join(self.home_dir, "broken.xml")
        write_text(input_path, "<Document>")
        result = self._run_xml2qif(input_path, os.path.join(self.home_dir, "out.qif"))
        self._assert_curated_error(result, f"{input_path}: no element found")

    def test_unknown_camt_code_exits_cleanly(self):
        """A well-formed statement with an unrecognized CAMT family code
        produces an error naming the enum and the code, not a KeyError
        traceback. The enum maps are hand-maintained subsets of the CAMT
        taxonomy, so unknown codes occur with real statements."""
        input_path = os.path.join(self.home_dir, "input.xml")
        write_text(
            input_path, _document_xml("<RmtInf></RmtInf>", "10.00", family="XBCT")
        )
        result = self._run_xml2qif(input_path, os.path.join(self.home_dir, "out.qif"))
        self._assert_curated_error(
            result, "Unrecognized PaymentsTransactionFamily code 'XBCT'"
        )

    def test_unwritable_output_path_exits_cleanly(self):
        """An output path in a nonexistent directory produces an error
        message, not a traceback."""
        input_path = os.path.join(self.home_dir, "input.xml")
        write_text(input_path, "<Document></Document>")
        result = self._run_xml2qif(
            input_path, os.path.join(self.home_dir, "no-such-dir", "out.qif")
        )
        self._assert_curated_error(result, "out.qif")


if __name__ == "__main__":
    unittest.main()
