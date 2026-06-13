"""Rule configuration for xml2qif: loading, matching, and shared errors."""

import configparser
import re
import unicodedata


def is_control_character(character):
    """Tell whether a character could alter what the terminal shows.

    Space-rendering separators (category Zs, e.g. NBSP) display like ordinary
    spaces and occur in bank-provided names, so they are not control
    characters."""
    return (
        character not in "\n\t"
        and not character.isprintable()
        and unicodedata.category(character) != "Zs"
    )


def replace_control_characters(value, single_line=False):
    """Sanitize untrusted text quoted in diagnostics, keeping it useful for
    debugging.

    In single-line contexts, line breaks and tabs are replaced too: a literal
    newline could spoof a line of program output on the terminal.

    single_line collapses logical lines but does not bound display width, so an
    over-width value still hard-wraps on the terminal with its continuation at
    column 0 — a residual spoof vector. This is accepted at the Before:/After:
    progress sinks, where the values are genuine transaction text the user
    reviews and a width cap (unlike quote_untrusted_lines') would elide content
    they need; the threat actor there is also only the statement counterparty."""
    return "".join(
        "?" if is_control_character(c) or (single_line and c in "\n\t") else c
        for c in value
    )


# Content columns per quoted line; with the 2-space indent the physical line
# stays within 78, below common terminal widths
MAX_QUOTED_LINE_WIDTH = 76


def display_width(character):
    """Estimate a character's terminal column span. East Asian wide, fullwidth,
    and ambiguous characters occupy two columns; everything else one."""
    return 2 if unicodedata.east_asian_width(character) in "WFA" else 1


def wrap_to_display_width(line, max_width):
    """Split a logical line into segments each spanning at most max_width
    terminal columns. An empty line yields one empty segment."""
    segments = []
    current = ""
    current_width = 0
    for character in line:
        width = display_width(character)
        if current and current_width + width > max_width:
            segments.append(current)
            current = ""
            current_width = 0
        current += character
        current_width += width
    segments.append(current)
    return segments


def quote_untrusted_lines(value):
    """Sanitize and indent untrusted multi-line text quoted in diagnostics, so
    none of its lines renders flush-left as program output.

    Each logical line is also soft-wrapped by terminal column width: an
    over-width line would hard-wrap on the terminal and render its continuation
    at column 0 without the indent, re-opening the spoof. Terminals narrower
    than the cap remain a residual exposure no finite cap can close."""
    # Expand tabs to spaces: a surviving tab is counted as one column here but
    # renders as up to eight on the terminal, which would let a tab-packed line
    # slip under the cap and still wrap flush-left
    sanitized = replace_control_characters(value).expandtabs(8)
    segments = []
    for line in sanitized.splitlines():
        segments.extend(wrap_to_display_width(line, MAX_QUOTED_LINE_WIDTH))
    return "\n".join(f"  {segment}" for segment in segments)


def collect_rules(config):
    """Collect transaction rewrite rules from the config file"""
    rules = []
    for section in config.sections():
        try:
            # Input
            type_key = config.get(section, "type", fallback="")
            amount = config.get(section, "amount", fallback="")
            pattern = re.compile(config[section]["match"])
            # Output
            payee = config[section]["payee"]
            category = config[section]["category"]
        except re.error as exc:
            raise RuleConfigError(f"section [{section}]: {exc}") from exc
        except KeyError as exc:
            raise RuleConfigError(
                f"section [{section}]: missing required key {exc}"
            ) from exc

        rules.append(
            {
                "type": type_key,
                "amount": amount,
                "pattern": pattern,
                "payee": payee,
                "category": category,
            }
        )
    return rules


def find_matching_rule(rules, amount, type_key, text):
    """Find the rule whose amount and/or type, if present, match the
    transaction and the regex matches the text."""
    for rule in rules:
        if rule["type"] and type_key != rule["type"]:
            continue
        if rule["amount"] and amount != rule["amount"]:
            continue
        if rule["pattern"].match(text):
            return rule

    return None


def find_transaction_rule(rules, xml_transaction, rule_type):
    """Find the first rule matching a transaction using the script's field order."""
    for text in (xml_transaction.debitor, xml_transaction.creditor):
        if text:
            rewrite_rule = find_matching_rule(
                rules, xml_transaction.amount, rule_type, text
            )
            if rewrite_rule is not None:
                return rewrite_rule
    return find_matching_rule(
        rules, xml_transaction.amount, rule_type, xml_transaction.unstructured_info
    )


def read_config_text(config_path):
    """Read the rule configuration as text."""
    try:
        with open(config_path, encoding="utf-8") as config_file:
            return config_file.read()
    except UnicodeDecodeError as exc:
        raise RuleConfigError(f"Invalid rule config {config_path}: {exc}") from exc


def load_rule_config(config_path):
    """Load the rule configuration file."""
    # No interpolation: values are regexes and literal strings where a bare %
    # is legitimate
    config = configparser.ConfigParser(interpolation=None)
    # Outside the try: read_config_text raises an already-curated
    # RuleConfigError that must not be double-prefixed
    config_text = read_config_text(config_path)
    try:
        config.read_string(config_text, source=config_path)
    except configparser.Error as exc:
        # configparser errors already name the source path; prefixing would
        # repeat it
        raise RuleConfigError(str(exc)) from exc
    try:
        # DEFAULT keys would be injected into every rule; a keyless [DEFAULT]
        # header has nothing to inject and passes
        if config.defaults():
            raise RuleConfigError("DEFAULT section is not supported")
        return collect_rules(config)
    except RuleConfigError as exc:
        raise RuleConfigError(f"Invalid rule config {config_path}: {exc}") from exc


class MissingRuleError(ValueError):
    """Raised when no configured rule matches a transaction."""

    def __init__(self, xml_transaction, rule_type):
        debitor, creditor, info = (
            replace_control_characters(value, single_line=True)
            for value in (
                xml_transaction.debitor,
                xml_transaction.creditor,
                xml_transaction.unstructured_info,
            )
        )
        super().__init__(f"Please add a rule for {debitor}, {creditor} or {info}")
        self.xml_transaction = xml_transaction
        self.rule_type = rule_type


class RuleConfigError(ValueError):
    """Raised when the rule configuration file is malformed."""


class RuleSuggestionError(ValueError):
    """Raised when Codex returns an unusable rule suggestion."""
