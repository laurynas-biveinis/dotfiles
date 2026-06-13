"""Shared fixtures and helpers for the xml2qif test modules."""

import json
import sys
import types
from pathlib import Path

BIN_DIR = Path(__file__).resolve().parent.parent.parent / "scripts" / "usr" / "bin"
if str(BIN_DIR) not in sys.path:
    sys.path.insert(0, str(BIN_DIR))

CONFIG_TEXT = """[First]
match = ^FIRST$
payee = First Payee
category = First:Category

[First Extra]
match = ^FIRST EXTRA$
payee = Extra Payee
category = Extra:Category

[Last]
match = ^LAST$
payee = Last Payee
category = Last:Category
"""

NEW_ENTRY = "[New Rule]\nmatch = ^NEW$\npayee = New Payee\ncategory = New:Category"

COMMENTED_CONFIG_TEXT = CONFIG_TEXT.replace(
    "[Last]", "# Last rules\n# more about Last\n[Last]"
)


def suggest_json(**overrides):
    """Build a valid Codex suggest JSON string with optional field overrides."""
    suggestion = {
        "action": "suggest",
        "confidence": 75,
        "insert_after": "First",
        "config_entry": NEW_ENTRY,
    }
    suggestion.update(overrides)
    return json.dumps(suggestion)


def make_transaction(
    debitor="", creditor="", unstructured_info="", amount="-10.00", xml_obj=None
):
    """Build a stub transaction with the attributes rule matching reads, plus
    xml_obj for the suggestion flow, which serializes it into the prompt."""
    return types.SimpleNamespace(
        debitor=debitor,
        creditor=creditor,
        unstructured_info=unstructured_info,
        amount=amount,
        xml_obj=xml_obj,
    )


def write_text(path, text):
    """Write text to a file as UTF-8."""
    with open(path, "w", encoding="utf-8") as text_file:
        text_file.write(text)


def read_text(path):
    """Read a file back as UTF-8 text."""
    with open(path, encoding="utf-8") as text_file:
        return text_file.read()
