"""Reading a sub-step's JSON reply file, tolerating the slips models make in one."""

import json
import re
from pathlib import Path

from rendering import FENCE

# A valid escape is consumed whole, so only a backslash starting none is doubled.
STRAY_ESCAPE = re.compile(r'\\(["\\/bfnrt]|u[0-9a-fA-F]{4})?')
# A regex or LaTeX `\b` or `\f` left unescaped decodes to a backspace or form
# feed, which review text shows as nothing.
CONTROL_ESCAPES = str.maketrans({"\b": "\\b", "\f": "\\f"})


class InvalidReply(Exception):
    """A reply that fails its step's schema."""


def unwrapped(text):
    """Return text with one wrapping code fence blanked out.

    A model may fence the JSON it writes. Blanking rather than dropping the
    fence lines keeps parse-error positions counting from the file.
    """
    lines = text.split("\n")
    filled = [index for index, line in enumerate(lines) if line.strip()]
    first, last = filled[0], filled[-1]
    if first < last and all(FENCE.match(lines[i].strip()) for i in (first, last)):
        for index in (first, last):
            lines[index] = " " * len(lines[index])
    return "\n".join(lines)


def parsed(text):
    """Parse a reply, repairing a raw control character or a stray backslash.

    Raw newlines and tabs in strings parse non-strictly; a backslash that starts
    no JSON escape, as in a quoted regex, is read as itself. An error names the
    text as written.
    """
    try:
        return json.loads(text, strict=False)
    except json.JSONDecodeError as error:
        repaired = STRAY_ESCAPE.sub(lambda m: m[0] if m[1] else "\\\\", text)
        try:
            return json.loads(repaired, strict=False)
        except json.JSONDecodeError:
            raise InvalidReply(f"reply is not JSON: {error}") from None


def spelled_out(value):
    """Return a parsed reply with each unwritable or invisible character escaped.

    `json` decodes an unpaired surrogate escape into a code point that no UTF-8
    stream can write; a backspace or form feed is spelled as its JSON escape.
    """
    if isinstance(value, str):
        text = value.encode("utf-8", "backslashreplace").decode("utf-8")
        return text.translate(CONTROL_ESCAPES)
    if isinstance(value, list):
        return [spelled_out(item) for item in value]
    if isinstance(value, dict):
        return {spelled_out(key): spelled_out(item) for key, item in value.items()}
    return value


def read_reply(path):
    """Return a reply file's JSON object."""
    try:
        text = Path(path).read_text(encoding="utf-8")
    except FileNotFoundError:
        text = ""
    except (OSError, UnicodeDecodeError) as error:
        raise InvalidReply(f"reply file unreadable: {error}") from None
    if not text.strip():
        raise InvalidReply("reply file missing or empty")
    reply = spelled_out(parsed(unwrapped(text)))
    if not isinstance(reply, dict):
        raise InvalidReply("reply is not a JSON object")
    return reply
