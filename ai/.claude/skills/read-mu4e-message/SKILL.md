---
description: >-
  Internal helper: read the message currently open in mu4e via emacsclient —
  message fields (sender, subject, maildir, docid) and the rendered body. Used
  by new-email-automation and process-email.
user-invocable: false
allowed-tools: Bash(emacsclient:*)
---

# Read the open mu4e message

Pull the message fields via emacsclient from `*mu4e-article*`, mu4e's view
buffer (`mu4e-view-buffer-name`). If that buffer does not exist, no message is
open: stop and ask the user to open one in mu4e. Do not read from
`*mu4e-headers*` point instead — it need not be on the message being viewed,
and the headers plist carries no body. If emacsclient cannot reach the server
(`can't find socket`), that is a different failure: ask the user to start
Emacs, not to open a message.

Fetch the fields and the rendered body in **one** eval. `*mu4e-article*` is a
single buffer reused under the same name — mu4e kills and recreates it per
message — so two separate evals can return fields from one message and a body
from another if the user navigates between them, with nothing in the output
revealing the swap.

```sh
emacsclient --eval '(with-current-buffer "*mu4e-article*"
  (let ((msg (mu4e-message-at-point)))
    (list :sender (mu4e-contact-email (car (mu4e-message-field msg :from)))
          :subject (mu4e-message-field msg :subject)
          :maildir (mu4e-message-field msg :maildir)
          :docid (mu4e-message-field msg :docid)
          :body (buffer-substring-no-properties (point-min) (point-max)))))'
```

Return the plist directly — do **not** wrap it in `format "%S"`. emacsclient
already prints the value with `pp`, so `format "%S"` would escape the body a
second time (a `"` arriving as `\\\"`, a `\` as `\\\\`), and callers that must
relay the body verbatim would have two levels to undo instead of one.

<!-- Keep in sync with the printed-form handling in new-email-automation
     (step 2) and process-email (step 1): this is the authority for the escape
     level; each caller states only what it does with it. -->

Every returned string arrives in `pp`'s printed form — one level of escaping,
with `\n` for newlines, `\"` for quotes, `\\` for backslashes, and the
surrounding quotes included. `pp` will not break a line inside a string, so
`:body` is one unbroken token that begins on a continuation line of the printed
plist: take it whole rather than by its first line. Which level a caller wants
differs by destination — text rendered as prose must be decoded first, while
text pasted into an elisp string literal must keep the printed form exactly.

`:body` is the rendered article text — headers, body, and attachment lines as
mu4e displays them. That is a display, not the message source: transfer
encodings and MIME structure are already resolved, the header block is filtered
to `mu4e-view-fields`, and for an HTML part `shr` renders the link text while
the target survives only in a `shr-url` text property that
`buffer-substring-no-properties` drops — so a link's URL can be absent from
`:body` entirely. A caller that must match text against the message source has
to read that source itself; `:body` cannot stand in for it.

`:maildir` is the folder the message arrived in — context for a caller, not a
matchable rule key (see new-email-automation step 3). `:docid` is mu's own
indexer-generated integer, so callers that substitute it into a command
interpolate no sender-controlled text. The message date is not returned as its
own field: `:body` carries the `Date:` header with the sender's UTC offset
intact, which `format-time-string` would discard.
