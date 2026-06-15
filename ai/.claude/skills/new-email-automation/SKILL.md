---
description: >-
  Write a new mu4e-autotask email automation rule for the message currently
  open in mu4e. Reads ~/secrets-local.el for conventions and the open
  *mu4e-article* message via emacsclient, asks how to handle it, then adds an
  action function and rule to ~/secrets-local.el.
argument-hint: optional description of how the message should be handled
allowed-tools: >-
  Bash(emacsclient:*)
  Read
  Edit
  Skill(elisp-dev)
  AskUserQuestion
---

# New Email Automation

Create a new `mu4e-autotask` email automation for the message the user has open
in mu4e. An automation is an action function plus a rule (a property list
matching sender/subject) in `mu4e-autotask-rules`. Both live in
`~/secrets-local.el`. Name new action functions `dotfiles--mu4e-automation-<name>`.

The `Read` and `Edit` tools require an absolute path: expand `~` to your home
directory when passing `~/secrets-local.el` to them.

Follow the user's Elisp guidelines throughout — invoke the `elisp-dev` skill
before writing any elisp.

## Procedure

### 1. Learn the conventions

`Read` `~/secrets-local.el`. Locate the `(setq mu4e-autotask-rules ...)` form and
the existing action functions, and note the surrounding style (docstrings, helper
usage).

**This file holds secrets** (app passwords, account addresses, calendar IDs).
Treat its contents as private: never quote, echo, transmit, or persist
secret-bearing lines in any output, commit, or message.

The `mu4e-autotask` helper API (documented in
`~/dotfiles/emacs/.emacs.d/elpa/mu4e-autotask/README.org`) — reuse these instead
of re-implementing MIME/parsing/sending logic. The action function receives
`msg`; the no-argument helpers below operate on that same current message view:

- Reading: `mu4e-autotask-raw-message (msg)`, `mu4e-autotask-text-content` (no
  args), `mu4e-autotask-html-content` (no args),
  `mu4e-autotask-msg-content (mime-type)`. The three content helpers signal a
  `user-error` when the message has no part of the requested type, so pick the
  helper matching the message's actual parts (e.g. `-text-content` only when a
  `text/plain` part is present), and guard or choose accordingly where a target
  message might lack the expected part.
- Attachments: `mu4e-autotask-pdf-part` (no args), `mu4e-autotask-csv-part` (no
  args), `mu4e-autotask-save-csv-part` (no args),
  `mu4e-autotask-save-part-file (part)`, `mu4e-autotask-for-each-attachment (fn)`,
  `mu4e-autotask-open-all-attachments (suffix)`, `mu4e-autotask-download-all-jpgs`
  (no args)
- Sending: `make-mu4e-autotask-email-template`, `mu4e-autotask-send-email`,
  `mu4e-autotask-do-send-email`

### 2. Read the open message

Pull the match fields via emacsclient. The snippets below target the common
`*mu4e-article*` buffer; if it does not exist, adapt them to the live view buffer
(`*mu4e-view*`) or read from `*mu4e-headers*` point. If no message is open, stop
and ask the user to open one in mu4e.

```sh
emacsclient --eval '(with-current-buffer "*mu4e-article*"
  (let ((msg (mu4e-message-at-point)))
    (format "%S" (list :sender (mu4e-contact-email
                                (car (mu4e-message-field msg :from)))
                       :subject (mu4e-message-field msg :subject)
                       :maildir (mu4e-message-field msg :maildir)))))'
```

Then capture the rendered body to understand what to act on (URLs, attachment
types, etc.):

```sh
emacsclient --eval '(with-current-buffer "*mu4e-article*"
  (buffer-substring-no-properties (point-min) (point-max)))'
```

### 3. Derive the match criteria

Use the `:sender` value captured in step 2 for `:sender-exact` (use
`:sender-match` only when the sender varies). Choose `:subject-exact` or
`:subject-match` depending on whether the subject is stable or templated.

Each rule may set **at most one** of `:sender-exact`/`:sender-match` and at most
one of `:subject-exact`/`:subject-match`. `mu4e-autotask-dispatch` validates
every rule up front and signals an error — breaking dispatch for _every_
message — if a single rule sets conflicting keys.

**Check the new rule against every existing rule.** `mu4e-autotask-dispatch` also
signals an error and runs nothing when two rules match the same message, so the
new rule must not overlap an existing one.

### 4. Get the desired handling from the user

If the invocation argument already describes how to handle the message, use it.
Otherwise ask (AskUserQuestion or plain dialogue). Map the intent to the helper
set above — e.g. open PDF attachments, open a URL parsed from the body/raw
message, forward to someone, send a templated reply, save photos, record a
calendar event. This is the handling the rule implements.

### 5. Write the rule and action function

First confirm the file's visiting buffer (if any) has no unsaved changes, so the
live re-eval below won't fight a modified buffer:

```sh
emacsclient --eval '(let ((b (get-file-buffer (expand-file-name "~/secrets-local.el"))))
  (and b (buffer-modified-p b)))'
```

If it returns `t`, ask the user to save the buffer and stop. `nil` means the
buffer is clean or the file is not open — either way it is safe to edit.

Then `Edit` `~/secrets-local.el`:

- Add `(defun dotfiles--mu4e-automation-<name> (msg) ...)` near the other action
  functions, with a docstring, reusing helpers — no bespoke MIME or parsing code
  when a helper exists.
- Add the rule plist as a new element inside the existing
  `(setq mu4e-autotask-rules ...)` literal, alongside the other rule plists:

  ```elisp
  (:sender-exact "noreply@example.com"
   :subject-match "^Your order .*$"
   :action-fn dotfiles--mu4e-automation-<name>)
  ```

### 6. Apply live

Eval just the new pieces into the running Emacs — do **not** reload the whole
file (that re-runs every side effect in it). Copy the `defun` and rule plist
**verbatim** from what you wrote in step 5, carrying the same match keys:

```sh
emacsclient --eval '(progn
  (defun dotfiles--mu4e-automation-<name> (msg) ...)
  (add-to-list (quote mu4e-autotask-rules)
               (quote (:sender-exact "noreply@example.com"
                       :subject-match "^Your order .*$"
                       :action-fn dotfiles--mu4e-automation-<name>))))'
```

Then revert the file's visiting buffer so it reflects the on-disk edit and a
later buffer save cannot clobber the new rule. If the buffer has unsaved changes,
do **not** revert (that would discard them) — report it and ask the user to
reconcile the buffer with the on-disk file manually:

```sh
emacsclient --eval '(let ((b (get-file-buffer (expand-file-name "~/secrets-local.el"))))
  (when b
    (if (buffer-modified-p b)
        "buffer has unsaved changes - reconcile manually"
      (progn (with-current-buffer b (revert-buffer t t t)) "reverted"))))'
```

### 7. Verify (do not trigger the automation)

- Confirm the elisp loads cleanly — the step-6 eval returns without error,
  parens balanced.
- Confirm the live `dotfiles--mu4e-automation-<name>` definition and rule match
  what step 5 wrote to the file (they were typed separately and must not have
  drifted).
- Confirm the rule uniquely matches the open message and conflicts with no
  existing rule.
- Do **not** run the automation (it may send mail, open apps, or create tasks).
  Tell the user to test it from the message view via the "Execute automation"
  mu4e view action.

### 8. Surface refactoring opportunities

After the code is written, scan `~/secrets-local.el` for action-function
patterns that have now recurred and contain **no secret-leaking** material (no
app passwords, account addresses, calendar IDs, or other machine-private data).
Such generic logic does not belong in the private file — propose lifting it into
the dotfiles repository:

- Truly generic mu4e automation building blocks → the `mu4e-autotask` package at
  `~/dotfiles/emacs/.emacs.d/elpa/mu4e-autotask/` (its own repository; follow its
  `CLAUDE.md` — TDD, README.org update, `./check.sh`).
- Dotfiles-specific but non-secret glue → the emacs config
  (`~/dotfiles/emacs/.emacs.d/my/`).

Only **propose** this — do not perform the refactor inline; it is a separate,
judgment-heavy concern. Present the candidate helper(s) and suggested home, and
let the user decide whether to act now or later.
