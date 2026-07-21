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
  Skill(read-mu4e-message)
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
- Acting on the message (reserve these for step 4):
  `mu4e-autotask-browse-url-matching`, arglist
  `(regexp content &optional description group)`, and
  `mu4e-autotask-when-body-matches`, arglist
  `(msg regexp action &optional skip-message)`. `group` defaults to 1, so pass
  `0` for a regular expression with no capture group; a non-participating group
  yields a message and nil rather than an error. `when-body-matches` matches
  against the raw message (headers included) and takes no content argument.

### 2. Read the open message

Invoke `Skill(read-mu4e-message)` to pull the message fields (`:sender`,
`:subject`, `:maildir`) and `:body`, the rendered message text. Only `:sender`
and `:subject` can appear in a rule (step 3); `:maildir` is context, not a
match key.

Use `:body` to decide _what_ to act on — a tracking link, a PDF invoice, a
marker that separates two notification kinds. Do not lift a matching pattern
out of it: the action function matches the message source, which `:body` is
not (step 4).

### 3. Derive the match criteria

Use the `:sender` value captured in step 2 for `:sender-exact` (use
`:sender-match` only when the sender varies). Choose `:subject-exact` or
`:subject-match` depending on whether the subject is stable or templated.

<!-- Keep in sync with read-mu4e-message's printed-form paragraph: that skill is
     the authority for the escape level; this states only how to paste it. -->

Step 2's values arrive in printed form, one level of string escaping. For an
`-exact` key, paste that printed token whole, surrounding quotes included: it is
already a valid elisp string literal. Do **not** strip the quotes and re-add
them, and do not decode the `\"` / `\\` inside — a subject containing either
would then write broken elisp into `~/secrets-local.el`, which `init.el` loads
at every start, and paren balance alone would not catch it.

Prefer the `-exact` keys — they compare with `string=`, so the captured value
goes in as-is, and two literals are something you can actually check for overlap
below. A `-match` value is a live regular expression: its metacharacters must be
escaped in any literal text carried from the message. The rules list is
backquoted (its `setq` value is `` `( … ) ``, and the file already unquotes on
`:subject-match`), so you have two ways to do that — write the escapes into the
string literal, or unquote a call that produces them:

```elisp
:subject-match "^\\[GitHub\\] Build #.* passed$"
:subject-match ,(concat "^" (regexp-quote "[GitHub] Build #") ".* passed$")
```

A **bare** `(regexp-quote …)` with no leading `,` is data, not a call: the key
would hold a list, `string-match-p` would signal at match time, and step 6 would
still print `"installed"`.

Each rule must set **at least one** of `:sender-exact`/`:sender-match`: a rule
with neither never matches any message, and nothing reports it. (The subject
pair is the opposite — absent, it matches every subject.) It may set **at most
one** of each pair, and must carry an `:action-fn` that is a live function.
`mu4e-autotask-dispatch` validates every rule up front, before matching any of
them, and signals an error — breaking dispatch for _every_ message, not just the
ones the bad rule would match — if a rule sets conflicting keys or names an
`:action-fn` that is not a live function (its `defun` unevaluated in this Emacs,
or no `defun` of that name anywhere). It does **not** check for a missing sender
key: that rule installs, step 6 prints `"installed"`, and it silently never
fires — so the sender-key requirement is on you.

**Check the new rule against every existing rule.** `mu4e-autotask-dispatch` also
signals an error and runs nothing when two rules match the same message, so the
new rule must not overlap an existing one.

### 4. Get the desired handling from the user

If the invocation argument already describes how to handle the message, use it.
Otherwise ask (AskUserQuestion or plain dialogue). Map the intent to the helper
set above — e.g. open PDF attachments, open a URL from the message, forward to
someone, send a templated reply, save photos, record a calendar event. This is
the handling the rule implements.

When the handling turns on message content — a URL, a marker — write its pattern
against a source you have actually looked at, and choose that source
deliberately. `:body` settles nothing here: it is a rendered display, not a
source you can match (step 2), and an HTML link's target is not even in it.

- **Match a decoded body part, not the raw message.** `mu4e-autotask-html-content`
  / `-text-content` return the part with its transfer encoding already undone by
  `mm-get-part`, so quoted-printable soft breaks, `=3D` escapes, and base64 are
  gone before you match. Matching `(mu4e-autotask-raw-message msg)` instead hits
  all three: `browse-url-matching` repairs only folds and `&amp;` (never `=3D`,
  so a query-string URL keeps it), a base64 part cannot be matched at all, and
  `when-body-matches` matches raw with no repair whatsoever. Reserve raw for a
  marker in the headers or spanning parts, where no decoded part holds it.
- **Key the character class to the delimiter.** On a decoded source there is no
  fold to span, so use the class that stops at the link's real delimiter, with a
  capture group and explicit terminator — not a blanket greedy `[^>]`, which
  runs past a quoted `href` to the tag's `>`:

  ```elisp
  (mu4e-autotask-browse-url-matching "href=\"\\(https://[^\"]+\\)\""
                                     (mu4e-autotask-html-content) nil 1)
  ```

- **The pattern matches bytes, not characters.** These helpers return unibyte
  strings, so a non-ASCII marker in an ordinary multibyte pattern will not
  match. Keep the pattern ASCII — anchor on an ASCII neighbour (a domain, an
  ID), as the `README.org` examples do — or wrap it in `encode-coding-string`.
  That
  wrap is legal here because the pattern lives in the action function's `defun`
  body, which is evaluated — unlike a bare form in the backquoted rules list,
  which is data unless prefixed with `,` (step 3).

Look at the chosen source before writing the pattern (`browse-url-matching` and
`when-body-matches` are in step 1; the worked examples are in `README.org`). If
the target is not visible in any source you can reach, ask the user for it here
rather than guessing.

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
  when a helper exists. Build `<name>` from lowercase letters, digits, and
  hyphens only, as every existing name is: step 6 interpolates it twice — into
  a regular expression, where `+`, `*`, `?`, `[`, or `\` would change what the
  anchor matches, and into an `intern` that must yield the very symbol the
  anchor found, which an uppercase letter would break because the search folds
  case and `eq` does not. `<name>` must also not already name an action function
  in `~/secrets-local.el` — check against the names step 1 read. Step 6's anchor
  evaluates the first matching `defun` while a restart evaluates the last, so a
  duplicate name installs one body now and the other after a restart, silently
  repointing an existing rule at your `defun` — or yours at its — with nothing
  anywhere reporting it.
- Add the rule plist as a new element inside the existing
  `(setq mu4e-autotask-rules ...)` literal, alongside the other rule plists:

  ```elisp
  (:sender-exact "noreply@example.com"
   :subject-match "^Your order .*$"
   :action-fn dotfiles--mu4e-automation-<name>)
  ```

### 6. Apply live

Install just the two forms step 5 wrote — do **not** reload the whole file
(that re-runs every side effect in it), and do **not** retype the forms into
the eval.

Never interpolate the `defun` or the rule plist into an `emacsclient --eval`
shell string. Both carry message-derived text — the address in `:sender-exact`,
the subject in `:subject-match` — and a single quote in either ("O'Brien",
"Your order's ready") terminates the quoted string; a hostile sender chooses
that text deliberately, and what follows it is parsed as further arguments to
the same command. Instead have Emacs read the two forms back out of
`~/secrets-local.el` itself: the command below interpolates one value, the
action function's name you chose in step 5, so no message-derived byte reaches
a shell word.

```sh
emacsclient --eval '(with-temp-buffer
  (insert-file-contents (expand-file-name "~/secrets-local.el"))
  (dolist (anchor (list "^(defun dotfiles--mu4e-automation-<name>\\_>"
                        "^(setq mu4e-autotask-rules\\_>"))
    (goto-char (point-min))
    (re-search-forward anchor)
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)) t))
  (let ((fns (mapcar (lambda (rule) (plist-get rule :action-fn))
                     mu4e-autotask-rules))
        (clashes (mapcar (lambda (rule) (plist-get rule :action-fn))
                         (seq-filter
                          (lambda (rule)
                            (or (and (plist-get rule :sender-exact)
                                     (plist-get rule :sender-match))
                                (and (plist-get rule :subject-exact)
                                     (plist-get rule :subject-match))))
                          mu4e-autotask-rules))))
    (cond ((seq-remove (function functionp) fns))
          (clashes (format "conflicting-match-keys: %S" clashes))
          ((not (memq (intern "dotfiles--mu4e-automation-<name>") fns))
           "no-rule-for-action-fn")
          (t "installed"))))'
```

It must print `"installed"`. Before reading any other outcome as a file problem,
rule out the command itself. Two outcomes turn on the name you interpolated —
`"no-rule-for-action-fn"` and `search-failed` — so for those, first confirm both
`<name>` sites in the command carry the same text as the `defun` in the file; if
they differ, the file is fine and the command is wrong, and re-running it
unchanged returns the same result. And if the command never reached Emacs — a
shell error, `can't find socket`, `emacsclient` not found — nothing was
installed, the file still holds step 5's edit and needs no change: fix the
command, or ask the user to start Emacs (a server unreachable _after_ the user
confirms Emacs is running points at `~/secrets-local.el` aborting `init.el` —
see the last bullet — not at the forms). Any other outcome from Emacs:

- **`"no-rule-for-action-fn"`** — both forms installed, but no rule in the live
  list carries the `:action-fn` step 5 wrote, so the automation would never
  fire. Either the plist landed outside the `(setq mu4e-autotask-rules ...)`
  literal — one paren past its closer it is a balanced top-level form, so
  nothing has flagged it and Emacs will signal on it at the next start — or it
  is inside it under a different `:action-fn`. Fix the file and re-run.
- **A non-empty list** — both forms installed, but the listed `:action-fn`
  values are not live functions, which breaks dispatch for _every_ message until
  they are (step 3). Branch on what the elements are. `nil`, a keyword, or a
  non-symbol such as `(quote f)` means a rule plist in the file is malformed —
  most likely the one step 5 just wrote, since a bare `'` or `#'` inside the
  rules literal is not evaluated and so does not name a function — so fix the
  file and re-run. A
  bare symbol is never the name you interpolated — the first anchor made that
  one live — so check whether `~/secrets-local.el` defines it: a
  `(defun <that symbol>` opening at column 0. You read the file in step 1;
  re-`Read` it if unsure. If it does, the file is fine and that `defun` has not
  been evaluated in this Emacs — a rule the file gained since Emacs started:
  report it and ask the user to restart Emacs or evaluate it, and do not define
  it yourself. If it does not, a rule names a function that does not exist — most
  likely step 5's rule and `defun` disagree on the name, or the rule kept the
  literal `<name>` — so fix the file and re-run.
- **`"conflicting-match-keys: (…)"`** — both forms installed and every
  `:action-fn` is live, but each rule owning a listed `:action-fn` sets both
  `:sender-exact` and `:sender-match`, or both `:subject-exact` and
  `:subject-match`, which breaks dispatch for _every_ message until one key of
  each pair is gone (step 3). This one does not depend on your interpolation —
  it is computed from the live rules list alone. If a listed name is the one
  step 5 wrote, fix that rule and re-run. If it is another rule's, the file
  already carried it and re-evaluating the whole `setq` is what made it live — a
  restart would do the same, so it still has to be fixed: report it and ask the
  user which key to keep, rather than choosing for them.
- **`search-failed`** — step 5's edit is not where this expects it. **Any other
  error from Emacs** — the forms themselves. On the first anchor nothing was
  installed; on the second the new `defun` is live with `mu4e-autotask-rules`
  untouched, which is inert (no rule references it). Either way, fix the file and
  re-run — the step is repeatable — never patch around it inside the eval, and do
  it before Emacs restarts: `init.el` loads `~/secrets-local.el` at every start,
  and its `noerror` covers a missing file, not a form that errors. A restart
  therefore re-runs whatever is in the file — completing the install if the file
  is now valid, or aborting `init.el` there and skipping the rest of the
  configuration, `server-start` included, which takes `emacsclient` and this
  step's re-run path offline. If you cannot make the file loadable again, restore
  its pre-edit content or tell the user plainly that Emacs will not start
  correctly — then **stop**, and do not run the revert below: the visiting buffer
  is still unmodified and still holds the pre-edit text, so reverting would
  overwrite the last loadable copy with the broken file.

Once the command prints `"installed"`, revert the file's visiting buffer so it
reflects the on-disk edit and a later buffer save cannot clobber the new rule.
If the buffer has unsaved changes, do **not** revert (that would discard them) —
report it and ask the user to reconcile the buffer with the on-disk file
manually:

```sh
emacsclient --eval '(let ((b (get-file-buffer (expand-file-name "~/secrets-local.el"))))
  (when b
    (if (buffer-modified-p b)
        "buffer has unsaved changes - reconcile manually"
      (progn (with-current-buffer b (revert-buffer t t t)) "reverted"))))'
```

<!-- The rest of step 6 is rationale for a future editor of the eval, not
     instructions the running agent acts on. -->

Five properties of the install eval are load-bearing; keep them if you edit it:

1. The `^` anchors restrict each match to column 0 — ruling out a commented-out
   copy (an elisp comment starts with `;`, so `^(` cannot match) and any
   indented occurrence quoted in a string. A column-0 line inside a multi-line
   string would still match.
2. Each anchor ends with `\\_>`, a symbol boundary, so neither can match a longer
   name that begins the same way; being zero-width it also matches at a line
   break, so a `defun` whose arglist wrapped onto the next line — or the newline
   the rules list usually starts on — is found anyway.
3. The `t` in `(eval FORM t)` supplies the lexical binding the file's own
   `-*- lexical-binding: t; -*-` cookie supplies at startup, so the live
   definition is the same dialect as the on-disk one.
4. Re-evaluating the whole `(setq mu4e-autotask-rules ...)` form, rather than
   pushing one rule onto the list, makes the step repeatable: run it again after
   correcting the rule and the live list still matches the file, with no stale
   duplicate left to break dispatch. It also installs every other rule the file
   gained since Emacs started — but not their `defun`s, which is what the
   `cond`'s first clause checks.
5. No apostrophe appears anywhere in the form: it is a POSIX single-quoted shell
   string, so a `'` would end it. That is why the anchors use `(list …)` not
   `'(…)`, the predicate is `(function functionp)` not `#'functionp`, and the
   membership test is `(intern "…")` not `'…` — overriding `elisp-dev`'s
   literal-list and sharp-quote preferences, which apply to ordinary elisp
   source, not to a string handed to the shell.

The returned value is load-bearing too. emacsclient prints the value of the form
it evaluates; the `dolist` discards the `setq`'s, and the `cond` makes the tail
an explicit token. Never let an `(eval (read …))` of the rules form be the last
form — unrolling the `dolist` into straight-line evals would print every rule in
the secrets file, with every stored address. Keep the check returning
`:action-fn` values, never the rules that contain them; that is also why it does
not simply `mapc` the package's own `mu4e-autotask--validate-rule`, whose
`user-error` quotes the offending rule in full.

<!-- Keep in sync with mu4e-autotask--validate-rule: the cond's first clause
     (functionp) and the clashes filter (the two conflicting-key pairs) mirror
     that validator's three conditions. If it gains a condition, add it here too,
     or "installed" will certify a rule that then breaks dispatch. The
     sender-key requirement (step 3) is deliberately not the validator's and
     stays prose-only. -->

The two conflicting-key conditions are re-implemented here for that reason too,
reporting the offending rule's `:action-fn` instead of the rule. They test with
`plist-get`, as that validator does, not `plist-member`: a rule carrying
`:sender-match nil` explicitly is not a conflict, and `plist-member` would
over-report against the very function this clause exists to predict. Use
`functionp` — the same test that validator applies — not `fboundp`, which is
weaker (also true of macros) and signals on a non-symbol `:action-fn`. The
membership clause is what makes `"installed"` mean the new rule is live rather
than merely that the surviving rules are healthy. It uses `intern` rather than
`intern-soft` only so that it cannot return `nil`: clause 1 has already excluded
`nil` from `fns`, so `intern-soft` would be correct here too — but correct only
because clause 1 runs first. That ordering is why the conflict clause sits second
rather than first: clause 1 guarantees every `:action-fn` the conflict clause
then prints is a live function the user can locate, and a plist garbled enough to
set both keys of a pair may also have lost its `:action-fn`, where the `(nil)`
report is the more fundamental diagnosis.

### 7. Verify (do not trigger the automation)

- Confirm step 6 printed `"installed"`. That is the only success value, and step
  6 says what any other value or error means and what to do about it. It
  certifies the wiring, and only the wiring: both forms were installed from the
  file's own text rather than a retyped copy — so the live `defun` and rule are
  step 5's edit itself, which holds because step 5 requires `<name>` to be
  unused — a live rule names that `defun` as its `:action-fn`, every rule in the
  live list has a callable one, and no rule sets conflicting match keys. It
  certifies nothing about what those keys _match_: a `:subject-match` that is a
  valid regular expression over the wrong set of messages prints `"installed"`
  just the same. That is the next bullet.
- Confirm the rule sets at least one sender key — a rule with neither matches
  nothing, and `"installed"` does not detect that — then that it matches the
  open message, and that no other rule matches it too.
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
