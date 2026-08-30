;;; my-lib-test.el --- Tests for my-lib -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the personal helper library.

;;; Code:

(require 'cl-lib)
(require 'ert)

;; Stub unavailable mu4e features while loading `my-lib', then remove them so
;; later tests cannot use the stubs.
(provide 'mu4e-message)
(provide 'mu4e-autotask)
(require 'my-lib)
(dolist (stub '(mu4e-message mu4e-autotask))
  (setq features (delq stub features)))

(ert-deftest dotfiles--parse-gh-release-subject-same-v-tags-test ()
  (should (equal (dotfiles--parse-gh-release-subject
                  "[google/googletest] Release v1.18.0 - v1.18.0")
                 '(:gh-org "google" :gh-project "googletest"
                           :gh-name "google/googletest"
                           :rel-tag "v1.18.0"))))

(ert-deftest dotfiles--parse-gh-release-subject-v-title-only-test ()
  (should (equal (plist-get (dotfiles--parse-gh-release-subject
                             "[o/p] Release v1.0 - 1.0")
                            :rel-tag)
                 "v1.0")))

(ert-deftest dotfiles--parse-gh-release-subject-v-tag-only-test ()
  (should (equal (plist-get (dotfiles--parse-gh-release-subject
                             "[o/p] Release 1.0 - v1.0")
                            :rel-tag)
                 "1.0")))

(ert-deftest dotfiles--parse-gh-release-subject-no-v-tags-test ()
  (should (equal (plist-get (dotfiles--parse-gh-release-subject
                             "[o/p] Release 1.0 - 1.0")
                            :rel-tag)
                 "1.0")))

(ert-deftest dotfiles--parse-gh-release-subject-mismatched-tags-test ()
  (should-error (dotfiles--parse-gh-release-subject
                 "[o/p] Release v1.0 - v2.0")
                :type 'user-error))

(ert-deftest dotfiles--parse-gh-release-subject-non-release-test ()
  (should-error (dotfiles--parse-gh-release-subject
                 "[o/p] Some other notification")
                :type 'user-error))

;; Match `user-error' quoting across `text-quoting-style' values.
(ert-deftest dotfiles--get-project-branch-root-misconfigured-test ()
  (should (equal
           (cadr (should-error (dotfiles--get-project-branch-root
                                (make-my-dev-project :name "demo"))
                               :type 'user-error))
           (format-message
            "Project demo has no :branch-root in `my-projects'"))))

(defconst dotfiles--lib-test-append-link-org "\
* Container
** TODO Decoy
*** TODO Decoy child
** TODO Target
Body line.
** TODO After
"
  "Org text for the `dotfiles--org-append-mu4e-link' tests.
\"Target\" is neither the first nor the last entry, so a link appended to the
wrong subtree lands at a different place than one appended to Target's.")

(defconst dotfiles--lib-test-append-link-org-appended "\
* Container
** TODO Decoy
*** TODO Decoy child
** TODO Target
Body line.
[[mu4e:msgid:new@example.com][New]]
** TODO After
"
  "`dotfiles--lib-test-append-link-org' after the link is appended to Target.
`dotfiles--lib-test-append-link-org-linked' cannot stand in for it: that one
carries a body line below the link for the scan-from-heading test.")

(defconst dotfiles--lib-test-append-link-org-linked "\
* Container
** TODO Decoy
*** TODO Decoy child
** TODO Target
Body line.
[[mu4e:msgid:new@example.com][New]]
Content below the link.
** TODO After
"
  "Like `dotfiles--lib-test-append-link-org', with Target already linked.
The trailing body line sits below the link so a test can start point there:
appending again must still be a no-op, which the dedup scan only achieves by
covering Target's whole subtree rather than starting wherever point happens to
be.")

(defconst dotfiles--lib-test-append-link-org-parent "\
* Container
** TODO Decoy
*** TODO Decoy child
** TODO Target
Body line.
*** TODO Target child
Child body.
** TODO After
"
  "Like `dotfiles--lib-test-append-link-org', with Target holding a child entry.
Pins that the append reaches the end of Target's whole subtree rather than the
end of Target's own body -- the distinction its docstring's \"subtree\" turns
on, and the one an order task with a hand-added sub-action would meet.")

(defconst dotfiles--lib-test-append-link-org-parent-appended "\
* Container
** TODO Decoy
*** TODO Decoy child
** TODO Target
Body line.
*** TODO Target child
Child body.
[[mu4e:msgid:new@example.com][New]]
** TODO After
"
  "`dotfiles--lib-test-append-link-org-parent' after the link has been appended.
The link sits below the child, not between Target's body and the child.")

(defconst dotfiles--lib-test-append-link-org-confusable "\
* Container
** TODO Decoy
*** TODO Decoy child
** TODO Target
Body line.
[[mu4e:msgid:NEW@example.com][Upcased]]
[[mu4e:msgid:new@example.com.uk][Longer]]
** TODO After
[[mu4e:msgid:new@example.com][Elsewhere]]
"
  "Like `dotfiles--lib-test-append-link-org', with links dedup must reject.
A Message-ID differing only in case, one carrying the searched one as a proper
prefix, and an exact match outside Target's subtree.  Appending must still
happen, so each rejection pins one of the scan's three discriminators -- the
`case-fold-search' binding, the closing `]', and the subtree `end' bound.
Dropping any of them turns the append into a silent no-op, which is data loss:
the link is skipped and reported as already filed.")

(defconst dotfiles--lib-test-append-link-org-spaced "\
* Container
** TODO Decoy
*** TODO Decoy child
** TODO Target
Body line.

** TODO After
"
  "Like `dotfiles--lib-test-append-link-org', with a blank line ending Target.
The only shape in which the append's placement rule is observable: every other
fixture butts the body straight against the next heading, so an implementation
that inserted below the trailing whitespace instead of above it would reproduce
them all byte for byte while detaching the link from its own entry and eating
the separator before the next one.")

(defconst dotfiles--lib-test-append-link-org-spaced-appended "\
* Container
** TODO Decoy
*** TODO Decoy child
** TODO Target
Body line.
[[mu4e:msgid:new@example.com][New]]

** TODO After
"
  "`dotfiles--lib-test-append-link-org-spaced' after one append.")

(defun dotfiles--lib-test-goto-line-matching (regexp)
  "Move point to the beginning of the first line matching REGEXP.
Signal `search-failed' when nothing matches, so a fixture that stopped
containing the target line fails loudly."
  (goto-char (point-min))
  (re-search-forward regexp)
  (beginning-of-line))

(defun dotfiles--lib-test-should-be-folded (folded)
  "Assert the line at point is folded exactly when FOLDED."
  (if folded
      (should (org-invisible-p (point)))
    (should-not (org-invisible-p (point)))))

(defun dotfiles--lib-test-append-link-in-org (text fold &optional start-regexp)
  "Append a link to the \"Target\" entry of org TEXT.
Start with point at the line matching START-REGEXP, defaulting to Target's own
heading.  With FOLD non-nil, collapse the buffer first.  Assert the starting
line's visibility either way, so the unfolded case cannot quietly become a
second folded one should the `org' startup default ever change.
Return a cons of the appender's return value and the resulting buffer text."
  (with-temp-buffer
    (insert text)
    (org-mode)
    (dotfiles--lib-test-goto-line-matching
     (or start-regexp "^\\*\\* TODO Target$"))
    (when fold (org-overview))
    (dotfiles--lib-test-should-be-folded fold)
    (cons (dotfiles--org-append-mu4e-link
           "[[mu4e:msgid:new@example.com][New]]" "new@example.com")
          (buffer-substring-no-properties (point-min) (point-max)))))

(ert-deftest dotfiles--org-append-mu4e-link-unfolded-test ()
  (should (equal (dotfiles--lib-test-append-link-in-org
                  dotfiles--lib-test-append-link-org nil)
                 (cons t dotfiles--lib-test-append-link-org-appended))))

(ert-deftest dotfiles--org-append-mu4e-link-folded-test ()
  (should (equal (dotfiles--lib-test-append-link-in-org
                  dotfiles--lib-test-append-link-org t)
                 (cons t dotfiles--lib-test-append-link-org-appended))))

;; `fold' nil only: the fold axis is carried three times already, and the
;; subtree-end scan is text-based and indifferent to visibility.
(ert-deftest dotfiles--org-append-mu4e-link-keeps-trailing-blank-line-test ()
  (should (equal (dotfiles--lib-test-append-link-in-org
                  dotfiles--lib-test-append-link-org-spaced nil)
                 (cons t dotfiles--lib-test-append-link-org-spaced-appended))))

(ert-deftest dotfiles--org-append-mu4e-link-appends-past-child-test ()
  (should (equal (dotfiles--lib-test-append-link-in-org
                  dotfiles--lib-test-append-link-org-parent nil)
                 (cons t dotfiles--lib-test-append-link-org-parent-appended))))

;; The sole dedup test, and deliberately so: the scan is `search-forward', which
;; ignores invisibility, so an unfolded duplicate case would detect a subset of
;; what this one does.  Point starts below the existing link, so a dedup scan
;; that began at point instead of at the heading would miss it and append a
;; second copy; the folded buffer additionally guards a future scan that does
;; respect invisibility.
(ert-deftest dotfiles--org-append-mu4e-link-scans-from-heading-test ()
  (should (equal (dotfiles--lib-test-append-link-in-org
                  dotfiles--lib-test-append-link-org-linked t
                  "^Content below the link\\.$")
                 (cons nil dotfiles--lib-test-append-link-org-linked))))

;; The negative half of dedup: text the scan must not treat as a match.  The
;; test above pins that dedup fires on a real duplicate; without this one every
;; discriminator that keeps it from firing on a near miss is a free variable.
(ert-deftest dotfiles--org-append-mu4e-link-ignores-confusable-msgids-test ()
  (let ((result (dotfiles--lib-test-append-link-in-org
                 dotfiles--lib-test-append-link-org-confusable nil)))
    ;; Only the return value: each discriminator decides whether dedup fires,
    ;; and where the link lands is pinned by the tests above.
    (should (car result))
    (should (string-match-p
             (regexp-quote "[[mu4e:msgid:new@example.com][New]]")
             (cdr result)))))

(defconst dotfiles--lib-test-order-task-heading
  "Iš teststore 2026-08-17 12345 užsakymo"
  "The heading the order-task fixtures resolve to.
Do not splice this into the fixture text: there it is the input the code parses,
and deriving both sides from one binding would make the assertions
tautological.")

(defconst dotfiles--lib-test-order-file-org-decoys "\
* Tasks
** TODO Decoy
*** TODO Decoy child
"
  "Prologue shared by the flat order fixtures: \"Tasks\" and a decoy entry.
The keyword-less \"Tasks\" parent is what makes an order task under it count as
top-level and so archivable.  The \"Decoy\" entry ahead of the order task is
what the fold probe and the drift tests aim at.  Hoisted so each fixture spells
out only the lines it exists to exercise.")

(defconst dotfiles--lib-test-order-file-org
  (concat dotfiles--lib-test-order-file-org-decoys "\
** TODO Iš teststore 2026-08-17 12345 užsakymo :@waitingfor:
SCHEDULED: <2026-08-15 Sat>
[[mu4e:msgid:old@example.com][Old]]
** TODO After
")
  "Org text for the store order-task tests.
Both `dotfiles--store-file-order-email' and `dotfiles--mu4e-complete-order-task'
build their fixture from it.  The planning line carries a stale earlier
estimate, so rescheduling to the delivery date is an observable rewrite rather
than a no-op.  The `@waitingfor' tag is how the task is found at all; the
other load-bearing properties come with
`dotfiles--lib-test-order-file-org-decoys'.")

(defconst dotfiles--lib-test-order-file-org-id-only
  (concat dotfiles--lib-test-order-file-org-decoys "\
** TODO Iš teststore 12345 užsakymo :@waitingfor:
[[mu4e:msgid:old@example.com][Old]]
** TODO After
")
  "Like `dotfiles--lib-test-order-file-org', with an ID-only order-task title.
That is the title a shipping notice creates before an order confirmation
supplies the order date.  It also carries no planning line, so the reschedule
is an insertion rather than a rewrite.")

(defconst dotfiles--lib-test-order-file-org-no-task
  (concat dotfiles--lib-test-order-file-org-decoys "\
** TODO After
")
  "Like `dotfiles--lib-test-order-file-org', with no order task to find.")

(defconst dotfiles--lib-test-order-file-org-no-tasks-heading "\
* Elsewhere
** TODO Decoy
"
  "Org text with neither a matching order task nor a \"Tasks\" heading.")

(defconst dotfiles--lib-test-order-file-org-nested-no-task "\
* Container
** Tasks
*** TODO Decoy
**** TODO Decoy child
*** TODO After
** Other
*** TODO Elsewhere
"
  "Like `dotfiles--lib-test-order-file-org-no-task', \"Tasks\" under a parent.
Folded, \"Tasks\" is then itself invisible -- the case that makes task creation
re-anchor to the nearest visible ancestor.  \"Other\" follows it so a task
created against that ancestor lands somewhere observably different.")

(defconst dotfiles--lib-test-order-file-org-sub-action
  (concat dotfiles--lib-test-order-file-org-decoys "\
** TODO Project
*** TODO Iš teststore 2026-08-17 12345 užsakymo :@waitingfor:
SCHEDULED: <2026-08-15 Sat>
[[mu4e:msgid:old@example.com][Old]]
** TODO After
")
  "Like `dotfiles--lib-test-order-file-org', order task under a project task.
Its parent carries a TODO keyword, so `dotfiles--org-task-top-level-p' is nil
and completing the order must not archive it.")

(defconst dotfiles--lib-test-order-file-org-blocked
  (concat dotfiles--lib-test-order-file-org-decoys "\
** TODO Iš teststore 2026-08-17 12345 užsakymo :@waitingfor:
*** TODO Unfinished child
** TODO After
")
  "Order fixture whose matching task is blocked by an unfinished child.")

(defconst dotfiles--lib-test-order-file-org-rival-tasks
  (concat dotfiles--lib-test-order-file-org-decoys "\
** TODO Iš kitastore 2026-08-17 12345 užsakymo :@waitingfor:
** TODO Iš TESTSTORE 2026-08-17 12345 užsakymo :@waitingfor:
** TODO Iš teststore 2026-08-17 123456 užsakymo :@waitingfor:
** TODO Iš teststore 2026-08-16 12345 užsakymo
** TODO Iš teststore 2026-08-17 12345 užsakymo :@waitingfor:
SCHEDULED: <2026-08-15 Sat>
[[mu4e:msgid:old@example.com][Old]]
** TODO After
")
  "Like `dotfiles--lib-test-order-file-org', four rivals ahead of the target.
Each defeats one axis of `dotfiles--store-find-order-task''s filter -- a
different store, the same store in a different case, an order ID carrying the
key as a substring, and a right-shaped title without the `@waitingfor' tag --
and each precedes the target, so a filter that lost that axis would stop on the
rival instead.  Without them the finder never has a choice to make: the search
would return the same entry with the whole predicate replaced by t.")

(defconst dotfiles--lib-test-order-file-org-identical-successor
  (concat dotfiles--lib-test-order-file-org-decoys "\
** TODO Iš teststore 2026-08-17 12345 užsakymo :@waitingfor:
First copy.
** TODO Iš teststore 2026-08-17 12345 užsakymo :@waitingfor:
Second copy.
** TODO After
")
  "Order fixture whose selected task has an identical-title successor.")

(defconst dotfiles--lib-test-order-file-org-identical-archive-successor
  (concat dotfiles--lib-test-order-file-org-decoys "\
** TODO Iš teststore 2026-08-17 12345 užsakymo :@waitingfor:
[[mu4e:msgid:new@example.com][New]]
** DONE Iš teststore 2026-08-17 12345 užsakymo :@waitingfor:
[[mu4e:msgid:new@example.com][New]]
** TODO After
")
  "Order fixture whose selected task becomes identical to its successor.")

(defconst dotfiles--lib-test-order-file-org-same-file-archive
  (concat dotfiles--lib-test-order-file-org-decoys "\
** TODO Iš teststore 2026-08-17 12345 užsakymo :@waitingfor:
:PROPERTIES:
:ARCHIVE: ::* Archive
:END:
[[mu4e:msgid:old@example.com][Old]]
** TODO After
")
  "Order fixture whose task archives into the source file.")

(defconst dotfiles--lib-test-order-file-org-date-key
  (concat dotfiles--lib-test-order-file-org-decoys "\
** TODO Iš teststore 2026-08-16 12345 užsakymo :@waitingfor:
** TODO Grąžinti teststore prekę 2026-08-17 :@waitingfor:
** TODO Iš teststore 2026-08-17 67890 užsakymo :@waitingfor:
SCHEDULED: <2026-08-15 Sat>
[[mu4e:msgid:old@example.com][Old]]
** TODO After
")
  "Like `dotfiles--lib-test-order-file-org', for a lookup keyed by order date.
The target carries an order ID no test searches for, so the date is the only
thing selecting it.  Its two rivals defeat the two axes an ID-keyed search
cannot reach: a right-shaped same-store order on a different date, and an
unrelated `@waitingfor' task naming the store and carrying the searched date as
a whole token.  The second is the \"unrelated dated task\" the finder's
title-shape anchor exists for, and the only rival in any fixture that the store
name alone does not already reject, so it is what pins that anchor.")

(defconst dotfiles--lib-test-order-file-org-linked
  (concat dotfiles--lib-test-order-file-org-decoys "\
** TODO Iš teststore 2026-08-17 12345 užsakymo :@waitingfor:
SCHEDULED: <2026-08-15 Sat>
[[mu4e:msgid:old@example.com][Old]]
[[mu4e:msgid:new@example.com][Already filed]]
** TODO After
")
  "Like `dotfiles--lib-test-order-file-org', MSG's link already under the task.
The planning line is kept, so the reschedule here is the same observable
rewrite as in the base.
The re-delivered-email state the entry points' idempotency guard exists for, and
the only order fixture that reaches it: the others carry `old@example.com'
alone, so the dedup scan never matches and the guard's no-op branch never runs.
The description differs from the \"New\" the `org-store-link' stub returns, so a
dedup narrowed from message-id to whole-link equality would append a second
copy -- which `dotfiles--lib-test-append-link-org-linked' cannot show, its two
links being identical.")

(defun dotfiles--lib-test-activate-order-file-region (file)
  "Visit FILE and leave an active region anchored at `point-min'.
Models the region a user can leave behind in the order file: `mark-active' is
buffer-local, so one set before they switched to mail survives while they read
it.  The mark goes at `point-min' rather than `point-max' because the region
runs between mark and point, and only the mark is under the test's control --
the code under test moves point forward to the task or to \"Tasks\" before every
region-sensitive step, so anchoring behind it keeps the region spanning the
decoy entries throughout the run.  `dotfiles--lib-test-with-order-org-file'
calls this inside its `transient-mark-mode' binding: batch Emacs starts that
nil, which leaves `use-region-p', and hence `org-region-active-p', nil however
the mark is set."
  (with-current-buffer (find-file-noselect file)
    (push-mark (point-min) t t)))

(defun dotfiles--lib-test-should-disable-region-loop (region)
  "Assert the region is live exactly when REGION but its Org loop is disabled.
REGION is the runner's nil-or-t flag.  Pin the raw and Org-visible region before
the loop policy so a failure distinguishes broken fixture setup from a missing
operation-local guard.  Assert `org-ignore-region' is restored too: unlike the
loop policy, that global state would affect unrelated Org commands in hooks."
  (should (eq (use-region-p) region))
  (should-not org-ignore-region)
  (should (eq (org-region-active-p) region))
  (should-not org-loop-over-headlines-in-active-region))

(defmacro dotfiles--lib-test-with-order-org-file (spec &rest body)
  "Run BODY over a temp `org' file, with the shared order fixture set up.
SPEC is (TEXT FOLDED REGION).  Bind `file' to a temp file holding TEXT and
`msgid' to the message-id BODY files, and afterwards delete the file and the
buffer visiting it.  Stub `mu4e-message-field', which the stub `mu4e' feature
leaves undefined, `org-store-link', which is real but cannot produce a `mu4e'
link without that package's link type, and `save-buffer', which delegates to
the real one after probing the region guard; BODY adds its own stubs in a
nested `cl-letf', so an unexpected prompt or archive call in a test that did
not ask for one still fails loudly.
With FOLDED non-nil, visit the file collapsed (`org-startup-folded' defaults to
`showeverything', so a fresh visit is not).  With REGION non-nil, leave
an active region spanning the decoy entries before BODY runs, with
`transient-mark-mode' bound so `use-region-p' -- and hence
`org-region-active-p' -- can see it."
  (declare (indent 1) (debug ((form form form) body)))
  (let ((text (nth 0 spec))
        (folded (nth 1 spec))
        (region (nth 2 spec))
        ;; Not a literal name: `file' and `msgid' are leaked into BODY
        ;; deliberately and documented, and a third such name is not.
        (body-error (gensym "body-error")))
    `(let ((file (make-temp-file "dotfiles-order" nil ".org" ,text))
           (msgid "new@example.com")
           (,body-error nil)
           ;; The weekday `org' writes into a SCHEDULED stamp comes from
           ;; `format-time-string' "%a", which follows the ambient LC_TIME; pin
           ;; it so the timestamp assertions do not depend on the invoking
           ;; environment.
           (system-time-locale "C")
           ;; `org' applies the fold when the file is visited, so this must
           ;; enclose every visit: the region helper's below when REGION is set,
           ;; and otherwise the entry point's own inside BODY.
           (org-startup-folded (if ,folded 'fold 'showeverything))
           (transient-mark-mode ,region))
       (unwind-protect
           (cl-letf (((symbol-function 'mu4e-message-field) #'plist-get)
                     ((symbol-function 'org-store-link)
                      (lambda (&rest _)
                        (concat "[[mu4e:msgid:" msgid "][New]]")))
                     ;; A yield, so the guard must be off here; see
                     ;; `dotfiles--with-store-order-task'.  Probed on the
                     ;; variable: by now `org-with-wide-buffer' has restored
                     ;; point, collapsing the region onto the mark.  Delegating
                     ;; to the real function is what leaves the runners'
                     ;; `buffer-modified-p' assertions with anything to check.
                     ((symbol-function 'save-buffer)
                     (let ((real (symbol-function 'save-buffer)))
                        (lambda (&rest args)
                          (when (or (equal buffer-file-name file)
                                    (equal buffer-file-name
                                           (concat file "_archive")))
                            (should-not org-ignore-region))
                          (apply real args)))))
             ;; `handler-bind' runs at signal time and lets the search continue
             ;; outward, so it records the error without catching it or moving
             ;; its backtrace; a `condition-case' that re-signalled would do
             ;; both.  It covers the region helper as well as BODY, that being
             ;; the other visit that can fail before BODY runs.
             (handler-bind ((error (lambda (err) (setq ,body-error err))))
               (when transient-mark-mode
                 (dotfiles--lib-test-activate-order-file-region file))
               ,@body))
         ;; The completion runner's ARCHIVE-BUFFER key visits an `_archive'
         ;; file beside FILE.  Discard it the same way, and whether or not the
         ;; run got as far as saving it.
         (let* ((archive (concat file "_archive"))
                (archive-buffer (find-buffer-visiting archive)))
           (when archive-buffer
             (with-current-buffer archive-buffer (set-buffer-modified-p nil))
             (kill-buffer archive-buffer))
           (when (file-exists-p archive) (delete-file archive)))
         (let ((buffer (find-buffer-visiting file)))
           ;; Discard whatever a failing BODY left unsaved before killing: the
           ;; file goes next anyway, and outside batch `kill-buffer' asks
           ;; before killing a modified file buffer, which would stall an
           ;; interactive `M-x ert' run on every such failure.
           (when buffer
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))
           (delete-file file)
           ;; Both runners re-visit FILE on their success paths, so this is
           ;; load-bearing only where they do not: the two `should-error' tests
           ;; whose form sits outside this fixture, which otherwise never check
           ;; that the entry point got as far as opening the file.  Kept
           ;; unconditional -- muting it while an error unwinds would disable it
           ;; in exactly those tests -- and carrying that error, since a signal
           ;; from a cleanup form replaces whatever was unwinding through it.
           ;; Last in the cleanup, so the kill and the delete still run.
           (unless buffer
             (error "BODY never visited %s%s" file
                    (if ,body-error
                        (format "; BODY signalled %S" ,body-error)
                      ""))))))))

(defun dotfiles--lib-test-goto-order-link (folded msgid)
  "Move point to the heading of the entry whose subtree holds MSGID's link.
The current buffer must be the visited order file.  Assert first that its fold
state matches FOLDED, probing the \"Decoy\" heading, which no order entry point
edits: both order helpers get their fold from `org-startup-folded', which `org'
applies on visiting and which two separate switches can suppress, so without
this the folded runs would silently degrade into duplicates of the unfolded ones
and still pass.
Then assert MSGID's link occurs exactly once: both entry points are idempotent,
and nothing in the returned plists can see a duplicate, which sits in the same
subtree as the original.
Finally assert the entry the link landed under carries FOLDED's fold state too
-- a distinct probe from the Decoy one, which pins only that the fixture is
folded at all.  This one pins that the automation did not reveal the heading it
touched: on the creation path the new task inherits the enclosing fold rather
than punching a hole in it, and on the found-task paths nothing along the
reschedule or the completion reveals that heading.
Return the heading, leaving point on it so callers can read the rest of the
entry."
  (dotfiles--lib-test-goto-line-matching "^\\*+ TODO Decoy$")
  (dotfiles--lib-test-should-be-folded folded)
  (goto-char (point-min))
  (should (search-forward msgid nil t))
  (save-excursion (should-not (search-forward msgid nil t)))
  (org-back-to-heading t)
  (dotfiles--lib-test-should-be-folded folded)
  (org-get-heading t t t t))

(cl-defun dotfiles--lib-test-order-file-run
    (&key (text dotfiles--lib-test-order-file-org)
          order-date
          (delivery-date "2026-08-18")
          drift-to folded region)
  "File an order link into a temp copy of TEXT and report where it landed.
ORDER-DATE and DELIVERY-DATE are passed through to
`dotfiles--store-file-order-email'.  The real `org-schedule' runs; DRIFT-TO,
when non-nil, is a regexp whose first matching line point is parked on right
after it, modelling point drifting out of the entry.  FOLDED and REGION are as
in `dotfiles--lib-test-with-order-org-file'.  Return a plist of the heading
the link ended up under, that entry's SCHEDULED timestamp, and the last
entry under \"Tasks\"."
  (dotfiles--lib-test-with-order-org-file (text folded region)
    (let ((real-schedule (symbol-function 'org-schedule)))
      (cl-letf (((symbol-function 'org-schedule)
                 (lambda (&rest args)
                   ;; Pins the operation-local region policy at this call,
                   ;; which no assertion on the returned plist can see -- a
                   ;; leaked loop reschedules the decoys, not the order task.
                   (dotfiles--lib-test-should-disable-region-loop region)
                   (apply real-schedule args)
                   (when drift-to
                     (dotfiles--lib-test-goto-line-matching drift-to)))))
        (dotfiles--store-file-order-email
         file "teststore" (list :message-id msgid) "12345" order-date
         delivery-date)
        (with-current-buffer (find-file-noselect file)
          ;; Every other assertion here reads the same live buffer the entry
          ;; point edited -- `dotfiles--get-org-buffer' reuses the visiting
          ;; buffer and the runner's own `find-file-noselect' hands it straight
          ;; back -- so nothing else can tell "filed" from "edited in memory",
          ;; and `save-buffer' could be deleted with the suite still green.
          ;; Asserted before the `org' calls below, which touch text properties.
          (should-not (buffer-modified-p))
          (list :heading (dotfiles--lib-test-goto-order-link folded msgid)
                :scheduled (org-entry-get nil "SCHEDULED")
                :last-task (progn
                             (goto-char (org-find-exact-headline-in-buffer
                                         "Tasks"))
                             (org-end-of-subtree t)
                             (org-back-to-heading t)
                             (org-get-heading t t t t))))))))

;; The finder is driven directly here, and with a date key: both entry points
;; pass an order ID, so this is the only coverage of the other key kind its
;; docstring documents.  Direct rather than through an entry point because
;; `dotfiles--mu4e-complete-order-task' documents KEY as an order ID, and a
;; file-backed fixture rather than `with-temp-buffer' because `org-map-entries'
;; with scope `file' resolves the scope from `buffer-file-name' and silently
;; scans nothing without one.
(ert-deftest dotfiles--store-find-order-task-by-date-test ()
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org-date-key nil nil)
    (with-current-buffer (find-file-noselect file)
      (let ((task (dotfiles--store-find-order-task "teststore" "2026-08-17")))
        (should (markerp task))
        (goto-char task)
        (should (equal (org-get-heading t t t t)
                       "Iš teststore 2026-08-17 67890 užsakymo"))))))

(defconst dotfiles--lib-test-order-filed-result
  `(:heading ,dotfiles--lib-test-order-task-heading
             :scheduled "<2026-08-18 Tue>"
             :last-task "After")
  "The `dotfiles--lib-test-order-file-run' result for an existing order task.
The task is rescheduled to the runner's default DELIVERY-DATE, as `org-schedule'
writes it, and \"After\" stays the last entry under \"Tasks\".  Shared for the
same reason as `dotfiles--lib-test-complete-order-done-result'.")

(defconst dotfiles--lib-test-order-created-result
  `(:heading ,dotfiles--lib-test-order-task-heading
             :scheduled "<2026-08-18 Tue>"
             :last-task ,dotfiles--lib-test-order-task-heading)
  "The `dotfiles--lib-test-order-file-run' result for a created order task.
As `dotfiles--lib-test-order-filed-result', except that the new task is itself
the last entry under \"Tasks\".")

(ert-deftest dotfiles--store-file-order-email-files-under-order-task-test ()
  (should (equal (dotfiles--lib-test-order-file-run)
                 dotfiles--lib-test-order-filed-result)))

(ert-deftest dotfiles--store-file-order-email-files-under-folded-task-test ()
  (should (equal (dotfiles--lib-test-order-file-run :folded t)
                 dotfiles--lib-test-order-filed-result)))

;; A contract test: the outcome must not depend on where point happens to be
;; after the reschedule.  `org-schedule' does not itself move point, so the stub
;; injects the drift; what this pins is the re-anchor before the link append.
;; See `dotfiles--with-store-order-task'.
(ert-deftest dotfiles--store-file-order-email-survives-point-drift-test ()
  (should (equal (dotfiles--lib-test-order-file-run
                  :drift-to "^\\*\\* TODO Decoy$")
                 dotfiles--lib-test-order-filed-result)))

(ert-deftest dotfiles--store-file-order-email-completes-id-only-title-test ()
  (should (equal (dotfiles--lib-test-order-file-run
                  :text dotfiles--lib-test-order-file-org-id-only
                  :order-date "2026-08-17")
                 dotfiles--lib-test-order-filed-result)))

;; Folded, because this is the only run that puts `org-edit-headline' inside a
;; fold and the fold is not announced in the test name.  Its `invisible-ok'
;; anchor is internal to `org', so unlike the other fold-sensitive calls here
;; the dependency cannot be shown at the call site -- only a test records it.
;; Unfolded coverage of the same call stays in the id-only-title test above.
(ert-deftest dotfiles--store-file-order-email-corrects-stale-order-date-test ()
  (should (equal (dotfiles--lib-test-order-file-run :order-date "2026-08-18"
                                                    :folded t)
                 '(:heading "Iš teststore 2026-08-18 12345 užsakymo"
                            :scheduled "<2026-08-18 Tue>"
                            :last-task "After"))))

(ert-deftest dotfiles--store-file-order-email-creates-missing-task-test ()
  (should (equal (dotfiles--lib-test-order-file-run
                  :text dotfiles--lib-test-order-file-org-no-task
                  :order-date "2026-08-17")
                 dotfiles--lib-test-order-created-result)))

;; The ID-only title is what a shipping notice creates before any order
;; confirmation supplies the date, and the created heading must still be
;; re-findable by order ID alone.
(ert-deftest dotfiles--store-file-order-email-creates-id-only-task-test ()
  (should (equal (dotfiles--lib-test-order-file-run
                  :text dotfiles--lib-test-order-file-org-no-task)
                 '(:heading "Iš teststore 12345 užsakymo"
                            :scheduled "<2026-08-18 Tue>"
                            :last-task "Iš teststore 12345 užsakymo"))))

;; A task created without a delivery date gets no planning line at all.
;; `:scheduled' nil pins the `when delivery-date' guard: without it
;; `org-schedule' would fall through to an interactive date prompt.
(ert-deftest dotfiles--store-file-order-email-creates-unscheduled-task-test ()
  (should (equal (dotfiles--lib-test-order-file-run
                  :text dotfiles--lib-test-order-file-org-no-task
                  :order-date "2026-08-17"
                  :delivery-date nil)
                 `(:heading ,dotfiles--lib-test-order-task-heading
                            :scheduled nil
                            :last-task
                            ,dotfiles--lib-test-order-task-heading))))

;; The end-to-end half of the `org-ignore-region' coverage, on the creation
;; fixture because it is the only one that runs region-sensitive `org' other
;; than `org-schedule': `org-do-demote' directly, and `org-todo' through the
;; real `org-autotask-insert-waiting-for-next-action'.  Without the guard the
;; whole-prefix region makes `org-do-demote' demote every heading before point
;; and `org-todo' put a keyword on each, including the keyword-less "Tasks"
;; that `:last-task' is read against.  The found-task fixtures cannot show this:
;; a leaked region reschedules the decoys, which no key of the plist reports --
;; hence the `should-not' probe inside the `org-schedule' stub.
(ert-deftest dotfiles--store-file-order-email-creates-task-with-region-test ()
  (should (equal (dotfiles--lib-test-order-file-run
                  :text dotfiles--lib-test-order-file-org-no-task
                  :order-date "2026-08-17"
                  :region t)
                 dotfiles--lib-test-order-created-result)))

;; Only `:last-task' catches the misfile: the link still lands on the correctly
;; titled task wherever it ended up, so `:heading' reads the same either way.
(ert-deftest dotfiles--store-file-order-email-creates-task-when-folded-test ()
  (should (equal (dotfiles--lib-test-order-file-run
                  :text dotfiles--lib-test-order-file-org-nested-no-task
                  :order-date "2026-08-17"
                  :folded t)
                 dotfiles--lib-test-order-created-result)))

;; The only case setting both hazards, and the one that pins the fixture-setup
;; ordering rule in `dotfiles--lib-test-with-order-org-file': the region helper
;; visits the file, so it must run inside the `org-startup-folded' binding,
;; that visit being the one the entry point reuses.  Hoisting it out leaves the
;; buffer unfolded, which only a folded-and-region run can see.  Built on the
;; creation fixture, the case carries the two hazards' real interaction rather
;; than only guarding the harness.
(ert-deftest dotfiles--store-file-order-email-creates-task-folded-region-test ()
  (should (equal (dotfiles--lib-test-order-file-run
                  :text dotfiles--lib-test-order-file-org-nested-no-task
                  :order-date "2026-08-17"
                  :folded t
                  :region t)
                 dotfiles--lib-test-order-created-result)))

(ert-deftest dotfiles--store-file-order-email-skips-already-filed-link-test ()
  (should (equal (dotfiles--lib-test-order-file-run
                  :text dotfiles--lib-test-order-file-org-linked)
                 dotfiles--lib-test-order-filed-result)))

(ert-deftest dotfiles--store-file-order-email-without-delivery-date-test ()
  (should (equal (dotfiles--lib-test-order-file-run :delivery-date nil)
                 `(:heading ,dotfiles--lib-test-order-task-heading
                            :scheduled "<2026-08-15 Sat>"
                            :last-task "After"))))

(ert-deftest dotfiles--store-file-order-email-without-tasks-heading-test ()
  (should-error (dotfiles--lib-test-order-file-run
                 :text dotfiles--lib-test-order-file-org-no-tasks-heading)
                :type 'user-error))

;; The finder needs ORDER-ID as one whitespace-delimited token of the heading,
;; so a task created for any other ID -- embedded or surrounding whitespace, or
;; nothing at all -- could never be re-found.  The rejection has to come before
;; any edit: the buffer outlives this call, and the next run's save would
;; persist whatever it left behind.  Driven through the fixture rather than the
;; runner so the test can reach the buffer.
(ert-deftest dotfiles--store-file-order-email-unfindable-order-id-test ()
  (dolist (order-id '("12 345" " 12345" ""))
    (dotfiles--lib-test-with-order-org-file
        (dotfiles--lib-test-order-file-org-no-task nil nil)
      (should (string-match-p
               "single whitespace-free token"
               (cadr (should-error
                      (dotfiles--store-file-order-email
                       file "teststore" (list :message-id msgid) order-id
                       nil nil)
                      :type 'user-error))))
      (with-current-buffer (find-buffer-visiting file)
        (should-not (buffer-modified-p))))))

;; ORDER-DATE is rejected before the org file is visited at all, so this test
;; visits it: the fixture's own visit assert is what the two `should-error'
;; tests outside it rely on, and muting it here would blunt it everywhere.
;; Both fixtures, because it is the found path -- where `org-edit-headline'
;; would splice the date into a live heading for the macro's save to persist --
;; that the check exists to keep out of the file.
(ert-deftest dotfiles--store-file-order-email-unfindable-order-date-test ()
  (dolist (text (list dotfiles--lib-test-order-file-org-id-only
                      dotfiles--lib-test-order-file-org-no-task))
    (dolist (order-date '("2026-08 17" " 2026-08-17" "2026-08-17\n" ""))
      (dotfiles--lib-test-with-order-org-file (text nil nil)
        (find-file-noselect file)
        (should (string-match-p
                 "single whitespace-free token"
                 (cadr (should-error
                        (dotfiles--store-file-order-email
                         file "teststore" (list :message-id msgid) "12345"
                         order-date nil)
                        :type 'user-error))))
        (with-current-buffer (find-buffer-visiting file)
          (should-not (buffer-modified-p)))))))

(cl-defun dotfiles--lib-test-complete-order-run
    (&key (text dotfiles--lib-test-order-file-org)
          (answer t)
          keyword
          drift-to drift-after-done folded region
          insert-before insert-at-task-end delete-task change-during-archive
          insert-during-archive mark-during-archive drift-before-copy
          drift-to-identical-subtree clobber-clip change-body-during-archive
          delete-during-archive
          foreign-archive nested-archive archive-buffer archive-initial-text
          retry-after-abort move-org-marker kill-archive-buffer)
  "Complete the order task in a temp copy of TEXT and report what happened.
ANSWER is what the confirmation prompt returns.  KEYWORD, when non-nil, is the
target TODO keyword handed to `dotfiles--mu4e-complete-order-task'; a terminal
keyword other than `org-autotask-keyword-done' bypasses
`org-autotask-complete-item', leaving `:completed' nil, so the transition's
aim is read from `:archived' and `:todo' instead.  DRIFT-TO, when non-nil, is
a regexp whose first matching line point is parked on while the prompt is
answered, modelling point drifting out of the entry.  DRIFT-AFTER-DONE is the
same regexp, applied instead from inside the `org-autotask-complete-item' stub
once it has recorded its heading -- the one key that reaches the re-anchor
guarding the archive; the real `org-todo' restores point, so the drift is
synthetic and pins the contract rather than today's behaviour.  Because only
that stub injects the drift, combining this key with a KEYWORD other than
`org-autotask-keyword-done' is rejected at entry.  FOLDED and
REGION are as in `dotfiles--lib-test-with-order-org-file'.  INSERT-BEFORE,
when non-nil, is a regexp before whose first matching line a heading is
inserted while the prompt is answered.  DELETE-TASK, when non-nil, deletes the
order task's whole subtree while the prompt is answered.  CHANGE-DURING-ARCHIVE,
when non-nil, rewrites the task's heading from inside `org-archive-subtree',
before its `org-archive-hook' runs -- the window between the archive copy and
the irreversible cut.  Both abort the run.  INSERT-DURING-ARCHIVE, when non-nil,
inserts a heading at the task's own beginning of line in that same window,
under `save-excursion' so the stub's point stays behind it as the real archive's
would; this run aborts.  MARK-DURING-ARCHIVE, when non-nil, activates a
region from inside the archive, modelling the user setting one during one of
the archive's own prompts.  DRIFT-BEFORE-COPY, when non-nil, moves point to
\"Decoy\" before the archive takes its copy, so the copy and the restored point
both leave the task as the real function's would; the run aborts.
DRIFT-TO-IDENTICAL-SUBTREE does the same after giving the decoy the target's
complete DONE headline but different body text; the run still aborts.
CLOBBER-CLIP,
when non-nil, replaces `org-subtree-clip' with an unrelated subtree after the
copy, modelling a `org-copy-subtree'/`org-refile' the user runs during one of
the archive's yields; fail closed because the copy can no longer be verified.
CHANGE-BODY-DURING-ARCHIVE inserts body text after the copy without changing
the heading; it must abort rather than cut content absent from the archive.
INSERT-AT-TASK-END appends a child at the task's pre-prompt structural boundary.
DELETE-DURING-ARCHIVE deletes the copied task before the source hook runs.
FOREIGN-ARCHIVE, when non-nil, runs `org-archive-hook' once in an
unrelated buffer first, modelling an `org-archive-subtree' the user starts
elsewhere during one of the archive's yields; that must be left alone, so this
run succeeds too.  NESTED-ARCHIVE does the same at the Decoy task in the source
buffer under a distinct archive-invocation token; the order guard must ignore
it without losing its protection over the outer invocation.  ARCHIVE-BUFFER,
when non-nil, has the stub paste the copy
into a buffer visiting an `_archive' file beside FILE and leave it unsaved,
modelling the half of the real function the suppressed save exposes.
ARCHIVE-INITIAL-TEXT is an unsaved edit already in that buffer.
RETRY-AFTER-ABORT repeats the completion after the first archive abort and
asserts that exactly one copy is eventually saved.  MOVE-ORG-MARKER simulates
the paste relocating `org-log-note-marker'; an abort must restore it to the
source before a retry.  KILL-ARCHIVE-BUFFER kills the destination after the
paste, as an earlier archive hook can, and must make the run abort.
Return a plist of the headings `org-autotask-complete-item' and
`org-archive-subtree' were called on -- either nil when that step did not run --
the heading whose subtree the mu4e link ended up under, and the TODO state the
task was left in.  With REGION, include every heading's TODO state too, so the
active-region case can detect collateral transitions.  With DELETE-TASK the run
aborts, so the plist carries only the first two keys.  Reporting the link target
matters because a link appended to the wrong subtree would otherwise leave
every completion test green, `task' being a marker the later re-anchors reach
regardless of a stray earlier insertion.  Reporting `:todo' matters because
`:completed' and `:archived' are the stubs' own account of where each step was
aimed, both taken at stub entry; `:linked' and `:todo' read the buffer back."
  (when (and drift-after-done keyword
             (not (equal keyword org-autotask-keyword-done)))
    (error "DRIFT-AFTER-DONE requires %s" org-autotask-keyword-done))
  (dotfiles--lib-test-with-order-org-file (text folded region)
    (let ((real-complete (symbol-function 'org-autotask-complete-item))
          (archive (concat file "_archive"))
          (archive-attempt 0)
          (org-log-note-marker
           (if move-org-marker (make-marker) org-log-note-marker))
          marker-source-position completed archived)
      (when archive-initial-text
        (with-current-buffer (find-file-noselect archive)
          (insert archive-initial-text)))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _)
                   ;; A yield, so the guard must be off; see
                   ;; `dotfiles--with-store-order-task'.  The raw region is
                   ;; pinned first, as
                   ;; `dotfiles--lib-test-should-disable-region-loop' does and
                   ;; for the same reason.
                   (should (eq (use-region-p) region))
                   (should (eq (org-region-active-p) region))
                   (when (and move-org-marker
                              (not (marker-buffer org-log-note-marker)))
                     (save-excursion
                       (dotfiles--lib-test-goto-line-matching
                        "^\\*\\* TODO Iš teststore ")
                       (forward-line 1)
                       (move-marker org-log-note-marker (point)
                                    (current-buffer))))
                   (when drift-to
                     (dotfiles--lib-test-goto-line-matching drift-to))
                   (when insert-before
                     (save-excursion
                       (dotfiles--lib-test-goto-line-matching insert-before)
                       (insert "** TODO Intruder\n")))
                   (when insert-at-task-end
                     (save-excursion
                       (dotfiles--lib-test-goto-line-matching
                        "^\\*\\* TODO Iš teststore ")
                       (org-end-of-subtree t t)
                       (insert "*** TODO Appended during prompt\n")))
                   (when delete-task
                     (save-excursion
                       (dotfiles--lib-test-goto-line-matching
                        "^\\*\\* TODO Iš teststore ")
                       (delete-region (point)
                                      (save-excursion
                                        (org-end-of-subtree t t)
                                        (point)))))
                   answer))
                ;; Wraps rather than replaces: the real `org-todo' is what the
                ;; suite exists to exercise, and only running it makes `:todo'
                ;; below an observation instead of a restatement.  Replacing is
                ;; reserved for what cannot run -- `y-or-n-p', which has no
                ;; answer in batch, and `org-archive-subtree', which writes and
                ;; saves an `_archive' file the fixture would not clean up.
                ;; Probing the policy here is still load-bearing: a leaked
                ;; region would derail the real call, and the plist cannot see
                ;; whether the global region state was hidden from hooks.
                ((symbol-function 'org-autotask-complete-item)
                 (lambda ()
                   (dotfiles--lib-test-should-disable-region-loop region)
                   (setq completed (org-get-heading t t t t))
                   (funcall real-complete)
                   ;; Last, so the injected drift is not undone by `org-todo',
                   ;; which restores point; and after the heading is recorded,
                   ;; so a failure names the re-anchor before the archive rather
                   ;; than this step.
                   (when drift-after-done
                     (dotfiles--lib-test-goto-line-matching
                      drift-after-done))))
                ;; Here the global guard must be off and the region gate shut
                ;; by policy; see `dotfiles--mu4e-complete-order-task'.  The raw
                ;; region is pinned against the requested state for the same
                ;; reason `dotfiles--lib-test-should-disable-region-loop' does
                ;; it.
                ;; Stubbed rather than delegated because the real one writes and
                ;; saves an `_archive' file the fixture would not clean up, but
                ;; it still runs `org-archive-hook': the real function runs it
                ;; with the subtree copied and not yet cut, which is the only
                ;; point at which the guard hanging there can still abort.
                ((symbol-function 'org-archive-subtree)
                 (lambda (&rest _)
                   (cl-incf archive-attempt)
                   (should-not org-ignore-region)
                   (should-not org-loop-over-headlines-in-active-region)
                   ;; The entry point suppresses the archive's own save and
                   ;; performs it afterwards; see
                   ;; `dotfiles--mu4e-complete-order-task'.
                   (should-not org-archive-subtree-save-file-p)
                   (should (eq (use-region-p) region))
                   ;; Outside the `save-excursion' below, so the stub's point
                   ;; leaves the task too: the real function takes its copy and
                   ;; the point it restores from one anchor, so a drift before
                   ;; the copy moves both, and a guard reading only one of them
                   ;; would not see this.
                   (when (or drift-before-copy drift-to-identical-subtree)
                     (dotfiles--lib-test-goto-line-matching
                      "^\\*\\* TODO Decoy$")
                     (when drift-to-identical-subtree
                       (delete-region (line-beginning-position)
                                      (line-end-position))
                       (insert "** DONE Iš teststore 2026-08-17 12345 užsakymo :@waitingfor:")))
                   ;; Both read at entry, as `:completed' is and for the same
                   ;; reason: this is what the real `org-archive-subtree'
                   ;; copies, and the only reading the guard's own `goto-char'
                   ;; cannot manufacture.  The clip is what the guard compares
                   ;; the cut anchor against, so a stub that left it empty
                   ;; would abort every run here.
                   (let* ((copied
                           (save-excursion
                             (org-back-to-heading t)
                             (cons (org-get-heading t t t t)
                                   (buffer-substring-no-properties
                                    (point)
                                    (progn (org-end-of-subtree t t)
                                           (point))))))
                          (aimed (car copied))
                          (org-subtree-clip
                           (if (and clobber-clip
                                    (or (not retry-after-abort)
                                        (= archive-attempt 1)))
                               (concat
                                (substring (cdr copied) 0
                                           (string-match "\n" (cdr copied)))
                                "\nDifferent archived body.\n")
                             (cdr copied))))
                     (let ((source-start
                            (save-excursion
                              (org-back-to-heading t)
                              (point))))
                       (when move-org-marker
                         (setq marker-source-position
                               (marker-position org-log-note-marker)))
                       (with-current-buffer (find-file-noselect archive)
                         (unless (derived-mode-p 'org-mode) (org-mode))
                         (goto-char (point-max))
                         (let ((archive-start (point)))
                           (insert (cdr copied))
                           (goto-char archive-start)
                           (when move-org-marker
                             (move-marker
                              org-log-note-marker
                              (+ archive-start
                                 (- marker-source-position source-start))
                              (current-buffer)))
                           (run-hooks 'org-archive-finalize-hook))))
                     (when change-during-archive
                       (save-excursion
                         ;; DONE, not TODO: the transition has already run by
                         ;; the time the archive does, which is what makes this
                         ;; the later of the two windows.
                         (dotfiles--lib-test-goto-line-matching
                          "^\\*\\* DONE Iš teststore ")
                         (org-edit-headline
                          "Iš teststore 2026-08-19 99999 užsakymo")))
                     (when change-body-during-archive
                       (save-excursion
                         (dotfiles--lib-test-goto-line-matching
                          "^\\*\\* DONE Iš teststore ")
                         (forward-line 1)
                         (insert "Changed after the archive copy.\n")))
                     (when delete-during-archive
                       (save-excursion
                         (dotfiles--lib-test-goto-line-matching
                          "^\\*\\* DONE Iš teststore ")
                         (delete-region
                          (point)
                          (progn (org-end-of-subtree t t) (point)))))
                     (when insert-during-archive
                       (save-excursion
                         (dotfiles--lib-test-goto-line-matching
                          "^\\*\\* DONE Iš teststore ")
                         (insert "** TODO Intruder\n")))
                     (when foreign-archive
                       ;; `org-archive-hook' is global, so the user's own
                       ;; archive, started in another buffer while this one
                       ;; waits at a prompt, runs the guard too -- on a marker
                       ;; that does not address their buffer at all.
                       (with-temp-buffer
                         (insert "* Elsewhere\n** TODO Another task\n")
                         (org-mode)
                         (dotfiles--lib-test-goto-line-matching
                          "^\\*\\* TODO Another task$")
                         (let ((dotfiles--org-archive-invocation-token
                                (make-symbol "foreign-archive")))
                           (run-hooks 'org-archive-hook))))
                     (when nested-archive
                       (save-excursion
                         (dotfiles--lib-test-goto-line-matching
                          "^\\*\\* TODO Decoy$")
                         (let ((dotfiles--org-archive-invocation-token
                                (make-symbol "nested-archive"))
                               (org-subtree-clip
                                "** TODO Decoy\n*** TODO Decoy child\n"))
                           (run-hooks 'org-archive-hook))))
                     (when kill-archive-buffer
                       (let ((buffer (find-buffer-visiting archive)))
                         (should buffer)
                         (with-current-buffer buffer
                           (set-buffer-modified-p nil))
                         (kill-buffer buffer)))
                     (run-hooks 'org-archive-hook)
                     ;; `org-cut-subtree' takes `org-back-to-heading' from
                     ;; point, so this is the subtree the real function would
                     ;; delete.  The plist cannot carry it -- `:archived' is
                     ;; the aim, read before any mid-archive edit -- and it is
                     ;; what the guard's `goto-char' re-establishes.
                     (should (equal (org-get-heading t t t t) aimed))
                     (when mark-during-archive
                       (push-mark (point-min) t t))
                     (let (this-command)
                       (org-cut-subtree))
                     ;; Published after the hook, so an abort still reports nil,
                     ;; as a real aborted cut would.
                     (setq archived aimed)))))
        (cl-flet ((complete ()
                    (dotfiles--mu4e-complete-order-task
                     file "teststore" (list :message-id msgid) "12345"
                     keyword))
                  (success-result ()
                    (when (or archive-buffer archive-initial-text
                              move-org-marker)
                      (let ((buffer (find-buffer-visiting archive)))
                        (should buffer)
                        (with-current-buffer buffer
                          (should-not (buffer-modified-p)))
                        (should (file-exists-p archive))))
                    (when mark-during-archive
                      (with-current-buffer (find-buffer-visiting file)
                        (should mark-active)))
                    (with-current-buffer
                        (find-file-noselect (if archived archive file))
                      (should-not (buffer-modified-p))
                      (let* ((linked
                              (if (not archived)
                                  (dotfiles--lib-test-goto-order-link
                                   folded msgid)
                                (goto-char (point-min))
                                (search-forward msgid)
                                (org-back-to-heading t)
                                (org-get-heading t t t t)))
                             (result
                              (list :completed completed
                                    :archived archived
                                    :linked linked
                                    :todo (org-get-todo-state))))
                        (if (not region)
                            result
                          (let (todo-states)
                            (with-current-buffer (find-file-noselect file)
                              (org-map-entries
                               (lambda ()
                                 (push (list (org-get-heading t t t t)
                                             (org-get-todo-state))
                                       todo-states))
                               nil 'file))
                            (append result
                                    (list :todo-states
                                          (nreverse todo-states)))))))))
          (if (or insert-at-task-end delete-task change-during-archive
                  insert-during-archive delete-during-archive
                  drift-before-copy drift-to-identical-subtree clobber-clip
                  change-body-during-archive kill-archive-buffer)
              (progn
                ;; The abort is what this run tests, so it is asserted here
                ;; rather than at the test site: letting the `user-error' out
                ;; of the runner discards COMPLETED and ARCHIVED, and a
                ;; `should-error' around the whole run then cannot tell
                ;; aborting before the DONE transition and the irreversible
                ;; archive from aborting after both of them have run.
                (should-error (complete) :type 'user-error)
                ;; No `:linked': under DELETE-TASK the deletion took the subtree
                ;; the link was appended to, and neither abort has anything
                ;; further to report.  The abort skips the save, leaving the
                ;; edits unsaved; nothing else can tell -- the
                ;; `save-buffer' stub records no call and the fixture deletes
                ;; the file unread -- so pin it here.  The fixture clears the
                ;; flag itself before killing the buffer.
                (with-current-buffer (find-buffer-visiting file)
                  (should (buffer-modified-p)))
                (when (or archive-buffer archive-initial-text
                          move-org-marker kill-archive-buffer)
                  (if archive-initial-text
                      (let ((buffer (find-buffer-visiting archive)))
                        (should buffer)
                        (with-current-buffer buffer
                          (should (equal (buffer-string)
                                         archive-initial-text))
                          (should (buffer-modified-p))))
                    (should-not (find-buffer-visiting archive)))
                  (should-not (file-exists-p archive)))
                (when move-org-marker
                  (should (eq (marker-buffer org-log-note-marker)
                              (find-buffer-visiting file)))
                  (should (= (marker-position org-log-note-marker)
                             marker-source-position)))
                (if retry-after-abort
                    (progn
                      (complete)
                      (with-current-buffer (find-buffer-visiting archive)
                        (goto-char (point-min))
                        (should (= (how-many
                                    "^\\*\\* DONE Iš teststore .*12345")
                                   1))
                        (should (string-prefix-p archive-initial-text
                                                 (buffer-string))))
                      (when move-org-marker
                        (should (eq (marker-buffer org-log-note-marker)
                                    (find-buffer-visiting archive))))
                      (success-result))
                  (if delete-task
                      (list
                       :completed completed :archived archived
                       :remaining-orders
                       (with-current-buffer (find-buffer-visiting file)
                         (let (orders)
                           (org-map-entries
                            (lambda ()
                              (push (list (org-get-heading t t t t)
                                          (org-get-todo-state))
                                    orders))
                            (concat "+" (org-autotask-list-tag
                                         org-autotask-waitingfor))
                            'file)
                           (nreverse orders))))
                    (list :completed completed :archived archived))))
            (complete)
            (success-result)))))))

(define-error 'dotfiles--lib-test-archive-save-error
  "Injected archive-save failure")

(define-error 'dotfiles--lib-test-source-save-error
  "Injected source-save failure")

(defun dotfiles--lib-test-paste-archive-copy
    (archive task invocation &optional widen)
  "Paste TASK into ARCHIVE and run finalize hooks for INVOCATION.
With WIDEN non-nil, paste outside any existing restriction.  Return its start."
  (let ((copy (dotfiles--store-order-subtree task)))
    (with-current-buffer (find-file-noselect archive)
      (unless (derived-mode-p 'org-mode) (org-mode))
      (save-restriction
        (when widen (widen))
        (goto-char (point-max))
        (let ((start (point-marker)))
          (insert copy)
          (goto-char start)
          (let ((dotfiles--org-archive-invocation-token invocation))
            (run-hooks 'org-archive-finalize-hook))
          start)))))

(defun dotfiles--lib-test-cut-archive-source (task invocation)
  "Run source archive hooks for INVOCATION, then cut TASK as Org does."
  (goto-char task)
  (let ((dotfiles--org-archive-invocation-token invocation))
    (run-hooks 'org-archive-hook))
  (goto-char task)
  (let (this-command)
    (org-cut-subtree)))

(defun dotfiles--lib-test-copy-and-cut-archive
    (archive task invocation kill-destination)
  "Paste TASK into ARCHIVE, cut it, then optionally KILL-DESTINATION."
  (dotfiles--lib-test-paste-archive-copy archive task invocation)
  (dotfiles--lib-test-cut-archive-source task invocation)
  (when kill-destination
    (let ((buffer (find-buffer-visiting archive)))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(defun dotfiles--lib-test-run-nested-cut-archive
    (archive nested-buffer nested-task nested-invocation)
  "Archive NESTED-TASK from NESTED-BUFFER into ARCHIVE."
  (with-current-buffer nested-buffer
    (dotfiles--store-archive-transaction
     archive nested-task nested-invocation
     (lambda ()
       (dotfiles--lib-test-copy-and-cut-archive
       archive nested-task nested-invocation nil)))))

(defun dotfiles--lib-test-make-nested-archive-hook
    (archive nested-buffer nested-task nested-invocation)
  "Return a one-shot finalize hook that archives NESTED-TASK."
  (let (nested-running)
    (lambda ()
      (unless nested-running
        (setq nested-running t)
        (dotfiles--lib-test-run-nested-cut-archive
         archive nested-buffer nested-task nested-invocation)))))

(defmacro dotfiles--lib-test-with-archive-transaction
    (archive-text &rest body)
  "Run BODY with a source task and archive buffer containing ARCHIVE-TEXT.
Bind `source-buffer', `archive', `archive-initial', `invocation' and `task'."
  (declare (indent 1) (debug (form body)))
  `(dotfiles--lib-test-with-order-org-file
       (dotfiles--lib-test-order-file-org nil nil)
     (let* ((source-buffer (find-file-noselect file))
            (archive (concat file "_archive"))
            (archive-initial ,archive-text)
            (invocation (make-symbol "archive-transaction-test"))
            (task (with-current-buffer source-buffer
                    (dotfiles--store-find-order-task "teststore" "12345"))))
       (when archive-initial
         (with-current-buffer (find-file-noselect archive)
           (insert archive-initial)))
       (unwind-protect
           (progn ,@body)
         (set-marker task nil)))))

(defmacro dotfiles--lib-test-with-nested-order-source (&rest body)
  "Run BODY with a distinct nested order source and task."
  (declare (indent 0) (debug body))
  `(let* ((nested-text
           (replace-regexp-in-string
            "old@example.com" "nested@example.com"
            dotfiles--lib-test-order-file-org t t))
          (nested-file
           (make-temp-file "dotfiles-nested-source" nil ".org" nested-text))
          (nested-buffer (find-file-noselect nested-file))
          (nested-invocation (make-symbol "nested-archive-transaction"))
          (nested-task
           (with-current-buffer nested-buffer
             (org-mode)
             (dotfiles--store-find-order-task "teststore" "12345"))))
     (unwind-protect
         (progn ,@body)
       (set-marker nested-task nil)
       (with-current-buffer nested-buffer (set-buffer-modified-p nil))
       (kill-buffer nested-buffer)
       (delete-file nested-file))))

(defun dotfiles--lib-test-run-failing-archive-transaction
    (source-buffer archive task invocation archive-call expected-error)
  "Run a failing archive transaction and assert EXPECTED-ERROR."
  (with-current-buffer source-buffer
    (let ((error (should-error
                  (dotfiles--store-archive-transaction
                   archive task invocation archive-call)
                  :type 'error)))
      (should (eq (car error) expected-error))
      error)))

(defun dotfiles--lib-test-run-failing-cut-transaction
    (source-buffer archive task invocation expected-error)
  "Copy and cut TASK transactionally, then assert EXPECTED-ERROR."
  (dotfiles--lib-test-run-failing-archive-transaction
      source-buffer archive task invocation
   (lambda ()
     (dotfiles--lib-test-copy-and-cut-archive archive task invocation nil))
   expected-error))

(defun dotfiles--lib-test-run-successful-cut-transaction
    (source-buffer archive task invocation)
  "Copy and cut TASK transactionally without injecting a failure."
  (with-current-buffer source-buffer
    (dotfiles--store-archive-transaction
     archive task invocation
     (lambda ()
       (dotfiles--lib-test-copy-and-cut-archive
        archive task invocation nil)))))

(defun dotfiles--lib-test-run-failing-paste-transaction
    (source-buffer archive task invocation expected-error &optional after-paste)
  "Paste TASK transactionally, run AFTER-PASTE, then assert EXPECTED-ERROR."
  (dotfiles--lib-test-run-failing-archive-transaction
   source-buffer archive task invocation
   (lambda ()
     (dotfiles--lib-test-paste-archive-copy archive task invocation)
     (when after-paste (funcall after-paste)))
   expected-error))

(defun dotfiles--lib-test-run-double-source-save-failure
    (source-buffer archive task invocation)
  "Run a cut transaction whose two source saves fail."
  (let ((real-save (symbol-function 'save-buffer))
        (source-save-count 0))
    (cl-letf (((symbol-function 'save-buffer)
               (lambda (&rest args)
                 (if (and (eq (current-buffer) source-buffer)
                          (<= (cl-incf source-save-count) 2))
                     (signal 'dotfiles--lib-test-source-save-error nil)
                   (apply real-save args)))))
      (dotfiles--lib-test-run-failing-cut-transaction
      source-buffer archive task invocation 'error))
    (should (= source-save-count 2))))

(defun dotfiles--lib-test-run-source-save-failure-with-archive-edit
    (source-buffer archive task invocation archive-edit)
  "Run ARCHIVE-EDIT during the first failing source save."
  (let ((real-save (symbol-function 'save-buffer))
        source-save-failed)
    (cl-letf (((symbol-function 'save-buffer)
               (lambda (&rest args)
                 (if (and (eq (current-buffer) source-buffer)
                          (not source-save-failed))
                     (progn
                       (setq source-save-failed t)
                       (with-current-buffer (find-buffer-visiting archive)
                         (funcall archive-edit))
                       (signal 'dotfiles--lib-test-source-save-error nil))
                   (apply real-save args)))))
      (dotfiles--lib-test-run-failing-cut-transaction
       source-buffer archive task invocation
       'dotfiles--lib-test-source-save-error))))

(defun dotfiles--lib-test-make-first-source-save-fail
    (source-buffer real-save save-count)
  "Return a save stub that fails SOURCE-BUFFER's first save.
Increment the integer in SAVE-COUNT for every source save."
  (lambda (&rest args)
    (if (eq (current-buffer) source-buffer)
        (progn
          (setcar save-count (1+ (car save-count)))
          (if (= (car save-count) 1)
              (signal 'dotfiles--lib-test-source-save-error nil)
            (apply real-save args)))
      (apply real-save args))))

(defun dotfiles--lib-test-add-once-source-save-edit
    (source-buffer hook edit)
  "Add buffer-local HOOK to insert EDIT once in SOURCE-BUFFER."
  (let (edited)
    (with-current-buffer source-buffer
      (add-hook
       hook
       (lambda ()
         (unless edited
           (setq edited t)
           (goto-char (point-min))
           (insert edit)))
       nil t))))

(defun dotfiles--lib-test-should-preserve-independent-finalize-save
    (source-buffer archive task invocation archive-initial)
  "Assert an outside finalize-hook edit survives a later abort."
  (let* ((hook-edit "Saved finalize-hook edit.\n")
         (expected (concat hook-edit archive-initial))
         (org-archive-finalize-hook
          (list
           (lambda ()
             (goto-char (point-min))
             (insert hook-edit)
             (save-buffer)))))
    (dotfiles--lib-test-run-failing-paste-transaction
     source-buffer archive task invocation 'user-error)
    (with-current-buffer (find-buffer-visiting archive)
      (should (equal (buffer-string) expected))
      (should-not (buffer-modified-p)))
    (with-temp-buffer
      (insert-file-contents archive)
      (should (equal (buffer-string) expected)))))

(defun dotfiles--lib-test-should-preserve-unsaved-archive-edit
    (archive edit initial)
  "Assert ARCHIVE retains unsaved EDIT before INITIAL."
  (with-current-buffer (find-buffer-visiting archive)
    (should (equal (buffer-string) (concat edit initial)))
    (should (buffer-modified-p))))

(defun dotfiles--lib-test-should-retain-edit-after-archive-file-removal
    (archive edit)
  "Assert ARCHIVE's file is gone while its live buffer retains EDIT."
  (should-not (file-exists-p archive))
  (with-current-buffer (find-buffer-visiting archive)
    (should (equal (buffer-string) edit))
    (should (buffer-modified-p))))

(defun dotfiles--lib-test-should-track-finalize-hook
    (source-buffer archive task invocation archive-initial hook)
  "Assert transaction rollback includes changes made by finalize HOOK."
  (let ((org-archive-finalize-hook hook))
    (dotfiles--lib-test-run-failing-paste-transaction
     source-buffer archive task invocation 'user-error
     (lambda () (user-error "abort after finalization")))
    (with-current-buffer (find-buffer-visiting archive)
      (should (equal (buffer-string) archive-initial)))))

(defun dotfiles--lib-test-should-have-archive-texts
    (archive live-text disk-text modified)
  "Assert ARCHIVE has LIVE-TEXT, DISK-TEXT and MODIFIED state."
  (with-current-buffer (find-buffer-visiting archive)
    (should (equal (buffer-string) live-text))
    (should (eq (buffer-modified-p) modified)))
  (with-temp-buffer
    (insert-file-contents archive)
    (should (equal (buffer-string) disk-text))))

(defun dotfiles--lib-test-prepare-dirty-archive (archive disk-text edit)
  "Save DISK-TEXT to ARCHIVE, append unsaved EDIT, and return its live text."
  (with-current-buffer (find-file-noselect archive)
    (insert disk-text)
    (save-buffer)
    (goto-char (point-max))
    (insert edit)
    (buffer-string)))

(defun dotfiles--lib-test-make-write-then-signal-hook (archive)
  "Return a finalize hook that makes ARCHIVE's next save write then fail."
  (lambda ()
    (add-hook
     'write-contents-functions
     (lambda ()
       (dotfiles--store-write-file-text
        archive (buffer-string) buffer-file-coding-system)
       (signal 'dotfiles--lib-test-archive-save-error nil))
     nil t)))

(defun dotfiles--lib-test-should-restore-live-transaction-state
    (source-buffer source-initial archive archive-initial)
  "Assert SOURCE-BUFFER and ARCHIVE retain their initial live state."
  (with-current-buffer source-buffer
    (should (equal (buffer-string) source-initial)))
  (with-current-buffer (find-buffer-visiting archive)
    (should (equal (buffer-string) archive-initial))
    (should (buffer-modified-p))))

(defun dotfiles--lib-test-should-source-be-durable (source-buffer expected)
  "Assert SOURCE-BUFFER and its file both contain EXPECTED."
  (with-current-buffer source-buffer
    (should (equal (buffer-string) expected))
    (should-not (buffer-modified-p)))
  (with-temp-buffer
    (insert-file-contents (buffer-file-name source-buffer))
    (should (equal (buffer-string) expected))))

(defun dotfiles--lib-test-should-fully-rollback
    (source-buffer expected archive)
  "Assert SOURCE-BUFFER is durable and ARCHIVE was removed."
  (dotfiles--lib-test-should-source-be-durable source-buffer expected)
  (should-not (file-exists-p archive))
  (should-not (find-buffer-visiting archive)))

(defun dotfiles--lib-test-run-invalid-archive-save
    (source-buffer archive task invocation install-save-hook expected-error)
  "Run a cut after INSTALL-SAVE-HOOK makes archive persistence invalid."
  (let ((source-initial (with-current-buffer source-buffer (buffer-string)))
        (org-archive-finalize-hook (list install-save-hook)))
    (dotfiles--lib-test-run-failing-cut-transaction
     source-buffer archive task invocation expected-error)
    (dotfiles--lib-test-should-source-be-durable
     source-buffer source-initial)))

(defun dotfiles--lib-test-delete-archived-order ()
  "Delete the test order subtree from the current archive buffer."
  (dotfiles--lib-test-goto-line-matching
   "Iš teststore .*12345 užsakymo")
  (org-back-to-heading t)
  (delete-region (point) (progn (org-end-of-subtree t t) (point))))

(defun dotfiles--lib-test-should-archive-contain-one-order (archive)
  "Assert ARCHIVE contains one persisted test order."
  (with-current-buffer (find-buffer-visiting archive)
    (goto-char (point-min))
    (should (= (how-many "Iš teststore .*12345 užsakymo") 1))
    (should-not (buffer-modified-p)))
  (with-temp-buffer
    (insert-file-contents archive)
    (should (= (how-many "Iš teststore .*12345 užsakymo") 1))))

(defun dotfiles--lib-test-one-shot-save-error
    (real-save buffer-p error-symbol)
  "Return a `save-buffer' stub that signals ERROR-SYMBOL once when BUFFER-P."
  (let (failed)
    (lambda (&rest args)
      (if (and (not failed) (funcall buffer-p))
          (progn
            (setq failed t)
            (signal error-symbol nil))
        (apply real-save args)))))

(defmacro dotfiles--lib-test-with-one-shot-archive-save-error
    (archive &rest body)
  "Run BODY with the next save of ARCHIVE signalling a test error."
  (declare (indent 1) (debug (form body)))
  `(let* ((real-save (symbol-function 'save-buffer))
          (failing-save
           (dotfiles--lib-test-one-shot-save-error
            real-save (lambda () (equal buffer-file-name ,archive))
            'dotfiles--lib-test-archive-save-error)))
     (cl-letf (((symbol-function 'save-buffer) failing-save))
       ,@body)))

(defun dotfiles--lib-test-complete-with-effect
    (file msgid effect archive-transaction)
  "Complete FILE's order, run EFFECT after DONE, using ARCHIVE-TRANSACTION."
  (let ((real-complete (symbol-function 'org-autotask-complete-item)))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'org-autotask-complete-item)
               (lambda ()
                 (funcall real-complete)
                 (funcall effect)))
              ((symbol-function 'dotfiles--store-archive-transaction)
               archive-transaction))
      (dotfiles--mu4e-complete-order-task
       file "teststore" (list :message-id msgid) "12345"))))

(defun dotfiles--lib-test-should-order-be-done (file)
  "Assert FILE's test order is DONE."
  (with-current-buffer (find-buffer-visiting file)
    (dotfiles--lib-test-goto-line-matching "^\\*\\* DONE Iš teststore ")
    (should (equal (org-get-todo-state) "DONE"))))

(defun dotfiles--lib-test-should-reject-order-archive
    (file msgid initial-text)
  "Assert FILE's archive is rejected before prompting or mutation."
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (&rest _)
               (ert-fail "invalid archive reached the prompt"))))
    (should-error
     (dotfiles--mu4e-complete-order-task
      file "teststore" (list :message-id msgid) "12345")
     :type 'user-error))
  (with-current-buffer (find-buffer-visiting file)
    (should (equal (buffer-string) initial-text))
    (should-not (buffer-modified-p))))

(defun dotfiles--lib-test-set-order-archive (file archive)
  "Set FILE's test order archive to ARCHIVE and return its saved text."
  (with-current-buffer (find-file-noselect file)
    (dotfiles--lib-test-goto-line-matching "^\\*\\* TODO Iš teststore ")
    (org-entry-put nil "ARCHIVE" (concat archive "::"))
    (save-buffer)
    (buffer-string)))

(defun dotfiles--lib-test-archive-redirect-observed-p (after-completion)
  "Return whether an archive redirect set AFTER-COMPLETION is observed."
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org nil nil)
    (let ((redirected (concat file ".redirected"))
          observed-archive)
      (if after-completion
          (dotfiles--lib-test-complete-with-effect
           file msgid
           (lambda ()
             (org-back-to-heading t)
             (org-up-heading-safe)
             (org-entry-put nil "ARCHIVE" (concat redirected "::")))
           (lambda (archive-file &rest _)
             (setq observed-archive archive-file)))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (dotfiles--lib-test-goto-line-matching
                      "^\\*\\* TODO Iš teststore ")
                     (org-up-heading-safe)
                     (org-entry-put nil "ARCHIVE" (concat redirected "::"))
                     t))
                  ((symbol-function 'dotfiles--store-archive-transaction)
                   (lambda (archive-file &rest _)
                     (setq observed-archive archive-file))))
          (dotfiles--mu4e-complete-order-task
           file "teststore" (list :message-id msgid) "12345")))
      (equal (file-truename observed-archive) (file-truename redirected)))))

;; Every archive fixture pastes at the destination's end, so the integration
;; suite only ever produces append-shaped changes; the mismatch branches of
;; the `compare-strings' arithmetic are pinned here directly.
(ert-deftest dotfiles--store-record-text-change-bounds-hunks-test ()
  (cl-flet ((record (before after)
              (with-temp-buffer
                (insert after)
                (let ((change (dotfiles--store-record-text-change before nil)))
                  (should-not (marker-insertion-type (nth 0 change)))
                  (should (eq (marker-insertion-type (nth 1 change)) t))
                  (list (marker-position (nth 0 change))
                        (marker-position (nth 1 change))
                        (nth 2 change)
                        (nth 3 change))))))
    ;; Middle replacement with differing hunk lengths.
    (should (equal (record "abQcd" "abXYcd") '(3 5 "Q" "XY")))
    ;; Change at index 0: zero prefix, nonzero suffix.
    (should (equal (record "Qcd" "XYcd") '(1 3 "Q" "XY")))
    ;; Pure append.
    (should (equal (record "ab" "abcd") '(3 5 "" "cd")))
    ;; Pure truncation.
    (should (equal (record "abcd" "ab") '(3 3 "cd" "")))
    ;; Empty before, as when archiving to a fresh archive file.
    (should (equal (record "" "x") '(1 2 "" "x")))
    ;; Empty after: whole-content deletion.
    (should (equal (record "x" "") '(1 1 "x" "")))
    ;; Identical snapshots record nothing.
    (with-temp-buffer
      (insert "ab")
      (should-not (dotfiles--store-record-text-change "ab" nil)))
    ;; Ambiguous alignment: the shared character binds to the prefix first.
    (should (equal (record "aa" "aba") '(2 3 "" "b")))
    ;; Multibyte: positions and hunks count characters, not bytes.
    (should (equal (record "Iš ųžsakymo" "Iš užsakymo") '(4 5 "ų" "u")))))

(ert-deftest dotfiles--store-archive-transaction-restores-cut-on-save-error-test
    ()
  (dotfiles--lib-test-with-archive-transaction
      "* Existing\nUnsaved note.\n"
    (let ((source-initial
           (with-current-buffer source-buffer
             (dotfiles--store-buffer-text))))
      (dotfiles--lib-test-with-one-shot-archive-save-error archive
        (dotfiles--lib-test-run-failing-cut-transaction
         source-buffer archive task invocation
         'dotfiles--lib-test-archive-save-error))
      (dotfiles--lib-test-should-restore-live-transaction-state
       source-buffer source-initial archive archive-initial))))

(ert-deftest dotfiles--store-archive-transaction-restores-cut-after-kill-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((source-initial (with-current-buffer source-buffer (buffer-string))))
      (dotfiles--lib-test-run-failing-archive-transaction
       source-buffer archive task invocation
       (lambda ()
         (dotfiles--lib-test-copy-and-cut-archive archive task invocation t))
       'user-error)
      (with-current-buffer source-buffer
        (should (equal (buffer-string) source-initial)))
      (should-not (find-buffer-visiting archive))
      (should-not (file-exists-p archive)))))

(ert-deftest
    dotfiles--store-archive-transaction-preserves-unrelated-destination-edit-test
    ()
  (dotfiles--lib-test-with-archive-transaction
      "* Existing\nUnsaved note.\n"
    (let ((user-edit "User edit during archive.\n"))
      (dotfiles--lib-test-run-failing-archive-transaction
       source-buffer archive task invocation
       (lambda ()
         (dotfiles--lib-test-paste-archive-copy archive task invocation)
         (with-current-buffer (find-buffer-visiting archive)
           (goto-char (point-min))
           (insert user-edit))
         (user-error "abort after destination edit"))
       'user-error)
      (dotfiles--lib-test-should-preserve-unsaved-archive-edit
       archive user-edit archive-initial))))

(ert-deftest
    dotfiles--store-archive-transaction-rejects-matching-token-in-other-buffer-test
    ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (dotfiles--lib-test-run-failing-archive-transaction
     source-buffer archive task invocation
     (lambda ()
       (with-temp-buffer
         (org-mode)
         (let ((dotfiles--org-archive-invocation-token invocation))
           (run-hooks 'org-archive-hook))))
     'user-error)))

(ert-deftest
    dotfiles--store-archive-transaction-preserves-unsaved-finalize-edit-test ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (let* ((user-edit "Unsaved finalize edit.\n")
           (org-archive-finalize-hook
            (list
             (lambda ()
               (goto-char (point-min))
               (insert user-edit)))))
      (dotfiles--lib-test-run-failing-paste-transaction
       source-buffer archive task invocation 'user-error)
      (dotfiles--lib-test-should-preserve-unsaved-archive-edit
       archive user-edit archive-initial))))

(ert-deftest
    dotfiles--store-archive-transaction-keeps-edited-archive-copy-test ()
  (dotfiles--lib-test-with-archive-transaction
      "* Existing\nUnsaved note.\n"
    (let* ((user-edit "User edit inside copy.\n")
           (copy (with-current-buffer source-buffer
                   (dotfiles--store-order-subtree task)))
           (line-end (1+ (string-match "\n" copy)))
           (expected (concat archive-initial
                             (substring copy 0 line-end)
                             user-edit
                             (substring copy line-end)))
           copy-start)
      (unwind-protect
          (let ((error
                 (dotfiles--lib-test-run-failing-archive-transaction
                  source-buffer archive task invocation
                  (lambda ()
                    (setq copy-start
                          (dotfiles--lib-test-paste-archive-copy
                           archive task invocation))
                    (with-current-buffer (find-buffer-visiting archive)
                      (goto-char copy-start)
                      (forward-line 1)
                      (insert user-edit))
                    (user-error "abort after archive-copy edit"))
                  'error)))
            (should (string-match-p
                     "abort after archive-copy edit.*rollback incomplete"
                     (error-message-string error))))
        (when copy-start (set-marker copy-start nil)))
      (with-current-buffer (find-buffer-visiting archive)
        (should (equal (buffer-string) expected))
        (should (buffer-modified-p))))))

(ert-deftest
    dotfiles--store-archive-transaction-tracks-finalized-copy-test ()
  (dotfiles--lib-test-with-archive-transaction
      "* Existing\nUnsaved note.\n"
    (dotfiles--lib-test-should-track-finalize-hook
     source-buffer archive task invocation archive-initial
     (list
      (lambda ()
        (forward-line 1)
        (insert "Finalized archive metadata.\n"))))))

(ert-deftest
    dotfiles--store-archive-transaction-runs-single-finalize-function-test ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (dotfiles--lib-test-should-track-finalize-hook
     source-buffer archive task invocation archive-initial
     (lambda ()
       (forward-line 1)
       (insert "Finalized archive metadata.\n")))))

(ert-deftest
    dotfiles--store-archive-transaction-runs-single-source-hook-function-test
    ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (let* (source-hook-ran
          (org-archive-hook (lambda () (setq source-hook-ran t))))
      (dotfiles--lib-test-run-successful-cut-transaction
       source-buffer archive task invocation)
      (should source-hook-ran))))

(ert-deftest
    dotfiles--store-archive-transaction-composes-destination-local-hook-test ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (let (hook-ran)
      (with-current-buffer (find-buffer-visiting archive)
        (org-mode)
        (setq-local org-archive-finalize-hook
                    (list (lambda () (setq hook-ran t)))))
      (dotfiles--lib-test-run-successful-cut-transaction
       source-buffer archive task invocation)
      (should hook-ran)
      (dotfiles--lib-test-should-archive-contain-one-order archive))))

(ert-deftest
    dotfiles--store-archive-transaction-preserves-finalize-hook-mutations-test
    ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (let (next-hook-ran one-shot-hook next-hook)
      (setq next-hook (lambda () (setq next-hook-ran t))
            one-shot-hook
            (lambda ()
              (remove-hook 'org-archive-finalize-hook one-shot-hook t)
              (add-hook 'org-archive-finalize-hook next-hook nil t)))
      (with-current-buffer (find-buffer-visiting archive)
        (org-mode)
        (setq-local org-archive-finalize-hook (list one-shot-hook)))
      (dotfiles--lib-test-run-successful-cut-transaction
       source-buffer archive task invocation)
      (with-current-buffer (find-buffer-visiting archive)
        (should (local-variable-p 'org-archive-finalize-hook))
        (should-not
         (memq one-shot-hook
               (dotfiles--store-hook-functions org-archive-finalize-hook)))
        (should
         (memq next-hook
               (dotfiles--store-hook-functions org-archive-finalize-hook)))
        (setq next-hook-ran nil)
        (run-hooks 'org-archive-finalize-hook))
      (should next-hook-ran))))

(ert-deftest
    dotfiles--store-archive-transaction-preserves-source-after-save-dirty-test
    ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (with-current-buffer source-buffer
      (add-hook 'after-save-hook
                (lambda () (set-buffer-modified-p t)) nil t))
    (dotfiles--lib-test-run-successful-cut-transaction
     source-buffer archive task invocation)
    (with-current-buffer source-buffer
      (should (buffer-modified-p)))))

(ert-deftest
    dotfiles--store-archive-transaction-preserves-source-after-save-edit-test
    ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (let ((hook-edit "Unsaved source after-save edit.\n"))
      (dotfiles--lib-test-add-once-source-save-edit
       source-buffer 'after-save-hook hook-edit)
      (dotfiles--lib-test-run-successful-cut-transaction
       source-buffer archive task invocation)
      (with-current-buffer source-buffer
        (should (string-prefix-p hook-edit (buffer-string)))
        (should (buffer-modified-p)))
      (with-temp-buffer
        (insert-file-contents (buffer-file-name source-buffer))
        (should-not (string-prefix-p hook-edit (buffer-string)))))))

(ert-deftest
    dotfiles--store-archive-transaction-accepts-source-before-save-edit-test ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (let ((hook-edit "Saved by source hook.\n"))
      (dotfiles--lib-test-add-once-source-save-edit
       source-buffer 'before-save-hook hook-edit)
      (dotfiles--lib-test-run-successful-cut-transaction
       source-buffer archive task invocation)
      (with-current-buffer source-buffer
        (should (string-prefix-p hook-edit (buffer-string)))
        (should-not (buffer-modified-p)))
      (with-temp-buffer
        (insert-file-contents (buffer-file-name source-buffer))
        (should (string-prefix-p hook-edit (buffer-string)))))))

(ert-deftest
    dotfiles--store-archive-transaction-preserves-rollback-after-save-dirty-test
    ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (let ((source-initial
           (with-current-buffer source-buffer (buffer-string))))
      (with-current-buffer source-buffer
        (add-hook 'after-save-hook
                  (lambda () (set-buffer-modified-p t)) nil t))
      (dotfiles--lib-test-with-one-shot-archive-save-error archive
        (dotfiles--lib-test-run-failing-cut-transaction
         source-buffer archive task invocation
         'dotfiles--lib-test-archive-save-error))
      (with-current-buffer source-buffer
        (should (equal (buffer-string) source-initial))
        (should (buffer-modified-p))))))

(ert-deftest
    dotfiles--store-archive-transaction-preserves-independently-moved-marker-test
    ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (let ((other-buffer (generate-new-buffer " *independent-marker*"))
          (org-log-note-marker (make-marker)))
      (unwind-protect
          (progn
            (move-marker org-log-note-marker task source-buffer)
            (with-current-buffer (find-buffer-visiting archive)
              (org-mode)
              (setq-local
               org-archive-finalize-hook
               (list
                (lambda ()
                  (move-marker org-log-note-marker 1 other-buffer)))))
            (dotfiles--lib-test-run-failing-paste-transaction
             source-buffer archive task invocation 'user-error)
            (should (eq (marker-buffer org-log-note-marker) other-buffer))
            (should (= (marker-position org-log-note-marker) 1)))
        (set-marker org-log-note-marker nil)
        (kill-buffer other-buffer)))))

(ert-deftest
    dotfiles--store-archive-transaction-tracks-before-finalize-error-test ()
  (dotfiles--lib-test-with-archive-transaction
      "* Existing\nUnsaved note.\n"
    (let ((org-archive-finalize-hook
           (list (lambda () (user-error "finalize failed")))))
      (dotfiles--lib-test-run-failing-paste-transaction
       source-buffer archive task invocation 'user-error)
      (with-current-buffer (find-buffer-visiting archive)
        (should (equal (buffer-string) archive-initial))))))

(ert-deftest
    dotfiles--store-archive-transaction-rolls-back-pre-finalize-error-test ()
  (dotfiles--lib-test-with-archive-transaction
      "* Existing\nUnsaved note.\n"
    (let ((source-initial (with-current-buffer source-buffer (buffer-string))))
      (dotfiles--lib-test-run-failing-archive-transaction
       source-buffer archive task invocation
       (lambda ()
         (with-current-buffer (find-buffer-visiting archive)
           (goto-char (point-max))
           (insert (with-current-buffer source-buffer
                     (dotfiles--store-order-subtree task))))
         (user-error "failed before finalization"))
       'user-error)
      (dotfiles--lib-test-should-restore-live-transaction-state
       source-buffer source-initial archive archive-initial)
      (set-marker task nil)
      (setq task
            (with-current-buffer source-buffer
              (dotfiles--store-find-order-task "teststore" "12345")))
      (dotfiles--lib-test-run-successful-cut-transaction
       source-buffer archive task invocation)
      (dotfiles--lib-test-should-archive-contain-one-order archive))))

(ert-deftest
    dotfiles--store-archive-transaction-removes-finalize-save-copy-test ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((org-archive-finalize-hook
           (list
            (lambda ()
              (save-buffer)
              (user-error "finalize failed after saving")))))
      (dotfiles--lib-test-run-failing-paste-transaction
       source-buffer archive task invocation 'user-error)
      (should-not (file-exists-p archive))
      (should-not (find-buffer-visiting archive)))))

(ert-deftest
    dotfiles--store-archive-transaction-rolls-back-failed-finalize-save-test ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (with-current-buffer (find-buffer-visiting archive) (save-buffer))
    (let ((real-save (symbol-function 'save-buffer))
          failed
          (org-archive-finalize-hook
           (list (lambda () (save-buffer)))))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest args)
                   (if (and (equal buffer-file-name archive) (not failed))
                       (progn
                         (setq failed t)
                         (run-hooks 'before-save-hook)
                         (signal 'dotfiles--lib-test-archive-save-error nil))
                     (apply real-save args)))))
        (dotfiles--lib-test-run-failing-paste-transaction
         source-buffer archive task invocation
         'dotfiles--lib-test-archive-save-error))
      (with-current-buffer (find-buffer-visiting archive)
        (should (equal (buffer-string) archive-initial))
        (should-not (buffer-modified-p)))
      (with-temp-buffer
        (insert-file-contents archive)
        (should (equal (buffer-string) archive-initial))))))

(ert-deftest
    dotfiles--store-archive-transaction-preserves-edit-after-finalize-save-test
    ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (with-current-buffer (find-buffer-visiting archive) (save-buffer))
    (let* ((saved-edit "Saved finalize edit.\n")
           (unsaved-edit "Later unsaved edit.\n")
           (org-archive-finalize-hook
            (list
             (lambda ()
               (goto-char (point-min))
               (insert saved-edit)
               (save-buffer)))))
      (dotfiles--lib-test-run-failing-paste-transaction
       source-buffer archive task invocation 'user-error
       (lambda ()
         (with-current-buffer (find-buffer-visiting archive)
           (goto-char (point-min))
           (insert unsaved-edit))
         (user-error "abort after later edit")))
      (dotfiles--lib-test-should-have-archive-texts
       archive
       (concat unsaved-edit saved-edit archive-initial)
       (concat saved-edit archive-initial)
       t))))

(ert-deftest
    dotfiles--store-archive-transaction-preserves-independent-finalize-save-test
    ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (with-current-buffer (find-buffer-visiting archive) (save-buffer))
    (dotfiles--lib-test-should-preserve-independent-finalize-save
     source-buffer archive task invocation "* Existing\n")))

(ert-deftest
    dotfiles--store-archive-transaction-preserves-independent-new-file-save-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (dotfiles--lib-test-should-preserve-independent-finalize-save
     source-buffer archive task invocation "")))

(ert-deftest
    dotfiles--store-archive-transaction-keeps-new-buffer-edit-after-file-removal-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((user-edit "Unsaved edit after independent save.\n"))
      (with-current-buffer (find-file-noselect archive)
        (org-mode)
        (setq-local org-archive-finalize-hook (list #'save-buffer)))
      (dotfiles--lib-test-run-failing-paste-transaction
       source-buffer archive task invocation 'user-error
       (lambda ()
         (with-current-buffer (find-buffer-visiting archive)
           (goto-char (point-max))
           (insert user-edit))
         (user-error "abort after later archive edit")))
      (dotfiles--lib-test-should-retain-edit-after-archive-file-removal
       archive user-edit))))

(ert-deftest
    dotfiles--store-archive-transaction-keeps-identical-preexisting-copy-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((copy (with-current-buffer source-buffer
                  (dotfiles--store-order-subtree task))))
      (with-current-buffer (find-file-noselect archive)
        (org-mode)
        (insert copy)
        (save-buffer)
        (setq-local
         org-archive-finalize-hook
         (list
          (lambda ()
            (add-hook
             'write-contents-functions
             (lambda ()
               (dotfiles--store-write-file-text
                archive copy buffer-file-coding-system)
               t)
             nil t)
            (save-buffer)
            (user-error "abort after unchanged independent save")))))
      (dotfiles--lib-test-run-failing-paste-transaction
       source-buffer archive task invocation 'user-error)
      (dotfiles--lib-test-should-have-archive-texts
       archive copy copy nil))))

(ert-deftest
    dotfiles--store-archive-transaction-tracks-after-finalize-narrowing-test ()
  (dotfiles--lib-test-with-archive-transaction
      "* Existing\nUnsaved note.\n"
    (let ((org-archive-finalize-hook
           (list
            (lambda ()
              (widen)
              (goto-char (point-min))
              (narrow-to-region (point-min) (line-end-position))))))
      (dotfiles--lib-test-run-failing-paste-transaction
       source-buffer archive task invocation 'user-error
       (lambda () (user-error "abort after narrowed finalization")))
      (with-current-buffer (find-buffer-visiting archive)
        (save-restriction
          (widen)
          (should (equal (buffer-string) archive-initial)))))))

(ert-deftest
    dotfiles--store-archive-transaction-rolls-back-while-narrowed-test ()
  (dotfiles--lib-test-with-archive-transaction
      "* Existing\nUnsaved note.\n"
    (let ((archive-buffer (find-buffer-visiting archive))
          narrowed-min narrowed-max)
      (with-current-buffer archive-buffer
        (goto-char (point-min))
        (forward-line 1)
        (narrow-to-region (point-min) (point))
        (setq narrowed-min (point-min)
              narrowed-max (point-max)))
      (dotfiles--lib-test-run-failing-archive-transaction
       source-buffer archive task invocation
       (lambda ()
         (dotfiles--lib-test-paste-archive-copy archive task invocation t)
         (user-error "abort while archive is narrowed"))
       'user-error)
      (with-current-buffer archive-buffer
        (should (buffer-narrowed-p))
        (should (= (point-min) narrowed-min))
        (should (= (point-max) narrowed-max))
        (save-restriction
          (widen)
          (should (equal (buffer-string) archive-initial)))))))

(ert-deftest
    dotfiles--store-archive-transaction-compensates-source-save-error-test ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((source-initial (with-current-buffer source-buffer (buffer-string)))
          (real-save (symbol-function 'save-buffer))
          (source-save-count (list 0)))
      (cl-letf (((symbol-function 'save-buffer)
                 (dotfiles--lib-test-make-first-source-save-fail
                  source-buffer real-save source-save-count)))
        (dotfiles--lib-test-run-failing-cut-transaction
         source-buffer archive task invocation
         'dotfiles--lib-test-source-save-error))
      (should (= (car source-save-count) 2))
      (should-not (file-exists-p archive))
      (should-not (find-buffer-visiting archive))
      (with-current-buffer source-buffer
        (should (equal (buffer-string) source-initial))
        (should-not (buffer-modified-p)))
      (with-temp-buffer
        (insert-file-contents (buffer-file-name source-buffer))
        (should (equal (buffer-string) source-initial))))))

(ert-deftest
    dotfiles--store-archive-transaction-restores-archive-metadata-after-source-save-error-test
    ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (let* ((archive-buffer (find-buffer-visiting archive))
           archive-point)
      (with-current-buffer archive-buffer
        (save-buffer)
        (goto-char (point-min))
        (setq archive-point (point))
        (set-buffer-modified-p t))
      (dotfiles--lib-test-run-source-save-failure-with-archive-edit
       source-buffer archive task invocation #'ignore)
      (with-current-buffer archive-buffer
        (should (= (point) archive-point)))
      (dotfiles--lib-test-should-have-archive-texts
       archive archive-initial archive-initial t))))

(ert-deftest
    dotfiles--store-archive-transaction-restores-source-after-before-save-removes-copy-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (dotfiles--lib-test-run-invalid-archive-save
     source-buffer archive task invocation
     (lambda ()
       (add-hook
        'before-save-hook
        #'dotfiles--lib-test-delete-archived-order
        nil t))
     'error)
    (with-temp-buffer
      (insert-file-contents archive)
      (should (= (how-many "Iš teststore .*12345 užsakymo") 0)))))

(ert-deftest
    dotfiles--store-archive-transaction-restores-source-after-wrong-write-contents-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (dotfiles--lib-test-run-invalid-archive-save
     source-buffer archive task invocation
     (lambda ()
       (add-hook
        'write-contents-functions
        (lambda ()
          (dotfiles--store-write-file-bytes archive "Wrong file.\n")
          t)
        nil t))
     'user-error)
    (should-not (file-exists-p archive))))

(ert-deftest
    dotfiles--store-archive-transaction-removes-copy-after-write-then-signal-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((source-initial (with-current-buffer source-buffer (buffer-string)))
          (org-archive-finalize-hook
           (list (dotfiles--lib-test-make-write-then-signal-hook archive))))
      (with-current-buffer source-buffer
        (should-error
         (dotfiles--store-archive-transaction
          archive task invocation
          (lambda ()
            (dotfiles--lib-test-copy-and-cut-archive
             archive task invocation nil)))
         :type 'error))
      (dotfiles--lib-test-should-fully-rollback
       source-buffer source-initial archive))))

(ert-deftest
    dotfiles--store-archive-transaction-rejects-unwritten-source-cut-test ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((source-initial (with-current-buffer source-buffer (buffer-string))))
      (with-current-buffer source-buffer
        (add-hook 'write-contents-functions (lambda () t) nil t)
        (should-error
         (dotfiles--store-archive-transaction
          archive task invocation
         (lambda ()
            (dotfiles--lib-test-copy-and-cut-archive
             archive task invocation nil)))
         :type 'user-error))
      (with-current-buffer source-buffer
        (should (equal (buffer-string) source-initial))
        (should (buffer-modified-p)))
      (with-temp-buffer
        (insert-file-contents (buffer-file-name source-buffer))
        (should (equal (buffer-string) source-initial)))
      (should-not (file-exists-p archive)))))

(ert-deftest
    dotfiles--store-archive-transaction-rejects-corrupt-source-cut-test ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((source-initial (with-current-buffer source-buffer (buffer-string)))
          wrong-write-done)
      (with-current-buffer source-buffer
        (add-hook
         'write-contents-functions
         (lambda ()
           (unless wrong-write-done
             (setq wrong-write-done t)
             (dotfiles--store-write-file-text
              file "Wrong source.\n" buffer-file-coding-system)
             (set-visited-file-modtime)
             t))
         nil t))
      (dotfiles--lib-test-run-failing-cut-transaction
       source-buffer archive task invocation 'user-error)
      (should wrong-write-done)
      (dotfiles--lib-test-should-fully-rollback
       source-buffer source-initial archive))))

(ert-deftest
    dotfiles--store-archive-transaction-keeps-copy-after-corrupt-source-rollback-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((source-initial (with-current-buffer source-buffer (buffer-string)))
          (task-text (with-current-buffer source-buffer
                       (dotfiles--store-order-subtree task)))
          (real-save (symbol-function 'save-buffer))
          first-source-save)
      (with-current-buffer source-buffer
        (add-hook
         'write-contents-functions
         (lambda ()
           (dotfiles--store-write-file-text
            file task-text buffer-file-coding-system)
           (set-visited-file-modtime)
           t)
         nil t))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest args)
                   (if (and (eq (current-buffer) source-buffer)
                            (not first-source-save))
                       (progn
                         (setq first-source-save t)
                         (signal 'dotfiles--lib-test-source-save-error nil))
                     (apply real-save args)))))
        (dotfiles--lib-test-run-failing-cut-transaction
         source-buffer archive task invocation
         'error))
      (should first-source-save)
      (with-current-buffer source-buffer
        (should (equal (buffer-string) source-initial)))
      (with-temp-buffer
        (insert-file-contents file)
        (should (equal (buffer-string) task-text)))
      (should (file-exists-p archive))
      (dotfiles--lib-test-should-archive-contain-one-order archive))))

(ert-deftest
    dotfiles--store-archive-transaction-restores-source-after-after-save-edits-copy-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((hook-edit "After-save edit.\n"))
      (dotfiles--lib-test-run-invalid-archive-save
       source-buffer archive task invocation
       (lambda ()
         (add-hook
          'after-save-hook
          (lambda ()
            (dotfiles--lib-test-goto-line-matching
             "Iš teststore .*12345 užsakymo")
            (forward-line 1)
            (insert hook-edit))
          nil t))
       'error)
      (with-current-buffer (find-buffer-visiting archive)
        (goto-char (point-min))
        (should (= (how-many (regexp-quote hook-edit)) 1))
        (should (buffer-modified-p)))
      (with-temp-buffer
        (insert-file-contents archive)
        (should (= (how-many (regexp-quote hook-edit)) 0))
        (should (= (how-many "Iš teststore .*12345 užsakymo") 1))))))

(ert-deftest
    dotfiles--store-archive-transaction-rechecks-copy-after-source-save-test ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((source-initial (with-current-buffer source-buffer (buffer-string)))
          invalidated)
      (with-current-buffer source-buffer
        (add-hook
         'before-save-hook
         (lambda ()
           (unless invalidated
             (setq invalidated t)
             (with-current-buffer (find-buffer-visiting archive)
               (dotfiles--lib-test-delete-archived-order))))
         nil t))
      (dotfiles--lib-test-run-failing-cut-transaction
       source-buffer archive task invocation 'error)
      (should invalidated)
      (dotfiles--lib-test-should-source-be-durable
       source-buffer source-initial)
      (with-current-buffer (find-buffer-visiting archive)
        (goto-char (point-min))
        (should (= (how-many "Iš teststore .*12345 užsakymo") 0))
        (should (buffer-modified-p)))
      (with-temp-buffer
        (insert-file-contents archive)
        (should (= (how-many "Iš teststore .*12345 užsakymo") 1))))))

(ert-deftest
    dotfiles--store-archive-transaction-removes-failed-copy-after-nested-archive-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (dotfiles--lib-test-with-nested-order-source
      (let ((org-archive-finalize-hook
             (list
              (dotfiles--lib-test-make-nested-archive-hook
               archive nested-buffer nested-task nested-invocation))))
        (dotfiles--lib-test-run-failing-paste-transaction
         source-buffer archive task invocation 'user-error)
        (with-current-buffer nested-buffer
          (goto-char (point-min))
          (should (= (how-many "nested@example.com") 0))
          (should-not (buffer-modified-p)))
        (with-current-buffer (find-buffer-visiting archive)
          (goto-char (point-min))
          (should (= (how-many "old@example.com") 0))
          (should (= (how-many "nested@example.com") 1))
          (should-not (buffer-modified-p)))
        (with-temp-buffer
          (insert-file-contents archive)
          (should (= (how-many "old@example.com") 0))
          (goto-char (point-min))
          (should (= (how-many "nested@example.com") 1)))))))

(ert-deftest
    dotfiles--store-archive-transaction-removes-copy-after-early-nested-failure-test
    ()
  (dotfiles--lib-test-with-archive-transaction "* Existing\n"
    (let* ((source-initial
            (with-current-buffer source-buffer (buffer-string)))
           (nested-running nil)
           (nested-invocation (make-symbol "early-nested-failure"))
           (org-archive-finalize-hook
            (list
             (lambda ()
               (unless nested-running
                 (setq nested-running t)
                 (with-current-buffer source-buffer
                   (dotfiles--store-archive-transaction
                    archive task nested-invocation
                    (lambda () (user-error "nested archive failed")))))))))
      (dotfiles--lib-test-run-failing-paste-transaction
       source-buffer archive task invocation 'user-error)
      (dotfiles--lib-test-should-restore-live-transaction-state
       source-buffer source-initial archive archive-initial))))

(ert-deftest
    dotfiles--store-archive-transaction-removes-copy-after-compensated-nested-failure-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (dotfiles--lib-test-with-nested-order-source
      (let* ((real-save (symbol-function 'save-buffer))
             (nested-save-count 0)
             (org-archive-finalize-hook
              (list
               (dotfiles--lib-test-make-nested-archive-hook
                archive nested-buffer nested-task nested-invocation))))
        (cl-letf (((symbol-function 'save-buffer)
                   (lambda (&rest args)
                     (if (eq (current-buffer) nested-buffer)
                         (progn
                           (cl-incf nested-save-count)
                           (if (= nested-save-count 1)
                               (signal
                                'dotfiles--lib-test-source-save-error nil)
                             (apply real-save args)))
                       (apply real-save args)))))
          (dotfiles--lib-test-run-failing-paste-transaction
           source-buffer archive task invocation
           'dotfiles--lib-test-source-save-error))
        (should (= nested-save-count 2))
        (with-current-buffer nested-buffer
          (goto-char (point-min))
          (should (= (how-many "nested@example.com") 1))
          (should-not (buffer-modified-p)))
        (should-not (file-exists-p archive))
        (should-not (find-buffer-visiting archive))))))

(ert-deftest dotfiles--store-archive-transaction-rejects-hard-link-test ()
  (let* ((file
          (make-temp-file "dotfiles-hard-linked-archive" nil ".org"
                          dotfiles--lib-test-order-file-org))
         (archive (concat file ".hardlink"))
         (source-buffer (find-file-noselect file))
         task archive-called)
    (unwind-protect
        (progn
          (with-current-buffer source-buffer
            (org-mode)
            (setq task
                  (dotfiles--store-find-order-task "teststore" "12345")))
          (add-name-to-file file archive)
          (with-current-buffer source-buffer
            (should-error
             (dotfiles--store-archive-transaction
              archive task (make-symbol "hard-link-archive")
              (lambda () (setq archive-called t)))
             :type 'user-error))
          (should-not archive-called))
      (when task (set-marker task nil))
      (when (file-exists-p archive) (delete-file archive))
      (with-current-buffer source-buffer (set-buffer-modified-p nil))
      (kill-buffer source-buffer)
      (delete-file file))))

(ert-deftest
    dotfiles--store-archive-transaction-keeps-disk-dirty-state-test ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let* ((disk-initial "* Existing\nSaved note.\n")
           (unsaved-edit "Unsaved note.\n")
           (real-save (symbol-function 'save-buffer))
           (failing-save
            (dotfiles--lib-test-one-shot-save-error
             real-save (lambda () (equal buffer-file-name archive))
             'dotfiles--lib-test-archive-save-error))
           live-initial)
      (setq live-initial
            (dotfiles--lib-test-prepare-dirty-archive
             archive disk-initial unsaved-edit))
      (cl-letf (((symbol-function 'save-buffer) failing-save))
        (dotfiles--lib-test-run-failing-cut-transaction
         source-buffer archive task invocation
         'dotfiles--lib-test-archive-save-error))
      (with-temp-buffer
        (insert-file-contents archive)
        (should (equal (buffer-string) disk-initial)))
      (with-current-buffer (find-buffer-visiting archive)
        (should (equal (buffer-string) live-initial))
        (should (buffer-modified-p))))))

(ert-deftest
    dotfiles--store-archive-transaction-restores-disk-dirty-state-after-write-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((disk-initial "* Existing\nSaved note.\n")
          (unsaved-edit "Unsaved note.\n")
          source-initial live-initial)
      (setq live-initial
            (dotfiles--lib-test-prepare-dirty-archive
             archive disk-initial unsaved-edit))
      (setq source-initial
            (with-current-buffer source-buffer (buffer-string)))
      (let ((org-archive-finalize-hook
             (list (dotfiles--lib-test-make-write-then-signal-hook archive))))
        (dotfiles--lib-test-run-failing-cut-transaction
         source-buffer archive task invocation
         'dotfiles--lib-test-archive-save-error))
      (dotfiles--lib-test-should-source-be-durable
       source-buffer source-initial)
      (dotfiles--lib-test-should-have-archive-texts
       archive live-initial disk-initial t))))

(ert-deftest
    dotfiles--store-archive-transaction-keeps-destination-after-source-rollback-error-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (dotfiles--lib-test-run-double-source-save-failure
     source-buffer archive task invocation)
    (should (file-exists-p archive))
    (with-temp-buffer
      (insert-file-contents archive)
      (should (= (how-many "Iš teststore .*12345 užsakymo") 1)))))

(ert-deftest
    dotfiles--store-archive-transaction-restores-source-before-archive-probe-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((source-initial (with-current-buffer source-buffer (buffer-string)))
          (real-save (symbol-function 'save-buffer))
          (real-file-text (symbol-function 'dotfiles--store-file-text))
          (source-save-count (list 0)))
      (cl-letf (((symbol-function 'save-buffer)
                 (dotfiles--lib-test-make-first-source-save-fail
                  source-buffer real-save source-save-count))
                ((symbol-function 'dotfiles--store-file-text)
                 (lambda (file)
                   (if (and (equal file archive)
                            (= (car source-save-count) 2))
                       (error "archive cleanup probe failed")
                     (funcall real-file-text file)))))
        (let ((error
               (dotfiles--lib-test-run-failing-cut-transaction
                source-buffer archive task invocation 'error)))
          (should (string-match-p
                   "archive cleanup probe failed"
                   (error-message-string error)))))
      (dotfiles--lib-test-should-source-be-durable
       source-buffer source-initial))))

(ert-deftest
    dotfiles--store-archive-transaction-keeps-destination-after-dirty-source-rollback-error-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((source-initial (with-current-buffer source-buffer (buffer-string)))
          (disk-initial "* Orders\n"))
      (dotfiles--store-write-file-bytes file disk-initial)
      (with-current-buffer source-buffer
        (set-visited-file-modtime)
        (set-buffer-modified-p t))
      (dotfiles--lib-test-run-double-source-save-failure
       source-buffer archive task invocation)
      (with-current-buffer source-buffer
        (should (equal (buffer-string) source-initial))
        (should (buffer-modified-p)))
      (with-temp-buffer
        (insert-file-contents file)
        (should (equal (buffer-string) disk-initial)))
      (should (file-exists-p archive))
      (with-temp-buffer
        (insert-file-contents archive)
        (should (= (how-many "Iš teststore .*12345 užsakymo") 1))))))

(ert-deftest
    dotfiles--store-archive-transaction-keeps-killed-saved-destination-test ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let* ((real-save (symbol-function 'save-buffer))
          (failing-save
           (dotfiles--lib-test-one-shot-save-error
            real-save (lambda () (eq (current-buffer) source-buffer))
            'dotfiles--lib-test-source-save-error))
          (org-archive-finalize-hook
           (list
            (lambda ()
              (add-hook 'after-save-hook
                        (lambda () (kill-buffer (current-buffer))) nil t)))))
      (cl-letf (((symbol-function 'save-buffer) failing-save))
        (dotfiles--lib-test-run-failing-cut-transaction
         source-buffer archive task invocation
         'error))
      (should (file-exists-p archive))
      (should-not (find-buffer-visiting archive)))))

(ert-deftest
    dotfiles--store-archive-transaction-keeps-new-file-unrelated-edit-test ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((user-edit "User edit during source save.\n"))
      (dotfiles--lib-test-run-source-save-failure-with-archive-edit
       source-buffer archive task invocation
       (lambda ()
         (goto-char (point-min))
         (insert-before-markers user-edit)))
      (dotfiles--lib-test-should-retain-edit-after-archive-file-removal
       archive user-edit))))

(ert-deftest
    dotfiles--store-archive-transaction-compensates-owned-save-with-later-edit-test
    ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((user-edit "Unsaved edit after owned archive save.\n"))
      (dotfiles--lib-test-run-source-save-failure-with-archive-edit
       source-buffer archive task invocation
       (lambda ()
         (goto-char (point-max))
         (insert user-edit)))
      (dotfiles--lib-test-should-retain-edit-after-archive-file-removal
       archive user-edit))))

(ert-deftest
    dotfiles--store-archive-transaction-preserves-source-save-hook-edit-test ()
  (dotfiles--lib-test-with-archive-transaction nil
    (let ((real-save (symbol-function 'save-buffer))
          (archive-save-count 0)
          (hook-edit "Source save-hook edit.\n"))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest args)
                   (prog1 (apply real-save args)
                     (when (and (equal buffer-file-name archive)
                                (= (cl-incf archive-save-count) 1))
                       (with-current-buffer source-buffer
                         (goto-char task)
                         (insert hook-edit))
                       (signal 'dotfiles--lib-test-archive-save-error nil))))))
        (dotfiles--lib-test-run-failing-cut-transaction
         source-buffer archive task invocation
         'dotfiles--lib-test-archive-save-error))
      (with-current-buffer source-buffer
        (should (= (how-many "Iš teststore .*12345 užsakymo") 1))
        (should (= (how-many (regexp-quote hook-edit)) 1))
        (should-not (buffer-modified-p))
        (let ((restored (buffer-string)))
          (with-temp-buffer
            (insert-file-contents (buffer-file-name source-buffer))
            (should (equal (buffer-string) restored)))))
      (should-not (find-buffer-visiting archive))
      (should-not (file-exists-p archive)))))

(ert-deftest
    dotfiles--store-archive-transaction-removes-new-file-scaffolding-test ()
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org nil nil)
    (let* ((source-buffer (find-file-noselect file))
           (archive (concat file "_archive"))
           (invocation (make-symbol "real-archive-transaction-test"))
           (task (with-current-buffer source-buffer
                   (dotfiles--store-find-order-task "teststore" "12345")))
           (fail-first t)
           (org-archive-location (concat archive "::* Archive"))
           (org-archive-subtree-save-file-p nil)
           (org-archive-hook
            (list
             (lambda ()
               (when (and fail-first
                          (eq invocation
                              dotfiles--org-archive-invocation-token))
                 (user-error "abort the first real archive"))))))
      (unwind-protect
          (with-current-buffer source-buffer
            (goto-char task)
            (should-error
             (dotfiles--store-archive-transaction
              archive task invocation
              (lambda ()
                (let ((dotfiles--org-archive-requested-token invocation))
                  (org-archive-subtree))))
             :type 'user-error)
            (should-not (find-buffer-visiting archive))
            (should-not (file-exists-p archive))
            (setq fail-first nil)
            (goto-char task)
            (dotfiles--store-archive-transaction
             archive task invocation
             (lambda ()
               (let ((dotfiles--org-archive-requested-token invocation))
                 (org-archive-subtree))))
            (with-current-buffer (find-buffer-visiting archive)
              (save-restriction
                (widen)
                (should (= (how-many "Archived entries from file") 1))
                (should (= (how-many "^\\* Archive$") 1))
                (should (= (how-many "Iš teststore .*12345 užsakymo") 1)))))
        (set-marker task nil)))))

(defconst dotfiles--lib-test-complete-order-done-result
  `(:completed ,dotfiles--lib-test-order-task-heading
               :archived ,dotfiles--lib-test-order-task-heading
               :linked ,dotfiles--lib-test-order-task-heading
               :todo "DONE")
  "The `dotfiles--lib-test-complete-order-run' result for a completed order.
All three steps -- the DONE transition, the archival and the link append -- must
name the order task, and `:todo' must show that the transition actually took:
`:completed' and `:archived' report where the two stubbed steps were aimed,
while `:linked' and `:todo' read the result back out of the buffer.  The tests
below differ only in the state or hazard they set up and all expect exactly
this, so the expectation is written once: a regression that misdirects any one
step has to change this constant to pass, which makes it obvious that it is
changing every one of them.")

(defconst dotfiles--lib-test-order-untouched-todo-states
  '(("Tasks" nil)
    ("Decoy" "TODO")
    ("Decoy child" "TODO")
    ("After" "TODO"))
  "TODO states of every fixture heading the completion must leave alone.")

(defconst dotfiles--lib-test-complete-order-region-result
  `(,@dotfiles--lib-test-complete-order-done-result
    :todo-states ,dotfiles--lib-test-order-untouched-todo-states)
  "Completed order result including every heading's TODO state.
Used by the active-region case to prove only the target changed state.")

(defconst dotfiles--lib-test-prompt-abort-result
  `(:completed nil :archived nil
               :remaining-orders
               ((,dotfiles--lib-test-order-task-heading "TODO")))
  "Completion result after the selected task changes during its prompt.")

(defconst dotfiles--lib-test-complete-order-kill-result
  `(:completed nil
               :archived ,dotfiles--lib-test-order-task-heading
               :linked ,dotfiles--lib-test-order-task-heading
               :todo "KILL")
  "The `dotfiles--lib-test-complete-order-run' result for a killed order.
`:completed' is nil because a terminal keyword other than
`org-autotask-keyword-done' goes through `org-todo' rather than
`org-autotask-complete-item'; that the transition still hit the order task is
what `:todo' shows, read back from the archived copy.")

(defconst dotfiles--lib-test-complete-order-kill-region-result
  `(,@dotfiles--lib-test-complete-order-kill-result
    :todo-states ,dotfiles--lib-test-order-untouched-todo-states)
  "Killed order result including every heading's TODO state.
Used by the active-region kill case to prove only the target changed state.")

(defconst dotfiles--lib-test-order-kill-todo-keywords
  '((sequence "TODO" "|" "DONE" "KILL"))
  "`org-todo-keywords' spec declaring KILL, absent from the defaults.
Bound around the kill tests so the fixture buffers created inside the run
parse and accept the keyword.")

(ert-deftest dotfiles--mu4e-complete-order-task-completes-order-task-test ()
  (should (equal (dotfiles--lib-test-complete-order-run)
                 dotfiles--lib-test-complete-order-done-result)))

(ert-deftest dotfiles--mu4e-complete-order-task-completes-folded-task-test ()
  (should (equal (dotfiles--lib-test-complete-order-run :folded t)
                 dotfiles--lib-test-complete-order-done-result)))

(ert-deftest dotfiles--mu4e-complete-order-task-kills-order-task-test ()
  (let ((org-todo-keywords dotfiles--lib-test-order-kill-todo-keywords))
    (should (equal (dotfiles--lib-test-complete-order-run :keyword "KILL")
                   dotfiles--lib-test-complete-order-kill-result))))

;; Same expectation as the default-keyword test, kept separate to pin that the
;; dispatch is by value: an explicit DONE routes through
;; `org-autotask-complete-item' exactly like the nil default, never through
;; the `org-todo' branch merely because a keyword was supplied.
(ert-deftest
    dotfiles--mu4e-complete-order-task-completes-with-explicit-done-test ()
  (should (equal (dotfiles--lib-test-complete-order-run :keyword "DONE")
                 dotfiles--lib-test-complete-order-done-result)))

(ert-deftest
    dotfiles--mu4e-complete-order-task-supports-single-archive-hook-test ()
  (let ((org-archive-hook #'ignore))
    (should (equal (dotfiles--lib-test-complete-order-run)
                   dotfiles--lib-test-complete-order-done-result))))

(ert-deftest
    dotfiles--mu4e-complete-order-task-preserves-archive-hook-mutations-test ()
  (let (next-hook-ran one-shot-hook next-hook)
    (setq next-hook (lambda () (setq next-hook-ran t))
          one-shot-hook
          (lambda ()
            (remove-hook 'org-archive-hook one-shot-hook)
            (add-hook 'org-archive-hook next-hook)))
    (let ((org-archive-hook (list one-shot-hook)))
      (should (equal (dotfiles--lib-test-complete-order-run)
                     dotfiles--lib-test-complete-order-done-result))
      (should-not
       (memq one-shot-hook
             (dotfiles--store-hook-functions org-archive-hook)))
      (should
       (memq next-hook (dotfiles--store-hook-functions org-archive-hook)))
      (setq next-hook-ran nil)
      (run-hooks 'org-archive-hook)
      (should next-hook-ran))))

;; The terminality guard must fire before any mutation: no prompt, no link
;; append, no archive.  "TODO" is a valid keyword outside `org-done-keywords',
;; so `org-todo' alone would accept it.
(ert-deftest
    dotfiles--mu4e-complete-order-task-rejects-non-terminal-keyword-test ()
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org nil nil)
    (let (prompted archive-called)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq prompted t) nil))
                ((symbol-function 'org-archive-subtree)
                 (lambda (&rest _) (setq archive-called t))))
        (should-error
         (dotfiles--mu4e-complete-order-task
          file "teststore" (list :message-id msgid) "12345" "TODO")
         :type 'user-error))
      (should-not prompted)
      (should-not archive-called)
      (with-current-buffer (find-buffer-visiting file)
        (should-not (buffer-modified-p)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (should (search-forward
                 "** TODO Iš teststore 2026-08-17 12345 užsakymo" nil t))
        (goto-char (point-min))
        (should-not (search-forward msgid nil t))))))

;; The mid-prompt analogue of the guard test above: the prompt yields to the
;; command loop, where a `#+TODO:' edit plus restart can move KEYWORD out of
;; the done partition while `org-todo' still accepts it, so the terminality
;; check must be re-established before the transition mutates the task.
(ert-deftest
    dotfiles--mu4e-complete-order-task-rejects-repartitioned-keyword-test ()
  (let ((org-todo-keywords dotfiles--lib-test-order-kill-todo-keywords))
    (dotfiles--lib-test-with-order-org-file
        (dotfiles--lib-test-order-file-org nil nil)
      (let (archive-called)
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (save-excursion
                       (goto-char (point-min))
                       (insert "#+TODO: TODO KILL | DONE\n"))
                     (org-mode-restart)
                     t))
                  ((symbol-function 'org-archive-subtree)
                   (lambda (&rest _) (setq archive-called t))))
          (should (string-match-p
                   "is no longer a terminal keyword"
                   (cadr (should-error
                          (dotfiles--mu4e-complete-order-task
                           file "teststore" (list :message-id msgid) "12345"
                           "KILL")
                          :type 'user-error)))))
        (should-not archive-called)
        (with-current-buffer (find-buffer-visiting file)
          (save-excursion
            (goto-char (point-min))
            (should (search-forward
                     "** TODO Iš teststore 2026-08-17 12345 užsakymo" nil t))))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (should-not (search-forward msgid nil t)))))))

(ert-deftest dotfiles--mu4e-complete-order-task-rejects-blocked-completion-test
    ()
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org-blocked nil nil)
    (let ((org-enforce-todo-dependencies t)
          (org-blocker-hook
           '(org-block-todo-from-children-or-siblings-or-parent))
          archive-called)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'org-archive-subtree)
                 (lambda (&rest _) (setq archive-called t))))
        (should-error
         (dotfiles--mu4e-complete-order-task
          file "teststore" (list :message-id msgid) "12345")
         :type 'user-error))
      (should-not archive-called)
      (with-temp-buffer
        (insert-file-contents file)
        (should (search-forward
                 "** TODO Iš teststore 2026-08-17 12345 užsakymo" nil t))))))

;; Without the re-anchor after the prompt, the wrong task is marked DONE and
;; archived.  See `dotfiles--with-store-order-task' for why point moves.
(ert-deftest dotfiles--mu4e-complete-order-task-survives-prompt-drift-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :drift-to "^\\*\\* TODO Decoy$")
                 dotfiles--lib-test-complete-order-done-result)))

;; Reaches the re-anchor between the DONE transition and the archive, which the
;; prompt drift above cannot: it fires before that re-anchor.  Deleting the
;; re-anchor archives "Decoy" instead.
(ert-deftest dotfiles--mu4e-complete-order-task-survives-done-drift-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :drift-after-done "^\\*\\* TODO Decoy$")
                 dotfiles--lib-test-complete-order-done-result)))

;; `:todo' "TODO" is what makes the `:completed' nil above discriminating: it
;; shows the entry was left alone, not merely that the stub went unrecorded.
(ert-deftest dotfiles--mu4e-complete-order-task-declined-prompt-test ()
  (should (equal (dotfiles--lib-test-complete-order-run :answer nil)
                 `(:completed nil :archived nil
                              :linked ,dotfiles--lib-test-order-task-heading
                              :todo "TODO"))))

;; `org-todo' runs for real here, with the raw region still visible but its loop
;; policy disabled.  A leaked loop would mark every heading in the region DONE,
;; which the plist's `:todo-states' shows.  The completion and archive wrappers
;; also pin their respective narrow policies at the call boundaries.
(ert-deftest dotfiles--mu4e-complete-order-task-ignores-region-test ()
  (should (equal (dotfiles--lib-test-complete-order-run :region t)
                 dotfiles--lib-test-complete-order-region-result)))

;; The kill analogue of the region test above: the `org-todo' branch bypasses
;; the completion wrapper and its policy probe, so `:todo-states' is the only
;; witness if a narrowed rebinding let the region loop leak here.
(ert-deftest dotfiles--mu4e-complete-order-task-ignores-region-on-kill-test ()
  (let ((org-todo-keywords dotfiles--lib-test-order-kill-todo-keywords))
    (should (equal (dotfiles--lib-test-complete-order-run
                    :region t :keyword "KILL")
                   dotfiles--lib-test-complete-order-kill-region-result))))

;; The insertion analogue of the drift tests; see
;; `dotfiles--store-find-order-task' for the marker contract it pins.
(ert-deftest dotfiles--mu4e-complete-order-task-survives-prompt-insert-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :insert-before "^\\*\\* TODO Iš teststore ")
                 dotfiles--lib-test-complete-order-done-result)))

;; The deletion analogue, and the one hazard of the three whose misfire is
;; silent and destructive; see `dotfiles--mu4e-complete-order-task'.
(ert-deftest dotfiles--mu4e-complete-order-task-aborts-on-deleted-task-test ()
  (should (equal (dotfiles--lib-test-complete-order-run :delete-task t)
                 '(:completed nil :archived nil :remaining-orders nil))))

(ert-deftest
    dotfiles--mu4e-complete-order-task-aborts-before-identical-successor-test ()
  (should
   (equal
    (dotfiles--lib-test-complete-order-run
     :text dotfiles--lib-test-order-file-org-identical-successor
     :delete-task t)
    dotfiles--lib-test-prompt-abort-result)))

(ert-deftest
    dotfiles--mu4e-complete-order-task-aborts-on-prompt-boundary-insert-test ()
  (should
   (equal
    (dotfiles--lib-test-complete-order-run :insert-at-task-end t)
    '(:completed nil :archived nil))))

(ert-deftest
    dotfiles--mu4e-complete-order-task-refreshes-archive-destination-test ()
  (should (dotfiles--lib-test-archive-redirect-observed-p nil)))

(ert-deftest
    dotfiles--mu4e-complete-order-task-refreshes-post-completion-destination-test
    ()
  (should (dotfiles--lib-test-archive-redirect-observed-p t)))

(ert-deftest
    dotfiles--mu4e-complete-order-task-refreshes-post-completion-project-status-test
    ()
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org nil nil)
    (dotfiles--lib-test-complete-with-effect
     file msgid
     (lambda ()
       (org-back-to-heading t)
       (org-up-heading-safe)
       (org-todo "TODO"))
     (lambda (&rest _)
       (ert-fail "post-completion sub-action reached archive")))
    (dotfiles--lib-test-should-order-be-done file)))

(ert-deftest
    dotfiles--mu4e-complete-order-task-aborts-on-post-completion-deletion-test
    ()
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org-identical-successor nil nil)
    (should-error
     (dotfiles--lib-test-complete-with-effect
      file msgid
      (lambda ()
        (org-back-to-heading t)
        (delete-region
         (point) (save-excursion (org-end-of-subtree t t) (point))))
      (lambda (&rest _)
        (ert-fail "identical successor reached archive")))
     :type 'user-error)
    (with-current-buffer (find-buffer-visiting file)
      (goto-char (point-min))
      (should (= (how-many "Iš teststore .*12345 užsakymo") 1))
      (should (search-forward "Second copy." nil t)))))

(ert-deftest
    dotfiles--mu4e-complete-order-task-refreshes-project-status-test ()
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org nil nil)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _)
                 (dotfiles--lib-test-goto-line-matching
                  "^\\*\\* TODO Iš teststore ")
                 (org-up-heading-safe)
                 (org-todo "TODO")
                 t))
              ((symbol-function 'dotfiles--store-archive-transaction)
               (lambda (&rest _)
                 (ert-fail "project sub-action reached the archive"))))
      (dotfiles--mu4e-complete-order-task
       file "teststore" (list :message-id msgid) "12345"))
    (dotfiles--lib-test-should-order-be-done file)))

(ert-deftest dotfiles--mu4e-complete-order-task-rejects-same-file-archive-test
    ()
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org-same-file-archive nil nil)
    (let ((initial-text dotfiles--lib-test-order-file-org-same-file-archive))
      (dotfiles--lib-test-should-reject-order-archive
       file msgid initial-text))))

(ert-deftest
    dotfiles--mu4e-complete-order-task-rejects-hard-linked-archive-test ()
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org nil nil)
    (let ((archive (concat file ".hardlink"))
          initial-text)
      (unwind-protect
          (progn
            (add-name-to-file file archive)
            (setq initial-text
                  (dotfiles--lib-test-set-order-archive file archive))
            (dotfiles--lib-test-should-reject-order-archive
             file msgid initial-text))
        (when (file-exists-p archive) (delete-file archive))))))

(ert-deftest
    dotfiles--mu4e-complete-order-task-rejects-dangling-archive-symlink-test ()
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org nil nil)
    (let ((archive (concat file ".symlink"))
          (target (concat file ".missing")))
      (unwind-protect
          (progn
            (make-symbolic-link target archive)
            (dotfiles--lib-test-should-reject-order-archive
             file msgid
             (dotfiles--lib-test-set-order-archive file archive))
            (should (equal (file-symlink-p archive) target))
            (should-not (file-exists-p target)))
        (when (file-symlink-p archive) (delete-file archive))))))

;; The archive-time analogue: `org-archive-subtree' yields to the command loop
;; between its copy and its irreversible cut, so the prompt-time checks above
;; cannot reach it; see `dotfiles--mu4e-complete-order-task'.  Deleting the
;; `org-archive-hook' guard cuts whatever the heading became.  `:completed'
;; non-nil pins that the abort lands after the DONE transition, which is the
;; price of guarding this late.
(ert-deftest dotfiles--mu4e-complete-order-task-aborts-on-archive-change-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :change-during-archive t)
                 `(:completed ,dotfiles--lib-test-order-task-heading
                              :archived nil))))

;; The insertion analogue at archive time, and the only run in which the two
;; anchors disagree: the heading lands at the task's own beginning of line, so
;; the archive's restored point sits on the intruder while `task', an
;; insertion-type-t marker, has moved past it.  The guard's `goto-char' is what
;; puts the cut back on the task -- deleting it, reordering it after the heading
;; check, or confining it to a `save-excursion' leaves the cut on the intruder,
;; which the stub's post-hook probe catches.
(ert-deftest dotfiles--mu4e-complete-order-task-aborts-on-archive-insert-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :insert-during-archive t)
                 `(:completed ,dotfiles--lib-test-order-task-heading
                              :archived nil))))

;; The archive's region guard has to be a policy variable rather than
;; `mark-active': the latter is buffer-local state the user's own commands
;; write, so unbinding it here would restore the pre-binding value and discard
;; the region they just made.
(ert-deftest dotfiles--mu4e-complete-order-task-keeps-user-region-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :mark-during-archive t)
                 dotfiles--lib-test-complete-order-done-result)))

;; The copy/cut divergence, and the only hazard here that no heading check can
;; see: point drifting before `org-archive-subtree' takes its copy leaves the
;; copy on one subtree and the re-anchored cut on another, both headings
;; intact.  Only what was copied tells them apart.  Without the guard's clip
;; check the cut runs, and the task is lost from the file without reaching the
;; archive; `:archived' nil pins that it never ran.
(ert-deftest dotfiles--mu4e-complete-order-task-aborts-on-copy-drift-test ()
  (should (equal (dotfiles--lib-test-complete-order-run :drift-before-copy t)
                 `(:completed ,dotfiles--lib-test-order-task-heading
                              :archived nil))))

(ert-deftest
    dotfiles--mu4e-complete-order-task-aborts-on-identical-copy-drift-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :drift-to-identical-subtree t)
                 `(:completed ,dotfiles--lib-test-order-task-heading
                              :archived nil))))

;; The other side of that clip check, and why it cannot stand alone:
;; `org-subtree-clip' is a global, so any `org-copy-subtree' or `org-refile' the
;; user runs during one of the archive's yields overwrites it.  The archive is
;; still cutting the task -- its restored point says so -- and aborting on the
;; clip alone would strand a copy in the archive for a run that was correct.
(ert-deftest dotfiles--mu4e-complete-order-task-aborts-on-clip-clobber-test ()
  (should (equal (dotfiles--lib-test-complete-order-run :clobber-clip t)
                 `(:completed ,dotfiles--lib-test-order-task-heading
                              :archived nil))))

(ert-deftest
    dotfiles--mu4e-complete-order-task-aborts-before-identical-archive-successor-test
    ()
  (should
   (equal
    (dotfiles--lib-test-complete-order-run
     :text dotfiles--lib-test-order-file-org-identical-archive-successor
     :delete-during-archive t)
    `(:completed ,dotfiles--lib-test-order-task-heading :archived nil))))

(ert-deftest dotfiles--mu4e-complete-order-task-aborts-on-body-change-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :change-body-during-archive t)
                 `(:completed ,dotfiles--lib-test-order-task-heading
                              :archived nil))))

;; `org-archive-subtree' saves the archive file before `org-archive-hook' can
;; abort the cut, so the entry point suppresses that save and performs it after
;; the cut instead.  On the success path the copy still has to reach the disk.
(ert-deftest dotfiles--mu4e-complete-order-task-saves-archive-copy-test ()
  (should (equal (dotfiles--lib-test-complete-order-run :archive-buffer t)
                 dotfiles--lib-test-complete-order-done-result)))

;; An abort rolls back only this invocation's paste, so no duplicate is left
;; for a later retry to persist.
(ert-deftest dotfiles--mu4e-complete-order-task-rolls-back-archive-copy-test
    ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :archive-buffer t :change-during-archive t)
                 `(:completed ,dotfiles--lib-test-order-task-heading
                              :archived nil))))

(ert-deftest dotfiles--mu4e-complete-order-task-retries-cleanly-after-abort-test
    ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :archive-buffer t
                  :archive-initial-text "* Existing\nUnsaved note.\n"
                  :clobber-clip t :retry-after-abort t :move-org-marker t)
                 dotfiles--lib-test-complete-order-done-result)))

(ert-deftest dotfiles--mu4e-complete-order-task-aborts-on-killed-archive-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                 :archive-buffer t :kill-archive-buffer t)
                 `(:completed ,dotfiles--lib-test-order-task-heading
                              :archived nil))))

;; The guard is installed globally for the archive's whole extent, which spans
;; the archive's own yields, so it also runs for an `org-archive-subtree' the
;; user starts meanwhile.  It must do nothing there: re-anchoring on `task'
;; would redirect their cut onto our order task, and signalling would abort
;; their command with their own copy already written.
(ert-deftest dotfiles--mu4e-complete-order-task-ignores-foreign-archive-test ()
  (should (equal (dotfiles--lib-test-complete-order-run :foreign-archive t)
                 dotfiles--lib-test-complete-order-done-result)))

(ert-deftest dotfiles--mu4e-complete-order-task-ignores-nested-archive-test ()
  (should (advice-member-p #'dotfiles--org-archive-with-invocation-token
                           'org-archive-subtree))
  (should (equal (dotfiles--lib-test-complete-order-run :nested-archive t)
                 dotfiles--lib-test-complete-order-done-result)))

(ert-deftest
    dotfiles--mu4e-complete-order-task-restores-nested-archive-policies-test ()
  (dotfiles--lib-test-with-order-org-file
      (dotfiles--lib-test-order-file-org nil nil)
    (let* ((nested-file
            (make-temp-file "dotfiles-nested-archive" nil ".org"
                            "* TODO First\n* TODO Second\n"))
           (nested-archive (concat nested-file "_archive"))
           (nested-buffer (find-file-noselect nested-file))
           nested-running)
      (unwind-protect
          (let ((org-loop-over-headlines-in-active-region t)
                (org-ignore-region t)
                (org-archive-subtree-save-file-p t)
                (org-archive-finalize-hook
                 (list
                  (lambda ()
                    (unless nested-running
                      (setq nested-running t)
                      (with-current-buffer nested-buffer
                        (org-mode)
                        (goto-char (point-min))
                        (push-mark (point-max) t t)
                        (let ((transient-mark-mode t)
                              (org-archive-location
                               (concat nested-archive "::")))
                          (org-archive-subtree))))))))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (should-error
               (dotfiles--mu4e-complete-order-task
                file "teststore" (list :message-id msgid) "12345")
               :type 'user-error))
            (with-current-buffer nested-buffer
              (goto-char (point-min))
              (should (= (how-many "^\\* TODO ") 1)))
            (should (file-exists-p nested-archive))
            (with-temp-buffer
              (insert-file-contents nested-archive)
              (should (= (how-many "^\\* TODO ") 1))))
        (let ((buffer (find-buffer-visiting nested-archive)))
          (when buffer
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))
        (with-current-buffer nested-buffer (set-buffer-modified-p nil))
        (kill-buffer nested-buffer)
        (when (file-exists-p nested-archive) (delete-file nested-archive))
        (delete-file nested-file)))))

(ert-deftest dotfiles--mu4e-complete-order-task-keeps-sub-action-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :text dotfiles--lib-test-order-file-org-sub-action)
                 `(:completed ,dotfiles--lib-test-order-task-heading
                              :archived nil
                              :linked ,dotfiles--lib-test-order-task-heading
                              :todo "DONE"))))

;; Mis-selection is destructive on this path -- the wrong task is marked DONE
;; and archived -- and a real order file, unlike every other fixture, holds many
;; rival @waitingfor tasks.
(ert-deftest dotfiles--mu4e-complete-order-task-picks-among-rival-tasks-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :text dotfiles--lib-test-order-file-org-rival-tasks)
                 dotfiles--lib-test-complete-order-done-result)))

;; The completion half of the idempotency contract; see
;; `dotfiles--store-file-order-email-skips-already-filed-link-test'.
(ert-deftest dotfiles--mu4e-complete-order-task-skips-already-filed-link-test ()
  (should (equal (dotfiles--lib-test-complete-order-run
                  :text dotfiles--lib-test-order-file-org-linked)
                 dotfiles--lib-test-complete-order-done-result)))

(ert-deftest dotfiles--mu4e-complete-order-task-without-order-task-test ()
  (should-error (dotfiles--lib-test-complete-order-run
                 :text dotfiles--lib-test-order-file-org-no-task)
                :type 'user-error))

;; The terminality guard precedes the missing-task error, so a misconfigured
;; keyword surfaces on every run instead of hiding behind "no task found".
(ert-deftest
    dotfiles--mu4e-complete-order-task-rejects-keyword-without-task-test ()
  (should (string-match-p
           "is not a terminal keyword"
           (cadr (should-error
                  (dotfiles--lib-test-complete-order-run
                   :text dotfiles--lib-test-order-file-org-no-task
                   :keyword "TODO")
                  :type 'user-error)))))

(provide 'my-lib-test)

;;; my-lib-test.el ends here
