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

(defconst dotfiles--lib-test-order-file-org-nested-no-task "\
* Container
** Tasks
*** TODO Decoy
**** TODO Decoy child
*** TODO After
** Other
*** TODO Elsewhere
"
  "Org text with an invisible \"Tasks\" heading nested under a folded parent.")

;; Only the final child catches the misfile: the link still lands on the
;; correctly titled task wherever it ended up.
(ert-deftest dotfiles--store-file-order-email-creates-task-when-folded-test ()
  (let ((file (make-temp-file
               "dotfiles-order" nil ".org"
               dotfiles--lib-test-order-file-org-nested-no-task))
        (org-startup-folded 'fold)
        (system-time-locale "C"))
    (unwind-protect
        (cl-letf (((symbol-function 'mu4e-message-field) #'plist-get)
                  ((symbol-function 'org-store-link)
                   (lambda (&rest _)
                     "[[mu4e:msgid:new@example.com][New]]")))
          (dotfiles--store-file-order-email
           file "teststore" '(:message-id "new@example.com") "12345"
           "2026-08-17" "2026-08-18")
          (with-current-buffer (find-file-noselect file)
            (goto-char (org-find-exact-headline-in-buffer "Tasks"))
            (org-end-of-subtree t)
            (org-back-to-heading t)
            (should (equal (org-get-heading t t t t)
                           "Iš teststore 2026-08-17 12345 užsakymo"))))
      (let ((buffer (find-buffer-visiting file)))
        (when buffer
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-file file))))

(ert-deftest dotfiles--store-file-order-email-unfindable-order-id-test ()
  (dolist (order-id '("12 345" " 12345" ""))
    (with-temp-buffer
      (org-mode)
      (should
       (string-match-p
        "single whitespace-free token"
        (cadr (should-error
               (dotfiles--store-find-order-task "teststore" order-id)
               :type 'user-error)))))))

(ert-deftest dotfiles--store-file-order-email-unfindable-order-date-test ()
  (let ((file (make-temp-name
               (expand-file-name "dotfiles-order-date-"
                                 temporary-file-directory))))
    (should-not (file-exists-p file))
    (dolist (order-date '("2026-08 17" " 2026-08-17" "2026-08-17\n" ""))
      (should
       (string-match-p
        "single whitespace-free token"
        (cadr (should-error
               (dotfiles--store-file-order-email
                file "teststore" nil "12345" order-date nil)
               :type 'user-error)))))
    (should-not (file-exists-p file))))

(provide 'my-lib-test)

;;; my-lib-test.el ends here
