;;; my-lib-test.el --- Tests for my-lib -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the personal helper library.

;;; Code:

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

(provide 'my-lib-test)

;;; my-lib-test.el ends here
