;;; my-lib-test.el --- Tests for my-lib -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the personal helper library.

;;; Code:

(require 'ert)

;; my-lib requires mu4e and org-autotask at load time; batch tests run without
;; them, and the functions under test do not call into them. The stubs are
;; removed after the load so a later test file in the same batch run cannot
;; silently `require' a stub instead of the real library. (Let-binding
;; `features' around the load does not work: `require' and `featurep' ignore
;; the binding.)
(provide 'mu4e-message)
(provide 'mu4e-autotask)
(provide 'org-autotask)
(require 'my-lib)
(dolist (stub '(mu4e-message mu4e-autotask org-autotask))
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

(provide 'my-lib-test)

;;; my-lib-test.el ends here
