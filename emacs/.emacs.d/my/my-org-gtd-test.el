;;; my-org-gtd-test.el --- Tests for my-org-gtd.el. -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for my-org-gtd.el using ERT.

;;; Code:

(require 'ert)
(require 'my-org-gtd)

(ert-deftest org-tag-alist-construction ()
  "Test that `org-tag-alist' is properly constructed."
  (let ((org-tag-alist nil)
        (my-org-gtd-contexts '(("@c1" . ?a) ("@c2" . ?b)))
        (my-org-gtd-waitingfor-tag "@sometag")
        (my-org-gtd-waitingfor-select ?f))
    (my-org-gtd-initialize)
    (should (equal (car org-tag-alist)
                   '(:startgroup
                     ("@c1" . ?a)
                     ("@c2" . ?b)
                     ("@sometag" . ?f)
                     :endgroup)))))

(provide 'my-org-gtd-test)
;;; my-org-gtd-test.el ends here
