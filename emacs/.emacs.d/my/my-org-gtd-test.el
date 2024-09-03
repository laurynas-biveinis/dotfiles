;;; my-org-gtd-test.el --- Tests for my-org-gtd.el. -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for my-org-gtd.el using ERT.

;;; Code:

(require 'ert)
(require 'my-org-gtd)

(ert-deftest org-tag-alist-construction-empty ()
  "Test that `org-tag-alist' is properly constructed."
  (let ((org-tag-alist nil)
        (my-org-gtd-contexts '(("@c1" . ?a) ("@c2" . ?b)))
        (my-org-gtd-waitingfor-tag "@sometag")
        (my-org-gtd-waitingfor-select ?f)
        (org-todo-keywords '((sequence "TODO(t!)" "DONE(d!)"))))
    (my-org-gtd-initialize)
    (should (equal org-tag-alist
                   '((:startgroup)
                     ("@c1" . ?a)
                     ("@c2" . ?b)
                     ("@sometag" . ?f)
                     (:endgroup))))))

(ert-deftest org-tag-alist-construction-preexisting ()
  "Test that `org-tag-alist' is properly constructed, when it's non-empty."
  (let ((org-tag-alist '(("@p" . ?p)))
        (my-org-gtd-contexts '(("@c1" . ?a) ("@c2" . ?b)))
        (my-org-gtd-waitingfor-tag "@sometag")
        (my-org-gtd-waitingfor-select ?f)
        (org-todo-keywords '((sequence "TODO(t!)" "DONE(d!)"))))
    (my-org-gtd-initialize)
    (should (equal org-tag-alist
                   '((:startgroup)
                     ("@c1" . ?a)
                     ("@c2" . ?b)
                     ("@sometag" . ?f)
                     (:endgroup)
                     ("@p" . ?p))))))

(ert-deftest my-org-gtd-not-waitingfor ()
  "Test that `my-org-gtd-not-waitingfor' is initialized correctly."
  (let ((my-org-gtd-waitingfor-tag "@foo")
        (org-todo-keywords '((sequence "TODO(t!)" "DONE(d!)"))))
    (my-org-gtd-initialize)
    (should (equal my-org-gtd-not-waitingfor "-@foo"))))

(ert-deftest my-org-gtd-next-action-keyword-not-in-org-todo-keywords ()
  "Test that the next action keyword must be present in `org-todo-keywords'."
  (let ((my-org-gtd-next-action-keyword "ABSENT(a!)")
        (org-todo-keywords
         '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED"))))
    (should-error (my-org-gtd-initialize))))

;; TODO(laurynas): idempotency
;; TODO(laurynas): uniqueness in tags
;; TODO(laurynas): uniqueness in keys
;; TODO(laurynas): uniqueness between contexts and waitingfor

(provide 'my-org-gtd-test)
;;; my-org-gtd-test.el ends here
