;;; my-org-gtd-test.el --- Tests for my-org-gtd.el. -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for my-org-gtd.el using ERT.

;;; Code:

;; Hard dependencies
(require 'ert)
(require 'my-org-gtd)

;; Soft dependencies
(defvar org-gcal-cancelled-todo-keyword)

(ert-deftest org-tag-alist-construction-empty ()
  "Test that `org-tag-alist' is properly constructed."
  (let ((org-tag-alist nil)
        (my-org-gtd-contexts '(("@c1" . ?a) ("@c2" . ?b)))
        (my-org-gtd-waitingfor-tag "@sometag")
        (my-org-gtd-waitingfor-select ?f)
        (my-org-gtd-project-tag "prj")
        (my-org-gtd-project-select ?c)
        (my-org-gtd-somedaymaybe-tag "maybesomeday")
        (my-org-gtd-somedaymaybe-select ?d)
        (org-todo-keywords '((sequence "TODO(t!)" "KILL(k!)"))))
    (my-org-gtd-initialize)
    (should (equal org-tag-alist
                   '((:startgroup)
                     ("@c1" . ?a)
                     ("@c2" . ?b)
                     ("@sometag" . ?f)
                     (:endgroup)
                     ("prj" . ?c)
                     ("maybesomeday" . ?d))))))

(ert-deftest org-tag-alist-construction-preexisting ()
  "Test that `org-tag-alist' is properly constructed, when it's non-empty."
  (let ((org-tag-alist '(("@x" . ?x)))
        (my-org-gtd-contexts '(("@c1" . ?a) ("@c2" . ?b)))
        (my-org-gtd-waitingfor-tag "@sometag")
        (my-org-gtd-waitingfor-select ?f)
        (my-org-gtd-project-tag "prj")
        (my-org-gtd-project-select ?c)
        (my-org-gtd-somedaymaybe-tag "maybesomeday")
        (my-org-gtd-somedaymaybe-select ?d)
        (org-todo-keywords '((sequence "TODO(t!)" "KILL(k!)"))))
    (my-org-gtd-initialize)
    (should (equal org-tag-alist
                   '((:startgroup)
                     ("@c1" . ?a)
                     ("@c2" . ?b)
                     ("@sometag" . ?f)
                     (:endgroup)
                     ("prj" . ?c)
                     ("maybesomeday" . ?d)
                     ("@x" . ?x))))))

(ert-deftest my-org-gtd-not-waitingfor ()
  "Test that `my-org-gtd-not-waitingfor' is initialized correctly."
  (let ((my-org-gtd-waitingfor-tag "@foo")
        (org-todo-keywords '((sequence "TODO(t!)" "KILL(k!)"))))
    (my-org-gtd-initialize)
    (should (equal my-org-gtd-not-waitingfor "-@foo"))))

(ert-deftest my-org-gtd-not-project ()
  "Test that `my-org-gtd-not-project' is initialized correctly."
  (let ((my-org-gtd-project-tag "foo")
        (org-todo-keywords '((sequence "TODO(t!)" "KILL(k!)"))))
    (my-org-gtd-initialize)
    (should (equal my-org-gtd-not-project "-foo"))))

(ert-deftest my-org-gtd-next-action-keyword-not-in-org-todo-keywords ()
  "Test that the next action keyword must be present in `org-todo-keywords'."
  (let ((my-org-gtd-next-action-keyword "ABSENT")
        (org-todo-keywords
         '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED"))))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-next-action-keyword-without-selection-char-ok ()
  "Test that the next action keyword is accepted without the selection char."
  (let ((my-org-gtd-next-action-keyword "TODO")
        (org-todo-keywords '((sequence "TODO(t!)" "KILL(k!)"))))
    (my-org-gtd-initialize)))

(ert-deftest my-org-gtd-next-action-keyword-absent-but-prefix ()
  "Test that the absent NA keyword is diagnosed when it's a prefix."
  (let ((my-org-gtd-next-action-keyword "TODO")
        (org-todo-keywords '((sequence "TODONE(t!)" "DONE(d!)"))))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-org-todo-repeat-to-state ()
  "Test that `org-todo-repeat-to-state' is initialized correctly."
  (let ((my-org-gtd-next-action-keyword "TODO")
        (org-todo-repeat-to-state nil)
        (org-todo-keywords '((sequence "TODO(t!)" "KILL(k!)"))))
    (my-org-gtd-initialize)
    (should (equal org-todo-repeat-to-state my-org-gtd-next-action-keyword))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-t ()
  "Test `org-use-tag-inheritance' when it's t."
  (let ((org-use-tag-inheritance t))
    (my-org-gtd-initialize)
    (should (equal org-use-tag-inheritance t))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-matching-regex ()
  "Test `org-use-tag-inheritance' matching `my-org-gtd-somedaymaybe-tag'."
  (let ((org-use-tag-inheritance "may.*")
        (my-org-gtd-somedaymaybe-tag "maybe"))
    (my-org-gtd-initialize)
    (should (equal org-use-tag-inheritance "may.*"))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-not-matching-regex ()
  "Test `org-use-tag-inheritance' not matching `my-org-gtd-somedaymaybe-tag'."
  (let ((org-use-tag-inheritance "foo.*")
        (my-org-gtd-somedaymaybe-tag "maybe"))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-add-to-list ()
  "Test adding `my-org-gtd-somedaymaybe-tag' to `org-use-tag-inheritance'."
  (let ((org-use-tag-inheritance '("foo" "bar"))
        (my-org-gtd-somedaymaybe-tag "somedaymaybe"))
    (my-org-gtd-initialize)
    (should (equal org-use-tag-inheritance '("somedaymaybe" "foo" "bar")))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-already-in-list ()
  "Test `org-use-tag-inheritance' containing `my-org-gtd-somedaymaybe-tag'."
  (let ((org-use-tag-inheritance '("foo" "bar"))
        (my-org-gtd-somedaymaybe-tag "foo"))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-wrong-type ()
  "Test `org-use-tag-inheritance' being of unrecognized type.."
  (let ((org-use-tag-inheritance 42)
        (my-org-gtd-somedaymaybe-tag "foo"))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-cancelled-keyword-not-in-org-todo-keywords ()
  "Test that the cancelled keyword must be present in `org-todo-keywords'."
  (let ((my-org-gtd-cancelled-keyword "ABSENT")
        (org-todo-keywords
         '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED"))))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-cancelled-keyword-without-selection-char-ok ()
  "Test that the cancelled keyword is accepted without the selection char."
  (let ((my-org-gtd-cancelled-keyword "KILL")
        (org-todo-keywords '((sequence "TODO(t!)" "KILL(k!)"))))
    (my-org-gtd-initialize)))

(ert-deftest my-org-gtd-cancelled-keyword-absent-but-prefix ()
  "Test that the absent cancelled keyword is diagnosed when it's a prefix."
  (let ((my-org-gtd-cancelled-keyword "KILL")
        (org-todo-keywords '((sequence "TODO(t!)" "KILLED(k!)"))))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-org-gcal-cancelled-todo-keyword ()
  "Test that `org-gcal-cancelled-todo-keyword' is initialized correctly."
  (let ((my-org-gtd-cancelled-keyword "CANCELLED")
        (org-todo-keywords '((sequence "TODO" "CANCELLED")))
        (org-gcal-cancelled-todo-keyword nil))
    (my-org-gtd-initialize)
    (should (equal org-gcal-cancelled-todo-keyword
                   my-org-gtd-cancelled-keyword))))

(defmacro my-org-gtd--buffer-test (&rest body)
  "Set up a temporary `org' buffer and execute BODY."
  `(with-temp-buffer
     (org-mode)
     (let ((org-todo-log-states nil))
       ,@body)))

(ert-deftest my-org-gtd-insert-waiting-for-next-action-basic ()
  "Basic test for `my-org-gtd-insert-waiting-for-next-action'."
  (my-org-gtd--buffer-test
   (org-insert-todo-heading-respect-content)
   (my-org-gtd-insert-waiting-for-next-action "Test title")
   (should (string= (org-get-heading t t) "Test title"))
   (should (string= (org-get-todo-state) my-org-gtd-next-action-keyword))
   (should (equal (org-get-tags) `(,my-org-gtd-waitingfor-tag)))))

(ert-deftest my-org-gtd-insert-waiting-for-next-action-reject-empty ()
  "Test that `my-org-gtd-insert-waiting-for-next-action' rejects empty title."
  (my-org-gtd--buffer-test
   (org-insert-todo-heading-respect-content)
   (should-error (my-org-gtd-insert-waiting-for-next-action ""))))

(ert-deftest my-org-gtd-insert-waiting-for-next-action-custom-state-tag ()
  "Test `my-org-gtd-insert-waiting-for-next-action' with non-default config."
  (let ((my-org-gtd-next-action-keyword "NEXT")
        (my-org-gtd-waitingfor-tag "@wait")
        (org-todo-keywords '((sequence "NEXT" "KILL"))))
    (my-org-gtd-initialize)
    (my-org-gtd--buffer-test
     (org-insert-todo-heading-respect-content)
     (my-org-gtd-insert-waiting-for-next-action "Title text")
     (should (string= (org-get-heading t t) "Title text"))
     (should (string= (org-get-todo-state) my-org-gtd-next-action-keyword))
     (should (equal (org-get-tags) `(,my-org-gtd-waitingfor-tag))))))

(ert-deftest my-org-gtd-insert-project-basic ()
  "Basic test for `my-org-gtd-insert-project'."
  (my-org-gtd--buffer-test
   (org-insert-todo-heading-respect-content)
   (my-org-gtd-insert-project "Test title")
   (should (string= (org-get-heading t t) "Test title"))
   (should (string= (org-get-todo-state) my-org-gtd-next-action-keyword))
   (should (equal (org-get-tags) `(,my-org-gtd-project-tag)))))

(ert-deftest my-org-gtd-insert-project-reject-empty ()
  "Test that `my-org-gtd-insert-project' rejects empty title."
  (my-org-gtd--buffer-test
   (org-insert-todo-heading-respect-content)
   (should-error (my-org-gtd-insert-project ""))))

(ert-deftest my-org-gtd-insert-project-custom-state-tag ()
  "Test `my-org-gtd-insert-project' with non-default config."
  (let ((my-org-gtd-next-action-keyword "FOO")
        (my-org-gtd-project-tag "bar")
        (org-todo-keywords '((sequence "FOO" "KILL"))))
    (my-org-gtd-initialize)
    (my-org-gtd--buffer-test
     (org-insert-todo-heading-respect-content)
     (my-org-gtd-insert-project "Title text")
     (should (string= (org-get-heading t t) "Title text"))
     (should (string= (org-get-todo-state) my-org-gtd-next-action-keyword))
     (should (equal (org-get-tags) `(,my-org-gtd-project-tag))))))

;; TODO(laurynas): idempotency
;; TODO(laurynas): uniqueness in tags
;; TODO(laurynas): uniqueness in keys
;; TODO(laurynas): uniqueness between contexts and waitingfor

(provide 'my-org-gtd-test)
;;; my-org-gtd-test.el ends here
