;;; my-org-gtd-test.el --- Tests for my-org-gtd.el. -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for my-org-gtd.el using ERT.

;;; Code:

;; Hard dependencies
(require 'ert)
(require 'my-org-gtd)

;; Soft dependencies
(defvar org-gcal-cancelled-todo-keyword)

;; Test fixture

(defconst my-org-gtd--test-contexts
  (vector
   (make-my-org-gtd-context
    :tag "@c1" :select-char ?a
    :description "c1 context")
   (make-my-org-gtd-context
    :tag "@c2" :select-char ?b
    :description "c2 context")))

(defmacro my-org-gtd--test-fixture (varlist &rest body)
  "A test fixture for `my-org-gtd' to bind VARLIST vars and execute BODY forms."
  (declare (indent 1) (debug t))
  `(let ((org-use-tag-inheritance nil)
         (org-todo-log-states nil)
         (org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)" "KILL(k!)")))
         ,@varlist)
     ,@body))

;; Test `my-org-gtd-context'

(ert-deftest my-org-gtd-context-not-tag-basic ()
  "Basic test for `my-org-gtd-context-not-tag'."
  (my-org-gtd--test-fixture
      ((my-org-gtd-waitingfor-context (make-my-org-gtd-context
                                       :tag "@foo" :select-char ?x
                                       :description "Waiting-for context")))
    (should (equal (my-org-gtd-context-not-tag my-org-gtd-waitingfor-context)
                   "-@foo"))))

;; Test `my-org-gtd-initialize'

(ert-deftest org-tag-alist-construction-empty ()
  "Test that `org-tag-alist' is properly constructed."
  (my-org-gtd--test-fixture ((org-tag-alist nil)
                             (my-org-gtd-contexts my-org-gtd--test-contexts)
                             (my-org-gtd-waitingfor-context
                              (make-my-org-gtd-context
                               :tag "@sometag" :select-char ?f
                               :description "Waiting-for context"))
                             (my-org-gtd-project-context
                              (make-my-org-gtd-context
                               :tag "prj" :select-char ?c
                               :description "Projects"))
                             (my-org-gtd-somedaymaybe-context
                              (make-my-org-gtd-context
                               :tag "maybesomeday" :select-char ?d
                               :description "Someday/maybe")))
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
  (my-org-gtd--test-fixture ((org-tag-alist '(("@x" . ?x)))
                             (my-org-gtd-contexts my-org-gtd--test-contexts)
                             (my-org-gtd-waitingfor-context
                              (make-my-org-gtd-context
                               :tag "@anothertag" :select-char ?x
                               :description "Waiting-for context"))
                             (my-org-gtd-project-context
                              (make-my-org-gtd-context
                               :tag "foo" :select-char ?f
                               :description "Foos"))
                             (my-org-gtd-somedaymaybe-context
                              (make-my-org-gtd-context
                               :tag "maybesomeday" :select-char ?d
                               :description "Someday/maybe")))
    (my-org-gtd-initialize)
    (should (equal org-tag-alist
                   '((:startgroup)
                     ("@c1" . ?a)
                     ("@c2" . ?b)
                     ("@anothertag" . ?x)
                     (:endgroup)
                     ("foo" . ?f)
                     ("maybesomeday" . ?d)
                     ("@x" . ?x))))))

(ert-deftest my-org-gtd-next-action-keyword-not-in-org-todo-keywords ()
  "Test that the next action keyword must be present in `org-todo-keywords'."
  (my-org-gtd--test-fixture ((my-org-gtd-next-action-keyword "ABSENT"))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-next-action-keyword-without-selection-char-ok ()
  "Test that the next action keyword is accepted without the selection char."
  (my-org-gtd--test-fixture ((my-org-gtd-next-action-keyword "TODO"))
    (my-org-gtd-initialize)))

(ert-deftest my-org-gtd-next-action-keyword-absent-but-prefix ()
  "Test that the absent NA keyword is diagnosed when it's a prefix."
  (my-org-gtd--test-fixture
      ((my-org-gtd-next-action-keyword "TODO")
       (org-todo-keywords '((sequence "TODONE(t!)" "|" "KILL(k!)" "DONE(d!)"))))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-done-keyword-not-in-org-todo-keywords ()
  "Test that the completed keyword must be present in `org-todo-keywords'."
  (my-org-gtd--test-fixture ((my-org-gtd-done-keyword "ABSENT"))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-done-keyword-without-selection-char-ok ()
  "Test that the completed keyword is accepted without the selection char."
  (my-org-gtd--test-fixture ((my-org-gtd-done-keyword "DONE"))
    (my-org-gtd-initialize)))

(ert-deftest my-org-gtd-done-keyword-absent-but-prefix ()
  "Test that the absent done keyword is diagnosed when it's a prefix."
  (my-org-gtd--test-fixture
      ((my-org-gtd-done-keyword "DONE")
       (org-todo-keywords '((sequence "TODO(t!)" "|" "KILL(k!)"
                                      "DONEANDDONE(d!)"))))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-org-todo-repeat-to-state ()
  "Test that `org-todo-repeat-to-state' is initialized correctly."
  (my-org-gtd--test-fixture
      ((my-org-gtd-next-action-keyword "TODO")
       (org-todo-repeat-to-state nil)
       (org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)" "KILL(k!)"))))
    (my-org-gtd-initialize)
    (should (equal org-todo-repeat-to-state my-org-gtd-next-action-keyword))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-t ()
  "Test `org-use-tag-inheritance' when it's t."
  (my-org-gtd--test-fixture ((org-use-tag-inheritance t))
    (my-org-gtd-initialize)
    (should (equal org-use-tag-inheritance t))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-matching-regex ()
  "Test `org-use-tag-inheritance' matching `my-org-gtd-somedaymaybe-tag'."
  (my-org-gtd--test-fixture ((org-use-tag-inheritance "may.*")
                             (my-org-gtd-somedaymaybe-context
                              (make-my-org-gtd-context
                               :tag "maybe" :select-char ?d
                               :description "Someday/maybe")))
    (my-org-gtd-initialize)
    (should (equal org-use-tag-inheritance "may.*"))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-not-matching-regex ()
  "Test `org-use-tag-inheritance' not matching `my-org-gtd-somedaymaybe-tag'."
  (my-org-gtd--test-fixture ((org-use-tag-inheritance "foo.*")
                             (my-org-gtd-somedaymaybe-context
                              (make-my-org-gtd-context
                               :tag "maybe" :select-char ?d
                               :description "Someday/maybe")))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-add-to-list ()
  "Test adding `my-org-gtd-somedaymaybe-tag' to `org-use-tag-inheritance'."
  (my-org-gtd--test-fixture ((org-use-tag-inheritance '("foo" "bar"))
                             (my-org-gtd-somedaymaybe-context
                              (make-my-org-gtd-context
                               :tag "somedaymaybe" :select-char ?d
                               :description "Someday/maybe")))
    (my-org-gtd-initialize)
    (should (equal org-use-tag-inheritance '("somedaymaybe" "foo" "bar")))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-already-in-list ()
  "Test `org-use-tag-inheritance' containing `my-org-gtd-somedaymaybe-tag'."
  (my-org-gtd--test-fixture ((org-use-tag-inheritance '("foo" "bar"))
                             (my-org-gtd-somedaymaybe-context
                              (make-my-org-gtd-context
                               :tag "foo" :select-char ?d
                               :description "Someday/maybe")))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-org-use-tag-inheritance-wrong-type ()
  "Test `org-use-tag-inheritance' being of unrecognized type.."
  (my-org-gtd--test-fixture ((org-use-tag-inheritance 42)
                             (my-org-gtd-somedaymaybe-context
                              (make-my-org-gtd-context
                               :tag "foo" :select-char ?d
                               :description "Someday/maybe")))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-cancelled-keyword-not-in-org-todo-keywords ()
  "Test that the cancelled keyword must be present in `org-todo-keywords'."
  (my-org-gtd--test-fixture ((my-org-gtd-cancelled-keyword "ABSENT"))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-cancelled-keyword-without-selection-char-ok ()
  "Test that the cancelled keyword is accepted without the selection char."
  (my-org-gtd--test-fixture ((my-org-gtd-cancelled-keyword "KILL"))
    (my-org-gtd-initialize)))

(ert-deftest my-org-gtd-cancelled-keyword-absent-but-prefix ()
  "Test that the absent cancelled keyword is diagnosed when it's a prefix."
  (my-org-gtd--test-fixture
      ((my-org-gtd-cancelled-keyword "KILL")
       (org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)" "KILLED(k!)"))))
    (should-error (my-org-gtd-initialize))))

(ert-deftest my-org-gtd-org-gcal-cancelled-todo-keyword ()
  "Test that `org-gcal-cancelled-todo-keyword' is initialized correctly."
  (my-org-gtd--test-fixture
      ((my-org-gtd-cancelled-keyword "CANCELLED")
       (org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
       (org-gcal-cancelled-todo-keyword nil))
    (my-org-gtd-initialize)
    (should (equal org-gcal-cancelled-todo-keyword
                   my-org-gtd-cancelled-keyword))))

(ert-deftest my-org-gtd-active-todo-search-one-context ()
  "Test for `my-org-gtd-active-todo-search' with one context."
  (my-org-gtd--test-fixture
      ((ctx (make-my-org-gtd-context :tag "ctx" :select-char ?c
                                     :description "ctx description"))
       (my-org-gtd-somedaymaybe-context
        (make-my-org-gtd-context :tag "oneday" :select-char ?d
                                 :description "Someday/maybe"))
       (my-org-gtd-next-action-keyword "DOIT")
       (org-todo-keywords '((sequence "DOIT(t!)" "|" "DONE(d!)" "KILL(k!)"))))
    (my-org-gtd-initialize)
    (should (equal (my-org-gtd-active-todo-search ctx) "ctx-oneday/!DOIT"))))

(ert-deftest my-org-gtd-active-todo-search-two-contexts ()
  "Test for `my-org-gtd-active-todo-search' with two contexts."
  (my-org-gtd--test-fixture
      ((ctx (make-my-org-gtd-context :tag "ctx" :select-char ?c
                                     :description "ctx description"))
       (ctx2 (make-my-org-gtd-context :tag "foo" :select-char ?f
                                      :description "foo description"))
       (my-org-gtd-somedaymaybe-context
        (make-my-org-gtd-context :tag "maybe" :select-char ?m
                                 :description "Someday/maybe"))
       (my-org-gtd-next-action-keyword "DOIT")
       (org-todo-keywords '((sequence "DOIT(t!)" "|" "DONE(d!)" "KILL(k!)"))))
    (my-org-gtd-initialize)
    (should (equal (my-org-gtd-active-todo-search ctx ctx2)
                   "ctx-maybe|foo-maybe/!DOIT"))))

(ert-deftest my-org-gtd-agenda-basic ()
  "Basic test for `my-org-gtd-agenda'."
  (my-org-gtd--test-fixture
      ((context (make-my-org-gtd-context :tag "foo" :select-char ?f
                                         :description "Foo description")))
    (should (equal (my-org-gtd-agenda context)
                   '("Foo description" tags-todo "foo")))))

(ert-deftest my-org-gtd-somedaymaybe-agenda-basic ()
  "Basic test for `my-org-gtd-somedaymaybe-agenda'."
  (my-org-gtd--test-fixture
      ((my-org-gtd-somedaymaybe-context
        (make-my-org-gtd-context :tag "bar" :select-char ?b
                                 :description "Bar description")))
    (should (equal (my-org-gtd-somedaymaybe-agenda)
                   '("Bar description" tags-todo "bar+LEVEL=2"
                     ((org-agenda-dim-blocked-tasks nil)))))))

(ert-deftest my-org-gtd-active-non-project-tasks-basic ()
  "Basic test for `my-org-gtd-active-non-project-tasks-agenda'."
  (my-org-gtd--test-fixture
      ((my-org-gtd-project-context
        (make-my-org-gtd-context :tag "prj" :select-char ?p
                                 :description "Prj description"))
       (my-org-gtd-somedaymaybe-context
        (make-my-org-gtd-context :tag "maybe" :select-char ?m
                                 :description "Maybe description"))
       (my-org-gtd-waitingfor-context
        (make-my-org-gtd-context :tag "wait" :select-char ?w
                                 :description "Wait context"))
       (my-org-gtd-next-action-keyword "NEXT"))
    (should (equal (my-org-gtd-active-non-project-tasks-agenda)
                   '("Non-project next actions" tags-todo
                     "-prj-wait-maybe/!NEXT"
                     ((org-use-tag-inheritance ("prj" "maybe"))))))))

(defmacro my-org-gtd--buffer-test (varlist &rest body)
  "Set up a temp `org' buffer, bind VARLIST and execute BODY in the fixture."
  (declare (indent 1) (debug t))
  `(my-org-gtd--test-fixture ,varlist
     (with-temp-buffer
       (org-mode)
       ,@body)))

(ert-deftest my-org-gtd-insert-waiting-for-next-action-basic ()
  "Basic test for `my-org-gtd-insert-waiting-for-next-action'."
  (my-org-gtd--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (my-org-gtd-insert-waiting-for-next-action "Test title")
    (should (string= (org-get-heading t t) "Test title"))
    (should (string= (org-get-todo-state) my-org-gtd-next-action-keyword))
    (should (equal (org-get-tags) (list (my-org-gtd-context-tag
                                         my-org-gtd-waitingfor-context))))))

(ert-deftest my-org-gtd-insert-waiting-for-next-action-reject-empty ()
  "Test that `my-org-gtd-insert-waiting-for-next-action' rejects empty title."
  (my-org-gtd--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (should-error (my-org-gtd-insert-waiting-for-next-action ""))))

(ert-deftest my-org-gtd-insert-waiting-for-next-action-custom-state-tag ()
  "Test `my-org-gtd-insert-waiting-for-next-action' with non-default config."
  (my-org-gtd--buffer-test
      ((my-org-gtd-next-action-keyword "NEXT")
       (my-org-gtd-waitingfor-context
        (make-my-org-gtd-context
         :tag "@wait" :select-char ?f
         :description "Waiting-for context"))
       (org-todo-keywords '((sequence "NEXT(n!)" "|" "DONE(d!)" "KILL(k!)"))))
    (my-org-gtd-initialize)
    (org-insert-todo-heading-respect-content)
    (my-org-gtd-insert-waiting-for-next-action "Title text")
    (should (string= (org-get-heading t t) "Title text"))
    (should (string= (org-get-todo-state) my-org-gtd-next-action-keyword))
    (should (equal (org-get-tags) (list (my-org-gtd-context-tag
                                         my-org-gtd-waitingfor-context))))))

(ert-deftest my-org-gtd-insert-project-basic ()
  "Basic test for `my-org-gtd-insert-project'."
  (my-org-gtd--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (my-org-gtd-insert-project "Test title")
    (should (string= (org-get-heading t t) "Test title"))
    (should (string= (org-get-todo-state) my-org-gtd-next-action-keyword))
    (should (equal (org-get-tags) (list (my-org-gtd-context-tag
                                         my-org-gtd-project-context))))))

(ert-deftest my-org-gtd-insert-project-reject-empty ()
  "Test that `my-org-gtd-insert-project' rejects empty title."
  (my-org-gtd--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (should-error (my-org-gtd-insert-project ""))))

(ert-deftest my-org-gtd-insert-project-custom-state-tag ()
  "Test `my-org-gtd-insert-project' with non-default config."
  (my-org-gtd--buffer-test
      ((my-org-gtd-next-action-keyword "FOO")
       (my-org-gtd-project-context
        (make-my-org-gtd-context
         :tag "bar" :select-char ?b
         :description "Bars"))
       (org-todo-keywords '((sequence "FOO" "|" "DONE" "KILL"))))
    (my-org-gtd-initialize)
    (org-insert-todo-heading-respect-content)
    (my-org-gtd-insert-project "Title text")
    (should (string= (org-get-heading t t) "Title text"))
    (should (string= (org-get-todo-state) my-org-gtd-next-action-keyword))
    (should (equal (org-get-tags) (list (my-org-gtd-context-tag
                                         my-org-gtd-project-context))))))

(ert-deftest my-org-gtd-complete-item-basic ()
  "Basic test for `my-org-complete-item'."
  (my-org-gtd--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (my-org-gtd-insert-waiting-for-next-action "Test title")
    (should (string= (org-get-todo-state) my-org-gtd-next-action-keyword))
    (my-org-gtd-complete-item)
    (should (string= (org-get-todo-state) my-org-gtd-done-keyword))
    (should (string= (org-get-heading t t) "Test title"))
    (should (equal (org-get-tags) (list (my-org-gtd-context-tag
                                         my-org-gtd-waitingfor-context))))))

;; TODO(laurynas): idempotency
;; TODO(laurynas): uniqueness in tags
;; TODO(laurynas): uniqueness in keys
;; TODO(laurynas): uniqueness between contexts and waitingfor

(provide 'my-org-gtd-test)
;;; my-org-gtd-test.el ends here
