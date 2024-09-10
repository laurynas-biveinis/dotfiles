;;; my-org-gtd.el --- My GTD setup in `org' -*- lexical-binding: t -*-

;; Version: 0.1
;; URL: https://github.com/laurynas-biveinis/dotfiles/
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines

;;; Commentary:

;; This is a proto-package for my GTD implementation in `org'.

;;; Code:

;; Hard dependencies
(require 'org)
(require 'cl-lib)

;; Soft dependencies
(defvar org-gcal-cancelled-todo-keyword)

;; The context structure
(cl-defstruct (my-org-gtd-context)
  "A single GTD context."
  (tag "" :type string :read-only t :documentation "The `org' tag.")
  (select-char
   ? :type character :read-only t
   :documentation "The `org' quick selection character for the tag.")
  (description "" :type string :read-only t
               :documentation "The description string for this context."))

(defun my-org-gtd-context-not-tag (gtd-context)
  "Get the substring for `org-agenda' blocks to exclude GTD-CONTEXT."
  (concat "-" (my-org-gtd-context-tag gtd-context)))

;; Customization
(defgroup my-org-gtd nil
  "Configure `org' for GTD."
  :group 'org)

(defcustom my-org-gtd-contexts nil
  "GTD contexts with `org' tags, quick selection characters, and descriptions.
The tags and the selection keys will be added to as a single group to
`org-tag-alist', together with (`my-org-gtd-waitingfor-tag' .
`my-org-gtd-waitingfor-select')  by`my-org-gtd-initialize'."
  :type '(repeat (struct :tag "Context"
                         (string :tag "`org' Tag")
                         (character :tag "Quick selection character")
                         (string :tag "Description")))
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-waitingfor-context
  (make-my-org-gtd-context :tag "@waitingfor" :select-char ?w
                           :description "Waiting-for items")
  "The GTD waiting-for context."
  :type '(struct :tag "GTD waiting-for context"
                 (string :tag "`org Tag")
                 (character :tag "Quick selection character")
                 (string :tag "Description"))
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-project-context
  (make-my-org-gtd-context :tag "project" :select-char ?p
                           :description "Projects")
  "The GTD project context."
  :type '(struct :tag "Projects. Not a GTD context but works as one in `org'."
                 (string :tag "`org Tag")
                 (character :tag "Quick selection character")
                 (string :tag "Description"))
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defvar my-org-gtd-not-somedaymaybe nil
  "A substring for `org-agenda' to exclude `my-org-gtd-somedaymaybe-tag'.
Initialized by `my-org-gtd-initialize'.")

(defcustom my-org-gtd-somedaymaybe-tag "somedaymaybe"
  "The `org' tag used for GTD someday/maybe items."
  :type '(string)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-somedaymaybe-select ?m
  "The character to select the GTD someday/maybe tag."
  :type '(character)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-next-action-keyword "TODO"
  "The TODO entry keyword that designates a GTD next action.
Projects also have this keyword (in addition to `my-org-gtd-project-tag' tag.)
It must be present in `org-todo-keywords', either directly or through per-file
configuration, with an optional fast state selection character."
  :type '(string)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-done-keyword "DONE"
  "The TODO entry keyword that designates a completed task or project.
It must be present in `org-todo-keyword', either directly or thorugh per-file
configuration, with an optional fast state selection character."
  :type '(string)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-cancelled-keyword "KILL"
  "The TODO entry keyword that designates a cancelled task or project.
It must be present in `org-todo-keywords', either directly or through per-file
configuration, with an optional fast state selection character."
  :type '(string)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defun my-org-gtd--check-keyword-in-org-todo-keywords (keyword)
  "Check that KEYWORD in present in `org-todo-keywords'."
  (unless (seq-some
           (lambda (todo-sequence)
             (seq-some (lambda (keyword-and-char)
                         (when (stringp keyword-and-char)
                           (string= (car (split-string keyword-and-char "("))
                                    keyword)))
                       todo-sequence))
           org-todo-keywords)
    (user-error "'%s' must be present in `org-todo-keywords'" keyword)))

(defun my-org-gtd--make-org-alist-cons-cell (context)
  "Convert a CONTEXT to a cons cell for `org-tag-alist'."
  (cons (my-org-gtd-context-tag context)
        (my-org-gtd-context-select-char context)))

(defun my-org-gtd-initialize ()
  "Initialize `my-org-gtd'.
Checks `org-todo-keywords' against keyword configuration, initializes
`org-todo-repeat-to-state'. Adds to `org-use-tag-inheritance' and to
`org-tag-alist' from the tag  variables, selection character variables, and the
GTD contexts variables."
  ;; Validate config
  (my-org-gtd--check-keyword-in-org-todo-keywords
   my-org-gtd-next-action-keyword)
  (my-org-gtd--check-keyword-in-org-todo-keywords my-org-gtd-done-keyword)
  (my-org-gtd--check-keyword-in-org-todo-keywords my-org-gtd-cancelled-keyword)
  ;; Configure itself
  (setq my-org-gtd-not-somedaymaybe (concat "-" my-org-gtd-somedaymaybe-tag))
  ;; Configure `org'
  (cond
   ((eq org-use-tag-inheritance t)
    nil)
   ((stringp org-use-tag-inheritance)
    (unless (string-match-p org-use-tag-inheritance my-org-gtd-somedaymaybe-tag)
      (user-error
       "`my-org-gtd-somedaymaybe-tag' %s does not match `org-use-tag-inheritance' regex %s"
       my-org-gtd-somedaymaybe-tag org-use-tag-inheritance)))
   ((listp org-use-tag-inheritance)
    (when (member my-org-gtd-somedaymaybe-tag org-use-tag-inheritance)
      (user-error
       "`my-org-gtd-somedaymaybe-tag' %s already in `org-use-tag-inheritance' %S"
       my-org-gtd-somedaymaybe-tag org-use-tag-inheritance))
    (push my-org-gtd-somedaymaybe-tag org-use-tag-inheritance))
   (t (user-error "Don't know how handle `org-use-tag-inheritance' value %S"
                  org-use-tag-inheritance)))
  (setq org-todo-repeat-to-state my-org-gtd-next-action-keyword)
  (setq org-tag-alist
        (append (list (cons :startgroup nil))
                (mapcar #'my-org-gtd--make-org-alist-cons-cell
                        (append my-org-gtd-contexts
                                (list my-org-gtd-waitingfor-context)))
                (list (cons :endgroup nil))
                (list (my-org-gtd--make-org-alist-cons-cell
                       my-org-gtd-project-context))
                (list (cons my-org-gtd-somedaymaybe-tag
                            my-org-gtd-somedaymaybe-select))
                org-tag-alist))
  ;; Configure `org-gcal'
  (setq org-gcal-cancelled-todo-keyword my-org-gtd-cancelled-keyword))

(defun my-org-gtd-active-todo-search (context)
  "Return an `org' search string for next actions in CONTEXT."
  (concat context my-org-gtd-not-somedaymaybe "/!"
          my-org-gtd-next-action-keyword))

(defun my-org-gtd--insert-item (title keyword tag)
  "Insert a new `org' item with TITLE, KEYWORD, & TAG at point.
The heading must be already created."
  (when (string-empty-p title)
    (user-error "Title cannot be empty"))
  (insert title)
  (org-todo keyword)
  (org-set-tags tag))

(defun my-org-gtd-insert-project (title)
  "Insert a new project task with TITLE at point.
The heading must be already created."
  (my-org-gtd--insert-item title my-org-gtd-next-action-keyword
                           (my-org-gtd-context-tag
                            my-org-gtd-project-context)))

(defun my-org-gtd-insert-waiting-for-next-action (title)
  "Insert a new next action waiting-for task with TITLE at point.
The heading must be already created."
  (my-org-gtd--insert-item title my-org-gtd-next-action-keyword
                           (my-org-gtd-context-tag
                            my-org-gtd-waitingfor-context)))

(defun my-org-gtd-complete-item ()
  "Mark the item (a task or a project) at point as done."
  (org-todo my-org-gtd-done-keyword))

;; TODO(laurynas): README.org. What constitutes a project? Compare to org-gtd,
;; org-edna.

(provide 'my-org-gtd)
;;; my-org-gtd.el ends here
