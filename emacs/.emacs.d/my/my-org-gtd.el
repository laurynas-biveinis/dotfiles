;;; my-org-gtd.el --- My GTD setup in `org' -*- lexical-binding: t -*-

;; Version: 0.1
;; URL: https://github.com/laurynas-biveinis/dotfiles/
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines

;;; Commentary:

;; This is a proto-package for my GTD implementation in `org'.

;;; Code:

(require 'org)

(defgroup my-org-gtd nil
  "Configure `org' for GTD."
  :group 'org)

(defcustom my-org-gtd-contexts nil
  "Context strings and select keys for GTD contexts.
They are exclusive and will be added as a single group to `org-tag-alist',
together with (`my-org-gtd-waitingfor-tag' . `my-org-gtd-waitingfor-select') by
`my-org-gtd-initialize'."
  :type '(repeat (cons string character))
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defvar my-org-gtd-not-waitingfor nil
  "A substring for `org-agenda' blocks to exclude `my-org-gtd-waitingfor-tag'.
Initialized by `my-org-gtd-initialize'.")

(defcustom my-org-gtd-waitingfor-tag "@waitingfor"
  "The `org' tag used for GTD waiting-for items."
  :type '(string)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-waitingfor-select ?w
  "The character to select the GTD waiting-for tag."
  :type '(character)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defvar my-org-gtd-not-project nil
  "A substring for `org-agenda' blocks to exclude `my-org-gtd-project-tag'.
Initialized by `my-org-gtd-initialize'.")

(defcustom my-org-gtd-project-tag "project"
  "The `org' tag used for GTD projects."
  :type '(string)
  :group 'my-org-gtd
  :package-version '(my-org-gtd . "0.1"))

(defcustom my-org-gtd-project-select ?p
  "The character to select the GTD project tag."
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

(defun my-org-gtd-initialize ()
  "Initialize `my-org-gtd'.
Checks `org-todo-keywords' against `my-org-gtd-next-action-keyword', initializes
`org-todo-repeat-to-state'. Constructs`org-tag-alist' while keeping any existing
values from the tag, their selection characters, and the GTD contexts
variables."
  (unless (seq-some
           (lambda (todo-sequence)
             (seq-some (lambda (keyword-and-char)
                         (when (stringp keyword-and-char)
                           (string= (car (split-string keyword-and-char "("))
                                    my-org-gtd-next-action-keyword)))
                       todo-sequence))
           org-todo-keywords)
    (user-error
     "`my-org-gtd-next-action-keyword' must be present in `org-todo-keywords'"))
  (setq my-org-gtd-not-project (concat "-" my-org-gtd-project-tag))
  (setq my-org-gtd-not-waitingfor (concat "-" my-org-gtd-waitingfor-tag))
  (setq org-todo-repeat-to-state my-org-gtd-next-action-keyword)
  (setq org-tag-alist
        (append (list (cons :startgroup nil))
                my-org-gtd-contexts
                (list (cons my-org-gtd-waitingfor-tag
                            my-org-gtd-waitingfor-select))
                (list (cons :endgroup nil))
                (list (cons my-org-gtd-project-tag
                            my-org-gtd-project-select))
                org-tag-alist)))

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
                           my-org-gtd-project-tag))

(defun my-org-gtd-insert-waiting-for-next-action (title)
  "Insert a new next action waiting-for task with TITLE at point.
The heading must be already created."
  (my-org-gtd--insert-item title my-org-gtd-next-action-keyword
                           my-org-gtd-waitingfor-tag))

;; TODO(laurynas): README.org. What constitutes are project? Compare to org-gtd,
;; org-edna.

(provide 'my-org-gtd)
;;; my-org-gtd.el ends here
