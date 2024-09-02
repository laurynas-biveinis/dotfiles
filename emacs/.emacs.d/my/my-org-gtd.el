;;; my-org-gtd.el --- my GTD setup in `org'. -*- lexical-binding: t -*-

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
together with (`my-org-gtd-waitingfor-tag' . `my-org-gtd-waitingfor-select')"
  :type '(repeat (cons string character))
  :group 'my-org-gtd)

(defcustom my-org-gtd-waitingfor-tag "@waitingfor"
  "The `org' tag used for GTD waiting-for items."
  :type '(string)
  :group 'my-org-gtd)

(defcustom my-org-gtd-waitingfor-select ?w
  "The character to select the GTD waiting-for tag."
  :type '(character))

(defun my-org-gtd-initialize ()
  "Initialize `my-org-gtd'.
Constructs `org-tag-alist' from `my-org-gtd-contexts',
`my-org-gtd-waitingfor-tag', and `my-org-gtd-waitingfor-select'. Keeps any
existing values."
  (add-to-list
   'org-tag-alist
   (append '(:startgroup)
           my-org-gtd-contexts
           `((,my-org-gtd-waitingfor-tag . ,my-org-gtd-waitingfor-select))
           '(:endgroup))))

(provide 'my-org-gtd)
;;; my-org-gtd.el ends here
