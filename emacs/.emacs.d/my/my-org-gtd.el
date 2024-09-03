;;; my-org-gtd.el --- My GTD setup in `org' -*- lexical-binding: t -*-

;; Version: 0.1
;; URL: https://github.com/laurynas-biveinis/dotfiles/
;; Package-Requires: ((emacs "24.1"))

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
  :group 'my-org-gtd)

(defvar my-org-gtd-minus-waitingfor nil
  "A substring for `org-agenda' blocks to exclude `my-org-gtd-waitingfor-tag'.
Initialized by `my-org-gtd-initialize'.")

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
  (setq my-org-gtd-minus-waitingfor (concat "-" my-org-gtd-waitingfor-tag))
  (setq org-tag-alist
        (append (list (cons :startgroup nil))
                my-org-gtd-contexts
                (list (cons my-org-gtd-waitingfor-tag
                            my-org-gtd-waitingfor-select))
                (list (cons :endgroup nil))
                org-tag-alist)))

(provide 'my-org-gtd)
;;; my-org-gtd.el ends here
