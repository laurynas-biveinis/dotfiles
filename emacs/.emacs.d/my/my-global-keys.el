;;; my-global-keys.el --- global keybindings.  -*- lexical-binding: t; -*-
;;; Commentary:

;; My global personal user keybindings.
;;
;; Like in the rest of my personal configuration, all features (packages and
;; external tools) are assumed to exist, because this is a part of my dotfiles
;; repo where the needed packages are committed too. Thus, no error handling,
;; and no need to ensure compatibility with different Emacs or package versions.
;;
;; Some keybindings are declared elsewhere when it was not practical to collect
;; them here.

;;; Code:

(require 'ol)
(require 'org-clock)
(require 'org-roam-node)
(require 'magit-status)
(require 'deadgrep)
(require 'mu4e)
(declare-function my-secrets "secrets" ())
(declare-function my-gtd "secrets" ())
(declare-function my-main-agenda "secrets" ())
(declare-function end-of-line-and-newline-and-indent "my-edit" ())
(declare-function my-switch-to-scratch "my-setup" ())

;; Editing
(global-set-key (kbd "<M-RET>") #'end-of-line-and-newline-and-indent)

;; Navigation
(global-set-key (kbd "<home>") #'move-beginning-of-line)
(global-set-key (kbd "<end>") #'move-end-of-line)

;; Window resizing
(global-set-key (kbd "C-S-<up>") #'enlarge-window)
(global-set-key (kbd "C-S-<down>") #'shrink-window)
(global-set-key (kbd "C-S-<right>") #'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<left>") #'shrink-window-horizontally)

;; `org-mode'
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c C-x C-o") #'org-clock-out)
(global-set-key (kbd "C-c C-x C-j") #'org-clock-goto)

;; `org-roam-mode'
(global-set-key (kbd "C-c n f") #'org-roam-node-find)

;; Invoking functionality
(global-set-key (kbd "<f5>") #'deadgrep)
(global-set-key (kbd "<f7>") #'my-main-agenda)
(global-set-key (kbd "<f8>") #'my-secrets)
(global-set-key (kbd "<f9>") #'my-gtd)
(global-set-key (kbd "<f12>") #'my-switch-to-scratch)
(global-set-key (kbd "C-c m") #'mu4e)

;; `magit'
(global-set-key (kbd "C-x g") #'magit-status)

(provide 'my-global-keys)
;;; my-global-keys.el ends here
