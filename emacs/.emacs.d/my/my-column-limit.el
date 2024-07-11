;;; my-column-limit.el --- fill column and co.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This configures fill column and all the related things, such as indicating
;; too-long lines.
;;
;; This is a part of my personal configuration. No need to ensure compatibility
;; with different Emacs or package versions.

;;; Code:
(require 'whitespace)
(require 'display-fill-column-indicator)

(defconst dotfiles--column-limit 80
  "The column for line filling and any indicator showing.")

(defconst dotfiles--long-line-warning-column (+ dotfiles--column-limit 1)
  "The column at which the line is too long.")

(setq-default fill-column dotfiles--column-limit)

;; Callers outside this file. It does not make `dotfiles--column-limit' and
;; `dotfiles--long-line-warning-column' dynamic, rather, it is used in places
;; that must have a separate setting from these two variables.
(defun dotfiles--set-fill-column (column)
  "Set `fill-column' and related vars to COLUMN."
  (setq-local fill-column column
              whitespace-line-column (+ column 1)
              display-fill-column-indicator-column (+ column 1)))

;;; `whitespace-mode', whose main configuration is elsewhere. Here we only add
;;; too-long line stuff.
(setq whitespace-line-column dotfiles--long-line-warning-column)
(add-to-list 'whitespace-style 'face)
(add-to-list 'whitespace-style 'lines-tail)

;; display-fill-column-indicator
(setq-default display-fill-column-indicator-column
              dotfiles--long-line-warning-column)

(global-display-fill-column-indicator-mode 1)

;; 28.1 introduces `global-display-fill-column-indicator-modes' but being purely
;; mode-based it cannot replace buffer name checking.
(defun dotfiles--maybe-disable-fci ()
  "Selectively disable `display-fill-column-indicator' in some buffers."
  (let ((buf-name (buffer-name)))
    (when (or buffer-read-only (derived-mode-p 'org-agenda-mode)
              (equal " *Agenda Commands*" buf-name)
              (equal " *Org Select*" buf-name))
      (setq-local display-fill-column-indicator nil))))

(add-hook 'display-fill-column-indicator-mode-hook
          #'dotfiles--maybe-disable-fci)

(add-hook 'prog-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook #'turn-on-auto-fill)

(provide 'my-column-limit)
;;; my-column-limit.el ends here
