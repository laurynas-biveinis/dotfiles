;;; my-column-limit.el --- fill column and co.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This configure fill column and all the related things, such as indicating
;; too-long lines.

;;; Code:

(defconst dotfiles--column-limit 80
  "The column for line filling and any indicator showing.")

(defconst dotfiles--long-line-warning-column (+ dotfiles--column-limit 1)
  "The column at which the line is too long.")

(setq-default fill-column dotfiles--column-limit)

;;; whitespace-mode
(require 'whitespace)
(setq whitespace-line-column dotfiles--long-line-warning-column)
(add-to-list 'whitespace-style 'face)
(add-to-list 'whitespace-style 'lines-tail)

;; display-fill-column-indicator
(require 'display-fill-column-indicator)
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
              (equal " *Org Select*" buf-name)
              (string-prefix-p "*helm" buf-name))
      (setq-local display-fill-column-indicator nil))))

(add-hook 'display-fill-column-indicator-mode-hook
          #'dotfiles--maybe-disable-fci)

(provide 'my-column-limit)
;;; my-column-limit.el ends here
