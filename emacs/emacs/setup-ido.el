;;; setup-ido.el --- IDO config.  -*- lexical-binding: t; -*-
;;; Commentary:
;; Config made redundant by Helm. Useful shortcuts to learn when it's enabled:
;; C-. & C-,
;;; Code:

(declare-function dotfiles-kill-buffer-if-exists "setup.el" (buffer))

;; Better C-x b menu by IDO mode. Alternatives: helm, ivy
(require 'ido)
(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-default-buffer-method 'selected-window)
(setq ido-use-filename-at-point 'guess)

(defun dotfiles--kill-ido-completion-buffer ()
  "Kill IDO completion buffer, if it exists."
  (dotfiles-kill-buffer-if-exists "*Ido Completions*"))

(add-hook 'minibuffer-exit-hook #'dotfiles--kill-ido-completion-buffer)

(provide 'setup-ido)
;;; setup-ido.el ends here
