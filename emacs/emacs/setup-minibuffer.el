;;; setup-minibuffer.el --- minibuffer config.  -*- lexical-binding: t; -*-
;;; Commentary:
;; minibuffer configuration made redundant by Helm.
;;; Code:

(defun dotfiles-kill-buffer-if-exists (buffer)
  "Kill the BUFFER if it exists."
  (if (buffer-live-p buffer)
      (kill-buffer buffer)))

(defun dotfiles--kill-completion-buffer ()
  "Kill completion buffer, if it exists."
  (dotfiles-kill-buffer-if-exists "*Completions*"))

(add-hook 'minibuffer-exit-hook #'dotfiles--kill-completion-buffer)

(provide 'setup-minibuffer)
;;; setup-minibuffer.el ends here
