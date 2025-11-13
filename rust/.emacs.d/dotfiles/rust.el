;;; rust.el --- Emacs setup for Rust development.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Emacs setup specific for Rust development

;;; Code:

(require 'my-lib)

(dotfiles--ensure-optional-packages '(rust-mode rustic))

;; `rust-mode'
(require 'rust-mode)

(setq rust-mode-treesitter-derive t)

(defun dotfiles--rust-set-fill-column ()
  "Set the correct `fill-column' for `rust-mode'."
  (dotfiles--set-fill-column 100))

(add-hook 'rust-mode-hook #'dotfiles--rust-set-fill-column)

;; `rustic'
(require 'rustic)

(provide 'rust)
;;; rust.el ends here
