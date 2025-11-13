;;; rust.el --- Emacs setup for Rust development.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Emacs setup specific for Rust development

;;; Code:

(require 'my-lib)

;; Depends on org
(dotfiles--ensure-optional-packages '(ob-rust rust-mode rustic))

;; `rust-mode'
(require 'rust-mode)

(setq rust-mode-treesitter-derive t)

(defun dotfiles--rust-set-fill-column ()
  "Set the correct `fill-column' for `rust-mode'."
  (dotfiles--set-fill-column 100))

(add-hook 'rust-mode-hook #'dotfiles--rust-set-fill-column)

;; `rustic'
(require 'rustic)

;; Try to workaround `ob-rust' ruining `org-id-locations-file':
;; https://github.com/micanzhang/ob-rust/issues/12
(with-eval-after-load 'ob-rust-test
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations"))

(provide 'rust)
;;; rust.el ends here
