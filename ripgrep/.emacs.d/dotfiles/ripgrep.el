;;; ripgrep.el --- Emacs setup for ripgrep.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Emacs setup specific for ripgrep

;;; Code:

(require 'my-lib)

(dotfiles--ensure-optional-packages
 '(deadgrep wgrep-deadgrep))

;; `deadgrep' alternatives: rg.el, ripgrep.el, counsel/helm, etc.
(require 'deadgrep)

(global-set-key (kbd "<f5>") #'deadgrep)

(require 'wgrep-deadgrep)
(add-hook 'deadgrep-finished-hook #'wgrep-deadgrep-setup)

(provide 'ripgrep)
;;; ripgrep.el ends here
