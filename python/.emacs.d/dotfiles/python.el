;;; python.el --- Python development setup.  -*- lexical-binding: t; -*-
;;; Commentary

;; Emacs setup specific for Python development

;;; Code:

(require 'my-lib)

(dotfiles--ensure-optional-packages '(elpy))

;; `elpy'
(require 'elpy)
(setq elpy-modules (delq #'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook #'flycheck-mode)
(setq elpy-rpc-virtualenv-path 'system)
(elpy-enable)

(provide 'python)
;;; python.el ends here
