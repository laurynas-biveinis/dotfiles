;;; cpp.el --- Emacs setup for C++ development.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Emacs setup specific for C++ development

;;; Code:

(require 'my-lib)

(dotfiles--ensure-optional-packages
 '(bison-mode cmake-font-lock cmake-mode eldoc-cmake flycheck-google-cpplint))

;; `flycheck-google-cpplint'

(require 'flycheck-google-cpplint)

;; TODO(laurynas): it can be enabled without LSP as well, but there is no C/C++
;; checker chain in that case.
(defun dotfiles--lsp-flycheck-enable-cpplint ()
  "Enable cpplint for C and C++ buffers under LSP."
  (when (derived-mode-p '(c-mode c++-mode))
    (flycheck-add-next-checker 'lsp 'c/c++-googlelint)))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-flycheck-enable-cpplint)

;; `cmake-mode'

(add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))

(provide 'cpp)
;;; cpp.el ends here
