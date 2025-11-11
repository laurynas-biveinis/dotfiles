;;; my-lsp.el --- Language Server Protocol.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure `lsp-mode'. This includes setup for 'clangd' for C++ development
;; and `lsp-ui' package. It also includes configuration for `topsy', which, while
;; not related to LSP, shares the same window header line space.
;;
;; Like in the rest of my personal configuration, all features (packages and
;; external tools) are assumed to exist, because this is a part of my dotfiles
;; repo where the needed packages are committed too. Thus, no error handling,
;; and no need to ensure compatibility with different Emacs or package versions.

;;; Code:

(require 'lsp-mode)
(require 'lsp-clangd)
(setq lsp-clients-clangd-args '("--all-scopes-completion"
                                "--background-index"
                                "--clang-tidy"
                                "--cross-file-rename"
                                "--header-insertion=never"
                                "--enable-config"
                                "-j=5"
                                "--pch-storage=memory"
                                ;; So that edited but not yet saved header
                                ;; contents are used instead of the disk version
                                "-use-dirty-headers"))
(setq lsp-eldoc-render-all t)
(setq lsp-before-save-edits nil)
(setq lsp-restart 'auto-restart)
(setq lsp-semantic-tokens-enable t)
(setq lsp-headerline-breadcrumb-enable t)

(defun dotfiles--lsp-mode-line ()
  "Construct the mode line text for `lsp-mode'."
  (declare (ftype (function () string))
           (important-return-value t))
  (if (lsp-workspaces)
      " LSP"
    " !LSP"))

(setf (alist-get 'lsp-mode minor-mode-alist)
      '((:eval (dotfiles--lsp-mode-line))))

(require 'lsp-headerline)
(setq lsp-headerline-breadcrumb-segments '(project file symbols))

(require 'lsp-diagnostics)
(add-hook 'lsp-managed-mode-hook #'lsp-diagnostics-mode)

;; TODO(laurynas): once the inlay display becomes less jerky, enable it.
;; (setq lsp-inlay-hint-enable t)
;; (add-hook 'lsp-managed-mode-hook #'lsp-inlay-hints-mode)

(require 'lsp-modeline)
(add-hook 'lsp-managed-mode-hook #'lsp-modeline-diagnostics-mode)

(require 'lsp-ui)
(setq lsp-ui-sideline-ignore-duplicate t)
(setq lsp-ui-sideline-show-symbol nil)
(setq lsp-ui-peek-peek-height 30)
(setq lsp-ui-sideline-actions-kind-regex ".*")

(require 'lsp-ui-doc)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature t)

;; `lsp-ui' and `lsp-ui-doc' integration: avoid doc popups hiding reference
;; popups by hiding the former.
(advice-add #'lsp-ui-peek-find-references :before #'lsp-ui-doc-hide)

(add-hook 'lsp-after-open-hook #'yas-minor-mode-on)

;;; Setup `topsy', make `lsp-mode' play nicely with it.
(require 'topsy)

(defun dotfiles--lsp-deferred-or-topsy ()
  "Run `lsp-deferred' if it's a supported mode, otherwise enable `topsy-mode'."
  (if (derived-mode-p 'emacs-lisp-mode 'makefile-bsdmake-mode
                      'makefile-gmake-mode 'asm-mode)
      (topsy-mode)
    (lsp-deferred)))

(add-hook 'prog-mode-hook #'dotfiles--lsp-deferred-or-topsy)

;; https://clang.llvm.org/extra/clangd/Features.html#formatting -
;; "Format-as-you-type is experimental and doesn’t work well yet."
(defun dotfiles--lsp-turn-off-on-type-formatting ()
  "Turn off LSP on type formatting for the current buffer."
  (setq-local lsp-enable-on-type-formatting nil))

(add-hook 'c-mode-common-hook #'dotfiles--lsp-turn-off-on-type-formatting)

;;; Rust in `lsp-mode'
(require 'lsp-rust)
(setq lsp-rust-analyzer-cargo-watch-command "clippy")

;;; Python in `lsp-mode'
(require 'lsp-pylsp)
(setq lsp-pylsp-plugins-pylint-enabled t)
(setq lsp-pylsp-plugins-pycodestyle-enabled t)

(require 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)

;;; Integrate with `project.el' / `projectile'
(setq lsp-auto-guess-root t)

;;; Integrate with `which-key'
(defun dotfiles--lsp-enable-which-key ()
  "Enable `lsp-mode' integration with `which-key' for all major modes."
  (lsp-enable-which-key-integration t))

(add-hook 'lsp-mode-hook #'dotfiles--lsp-enable-which-key)

;; Integrate with with Flycheck `sh-shellcheck' checker. It will become
;; redundant if bash-language-server implements
;; https://github.com/bash-lsp/bash-language-server/issues/104
(defun dotfiles--lsp-flycheck-enable-shellcheck ()
  "Enable Shellcheck for shell buffers under LSP."
  (when (derived-mode-p 'sh-mode)
    (flycheck-add-next-checker 'lsp 'sh-shellcheck)))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-flycheck-enable-shellcheck)

(provide 'my-lsp)
;;; my-lsp.el ends here
