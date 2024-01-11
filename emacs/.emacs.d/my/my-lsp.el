;;; my-lsp.el --- Language Server Protocol.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure `lsp-mode' and related packages, as well as `topsy', which, while
;; not related to LSP, shares the same window header line space.

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
                                "-use-dirty-headers"))
;; TODO(laurynas): LSP-formatting yanked region is nice, but it formats
;; surroundings of the region too, which is very annoying in case of i.e. C++
;; and a missing semicolon at the end of the region.
(setq lsp-enable-indentation nil)
;; TODO(laurynas): C and C++ docs render with interspersed random backslashes:
;; https://emacs.stackexchange.com/questions/55056/how-to-improve-eldoc-lsp-mode-output-with-c-c-comments
;; https://github.com/jrblevin/markdown-mode/issues/409
(setq lsp-eldoc-render-all t)
(setq lsp-before-save-edits nil)
(setq lsp-restart 'auto-restart)
(setq lsp-semantic-tokens-enable t)
(setq lsp-headerline-breadcrumb-enable t)

(defun dotfiles--lsp-mode-line ()
  "Construct the mode line text for `lsp-mode'."
  (if (lsp-workspaces)
      " LSP"
    " !LSP"))

(setf (alist-get 'lsp-mode minor-mode-alist)
      '((:eval (dotfiles--lsp-mode-line))))

(require 'lsp-headerline)
(setq lsp-headerline-breadcrumb-segments '(project file symbols))

(require 'lsp-diagnostics)
(add-hook 'lsp-managed-mode-hook #'lsp-diagnostics-mode)

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

;; Integration with TRAMP: do not flycheck too eagerly
(defun dotfiles--lsp-tramp-flycheck-reduce ()
  "Tune down Flycheck eagerness in `lsp-mode' for TRAMP buffers."
  (setq-local flycheck-check-syntax-automatically '(save idle-change new-line)))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-tramp-flycheck-reduce)

;; Breaks idempotence of this file, which I am not using anyway.
(if (fboundp #'lsp-format-defun)
    (display-warning
     'dotfiles
     "‘lsp-format-defun’ defined by ‘lsp-mode’: fix it in setup.el!"
     :warning))

(defvar-local dotfiles--use-lsp-indent nil
  "If t, use LSP instead of cc-mode for indentation in this buffer.")

(require 'cc-cmds)

(defun lsp-format-defun ()
  "Format the current `cc-mode' defun using LSP."
  (interactive "p" prog-mode)
  (save-mark-and-excursion
    (c-mark-function)
    (lsp-format-region (region-beginning) (region-end))))

(defun dotfiles--lsp-format-defun-advice (orig-fun)
  "Format the defun using LSP with a fallback to ORIG-FUN (‘c-indent-defun’)."
  (if dotfiles--use-lsp-indent (lsp-format-defun)
    (funcall orig-fun)))

(defun dotfiles--lsp-format-region-advice (orig-fun &rest args)
  "Format the region (ARGS) using LSP with a fallback to ORIG-FUN."
  (if dotfiles--use-lsp-indent (apply #'lsp-format-region args)
    (apply orig-fun args)))

(defun dotfiles--lsp-disable-electric-keys ()
  "Disable any electric keys if LSP on type formatting is enabled."
  ;; Using internal LSP symbols is not ideal but I don't see an alternative.
  (when (and lsp-enable-on-type-formatting (lsp--capability
                                            :documentOnTypeFormattingProvider))
    (electric-layout-mode -1)
    ;; It seems it's OK to call this in non-cc-mode buffers too
    (electric-pair-local-mode -1)
    (c-toggle-electric-state -1)))

(defun dotfiles--lsp-replace-cc-mode-indent ()
  "Make ‘c-indent-defun’ and ‘c-indent-region’ use LSP.

This is only done if LSP is configured to indent and the language server
supports range formatting."
  (when (and lsp-enable-indentation
             ;; Using internal LSP symbols is not ideal but I don't see an
             ;; alternative.
             (or (lsp--capability :documentRangeFormattingProvider)
                 (lsp--registered-capability "textDocument/rangeFormatting")))
    (setq-local dotfiles--use-lsp-indent t)
    (advice-add #'c-indent-defun :around #'dotfiles--lsp-format-defun-advice)
    (advice-add #'c-indent-region :around #'dotfiles--lsp-format-region-advice)))

(defun dotfiles--lsp-restore-cc-mode-indent (_lsp_workspace)
  "Make ‘c-indent-defun’ and ‘c-indent-region’ no longer use LSP."
  (setq-local dotfiles--use-lsp-indent nil))

(defun dotfiles--lsp-disable-eldoc ()
  "Disable eldoc for LSP."
  (eldoc-mode -1))

(defun dotfiles--lsp-uninitialization (_lsp_workspace)
  "General cleanup after LSP uninitialization."
  (electric-layout-mode)
  ;; It seems it's OK to call this in non-cc-mode buffers too
  (c-toggle-electric-state)
  (electric-pair-local-mode)
  (eldoc-mode))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-replace-cc-mode-indent)
(add-hook 'lsp-after-open-hook #'yas-minor-mode-on)
(add-hook 'lsp-after-open-hook #'dotfiles--lsp-disable-electric-keys)
(add-hook 'lsp-after-open-hook #'dotfiles--lsp-disable-eldoc)

(add-hook 'lsp-after-uninitialized-functions
          #'dotfiles--lsp-restore-cc-mode-indent)
(add-hook 'lsp-after-uninitialized-functions #'dotfiles--lsp-uninitialization)

;;; Setup `topsy', make `lsp-mode' play nicely with it.
(require 'topsy)

(defun dotfiles--lsp-deferred-or-topsy ()
  "Run `lsp-deferred' if it's a supported mode, otherwise enable `topsy-mode'."
  (if (derived-mode-p 'emacs-lisp-mode 'makefile-bsdmake-mode
                      'makefile-gmake-mode 'asm-mode)
      (topsy-mode)
    (lsp-deferred)))

(add-hook 'prog-mode-hook #'dotfiles--lsp-deferred-or-topsy)

;;; lsp-mode clangd setup
(defconst lsp-clients-clangd-tramp-executable "clangd")
(defun lsp-clients--clangd-tramp-command ()
  "Generate the clangd over Tramp startup command."
  `(,lsp-clients-clangd-tramp-executable ,@lsp-clients-clangd-args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection
                                   'lsp-clients--clangd-tramp-command)
                  :major-modes '(c-mode c++-mode objc-mode)
                  :priority -1
                  :server-id 'clangd-tramp
                  :remote? t))

;; https://clang.llvm.org/extra/clangd/Features.html#formatting -
;; "Format-as-you-type is experimental and doesn’t work well yet."
(defun dotfiles--lsp-turn-off-on-type-formatting ()
  "Turn off LSP on type formatting for the current buffer."
  (setq-local lsp-enable-on-type-formatting nil))

(add-hook 'c-mode-common-hook #'dotfiles--lsp-turn-off-on-type-formatting)

;;; Rust in `lsp-mode'
(require 'lsp-rust)
(setq lsp-rust-analyzer-cargo-watch-command "clippy")

(require 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)

;;; Integrate with `project.el' / `projectile'
(setq lsp-auto-guess-root t)

(require 'helm-lsp)

(defun dotfiles--lsp-bind-helm-lsp-workspace-symbol ()
  "Rebind C-M-. to helm-lsp-workspace-symbol."
  (define-key lsp-mode-map [remap xref-find-apropos]
    #'helm-lsp-workspace-symbol))

(defun dotfiles--lsp-unbind-helm-lsp-workspace-symbol (_lsp_workspace)
  "Restore global C-M-. binding."
  (define-key lsp-mode-map [remap xref-find-apropos] #'xref-find-apropos))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-bind-helm-lsp-workspace-symbol)
(add-hook 'lsp-after-uninitialized-functions
          #'dotfiles--lsp-unbind-helm-lsp-workspace-symbol)

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
