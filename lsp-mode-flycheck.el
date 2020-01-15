(add-to-list 'package-archives '("melpa-stable" .
                                "https://stable.melpa.org/packages/"))
(package-refresh-contents)
(package-install 'flycheck)

(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc c/c++-cppcheck))

(global-flycheck-mode)

(setq lsp-enable-snippet nil)
(setq lsp-eldoc-render-all t)
(setq lsp-before-save-edits nil)
(setq lsp-restart 'auto-restart)

(setq lsp-prefer-flymake nil)

(require 'lsp-ui)
(setq lsp-ui-sideline-ignore-duplicate t)
(setq lsp-ui-sideline-show-symbol nil)

(require 'lsp-ui-doc)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-doc-position 'top)


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

(add-hook 'prog-mode-hook #'lsp-deferred)

(setq enable-remote-dir-locals t)

(require 'tramp)
;; The default level 3 is too noisy with showing each file shipped
(setq tramp-verbose 4)
;; Not sure I care about scp overhead in 2020. Also, maybe this will help with
;; duplicated sshx/scpx paths in lsp-mode cache.
(setq tramp-copy-size-limit nil)
(setq tramp-default-method "scpx")
(setq tramp-completion-reread-directory-timeout t)
