(put 'upcase-region 'disabled nil)
;; `s' is a part of package-selected-packages because the ecosystem is broken:
;; 1) not enough package maintainers care about melpa-stable, only about melpa,
;; making it infeasible to use known-good melpa-stable versions due to forced
;; upgrades to melpa, including dependencies
;; 2) a repository cannot hold multiple package versions
;; So, for `s', `lsp-mode' uses `s-replace-regexp', which was introduced after
;; the latest stable release, which happened only over two years ago. /s
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages
   '(mcp-server-lib elisp-mcp-dev org-autotask-mcp elisp-autofmt simple-httpd elisp-lint org-autotask rustic forge magit magit-todos shfmt indent-bars elpy org-gcal pr-review git-modes ob-rust plantuml-mode emacsql-sqlite-builtin difftastic wgrep-deadgrep prism org-sticky-header topsy org-roam-ui org-roam fancy-compilation flycheck-status-emoji flycheck-google-cpplint git-messenger beginend cheat-sh info-colors grab-mac-link stripe-buffer beacon lua-mode wgrep lsp-treemacs s calfw-ical calfw calfw-org gcmh which-key keyfreq company-box yasnippet iedit page-break-lines xterm-color eldoc-cmake projectile vterm deadgrep all-the-icons-dired rich-minority git-gutter-fringe aggressive-indent lsp-ui lsp-mode flycheck dispwatch org-analyzer undo-tree yaml-mode markdown-mode ssh ssh-config-mode bison-mode cmake-font-lock cmake-mode solarized-theme wakatime-mode exec-path-from-shell))
 '(package-vc-selected-packages
   '((mcp-server-lib :vc-backend Git :url "https://github.com/laurynas-biveinis/mcp-server-lib.el/")
     (elisp-mcp-dev :vc-backend Git :url "https://github.com/laurynas-biveinis/elisp-mcp-dev")
     (org-autotask-mcp :vc-backend Git :url "https://github.com/laurynas-biveinis/org-autotask-mcp")
     (org-autotask :vc-backend Git :url "https://github.com/laurynas-biveinis/org-autotask/")))
 '(safe-local-variable-values
   '((package-lint-main-file . "mcp-server-lib.el")
     (elisp-lint-indent-specs
      (org-mcp-test--with-enabled . defun))
     (elisp-lint-indent-specs
      (elisp-dev-mcp-test-with-server . defun)
      (elisp-dev-mcp-test-with-bytecode-file . defun))
     (elisp-lint-indent-specs
      (mcp-server-lib-test--with-server . defun)
      (mcp-server-lib-test--with-tools . 1)
      (mcp-server-lib-test--successful-req . defun)
      (mcp-server-lib-test--verify-req-success . defun)
      (mcp-server-lib-test--with-error-tracking . 1)
      (cl-defstruct))
     (eval and buffer-file-name
           (not
            (eq major-mode 'package-recipe-mode))
           (or
            (require 'package-recipe-mode nil t)
            (let
                ((load-path
                  (cons "../package-build" load-path)))
              (require 'package-recipe-mode nil t)))
           (package-recipe-mode))
     (elisp-lint-indent-specs
      (mcp-server-lib-test--with-server . defun)
      (mcp-server-lib-test--with-tools . 1))
     (elisp-lint-ignored-validators "checkdoc")
     (elisp-lint-indent-specs
      (mcp-test--with-server . defun)
      (mcp-test--with-tools . 1))
     (elisp-lint-indent-specs
      (mcp-test--with-server . defun))
     (elisp-lint-indent-specs
      (mcp-test-with-server . defun))
     (elisp-lint-indent-specs
      (elisp-dev-mcp-test-with-server . defun))
     (elisp-lint-indent-specs
      (org-autotask-with-org-node-with-url . 1)
      (org-autotask--test-fixture . 1)
      (org-autotask--buffer-test . 1)
      (org-autotask--clock-in-action-test . 2)
      (org-autotask--with-replaced-action-fn . 3)
      (org-autotask--with-temp-org-agenda-files . defun))
     (elisp-lint-ignored-validators "package-lint")
     (eval setq-local undo-tree-auto-save-history nil)
     (eval setq-local backup-inhibited t)
     (magit-todos-exclude-globs ".git/" "arcanist/" "extra/")
     (magit-todos-exclude-globs "emacs/.emacs.d/abbrev_defs" "emacs/.emacs.d/elpa/*" "zsh/.p10k.zsh")
     (org-emphasis-alist)
     (eval setq-local ispell-personal-dictionary
           (expand-file-name ".ispell.dict"
                             (file-name-directory
                              (let
                                  ((d
                                    (dir-locals-find-file "./")))
                                (if
                                    (stringp d)
                                    d
                                  (car d))))))
     (org-fontify-emphasized-text)
     (c-tab-always-indent t)
     (compilation-read-command)
     (projectile-generic-command . "(cd ce && git ls-files -zc --exclude-standard --recurse-submodules | gsed -z 's/^/ce\\//g'); (cd ee && git ls-files -zc --exclude-standard --recurse-submodules | gsed -z 's/^/ee\\//g')"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
