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
   '(aggressive-indent all-the-icons-dired beacon beginend bison-mode calfw
                       calfw-ical calfw-org cheat-sh cmake-font-lock cmake-mode
                       company-box deadgrep difftastic dispwatch eldoc-cmake
                       elisp-autofmt elisp-dev-mcp elisp-lint elpy
                       emacsql-sqlite-builtin exec-path-from-shell
                       fancy-compilation flycheck flycheck-google-cpplint
                       flycheck-status-emoji forge gcmh git-gutter-fringe
                       git-messenger git-modes grab-mac-link iedit indent-bars
                       info-colors keyfreq lsp-mode lsp-treemacs lsp-ui lua-mode
                       magit magit-todos markdown-mode mcp-server-lib ob-rust
                       org-analyzer org-autotask org-gcal org-mcp org-roam
                       org-roam-ui org-sticky-header page-break-lines
                       plantuml-mode pr-review prism projectile relint
                       rich-minority rustic s shfmt simple-httpd solarized-theme
                       ssh ssh-config-mode stripe-buffer undo-tree vterm w3m
                       wakatime-mode wgrep wgrep-deadgrep xterm-color yaml-mode
                       yasnippet))
 '(package-vc-selected-packages
   '((org-mcp :vc-backend Git :url
              "https://github.com/laurynas-biveinis/org-mcp.git")
     (org-autotask :vc-backend Git :url
                   "https://github.com/laurynas-biveinis/org-autotask/")))
 '(safe-local-variable-values
   '((elisp-lint-indent-specs (org-mcp--modify-and-save . 3)
                              (org-mcp--with-uri-prefix-dispatch . 1)
                              (org-mcp-test--assert-error-and-file . 1)
                              (org-mcp-test--with-enabled . defun)
                              (org-mcp-test--with-config . 1)
                              (org-mcp-test--with-temp-org-file . 2)
                              (org-mcp-test--with-add-todo-setup . 2)
                              (org-mcp-test--with-id-tracking . 2)
                              (org-mcp-test--with-id-setup . 2)
                              (org-mcp-test--get-tag-config-and-check . defun)
                              (mcp-server-lib-ert-with-server . defun))
     (elisp-lint-indent-specs (org-mcp--with-org-file-buffer . 2)
                              (org-mcp--with-uri-prefix-dispatch . 1)
                              (org-mcp-test--assert-error-and-file . 1)
                              (org-mcp-test--with-enabled . defun)
                              (org-mcp-test--with-config . 1)
                              (org-mcp-test--with-temp-org-file . 2)
                              (org-mcp-test--with-add-todo-setup . 2)
                              (org-mcp-test--with-id-tracking . 2)
                              (org-mcp-test--with-id-setup . 2)
                              (org-mcp-test--get-tag-config-and-check . defun)
                              (mcp-server-lib-ert-with-server . defun))
     (elisp-lint-indent-specs (org-mcp--with-org-file-buffer . 2)
                              (org-mcp--with-uri-prefix-dispatch . 1)
                              (org-mcp-test--with-enabled . defun)
                              (org-mcp-test--with-config . 1)
                              (org-mcp-test--with-temp-org-file . 2)
                              (org-mcp-test--with-add-todo-setup . 2)
                              (org-mcp-test--with-id-tracking . 2)
                              (org-mcp-test--with-id-setup . 2)
                              (mcp-server-lib-ert-with-server . defun))
     (elisp-lint-indent-specs (org-mcp--with-org-file-buffer . 2)
                              (org-mcp--with-uri-prefix-dispatch . 1)
                              (org-mcp-test--with-enabled . defun)
                              (org-mcp-test--with-config . 1)
                              (org-mcp-test--with-temp-org-file . 2)
                              (org-mcp-test--with-add-todo-setup . 2)
                              (org-mcp-test--with-id-setup . 1)
                              (org-mcp-test--refile-and-verify . 2)
                              (org-mcp-test--refile-cross-file-and-verify . 2)
                              (mcp-server-lib-ert-with-server . defun))
     (elisp-lint-indent-specs (org-mcp--with-org-file-buffer . 2)
                              (org-mcp--with-uri-prefix-dispatch . 1)
                              (org-mcp-test--with-enabled . defun)
                              (org-mcp-test--with-config . 1)
                              (org-mcp-test--with-temp-org-file . 2)
                              (org-mcp-test--with-add-todo-setup . 2)
                              (org-mcp-test--with-id-setup . 1)
                              (org-mcp-test--refile-and-verify . 2)
                              (mcp-server-lib-ert-with-server . defun))
     (elisp-lint-indent-specs (if-let* . 1) (if-let . 1)
                              (mcp-server-lib-test--with-server . 0)
                              (mcp-server-lib-test--with-servers . 1)
                              (mcp-server-lib-test--with-tools . 1)
                              (mcp-server-lib-test--register-tool . 1)
                              (mcp-server-lib-test--with-resources . 1)
                              (mcp-server-lib-test--with-resource . 2)
                              (mcp-server-lib-test--with-resource-template . 2)
                              (mcp-server-lib-test--with-resource-templates . 1)
                              (mcp-server-lib-test--with-undefined-function . 1)
                              (mcp-server-lib-test--with-request . defun)
                              (mcp-server-lib-test--with-error-tracking . 1)
                              (mcp-server-lib-test--check-resource-read-error
                               . 0)
                              (mcp-server-lib-ert-with-metrics-tracking . 1)
                              (mcp-server-lib-ert-verify-req-success . defun)
                              (mcp-server-lib--with-hash-table-entries . 2)
                              (cl-defstruct))
     (elisp-lint-indent-specs (if-let* . 1) (if-let . 1)
                              (mcp-server-lib-test--with-server . 0)
                              (mcp-server-lib-test--with-tools . 1)
                              (mcp-server-lib-test--register-tool . 1)
                              (mcp-server-lib-test--with-resources . 1)
                              (mcp-server-lib-test--register-resource . 2)
                              (mcp-server-lib-test--register-resource-template
                               . 2)
                              (mcp-server-lib-test--with-resource-templates . 1)
                              (mcp-server-lib-test--with-undefined-function . 1)
                              (mcp-server-lib-test--with-request . defun)
                              (mcp-server-lib-test--with-error-tracking . 1)
                              (mcp-server-lib-test--check-resource-read-error
                               . 0)
                              (mcp-server-lib-ert-with-metrics-tracking . 1)
                              (mcp-server-lib-ert-verify-req-success . defun)
                              (mcp-server-lib--with-hash-table-entries . 2)
                              (cl-defstruct))
     (elisp-lint-indent-specs (org-mcp--with-org-file-buffer . 2)
                              (org-mcp--with-uri-prefix-dispatch . 1)
                              (org-mcp-test--with-enabled . defun)
                              (org-mcp-test--with-config . 1)
                              (org-mcp-test--with-temp-org-file . 2)
                              (org-mcp-test--with-add-todo-setup . 2)
                              (org-mcp-test--with-id-setup . 1)
                              (mcp-server-lib-ert-with-server . defun))
     (elisp-lint-indent-specs (org-mcp-test--with-enabled . defun)
                              (org-mcp-test--with-config . 1)
                              (org-mcp-test--with-temp-org-file . 2)
                              (org-mcp-test--with-add-todo-setup . 2)
                              (org-mcp-test--with-id-setup . 1)
                              (mcp-server-lib-ert-with-server . defun))
     (elisp-lint-indent-specs (org-mcp-test--with-enabled . defun)
                              (org-mcp-test--with-config . 1)
                              (org-mcp-test--with-temp-org-file . 2)
                              (org-mcp-test--with-add-todo-setup . 2)
                              (mcp-server-lib-ert-with-server . defun))
     (elisp-lint-indent-specs (org-mcp-test--with-enabled . defun)
                              (org-mcp-test--with-config . 1)
                              (org-mcp-test--with-temp-org-file . 2)
                              (mcp-server-lib-ert-with-server . defun))
     (elisp-lint-indent-specs (if-let* . 1) (if-let . 1)
                              (mcp-server-lib-test--with-server . 0)
                              (mcp-server-lib-test--with-tools . 1)
                              (mcp-server-lib-test--register-tool . 1)
                              (mcp-server-lib-test--with-resources . 1)
                              (mcp-server-lib-test--register-resource . 2)
                              (mcp-server-lib-test--register-resource-template
                               . 2)
                              (mcp-server-lib-test--with-resource-templates . 1)
                              (mcp-server-lib-test--with-undefined-function . 1)
                              (mcp-server-lib-test--with-request . defun)
                              (mcp-server-lib-test--with-error-tracking . 1)
                              (mcp-server-lib-test--check-resource-read-error
                               . 0)
                              (mcp-server-lib-ert-with-metrics-tracking . 1)
                              (mcp-server-lib-ert-verify-req-success . defun)
                              (cl-defstruct))
     (elisp-lint-indent-specs (if-let* . 1) (if-let . 1)
                              (mcp-server-lib-test--with-server . 0)
                              (mcp-server-lib-test--with-tools . 1)
                              (mcp-server-lib-test--register-tool . 1)
                              (mcp-server-lib-test--with-resources . 1)
                              (mcp-server-lib-test--register-resource . 2)
                              (mcp-server-lib-test--register-resource-template
                               . 2)
                              (mcp-server-lib-test--with-resource-templates . 1)
                              (mcp-server-lib-test--with-undefined-function . 1)
                              (mcp-server-lib-test--with-request . defun)
                              (mcp-server-lib-test--verify-req-success . defun)
                              (mcp-server-lib-test--with-error-tracking . 1)
                              (mcp-server-lib-test--check-resource-read-error
                               . 0)
                              (mcp-server-lib-ert-with-metrics-tracking . 1)
                              (mcp-server-lib-ert-verify-req-success . defun)
                              (cl-defstruct))
     (elisp-lint-indent-specs (if-let* . 1) (if-let . 1)
                              (mcp-server-lib-test--with-server . 0)
                              (mcp-server-lib-test--with-tools . 1)
                              (mcp-server-lib-test--register-tool . 1)
                              (mcp-server-lib-test--with-resources . 1)
                              (mcp-server-lib-test--register-resource . 2)
                              (mcp-server-lib-test--register-resource-template
                               . 2)
                              (mcp-server-lib-test--with-resource-templates . 1)
                              (mcp-server-lib-test--with-undefined-function . 1)
                              (mcp-server-lib-test--with-request . defun)
                              (mcp-server-lib-test--verify-req-success . defun)
                              (mcp-server-lib-test--with-error-tracking . 1)
                              (mcp-server-lib-test--check-resource-read-error
                               . 0)
                              (cl-defstruct))
     (elisp-lint-indent-specs (org-mcp-test--with-enabled . defun)
                              (org-mcp-test--with-config . 1))
     (elisp-lint-indent-specs (if-let* . 1)
                              (mcp-server-lib-test--with-server . 0)
                              (mcp-server-lib-test--with-tools . 1)
                              (mcp-server-lib-test--register-tool . 1)
                              (mcp-server-lib-test--with-resources . 1)
                              (mcp-server-lib-test--register-resource . 2)
                              (mcp-server-lib-test--register-resource-template
                               . 2)
                              (mcp-server-lib-test--with-resource-templates . 1)
                              (mcp-server-lib-test--with-undefined-function . 1)
                              (mcp-server-lib-test--with-request . defun)
                              (mcp-server-lib-test--verify-req-success . defun)
                              (mcp-server-lib-test--with-error-tracking . 1)
                              (mcp-server-lib-test--check-resource-read-error
                               . 0)
                              (cl-defstruct))
     (elisp-lint-indent-specs (elisp-dev-mcp-test--with-server . defun)
                              (elisp-dev-mcp-test--with-bytecode-file . defun))
     (package-lint-main-file . "mcp-server-lib.el")
     (elisp-lint-indent-specs (org-mcp-test--with-enabled . defun))
     (eval and buffer-file-name (not (eq major-mode 'package-recipe-mode))
           (or (require 'package-recipe-mode nil t)
               (let ((load-path (cons "../package-build" load-path)))
                 (require 'package-recipe-mode nil t)))
           (package-recipe-mode))
     (elisp-lint-ignored-validators "checkdoc")
     (elisp-lint-indent-specs (org-autotask-with-org-node-with-url . 1)
                              (org-autotask--test-fixture . 1)
                              (org-autotask--buffer-test . 1)
                              (org-autotask--clock-in-action-test . 2)
                              (org-autotask--with-replaced-action-fn . 3)
                              (org-autotask--with-temp-org-agenda-files . defun))
     (elisp-lint-ignored-validators "package-lint")
     (eval setq-local undo-tree-auto-save-history nil)
     (eval setq-local backup-inhibited t)
     (magit-todos-exclude-globs ".git/" "arcanist/" "extra/")
     (magit-todos-exclude-globs "emacs/.emacs.d/abbrev_defs"
                                "emacs/.emacs.d/elpa/*" "zsh/.p10k.zsh")
     (org-emphasis-alist)
     (eval setq-local ispell-personal-dictionary
           (expand-file-name ".ispell.dict"
                             (file-name-directory
                              (let ((d (dir-locals-find-file "./")))
                                (if (stringp d) d (car d))))))
     (org-fontify-emphasized-text) (c-tab-always-indent t)
     (compilation-read-command)
     (projectile-generic-command
      . "(cd ce && git ls-files -zc --exclude-standard --recurse-submodules | gsed -z 's/^/ce\\//g'); (cd ee && git ls-files -zc --exclude-standard --recurse-submodules | gsed -z 's/^/ee\\//g')"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
