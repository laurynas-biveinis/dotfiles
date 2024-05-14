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
 '(package-selected-packages
   '(shfmt indent-bars elpy org-gcal pr-review forge git-modes ob-rust plantuml-mode emacsql-sqlite-builtin difftastic wgrep-deadgrep prism org-sticky-header topsy rustic rust-mode org-roam-ui org-roam fancy-compilation flycheck-status-emoji flycheck-google-cpplint git-messenger beginend cheat-sh info-colors grab-mac-link stripe-buffer beacon lua-mode helm-icons wgrep-helm wgrep lsp-treemacs s calfw-ical calfw calfw-org helm-make gcmh which-key keyfreq helm-dash helm-org helm-lsp helm-descbinds helm-projectile helm company-box yasnippet iedit page-break-lines xterm-color eldoc-cmake projectile vterm deadgrep all-the-icons-dired rich-minority git-gutter-fringe aggressive-indent lsp-ui lsp-mode company flycheck dispwatch org-analyzer undo-tree yaml-mode markdown-mode ssh ssh-config-mode bison-mode cmake-font-lock cmake-mode solarized-theme wakatime-mode exec-path-from-shell magit))
 '(package-vc-selected-packages
   '((indent-bars :vc-backend Git :url "https://github.com/jdtsmith/indent-bars")))
 '(safe-local-variable-values
   '((org-emphasis-alist)
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
