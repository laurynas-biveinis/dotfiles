;;;; .emacs --- my initialization file for Emacs.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;; Various paths
(defconst home-dir (concat (replace-regexp-in-string "\\\\" "/"
                                                     (getenv "HOME")) "/"))
(defconst private-elisp (concat home-dir "emacs/"))
(defconst dotfiles-elisp (concat private-elisp "dotfiles/*.el"))
(defconst private-elisp-lib (concat private-elisp "lib/"))
(defconst elpa-dir (concat private-elisp "elpa/"))
(defconst cmake-build.el-lib (concat private-elisp-lib "cmake-build.el/"))

(load (concat home-dir "secrets"))

;; Setup ELPA
(setq package-user-dir elpa-dir)
(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("melpa-stable" . 15)
        ("melpa"        . 10)))
(package-initialize)

(add-to-list 'load-path private-elisp-lib)
(add-to-list 'load-path cmake-build.el-lib)

;; Load system-specific library and setup system-specific things that
;; must be setup before main setup
(cond ((eq system-type 'windows-nt) (load (concat private-elisp
                                                  "ntemacs-cygwin")))
      ((eq system-type 'gnu/linux) (load (concat private-elisp "linux")))
      ((eq system-type 'darwin) (load (concat private-elisp "darwin"))))

(load (concat private-elisp "setup"))

(when (fboundp 'system-specific-setup) (system-specific-setup))

(mapc #'load (file-expand-wildcards dotfiles-elisp))

(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (keyfreq helm-dash helm-org helm-lsp helm-descbinds helm-projectile helm company-box yasnippet iedit modern-cpp-font-lock highlight-indent-guides page-break-lines xterm-color eldoc-cmake projectile vterm deadgrep all-the-icons-dired rich-minority git-gutter-fringe aggressive-indent company-lsp lsp-ui lsp-mode company flycheck dispwatch org-analyzer undo-tree yaml-mode markdown-mode po-mode ssh ssh-config-mode bison-mode org-plus-contrib cmake-font-lock cmake-mode google-c-style solarized-theme wakatime-mode exec-path-from-shell autopair magit org auctex)))
 '(safe-local-variable-values
   (quote
    ((c-tab-always-indent t)
     (TeX-master . t)
     (compilation-read-command)
     (projectile-generic-command . "(cd ce && git ls-files -zco --exclude-standard | sed -z 's/^/ce\\//g'); (cd ee && git ls-files -zco --exclude-standard | sed -z 's/^/ee\\//g')")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; .emacs ends here
