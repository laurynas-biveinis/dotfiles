;;;; .emacs --- my initialization file for Emacs.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;; Some version checks
;; 23.1, released on 2009-07-29, is the default on CentOS 6
(defconst emacs-24-1-or-later (or (and (= emacs-major-version 24)
                                       (>= emacs-minor-version 1))
                                  (>= emacs-major-version 25)))
;; 24.3, released on 2013-03-11, is the default on CentOS 7
;; 24.4, released on 2014-10-20, is the default on Debian 8 - once it becomes
;; the oldest version, replace eval-after-load with with-eval-after-load, enable
;; lexical binding
(defconst emacs-24-4-or-later (or (and (= emacs-major-version 24)
                                   (>= emacs-minor-version 4))
                                  (>= emacs-major-version 25)))
;; 24.5, released on 2015-04-10, is the default on Ubuntu 16.04 LTS and Debian 9
(defconst emacs-24-5-or-later (or (and (= emacs-major-version 24)
                                       (>= emacs-minor-version 5))
                                  (>= emacs-major-version 25)))
;; 25.2, released on 2017-04-21, is the default on Ubuntu 18.04 LTS
(defconst emacs-25-3-or-later (or (>= emacs-major-version 26)
                                  (and (= emacs-major-version 25)
                                       (>= emacs-minor-version 3))))
;; 26.1, released on 2018-05-28, is the default on Debian 10
;; 26.3, released on 2019-08-28
(defconst emacs-26-3-or-later (or (>= emacs-major-version 27)
                                  (and (= emacs-major-version 26)
                                       (>= emacs-minor-version 3))))

;;; Various paths
(defconst home-dir (concat (replace-regexp-in-string "\\\\" "/"
                                                 (getenv "HOME")) "/"))
(defconst private-elisp (concat home-dir "emacs/"))
(defconst dotfiles-elisp (concat private-elisp "dotfiles/*.el"))
(defconst private-elisp-lib (concat private-elisp "lib/"))
(defconst elpa-dir (concat private-elisp "elpa/"))

;; Setup elisp search directories
(add-to-list 'load-path home-dir)
(add-to-list 'load-path private-elisp)

;; Load system-specific library and setup system-specific things that
;; must be setup before main setup
(cond ((eq system-type 'windows-nt) (load-library "ntemacs-cygwin"))
      ((eq system-type 'gnu/linux) (load-library "linux"))
      ((eq system-type 'darwin) (load-library "darwin")))

(when (fboundp 'system-specific-presetup) (system-specific-presetup))

(add-to-list 'load-path private-elisp-lib)

(load "secrets")
(load "defaults")

;; Setup ELPA
(setq package-user-dir elpa-dir)
(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("org"          . 20)
        ("melpa-stable" . 15)
        ("melpa"        . 10)))
(package-initialize)

(load "addon-modes")

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
    (flymake-diagnostic-at-point all-the-icons-dired rich-minority git-gutter-fringe aggressive-indent company-lsp lsp-ui lsp-mode company flycheck dispwatch org-analyzer undo-tree yaml-mode markdown-mode po-mode ssh ssh-config-mode bison-mode org-plus-contrib cmake-font-lock cmake-mode google-c-style solarized-theme wakatime-mode exec-path-from-shell autopair magit org auctex)))
 '(safe-local-variable-values (quote ((TeX-master . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; .emacs ends here
