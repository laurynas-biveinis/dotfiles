;;;; .emacs --- my initialization file for Emacs.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;; Various paths
(defconst home-dir (concat (replace-regexp-in-string "\\\\" "/"
                                                     (getenv "HOME")) "/"))
(defconst private-elisp (concat home-dir ".emacs.d/"))
(defconst dotfiles-elisp (concat private-elisp "dotfiles/*.el"))

(load (concat home-dir "secrets"))

;;; Setup packages
(require 'package)

(setq package-native-compile t)

;; TODO(laurynas): `package-quickstart-file', which is committed to dotfiles
;; repo,  contains absolute paths. This is not a problem for me right now since
;; the paths actually match on my machines. This might change in the future.
(setq package-quickstart t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu-elpa" .
                                 "https://elpa.nongnu.org/nongnu/"))
(setq package-archive-priorities
      '(("melpa-stable" . 15)
        ("melpa"        . 10)))

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

(setq custom-file (concat private-elisp "custom.el"))
(load custom-file)

;;; init.el ends here
