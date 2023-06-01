;;;; .emacs --- my initialization file for Emacs.  -*- lexical-binding: t; -*-
;;; Commentary:

;; This prepares for the main setup of my Emacs configuration. It sets up
;; various paths, loads secret configurations, initializes packages, loads
;; system-specific settings and finally loads the main setup.

;;; Code:

;;; Various paths
(defconst home-dir (expand-file-name "~"))
(defconst private-elisp (expand-file-name ".emacs.d" home-dir))
(defconst dotfiles-elisp (concat private-elisp "/dotfiles/*.el"))

(load (expand-file-name "secrets" home-dir))

;;; Setup packages
(require 'package)

(setq package-native-compile t)

;; TODO(laurynas): `package-quickstart-file', which is committed to dotfiles
;; repo, contains absolute paths. This is not a problem for me right now since
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
(cond ((eq system-type 'windows-nt) (load (expand-file-name "ntemacs-cygwin"
                                                            private-elisp)))
      ((eq system-type 'gnu/linux) (load (expand-file-name "linux"
                                                           private-elisp)))
      ((eq system-type 'darwin) (load (expand-file-name "darwin"
                                                        private-elisp))))

;; Load the main setup
(load (expand-file-name "setup" private-elisp))

;; Call system-specific setup function if it is defined
(when (fboundp 'system-specific-setup) (system-specific-setup))

;; Load all other setup files from the dotfiles directory
(mapc #'load (file-expand-wildcards dotfiles-elisp))

(server-start)

(setq custom-file (expand-file-name "custom.el" private-elisp))
(load custom-file)

;;; init.el ends here
