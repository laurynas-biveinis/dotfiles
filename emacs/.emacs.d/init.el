;;;; init.el --- my initialization file for Emacs.  -*- lexical-binding: t; -*-
;;; Commentary:

;; This prepares for the main setup of my Emacs configuration. It sets up
;; various paths, loads secret configurations, loads system-specific settings
;; and finally loads the main setup.

;;; Code:

;;; Various path constants
(defconst dotfiles--home-dir (file-name-as-directory (expand-file-name "~")))
(defconst dotfiles--my-elisp (file-name-as-directory
                              (expand-file-name "my" user-emacs-directory))
  "My Emacs configuration code directory.")
(defconst dotfiles--modules-elisp (concat user-emacs-directory "dotfiles/*.el")
  "Emacs configuration added by other dotfiles modules.")

(add-to-list 'load-path dotfiles--my-elisp)

;; The absence of secrets.el is not an error, but the user needs to be notified.
;; This file is not stored under dotfiles so that it does not get checked into
;; their repo.
(unless (load (expand-file-name "secrets" dotfiles--home-dir) 'noerror)
  (display-warning 'dotfiles "Failed to load secrets.el" :info))

;; Load optional system-specific library and setup system-specific things that
;; must be setup before main setup. All of these must exist and their absence
;; would be a fatal error.
(load (expand-file-name
       (cond ((eq system-type 'windows-nt) "nt-emacs-cygwin")
             ((eq system-type 'gnu/linux) "linux")
             ((eq system-type 'darwin) "darwin"))
       user-emacs-directory))

;; Load the main setup
(require 'my-setup)

;; Call system-specific setup function if it is defined
(when (fboundp 'system-specific-setup) (system-specific-setup))

;; Load all other setup files from the dotfiles directory. The order is
;; irrelevant.
(mapc #'load (file-expand-wildcards dotfiles--modules-elisp))

(require 'server)
(if (server-running-p)
    (display-warning 'dotfiles "Emacs server is already running" :warning)
  (server-start))

;; Load custom file with settings from `customize' interface. This file is
;; committed to the dotfiles repo too. Its absence is an error.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; init.el ends here
