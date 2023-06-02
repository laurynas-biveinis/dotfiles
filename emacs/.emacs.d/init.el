;;;; init.el --- my initialization file for Emacs.  -*- lexical-binding: t; -*-
;;; Commentary:

;; This prepares for the main setup of my Emacs configuration. It sets up
;; various paths, loads secret configurations, loads system-specific settings
;; and finally loads the main setup.

;;; Code:

;;; Various path constants
(defconst home-dir (expand-file-name "~"))
(defconst private-elisp (expand-file-name ".emacs.d" home-dir))
(defconst dotfiles-elisp (concat private-elisp "/dotfiles/*.el"))

;; The absence of secrets.el is not an error, but the user needs to be notified.
(unless (load (expand-file-name "secrets" home-dir) 'noerror)
  (display-warning 'dotfiles "Failed to load secrets.el" :info))

;; Load optional system-specific library and setup system-specific things that
;; must be setup before main setup. All of these must exist and their absence
;; would be a fatal error.
(load (expand-file-name
       (cond ((eq system-type 'windows-nt) "nt-emacs-cygwin")
             ((eq system-type 'gnu/linux) "linux")
             ((eq system-type 'darwin) "darwin"))
       private-elisp))

;; Load the main setup
(load (expand-file-name "setup" private-elisp))

;; Call system-specific setup function if it is defined
(when (fboundp 'system-specific-setup) (system-specific-setup))

;; Load all other setup files from the dotfiles directory
(mapc #'load (file-expand-wildcards dotfiles-elisp))

(require 'server)
(if (server-running-p)
    (display-warning 'dotfiles "Emacs server is already running" :warning)
  (server-start))

;; Load custom file with settings from `customize' interface. This file is
;; committed to the dotfiles repo too. Its absence is an error.
(setq custom-file (expand-file-name "custom.el" private-elisp))
(load custom-file)

;;; init.el ends here
