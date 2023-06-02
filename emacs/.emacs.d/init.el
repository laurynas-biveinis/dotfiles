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

;; The absence of secrets.el is not an error, but the user needs to be notified.
(unless (load (expand-file-name "secrets" home-dir) 'noerror)
  (display-warning 'dotfiles "Failed to load secrets.el" :info))

;; Load system-specific library and setup system-specific things that
;; must be setup before main setup. All of these must exist.
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

(require 'server)
(if (server-running-p)
    (display-warning 'dotfiles "Emacs server is already running" :warning)
  (server-start))

;; This file is committed to the dotfiles repo too. Its absence is an error.
(setq custom-file (expand-file-name "custom.el" private-elisp))
(load custom-file)

;;; init.el ends here
