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

;; Load custom file with settings from `customize' interface. This file is
;; committed to the dotfiles repo too. Its absence is an error.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Auto-regenerate package-quickstart if missing. Do this BEFORE initialization
;; so it can be used immediately rather than on next startup.
(when (and package-quickstart
           (not (file-exists-p package-quickstart-file)))
  (message "Generating missing package-quickstart.el...")
  ;; package-quickstart-refresh requires package-alist to be populated
  (package-initialize)
  (package-quickstart-refresh))

;; Initialize packages to populate `package-alist'. On a new system, this
;; ensures that VC-selected packages are installed too. Without `package-alist'
;; populated, `package-vc-install-selected-packages' cannot detect
;; already-installed VC packages and prompts to overwrite them.
;;
;; `package-initialize' is idempotent (it may have been called above when
;; regenerating the quickstart file).
(package-initialize)

(package-vc-install-selected-packages)

;; The absence of secret files is not an error, but the user needs to be
;; notified. These files are not stored under dotfiles so they don't get checked
;; into the repo.
(defun dotfiles--load-secrets-file (filename)
  "Load FILENAME with secrets from `dotfiles--home-dir', warn if not found."
  (unless (load (expand-file-name filename dotfiles--home-dir) 'noerror)
    (display-warning 'dotfiles (format "Failed to load %s" filename) :info)))

(dotfiles--load-secrets-file "secrets-local.el")
(dotfiles--load-secrets-file "secrets")

(defvar my-zettelkasten-p)

;; Report config
(message "Optional features:\n")
(message "- Zettelkasten: %S" my-zettelkasten-p)

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

;;; init.el ends here
