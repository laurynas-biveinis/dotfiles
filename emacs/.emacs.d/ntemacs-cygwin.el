;;; ntemacs-cygwin.el --- Cygwin-specific init.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Variables and functions defined elsewhere we'll be using
(defvar private-elisp-lib)
(defvar home-dir)
(defvar python-python-command)

(require 'comint)
(require 'shell)
(require 'term)

(defconst dotfiles--cygwin-root "c:/cygwin/")
(defconst dotfiles--private-bin (concat home-dir "/usr/bin"))

;; Setup paths
(setq exec-path (cons dotfiles--private-bin exec-path))
(setenv "PATH" (concat dotfiles--private-bin ";" (getenv "PATH")))
;; Add Cygwin Emacs stuff
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
;; Add Cygwin Info pages
(add-to-list 'Info-default-directory-list
             (concat dotfiles--cygwin-root "usr/share/info/"))

;; Setup shell
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)

(defun dotfiles--shell-setup()
  "Setup Cygwin bash shell."
  (setq comint-scroll-show-maximum-output 'this)
  (make-local-variable 'comint-completion-addsuffix)
  (setq comint-completion-addsuffix t)
  (setq comint-eol-on-send t))

(setq shell-mode-hook #'dotfiles--shell-setup)

(defun system-specific-setup()
  "NT Emacs on Cygwin .emacs specifics."
  ;; Path to Python
  (setq python-python-command "c:\\Python25\\python.exe")
  ;; AUCTeX should be loaded manually
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  (require 'tex-mik))

;;; ntemacs-cygwin.el ends here
