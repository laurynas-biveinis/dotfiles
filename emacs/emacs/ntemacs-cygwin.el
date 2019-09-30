;;; ntemacs-cygwin.el --- Cygwin-specific init.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Variables and functions defined elsewhere we'll be using
(defvar private-elisp-lib)
(defvar home-dir)
(defvar explicit-shell-file-name)
(defvar comint-scroll-show-maximum-output)
(defvar comint-completion-addsuffix)
(defvar comint-eol-on-send)
(defvar shell-mode-hook)
(defvar python-python-command)
(declare-function cygwin-mount-activate "cygwin-mount" ())

(defun system-specific-presetup()
  "NT Emacs on Cygwin: set some things before main .emacs setup."

  ;; Setup paths
  (defconst cygwin-root "c:/cygwin/")
  (defconst private-bin (concat home-dir "/usr/bin"))
  (setq exec-path (cons private-bin exec-path))
  (setenv "PATH" (concat private-bin ";" (getenv "PATH")))
  ; Add Cygwin Emacs stuff
  (add-to-list 'load-path "/usr/share/emacs/site-lisp")
  ; Add Cygwin Info pages
  (add-to-list 'Info-default-directory-list
               (concat cygwin-root "usr/share/info/"))

  ;; Setup shell
  (setq shell-file-name "bash")
  (setenv "SHELL" shell-file-name)
  (setq explicit-shell-file-name shell-file-name)

  (defun my-shell-setup()
    "For Cygwin bash"
    (setq comint-scroll-show-maximum-output 'this)
    (make-local-variable 'comint-completion-addsuffix)
    (setq comint-completion-addsuffix t)
    (setq comint-eol-on-send t))

  (setq shell-mode-hook 'my-shell-setup)
)

(defun system-specific-setup()
  "NT Emacs on Cygwin .emacs specifics."

  ;; Path to Python
  (setq python-python-command "c:\\Python25\\python.exe")

  ;; AUCTeX should be loaded manually
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  (require 'tex-mik)

  (require 'cygwin-mount)
  (cygwin-mount-activate))

;;; ntemacs-cygwin.el ends here
