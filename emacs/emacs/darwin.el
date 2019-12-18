;;; darwin.el --- Darwin-specific Emacs init.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Variables and functions defined elsewhere we'll be using
(defvar exec-path-from-shell-check-startup-files)
(defvar exec-path-from-shell-variables)
(declare-function exec-path-from-shell-initialize "exec-path-from-shell" ())

(defconst my-frame-font
  "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"
  "My default frame font on Darwin.")

(setq insert-directory-program "/usr/local/bin/gls")
(add-to-list 'exec-path "/usr/local/bin")

(defun system-specific-setup()
  "Setup specifics for Darwin."
  (require 'grep)
  (grep-apply-setting 'grep-command "ggrep -H -n")
  (grep-apply-setting 'grep-highlight-matches 'auto)
  (setq ns-right-alternate-modifier 'none)
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-check-startup-files nil)
  (add-to-list 'exec-path-from-shell-variables "LANG")
  (exec-path-from-shell-initialize))

;;; darwin.el ends here
