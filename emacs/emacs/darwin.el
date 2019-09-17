;;; -*- lexical-binding: t; -*-
;;; System specific setup for Darwin

;; Variables defined elsewhere we'll be using
(defvar exec-path-from-shell-check-startup-files)

(defconst my-frame-font
  "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"
  "My default frame font on Darwin.")

(defun system-specific-presetup()
  "Things that must be set on Darwin before main setup."

  (progn
    (setq insert-directory-program "/usr/local/bin/gls")
    (add-to-list 'exec-path "/usr/local/bin")))

(defun system-specific-setup()
  "Setup specifics for Darwin."
  (setq ns-right-alternate-modifier 'none)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))
