;;; darwin.el --- Darwin-specific Emacs init.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst my-frame-font
  "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"
  "My default frame font on Darwin.")

(setq insert-directory-program "/usr/local/bin/gls")

(setq ns-right-alternate-modifier 'none)

(require 'exec-path-from-shell)
(setq exec-path-from-shell-check-startup-files nil)
(add-to-list 'exec-path-from-shell-variables "LANG")
(exec-path-from-shell-initialize)

;;; darwin.el ends here
