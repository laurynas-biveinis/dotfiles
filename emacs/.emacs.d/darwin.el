;;; darwin.el --- Darwin-specific Emacs init.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst my-frame-font
  "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1"
  "My default frame font on Darwin.")

(setq insert-directory-program "/usr/local/bin/gls")

(setq ns-right-alternate-modifier 'none)

;; s-t binding is very annoying: a popular combination in other apps which opens
;; a font panel popup in macOS Emacs, which I never use.
(global-unset-key (kbd "s-t"))

;;; woman
;; Integrate `woman' with macOS XCode some better by adding the missing man
;; paths.
(require 'woman)
(add-to-list 'woman-manpath
             "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/share/man")
(add-to-list 'woman-manpath
             "/Applications/Xcode.app/Contents/Developer/usr/share/man")
(add-to-list 'woman-manpath
             "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/share/man")

(require 'exec-path-from-shell)
(setq exec-path-from-shell-check-startup-files nil)
(add-to-list 'exec-path-from-shell-variables "LANG")
(exec-path-from-shell-initialize)

;;; darwin.el ends here
