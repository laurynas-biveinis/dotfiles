;;; darwin.el --- Darwin-specific Emacs init.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst my-frame-font
  "-*-SFMono Nerd Font-normal-normal-normal-*-12-*-*-*-p-0-iso10646-1"
  "My default frame font on Darwin.")

(defvar dotfiles--homebrew-root)
(if (string-prefix-p "aarch64" system-configuration)
    (setq dotfiles--homebrew-root "/opt/homebrew/")
  (setq dotfiles--homebrew-root "/usr/local/"))

(setq insert-directory-program (concat dotfiles--homebrew-root "bin/gls"))

(require 'lsp-clangd)
(setq lsp-clients-clangd-executable (concat dotfiles--homebrew-root
                                            "opt/llvm/bin/clangd"))

(setq mac-right-option-modifier nil)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

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

;; Fix native compilation warnings: "Warning (comp): ld: warning: -undefined
;; dynamic_lookup may not work with chained fixups"
(setq native-comp-driver-options '("-Wl,-w"))

;;; darwin.el ends here
