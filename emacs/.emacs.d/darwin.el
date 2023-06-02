;;; darwin.el --- Darwin-specific Emacs init.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains Emacs initialization settings that are specific to
;; the Darwin operating system (macOS).

;;; Code:

(defconst my-frame-font
  "-*-SFMono Nerd Font-normal-normal-normal-*-12-*-*-*-p-0-iso10646-1"
  "My default frame font on Darwin.")

(defconst dotfiles--homebrew-root
  (string-trim-right (shell-command-to-string "brew --prefix"))
  "Homebrew installation prefix.")

(unless dotfiles--homebrew-root
  (display-warning 'dotfiles "Homebrew not found" :warning))

(defun dotfiles--set-exe-var (var name path)
  "Set VAR to PATH and warn if it is not an executable NAME."
  (set var path)
  (unless (file-executable-p path)
    (display-warning
     'dotfiles
     (format "Executable %s not found at %s" name path) :warning)))

(dotfiles--set-exe-var 'insert-directory-program "gls"
                       (concat dotfiles--homebrew-root "/bin/gls"))

(require 'lsp-clangd)
(dotfiles--set-exe-var 'lsp-clients-clangd-executable "clangd"
                       (concat dotfiles--homebrew-root "/opt/llvm/bin/clangd"))

;; The right option modifier is used by the keyboard layout third level instead.
(setq mac-right-option-modifier nil)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; s-t binding is very annoying: a popular combination in other apps which opens
;; a font panel popup in macOS Emacs, which I never use.
(global-unset-key (kbd "s-t"))

;;; woman
;; Integrate `woman' with macOS XCode some better by adding the missing man
;; paths.

(defconst dotfiles--xcode-dev-dir
  (string-trim-right (shell-command-to-string "xcode-select -p"))
  "The currently active XCode developer directory.")

(unless dotfiles--xcode-dev-dir
  (display-warning 'dotfiles "XCode not found" :warning))

(require 'woman)
(defun dotfiles--add-xcode-man-subdir-to-woman (subdir)
  "Add SUBDIR below XCode dev dir `woman-manpath' if it exists, warn otherwise."
  (let ((dir (concat dotfiles--xcode-dev-dir subdir)))
    (if (file-directory-p dir)
        (add-to-list 'woman-manpath dir)
      (display-warning
       'dotfiles (format "Directory %s not found" dir) :warning))))

(dotfiles--add-xcode-man-subdir-to-woman
 "/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/share/man")
(dotfiles--add-xcode-man-subdir-to-woman "/usr/share/man")
(dotfiles--add-xcode-man-subdir-to-woman
 "/Toolchains/XcodeDefault.xctoolchain/usr/share/man")

(require 'exec-path-from-shell)
(setq exec-path-from-shell-check-startup-files nil)
(add-to-list 'exec-path-from-shell-variables "LANG")
(exec-path-from-shell-initialize)

;; Fix native compilation warnings: "Warning (comp): ld: warning: -undefined
;; dynamic_lookup may not work with chained fixups"
(setq native-comp-driver-options '("-Wl,-w"))

;;; darwin.el ends here
