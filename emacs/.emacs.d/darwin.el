;;; darwin.el --- Darwin-specific Emacs init.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains Emacs initialization settings that are specific to
;; the Darwin operating system (macOS).

;;; Code:

(defconst my-frame-font
  "-*-SFMono Nerd Font-normal-normal-normal-*-12-*-*-*-p-0-iso10646-1"
  "My default frame font on Darwin.")

(defconst dotfiles--homebrew-root
  (without-remote-files
    (file-name-as-directory
     (string-trim-right (shell-command-to-string "brew --prefix"))))
  "Homebrew installation prefix.")

(unless dotfiles--homebrew-root
  (display-warning 'dotfiles "Homebrew not found" :warning))

(defun dotfiles--set-exe-var (var name path)
  "Set VAR to PATH and warn if it is not an executable NAME."
  (set var path)
  (without-remote-files
    (unless (file-executable-p path)
      (display-warning
       'dotfiles
       (format "Executable %s not found at %s" name path) :warning))))

(require 'lsp-clangd)
(dotfiles--set-exe-var 'lsp-clients-clangd-executable "clangd"
                       (concat dotfiles--homebrew-root "opt/llvm/bin/clangd"))

(require 'my-lib)

(dotfiles--ensure-optional-packages '(exec-path-from-shell grab-mac-link))

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
(require 'woman)

(defun dotfiles--get-xcode-man-paths ()
  "Return the list of XCode-managed man path strings."
  (let ((paths-string (shell-command-to-string "xcode-select --show-manpaths")))
    (when (string-empty-p paths-string)
      (display-warning 'dotfiles "No XCode man paths found" :warning)
      nil)
    (split-string paths-string "\n")))


(let ((paths (dotfiles--get-xcode-man-paths)))
  (dolist (path paths)
    (when (file-directory-p path)
      (add-to-list 'woman-manpath path))))

(require 'exec-path-from-shell)
(setq exec-path-from-shell-check-startup-files nil)
(add-to-list 'exec-path-from-shell-variables "LANG")
(exec-path-from-shell-initialize)

;; Fix native compilation warnings: "Warning (comp): ld: warning: -undefined
;; dynamic_lookup may not work with chained fixups"
(setq native-comp-driver-options '("-Wl,-w"))

;;; plantuml
(require 'ob-plantuml)
(setq org-plantuml-jar-path
      (concat dotfiles--homebrew-root "opt/plantuml/libexec/plantuml.jar"))

;;; mu4e
(defconst mu4e-lisp-load-path (concat dotfiles--homebrew-root
                                      "opt/mu/share/emacs/site-lisp/mu/mu4e"))
(add-to-list 'load-path mu4e-lisp-load-path)

;;; darwin.el ends here
