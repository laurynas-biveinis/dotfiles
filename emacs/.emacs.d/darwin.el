;;; darwin.el --- Darwin-specific Emacs init.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains Emacs initialization settings that are specific to
;; the Darwin operating system (macOS).

;;; Code:

(defconst my-frame-font
  "-*-SFMono Nerd Font-normal-normal-normal-*-12-*-*-*-p-0-iso10646-1"
  "My default frame font on Darwin.")

;; The byte compiler scopes the nested `defconst's below to their `let'.
(defvar dotfiles--homebrew-root)
(defvar dotfiles--homebrew-root-usable-p)

(without-remote-files
  (with-temp-buffer
    ;; Keep stdout only: stderr from the shell's startup files, which run
    ;; before any `2>/dev/null' here, and from brew would precede the prefix.
    (let* ((status (call-process-shell-command "brew --prefix" nil '(t nil)))
           (output (string-trim-right (buffer-string))))
      (defconst dotfiles--homebrew-root (file-name-as-directory output)
        "Homebrew prefix, valid only if `dotfiles--homebrew-root-usable-p'.")
      ;; `file-name-as-directory' turns empty or blank output into "./",
      ;; which `file-directory-p' accepts but `file-name-absolute-p' rejects.
      (defconst dotfiles--homebrew-root-usable-p
        (and (file-name-absolute-p dotfiles--homebrew-root)
             (file-directory-p dotfiles--homebrew-root))
        "Non-nil when the Homebrew prefix is an existing absolute directory.")
      (unless dotfiles--homebrew-root-usable-p
        (display-warning
         'dotfiles
         (format "Unusable Homebrew prefix %S from brew --prefix (exit %s)"
                 output status)
         :warning)))))

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
  "Return the list of existing XCode-managed man directories."
  (let* ((lines (process-lines-ignore-status "xcode-select" "--show-manpaths"))
         (paths (seq-filter #'file-directory-p lines)))
    (unless paths
      (display-warning
       'dotfiles
       (format "No existing XCode man directories in xcode-select output %S"
               lines)
       :warning))
    paths))

(dolist (path (dotfiles--get-xcode-man-paths))
  (add-to-list 'woman-manpath path))

(require 'exec-path-from-shell)
(add-to-list 'exec-path-from-shell-variables "LANG")
(exec-path-from-shell-initialize)

;; Fix native compilation warnings: "Warning (comp): ld: warning: -undefined
;; dynamic_lookup may not work with chained fixups"
(add-to-list 'native-comp-driver-options "-Wl,-w")

;;; plantuml
(require 'ob-plantuml)
(when dotfiles--homebrew-root-usable-p
  (setq org-plantuml-jar-path
        (concat dotfiles--homebrew-root "opt/plantuml/libexec/plantuml.jar")))

(provide 'darwin)
;;; darwin.el ends here
