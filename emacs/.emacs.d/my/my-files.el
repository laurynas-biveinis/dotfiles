;;; my-files.el --- everything related to files.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This configures visiting, saving, backups, file management, TRAMP remote
;; access, and everything else related to files.
;;
;; Like in the rest of configuration, all
;; features are assumed to exist, because this is a part of my dotfiles repo
;; where the needed packages are committed too.

;;; Code:

;;; Visiting files

(setq enable-remote-dir-locals t
      delete-by-moving-to-trash t)

(defun dotfiles--mark-new-files-as-modified ()
  "Treat new (empty) files as modified."
  (when (and (buffer-file-name)
             (not (file-exists-p (buffer-file-name))))
    (set-buffer-modified-p t)))

(add-hook 'find-file-hook #'dotfiles--mark-new-files-as-modified)

(recentf-mode)  ;; Recent files menu
(auto-image-file-mode 1)  ;; Show images as images and not binary files
;; autorevert
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)

;;; Backups
(setq make-backup-files nil  ;; Do not backup
      create-lockfiles nil   ;; Do not create lockfiles
      ;; But if we did backup, do not break macOS file metadata
      backup-by-copying t
      ;; From http://www.emacswiki.org/cgi-bin/wiki/DotEmacsChallenge -
      ;; Preserve hard links to the file you are editing
      backup-by-copying-when-linked t
      ;; Preserve the owner and group of the file you are editing
      ;; From http://www.emacswiki.org/cgi-bin/wiki/DotEmacsChallenge
      backup-by-copying-when-mismatch t)

;;; Saving files

;; Use Unix-style line endings.
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Mark executable files as executable on save
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Autosave should be idle-based only, it is very annoying when it autosaves in
;; the middle of typing, even more so with org-encrypted blocks.
(setq auto-save-interval 0
      auto-save-timeout 30)

;;; File management with `dired'

(require 'dired)
(setq dired-recursive-copies 'always) ;; Copy recursively

(require 'vc)
;; https://www.reddit.com/r/emacs/comments/u2lf9t/comment/i4n9aoa/
(defun dotfiles--dired-dim-git-ignores ()
  "Dim out .gitignore contents in Dired buffers."
  (when-let ((ignores (vc-default-ignore-completion-table 'git ".gitignore"))
             (exts (make-local-variable 'completion-ignored-extensions)))
    (dolist (item ignores) (add-to-list exts item))))
(add-hook 'dired-mode-hook #'dotfiles--dired-dim-git-ignores)

(require 'dired-aux)
(setq dired-create-destination-dirs 'ask)

(with-eval-after-load "dired" (load "dired-x"))

(require 'dired-async)
(dired-async-mode 1)

(require 'wdired)
(setq wdired-allow-to-change-permissions t)

(require 'all-the-icons-dired)
(add-hook 'dired-mode-hook #'all-the-icons-dired-mode)

;;; TRAMP
(require 'tramp)
;; The default level 3 is too noisy with showing each file shipped
(setq tramp-verbose 2)
;; Not sure I care about scp overhead in 2020. Also, maybe this will help with
;; duplicated sshx/scpx paths in lsp-mode cache.
(require 'tramp-sh)
(setq tramp-copy-size-limit nil)
(setq tramp-default-method "scpx")
(setq tramp-use-scp-direct-remote-copying t)
(setq remote-file-name-inhibit-cache t)

;; "Integration" with `vc': I only use git on remote hosts, handled by Magit,
;; thus disable VC over TRAMP
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; "Integration" with `epa': avoid choking trying to gpg-decrypt non-encrypted
;; ~/.authinfo.gpg.
(setq tramp-completion-use-auth-sources nil)

(provide 'my-files)
;;; my-files.el ends here
