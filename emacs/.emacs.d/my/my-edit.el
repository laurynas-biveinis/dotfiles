;;; my-edit.el --- Emacs editing configuration.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Configure Emacs editing features, including indentation, kill & yank, undo.
;; Depends on `major-mode-remap-alist' being already configured.
;;
;; Like in the rest of my personal configuration, all features (packages and
;; external tools) are assumed to exist, because this is a part of my dotfiles
;; repo where the needed packages are committed too. Thus, no error handling,
;; and no need to ensure compatibility with different Emacs or package versions.
;;
;; Custom keybindings:
;; <f6> - edit all occurrences of a symbol

;;; Code:

;;; Variables and functions defined elsewhere we'll be using
;; Defined in secrets.el: a list of strings of file names for which `undo-tree'
;; should be disabled.
(defvar no-undo-tree-file-names)

(setq read-quoted-char-radix 16  ;; Enter quoted chars in hex
      sentence-end-double-space nil)

(defun dotfiles--warn-if-no-ts ()
  "Warn if `treesit'-based major modes are not configured.
This is checked through `major-mode-remap-alist' being empty."
  (when (null major-mode-remap-alist)
    (warn "Config bug: treesit must be configured before this point.")))

(defun dotfiles--ts-major-mode-p ()
  "Check whether the current mode is a `treesit'-based one.
It is assumed that all the `treesit'-based modes, and only them, are values in
the `major-mode-remap-alist'."
  (member major-mode (mapcar #'cdr major-mode-remap-alist)))

(defun dotfiles--maybe-enable-electric-layout-mode ()
  "Enable `electric-layout-mode' if the major mode is not `treesit'-based.
For `treesit'-based modes, formatting-as-you-type is provided by the modes
themselves."
  (dotfiles--warn-if-no-ts)
  (unless (dotfiles--ts-major-mode-p)
    (electric-layout-local-mode 1)))

(add-hook 'prog-mode-hook #'dotfiles--maybe-enable-electric-layout-mode)

(global-so-long-mode 1)
(delete-selection-mode 1)  ;; Typing or <Delete> will remove selected text

(require 'elec-pair)
(electric-pair-mode)

;; `iedit': the default binding of C-; conflicts with `flyspell'.
;; TODO(laurynas): M-I/M-{/M-} could be useful but the keybindings seem to
;; conflict. iedit seems to be configured in an... unorthodox way.
;; TODO(laurynas): report a bug.
(defvar iedit-toggle-key-default)
(setq iedit-toggle-key-default (kbd "<f6>"))
(require 'iedit)

;; `grab-mac-link'
(require 'grab-mac-link)
(setq grab-mac-link-dwim-favourite-app 'safari)

;;; Indentation

;; Indentation can only insert spaces by default. If this ever
;; changes, add reset to `emacs-lisp-mode' and `rust-mode' hooks.
(setq-default indent-tabs-mode nil)

;; `aggressive-indent-mode'
(require 'aggressive-indent)
(require 'org-agenda)
(require 'package)
(setq aggressive-indent-comments-too t)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes #'help-mode)
(add-to-list 'aggressive-indent-excluded-modes #'Info-mode)
(add-to-list 'aggressive-indent-excluded-modes #'magit-status-mode)
(add-to-list 'aggressive-indent-excluded-modes #'org-agenda-mode)
(add-to-list 'aggressive-indent-excluded-modes #'grep-mode)
;; Incompatible (corrupts buffer) and redundant anyway with LSP server-provided
;; formatting
(add-to-list 'aggressive-indent-excluded-modes #'c-mode)
(add-to-list 'aggressive-indent-excluded-modes #'c++-mode)
(require 'term)
(add-to-list 'aggressive-indent-excluded-modes #'term-mode)
(add-to-list 'aggressive-indent-excluded-modes #'package-menu-mode)
;; https://github.com/Malabarba/aggressive-indent-mode/issues/140
(add-to-list 'aggressive-indent-excluded-modes #'makefile-bsdmake-mode)

;; `indent-bars'
(require 'indent-bars)
(require 'indent-bars-ts)
(setq indent-bars-color '(highlight :face-bg t :blend 0.15))
(setq indent-bars-pattern ".")
(setq indent-bars-width-frac 0.1)
(setq indent-bars-pad-frac 0.1)
(setq indent-bars-zigzag nil)
;; blend=1: blend with BG only
(setq indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1))
;; pump up the BG blend on current
(setq indent-bars-highlight-current-depth '(:blend 0.5))
(setq indent-bars-display-on-blank-lines t)
(setq indent-bars-treesit-support t)
(setq indent-bars-treesit-ignore-blank-lines-types '("module"))
(add-hook 'prog-mode-hook #'indent-bars-mode)

;; `shfmt'
;; For now leave `shfmt-buffer' and `shfmt-region' unbound, because
;; bash-language-server provides the same functionality. TODO(laurynas): we
;; could use the LSP shortcuts ("S-l = =" and "S-l = r") for zsh buffers though.
(require 'shfmt)

(defun end-of-line-and-newline-and-indent ()
  "Go to the end of line, insert a new line, and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;;; Kill and yank

(setq kill-whole-line t  ;; C-k kills line including its newline
      kill-do-not-save-duplicates t  ;; Do not store duplicate kills
      kill-read-only-ok t)

(defun dotfiles--indent-if-prog-mode (&optional _ARG)
  "Indent current region if in programming mode and no prefix arg."
  (interactive)
  (if (and (not current-prefix-arg)
           (derived-mode-p 'prog-mode))
      (indent-region (region-beginning) (region-end) nil)))

(advice-add #'yank :after #'dotfiles--indent-if-prog-mode)
(advice-add #'yank-pop :after #'dotfiles--indent-if-prog-mode)

;;; Undo

;; The tree-shaped edit history provided by `undo-tree' is the winner.
;;
;; TODO(laurynas): how to cleanup old persistent undo history files? For git
;; worktree-based source code workflow keeping them in or close to the original
;; directory would help. This could be possible to implement with
;; `undo-tree-history-directory-alist' with pattern matching, but need to
;; consider .gitignore for those worktrees too.
(require 'undo-tree)
(require 'magit-status)
(setq undo-tree-history-directory-alist
      `(("." . ,(expand-file-name "undo" user-emacs-directory))))
(add-to-list 'undo-tree-incompatible-major-modes #'help-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'Info-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'grep-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'magit-status-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'package-menu-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'messages-buffer-mode)

(defun dotfiles--disable-undo-tree-by-buffer-name (&optional _print-message)
  "Return nil if the buffer file name is in `dotfiles--no-undo-tree-names'."
  (let ((file-name (buffer-file-name)))
    ;; TODO(laurynas): replace special-casing of suffix and exact match against
    ;; `no-undo-tree-file-names' with a glob match.
    (if file-name (not (or (string-suffix-p "autoloads.el" file-name)
                           (member (file-name-nondirectory (buffer-file-name))
                                   no-undo-tree-file-names)))
      t)))

(advice-add 'turn-on-undo-tree-mode :before-while
            #'dotfiles--disable-undo-tree-by-buffer-name)

(global-undo-tree-mode)

(provide 'my-edit)
;;; my-edit.el ends here
