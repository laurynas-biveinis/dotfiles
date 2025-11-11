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
  (declare (ftype (function () boolean))
           (important-return-value t))
  (member major-mode (mapcar #'cdr major-mode-remap-alist)))

(defun dotfiles--maybe-enable-electric-layout-mode ()
  "Enable `electric-layout-mode' if the major mode is not `treesit'-based.
For `treesit'-based modes, formatting-as-you-type is provided by the modes
themselves."
  (dotfiles--warn-if-no-ts)
  (unless (dotfiles--ts-major-mode-p)
    (electric-layout-local-mode 1)))

(add-hook 'prog-mode-hook #'dotfiles--maybe-enable-electric-layout-mode)

(add-hook 'prog-mode-hook #'abbrev-mode)

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
  (declare (ftype (function () t)))
  (interactive)
  (end-of-line)
  (newline-and-indent))

;;; Kill and yank

(setq kill-whole-line t  ;; C-k kills line including its newline
      kill-do-not-save-duplicates t  ;; Do not store duplicate kills
      kill-read-only-ok t)

(defun dotfiles--indent-if-prog-mode (&optional _ARG)
  "Indent current region if in programming mode and no prefix arg."
  (declare (ftype (function (&optional t) t)))
  (interactive)
  (if (and (not current-prefix-arg)
           (derived-mode-p 'prog-mode))
      (indent-region (region-beginning) (region-end) nil)))

(advice-add #'yank :after #'dotfiles--indent-if-prog-mode)
(advice-add #'yank-pop :after #'dotfiles--indent-if-prog-mode)

;;; Undo

;; The tree-shaped edit history provided by `undo-tree' is the winner.
;;
;; TODO(laurynas): keep undo history for worktree files under the worktrees
;; themselves using `undo-tree-history-directory-alist' with pattern matching.
(require 'undo-tree)
(require 'magit-status)
(require 'seq)
(setq undo-tree-history-directory-alist
      `(("." . ,(expand-file-name "undo" user-emacs-directory))))
(add-to-list 'undo-tree-incompatible-major-modes #'help-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'Info-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'grep-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'magit-status-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'package-menu-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'messages-buffer-mode)

(defconst no-undo-tree-file-names
  '("*autoloads.el" "secrets.org" "secrets-local.el" ".DS_Store"))

(defun dotfiles--basename-matches-pattern-p (basename pattern)
  "Return non-nil if BASENAME matches the glob PATTERN.
PATTERN can use glob wildcards (* and ?)."
  (declare (ftype (function (string string) boolean))
           (important-return-value t))
  (string-match-p (wildcard-to-regexp pattern) basename))

(defun dotfiles--undo-tree-allowed-for-buffer-p (&optional _print-message)
  "Return non-nil if undo-tree is allowed for the current buffer.
Returns nil if the buffer basename matches any pattern in `no-undo-tree-file-names'.
Patterns can use glob wildcards (* and ?)."
  (declare (ftype (function (&optional t) boolean))
           (important-return-value t))
  (if-let ((file-name (buffer-file-name)))
      (let ((basename (file-name-nondirectory file-name)))
        (not (seq-some
              (apply-partially #'dotfiles--basename-matches-pattern-p basename)
              no-undo-tree-file-names)))
    t))

(advice-add 'turn-on-undo-tree-mode :before-while
            #'dotfiles--undo-tree-allowed-for-buffer-p)

(global-undo-tree-mode)

(defun my-undo-cleanup--undo-to-original-path (undo-filename)
  "Convert undo-tree UNDO-FILENAME to original file path.
Returns the absolute path to the original file, or nil if UNDO-FILENAME
is not a valid undo-tree filename."
  (declare (ftype (function (string) (or string null)))
           (important-return-value t))
  (let ((name (if (file-name-absolute-p undo-filename)
                  (file-name-nondirectory undo-filename)
                undo-filename)))
    (when (and (string-prefix-p ".!" name)
               (string-suffix-p ".~undo-tree~" name))
      (let ((path-part (substring name 2 -12)))
        (concat "/" (replace-regexp-in-string "!" "/" path-part t t))))))

(defun my-undo-cleanup-orphans (&optional dry-run)
  "Delete undo-tree files whose source files no longer exist.
With optional DRY-RUN non-nil, only report what would be deleted without
actually deleting anything.

Returns a plist with :deleted (number of files), :size (bytes freed),
and :files (list of deleted filenames in dry-run mode)."
  (declare (ftype (function (&optional (or null integer cons)) t)))
  (interactive "P")
  (let* ((undo-dir (expand-file-name "undo" user-emacs-directory))
         (undo-files (when (file-directory-p undo-dir)
                       (directory-files undo-dir t "\\.~undo-tree~\\'" t)))
         (deleted-count 0)
         (total-size 0)
         (deleted-files nil))
    (dolist (undo-file undo-files)
      (when-let ((original-path (my-undo-cleanup--undo-to-original-path undo-file)))
        (unless (file-exists-p original-path)
          (let ((file-size (file-attribute-size (file-attributes undo-file))))
            (if dry-run
                (progn
                  (push (file-name-nondirectory undo-file) deleted-files)
                  (setq total-size (+ total-size file-size))
                  (setq deleted-count (1+ deleted-count)))
              (condition-case err
                  (progn
                    (delete-file undo-file)
                    (setq total-size (+ total-size file-size))
                    (setq deleted-count (1+ deleted-count)))
                (error
                 (message "Failed to delete %s: %s" undo-file
                          (error-message-string err)))))))))
    (let ((result (list :deleted deleted-count
                        :size total-size
                        :files (nreverse deleted-files))))
      (message "%s %d orphaned undo files (%.1f MB)"
               (if dry-run "Would delete" "Deleted")
               deleted-count
               (/ total-size 1024.0 1024.0))
      result)))

(provide 'my-edit)
;;; my-edit.el ends here
