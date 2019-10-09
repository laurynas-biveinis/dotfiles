;;; defaults.el --- Emacs builtin mode config.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)

;; Variables defined elsewhere we'll be using
(defvar bookmark-save-flag)
(defvar display-time-24hr-format)
(defvar emacs-24-1-or-later)
(defvar emacs-24-5-or-later)
(defvar font-use-system-font)
(defvar ido-default-buffer-method)
(defvar which-func-modes)
(defvar dired-recursive-copies)
(defvar wdired-allow-to-change-permissions)
(defvar dired-auto-revert-buffer)
(defvar emacs-25-3-or-later)
(defvar compilation-scroll-output)
(defvar compilation-environment)
(defvar emacs-26-3-or-later)
(defvar gnutls-algorithm-priority)
(declare-function comint-watch-for-password-prompt "comint" (string))
(declare-function comint-strip-ctrl-m "comint" (&optional _string))

;; Keep all messages
(setq message-log-max t)

;; Emacs 23.2+: Active region becomes primary selection, default in 24.1+
(if (symbolp 'select-active-regions)
    (setq select-active-regions t))

;; C-k kills line including its newline
(setq kill-whole-line t)

;; Emacs 23.2+: do not store duplicate kills
(if (symbolp 'kill-do-not-save-duplicates)
    (setq kill-do-not-save-duplicates t))

;; Bookmarks are saved automatically
(setq bookmark-save-flag 1)

;; Trailing newlines are highlighted
(if (symbolp 'indicate-empty-lines) ; Emacs 23.2+
    (setq-default indicate-empty-lines t))

;; Should files end with newline?
(setq-default require-final-newline 'query)

;; Display trailing whitespace
(defun enable-show-trailing-ws ()
  "Enable showing of trailing whitespace."
  (setq show-trailing-whitespace t))

;;; header-line-format
;; which-function-mode
(unless emacs-24-1-or-later
  (setq which-func-modes t))
(which-function-mode)

(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))

(setq mode-line-misc-info (assq-delete-all 'which-function-mode mode-line-misc-info))

;;; modeline
;; display-time-mode
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; Do not check for mail
(require 'time)
(setq display-time-mail-file t)
(display-time-mode)

;; size-indication-mode
(size-indication-mode)

;; Show column number
(column-number-mode t)

;; display-battery-mode
(require 'battery)
(setq battery-mode-line-format " [%b%p%% %t]")
(display-battery-mode)

;;; auto-fill-mode and other filling-related matters
(setq-default fill-column 80)

(setq-default auto-fill-function #'do-auto-fill)

(setq sentence-end-double-space nil)

(add-hook 'dired-mode-hook #'turn-off-auto-fill)
(add-hook 'erc-mode-hook #'turn-off-auto-fill)
(add-hook 'help-mode-hook #'turn-off-auto-fill)
(add-hook 'Info-mode-hook #'turn-off-auto-fill)
(add-hook 'magit-status-mode-hook #'turn-off-auto-fill)
(add-hook 'org-agenda-mode-hook #'turn-off-auto-fill)
(add-hook 'grep-mode-hook #'turn-off-auto-fill)
(add-hook 'package-menu-mode-hook #'turn-off-auto-fill)
(add-hook 'term-mode-hook #'turn-off-auto-fill)

;;; whitespace-mode
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style '(face trailing lines-tail empty indentation big-intent
                              space-after-tab space-before-tab))
;; Use fill-column value
(setq whitespace-line-column nil)
(setq whitespace-global-modes '(not dired-mode erc-mode markdown-mode gfm-mode
                                    lisp-interaction-mode help-mode Info-mode
                                    magit-status-mode org-agenda-mode grep-mode
                                    package-menu-mode))

;; Don't interrupt redraw on input. Obsolete in 24.5+, default in 24.1+
(unless emacs-24-5-or-later
  (with-no-warnings
    (setq redisplay-dont-pause t)))

;; No annoying beeps
(setq visible-bell t)

;; Indentation can only insert spaces by default
(setq-default indent-tabs-mode nil)

;; If already indented, complete
(if (symbolp 'tab-always-indent)
    (setq tab-always-indent 'complete))

;; Diff options
(setq diff-switches "-u -p")

;; Preserve hard links to the file you are editing
;; From http://www.emacswiki.org/cgi-bin/wiki/DotEmacsChallenge
(setq backup-by-copying-when-linked t)

;; Preserve the owner and group of the file you are editing
;; From http://www.emacswiki.org/cgi-bin/wiki/DotEmacsChallenge
(setq backup-by-copying-when-mismatch t)

;; Do not backup
(setq make-backup-files nil)

;; Use Unix-style line endings.
(setq-default buffer-file-coding-system 'utf-8-unix)

;; XXI century encodings
(set-language-environment "UTF-8")

;; No startup message
(setq inhibit-startup-message t)

;;; Window and frame geometry
(defun two-windows ()
  "Make frame contain two vertical windows."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun six-windows ()
  "Make frame contain 2x3 windows."
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (split-window-right)
  (windmove-down)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(cl-defstruct frame-geometry top left height width)

(defun add-frame-geometry-to-initial-alist (geometry)
  "Add frame GEOMETRY to `initial-frame-alist'."
  (add-to-list 'initial-frame-alist `(top . ,(frame-geometry-top geometry)))
  (add-to-list 'initial-frame-alist `(left . ,(frame-geometry-left geometry)))
  (add-to-list 'initial-frame-alist `(height . ,(frame-geometry-height geometry)))
  (add-to-list 'initial-frame-alist `(width . ,(frame-geometry-width geometry))))

(defun move-to-frame-geometry (geometry)
  "Resize and repositon frame to GEOMETRY."
  (set-frame-position
   nil (frame-geometry-left geometry) (frame-geometry-top geometry))
  (set-frame-size
   nil (frame-geometry-width geometry) (frame-geometry-height geometry)))

(defconst darkstar-laptop-screen '(1680 . 1050))
(defconst darkstar-laptop-geometry
  (make-frame-geometry :top 1 :left 1 :height 65 :width 237))

(defconst darkstar-external-screen '(7696 . 1692))
(defconst darkstar-external-geometry
  (make-frame-geometry :top 4 :left 3011 :height 117 :width 426))

;; Possible interim states while docking/undocking - ignore
(defconst darkstar-ignore '(3600 . 1080))
(defconst darkstar-ignore2 '(5520 . 1080))
(defconst darkstar-ignore3 '(4688 . 1692))
(defconst darkstar-ignore4 '(3600 . 1692))

(defun diagnose-unknown-display-geometry (display-geometry)
  "Diagnose unknown DISPLAY-GEOMETRY."
  (message "Unknown display size %sx%s"
           (car display-geometry) (cdr display-geometry)))

(let ((display-geometry (cons (display-pixel-width) (display-pixel-height))))
  (cond ((equal display-geometry darkstar-laptop-screen)
         ;; darkstar without external screens: initial frame positioned in the
         ;; top left corner
         (add-frame-geometry-to-initial-alist darkstar-laptop-geometry)
         (two-windows))
        ((equal display-geometry darkstar-external-screen)
         ;; darkstar with external screens: initial frame maximized in the
         ;; middle screen
         (add-frame-geometry-to-initial-alist darkstar-external-geometry)
         (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
         (add-to-list 'initial-frame-alist '(fullscreen-restore . maximized))
         (six-windows))
        ((equal display-geometry darkstar-ignore) ())
        ((equal display-geometry darkstar-ignore2) ())
        ((equal display-geometry darkstar-ignore3) ())
        ((equal display-geometry darkstar-ignore4) ())
        (t (diagnose-unknown-display-geometry display-geometry))))

;; Use system font if under Gnome, otherwise use a specified font if one was
;; specified
(cond ((symbolp 'font-use-system-font)
       (setq font-use-system-font t))
      ((symbolp 'my-frame-font)
       (add-to-list 'default-frame-alist `(font . ,my-frame-font))
       (add-to-list 'initial-frame-alist `(font . ,my-frame-font))))

(defun treat-new-files-as-modified ()
  "Treat new (empty) files as modified."
  (unless (file-exists-p (buffer-file-name))
    (set-buffer-modified-p t)))

(add-hook 'find-file-hook #'treat-new-files-as-modified)

(defun follow-cygwin-symlinks ()
  "Follow Cygwin symlinks.
Handles old-style (text file) and new-style (.lnk file) symlinks.
\(Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still
loaded as such.)"
  (save-excursion
    (goto-char 0)
    (if (looking-at
         "L\x000\x000\x000\x001\x014\x002\x000\x000\x000\x000\x000\x0C0\x000\x000\x000\x000\x000\x000\x046\x00C")
        (progn
          (re-search-forward
           "\x000\\([-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`][-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`]+\\)")
          (find-alternate-file (match-string 1)))
      (when (looking-at "!<symlink>")
        (re-search-forward "!<symlink>\\(.*\\)\0")
        (find-alternate-file (match-string 1))))))

(add-hook 'find-file-hook #'follow-cygwin-symlinks)

;; Mark executable files as executable on save
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;;; Keybindings
(global-set-key (kbd "<home>") #'move-beginning-of-line)
(global-set-key (kbd "<end>") #'move-end-of-line)

(global-set-key "\C-cg" #'goto-line)
(global-set-key "\C-cn" #'next-error)
(global-set-key "\C-cp" #'previous-error)
(global-set-key "\C-x\C-m" #'execute-extended-command)
(global-set-key "\C-c\C-m" #'execute-extended-command)

(global-set-key [(control shift up)] #'enlarge-window)
(global-set-key [(control shift down)] #'shrink-window)
(global-set-key [(control shift left)] #'enlarge-window-horizontally)
(global-set-key [(control shift right)] #'shrink-window-horizontally)

;; Enable some disabled commands
(put 'narrow-to-region 'disabled nil)

;; No scroll bars, introduced in 24.1.
(if (fboundp #'set-scroll-bar-mode) (set-scroll-bar-mode nil))

;; Don't bother entering search and replace args if the buffer is read-only
(defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))

;; No toolbar, introduced in 24.1
(if (fboundp #'tool-bar-mode) (tool-bar-mode -1))

;; Typing or <Delete> will remove selected text
(delete-selection-mode 1)

;; Mouse avoidance. The var was introduced in 23.2.
(if (symbolp 'make-pointer-invisible)
    (setq make-pointer-invisible t)
  (mouse-avoidance-mode))

;; Enable visual feedback on selections
(setq transient-mark-mode t)

;; Recent files menu
(recentf-mode)

(global-font-lock-mode 1)

;; Better C-x b menu by IDO mode. Alternatives: helm, ivy
(ido-mode t)

(setq ido-default-buffer-method 'selected-window)

(defun kill-buffer-if-exists (buffer)
  "Kill the BUFFER if it exists."
  (if (buffer-live-p buffer)
      (kill-buffer buffer)))

(defun kill-completion-buffers ()
  "Kill completion buffers, if they exist."
  (kill-buffer-if-exists "*Completions*")
  (kill-buffer-if-exists "*Ido Completions*"))

(add-hook 'minibuffer-exit-hook #'kill-completion-buffers)

;;; flyspell
(add-hook 'cperl-mode-hook      #'flyspell-prog-mode)
(add-hook 'autoconf-mode-hook   #'flyspell-prog-mode)
(add-hook 'autotest-mode-hook   #'flyspell-prog-mode)
(add-hook 'makefile-mode-hook   #'flyspell-prog-mode)

;;; text-mode
(add-hook 'text-mode-hook #'enable-show-trailing-ws)
(add-hook 'text-mode-hook #'turn-on-flyspell)

;; Soft word wrap
(global-visual-line-mode 1)

;; Nice unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Automatically show images as images
(auto-image-file-mode 1)

;;; dired
;; Copy recursively
(setq dired-recursive-copies 'always)

(defun load-dired-x ()
  "Load dired-x."
  (load "dired-x"))

(add-hook 'dired-load-hook #'load-dired-x)

(add-hook 'Man-mode-hook #'goto-address)

(put 'dired-find-alternate-file 'disabled nil)

(setq wdired-allow-to-change-permissions t)

(setq dired-auto-revert-buffer t)

;; From http://atomized.org/2008/12/emacs-create-directory-before-saving/
(defun create-missing-parent-dirs ()
  "Create parent directories automatically, if missing."
  (or (file-exists-p (file-name-directory buffer-file-name))
      (make-directory (file-name-directory buffer-file-name) t)))

(add-hook 'before-save-hook #'create-missing-parent-dirs)

;;; cc-mode

(require 'cc-vars)
(add-hook 'c-mode-common-hook #'enable-show-trailing-ws)
(add-hook 'c-mode-common-hook #'flyspell-prog-mode)

;; TAB indents only if point in the beginning of the line
(setq c-tab-always-indent 1)

(setq c-doc-comment-style
      '((c-mode . javadoc)
        (c++-mode . javadoc)))

(defun aerospike-c ()
  "A temporary hack until LSP is sorted out."
  (interactive)
  (c-set-style "k&r")
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

;; Grand Unified Debugger
(gud-tooltip-mode t)

;; windmove
(when (fboundp #'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; Workaround Emacs 25.2- security vuln
(unless emacs-25-3-or-later
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional _param)
       (list start end))))

;; Compilation
(setq compilation-scroll-output 'first-error)

(setq compilation-environment '("LANG=C"))

;; GNU TLS
;; Workaround https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341 until 26.3
(unless emacs-26-3-or-later
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;; eldoc-mode: show the args of the current function in the echo aread
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

;;; elisp-mode
(defun my-emacs-lisp-mode-hook ()
  "Configuration for 'emacs-lisp-mode'."
  (enable-show-trailing-ws)
  (flyspell-prog-mode)
  (eldoc-mode)
  ;; Should the global default ever change, elisp should stay with spaces
  (setq indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)

;;; ispell
(require 'ispell)
(setq ispell-program-name "hunspell")
(setq ispell-really-hunspell t)
(setq ispell-dictionary "en_US,lt")
(ispell-set-spellchecker-params)
(ispell-hunspell-add-multi-dic "en_US,lt")
(add-to-list 'ispell-skip-region-alist
             '("^-----BEGIN PGP MESSAGE-----$" . "^-----END PGP MESSAGE-----$"))
;; Workaround header line covering the ispell choices window
(setq ispell-choices-win-default-height 3)

;;; Show matching parents
(require 'paren)
(setq show-paren-style 'mixed)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(show-paren-mode 1)

;;; electric-pair-mode
(require 'elec-pair)
(electric-pair-mode)

;;; sh-mode
(add-hook 'sh-mode-hook #'flyspell-prog-mode)

;; In Shell mode, do not echo passwords
(add-hook 'comint-output-filter-functions
          #'comint-watch-for-password-prompt
          #'comint-strip-ctrl-m)

;; Colors
(require 'ansi-color)
(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

;;; easypg
;; Still only works if there's a symlink gpg -> gpg1, and I was not able to
;; find what uses gpg.
(require 'epg-config)
(setq epg-gpg-program "gpg1")

;;; Tramp
;; The default level 3 is too noisy with showing each file shipped
(require 'tramp)
(setq tramp-verbose 2)

;;; calendar
(require 'calendar)
(setq calendar-week-start-day 1)

;;; Change appearance for screen sharing
(defconst screen-sharing-default-height
  (face-attribute 'default :height)
  "Default frame font height, when screen sharing is off.")

(defconst screen-sharing-larger-height
  (+ screen-sharing-default-height 10)
  "Larger frame font height, when screen sharing is on.")

(defun start-screen-sharing ()
  "Change Emacs appearance for screen sharing."
  (interactive)
  (set-face-attribute 'default nil :height screen-sharing-larger-height)
  (balance-windows)
  (global-linum-mode 1))

(defun stop-screen-sharing ()
  "Restore Emacs appearance after screen sharing."
  (interactive)
  (set-face-attribute 'default nil :height screen-sharing-default-height)
  (balance-windows)
  (global-linum-mode -1))

;;; defaults.el ends here
