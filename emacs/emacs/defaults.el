;;; defaults.el --- Emacs builtin mode config.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)

;; Variables defined elsewhere we'll be using
(defvar bookmark-save-flag)
(defvar display-time-24hr-format)
(defvar font-use-system-font)
(defvar dired-recursive-copies)
(defvar wdired-allow-to-change-permissions)
(defvar dired-auto-revert-buffer)
(defvar compilation-scroll-output)
(defvar compilation-environment)
(declare-function comint-watch-for-password-prompt "comint" (string))
(declare-function comint-strip-ctrl-m "comint" (&optional _string))

;;; General settings
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

;; Raw bytes in hexadecimal not octal
(setq display-raw-bytes-as-hex t)

;; Enter quoted chars in hex
(setq read-quoted-char-radix 16)

;; Trailing newlines are highlighted
(if (symbolp 'indicate-empty-lines) ; Emacs 23.2+
    (setq-default indicate-empty-lines t))

;; Should files end with newline?
(setq-default require-final-newline 'query)

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

;; But if we did backup, do not break macOS file metadata
(setq backup-by-copying t)

;; Use Unix-style line endings.
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Enable visual feedback on selections
(setq transient-mark-mode t)

;; No startup message
(setq inhibit-startup-message t)

(setq kill-read-only-ok t)

(setq scroll-error-top-bottom t)

(setq fast-but-imprecise-scrolling t)

(setq recenter-redisplay t)

(setq-default indicate-buffer-boundaries t)

(setq blink-cursor-blinks -1)

(setq x-stretch-cursor t)

(setq delete-by-moving-to-trash t)

(setq use-dialog-box nil)

(setq load-prefer-newer t)

;;; Hook helpers
;; Display trailing whitespace
(defun enable-show-trailing-ws ()
  "Enable showing of trailing whitespace."
  (setq show-trailing-whitespace t))

;;;; Bundled modes
;;; header-line-format
;; which-function-mode
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

;;; fill-column and other filling-related matters
(setq-default fill-column 80)

(setq sentence-end-double-space nil)

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

;; XXI century encodings
(set-language-environment "UTF-8")

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

(defun dotfiles--add-frame-geometry-to-initial-alist (geometry)
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
(defconst frame-geometries-to-ignore [(3600 . 1080) (5520 . 1080) (4688 . 1692)
                                      (3600 . 1692)])

(defun diagnose-unknown-display-geometry (display-geometry)
  "Diagnose unknown DISPLAY-GEOMETRY."
  (message "Unknown display size %sx%s"
           (car display-geometry) (cdr display-geometry)))

(require 'seq)
(let ((display-geometry (cons (display-pixel-width) (display-pixel-height))))
  (cond ((equal display-geometry darkstar-laptop-screen)
         ;; darkstar without external screens: initial frame positioned in the
         ;; top left corner
         (dotfiles--add-frame-geometry-to-initial-alist
          darkstar-laptop-geometry)
         (two-windows))
        ((equal display-geometry darkstar-external-screen)
         ;; darkstar with external screens: initial frame maximized in the
         ;; middle screen
         (dotfiles--add-frame-geometry-to-initial-alist
          darkstar-external-geometry)
         (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
         (add-to-list 'initial-frame-alist '(fullscreen-restore . maximized))
         (six-windows))
        ((seq-position frame-geometries-to-ignore display-geometry) ())
        (t (diagnose-unknown-display-geometry display-geometry))))

;; Use system font if under Gnome, otherwise use a specified font if one was
;; specified
(cond ((symbolp 'font-use-system-font)
       (setq font-use-system-font t))
      ((symbolp 'my-frame-font)
       (add-to-list 'default-frame-alist `(font . ,my-frame-font))
       (add-to-list 'initial-frame-alist `(font . ,my-frame-font))))

;;; files, directories, and similar things
(defun dotfiles--treat-new-files-as-modified ()
  "Treat new (empty) files as modified."
  (unless (file-exists-p (buffer-file-name))
    (set-buffer-modified-p t)))

(add-hook 'find-file-hook #'dotfiles--treat-new-files-as-modified)

(defun dotfiles--follow-cygwin-symlinks ()
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

(add-hook 'find-file-hook #'dotfiles--follow-cygwin-symlinks)

;; Mark executable files as executable on save
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(setq enable-remote-dir-locals t)

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

(defun end-of-line-and-newline-and-indent ()
  "Go to the end of line, insert a new line, and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<M-RET>") #'end-of-line-and-newline-and-indent)

;; Yank should indent in programming modes
(defun indent-if-prog-mode ()
  "Indent current region if in programming mode and no prefix arg."
  (interactive)
  (if (and (not current-prefix-arg) (derived-mode-p 'prog-mode))
      (indent-region (region-beginning) (region-end) nil)))

(advice-add #'yank :after #'indent-if-prog-mode)
(advice-add #'yank-pop :after #'indent-if-prog-mode)

;; Enable some disabled commands
(put 'narrow-to-region 'disabled nil)

;; No scroll bars, introduced in 24.1.
(if (fboundp #'set-scroll-bar-mode) (set-scroll-bar-mode nil))

(setq search-nonincremental-instead nil)

(setq query-replace-skip-read-only t)

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

;; Recent files menu
(recentf-mode)

(global-font-lock-mode 1)

;;; minibuffer
;; icomplete
(icomplete-mode)

;; Better C-x b menu by IDO mode. Alternatives: helm, ivy
(require 'ido)
(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-default-buffer-method 'selected-window)
(setq ido-use-filename-at-point 'guess)

(defun dotfiles--kill-buffer-if-exists (buffer)
  "Kill the BUFFER if it exists."
  (if (buffer-live-p buffer)
      (kill-buffer buffer)))

(defun dotfiles--kill-completion-buffers ()
  "Kill completion buffers, if they exist."
  (dotfiles--kill-buffer-if-exists "*Completions*")
  (dotfiles--kill-buffer-if-exists "*Ido Completions*"))

(add-hook 'minibuffer-exit-hook #'dotfiles--kill-completion-buffers)

(require 'minibuf-eldef)
(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode)

;;; autorevert
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)

;;; common programming modes
(add-hook 'prog-mode-hook #'turn-on-auto-fill)
(add-hook 'prog-mode-hook #'enable-show-trailing-ws)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'electric-layout-mode)
(add-hook 'prog-mode-hook #'goto-address-prog-mode)

;;; text-mode
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook #'enable-show-trailing-ws)
(add-hook 'text-mode-hook #'turn-on-flyspell)
(add-hook 'text-mode-hook #'goto-address-mode)

;; Soft word wrap
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

(global-visual-line-mode 1)

;; Nice unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Automatically show images as images
(auto-image-file-mode 1)

;;; dired
;; Copy recursively
(setq dired-recursive-copies 'always)

(defun dotfiles--load-dired-x ()
  "Load dired-x."
  (load "dired-x"))

(add-hook 'dired-load-hook #'dotfiles--load-dired-x)

(add-hook 'Man-mode-hook #'goto-address)

(put 'dired-find-alternate-file 'disabled nil)

(setq wdired-allow-to-change-permissions t)

;; From http://atomized.org/2008/12/emacs-create-directory-before-saving/
(defun dotfiles--create-missing-parent-dirs ()
  "Create parent directories automatically, if missing."
  (or (file-exists-p (file-name-directory buffer-file-name))
      (make-directory (file-name-directory buffer-file-name) t)))

(add-hook 'before-save-hook #'dotfiles--create-missing-parent-dirs)

;;; cc-mode
(require 'cc-mode)
(require 'cc-vars)

;; TAB indents only if point in the beginning of the line
(setq c-tab-always-indent 1)

;; Typing # moves it to the first column
(setq c-electric-pound-behavior '(alignleft))

(setq c-doc-comment-style
      '((c-mode . javadoc)
        (c++-mode . javadoc)))

(defun dotfiles--c-mode-common-hook ()
  "My customization of cc-mode init."
  (define-key c-mode-base-map (kbd "<RET>") #'c-context-line-break)
  (define-key c-mode-base-map (kbd "C-o") #'c-context-open-line)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c-initialization-hook #'dotfiles--c-mode-common-hook)

;; Grand Unified Debugger
(gud-tooltip-mode t)

;; windmove
(when (fboundp #'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; Compilation
(setq compilation-scroll-output 'first-error)

(setq compilation-environment '("LANG=C"))

;;; eldoc-mode: show the args of the current function in the echo area
(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)

;;; elisp-mode
(defun dotfiles--emacs-lisp-mode-hook ()
  "Configuration for 'emacs-lisp-mode'."
  (eldoc-mode)
  ;; Should the global default ever change, elisp should stay with spaces
  (setq indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook #'dotfiles--emacs-lisp-mode-hook)

;;; gpg
(require 'epa)
(setq epa-pinentry-mode 'loopback)

;; TODO(laurynas): report this bug upstream
(defun dotfiles--set-epg-context-pinentry-mode (context _cipher)
  "Fix epg CONTEXT to the correct pinentry mode."
  (setf (epg-context-pinentry-mode context) epa-pinentry-mode))

(advice-add #'epg-decrypt-string :before
            #'dotfiles--set-epg-context-pinentry-mode)

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

;; In Shell mode, do not echo passwords
(require 'comint)
(add-hook 'comint-output-filter-functions
          #'comint-watch-for-password-prompt
          #'comint-strip-ctrl-m)

(setq comint-scroll-to-bottom-on-input 'all)
(setq comint-scroll-to-bottom-on-output 'other)
(setq comint-prompt-read-only t)
(setq comint-input-ignoredups t)
(setq comint-terminfo-terminal "ansi")

;; Colors
(require 'ansi-color)
(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

;;; Tramp
(require 'tramp)
;; The default level 3 is too noisy with showing each file shipped
(setq tramp-verbose 2)
(setq tramp-inline-compress-start-size 10240)
(setq tramp-copy-size-limit 102400)
(setq tramp-default-method "scpx")
(setq tramp-completion-reread-directory-timeout t)

;;; calendar
(require 'calendar)
(require 'solar)
(setq calendar-week-start-day 1)
(setq calendar-date-style 'iso)
;; Vilnius!
(setq calendar-latitude 54.7)
(setq calendar-longitude 25.3)
(setq calendar-location-name "Vilnius, Lithuania")

;;; display-line-numbers
(require 'display-line-numbers)
(setq display-line-numbers-grow-only t)

;;; calculator
(require 'calculator)
(setq calculator-electric-mode t)

;;; ediff
(require 'ediff-wind)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-merge-split-window-function #'split-window-horizontally)
(setq ediff-quit-widened nil)

(defvar dotfiles--pre-ediff-window-config nil)

(defun dotfiles--save-pre-ediff-window-config ()
  "Save the current window configuration, to be used as an ediff pre-setup hook."
  (setq dotfiles--pre-ediff-window-config (current-window-configuration)))

(defun dotfiles--restore-pre-ediff-window-config ()
  "Restore the current window configuration, to be used as an ediff exit hook."
  (set-window-configuration dotfiles--pre-ediff-window-config))

(add-hook 'ediff-before-setup-hook #'dotfiles--save-pre-ediff-window-config)
(add-hook 'ediff-quit-hook #'dotfiles--restore-pre-ediff-window-config)

;;; printing
(setq ps-print-color-p 'black-white)

;;; Change appearance for screen sharing
(defconst screen-sharing-default-height
  (face-attribute 'default :height)
  "Default frame font height, when screen sharing is off.")

(defconst screen-sharing-larger-height
  (+ screen-sharing-default-height 20)
  "Larger frame font height, when screen sharing is on.")

(defun start-screen-sharing ()
  "Change Emacs appearance for screen sharing."
  (interactive)
  (set-face-attribute 'default nil :height screen-sharing-larger-height)
  (balance-windows)
  (global-display-line-numbers-mode))

(defun stop-screen-sharing ()
  "Restore Emacs appearance after screen sharing."
  (interactive)
  (set-face-attribute 'default nil :height screen-sharing-default-height)
  (balance-windows)
  (global-display-line-numbers-mode nil))

;;; defaults.el ends here
