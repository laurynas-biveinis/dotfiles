;; Variables defined elsewhere we'll be using
(defvar bookmark-save-flag)
(defvar display-time-24hr-format)
(defvar whitespace-style)
(defvar whitespace-line-column)
(defvar whitespace-global-modes)
(defvar emacs-24-5-or-later)
(defvar font-use-system-font)
(defvar ido-default-buffer-method)
(defvar which-func-modes)
(defvar dired-recursive-copies)
(defvar wdired-allow-to-change-permissions)
(defvar dired-auto-revert-buffer)
(defvar c-tab-always-indent)
(defvar c-doc-comment-style)
(defvar emacs-25-3-or-later)
(defvar compilation-scroll-output)
(defvar compilation-environment)
(defvar emacs-26-3-or-later)
(defvar gnutls-algorithm-priority)

;; User info
(setq user-full-name "Laurynas Biveinis")
(setq user-mail-address "laurynas.biveinis@gmail.com")

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

;; 24h time format
(setq display-time-24hr-format t)

;; Should files end with newline?
(setq-default require-final-newline 'query)

;; Display trailing whitespace
(setq-default show-trailing-whitespace t)

;;; whitespace-mode
(global-whitespace-mode)
(setq whitespace-style (quote (face trailing lines-tail empty indentation
                                    big-intent space-after-tab space-before-tab)))
;; Use fill-column value
(setq whitespace-line-column nil)
(setq whitespace-global-modes '(not dired-mode))

;; Don't interrupt redraw on input. Obsolete in 24.5+, default in 24.1+
(unless emacs-24-5-or-later
  (setq redisplay-dont-pause t))

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

(defconst darkstar-laptop-screen '(1680 . 1050))
(defconst darkstar-laptop-top 1)
(defconst darkstar-laptop-left 1)
(defconst darkstar-laptop-height 65)
(defconst darkstar-laptop-width 237)

(defun diagnose-unknown-display-geometry (display-geometry)
  "Diagnose unknown DISPLAY-GEOMETRY."
  (message "Unknown display size %sx%s"
           (car display-geometry) (cdr display-geometry)))

(let ((display-geometry (cons (display-pixel-width) (display-pixel-height))))
  (cond ((equal display-geometry darkstar-laptop-screen)
         ;; darkstar without external screens: initial frame positioned in the
         ;; top left corner
         (add-to-list 'initial-frame-alist `(top . ,darkstar-laptop-top))
         (add-to-list 'initial-frame-alist `(left . ,darkstar-laptop-left))
         (add-to-list 'initial-frame-alist `(height . ,darkstar-laptop-height))
         (add-to-list 'initial-frame-alist `(width . ,darkstar-laptop-width))
         (two-windows))
        (t (diagnose-unknown-display-geometry display-geometry))))

;; Use system font if under Gnome, otherwise use a specified font if one was
;; specified
(cond ((symbolp 'font-use-system-font)
       (setq font-use-system-font t))
      ((symbolp 'my-frame-font)
       (add-to-list 'default-frame-alist `(font . ,my-frame-font))
       (add-to-list 'initial-frame-alist `(font . ,my-frame-font))))

;; Treat new (empty) files as modified
(add-hook 'find-file-hooks
          '(lambda ()
             (when (not (file-exists-p (buffer-file-name)))
               (set-buffer-modified-p t))))

;; Mark executable files as executable on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Keybindings
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cn" 'next-error)
(global-set-key "\C-cp" 'previous-error)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key [(control shift up)] 'enlarge-window)
(global-set-key [(control shift down)] 'shrink-window)
(global-set-key [(control shift left)] 'enlarge-window-horizontally)
(global-set-key [(control shift right)] 'shrink-window-horizontally)

;; Enable some disabled commands
(put 'narrow-to-region 'disabled nil)

;; No scroll bars, introduced in 24.1.
(if (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))

;; Don't bother entering search and replace args if the buffer is read-only
(defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))

;; No toolbar, introduced in 24.1
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Typing or <Delete> will remove selected text
(delete-selection-mode 1)

;; Mouse avoidance. The var was introduced in 23.2.
(if (symbolp 'make-pointer-invisible)
    (setq make-pointer-invisible t)
  (moquse-avoidance-mode))

;; Enable visual feedback on selections
(setq transient-mark-mode t)

;; Recent files menu
(recentf-mode)

(global-font-lock-mode 1)

;; Show column number
(column-number-mode t)

;; Better C-x b menu by IDO mode. Alternatives: helm, ivy
(ido-mode t)

(setq ido-default-buffer-method 'selected-window)

;; Kill completion buffers after completing
(defun kill-buffer-if-exists (buffer)
  "Kill the BUFFER if it exists."
  (if (buffer-live-p buffer)
      (kill-buffer buffer)))
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (progn
               (kill-buffer-if-exists "*Completions*")
               (kill-buffer-if-exists "*Ido Completions*"))))

;; Auto Fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Show which function we are at. which-func-modes default to t in 24.1+.
(setq which-func-modes t)
(which-function-mode)

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

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(add-hook 'Man-mode-hook 'goto-address)

(put 'dired-find-alternate-file 'disabled nil)

(setq wdired-allow-to-change-permissions t)

(setq dired-auto-revert-buffer t)

;; On save, create missing parent directories automatically
;; From http://atomized.org/2008/12/emacs-create-directory-before-saving/
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;;; cc-mode

;; TAB indents only if point in the beginning of the line
(setq c-tab-always-indent 1)

(setq c-doc-comment-style
      '((c-mode . javadoc)
        (c++-mode . javadoc)))

;; Grand Unified Debugger
(gud-tooltip-mode t)

;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; Workaround Emacs 25.2- security vuln
(unless emacs-25-3-or-later
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end))))

;; Compilation
(setq compilation-scroll-output 'first-error)

(setq compilation-environment '("LANG=C"))

;; GNU TLS
;; Workaround https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341 until 26.3
(unless emacs-26-3-or-later
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
