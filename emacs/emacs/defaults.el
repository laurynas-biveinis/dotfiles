; User info
(setq user-full-name "Laurynas Biveinis")
(setq user-mail-address "laurynas.biveinis@gmail.com")

; Keep all messages
(setq message-log-max t)

; Emacs 23.2+: Active region becomes primary selection
(if (symbolp 'select-active-regions)
    (setq select-active-regions t))

; C-k kills line including its newline
(setq kill-whole-line t)

; Emacs 23.2+: do not store duplicate kills
(if (symbolp 'kill-do-not-save-duplicates)
    (setq kill-do-not-save-duplicates t))

; Bookmarks are saved automatically
(setq bookmark-save-flag 1)

; Trailing newlines are highlighted
(if (symbolp 'indicate-empty-lines) ; Emacs 23.2+
    (setq-default indicate-empty-lines t))

; 24h time format
(setq display-time-24hr-format t)

; Should files end with newline?
(setq-default require-final-newline 'query)

; Display trailing whitespace
(setq-default show-trailing-whitespace t)

; Don't interrupt redraw on input
(unless emacs-24-5-or-later
  (setq redisplay-dont-pause t))

; No annoying beeps
(setq visible-bell t)

; Indentation can only insert spaces by default
(setq-default indent-tabs-mode nil)

; If already indented, complete
(if (symbolp 'tab-always-indent)
    (setq tab-always-indent 'complete))

; Diff options
(setq diff-switches "-u -p")

; Preserve hard links to the file you are editing
; From http://www.emacswiki.org/cgi-bin/wiki/DotEmacsChallenge
(setq backup-by-copying-when-linked t)

; Preserve the owner and group of the file you are editing
; From http://www.emacswiki.org/cgi-bin/wiki/DotEmacsChallenge
(setq backup-by-copying-when-mismatch t)

; Do not backup
(setq make-backup-files nil)

; Use Unix-style line endings.
(setq-default buffer-file-coding-system 'utf-8-unix)

; XXI century encodings
(set-language-environment "UTF-8")

; No startup message
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
         ; darkstar without external screens: initial frame positioned in the
         ; top left corner
         (add-to-list 'initial-frame-alist `(top . ,darkstar-laptop-top))
         (add-to-list 'initial-frame-alist `(left . ,darkstar-laptop-left))
         (add-to-list 'initial-frame-alist `(height . ,darkstar-laptop-height))
         (add-to-list 'initial-frame-alist `(width . ,darkstar-laptop-width))
         (two-windows))
        (t (diagnose-unknown-display-geometry display-geometry))))

; Use system font if under Gnome, otherwise use a specified font if one was
; specified
(cond ((symbolp 'font-use-system-font)
       (setq font-use-system-font t))
      ((symbolp 'my-frame-font)
       (add-to-list 'default-frame-alist `(font . ,my-frame-font))
       (add-to-list 'initial-frame-alist `(font . ,my-frame-font))))

; Treat new (empty) files as modified
(add-hook 'find-file-hooks
          '(lambda ()
             (when (not (file-exists-p (buffer-file-name)))
               (set-buffer-modified-p t))))

; Mark executable files as executable on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

; Custom keybindings
(defun smart-home ()
  "Move point to first non-whitespace character or 'beginning-of-line'.

Move point to the first non-whitespace character on this line.  If point was
already at that position, move point to the beginning of line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line)
         )
    )
  )

; Keybindings
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cn" 'next-error)
(global-set-key "\C-cp" 'previous-error)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key [(control shift up)] 'enlarge-window)
(global-set-key [(control shift down)] 'shrink-window)
(global-set-key [(control shift left)] 'enlarge-window-horizontally)
(global-set-key [(control shift right)] 'shrink-window-horizontally)

(global-set-key [home] 'smart-home)
(global-set-key "\C-a" 'smart-home)

; Enable some disabled commands
(put 'narrow-to-region 'disabled nil)

; No scroll bars
(if (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))

; Don't bother entering search and replace args if the buffer is read-only
(defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))

; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

; Typing or <Delete> will remove selected text
(delete-selection-mode 1)

; Mouse avoidance
(if (symbolp 'make-pointer-invisible)
    (setq make-pointer-invisible t)
  (mouse-avoidance-mode))

; Enable visual feedback on selections
(setq transient-mark-mode t)

; Recent files menu
(recentf-mode)

; Turn on font-lock mode
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)
(setq font-lock-support-mode 'jit-lock-mode)

; Show column number
(column-number-mode t)

; Better C-x b menu by IDO mode
(ido-mode t)

(setq ido-default-buffer-method 'selected-window)

; Kill completion buffers after completing
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

;; Show which function we are at
(setq which-func-modes t)
(which-function-mode)

; Soft word wrap
(global-visual-line-mode 1)

; Nice unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)

; Automatically show images as images
(auto-image-file-mode 1)

;; -----
;; dired
;; -----
; Copy recursively
(setq dired-recursive-copies 'always)
; Delete recursively, but ask
(setq dired-recursive-deletes 'top)

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(add-hook 'Man-mode-hook 'goto-address)

(put 'dired-find-alternate-file 'disabled nil)

(setq wdired-allow-to-change-permissions t)

(setq dired-auto-revert-buffer t)

; On save, create missing parent directories automatically
; From http://atomized.org/2008/12/emacs-create-directory-before-saving/
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

; -------
; CC Mode
; -------

;; Default indentation offset
(setq c-basic-offset 4)

;; TAB indents only if point in the beginning of the line
(setq c-tab-always-indent nil)

;; Styles I use
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c++-mode . "stroustrup")
                        (other . "gnu")))

;; MySQL

(setq c-doc-comment-style
      '((c-mode . javadoc)
        (c++-mode . javadoc)))

; Grand Unified Debugger
(defun my-gud-hook ()
  (gud-tooltip-mode t))
(add-hook 'gdb-mode-hook 'my-gud-hook)
(add-hook 'sdb-mode-hook 'my-gud-hook)
(add-hook 'xdb-mode-hook 'my-gud-hook)
(add-hook 'perldb-mode-hook 'my-gud-hook)
(add-hook 'jdb-mode-hook 'my-gud-hook)

(defconst auto-indent-paste-modes '(emacs-lisp-mode c-mode c++-mode)
  "Automatically ident pasted code in these modes.")

(defadvice yank (after indent-region activate)
  (if (member major-mode auto-indent-paste-modes)
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode auto-indent-paste-modes)
      (indent-region (region-beginning) (region-end) nil)))

; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

; Workaround Emacs 25.2- security vuln
(unless emacs-25-3-or-later
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end))))

; Compilation
(setq compilation-scroll-output 'first-error)

(setq compilation-environment '("LANG=C"))

; GNU TLS
; Workaround https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341 until 26.3
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
