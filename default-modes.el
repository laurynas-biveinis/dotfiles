; Configure, enable or disable various standard minor modes

; Scroll bars on the right
(if (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode 'right))

; Show time
(display-time)

; Don't bother entering search and replace args if the buffer is read-only
(defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))

; No scrollbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

; No menus.
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

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

(setq shift-select-mode t)

; Ignore file size in fontlocking
(setq font-lock-maximum-size nil)

; Turn on font-lock mode
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)       ; GNU Emacs
  (setq font-lock-auto-fontify t))  ; XEmacs
(setq font-lock-maximum-decoration t)
(setq font-lock-support-mode 'jit-lock-mode)

; Show column number
(column-number-mode t)

; Better C-x b menu by IDO mode
(ido-mode t)

(setq ido-default-buffer-method 'selected-window)

; Kill completion buffers after completing
(defun kill-buffer-if-exists (buffer)
  "Kill the buffer if it exists"
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
(cond ((>= emacs-major-version 23)
       (global-visual-line-mode 1)))

; Uniquify buffer names
(setq uniquify-buffer-name-style 'forward)

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

(if (symbolp dired-auto-revert-buffer)
    (setq dired-auto-revert-buffer t)
  (progn
    ; Refresh dired buffers on redisplay or buffer change
    ; From http://nflath.com/2009/07/dired/
    (defadvice switch-to-buffer-other-window
      (after auto-refresh-dired (buffer &optional norecord) activate)
      (if (equal major-mode 'dired-mode)
          (revert-buffer)))
    (defadvice switch-to-buffer
      (after auto-refresh-dired (buffer &optional norecord) activate)
      (if (equal major-mode 'dired-mode)
          (revert-buffer)))
    (defadvice display-buffer
      (after auto-refresh-dired (buffer &optional not-this-window frame)
             activate)
      (if (equal major-mode 'dired-mode)
          (revert-buffer)))
    (defadvice other-window
      (after auto-refresh-dired (arg &optional all-frame) activate)
      (if (equal major-mode 'dired-mode)
          (revert-buffer)))))

; On save, create missing parent directories automatically
; From http://atomized.org/2008/12/emacs-create-directory-before-saving/
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

; -------
; CC Mode
; -------
; (CEDET configuration is in addon-modes.el)

;; Default indentation offset
(setq c-basic-offset 4)

;; TAB indents only if point in the beginning of the line
(setq c-tab-always-indent nil)

;; Styles I use
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c++-mode . "stroustrup")
                        (other . "gnu")))

;; Enter indents the new line
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

;; MySQL

(c-add-style "MySQL"
             '("K&R"
               (indent-tabs-mode . nil)
               (c-basic-offset . 2)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist . ((statement-block-intro . +)
                                   (knr-argdecl-intro . 0)
                                   (substatement-open . 0)
                                   (label . -)
                                   (statement-cont . +)
                                   (arglist-intro . c-lineup-arglist-intro-after-paren)
                                   (arglist-close . c-lineup-arglist)
                                   (innamespace . 0)
                                   (inline-open . 0)
                                   (statement-case-open . +)))))

(c-add-style "InnoDB"
             '("K&R"
               (indent-tabs-mode . t)
               (c-basic-offset . 8)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist . ((statement-block-intro . +)
                                   (knr-argdecl-intro . 0)
                                   (substatement-open . 0)
                                   (label . [0])
                                   (c . 0)
                                   (statement-cont . +)
                                   (arglist-intro . +)
                                   (arglist-close . c-lineup-arglist)
                                   (innamespace . 0)
                                   (inline-open . 0)
                                   (statement-case-open . +)
                                   ))
               ))

(c-add-style "Drizzle"
             '((indent-tabs-mode . nil)
               (c-basic-offset . 2)
               (c-comment-only-line-offset . 0)
               (setq c-hanging-braces-alist
                     (append '((class-open before)) c-hanging-braces-alist))
               (c-offsets-alist . ((statement-block-intro . 2)
                                   (knr-argdecl-intro . 0)
                                   (substatement-open . 0)
                                   (label . -)
                                   (statement-cont . +)
                                   (arglist-intro . c-lineup-arglist-intro-after-paren)
                                   (arglist-close . c-lineup-arglist)))))

(add-to-list 'auto-mode-alist '("\\.ic\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.i\\'" . c++-mode))

(dir-locals-set-class-variables 'innodb-source
                                '((c-mode . ((c-file-style . "InnoDB")))
                                  (c++-mode . ((c-file-style . "InnoDB")))))
(dir-locals-set-class-variables 'mysql-source
                                '((c-mode . ((c-file-style . "MySQL")))
                                  (c++-mode . ((c-file-style . "MySQL")))))
(dir-locals-set-class-variables 'drizzle-source
                                '((c-mode . ((c-file-style . "Drizzle")))
                                  (c++-mode . ((c-file-style . "Drizzle")))))

(defvar mysql-dirs '(("innodb_plugin"     . innodb-source)
                     ("innobase"          . innodb-source)
                     ("xtrabackup"        . innodb-source)
                     ("haildb"            . innodb-source)
                     ("Percona-Server"    . mysql-source)
                     ("mysql"             . mysql-source)
                     ("mysql-[:digit:].*" . mysql-source)
                     ("drizzle"           . drizzle-source))
  "An association list of regexps matching directory name substrings and their
corresponding directory classes.")

(defadvice dir-locals-find-file (before mysql-set-file-class
                                        (file))
  (let* ((file (expand-file-name file))
         (directory (file-name-directory file))
         (found-in-cache-p nil)
         (dlocals-itr dir-locals-directory-cache)
         (mysql-dirs-itr mysql-dirs))
    ; Check if this directory has not been processed before
    (while dlocals-itr
      (let ((elt (car dlocals-itr)))
        (if (eq t (compare-strings directory nil nil
                                   (car elt) nil nil
                                   (memq system-type
                                         '(windows-nt cygwin ms-dos))))
            (progn (setq found-in-cache-p 't)
                   (setq dlocals-itr nil))
          (setq dlocals-itr (cdr dlocals-itr)))))
    ; No - classify this directory
    (unless found-in-cache-p
      (while mysql-dirs-itr
        (let ((elt (car mysql-dirs-itr)))
          (if (string-match (car elt) directory)
              (progn (dir-locals-set-directory-class directory (cdr elt))
                     (setq mysql-dirs-itr nil))
            (setq mysql-dirs-itr (cdr mysql-dirs-itr))))))))

(ad-activate 'dir-locals-find-file)

; Grand Unified Debugger
(defun my-gud-hook ()
  (gud-tooltip-mode t))
(add-hook 'gdb-mode-hook 'my-gud-hook)
(add-hook 'sdb-mode-hook 'my-gud-hook)
(add-hook 'xdb-mode-hook 'my-gud-hook)
(add-hook 'perldb-mode-hook 'my-gud-hook)
(add-hook 'jdb-mode-hook 'my-gud-hook)

; Automatically indent pasted code
(setq auto-indent-paste-modes '(emacs-lisp-mode c-mode c++-mode))

(defadvice yank (after indent-region activate)
  (if (member major-mode auto-indent-paste-modes)
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode auto-indent-paste-modes)
      (indent-region (region-beginning) (region-end) nil)))

; sql-mode
(add-to-list 'auto-mode-alist '("\\.test\\'" . sql-mode)) ; MySQL test files

; Thanks to Alexey Kopytov
(add-hook 'sql-mode-hook 'my-sql-mode-hook)
(defun my-sql-mode-hook ()
  (define-key sql-mode-map (kbd "RET") 'newline-and-indent)

  ;; Make # start a new line comment in SQL. This is a MySQL-specific
  ;; syntax.

  (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
  (set-syntax-table sql-mode-syntax-table)
)
