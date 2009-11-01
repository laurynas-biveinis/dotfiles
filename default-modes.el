; Configure, enable or disable various standard minor modes

; Scroll bars on the right
(set-scroll-bar-mode 'right)

; Show time
(display-time)

; Don't bother entering search and replace args if the buffer is read-only
(defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))

; No scrollbars
;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

; No menus.
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

; Typing or <Delete> will remove selected text
(delete-selection-mode 1)

; Mouse avoidance
(mouse-avoidance-mode)

; Enable visual feedback on selections
(setq transient-mark-mode t)

; Recent files menu
(recentf-mode)

; CUA mode
(cua-mode t)
; Keep region after copying
(setq cua-keep-region-after-copy t)

; Turn on font-lock mode
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)       ; GNU Emacs
  (setq font-lock-auto-fontify t))  ; XEmacs
(setq font-lock-maximum-decoration t)
(setq font-lock-support-mode 'jit-lock-mode)

; Show column number
(column-number-mode t)

; Better C-x b menu
(ido-mode t)

;; Auto Fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Show which function we are at
(setq which-func-modes t)
(which-function-mode)

(cond ((>= emacs-major-version 23)
    (global-visual-line-mode 1)))

; Uniquify buffer names
(setq uniquify-buffer-name-style 'forward)

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
  (after auto-refresh-dired (buffer &optional not-this-window frame)  activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))
(defadvice other-window
  (after auto-refresh-dired (arg &optional all-frame) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))

; On save, create missing parent directories automatically
; From http://atomized.org/2008/12/emacs-create-directory-before-saving/
(add-hook 'before-save-hook
	  '(lambda ()
	     (or (file-exists-p (file-name-directory buffer-file-name))
		 (make-directory (file-name-directory buffer-file-name) t))))

; CC Mode
; (CEDET configuration is in addon-modes.el)

;; Default indentation offset
(setq c-basic-offset 4)

;; Styles I use
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(c++-mode . "stroustrup")
			(other . "gnu")))

;; Enter indents the new line
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

; Grand Unified Debugger
(gud-tooltip-mode t)
