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

;; -----
;; dired
;; -----
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")))
(add-hook 'Man-mode-hook 'goto-address)
