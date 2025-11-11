;;; my-complete.el --- Completions configuration.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Configure everything related to completions: built-in completion at point &
;; `company'.
;; Packages related to `company' that I wanted to use but couldn't:
;; - `company-box': does not play well with Solarized Dark theme, resulting in
;;   too bright highlights.
;; - `company-quickhelp-mode': `pos-tip' results in ugly tooltips on macOS:
;; https://github.com/pitkali/pos-tip/issues/11.
;;
;; Like in the rest of my personal configuration, all features (packages and
;; external tools) are assumed to exist, because this is a part of my dotfiles
;; repo where the needed packages are committed too. Thus, no error handling,
;; and no need to ensure compatibility with different Emacs or package versions.

;;; Code:

;;; Built-in configuration and completion at point
(setq tab-always-indent 'complete  ;; If already indented, complete
      completion-styles '(substring flex)
      ;; Remove the default `tags-completion-at-point', I never use tags.
      completion-at-point-functions nil
      completions-format 'vertical
      minibuffer-visible-completions t)

;;; Integration with `projectile'

;; Make `projectile' minibuffer completions confirm inputs that were not from
;; the valid completion alternative set

(defun dotfiles--completion-confirm (orig-fun &rest args)
  "Make minibuffer completion for (ORIG-FUN ARGS) confirm non-valid completion."
  (declare (ftype (function (function &rest t) t))
           (important-return-value t))
  (let ((minibuffer-completion-confirm 'confirm))
    (apply orig-fun args)))

(advice-add 'projectile-completing-read :around #'dotfiles--completion-confirm)

;;; Integration with `org-roam'

;; Make `org-roam' node completion case-insensitive
(defun dotfiles--completion-case-insensitive (orig-fun &rest args)
  "Make minibuffer completion for (ORIG-FUN ARGS) case-insensitive."
  (declare (ftype (function (function &rest t) t))
           (important-return-value t))
  (let ((completion-ignore-case t))
    (apply orig-fun args)))

(advice-add 'org-roam-node-read :around #'dotfiles--completion-case-insensitive)

;;; `company'
;; I looked into replacing it with `corfu', but `elpy' uses `company'.
(require 'company)
(add-hook 'after-init-hook #'global-company-mode)

;; Remove `company-semantic', `company-bbdb', `company-eclim', `company-clang',
;; `company-xcode', `company-oddmuse', (`company-gtags', `company-etags'), and
;; `company-dabbrev' from company backends, because I never use them.
(setq company-backends '(company-capf company-files
                                      (company-dabbrev-code company-keywords)))

(setq company-global-modes '(not Info-mode help-mode magit-status-mode
                                 org-agenda-mode grep-mode package-menu-mode
                                 vterm-mode))

(setq company-abort-manual-when-too-short t
      company-idle-delay 0.2
      company-minimum-prefix-length 4
      company-tooltip-idle-delay 0.5
      company-selection-wrap-around t)


(provide 'my-complete)
;;; my-complete.el ends here
