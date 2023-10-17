;;; my-complete.el --- Completions configuration.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Configure everything related to completions: built-in completion at point,
;; company, and Helm.
;; Packages related to company that I wanted to use but couldn't:
;; - `company-box': does not play well with Solarized Dark theme, resulting in
;;   too bright highlights.
;; - `company-quickhelp-mode': `pos-tip' results in ugly tooltips on macOS:
;; https://github.com/pitkali/pos-tip/issues/11.

;;; Code:

;;; Built-in configuration and completion at point
(setq tab-always-indent 'complete  ;; If already indented, complete
      completion-styles '(flex)
      ;; Remove the default `tags-completion-at-point', I never use tags.
      completion-at-point-functions nil)

;;; Company
(require 'company)
(add-hook 'after-init-hook #'global-company-mode)

;; Remove `company-semantic', `company-bbdb', `company-eclim', `company-clang',
;; `company-xcode', `company-oddmuse', (`company-gtags', `company-etags'), and
;; `company-dabbrev' from company backends.
(setq company-backends '(company-capf company-files
                                      (company-dabbrev-code company-keywords)))

(setq company-global-modes '(not Info-mode help-mode magit-status-mode
                                 org-agenda-mode grep-mode package-menu-mode
                                 vterm-mode))

(setq company-abort-manual-when-too-short t
      company-idle-delay 0.1
      company-minimum-prefix-length 1
      company-tooltip-idle-delay .3
      company-selection-wrap-around t)

;;; Helm

;; `helm-icons'
(require 'helm-icons)
(setq helm-icons-provider 'all-the-icons)
(helm-icons-enable)

;; Workaround https://github.com/yyoncho/helm-icons/issues/16 (Bringing up
;; helm-buffers-list breaks when using all-the-icons provider.)

(defun dotfiles--helm-icons--get-icon (file)
  "Get icon for FILE."
  (cond ((eq helm-icons-provider 'all-the-icons)
         (require 'all-the-icons)
         (concat
          (or (cond ((not (stringp file)) (all-the-icons-octicon "gear"))
                    ((or
                      (member (f-base file) '("." ".."))
                      (f-dir? file))
                     (all-the-icons-octicon "file-directory")))
              (all-the-icons-icon-for-file file))
          " "))
        ((eq helm-icons-provider 'treemacs)
         (helm-icons--treemacs-icon file))))

(advice-add #'helm-icons--get-icon :override #'dotfiles--helm-icons--get-icon)

(defun dotfiles--helm-icons--get-icon-for-mode (mode)
  "Get icon for MODE.
First it will use the customized helm-icons-mode->icon to resolve the icon,
otherwise it tries to use the provider."
  (or (-some->> (assoc major-mode helm-icons-mode->icon)
        (cl-rest)
        helm-icons--get-icon)
      (cond ((eq helm-icons-provider 'all-the-icons)
             (-let ((icon (all-the-icons-icon-for-mode mode)))
               (when (stringp icon) (concat icon " "))))
            (t nil))))

(defun dotfiles--helm-icons-buffers-add-icon (candidates _source)
  "Add icon to buffers source.
CANDIDATES is the list of candidates."
  (-map (-lambda ((display . buffer))
          (cons (concat
                 (with-current-buffer buffer
                   (or (dotfiles--helm-icons--get-icon-for-mode major-mode)
                       (-some->> (buffer-file-name)
                         helm-icons--get-icon)
                       (helm-icons--get-icon 'fallback)))
                 display)
                buffer))
        candidates))

(advice-add #'helm-icons-buffers-add-icon :override
            #'dotfiles--helm-icons-buffers-add-icon)

;; Helm core configuration
(require 'helm)
(require 'helm-files)
(require 'helm-for-files)

(setq helm-split-window-inside-p t
      helm-echo-input-in-header-line t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-net-prefer-curl t
      helm-list-directory-function #'helm-list-dir-external
      ;; So that `helm-imenu' shows everything for big source files.
      helm-candidate-number-limit 900)

(helm-mode 1)

(require 'helm-buffers)
(setq helm-buffers-favorite-modes '(text-mode))

(helm-autoresize-mode)

;; Integration with `recentf'
(setq helm-ff-file-name-history-use-recentf t
      helm-recentf-fuzzy-match t)

(require 'helm-descbinds)
(helm-descbinds-mode)

(require 'helm-imenu)
(setq helm-imenu-fuzzy-match t)

(require 'helm-grep)
(setq helm-grep-default-command "grep --color-always -d skip %e -n%cH -e %p %f")
(setq helm-grep-default-recurse-command
      "grep --color=always -d recurse %e -n%cH -e %p %f")
(setq helm-grep-file-path-style 'relative)

;; `helm-dash'
;; It's a shame Man_Pages documentation set is a dummy one and cannot be browsed
;; with `helm-dash'. If it becomes too annoying, look into dash-at-point instead.
(require 'helm-dash)
(setq helm-dash-browser-func 'eww)
;; TODO(laurynas): `thing-at-point' at "std::foo" returns "foo" whereas for
;; `helm-dash' std:: prefix would be useful too.

;; `helm-dash' integration with `sh-mode'
(defun dotfiles--helm-dash-sh-mode-hook ()
  "Integrate `helm-dash' with `sh-mode'."
  ;; We do not want to have (defvar dash-docs-docsets), it should be a local
  ;; variable only
  (with-suppressed-warnings ((free-vars dash-docs-docsets))
    (setq-local dash-docs-docsets '("Bash"))))
(add-hook 'sh-mode-hook #'dotfiles--helm-dash-sh-mode-hook)

;; `helm-dash' integration with `c-mode'
(defun dotfiles--helm-dash-c-mode-hook ()
  "Integrate `helm-dash' with `c-mode'.."
  (with-suppressed-warnings ((free-vars dash-docs-docsets))
    (setq-local dash-docs-docsets '("C"))))
(add-hook 'c-mode-hook #'dotfiles--helm-dash-c-mode-hook)

;; `helm-dash' integration with `c++-mode'
(defun dotfiles--helm-dash-c++-mode-hook ()
  "Integrate `helm-dash' with `c++-mode'.."
  (with-suppressed-warnings ((free-vars dash-docs-docsets))
    (setq-local dash-docs-docsets '("Boost" "C" "C++" "CMake"))))
(add-hook 'c++-mode-hook #'dotfiles--helm-dash-c++-mode-hook)

;; `helm-dash' integration with `emacs-lisp-mode'
(defun dotfiles--helm-dash-emacs-lisp-mode-hook ()
  "Integrate `helm-dash' with `emacs-lisp-mode'.."
  (with-suppressed-warnings ((free-vars dash-docs-docsets))
    (setq-local dash-docs-docsets '("Emacs Lisp"))))
(add-hook 'emacs-lisp-mode-hook #'dotfiles--helm-dash-emacs-lisp-mode-hook)

;; `helm-dash' integration with `markdown-mode'
(defun dotfiles--helm-dash-markdown-mode-hook ()
  "Integrate `helm-dash' with `markdown-mode'.."
  (with-suppressed-warnings ((free-vars dash-docs-docsets))
    (setq-local dash-docs-docsets '("Markdown"))))
(add-hook 'markdown-mode-hook #'dotfiles--helm-dash-markdown-mode-hook)

;;; Integration with `helm-org'
(require 'helm-mode)
(require 'helm-org)
(setq helm-org-headings-fontify t)
(setq helm-org-format-outline-path t)

;;; TODO(laurynas): integrate Helm with flyspell? Neither flyspell-correct /
;;; flyspell-correct-helm nor helm-flyspell replace ispell-word.

;;; TODO(laurynas): integrate Helm with rg?

(provide 'my-complete)
;;; my-complete.el ends here
