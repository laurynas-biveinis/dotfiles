;;; setup-helm.el --- Helm config. -*- lexical-binding: t; -*-

;;; Commentary:
;; If this ever gets re-enabled, re-add "(string-prefix-p "*helm" buf-name)" to
;; the condition in `dotfiles--maybe-disable-fci'
;;
;; Emacs quit on startup with no notice, no error, no crash, no nothing if
;; `helm-org' is installed!
;;
;; `helm-lsp' needs to be melpa and not melpa-stable due to
;; https://github.com/emacs-lsp/helm-lsp/issues/21

;;; Code:

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
      ;; Emacs 29.1 treesit crashes on C++ projects
      helm-ff-search-library-in-sexp nil
      helm-net-prefer-curl t
      helm-list-directory-function #'helm-list-dir-external
      ;; So that `helm-imenu' shows everything for big source files.
      helm-candidate-number-limit 900)

(require 'helm-mode)
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

;;; TODO(laurynas): integrate Helm with `flyspell'? Neither `flyspell-correct' /
;;; `flyspell-correct-helm' nor `helm-flyspell' replace `ispell-word'.

;;; TODO(laurynas): integrate Helm with rg?

;; `org' integration with `helm'
(require 'org-refile)
(setq org-outline-path-complete-in-steps nil)

;; `helm' and `rich-minority'
(require 'rich-minority)
(add-to-list 'rm-blacklist " Helm")

;; `helm' and `keyfreq'
(add-to-list 'keyfreq-excluded-commands helm-next-line
             helm-maybe-exit-minibuffer helm-delete-char-backward helm-mini
             helm-M-x helm-ff-RET helm-projectile-switch-project
             helm-previous-line helm-keyboard-quit helm-find-files
             helm-find-files-up-one-level
             helm-helm-ff-delete-char-backward-with-subkeys)

;; Global keybindings
(require 'helm-command)
(require 'helm-bookmark)
(require 'helm-ring)
(require 'helm-dash)
(require 'helm-lsp)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-x b") #'helm-mini)
(substitute-key-definition #'apropos-command #'helm-apropos
                           (current-global-map))
(global-set-key (kbd "<C-f1>") #'helm-dash-at-point)
(global-set-key [remap xref-find-apropos] #'helm-lsp-global-workspace-symbol)

;; `helm' and `lsp-mode'
(require 'helm-lsp)

(defun dotfiles--lsp-bind-helm-lsp-workspace-symbol ()
  "Rebind C-M-. to helm-lsp-workspace-symbol."
  (define-key lsp-mode-map [remap xref-find-apropos]
              #'helm-lsp-workspace-symbol))

(defun dotfiles--lsp-unbind-helm-lsp-workspace-symbol (_lsp_workspace)
  "Restore global C-M-. binding."
  (define-key lsp-mode-map [remap xref-find-apropos] #'xref-find-apropos))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-bind-helm-lsp-workspace-symbol)
(add-hook 'lsp-after-uninitialized-functions
          #'dotfiles--lsp-unbind-helm-lsp-workspace-symbol)

;; `helm' and `projectile'
(require 'projectile)
(setq projectile-completion-system 'helm
      projectile-switch-project-action #'helm-projectile)

(require 'helm-projectile)

(helm-projectile-on)

;; Remove "-a" from grep options, because it kills grepping over TRAMP for some
;; projects.
(setq helm-projectile-grep-command "grep -r %e -n%cH -e %p %f .")

;;; `helm-org'

(require 'helm-mode)
(require 'helm-org)
(setq helm-org-headings-fontify t)
(setq helm-org-format-outline-path t)

(provide 'setup-helm)
;;; setup-helm.el ends here
