;;; my-project.el --- Project work support.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure everything related to projects. The core package is `projectile',
;; which is also integrated with Helm through `helm-projectile', extended with
;; project reconfiguration command for CMake projects, and integrated with my
;; dotfiles "gitrmworktree" utility.

;;; Code:

(require 'projectile)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action #'helm-projectile)
(setq projectile-use-git-grep t)
(setq projectile-enable-cmake-presets t)
;; Steal s-p from `ns-print-buffer'. I never print buffers
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Save mode line space
(setq projectile-mode-line-prefix " ")
;; Only use xref for cross-references
(setq projectile-tags-backend 'xref)
;; Exclude some more modes from projectile
(add-to-list 'projectile-globally-ignored-modes "lisp-interaction-mode")
(add-to-list 'projectile-globally-ignored-modes "org-agenda-mode")
(add-to-list 'projectile-globally-ignored-modes "package-menu-mode")
(projectile-mode +1)

(require 'helm-projectile)

;; Workaround https://github.com/mhayashi1120/Emacs-wgrep/issues/75 by having
;; both projectile-grep and helm-projectile-grep available in projectile keymap,
;; so that the plain version can be used if it's going to be edited with wgrep.
(defun dotfiles--helm-projectile-commander-bindings ()
  "Map both projectile-grep and helm-projectile-grep to own keys."

  (def-projectile-commander-method ?g
    "Run grep on project."
    (projectile-grep))

  (def-projectile-commander-method ?h
    "Run grep on project using Helm."
    (helm-projectile-grep)))

(defun dotfiles--helm-projectile-toggle (toggle)
  "Remap helm-projectile-grep to a separate key, depending on TOGGLE."
  (if (> toggle 0)
      (progn
        (define-key projectile-mode-map [remap projectile-grep] nil)
        (define-key projectile-command-map (kbd "s h") #'helm-projectile-grep))
    (define-key projectile-command-map (kbd "s h") nil)))

(advice-add #'helm-projectile-commander-bindings :after
            #'dotfiles--helm-projectile-commander-bindings)
(advice-add #'helm-projectile-toggle :after #'dotfiles--helm-projectile-toggle)

(helm-projectile-on)

;; Remove "-a" from grep options, because it kills grepping over TRAMP for some
;; projects.
(setq helm-projectile-grep-command "grep -r %e -n%cH -e %p %f .")

;; Workaround https://github.com/bbatsov/projectile/issues/347: remote projects
;; do not get added to known project list automatically. Also workaround the
;; lack of dynamic mode line on remote projects. It seems that after
;; https://github.com/bbatsov/projectile/pull/1096 there is no reason not to
;; enable it.
(defun dotfiles--projectile-find-file-hook-function ()
  "Hook to set up Projectile when called by `find-file-hook' on remote files."
  (when (file-remote-p default-directory)
    (when projectile-dynamic-mode-line
      (projectile-update-mode-line))
    (projectile-track-known-projects-find-file-hook)))
(advice-add #'projectile-find-file-hook-function :after
            #'dotfiles--projectile-find-file-hook-function)

;; Implement https://github.com/bbatsov/projectile/issues/1676 (Unable to
;; completing-read CMake preset the second time) by adding a new command for
;; reconfigure.
(defun dotfiles--projectile-reconfigure-command (compile-dir)
  "Retrieve the configure command for COMPILE-DIR without considering history.

The command is determined like this:

- first we check for `projectile-project-configure-cmd' supplied
via .dir-locals.el

- finally we check for the default configure command for a
project of that type"
  (or projectile-project-configure-cmd
      (let ((cmd-format-string (projectile-default-configure-command
                                (projectile-project-type))))
        (when cmd-format-string
          (format cmd-format-string (projectile-project-root) compile-dir)))))

(defun projectile-reconfigure-project (arg)
  "Run project configure command without considering command history.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (dotfiles--projectile-reconfigure-command
                  (projectile-compilation-dir))))
    (projectile--run-project-cmd command projectile-configure-cmd-map
                                 :show-prompt arg
                                 :prompt-prefix "Configure command: "
                                 :save-buffers t)))

(define-key projectile-command-map "w" #'projectile-reconfigure-project)

;; Integrate projectile/lsp-mode with my gitrmworktree
(require 'lsp-mode)
(defun kill-buffers-rm-worktree ()
  "Remove the git worktree and kill project buffers."
  (interactive)
  ;; TODO(laurynas): don't know whether `projectile-kill-buffers' prompt will be
  ;; answered with 'y' or 'n' – assume that zero existing buffers means 'y'. For
  ;; that, `projectile-kill-buffers-filter' must be set to `'kill-all'.
  (when (not (equal projectile-kill-buffers-filter 'kill-all))
    (user-error "Unsupported `projectile-kill-buffers-filter' value %:S"
                projectile-kill-buffers-filter))
  (let ((project-root-path (projectile-acquire-root)))
    (projectile-with-default-dir project-root-path
      (projectile-kill-buffers)
      (let ((buffers (projectile-project-buffers project-root-path)))
        (when (null buffers)
          (projectile-remove-known-project project-root-path)
          ;; TODO(laurynas): check gitrmworktree result code before removing the
          ;; project from the project list.
          (shell-command (concat "gitrmworktree " project-root-path))
          ;; TODO(laurynas): remove the project from treemacs
          (lsp-workspace-folders-remove project-root-path))))))

(define-key projectile-command-map "y" #'kill-buffers-rm-worktree)

(provide 'my-project)
;;; my-project.el ends here
