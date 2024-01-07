;;; my-project.el --- Project work support.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure everything related to projects. The core package is `projectile',
;; which is also integrated with Helm through `helm-projectile', extended with
;; project reconfiguration command for CMake projects, and integrated with my
;; dotfiles "gitrmworktree" utility. Other packages are `deadgrep' and `wgrep'.
;;
;; Like in the rest of configuration, all
;; features are assumed to exist, because this is a part of my dotfiles repo
;; where the needed packages are committed too.

;;; Code:

(require 'projectile)

(setq projectile-completion-system 'helm
      projectile-switch-project-action #'helm-projectile
      projectile-use-git-grep t
      projectile-enable-cmake-presets t
      projectile-mode-line-prefix " "  ;; Save mode line space
      projectile-tags-backend 'xref  ;; Only use `xref' for cross-references
      projectile-enable-caching t)

;; Exclude some more modes from projectile
(add-to-list 'projectile-globally-ignored-modes "lisp-interaction-mode")
(add-to-list 'projectile-globally-ignored-modes "org-agenda-mode")
(add-to-list 'projectile-globally-ignored-modes "org-mode")
(add-to-list 'projectile-globally-ignored-modes "package-menu-mode")

;; Steal s-p from `ns-print-buffer'. I never print buffers
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

(defun dotfiles--projectile-mode-line ()
  "Report project name and, if needed, type in the modeline.

The type is not displayed if it is generic or rust-cargo."
  (let ((project-name (projectile-project-name)))
    (if (string= project-name "-")
        ""
      (let ((project-type (projectile-project-type)))
        (concat projectile-mode-line-prefix "["
                project-name
                (unless (memq project-type '(generic rust-cargo))
                  (concat ":" (symbol-name project-type)))
                "]")))))
(setq projectile-mode-line-function #'dotfiles--projectile-mode-line)


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

(defun dotfiles--projectile-set-build-dir-and-command ()
  "Discover the build dir and command for a `projectile' project if not set.

The current implementation uses the presence of a compilation database symlink
in the project root (and absence of CMakePresets.json) to point to the build
directory. If found, sets the compilation and test commands too."
  (let* ((project-root (projectile-project-root))
         (cmake-presets-path (expand-file-name "CMakePresets.json" project-root))
         (cdb-path (expand-file-name "compile_commands.json" project-root)))
    (when (and (file-symlink-p cdb-path)
               (not (file-exists-p cmake-presets-path)))
      (let ((resolved-cdb-path (file-truename cdb-path)))
        (when (file-exists-p resolved-cdb-path)
          (let ((resolved-cdb-dir (file-name-directory resolved-cdb-path))
                (make_j (or (getenv "MAKE_J") "1")))
            (setq-local projectile-project-compilation-dir
                        (file-relative-name resolved-cdb-dir project-root))
            (setq-local projectile-project-compilation-cmd "ninja")
            (setq-local projectile-project-test-cmd
                        (format "ctest -j%s" make_j))))))))

(advice-add 'projectile-compilation-dir :before
            #'dotfiles--projectile-set-build-dir-and-command)

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
      (when (null (projectile-project-buffers project-root-path))
        (let ((grmwt-ret-code
               (shell-command (concat "gitrmworktree " project-root-path))))
          (if (= grmwt-ret-code 0)
              (progn
                (projectile-remove-known-project project-root-path)
                ;; TODO(laurynas): remove the project from treemacs. ChatGPT
                ;; suggested to use `tremacs-remove-project-from-workspace', but
                ;; I am not sure this is the right function.
                (lsp-workspace-folders-remove project-root-path)
                )
            (user-error "'gitrmworktree %s' failed" project-root-path)))))))

(define-key projectile-command-map "y" #'kill-buffers-rm-worktree)

;; `deadgrep' alternatives: rg.el, ripgrep.el, counsel/helm, etc.
(require 'deadgrep)

(require 'wgrep)
(setq wgrep-auto-save-buffer t)

(require 'wgrep-deadgrep)
(add-hook 'deadgrep-finished-hook #'wgrep-deadgrep-setup)

(provide 'my-project)
;;; my-project.el ends here
