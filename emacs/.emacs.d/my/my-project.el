;;; my-project.el --- Project work support.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure everything related to projects. The core package is `projectile',
;; which is also integrated with `lsp-mode', extended with project
;; reconfiguration command for CMake projects, and integrated with my dotfiles
;; "gitrmworktree" utility. Other packages are `deadgrep' and `wgrep'.
;; Additionally there are custom commands for CMake project management.
;;
;; Like in the rest of my personal configuration, all features (packages and
;; external tools) are assumed to exist, because this is a part of my dotfiles
;; repo where the needed packages are committed too. Thus, no error handling,
;; and no need to ensure compatibility with different Emacs or package versions.
;;
;; Custom keybindings:
;; s-p    - Projectile command map
;; s-p w  - Projectile reconfigure CMake project
;; s-p y  - Projectile kill all buffers and remove the worktree

;;; Code:

;;; `projectile'

(require 'projectile)

(setq projectile-use-git-grep t
      projectile-enable-cmake-presets t
      projectile-mode-line-prefix " "  ;; Save mode line space
      ;; Only use `xref' for cross-references. Any regex-based tooling is
      ;; inaccurate.
      projectile-tags-backend 'xref
      ;; Browsing project files without caching is prohibitively slow on large
      ;; projects such as MySQL.
      projectile-enable-caching t
      projectile-switch-project-action 'projectile-vc
      projectile-completion-system 'default)

;; Exclude some more modes from projectile
(add-to-list 'projectile-globally-ignored-modes "lisp-interaction-mode")
(add-to-list 'projectile-globally-ignored-modes "org-agenda-mode")
(add-to-list 'projectile-globally-ignored-modes "org-mode")
(add-to-list 'projectile-globally-ignored-modes "package-menu-mode")

;; For the global binding of Projectile command map, steal s-p from
;; `ns-print-buffer'. I never print buffers.
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

(defun dotfiles--projectile-mode-line ()
  "Report `projectile' project name and, if needed, type in the modeline.
The type is not displayed if it is generic or rust-cargo. This function is meant
to be set as the value of `projectile-mode-line-function' to replace the default
behavior."
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

(defun dotfiles--projectile-set-build-dir-and-command ()
  "Discover the build dir and command for a `projectile' project if not set.

The current implementation tries to discover the build directory looking for a
symlink to a compilation database ('compile_commands.json') in the project root.
If found, it assumes the symlink points to the actual build directory. If,
additionally, 'CMakePresets.json' is absent, indicating that CMake presets are
not used for this project, this function configures the build and test commands
for the project to use Ninja and CTest, respectively. For the the latter, the
'MAKE_J' environment variable is used for setting the CTest parallelism level,
defaulting to 1 if not set.

This function is meant to be used as before-advice for
`projectile-compilation-dir', handling the case if the CDB symlink exists,
leaving other cases for that function."
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

Not considering the history is what it makes it different from
`projectile-configure-command'.

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

;; Integrate `projectile' and `lsp-mode' with my gitrmworktree
(require 'lsp-mode)
(defun kill-buffers-rm-worktree (force)
  "Remove the git worktree and kill project buffers, optionally with FORCE.
The prefix argument enables FORCE."
  (interactive "P")
  ;; We don't know whether `projectile-kill-buffers' prompt will be answered
  ;; with 'y' or 'n' – assume that zero existing buffers means 'y'. For that to
  ;; work, `projectile-kill-buffers-filter' must be set to `'kill-all'.
  (let ((projectile-kill-buffers-filter 'kill-all)
        (project-root-path (projectile-acquire-root))
        (force-arg (if force "--force " "")))
    (projectile-with-default-dir project-root-path
      (projectile-kill-buffers)
      (when (null (projectile-project-buffers project-root-path))
        (let* ((cmd (concat "gitrmworktree " force-arg project-root-path))
               (ret-code (shell-command cmd)))
          (when (/= ret-code 0)
            (user-error
             "'%s' failed with exit code %d, check `shell-command-buffer-name'"
             cmd ret-code))
          (projectile-remove-known-project project-root-path)
          ;; TODO(laurynas): remove the project from `treemacs'. For that
          ;; package it is actually a 'workspace' that needs to be removed and
          ;; probably `treemacs-remove-workspace' is the right function, but
          ;; need figure out how to get from path to workspace name first.
          (lsp-workspace-folders-remove project-root-path))))))

(define-key projectile-command-map "y" #'kill-buffers-rm-worktree)

;; `deadgrep' alternatives: rg.el, ripgrep.el, counsel/helm, etc.
(require 'deadgrep)

(require 'wgrep)
(setq wgrep-auto-save-buffer t)

(require 'wgrep-deadgrep)
(add-hook 'deadgrep-finished-hook #'wgrep-deadgrep-setup)

(provide 'my-project)
;;; my-project.el ends here
