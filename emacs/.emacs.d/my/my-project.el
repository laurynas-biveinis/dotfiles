;;; my-project.el --- Project work support.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure everything related to projects. The core package is `projectile',
;; which is also integrated with `lsp-mode' and with my dotfiles
;; "gitrmworktree" utility.
;;
;; jscpd:ignore-start
;; Like in the rest of my personal configuration, all features (packages and
;; external tools) are assumed to exist, because this is a part of my dotfiles
;; repo where the needed packages are committed too. Thus, no error handling,
;; and no need to ensure compatibility with different Emacs or package versions.
;; jscpd:ignore-end
;;
;; Custom keybindings:
;; s-p    - Projectile command map
;; s-p y  - Kill project buffers; if none remain, remove worktree and branch

;;; Code:

;;; `projectile'

(require 'projectile)

(setq projectile-use-git-grep t
      projectile-enable-cmake-presets t
      ;; Separate compile/test/run buffers so one doesn't clobber another
      projectile-per-command-compilation-buffer t
      projectile-mode-line-prefix " "  ;; Save mode line space
      ;; Browsing project files without caching is prohibitively slow on large
      ;; projects such as MySQL.
      projectile-enable-caching 'persistent
      projectile-switch-project-action 'projectile-vc
      projectile-completion-system 'default
      projectile-auto-cleanup-known-projects t)

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
  (declare (ftype (function () string))
           (important-return-value t))
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

;; Workaround the lack of a dynamic mode line on remote projects: upstream's
;; `projectile-find-file-hook-function' deliberately skips the mode-line update
;; for remote buffers (probing project-type markers is slow on a cold TRAMP
;; cache), but per https://github.com/bbatsov/projectile/pull/1096 there is no
;; reason not to enable it.
(defun dotfiles--projectile-find-file-hook-function ()
  "Update the Projectile mode line for remote files.
Upstream's `find-file-hook' handler skips the mode-line update for remote
buffers."
  (when (and (file-remote-p default-directory)
             projectile-dynamic-mode-line)
    (projectile-update-mode-line)))
(advice-add #'projectile-find-file-hook-function :after
            #'dotfiles--projectile-find-file-hook-function)

;; Integrate `projectile' and `lsp-mode' with my gitrmworktree
(require 'lsp-mode)
(defun kill-buffers-rm-worktree (force)
  "Kill project buffers and, if none remain, remove the git worktree.
Ask for confirmation first.  gitrmworktree also deletes the worktree's
branch, locally and on its remote.  It refuses uncommitted changes by
default.  The prefix argument FORCE passes --force, skipping the local
merged-into-master check and discarding uncommitted work."
  (interactive "P")
  (let* ((project-root-path (projectile-acquire-root))
         ;; `projectile-project-buffers' matches truenames, but a root picked
         ;; from the known projects is spelled "~/...", and a buffer-local
         ;; `projectile-project-root' is returned as spelled.
         (buffers-root (projectile--directory-key project-root-path))
         ;; Org buffers are hidden from Projectile above, but they visit files
         ;; that gitrmworktree deletes.
         (projectile-globally-ignored-modes
          (remove "org-mode" projectile-globally-ignored-modes))
         (buffers (projectile-project-buffers buffers-root))
         (force-arg (if force "--force " "")))
    (when (yes-or-no-p
           (format "Kill %d buffers, remove worktree %s and its branch%s? "
                   (length buffers) project-root-path
                   (if force ", even if unmerged" "")))
      (dolist (buffer buffers)
        ;; Killing a base buffer kills its indirect buffers.
        (unless (buffer-base-buffer buffer)
          (kill-buffer buffer)))
      (when-let* ((remaining (projectile-project-buffers buffers-root)))
        (user-error "Not removing %s, buffers still open: %s" project-root-path
                    (mapconcat #'buffer-name remaining ", ")))
      ;; Bind only now: the kills above may have replaced the current buffer.
      ;; Not to the worktree itself: `shell-command' copies this into its
      ;; output buffer, and gitrmworktree deletes it.
      (let* ((default-directory (file-name-directory
                                 (directory-file-name project-root-path)))
             (cmd (concat "gitrmworktree " force-arg project-root-path))
             (ret-code (shell-command cmd)))
        (when (/= ret-code 0)
          (user-error
           "'%s' failed with exit code %d, check `shell-command-buffer-name'"
           cmd ret-code))
        (projectile-remove-known-project
         (projectile--known-project-root project-root-path))
        ;; TODO(laurynas): remove the project from `treemacs'. For that
        ;; package it is actually a 'workspace' that needs to be removed and
        ;; probably `treemacs-remove-workspace' is the right function, but
        ;; need figure out how to get from path to workspace name first.
        (lsp-workspace-folders-remove project-root-path)))))

(define-key projectile-command-map "y" #'kill-buffers-rm-worktree)

(require 'wgrep)
(setq wgrep-auto-save-buffer t)

(provide 'my-project)
;;; my-project.el ends here
