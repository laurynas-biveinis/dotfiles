;;; gh.el --- Emacs setup for GitHub.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Emacs setup specific for GitHub

;;; Code:

(require 'my-lib)

(dotfiles--ensure-optional-packages '(ghub forge pr-review))

;; `forge'
(require 'forge)
(require 'org-autotask)

(defun my-pull-all-forge-repos ()
  "Pull everything for all known `forge' repos."
  (interactive)
  (org-autotask-require-org-clock)
  (dolist (row (forge-sql [:select * :from repository]))
    ;; Dependency on the internal `forge' schema. I haven't found a better way
    ;; to get this information.
    (let ((worktree-path (nth 33 row)))
      (when (and worktree-path (file-directory-p worktree-path))
        (let ((default-directory worktree-path))
          (forge-pull))))))

(provide 'gh)
;; gh.el ends here
