;;; xgit-rebase-todo.el --- Major mode for editting git-rebase-todo files.

;; Copyright (C) 2009  Matthieu Moy

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("/git-rebase-todo$" . xgit-rebase-todo-mode))

(defvar xgit-rebase-todo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(meta ?n)] 'xgit-rebase-todo-move-down)
    (define-key map [(meta ?p)] 'xgit-rebase-todo-move-up)
    map)
  "Keymap used in `xgit-rebase-todo-mode' buffers.")

(defun xgit-rebase-todo-move-down ()
  (interactive)
  (beginning-of-line)
  (let* ((next (+ 1 (line-end-position)))
         (line (buffer-substring (point) next)))
    (delete-region (point) next)
    (forward-line 1)
    (insert line)
    (forward-line -1)))

(defun xgit-rebase-todo-move-up ()
  (interactive)
  (beginning-of-line)
  (let* ((next (+ 1 (line-end-position)))
         (line (buffer-substring (point) next)))
    (delete-region (point) next)
    (forward-line -1)
    (insert line)
    (forward-line -1)))

;; (easy-menu-define xgit-rebase-todo-mode-menu xgit-rebase-todo-mode-map
;;   "`xgit-rebase-todo-mode' menu"
;;   '("Rebase-todo"
;;     ["Action"     xgit-rebase-todo-function t]
;;     ))

(defvar xgit-rebase-todo-font-lock-keywords
  '(("^\\([a-z]+\\) \\([0-9a-f]+\\) \\(.*\\)$"
     (1 'dvc-keyword)
     (2 'dvc-revision-name))
    ("^#.*$" . 'dvc-comment))
  "Keywords in xgit-rebase-todo mode.")

;;;###autoload
(define-derived-mode xgit-rebase-todo-mode fundamental-mode "xgit-rebase-todo"
  "Major Mode to edit xgit rebase-todo files.

These files are the ones on which git launches the editor for
'git rebase --interactive' commands.

Commands:
\\{xgit-rebase-todo-mode-map}
"
  (use-local-map xgit-rebase-todo-mode-map)
  ;;(easy-menu-add xgit-rebase-todo-mode-menu)
  (dvc-install-buffer-menu)
  (set (make-local-variable 'font-lock-defaults)
       '(xgit-rebase-todo-font-lock-keywords t))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (run-hooks 'xgit-rebase-todo-mode-hook))


(provide 'xgit-rebase-todo)
;;; xgit-rebase-todo.el ends here
