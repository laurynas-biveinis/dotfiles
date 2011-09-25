;;; xgit-log-edit.el --- Major mode to edit commit messages for git

;; Copyright (C) 2009  Matthieu Moy

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Keywords: git

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
(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG$" . xgit-log-edit-mode))

(defvar xgit-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?s)] 'xgit-log-edit-insert-sob)
    map)
  "Keymap used in `xgit-log-edit-mode' buffers.")

(easy-menu-define xgit-log-edit-mode-menu xgit-log-edit-mode-map
  "`xgit-log-edit-mode' menu"
  '("Log"
    ["Insert Signed-off-by:"     xgit-log-edit-insert-sob t]
    ))

(defvar xgit-log-edit-font-lock-keywords
  `(("^Signed-off-by: " . 'dvc-header)
    ("^#.*$" . 'dvc-comment))
  "Keywords in xgit-log-edit mode.")

(defun xgit-log-edit-insert-sob ()
  (interactive)
  (goto-char (point-max))
  (re-search-backward "^[^#\n]")
  (end-of-line)
  (newline 2)
  (insert "Signed-off-by: " user-full-name " <" user-mail-address ">"))

;;;###autoload
(define-derived-mode xgit-log-edit-mode dvc-log-edit-mode "xgit-log-edit"
  "Major Mode to edit xgit log messages.
Commands:
\\{xgit-log-edit-mode-map}
"
  (use-local-map xgit-log-edit-mode-map)
  (easy-menu-add xgit-log-edit-mode-menu)
  (dvc-install-buffer-menu)
  (set (make-local-variable 'font-lock-defaults)
       '(xgit-log-edit-font-lock-keywords t))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (setq fill-column 73)
  (run-hooks 'xgit-log-edit-mode-hook))

(provide 'xgit-log-edit)
;;; xgit-log-edit.el ends here
