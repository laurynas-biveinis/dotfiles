;;; xmtn-propagate.el --- manage multiple propagations for DVC backend for monotone

;; Copyright (C) 2009 - 2011 Stephen Leake

;; Author: Stephen Leake
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301  USA.

(eval-when-compile
  ;; these have macros we use
  (require 'xmtn-ids))

(eval-and-compile
  ;; these have functions we use
  (require 'xmtn-automate)
  (require 'xmtn-base)
  (require 'xmtn-conflicts))

(defvar xmtn-propagate-from-root ""
  "Buffer-local variable holding `from' root directory.")
(make-variable-buffer-local 'xmtn-propagate-from-root)
(put 'xmtn-propagate-from-root 'permanent-local t)

(defvar xmtn-propagate-to-root ""
  "Buffer-local variable holding `to' root directory.")
(make-variable-buffer-local 'xmtn-propagate-to-root)
(put 'xmtn-propagate-to-root 'permanent-local t)

(defvar xmtn-propagate-ewoc nil
  "Buffer-local ewoc for displaying propagations.
All xmtn-propagate functions operate on this ewoc.
The elements must all be of class xmtn-propagate-data.")
(make-variable-buffer-local 'xmtn-propagate-ewoc)
(put 'xmtn-propagate-ewoc 'permanent-local t)

(defstruct (xmtn-propagate-data (:copier nil))
  from-work          ; directory name relative to xmtn-propagate-from-root
  to-work            ; directory name relative to xmtn-propagate-to-root
		     ; from-work is often the same as to-work
  from-name          ; display name, in buffer and menus; usually root dir name
  to-name            ;
  from-branch        ; branch name (assumed never changes)
  to-branch          ;
  need-refresh       ; nil | t; if an async process was started that invalidates state data
  from-head-revs     ; mtn rev string; current head revision or (left right) if multiple heads
  to-head-revs       ;
  conflicts-buffer   ; *xmtn-conflicts* buffer for this propagation
  from-status-buffer ; *xmtn-status* buffer for commit in from
  to-status-buffer   ; *xmtn-status* buffer for commit in to
  propagate-needed   ; nil | t
  from-heads         ; 'at-head | 'need-update | 'need-merge)
  to-heads           ;
  (from-local-changes
   'need-scan)       ; 'need-scan | 'need-commit | 'ok
  (to-local-changes
   'need-scan)       ;
  (conflicts
   'need-scan)       ; 'need-scan | 'need-resolve | 'need-review-resolve-internal | 'resolved | 'none
		     ; for propagate
  )

(defun xmtn-propagate-from-work (data)
  (concat xmtn-propagate-from-root (xmtn-propagate-data-from-work data)))

(defun xmtn-propagate-to-work (data)
  (concat xmtn-propagate-to-root (xmtn-propagate-data-to-work data)))

(defun xmtn-propagate-from-name ()
  "Display name for current `from' workspace."
  (xmtn-propagate-data-from-name (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))

(defun xmtn-propagate-to-name ()
  "Display name for current `to' workspace."
  (xmtn-propagate-data-to-name (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))

(defun xmtn-propagate-need-refresh (elem data)
  (setf (xmtn-propagate-data-need-refresh data) t)
  (ewoc-invalidate xmtn-propagate-ewoc elem))

(defun xmtn-propagate-printer (data)
  "Print an ewoc element."
  (if (string= (xmtn-propagate-data-from-work data)
               (xmtn-propagate-data-to-work data))
      (insert (dvc-face-add (format "%s\n" (xmtn-propagate-data-from-work data)) 'dvc-keyword))
    (insert (dvc-face-add (format "%s -> %s\n"
                                  (xmtn-propagate-data-from-work data)
                                  (xmtn-propagate-data-to-work data))
                          'dvc-keyword)))

  (if (xmtn-propagate-data-need-refresh data)
      (insert (dvc-face-add "  need refresh\n" 'dvc-conflict))

    (ecase (xmtn-propagate-data-from-local-changes data)
      (need-scan (insert "  local changes not checked " (xmtn-propagate-data-from-name data) "\n"))
      (need-commit
       (insert (dvc-face-add (concat "  need commit " (xmtn-propagate-data-from-name data) "\n")
                             'dvc-header)))
      (ok nil))

    (ecase (xmtn-propagate-data-to-local-changes data)
      (need-scan (insert "  local changes not checked " (xmtn-propagate-data-to-name data) "\n"))
      (need-commit
       (insert (dvc-face-add (concat "  need commit " (xmtn-propagate-data-to-name data) "\n")
                             'dvc-header)))
      (ok nil))

    (ecase (xmtn-propagate-data-from-heads data)
      (at-head     nil)
      (need-update
       (insert (dvc-face-add (concat "  need update " (xmtn-propagate-data-from-name data) "\n")
                             'dvc-conflict)))
      (need-merge
       (insert (dvc-face-add (concat "  need status for merge " (xmtn-propagate-data-from-name data) "\n")
                             'dvc-conflict))))

    (ecase (xmtn-propagate-data-to-heads data)
      (at-head     nil)
      (need-update
       (insert (dvc-face-add (concat "  need update " (xmtn-propagate-data-to-name data) "\n")
                             'dvc-conflict)))
      (need-merge
       (insert (dvc-face-add (concat "  need status for merge " (xmtn-propagate-data-to-name data) "\n")
                                   'dvc-conflict))))

    (if (xmtn-propagate-data-propagate-needed data)

        (if (and (eq 'at-head (xmtn-propagate-data-from-heads data))
                 (eq 'at-head (xmtn-propagate-data-to-heads data)))
            (ecase (xmtn-propagate-data-conflicts data)
              (need-scan
               (insert "conflicts need scan\n"))
              (need-resolve
               (insert (dvc-face-add "  need resolve conflicts\n" 'dvc-conflict)))
              (need-review-resolve-internal
               (insert (dvc-face-add "  need review resolve internal\n" 'dvc-header))
               (insert (dvc-face-add "  need propagate\n" 'dvc-conflict)))
              ((resolved none)
               (insert (dvc-face-add "  need propagate\n" 'dvc-conflict)))))

      (if (eq 'at-head (xmtn-propagate-data-to-heads data))
          (insert (dvc-face-add "  need clean\n" 'dvc-conflict)))
      ))
  ;; ewoc ought to do this, but it doesn't
  (redisplay))

(defun xmtn-propagate-kill-conflicts-buffer (data)
  (if (buffer-live-p (xmtn-propagate-data-conflicts-buffer data))
      (let ((buffer (xmtn-propagate-data-conflicts-buffer data)))
        (with-current-buffer buffer (save-buffer))
        (kill-buffer buffer))))

(defun xmtn-propagate-save-conflicts-buffer (data)
  (if (buffer-live-p (xmtn-propagate-data-conflicts-buffer data))
      (with-current-buffer (xmtn-propagate-data-conflicts-buffer data) (save-buffer))))

(defun xmtn-propagate-create-to-status-buffer (data)
  "Create to-status buffer for DATA"
  (let ((result (xmtn--status-inventory-sync (xmtn-propagate-to-work data))))
    (setf (xmtn-propagate-data-to-status-buffer data) (car result)
	  (xmtn-propagate-data-to-local-changes data) (cadr result))))

(defun xmtn-propagate-create-from-status-buffer (data)
  "Create from-status buffer for DATA"
  (let ((result (xmtn--status-inventory-sync (xmtn-propagate-from-work data))))
    (setf (xmtn-propagate-data-from-status-buffer data) (car result)
	  (xmtn-propagate-data-from-local-changes data) (cadr result))))

(defun xmtn-propagate-kill-status-buffers (data)
  (if (buffer-live-p (xmtn-propagate-data-from-status-buffer data))
      (kill-buffer (xmtn-propagate-data-from-status-buffer data)))
  (if (buffer-live-p (xmtn-propagate-data-to-status-buffer data))
      (kill-buffer (xmtn-propagate-data-to-status-buffer data))))

(defun xmtn-propagate-clean-1 (data save-conflicts)
  "Clean DATA workspace, kill associated automate session.
If SAVE-CONFLICTS non-nil, don't delete conflicts files."
  (xmtn-automate-kill-session (xmtn-propagate-from-work data))
  (xmtn-automate-kill-session (xmtn-propagate-to-work data))
  (xmtn-propagate-kill-conflicts-buffer data)
  (xmtn-propagate-kill-status-buffers data)
  (unless save-conflicts
    (xmtn-conflicts-clean (xmtn-propagate-to-work data))))

(defun xmtn-propagate-clean ()
  "Clean current workspace, delete from ewoc."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))

    (xmtn-propagate-clean-1 data nil)
    (let ((inhibit-read-only t))
      (ewoc-delete xmtn-propagate-ewoc elem))))

(defun xmtn-propagate-clean-all (&optional save-conflicts)
  "Clean all remaining workspaces."
  (interactive)
  (ewoc-map 'xmtn-propagate-clean-1 xmtn-propagate-ewoc save-conflicts))

(defun xmtn-propagate-cleanp ()
  "Non-nil if clean is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    ;; don't check need-refresh here; allow deleting after just doing
    ;; final required action in another buffer. Or we've just started,
    ;; but the user knows it's ok.
    (and (member (xmtn-propagate-data-from-local-changes data) '(need-scan ok))
         (member (xmtn-propagate-data-to-local-changes data) '(need-scan ok))
         (not (xmtn-propagate-data-propagate-needed data))
         (member (xmtn-propagate-data-to-heads data) '(need-scan at-head)))))

(defun xmtn-propagate-do-refresh-one ()
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-refresh-one data (or current-prefix-arg
                                         (not (xmtn-propagate-data-need-refresh data))))
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-refreshp ()
  "Non-nil if refresh is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (or (xmtn-propagate-data-need-refresh data)
        (eq 'need-scan (xmtn-propagate-data-from-local-changes data))
        (eq 'need-scan (xmtn-propagate-data-to-local-changes data)))))

(defun xmtn-propagate-commit-to ()
  "Show commit buffer for `to' workspace, so it can be committed, updated, or merged."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    ;; assume the commit is successful
    (setf (xmtn-propagate-data-to-local-changes data) 'ok)
    (if (not (buffer-live-p (xmtn-propagate-data-to-status-buffer data)))
	(xmtn-propagate-create-to-status-buffer data))
    (pop-to-buffer (xmtn-propagate-data-to-status-buffer data))))

(defun xmtn-propagate-commit-top ()
  "Non-nil if commit is appropriate for current `to' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
	 (member (xmtn-propagate-data-to-local-changes data) '(need-commit need-scan)))))

(defun xmtn-propagate-commit-from ()
  "Show commit buffer for `from' workspace, so it can be committed, updated, or merged."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    ;; assume the commit is successful
    (setf (xmtn-propagate-data-from-local-changes data) 'ok)
    (if (not (buffer-live-p (xmtn-propagate-data-from-status-buffer data)))
	(xmtn-propagate-create-from-status-buffer data))
    (pop-to-buffer (xmtn-propagate-data-from-status-buffer data))))

(defun xmtn-propagate-commit-fromp ()
  "Non-nil if commit is appropriate for current `from' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
	 (member (xmtn-propagate-data-from-local-changes data) '(need-commit need-scan)))))

(defun xmtn-propagate-update-to ()
  "Update current `to' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    (xmtn--update (xmtn-propagate-to-work data)
                  (xmtn-propagate-data-to-head-revs data)
                  nil t)
    (xmtn-propagate-refresh-one data nil)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-update-top ()
  "Non-nil if update is appropriate for current `to' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
	 (eq (xmtn-propagate-data-to-heads data)
	     'need-update))))

(defun xmtn-propagate-update-from ()
  "Update current `from' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    (xmtn--update (xmtn-propagate-from-work data)
                  (xmtn-propagate-data-from-head-revs data)
                  nil t)
    (xmtn-propagate-refresh-one data nil)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-update-fromp ()
  "Non-nil if update is appropriate for current `from' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
	 (eq (xmtn-propagate-data-from-heads data)
	     'need-update))))

(defun xmtn-propagate-propagate ()
  "Propagate current workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)

    (if (not (buffer-live-p (xmtn-propagate-data-conflicts-buffer data)))
        ;; user deleted conflicts buffer after resolving conflicts; get it back
	(xmtn-propagate-conflicts data))

    (with-current-buffer (xmtn-propagate-data-conflicts-buffer data)
      (let ((xmtn-confirm-operation nil))
	(save-some-buffers t); log buffer
	;; save-some-buffers does not save the conflicts buffer, which is the current buffer
	(save-buffer)
	(xmtn-propagate-from
	 (xmtn-propagate-data-from-branch data) ; = left
	 (xmtn-propagate-data-to-branch data) ; = right
	 )))
    (xmtn-propagate-refresh-one data nil)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-propagatep ()
  "Non-nil if propagate is appropriate for current workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (xmtn-propagate-data-propagate-needed data)
         (eq 'at-head (xmtn-propagate-data-from-heads data))
         (eq 'at-head (xmtn-propagate-data-to-heads data))
         (member (xmtn-propagate-data-conflicts data)
                 '(need-review-resolve-internal resolved none)))))

(defun xmtn-propagate-resolve-conflicts ()
  "Resolve conflicts for current workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    (setf (xmtn-propagate-data-conflicts data) 'ok)
    (pop-to-buffer (xmtn-propagate-data-conflicts-buffer data))))

(defun xmtn-propagate-resolve-conflictsp ()
  "Non-nil if resolve conflicts is appropriate for current workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (xmtn-propagate-data-propagate-needed data)
         (eq 'at-head (xmtn-propagate-data-from-heads data))
         (eq 'at-head (xmtn-propagate-data-to-heads data))
         (member (xmtn-propagate-data-conflicts data)
                 '(need-resolve need-review-resolve-internal)))))

(defun xmtn-propagate-local-changes-to-ok ()
  "Ignore local changes in current `to' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (setf (xmtn-propagate-data-to-local-changes data) 'ok)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-local-changes-top ()
  "Non-nil if local-changes-to-ok is appropriate for current `to' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (member (xmtn-propagate-data-to-local-changes data)
                 '(need-scan need-commit)))))

(defun xmtn-propagate-local-changes-from-ok ()
  "Ignore local changes in current `from' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (setf (xmtn-propagate-data-from-local-changes data) 'ok)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-local-changes-fromp ()
  "Non-nil if local-changes-from-ok is appropriate for current `from' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (member (xmtn-propagate-data-from-local-changes data)
                 '(need-scan need-commit)))))

(defun xmtn-propagate-status-to ()
  "Show status buffer for `to' workspace, so it can be committed, updated, or merged."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    (xmtn-status-one-1
     xmtn-propagate-to-root
     (xmtn-propagate-data-to-work data)
     (xmtn-propagate-data-to-head-revs data)
     (xmtn-propagate-data-to-status-buffer data)
     (xmtn-propagate-data-to-heads data)
     (xmtn-propagate-data-to-local-changes data))

    ;; Assume the user completely handles the local changes in the
    ;; status buffer, so they are now ok
    (setf (xmtn-propagate-data-to-local-changes data) 'ok)))

(defun xmtn-propagate-status-top ()
  "Non-nil if xmtn-status is appropriate for current `to' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (or
	  (member (xmtn-propagate-data-to-heads data)
		 '(need-update need-merge))
	  (eq (xmtn-propagate-data-to-local-changes data) 'need-commit)))))

(defun xmtn-propagate-status-from ()
  "Show status buffer for `from' workspace, so it can be committed, updated, or merged."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    (xmtn-status-one-1
     xmtn-propagate-from-root
     (xmtn-propagate-data-from-work data)
     (xmtn-propagate-data-from-head-revs data)
     (xmtn-propagate-data-from-status-buffer data)
     (xmtn-propagate-data-from-heads data)
     (xmtn-propagate-data-from-local-changes data))
    (setf (xmtn-propagate-data-from-local-changes data) 'ok)))

(defun xmtn-propagate-status-fromp ()
  "Non-nil if xmtn-status-one is appropriate for current `from' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
	 (or
	  (member (xmtn-propagate-data-from-heads data)
		  '(need-update need-merge))
	  (eq (xmtn-propagate-data-from-local-changes data) 'need-commit)))))

(defun xmtn-propagate-quit-save ()
  "Quit, but save conflicts files for later resume."
  (interactive)
  (remove-hook 'kill-buffer-hook 'xmtn-propagate-clean-all t)
  (xmtn-propagate-clean-all t)
  (kill-buffer))

(defvar xmtn-propagate-actions-map
  (let ((map (make-sparse-keymap "actions")))
    (define-key map [?c]  '(menu-item "c) clean/delete"
                                      xmtn-propagate-clean
                                      :visible (xmtn-propagate-cleanp)))
    (define-key map [?g]  '(menu-item "g) refresh"
                                      xmtn-propagate-do-refresh-one
                                      :visible (xmtn-propagate-refreshp)))
    (define-key map [?9]  '(menu-item (concat "9) status " (xmtn-propagate-to-name))
                                      xmtn-propagate-status-to
                                      :visible (xmtn-propagate-status-top)))
    (define-key map [?8]  '(menu-item (concat "8) status " (xmtn-propagate-from-name))
                                      xmtn-propagate-status-from
                                      :visible (xmtn-propagate-status-fromp)))
    (define-key map [?7]  '(menu-item (concat "7) update " (xmtn-propagate-to-name))
                                      xmtn-propagate-update-to
                                      :visible (xmtn-propagate-update-top)))
    (define-key map [?6]  '(menu-item (concat "6) update " (xmtn-propagate-from-name))
                                      xmtn-propagate-update-from
                                      :visible (xmtn-propagate-update-fromp)))
    (define-key map [?5]  '(menu-item "5) propagate"
                                      xmtn-propagate-propagate
                                      :visible (xmtn-propagate-propagatep)))
    (define-key map [?4]  '(menu-item "4) resolve conflicts"
                                      xmtn-propagate-resolve-conflicts
                                      :visible (xmtn-propagate-resolve-conflictsp)))
    (define-key map [?3]  '(menu-item (concat "3) ignore local changes " (xmtn-propagate-to-name))
                                      xmtn-propagate-local-changes-to-ok
                                      :visible (xmtn-propagate-local-changes-top)))
    (define-key map [?2]  '(menu-item (concat "2) ignore local changes " (xmtn-propagate-from-name))
                                      xmtn-propagate-local-changes-from-ok
                                      :visible (xmtn-propagate-local-changes-fromp)))
    (define-key map [?1]  '(menu-item (concat "1) commit " (xmtn-propagate-to-name))
                                      xmtn-propagate-commit-to
                                      :visible (xmtn-propagate-commit-top)))
    (define-key map [?0]  '(menu-item (concat "0) commit " (xmtn-propagate-from-name))
                                      xmtn-propagate-commit-from
                                      :visible (xmtn-propagate-commit-fromp)))
    map)
  "Keyboard menu keymap used to manage propagates.")

(dvc-make-ewoc-next xmtn-propagate-next xmtn-propagate-ewoc)
(dvc-make-ewoc-prev xmtn-propagate-prev xmtn-propagate-ewoc)

(defvar xmtn-propagate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-d" xmtn-propagate-actions-map)
    (define-key map [?g]  'xmtn-propagate-refresh)
    (define-key map [?n]  'xmtn-propagate-next)
    (define-key map [?p]  'xmtn-propagate-prev)
    (define-key map [?s]  'xmtn-propagate-quit-save)
    (define-key map [?q]  'dvc-buffer-quit)
    map)
  "Keymap used in `xmtn-propagate-mode'.")

(easy-menu-define xmtn-propagate-mode-menu xmtn-propagate-mode-map
  "Mtn specific status menu."
  `("DVC-Mtn"
    ["Do the right thing"    xmtn-status-actions-map t]
    ["Quit, clean conflicts" dvc-buffer-quit t]
    ["Quit, save conflicts"  xmtn-propagate-quit-save t]
    ))

(define-derived-mode xmtn-propagate-mode nil "xmtn-propagate"
  "Major mode to propagate multiple workspaces."
  (setq dvc-buffer-current-active-dvc 'xmtn)
  (setq buffer-read-only nil)

  ;; don't do normal clean up stuff
  (set (make-local-variable 'before-save-hook) nil)
  (set (make-local-variable 'write-file-functions) nil)

  (dvc-install-buffer-menu)
  (add-hook 'kill-buffer-hook 'xmtn-propagate-clean-all nil t)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil)
  (xmtn-propagate-refresh)
  (xmtn-propagate-next nil t))

(defun xmtn-propagate-needed (data)
  "t if DATA needs propagate."
  (let ((result t)
        (from-work (xmtn-propagate-from-work data))
        (from-head-rev (xmtn-propagate-data-from-head-revs data))
        (to-head-rev   (xmtn-propagate-data-to-head-revs data)))

    (if (or (listp from-head-rev)
            (listp to-head-rev))
        ;; multiple heads; can't propagate
        (setq result nil)

      ;; cases:
      ;; 1) to branched off earlier, and propagate is needed
      ;; 2) propagate was just done but required no changes; no propagate needed
      ;;
      (if (string= from-head-rev to-head-rev)
          ;; case 2
          (setq result nil)
        (let ((descendents (xmtn-automate-command-output-lines from-work (list "descendents" from-head-rev)))
              done)
          (if (not descendents)
              ;; case 1
              (setq result t)
            (while (and descendents (not done))
              (if (string= to-head-rev (car descendents))
                  (progn
                    (setq result nil)
                    (setq done t)))
              (setq descendents (cdr descendents)))))))
    result
  ))

(defun xmtn-propagate-conflicts (data)
  "Return value for xmtn-propagate-data-conflicts for DATA."
  ;; Only called if neither side needs merge. See
  ;; xmtn-propagate-propagate for assignment of 'left' = 'from'.
  (let ((result (xmtn-conflicts-status
		 (xmtn-propagate-data-conflicts-buffer data) ; buffer
		 (xmtn-propagate-from-work data) ; left-work
		 (xmtn-propagate-data-from-head-revs data) ; left-rev
		 (xmtn-propagate-to-work data) ; right-work
		 (xmtn-propagate-data-to-head-revs data) ; right-rev
		 (xmtn-propagate-data-from-branch data) ; left-branch
		 (xmtn-propagate-data-to-branch data) ; right-branch
		 )))
    (setf (xmtn-propagate-data-conflicts-buffer data) (car result))
    (cadr result)))

(defun xmtn-propagate-refresh-one (data refresh-local-changes)
  "Refresh DATA."
  (let ((from-work (xmtn-propagate-from-work data))
        (to-work (xmtn-propagate-to-work data)))

    (dvc-trace "xmtn-propagate-refresh-one: %s" from-work)

    (let ((heads (xmtn--heads from-work (xmtn-propagate-data-from-branch data)))
          (from-base-rev (xmtn--get-base-revision-hash-id-or-null from-work)))
      (case (length heads)
        (1
         (setf (xmtn-propagate-data-from-head-revs data) (nth 0 heads))
         (if (string= (xmtn-propagate-data-from-head-revs data) from-base-rev)
               (setf (xmtn-propagate-data-from-heads data) 'at-head)
             (setf (xmtn-propagate-data-from-heads data) 'need-update)))
        (t
         (setf (xmtn-propagate-data-from-head-revs data) (list (nth 0 heads) (nth 1 heads)))
         (setf (xmtn-propagate-data-from-heads data) 'need-merge))))

    (let ((heads (xmtn--heads to-work (xmtn-propagate-data-to-branch data)))
          (to-base-rev (xmtn--get-base-revision-hash-id-or-null to-work)))
      (case (length heads)
        (1
         (setf (xmtn-propagate-data-to-head-revs data) (nth 0 heads))
         (if (string= (xmtn-propagate-data-to-head-revs data) to-base-rev)
               (setf (xmtn-propagate-data-to-heads data) 'at-head)
             (setf (xmtn-propagate-data-to-heads data) 'need-update)))
        (t
         (setf (xmtn-propagate-data-to-head-revs data) (list (nth 0 heads) (nth 1 heads)))
         (setf (xmtn-propagate-data-to-heads data) 'need-merge))))

    (setf (xmtn-propagate-data-propagate-needed data)
          (xmtn-propagate-needed data))

    (if refresh-local-changes
        (progn
          (setf (xmtn-propagate-data-from-local-changes data) 'need-scan)
          (setf (xmtn-propagate-data-to-local-changes data) 'need-scan)))

    (ecase (xmtn-propagate-data-from-local-changes data)
      (need-scan
       (xmtn-propagate-create-from-status-buffer data))
      (t nil))

    (ecase (xmtn-propagate-data-to-local-changes data)
      (need-scan
       (xmtn-propagate-create-to-status-buffer data))
      (t nil))

    (if (xmtn-propagate-data-propagate-needed data)
        (progn
          (if refresh-local-changes
              (progn
                (xmtn-propagate-kill-conflicts-buffer data)
                (xmtn-conflicts-clean (xmtn-propagate-to-work data))))

          (setf (xmtn-propagate-data-conflicts data)
                (xmtn-propagate-conflicts data)))

      ;; can't compute conflicts if propagate not needed
      (setf (xmtn-propagate-data-conflicts data) 'need-scan))

    (setf (xmtn-propagate-data-need-refresh data) nil))

  ;; return non-nil to refresh display as we go along
  t)

(defun xmtn-propagate-refresh ()
  "Refresh status of each ewoc element. With prefix arg, reset local changes status to `unknown'."
  (interactive)
  (ewoc-map 'xmtn-propagate-refresh-one xmtn-propagate-ewoc current-prefix-arg)
  ;; leaves point at (point-min)
  (xmtn-propagate-next nil t)
  (message "done"))

(defun xmtn-propagate-make-data (from-workspace to-workspace from-name to-name)
  "FROM-WORKSPACE, TO-WORKSPACE are relative names, FROM-NAME, TO_NAME should be root dir names."
    (let* ((from-work (concat xmtn-propagate-from-root from-workspace))
           (to-work (concat xmtn-propagate-to-root to-workspace))
           )

      (ewoc-enter-last
       xmtn-propagate-ewoc
       (make-xmtn-propagate-data
        :from-work from-workspace
        :to-work to-workspace
        :from-name from-name
        :to-name to-name
        :from-branch (xmtn--tree-default-branch from-work)
        :to-branch (xmtn--tree-default-branch to-work)
        :need-refresh t))))

;;;###autoload
(defun xmtn-propagate-multiple (from-dir to-dir &optional workspaces)
  "Show all actions needed to propagate projects under FROM-DIR
to TO-DIR. WORKSPACES (default nil) is a list of workspaces
common to from-dir and to-dir; if nil, the directories are
scanned and all common ones found are used."
  (interactive "DPropagate all from (root directory): \nDto (root directory): ")
  (pop-to-buffer (get-buffer-create "*xmtn-propagate*"))
  ;; xmtn-propagate-*-root are buffer-local. Note that we don't care
  ;; what 'default-directory' is for xmtn-propagate buffer.
  (setq xmtn-propagate-from-root (file-name-as-directory (expand-file-name (substitute-in-file-name from-dir))))
  (setq xmtn-propagate-to-root (file-name-as-directory (expand-file-name (substitute-in-file-name to-dir))))
  (let ((from-workspaces (or workspaces
                             (xmtn--filter-non-ws xmtn-propagate-from-root)))
        (to-workspaces (or workspaces
                           (xmtn--filter-non-ws xmtn-propagate-to-root))))

    (setq xmtn-propagate-ewoc (ewoc-create 'xmtn-propagate-printer))
    (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
    (ewoc-set-hf
     xmtn-propagate-ewoc
     (concat
      (format "From root : %s\n" xmtn-propagate-from-root)
      (format "  To root : %s\n" xmtn-propagate-to-root)
      )
     "")
    (dolist (workspace from-workspaces)
      (if (member workspace to-workspaces)
          (xmtn-propagate-make-data
           workspace
           workspace
           (file-name-nondirectory (directory-file-name xmtn-propagate-from-root))
           (file-name-nondirectory (directory-file-name xmtn-propagate-to-root)))))
    (redisplay)
    (xmtn-propagate-mode)))

;;;###autoload
(defun xmtn-propagate-one (from-work to-work)
  "Show all actions needed to propagate FROM-WORK to TO-WORK."
  (interactive "DPropagate all from (workspace): \nDto (workspace): ")
  (setq from-work (file-name-as-directory (expand-file-name (substitute-in-file-name from-work))))
  (setq to-work (file-name-as-directory (expand-file-name (substitute-in-file-name to-work))))
  (pop-to-buffer (get-buffer-create "*xmtn-propagate*"))
  (setq default-directory to-work)
  (setq xmtn-propagate-from-root (expand-file-name (concat from-work "../")))
  (setq xmtn-propagate-to-root (expand-file-name (concat to-work "../")))
  (setq xmtn-propagate-ewoc (ewoc-create 'xmtn-propagate-printer))
  (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
  (ewoc-set-hf
   xmtn-propagate-ewoc
   (concat
    (format "From root : %s\n" xmtn-propagate-from-root)
    (format "  To root : %s\n" xmtn-propagate-to-root)
    )
   "")
  (let ((from-name (file-name-nondirectory (directory-file-name from-work)))
	(to-name (file-name-nondirectory (directory-file-name to-work))))
    (if (string-equal from-name to-name)
	(progn
	  (setq from-name (file-name-nondirectory (directory-file-name xmtn-propagate-from-root)))
	  (setq to-name (file-name-nondirectory (directory-file-name xmtn-propagate-to-root)))))
    (xmtn-propagate-make-data
     (file-name-nondirectory (directory-file-name from-work))
     (file-name-nondirectory (directory-file-name to-work))
     from-name
     to-name))
  (xmtn-propagate-mode))

(provide 'xmtn-propagate)

;; end of file
