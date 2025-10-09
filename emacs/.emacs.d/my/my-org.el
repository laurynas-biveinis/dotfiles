;;; my-org.el --- everything related to `org'.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This configures `org', `org-babel', `org-roam', and everything related.
;; Expects that `org-tag-alist', `org-agenda-custom-commands',
;; `org-capture-templates', `org-todo-keywords', and
;; `org-roam-capture-templates' are set elsewhere, in secrets.el in my setup.
;; That file must be loaded first.
;;
;; The configuration of `org-roam' is optional and gated on `my-zettelkasten-p'
;; value.
;;
;; Like in the rest of my personal configuration, all features (packages and
;; external tools) are assumed to exist, because this is a part of my dotfiles
;; repo where the needed packages are committed too. Thus, no error handling,
;; and no need to ensure compatibility with different Emacs or package versions.
;;
;; Custom keybindings:
;; C-c C-x C-k - Decrypt encrypted org entry
;; C-c C-x C-u - Open URL at point with org-autotask
;; C-c n i     - Insert a link to `org-roam' entry
;; C-c n l     - Toggle the `org-roam' buffer
;; C-c x       - Copy org table cell contents

;;; Code:

;; Defined in secrets.el:
(defvar main-org-file)
(defvar secrets-org-file)
(defvar my-zettelkasten-p)

(require 'my-column-limit)

;; Needs to be set before `org' is loaded
(defvar org-enforce-todo-checkbox-dependencies)
(setq org-enforce-todo-checkbox-dependencies t)

(require 'org)
(require 'org-element)
(require 'org-keys)
(require 'org-agenda)
(require 'org-clock)
(require 'org-capture)
(require 'org-footnote)
(require 'org-mobile)
(require 'org-habit)
(require 'org-crypt)
(require 'org-id)
(require 'org-sticky-header)

(when my-zettelkasten-p
  (require 'org-roam-node))


;;; Setup hook: keyboard shortcuts and fill column setting
(defun dotfiles--update-org-tags-column (&optional _frame)
  "Update `org-tags-column' based on current window width.
Positions tags 3 columns from the right edge, with bounds checking to prevent
overlap (min -85) and invalid positive values (max -1).  Does not realign
existing tags; they will align on next edit or manual realignment."
  (setq-local org-tags-column
              (max -85 (min -1 (- 3 (window-body-width))))))

(defun dotfiles--org-mode-hook ()
  "My configuration hook for `org-mode'.
Sets up keybindings and adjusts the fill column."
  ;; Keybindings
  (local-set-key (kbd "C-c C-x C-k") #'org-decrypt-entry)
  (local-set-key (kbd "C-c C-x C-u") #'org-autotask-open-url-at-point)
  (when my-zettelkasten-p
    (local-set-key (kbd "C-c n i") #'org-roam-node-insert)
    (local-set-key (kbd "C-c n l") #'org-roam-buffer-toggle))
  ;; Fill column
  (dotfiles--set-fill-column 85)
  ;; Dynamic tags column with window resize handling
  (dotfiles--update-org-tags-column)
  (add-hook 'window-size-change-functions
            #'dotfiles--update-org-tags-column
            nil t))
(add-hook 'org-mode-hook #'dotfiles--org-mode-hook)

;;; Editing, navigation, folding, state changes
(setq org-use-speed-commands t
      org-M-RET-may-split-line '((default . nil))
      org-support-shift-select t
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-ctrl-k-protect-subtree t
      org-yank-adjusted-subtrees t
      org-cycle-separator-lines 0
      org-use-fast-todo-selection 'expert
      org-fast-tag-selection-single-key 'expert)

;;; Saving
(add-hook 'auto-save-hook #'org-save-all-org-buffers)  ;; Save automatically

;;; Footnotes
(setq org-footnote-auto-adjust t)

;;; Display
(setq org-fontify-todo-headline t
      org-fontify-done-headline t
      org-habit-graph-column 65  ;; TODO(laurynas): compute automatically
      org-image-actual-width 720 ;; TODO(laurynas): compute automatically
      org-startup-with-inline-images t
      org-startup-folded 'showall)
(add-hook 'org-mode-hook #'org-sticky-header-mode)

;;; Agenda
(setq org-agenda-tags-todo-honor-ignore-options t
      org-agenda-dim-blocked-tasks nil  ;; Build agenda buffers faster
      org-agenda-start-on-weekday nil
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-todo-ignore-scheduled 'all
      org-agenda-todo-ignore-deadlines 'all
      org-agenda-todo-ignore-timestamp 'all
      org-agenda-sticky t
      org-agenda-window-setup 'current-window)

;;; Logging
(setq org-log-done 'time)

;;; Capture
(setq org-default-notes-file main-org-file)

;;; Scheduling and deadlines
(setq org-deadline-warning-days 30
      org-log-redeadline 'time
      org-log-reschedule 'time)

;;; Time tracking (clocking)
(setq org-clock-display-default-range 'untilnow
      org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;; Clock tables
(setq org-clocktable-defaults
      (list
       :maxlevel 99
       :scope 'agenda-with-archives
       :stepskip0 t
       :fileskip0 t
       :narrow 45
       :link t
       :indent t
       :tcolumns 0))

;;; Logging
;; Since tasks start out in "WAIT", this setting logs the time of the change to
;; "TODO", roughly serving as the task creation timestamp.
(setq org-log-into-drawer t)
(setq org-closed-keep-when-no-todo t)

;;; Linking
(setq org-return-follows-link t
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;;; Refiling
(setq org-refile-targets '((org-agenda-files :maxlevel . 9))
      ;; or `'buffer-name' starting with 9.1, not much difference in my setup
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm
      org-log-refile 'time
      org-outline-path-complete-in-steps nil)

;; Borrowed from https://emacs.nasy.moe/
(defun dotfiles--org-verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function #'dotfiles--org-verify-refile-target)

;;; Mobile
(setq org-mobile-inbox-for-pull main-org-file)

;;; Tables
(setq org-table-header-line-p t)

;; A hack, it is surprising no official function for this exists. But then
;; again, I need to `string-trim' it too.
(defun my-copy-cell ()
  "Copy the current org table cell to the kill ring."
  (interactive nil org-mode)
  (let ((p (point)))
    (org-table-copy-region p p))
  (kill-new (string-trim (caar org-table-clip))))

(define-key org-mode-map (kbd "C-c x") #'my-copy-cell)

;;; Encryption
(org-crypt-use-before-save-magic)
(setq org-crypt-disable-auto-save 'encrypt)

;; Integrate with `flyspell' by disabling it over encrypted blocks to avoid
;; random wiggles under random passwords.
(defun dotfiles--org-mode-flyspell-verify-disable-for-org-crypt ()
  "Do not flyspell blocks encrypted by `org-crypt'."
  (not (org-at-encrypted-entry-p)))
(advice-add 'org-mode-flyspell-verify :before-while
            #'dotfiles--org-mode-flyspell-verify-disable-for-org-crypt)


;;; `org-babel'
(org-babel-do-load-languages 'org-babel-load-languages
                             (append org-babel-load-languages
                                     '((plantuml . t))))

;; Try to workaround `ob-rust' ruining `org-id-locations-file':
;; https://github.com/micanzhang/ob-rust/issues/12
(with-eval-after-load 'ob-rust-test
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations"))

(add-hook 'org-todo-repeat-hook #'org-reset-checkbox-state-subtree)

;; Make C-c C-c on a checkbox item check it and move point to the next unchecked
;; item. It's magic from Internet:
;; https://emacs.stackexchange.com/a/17281/16376
;; TODO(laurynas): make it work only on unchecked items
(defmacro dotfiles--with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

(defun dotfiles--org-checkbox-toggle-advice (orig-fn &rest args)
  "Advice ORIG-FN with ARGS to move to next list item on checkbox toggle."
  (dotfiles--with-advice
      ((#'org-update-checkbox-count-maybe
        :after (lambda ()
                 (ignore-errors (org-next-item)))))
    (apply orig-fn args)))

(advice-add #'org-ctrl-c-ctrl-c   :around #'dotfiles--org-checkbox-toggle-advice)
(advice-add #'org-toggle-checkbox :around #'dotfiles--org-checkbox-toggle-advice)

;; Change task to DONE if all checkboxes are checked off, taken from
;; https://orgmode.org/worg/org-hacks.html#mark-done-when-all-checkboxes-checked
(defun dotfiles--checkbox-list-complete ()
  "Mark heading done when all checkboxes are completed."
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
          (if (match-end 1)
              (if (equal (match-string 1) "100%")
                  ;; all done - do the state change
                  (org-todo "DONE")
                (org-todo "TODO"))
            (if (and (> (match-end 2) (match-beginning 2))
                     (equal (match-string 2) (match-string 3)))
                (org-todo "DONE")
              (org-todo "TODO")))))))
(add-hook 'org-checkbox-statistics-hook #'dotfiles--checkbox-list-complete)

(defun org-dblock-write:filecount (params)
  "Write a dynamic block that counts files in a :dir from PARAMS."
  (let ((dir (plist-get params :dir)))
    (insert (format "Files in %s: %d" dir
                    (- (length (directory-files dir)) 2)))))

;;; `org-roam'
(when my-zettelkasten-p
  (progn
    (setq org-roam-database-connector 'sqlite-builtin)
    (setq org-roam-mode-sections
          (list #'org-roam-backlinks-section #'org-roam-reflinks-section
                #'org-roam-unlinked-references-section))

    (require 'org-roam-db)
    (org-roam-db-autosync-mode)))

;;; `org-gcal'

;; Do not require `org-gcal' to avoid the warning about needing to call
;; `org-gcal-reload-client-id-secret'

;; TODO(laurynas): fetch automatically?
;; TODO(laurynas): Can `org-gcal-local-timezeone' be automated?
;; TODO(laurynas): key binding for `org-gcal-post-at-point'?
;; TODO(laurynas): key binding (and some command) to create a new event? How to
;; check for scheduling conflicts?

(provide 'my-org)
;;; my-org.el ends here
