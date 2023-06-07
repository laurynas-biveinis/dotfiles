;;; my-org.el --- everything related to org.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This configures Org, `org-roam', and everything related.

;;; Code:

;; Defined in secrets.el:
(defvar main-org-file)
(defvar secrets-org-file)

(require 'my-column-limit)

(require 'org)
(setq org-M-RET-may-split-line '((default . nil)))
(require 'org-element)
(setq org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)
(require 'org-keys)
(setq org-return-follows-link t)
;; Common
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(require 'org-agenda)
(require 'org-clock)
(require 'org-capture)
(setq org-use-speed-commands t
      org-log-done t
      org-default-notes-file main-org-file)
;; `org-mobile'
(require 'org-mobile)
(setq org-mobile-inbox-for-pull main-org-file
      org-ctrl-k-protect-subtree t
      org-support-shift-select t
      org-yank-adjusted-subtrees t
      org-fold-catch-invisible-edits 'smart
      org-fontify-todo-headline t
      org-fontify-done-headline t
      org-adapt-indentation nil)

;; Tags
(setq org-tag-alist '((:startgroup . nil)
                      ("@agenda" . ?a)
                      ("@call" . ?t)
                      ("@checklist" . ?l)
                      ("@computer" . ?c)
                      ("@home" . ?h)
                      ("@internet" . ?i)
                      ("@phone" . ?f)
                      ("@vilnius" . ?v)
                      ("@waitingfor" . ?w)
                      ("@watchlisten" . ?z)
                      (:endgroup . nil)
                      ("project" . ?p)
                      ("somedaymaybe" . ?s)
                      ("crypt" . ?k)))

(setq org-use-tag-inheritance '("somedaymaybe" "@watchlisten")
      org-agenda-tags-todo-honor-ignore-options t
      org-fast-tag-selection-single-key 'expert
      org-agenda-dim-blocked-tasks nil) ;; Build agenda buffers faster

;; Agendas
(setq org-agenda-custom-commands
      '(("c" "Calls" tags-todo "@call-somedaymaybe/!TODO")
        ("p" "Projects" tags-todo "project-somedaymaybe/!TODO")
        ("l" "Checklists" tags "@checklist-somedaymaybe")
        ("k" "Someday/maybe" tags-todo "somedaymaybe+LEVEL=2"
         ((org-agenda-dim-blocked-tasks nil)))
        ("v" "Vilnius" tags-todo "@vilnius-somedaymaybe/!TODO")
        ("n" "Non-project tasks" tags-todo "-project-@waitingfor-somedaymaybe/!TODO"
         ((org-use-tag-inheritance '("project" "somedaymaybe"))))
        ("A" "Agenda"
         ((agenda "" nil)
          (tags-todo "@phone-somedaymaybe|@call-somedaymaybe|@internet-somedaymaybe|@computer-somedaymaybe/!TODO"
                     ((org-agenda-overriding-header "Common next actions")
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (tags-todo "@agenda-somedaymaybe/!TODO"
                     ((org-agenda-overriding-header "Agendas")
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (tags-todo "@home-somedaymaybe/!TODO"
                     ((org-agenda-overriding-header "Home actions")
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (tags-todo "@waitingfor-somedaymaybe/!TODO"
                     ((org-agenda-overriding-header "Waiting for")
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (tags-todo "@vilnius-somedaymaybe/!TODO"
                     ((org-agenda-overriding-header "Errands")
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (tags-todo "@watchlisten-somedaymaybe/!TODO"
                     ((org-agenda-overriding-header "Watch/listen")
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (todo "TIME"
                ((org-agenda-overriding-header "Time log actions")
                 (org-agenda-dim-blocked-tasks 'invisible)))
          (tags "-project/+DONE|+KILL"
                ((org-agenda-overriding-header "Archivable tasks")
                 (org-use-tag-inheritance '("project"))))
          (todo "-@agenda-@phone-@call-@internet-@computer-@home-@watchlisten-@vilnius-@waitingfor-@checklist-project-somedaymaybe"
                ((org-agenda-overriding-header "Contextless tasks")))))))

(setq org-agenda-start-on-weekday nil
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-todo-ignore-scheduled 'all
      org-agenda-todo-ignore-deadlines 'all
      org-agenda-todo-ignore-timestamp 'all)

(setq org-agenda-clock-consistency-checks
      (list
       :max-duration "6:00"
       :min-duration "0:00"
       :max-gap "0:05"
       :gap-ok-around (list "2:00" "12:30")))

(setq org-agenda-sticky t
      org-agenda-window-setup 'current-window)

;; Scheduling and deadlines
(setq org-deadline-warning-days 30)

;; Drawers

;; Clock tables
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

;; Logging
(setq org-log-into-drawer t
      org-clock-into-drawer t
      org-closed-keep-when-no-todo t)

;; Refiling
(setq org-refile-targets '((org-agenda-files :maxlevel . 9))
      ;; or `'buffer-name' starting with 9.1, not much difference in my setup
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm
      org-log-refile 'time)

;; Borrowed from https://emacs.nasy.moe/
(defun dotfiles--org-verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function #'dotfiles--org-verify-refile-target)

(setq org-clock-display-default-range 'untilnow
      org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline main-org-file "Tasks")
         "** TODO %?\n  %i\n  %a")
        ("i" "Inbox" entry (file+headline main-org-file "Inbox")
         "** %?\n  %i\n  %a")
        ("c" "Current" plain (clock) "" :clock-in :clock-keep)))
(setq org-todo-keywords
      '((sequence "WAIT(w!)" "TODO(t!)" "|" "DONE(d!)" "KILL(k!)")
        (sequence "TIME(l!)" "|")))

(setq org-todo-keyword-faces
      '(("WAIT" . (:foreground "OrangeRed" :weight bold))
        ("TIME" . (:foreground "OrangeRed" :weight bold))
        ("TODO" . (:foreground "Red" :weight bold))))

(require 'org-habit)
(setq org-habit-graph-column 50)

(setq org-log-redeadline t
      org-log-reschedule t
      org-stuck-projects '("+project-somedaymaybe/!TODO" ("TODO") nil "")
      org-todo-repeat-to-state "TODO"
      org-use-fast-todo-selection 'expert
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-cycle-separator-lines 1
      ;; TODO(laurynas): compute these columns automatically
      org-tags-column -85
      org-agenda-tags-column 'auto
      org-table-header-line-p t)

;;; org-checklist
;; Comes from org-contrib
(require 'org-checklist)

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

;; org-mode encryption of selected subtrees
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-crypt-disable-auto-save 'encrypt)

(defun dotfiles--org-mode-flyspell-verify-disable-for-org-crypt ()
  "Do not flyspell blocks encrypted by `org-crypt'."
  (not (org-at-encrypted-entry-p)))

(advice-add 'org-mode-flyspell-verify :before-while
            #'dotfiles--org-mode-flyspell-verify-disable-for-org-crypt)


;; A hack, it is surprising no official function for this exists. But then
;; again, I need to `string-trim' it too.
(defun my-copy-cell ()
  "Copy the current org table cell to the kill ring."
  (interactive nil org-mode)
  (let ((p (point)))
    (org-table-copy-region p p))
  (kill-new (string-trim (caar org-table-clip))))

(define-key org-mode-map (kbd "<f7>") #'my-copy-cell)

;; org-id
(require 'org-id)
(setq org-id-link-to-org-use-id t)

;; Save org buffers automatically
(add-hook 'auto-save-hook #'org-save-all-org-buffers)

(require 'org-roam-node)
(defun dotfiles--org-mode-hook ()
  "My configuration hook for 'org-mode'."
  (local-set-key (kbd "C-c C-x C-k") #'org-decrypt-entry)
  (local-set-key (kbd "C-c n i") #'org-roam-node-insert)
  (local-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
  (dotfiles--set-fill-column 85))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
         (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
Created: %U
")
         :unnarrowed t)))

(add-hook 'org-mode-hook #'dotfiles--org-mode-hook)

;; org integration with Helm
(setq org-outline-path-complete-in-steps nil)

;;; `org-sticky-header'
(require 'org-sticky-header)
(add-hook 'org-mode-hook #'org-sticky-header-mode)

;;; org-roam
(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section #'org-roam-reflinks-section
            #'org-roam-unlinked-references-section))

(require 'org-roam-db)
(org-roam-db-autosync-mode)

(provide 'my-org)
;;; my-org.el ends here
