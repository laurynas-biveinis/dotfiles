;;; addon-modes.el --- Configure extra packages.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Variables and functions defined elsewhere we'll be using
(defvar google-c-style)
(defvar auto-indent-key-for-end-of-line-then-newline)
(defvar emacs-24-4-or-later)
(defvar TeX-auto-save)
(defvar TeX-parse-self)
(defvar TeX-expand-list)
(defvar TeX-command-list)
(defvar TeX-source-correlate-start-server)
(defvar reftex-plug-into-AUCTeX)
(defvar bib-cite-use-reftex-view-crossref)
(defvar nxml-slash-auto-complete-flag)
(defvar main-org-file)
(defvar autopair-dont-activate)
(defvar darkstar-laptop-screen)
(defvar darkstar-laptop-geometry)
(defvar darkstar-external-screen)
(defvar darkstar-external-geometry)
(defvar darkstar-ignore)
(defvar darkstar-ignore2)
(defvar darkstar-ignore3)
(defvar darkstar-ignore4)
(declare-function turn-on-flyspell "flyspell" ())
(declare-function autopair-global-mode "autopair" (&optional arg))
(declare-function LaTeX-install-toolbar "tex-bar" ())
(declare-function TeX-source-correlate-mode "tex" (&optional arg))
(declare-function move-to-frame-geometry "" (geometry))
(declare-function two-windows "" ())
(declare-function six-windows "" ())
(declare-function start-erc-chats "" ())
(declare-function diagnose-unknown-display-geometry "" (geometry))
(declare-function enable-show-trailing-ws "" ())

;;; undo-tree
(require 'undo-tree)
(add-to-list 'undo-tree-incompatible-major-modes #'help-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'Info-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'grep-mode)
(require 'magit)
(add-to-list 'undo-tree-incompatible-major-modes #'magit-status-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'package-menu-mode)
(global-undo-tree-mode)

;; Google C style
(c-add-style "google" google-c-style)

;;; auto-indent-mode
(setq auto-indent-key-for-end-of-line-then-newline "<M-RET>")
(require 'auto-indent-mode)
(add-to-list 'auto-indent-disabled-modes-list #'help-mode)
(add-to-list 'auto-indent-disabled-modes-list #'Info-mode)
(add-to-list 'auto-indent-disabled-modes-list #'magit-status-mode)
(add-to-list 'auto-indent-disabled-modes-list #'org-agenda-mode)
(add-to-list 'auto-indent-disabled-modes-list #'erc-mode)
(add-to-list 'auto-indent-disabled-modes-list #'grep-mode)
(require 'term)
(add-to-list 'auto-indent-disabled-modes-list #'term-mode)
(add-to-list 'auto-indent-disabled-modes-list #'package-menu-mode)
(auto-indent-global-mode)
;; Leave tabs/spaces alone on paste. TODO(laurynas): we would like to DTRT
;; instead, not sure how
(setq auto-indent-mode-untabify-on-yank-or-paste nil)
;; Leave tabs/spaces alone on save.
(setq auto-indent-untabify-on-save-file nil)
;; Make delete less hungry, but hungry nevertheless
(setq auto-indent-backward-delete-char-behavior 'hungry)
;; Don't add bunch of WS just by scrolling
(setq auto-indent-blank-lines-on-move nil)

;;; Autopair, only in 24.3-
(unless emacs-24-4-or-later
  (require 'autopair)
  (autopair-global-mode))

;; Disable linum where it makes sense and fixes performance
(require 'linum-off)

;; Imenu
(require 'imenu+)
(defun try-to-add-imenu ()
  "Try to add imenu to menu bar, silently eating any errors."
  (condition-case nil (imenu-add-menubar-index) (error nil)))
(add-hook 'font-lock-mode-hook #'try-to-add-imenu)

;;;; Programming language modes

;; ssh mode on the top of shell
(require 'ssh)

;;;; AUCTeX and other TeX stuff

;; Enable document parsing for AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Make AUCTeX aware of multifile documents
(setq-default TeX-master nil)

(defun my-latex-mode-hook ()
  "My configuration hook for 'latex-mode'."
  (LaTeX-install-toolbar)
  (turn-on-reftex)
  (turn-on-flyspell)
  (enable-show-trailing-ws)
  ;; Source specials
  (TeX-source-correlate-mode 1)
  ;; Set up -pdf option for latexmk
  (push
   '("%(-PDF)"
     (lambda ()
       (if (and
            (eq TeX-engine 'default)
            (or TeX-PDF-mode TeX-DVI-via-PDFTeX))
           "-pdf" "")))
   TeX-expand-list)
  ;; Use latexmk
  (push
   '("latexmk" "latexmk %(-PDF) %s" TeX-run-TeX nil t
     :help "Run latexmk on file")
   TeX-command-list))

(add-hook 'LaTeX-mode-hook #'my-latex-mode-hook)
(setq TeX-source-correlate-start-server t)

;; Integrate RefTeX into AUCTeX
(setq reftex-plug-into-AUCTeX t)

;; Integrate RefTeX with bib-cite
(setq bib-cite-use-reftex-view-crossref t)

;;; po-mode.el for PO file editing
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)
(add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))

(require 'po)
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
   'po-find-file-coding-system)
(autoload 'po-find-file-coding-system "po-mode")

;;; nXML
;; Autocomplete closing tags
(setq nxml-slash-auto-complete-flag t)
(add-to-list 'auto-mode-alist
             '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))

;;; org-mode
;; Prerequisites: const main-org-file and list org-agenda-files, that must be
;; set elsewhere (i.e. secrets.el)
(require 'org)
(require 'org-element)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
;; Bendra
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(require 'org-agenda)
(require 'org-clock)
(require 'org-capture)
(define-key global-map "\C-cl" #'org-store-link)
(define-key global-map "\C-ca" #'org-agenda)
(define-key global-map "\C-cc" #'org-capture)
(define-key global-map "\C-c\C-x\C-o" #'org-clock-out)
(setq org-use-speed-commands t)
(setq org-log-done t)
(setq org-default-notes-file main-org-file)
;; org-mobile
(require 'org-mobile)
(setq org-mobile-inbox-for-pull main-org-file)
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-use-encryption t)
(setq org-ctrl-k-protect-subtree t)
(setq org-support-shift-select t)
(setq org-yank-adjusted-subtrees t)
(setq org-catch-invisible-edits 'smart)

;; Tags
(setq org-tag-alist '((:startgroup . nil)
                      ("@agenda" . ?a)
                      ("@anywhere" . ?w)
                      ("@call" . ?t)
                      ("@internet" . ?i)
                      ("@computer" . ?c)
                      ("@home" . ?h)
                      ("@readreview" . ?r)
                      ("@vilnius" . ?v)
                      ("@waitingfor" . ?f)
                      ("@checklist" . ?l)
                      (:endgroup . nil)
                      ("project" . ?p)
                      ("somedaymaybe" . ?s)
                      ("crypt" . ?k)))
(setq org-use-tag-inheritance '("somedaymaybe" "@readreview"))
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-fast-tag-selection-single-key 'expert)

;; Agendas
(setq org-agenda-custom-commands
      '(("c" "Calls" tags-todo "@call-somedaymaybe/!TODO")
        ("p" "Projects" tags-todo "project-somedaymaybe/!TODO")
        ("l" "Checklists" tags "@checklist-somedaymaybe")
        ("k" "Someday/maybe" tags-todo "somedaymaybe+LEVEL=2/!TODO"
         ((org-agenda-dim-blocked-tasks nil)))
        ("v" "Vilnius" tags-todo "@vilnius-somedaymaybe/!TODO")
        ("n" "Non-project tasks" tags-todo "-project-@waitingfor-somedaymaybe/!TODO"
         ((org-use-tag-inheritance '("project" "somedaymaybe"))))
        (" " "Agenda"
         ((agenda "" nil)
          (tags-todo "@anywhere-somedaymaybe|@call-somedaymaybe|@internet-somedaymaybe|@computer-somedaymaybe/!TODO"
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
          (tags-todo "@readreview-somedaymaybe/!TODO"
                     ((org-agenda-overriding-header "Read/review")
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (todo "LOGTIME"
                ((org-agenda-overriding-header "Time log actions")
                 (org-agenda-dim-blocked-tasks 'invisible)))
          (tags "-project/+DONE|+CANCELLED"
                ((org-agenda-overriding-header "Archivable tasks")
                 (org-use-tag-inheritance '("project"))))
          (todo "-@agenda-@anywhere-@call-@internet-@computer-@home-@readreview-@vilnius-@waitingfor-@checklist-project-somedaymaybe"
                ((org-agenda-overriding-header "Contextless tasks")))))))

(setq org-agenda-start-on-weekday nil)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-todo-ignore-scheduled 'all)
(setq org-agenda-todo-ignore-deadlines 'all)
(setq org-agenda-todo-ignore-timestamp 'all)

(setq org-agenda-clock-consistency-checks
      (list
       :max-duration "6:00"
       :min-duration "0:00"
       :max-gap "0:05"
       :gap-ok-around (list "2:00" "12:30")))
(setq org-agenda-sticky t)
(setq org-agenda-window-setup 'current-window)

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
(setq org-log-into-drawer t)
(setq org-clock-into-drawer t)
(setq org-closed-keep-when-no-todo t)

;; Refiling
(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
;; or 'buffer-name starting with 9.1, not much difference in my setup
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-log-refile 'time)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline main-org-file "Tasks")
         "** TODO %?\n  %i\n  %a")
        ("i" "Inbox" entry (file+headline main-org-file "Inbox")
         "** INBOX: %?\n  %i\n  %a")
        ("c" "Current" plain (clock) "" :clock-in :clock-keep)))
(setq org-todo-keywords
      '((sequence "WAITING(w)" "TODO(t)" "LOGTIME(l)"
                  "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("WAITING" . (:foreground "OrangeRed" :weight bold))
        ("LOGTIME" . (:foreground "OrangeRed" :weight bold))
        ("TODO" . (:foreground "Red" :weight bold))))

(require 'org-habit)
(setq org-habit-graph-column 50)

(setq org-log-redeadline t)
(setq org-log-reschedule t)
(setq org-stuck-projects
      '("+project-somedaymaybe/!TODO" ("TODO") nil ""))
(setq org-todo-repeat-to-state "TODO")
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-cycle-separator-lines 1)
;; TODO: compute these columns from the defaults.el frame size calculations.
(setq org-tags-column -85)
(setq org-agenda-tags-column 'auto)

(require 'org-checklist)

;; org-mode encryption of selected subtrees
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-crypt-disable-auto-save 'encrypt)

(defun org-mode-flyspell-verify-disable-for-org-crypt ()
  "Do not flyspell blocks encrypted by `org-crypt'."
  (not (org-at-encrypted-entry-p)))

(advice-add 'org-mode-flyspell-verify :before-while
            #'org-mode-flyspell-verify-disable-for-org-crypt)

;; org-id
(require 'org-id)
(setq org-id-link-to-org-use-id t)

;; Save org buffers automatically
(add-hook 'auto-save-hook #'org-save-all-org-buffers)

(defun my-org-mode-hook ()
  "My configuration hook for 'org-mode'."
  (enable-show-trailing-ws)
  (turn-on-flyspell)
  (local-set-key (kbd "C-c C-x C-k") #'org-decrypt-entry)
  (setq fill-column 85))

(add-hook 'org-mode-hook #'my-org-mode-hook)

;;; Solarized-dark color theme
(load-theme 'solarized-dark t)

;;; IRC (ERC)
(require 'erc)

(setq erc-user-full-name user-full-name)

(require 'erc-spelling)

(defun my-erc-mode-hook ()
  "My configuration hook for `erc-mode'."
  (erc-spelling-mode)
  (unless emacs-24-4-or-later
    (setq autopair-dont-activate t)))

(with-no-warnings (add-hook 'erc-mode-hook #'my-erc-mode-hook))

(require 'erc-log)

;; (setq erc-log-insert-log-on-open t)

(setq erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(setq erc-join-buffer 'bury)

(require 'erc-networks)
(defun my-erc-generate-log-file-name-channel-network
  (buffer target _nick server _port)
  "Generate an ERC log file name in form of #channel@network.txt.
BUFFER, TARGET, NICK, SERVER, and PORT are ERC-provided."
  (require 'erc-networks)
  (let ((file (concat
               target "!"
               (or (with-current-buffer buffer (erc-network-name)) server)
               ".txt")))
    ;; we need a make-safe-file-name function.
    (convert-standard-filename file)))

(setq erc-generate-log-file-name-function
      #'my-erc-generate-log-file-name-channel-network)

(erc-log-enable)

(require 'erc-track)
(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))

(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))

(defadvice erc-track-find-face
  (around erc-track-find-face-promote-query activate)
  "Pretend that any ERC query buffers contain my name, for better modeline notification."
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

(setq erc-paranoid t)

;; TODO: this is in-buffer highlight, right?
;; (require 'erc-highlight-nicknames)
;; (add-to-list 'erc-modules 'highlight-nicknames)
;; (erc-update-modules)

(require 'erc-services)
(erc-services-mode 1)

(setq erc-prompt-for-nickserv-password nil)

(setq erc-server-coding-system '(utf-8 . utf-8))

(setq erc-server-reconnect-attempts 0)

(require 'erc-join)
(setq erc-autojoin-timing 'ident)

(defun start-chats ()
  "Connect to all chats."
  (interactive)
  (start-erc-chats))

(defun stop-chats ()
  "Disconnect from all chats."
  (interactive)
  (erc-cmd-GQ "Leaving"))

;;; Magit
(global-set-key (kbd "C-x g") #'magit-status)

(setq vc-handled-backends (delq 'Git vc-handled-backends))

(defun turn-off-size-indication-mode ()
  "Turn off function `size-indication-mode' unconditionally."
  (size-indication-mode -1))

(add-hook 'magit-status-mode-hook #'turn-off-size-indication-mode)

;;; Wakatime
(require 'wakatime-mode)
(global-wakatime-mode)

;;; Flycheck
(require 'flycheck)
(setq flycheck-global-modes '(not org-agenda-mode))
(global-flycheck-mode)
(setq flycheck-emacs-lisp-load-path 'inherit)

;;; Company mode
(require 'company)
(add-hook 'after-init-hook #'global-company-mode)
(setq company-global-modes '(not Info-mode help-mode magit-status-mode
                                 org-agenda-mode grep-mode package-menu-mode))

;;; lsp-mode
(require 'lsp-mode)
(require 'lsp-clients)
(setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
(setq lsp-clients-clangd-args '("-background-index"))
(setq lsp-enable-snippet nil)
(setq lsp-prefer-flymake nil)

(require 'lsp-ui)
(setq lsp-ui-sideline-ignore-duplicate t)
(setq lsp-ui-sideline-show-symbol nil)

(define-key lsp-ui-mode-map [remap xref-find-definitions]
  #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references]
  #'lsp-ui-peek-find-references)

(require 'lsp-ui-doc)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-doc-position 'top)

(defconst lsp-clients-clangd-tramp-executable "clangd")
(defun lsp-clients--clangd-tramp-command ()
  "Generate the clangd over Tramp startup command."
  `(,lsp-clients-clangd-tramp-executable ,@lsp-clients-clangd-args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection
                                   'lsp-clients--clangd-tramp-command)
                  :major-modes '(c-mode c++-mode objc-mode)
                  :priority -1
                  :server-id 'clangd-tramp
                  :remote? t))

(add-hook 'prog-mode-hook #'lsp)

;;; SSH config
(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '("/\\.ssh/config\\'"     . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/sshd?_config\\'"      . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/known_hosts\\'"       . ssh-known-hosts-mode))
(add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
(add-hook 'ssh-config-mode-hook #'turn-on-font-lock)
(add-hook 'ssh-config-mode-hook #'enable-show-trailing-ws)

;;; dispwatch
(require 'dispwatch)
(defun my-display-changed-hook (new-display-geometry)
  "Reconfigure frame windows on display geometry change to NEW-DISPLAY-GEOMETRY."
  (message "Resizing for %s" new-display-geometry)
  (cond ((equal new-display-geometry darkstar-laptop-screen)
         (move-to-frame-geometry darkstar-laptop-geometry)
         (set-frame-parameter nil 'fullscreen 'maximized)
         (two-windows))
        ((equal new-display-geometry darkstar-external-screen)
         (move-to-frame-geometry darkstar-external-geometry)
         (set-frame-parameter nil 'fullscreen 'fullboth)
         (six-windows))
        ((equal new-display-geometry darkstar-ignore) ())
        ((equal new-display-geometry darkstar-ignore2) ())
        ((equal new-display-geometry darkstar-ignore3) ())
        ((equal new-display-geometry darkstar-ignore4) ())
        (t (diagnose-unknown-display-geometry new-display-geometry))))

(add-hook 'dispwatch-display-change-hooks #'my-display-changed-hook)
(dispwatch-mode 1)

;;; yaml-mode
(add-to-list 'auto-mode-alist '("/.clang-format\\'" . yaml-mode))

;;; addon-modes.el ends here
