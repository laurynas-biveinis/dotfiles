; Autopair
(unless emacs-24-4-or-later
  (progn
    (require 'autopair)
    (autopair-global-mode)))

; Disable linum where it makes sense and fixes performance
(require 'linum-off)

; Show matching parents
(require 'paren)
(show-paren-mode 1)

; Develock
(cond ((featurep 'xemacs)
       (require 'develock)
       ;; `turn-on-develock' is equivalent to `turn-on-font-lock',
       ;;  except that it does not highlight the startup screen.
       (add-hook 'lisp-interaction-mode-hook 'turn-on-develock)
       (add-hook 'mail-setup-hook 'turn-on-font-lock))
      ((>= emacs-major-version 20)
       (require 'develock)
       (global-font-lock-mode t)))


;; Imenu
(require 'imenu+)
(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-menubar-index) (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;; --------------------------
;; Programming language modes
;; --------------------------

;; Show the arguments of the currently written function in the echo area
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; ----------
;; Shell mode
;; ----------

;; In Shell mode, do not echo passwords
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt
          'comint-strip-ctrl-m)

;; Colors
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ----------------------------
;; ssh mode on the top of shell
;; ----------------------------
(require 'ssh)

;; -------------------------------------------
;; Multiple spell checkers in the same buffers
;; -------------------------------------------
;(require 'ispell-multi)

;; In XML modes
;(autoload 'flyspell-xml-lang-setup "flyspell-xml-lang")
;(add-hook 'xml-mode-hook 'flyspell-xml-lang-setup)
;(add-hook 'nxml-mode-hook 'flyspell-xml-lang-setup)

;; --------------------------
;; AUCTeX and other TeX stuff
;; --------------------------

;; preview-latex image type
(setq preview-image-type 'png)

;; Enable document parsing for AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Make AUCTeX aware of multifile documents
(setq-default TeX-master nil)

;; AUCTeX toolbar support
(add-hook 'LaTeX-mode-hook #'LaTeX-install-toolbar)

;; Source specials
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-source-specials-mode 1)))
(setq TeX-source-specials-view-start-server t)

;; Use RefTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Set up -pdf option for latexmk
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("%(-PDF)"
               (lambda ()
                 (if (and
                      (eq TeX-engine 'default)
                      (or TeX-PDF-mode TeX-DVI-via-PDFTeX))
                     "-pdf" "")))
             TeX-expand-list)))


;; Use latexmk
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("latexmk" "latexmk %(-PDF) %s" TeX-run-TeX nil t
               :help "Run latexmk on file")
             TeX-command-list)))



;; Spellcheck on the fly
;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;(add-hook 'c-mode-hook          'flyspell-prog-mode 1)
;(add-hook 'c++-mode-hook        'flyspell-prog-mode 1)
;(add-hook 'cperl-mode-hook      'flyspell-prog-mode 1)
;(add-hook 'autoconf-mode-hook   'flyspell-prog-mode 1)
;(add-hook 'autotest-mode-hook   'flyspell-prog-mode 1)
;(add-hook 'sh-mode-hook         'flyspell-prog-mode 1)
;(add-hook 'makefile-mode-hook   'flyspell-prog-mode 1)
;(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode 1)

;; Spellcheck on the fly multiple languages at once
;(autoload 'flyspell-babel-setup "flyspell-babel")
;(add-hook 'LaTeX-mode-hook 'flyspell-babel-setup)

;; Integrate RefTeX into AUCTeX
(setq reftex-plug-into-AUCTeX t)

;; Integrate RefTeX with bib-cite
(setq bib-cite-use-reftex-view-crossref t)

;; ----------------------------------------------
;; cmd-mode.el major mode for cmd and bat scripts
;; ----------------------------------------------
(autoload 'cmd-mode "cmd-mode" "CMD mode." t)
(setq auto-mode-alist (append '(("\\.\\(cmd\\|bat\\)$" . cmd-mode))
                              auto-mode-alist))

;; ------------------------------
;; po-mode.el for PO file editing
;; ------------------------------

(setq auto-mode-alist
   (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

(require 'po)
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
   'po-find-file-coding-system)
(autoload 'po-find-file-coding-system "po-mode")

;; ----
;; nXML
;; ----
;; Autocomplete closing tags
(setq nxml-slash-auto-complete-flag t)

;; Automatically use nXML for interesting file types
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))

;; ---
;; CEDET
;; ---
(if (boundp 'cedet-lib)
    (load-file cedet-lib))

(if integrated-cedet-p (semantic-mode 1))

;; Additional global include directories
(semantic-add-system-include "/usr/local/include" 'c-mode)
(semantic-add-system-include "/usr/local/include" 'c++-mode)

(global-ede-mode t)

(if (boundp 'semantic-load-enable-excessive-code-helpers)
    (progn
      (semantic-load-enable-minimum-features)
      (semantic-load-enable-code-helpers)
      (semantic-load-enable-gaudy-code-helpers)
      (semantic-load-enable-excessive-code-helpers)
      ; TODO: should be already enabled by the previous line
      (global-semantic-idle-completions-mode)
      (global-semantic-tag-folding-mode))
  (setq semantic-default-submodes
        '(global-semanticdb-minor-mode
          global-semantic-idle-scheduler-mode
          global-semantic-idle-summary-mode
          global-semantic-idle-completions-mode
          global-semantic-decoration-mode
          global-semantic-highlight-func-mode
          global-semantic-stickyfunc-mode
          global-semantic-mru-bookmark-mode)))

(if (boundp 'semantic-ia) (require 'semantic-ia))
(if (boundp 'semantic-gcc) (require 'semantic-gcc))


; Loosely inspired by JetBrains IntelliJ IDEA
(defun my-c-mode-cedet-hook ()
  (local-set-key [(control tab)] 'semantic-ia-complete-symbol)
  (local-set-key (kbd "C-?") 'semantic-ia-complete-symbol-menu)
; TODO: try CVS post 1.0pre7
;  (local-set-key "." 'semantic-complete-self-insert) ; These suck seriously FIXME
;  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key (kbd "C-c q") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-c <f1>") 'semantic-ia-show-doc)
  ; TODO gather all jumps to C-c b
  (local-set-key (kbd "C-c b") 'semantic-complete-jump)
  (local-set-key (kbd "C-c C-b") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c C-i") 'semantic-decoration-include-visit)
  (local-set-key (kbd "M-<f7>") 'semantic-symref)
)
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; --------
;; PHP mode
;; --------
(autoload 'php-mode "php-mode" "PHP mode" t)
(setq auto-mode-alist
      (append
       '(("\\.php$" . php-mode)
         ("\\.php5$" . php-mode))
       auto-mode-alist))

; Xrefactory configuration part ;;
(defvar xref-current-project nil) ;; can be also "my_project_name"
(defvar xref-key-binding 'global) ;; can be also 'local or 'none
(if use-xref
      (progn
        (setq exec-path (cons xref-dir exec-path))
        (load "xrefactory")))

; JDEE
; TODO: broken with Emacs 23.2 integrated CEDET
;(require 'jde)
;(setq jde-jdk-registry
;      (quote (("1.6.0.16" . "/usr/lib/jvm/java-6-sun-1.6.0.16"))))


; --------
; org-mode
; --------
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
; Bendra
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cs" 'org-iswitchb)
(define-key global-map "\C-cc" 'org-capture)
(setq org-use-speed-commands t)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/percona.org"
                             "~/org/phd.org"
                             "~/org/gtd.org"
                             "~/org/music.org"))
(setq org-default-notes-file "~/org/gtd.org")
(setq org-mobile-inbox-for-pull "~/org/gtd.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-use-encryption t)
(setq org-mobile-encryption-password "2878")
(setq org-ctrl-k-protect-subtree t)
(setq org-support-shift-select t)
(setq org-yank-adjusted-subtrees t)

; Tags
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

; Agendas
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
(setq org-agenda-repeating-timestamp-show-all nil)
(setq org-agenda-clock-consistency-checks
      (list
       :max-duration "6:00"
       :min-duration "0:00"
       :max-gap "0:05"
       :gap-ok-around (list "2:00" "12:30")))
(setq org-agenda-sticky t)

; Scheduling and deadlines
(setq org-deadline-warning-days 30)

; Drawers

; Clock tables
(setq org-clocktable-defaults
      (list
       :maxlevel 99
       :scope 'agenda-with-archives
       :stepskip0 t
       :fileskip0 t
       :narrow 50
       :link t
       :indent t
       :tcolumns 0))

; Logging
(setq org-log-into-drawer t)
(setq org-clock-into-drawer t)
(setq org-closed-keep-when-no-todo t)

; Refiling
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-log-refile 'time)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline "~/org/gtd.org" "Tasks")
         "** TODO %?\n  %i\n  %a")
        ("i" "Inbox" entry (file+headline "~/org/gtd.org" "Inbox")
         "** INBOX: %?\n  %i\n  %a" :killbuffer)
        ("c" "Current" plain (clock) "" :clock-in :clock-keep)))
(setq org-todo-keywords
      '((sequence "WAITING(w)" "TODO(t)" "LOGTIME(l)"
                  "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("WAITING" . (:foreground "OrangeRed" :weight bold))
        ("LOGTIME" . (:foreground "OrangeRed" :weight bold))
        ("TODO" . (:foreground "Red" :weight bold))))

(setq org-modules
      '(org-habit org-bbdb org-bibtex org-docview org-gnus org-info
        org-irc org-mew org-mhe org-rmail org-vm org-w3m org-wl)
)
(setq org-log-redeadline t)
(setq org-log-reschedule t)
(setq org-stuck-projects
      '("+project-somedaymaybe/!TODO" ("TODO") nil ""))
(setq org-todo-repeat-to-state "TODO")
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-cycle-separator-lines 1)
; TODO: compute these columns from the defaults.el frame size calculations.
(setq org-tags-column -90)
(setq org-agenda-tags-column -90)
(setq org-habit-graph-column 50)

; And load everything except crypt
(require 'org-install)
(require 'org-checklist)

; org-mode encryption of selected subtrees
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-crypt-key "B8D47CD8")
(setq org-crypt-disable-auto-save 'encrypt)

; DVC
(require 'dvc-autoloads)

; Solarized-dark color theme
(setq solarized-termcolors 256)
(if emacs-24-or-later
    (progn
      (add-to-list 'custom-theme-load-path solarized-theme-dir)
      (load-theme 'solarized-dark t))
  (progn
    (require 'color-theme)
    (require 'color-theme-solarized)
    (eval-after-load "color-theme"
      '(progn
         (color-theme-initialize)
         (color-theme-solarized-dark)))))

; todochiku
(require 'cl)
(require 'todochiku)

(setq todochiku-icons-directory todochiku-icons-dir)

; IRC (ERC)
(require 'erc)

(setq erc-user-full-name "Laurynas Biveinis")

; Disable autopair
(add-hook 'erc-mode-hook
          (lambda ()
              (setq autopair-dont-activate t)))

(require 'erc-log)

(setq erc-log-channels-directory erc-log-dir)

; (setq erc-log-insert-log-on-open t)

(setq erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(defun my-erc-generate-log-file-name-channel-network
  (buffer target nick server port)
  "Generates an ERC log fle name using the channel and network name, resulting
in #channl@network.txt.
Ths function is a possible values for `erc-generate-log-file-name-function'."
  (require 'erc-networks)
  (let ((file (concat
               target "!"
               (or (with-current-buffer buffer (erc-network-name)) server)
               ".txt")))
    ;; we need a make-safe-file-name function.
    (convert-standard-filename file)))

(setq erc-generate-log-file-name-function
      'my-erc-generate-log-file-name-channel-network)

(erc-log-enable)

; Disabled until ERC supports SASL
; (setq erc-keywords '("team" "laurynas"))
(setq erc-keywords '("laurynas"))

(setq erc-current-nick-highlight-type 'all)
(setq erc-keyword-highlight-type 'all)

(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))

(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))

(defadvice erc-track-find-face
  (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

(setq erc-paranoid t)

; TODO: this is in-buffer highlight, right?
; (require 'erc-highlight-nicknames)
; (add-to-list 'erc-modules 'highlight-nicknames)
; (erc-update-modules)
(add-hook 'erc-text-matched-hook
          (lambda (match-type nick message)
            (unless (or
                     (posix-string-match "^\\** *Users on #" message)
                     (posix-string-match "^\\** *Your new nickname is "
                                         message))
              (todochiku-message (format "IRC: %s says" nick)
                                 message
                                 (todochiku-icon 'irc)))))


(setq erc-autojoin-channels-alist
      '(("MPB" "#mysqldev" "#percona")
        ("freenode.net" "#percona" "#maria" "#mysql" "#mysql-dev"
         "#webscalesql")))

(require 'erc-services)
(erc-services-mode 1)

(setq erc-prompt-for-nickserv-password nil)

(setq erc-nickserv-passwords
      `((freenode (("laurynas" . ,freenode-nickserv-password)))))

(setq erc-server-coding-system '(utf-8 . utf-8))

(setq erc-server-reconnect-attempts t)

; TODO
;(require 'erc-spelling)
;(add-hook 'erc-mode-hook
          ;(lambda()
            ;(erc-spelling-mode)))


; (setq erc-modules 'autojoin 'log 'readonly 'services 'stamp)
; (erc-update-modules)


; Jabber (Google Talk)
(require 'jabber-autoloads)

(setq jabber-account-list
      `(("laurynas.biveinis@gmail.com/emacs"
         (:network-server . "talk.google.com")
         (:connection-type . ssl)
         (:password . ,g-talk-password))))

(add-hook 'jabber-post-connect-hooks 'jabber-autoaway-start)

; Disable autopair
(add-hook 'jabber-chat-mode-hook
          (lambda ()
            (setq autopair-dont-activate t)))

(setq jabber-auto-reconnect t)

(setq jabber-activity-count-in-title t)

(setq jabber-history-enabled t)
(setq jabber-use-global-history nil)
(setq jabber-backlog-number 50)
(setq jabber-backlog-days 14)

(setq jabber-mode-line-mode t)

(add-hook 'jabber-alert-message-hooks 'libnotify)

(defun start-chats ()
  "Connect to all chats"
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "laurynas")
  (jabber-connect-all))

(defun stop-chats ()
  "Disconnect from all chats"
  (interactive)
  (jabber-disconnect)
  (erc-cmd-GQ "Leaving"))
