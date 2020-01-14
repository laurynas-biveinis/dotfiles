;;; setup.el --- Main Emacs configuration.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)

;; Variables and functions defined elsewhere we'll be using
(defvar bookmark-save-flag)
(defvar font-use-system-font)
(defvar wdired-allow-to-change-permissions)
(defvar TeX-auto-save)
(defvar TeX-parse-self)
(defvar TeX-expand-list)
(defvar TeX-command-list)
(defvar TeX-source-correlate-start-server)
(defvar reftex-plug-into-AUCTeX)
(defvar bib-cite-use-reftex-view-crossref)
(defvar main-org-file)
(declare-function LaTeX-install-toolbar "tex-bar" ())
(declare-function TeX-source-correlate-mode "tex" (&optional arg))
(declare-function start-erc-chats "" ())

;;; General settings
;; Keep all messages
(setq message-log-max t)

;; C-k kills line including its newline
(setq kill-whole-line t)

;; Do not store duplicate kills
(setq kill-do-not-save-duplicates t)

;; Bookmarks are saved automatically
(setq bookmark-save-flag 1)

;; Raw bytes in hexadecimal not octal
(setq display-raw-bytes-as-hex t)

;; Enter quoted chars in hex
(setq read-quoted-char-radix 16)

;; Trailing newlines are highlighted
(setq-default indicate-empty-lines t)

;; Should files end with newline?
(setq-default require-final-newline 'query)

;; No annoying beeps
(setq visible-bell t)

;; Indentation can only insert spaces by default
(setq-default indent-tabs-mode nil)

;; If already indented, complete
(setq tab-always-indent 'complete)

;; Diff options
(setq diff-switches "-u -p")

;; Preserve hard links to the file you are editing
;; From http://www.emacswiki.org/cgi-bin/wiki/DotEmacsChallenge
(setq backup-by-copying-when-linked t)

;; Preserve the owner and group of the file you are editing
;; From http://www.emacswiki.org/cgi-bin/wiki/DotEmacsChallenge
(setq backup-by-copying-when-mismatch t)

;; Do not backup
(setq make-backup-files nil)

;; But if we did backup, do not break macOS file metadata
(setq backup-by-copying t)

;; Use Unix-style line endings.
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Enable visual feedback on selections
(setq transient-mark-mode t)

;; No startup message
(setq inhibit-startup-message t)

(setq kill-read-only-ok t)

(setq scroll-error-top-bottom t)

(setq fast-but-imprecise-scrolling t)

(setq recenter-redisplay t)

(setq-default indicate-buffer-boundaries t)

(setq blink-cursor-blinks -1)

(setq x-stretch-cursor t)

(setq delete-by-moving-to-trash t)

(setq use-dialog-box nil)

(setq load-prefer-newer t)

;;; Hook helpers
;; Display trailing whitespace
(defun dotfiles--enable-show-trailing-ws ()
  "Enable showing of trailing whitespace."
  (setq show-trailing-whitespace t))

;;;; Bundled modes
;;; header-line-format
;; which-function-mode
(which-function-mode)

(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))

(setq mode-line-misc-info (assq-delete-all 'which-function-mode
                                           mode-line-misc-info))

;;; modeline

;; size-indication-mode
(size-indication-mode)

;; Show column number
(column-number-mode t)

;;; fill-column and other filling-related matters
(setq-default fill-column 80)

(setq sentence-end-double-space nil)

;;; whitespace-mode
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style '(face trailing lines-tail empty indentation big-intent
                              space-after-tab space-before-tab))
;; Use fill-column value
(setq whitespace-line-column nil)
(setq whitespace-global-modes '(not dired-mode erc-mode markdown-mode gfm-mode
                                    lisp-interaction-mode help-mode Info-mode
                                    magit-status-mode org-agenda-mode grep-mode
                                    package-menu-mode vterm-mode))

;; XXI century encodings
(set-language-environment "UTF-8")

;;; Window and frame geometry
(defun two-windows ()
  "Make frame contain two vertical windows."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun six-windows ()
  "Make frame contain 2x3 windows."
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (split-window-right)
  (windmove-down)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(cl-defstruct dotfiles--frame-geometry top left height width)

(defun dotfiles--add-frame-geometry-to-initial-alist (geometry)
  "Add frame GEOMETRY to `initial-frame-alist'."
  (add-to-list 'initial-frame-alist
               `(top . ,(dotfiles--frame-geometry-top geometry)))
  (add-to-list 'initial-frame-alist
               `(left . ,(dotfiles--frame-geometry-left geometry)))
  (add-to-list 'initial-frame-alist
               `(height . ,(dotfiles--frame-geometry-height geometry)))
  (add-to-list 'initial-frame-alist
               `(width . ,(dotfiles--frame-geometry-width geometry))))

(defun dotfiles--move-to-frame-geometry (geometry)
  "Resize and repositon frame to GEOMETRY."
  (set-frame-position nil (dotfiles--frame-geometry-left
                           geometry)
                      (dotfiles--frame-geometry-top geometry))
  (set-frame-size nil (dotfiles--frame-geometry-width geometry)
                  (dotfiles--frame-geometry-height geometry)))

(defconst darkstar-laptop-screen '(1680 . 1050))
(defconst darkstar-laptop-geometry
  (make-dotfiles--frame-geometry :top 1 :left 1 :height 65 :width 237))

(defconst darkstar-external-screen '(7696 . 1692))
(defconst darkstar-external-geometry
  (make-dotfiles--frame-geometry :top 4 :left 3011 :height 117 :width 426))

;; Possible interim states while docking/undocking - ignore
(defconst frame-geometries-to-ignore [(3600 . 1080) (5520 . 1080) (4688 . 1692)
                                      (3600 . 1692) (3008 . 1692)])

(defun diagnose-unknown-display-geometry (display-geometry)
  "Diagnose unknown DISPLAY-GEOMETRY."
  (message "Unknown display size %sx%s"
           (car display-geometry) (cdr display-geometry)))

(require 'seq)
(let ((display-geometry (cons (display-pixel-width) (display-pixel-height))))
  (cond ((equal display-geometry darkstar-laptop-screen)
         ;; darkstar without external screens: initial frame positioned in the
         ;; top left corner
         (dotfiles--add-frame-geometry-to-initial-alist
          darkstar-laptop-geometry)
         (two-windows))
        ((equal display-geometry darkstar-external-screen)
         ;; darkstar with external screens: initial frame maximized in the
         ;; middle screen
         (dotfiles--add-frame-geometry-to-initial-alist
          darkstar-external-geometry)
         (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
         (add-to-list 'initial-frame-alist '(fullscreen-restore . maximized))
         (six-windows))
        ((seq-position frame-geometries-to-ignore display-geometry) ())
        (t (diagnose-unknown-display-geometry display-geometry))))

;; Use system font if under Gnome, otherwise use a specified font if one was
;; specified
(cond ((symbolp 'font-use-system-font)
       (setq font-use-system-font t))
      ((symbolp 'my-frame-font)
       (add-to-list 'default-frame-alist `(font . ,my-frame-font))
       (add-to-list 'initial-frame-alist `(font . ,my-frame-font))))

;;; files, directories, and similar things
(defun dotfiles--treat-new-files-as-modified ()
  "Treat new (empty) files as modified."
  (unless (file-exists-p (buffer-file-name))
    (set-buffer-modified-p t)))

(add-hook 'find-file-hook #'dotfiles--treat-new-files-as-modified)

(defun dotfiles--follow-cygwin-symlinks ()
  "Follow Cygwin symlinks.
Handles old-style (text file) and new-style (.lnk file) symlinks.
\(Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still
loaded as such.)"
  (save-excursion
    (goto-char 0)
    (if (looking-at
         "L\x000\x000\x000\x001\x014\x002\x000\x000\x000\x000\x000\x0C0\x000\x000\x000\x000\x000\x000\x046\x00C")
        (progn
          (re-search-forward
           "\x000\\([-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`][-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`]+\\)")
          (find-alternate-file (match-string 1)))
      (when (looking-at "!<symlink>")
        (re-search-forward "!<symlink>\\(.*\\)\0")
        (find-alternate-file (match-string 1))))))

(add-hook 'find-file-hook #'dotfiles--follow-cygwin-symlinks)

;; Mark executable files as executable on save
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(setq enable-remote-dir-locals t)

;;; Keybindings
(global-set-key (kbd "<home>") #'move-beginning-of-line)
(global-set-key (kbd "<end>") #'move-end-of-line)

(global-set-key "\C-cg" #'goto-line)
(global-set-key "\C-cn" #'next-error)
(global-set-key "\C-cp" #'previous-error)
(global-set-key "\C-x\C-m" #'execute-extended-command)
(global-set-key "\C-c\C-m" #'execute-extended-command)

(global-set-key [(control shift up)] #'enlarge-window)
(global-set-key [(control shift down)] #'shrink-window)
(global-set-key [(control shift left)] #'enlarge-window-horizontally)
(global-set-key [(control shift right)] #'shrink-window-horizontally)

(defun end-of-line-and-newline-and-indent ()
  "Go to the end of line, insert a new line, and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<M-RET>") #'end-of-line-and-newline-and-indent)

;; Yank should indent in programming modes
(defun indent-if-prog-mode ()
  "Indent current region if in programming mode and no prefix arg."
  (interactive)
  (if (and (not current-prefix-arg) (derived-mode-p 'prog-mode))
      (indent-region (region-beginning) (region-end) nil)))

(advice-add #'yank :after #'indent-if-prog-mode)
(advice-add #'yank-pop :after #'indent-if-prog-mode)

;; Enable some disabled commands
(put 'narrow-to-region 'disabled nil)

;; No scollbars
(set-scroll-bar-mode nil)

(setq search-nonincremental-instead nil)

(setq query-replace-skip-read-only t)

;; No toolbar
(tool-bar-mode -1)

;; Typing or <Delete> will remove selected text
(delete-selection-mode 1)

;; Mouse avoidance.
(setq make-pointer-invisible t)

;; Recent files menu
(recentf-mode)

(global-font-lock-mode 1)

;;; minibuffer
;; icomplete
(icomplete-mode)

;; Better C-x b menu by IDO mode. Alternatives: helm, ivy
(require 'ido)
(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-default-buffer-method 'selected-window)
(setq ido-use-filename-at-point 'guess)

(defun dotfiles--kill-buffer-if-exists (buffer)
  "Kill the BUFFER if it exists."
  (if (buffer-live-p buffer)
      (kill-buffer buffer)))

(defun dotfiles--kill-completion-buffers ()
  "Kill completion buffers, if they exist."
  (dotfiles--kill-buffer-if-exists "*Completions*")
  (dotfiles--kill-buffer-if-exists "*Ido Completions*"))

(add-hook 'minibuffer-exit-hook #'dotfiles--kill-completion-buffers)

(require 'minibuf-eldef)
(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode)

;;; autorevert
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)

;;; common programming modes
(add-hook 'prog-mode-hook #'turn-on-auto-fill)
(add-hook 'prog-mode-hook #'dotfiles--enable-show-trailing-ws)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'electric-layout-mode)
(add-hook 'prog-mode-hook #'goto-address-prog-mode)

;;; text-mode
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook #'dotfiles--enable-show-trailing-ws)
(add-hook 'text-mode-hook #'turn-on-flyspell)
(add-hook 'text-mode-hook #'goto-address-mode)

;; Soft word wrap
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

(global-visual-line-mode 1)

;; Nice unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Automatically show images as images
(auto-image-file-mode 1)

;;; dired
;; Copy recursively
(require 'dired)
(setq dired-recursive-copies 'always)

(defun dotfiles--load-dired-x ()
  "Load dired-x."
  (load "dired-x"))

(add-hook 'dired-load-hook #'dotfiles--load-dired-x)

(add-hook 'Man-mode-hook #'goto-address)

(put 'dired-find-alternate-file 'disabled nil)

(setq wdired-allow-to-change-permissions t)

;; From http://atomized.org/2008/12/emacs-create-directory-before-saving/
(defun dotfiles--create-missing-parent-dirs ()
  "Create parent directories automatically, if missing."
  (or (file-exists-p (file-name-directory buffer-file-name))
      (make-directory (file-name-directory buffer-file-name) t)))

(add-hook 'before-save-hook #'dotfiles--create-missing-parent-dirs)

;;; cc-mode
(require 'cc-mode)
(require 'cc-vars)

;; TAB indents only if point in the beginning of the line
(setq c-tab-always-indent 1)

;; Typing # moves it to the first column
(setq c-electric-pound-behavior '(alignleft))

(setq c-doc-comment-style
      '((c-mode . javadoc)
        (c++-mode . javadoc)))

(defun dotfiles--c-mode-common-hook ()
  "My customization of cc-mode init."
  (define-key c-mode-base-map (kbd "<RET>") #'c-context-line-break)
  (define-key c-mode-base-map (kbd "C-o") #'c-context-open-line)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c-initialization-hook #'dotfiles--c-mode-common-hook)

;; Grand Unified Debugger
(gud-tooltip-mode t)

;; windmove
(windmove-default-keybindings 'super)

;; Compilation
(require 'compile)
(setq compilation-scroll-output 'first-error)
(setq compilation-environment '("LANG=C" "TERM=xterm-256color"))

(require 'xterm-color)
(defun dotfiles--compilation-filter-advice (f proc string)
  "Compilation filter for xterm-256color, taking F, PROC, & STRING."
  (funcall f proc (xterm-color-filter string)))

(advice-add #'compilation-filter :around #'dotfiles--compilation-filter-advice)

;;; eldoc-mode: show the args of the current function in the echo area
(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)

;;; elisp-mode
(defun dotfiles--emacs-lisp-mode-hook ()
  "Configuration for 'emacs-lisp-mode'."
  (eldoc-mode)
  ;; Should the global default ever change, elisp should stay with spaces
  (setq indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook #'dotfiles--emacs-lisp-mode-hook)

;;; gpg
(require 'epa)
(setq epa-pinentry-mode 'loopback)

;; TODO(laurynas): report this bug upstream
(defun dotfiles--set-epg-context-pinentry-mode (context _cipher)
  "Fix epg CONTEXT to the correct pinentry mode."
  (setf (epg-context-pinentry-mode context) epa-pinentry-mode))

(advice-add #'epg-decrypt-string :before
            #'dotfiles--set-epg-context-pinentry-mode)

;;; ispell
(require 'ispell)
(setq ispell-program-name "hunspell")
(setq ispell-really-hunspell t)
(setq ispell-dictionary "en_US,lt")
(ispell-set-spellchecker-params)
(ispell-hunspell-add-multi-dic "en_US,lt")
(add-to-list 'ispell-skip-region-alist
             '("^-----BEGIN PGP MESSAGE-----$" . "^-----END PGP MESSAGE-----$"))
;; Workaround header line covering the ispell choices window
(setq ispell-choices-win-default-height 3)

;;; Show matching parents
(require 'paren)
(setq show-paren-style 'mixed)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(show-paren-mode 1)

;;; electric-pair-mode
(require 'elec-pair)
(electric-pair-mode)

;; In Shell mode, do not echo passwords
(require 'comint)
(add-hook 'comint-output-filter-functions
          #'comint-watch-for-password-prompt
          #'comint-strip-ctrl-m)

(setq comint-scroll-to-bottom-on-input 'all)
(setq comint-scroll-to-bottom-on-output 'other)
(setq comint-prompt-read-only t)
(setq comint-input-ignoredups t)

;; Colors. Should I ever use eshell, apply its xterm-color integration too.
(require 'ansi-color)
(setq comint-terminfo-terminal "xterm-256color")
(setq comint-output-filter-functions
      (remove #'ansi-color-process-output comint-output-filter-functions))

(require 'font-core)
(defun dotfiles--shell-mode-hook ()
  "My hook for shell-mode."
  (font-lock-mode -1)
  (make-local-variable 'font-lock-function)
  (setq font-lock-function (lambda (_) nil))
  (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter nil t))

(add-hook 'shell-mode-hook #'dotfiles--shell-mode-hook)

;;; Tramp
(require 'tramp)
;; The default level 3 is too noisy with showing each file shipped
(setq tramp-verbose 2)
;; Not sure I care about scp overhead in 2020. Also, maybe this will help with
;; duplicated sshx/scpx paths in lsp-mode cache.
(setq tramp-copy-size-limit nil)
(setq tramp-default-method "scpx")
(setq tramp-completion-reread-directory-timeout t)

;;; calendar
(require 'calendar)
(require 'solar)
(setq calendar-week-start-day 1)
(setq calendar-date-style 'iso)
;; Vilnius!
(setq calendar-latitude 54.7)
(setq calendar-longitude 25.3)
(setq calendar-location-name "Vilnius, Lithuania")

;;; display-line-numbers
(require 'display-line-numbers)
(setq display-line-numbers-grow-only t)

;;; calculator
(require 'calculator)
(setq calculator-electric-mode t)

;;; ediff
(require 'ediff-wind)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-merge-split-window-function #'split-window-horizontally)
(setq ediff-quit-widened nil)

(defvar dotfiles--pre-ediff-window-config nil)

(defun dotfiles--save-pre-ediff-window-config ()
  "Save the current window configuration, to be used as an ediff pre-setup hook."
  (setq dotfiles--pre-ediff-window-config (current-window-configuration)))

(defun dotfiles--restore-pre-ediff-window-config ()
  "Restore the current window configuration, to be used as an ediff exit hook."
  (set-window-configuration dotfiles--pre-ediff-window-config))

(add-hook 'ediff-before-setup-hook #'dotfiles--save-pre-ediff-window-config)
(add-hook 'ediff-quit-hook #'dotfiles--restore-pre-ediff-window-config)

;;; printing
(setq ps-print-color-p 'black-white)

;;; Change appearance for screen sharing
(defconst screen-sharing-default-height
  (face-attribute 'default :height)
  "Default frame font height, when screen sharing is off.")

(defconst screen-sharing-larger-height
  (+ screen-sharing-default-height 20)
  "Larger frame font height, when screen sharing is on.")

(defun start-screen-sharing ()
  "Change Emacs appearance for screen sharing."
  (interactive)
  (set-face-attribute 'default nil :height screen-sharing-larger-height)
  (balance-windows)
  (global-display-line-numbers-mode))

(defun stop-screen-sharing ()
  "Restore Emacs appearance after screen sharing."
  (interactive)
  (set-face-attribute 'default nil :height screen-sharing-default-height)
  (balance-windows)
  (global-display-line-numbers-mode nil))

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
(require 'google-c-style)
(c-add-style "google" google-c-style)

;; Imenu
(require 'imenu+)
(defun dotfiles--try-to-add-imenu ()
  "Try to add imenu to menu bar, silently eating any errors."
  (condition-case nil (imenu-add-menubar-index) (error nil)))
(add-hook 'font-lock-mode-hook #'dotfiles--try-to-add-imenu)

(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout 6000000)

;; ssh mode on the top of shell
(require 'ssh)

;;;; AUCTeX and other TeX stuff

;; Enable document parsing for AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Make AUCTeX aware of multifile documents
(setq-default TeX-master nil)

(defun dotfiles--latex-mode-hook ()
  "My configuration hook for 'latex-mode'."
  (LaTeX-install-toolbar)
  (turn-on-reftex)
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

(add-hook 'LaTeX-mode-hook #'dotfiles--latex-mode-hook)
(setq TeX-source-correlate-start-server t)

;; Integrate RefTeX into AUCTeX
(setq reftex-plug-into-AUCTeX t)

;; Integrate RefTeX with bib-cite
(setq bib-cite-use-reftex-view-crossref t)

;;; nXML
(require 'nxml-mode)
;; Autocomplete closing tags
(setq nxml-slash-auto-complete-flag t)

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
(define-key global-map "\C-c\C-x\C-j" #'org-clock-goto)
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
        ("A" "Agenda"
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

(setq org-clock-display-default-range 'untilnow)
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
;; TODO: compute these columns from the frame size calculations above.
(setq org-tags-column -85)
(setq org-agenda-tags-column 'auto)

;; Comes from org-contrib
(require 'org-checklist)

;; org-mode encryption of selected subtrees
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-crypt-disable-auto-save 'encrypt)

(defun dotfiles--org-mode-flyspell-verify-disable-for-org-crypt ()
  "Do not flyspell blocks encrypted by `org-crypt'."
  (not (org-at-encrypted-entry-p)))

(advice-add 'org-mode-flyspell-verify :before-while
            #'dotfiles--org-mode-flyspell-verify-disable-for-org-crypt)

;; org-id
(require 'org-id)
(setq org-id-link-to-org-use-id t)

;; Save org buffers automatically
(add-hook 'auto-save-hook #'org-save-all-org-buffers)

(defun dotfiles--org-mode-hook ()
  "My configuration hook for 'org-mode'."
  (local-set-key (kbd "C-c C-x C-k") #'org-decrypt-entry)
  (setq fill-column 85))

(add-hook 'org-mode-hook #'dotfiles--org-mode-hook)

;;; Solarized-dark color theme
(load-theme 'solarized-dark t)

;;; IRC (ERC)
(require 'erc)

(setq erc-user-full-name user-full-name)

(require 'erc-spelling)
(add-hook 'erc-mode-hook #'erc-spelling-mode)

(require 'erc-log)

;; (setq erc-log-insert-log-on-open t)

(setq erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(setq erc-join-buffer 'bury)

(require 'erc-networks)
(defun dotfiles--erc-generate-log-file-name-channel-network
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
      #'dotfiles--erc-generate-log-file-name-channel-network)

(erc-log-enable)

(require 'erc-track)
(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))

(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))

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

(defun dotfiles--turn-off-size-indication-mode ()
  "Turn off function `size-indication-mode' unconditionally."
  (size-indication-mode -1))

(add-hook 'magit-status-mode-hook #'dotfiles--turn-off-size-indication-mode)

;;; git-gutter-fringe
(require 'git-gutter-fringe)
(global-git-gutter-mode +1)

;;; Wakatime
(require 'wakatime-mode)
(global-wakatime-mode)

;;; Flycheck. 26.1+ flymake works too.
(require 'flycheck)
(setq flycheck-global-modes '(not org-agenda-mode vterm-mode))
(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc c/c++-cppcheck))

(define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
(define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)

(global-flycheck-mode)
(setq flycheck-emacs-lisp-load-path 'inherit)

;;; Company mode
(require 'company)
(add-hook 'after-init-hook #'global-company-mode)
(setq company-global-modes '(not Info-mode help-mode magit-status-mode
                                 org-agenda-mode grep-mode package-menu-mode
                                 vterm-mode))

;;; lsp-mode
(require 'lsp-mode)
(require 'lsp-clients)
(setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
(setq lsp-enable-snippet nil)
(setq lsp-eldoc-render-all t)
(setq lsp-before-save-edits nil)
(setq lsp-restart 'auto-restart)
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

;; Breaks idempotence of this file, which I am not using anyway.
(if (fboundp #'lsp-format-defun)
    (display-warning
     'dotfiles
     "‘lsp-format-defun’ defined by ‘lsp-mode’: fix it in setup.el!"
     :warning))

(defvar-local dotfiles--use-lsp-indent nil
  "If t, use LSP instead of cc-mode for indentation in this buffer.")

(defun lsp-format-defun ()
  "Format the current cc-mode defun using LSP."
  (interactive)
  (save-mark-and-excursion
    (c-mark-function)
    (lsp-format-region (region-beginning) (region-end))))

(defun dotfiles--lsp-format-defun-advice (orig-fun)
  "Format the defun using LSP with a fallback to ORIG-FUN (‘c-indent-defun’)."
  (if dotfiles--use-lsp-indent (lsp-format-defun)
    (funcall orig-fun)))

(defun dotfiles--lsp-format-region-advice (orig-fun &rest args)
  "Format the region (ARGS) using LSP with a fallback to ORIG-FUN (‘c-indent-region’)."
  (if dotfiles--use-lsp-indent (apply #'lsp-format-region args)
    (apply orig-fun args)))

(defun dotfiles--lsp-replace-cc-mode-indent ()
  "Make ‘c-indent-defun’ and ‘c-indent-region’ use LSP."
  (when (and lsp-enable-indentation
             ;; Using internal LSP symbols is not ideal but I don't see an
             ;; alternative.
             (or (lsp--capability "documentRangeFormattingProvider")
                 (lsp--registered-capability "textDocument/rangeFormatting")))
    (setq-local dotfiles--use-lsp-indent t)
    (advice-add #'c-indent-defun :around #'dotfiles--lsp-format-defun-advice)
    (advice-add #'c-indent-region :around #'dotfiles--lsp-format-region-advice)))

(defun dotfiles--lsp-restore-cc-mode-indent (_lsp_workspace)
  "Make ‘c-indent-defun’ and ‘c-indent-region’ no longer use LSP."
  (setq-local dotfiles--use-lsp-indent nil))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-replace-cc-mode-indent)
(add-hook 'lsp-after-uninitialized-hook #'dotfiles--lsp-restore-cc-mode-indent)

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

(add-hook 'prog-mode-hook #'lsp-deferred)

;; Integrate lsp-mode with projectile
(setq lsp-auto-guess-root t)

;;; SSH config mode
(add-hook 'ssh-config-mode-hook #'turn-on-font-lock)
(add-hook 'ssh-config-mode-hook #'dotfiles--enable-show-trailing-ws)
(add-hook 'ssh-config-mode-hook #'turn-on-auto-fill)

;;; dispwatch
(require 'dispwatch)
(defun dotfiles--display-changed-hook (new-display-geometry)
  "Reconfigure frame windows on display geometry change to NEW-DISPLAY-GEOMETRY."
  (message "Resizing for %s" new-display-geometry)
  (cond ((equal new-display-geometry darkstar-laptop-screen)
         (dotfiles--move-to-frame-geometry darkstar-laptop-geometry)
         (set-frame-parameter nil 'fullscreen 'maximized)
         (two-windows))
        ((equal new-display-geometry darkstar-external-screen)
         (dotfiles--move-to-frame-geometry darkstar-external-geometry)
         (set-frame-parameter nil 'fullscreen 'fullboth)
         (six-windows))
        ((seq-position frame-geometries-to-ignore new-display-geometry) ())
        (t (diagnose-unknown-display-geometry new-display-geometry))))

(add-hook 'dispwatch-display-change-hooks #'dotfiles--display-changed-hook)
(dispwatch-mode 1)

;;; yaml-mode
(add-to-list 'auto-mode-alist '("/.clang-format\\'" . yaml-mode))

;;; aggressive-indent-mode
(require 'aggressive-indent)
(setq aggressive-indent-comments-too t)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes #'help-mode)
(add-to-list 'aggressive-indent-excluded-modes #'Info-mode)
(add-to-list 'aggressive-indent-excluded-modes #'magit-status-mode)
(add-to-list 'aggressive-indent-excluded-modes #'org-agenda-mode)
(add-to-list 'aggressive-indent-excluded-modes #'erc-mode)
(add-to-list 'aggressive-indent-excluded-modes #'grep-mode)
;; Incompatible (corrupts buffer) and redundant anyway with LSP server-provided
;; formatting
(add-to-list 'aggressive-indent-excluded-modes #'c-mode)
(add-to-list 'aggressive-indent-excluded-modes #'c++-mode)
(require 'term)
(add-to-list 'aggressive-indent-excluded-modes #'term-mode)
(add-to-list 'aggressive-indent-excluded-modes #'package-menu-mode)

;;; rich-minority-mode
(require 'rich-minority)
(setq rm-blacklist '(" company" " waka" " Undo-Tree" " =>" " GitGutter" " WS"
                     " ElDoc" " Wrap" " Fill" " all-the-icons-dired-mode"
                     " Projectile"))
(rich-minority-mode)

;;; projectile
(require 'projectile)
;; Steal s-p from ns-print-buffer. I never print buffers
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Save mode line space
(setq projectile-mode-line-prefix " ")
;; Exclude some more modes from projectile
(add-to-list 'projectile-globally-ignored-modes "lisp-interaction-mode")
(add-to-list 'projectile-globally-ignored-modes "org-agenda-mode")
(add-to-list 'projectile-globally-ignored-modes "package-menu-mode")
(projectile-mode +1)

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

;; Workaround https://github.com/bbatsov/projectile/issues/1282 (project.el and
;; projectile are not integrated) some more, so that xref uses projectile
;; project roots.

;; I don't know how to advice generic method default body nor how to write
;; cl-defmethod that would replace it in all cases so ended up redefining it.
;; I have a feeling I am going to regret this.
(cl-defgeneric xref-backend-references (_backend identifier)
  "Find references of IDENTIFIER.
The result must be a list of xref objects.  If no references can
be found, return nil.

The default implementation uses `semantic-symref-tool-alist' to
find a search tool; by default, this uses \"find | grep\" in the
`project-current' roots."
  (cl-mapcan
   (lambda (dir)
     (xref-collect-references identifier dir))
   (let ((pr (projectile-project-root)))
     (if pr
         (list pr)
       (let ((pr (project-current t)))
         (append
          (project-roots pr)
          (project-external-roots pr)))))))

;;; cmake-build.el
;; Yet another not-completely integrated but very useful project management
;; package. If I continue using it, then:
;; TODO: advise projectile-compile-project to do cmake-build-current if cmake
;; config exists in the current project.
;; TODO: make projectile-configure-project to do cmake-build-set-cmake-profile
;; and cmake-build-clear-cache-and-configure if cmake config exists in the
;; current project.
;; TODO: make projectile-test-project to run cmake test config?
(require 'cmake-build)
(setq cmake-build-options "-j5")
(global-set-key (kbd "<f9>") #'cmake-build-current)
(global-set-key (kbd "<C-f9>") #'cmake-build-run)

;;; all-the-icons-dired
(require 'all-the-icons-dired)
(add-hook 'dired-mode-hook #'all-the-icons-dired-mode)

;;; deadgrep. Alternatives: rg.el, ripgrep.el, counsel/helm, etc.
(require 'deadgrep)
(global-set-key (kbd "<f5>") #'deadgrep)
;; Integrate deadgrep with projectile by using the latter's project root, if
;; available. Deadgrep used to have a hard dependency on projectile but they
;; removed it altogether instead of making it a soft one. Projectile itself
;; should integrate with project.el but I am not holding my breath having read
;; https://github.com/bbatsov/projectile/issues/1282
(advice-add #'deadgrep--project-root :before-until #'projectile-project-root)

;;; setup.el ends here
