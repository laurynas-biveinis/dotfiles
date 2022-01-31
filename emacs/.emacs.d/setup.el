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
(defvar secrets-org-file)
(defvar my-frame-font)
(defvar dotfiles--initial-file-name-handler-alist)
(declare-function LaTeX-install-toolbar "tex-bar" ())
(declare-function TeX-source-correlate-mode "tex" (&optional arg))
(declare-function start-erc-chats "" ())

;;;; General settings

;;; Autosave
;; Autosave should be idle-based only, it is very annoying when it autosaves in
;; the middle of typing, even more so with org-encrypted blocks.
(setq auto-save-interval 0)
(setq auto-save-timeout 30)

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

(setq delete-by-moving-to-trash t)

(setq use-dialog-box nil)

(setq history-delete-duplicates t)

(setq read-process-output-max (* 1024 1024))

(setq what-cursor-show-names t)

(setq switch-to-prev-buffer-skip 'this)

;; isearch
(setq isearch-lazy-count t)
(setq isearch-yank-on-move 'shift)

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
(defconst dotfiles--fill-column 80)
(setq-default fill-column dotfiles--fill-column)

(setq sentence-end-double-space nil)

;;; whitespace-mode
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style '(face trailing lines-tail empty indentation big-intent
                              space-after-tab space-before-tab))
(setq whitespace-line-column (+ dotfiles--fill-column 1))
(setq whitespace-global-modes '(not dired-mode erc-mode markdown-mode gfm-mode
                                    lisp-interaction-mode help-mode Info-mode
                                    magit-status-mode org-agenda-mode grep-mode
                                    package-menu-mode vterm-mode))

(global-so-long-mode 1)

(global-hl-line-mode)

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

(defun eight-windows ()
  "Make frame contain 2x4 windows."
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (split-window-right)
  (split-window-right)
  (windmove-down)
  (split-window-right)
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
  "Resize and reposition frame to GEOMETRY."
  (set-frame-position nil (dotfiles--frame-geometry-left
                           geometry)
                      (dotfiles--frame-geometry-top geometry))
  (set-frame-size nil (dotfiles--frame-geometry-width geometry)
                  (dotfiles--frame-geometry-height geometry)))

(defconst dotfiles--darkstar-laptop-screen '(1440 . 900))
(defconst dotfiles--darkstar-laptop-geometry
  (make-dotfiles--frame-geometry :top 1 :left 1 :height 55 :width 202))

(defconst dotfiles--darkstar-external-screen '(7456 . 1692))
(defconst dotfiles--darkstar-external-geometry
  (make-dotfiles--frame-geometry :top 4 :left 3011 :height 117 :width 426))

(defconst dotfiles--frame-geometries-to-ignore
  [(3600 . 1080) (5520 . 1080) (4688 . 1692) (3600 . 1692) (3008 . 1692)]
  "Possible interim screen resolutions while docking/undocking to be ignored.")

(defun dotfiles--diagnose-unknown-display-geometry (display-geometry)
  "Diagnose unknown DISPLAY-GEOMETRY."
  (message "Unknown display size %sx%s"
           (car display-geometry) (cdr display-geometry)))

(require 'seq)
(let ((display-geometry (cons (display-pixel-width) (display-pixel-height))))
  (cond ((equal display-geometry dotfiles--darkstar-laptop-screen)
         ;; darkstar without external screens: initial frame positioned in the
         ;; top left corner
         (dotfiles--add-frame-geometry-to-initial-alist
          dotfiles--darkstar-laptop-geometry)
         (two-windows))
        ((equal display-geometry dotfiles--darkstar-external-screen)
         ;; darkstar with external screens: initial frame maximized in the
         ;; middle screen
         (dotfiles--add-frame-geometry-to-initial-alist
          dotfiles--darkstar-external-geometry)
         (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
         (add-to-list 'initial-frame-alist '(fullscreen-restore . maximized))
         (eight-windows))
        ((seq-position dotfiles--frame-geometries-to-ignore display-geometry)
         ())
        (t (dotfiles--diagnose-unknown-display-geometry display-geometry))))

;; Use specified font if any, otherwise use system font
(cond ((symbolp 'my-frame-font)
       (add-to-list 'default-frame-alist `(font . ,my-frame-font))
       (add-to-list 'initial-frame-alist `(font . ,my-frame-font)))
      ((symbolp 'font-use-system-font)
       (setq font-use-system-font t)))

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
(defun indent-if-prog-mode (&optional _ARG)
  "Indent current region if in programming mode and no prefix arg."
  (interactive)
  (if (and (not current-prefix-arg) (derived-mode-p 'prog-mode))
      (indent-region (region-beginning) (region-end) nil)))

(advice-add #'yank :after #'indent-if-prog-mode)
(advice-add #'yank-pop :after #'indent-if-prog-mode)

;; Enable some disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)

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

;;; display-fill-column-indicator
(require 'display-fill-column-indicator)
(setq-default display-fill-column-indicator-column (+ dotfiles--fill-column 1))

(global-display-fill-column-indicator-mode 1)

(defun dotfiles--maybe-disable-fci ()
  "Selectively disable `display-fill-column-indicator' in some buffers."
  (let ((buf-name (buffer-name)))
    (if (or buffer-read-only (derived-mode-p 'org-agenda-mode)
            (equal " *Agenda Commands*" buf-name)
            (equal " *Org Select*" buf-name)
            (string-prefix-p "*helm" buf-name))
        (setq-local display-fill-column-indicator nil))))

(add-hook 'display-fill-column-indicator-mode-hook
          #'dotfiles--maybe-disable-fci)

;;; minibuffer
;; All config made redundant by Helm:
;; (require 'setup-icomplete)
;; (require 'setup-ido)
;; (require 'setup-minibuf-eldef)
;; (require 'setup-minibuffer)

;;; autorevert
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)

;;; Completion at point
(setq completion-styles '(flex))
;; Remove the default `tags-completion-at-point', I never use tags.
(setq completion-at-point-functions nil)

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

;; `goto-address-mode' integration with `flyspell': do not create `flypsell'
;; overlays, if a `goto-address' one already exists at the location. Otherwise
;; a mouse click would offer spelling corrections instead of going to the URL.
(require 'flyspell)

(defun dotfiles--goto-address-overlay-p (o)
  "Return t if O is an overlay used by `goto-address'."
  (and (overlayp o) (overlay-get o 'goto-address)))

(defun dotfiles--no-flyspell-overlay-on-goto-address (beg _end _face
                                                          _mouse-face)
  "Do not create a `flyspell' overlay if a `goto-address' one exists at BEG."
  (seq-every-p #'null (mapcar #'dotfiles--goto-address-overlay-p (overlays-at
                                                                  beg))))

(advice-add #'make-flyspell-overlay :before-while
            #'dotfiles--no-flyspell-overlay-on-goto-address)

;;; dired
;; Copy recursively
(require 'dired)
(setq dired-recursive-copies 'always)

(require 'dired-aux)
(setq dired-create-destination-dirs 'ask)

(defun dotfiles--load-dired-x ()
  "Load dired-x."
  (load "dired-x"))

(add-hook 'dired-load-hook #'dotfiles--load-dired-x)

(add-hook 'Man-mode-hook #'goto-address)

(put 'dired-find-alternate-file 'disabled nil)

(setq wdired-allow-to-change-permissions t)

;;; cc-mode
(require 'cc-mode)
(require 'cc-vars)

;; TAB indents only if point in the beginning of the line
(setq c-tab-always-indent 1)

;; Typing # moves it to the first column, when not under LSP
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

;;; modern-cpp-font-lock
(require 'modern-cpp-font-lock)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; Grand Unified Debugger
(gud-tooltip-mode t)

;;; windmove
(windmove-default-keybindings 'super)
;; TODO(laurynas): if I ever move to two frames or more setup, look into
;; framemove

;; Compilation
(require 'compile)
(setq compilation-scroll-output 'first-error)
(setq compilation-environment '("LANG=C" "TERM=xterm-256color"))

(require 'xterm-color)
(defun dotfiles--compilation-filter-advice (f proc string)
  "Compilation filter for xterm-256color, taking F, PROC, & STRING."
  (funcall f proc (xterm-color-filter string)))

(advice-add #'compilation-filter :around #'dotfiles--compilation-filter-advice)

;;; elisp-mode
(defun dotfiles--emacs-lisp-mode-hook ()
  "Configuration for 'emacs-lisp-mode'."
  ;; Should the global default ever change, elisp should stay with spaces
  (setq indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook #'dotfiles--emacs-lisp-mode-hook)

;;; gnutls
(require 'gnutls)
(setq gnutls-verify-error t)
;; Because macOS + Emacs + gnutls is broken:
;; http://emacs.1067599.n8.nabble.com/bug-36017-27-0-50-TLS-1-3-on-macOS-exhibits-similar-issue-to-34341-td485542.html#a485721
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;; gpg/epa/EasyPG
(require 'epa)
(setq epg-pinentry-mode 'loopback)

;; TODO(laurynas): report this bug upstream
(defun dotfiles--set-epg-context-pinentry-mode (context _cipher)
  "Fix epg CONTEXT to the correct pinentry mode."
  (setf (epg-context-pinentry-mode context) epg-pinentry-mode))

(advice-add #'epg-decrypt-string :before
            #'dotfiles--set-epg-context-pinentry-mode)

;; TODO(laurynas): would like set `epa-file-name-regexp' to exclude
;; ".authinfo.gpg", but it's too much pain to write an excluding Emacs regexp,
;; see i.e.
;; https://stackoverflow.com/questions/2217928/how-do-i-write-a-regular-expression-that-excludes-rather-than-matches-e-g-not
;; . I have also looked into advising `epa-file-name-regexp' users, but that
;; immediately spills over to `epa-file-handler' to `file-name-handler-alist',
;; making it non-viable.

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

;;; smartparens
;; I tried it, but it didn't stick. Performance issues, strict mode getting in
;; the way, etc.
;; (require 'setup-smartparens)

;;; vterm
(require 'vterm)
(define-key vterm-mode-map (kbd "<S-prior>") #'scroll-down-command)
(setq vterm-max-scrollback 100000)
(setq vterm-buffer-name-string "vterm %s")

;;; Tramp
(require 'tramp)
;; The default level 3 is too noisy with showing each file shipped
(setq tramp-verbose 2)
;; Not sure I care about scp overhead in 2020. Also, maybe this will help with
;; duplicated sshx/scpx paths in lsp-mode cache.
(require 'tramp-sh)
(setq tramp-copy-size-limit nil)
(setq tramp-default-method "scpx")
(setq remote-file-name-inhibit-cache t)

;; TRAMP "integration" with VC: I only use git on remote hosts, handled by
;; Magit, thus disable VC over TRAMP
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; epa "integration" with TRAMP: avoid choking trying to gpg-decrypt
;; non-encrypted ~/.authinfo.gpg.
(setq tramp-completion-use-auth-sources nil)


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
  (+ screen-sharing-default-height 70)
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
  (global-display-line-numbers-mode -1))

;; Google C style
(require 'google-c-style)
(c-add-style "google" google-c-style)

;;; `imenu'
(require 'imenu)
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
(setq org-M-RET-may-split-line '((default . nil)))
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
(setq org-fontify-todo-headline t)
(setq org-fontify-done-headline t)
(setq org-adapt-indentation t)

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
;; Build agenda buffers faster
(setq org-agenda-dim-blocked-tasks nil)

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
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
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

;; Borrowed from https://emacs.nasy.moe/
(defun dotfiles--org-verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function #'dotfiles--org-verify-refile-target)

(setq org-clock-display-default-range 'untilnow)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline main-org-file "Tasks")
         "** TODO %?\n  %i\n  %a")
        ("i" "Inbox" entry (file+headline main-org-file "Inbox")
         "** %?\n  %i\n  %a")
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
;; TODO(laurynas): compute these columns from the frame size calculations above.
(setq org-tags-column -85)
(setq org-agenda-tags-column 'auto)

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

(defun my-secrets ()
  "Open my secrets."
  (interactive)
  (find-file secrets-org-file)
  (org-decrypt-entries))

(global-set-key (kbd "<f8>") #'my-secrets)

;; A hack, it is surprising no official function for this exists. But then
;; again, I need to `string-trim' it too.
(defun my-copy-cell ()
  "Copy the current org table cell to the kill ring."
  (interactive)
  (let ((p (point)))
    (org-table-copy-region p p))
  (kill-new (string-trim (caar org-table-clip))))

(define-key org-mode-map (kbd "<f7>") #'my-copy-cell)

;; org-id
(require 'org-id)
(setq org-id-link-to-org-use-id t)

;; Save org buffers automatically
(add-hook 'auto-save-hook #'org-save-all-org-buffers)

(defun dotfiles--org-mode-hook ()
  "My configuration hook for 'org-mode'."
  (local-set-key (kbd "C-c C-x C-k") #'org-decrypt-entry)
  (setq fill-column 85)
  (setq whitespace-line-column 86))

(add-hook 'org-mode-hook #'dotfiles--org-mode-hook)

;; org integration with Helm
(setq org-outline-path-complete-in-steps nil)

;;; neuron-mode
;; Load it (autoload is not enough) so that it automatically overrides
;; markdown-mode on qualifying buffers.
(require 'neuron-mode)

(declare-function my-open-zettelkasten "secrets" nil)

(global-set-key (kbd "<f12>") #'my-open-zettelkasten)

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

;; TODO(laurynas): this is in-buffer highlight, right?
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
(require 'magit)
(global-set-key (kbd "C-x g") #'magit-status)

(setq magit-status-goto-file-position t)

;; Magit "integration" with VC
(setq vc-handled-backends (delq 'Git vc-handled-backends))

(defun dotfiles--turn-off-size-indication-mode ()
  "Turn off function `size-indication-mode' unconditionally."
  (size-indication-mode -1))

(add-hook 'magit-status-mode-hook #'dotfiles--turn-off-size-indication-mode)

;;; git-gutter-fringe
(require 'git-gutter-fringe)
(global-git-gutter-mode +1)

;; Disable git-gutter-fringe over TRAMP. Not the best option to replace an
;; internal function but oh well. Not much to be gained by advising neither.
(defun git-gutter--turn-on ()
  "Upstream git-gutter--turn-on replacement to disable git-gutter for TRAMP."
  (when (and (buffer-file-name)
             (not (file-remote-p (buffer-file-name)))
             (not (memq major-mode git-gutter:disabled-modes)))
    (git-gutter-mode +1)))

;; Integrate git-gutter with magit: update markings on magit refresh
(add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)

;;; undo-tree
(require 'undo-tree)
(add-to-list 'undo-tree-incompatible-major-modes #'help-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'Info-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'grep-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'magit-status-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'package-menu-mode)
(global-undo-tree-mode)

;;; Wakatime
(require 'wakatime-mode)
(global-wakatime-mode)

;;; Flycheck. 26.1+ flymake works too.
(require 'flycheck)
(setq flycheck-global-modes '(not org-agenda-mode vterm-mode erc-mode))
(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc c/c++-cppcheck))

(global-flycheck-mode)
(setq flycheck-emacs-lisp-load-path 'inherit)

;; flycheck-color-mode
(require 'flycheck-color-mode-line)
(add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode)

;; `flycheck-google-cpplint'
(require 'flycheck-google-cpplint)
;; TODO(laurynas): it can be enabled without LSP as well, but there is no C/C++
;; checker chain in that case.
(defun dotfiles--lsp-flycheck-enable-cpplint ()
  "Enable cpplint for C and C++ buffers under LSP."
  (when (derived-mode-p 'c-mode 'c++-mode)
    (flycheck-add-next-checker 'lsp 'c/c++-googlelint)))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-flycheck-enable-cpplint)

;; `flycheck-status-emoji-mode'
(require 'flycheck-status-emoji)
(flycheck-status-emoji-mode)

;;; Company mode
(require 'company)
(add-hook 'after-init-hook #'global-company-mode)

;; Remove `company-semantic', `company-bbdb', `company-eclim', `company-clang',
;; `company-xcode', `company-oddmuse', (`company-gtags', `company-etags'), and
;; `company-dabbrev' from company backends.
(setq company-backends '(company-capf company-files
                                      (company-dabbrev-code company-keywords)))

(setq company-global-modes '(not Info-mode help-mode magit-status-mode
                                 org-agenda-mode grep-mode package-menu-mode
                                 vterm-mode))

(setq company-abort-manual-when-too-short t)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-idle-delay .3)
(setq company-selection-wrap-around t)

;;; company-box
;; I would like to use it, but solarized dark theme and company-box are not
;; integrated, resulting in too bright highlights. Maybe I will fix it myself
;; later.
;; (require 'setup-company-box)

;;; company-quickhelp-mode
;; I would like to use it, but pos-tip results in ugly tooltips on macOS:
;; https://github.com/pitkali/pos-tip/issues/11.
;; (require 'setup-company-quickhelp)

;;;; Helm

;;; helm-icons
(require 'helm-icons)
(setq helm-icons-provider 'all-the-icons)
(helm-icons-enable)

;; Workaround https://github.com/yyoncho/helm-icons/issues/16 (Bringing up
;; helm-buffers-list breaks when using all-the-icons provider.)

(defun dotfiles--helm-icons--get-icon (file)
  "Get icon for FILE."
  (cond ((eq helm-icons-provider 'all-the-icons)
         (require 'all-the-icons)
         (concat
          (or (cond ((not (stringp file)) (all-the-icons-octicon "gear"))
                    ((or
                      (member (f-base file) '("." ".."))
                      (f-dir? file))
                     (all-the-icons-octicon "file-directory")))
              (all-the-icons-icon-for-file file))
          " "))
        ((eq helm-icons-provider 'treemacs)
         (helm-icons--treemacs-icon file))))

(advice-add #'helm-icons--get-icon :override #'dotfiles--helm-icons--get-icon)

(defun dotfiles--helm-icons--get-icon-for-mode (mode)
  "Get icon for MODE.
First it will use the customized helm-icons-mode->icon to resolve the icon,
otherwise it tries to use the provider."
  (or (-some->> (assoc major-mode helm-icons-mode->icon)
        (cl-rest)
        helm-icons--get-icon)
      (cond ((eq helm-icons-provider 'all-the-icons)
             (-let ((icon (all-the-icons-icon-for-mode mode)))
               (when (stringp icon) (concat icon " "))))
            (t nil))))


(defun dotfiles--helm-icons-buffers-add-icon (candidates _source)
  "Add icon to buffers source.
CANDIDATES is the list of candidates."
  (-map (-lambda ((display . buffer))
          (cons (concat
                 (with-current-buffer buffer
                   (or (dotfiles--helm-icons--get-icon-for-mode major-mode)
                       (-some->> (buffer-file-name)
                         helm-icons--get-icon)
                       (helm-icons--get-icon 'fallback)))
                 display)
                buffer))
        candidates))

(advice-add #'helm-icons-buffers-add-icon :override
            #'dotfiles--helm-icons-buffers-add-icon)


;;; Core
(require 'helm-config)
(require 'helm)
(require 'helm-files)
(require 'helm-for-files)
(setq helm-split-window-inside-p t)
(setq helm-echo-input-in-header-line t)
(setq helm-move-to-line-cycle-in-source t)
(setq helm-ff-search-library-in-sexp t)
(setq helm-net-prefer-curl t)
(setq helm-list-directory-function #'helm-list-dir-external)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-x b") #'helm-mini)
(substitute-key-definition #'apropos-command #'helm-apropos
                           (current-global-map))
(helm-mode 1)

;; helm-buffers
(require 'helm-buffers)
(setq helm-buffers-favorite-modes '(text-mode))

;; helm-autoresize-mode
(helm-autoresize-mode)

;; Helm integration with recentf
(setq helm-ff-file-name-history-use-recentf t)
(setq helm-recentf-fuzzy-match t)

;;; helm-descbinds
(require 'helm-descbinds)
(helm-descbinds-mode)

;;; helm-imenu
(require 'helm-imenu)
(setq helm-imenu-fuzzy-match t)

;;; helm-grep
(require 'helm-grep)
(setq helm-grep-default-command "grep --color-always -d skip %e -n%cH -e %p %f")
(setq helm-grep-default-recurse-command
      "grep --color=always -d recurse %e -n%cH -e %p %f")
(setq helm-grep-file-path-style 'relative)

;;; helm-dash
;; It's a shame Man_Pages documentation set is a dummy one and cannot be browsed
;; with `helm-dash'. If it becomes too annoying, look into dash-at-point instead.
(require 'helm-dash)
(setq helm-dash-browser-func 'eww)
(define-key global-map (kbd "<C-f1>") #'helm-dash-at-point)
;; TODO(laurynas): `thing-at-point' at "std::foo" returns "foo" whereas for
;; `helm-dash' std:: prefix would be useful too.

;; helm-dash integration with sh-mode
(defun dotfiles--helm-dash-sh-mode-hook ()
  "Integrate `helm-dash' with `sh-mode'."
  ;; We do not want to have (defvar dash-docs-docsets), it should be a local
  ;; variable only
  (with-suppressed-warnings ((free-vars dash-docs-docsets))
    (setq-local dash-docs-docsets '("Bash"))))
(add-hook 'sh-mode-hook #'dotfiles--helm-dash-sh-mode-hook)

;; helm-dash integration with c-mode
(defun dotfiles--helm-dash-c-mode-hook ()
  "Integrate `helm-dash' with `c-mode'.."
  (with-suppressed-warnings ((free-vars dash-docs-docsets))
    (setq-local dash-docs-docsets '("C"))))
(add-hook 'c-mode-hook #'dotfiles--helm-dash-c-mode-hook)

;; helm-dash integration with c++-mode
(defun dotfiles--helm-dash-c++-mode-hook ()
  "Integrate `helm-dash' with `c++-mode'.."
  (with-suppressed-warnings ((free-vars dash-docs-docsets))
    (setq-local dash-docs-docsets '("Boost" "C" "C++" "CMake"))))
(add-hook 'c++-mode-hook #'dotfiles--helm-dash-c++-mode-hook)

;; helm-dash integration with emacs-lisp-mode
(defun dotfiles--helm-dash-emacs-lisp-mode-hook ()
  "Integrate `helm-dash' with `emacs-lisp-mode'.."
  (with-suppressed-warnings ((free-vars dash-docs-docsets))
    (setq-local dash-docs-docsets '("Emacs Lisp"))))
(add-hook 'emacs-lisp-mode-hook #'dotfiles--helm-dash-emacs-lisp-mode-hook)

;; helm-dash integration with markdown-mode
(defun dotfiles--helm-dash-markdown-mode-hook ()
  "Integrate `helm-dash' with `markdown-mode'.."
  (with-suppressed-warnings ((free-vars dash-docs-docsets))
    (setq-local dash-docs-docsets '("Markdown"))))
(add-hook 'markdown-mode-hook #'dotfiles--helm-dash-markdown-mode-hook)

;;; helm-org integration
(require 'helm-mode)
(require 'helm-org)
(setq helm-org-headings-fontify t)
(setq helm-org-format-outline-path t)

;;; TODO(laurynas): integrate Helm with flyspell? Neither flyspell-correct /
;;; flyspell-correct-helm nor helm-flyspell replace ispell-word.

;;; TODO(laurynas): integrate Helm with rg?

;;;; xref
;; Do not initialize `xref-backend-functions' to `etags--xref-backend', we never
;; use etags and let major modes (LSP, elisp) define useful backends themselves.
(remove-hook 'xref-backend-functions #'etags--xref-backend)

;;;; lsp-mode
(require 'lsp-mode)
(require 'lsp-clangd)
(setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
(setq lsp-clients-clangd-args '("--all-scopes-completion"
                                "--background-index"
                                "--clang-tidy"
                                "--cross-file-rename"
                                "--header-insertion=never"
                                "--enable-config"
                                "-j=5"
                                "--pch-storage=memory"))
(setq lsp-enable-snippet t)
;; TODO(laurynas): LSP-formatting yanked region is nice, but it formats
;; surroundings of the region too, which is very annoying in case of i.e. C++
;; and a missing semicolon at the end of the region.
(setq lsp-enable-indentation nil)
;; TODO(laurynas): C and C++ docs render with interspersed random backslashes:
;; https://emacs.stackexchange.com/questions/55056/how-to-improve-eldoc-lsp-mode-output-with-c-c-comments
;; https://github.com/jrblevin/markdown-mode/issues/409
(setq lsp-eldoc-render-all t)
(setq lsp-before-save-edits nil)
(setq lsp-restart 'auto-restart)
(setq lsp-semantic-tokens-enable t)
(setq lsp-headerline-breadcrumb-enable t)

(defun dotfiles--lsp-mode-line ()
  "Construct the mode line text for `lsp-mode'."
  (if (lsp-workspaces)
      " LSP"
    " !LSP"))

(setf (alist-get 'lsp-mode minor-mode-alist)
      '((:eval (dotfiles--lsp-mode-line))))

(require 'lsp-headerline)
(setq lsp-headerline-breadcrumb-segments '(project file symbols))

;;; lsp-diagnostics-mode
(require 'lsp-diagnostics)
(add-hook 'lsp-managed-mode-hook #'lsp-diagnostics-mode)

;;; lsp-modeline-diagnostics
(require 'lsp-modeline)
(setq lsp-modeline-diagnostics-scope :workspace)
(add-hook 'lsp-managed-mode-hook #'lsp-modeline-diagnostics-mode)

(require 'lsp-ui)
(setq lsp-ui-sideline-ignore-duplicate t)
(setq lsp-ui-sideline-show-symbol nil)
(setq lsp-ui-peek-fontify 'always)
(setq lsp-ui-peek-peek-height 30)
(setq lsp-ui-sideline-actions-kind-regex ".*")

(define-key lsp-ui-mode-map [remap xref-find-definitions]
  #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references]
  #'lsp-ui-peek-find-references)

(require 'lsp-ui-doc)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature t)

;; lsp-ui and lsp-ui-doc integration: avoid doc popups hiding reference popups
;; by hiding the former.
(advice-add #'lsp-ui-peek-find-references :before #'lsp-ui-doc-hide)

;; lsp-mode and TRAMP interaction: do not flycheck too eagerly
(defun dotfiles--lsp-tramp-flycheck-reduce ()
  "Tune down Flycheck eagerness in `lsp-mode' for TRAMP buffers."
  (setq-local flycheck-check-syntax-automatically '(save idle-change new-line)))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-tramp-flycheck-reduce)

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

(defun dotfiles--lsp-disable-electric-keys ()
  "Disable any electric keys if LSP on type formatting is enabled."
  ;; Using internal LSP symbols is not ideal but I don't see an alternative.
  (when (and lsp-enable-on-type-formatting (lsp--capability
                                            :documentOnTypeFormattingProvider))
    (electric-layout-mode -1)
    ;; It seems it's OK to call this in non-cc-mode buffers too
    (electric-pair-local-mode -1)
    (c-toggle-electric-state -1)))

(defun dotfiles--lsp-replace-cc-mode-indent ()
  "Make ‘c-indent-defun’ and ‘c-indent-region’ use LSP."
  (when (and lsp-enable-indentation
             ;; Using internal LSP symbols is not ideal but I don't see an
             ;; alternative.
             (or (lsp--capability :documentRangeFormattingProvider)
                 (lsp--registered-capability "textDocument/rangeFormatting")))
    (setq-local dotfiles--use-lsp-indent t)
    (advice-add #'c-indent-defun :around #'dotfiles--lsp-format-defun-advice)
    (advice-add #'c-indent-region :around #'dotfiles--lsp-format-region-advice)))

(defun dotfiles--lsp-restore-cc-mode-indent (_lsp_workspace)
  "Make ‘c-indent-defun’ and ‘c-indent-region’ no longer use LSP."
  (setq-local dotfiles--use-lsp-indent nil))

(defun dotfiles--lsp-disable-eldoc ()
  "Disable eldoc for LSP."
  (eldoc-mode -1))

(defun dotfiles--lsp-uninitialization (_lsp_workspace)
  "General cleanup after LS uninitialization."
  (electric-layout-mode)
  ;; It seems it's OK to call this in non-cc-mode buffers too
  (c-toggle-electric-state)
  (electric-pair-local-mode)
  (eldoc-mode))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-replace-cc-mode-indent)
(add-hook 'lsp-after-open-hook #'yas-minor-mode-on)
(add-hook 'lsp-after-open-hook #'dotfiles--lsp-disable-electric-keys)
(add-hook 'lsp-after-open-hook #'dotfiles--lsp-disable-eldoc)

(add-hook 'lsp-after-uninitialized-functions
          #'dotfiles--lsp-restore-cc-mode-indent)
(add-hook 'lsp-after-uninitialized-functions #'dotfiles--lsp-uninitialization)

(defun dotfiles--lsp-deferred-if-supported ()
  "Run `lsp-deferred' if it's a supported mode."
  (unless (derived-mode-p 'emacs-lisp-mode 'makefile-bsdmake-mode
                          'makefile-gmake-mode 'asm-mode)
    (lsp-deferred)))

(add-hook 'prog-mode-hook #'dotfiles--lsp-deferred-if-supported)

;;; lsp-mode clangd setup
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

;; https://clang.llvm.org/extra/clangd/Features.html#formatting -
;; "Format-as-you-type is experimental and doesn’t work well yet."
(defun dotfiles--lsp-turn-off-on-type-formatting ()
  "Turn off LSP on type formatting for the current buffer."
  (setq-local lsp-enable-on-type-formatting nil))

(add-hook 'c-mode-common-hook #'dotfiles--lsp-turn-off-on-type-formatting)

;;; lsp-treemacs
(require 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)

;;; Integrate lsp-mode with projectile
(setq lsp-auto-guess-root t)

;;; helm-lsp
(require 'helm-lsp)

(defun dotfiles--lsp-bind-helm-lsp-workspace-symbol ()
  "Rebind C-M-. to helm-lsp-workspace-symbol."
  (define-key lsp-mode-map [remap xref-find-apropos]
    #'helm-lsp-workspace-symbol))

(defun dotfiles--lsp-unbind-helm-lsp-workspace-symbol (_lsp_workspace)
  "Restore global C-M-. binding."
  (define-key lsp-mode-map [remap xref-find-apropos] #'xref-find-apropos))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-bind-helm-lsp-workspace-symbol)
(add-hook 'lsp-after-uninitialized-functions
          #'dotfiles--lsp-unbind-helm-lsp-workspace-symbol)

(define-key global-map [remap xref-find-apropos]
  #'helm-lsp-global-workspace-symbol)

;;; lsp-mode integration with which-key
(defun dotfiles--lsp-enable-which-key ()
  "Enable `lsp-mode' integration with `which-key' for all major modes."
  (lsp-enable-which-key-integration t))

(add-hook 'lsp-mode-hook #'dotfiles--lsp-enable-which-key)

;;; `lsp-mode' integration with Flycheck `sh-shellcheck' checker. It will become
;;; redundant if bash-language-server implements
;;; https://github.com/bash-lsp/bash-language-server/issues/104
(defun dotfiles--lsp-flycheck-enable-shellcheck ()
  "Enable Shellcheck for shell buffers under LSP."
  (when (derived-mode-p 'sh-mode)
    (flycheck-add-next-checker 'lsp 'sh-shellcheck)))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-flycheck-enable-shellcheck)

;;;; SSH config mode
(add-hook 'ssh-config-mode-hook #'turn-on-font-lock)
(add-hook 'ssh-config-mode-hook #'dotfiles--enable-show-trailing-ws)
(add-hook 'ssh-config-mode-hook #'turn-on-auto-fill)

;;; dispwatch
(require 'dispwatch)
(defun dotfiles--display-changed-hook (new-display-geometry)
  "Reconfigure windows on screen resolution change to NEW-DISPLAY-GEOMETRY."
  (message "Resizing for %s" new-display-geometry)
  (cond ((equal new-display-geometry dotfiles--darkstar-laptop-screen)
         (dotfiles--move-to-frame-geometry dotfiles--darkstar-laptop-geometry)
         (set-frame-parameter nil 'fullscreen 'maximized)
         (two-windows))
        ((equal new-display-geometry dotfiles--darkstar-external-screen)
         (dotfiles--move-to-frame-geometry dotfiles--darkstar-external-geometry)
         (set-frame-parameter nil 'fullscreen 'fullboth)
         (eight-windows))
        ((seq-position dotfiles--frame-geometries-to-ignore
                       new-display-geometry) ())
        (t (dotfiles--diagnose-unknown-display-geometry new-display-geometry))))

(add-hook 'dispwatch-display-change-hooks #'dotfiles--display-changed-hook)
(dispwatch-mode 1)

;;; yaml-mode
(add-to-list 'auto-mode-alist '("/.clang-format\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("/.clang-tidy\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("/.clangd\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("/.oclint\\'" . yaml-mode))

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
;; https://github.com/Malabarba/aggressive-indent-mode/issues/140
(add-to-list 'aggressive-indent-excluded-modes #'makefile-bsdmake-mode)

;;; rich-minority-mode
(require 'rich-minority)
(setq rm-blacklist '(" company" " waka" " Undo-Tree" " =>" " GitGutter" " WS"
                     " ElDoc" " Wrap" " Fill" " all-the-icons-dired-mode"
                     " Projectile" " PgLn" " h-i-g" " mc++fl" " yas" " Helm"
                     " WK" " GCMH" " (*)" " ColorIds" " be" " Fly" " ARev"
                     " tree-sitter"))
(rich-minority-mode)

;;; projectile
(require 'projectile)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action #'helm-projectile)
(setq projectile-use-git-grep t)
(setq projectile-enable-cmake-presets t)
;; Steal s-p from ns-print-buffer. I never print buffers
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Save mode line space
(setq projectile-mode-line-prefix " ")
;; Only use xref for cross-references
(setq projectile-tags-backend 'xref)
;; Exclude some more modes from projectile
(add-to-list 'projectile-globally-ignored-modes "lisp-interaction-mode")
(add-to-list 'projectile-globally-ignored-modes "org-agenda-mode")
(add-to-list 'projectile-globally-ignored-modes "package-menu-mode")
(projectile-mode +1)

;;; helm-projectile
(require 'helm-projectile)

;; Workaround https://github.com/mhayashi1120/Emacs-wgrep/issues/75 by having
;; both projectile-grep and helm-projectile-grep available in projectile keymap,
;; so that the plain version can be used if it's going to be edited with wgrep.
(defun dotfiles--helm-projectile-commander-bindings ()
  "Map both projectile-grep and helm-projectile-grep to own keys."

  (def-projectile-commander-method ?g
    "Run grep on project."
    (projectile-grep))

  (def-projectile-commander-method ?h
    "Run grep on project using Helm."
    (helm-projectile-grep)))

(defun dotfiles--helm-projectile-toggle (toggle)
  "Remap helm-projectile-grep to a separate key, depending on TOGGLE."
  (if (> toggle 0)
      (progn
        (define-key projectile-mode-map [remap projectile-grep] nil)
        (define-key projectile-command-map (kbd "s h") #'helm-projectile-grep))
    (define-key projectile-command-map (kbd "s h") nil)))

(advice-add #'helm-projectile-commander-bindings :after
            #'dotfiles--helm-projectile-commander-bindings)
(advice-add #'helm-projectile-toggle :after #'dotfiles--helm-projectile-toggle)

(helm-projectile-on)

;; Remove "-a" from grep options, because it kills grepping over TRAMP for some
;; projects.
(setq helm-projectile-grep-command "grep -r %e -n%cH -e %p %f .")

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
     (xref-references-in-directory identifier dir))
   (let ((pr (projectile-project-root)))
     (if pr
         (list pr)
       (let ((pr (project-current t)))
         (append
          (project-roots pr)
          (project-external-roots pr)))))))

;; Implement https://github.com/bbatsov/projectile/issues/1676 (Unable to
;; completing-read CMake preset the second time) by adding a new command for
;; reconfigure.
(defun dotfiles--projectile-reconfigure-command (compile-dir)
  "Retrieve the configure command for COMPILE-DIR without considering history.

The command is determined like this:

- first we check for `projectile-project-configure-cmd' supplied
via .dir-locals.el

- finally we check for the default configure command for a
project of that type"
  (or projectile-project-configure-cmd
      (let ((cmd-format-string (projectile-default-configure-command
                                (projectile-project-type))))
        (when cmd-format-string
          (format cmd-format-string (projectile-project-root) compile-dir)))))

(defun projectile-reconfigure-project (arg)
  "Run project configure command without considering command history.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (dotfiles--projectile-reconfigure-command
                  (projectile-compilation-dir))))
    (projectile--run-project-cmd command projectile-configure-cmd-map
                                 :show-prompt arg
                                 :prompt-prefix "Configure command: "
                                 :save-buffers t)))

(define-key projectile-command-map "w" #'projectile-reconfigure-project)

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

;; wgrep-like bindings for deadgrep
(define-key deadgrep-mode-map (kbd "C-c C-p") #'deadgrep-edit-mode)
(define-key deadgrep-edit-mode-map (kbd "C-c C-e") #'deadgrep-mode)

;;; wgrep
(require 'wgrep)
(setq wgrep-auto-save-buffer t)

;;; `beginend'
(require 'beginend)
(beginend-global-mode)

;;; beacon
(require 'beacon)
(setq beacon-blink-when-window-scrolls nil)
(setq beacon-blink-when-window-changes t)
(setq beacon-blink-when-point-moves-vertically nil)
(setq beacon-blink-when-point-moves-horizontally nil)
(setq beacon-blink-when-focused t)
(setq beacon-blink-duration 0.2)
(setq beacon-blink-delay 0.2)
(beacon-mode)

;;; page-break-lines
(require 'page-break-lines)
(global-page-break-lines-mode)

;;; highlight-indent-guides
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-responsive 'stack)
(setq highlight-indent-guides-delay 0)
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)

;;; iedit
;; The default binding of C-; conflicts with flyspell. TODO(laurynas):
;; M-I/M-{/M-} could be useful but the keybindings seem to conflict.
;; iedit seems to be configured in an... unorthodox way. TODO(laurynas): report
;; a bug.
(defvar iedit-toggle-key-default)
(setq iedit-toggle-key-default (kbd "<f6>"))
(require 'iedit)

;;; keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;; which-key
(require 'which-key)
(which-key-mode)

;;;; calfw
;; Workaround warnings stemming from
;; https://github.com/kiwanami/emacs-calfw/issues/101 - "Package `cl' is
;; obsolete."
(with-suppressed-warnings ((obsolete cl)) (require 'calfw))
(require 'calfw-org)
(require 'calfw-ical)

;; Unicode characters
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

(setq cfw:render-line-breaker #'cfw:render-line-breaker-wordwrap)

;;; `stripe-buffer'
(require 'stripe-buffer)
(add-hook 'dired-mode-hook #'stripe-listify-buffer)
(add-hook 'package-menu-mode-hook #'stripe-listify-buffer)
(add-hook 'org-agenda-mode-hook #'stripe-listify-buffer)

;;; `color-identifiers-mode'
(require 'color-identifiers-mode)
(global-color-identifiers-mode)

;; Integrate `color-identifiers-mode' with `lsp-mode': if semantic highlighting
;; is enabled, disable `color-identifiers-mode', because semantic highlighting
;; overwrites any faces of the latter. I did not find a way to make the two
;; interplay more nicely.
(defun dotfiles--lsp-disable-color-identifiers-mode ()
  "Disable `color-identifiers-mode' if semantic higlighting is enabled."
  (if (and lsp-semantic-tokens-enable (lsp--capability :semanticTokensProvider))
      (color-identifiers-mode -1)))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-disable-color-identifiers-mode)

;;; `grab-mac-link'
(require 'grab-mac-link)
(setq grab-mac-link-dwim-favourite-app 'safari)

;;; `info-colors'
(require 'info-colors)
(add-hook 'Info-selection-hook #'info-colors-fontify-node)

;;; `rainbow-delimiters'
(require 'rainbow-delimiters)
;; TODO(laurynas): add to more modes as appropriate
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)

;;; `tree-sitter'
(require 'tree-sitter)
(require 'tree-sitter-langs)

(global-tree-sitter-mode)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;;;; Upgrade helper
(defun my-recompile-packages ()
  "Force full recompilation of installed packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

;;; Restore temporarily swapped for startup duration variables
(dolist (handler file-name-handler-alist)
  (add-to-list 'dotfiles--initial-file-name-handler-alist handler))
(setq file-name-handler-alist dotfiles--initial-file-name-handler-alist)

;;; GC tuning for interactive use. We already set `gc-cons-threshold' to
;;; `most-positive-fixnum' in early-init.el for startup.

;; Collect garbage once Emacs goes out of focus or is suspended, originally
;; thanks to https://github.com/MatthewZMD/.emacs.d, later rewritten for 27.1
;; `after-focus-change-function'.

(defun dotfiles--gc-on-last-frame-out-of-focus ()
  "GC if all frames are inactive."
  (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
      (garbage-collect)))

(add-function :after after-focus-change-function
              #'dotfiles--gc-on-last-frame-out-of-focus)

(add-hook 'suspend-hook #'garbage-collect)

;; Copied with modifications from https://github.com/KaratasFurkan/.emacs.d
;; which seems to have copied it from Doom.
(defvar dotfiles--saved-gc-cons-threshold nil)

(defun dotfiles--defer-gc ()
  "Defer GC by setting `gc-cons-threshold' to `most-positive-fixnum'."
  (setq dotfiles--saved-gc-cons-threshold gc-cons-threshold)
  (setq gc-cons-threshold most-positive-fixnum))

(defun dotfiles--restore-gc ()
  "Re-enable GC by restoring `gc-cons-threshold' to the saved value."
  (setq gc-cons-threshold dotfiles--saved-gc-cons-threshold))

(defun dotfiles--restore-gc-with-delay ()
  "Re-enable GC with a delay to let the launched command execute quickly."
  (run-at-time 1 nil #'dotfiles--restore-gc))

(add-hook 'minibuffer-setup-hook #'dotfiles--defer-gc)
(add-hook 'minibuffer-exit-hook #'dotfiles--restore-gc-with-delay)

(require 'gcmh)
(gcmh-mode)

;;; setup.el ends here
