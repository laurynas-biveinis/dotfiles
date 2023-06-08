;;; my-setup.el --- Main Emacs configuration.  -*- lexical-binding: t; -*-
;;; Commentary:

;; This is the main part of my non-system-specific Emacs setup. Sets the
;; configuration variables, configures the built-in modes, installed packages,
;; and my own code.
;; All features are assumed to exist, because this is a part of my dotfiles repo
;; where the needed packages are committed too.

;;; Code:

;;; Variables and functions defined elsewhere we'll be using
;; Defined in secrets.el:
(defvar no-undo-tree-file-names)
;; Defined in system-specific config files:
(defvar my-frame-font)
;; Defined in early-init.el:
(defvar dotfiles--initial-file-name-handler-alist)

(require 'my-global-keys)

;;; Encodings

(set-language-environment "UTF-8")

;;; Editing

(setq read-quoted-char-radix 16  ;; Enter quoted chars in hex
      sentence-end-double-space nil)

(add-hook 'prog-mode-hook #'electric-layout-mode)

(global-so-long-mode 1)
(delete-selection-mode 1)  ;; Typing or <Delete> will remove selected text

(require 'elec-pair)
(electric-pair-mode)

;; `iedit': the default binding of C-; conflicts with `flyspell'.
;; TODO(laurynas): M-I/M-{/M-} could be useful but the keybindings seem to
;; conflict. iedit seems to be configured in an... unorthodox way.
;; TODO(laurynas): report a bug.
(defvar iedit-toggle-key-default)
(setq iedit-toggle-key-default (kbd "<f6>"))
(require 'iedit)

;;; Indentation

;; Indentation can only insert spaces by default. If this ever
;; changes, add reset to `emacs-lisp-mode' and `rust-mode' hooks.
(setq-default indent-tabs-mode nil)

;; `aggressive-indent-mode'
(require 'aggressive-indent)
(setq aggressive-indent-comments-too t)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes #'help-mode)
(add-to-list 'aggressive-indent-excluded-modes #'Info-mode)
(add-to-list 'aggressive-indent-excluded-modes #'magit-status-mode)
(add-to-list 'aggressive-indent-excluded-modes #'org-agenda-mode)
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

(defun end-of-line-and-newline-and-indent ()
  "Go to the end of line, insert a new line, and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;;; Kill and yank

(setq kill-whole-line t  ;; C-k kills line including its newline
      kill-do-not-save-duplicates t  ;; Do not store duplicate kills
      kill-read-only-ok t)

;; Yank should indent in programming modes
(defun indent-if-prog-mode (&optional _ARG)
  "Indent current region if in programming mode and no prefix arg."
  (interactive)
  (if (and (not current-prefix-arg) (derived-mode-p 'prog-mode))
      (indent-region (region-beginning) (region-end) nil)))

(advice-add #'yank :after #'indent-if-prog-mode)
(advice-add #'yank-pop :after #'indent-if-prog-mode)

;;; Undo

;; The tree-shaped edit history provided by this package is the winner.
(require 'undo-tree)
(require 'magit-status)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(add-to-list 'undo-tree-incompatible-major-modes #'help-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'Info-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'grep-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'magit-status-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'package-menu-mode)
(add-to-list 'undo-tree-incompatible-major-modes #'messages-buffer-mode)

(defun dotfiles--disable-undo-tree-by-name (&optional _print-message)
  "Return nil if the buffer file name is in `dotfiles--no-undo-tree-names'."
  (let ((file-name (buffer-file-name)))
    ;; TODO(laurynas): replace special-casing of suffix and exact match against
    ;; `no-undo-tree-file-names' with a glob match.
    (if file-name (not (or (string-suffix-p "autoloads.el" file-name)
                           (member (file-name-nondirectory (buffer-file-name))
                                   no-undo-tree-file-names)))
      t)))

(advice-add 'turn-on-undo-tree-mode :before-while
            #'dotfiles--disable-undo-tree-by-name)

(global-undo-tree-mode)

;;; Navigation
(setq scroll-error-top-bottom t)

;; `imenu'
(require 'imenu)
(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout 6000000)

;; We could use `global-goto-address-mode' to turn on `goto-address-mode'
;; everywhere but unfortunately it does not seem to turn on
;; `goto-address-prog-mode'.
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
(add-hook 'text-mode-hook #'goto-address-mode)
(add-hook 'Man-mode-hook #'goto-address-mode)

;; `beginend'
(require 'beginend)
(beginend-global-mode)

(windmove-default-keybindings 'super)
;; If I ever move to two frames or more setup, look into `framemove'.

;;; Whitespace

(setq-default indicate-empty-lines t  ;; Trailing newlines are highlighted
              require-final-newline 'query)  ;; Should files end with newline?

;; Display trailing whitespace
(defun dotfiles--enable-trailing-whitespace ()
  "Enable showing of trailing whitespace."
  (setq show-trailing-whitespace t))

(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style '(face trailing empty indentation big-intent
                              space-after-tab space-before-tab))
(setq whitespace-global-modes '(not dired-mode markdown-mode gfm-mode
                                    lisp-interaction-mode help-mode Info-mode
                                    magit-status-mode org-mode org-agenda-mode
                                    grep-mode package-menu-mode vterm-mode))

(add-hook 'prog-mode-hook #'dotfiles--enable-trailing-whitespace)
(add-hook 'text-mode-hook #'dotfiles--enable-trailing-whitespace)

;;; Long line handling
(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

(require 'my-column-limit)
(require 'my-files)

;;; Cursor
(setq-default cursor-in-non-selected-windows nil)
(setq what-cursor-show-names t)
(global-hl-line-mode)

;; `beacon'
(require 'beacon)
(setq beacon-blink-when-window-scrolls nil)
(setq beacon-blink-when-window-changes t)
(setq beacon-blink-when-point-moves-vertically nil)
(setq beacon-blink-when-point-moves-horizontally nil)
(setq beacon-blink-when-focused t)
(setq beacon-blink-duration 0.2)
(setq beacon-blink-delay 0.2)
(beacon-mode)

;;; Message settings
(setq message-log-max t  ;; Keep all messages
      inhibit-startup-message t)  ;; No startup message

;;; Display
(setq transient-mark-mode t  ;; Enable visual feedback on selections
      display-raw-bytes-as-hex t  ;; Raw bytes in hexadecimal not octal
      visible-bell t  ;; No annoying beeps
      fast-but-imprecise-scrolling nil
      redisplay-skip-fontification-on-input t
      recenter-redisplay t)

(setq-default indicate-buffer-boundaries t)

(global-font-lock-mode 1)

;; Show matching parents
(require 'paren)
(setq show-paren-style 'mixed)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(show-paren-mode 1)

;; `stripe-buffer'
(require 'stripe-buffer)
(add-hook 'dired-mode-hook #'stripe-listify-buffer)
(add-hook 'package-menu-mode-hook #'stripe-listify-buffer)
(add-hook 'org-agenda-mode-hook #'stripe-listify-buffer)

;; `page-break-lines'
(require 'page-break-lines)
(global-page-break-lines-mode)

;; `highlight-indent-guides'
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-responsive 'stack)
(setq highlight-indent-guides-delay 0)
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)

;;; isearch
(setq isearch-lazy-count t
      isearch-yank-on-move 'shift
      search-nonincremental-instead nil
      query-replace-skip-read-only t)

;;; Spellchecking

(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'turn-on-flyspell)
;; `ispell'
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

;;; `bookmark'
(require 'bookmark)

;; Save bookmarks automatically
(setq bookmark-save-flag 1)

;;; UI

(load-theme 'solarized-dark t)

(setq use-dialog-box nil)

;; Use specified font if any
(when (boundp 'my-frame-font)
  (add-to-list 'default-frame-alist `(font . ,my-frame-font))
  (add-to-list 'initial-frame-alist `(font . ,my-frame-font)))

;;; modeline
(size-indication-mode)
(column-number-mode t)

;; rich-minority-mode
(require 'rich-minority)
(setq rm-blacklist '(" company" " waka" " Undo-Tree" " =>" " GitGutter" " WS"
                     " ElDoc" " Wrap" " Fill" " all-the-icons-dired-mode"
                     " Projectile" " PgLn" " h-i-g" " mc++fl" " yas" " Helm"
                     " WK" " GCMH" " (*)" " ColorIds" " be" " Fly" " ARev"
                     " tree-sitter" " Abbrev" " org-roam-ui" " TblHeader"
                     " Habit" " :ARCHIVE:" " activity-watch"))
(rich-minority-mode)

;;; Misc settings

(setq history-delete-duplicates t
      read-process-output-max (* 1024 1024)
      switch-to-prev-buffer-skip 'this
      next-error-message-highlight t
      ps-print-color-p 'black-white)

(require 'help-fns)
(setq help-enable-symbol-autoload t)

;;; Enable some disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(require 'my-ui-geometry)

;;; Diffing
(setq diff-switches "-u -p")

;; `ediff'
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

;;; Version control

;; `magit'
(require 'magit)

(setq magit-display-buffer-function
      #'magit-display-buffer-same-window-except-diff-v1)
(setq magit-status-goto-file-position t)
(setq magit-diff-refine-hunk t)
(setq magit-process-popup-time 10)

;; Magit "integration" with VC
(setq vc-handled-backends (delq 'Git vc-handled-backends))

(defun dotfiles--turn-off-size-indication-mode ()
  "Turn off function `size-indication-mode' unconditionally."
  (size-indication-mode -1))

(add-hook 'magit-status-mode-hook #'dotfiles--turn-off-size-indication-mode)

;; `git-gutter'
(require 'git-gutter)
(setq git-gutter:update-interval 0.02)

;; `git-gutter-fringe'
(require 'git-gutter-fringe)

(define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)

(global-git-gutter-mode +1)

;; Disable git-gutter-fringe over TRAMP. Not the best option to replace an
;; internal function but oh well. Not much to be gained by advising neither.
(defun git-gutter--turn-on ()
  "Upstream git-gutter--turn-on replacement to disable git-gutter for TRAMP."
  (when (and (buffer-file-name)
             (not (file-remote-p (buffer-file-name)))
             (not (memq major-mode git-gutter:disabled-modes)))
    (git-gutter-mode +1)))

;;; Structured format file editing

;; XML
(require 'nxml-mode)
(setq nxml-slash-auto-complete-flag t)  ;; Autocomplete closing tags

;; SSH configuration
(add-hook 'ssh-config-mode-hook #'turn-on-font-lock)
(add-hook 'ssh-config-mode-hook #'dotfiles--enable-trailing-whitespace)
(add-hook 'ssh-config-mode-hook #'turn-on-auto-fill)

;; YAML
(add-to-list 'auto-mode-alist '("/.clang-format\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("/.clang-tidy\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("/.clangd\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("/.oclint\\'" . yaml-mode))

;; `prism'
(require 'prism)
(add-hook 'c-mode-common-hook #'prism-mode)
(add-hook 'emacs-lisp-mode-hook #'prism-mode)
(add-hook 'rust-mode-hook #'prism-mode)

(add-hook 'python-mode-hook #'prism-whitespace-mode)
(add-hook 'yaml-mode #'prism-whitespace-mode)

;;; Syntax checking

;; The syntax checker is `flycheck'. 26.1+ flymake would work too.
(require 'flycheck)
(setq flycheck-global-modes '(not org-agenda-mode vterm-mode erc-mode))
(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc
                                                       c/c++-cppcheck))
(setq flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch
                                                 new-line mode-enabled))
(setq flycheck-buffer-switch-check-intermediate-buffers t)

(global-flycheck-mode)
(setq flycheck-emacs-lisp-load-path 'inherit)

;; `flycheck-color-mode'
(require 'flycheck-color-mode-line)
(add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode)

;; `flycheck-status-emoji-mode'
(require 'flycheck-status-emoji)
(flycheck-status-emoji-mode)

;;; Programming

;; Grand Unified Debugger
(gud-tooltip-mode t)

;; `xref': do not initialize `xref-backend-functions' to `etags--xref-backend',
;; we never use etags and let major modes (LSP, elisp) define useful backends
;; themselves.
(remove-hook 'xref-backend-functions #'etags--xref-backend)

;; `tree-sitter'
(require 'tree-sitter)
(require 'tree-sitter-langs)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Compilation
(require 'compile)
(setq compilation-scroll-output 'first-error)
(setq compilation-environment '("LANG=C"))

(require 'fancy-compilation)
(with-eval-after-load 'compile (fancy-compilation-mode))

;;; C and C++ programming

;; `cc-mode'
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

;; `modern-cpp-font-lock'
(require 'modern-cpp-font-lock)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; Google C style
(require 'google-c-style)
(c-add-style "google" google-c-style)

;; `flycheck-google-cpplint'
(require 'flycheck-google-cpplint)
;; TODO(laurynas): it can be enabled without LSP as well, but there is no C/C++
;; checker chain in that case.
(defun dotfiles--lsp-flycheck-enable-cpplint ()
  "Enable cpplint for C and C++ buffers under LSP."
  (when (derived-mode-p 'c-mode 'c++-mode)
    (flycheck-add-next-checker 'lsp 'c/c++-googlelint)))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-flycheck-enable-cpplint)

;;; Shell and terminal emulation

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
(require 'xterm-color)

(defun dotfiles--shell-mode-hook ()
  "My hook for shell-mode."
  (font-lock-mode -1)
  (make-local-variable 'font-lock-function)
  (setq font-lock-function (lambda (_) nil))
  (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter nil t))

(add-hook 'shell-mode-hook #'dotfiles--shell-mode-hook)

;; `vterm'
(require 'vterm)
(define-key vterm-mode-map (kbd "<S-prior>") #'scroll-down-command)
(setq vterm-max-scrollback 100000)
(setq vterm-buffer-name-string "vterm %s")

;;; Cryptography

;; `auth-sources'
(require 'auth-source)
(setq auth-sources '("~/.authinfo.gpg"))

;; `gnutls'
(require 'gnutls)
(setq gnutls-verify-error t)
;; Because macOS + Emacs + gnutls is broken:
;; http://emacs.1067599.n8.nabble.com/bug-36017-27-0-50-TLS-1-3-on-macOS-exhibits-similar-issue-to-34341-td485542.html#a485721
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; gpg/epa/EasyPG
(require 'epa)
(setq epg-pinentry-mode 'loopback)

;; TODO(laurynas): report this bug upstream
(defun dotfiles--set-epg-context-pinentry-mode (context _cipher)
  "Fix epg CONTEXT to the correct pinentry mode."
  (setf (epg-context-pinentry-mode context) epg-pinentry-mode))

(advice-add #'epg-decrypt-string :before
            #'dotfiles--set-epg-context-pinentry-mode)

(require 'my-org)

(require 'my-complete)

;;;; lsp-mode
(require 'lsp-mode)
(require 'lsp-clangd)
(setq lsp-clients-clangd-args '("--all-scopes-completion"
                                "--background-index"
                                "--clang-tidy"
                                "--cross-file-rename"
                                "--header-insertion=never"
                                "--enable-config"
                                "-j=5"
                                "--pch-storage=memory"
                                "-use-dirty-headers"))
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
  (interactive "p" prog-mode)
  (save-mark-and-excursion
    (c-mark-function)
    (lsp-format-region (region-beginning) (region-end))))

(defun dotfiles--lsp-format-defun-advice (orig-fun)
  "Format the defun using LSP with a fallback to ORIG-FUN (‘c-indent-defun’)."
  (if dotfiles--use-lsp-indent (lsp-format-defun)
    (funcall orig-fun)))

(defun dotfiles--lsp-format-region-advice (orig-fun &rest args)
  "Format the region (ARGS) using LSP with a fallback to ORIG-FUN."
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

;;; Setup `topsy', make `lsp-mode' play nicely with it.
(require 'topsy)

(defun dotfiles--lsp-deferred-or-topsy ()
  "Run `lsp-deferred' if it's a supported mode, otherwise enable `topsy-mode'."
  (if (derived-mode-p 'emacs-lisp-mode 'makefile-bsdmake-mode
                      'makefile-gmake-mode 'asm-mode)
      (topsy-mode)
    (lsp-deferred)))

(add-hook 'prog-mode-hook #'dotfiles--lsp-deferred-or-topsy)

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

;;; Rust in `lsp-mode'
(require 'lsp-rust)
(setq lsp-rust-analyzer-cargo-watch-command "clippy")

;;; lsp-treemacs
(require 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)

;;; Integrate `lsp-mode' with `project.el' / `projectile'
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

;; Integrate projectile/lsp-mode with my gitrmworktree
(require 'projectile)
(defun kill-buffers-rm-worktree ()
  "Remove the git worktree and kill project buffers."
  (interactive)
  ;; TODO(laurynas): don't know whether `projectile-kill-buffers' prompt will be
  ;; answered with 'y' or 'n' – assume that zero existing buffers means 'y'. For
  ;; that, `projectile-kill-buffers-filter' must be set to `'kill-all'.
  (when (not (equal projectile-kill-buffers-filter 'kill-all))
    (user-error "Unsupported `projectile-kill-buffers-filter' value %:S"
                projectile-kill-buffers-filter))
  (let ((project-root-path (projectile-acquire-root)))
    (projectile-with-default-dir project-root-path
      (projectile-kill-buffers)
      (let ((buffers (projectile-project-buffers project-root-path)))
        (when (null buffers)
          (projectile-remove-known-project project-root-path)
          ;; TODO(laurynas): check gitrmworktree result code before removing the
          ;; project from the project list.
          (shell-command (concat "gitrmworktree " project-root-path))
          ;; TODO(laurynas): remove the project from treemacs
          (lsp-workspace-folders-remove project-root-path))))))

(define-key projectile-command-map "y" #'kill-buffers-rm-worktree)

;;; all-the-icons-dired
(require 'all-the-icons-dired)
(add-hook 'dired-mode-hook #'all-the-icons-dired-mode)

;;; deadgrep. Alternatives: rg.el, ripgrep.el, counsel/helm, etc.
(require 'deadgrep)

;; wgrep-like bindings for deadgrep
(define-key deadgrep-mode-map (kbd "C-c C-p") #'deadgrep-edit-mode)
(define-key deadgrep-edit-mode-map (kbd "C-c C-e") #'deadgrep-mode)

;;; wgrep
(require 'wgrep)
(setq wgrep-auto-save-buffer t)

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

;;; `grab-mac-link'
(require 'grab-mac-link)
(setq grab-mac-link-dwim-favourite-app 'safari)

;;; `info-colors'
(require 'info-colors)
(add-hook 'Info-selection-hook #'info-colors-fontify-node)

;;;; Rust
;;; `rust-mode'
(require 'rust-mode)

(defun dotfiles--rust-set-fill-column ()
  "Set the correct `fill-column' for `rust-mode'."
  (dotfiles--set-fill-column 100))

(add-hook 'rust-mode-hook #'dotfiles--rust-set-fill-column)

;;; `rustic'
(require 'rustic)

;;; Features: `calculator'
(require 'calculator)
(setq calculator-electric-mode t)

;;; Features: `calendar'
(require 'calendar)
(require 'solar)
(setq calendar-week-start-day 1)
(setq calendar-date-style 'iso)
;; Vilnius!
(setq calendar-latitude 54.7)
(setq calendar-longitude 25.3)
(setq calendar-location-name "Vilnius, Lithuania")

;;; Features: Wakatime
(require 'wakatime-mode)
(global-wakatime-mode)

;;; Features: ActivityWatch
(require 'activity-watch-mode)
(global-activity-watch-mode)

;;; Utilities

(defun my-recompile-packages ()
  "Force full recompilation of installed packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

;; Change appearance for screen sharing

(require 'display-line-numbers)
(setq display-line-numbers-grow-only t)

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

;;; Finish initialization
;; Restore temporarily swapped for startup duration variables
(dolist (handler file-name-handler-alist)
  (add-to-list 'dotfiles--initial-file-name-handler-alist handler))
(setq file-name-handler-alist dotfiles--initial-file-name-handler-alist)

;;; GC tuning for interactive use.

;; `gcmh'. The setup used to disable GC on entering the minibuffer and to do GC
;; on frame becoming inactive, but `gcmh' should handle those cases too.
(require 'gcmh)
(gcmh-mode)

(provide 'my-setup)
;;; my-setup.el ends here
