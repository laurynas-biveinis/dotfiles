;;; my-setup.el --- Main Emacs configuration.  -*- lexical-binding: t; -*-
;;; Commentary:

;; This is the main part of my non-system-specific Emacs setup. Sets the
;; configuration variables, configures the built-in modes, installed packages,
;; and my own code.
;;
;; Like in the rest of my personal configuration, all features (packages and
;; external tools) are assumed to exist, because this is a part of my dotfiles
;; repo where the needed packages are committed too. Thus, no error handling,
;; and no need to ensure compatibility with different Emacs or package versions.
;;
;; Not using `use-package', because a big part of my setup is cross-package
;; integration, where it would not be as useful. Lazy package loading to reduce
;; startup times is of secondary importance because I rarely start new Emacs
;; instances. I may change my mind in the future.

;;; Code:

;;; Variables and functions defined elsewhere we'll be using
;; Defined in system-specific config files:
(defvar my-frame-font)
;; Defined in early-init.el:
(defvar dotfiles--initial-file-name-handler-alist)

(require 'my-global-keys)

;;; Internals

;; So that `memory-report' works
(setq max-lisp-eval-depth 51200)


;;; Encodings

(set-language-environment "UTF-8")

;;; Navigation
(setq scroll-error-top-bottom t)

;; `isearch'
(setq isearch-lazy-count t
      isearch-yank-on-move 'shift
      search-nonincremental-instead nil
      query-replace-skip-read-only t)

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
              require-final-newline t)  ;; Should files end with newline?

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
(setq-default cursor-type 'bar)
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

;; For scrolling, as of 29.1, `pixel-scroll-precision-mode' should not be
;; enabled. Maybe it's due to macOS-specific patches in the port I'm using?

;; Minibuffer
(push #'set-multi-message set-message-functions)

;; Show matching parents
(require 'paren)
(setq show-paren-style 'mixed)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-predicate '(not (or
                                  (derived-mode . special-mode)
                                  (derived-mode . authinfo-mode))))
(setq show-paren-context-when-offscreen t)
(show-paren-mode 1)

;; `stripe-buffer'
(require 'stripe-buffer)
(add-hook 'dired-mode-hook #'stripe-listify-buffer)
(add-hook 'package-menu-mode-hook #'stripe-listify-buffer)
(add-hook 'org-agenda-mode-hook #'stripe-listify-buffer)

;; `page-break-lines'
(require 'page-break-lines)
(global-page-break-lines-mode)

;; `info-colors'
(require 'info-colors)
(add-hook 'Info-selection-hook #'info-colors-fontify-node)

;;; Spellchecking

;; `ispell'
(require 'ispell)
(setq ispell-program-name "hunspell")
(setq ispell-really-hunspell t)
(setq ispell-dictionary "en_US,lt")
(setq ispell-silently-savep t)
(ispell-set-spellchecker-params)
(ispell-hunspell-add-multi-dic "en_US,lt")
(add-to-list 'ispell-skip-region-alist
             '("^-----BEGIN PGP MESSAGE-----$" . "^-----END PGP MESSAGE-----$"))
;; Workaround header line covering the ispell choices window
(setq ispell-choices-win-default-height 3)

;; `flyspell'
;; TODO(laurynas): slow scrolling in `org-mode' buffers. Profiler shows
;; significant CPU spent in `flyspell-post-command-hook'.
(require 'flyspell)

(setq flyspell-issue-message-flag nil)

;; Pixel height is 20, compatible with `flycheck-status-emoji-mode' settings.
(setq flyspell-mode-line-string " ✏️")

(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'turn-on-flyspell)

;; `goto-address-mode' integration with `flyspell': do not create `flyspell'
;; overlays, if a `goto-address' one already exists at the location. Otherwise
;; a mouse click would offer spelling corrections instead of going to the URL.

(defun dotfiles--goto-address-overlay-p (o)
  "Return t if O is an overlay used by `goto-address'."
  (declare (ftype (function (t) boolean))
           (important-return-value t)
           (side-effect-free 'error-free))
  (and (overlayp o) (overlay-get o 'goto-address)))

(defun dotfiles--no-flyspell-overlay-on-goto-address (beg _end _face
                                                          _mouse-face)
  "Do not create a `flyspell' overlay if a `goto-address' one exists at BEG."
  (declare (ftype (function (integer-or-marker t t t) boolean))
           (important-return-value t)
           (side-effect-free t))
  (seq-every-p #'null (mapcar #'dotfiles--goto-address-overlay-p (overlays-at
                                                                  beg))))

(advice-add #'make-flyspell-overlay :before-while
            #'dotfiles--no-flyspell-overlay-on-goto-address)

;;; History
(setq history-length t
      history-delete-duplicates t)

;; History persistence
(require 'savehist)
(setq savehist-additional-variables '(search-ring regexp-search-ring))
(savehist-mode 1)

;;; `bookmark'
(require 'bookmark)
;; Save bookmarks automatically
(setq bookmark-save-flag 1)

;;; Help

;; `which-key'
(require 'which-key)
(which-key-mode)

;;; UI

(load-theme 'solarized-dark t)

;; Fix modeline by showing its lower line below the whole letter height
(setq x-underline-at-descent-line t)

(setq use-dialog-box nil)

;; Use specified font if any
(when (boundp 'my-frame-font)
  (add-to-list 'default-frame-alist `(font . ,my-frame-font))
  (add-to-list 'initial-frame-alist `(font . ,my-frame-font)))

;;; modeline
(size-indication-mode)
(column-number-mode t)

;; `rich-minority'
(require 'rich-minority)
(setq rm-blacklist '(" company" " waka" " Undo-Tree" " =>" " GitGutter" " WS"
                     " ElDoc" " Wrap" " Fill" " all-the-icons-dired-mode"
                     " Projectile" " PgLn" " h-i-g" " mc++fl" " yas" " WK"
                     " GCMH" " (*)" " ColorIds" " be" " Fly" " ARev"
                     " tree-sitter" " Abbrev" " org-roam-ui" " TblHeader"
                     " Habit" " :ARCHIVE:" " activity-watch" " Async"))
(rich-minority-mode)

;;; Misc settings

(setq read-process-output-max (* 1024 1024)
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

;; I used to remove Git from `vc-handled-backends', but 29.1 package
;; installation from VC feature expects it.

(defun dotfiles--turn-off-size-indication-mode ()
  "Turn off function `size-indication-mode' unconditionally."
  (size-indication-mode -1))

(add-hook 'magit-status-mode-hook #'dotfiles--turn-off-size-indication-mode)

(defun dotfiles--read-non-existing-branch-name (prompt)
  "Read a non-existing branch with PROMPT, stolen from `magit-branch-read-args'."
  (let ((branch (magit-read-string-ns (concat prompt " named"))))
    (if (magit-branch-p branch)
        (dotfiles--read-non-existing-branch-name
         (format "Branch `%s' already exists; pick another name" branch)))
    branch))

(defun dotfiles--magit-worktree-branch ()
  "Create a new git branch and its worktree in a sibling dir, return its path."
  (declare (ftype (function () string))
           (important-return-value t))
  (let* ((name (dotfiles--read-non-existing-branch-name
                "Create branch and worktree"))
         (absolute-path (file-truename (concat "../" name))))
    (magit-worktree-branch absolute-path name (magit-get-current-branch))
    absolute-path))

(require 'projectile)

(defun my-magit-worktree-branch ()
  "Branch a new project."
  (interactive)
  (org-autotask-require-org-clock)
  (let ((path (dotfiles--magit-worktree-branch)))
    (projectile-add-known-project path)
    ;; TODO(laurynas): make `projectile' initialize its file cache. My previous
    ;; attempt by (projectile-project-files path) initialized the cache, but the
    ;; next access would initialize it again.
    ;; TODO(laurynas): install https://github.com/magit/orgit, add a property
    ;; with link to the magit-status buffer of this project to the currently
    ;; clocked-in task. Set up that clocking-in with this property existing goes
    ;; to that link, similar to existing =URL= handling.
    ))

(transient-append-suffix 'magit-worktree '(0 0 -1)
  '("C" "Create a branch in a sibling worktree" my-magit-worktree-branch))

;; `magit-todos'
(require 'magit-todos)
(setq magit-todos-group-by '(magit-todos-item-suffix magit-todos-item-keyword
                                                     magit-todos-item-filename))
(magit-todos-mode 1)

;; `git-gutter'
(require 'git-gutter)
(setq git-gutter:update-interval 0.02)

;; `git-gutter-fringe'
;; TODO(laurynas): the fringe markings get stuck sometimes
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

;; `difftastic'
(require 'difftastic)
(require 'magit-diff)

(defun my--difftastic-window-width ()
  "Return the target window width for `difftastic'."
  (declare (ftype (function () integer))
           (important-return-value t)
           (side-effect-free 'error-free))
  180)

(setq difftastic-requested-window-width-function #'my--difftastic-window-width)

(transient-append-suffix 'magit-diff '(-1 -1)
  [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
   ("S" "Difftastic show" difftastic-magit-show)])

(defun my--magit-blame-difftastic ()
  "Integrate `difftastic' with `magit-blame'."
  (keymap-set magit-blame-read-only-mode-map "D" #'difftastic-magit-show)
  (keymap-set magit-blame-read-only-mode-map "S" #'difftastic-magit-show))

(add-hook 'magit-blame-read-only-mode-hook #'my--magit-blame-difftastic)

;;; Structured format file editing

;; XML
(require 'nxml-mode)
(setq nxml-slash-auto-complete-flag t)  ;; Autocomplete closing tags

;; SSH configuration
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
(setq flycheck-global-modes '(not org-mode org-agenda-mode vterm-mode erc-mode))
(setq-default flycheck-disabled-checkers
              '(c/c++-clang c/c++-gcc c/c++-cppcheck textlint))
(setq flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch
                                                 new-line mode-enabled))
(setq flycheck-buffer-switch-check-intermediate-buffers t)

(global-flycheck-mode)
(setq flycheck-emacs-lisp-load-path 'inherit)

;; `flycheck-status-emoji-mode'
(require 'flycheck-status-emoji)
;; Since I spent way too much time measuring, here's the height in pixels
;; (`line-pixel-height') for some symbols in my font. Note that combined symbols
;; such as yellow warning triangle are not usable in `flycheck-status-emoji'.
;; "running": ⧗: 17, ⌛: 20
;; "finished ok": ✔: 16, ✓: 14, ✅: 20
;; "finished error": Ⓧ: 16, ⦸: 17, ✖: 16, ❌: 20
;; "finished warning": ⚠: 15, ⚠️: 20, ❗: 20, ‼: 14
;; "finished info": ℹ: 17,ℹ️: 20, ⓘ: 16, 🅘: 17, ⒤: 16, 🄘: 17, 💁: 20, 🄸: 17
;; "no checker"/"suspicious": ?: 20, ？: 16, ?: 14, ❓: 20, ❔: 20
;;
;; All of these have height of 20, except for the "finished info" one, which is
;; rarely shown, but makes modeline height jumpy.
(setq flycheck-status-emoji-indicator-running ?⌛
      flycheck-status-emoji-indicator-finished-ok ?✅
      flycheck-status-emoji-indicator-finished-error ?❌
      flycheck-status-emoji-indicator-finished-warning ?❗
      flycheck-status-emoji-indicator-finished-info ?ℹ
      flycheck-status-emoji-indicator-no-checker ?❔
      flycheck-status-emoji-indicator-errored ?❌
      flycheck-status-emoji-indicator-interrupted ?❗
      flycheck-status-emoji-indicator-suspicious ?❔)
(flycheck-status-emoji-mode)

;;;; Programming

;; Compilation
(require 'compile)
(setq compilation-scroll-output 'first-error)
(setq compilation-environment '("LANG=C"))

(require 'fancy-compilation)
(with-eval-after-load 'compile (fancy-compilation-mode))

;; Grand Unified Debugger
(gud-tooltip-mode t)

;; `xref'
(setq xref-prompt-for-identifier nil
      ;; If only one reference, jump to it directly instead of creating the
      ;; reference buffer
      xref-show-xrefs-function #'xref-show-definitions-buffer)
;; Do not initialize `xref-backend-functions' to `etags--xref-backend',
;; we never use etags and let major modes (LSP, elisp) define useful backends
;; themselves.
(remove-hook 'xref-backend-functions #'etags--xref-backend)

;;; tree-sitter
;; TODO(laurynas): add more supported languages
;; TODO(laurynas): if adding org support, confirm there is no large performance
;; penalty on large buffers, like it was with the external tree-sitter.
(require 'treesit)

;; ~10x more than the largest source code buffer size
(setq treesit-max-buffer-size (* 80 1024 1024))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c/")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp/")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (make "https://github.com/tree-sitter-grammars/tree-sitter-make")
        (perl "https://github.com/ganezdragon/tree-sitter-perl")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

;; Install any missing tree-sitter language grammars
(without-remote-files
  (dolist (lang-source treesit-language-source-alist)
    (let ((lang (car lang-source)))
      (unless (treesit-language-available-p lang)
        (message "Installing grammar for %s" lang)
        (treesit-install-language-grammar lang)))))

(defun dotfiles--tree-sitter-error-language-name (error-entry)
  "Extract language name from tree-sitter ERROR-ENTRY.
ERROR-ENTRY is a cons cell of (LANG . ERROR-MESSAGE-STRING)."
  (declare (side-effect-free 'error-free))
  (symbol-name (car error-entry)))

(defun my-update-tree-sitter-grammars ()
  "Update all tree-sitter grammars defined in `treesit-language-source-alist'.

It reinstalls each grammar by cloning its repository and recompiling, regardless
of the current version. This operation may take several minutes.

Errors for individual grammars are caught and reported at the end,
allowing the update process to continue for other grammars.

When called interactively, displays progress and final result messages
in the echo area showing the number of successful and failed updates."
  (interactive)
  (let ((langs (mapcar #'car treesit-language-source-alist))
        (errors '()))
    (message "Updating %d tree-sitter grammars..." (length langs))
    (dolist (lang langs)
      (condition-case err
          (treesit-install-language-grammar lang)
        (error
         (message "ERROR: Failed to update tree-sitter grammar for %s: %s"
                  lang (error-message-string err))
         (push (cons lang (error-message-string err)) errors))))
    (if errors
        (message "Tree-sitter: Updated %d, %d failed (%s)"
                 (- (length langs) (length errors))
                 (length errors)
                 (mapconcat #'dotfiles--tree-sitter-error-language-name errors ", "))
      (message "Tree-sitter: Updated %d grammars" (length langs)))))

(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(makefile-mode . makefile-ts-mode))
(add-to-list 'major-mode-remap-alist '(perl-mode . perl-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

;; Ideally this should come earlier but need to have `major-mode-remap-alist'
;; configured first.
(require 'my-edit)

;;; Elisp

(defun my-eval-buf-and-run-ert-test-at-point ()
  "Evaluate the current buffer and run the ERT test at point."
  (interactive)
  (org-autotask-require-org-clock)
  (save-excursion
    (beginning-of-defun)
    (unless (looking-at "(ert-deftest\\s-+")
      (user-error "Not at an ERT test"))
    (goto-char (match-end 0))
    (let ((test-name (thing-at-point 'symbol)))
      (unless test-name
        (user-error "Couldn't get ERT test name"))
      (eval-buffer)
      (ert-run-tests-interactively test-name))))

(define-key emacs-lisp-mode-map (kbd "C-c C-t")
            #'my-eval-buf-and-run-ert-test-at-point)

;;; C and C++ programming

(require 'c-ts-mode)
;; Not an Emacs developer. If I ever become one, use project- or directory-local
;; variables to set it.
(setq c-ts-mode-emacs-sources-support nil)

;; For the life of me I can't figure out what is enabling `electric-indent-mode'
;; locally in these buffers, so let's just disable it.
(defun dotfiles--disable-electric-indent-locally ()
  "Disable electric indentation in the current buffer."
  (electric-indent-local-mode -1))
(add-hook 'c-ts-base-mode-hook #'dotfiles--disable-electric-indent-locally)

;; Google C/C++ style for tree-sitter. Stolen from
;; https://github.com/ankurdave/dotfiles/blob/master/.emacs.d/packages.el#L673

(defun ankurdave--treesit-left-child-while (node pred)
  "Return the deepest left child of NODE that satisfies PRED."
  (let ((last nil))
    (while (and node (funcall pred node))
      (setq last node
            node (treesit-node-child-by-field-name node "left")))
    last))

(defun ankurdave--c-ts-mode--deepest-binary-expression-left-shift-operator
    (_n parent &rest _)
  "Anchor to the `<<' operator of the deepest binary expression within PARENT."
  (save-excursion
    (treesit-node-start
     (treesit-node-child-by-field-name
      (ankurdave--treesit-left-child-while
       parent
       (lambda (node)
         (and node
              (string-match-p "binary_expression" (treesit-node-type node))
              (string-match-p "<<" (treesit-node-string
                                    (treesit-node-child-by-field-name
                                     node "operator"))))))
      "operator"))))

(defun ankurdave--c-ts-mode--parent-operator-is-left-shift (node parent _bol
                                                                 &rest _)
  "Check if the PARENT for NODE at BOL operator is <<."
  (and
   (string-match-p "binary_expression" (treesit-node-type parent))
   (string-match-p "<<" (treesit-node-string
                         (treesit-node-child-by-field-name parent "operator")))
   (not (equal
         (treesit-node-start node)
         (ankurdave--c-ts-mode--deepest-binary-expression-left-shift-operator
          node parent)))))

(defun google-c-style-ts-indent-style ()
  "Configure Google C++ style indentation."
  (declare (ftype (function () list))
           (important-return-value t))
  `(
    ;;
    ;; Similar to BSD style, but use `standalone-parent' instead of
    ;; `parent-bol'. This handles cases like the third line below:
    ;;
    ;;   int main(
    ;;       int a) {
    ;;   }    ((node-is "}") standalone-parent 0)
    ((node-is "labeled_statement") standalone-parent c-ts-mode-indent-offset)

    ;; Align function arguments and parameters to the start of the first one,
    ;; offset if standalone. For example:
    ;;
    ;;   int foo(int a,
    ;;           int b) {}
    ;;   int foo(
    ;;       int a, int b) {}
    ((and (match nil "argument_list" nil 1 1)
          (not (node-is ")")))
     parent-bol ,(* c-ts-mode-indent-offset 2))
    ((parent-is "argument_list") (nth-sibling 1) 0)
    ((and (match nil "parameter_list" nil 1 1)
          (not (node-is ")")))
     parent-bol ,(* c-ts-mode-indent-offset 2))

    ;; The ":" in field initializer lists should be offset. For example:
    ;;
    ;;   Foo::Foo(int bar)
    ;;       : bar_(bar) {}
    ((node-is "field_initializer_list")
     standalone-parent ,(* c-ts-mode-indent-offset 2))
    ;; Field initializers should line up, or should be offset if standalone. For
    ;;example:
    ;;
    ;;   Foo::Foo(int bar, int baz) :
    ;;       bar_(bar),
    ;;       baz_(baz) {}
    ((match nil "field_initializer_list" nil 1 1)
     standalone-parent ,(* c-ts-mode-indent-offset 2))
    ((parent-is "field_initializer_list") (nth-sibling 1) 0)

    ;; Class/struct members should be indented one step. Access specifiers
    ;; should be indented half a step. For example:
    ((and (node-is "access_specifier")
          (parent-is "field_declaration_list"))
     standalone-parent ,(/ c-ts-mode-indent-offset 2))
    ((parent-is "field_declaration_list") standalone-parent
     c-ts-mode-indent-offset)

    ;; Indent inside case blocks. For example:
    ;;
    ;;  switch (a) {
    ;;    case 0:
    ;;      123;
    ;;    case 1: {
    ;;      456;
    ;;    }
    ;;    default:
    ;;  }
    ((parent-is "case_statement") standalone-parent c-ts-mode-indent-offset)

    ;; Do not indent preprocessor statements
    ((node-is "preproc") column-0 0)

    ;; Don't indent inside namespaces
    ((n-p-gp nil nil "namespace_definition") grand-parent 0)

    ;; Offset line continuations. For example, indent the second line as follows:
    ;;
    ;;   int64_t foo =
    ;;       bar - baz;
    ;;   foo =
    ;;       bar - baz;
    ((parent-is "init_declarator") parent-bol ,(* c-ts-mode-indent-offset 2))
    ((parent-is "assignment_expression") parent-bol ,(* c-ts-mode-indent-offset 2))
    ((parent-is "conditional_expression") parent 0)

    ;; For the left-shift operator as used with iostreams, line up the operators.
    ;;
    ;;   LOG(INFO) << "hello"
    ;;             << "world" << "foo" << "bar";
    (ankurdave--c-ts-mode--parent-operator-is-left-shift
     ankurdave--c-ts-mode--deepest-binary-expression-left-shift-operator 0)

    ;; Offset children of standalone binary expressions. For example:
    ;;   DCHECK(foo ||
    ;;          bar)
    ;;       << "Failed";
    (ankurdave--c-ts-mode--parent-is-standalone-binary-expression
     parent ,(* c-ts-mode-indent-offset 2))

    ;; Align non-standalone binary expressions to their parent. For example:
    ;;
    ;;   foo + (bar *
    ;;          baz);
    ;;   abc = b + c
    ;;         + d + e;
    ((parent-is "binary_expression") parent 0)

    ((parent-is "labeled_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "compound_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "if_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "for_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "while_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "switch_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "do_statement") standalone-parent c-ts-mode-indent-offset)

    ;; For example, indents the third line as follows:
    ;;   void foo(
    ;;       int bar) {
    ;;     baz();
    ;;   }
    ((or (match nil "compound_statement" nil 1 1)
         (match null "compound_statement"))
     standalone-parent c-ts-mode-indent-offset)
    ,@(alist-get 'common (c-ts-mode--indent-styles 'cpp))))
(setq c-ts-mode-indent-style #'google-c-style-ts-indent-style)

;; TODO(laurynas): restore documentation comment font locking
;; (`c-doc-comment-style' in the non-TS `c-mode'). Check
;; https://github.com/Lindydancer/highlight-doxygen and
;; https://emacs.stackexchange.com/q/78274/16376
;; TODO(laurynas): the equivalent of `c-tab-always-indent' in `c-ts-mode'

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

(require 'my-complete)

(require 'my-lsp)

(require 'my-project)

;;; Features: email
;; `mu4e'
;; Known issues: the headers buffer can get misaligned:
;; https://www.djcbsoftware.nl/code/mu/mu4e/Known-issues.html#Headers_002dbuffer-can-get-mis_002daligned
;; The suggested partial fix in the manual does not appear to work.
;; `mu4e-attachment-dir', `mu4e-contexts', `mu4e-bookmarks', &
;; `mu4e-maildir-shortcuts' are defined elsewhere.
(require 'mu4e)
;; Do not take the whole frame, use the existing window
(add-to-list 'display-buffer-alist
             `(,(regexp-quote mu4e-main-buffer-name)
               display-buffer-same-window))

;; Force mu4e view/article buffers to always split from the headers window,
;; not from whatever window happens to be selected (e.g., after refiling).
;; This fixes window layout issues when navigating between messages with "r".
(defun dotfiles--display-mu4e-view-below-headers (buffer alist)
  "Display BUFFER below the mu4e headers window.
Custom display action function that finds the *mu4e-headers* window
and splits it below, regardless of which window is currently selected.
Returns the new window if successful, nil otherwise."
  (declare (ftype (function (buffer t) (or null window)))
           (important-return-value t))
  (when-let* ((headers-win (get-buffer-window "*mu4e-headers*"))
              (new-win (condition-case nil
                           (split-window-below nil headers-win)
                         (error nil))))
    (set-window-buffer new-win buffer)
    (display-buffer-record-window 'window new-win buffer)
    new-win))

(add-to-list 'display-buffer-alist
             '("\\*mu4e-\\(view\\|article\\)\\*"
               (display-buffer-reuse-window
                dotfiles--display-mu4e-view-below-headers)))

(setq mu4e-mu-binary (executable-find "mu"))
(setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
;; If updating the value below, keep in mind that each update takes about 5
;; minutes.
(setq mu4e-update-interval 3600)
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-confirm-quit nil)
(setq mu4e-context-policy 'ask-if-none)
(setq mu4e-compose-context-policy 'ask)
(setq mu4e-compose-format-flowed t)
;; GMail-specific behavior. Make it per-context if adding a non-GMail acocunt
(setq mu4e-sent-messages-behavior 'delete)
;; Locales on macOS are, uhm, interesting:
;; - System settings -> General -> Language & Region does not propagate to the
;;   POSIX shell variables, and a setting of date format to YYYY-MM-DD there
;;   remains ignored in Emacs.
;; - Out of all POSIX locales available by default at least on macOS 14.4, only
;;   two format the date as YYYY-MM-DD: sv_SE.ISO8859-1 & sv_SE.ISO8859-15. But
;;   these, being Swedish, carry Swedish weekday and month names, which I don't
;;   want to see randomly in other contexts.
;; Thus, override date format to YYYY-MM-DD manually.
(setq mu4e-headers-date-format "%F")
(setq mu4e-headers-fields '((:human-date . 10)
                            (:from . 22)
                            (:thread-subject)))
(setq mu4e-use-fancy-chars t)
(add-hook 'mu4e-thread-mode-hook #'mu4e-thread-fold-apply-all)
(setq mu4e-search-include-related nil)
(setq mu4e-eldoc-support t)

;; TODO(laurynas): iCalendar support
;; (https://www.djcbsoftware.nl/code/mu/mu4e/iCalendar.html)
;; TODO(laurynas): `dired' integration
;; (https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html)

;; Clean up mu4e mode names in the modeline
(defun dotfiles--mu4e-clean-mode-name ()
  "Remove mu4e mode names from the modeline."
  (setq mode-name ""))

(add-hook 'mu4e-main-mode-hook #'dotfiles--mu4e-clean-mode-name)
(add-hook 'mu4e-headers-mode-hook #'dotfiles--mu4e-clean-mode-name)
(add-hook 'mu4e-view-mode-hook #'dotfiles--mu4e-clean-mode-name)

;; `mu4e' automation

(defun dotfiles--subject-matches-rule (rule subject)
  "Check if the email SUBJECT matches the criteria defined in RULE.
RULE is a plist containing either :subject-exact or :subject-match."
  (declare (ftype (function (list string) boolean))
           (important-return-value t)
           (side-effect-free t))
  (let ((subject-exact (plist-get rule :subject-exact))
        (subject-match (plist-get rule :subject-match)))
    (when (and subject-exact subject-match)
      (user-error "Both :subject-exact and :subject-match set in %s, fix your dotfiles--email-automations"
                  rule))
    (or (and (not subject-exact) (not subject-match))
        (and subject-exact (string= subject-exact subject))
        (and subject-match (string-match subject-match subject)))))

(defvar dotfiles--email-automations '()
  "List of email automation rules.
Each rule is a property list with the following properties:
  :sender-exact - Exact sender email address to match
  :subject-match - Regex pattern to match against the email subject
  :subject-exact - Exact subject text to match
  :action-fn - Function to call when the rule matches

Only one of :subject-match or :subject-exact can be specified per rule.
The list is processed in order and stops at the first matching rule.
Having multiple matching automation rules for the same email is an error.")
(defun dotfiles--mu4e-email-automation (msg)
  "Run any matching email automation for a `mu4e' MSG."
  (org-autotask-require-org-clock)
  (let ((sender (car (cdr (car (mu4e-message-field msg :from)))))
        (subject (mu4e-message-field msg :subject))
        (already-matched nil))
    (dolist (rule dotfiles--email-automations)
      (when (and (string= sender (plist-get rule :sender-exact))
                 (dotfiles--subject-matches-rule rule subject))
        (when already-matched
          (user-error "A second rule %s matches on this message, fix your dotfiles--email-automations"
                      rule))
        (setq already-matched t)
        (funcall (plist-get rule :action-fn) msg)))
    (unless already-matched
      (message "No automation rule matched for this message"))))

(add-to-list 'mu4e-view-actions
             '("Execute automation" . dotfiles--mu4e-email-automation) t)

;; Integrate `mu4e' with the rest of Emacs
(setq mail-user-agent 'mu4e-user-agent)
(set-variable 'read-mail-command #'mu4e)

;; `smtpmail'
(require 'smtpmail)

;; `message'
(setq message-kill-buffer-on-exit t)
(setq message-send-mail-function #'message-send-mail-with-sendmail)
(setq message-sendmail-envelope-from 'header)

;; TODO(laurynas): would be nice if the lines wrapped visually not at the window
;; border. See also `mu4e-compose-format-flowed'.
(defun dotfiles--message-mode-hook ()
  "Work with format=flowed in `message-mode'."
  (use-hard-newlines 1 'always)
  (auto-fill-mode -1))
(add-hook 'message-mode-hook #'dotfiles--message-mode-hook)

;; `mml'
(setq mml-attach-file-at-the-end t)

;; `sendmail'
(setq sendmail-program (executable-find "msmtp"))
(setq send-mail-function #'message-send-mail-with-sendmail)

;;; Features: `calculator'
(require 'calculator)
(setq calculator-electric-mode t)

;;; Features: calendar
;; `calendar'
(require 'calendar)
(require 'solar)
(setq calendar-week-start-day 1)
(setq calendar-date-style 'iso)
;; Vilnius!
(setq calendar-latitude 54.7)
(setq calendar-longitude 25.3)
(setq calendar-location-name "Vilnius, Lithuania")

;;; Utilities

;; Written mostly by ChatGPT
(defun my-find-duplicate-packages ()
  "Find and list duplicate package installations."
  (interactive)
  (let ((seen-packages (make-hash-table :test 'equal))
        duplicates)
    (dolist (pkg package-alist)
      (let* ((pkg-name (symbol-name (car pkg)))
             (pkg-desc (car (cdr pkg)))
             (pkg-version (package-version-join (package-desc-version pkg-desc)))
             (existing (gethash pkg-name seen-packages)))
        (if existing
            ;; If already seen, add to duplicates if different version
            (unless (member pkg-version existing)
              (push pkg-name duplicates))
          ;; Mark as seen
          (puthash pkg-name (list pkg-version) seen-packages))))
    ;; Display results
    (if duplicates
        (with-current-buffer (get-buffer-create "*Duplicate Packages*")
          (erase-buffer)
          (insert "Duplicate packages found:\n")
          (dolist (dup duplicates)
            (insert (format "%s\n" dup)))
          (display-buffer (current-buffer)))
      (message "No duplicate packages found."))))

(defun my-switch-to-scratch ()
  "Switch the current window to the *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

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

;;; Statistics and internals

;; `keyfreq'
(require 'keyfreq)
(setq keyfreq-file "~/.emacs.d/.emacs.keyfreq")
(setq keyfreq-excluded-commands
      '(self-insert-command next-line lsp-ui-doc--handle-mouse-movement mu4e
                            org-self-insert-command previous-line query-replace
                            magit-next-line gud-tooltip-mouse-motion left-char
                            org-delete-backward-char isearch-printing-char undo
                            magit-previous-line right-char forward-word my-gtd
                            delete-backward-char scroll-up-command magit-commit
                            c-electric-backspace org-agenda-next-line find-file
                            mouse-drag-region mac-mwheel-scroll mouse-set-point
                            org-cycle backward-word save-buffer cycle-spacing
                            isearch-repeat-forward scroll-down-command ignore
                            beginning-of-visual-line windmove-right eval-defun
                            org-agenda-previous-line windmove-left push-button
                            backward-delete-char-untabify org-ctrl-c-ctrl-c
                            org-beginning-of-line delete-forward-char org-refile
                            set-mark-command backward-kill-word magit-discard
                            python-indent-dedent-line-backspace magit-push
                            end-of-visual-line windmove-up kill-buffer
                            markdown-outdent-or-delete newline org-agenda-goto
                            windmove-down org-agenda-redo org-return my-morning
                            xwidget-webkit-scroll-up-line newline-and-indent
                            mu4e-view-mark-for-refile xwidget-webkit-scroll-up
                            mu4e-view-headers-next magit-section-toggle
                            isearch-forward minibuffer-complete my-main-agenda
                            mu4e-headers-next switch-to-buffer transpose-chars
                            yank mu4e-view-marked-execute mu4e-view-action
                            kill-ring-save mu4e-headers-view-message dired-jump
                            magit-stage org-todo goto-line mu4e-view-quit
                            org-agenda-clock-in balance-windows my-secrets
                            isearch-delete-char xref-find-definitions iedit-mode
                            beginend-prog-mode-goto-beginning kill-region
                            lsp-execute-code-action back-to-indentation
                            kill-word org-end-of-line markdown-enter-key
                            deadgrep-toggle-file-results with-indent-finish
                            exit-minibuffer magit-status with-editor-finish
                            beginend-org-mode-goto-beginning flycheck-next-error
                            isearch-forward-thing-at-point kill-visual-line
                            isearch-repeat-backward beginend-prog-mode-goto-end
                            undo-tree-undo org-set-tags-command org-meta-return
                            xref-go-back keyboard-quit execute-extended-command
                            beginend-org-agenda-mode-goto-beginning my-copy-cell
                            deadgrep undo-tree-visualize-undo describe-variable
                            company-select-next-or-abort projectile-find-file
                            org-agenda-do-date-later org-shiftright lsp-rename
                            deadgrep-visit-result-other-window fill-paragraph
                            beginning-of-buffer xwidget-webkit-scroll-down-line
                            lsp-format-buffer mouse-set-region shr-browse-url
                            w3m-view-this-url w3m-previous-anchor
                            mu4e-search-maildir company-complete-selection
                            beginend-magit-status-mode-goto-beginning
                            mu4e~headers-quit-buffer org-shifttab magit-refresh
                            magit-commit-create beginend-org-mode-goto-end
                            dired-next-line projectile-switch-project
                            mu4e-search-rerun minibuffer-keyboard-quit
                            quit-window org-clock-in org-clock-out forward-char
                            dired-previous-line minibuffer-next-completion
                            xwidget-webkit-scroll-down undo-tree-visualize-redo
                            org-clock-goto org-shiftleft org-yank org-kill-line
                            previous-history-element org-mouse-down-mouse
                            describe-function next-line-or-history-element
                            magit-push-current-to-upstream lsp-format-region
                            company-complete-mouse projectile-kill-buffers
                            indent-for-tab-command mu4e-mark-execute-all
                            beginend-org-agenda-mode-goto-end end-of-buffer
                            recenter-top-bottom xref-find-references
                            my-eval-buf-and-run-ert-test-at-point undefined
                            minibuffer-complete-and-exit magit-diff-visit-file
                            projectile-find-other-file electric-pair-delete-pair
                            previous-line-or-history-element org-fill-paragraph
                            company-select-previous-or-abort org-insert-link
                            org-roam-node-insert projectile-switch-to-buffer
                            org-insert-todo-heading-respect-content org-capture
                            minibuffer-previous-completion my-visit-mtr-test
                            org-latex-preview mu4e-update-mail-and-index
                            deadgrep-visit-result xref-show-location-at-point
                            org-agenda-clock-out org-roam-node-find crm-complete
                            mu4e-view-scroll-up-or-next magit-commit-amend
                            minibuffer-complete-word mu4e-view-headers-prev
                            org-archive-subtree goto-address-at-point
                            org-fold-reveal transient-update xref-goto-xref
                            toggle-frame-fullscreen package-menu-mark-upgrades
                            projectile-compile-project kill-whole-line
                            beginend-compilation-mode-goto-beginning ispell-word
                            handle-switch-frame package-menu-execute
                            transient:magit-push:--force-with-lease magit-rebase
                            markdown-electric-backquote message-send-and-exit
                            projectile-configure-project magit-submodule
                            my-switch-to-scratch magit-submodule-populate
                            xref-revert-buffer isearch-abort org-metaright
                            magit-copy-buffer-revision magit-worktree
                            deadgrep-restart image-forward-hscroll my-close-pr
                            capitalize-word downcase-word comment-region
                            doc-view-next-line-or-next-page mark-whole-buffer
                            doc-view-previous-line-or-previous-page my-create-pr
                            my-magit-worktree-branch projectile-test-project
                            beginend-compilation-mode-goto-end left-word
                            indent-rigidly-right magit-rebase-branch
                            markdown-insert-list-item image-backward-hscroll
                            org-decrypt-entry dired-find-file save-some-buffers
                            deadgrep-toggle-all-file-results indent-rigidly
                            save-buffers-kill-terminal indent-rigidly-left
                            view-echo-area-messages transient-quit-one
                            flycheck-list-errors org-set-property delete-window
                            deadgrep-directory magit-push-current-to-pushremote
                            org-agenda-do-date-earlier transpose-lines
                            beginend-message-mode-goto-beginning mml-attach-file
                            mouse-save-then-kill deadgrep-search-term
                            delete-indentation mu4e-view-mark-for-move
                            magit-section-cycle-global compile-goto-error
                            transient-backward-button magit-mode-bury-buffer
                            mu4e-headers-mark-for-refile org-schedule
                            exchange-point-and-mark forward-paragraph
                            backward-paragraph end-of-defun revert-buffer-quick
                            next-history-element reposition-window revert-buffer
                            eval-last-sexp eval-expression org-force-self-insert
                            mu4e-headers-mark-for-read markdown-end-of-line
                            profiler-report-toggle-entry magit-show-commit
                            magit-file-dispatch magit-blame-addition
                            magit-unstage markdown-beginning-of-line
                            magit-blame-quit org-ctrl-c-minus isearch-backward
                            read-char-from-minibuffer-insert-char org-metaleft
                            transient-forward-button magit-kill-this-buffer
                            isearch-yank-kill mu4e-view-go-to-url
                            dired-find-alternate-file org-deadline
                            w3m-next-anchor w3m-view-url-with-browse-url
                            mu4e-compose-wide-reply magit-dired-jump
                            magit-stash-drop gnus-article-press-button
                            magit-bury-or-kill-buffer))
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;; Finish initialization

;; Restore temporarily swapped for startup duration variables
(dolist (handler file-name-handler-alist)
  (add-to-list 'dotfiles--initial-file-name-handler-alist handler))
(setq file-name-handler-alist dotfiles--initial-file-name-handler-alist)

;; GC tuning for interactive use: `gcmh'. The setup used to disable GC on
;; entering the minibuffer and to do GC on frame becoming inactive, but `gcmh'
;; should handle those cases too.
(require 'gcmh)
(gcmh-mode)

(provide 'my-setup)
;;; my-setup.el ends here
