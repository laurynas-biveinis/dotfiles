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

;;; Encodings

(set-language-environment "UTF-8")

(require 'my-edit)

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
(setq show-paren-predicate '(not (derived-mode . authinfo-mode)))
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

;; `highlight-indent-guides'
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-responsive 'stack)
(setq highlight-indent-guides-delay 0)
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)

;; `info-colors'
(require 'info-colors)
(add-hook 'Info-selection-hook #'info-colors-fontify-node)

;;; Spellchecking

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

;; rich-minority-mode
(require 'rich-minority)
(setq rm-blacklist '(" company" " waka" " Undo-Tree" " =>" " GitGutter" " WS"
                     " ElDoc" " Wrap" " Fill" " all-the-icons-dired-mode"
                     " Projectile" " PgLn" " h-i-g" " mc++fl" " yas" " Helm"
                     " WK" " GCMH" " (*)" " ColorIds" " be" " Fly" " ARev"
                     " tree-sitter" " Abbrev" " org-roam-ui" " TblHeader"
                     " Habit" " :ARCHIVE:" " activity-watch" " Async"))
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

(defun dotfiles--read-non-existing-branch-name (prompt)
  "Read a non-existing branch with PROMPT, stolen from `magit-branch-read-args'."
  (let ((branch (magit-read-string-ns (concat prompt " named"))))
    (if (magit-branch-p branch)
        (dotfiles--read-non-existing-branch-name
         (format "Branch `%s' already exists; pick another name" branch)))
    branch))

(defun my-magit-worktree-branch ()
  "Create a new git branch and its worktree in a sibling dir."
  (interactive
   (let* ((name (dotfiles--read-non-existing-branch-name
                 "Create branch and worktree"))
          (absolute-path (file-truename (concat "../" name))))
     (magit-worktree-branch absolute-path name (magit-get-current-branch)))))

(transient-append-suffix 'magit-worktree '(0 0 -1)
  '("C" "Create a branch in a sibling worktree" my-magit-worktree-branch))

;; `forge'
(require 'forge)

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
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c/")
        (c++ "https://github.com/tree-sitter/tree-sitter-cpp/")
        (python "https://github.com/tree-sitter/tree-sitter-python")))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;;; C and C++ programming

(require 'c-ts-mode)
;; Not an Emacs developer. If I ever become one, use project- or directory-local
;; variables to set it.
(setq c-ts-mode-emacs-sources-support nil)

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

;; `flycheck-google-cpplint'
(require 'flycheck-google-cpplint)
;; TODO(laurynas): it can be enabled without LSP as well, but there is no C/C++
;; checker chain in that case.
(defun dotfiles--lsp-flycheck-enable-cpplint ()
  "Enable cpplint for C and C++ buffers under LSP."
  (when (derived-mode-p 'c-mode 'c++-mode)
    (flycheck-add-next-checker 'lsp 'c/c++-googlelint)))

(add-hook 'lsp-after-open-hook #'dotfiles--lsp-flycheck-enable-cpplint)

;;; Rust programming

;; `rust-mode'
(require 'rust-mode)

(defun dotfiles--rust-set-fill-column ()
  "Set the correct `fill-column' for `rust-mode'."
  (dotfiles--set-fill-column 100))

(add-hook 'rust-mode-hook #'dotfiles--rust-set-fill-column)

;; `rustic'
(require 'rustic)

;;; Python programming

;; `elpy'
(require 'elpy)
(setq elpy-modules (delq #'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook #'flycheck-mode)
(setq elpy-rpc-virtualenv-path 'system)
(elpy-enable)

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

;; `plstore'
(require 'plstore)

(defun dotfiles--with-auto-trailing-newline (orig-fun &rest args)
  "Enable `require-final-newline' while calling ORIG-FUN with ARGS."
  (let ((require-final-newline t))
    (apply orig-fun args)))
(advice-add 'plstore-save :around #'dotfiles--with-auto-trailing-newline)

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

(require 'my-lsp)

(require 'my-project)

;;; Features: email
;; `mu4e'
;; `mu4e-attachment-dir', `mu4e-contexts', `mu4e-bookmarks', &
;; `mu4e-maildir-shortcuts' are defined elsewhere.
(require 'mu4e)
;; Do not take the whole frame, use the existing window
(add-to-list 'display-buffer-alist
             `(,(regexp-quote mu4e-main-buffer-name)
               display-buffer-same-window))
(setq mu4e-mu-binary (executable-find "mu"))
(setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
;; Each update takes about 5 minutes, thus doing it every 20 minutes seems
;; reasonable.
(setq mu4e-update-interval 1200)
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-confirm-quit nil)
(setq mu4e-context-policy 'ask)
(setq mu4e-compose-context-policy 'ask)
;; GMail-specific behavior. Make it per-context if adding a non-GMail acocunt
(setq mu4e-sent-messages-behavior 'delete)
;; FIXME(laurynas): why the default of using the locale results in incorrect
;; formatting?
(setq mu4e-headers-date-format "%F")
(setq mu4e-headers-fields '((:human-date . 10)
                            (:from . 22)
                            (:thread-subject)))
;; TODO(laurynas): solve misalignment issues with the SFMono font and enabled
;; fancy chars
(setq mu4e-use-fancy-chars t)
(add-hook 'mu4e-thread-mode-hook #'mu4e-thread-fold-apply-all)
(setq mu4e-search-include-related nil)
(setq mu4e-eldoc-support t)
;; TODO(laurynas): context switching not fully automatic
;; TODO(laurynas): make window background white for HTML emails to match GMail?
;; TODO(laurynas): iCalendar support
;; (https://www.djcbsoftware.nl/code/mu/mu4e/iCalendar.html)
;; TODO(laurynas): `dired' integration
;; (https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html)
;; TODO(laurynas): remove "mu4e:main", "mu4e:headers", & "mu4e:view" from the
;; modeline. Simply adding these string to `rich-minority' settings does not
;; appear to work. Maybe because these are major modes.

;; Integrate `mu4e' with the rest of Emacs
(setq mail-user-agent 'mu4e-user-agent)
(set-variable 'read-mail-command #'mu4e)

;; `smtpmail'
(require 'smtpmail)

;; `message'
(setq message-kill-buffer-on-exit t)
(setq message-send-mail-function #'message-send-mail-with-sendmail)
(setq message-sendmail-envelope-from 'header)

;; `mml'
;; TODO(laurynas): this one is new in 29.1. Confirm that it has effect in
;; `mu4e'.
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

;; `calfw'
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
      '(self-insert-command next-line lsp-ui-doc--handle-mouse-movement
                            org-self-insert-command previous-line
                            magit-next-line gud-tooltip-mouse-motion left-char
                            org-delete-backward-char isearch-printing-char
                            magit-previous-line right-char forward-word
                            delete-backward-char scroll-up-command
                            c-electric-backspace org-agenda-next-line
                            mouse-drag-region mac-mwheel-scroll mouse-set-point
                            org-cycle helm-next-line backward-word save-buffer
                            isearch-repeat-forward scroll-down-command
                            beginning-of-visual-line windmove-right
                            org-agenda-previous-line windmove-left
                            backward-delete-char-untabify org-ctrl-c-ctrl-c
                            org-beginning-of-line delete-forward-char
                            set-mark-command backward-kill-word
                            python-indent-dedent-line-backspace
                            end-of-visual-line windmove-up kill-buffer
                            markdown-outdent-or-delete newline org-agenda-goto
                            windmove-down org-agenda-redo org-return))
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
