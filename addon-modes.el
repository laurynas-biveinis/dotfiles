; Nice unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)

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

;; git mode
;(autoload 'magit-status "magit" nil t)
(require 'egg)

;; ------------------------
;; psvn mode for Subversion
;; ------------------------
(require 'psvn)

; Short status
(setq svn-status-verbose nil)

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

;; X-symbol
(setq x-symbol-data-directory
      (concat private-x-symbol-dir "etc/x-symbol/"))
(load (expand-file-name "auto-autoloads" private-x-symbol-lisp-dir))
(or (fboundp 'custom-add-loads)
    (defun custom-add-loads (symbol list)
      (dolist (load list) (custom-add-load symbol load))))
(load (expand-file-name "custom-load" private-x-symbol-lisp-dir))
(x-symbol-initialize)
(add-to-list 'Info-default-directory-list
	     (concat private-x-symbol-dir "info/"))

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

;; GNU Global
; TODO5: gracefully degrade in case Global is not installed
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (gtags-mode 1)
             )
)

;; ---
;; CEDET
;; ---
(load-file cedet-lib)

;; Additional global include directories
(semantic-add-system-include "/usr/local/include" 'c-mode)
(semantic-add-system-include "/usr/local/include" 'c++-mode)

(global-ede-mode t)
; Use GNU Global
; TODO: gracefully degrade in case Global is not installed
(require 'semanticdb-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)
(semantic-load-enable-excessive-code-helpers)
; TODO: should already be enabled by previous line
(global-semantic-idle-completions-mode)
(global-semantic-tag-folding-mode)
(require 'semantic-ia)
(require 'semantic-gcc)


; Loosely inspired by JetBrains IntelliJ IDEA
(defun my-c-mode-cedet-hook ()
  (local-set-key [(control tab)] 'semantic-ia-complete-symbol)
  (local-set-key (kbd "C-?") 'semantic-ia-complete-symbol-menu)
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key (kbd "C-c q") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-c <f1>") 'semantic-ia-show-doc)
  (local-set-key (kbd "C-c b") 'semantic-complete-jump)
  (local-set-key (kbd "C-c C-b") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c C-i") 'semantic-decoration-include-visit) ; TODO gather all jumps to C-c b
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
(if (file-exists-p xref-dir)
      (progn
	(setq exec-path (cons xref-dir exec-path))
	(load "xrefactory")))

; Monotone
(require 'monotone)
(monotone-set-vc-prefix-key "\C-cm")
(setq monotone-passwd-remember t)

; JDEE
(require 'jde)
(setq jde-jdk-registry
      (quote (("1.6.0.16" . "/usr/lib/jvm/java-6-sun-1.6.0.16"))))
