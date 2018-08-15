;; Specific setup for NTEmacs + EmacsW32 with Cygwin

(defun system-specific-presetup()
  "NT Emacs on Cygwin: set some things before main .emacs setup"

  ;; Setup paths
  ;; EmacsW32
  (setq emacsw32-root (concat private-elisp-lib "EmacsW32"))
  (add-to-list 'load-path emacsw32-root)

  (setq cygwin-root "c:/cygwin/")
  (setq private-bin (concat home-dir "/usr/bin"))
  (setq exec-path (cons private-bin exec-path))
  (setenv "PATH" (concat private-bin ";" (getenv "PATH")))
  ; Add Cygwin Emacs stuff
  (add-to-list 'load-path "/usr/share/emacs/site-lisp")
  ; Add Cygwin Info pages
  (add-to-list 'Info-default-directory-list
               (concat cygwin-root "usr/share/info/"))

  ;; Setup shell
  (setq shell-file-name "bash")
  (setenv "SHELL" shell-file-name)
  (setq explicit-shell-file-name shell-file-name)

  ;; Use Cygwin inferior shell
  (setq w32shell-cygwin-bin "c:\\cygwin\\bin")
  (require 'w32shell)
  (w32shell-add-emacs)
  (w32shell-set-shell "cygwin")

  (defun my-shell-setup()
    "For Cygwin bash"
    (setq comint-scroll-show-maximum-output 'this)
    (setq comint-completion-addsuffix t)
    (setq comint-eol-on-send t)
    (setq w32-quote-process-args ?\")
    (make-variable-buffer-local 'comint-completion-addsuffix))

  (setq shell-mode-hook 'my-shell-setup)
)

(defun system-specific-setup()
  "NT Emacs on Cygwin .emacs specifics"

  ;; Path to Python
  (setq python-python-command "c:\\Python25\\python.exe")

  ;; AUCTeX should be loaded manually
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  (require 'tex-mik)

  (require 'cygwin-mount)
  (cygwin-mount-activate)

  ;; EmacsW32
  '(emacsw32-mode t)
  '(emacsw32-style-frame-title t)
  '(menuacc-active t nil (menuacc)))
