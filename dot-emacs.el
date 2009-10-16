; Various paths
(setq home-dir (replace-regexp-in-string "\\\\" "/" (getenv "HOME")))
(setq private-elisp
      (concat home-dir "/emacs/"))
(setq private-elisp-lib (concat private-elisp "lib/"))
(setq private-x-symbol-dir (concat private-elisp-lib "x-symbol-4.51/"))
(setq private-x-symbol-lisp-dir (concat private-x-symbol-dir "lisp/x-symbol/"))
(setq xref-dir (concat home-dir "/opt/xref/"))
(setq xref-lib (concat xref-dir "emacs/"))
(setq cedet-lib (concat private-elisp-lib "cedet/common/cedet.el"))
(setq cedet-info-dir (concat private-elisp-lib "cedet-info"))
(setq jdee-dir (concat private-elisp-lib "jdee/lisp"))
(setq elib-dir "/usr/share/emacs/site-lisp/elib")

(setq elpa-dir (concat private-elisp "elpa/"))

; Setup elisp search directories
(defun add-to-load-path (new)
  (setq load-path
	(cons new load-path)))

(add-to-load-path private-elisp)
(add-to-load-path private-elisp-lib)
(add-to-load-path private-x-symbol-lisp-dir)
(add-to-load-path xref-lib)
(add-to-load-path jdee-dir)
(add-to-load-path elib-dir)


; Load system-specific library and setup system-specific things that
; must be setup before main setup
(cond ((eq system-type 'windows-nt) (load-library "ntemacs-cygwin"))
      ((eq system-type 'gnu/linux) (load-library "linux"))
      (t (load-library "default")))

(system-specific-presetup)

(load "defaults")
(load "default-modes")

; ELPA
(setq package-user-dir elpa-dir)
(when
    (load
     (expand-file-name (concat elpa-dir "package.el")))
  (package-initialize))

(load "addon-modes")
(load "misc")

(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cn" 'next-error)
(global-set-key "\C-cp" 'previous-error)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

; TODO: get rid of it
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(ecb-options-version "2.32")
 '(preview-auto-cache-preamble t)
 '(preview-image-type (quote dvipng))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(x-symbol-image-convert-program "convert")
 '(gud-tooltip-mode t))

(system-specific-setup)

(load "projects")

(load "load-desktop")


