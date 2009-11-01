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
(setq elib-dir (concat private-elisp-lib "elib-1.0"))
(setq jdee-dir (concat private-elisp-lib "jdee/lisp"))

(setq elpa-dir (concat private-elisp "elpa/"))

; Setup elisp search directories
(defun add-to-load-path (new)
  (setq load-path
	(cons new load-path)))

(add-to-load-path private-elisp)
(add-to-load-path private-elisp-lib)
(add-to-load-path private-x-symbol-lisp-dir)
(add-to-load-path xref-lib)
(add-to-load-path elib-dir)
(add-to-load-path jdee-dir)


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
(system-specific-setup)
(load "projects")
(load "load-desktop")
