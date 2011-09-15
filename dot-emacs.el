; Integrated or 3rd party?
(setq integrated-cedet-p (or (and (= emacs-major-version 23)
                                  (>= emacs-minor-version 2))
                             (>= emacs-major-version 24)
                             )
)

; Various paths
(setq home-dir (replace-regexp-in-string "\\\\" "/" (getenv "HOME")))
(setq private-elisp
      (concat home-dir "/emacs/"))
(setq private-elisp-lib (concat private-elisp "lib/"))
(setq xref-dir (concat home-dir "/opt/xref/"))
(setq xref-lib (concat xref-dir "emacs/"))
(unless integrated-cedet-p
  (progn
    (setq cedet-lib (concat private-elisp-lib "cedet/common/cedet.el"))
    (setq cedet-info-dir (concat private-elisp-lib "cedet-info"))))
(setq elib-dir (concat private-elisp-lib "elib-1.0"))
(setq jdee-dir (concat private-elisp-lib "jdee/lisp"))
(setq auctex-dir (concat private-elisp-lib "auctex-11.86/"))
(setq auctex-lisp-dir (concat auctex-dir "lisp/"))
(setq auctex-info-dir (concat auctex-dir "info/"))
(setq org-mode-root (concat private-elisp-lib "org/"))
(setq cc-mode-root (concat private-elisp-lib "cc-mode/"))

(setq elpa-dir (concat private-elisp "elpa/"))

; Setup elisp search directories
(defun add-to-load-path (new)
  (setq load-path
        (cons new load-path)))

(add-to-load-path private-elisp)

; Load system-specific library and setup system-specific things that
; must be setup before main setup
(cond ((eq system-type 'windows-nt) (load-library "ntemacs-cygwin"))
      ((eq system-type 'gnu/linux) (load-library "linux"))
      (t (load-library "default")))

(system-specific-presetup)

(add-to-load-path private-elisp-lib)
(add-to-load-path xref-lib)
(add-to-load-path elib-dir)
(add-to-load-path jdee-dir)
(add-to-load-path auctex-lisp-dir)
(add-to-load-path (concat org-mode-root "lisp/"))
(add-to-load-path (concat org-mode-root "contrib/lisp/"))
(add-to-load-path (concat cc-mode-root "lisp/"))

; Setup info search directories
(defun add-to-info-path (new)
  (add-to-list 'Info-default-directory-list new))

(add-to-info-path auctex-info-dir)
(unless integrated-cedet-p
  (add-to-info-path cedet-info-dir))
(add-to-info-path (concat org-mode-root "info/"))
(add-to-info-path (concat cc-mode-root "info/"))

(load "defaults")
(load "default-modes")
(load "locate-tools")

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
