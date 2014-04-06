; Some version checks
(setq emacs-23-2-or-later (or (and (= emacs-major-version 23)
                                   (>= emacs-minor-version 2))
                              (>= emacs-major-version 24)))
(setq emacs-24-or-later (>= emacs-major-version 24))
; Integrated or 3rd party?
(setq integrated-cedet-p emacs-24-or-later)

; Various paths
(setq home-dir (concat (replace-regexp-in-string "\\\\" "/"
                                                 (getenv "HOME")) "/"))
(setq private-elisp
      (concat home-dir "emacs/"))
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
(setq dvc-mode-root (concat private-elisp-lib "dvc/"))
(unless emacs-24-or-later
  (setq color-theme-dir (concat private-elisp-lib "color-theme-6.6.0/")))
(setq solarized-theme-dir (concat private-elisp-lib
                                  "emacs-color-theme-solarized"))
(setq todochiku-icons-dir (concat private-elisp-lib "todochiku-icons"))
(setq jabber-dir (concat private-elisp-lib "emacs-jabber-0.8.90/"))

(if emacs-24-or-later
    (setq elpa-dir (concat private-elisp "elpa/"))
  (setq elpa-dir (concat private-elisp "elpa-23/")))

(setq erc-log-dir (concat home-dir "erclogs"))

; Setup elisp search directories
(defun add-to-load-path (new)
  (setq load-path
        (cons new load-path)))

(add-to-load-path home-dir)
(add-to-load-path private-elisp)

; Load system-specific library and setup system-specific things that
; must be setup before main setup
(cond ((eq system-type 'windows-nt) (load-library "ntemacs-cygwin"))
      ((eq system-type 'gnu/linux) (load-library "linux"))
      ((eq system-type 'darwin) (load-library "darwin"))
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
(add-to-load-path (concat dvc-mode-root "elisp/"))
(unless emacs-24-or-later
  (add-to-load-path color-theme-dir))
(add-to-load-path solarized-theme-dir)
(add-to-load-path jabber-dir)

; Setup info search directories
(defun add-to-info-path (new)
  (add-to-list 'Info-default-directory-list new))

(add-to-info-path auctex-info-dir)
(unless integrated-cedet-p
  (add-to-info-path cedet-info-dir))
(add-to-info-path (concat org-mode-root "info/"))
(add-to-info-path (concat cc-mode-root "info/"))
(add-to-info-path (concat dvc-mode-root "info/"))

(load "secrets")
(load "defaults")
(load "default-modes")

; ELPA
(setq package-user-dir elpa-dir)
(if emacs-24-or-later
    (require 'package)
  (when
       (load
        (expand-file-name (concat elpa-dir "package.el")))
     (package-initialize)))

(load "addon-modes")
(load "misc")
(system-specific-setup)
(load "projects")
(load "load-desktop")

(server-start)
