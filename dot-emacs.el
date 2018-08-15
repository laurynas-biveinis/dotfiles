; Some version checks
(defconst emacs-23-2-or-later (or (and (= emacs-major-version 23)
                                   (>= emacs-minor-version 2))
                              (>= emacs-major-version 24)))
(defconst emacs-24-or-later (>= emacs-major-version 24))
(defconst emacs-24-4-or-later (or (and (= emacs-major-version 24)
                                   (>= emacs-minor-version 4))
                              (>= emacs-major-version 25)))
(defconst emacs-25-3-or-later (or (>= emacs-major-version 26)
                                  (and (= emacs-major-version 25)
                                       (>= emacs-minor-version 3))))

; Integrated or 3rd party?
(defconst integrated-cc-mode-p emacs-24-4-or-later)

; Various paths
(defconst home-dir (concat (replace-regexp-in-string "\\\\" "/"
                                                 (getenv "HOME")) "/"))
(defconst private-elisp
      (concat home-dir "emacs/"))
(defconst private-elisp-lib (concat private-elisp "lib/"))
(defconst elib-dir (concat private-elisp-lib "elib-1.0"))
(defconst jdee-dir (concat private-elisp-lib "jdee/lisp")) ; TODO outdated
(defconst cc-mode-root (concat private-elisp-lib "cc-mode/"))
(defconst dvc-mode-root (concat private-elisp-lib "dvc/"))
(defconst color-theme-dir (concat private-elisp-lib "color-theme-6.6.0/"))
(defconst solarized-theme-dir (concat private-elisp-lib
                                  "emacs-color-theme-solarized"))

(if emacs-24-or-later
    (defconst elpa-dir (concat private-elisp "elpa/"))
  (defconst elpa-dir (concat private-elisp "elpa-23/")))

(defconst erc-log-dir (concat home-dir "erclogs"))

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
(add-to-load-path elib-dir)
(add-to-load-path jdee-dir)
(unless integrated-cc-mode-p
  (add-to-load-path (concat cc-mode-root "lisp/")))
(add-to-load-path (concat dvc-mode-root "elisp/"))
(unless emacs-24-or-later
  (add-to-load-path color-theme-dir))
(add-to-load-path solarized-theme-dir)

; Setup info search directories
(defun add-to-info-path (new)
  (add-to-list 'Info-default-directory-list new))

(unless integrated-cc-mode-p
  (add-to-info-path (concat cc-mode-root "info/")))
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
        (expand-file-name (concat elpa-dir "package.el")))))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("org"          . 20)
        ("melpa-stable" . 15)
        ("melpa"        . 10)))
(package-initialize)

(load "addon-modes")
(load "misc")
(system-specific-setup)
(load "projects")

(six-windows)

(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-irony-c-headers irony-eldoc flycheck-irony wakatime-mode exec-path-from-shell php-mode autopair magit org auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
