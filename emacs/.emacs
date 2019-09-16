; Some version checks
; 22.1, released on 2007-06-02, is the default on macOS, we don't use that.
; 23.1, released on 2009-07-29, is the default on CentOS 6
; 24.3, released on 2013-03-11, is the default on CentOS 7
; 24.4, released on 2014-10-20, is the default on Debian 8
(defconst emacs-24-4-or-later (or (and (= emacs-major-version 24)
                                   (>= emacs-minor-version 4))
                                  (>= emacs-major-version 25)))
; 24.5, released on 2015-04-10, is the default on Ubuntu 16.04 LTS and Debian 9
(defconst emacs-24-5-or-later (or (and (= emacs-major-version 24)
                                       (>= emacs-minor-version 5))
                                  (>= emacs-major-version 25)))
; 25.2, released on 2017-04-21, is the default on Ubuntu 18.04 LTS
(defconst emacs-25-3-or-later (or (>= emacs-major-version 26)
                                  (and (= emacs-major-version 25)
                                       (>= emacs-minor-version 3))))
; 26.1, released on 2018-05-28, is the default on Debian 10

(defconst integrated-cc-mode-p emacs-24-4-or-later
  "Whether we are using integrated or 3rd party cc-mode.")

; Various paths
; TODO: duplicated with .epm.el
(defconst home-dir (concat (replace-regexp-in-string "\\\\" "/"
                                                 (getenv "HOME")) "/"))
(defconst private-elisp
  (concat home-dir "emacs/"))
(defconst dotfiles-elisp (concat private-elisp "dotfiles/*.el"))
(defconst private-elisp-lib (concat private-elisp "lib/"))
(defconst cc-mode-root (concat private-elisp-lib "cc-mode/"))

; Setup elisp search directories
(add-to-list 'load-path home-dir)
(add-to-list 'load-path private-elisp)

; Load system-specific library and setup system-specific things that
; must be setup before main setup
(cond ((eq system-type 'windows-nt) (load-library "ntemacs-cygwin"))
      ((eq system-type 'gnu/linux) (load-library "linux"))
      ((eq system-type 'darwin) (load-library "darwin"))
      (t (load-library "platform-default")))

(system-specific-presetup)

(add-to-list 'load-path private-elisp-lib)
(unless integrated-cc-mode-p
  (add-to-list 'load-path (concat cc-mode-root "lisp/")))

; Setup info search directories
(unless integrated-cc-mode-p
  (add-to-list 'Info-default-directory-list (concat cc-mode-root "info/")))

(load "secrets")
(load "defaults")

(load "~/.epm.el")

(load "addon-modes")
(load "misc")
(system-specific-setup)

(mapc 'load (file-expand-wildcards dotfiles-elisp))

(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dispwatch org-analyzer undo-tree epm yaml-mode markdown-mode linum-off po-mode ssh ssh-config-mode bison-mode company-irony org-plus-contrib cmake-font-lock cmake-mode google-c-style solarized-theme company-irony-c-headers irony-eldoc flycheck-irony wakatime-mode exec-path-from-shell autopair magit org auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
