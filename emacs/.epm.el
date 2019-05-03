; ELPA setup moved to its own file for EPM

; TODO: duplicated with .emacs
(defconst home-dir (concat (replace-regexp-in-string "\\\\" "/"
                                                     (getenv "HOME")) "/"))
(defconst private-elisp
  (concat home-dir "emacs/"))
(defconst elpa-dir (concat private-elisp "elpa/"))

(setq package-user-dir elpa-dir)
(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("org"          . 20)
        ("melpa-stable" . 15)
        ("melpa"        . 10)))
(package-initialize)

