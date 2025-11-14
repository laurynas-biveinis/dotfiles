;;; mail.el --- Emacs setup for email.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Emacs setup for email

;;; Code:

(require 'my-lib)

;; mu4e itself is installed through Homebrew
(dotfiles--ensure-optional-packages '(w3m))

(provide 'mail)
;;; mail.el ends here
