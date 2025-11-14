;;; mail.el --- Emacs setup for email.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Emacs setup for email

;;; Code:

(require 'my-lib)

;; mu4e itself is installed through Homebrew
(dotfiles--ensure-optional-packages '(w3m))

;; Use 'w3m not 'gnus-w3m - the latter calls gnus-article-html which requires a
;; Gnus article buffer environment and fails silently in mu4e.
(setq mm-text-html-renderer 'w3m)

;; `w3m'
(require 'w3m)

(setq w3m-default-display-inline-images t)

(require 'gnus-art)

;; Remove "r" Gnus MIME keybinding, which conflicts with
;; `mu4e-view-mark-for-refile' and sometimes takes precedence.
(keymap-unset gnus-mime-button-map "r")

;; Bind "v" to open URLs in external browser. w3m's default "M" key is shadowed
;; by mu4e's `mu4e-view-massage'.
(define-key mu4e-view-mode-map "v" #'w3m-view-url-with-browse-url)

;; `dotfiles--mu4e-allowed-image-email-regexps' is defined elsewhere.
(defun dotfiles--mu4e-block-external-images(&optional _ignore)
  "Decides what external image links to allow.
The decision is made using `dotfiles--mu4e-allowed-image-email-regexps', which
should be a list of regexp strings to check the e-mail addresses for match. In
the case there is no current message but this function is still called, return
decision to block all the images."
  (let ((msg (mu4e-message-at-point t)))
    (if (not msg)
        ".*"
      (let* ((from-email (plist-get (car (mu4e-message-field msg :from)) :email))
             (allow-images (seq-find (lambda (email-regexp)
                                       (string-match email-regexp from-email))
                                     dotfiles--mu4e-allowed-image-email-regexps)))
        (if allow-images nil ".*")))))
(setq gnus-blocked-images #'dotfiles--mu4e-block-external-images)

(provide 'mail)
;;; mail.el ends here
