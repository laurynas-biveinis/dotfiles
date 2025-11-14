;;; wakatime.el --- Emacs setup for wakatime.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Emacs setup specific for Wakatime

;;; Code:

(require 'my-lib)

(dotfiles--ensure-optional-packages '(wakatime-mode))

(require 'wakatime-mode)

(if (not (file-exists-p "~/.wakatime.cfg"))
    (message "WakaTime not enabled: ~/.wakatime.cfg not found")
  (require 'wakatime-mode)
  (global-wakatime-mode))

(provide 'wakatime)
;;; wakatime.el ends here
