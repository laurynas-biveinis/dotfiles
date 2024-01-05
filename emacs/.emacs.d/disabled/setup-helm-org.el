;;; setup-helm-org.el --- `helm-org' config. -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs quit on startup with no notice, no error, no crash, no nothing if
;; `helm-org' is installed!

;;; Code:

(require 'helm-mode)
(require 'helm-org)
(setq helm-org-headings-fontify t)
(setq helm-org-format-outline-path t)

(provide 'setup-helm-org)
;;; setup-helm-org.el ends here
