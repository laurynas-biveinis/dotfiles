;;; setup-company-box.el --- company-box config.  -*- lexical-binding: t; -*-
;;; Commentary:
;; I would like to use it, but solarized dark theme and company-box are not
;; integrated, resulting in too bright highlights. Maybe I will fix it myself
;; later.
;;; Code:
(require 'company-box)
(require 'all-the-icons)
(setq company-box-icons-alist 'company-box-icons-all-the-icons)

(require 'company)
(add-hook 'company-mode-hook #'company-box-mode)

(provide 'setup-company-box)
;;; setup-company-box.el ends here
