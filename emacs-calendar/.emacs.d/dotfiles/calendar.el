;;; calendar.el --- Emacs setup for calendars.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Emacs setup specific for calendars

;;; Code:

(require 'my-lib)

;; Dependency on Org
(dotfiles--ensure-optional-packages '(calfw calfw-ical calfw-org org-gcal))

;; `calfw'
;; Workaround warnings stemming from
;; https://github.com/kiwanami/emacs-calfw/issues/101 - "Package `cl' is
;; obsolete."
(with-suppressed-warnings ((obsolete cl)) (require 'calfw))
(require 'calfw-org)
(require 'calfw-ical)

;; Unicode characters
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

(setq cfw:render-line-breaker #'cfw:render-line-breaker-wordwrap)

;;; `org-gcal'

;; Do not require `org-gcal' to avoid the warning about needing to call
;; `org-gcal-reload-client-id-secret'

;; TODO(laurynas): fetch automatically?
;; TODO(laurynas): Automate setting `org-gcal-local-timezone': check
;; (getenv "TZ") first, fallback to parsing (file-symlink-p "/etc/localtime"),
;; and extract the IANA timezone from the zoneinfo path.
;; TODO(laurynas): key binding for `org-gcal-post-at-point'?
;; TODO(laurynas): key binding (and some command) to create a new event? How to
;; check for scheduling conflicts?

(provide 'calendar)
;;; calendar.el ends here
