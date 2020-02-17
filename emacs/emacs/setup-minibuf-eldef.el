;;; setup-minibuf-eldef.el --- minibuf-eldef config.  -*- lexical-binding: t; -*-
;;; Commentary:
;; minibuf-eldef configuration made redundant by Helm.
;;; Code:

(require 'minibuf-eldef)
(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode)

(provide 'setup-minibuf-eldef)
;;; setup-minibuf-eldef.el ends here
