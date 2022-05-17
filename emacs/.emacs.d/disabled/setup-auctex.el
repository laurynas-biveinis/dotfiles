;;; setup-auctex.el --- AUCTeX config.  -*- lexical-binding: t; -*-
;;; Commentary:
;; AUCteX configuration
;;; Code:

(defvar reftex-plug-into-AUCTeX)

;;;; AUCTeX and other TeX stuff

;; Enable document parsing for AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Make AUCTeX aware of multifile documents
(setq-default TeX-master nil)

(defun dotfiles--latex-mode-hook ()
  "My configuration hook for 'latex-mode'."
  (LaTeX-install-toolbar)
  (turn-on-reftex)
  ;; Source specials
  (TeX-source-correlate-mode 1)
  ;; Set up -pdf option for latexmk
  (push
   '("%(-PDF)"
     (lambda ()
       (if (and
            (eq TeX-engine 'default)
            (or TeX-PDF-mode TeX-DVI-via-PDFTeX))
           "-pdf" "")))
   TeX-expand-list)
  ;; Use latexmk
  (push
   '("latexmk" "latexmk %(-PDF) %s" TeX-run-TeX nil t
     :help "Run latexmk on file")
   TeX-command-list))

(add-hook 'LaTeX-mode-hook #'dotfiles--latex-mode-hook)
(setq TeX-source-correlate-start-server t)

;; Integrate RefTeX into AUCTeX
(setq reftex-plug-into-AUCTeX t)

(provide 'setup-auctex)
;;; setup-auctex.el ends here
