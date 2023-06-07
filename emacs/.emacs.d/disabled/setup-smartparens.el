;;; setup-smartparens.el --- smartparens config. -*- lexical-binding: t; -*-

;;; Commentary:
;; I tried, but it didn't stick. Performance issues, strict mode getting in
;; the way, etc.

;;; Code:

(require 'smartparens-config)

;; Borrowed from https://ebzzry.io/en/emacs-pairs/
(defmacro dotfiles--def-sp-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (dotfiles--def-sp-pairs ((paren . \"(\")
                           (bracket . \"[\"))

defines the functions WRAP-WITH-PARENS and WRAP-WITH-BRACKETS,
respectively."
  `(progn
     ,@(cl-loop for (key . val) in pairs
                collect
                `(defun ,(read (concat
                                "wrap-with-"
                                (prin1-to-string key)
                                "s"))
                     (&optional _arg)
                   (interactive "p")
                   (sp-wrap-with-pair ,val)))))

(dotfiles--def-sp-pairs ((paren . "(")
                         (bracket . "[")
                         (brace . "{")
                         (single-quote . "'")
                         (double-quote . "\"")
                         (back-quote . "`")))

(define-key smartparens-mode-map (kbd "C-s-a") #'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-s-e") #'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-s-<down>") #'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-s-<up>") #'sp-up-sexp)
(define-key smartparens-mode-map (kbd "M-s-<down>") #'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "M-s-<up>") #'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-f") #'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") #'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") #'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") #'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-s-b") #'sp-backward-symbol)
(define-key smartparens-mode-map (kbd "C-s-f") #'sp-forward-symbol)

(define-key smartparens-mode-map (kbd "C-c (") #'wrap-with-parens)
(define-key smartparens-mode-map (kbd "C-c [") #'wrap-with-brackets)
(define-key smartparens-mode-map (kbd "C-c {") #'wrap-with-braces)

(define-key smartparens-mode-map (kbd "M-[") #'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-]") #'sp-unwrap-sexp)

(define-key smartparens-mode-map (kbd "C-s-<right>") #'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-s-<right>") #'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-s-<left>") #'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-s-<left>") #'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") #'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-s-k") #'sp-backward-kill-sexp)

;; Hybrid sexps

(define-key smartparens-mode-map (kbd "C-M-t") #'sp-push-hybrid-sexp)

;; Choose hybrid or pure sexp function depending on the major mode

(defvar my-pure-sexp-modes '(emacs-lisp-mode) "List of pure sexp modes.")
(defvar my-hybrid-sexp-modes '(c-mode c++-mode) "List of hybrid sexp modes.")

(defun my-transpose-pure-or-hybrid-sexp (&optional arg)
  "Transpose sexp or hybrid sexp depending on the major mode, forwarding ARG."
  (interactive)
  (cond ((seq-some #'derived-mode-p my-pure-sexp-modes) (sp-transpose-sexp arg))
        ((seq-some #'derived-mode-p my-hybrid-sexp-modes)
         (sp-transpose-hybrid-sexp arg))
        (t (progn (message "Major mode `%s' not in `my-pure-sexp-modes' nor
 `my-hybrid-sexp-modes'" major-mode)
                  (sp-transpose-sexp arg)))))

(defun my-slurp-pure-or-hybrid-sexp (&optional arg)
  "Slurp sexp or hybrid sexp depending on the major mode, forwarding ARG."
  (interactive)
  (cond ((seq-some #'derived-mode-p my-pure-sexp-modes) (sp-forward-slurp-sexp
                                                         arg))
        ((seq-some #'derived-mode-p my-hybrid-sexp-modes)
         (sp-slurp-hybrid-sexp))
        (t (progn (message "Major mode `%s' not in `my-pure-sexp-modes' nor
 `my-hybrid-sexp-modes'" major-mode)
                  (sp-forward-slurp-sexp arg)))))

(define-key smartparens-mode-map (kbd "C-s-t")
  #'my-transpose-pure-or-hybrid-sexp)

(define-key smartparens-mode-map (kbd "C-s-<right>")
  #'my-slurp-pure-or-hybrid-sexp)

(smartparens-global-strict-mode 1)
(add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook 'text-mode-hook #'turn-on-smartparens-strict-mode)

(show-smartparens-global-mode 1)

;; Regular not strict smartparens in `diff-mode'
(add-hook 'diff-mode-hook #'turn-off-smartparens-strict-mode)
(add-hook 'diff-mode-hook #'turn-on-smartparens-mode)

;; And in `org-mode'
(add-hook 'org-mode-hook #'turn-off-smartparens-strict-mode)
(add-hook 'org-mode-hook #'turn-on-smartparens-mode)


;; Regular not strict smartparens in minibuffer
(setq sp-ignore-modes-list (delete 'minibuffer-inactive-mode
                                   sp-ignore-modes-list))

;; But not the strict version
(add-hook 'minibuffer-setup-hook #'turn-on-smartparens-mode)

(provide 'setup-smartparens)
;;; setup-smartparens.el ends here
