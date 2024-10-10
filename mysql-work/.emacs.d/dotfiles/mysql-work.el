;;; mysql-work.el --- Emacs helpers for MySQL dev.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Emacs setup specific for MySQL development.

;;; Code:

(require 'projectile)

(add-to-list 'projectile-other-file-alist '("test" "result"))
(add-to-list 'projectile-other-file-alist '("result" "test"))

;; This is a not a MySQL-specific implementation of `projectile-find-test-file'.
;; That function is meant for jumping from a source file to its test file.
;; TODO(laurynas): this is not an exhaustive implementation for all locations
;; test files can reside. Add them as the need arises.
(defun my-visit-mtr-test ()
  "Input MySQL MTR test name and visit it."
  (interactive)
  (let* ((input (read-string "MySQL MTR test name: "))
         ;; TODO(laurynas): error checking
         (parts (split-string input "\\."))
         (suite-name (car parts))
         (test-in-suite (concat "t/" (cadr parts) ".test"))
         (test-root (concat (projectile-project-root) "mysql-test/"))
         (file-path (if (string= suite-name "main")
                        (concat test-root test-in-suite)
                      (concat test-root "suite/" suite-name "/"
                              test-in-suite))))
    (find-file file-path)))

(define-key projectile-command-map "M" #'my-visit-mtr-test)

(c-add-style "MySQL-5.7"
             '("K&R"
               (indent-tabs-mode . nil)
               (c-basic-offset . 2)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist . ((statement-block-intro . +)
                                   (knr-argdecl-intro . 0)
                                   (substatement-open . 0)
                                   (label . -)
                                   (statement-cont . +)
                                   (arglist-intro . c-lineup-arglist-intro-after-paren)
                                   (arglist-close . c-lineup-arglist)
                                   (innamespace . 0)
                                   (inline-open . 0)
                                   (statement-case-open . +)))))

(c-add-style "InnoDB-5.7"
             '("K&R"
               (indent-tabs-mode . t)
               (c-basic-offset . 8) ; InnoDB-specific
               (c-label-minimum-indentation . 0) ; InnoDB-specific
               (c-comment-only-line-offset . 0)
               (c-offsets-alist . ((statement-block-intro . +)
                                   (knr-argdecl-intro . 0)
                                   (substatement-open . 0)
                                   (label . [0]) ; InnoDB-specific
                                   (c . 0) ; InnoDB-specific
                                   (statement-cont . +)
                                   (arglist-intro . +) ; InnoDB-specific
                                   (arglist-close . c-lineup-arglist)
                                   (innamespace . 0)
                                   (inline-open . 0)
                                   (statement-case-open . +)
                                   ))))

(add-to-list 'auto-mode-alist '("\\.ic\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.i\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("storage/innobase/.*\\.h\\'" . c++-mode))

(dir-locals-set-class-variables 'innodb-source-5.7
                                '((c-mode . ((c-file-style . "InnoDB-5.7")))
                                  (c++-mode . ((c-file-style . "InnoDB-5.7")))))
(dir-locals-set-class-variables 'mysql-source-5.7
                                '((c-mode . ((c-file-style . "MySQL-5.7")))
                                  (c++-mode . ((c-file-style . "MySQL-5.7")))))

;; sql-mode
(add-to-list 'auto-mode-alist '("\\.test\\'" . sql-mode)) ; MySQL test files

(require 'sql)

;; Thanks to Alexey Kopytov
(defun dotfiles--sql-mode-hook ()
  "Make # start a new line comment in SQL."
  (define-key sql-mode-map (kbd "RET") #'newline-and-indent)
  (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
  (set-syntax-table sql-mode-syntax-table))

(add-hook 'sql-mode-hook #'dotfiles--sql-mode-hook)

(provide 'mysql-work)
;;; mysql-work.el ends here
