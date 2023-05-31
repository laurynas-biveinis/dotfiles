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

;; Thanks to Alexey Kopytov
(defun dotfiles--sql-mode-hook ()
  (define-key sql-mode-map (kbd "RET") #'newline-and-indent)
  ;; Make # start a new line comment in SQL. This is a MySQL-specific
  ;; syntax.
  (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
  (set-syntax-table sql-mode-syntax-table))

(add-hook 'sql-mode-hook #'dotfiles--sql-mode-hook)
