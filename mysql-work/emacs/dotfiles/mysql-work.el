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
                                   ))
               ))

(add-to-list 'auto-mode-alist '("\\.ic\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.i\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("storage/innobase/.*\\.h\\'" . c++-mode))

(dir-locals-set-class-variables 'innodb-source-5.7
                                '((c-mode . ((c-file-style . "InnoDB-5.7")))
                                  (c++-mode . ((c-file-style . "InnoDB-5.7")))))
(dir-locals-set-class-variables 'mysql-source-5.7
                                '((c-mode . ((c-file-style . "MySQL-5.7")))
                                  (c++-mode . ((c-file-style . "MySQL-5.7")))))

; sql-mode
(add-to-list 'auto-mode-alist '("\\.test\\'" . sql-mode)) ; MySQL test files

; Thanks to Alexey Kopytov
(add-hook 'sql-mode-hook 'my-sql-mode-hook)
(defun my-sql-mode-hook ()
  (define-key sql-mode-map (kbd "RET") 'newline-and-indent)

  ;; Make # start a new line comment in SQL. This is a MySQL-specific
  ;; syntax.

  (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
  (set-syntax-table sql-mode-syntax-table))

; TODO: provide this as default value to switch-mysql-build
;(defconst mysql-git-path "~/percona/mysql-server"
;  "Default path to MySQL git checkout.")

(defconst cdb-json-fn "compile_commands.json"
  "Default file name for compilation database in JSON format.")

(defconst mysql-mtr-error-log-dir "mysql-test/var"
  "The root of directory containing MySQL error logs after MTR run.")

(defconst mysql-mtr-error-log-pattern "*.err"
  "Shell pattern to match MySQL error logs after MTR run.")

(defconst mysql-crash-marker "UTC - mysqld got signal"
  "A string in MySQL error log indicating a crash.")

(defvar mysql-build-dir ""
  "Currently active MySQL build directory.")

(defun switch-mysql-build (mysql-build-dir-arg mysql-work-dir-arg)
  "Switch MySQL git worktree and build directories.
The build directory is switched to MYSQL-BUILD-DIR-ARG and the git worktree to
MYSQL-WORK-DIR-ARG."
  (interactive "DSwitch to MySQL build directory: \nDFor git worktree at: ")
  (require 'irony)
  (setq mysql-build-dir (file-name-as-directory mysql-build-dir-arg))
  (let ((mysql-build-dir-cdb (expand-file-name cdb-json-fn mysql-build-dir)))
    (irony-cdb-json-add-compile-commands-path
     mysql-work-dir-arg mysql-build-dir-cdb)))

(defun compile-mysql (num-of-workers)
  "Compile preconfigured MySQL in the current MySQL build directory.
Compilation will use NUM-OF-WORKERS parallel workers."
  (interactive "nNumber of make workers: ")
  (require 'compile)
  (setq compile-command
        (concat "make -j" (number-to-string num-of-workers) " "))
  (let ((saved-default-directory default-directory))
    (setq default-directory mysql-build-dir)
    (recompile)
    (setq default-directory saved-default-directory)))

(global-set-key [f9] 'compile-mysql)

(defun find-crash ()
  "Grep current build dir for assertion errors in MTR MySQL error logs."
  (interactive)
  (let ((mysql-error-log-path
         (expand-file-name (concat mysql-build-dir mysql-mtr-error-log-dir))))
    (rgrep mysql-crash-marker mysql-mtr-error-log-pattern mysql-error-log-path
           nil)))
