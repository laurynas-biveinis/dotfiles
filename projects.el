; MySQL work setup

(defconst mysql-git-path "~/percona/mysql-server"
  "Default path to MySQL git checkout.")

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

(defun switch-mysql-build (mysql-build-dir-arg)
  "Switch MySQL build directory to MYSQL-BUILD-DIR-ARG."
  (interactive "DSwitch to MySQL build directory: ")
  (require 'irony)
  (setq mysql-build-dir (file-name-as-directory mysql-build-dir-arg))
  (let ((mysql-build-dir-cdb (expand-file-name cdb-json-fn mysql-build-dir)))
    (irony-cdb-json-add-compile-commands-path
     mysql-git-path mysql-build-dir-cdb)))

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
