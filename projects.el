; MySQL work setup
(require 'irony)

(defconst mysql-git-path "~/percona/mysql-server"
  "Default path to MySQL git checkout")

(defconst cdb-json-fn "compile_commands.json"
  "Default file name for compilation database in JSON format")

(defvar mysql-build-dir ""
  "Currently active MySQL build directory")

(defun switch-mysql-build (mysql-build-dir-arg)
  "Switch MySQL build directory"
  (interactive "DSwitch to MySQL build directory: ")
  (setq mysql-build-dir mysql-build-dir-arg)
  (let ((mysql-build-dir-cdb (concat mysql-build-dir cdb-json-fn)))
    (irony-cdb-json-add-compile-commands-path
     mysql-git-path mysql-build-dir-cdb)))

(defun compile-mysql (num-of-workers)
  "Compile preconfigured MySQL in the current MySQL build directory"
  (interactive "nNumber of make workers: ")
  (setq compile-command
        (concat "make -j" (number-to-string num-of-workers) " "))
  (let ((saved-default-directory default-directory))
    (setq default-directory mysql-build-dir)
    (recompile)
    (setq default-directory saved-default-directory)))

(global-set-key [f9] 'compile-mysql)
