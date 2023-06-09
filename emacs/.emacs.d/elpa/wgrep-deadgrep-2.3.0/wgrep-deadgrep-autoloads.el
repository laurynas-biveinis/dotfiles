;;; wgrep-deadgrep-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wgrep-deadgrep" "wgrep-deadgrep.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from wgrep-deadgrep.el

(autoload 'wgrep-deadgrep-setup "wgrep-deadgrep" "\
Setup `wgrep-deadgrep' for `deadgrep'." nil nil)

(add-hook 'deadgrep-finished-hook 'wgrep-deadgrep-setup)

(register-definition-prefixes "wgrep-deadgrep" '("wgrep-deadgrep-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wgrep-deadgrep-autoloads.el ends here
