;;; bison-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bison-mode" "bison-mode.el" (0 0 0 0))
;;; Generated autoloads from bison-mode.el

(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))

(add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode))

(add-to-list 'auto-mode-alist '("\\.jison\\'" . jison-mode))

(autoload 'bison-mode "bison-mode" "\
Major mode for editing bison/yacc files.

\(fn)" t nil)

(autoload 'jison-mode "bison-mode" "\
Major mode for editing jison files.

\(fn)" t nil)

(autoload 'flex-mode "bison-mode" "\
Major mode for editing flex files. (bison-mode by any other name)

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bison-mode" '("bison-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bison-mode-autoloads.el ends here
