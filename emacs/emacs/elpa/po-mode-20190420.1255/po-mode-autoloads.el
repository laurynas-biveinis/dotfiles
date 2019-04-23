;;; po-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "po-mode" "po-mode.el" (0 0 0 0))
;;; Generated autoloads from po-mode.el

(autoload 'po-mode "po-mode" "\
Major mode for translators when they edit PO files.

Special commands:
\\{po-mode-map}
Turning on PO mode calls the value of the variable 'po-mode-hook',
if that value is non-nil.  Behaviour may be adjusted through some variables,
all reachable through 'M-x customize', in group 'Emacs.Editing.I18n.Po'.

\(fn)" t nil)
 (add-to-list 'auto-mode-alist '("\\.po[tx]?\\'\\|\\.po\\." . po-mode))
 (modify-coding-system-alist 'file "\\.po[tx]?\\'\\|\\.po\\." 'po-find-file-coding-system)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "po-mode" '("po-" "N_")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; po-mode-autoloads.el ends here
