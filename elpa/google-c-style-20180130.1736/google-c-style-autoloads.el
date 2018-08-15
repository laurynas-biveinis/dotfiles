;;; google-c-style-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "google-c-style" "google-c-style.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from google-c-style.el

(defconst google-c-style `((c-recognize-knr-p) (c-enable-xemacs-performance-kludge-p . t) (c-basic-offset . 2) (indent-tabs-mode) (c-comment-only-line-offset . 0) (c-hanging-braces-alist (defun-open after) (defun-close before after) (class-open after) (class-close before after) (inexpr-class-open after) (inexpr-class-close before) (namespace-open after) (inline-open after) (inline-close before after) (block-open after) (block-close . c-snug-do-while) (extern-lang-open after) (extern-lang-close after) (statement-case-open after) (substatement-open after)) (c-hanging-colons-alist (case-label) (label after) (access-label after) (member-init-intro before) (inher-intro)) (c-hanging-semi&comma-criteria c-semi&comma-no-newlines-for-oneline-inliners c-semi&comma-inside-parenlist c-semi&comma-no-newlines-before-nonblanks) (c-indent-comments-syntactically-p . t) (comment-column . 40) (c-indent-comment-alist (other space . 2)) (c-cleanup-list brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces defun-close-semi list-close-comma scope-operator) (c-offsets-alist (arglist-intro google-c-lineup-expression-plus-4) (func-decl-cont . ++) (member-init-intro . ++) (inher-intro . ++) (comment-intro . 0) (arglist-close . c-lineup-arglist) (topmost-intro . 0) (block-open . 0) (inline-open . 0) (substatement-open . 0) (statement-cont ,(when (fboundp 'c-no-indent-after-java-annotations) 'c-no-indent-after-java-annotations) ,(when (fboundp 'c-lineup-assignments) 'c-lineup-assignments) ++) (label . /) (case-label . +) (statement-case-open . +) (statement-case-intro . +) (access-label . /) (innamespace . 0))) "\
Google C/C++ Programming Style.")

(autoload 'google-set-c-style "google-c-style" "\
Set the current buffer's c-style to Google C/C++ Programming
  Style. Meant to be added to `c-mode-common-hook'.

\(fn)" t nil)

(autoload 'google-make-newline-indent "google-c-style" "\
Sets up preferred newline behavior. Not set by default. Meant
  to be added to `c-mode-common-hook'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "google-c-style" '("google-c-lineup-expression-plus-4")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; google-c-style-autoloads.el ends here
