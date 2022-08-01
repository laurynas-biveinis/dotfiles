;;; fancy-compilation-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fancy-compilation" "fancy-compilation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from fancy-compilation.el

(defvar fancy-compilation-mode nil "\
Non-nil if Fancy-Compilation mode is enabled.
See the `fancy-compilation-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `fancy-compilation-mode'.")

(custom-autoload 'fancy-compilation-mode "fancy-compilation" nil)

(autoload 'fancy-compilation-mode "fancy-compilation" "\
Enable enhanced compilation.

This is a minor mode.  If called interactively, toggle the
`Fancy-Compilation mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='fancy-compilation-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "fancy-compilation" '("fancy-compilation-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fancy-compilation-autoloads.el ends here
