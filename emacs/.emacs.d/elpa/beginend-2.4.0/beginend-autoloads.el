;;; beginend-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "beginend" "beginend.el" (0 0 0 0))
;;; Generated autoloads from beginend.el

(autoload 'beginend-setup-all "beginend" "\
Use beginend on all compatible modes.
For example, this activates function `beginend-dired-mode' in `dired' and
function `beginend-message-mode' in `message-mode'.  All affected minor
modes are described in `beginend-modes'." nil nil)

(autoload 'beginend-unsetup-all "beginend" "\
Remove beginend from all compatible modes in `beginend-modes'." nil nil)

(defvar beginend-global-mode nil "\
Non-nil if beginend-Global mode is enabled.
See the `beginend-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `beginend-global-mode'.")

(custom-autoload 'beginend-global-mode "beginend" nil)

(autoload 'beginend-global-mode "beginend" "\
Toggle beginend mode.
Interactively with no argument, this command toggles the mode.  A positive
prefix argument enables the mode, any other prefix argument disables it.
From Lisp, argument omitted or nil enables the mode, `toggle' toggles the
state.

This is a minor mode.  If called interactively, toggle the
`beginend-Global mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='beginend-global-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When beginend mode is enabled, modes such as `dired-mode', `message-mode'
and `compilation-mode' will have their \\[beginning-of-buffer] and
\\[end-of-buffer] keys adapted to go to meaningful places.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "beginend" '("beginend-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; beginend-autoloads.el ends here
