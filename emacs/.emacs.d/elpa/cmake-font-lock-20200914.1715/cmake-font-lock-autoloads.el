;;; cmake-font-lock-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cmake-font-lock" "cmake-font-lock.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from cmake-font-lock.el

(defvar cmake-font-lock-modes '(cmake-mode) "\
List of major modes this package should be activated for.

Set this to nil to disable automatic activation.")

(autoload 'cmake-font-lock-activate "cmake-font-lock" "\
Activate advanced CMake colorization.

To activate this every time a CMake file is opened, use the following:

    (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)" t nil)

(add-hook 'change-major-mode-after-body-hook (lambda nil (when (apply #'derived-mode-p cmake-font-lock-modes) (cmake-font-lock-activate))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cmake-font-lock" '("cmake-font-lock-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cmake-font-lock-autoloads.el ends here
