; -*- mode: emacs-lisp -*-
;;
;; Load DVC easily ...
;;
;; Manually, you can run
;;
;;   M-x load-file RET /path/to/dvc-load.el RET
;;
;; (usefull when you want to load DVC after starting "emacs -q"!), or
;; add
;;
;;   (load-file "/path/to/this/file/in/installdir/dvc-load.el")
;;
;; to your ~/.emacs.el

(add-to-list 'load-path "/home/laurynas/emacs/lib/dvc/elisp/")
(add-to-list 'Info-default-directory-list "/home/laurynas/emacs/lib/dvc/info")

(if (featurep 'dvc-core)
    (dvc-reload)
  (if (featurep 'xemacs)
      (require 'auto-autoloads)
    (require 'dvc-autoloads)))

