;;; my-lib.el --- helpers for other code.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This contains internal helpers used elsewhere.

;;; Code:

(require 'seq)
(require 'mu4e-mime-parts)

(defun dotfiles--get-mu4e-msg-csv-part ()
  "For a `mu4e' message, get its first .csv attachment part, if any."
  (seq-find (lambda (part)
              (and (plist-member part :filename)
                   (string-suffix-p ".csv" (plist-get part :filename) t)))
            (mu4e-view-mime-parts)))

(defun dotfiles--save-mu4e-msg-part-file (part)
  "For a `mu4e' message PART, save it as a file and return its path."
  (let* ((base-dir (plist-get part :target-dir))
         (file-path (mu4e-join-paths base-dir (plist-get part :filename))))
    (mm-save-part-to-file (plist-get part :handle) file-path)
    file-path))

(provide 'my-lib)
;;; my-lib.el ends here
