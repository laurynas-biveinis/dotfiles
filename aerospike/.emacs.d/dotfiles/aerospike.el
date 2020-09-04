;;; aerospike.el --- Emacs settings for Aerospike.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defconst aerospike-c-style
  '("k&r"
    (tab-width . 4)
    (indent-tabs-mode . t)
    (c-basic-offset . 4)
    ;; If this becomes not based on K&R style, remove all the ""(before after)"
    ;; from c-hanging-braces-alist
    (c-hanging-braces-alist . ((defun-open . (before after))
                               (class-open . (brace-entry-open))
                               (substatement-open . (brace-entry-open))
                               (block-close . (before after))))
    (c-offsets-alist . ((topmost-intro-cont . 0)
                        (func-decl-cont . ++)
                        (arglist-cont . ++)
                        (arglist-cont-nonempty . ++)
                        (cpp-macro-cont . ++)
                        (statement-cont . ++)
                        (label . [0])))))

(c-add-style "aerospike" aerospike-c-style)

(require 'magit-status)

(defun aerospike-sibling-magit-status ()
  "Open Magit status for Aerospike server sibling repo."
  (interactive)
  (let* ((cur-dir default-directory)
         (sibling-dir (cond ((string-suffix-p "/ce/" cur-dir)
                             (replace-regexp-in-string "/ce/$" "/ee/" cur-dir))
                            ((string-suffix-p "/ee/" cur-dir)
                             (replace-regexp-in-string "/ee/$" "/ce/" cur-dir))
                            (t nil))))
    (if sibling-dir
        (magit-status-internal sibling-dir)
      (message "Not an Aerospike Server git directory: `%s'" cur-dir))))

(global-set-key (kbd "C-x j") #'aerospike-sibling-magit-status)

;;; aerospike.el ends here
