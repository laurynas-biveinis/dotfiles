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
                        (statement-cont . ++)))))

(c-add-style "aerospike" aerospike-c-style)

;;; aerospike.el ends here
