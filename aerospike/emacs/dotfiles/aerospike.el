;;; aerospike.el --- Emacs settings for Aerospike.  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defconst aerospike-c-style
  '("k&r"
    (indent-tabs-mode . t)
    (c-basic-offset . 4)
    (tab-width . 4)))

(c-add-style "aerospike" aerospike-c-style)

;;; aerospike.el ends here
