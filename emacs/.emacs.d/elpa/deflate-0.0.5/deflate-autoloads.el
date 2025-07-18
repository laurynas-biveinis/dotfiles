;;; deflate-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from deflate.el

(autoload 'deflate-compress "deflate" "\
Compress DATA using the DEFLATE algorithm.
DATA should be a string or a vector of bytes.
Returns a vector of compressed bytes.
COMPRESSION-TYPE is one of the following:
  `'dynamic' (default) - Use dynamic Huffman coding
  `'static' - Use static Huffman coding
  `'none' - Store without compression.
If FINAL is non-nil (default) it produces a final block (BFINAL=1).

(fn DATA &optional COMPRESSION-TYPE FINAL)")
(register-definition-prefixes "deflate" '("deflate-"))

;;; End of scraped data

(provide 'deflate-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; deflate-autoloads.el ends here
