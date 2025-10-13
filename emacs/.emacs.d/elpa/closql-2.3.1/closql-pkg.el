;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "2.3.1"
  "Store EIEIO objects using EmacSQL."
  '((emacs   "28.1")
    (compat  "30.1")
    (emacsql "4.3"))
  :url "https://github.com/emacscollective/closql"
  :commit "cc291a0d8f527a36315fca4c8e1dd32e81fdf871"
  :revdesc "v2.3.1-0-gcc291a0d8f52"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
