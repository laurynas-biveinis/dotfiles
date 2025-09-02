;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "2.3.0"
  "Store EIEIO objects using EmacSQL."
  '((emacs   "28.1")
    (compat  "30.1")
    (emacsql "4.3"))
  :url "https://github.com/emacscollective/closql"
  :commit "80e91c557331225b5c4b420ec6f283ac3c40bbac"
  :revdesc "v2.3.0-0-g80e91c557331"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
