;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "2.2.0"
  "Store EIEIO objects using EmacSQL."
  '((emacs   "26.1")
    (compat  "30.0.0.0")
    (emacsql "4.1.0"))
  :url "https://github.com/emacscollective/closql"
  :commit "d925b8c9c1c724b49d7af7806b7ec1d71209d6e3"
  :revdesc "v2.2.0-0-gd925b8c9c1c7"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
