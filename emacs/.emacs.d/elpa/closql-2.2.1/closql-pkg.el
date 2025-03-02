;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "2.2.1"
  "Store EIEIO objects using EmacSQL."
  '((emacs   "26.1")
    (compat  "30.0.2.0")
    (emacsql "4.2.0"))
  :url "https://github.com/emacscollective/closql"
  :commit "dc7924c1d206483a2555a98470c96fadf419f32d"
  :revdesc "v2.2.1-0-gdc7924c1d206"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
