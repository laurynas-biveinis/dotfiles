;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "2.4.2"
  "Store EIEIO objects using EmacSQL."
  '((emacs    "28.1")
    (compat   "31.1")
    (cond-let "1.1")
    (emacsql  "4.4")
    (llama    "1.0"))
  :url "https://github.com/emacscollective/closql"
  :commit "48955ae02cfc7dad93b6bdc9ffd8d4b760dfb62b"
  :revdesc "v2.4.2-0-g48955ae02cfc"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
