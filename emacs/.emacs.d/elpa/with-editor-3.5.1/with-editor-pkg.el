;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "with-editor" "3.5.1"
  "Use the Emacsclient as $EDITOR."
  '((emacs    "28.1")
    (compat   "31.0")
    (cond-let "1.1"))
  :url "https://github.com/magit/with-editor"
  :commit "cdf2ac2314042243fae385bb8c2821ec9c3888ae"
  :revdesc "v3.5.1-0-gcdf2ac231404"
  :keywords '("processes" "terminals")
  :authors '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev")))
