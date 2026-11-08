;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "with-editor" "3.6.0"
  "Use the Emacsclient as $EDITOR."
  '((emacs    "28.1")
    (compat   "31.1")
    (cond-let "1.1")
    (llama    "1.0"))
  :url "https://github.com/magit/with-editor"
  :commit "3218d480d2c2258835c57f1200afead7f143f3ad"
  :revdesc "v3.6.0-0-g3218d480d2c2"
  :keywords '("processes" "terminals")
  :authors '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev")))
