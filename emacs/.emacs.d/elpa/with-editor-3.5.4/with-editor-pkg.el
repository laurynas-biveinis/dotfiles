;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "with-editor" "3.5.4"
  "Use the Emacsclient as $EDITOR."
  '((emacs    "28.1")
    (compat   "31.0")
    (cond-let "1.1")
    (llama    "1.0"))
  :url "https://github.com/magit/with-editor"
  :commit "5021ef6885381cf5b2852f7a3f67ca8c4be1dca2"
  :revdesc "v3.5.4-0-g5021ef688538"
  :keywords '("processes" "terminals")
  :authors '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev")))
