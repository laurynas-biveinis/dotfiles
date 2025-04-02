;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "ghub" "4.3.0"
  "Client libraries for Git forge APIs."
  '((emacs     "29.1")
    (compat    "30.0.2.0")
    (let-alist "1.0.6")
    (llama     "0.6.1")
    (treepy    "0.1.2"))
  :url "https://github.com/magit/ghub"
  :commit "1fbce5379e21565f497c0f59bbe5349773c4be62"
  :revdesc "v4.3.0-0-g1fbce5379e21"
  :keywords '("tools")
  :authors '(("Jonas Bernoulli" . "emacs.ghub@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.ghub@jonas.bernoulli.dev")))
