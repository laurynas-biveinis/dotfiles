;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "ghub" "4.2.2"
  "Client libraries for Git forge APIs."
  '((emacs     "29.1")
    (compat    "30.0.2.0")
    (let-alist "1.0.6")
    (llama     "0.6.1")
    (treepy    "0.1.2"))
  :url "https://github.com/magit/ghub"
  :commit "af663777c47a3dce64b2144b4409587b35521e47"
  :revdesc "v4.2.2-0-gaf663777c47a"
  :keywords '("tools")
  :authors '(("Jonas Bernoulli" . "emacs.ghub@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.ghub@jonas.bernoulli.dev")))
