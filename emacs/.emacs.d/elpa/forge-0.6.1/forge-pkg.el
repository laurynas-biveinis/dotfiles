;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "0.6.1"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.1")
    (closql        "2.3")
    (cond-let      "0.1")
    (emacsql       "4.3")
    (ghub          "5.0")
    (llama         "1.0")
    (magit         "4.4")
    (markdown-mode "2.7")
    (seq           "2.24")
    (transient     "0.10")
    (yaml          "1.2"))
  :url "https://github.com/magit/forge"
  :commit "34e7b77602b2018f64bd7e38314a0a2cb4a32227"
  :revdesc "v0.6.1-0-g34e7b77602b2"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
