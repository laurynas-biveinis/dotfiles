;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "0.4.4"
  "Access Git forges from Magit."
  '((emacs         "27.1")
    (compat        "30.0.0.0")
    (closql        "2.0.0")
    (dash          "2.19.1")
    (emacsql       "4.0.3")
    (ghub          "4.1.1")
    (let-alist     "1.0.6")
    (magit         "4.1.1")
    (markdown-mode "2.6")
    (seq           "2.24")
    (transient     "0.7.6")
    (yaml          "0.5.5"))
  :url "https://github.com/magit/forge"
  :commit "a63685b921f25e861bf1e172e46beb771e56b924"
  :revdesc "v0.4.4-0-ga63685b921f2"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
