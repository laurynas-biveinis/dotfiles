;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "0.4.5"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.0.0.0")
    (closql        "2.1.0")
    (dash          "2.19.1")
    (emacsql       "4.1.0")
    (ghub          "4.2.0")
    (let-alist     "1.0.6")
    (magit         "4.1.3")
    (markdown-mode "2.6")
    (seq           "2.24")
    (transient     "0.8.0")
    (yaml          "0.5.5"))
  :url "https://github.com/magit/forge"
  :commit "b2bf67a8fef501ebe01a19b39fa832482738f2f5"
  :revdesc "v0.4.5-0-gb2bf67a8fef5"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
