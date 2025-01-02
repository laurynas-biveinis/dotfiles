;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "0.4.6"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.0.1.0")
    (closql        "2.1.0")
    (dash          "2.19.1")
    (emacsql       "4.1.0")
    (ghub          "4.2.0")
    (let-alist     "1.0.6")
    (magit         "4.2.0")
    (markdown-mode "2.6")
    (seq           "2.24")
    (transient     "0.8.2")
    (yaml          "0.5.5"))
  :url "https://github.com/magit/forge"
  :commit "0c9060626200902f7b0078a85566ef0eea8cc0b6"
  :revdesc "v0.4.6-0-g0c9060626200"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
