;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "0.5.0"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.0.2.0")
    (closql        "2.2.1")
    (emacsql       "4.3.0")
    (ghub          "4.3.0")
    (let-alist     "1.0.6")
    (llama         "0.6.2")
    (magit         "4.3.2")
    (markdown-mode "2.7")
    (seq           "2.24")
    (transient     "0.8.7")
    (yaml          "1.2.0"))
  :url "https://github.com/magit/forge"
  :commit "9db4d386a1ce32b574e413771996d41d9b2407e8"
  :revdesc "v0.5.0-0-g9db4d386a1ce"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
