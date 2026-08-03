;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "4.7.0"
  "A Git porcelain inside Emacs."
  '((emacs         "28.1")
    (compat        "31.0")
    (cond-let      "1.1")
    (llama         "1.0")
    (magit-section "4.7")
    (seq           "2.24")
    (transient     "0.13")
    (with-editor   "3.5"))
  :url "https://github.com/magit/magit"
  :commit "67f203853e74e926e2c99f60ed508840714f7ced"
  :revdesc "v4.7.0-0-g67f203853e74"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
