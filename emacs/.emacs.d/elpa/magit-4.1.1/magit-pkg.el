;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "4.1.1"
  "A Git porcelain inside Emacs."
  '((emacs         "26.1")
    (compat        "30.0.0.0")
    (dash          "2.19.1")
    (magit-section "4.1.1")
    (seq           "2.24")
    (transient     "0.7.6")
    (with-editor   "3.4.2"))
  :url "https://github.com/magit/magit"
  :commit "93e86ceca7bf5b0ff9023d7f138e5f06990fc276"
  :revdesc "v4.1.1-0-g93e86ceca7bf"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
