;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "4.1.2"
  "A Git porcelain inside Emacs."
  '((emacs         "26.1")
    (compat        "30.0.0.0")
    (dash          "2.19.1")
    (magit-section "4.1.2")
    (seq           "2.24")
    (transient     "0.7.8")
    (with-editor   "3.4.2"))
  :url "https://github.com/magit/magit"
  :commit "4992c3d1f64e0e983692c7a61d47069f47380dbf"
  :revdesc "v4.1.2-0-g4992c3d1f64e"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
