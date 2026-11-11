;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "4.7.2"
  "A Git porcelain inside Emacs."
  '((emacs         "28.1")
    (compat        "31.1")
    (cond-let      "1.1")
    (llama         "1.0")
    (magit-section "4.7")
    (seq           "2.24")
    (transient     "0.13")
    (with-editor   "3.6"))
  :url "https://github.com/magit/magit"
  :commit "602760478f6d4a67e7064f5bcd2e294aa3f38f8e"
  :revdesc "v4.7.2-0-g602760478f6d"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
