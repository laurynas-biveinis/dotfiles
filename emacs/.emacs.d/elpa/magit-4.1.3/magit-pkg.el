;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "4.1.3"
  "A Git porcelain inside Emacs."
  '((emacs         "26.1")
    (compat        "30.0.0.0")
    (dash          "2.19.1")
    (magit-section "4.1.3")
    (seq           "2.24")
    (transient     "0.8.0")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "7adad8c8d3bd61ae36659638751223cfa2c7d720"
  :revdesc "v4.1.3-0-g7adad8c8d3bd"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
