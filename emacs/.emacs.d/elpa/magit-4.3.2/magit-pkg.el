;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "4.3.2"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.0.2.0")
    (llama         "0.6.2")
    (magit-section "4.3.2")
    (seq           "2.24")
    (transient     "0.8.7")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "bf58615a033b8c827bf630962531c67539789215"
  :revdesc "v4.3.2-0-gbf58615a033b"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
