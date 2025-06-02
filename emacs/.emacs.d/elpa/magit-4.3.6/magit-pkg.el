;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "4.3.6"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.1")
    (llama         "0.6.3")
    (magit-section "4.3.6")
    (seq           "2.24")
    (transient     "0.9.0")
    (with-editor   "3.4.4"))
  :url "https://github.com/magit/magit"
  :commit "2f1ff91f128f28aa277e0e060ef44b4be8a989c1"
  :revdesc "v4.3.6-0-g2f1ff91f128f"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
