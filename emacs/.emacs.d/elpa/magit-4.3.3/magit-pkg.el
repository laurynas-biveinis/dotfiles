;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "4.3.3"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.1.0.0")
    (llama         "0.6.2")
    (magit-section "4.3.3")
    (seq           "2.24")
    (transient     "0.8.8")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "531e7ca6190e71e6d500ddd1a1f6e5cf8402aeca"
  :revdesc "v4.3.3-0-g531e7ca6190e"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
