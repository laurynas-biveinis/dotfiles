(define-package "magit" "20240808.1208" "A Git porcelain inside Emacs."
  '((emacs "26.1")
    (compat "30.0.0.0")
    (dash "20240510")
    (git-commit "20240623")
    (magit-section "20240710")
    (seq "2.24")
    (transient "20240629")
    (with-editor "20240702"))
  :commit "27f81d6e5cdcc7ddfb0d4a33f6566ef98a0faaa5" :authors
  '(("Marius Vollmer" . "marius.vollmer@gmail.com")
    ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainer
  '("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
  :keywords
  '("git" "tools" "vc")
  :url "https://github.com/magit/magit")
;; Local Variables:
;; no-byte-compile: t
;; End:
