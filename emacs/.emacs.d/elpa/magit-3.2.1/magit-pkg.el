(define-package "magit" "3.2.1" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (dash "2.18.1")
    (git-commit "3.2.1")
    (magit-section "3.2.1")
    (transient "0.3.6")
    (with-editor "3.0.4"))
  :commit "b70f660e36c024fa9319ea0e2977e45ef3c6f3ac" :authors
  '(("Marius Vollmer" . "marius.vollmer@gmail.com")
    ("Jonas Bernoulli" . "jonas@bernoul.li"))
  :maintainer
  '("Jonas Bernoulli" . "jonas@bernoul.li")
  :keywords
  '("git" "tools" "vc")
  :url "https://github.com/magit/magit")
;; Local Variables:
;; no-byte-compile: t
;; End:
