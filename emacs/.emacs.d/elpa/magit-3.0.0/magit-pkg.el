(define-package "magit" "3.0.0" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (dash "2.18.1")
    (git-commit "3.0.0")
    (magit-section "3.0.0")
    (transient "0.3.3")
    (with-editor "3.0.4"))
  :commit "c3bbc9b9425f3370690cabb11bd35b9040773fdc" :authors
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
