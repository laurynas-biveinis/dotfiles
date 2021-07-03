(define-package "magit" "3.1.1" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (dash "2.18.1")
    (git-commit "3.1.0")
    (magit-section "3.1.0")
    (transient "0.3.6")
    (with-editor "3.0.4"))
  :commit "143d95cced1ee793106d16da3a182dcc2dd01e88" :authors
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
