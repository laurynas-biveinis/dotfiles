(define-package "magit" "20240206.2256" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (compat "29.1.4.4")
    (dash "20240103")
    (git-commit "20240123")
    (magit-section "20240114")
    (seq "2.24")
    (transient "20240201")
    (with-editor "20240101"))
  :commit "b68e0a3c3388af8daac662f25ccfd3e980590e12" :authors
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
