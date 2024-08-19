(define-package "magit" "4.0.0" "A Git porcelain inside Emacs."
  '((emacs "26.1")
    (compat "30.0.0.0")
    (dash "2.19.1")
    (git-commit "4.0.0")
    (magit-section "4.0.0")
    (seq "2.24")
    (transient "0.7.4")
    (with-editor "3.4.1"))
  :commit "020aca7c9c4154dbc4a72acbd56165ecccea1bf1" :authors
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
