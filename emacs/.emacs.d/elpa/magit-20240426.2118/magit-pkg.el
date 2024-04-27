(define-package "magit" "20240426.2118" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (compat "29.1.4.4")
    (dash "20240103")
    (git-commit "20240123")
    (magit-section "20240114")
    (seq "2.24")
    (transient "20240201")
    (with-editor "20240101"))
  :commit "49ba535f526266b69fcaf7442acfa59ad8473caf" :authors
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
