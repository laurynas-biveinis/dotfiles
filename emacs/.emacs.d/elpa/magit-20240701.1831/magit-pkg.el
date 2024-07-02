(define-package "magit" "20240701.1831" "A Git porcelain inside Emacs."
  '((emacs "26.1")
    (compat "29.1.4.5")
    (dash "20240405")
    (git-commit "20240429")
    (magit-section "20240429")
    (seq "2.24")
    (transient "20240421")
    (with-editor "20240415"))
  :commit "e2876c8bc9d92443488bb73ffb0d4281023d0d74" :authors
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
