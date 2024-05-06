(define-package "magit" "20240505.1801" "A Git porcelain inside Emacs."
  '((emacs "26.1")
    (compat "29.1.4.5")
    (dash "20240405")
    (git-commit "20240429")
    (magit-section "20240429")
    (seq "2.24")
    (transient "20240421")
    (with-editor "20240415"))
  :commit "ea0d30d9a6056064b6d2cb4b4a5a8ca8fd036d2d" :authors
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
