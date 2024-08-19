(define-package "git-commit" "4.0.0" "Edit Git commit messages"
  '((emacs "26.1")
    (compat "30.0.0.0")
    (seq "2.24")
    (transient "0.7.4")
    (with-editor "3.4.1"))
  :commit "020aca7c9c4154dbc4a72acbd56165ecccea1bf1" :authors
  '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
    ("Sebastian Wiesner" . "lunaryorn@gmail.com")
    ("Florian Ragwitz" . "rafl@debian.org")
    ("Marius Vollmer" . "marius.vollmer@gmail.com"))
  :maintainers
  '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainer
  '("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
  :keywords
  '("git" "tools" "vc")
  :url "https://github.com/magit/magit")
;; Local Variables:
;; no-byte-compile: t
;; End:
