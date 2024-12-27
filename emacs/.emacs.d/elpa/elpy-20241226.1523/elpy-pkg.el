;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "elpy" "20241226.1523"
  "Emacs Python Development Environment."
  '((company               "0.9.10")
    (emacs                 "24.4")
    (highlight-indentation "0.7.0")
    (pyvenv                "1.20")
    (yasnippet             "0.13.0")
    (s                     "1.12.0"))
  :url "https://github.com/jorgenschaefer/elpy"
  :commit "61540e7138207a8d46cbb0feb88caf27cbb38611"
  :revdesc "61540e713820"
  :keywords '("python" "ide" "languages" "tools")
  :authors '(("Jorgen Schaefer" . "contact@jorgenschaefer.de")
             ("Gaby Launay" . "gaby.launay@protonmail.com"))
  :maintainers '(("Jorgen Schaefer" . "contact@jorgenschaefer.de")
                 ("Gaby Launay" . "gaby.launay@protonmail.com")))
