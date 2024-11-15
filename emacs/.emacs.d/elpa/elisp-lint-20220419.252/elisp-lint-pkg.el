;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "elisp-lint" "20220419.252"
  "Basic linting for Emacs Lisp."
  '((emacs        "24.4")
    (dash         "2.15.0")
    (package-lint "0.11"))
  :url "https://github.com/gonewest818/elisp-lint"
  :commit "c5765abf75fd1ad22505b349ae1e6be5303426c2"
  :revdesc "c5765abf75fd"
  :keywords '("lisp" "maint" "tools")
  :authors '(("Nikolaj Schumacher" . "bugs*nschumde"))
  :maintainers '(("Neil Okamoto" . "neil.okamoto+melpa@gmail.com")))
