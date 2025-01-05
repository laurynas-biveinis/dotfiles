;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "20250105.443"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (dash          "2.13")
    (org           "9.4")
    (emacsql       "20230228")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "cad3518788991623aa5621341471aef67108937d"
  :revdesc "cad351878899"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
