;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "20250111.252"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (dash          "2.13")
    (org           "9.4")
    (emacsql       "4.0.0")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "64e302c1269a1a16c4c0b5a7d1e3baf2d5ded174"
  :revdesc "64e302c1269a"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
