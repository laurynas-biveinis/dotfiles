;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "2.3.0"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (dash          "2.13")
    (org           "9.6")
    (emacsql       "4.1.0")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "2ff616fbd8277bd797254befe34d5036b35a7dbf"
  :revdesc "2ff616fbd827"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
