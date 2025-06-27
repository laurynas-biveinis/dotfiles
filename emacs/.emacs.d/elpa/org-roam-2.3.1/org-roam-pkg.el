;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "2.3.1"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (dash          "2.13")
    (org           "9.6")
    (emacsql       "4.1.0")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "7ce95a286ba7d0383f2ab16ca4cdbf79901921ff"
  :revdesc "7ce95a286ba7"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
