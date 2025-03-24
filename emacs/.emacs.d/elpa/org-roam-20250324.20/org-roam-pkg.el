;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "20250324.20"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (dash          "2.13")
    (org           "9.6")
    (emacsql       "4.1.0")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "cce9591c1c13718beb6dc809fbdca3f1b1cc075b"
  :revdesc "cce9591c1c13"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
