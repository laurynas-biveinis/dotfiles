;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "20250313.134"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (dash          "2.13")
    (org           "9.6")
    (emacsql       "4.1.0")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "db4170a459cba06bd3d36fb6dc748364774ec204"
  :revdesc "db4170a459cb"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
