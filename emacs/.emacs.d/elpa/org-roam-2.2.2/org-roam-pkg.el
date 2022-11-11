(define-package "org-roam" "2.2.2" "A database abstraction layer for Org-mode"
  '((emacs "26.1")
    (dash "2.13")
    (org "9.4")
    (emacsql "3.0.0")
    (emacsql-sqlite "1.0.0")
    (magit-section "3.0.0"))
  :commit "69116a4da49448e79ac03aedceeecd9f5ae9b2d4" :authors
  '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainer
  '("Jethro Kuan" . "jethrokuan95@gmail.com")
  :keywords
  '("org-mode" "roam" "convenience")
  :url "https://github.com/org-roam/org-roam")
;; Local Variables:
;; no-byte-compile: t
;; End:
