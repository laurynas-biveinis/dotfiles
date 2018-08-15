;; System specific setup for Linux

; Default font
(defconst my-frame-font
  "-Misc-Fixed-Medium-R-Normal--14-130-75-75-C-70-ISO8859-1"
  "My default frame font for Emacs 23.1 and lower.")

(defun system-specific-presetup()
  "Things that must be set on Linux before main setup."

  (if (symbolp 'font-use-system-font)
      (setq font-use-system-font t)
    (progn
      (add-to-list
       'default-frame-alist
       `(font . ,my-frame-font))
      (set-frame-font my-frame-font))))

(defun system-specific-setup()
  "Setup specifics for Linux."

  ;; Make shifted direction keys work on the Linux console or in an xterm
   (when (member (getenv "TERM") '("linux" "xterm"))
     (dolist (prefix '("\eO" "\eO1;" "\e[1;"))
       (dolist (m '(("2" . "S-") ("3" . "M-") ("4" . "S-M-") ("5" . "C-")
                    ("6" . "S-C-") ("7" . "C-M-") ("8" . "S-C-M-")))
         (dolist (k '(("A" . "<up>") ("B" . "<down>") ("C" . "<right>")
                      ("D" . "<left>") ("H" . "<home>") ("F" . "<end>")))
           (define-key function-key-map
             (concat prefix (car m) (car k))
             (read-kbd-macro (concat (cdr m) (cdr k)))))))
   )
  )
