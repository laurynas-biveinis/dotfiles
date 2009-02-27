;; System specific setup for Linux

(defun system-specific-presetup()
  "Things that must be set on Linux before main setup"

  ; Default font
  (if (>= emacs-major-version 23)
      (add-to-list 'default-frame-alist '(font . "Bitstream Vera Sans Mono-8"))
    (add-to-list 'default-frame-alist
		 '(font . "-Misc-Fixed-Medium-R-Normal--14-130-75-75-C-70-ISO8859-1"))))

(defun system-specific-setup()
  "Setup specifics for Linux"
  ())

