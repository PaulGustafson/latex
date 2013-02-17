(TeX-add-style-hook "2"
 (lambda ()
    (LaTeX-add-labels
     "1"
     "9"
     "5"
     "6"
     "7")
    (TeX-run-style-hooks
     "latex2e"
     "art10"
     "article")))

