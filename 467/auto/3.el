(TeX-add-style-hook "3"
 (lambda ()
    (LaTeX-add-bibliographies
     "paper1")
    (TeX-run-style-hooks
     "latex2e"
     "art10"
     "article")))

