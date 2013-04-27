(TeX-add-style-hook "paper2"
 (lambda ()
    (LaTeX-add-bibliographies
     "paper1")
    (TeX-run-style-hooks
     "m467"
     "latex2e"
     "art10"
     "article")))

