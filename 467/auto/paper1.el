(TeX-add-style-hook "paper1"
 (lambda ()
    (LaTeX-add-bibliographies)
    (TeX-run-style-hooks
     "graphicx"
     "latex2e"
     "art10"
     "article")))

