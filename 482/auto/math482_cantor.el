(TeX-add-style-hook "math482_cantor"
 (lambda ()
    (LaTeX-add-environments
     "theorem"
     "definition"
     "corollary"
     "lemma"
     "example")
    (LaTeX-add-bibitems
     "Carothers"
     "Munkres")
    (LaTeX-add-labels
     "subc"
     "hom"
     "cfunction"
     "continuous"
     "split")
    (TeX-add-symbols
     "diam"
     "length"
     "R"
     "N"
     "m")
    (TeX-run-style-hooks
     "verbatim"
     "amsthm"
     "amsmath"
     "amssymb"
     "latex2e"
     "art10"
     "article")))

