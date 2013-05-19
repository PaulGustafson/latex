(TeX-add-style-hook "m"
 (lambda ()
    (LaTeX-add-environments
     "theorem"
     "definition"
     "corollary"
     "lemma"
     "example"
     "prop")
    (TeX-add-symbols
     "cross"
     "diam"
     "length"
     "R"
     "N"
     "Q"
     "C"
     "Z"
     "F"
     "p"
     "isomorphicto")
    (TeX-run-style-hooks
     "enumerate"
     "verbatim"
     "amsthm"
     "amsmath"
     "amssymb")))

