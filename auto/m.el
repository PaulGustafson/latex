(TeX-add-style-hook "m"
 (lambda ()
    (LaTeX-add-environments
     "theorem"
     "definition"
     "corollary"
     "lemma"
     "example")
    (TeX-add-symbols
     "diam"
     "length"
     "R"
     "N"
     "Q"
     "C"
     "Z"
     "p")
    (TeX-run-style-hooks
     "enumerate"
     "verbatim"
     "amsthm"
     "amsmath"
     "amssymb")))

