(TeX-add-style-hook "lambda-beamer"
 (lambda ()
    (TeX-add-symbols
     "true"
     "false")
    (TeX-run-style-hooks
     "inputenc"
     "latin1"
     "latex2e"
     "beamer10"
     "beamer")))

