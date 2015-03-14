(TeX-add-style-hook
 "writeup"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fullpage" "in") ("answers" "nosolutionfiles")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "fullpage"
    "listings"
    "cleveref"
    "answers"
    "graphicx"
    "xcolor"
    "color"
    "enumerate")
   (TeX-add-symbols
    '("dddt" 1)
    '("ddt" 1)
    '("dd" 1)
    '("Opentesthook" 2)
    "todo")
   (LaTeX-add-environments
    "Ex")))

