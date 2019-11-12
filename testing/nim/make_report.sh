#!/usr/bin/env bash

cat << EOF > default.toml
org_header = """
#+LATEX_CLASS_OPTIONS: [a4paper,12pt]
#+LATEX_HEADER: \\\usepackage[left=1.5cm,right=2cm,top=3cm,bottom=3cm]{geometry}
#+LATEX_HEADER: \\\usepackage[pdfborder={0,0,0}]{hyperref}
#+LATEX_HEADER: \\\hypersetup{colorlinks=true,linkcolor=blue}

#+LATEX_HEADER: \\\usepackage[T2A]{fontenc}
#+LATEX_HEADER: \\\usepackage[utf8]{inputenc}
#+LATEX_HEADER: \\\usepackage[russian]{babel}

#+LATEX_HEADER: \\\addto\\\\captionsenglish{\\\\renewcommand{\\\\contentsname}{Оглавление}}

#+OPTIONS: toc:1
"""
EOF


./report_generator.nim.bin --input:"$1" --output:"$1.org"

emacs \
       --batch \
       "$1.org" \
       -f org-latex-export-to-pdf
