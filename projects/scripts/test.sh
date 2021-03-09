#!/usr/bin/env bash

intermediate="output.tmp.tex_pstricks"

m4 -I ~/.config/hax-software/m4circuit pgf.m4 input.tmp.m4 \
    | dpic -g > $intermediate

outtex="output.tmp.tex"

{
    cat << EOF
\documentclass[convert = false]{standalone}
\usepackage{tikz}

\begin{document}
EOF

cat $intermediate

    cat << EOF
\end{document}
EOF

} > $outtex

latexmk -C $outtex
latexmk -pdf -latexoption="-shell-escape" --interaction=nonstopmode $outtex
