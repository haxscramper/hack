#!/usr/bin/env -S guile --no-auto-compile -s
!#

(use-modules (ice-9 readline)) (activate-readline)
(add-to-load-path "/usr/share/guile/site/3.0")
(add-to-load-path ".")
(use-modules (opencog))
(use-modules (opencog query))
(use-modules (opencog exec))
