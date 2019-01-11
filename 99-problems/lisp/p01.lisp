#!/usr/bin/sbcl --script

(defun last-box (list)
  (last list))

(print (last-box '(a b c d)))
