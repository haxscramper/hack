#!/usr/bin/env -S sbcl --script

(declaim (sb-ext:muffle-conditions cl:warning))

(defun bound? (x s) (assoc x s))
(defun var? (x) (and (symbolp x) (char= (string x) 0 #\_)))
(defun value (x s) (cdr (assoc x s)))

(defun val (te s)
  (if (var? te)
    (ult te s)
    te))

(defun ult (v s)
  (if (bound? v s)
    (val (value v s) s)
    v))

(setq s '((_v1 . 1)))
(val '_v1 s)
