#!/usr/bin/env -S guile --no-auto-compile -s
!#

(use-modules (oop goops))
(define-class cli ()
  (x #:init-value 0 #:init-keyword #:x #:getter get-x))

(define-method (+ (u cli) (v cli))
  (make cli #:x (+ (get-x u) (get-x v))))

(display (class-direct-slots cli))

(define a (make cli #:x 123))
(get-x a)
(+ a a)
(get-x (+ a a))
