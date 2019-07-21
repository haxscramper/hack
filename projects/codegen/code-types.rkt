#lang typed/racket


(struct CNode
  ([code : (Listof (U CNode String))]))

(struct variable
  ([type : String]
   [name : String]))


(define (make-CNode [s : String]) (CNode (list s)))
(define (% [s : String]) (CNode (list s)))
(define (%l [s : String]) (list (% s)))


(provide (all-defined-out))
