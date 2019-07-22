#lang typed/racket

(struct Type
  ([name : String]))

(struct Variable
  ([type : Type]
   [name : String]))

(struct Function
  ([name : String]
   [return : Type]
   [args : (Listof Variable)]))

(struct EnumType
  ([name : String]
   [fields : (Listof (U String
                        (Pairof String String)))]))

(struct SumType
  ([name : String]
   [types : (Listof Type)]))

(struct Class
  ([name : String]
   [inherited-from : (Listof (Pairof String Class))]))


(struct CNode
  ([code : (Listof (U CNode String))]))

(struct variable
  ([type : String]
   [name : String]))


(define (make-CNode [s : String]) (CNode (list s)))
(define (% [s : String]) (CNode (list s)))
(define (%l [s : String]) (list (% s)))


(provide (all-defined-out))
