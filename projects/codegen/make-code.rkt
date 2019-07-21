#lang typed/racket

(require "code-types.rkt")

(define (make-class
         [name : String]
         [inherited-from : (Listof String)]
         [body : (Listof CNode)]) : CNode
  (CNode
   (append
    (list
     (% " class ")
     (% name)
     (% " : ")
     (% (string-join
         (map (lambda ([cls : String])
                (string-append " public " cls))
              inherited-from) " , "))
     (% " { "))
    body
    (list (% " }; ")))))


(define (make-enum
         [name : String]
         [fields : (Listof String)]) : CNode
  (CNode
   (list
    (% "enum class")
    (% name)
    (% " { ")
    (% (string-join
        (map
         (lambda
             ([s : String])
           (string-append s " , "))
         fields)))
    (% "};"))))

(define (make-while
         [while-cond : CNode]
         [while-body : CNode]) : CNode
  (let ([while-head (% "while (")]
        [while-tail (% "}")]
        [while-middle (% ") {")])
    (CNode
     (list while-head
           while-cond
           while-middle
           while-body
           while-tail))))

(define (make-if
         [if-cond : CNode]
         [if-body :  CNode]) : CNode
  (CNode (list "if (" if-cond ") {" if-body "}")))

(define (make-else-if
         [if-conf : CNode]
         [if-body :  CNode]) : CNode
  (CNode (list (% "else") (make-if if-conf if-body))))

(define (make-else
         [else-body : CNode]) : CNode
  (CNode (list (% "else {") else-body (% "}"))))

(define (make-function
         [return : String]
         [name : String]
         [args : (Listof variable)]
         [body : (Listof CNode)]) : CNode
  (CNode
   (append
    (list
     (% return)
     (% name)
     (% "(")
     (% (string-join
         (map
          (lambda ([var : variable])
            (string-append (variable-type var) " "
                           (variable-name var)))
          args)
         ", "))
     (% ") {"))
    body
    (list (% "}")))))

(provide (all-defined-out))
