#! /usr/bin/env racket
#lang typed/racket

(struct code
  ([statements : (Listof String)]))

(struct CStatement
  ([code : (Listof CNode)]))

(struct CNode
  ([code : (Listof (U CNode String))]))

(struct variable
  ([name : String]
   [type : String]))

(struct function
  ([name : String]
   [args : (Listof variable)]
   [return : String]))


(define (make-CNode [s : String]) (CNode (list s)))
(define (% [s : String]) (CNode (list s)))

(define
  (make-class
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


(define
  (make-enum
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

(define
  (make-while
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

(define
  (CNode->code
   [node : CNode]) : String
  (string-join
   (map
    (lambda
        ([var : (U CNode String)])
      (cond
        [(CNode? var) (CNode->code var)]
        [(string? var) var]))
    (CNode-code node))
   "\n"))



(define
  (code->c++-code
   [c : code]) : String
  (string-append
   (string-join (code-statements c) "\n")
   "\n"))

(define
  (get-signature [f : function]) : String
  (string-append
   (function-name f)
   " : "
   (string-join
    (map
     (lambda ([var : variable]) (variable-type var))
     (function-args f))
    " X ")
   " |-> "
   (function-return f)))

(define
  (function+code->c++-function
   [f : function]
   [c : code]) : String
  (string-append
   (function-return f)
   " "
   (function-name f)
   "("
   (string-join
    (map
     (lambda ([var : variable])
       (string-append
        (variable-type var)
        " "
        (variable-name var)))
     (function-args f))
    ", ")
   ") {\n"
   (code->c++-code c)
   "}"))


(let ([priorities
       '("Undefined"
         "NoPriority"
         "Low"
         "Medium"
         "High"
         "Critical"
         "Blocking"
         "CurrentlyWorking"
         "Organization"
         "Later")])
  (display
   (CNode->code
    (make-class
     "QSTodo"
     '("DataItem")
     (list
      (make-enum "Priority" priorities))))

   (open-output-file
    "parse.cpp"
    #:exists 'replace)))
