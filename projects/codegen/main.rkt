#! /usr/bin/env racket
#lang typed/racket


(struct variable
  ([name : String]
   [type : String]))

(struct function
  ([name : String]
   [args : (Listof variable)]
   [return : String]))

(struct code
  ([statements : (Listof String)]))

(struct CStatement
  ([code : (Listof CNode)]))

(struct CNode
  ([code : (Listof (U CNode String))]))

(define (make-CNode [s : String]) (CNode (list s)))

(define
  (make-while
   [while-cond : CNode]
   [while-body : CNode]) : CNode
  (let ([while-head (make-CNode "while (")]
        [while-tail (make-CNode "}")]
        [while-middle (make-CNode ") {")])
    (CNode
     (list while-head
           while-cond
           while-middle
           while-body
           while-tail))))

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


(let ([f (function
          "print_hello"
          '()
          "void")]
      [c (code '("std::cout << \"Hello world!\\n\";"))])
  (display
   (function+code->c++-function f c)
   (open-output-file
    "parse.cpp"
    #:exists 'replace)))
