#! /usr/bin/env racket
#lang typed/racket

(require "code-types.rkt")
(require "make-code.rkt")

(define (CNode->code
         [node : CNode]) : String
  (string-join
   (map
    (lambda ([var : (U CNode String)])
      (cond
        [(CNode? var) (CNode->code var)]
        [(string? var) var]))
    (CNode-code node))
   "\n"))

(define (make-string->enum-converter
         [enum-name : String]
         [enum-fields : (Listof String)]
         [from-type : String "const std::string&"]
         [class-name : String ""]) : CNode
  (make-function
   enum-name
   (string-append "strTo" enum-name)
   (list (variable from-type "arg"))
   (list
    (make-if
     (% (string-append "arg == \"" (car enum-fields) "\""))
     (% (string-append "return " enum-name "::" (car enum-fields) ";")))
    (CNode (map (lambda ([enum-field : String])
                  (make-else-if
                   (% (string-append "arg == \"" enum-field "\""))
                   (% (string-append "return " enum-name "::" enum-field ";"))))
                (cdr enum-fields)))
    (% " ")
    (% (string-append
        "throw std::invalid_argument("
        "\"Invalid value for enum conversion. "
        "Expected values: "
        (string-join enum-fields ", ")
        " got: \"+ arg );")))))

(let ([enums
       '(("Priority" . ("Undefined"
                        "NoPriority"
                        "Organization"
                        "Later")))])
  (display
   (CNode->code
    (make-class
     "QSTodo"
     '("DataItem")
     (map
      (lambda ([enum : (Pairof String (Listof String))])
        (let ([enum-name (car enum)]
              [enum-fields (cdr enum)])
          (CNode
           (list
            (% (string-append "//#=== " enum-name))
            (make-enum enum-name enum-fields)
            (% " ")
            (make-string->enum-converter
             enum-name
             enum-fields)))))
      enums)))

   (open-output-file
    "parse.cpp"
    #:exists 'replace)))
