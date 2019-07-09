#! /usr/local/bin/Racket
#lang typed/racket

(struct point
  ([x : Integer]
   [y : Integer]))

(define (distance [p1 : point] [p2 : point])
  (sqrt (+ (sqr (- (point-x p2) (point-x p1)))
           (sqr (- (point-y p2) (point-y p1))))))

(let ((origin (point 0 0))
      (test (point 2 2)))
  (distance origin test))
