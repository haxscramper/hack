#!/usr/bin/sbcl --script
(load "~/.sbclrc")
(load "converter.lisp")
(require 'cl-json)

(defun pair-list->point-list (list)
  (map 'list
       (lambda (pair)
         (make-point :x (first! pair)
                     :y (last! pair)))
       list))

(defmacro fail!(assertion-name message)
  `(format t "~c[31m~A has failed~%~c[0m~A~%" #\ESC ,assertion-name #\ESC ,message))

(defmacro success!(assertion-name message)
  `(format t "~c[32m~A has succeded~%~c[0m~A~%" #\ESC ,assertion-name #\ESC ,message))

(defmacro assert! (assertion-name message expression)
  `(let ((result ,expression))
     (if result
         (success! ,assertion-name ,message)
         (fail! ,assertion-name ,message))))

(defun format-list (list)
  (format nil "~{~{  ~:@(~15,,,'_A~): ~A~}~%~}" list))

(defun format-named-list (name list)
  (format nil "~:@(~A~):~%~A" name (format-list list)))

(defun make-points (list)
  (map 'list
       (lambda
           (pair) (make-point
                   :x (first! pair)
                   :y (last! pair)))
       list))

(defmacro line-separation-test
    (line points test-name &optional
                             (inververt-p nil))
  `(let ((test-line ,line)
         (points ,points))
     (assert!
      ,test-name
      (format-named-list "input"
                         `(("Points" ,points)
                           ("Line" ,test-line)))
      (let ((separates? (separates-plane? test-line (make-points points))))
        (format t "Separates: ~A~%" separates?)
        (if ,inververt-p
            (not separates?)
            separates?)))))

(format t "============ START~%")

(line-separation-test
 (make-line-2d)
 '((0 -2) (0 -4))
 "Simple separation test")

(line-separation-test
 (make-line-2d :x-angle-rad (/ pi 4))
 '((-1 -2) (2 1) (8 7))
 "Tilted separation test")

(line-separation-test
 (make-line-2d)
 '((0 1) (0 -1))
 "Simple non-separation test"
 t)
