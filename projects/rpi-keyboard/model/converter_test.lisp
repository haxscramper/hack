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


(defun line-separation-tests ()
  "Run line separatino tests"
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

  (line-separation-test
   (make-line-2d :base-point (make-point :x 3 :y 3))
   '((0 -2) (0 -4))
   "Simple separation test shifted")

  (line-separation-test
   (make-line-2d
    :x-angle-rad (/ pi 4)
    :base-point (make-point :x 4 :y 4))
   '((-1 -2) (2 1) (8 7))
   "Tilted shifted separation test")


  (line-separation-test
   (make-line-2d
    :base-point (make-point :x 200))
   '((0 1) (0 -1))
   "Simple shifted non-separation test"
   t))

(defun line-2d->svg (l image-w image-h)
  (let ((half-image-h (/ image-h 2))
        (half-image-w (/ image-w 2)))
    (concatenate
     'string
     (format
      nil
      "<line x1=\"~F\" y1=\"~F\" x2=\"~F\" y2=\"~F\" stroke=\"black\"/>~%"
      0 ; x1
      (+ ; convert to svg coordinate system
       (* 10 ; convert to svg coordinate scale
          (+ (point-y (line-2d-base-point l))
             (* (tan (line-2d-x-angle-rad l))
                (/ image-w 20))))
       half-image-h)
      image-w
      (+ ; convert to svg coordinate system
       (* 10 ; convert to svg coordinate scale
          (+ ; add base line point offset
           (point-y (line-2d-base-point l))
           (* (tan (line-2d-x-angle-rad l))
              (/ image-w -20))))
       half-image-h))
     (format
      nil
      "<circle cx=\"~F\" cy=\"~F\" r=\"3\" fill=\"none\" stroke=\"red\"/>"
      (+ (* (point-x (line-2d-base-point l)) 10) half-image-w)
      (+ (* (point-y (line-2d-base-point l)) 10) half-image-h)))))

(defun point->svg (p half-image-w half-image-h)
  (format
   nil
   "<circle cx=\"~A\" cy=\"~A\" r=\"2\"/>"
   (+ (* (point-x p) 10) half-image-w)
   (+ (* (point-y p) 10) half-image-h)))

(defun generate-svg-debug (file-name points lines)
  "Given set of points and lines, generate svg image file"
  (with-open-file (stream file-name
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let*
        ((image-w 480)
         (image-h 480)
         (half-image-w (/ image-w 2))
         (half-image-h (/ image-h 2))
         )
      (format stream "
<svg version=\"1.1\"
     baseProfile=\"full\"
     width=\"~A\" height=\"~A\"
     xmlns=\"http://www.w3.org/2000/svg\">
<rect width=\"100%\" height=\"100%\" fill=\"white\"/>
~A~A</svg>~%"
              image-w
              image-h
              (format
               nil "~{~A~%~}~%"
               (loop
                 for p in points
                 collect (point->svg p half-image-w half-image-h)))
              (format
               nil "~{~A~%~}~%"
               (loop
                 for l in lines
                 collect (line-2d->svg l image-w image-h)))) )))


(format t "============ START~%")

;; (line-separation-tests)

(generate-svg-debug
 "simple-separating-line-find.svg"
 (make-points '((0 -2) (0 -4)))
 (list (make-line-2d :x-angle-rad (/ pi 12))
       ))


(format t "============ END~%")
