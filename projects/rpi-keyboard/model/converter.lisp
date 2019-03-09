(load "~/.sbclrc")
(require 'cl-json)

(defstruct point
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum))

(defstruct keyboard-key
  (legend "none" :type string)
  (offset (make-point) :type point)
  (width 1.0 :type real)
  (height 1.0 :type real)
  (depth 1.0 :type real))

(defstruct section-row
  (keys (list) :type list))

(defstruct  keyboard-section
  (tilt-angle 0 :type fixnum)
  (rotation-angle 0 :type fixnum)
  (rotation-pos-center (make-point) :type point)
  (rotation-offset (make-point) :type point)
  (rows (list) :type list))

(defstruct keyboard
  (sections (list) :type list))

(defstruct bounding-projection
  (lower-left (make-point) :type point)
  (lower-right (make-point) :type point)
  (upper-left (make-point) :type point)
  (upper-right (make-point) :type point))

(defstruct line-2d
  (base-point (make-point) :type point)
  (x-angle-rad 0 :type real))

;;;#== Helper functions

(defun sum (list)
  "Calculate sum of items in list"
  (reduce #'+ list))

(defun avg (list)
  "Calculate average value of items in list"
  (/ (reduce #'+ list)
     (length list)))

(defun xor (x y)
  (or (and (not x)
           y)
      (and x
           (not y))))

(defun deg->rad (x)
  (* x (/ pi 180)))


(defmacro first! (list)
  `(car ,list))

(defmacro last! (list)
  `(car (last ,list)))

(defun row-bounding-projection (row)
  "Find boinding projection for xy coordinates of the row"
  (let ((res-projection (make-bounding-projection))
        ;; Sort keys by their x and y coordinates
        (sorted-x
          (sort (section-row-keys row)
                (lambda (lhs rhs)
                  (> (point-x (keyboard-key-offset lhs))
                     (point-x (keyboard-key-offset rhs))))))
        (sorted-y
          (sort (section-row-keys row)
                (lambda (lhs rhs)
                  (> (point-y (keyboard-key-offset lhs))
                     (point-y (keyboard-key-offset rhs)))))))
    ;; Find max/min for x/y coordinates
    (let ((min-x (point-x (keyboard-key-offset (last! sorted-x))))
          (min-y (point-y (keyboard-key-offset (last! sorted-y))))
          (max-x (+ (point-x (keyboard-key-offset (last! sorted-x)))
                    (keyboard-key-width (last! sorted-x))))
          (max-y (+ (point-y (keyboard-key-offset (last! sorted-y)))
                    (keyboard-key-width (last! sorted-y)))))
      ;; Build boinding rectangle sides from min/max coordinates
      (setf (bounding-projection-lower-left
             res-projection)
            (make-point :x min-x :y min-y))
      (setf (bounding-projection-lower-right
             res-projection)
            (make-point :x max-x :y min-y))
      (setf (bounding-projection-upper-left
             res-projection)
            (make-point :x min-x :y max-y))
      (setf (bounding-projection-upper-right
             res-projection)
            (make-point :x max-x :y max-y)))
    res-projection))

(defun line-point-distance (line point)
  "Find shortest distance between line and point."
  (let ((x_0 (point-x (base-point line)))
        (x_1 (point-x point))
        (y_0 (point-y (base-point line)))
        (y_1 (point-y point)))
    (let ((tg_a (tan (x-angle-rad line))))
      (/ (abs (+ (- x_0 x_1)
                 (* tg_a
                    (- y_1 y_0))))
         (sqrt (+ 1 (* tg_a tg_a)))))))

(defun and-list (list)
  (reduce (lambda (lhs rhs) (and lhs rhs)) list))

(defun separates-plane? (line points)
  "Check if lines separates XY plane into two parts only one of which
  contains all points and the second contains zero points

  Args:
  - line(line-2d): line to test
  - points(list(point)): list of poinst to set agains

  Returns: t or false depeding on test resuts"
  (let ((is-above
          (map
           'list
           (lambda (point)
             (let
                 ((above?
                    (<= (point-y point)
                        (+ (* (tan (line-2d-x-angle-rad line))
                              (point-x point))
                           (point-y (line-2d-base-point line))))))
               ;; (format t "Point ~A is above ~%~A: ~A~%~%" point line above?)
               above?))
           points)))
    (or (and-list is-above)
        (and-list (loop for x in is-above collect (not x))))))


(defun average-line-points-distance (line points)
  "Get average distance from line to set of points"
  (avg (map
        'list
        (lambda (point)
          (line-point-distance line point))
        points)))

(defun find-separating-line (base-point points &optional
                                                 (angle-steps nil)
                                                 (increment-direction 1))
  "Find line that passes through base-point and separtes XY plane into
  two parts: one contains all of the points and other contains exactly
  zero points.

  Args:
  - base-point(point): base-point of resulting line
  - points(list(point)): list of points
  - angle-steps(list(real)): list of x-angle-rads to try for resuting line

  Returns: fitting line-2d."
  (when (eq angle-steps nil)
    (setf angle-steps (loop for n from 0 below 360 by 15 collect n)))
  (let ((best-angle
          (loop
            for angle in angle-steps
            minimize (average-line-points-distance
                      (make-line-2d
                       :base-point base-point
                       :x-angle-rad angle)
                      points)))
        (distance-increment 0.1))
    (make-line-2d
     :base-point (make-point
                  :x (+ (point-x base-point)
                        (do ((increment 0 (+ distance-increment
                                             increment)))
                            ((not (separates-plane?
                                   (make-line-2d
                                    :base-point base-point
                                    :x-angle-rad best-angle)))
                             increment)))
                  :y (+ (point-y base-point)))
     :x-angle-rad best-angle)))

(defun section-bounding-projection(section)
  (let* ((res-projection (make-bounding-projection))
         (row-bounding-rects
           (map 'list #'row-bounding-projection
                (keyboard-section-rows section)))
         (sorted-x
           ;; From leftmost to right most row (comparing on left edge)
           (sort row-bounding-rects
                 (lambda (lhs rhs)
                   (> (point-x (bounding-projection-lower-left lhs))
                      (point-x (bounding-projection-lower-left rhs))))))
         (sorted-y
           ;; From lowest to highest row (comparing on top edge)
           (sort row-bounding-rects
                 (lambda (lhs rhs)
                   (> (point-y (bounding-projection-upper-right lhs))
                      (point-y (bounding-projection-upper-right rhs)))))))
    res-projection))



;; TODO rewrite using macros


(defun key-from-json (alist-json)
  (let ((res-key (make-keyboard-key)))
    (if (assoc :legend alist-json)
        (setf (keyboard-key-legend res-key)
              (rest (assoc :legend alist-json))))
    (if (assoc :width alist-json)
        (setf (keyboard-key-width res-key)
              (rest (assoc :width alist-json))))
    (if (assoc :height alist-json)
        (setf (keyboard-key-height res-key)
              (rest (assoc :height alist-json))))
    res-key))

(defun row-from-json (alist-row)
  (let ((res-row (make-section-row)))
    (loop
          for key in (rest (assoc :keys alist-row))
          do (setf (section-row-keys res-row)
                   (append (section-row-keys res-row)
                           (list (key-from-json key)))))
    res-row))

(defun section-from-json (alist-section)
  (let ((res-section (make-keyboard-section)))
    (loop
          for row in (rest (assoc :rows alist-section))
          do (setf (keyboard-section-rows res-section)
                   (append (keyboard-section-rows res-section)
                           (list (row-from-json row)))))
    res-section))

(defun keyboard-from-json (file-path)
  (let ((keyboard-json
          (json:decode-json-from-source
           (make-pathname :name file-path))))
    (let ((sections-list (rest (assoc :sections keyboard-json)))
          (res-keyboard (make-keyboard)))
      (loop
        for section in sections-list
        do (setf (keyboard-sections res-keyboard)
                 (append (keyboard-sections res-keyboard)
                         (list (section-from-json section)))))
      res-keyboard)))

(defun keyboard-key->openscad (key)
  (format nil "[~%~A, // position ~%[~A, ~A, ~A] // size ~%]"
          (point->openscad (keyboard-key-offset key))
          (keyboard-key-depth key)
          (keyboard-key-height key)
          (keyboard-key-width key))
  )

(defun section-row->openscad (row)
  (let ((res-string ""))
    (loop
      for key in (section-row-keys row)
      do (setf res-string
               (format nil "~A~A, // key ~%" res-string
                       (keyboard-key->openscad key))))
    (setf res-string (format nil "~%[~A], // row~%" res-string))
    res-string))



(defun section-rows->openscad(section)
  "Convert rows of given `keyboard-section' to openscad string"
  (let ((res-string ""))
    (loop
      for row in (keyboard-section-rows section)
      do (setf res-string
               (concatenate 'string res-string
                            (section-row->openscad row))))
    (setf res-string (format nil "[~A]" res-string))
    res-string))

(defun point->openscad (point)
  "Convert `point' to openscad code string"
  (format nil "[ ~A, ~A, ~A ]"
          (point-x point)
          (point-y point)
          (point-z point)))


(defun keyboard-section->openscad (section)
  "Convert `keyboard-section' object to openscad code string"
  (let ((res-string ""  ))
    (setf res-string (concatenate 'string res-string
                                  (format nil
                                          "section(rowlist = ~A, // rowlist~%~
                                                           tilt_angle = ~A,~%~
                                                           rot_angle = ~A,~%~
                                                           rot_center = ~A~%);~%~%"
  (section-rows->openscad section)
  (keyboard-section-tilt-angle section)
  (point->openscad
   (keyboard-section-rotation-pos-center section))
  (point->openscad
   (keyboard-section-rotation-offset section)))))
    res-string))

(defun prettify-openscad (file-string)
  ;; "Prettify openscad code using clang-format"
  ;;(sb-ext:run-program
  ;; "/usr/bin/clang-format"
  ;; (list)
  ;; :input file-string
  ;; :output *standard-output*)
  ;;"<<<"
  file-string
  )

(defun keyboard->openscad (keyboard)
  (let ((res-string ""))
    (loop
      for section in (keyboard-sections keyboard)
      do (progn
           (setf res-string
                 (concatenate 'string res-string
                              (keyboard-section->openscad section)))
           ))
    res-string))

(defun json->openscad (json-file-name openscad-file-name)
  (with-open-file (output (make-pathname :name openscad-file-name)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format output "include <keyboard_lib_2.scad> // ~%")
    (format output "~A"
            (keyboard->openscad
             (keyboard-from-json json-file-name)))))



(defun main()
  (let ((json-file "lisp_layout.json")
        (scad-file "lisp_layout.scad"))
    ;; (format t "~A~%" (keyboard-from-json json-file) )
    (json->openscad json-file scad-file)
    ;; (format t "~A~%" (prettify-openscad (keyboard->openscad (keyboard-from-json json-file))))
    ))

;; (format t ">>>>>>>>>>>> START~%")
;;
;; (main)
;;
;; (format t "<<<<<<<<<<<< END~%")
