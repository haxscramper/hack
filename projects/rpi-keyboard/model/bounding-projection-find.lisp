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
      (setf (bounding-projection-edges res-projection)
            (list (make-point :x min-x :y min-y)
                  (make-point :x max-x :y min-y)
                  (make-point :x min-x :y max-y)
                  (make-point :x max-x :y max-y))))
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

(defun find-separating-line (base-point points &key
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


(defun section-bound-directions (secton)
  "For each direction (up,right,left,down) find section's point that
is the farthest in that direction."
  (let ((increment-directions (make-points '((0 1) (1 0) (0 -1) (-1 0)))))))

(defun section-bounding-projection (section)
  "Find bounding projection the keyboard section"
  (let* ((row-bounding-rects
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
    (let ((increment-directions (make-points '((0 1) (1 0) (0 -1) (-1 0)))))
      (make-bounding-projection
       :edges
       ;; For each increment direction find line that separates plane
       (loop
         for direction in increment-directions ; All four sides
         for base-point in base-points ; Get base point for each direction
         collect
         (find-separating-line
          base-points
          :increment-direction direction))))
    res-projection))


(defun line-line-intersection (lhs rhs)
  "Calculate point of intersection of two lines. If lines do not
  intersect return `nil'"
  (if (= (line-2d-x-angle-rad lhs)
         (line-2d-x-angle-rad rhs))
      nil
      (let ((y2 (point-y (base-point rhs)))
            (y1 (point-y (base-point lhs)))
            (x2 (point-x (base-point rhs)))
            (x1 (point-x (base-point lhs)))
            (a2 (line-2d-x-angle-rad rhs))
            (a1 (line-2d-x-angle-rad lhs)))
        (let ((l1
                (/ ( - (* (cos a2)
                          (- y2 y1))
                       (* (sin a2)
                          (- x2 x1)))
                   (- (* (cos a2)
                         (sin a1))
                      (* (cos a1)
                         (sin a2)))))))
        (make-point
         :x (+ (* l1 (cos a1))
               x1)
         :y (+ (* l1 (sin a1))
               y1)))))

(defun bounding-projection-edges (bounding-projection)
  ""
  )
