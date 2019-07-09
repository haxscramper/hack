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

(defun and-list (list)
  (reduce (lambda (lhs rhs) (and lhs rhs)) list))
