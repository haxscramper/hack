#!/usr/bin/env -S sbcl --script

(declaim (sb-ext:muffle-conditions cl:warning))


(defun bind (x sq s)
  (if (or (var? sq) (atom sq) (copy? sq))
      (add x sq s)
    (multiple-value-bind (te e)
        (copy sq s)
      (add x te e))))

(defun add (x val s) (acons x val s))

(defmacro copy? (x) `(eql (car ,x) #\*))
(defmacro tcons (x y) `(cons #\* (cons ,x ,y)))

(defun copy (x s)
  (cond
   ((var? x)
    (let ((a (ult x s)))
      (if (var? a)
          (let ((b (genvar a)))
            (values b (add a b s)))
        (values a s))))
   ((atom x) (values x s))
   (t
    (multiple-value-bind (tl el) (copy (car x) s)
      (multiple-value-bind (t2 e2) (copy (cdr x) el)
        (values (tcons tl t2) e2))))))


(defun genvar (x)
  (gentemp (concatenate 'string (string x) "-")))

(defmacro mycar (x)
  `(if (copy? ,x) (cadr ,x) (car ,x)))
(defmacro mycdr (x)
  `(if (copy? ,x) (cddr ,x) (cdr ,x)))

(defun unif (x y s)
  (let ((x1 (val x s)) (y1 (val y s)))
    (cond
     ((eql x1 y1) s)
     ((var? x1) (bind x1 y1 s))
     ((var? y1) (bind y1 x1 s))
     ((or (atom x1) (atom y1)) 'fail)
     (t (let ((news (unif (mycar x1) (mycar y1) s)))
          (if (eq news 'fail)
              news
            (unif (mycdr x1) (mycdr y1) news)))))))



(defun bound? (x s) (assoc x s))
(defun var? (x) (and (symbolp x) (char= (char (string x) 0) #\_)))
(defun value (x s) (cdr (assoc x s)))

(defun val (te s)
  (if (var? te)
      (ult te s)
    te))

(defun ult (v s)
  (if (bound? v s)
      (val (value v s) s)
    v))

(defun ext (v s)
  (if (atom v)
      v
    (cons (ext (cadr v) s) (ext (cddr v) s))))


(defmacro debug-copy (te env)
  `(multiple-value-bind (te env) (copy ,te ,env)
     (format t "Te: ~A, env: ~A~%" te env)))

(defmacro debug-unif (t1 t2)
  `(format t "t1: ~A t2: ~A env: ~A~%" ,t1 ,t2 (unif ,t1 ,t2 nil)))

(debug-unif '(t _X) '(t 1))
(debug-unif '(t g(_X)) '(t g(2)))

(debug-copy '_X '())
(debug-copy '(t f(g(a _X))) '())



;; (test '(t _X _Y (f (g _X) _Y)) '(t c b _Z))
