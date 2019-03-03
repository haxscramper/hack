#!/usr/bin/sbcl --script
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
                                 rot_center = ~A,~%);~%~%"
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
    (format output "~A"
            (keyboard->openscad
             (keyboard-from-json json-file-name)))))


;;#= Main


(defun main()
  (let ((json-file "lisp_layout.json")
        (scad-file "lisp_layout.scad"))
    ;; (format t "~A~%" (keyboard-from-json json-file) )
    (json->openscad json-file scad-file)
    ;; (format t "~A~%" (prettify-openscad (keyboard->openscad (keyboard-from-json json-file))))
    ))

(format t ">>>>>>>>>>>> START~%")

(main)

(format t "<<<<<<<<<<<< END~%")
