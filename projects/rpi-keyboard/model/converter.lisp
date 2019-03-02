#!/usr/bin/sbcl --script
(load "~/.sbclrc")
(require 'cl-json)

(defstruct point
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct keyboard-key
  (legend "none" :type string)
  (width 1 :type ratio)
  (height 1 :type ratio))

(defstruct section-row
  (keys (list) :type list))

(defstruct  keyboard-section
  (rotation-angle 0 :type fixnum)
  (rotation-pos-center (make-instance point) :type point)
  (rotation-offset (make-instance point) :type point)
  (rows (list) :type list))

(defstruct keyboard
  (sections (list) :type list))

(defun section-from-json (alist-section)
  (let ((res-section (make-keyboard-section)))
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
                             (section-from-json section)))))))

(defun main()
  (keyboard-from-json "lisp_layout.json"))

(format t ">>>>>>>>>>>>~%")

(main)

(format t "<<<<<<<<<<<<~%")
