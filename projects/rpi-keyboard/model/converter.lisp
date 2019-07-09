(load "~/.sbclrc")
(require 'cl-json)

(load "structs.lisp")
(load "support.lisp")
(load "bounding-projection-find.lisp")
(load "json-scad-conversion.lisp")

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
