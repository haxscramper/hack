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

(defstruct bounding-projection (edges '()))

(defstruct line-2d
  (base-point (make-point) :type point)
  (x-angle-rad 0 :type real))
