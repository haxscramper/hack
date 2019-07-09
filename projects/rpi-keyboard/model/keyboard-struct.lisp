(defstruct section-vertex
  (is-p1 nil
   :documentation "Whether or not this vertex is 'first' in the edge")
  (p
   :type point
   :documentation "Coordinates of the vertex")
  (section-id 0
   :type integer
   :documentation "Parent section id")
  (edge-id 0
   :type integer
   :documentation "Parent edge id"))

(defstruct edge-final
  "This structure represents edge in final keyboard assembly"
  (p1 (make-point) :type section-vertex)
  (p2 (make-point) :type section-vertex)
  (paired-section-id 0
   :type integer
   :documentation "ID of section paired with this edge")
  (paired-edge-id 0
   :type integer
   :documentation "ID of edge paired with this one"))

(defstruct section-final
  "This structure represents section in final keyboard assembly"
  (*edge-list* (make-hash-table)
   :documentation "List of edges on this section"))


(defstruct keyboard-final
  "This structure represents final keyboard assembly, ready to be
exported in openscad format"
  (main-section-id 0 :type integer)
  (*sections* (make-hash-table)))


(defun sort-by-vertex-distance (vertex vertex-list)
  (sort vertex-list
        :key (lambda (lhs rhs)
               (> (point-distance (section-vertex-p lhs)
                                  (section-vertex-p vertex))
                  (point-distance (section-vertex-p rhs)
                                  (section-vertex-p vertex))))))


(defun edge-edge-point-distance (lhs rhs)
  "Get sum of the distance between p1 and p2 of two edges (distance
  between p1 and p1 + distance between p2 and p2)"
  (+ (point-distance
      (section-vertex-p (edge-final-p1 lhs))
      (section-vertex-p (edge-final-p1 rhs)))
     (point-distance
      (section-vertex-p (edge-final-p2 lhs))
      (section-vertex-p (edge-final-p2 rhs)))))

(defun get-all-edges (keyboard)
  "Get `list' of all edges in all sections of the keyboard"
  (let ((edges (list)))
    ;; (loop
    ;;   :for section-hash :being :each hash-value
    ;;     :of (keyboard-final-*sections*
    ;;          keyboard)
    ;;   :do (loop
    ;;         :for edge-hash :being :each has-value
    ;;           :of (section-final-*edge-list*
    ;;                ))
    ;;   )
    edges))

(defun find-closest-edge (section-id edge-id keyboard)
  "Return edge that is closes (measured by `edge-edge-point-distance'
  to base edget (from `section-id' and `edge-id'))"

  )
