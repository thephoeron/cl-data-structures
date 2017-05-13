(in-package :cl-data-structures.utils)


(-> range-sub-vector (vector index index) vector)
(defun range-sub-vector (vector start end)
  "Returns array displaced to the vector (starting with start, ending on end)"
  (make-array (- end start 1)
              :element-type (array-element-type vector)
              :displaced-to vector
              :displaced-index-offset start))
