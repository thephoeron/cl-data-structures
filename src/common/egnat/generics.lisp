(in-package #:cl-data-structures.common.egnat)


(defgeneric bucket-head (container bucket)
  (:method ((container fundamental-egnat-container)
            (bucket vector))
    (assert (not (emptyp bucket)))
    (aref bucket 0)))


(defgeneric select-children (range subtrees)
  (:method ((range egnat-range) (subtrees cl:sequence))
    (make-array (length subtrees)
                :element-type 'bit
                :initial-element 1)))


(defgeneric next-bucket-position (range bucket index))
