(in-package #:cl-data-structures.common.egnat)


(defgeneric select-children (range subtrees)
  (:method ((range egnat-range) (subtrees cl:sequence))
    (make-array (length subtrees)
                :element-type 'bit
                :initial-element 1)))


(defgeneric next-position (range data index)
  (:method ((range egnat-range) data index)
    (when (< index (length data))
      (cons (aref data index) (1+ index))))
  (:method ((range egnat-range-around) data index)
    (bind (((:slots %container %near %margin) range)
           (result (iterate
                     (for i from index below (length data))
                     (for content = (aref data i))
                     (for distance = (distance %container %near content))
                     (finding i such-that (<= distance %margin)))))
      (when result
        (cons (aref data result) (1+ result))))))


(defgeneric distance (container bucket element)
  (:method ((container fundamental-egnat-container)
            (bucket t)
            (element t))
    (funcall (read-metric-fn container) bucket element)))


(defgeneric same (container bucket element)
  (:method ((container fundamental-egnat-container)
            (bucket t)
            (element t))
    (funcall (read-same-fn container) bucket element)))
