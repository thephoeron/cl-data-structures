(in-package #:cl-data-structures.common.egnat)


(defgeneric select-children (range node)
  (:method ((range egnat-range) (node egnat-node))
    (make-array (length (read-children node))
                :element-type 'bit
                :initial-element 1))
  (:method ((range egnat-range-around) (node egnat-node))
    (prune-subtrees (read-container range)
                    (read-children node)
                    (read-close-range node)
                    (read-distant-range node)
                    (read-near range)
                    (read-margin range)))
  (:method ((range egnat-grow-range) (node egnat-node))
    (let ((result (call-next-method))
          (children (read-children node))
          (paths (read-possible-paths range)))
      (unless (null children)
        (iterate
          (for bit in-vector result)
          (for child in-vector children)
          (for i from 0)
          (when (eql 1 bit)
            (setf (gethash child paths) (cons node i)))))
      result)))


(defgeneric same (container bucket element))


(defgeneric distance (container bucket element))


(defgeneric next-position (range data index)
  (:method ((range egnat-range) data index)
    (when (< index (length data))
      (cons (aref data index) (1+ index))))
  (:method ((range egnat-range-around) data index)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (bind (((:slots %container %near %margin) range)
           (result (iterate
                     (declare (type fixnum i))
                     (for i from index below (length data))
                     (for content = (aref data i))
                     (for distance = (distance %container %near content))
                     (finding i such-that (<= distance %margin)))))
      (when result
        (cons (aref data result) (1+ result)))))
  (:method ((range egnat-grow-range) data index)
    (bind (((:slots %container %near) range)
           (result (iterate
                     (for i from index below (length data))
                     (for content = (aref data i))
                     (for same = (same %container content %near))
                     (finding i such-that same))))
      (when result
        (cons (aref data result) (1+ result))))))
